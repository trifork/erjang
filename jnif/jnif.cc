
#include "jnif.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <list>
#include <dlfcn.h>
#include <errno.h>

#include "erjang_NIF.h"

/** global data */

static JavaVM *jvm;

static jmethodID m_eobject__testReference;

static jclass    ERT_class;
static jmethodID m_ERT__badarg;


void *enif_alloc(size_t size)
{
  return malloc(size);
}

void enif_free(void *data)
{
  free(data);
}

int enif_is_ref(ErlNifEnv* ee, ERL_NIF_TERM term)
{
  JNIEnv *je = ee->je;
  jobject ok = je->CallObjectMethod((jobject)term, m_eobject__testReference);
  return ok != JVM_NULL;
}


ERL_NIF_TERM enif_make_badarg(ErlNifEnv* env)
{
  return THE_BADARG;
}

int enif_is_exception(ErlNifEnv *env, ERL_NIF_TERM term)
{
  return term == THE_BADARG;
}

typedef ErlNifEntry* (*nif_init_t)();

JNIEXPORT jlong JNICALL Java_erjang_NIF_jni_1load
  (JNIEnv * je, jclass self, jstring library, jobject term)
{
  jboolean isCopy;
  const char *path = je->GetStringUTFChars(library, &isCopy);
  if (path == NULL)
    return 0;
  void * so_handle = dlopen( path, /* RTLD_LAZY | */ RTLD_FIRST);

  if (so_handle == NULL) {
    fprintf(stderr, "did not load %s (err: %i)\n", path, errno);
    je->ReleaseStringUTFChars(library, path);
    return 0;
  }

  je->ReleaseStringUTFChars(library, path);

  void *address =  dlsym( so_handle, "nif_init" );
  if (address == NULL) {
    fprintf(stderr, "did not find nif_init (err: %i)\n", errno);
    dlclose(so_handle);
    return 0L;
  }

  nif_init_t nif_init_fp = reinterpret_cast<nif_init_t>(address);
  ErlNifEntry *entry = (*nif_init_fp)();

  struct jnif_module *mod = new jnif_module();
  mod->so_handle = so_handle;
  mod->entry = entry;
  mod->priv_data = NULL;
  mod->global = new enif_environment_t();

  // Call the on-load callback. Terms allocated
  // durin onload are retained until we unload.
  jnif_init_env(mod->global, je, mod);
  if (entry->load != 0) {
    int success = (*entry->load)( mod->global,
                                  &mod->priv_data,
                                  J2E(term));

    if (success != 0) {
      fprintf(stderr, "%s:load returned %i\n",
              mod->entry->name,
              success);

      dlclose( so_handle );
      jnif_release_env(mod->global);
      delete mod;
      mod = NULL;
    }
  }

  // mod->global should be released when unloading
  return reinterpret_cast<jlong>(mod);
}

/*
 * Class:     erjang_NIF
 * Method:    jni_module_name
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_erjang_NIF_jni_1module_1name
  (JNIEnv * je, jclass self, jlong mod_ptr)
{
    struct jnif_module *mod =
    reinterpret_cast<struct jnif_module *>(mod_ptr);

    return je->NewStringUTF(mod->entry->name);
}

/*
 * Class:     erjang_NIF
 * Method:    jni_fun_count
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL Java_erjang_NIF_jni_1fun_1count
  (JNIEnv * je, jclass self, jlong mod_ptr)
{
    struct jnif_module *mod =
    reinterpret_cast<struct jnif_module *>(mod_ptr);

    return mod->entry->num_of_funcs;
}

/*
 * Class:     erjang_NIF
 * Method:    jni_fun_name
 * Signature: (JI)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_erjang_NIF_jni_1fun_1name
  (JNIEnv *je, jclass self, jlong mod_ptr, jint idx)
{
    struct jnif_module *mod =
    reinterpret_cast<struct jnif_module *>(mod_ptr);

    return je->NewStringUTF(mod->entry->funcs[idx].name);
}

/*
 * Class:     erjang_NIF
 * Method:    jni_fun_arity
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL Java_erjang_NIF_jni_1fun_1arity
  (JNIEnv *je, jclass self, jlong mod_ptr, jint idx)
{
    struct jnif_module *mod =
    reinterpret_cast<struct jnif_module *>(mod_ptr);

    return mod->entry->funcs[idx].arity;
}

JNIEXPORT jobject JNICALL Java_erjang_NIF_jni_1invoke
   (JNIEnv* je,
    jobject self,
    jlong mod_addr,
    jint index,
    jobjectArray args)
{
  struct jnif_module *mod =
    reinterpret_cast<struct jnif_module *>(mod_addr);

  ErlNifEntry *entry = mod->entry;
  if (index < 0 || index >= entry->num_of_funcs) {
    return NULL;
  }

  ErlNifFunc *func = &entry->funcs[index];
  jint size = je->GetArrayLength(args);

  ERL_NIF_TERM nif_args[size];
  for (int i = 0; i < size; i++) {
    jobject term = je->GetObjectArrayElement(args, i);
    nif_args[i] = (ERL_NIF_TERM)term;
  }

  struct enif_environment_t ee;
  ee.type = enif_environment_t::STACK;
  jnif_init_env ( &ee, je, mod );

  ERL_NIF_TERM result = (*func->fptr)(&ee, size, nif_args);

  jnif_release_env( &ee );

  if (result == THE_BADARG) {
    je->CallStaticObjectMethod( ERT_class, m_ERT__badarg);
    return NULL;
  }

  return (jobject)result;
}

////////////

static void init_jvm_data(JavaVM *vm, JNIEnv* je)
{
  jvm = vm;

  jclass eobject_class      = je->FindClass("erjang/EObject");
  m_eobject__testReference  = je->GetMethodID(eobject_class, "testReference", "()Lerjang/ERef;");

  ERT_class = je->FindClass("erjang/ERT");
  ERT_class = (jclass)je->NewGlobalRef(ERT_class);
  m_ERT__badarg  = je->GetStaticMethodID(ERT_class, "badarg", "()Lerjang/ErlangError;");
}


extern "C" jint JNI_OnLoad(JavaVM *vm, void *reserved)
{
  JNIEnv *je;
  if (vm->AttachCurrentThreadAsDaemon((void**)&je, NULL) == JNI_OK) {
    init_jvm_data(vm, je);
    initialize_jnif_env(vm,je);
    initialize_jnif_binary(vm,je);
    initialize_jnif_resource(vm,je);
    initialize_jnif_string(vm,je);
    initialize_jnif_number(vm,je);
    initialize_jnif_atom(vm,je);
    initialize_jnif_tuple(vm,je);
    initialize_jnif_list(vm, je);
  }
  return JNI_VERSION_1_4;
}

extern "C" void JNI_OnUnload(JavaVM *vm, void *reserved)
{
  if (vm == jvm) {
    JNIEnv *je;
    if (vm->AttachCurrentThreadAsDaemon((void**)&je, NULL) == JNI_OK) {
      uninitialize_jnif_binary(vm, je);
    }
    jvm = NULL;
  }
}
