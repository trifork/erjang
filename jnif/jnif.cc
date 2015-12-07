/* -*- c-basic-offset: 2 -*- */

#include "jnif.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <list>
#include <dlfcn.h>

#include "erjang_NIF.h"

/** global data */

#ifndef __MACH__
static void* self_so_handle;
#endif

static JavaVM *jvm;

static jmethodID m_eobject__testReference;
static jmethodID m_eobject__testFunction;
static jmethodID m_eobject__testPort;
static jmethodID m_eobject__equals;
static jmethodID m_eobject__compare;

static jclass    ERT_class;
static jmethodID m_ERT__badarg;
static jmethodID m_ERT__make_ref;


void* enif_realloc(void* ptr, size_t size)
{
  return realloc(ptr, size);
}

void *enif_alloc(size_t size)
{
  return malloc(size);
}

void enif_free(void *data)
{
  free(data);
}

int enif_fprintf(void/* FILE* */ *filep, const char *format, ...)
{
  va_list vl;
  va_start(vl, format);
  int result = vfprintf((FILE*) filep, format, vl);
  va_end(vl);
  return result;
}

int enif_consume_timeslice(ErlNifEnv* env, int percent)
{
  // TODO: Figure out if this is really needed.
  // We can definitively do something reasonable, but need to understand what.
  return 0;
}

int enif_is_identical(ERL_NIF_TERM term1, ERL_NIF_TERM term2)
{
  if (term1 == term2)
    return NIF_TRUE;

  jobject o1 = E2J(term1);
  jobject o2 = E2J(term2);

  JNIEnv *je;
  if (jvm->AttachCurrentThreadAsDaemon((void**)&je, NULL) == JNI_OK) {
    if (je->CallBooleanMethod(o1, m_eobject__equals, o2)) {
      return NIF_TRUE;
    } else {
      return NIF_FALSE;
    }
  }

  return NIF_FALSE;
}

int enif_compare(ERL_NIF_TERM term1, ERL_NIF_TERM term2)
{
  if (term1 == term2)
    return 0;

  jobject o1 = E2J(term1);
  jobject o2 = E2J(term2);

  JNIEnv *je;
  if (jvm->AttachCurrentThreadAsDaemon((void**)&je, NULL) == JNI_OK) {
    return je->CallIntMethod(o1, m_eobject__compare, o2);
  }

  abort();
}

ERL_NIF_TERM enif_make_ref(ErlNifEnv *ee) {
  return jnif_retain( ee, ee->je->CallStaticObjectMethod(ERT_class, m_ERT__make_ref));
}

int enif_is_ref(ErlNifEnv* ee, ERL_NIF_TERM term)
{
  JNIEnv *je = ee->je;
  jobject ok = je->CallObjectMethod((jobject)term, m_eobject__testReference);
  return ok != JVM_NULL;
}

int enif_is_fun(ErlNifEnv* ee, ERL_NIF_TERM term)
{
  JNIEnv *je = ee->je;
  jobject ok = je->CallObjectMethod((jobject)term, m_eobject__testFunction);
  return ok != JVM_NULL;
}

int enif_is_port(ErlNifEnv* ee, ERL_NIF_TERM term)
{
  JNIEnv *je = ee->je;
  jobject ok = je->CallObjectMethod((jobject)term, m_eobject__testPort);
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
  void * so_handle = dlopen( path, /* RTLD_LAZY | */
#ifdef __MACH__
RTLD_NOW | RTLD_LOCAL | RTLD_FIRST
#else
#ifdef __GNU
RTLD_NOW | RTLD_LOCAL //RTLD_DEEPBIND
#else
RTLD_NOW | RTLD_LOCAL
#endif
#endif
);

  if (so_handle == NULL) {
    fprintf(stderr, "did not load %s (error: %s)\n", path, dlerror());
    je->ReleaseStringUTFChars(library, path);
    return 0;
  }

  je->ReleaseStringUTFChars(library, path);

  void *address =  dlsym( so_handle, "nif_init" );
  if (address == NULL) {
    fprintf(stderr, "did not find nif_init (error: %s)\n", dlerror());
    dlclose(so_handle);
    return 0L;
  }

  nif_init_t nif_init_fp = reinterpret_cast<nif_init_t>(address);
  ErlNifEntry *entry = (*nif_init_fp)();

  if (entry->major != ERL_NIF_MAJOR_VERSION) {
    // fail
    fprintf(stderr, "bad erl_nif_major_version %i (vs. expected %i)\n",
            entry->major,
            ERL_NIF_MAJOR_VERSION);
    dlclose(so_handle);
    return 0L;
  }

  struct jnif_module *mod = new jnif_module();
  mod->so_handle = so_handle;
  mod->entry = entry;
  mod->priv_data = NULL;
  mod->global = new enif_environment_t();

  // Call the on-load callback. Terms allocated
  // durin onload are retained until we unload.
  jnif_init_env(mod->global, je, mod, enif_environment_t::ALLOC);
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

  // when nif_init is done, we may need to "commit" data that
  // was made available to the NIF as char* data.
  if (mod != NULL) {
    jnif_commit_env(mod->global);
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
    jobject proc,
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
  jnif_init_env ( &ee, je, mod, enif_environment_t::STACK );
  ee.self = proc;

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
  m_eobject__testFunction   = je->GetMethodID(eobject_class, "testFunction", "()Lerjang/EFun;");
  m_eobject__testPort       = je->GetMethodID(eobject_class, "testPort", "()Lerjang/EPort;");
  m_eobject__equals         = je->GetMethodID(eobject_class, "equalsExactly", "(Lerjang/EObject;)Z");
  m_eobject__compare        = je->GetMethodID(eobject_class, "erlangCompareTo", "(Lerjang/EObject;)I");

  jclass ERT_class      = je->FindClass("erjang/ERT");

  ERT_class = je->FindClass("erjang/ERT");
  ERT_class = (jclass)je->NewGlobalRef(ERT_class);
  m_ERT__badarg   = je->GetStaticMethodID(ERT_class, "badarg", "()Lerjang/ErlangError;");
  m_ERT__make_ref = je->GetStaticMethodID(ERT_class, "make_ref", "()Lerjang/ERef;");
}

/** Makes the exported symbols in the JNIF library visible from libraries
 *  to be loaded later.
 *  This is necessary at least on OpenJDK 1.7.0_25 on Linux;
 *  apparently, java.lang.System.loadLibrary() does not load with the
 *  RTLD_GLOBAL flag.
 */
static int export_jnif_symbols() {
#ifndef __MACH__
  self_so_handle = dlopen("libjnif.so", RTLD_NOW | RTLD_GLOBAL);
  if (self_so_handle == NULL) {
    fprintf(stderr, "JNIF: self-exporting failed (error: %s)\n", dlerror());
  }
#endif
  return 1;
}


extern "C" jint JNI_OnLoad(JavaVM *vm, void *reserved)
{
  JNIEnv *je;
  if (export_jnif_symbols() &&
      vm->AttachCurrentThreadAsDaemon((void**)&je, NULL) == JNI_OK) {
    init_jvm_data(vm, je);
    initialize_jnif_env(vm,je);
    initialize_jnif_binary(vm,je);
    initialize_jnif_resource(vm,je);
    initialize_jnif_string(vm,je);
    initialize_jnif_number(vm,je);
    initialize_jnif_atom(vm,je);
    initialize_jnif_tuple(vm,je);
    initialize_jnif_list(vm, je);
    initialize_jnif_sys(vm, je);
    initialize_jnif_process(vm, je);
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

#ifndef __MACH__
    if (self_so_handle != NULL) {
      dlclose(self_so_handle);
    }
#endif
  }
}
