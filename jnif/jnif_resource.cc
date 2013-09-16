
#include "jnif.h"
#include "erjang_EResource.h"

struct enif_resource_type_t
{
  const char* module_str;
  const char* name_str;
  void (*dtor)(ErlNifEnv*,void *);
  ErlNifResourceFlags flags;
  ErlNifResourceFlags* tried;
  jnif_module *module;
};

// "RSRC"
#define RESOURCE_MAGIC 0x52535253UL

struct jnif_resource_hdr {
  uint32_t magic;
  ErlNifResourceType *type;
  size_t size;
  int refcount;
};

static JavaVM *jvm;

static jclass    eresource_class;
static jfieldID  f_eresource__handle;
static jmethodID m_eresource__make;


static inline jnif_resource_hdr* get(void *obj)
{
  struct jnif_resource_hdr *hdr = ((struct jnif_resource_hdr*) obj) - 1;
  if (hdr->magic == RESOURCE_MAGIC)
    return hdr;
  return NULL;
}

void initialize_jnif_resource(JavaVM* vm, JNIEnv *je)
{
  jvm = vm;

  eresource_class = je->FindClass("erjang/EResource");
  eresource_class = (jclass)je->NewGlobalRef(eresource_class);

  m_eresource__make  = je->GetStaticMethodID(eresource_class, "make", "(J)Lerjang/EResource;");

  f_eresource__handle  = je->GetFieldID(eresource_class, "handle", "J");
}





ErlNifResourceType* enif_open_resource_type(ErlNifEnv* ee,
                                            const char* module_str,
                                            const char* name_str,
                                            void (*dtor)(ErlNifEnv*,void *),
                                            ErlNifResourceFlags flags,
                                            ErlNifResourceFlags* tried)
{
  // VERY SIMPLE; just always create the resource

  struct enif_resource_type_t *res = new enif_resource_type_t();
  res->module_str = module_str == NULL ? NULL : strdup(module_str);
  res->name_str = name_str == NULL ? NULL : strdup(name_str);
  res->dtor = dtor;
  res->flags = flags;
  res->module = ee->module;
  if (tried != NULL) {
    *tried = ERL_NIF_RT_CREATE;
  }
  return res;
}

void* enif_alloc_resource(ErlNifResourceType* type, size_t size)
{
  size_t total_size = sizeof(struct jnif_resource_hdr) + size;
  void *mem = malloc(total_size);
  struct jnif_resource_hdr *hdr = (struct jnif_resource_hdr*) mem;
  hdr->type = type;
  hdr->size = size;
  hdr->refcount = 1;
  hdr->magic = RESOURCE_MAGIC;

  return (void*)(& hdr[1]);
}


static int jnif_release_resource(JNIEnv *je, void* obj)
{
  struct jnif_resource_hdr *hdr = get(obj);

  if (hdr == NULL) {
    return 0;
  }

  hdr->refcount -= 1;

  if (hdr->refcount == 0) {

    struct enif_environment_t ee;
    jnif_init_env ( &ee, je, hdr->type->module, enif_environment_t::STACK  );

    hdr->type->dtor( &ee, obj );

    jnif_release_env( &ee );

    free(hdr);
    return 1;
  }

  return 0;
}

void enif_release_resource(void* obj)
{
    JNIEnv *je;
    jvm->AttachCurrentThreadAsDaemon((void**)&je, NULL);
    jnif_release_resource(je, obj);
}



ERL_NIF_TERM enif_make_resource(ErlNifEnv* ee, void* obj)
{
  JNIEnv *je = ee->je;

  struct jnif_resource_hdr *hdr = get(obj);
  hdr->refcount += 1;

  jobject resource = je->CallStaticObjectMethod(eresource_class, m_eresource__make, (jlong) obj);

  return (ERL_NIF_TERM) jnif_retain(ee, resource);
}

int enif_get_resource(ErlNifEnv* ee, ERL_NIF_TERM term, ErlNifResourceType* type, void** objp)
{
  JNIEnv *je = ee->je;
  jobject eres = E2J(term);

  jlong handle = je->GetLongField(eres, f_eresource__handle);

  struct jnif_resource_hdr *hdr = get((void*)handle);

  if (hdr->type == type) {
    *objp = (void*)handle;
    return NIF_TRUE;
  }

  return NIF_FALSE;
}

size_t enif_sizeof_resource(void* mem)
{
  struct jnif_resource_hdr *hdr = get(mem);
  return hdr->size;
}


void enif_keep_resource(void* obj)
{
  struct jnif_resource_hdr *hdr = get(obj);
  hdr->refcount += 1;
}

ERL_NIF_TERM enif_make_resource_binary(ErlNifEnv* ee,
                                       void* obj,
                                       const void* data,
                                       size_t size)
{
  // NOT supported
  assert(false);
}

JNIEXPORT void JNICALL Java_erjang_EResource_jni_1finalize
  (JNIEnv *je, jclass _, jlong handle)
{
#ifdef DEBUG
  fprintf(stderr, "... finalize ...");
  struct jnif_resource_hdr *hdr = get((void*)handle);
  const char *mod  = hdr->type->module_str;
  const char *rnam = hdr->type->name_str;
  fprintf(stderr, "finalize(%p) %s:%s\n", (void*)handle, mod, rnam);
#endif
  int did_free = jnif_release_resource(je, (void*)handle);
#ifdef DEBUG
  fprintf(stderr, " -> done (%s)\n", did_free ? "feed" : "retained");
#endif
}


/*
 * Class:     erjang_EResource
 * Method:    jnif_module
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_erjang_EResource_jnif_1module
  (JNIEnv *je, jclass _, jlong handle)
{
  struct jnif_resource_hdr *hdr = get((void*)handle);
  if (hdr == NULL || hdr->type == NULL) return NULL;

  const char *mod_name = hdr->type->module_str;

  if (mod_name == NULL
      && hdr->type != NULL
      && hdr->type->module != NULL
      && hdr->type->module->entry != NULL) {
    mod_name = hdr->type->module->entry->name;
  }

  if (mod_name != NULL)
    return je->NewStringUTF(mod_name);

  return NULL;
}

/*
 * Class:     erjang_EResource
 * Method:    jnif_type_name
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_erjang_EResource_jnif_1type_1name
  (JNIEnv *je, jclass _, jlong handle)
{
  struct jnif_resource_hdr *hdr = get((void*)handle);
  if (hdr == NULL) return NULL;

  const char *rnam = hdr->type->name_str;
  if (rnam != NULL)
    return je->NewStringUTF(rnam);

  return NULL;
}
