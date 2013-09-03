
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

  m_eresource__make  = je->GetStaticMethodID(eresource_class, "make", "(L)Lerjang/EResource;");

  f_eresource__handle  = je->GetFieldID(eresource_class, "handle", "L");
}





ErlNifResourceType* enif_open_resource_type(ErlNifEnv*,
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


void enif_release_resource(void* obj)
{
  struct jnif_resource_hdr *hdr = get(obj);

  if (hdr == NULL) {
    return;
  }

  hdr->refcount -= 1;

  if (hdr->refcount == 0) {

    JNIEnv *je;
    jvm->AttachCurrentThreadAsDaemon((void**)je, NULL);

    struct enif_environment_t ee;
    ee.type = enif_environment_t::STACK;
    jnif_init_env ( &ee, je, hdr->type->module );

    hdr->type->dtor( &ee, obj );

    jnif_release_env( &ee );

    free(hdr);
  }
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
  enif_release_resource((void*)handle);
}
