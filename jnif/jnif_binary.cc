
#include "jnif.h"


static jmethodID m_eobject__testBinary;

static jfieldID  f_ebitstring__data;
static jfieldID  f_ebitstring__byte_size;
static jfieldID  f_ebitstring__data_offset;

static jclass    ErlConvert_class;
static jclass    ebinary_class;
static jmethodID m_ebinary__make;
static jmethodID m_ErlConvert__iolist_to_binary;


void initialize_jnif_binary(JavaVM* vm, JNIEnv *je)
{
  jclass eobject_class      = je->FindClass("erjang/EObject");
  m_eobject__testBinary     = je->GetMethodID(eobject_class, "testBinary", "()Lerjang/EBinary;");

  jclass ebitstring_class       = je->FindClass("erjang/EBitString");
  f_ebitstring__byte_size   = je->GetFieldID(ebitstring_class, "byte_size", "I");
  f_ebitstring__data_offset = je->GetFieldID(ebitstring_class, "data_offset", "I");
  f_ebitstring__data        = je->GetFieldID(ebitstring_class, "data", "[B");

  ebinary_class = je->FindClass("erjang/EBinary");
  ebinary_class = (jclass)je->NewGlobalRef(ebinary_class);

  m_ebinary__make  = je->GetStaticMethodID(ebinary_class, "make", "([B)Lerjang/EBinary;");

  ErlConvert_class = je->FindClass("erjang/m/erlang/ErlConvert");
  ErlConvert_class = (jclass) je->NewGlobalRef(ErlConvert_class);
  m_ErlConvert__iolist_to_binary =
    je->GetStaticMethodID(ErlConvert_class,
                          "iolist_to_binary",
                          "(Lerjang/EObject;)Lerjang/EBitString;");
}

static void jnif_release_binary(struct jnif_bin_data *bd);

struct jnif_bin_data : jnif_dtor {
  enum { SHOULD_RELEASE, SHOULD_FREE, IS_FREE } type;
  JNIEnv     *je;
  jbyteArray  array;
  jbyte      *elements;

  void release(ErlNifEnv *env) {
    jnif_release_binary(this);
  }
};


void uninitialize_jnif_binary(JavaVM* vm, JNIEnv* je)
{
  je->DeleteGlobalRef(ebinary_class);
}

int enif_is_binary(ErlNifEnv* ee, ERL_NIF_TERM term)
{
  JNIEnv *je = ee->je;
  jobject ok = je->CallObjectMethod(E2J(term), m_eobject__testBinary);
  return ok != JVM_NULL;
}

int enif_inspect_iolist_as_binary(ErlNifEnv* ee, ERL_NIF_TERM bin_term, ErlNifBinary* bin)
{
  JNIEnv *je = ee->je;
  jobject binary = je->CallStaticObjectMethod(ErlConvert_class,
                                              m_ErlConvert__iolist_to_binary,
                                              E2J(bin_term));
  if (je->ExceptionOccurred())
    return 0;

  return enif_inspect_binary(ee, J2E(binary), bin);
}

int enif_inspect_binary(ErlNifEnv* ee, ERL_NIF_TERM bin_term, ErlNifBinary* bin)
{
  if (!enif_is_binary(ee, bin_term) || bin == NULL)
    return NIF_FALSE;

  JNIEnv *je = ee->je;

  jobject binary = (jobject)bin_term;

  jsize size       = je->GetIntField(binary, f_ebitstring__byte_size);
  jsize offset     = je->GetIntField(binary, f_ebitstring__data_offset);
  jobject array    = je->GetObjectField(binary, f_ebitstring__data);

  jbyte* elements = (jbyte*)malloc( size );

  je->GetByteArrayRegion((jbyteArray) array, offset, size, elements);

  if (elements == NULL) {
    bin->ref_bin = NULL;
    bin->data = NULL;
    bin->size = 0;
    return NIF_FALSE;
  }

  // TODO: If we redefine nif headers, then we can avoid this
  jnif_bin_data *bd = new jnif_bin_data();
  bd->type     = jnif_bin_data::SHOULD_FREE;
  bd->elements = elements;

  // add to "autorelease pool" for this env
  ee->dtors.push_back( bd );

  bin->ref_bin = bd;

  bin->size = size;
  bin->data = (unsigned char*)elements;

  return NIF_TRUE;
}

int enif_alloc_binary(size_t size, ErlNifBinary* bin)
{
  void *mem = malloc( size );
  if (mem == NULL)
    return NIF_FALSE;

  bin->size = size;
  bin->data = (unsigned char*)mem;

  jnif_bin_data *bd = new jnif_bin_data();
  bd->type     = jnif_bin_data::SHOULD_FREE;
  bd->elements = (jbyte*)mem;
  bin->ref_bin = bd;

  return NIF_TRUE;
}

int enif_realloc_binary(ErlNifBinary* bin, size_t size)
{
  if (bin->ref_bin != NULL) {
    jnif_bin_data *bd = (jnif_bin_data*)bin->ref_bin;
    if (bd->type == jnif_bin_data::SHOULD_FREE) {
      bin->data = (unsigned char *) realloc(bin->data, size);
      bin->size = size;
      return bin->data != NULL;
    }
  }

  return NIF_FALSE;
}


void jnif_release_binary(struct jnif_bin_data *bd)
{
  if (bd->type == jnif_bin_data::SHOULD_RELEASE) {
    bd->je->ReleaseByteArrayElements(bd->array, bd->elements, JNI_ABORT);
    bd->type = jnif_bin_data::IS_FREE;

  } else if (bd->type == jnif_bin_data::SHOULD_FREE) {
    free( bd->elements );
    bd->type = jnif_bin_data::IS_FREE;
  }
}

void enif_release_binary(ErlNifBinary* bin)
{
  if (bin->ref_bin != NULL) {
    jnif_bin_data *bd = (jnif_bin_data*)bin->ref_bin;
    delete bd;
    bin->ref_bin = 0;
    bin->data = NULL;
    bin->ref_bin = NULL;
  }
}

ERL_NIF_TERM enif_make_binary(ErlNifEnv *ee, ErlNifBinary* bin)
{
  JNIEnv *je = ee->je;

  // construct byte[]
  jbyteArray barr = je->NewByteArray(bin->size);
  jbyte* arr = (jbyte*)je->GetPrimitiveArrayCritical(barr, 0);
  memcpy(arr, bin->data, bin->size);
  je->ReleasePrimitiveArrayCritical(barr, arr, JNI_COMMIT);

  jobject binary = je->CallStaticObjectMethod(ebinary_class, m_ebinary__make, barr);

  ERL_NIF_TERM result = jnif_retain(ee, binary);

  enif_release_binary(bin);

  return result;
}

