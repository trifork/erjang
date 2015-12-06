
#include "jnif.h"

#include <cstring>

static jmethodID m_eobject__testString;
static jmethodID m_estring__stringValue;
static jmethodID m_estring__make;

static jclass estring_class;


void initialize_jnif_string(JavaVM* vm, JNIEnv *je)
{
  jclass eobject_class      = je->FindClass("erjang/EObject");
  m_eobject__testString     = je->GetMethodID(eobject_class,
                                              "testString",
                                              "()Lerjang/EString;");

  estring_class             = je->FindClass("erjang/EString");
  estring_class             = (jclass) je->NewGlobalRef(estring_class);
  m_estring__stringValue    = je->GetMethodID(estring_class,
                                              "stringValue",
                                              "()Ljava/lang/String;");
  m_estring__make           = je->GetStaticMethodID(estring_class,
                                                    "make",
                                                    "([BII)Lerjang/EString;");

}

ERL_NIF_TERM enif_make_string(ErlNifEnv* ee, const char* string, ErlNifCharEncoding encoding)
{
  return enif_make_string_len(ee, string, strlen(string), encoding);
}


ERL_NIF_TERM enif_make_string_len(ErlNifEnv* ee, const char* string, size_t size, ErlNifCharEncoding encoding)
{
  JNIEnv *je = ee->je;

  if (encoding == ERL_NIF_LATIN1) {
    jbyteArray barr = je->NewByteArray(size);
    jbyte* arr = (jbyte*)je->GetPrimitiveArrayCritical(barr, 0);
    memcpy(arr, string, size);
    je->ReleasePrimitiveArrayCritical(barr, arr, JNI_COMMIT);

    jobject result = je->CallStaticObjectMethod(estring_class,
                                                m_estring__make,
                                                barr, (jint)0, size);

    return jnif_retain( ee, result );
  } else {
    // TODO: what do do?
    abort();
  }
}

int enif_get_string(ErlNifEnv* ee,
                    ERL_NIF_TERM list,
                    char* buf,
                    unsigned size,
                    ErlNifCharEncoding encode)
{
  JNIEnv *je = ee->je;

  if (encode != ERL_NIF_LATIN1)
    return 0;

  jobject o = je->CallObjectMethod(E2J(list), m_eobject__testString);
  if (o == NULL)
    return 0;

  jstring s = (jstring) je->CallObjectMethod(o, m_estring__stringValue);
  if (s == NULL)
    return 0;

  int ss = je->GetStringLength(s);
  int lim = ss > size-1 ? size-1 : ss;

  const jchar *arr = je->GetStringCritical(s, 0);
  for (int i = 0; i < lim; i++) {
    buf[i] = arr[i];
  }
  je->ReleaseStringCritical(s, arr);

  buf[lim] = 0;
  if (lim != ss)
    return -size;
  else
    return lim+1;
}
