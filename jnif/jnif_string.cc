
#include "jnif.h"


static jmethodID m_eobject__testString;
static jmethodID m_estring__stringValue;


void initialize_jnif_string(JavaVM* vm, JNIEnv *je)
{
  jclass eobject_class      = je->FindClass("erjang/EObject");
  m_eobject__testString     = je->GetMethodID(eobject_class,
                                              "testString",
                                              "()Lerjang/EString;");

  jclass estring_class      = je->FindClass("erjang/EString");
  m_estring__stringValue    = je->GetMethodID(estring_class,
                                              "stringValue",
                                              "()Ljava/lang/String;");

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
