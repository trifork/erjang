
#include "jnif.h"

#include <cstring>

static jmethodID m_eobject__testAtom;
static jclass    eatom_class;
static jmethodID m_eatom__intern;
static jmethodID m_eatom__existing_atom_or_null;
static jmethodID m_eatom__latin1_bytes;
static jmethodID m_eatom__latin1_length;

void initialize_jnif_atom(JavaVM* vm, JNIEnv *je)
{
  jclass eobject_class      = je->FindClass("erjang/EObject");
  m_eobject__testAtom     = je->GetMethodID(eobject_class,
                                              "testAtom",
                                              "()Lerjang/EAtom;");

  eatom_class      = je->FindClass("erjang/EAtom");
  eatom_class      = (jclass) je->NewGlobalRef(eatom_class);
  m_eatom__intern    = je->GetStaticMethodID(eatom_class,
                                           "intern",
                                           "(Ljava/lang/String;)Lerjang/EAtom;");
  m_eatom__existing_atom_or_null    = je->GetStaticMethodID(eatom_class,
                                           "existing_atom_or_null",
                                           "(Ljava/lang/String;)Lerjang/EAtom;");
  m_eatom__latin1_bytes             = je->GetMethodID(eatom_class,
                                                      "latin1_bytes",
                                                      "()[B");
  m_eatom__latin1_length            = je->GetMethodID(eatom_class,
                                                      "latin1_length",
                                                      "()I");

}

int enif_get_atom_length(ErlNifEnv* ee, ERL_NIF_TERM term, unsigned* len, ErlNifCharEncoding encoding)
{
  JNIEnv *je = ee->je;
  jobject atom = je->CallObjectMethod(E2J(term), m_eobject__testAtom);
  if (atom == JVM_NULL || encoding != ERL_NIF_LATIN1)
    return NIF_FALSE;

  *len = je->CallIntMethod(atom, m_eatom__latin1_length);

  return NIF_TRUE;
}

int enif_get_atom(ErlNifEnv* ee, ERL_NIF_TERM term, char* buf, unsigned size, ErlNifCharEncoding encoding)
{
  JNIEnv *je = ee->je;
  jobject atom = je->CallObjectMethod(E2J(term), m_eobject__testAtom);
  if (atom == JVM_NULL || encoding != ERL_NIF_LATIN1)
    return 0;

  jbyteArray barr = (jbyteArray) je->CallObjectMethod(atom, m_eatom__latin1_bytes);
  jint byte_size = je->GetArrayLength(barr);
  if (byte_size+1 > size)
    return 0;

  jbyte* arr = (jbyte*)je->GetPrimitiveArrayCritical(barr, 0);
  memcpy(buf, arr, byte_size);
  je->ReleasePrimitiveArrayCritical(barr, arr, JNI_ABORT);
  buf[byte_size] = 0;

  return byte_size+1;
}



int enif_is_atom(ErlNifEnv* ee, ERL_NIF_TERM term)
{
  JNIEnv *je = ee->je;
  jobject ok = je->CallObjectMethod((jobject)term, m_eobject__testAtom);
  return ok != JVM_NULL;
}


extern ERL_NIF_TERM enif_make_atom (ErlNifEnv* ee, __const char* name)
{
  return enif_make_atom_len(ee, name, strlen(name));
}

extern ERL_NIF_TERM  enif_make_atom_len (ErlNifEnv* ee, __const char* name, size_t len)
{
  jchar buf[len];
  for (int i = 0; i < len; i++) {
    buf[i] = name[i];
  }

  jstring str = ee->je->NewString(buf, len);
  jobject am = ee->je->CallStaticObjectMethod(eatom_class,
                                          m_eatom__intern,
                                          str);

  return jnif_retain(ee, am);
}


extern int enif_make_existing_atom (ErlNifEnv* ee, __const char* name, ERL_NIF_TERM* atom, ErlNifCharEncoding encoding)
{
  return enif_make_existing_atom_len(ee, name, strlen(name), atom, encoding);
}

extern int  enif_make_existing_atom_len (ErlNifEnv* ee, __const char* name, size_t len, ERL_NIF_TERM* atom, ErlNifCharEncoding encoding)
{
  if (encoding != ERL_NIF_LATIN1)
    return NIF_FALSE;

  jchar buf[len];
  for (int i = 0; i < len; i++) {
    buf[i] = name[i];
  }

  jstring str = ee->je->NewString(buf, len);
  jobject am = ee->je->CallStaticObjectMethod(eatom_class,
                                          m_eatom__existing_atom_or_null,
                                          str);

  if (am == NULL)
    return NIF_FALSE;

  *atom = jnif_retain(ee, am);

  return NIF_TRUE;
}
