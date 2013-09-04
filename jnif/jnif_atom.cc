
#include "jnif.h"


static jmethodID m_eobject__testAtom;
static jclass    eatom_class;
static jmethodID m_eatom__intern;
static jmethodID m_eatom__existing_atom_or_null;

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
