
#include "jnif.h"


static jmethodID m_eobject__testAtom;
static jclass    eatom_class;
static jmethodID m_eatom__intern;


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
