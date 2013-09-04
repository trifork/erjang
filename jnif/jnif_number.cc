
#include "jnif.h"


static jmethodID m_eobject__testNumber;
static jmethodID m_enumber__doubleValue;
static jmethodID m_enumber__intValue;
static jmethodID m_enumber__longValue;


void initialize_jnif_number(JavaVM* vm, JNIEnv *je)
{
  jclass eobject_class      = je->FindClass("erjang/EObject");
  m_eobject__testNumber     = je->GetMethodID(eobject_class,
                                              "testNumber",
                                              "()Lerjang/ENumber;");

  jclass enumber_class      = je->FindClass("erjang/ENumber");
  m_enumber__doubleValue    = je->GetMethodID(enumber_class,
                                              "doubleValue",
                                              "()D");
  m_enumber__intValue       = je->GetMethodID(enumber_class,
                                              "intValue",
                                              "()I");
  m_enumber__longValue      = je->GetMethodID(enumber_class,
                                              "longValue",
                                              "()J");

}


int enif_get_double(ErlNifEnv* ee, ERL_NIF_TERM term, double* dp)
{
  jobject o = ee->je->CallObjectMethod(E2J(term), m_eobject__testNumber);
  if (o == NULL)
    return NIF_FALSE;

  *dp = ee->je->CallDoubleMethod(o, m_enumber__doubleValue);
  return NIF_TRUE;
}

int enif_get_int(ErlNifEnv* ee, ERL_NIF_TERM term, int* ip)
{
  jobject o = ee->je->CallObjectMethod(E2J(term), m_eobject__testNumber);
  if (o == NULL)
    return NIF_FALSE;

  *ip = ee->je->CallIntMethod(o, m_enumber__intValue);
  return NIF_TRUE;
}

int enif_get_int64(ErlNifEnv* ee, ERL_NIF_TERM term, ErlNifSInt64* ip)
{
  jobject o = ee->je->CallObjectMethod(E2J(term), m_eobject__testNumber);
  if (o == NULL)
    return NIF_FALSE;

  *ip = ee->je->CallLongMethod(o, m_enumber__longValue);
  return NIF_TRUE;
}

