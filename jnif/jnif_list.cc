

#include "jnif.h"

static jclass    eobject_class;
static jmethodID m_eobject__testCons;
static jmethodID m_eobject__testSeq;

static jclass    elist_class;
static jmethodID m_elist__make;
static jmethodID m_eseq__length;
static jmethodID m_econs__head;
static jmethodID m_econs__tail;

static jclass    ERT_class;
static jmethodID m_ERT__cons;

static jobject empty_list;

extern int enif_is_list (ErlNifEnv* ee, ERL_NIF_TERM term)
{
  jobject jterm = E2J(term);

  if ( ee->je->CallObjectMethod(jterm, m_eobject__testCons) != NULL) {
    return NIF_TRUE;
  }

  if ( ee->je->CallObjectMethod(jterm, m_eobject__testSeq) != NULL) {
    return NIF_TRUE;
  }

  return NIF_FALSE;
}

extern int enif_get_list_length (ErlNifEnv* ee, ERL_NIF_TERM term, unsigned* len)
{
  jobject seq = ee->je->CallObjectMethod(E2J(term), m_eobject__testSeq);
  if (seq == NULL)
    return NIF_FALSE;

  *len = ee->je->CallIntMethod(seq, m_eseq__length);
  return NIF_TRUE;
}

extern int enif_get_list_cell (ErlNifEnv* ee, ERL_NIF_TERM term, ERL_NIF_TERM* head, ERL_NIF_TERM* tail)
{
  JNIEnv *je = ee->je;

  jobject cons = je->CallObjectMethod(E2J(term), m_eobject__testCons);
  if (cons == NULL)
    return NIF_FALSE;

  *head = J2E(  je->CallObjectMethod(cons, m_econs__head)  );
  *tail = J2E(  je->CallObjectMethod(cons, m_econs__tail)  );

  return NIF_TRUE;
}


extern ERL_NIF_TERM enif_make_list_cell (ErlNifEnv* ee, ERL_NIF_TERM car, ERL_NIF_TERM cdr)
{
  jobject jcar = E2J(car);
  jobject jcdr = E2J(cdr);
  jobject result = ee->je->CallStaticObjectMethod(ERT_class, m_ERT__cons,
                                                  jcar, jcdr);

  return jnif_retain(ee, result);
}


extern ERL_NIF_TERM enif_make_list (ErlNifEnv* ee, unsigned cnt, ...)
{

  JNIEnv *je = ee->je;

  if (cnt == 0) {
    return (ERL_NIF_TERM) empty_list;
  }

  va_list vl;
  jobjectArray arr = je->NewObjectArray(cnt, eobject_class, NULL);

  va_start(vl,cnt);
  for (int i = 0; i < cnt; i++)
    {
      ERL_NIF_TERM val=va_arg(vl,ERL_NIF_TERM);
      ee->je->SetObjectArrayElement(arr, i, E2J(val));
    }
  va_end(vl);

  jobject list = ee->je->CallStaticObjectMethod(elist_class, m_elist__make, cnt);
  return jnif_retain(ee, list);
}

void initialize_jnif_list(JavaVM* vm, JNIEnv *je)
{
  eobject_class      = je->FindClass("erjang/EObject");
  eobject_class      = (jclass) je->NewGlobalRef(eobject_class);
  m_eobject__testSeq     = je->GetMethodID(eobject_class,
                                              "testSeq",
                                              "()Lerjang/ESeq;");
  m_eobject__testCons    = je->GetMethodID(eobject_class,
                                              "testCons",
                                              "()Lerjang/ECons;");

  elist_class      = je->FindClass("erjang/EList");
  elist_class      = (jclass) je->NewGlobalRef(elist_class);
  m_elist__make    = je->GetStaticMethodID(elist_class,
                                           "make",
                                           "([Ljava/lang/Object;)Lerjang/ESeq;");

  jclass eseq_class = je->FindClass("erjang/ESeq");
  m_eseq__length    = je->GetMethodID(eseq_class,
                                     "length",
                                     "()I");

  jclass econs_class = je->FindClass("erjang/ECons");
  m_econs__head    = je->GetMethodID(econs_class, "head", "()Lerjang/EObject;");
  m_econs__tail    = je->GetMethodID(econs_class, "tail", "()Lerjang/EObject;");

  ERT_class = (jclass) je->NewGlobalRef ( je->FindClass("erjang/ERT") );
  m_ERT__cons = je->GetStaticMethodID
    (ERT_class, "cons", "(Lerjang/EObject;Lerjang/EObject;)Lerjang/ECons;");

  jfieldID f_ERT__NIL = je->GetStaticFieldID(ERT_class, "NIL", "Lerjang/ENil;");
  empty_list = je->GetStaticObjectField( ERT_class, f_ERT__NIL );
  empty_list = je->NewGlobalRef( empty_list );
}
