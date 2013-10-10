

#include "jnif.h"

static jmethodID m_eobject__testTuple;
static jclass    etuple_class;
static jmethodID m_etuple__make;
static jmethodID m_etuple__set;
static jmethodID m_etuple__elm;
static jmethodID m_etuple__arity;
static jmethodID m_object__toString;

extern int enif_is_tuple (ErlNifEnv* ee, ERL_NIF_TERM term)
{
  if ( ee->je->CallObjectMethod(E2J(term), m_eobject__testTuple) == NULL) {
    return NIF_FALSE;
  } else {
    return NIF_TRUE;
  }
}

ERL_NIF_TERM enif_make_tuple_from_array(ErlNifEnv* ee, const ERL_NIF_TERM arr[], unsigned cnt)
{
  jobject tup;
  tup = ee->je->CallStaticObjectMethod(etuple_class, m_etuple__make, cnt);

  for (int i = 0; i < cnt; i++) {
    ee->je->CallVoidMethod(tup, m_etuple__set, (jint)(i+1), E2J(arr[i]));
  }

  return jnif_retain(ee, tup);

}

int enif_get_tuple(ErlNifEnv* ee, ERL_NIF_TERM tpl, int* arity, const ERL_NIF_TERM** a0)
{
  JNIEnv *je = ee->je;
  jobject tup = je->CallObjectMethod(E2J(tpl), m_eobject__testTuple);
  if (tup == NULL)
    return NIF_FALSE;

  int count = *arity = je->CallIntMethod(tup, m_etuple__arity);
  if (count == 0) { return NIF_TRUE; }

  ERL_NIF_TERM** array = (ERL_NIF_TERM**) a0;

  // ok ... check arity
  if (array == NULL)
    return NIF_TRUE;

  *array = (ERL_NIF_TERM*)malloc( sizeof(ERL_NIF_TERM) * count );
  for (int i = 0; i < count; i++) {
    (*array)[i] = jnif_retain(ee, je->CallObjectMethod( tup, m_etuple__elm, i+1) );
  }

  // TODO: free *array in environment

  return NIF_TRUE;
}


extern ERL_NIF_TERM enif_make_tuple (ErlNifEnv* ee, unsigned cnt, ...)
{
  ERL_NIF_TERM args[cnt];

  if (cnt > 0) {
  va_list vl;
  va_start(vl,cnt);
  for (int i = 0; i < cnt; i++)
    {
      args[i]  = va_arg(vl,ERL_NIF_TERM);
    }
  va_end(vl);
  }

  jobject tup;
  tup = ee->je->CallStaticObjectMethod(etuple_class, m_etuple__make, cnt);

  for (int i = 0; i < cnt; i++) {
    ee->je->CallVoidMethod(tup, m_etuple__set, (jint)(i+1), (jobject)E2J(args[i]));
  }

#ifdef DEBUG
  jstring str = (jstring) ee->je->CallObjectMethod(tup, m_object__toString);
  const char *path = str == NULL ? NULL : ee->je->GetStringUTFChars(str, NULL);
  fprintf(stderr, "created: %s\n", path);
  ee->je->ReleaseStringUTFChars(str, path);
#endif

  return jnif_retain(ee, tup);
}

void initialize_jnif_tuple(JavaVM* vm, JNIEnv *je)
{
  jclass eobject_class      = je->FindClass("erjang/EObject");
  m_eobject__testTuple     = je->GetMethodID(eobject_class,
                                              "testTuple",
                                              "()Lerjang/ETuple;");

  etuple_class      = je->FindClass("erjang/ETuple");
  etuple_class      = (jclass) je->NewGlobalRef(etuple_class);
  m_etuple__make    = je->GetStaticMethodID(etuple_class,
                                           "make",
                                           "(I)Lerjang/ETuple;");
  m_etuple__set    = je->GetMethodID(etuple_class,
                                     "set",
                                     "(ILerjang/EObject;)V");
  m_etuple__elm    = je->GetMethodID(etuple_class,
                                     "elm",
                                     "(I)Lerjang/EObject;");
  m_etuple__arity    = je->GetMethodID(etuple_class,
                                     "arity",
                                     "()I");

  jclass object_class = je->FindClass("java/lang/String");
  m_object__toString = je->GetMethodID(object_class,
                                       "toString",
                                       "()Ljava/lang/String;");

}
