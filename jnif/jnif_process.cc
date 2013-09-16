

#include "jnif.h"


static jmethodID m_eobject__testPID;
static jmethodID m_eobject__testInternalPID;
static jclass NIF_class;
static jmethodID m_NIF__send;

static jclass eproc_class;
static jmethodID m_eproc__find;
static jmethodID m_eproc__key;


//
// potential problem: the value returned by enif_self is only
// valid for the life time of the given environment; otherwise it
// will have to be copied.
//
ErlNifPid* enif_self(ErlNifEnv* ee, ErlNifPid* pid)
{
  if (ee->self == NULL) return NULL;
  jlong key = ee->je->CallIntMethod( ee->self, m_eproc__key );
  pid->pid = key;
  return pid;
}

int enif_get_local_pid(ErlNifEnv* ee, ERL_NIF_TERM term, ErlNifPid* pid)
{
  JNIEnv* je = ee->je;
  if (je->CallObjectMethod( E2J(term), m_eobject__testInternalPID) == NULL) {
    return NIF_FALSE;
  } else {
    jlong key = je->CallIntMethod( E2J(term), m_eproc__key );
    pid->pid = key;
    return NIF_TRUE;
  }
}

int enif_is_pid(ErlNifEnv* ee, ERL_NIF_TERM term)
{
  if (ee->je->CallObjectMethod( E2J(term), m_eobject__testPID) == NULL) {
    return NIF_FALSE;
  } else {
    return NIF_TRUE;
  }
}

extern "C" int enif_send(ErlNifEnv* ee, const ErlNifPid* to_pid,
              ErlNifEnv* msg_env, ERL_NIF_TERM msg)
{
  JNIEnv* je = ee->je;
  jint key = to_pid->pid;
  jobject pid = je->CallStaticObjectMethod( eproc_class,
                                            m_eproc__find,
                                            (jint)key );
  if (pid == NULL) {
    return 0;
  }

  je->CallStaticVoidMethod( NIF_class,
                            m_NIF__send,
                            pid,
                            E2J(msg) );

  return 1;
}


void initialize_jnif_process(JavaVM* vm, JNIEnv *je)
{
  jclass eobject_class      = je->FindClass("erjang/EObject");
  m_eobject__testPID        = je->GetMethodID(eobject_class,
                                              "testPID",
                                              "()Lerjang/EPID;");
  m_eobject__testInternalPID = je->GetMethodID(eobject_class,
                                              "testInternalPID",
                                              "()Lerjang/EInternalPID;");

  NIF_class = je->FindClass("erjang/NIF");
  NIF_class = (jclass) je->NewGlobalRef( NIF_class );

  m_NIF__send = je->GetStaticMethodID(NIF_class,
                                      "send",
                                      "(Lerjang/EPID;Lerjang/EObject;)V");

  eproc_class = je->FindClass("erjang/EProc");
  eproc_class = (jclass) je->NewGlobalRef( eproc_class );

  m_eproc__find= je->GetStaticMethodID(eproc_class,
                                       "find",
                                       "(I)Lerjang/EProc;");

  m_eproc__key = je->GetMethodID(eproc_class,
                                 "key",
                                 "()I");

}
