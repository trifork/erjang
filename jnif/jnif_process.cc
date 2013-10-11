

#include "jnif.h"

JavaVM* jvm;

static jmethodID m_eobject__testPID;
static jmethodID m_eobject__testInternalPID;
static jclass NIF_class;
static jmethodID m_NIF__send;
static jfieldID f_einternalpid__gref;
static jfieldID f_einternalpid__id;
static jmethodID m_eproc__self_handle;

static jclass eproc_class;

int assign_global_ref(JNIEnv *je, ERL_NIF_TERM term, ERL_NIF_TERM *out_pid)
{
  jobject pid_object;
  pid_object = je->CallObjectMethod( E2J(term), m_eobject__testInternalPID);
  if (pid_object == NULL) {
    return NIF_FALSE;
  }

  jlong value = je->GetLongField( pid_object, f_einternalpid__gref );
  jobject gref = (jobject)value;
  if (gref == NULL) {
    gref = je->NewGlobalRef( pid_object );
    fprintf(stderr, "assigned gref=%p for id=%i\n", gref, je->GetIntField( pid_object, f_einternalpid__id));
    je->SetLongField (pid_object, f_einternalpid__gref, (jlong)gref);
  }

  *out_pid = J2E( gref );
  return NIF_TRUE;
}

//
// potential problem: the value returned by enif_self is only
// valid for the life time of the given environment; otherwise it
// will have to be copied.
//
ErlNifPid* enif_self(ErlNifEnv* ee, ErlNifPid* out_pid)
{
  if (ee == NULL || ee->je == NULL || ee->self == NULL || out_pid == NULL) return NULL;

  jobject self_pid = ee->je->CallObjectMethod(ee->self, m_eproc__self_handle);

  if (assign_global_ref( ee->je, J2E(self_pid), &out_pid->pid ) == NIF_FALSE) {
    return NULL;
  }

  return out_pid;
}

int enif_get_local_pid(ErlNifEnv* ee, ERL_NIF_TERM term, ErlNifPid* pid)
{
  return assign_global_ref( ee->je, term, &pid->pid);
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
  JNIEnv* je;

  if (ee == NULL) {
    if (jvm->AttachCurrentThreadAsDaemon((void**)&je, NULL) != JNI_OK) {
      return NIF_FALSE;
    }
  } else {
    je = ee->je;
  }

  if (to_pid == NULL)
    return NIF_FALSE;

  jobject pid = E2J(to_pid->pid);
  if (pid == NULL) {
    return NIF_FALSE;
  }

  je->CallStaticVoidMethod( NIF_class,
                            m_NIF__send,
                            pid,
                            E2J(msg) );

  return NIF_TRUE;
}


void initialize_jnif_process(JavaVM* vm, JNIEnv *je)
{
  jvm = vm;

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

  m_eproc__self_handle = je->GetMethodID(eproc_class, "self_handle", "()Lerjang/EInternalPID;");

  jclass einternalpid_class      = je->FindClass("erjang/EInternalPID");
  f_einternalpid__gref = je->GetFieldID(einternalpid_class, "gref", "J");
  f_einternalpid__id = je->GetFieldID(einternalpid_class, "id", "I");


}
