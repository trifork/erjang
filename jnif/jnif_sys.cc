#include "jnif.h"
#include <erl_driver.h>

static jclass NIF_class;
static jmethodID m_NIF__get_erts_version;
static jmethodID m_NIF__get_otp_release;
static jmethodID m_NIF__get_num_async_threads;
static jmethodID m_NIF__get_num_scheduler_threads;

static JavaVM *jvm;

void enif_system_info(ErlNifSysInfo* info, size_t si_size)
{
  if (sizeof(*info) >= si_size) {
    memset(info, 0, si_size);
    info->driver_major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
    info->driver_minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;
    return;
  }

  JNIEnv *je;
  if (jvm->AttachCurrentThreadAsDaemon((void**)&je, NULL) == JNI_OK) {

    info->driver_major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
    info->driver_minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;

    jstring erts_version = (jstring)
      je->CallStaticObjectMethod(NIF_class, m_NIF__get_erts_version);

    if (erts_version == NULL) {
      info->erts_version = NULL;
    } else {
      info->erts_version = strdup(je->GetStringUTFChars(erts_version, NULL));
    }

    jstring otp_release = (jstring)
      je->CallStaticObjectMethod(NIF_class, m_NIF__get_otp_release);

    if (otp_release == NULL) {
      info->otp_release = NULL;
    } else {
      info->otp_release = strdup(je->GetStringUTFChars(otp_release, NULL));
    }

    info->thread_support = 1;
    info->smp_support = 1;

    info->async_threads =
      je->CallStaticIntMethod(NIF_class, m_NIF__get_num_async_threads);
    info->scheduler_threads =
      je->CallStaticIntMethod(NIF_class, m_NIF__get_num_scheduler_threads);

    info->nif_major_version = ERL_NIF_MAJOR_VERSION;
    info->nif_major_version = ERL_NIF_MINOR_VERSION;
  }
}


void initialize_jnif_sys(JavaVM* vm, JNIEnv *je)
{
  jvm = vm;

  NIF_class       = je->FindClass("erjang/NIF");
  NIF_class      = (jclass) je->NewGlobalRef(NIF_class);

  m_NIF__get_erts_version = je->GetStaticMethodID(NIF_class,
                                                   "get_erts_version",
                                                   "()Ljava/lang/String;");

  m_NIF__get_otp_release = je->GetStaticMethodID(NIF_class,
                                                  "get_otp_release",
                                                  "()Ljava/lang/String;");

  m_NIF__get_num_async_threads = je->GetStaticMethodID(NIF_class,
                                                        "get_num_async_threads",
                                                        "()I");

  m_NIF__get_num_scheduler_threads = je->GetStaticMethodID(NIF_class,
                                                        "get_num_scheduler_threads",
                                                        "()I");
}
