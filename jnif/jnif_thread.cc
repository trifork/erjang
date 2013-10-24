
#include "jnif.h"

#include <pthread.h>

#ifdef __APPLE_CC__
struct init_arg {
  const char *name;
  void *arg;
  void * (*fun)(void *);
};

//
// we use a separate start routine to be able to set the thread name on macosx
//
static void* jnif_macos_start_routine(void* a)
{
  init_arg *init = (init_arg*)a;
  if (init->name != NULL) {
    pthread_setname_np(init->name);
  }

  void *arg = init->arg;
  void * (*fun)(void *) = init->fun;

  // free it, now we've taken the values
  delete init;

  // now go!
  return (*fun)(arg);
}

#endif

int enif_equal_tids(ErlNifTid tid1, ErlNifTid tid2)
{
  return pthread_equal((pthread_t)tid1, (pthread_t)tid2) ? NIF_TRUE : NIF_FALSE;
}

int enif_thread_create(char *name,
                       ErlNifTid *tid,
                       void * (*start_routine)(void *),
                       void *args,
                       ErlNifThreadOpts *opts)
{
  pthread_attr_t attr;

  pthread_attr_init(&attr);

  if (opts != NULL) {
    pthread_attr_setstacksize(&attr, opts->suggested_stack_size);
  }

#ifdef __APPLE_CC__
  init_arg *thread_init = new init_arg();
  thread_init->name = name;
  thread_init->arg  = args;
  thread_init->fun  = start_routine;
#endif

  pthread_t thread;
  int err = pthread_create(&thread,
                           &attr,
#ifdef __APPLE_CC__
                           &jnif_macos_start_routine,
                           thread_init
#else
                           start_routine,
                           args
#endif
                           );

  if (err == 0 && tid != NULL) {
    *tid = (ErlNifTid)thread;

#ifndef __APPLE_CC__

    if (name != NULL) {
#ifdef __OpenBSD__
      pthread_set_name_np(thread, name);
#endif
#ifdef LINUX
      pthread_setname_np(thread, name);
#endif
    }

#endif
  }

  pthread_attr_destroy(&attr);

  return err;
}

void enif_thread_exit(void *resp)
{
  pthread_exit(resp);
}

int enif_thread_join(ErlNifTid tid, void **respp)
{
  return pthread_join((pthread_t)tid, respp);
}

ErlNifThreadOpts *enif_thread_opts_create(char *name)
{
  return new ErlNifThreadOpts();
}

void enif_thread_opts_destroy(ErlNifThreadOpts *opts)
{
  delete opts;
}

ErlNifTid enif_thread_self(void)
{
  return (ErlNifTid) pthread_self();
}

int enif_tsd_key_create(char *name, ErlNifTSDKey *key)
{
  int err = pthread_key_create((pthread_key_t*)key, NULL);
  return err;
}

void enif_tsd_key_destroy(ErlNifTSDKey key)
{
  pthread_key_delete((pthread_key_t) key);
}

void *enif_tsd_get(ErlNifTSDKey key)
{
  return pthread_getspecific((pthread_key_t)key);
}

void enif_tsd_set(ErlNifTSDKey key, void *data)
{
  pthread_setspecific((pthread_key_t)key, data);
}

