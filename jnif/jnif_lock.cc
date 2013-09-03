
#include "jnif.h"
#include <pthread.h>

// bare-bones mutex/rwlock/condition based on pthreads

struct ErlDrvMutex_ {
  pthread_mutex_t mux;
  const char* name;
  ErlDrvMutex_(const char *name) {
    this->name = name;
    pthread_mutex_init(&mux, 0);
  }

  ~ErlDrvMutex_() { pthread_mutex_destroy(&mux); }

  void lock() {
    pthread_mutex_lock(&mux);
  }
  void unlock() {
    pthread_mutex_unlock(&mux);
  }
  bool trylock() {
    return pthread_mutex_trylock(&mux) == 0;
  }
};

struct ErlDrvCond_ {
  const char *name;
  pthread_cond_t cv;
  ErlDrvCond_(const char *name) {
    this->name = name;
    pthread_cond_init(&cv, NULL);
  }

  ~ErlDrvCond_() {
    pthread_cond_destroy(&cv);
  }

  void signal() {
    pthread_cond_signal(&cv);
  }

  void broadcast() {
    pthread_cond_broadcast(&cv);
  }

  void wait(ErlNifMutex *mtx) {
    pthread_cond_wait(&cv, &mtx->mux);
  }
};

ErlNifCond* enif_cond_create (char *name)
{
  return new ErlDrvCond_(name);
}

void enif_cond_destroy (ErlNifCond *cnd)
{
  delete cnd;
}

void enif_cond_signal (ErlNifCond *cnd)
{
  cnd->signal();
}

void enif_cond_broadcast (ErlNifCond *cnd)
{
  cnd->broadcast();
}

void enif_cond_wait (ErlNifCond *cnd, ErlNifMutex *mtx)
{
  cnd->wait( mtx );
}


struct ErlDrvRWLock_ {
  pthread_rwlock_t rwlock;
  const char *name;
  ErlDrvRWLock_(const char* name) {
    this->name = name;
    pthread_rwlock_init(&rwlock, 0);
  }
  ~ErlDrvRWLock_() { }

  void rlock() {
    pthread_rwlock_rdlock(&rwlock);
  }

  void wlock() {
    pthread_rwlock_wrlock(&rwlock);
  }

  void unlock() {
    pthread_rwlock_unlock(&rwlock);
  }

  bool tryrlock() {
    return pthread_rwlock_tryrdlock(&rwlock) == 0;
  }

  bool trywlock() {
    return pthread_rwlock_tryrdlock(&rwlock) == 0;
  }

};



ErlNifMutex* enif_mutex_create (char *name)
{
  return new ErlNifMutex(name);
}

void enif_mutex_destroy (ErlNifMutex *mtx)
{
  delete mtx;
}

int enif_mutex_trylock (ErlNifMutex *mtx)
{
  return mtx->trylock() ? NIF_TRUE : NIF_FALSE;
}

void enif_mutex_lock (ErlNifMutex *mtx) {
  mtx->lock();
}

void enif_mutex_unlock (ErlNifMutex *mtx)
{
  mtx->unlock();
}

ErlNifRWLock* enif_rwlock_create (char *name)
{
  return new ErlNifRWLock(name);
}

void enif_rwlock_destroy (ErlNifRWLock *rwlck)
{
  delete rwlck;
}

void enif_rwlock_rlock (ErlNifRWLock *rwlck)
{
  rwlck->rlock();
}

void enif_rwlock_runlock (ErlNifRWLock *rwlck)
{
  rwlck->unlock();
}

int enif_rwlock_tryrlock (ErlNifRWLock *rwlck)
{
  return rwlck->tryrlock() ? NIF_TRUE : NIF_FALSE;
}

int enif_rwlock_tryrwlock (ErlNifRWLock *rwlck)
{
  return rwlck->trywlock() ? NIF_TRUE : NIF_FALSE;
}

void enif_rwlock_rwlock (ErlNifRWLock *rwlck)
{
  rwlck->wlock();
}

void enif_rwlock_rwunlock (ErlNifRWLock *rwlck)
{
  rwlck->unlock();
}


