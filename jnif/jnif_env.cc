
#include "jnif.h"
#include <assert.h>

static JavaVM *jvm;

void initialize_jnif_env(JavaVM* vm, JNIEnv *e)
{
  jvm = vm;
}


ERL_NIF_TERM
jnif_retain(ErlNifEnv* ee, jobject obj)
{
  if (ee->type == enif_environment_t::ALLOC) {
    jobject global = ee->je->NewGlobalRef(obj);
    ee->globals.push_back(global);
    return J2E(global);
  } else {
    return J2E(obj);
  }
}

ERL_NIF_TERM
jnif_retain(ErlNifEnv* ee, ERL_NIF_TERM term)
{
  return jnif_retain(ee, E2J(term));
}


void
jnif_init_env( ErlNifEnv * ee, JNIEnv *je, struct jnif_module *mod )
{
  assert (je != NULL);
  ee->je = je;
  ee->module = mod;
}

void *enif_priv_data(ErlNifEnv *ee)
{
  if (ee->module != NULL) {
    return ee->module->priv_data;
  }

  return NULL;
}


void
jnif_release_env( ErlNifEnv * ee)
{
  // release any jnif_retained byte array elements
  for (std::list<jnif_bin_data*>::iterator it = ee->binaries.begin();
       it != ee->binaries.end();
       it++ )
    {
      jnif_release_binary(*it);
    }

  ee->binaries.clear();

  for (std::list<jobject>::iterator it = ee->globals.begin();
       it != ee->globals.end();
       it++ )
    {
      ee->je->DeleteGlobalRef( *it );
    }

  ee->globals.clear();

}

ErlNifEnv *
enif_alloc_env()
{
  JNIEnv *je;
  jvm->AttachCurrentThreadAsDaemon((void**)&je, NULL);

  enif_environment_t *ee = new enif_environment_t();
  ee->type = enif_environment_t::ALLOC;
  jnif_init_env( ee, je, NULL );
  return ee;
}

void enif_free_env(ErlNifEnv* ee)
{
  jnif_release_env(ee);
  if (ee->type == enif_environment_t::ALLOC) {
    delete ee;
  }
}

void enif_clear_env(ErlNifEnv* ee)
{
  JNIEnv *je = ee->je;
  jnif_release_env(ee);
  jnif_init_env(ee,je,ee->module);
}

ERL_NIF_TERM enif_make_copy(ErlNifEnv* ee, ERL_NIF_TERM src_term)
{
  if (src_term == THE_BADARG) return THE_BADARG;
  return jnif_retain(ee, src_term);
}
