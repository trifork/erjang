
#include "jnif.h"
#include <assert.h>

static JavaVM *jvm;

void initialize_jnif_env(JavaVM* vm, JNIEnv *e)
{
  jvm = vm;
}

struct global_dtor : jnif_dtor {
  jobject global;
  global_dtor(jobject g) : global(g) {}
  void release(ErlNifEnv* ee) {
    ee->je->DeleteGlobalRef( global );
  }
};


ERL_NIF_TERM
jnif_retain(ErlNifEnv* ee, jobject obj)
{
  if (ee->type == enif_environment_t::ALLOC) {
    jobject global = ee->je->NewGlobalRef(obj);
    ee->dtors.push_back (new global_dtor(global));
    //    ee->globals.push_back(global);
    return J2E(global);
  } else if (ee->type == enif_environment_t::STACK) {
    return J2E(obj);
  } else {
    abort();
  }
}

ERL_NIF_TERM
jnif_retain(ErlNifEnv* ee, ERL_NIF_TERM term)
{
  return jnif_retain(ee, E2J(term));
}


void
jnif_init_env( ErlNifEnv * ee, JNIEnv *je, struct jnif_module *mod, enif_environment_t::TYPE type )
{
  assert (je != NULL);
  ee->je = je;
  ee->module = mod;
  ee->type = type;
}

void *enif_priv_data(ErlNifEnv *ee)
{
  if (ee->module != NULL) {
    return ee->module->priv_data;
  }

  return NULL;
}


void jnif_commit_env( ErlNifEnv *ee)
{
  for (std::list<jnif_dtor*>::iterator it = ee->commits.begin();
       it != ee->commits.end();
       it++ )
    {
      jnif_dtor *dtor = *it;
      dtor->release( ee );
      delete dtor;
    }

  ee->commits.clear();

}

void
jnif_release_env( ErlNifEnv * ee)
{
  jnif_commit_env( ee );

  for (std::list<jnif_dtor*>::iterator it = ee->dtors.begin();
       it != ee->dtors.end();
       it++ )
    {
      jnif_dtor *dtor = *it;
      dtor->release( ee );
      delete dtor;
    }

  ee->dtors.clear();

}

ErlNifEnv *
enif_alloc_env()
{
  JNIEnv *je;
  jvm->AttachCurrentThreadAsDaemon((void**)&je, NULL);

  enif_environment_t *ee = new enif_environment_t();
  jnif_init_env( ee, je, NULL, enif_environment_t::ALLOC );
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
  jnif_init_env(ee,je,ee->module,ee->type);
}

ERL_NIF_TERM enif_make_copy(ErlNifEnv* ee, ERL_NIF_TERM src_term)
{
  if (src_term == THE_BADARG) return THE_BADARG;
  return jnif_retain(ee, src_term);
}
