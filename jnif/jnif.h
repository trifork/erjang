
#ifndef __JNFI_H
#define __JNIF_H

#include <jni.h>
#include <list>
#include <assert.h>

extern "C" {
#include "erl_nif.h"
};

#define JVM_NULL ((jobject)NULL)
#define NIF_TRUE 1
#define NIF_FALSE 0
#define THE_BADARG (reinterpret_cast<ERL_NIF_TERM>(0xffffffffUL))

struct jnif_module {
  void *so_handle;
  ErlNifEntry *entry;
  void *priv_data;
  ErlNifEnv *global;
};

struct jnif_bin_data {
  enum { SHOULD_RELEASE, SHOULD_FREE, IS_FREE } type;
  JNIEnv     *je;
  jbyteArray  array;
  jbyte      *elements;
};

struct enif_environment_t {
  JNIEnv *je;
  enum { ALLOC, STACK } type;

  // globals to DeleteGlobalRef
  std::list< jobject > globals;

  // binaries to ReleaseByteArrayElements
  std::list< jnif_bin_data * > binaries;

  struct jnif_module *module;
};

inline ERL_NIF_TERM J2E(jobject o) {
  assert (o != NULL);
  return reinterpret_cast<ERL_NIF_TERM>(o);
}

inline jobject E2J(ERL_NIF_TERM o) {
  assert (o != 0);
  return reinterpret_cast<jobject>(o);
}

// jnif_env.cc
void initialize_jnif_env(JavaVM* vm, JNIEnv *je);
ERL_NIF_TERM jnif_retain(ErlNifEnv* ee, jobject obj);
ERL_NIF_TERM jnif_retain(ErlNifEnv* ee, ERL_NIF_TERM term);
void jnif_init_env( ErlNifEnv * ee, JNIEnv *je, struct jnif_module *mod );
void jnif_release_env( ErlNifEnv * ee);

// jnif_binary.cc
void initialize_jnif_binary(JavaVM* vm, JNIEnv *e);
void uninitialize_jnif_binary(JavaVM* vm, JNIEnv* je);
void jnif_release_binary(struct jnif_bin_data *bd);

// jnif_resource.cc
void initialize_jnif_resource(JavaVM* vm, JNIEnv *je);

// jnif_string.cc
void initialize_jnif_string(JavaVM* vm, JNIEnv *je);

// jnif_number.cc
void initialize_jnif_number(JavaVM* vm, JNIEnv *je);

// jnif_atom.cc
void initialize_jnif_atom(JavaVM* vm, JNIEnv *je);

// jnif_tuple.cc
void initialize_jnif_tuple(JavaVM* vm, JNIEnv *je);

#endif
