/* -*- c-basic-offset: 2 -*- */

#include "jnif.h"

#include <dlfcn.h>
#include <cstring>

typedef ErlNifEntry* (*nif_init_t)();

void* enif_dlopen(const char* lib,
		  void (*err_handler)(void*,const char*), void* err_arg)
{
  char buffer[strlen(lib)+4];
  void *so_handle;

  sprintf(buffer, "%s.so", lib);

  so_handle = dlopen(buffer, RTLD_NOW);
  if (so_handle == NULL) {
      const char *err = dlerror();
      fprintf(stderr, "could not load %s (error: %s)\n", buffer, err);

    if (err_handler != NULL) {
      (*err_handler) (err_arg, err);
    }
    return NULL;
  }

  dlerror(); // Clear error flag
  void *address =  dlsym( so_handle, "nif_init" );
  if (address == NULL) {
      const char *err = dlerror();
      fprintf(stderr, "could not find nif_init in %s (error: %s)\n", buffer, err);

    if (err_handler != NULL) {
      (*err_handler) (err_arg, err);
    }
    dlclose(so_handle);
    return NULL;
  }

  nif_init_t nif_init_fp = reinterpret_cast<nif_init_t>(address);
  ErlNifEntry *entry = (*nif_init_fp)();

  return so_handle;
}


void* enif_dlsym(void* so_handle, const char* symbol,
		 void (*err_handler)(void*,const char*), void* err_arg)
{
    void *address =  dlsym( so_handle, symbol );
    if (address == NULL) {
      if (err_handler != NULL) {
        const char *err = dlerror();
        (*err_handler) (err_arg, err);
      }
      return NULL;
    }

    return address;
}
