#include <pthread.h>

static pthread_mutex_t llgo_gcroot_registry = PTHREAD_MUTEX_INITIALIZER;

void llgo_gcroot_lock(void) {
  if (pthread_mutex_lock(&llgo_gcroot_registry) != 0)
    __builtin_trap();
}

void llgo_gcroot_unlock(void) {
  if (pthread_mutex_unlock(&llgo_gcroot_registry) != 0)
    __builtin_trap();
}
