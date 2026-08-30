#define _POSIX_C_SOURCE 200809L
#if defined(__APPLE__)
#define _DARWIN_C_SOURCE 1
#endif

#include <pthread.h>
#include <stdint.h>
#include <time.h>

#define LLGO_TIMER_MAX_WAIT_NANOS INT64_C(86400000000000)

int llgo_timer_cond_init(pthread_cond_t *condition)
{
#if defined(__APPLE__)
    return pthread_cond_init(condition, 0);
#else
    pthread_condattr_t attributes;
    int result = pthread_condattr_init(&attributes);

    if (result != 0)
        return result;
    result = pthread_condattr_setclock(&attributes, CLOCK_MONOTONIC);
    if (result == 0)
        result = pthread_cond_init(condition, &attributes);
    pthread_condattr_destroy(&attributes);
    return result;
#endif
}

int llgo_timer_cond_timedwait(pthread_cond_t *condition,
                              pthread_mutex_t *mutex,
                              int64_t wait_nanos)
{
    struct timespec deadline;

    if (wait_nanos < 0)
        wait_nanos = 0;
    else if (wait_nanos > LLGO_TIMER_MAX_WAIT_NANOS)
        wait_nanos = LLGO_TIMER_MAX_WAIT_NANOS;
#if defined(__APPLE__)
    deadline.tv_sec = (time_t)(wait_nanos / 1000000000LL);
    deadline.tv_nsec = (long)(wait_nanos % 1000000000LL);
    return pthread_cond_timedwait_relative_np(condition, mutex, &deadline);
#else
    if (clock_gettime(CLOCK_MONOTONIC, &deadline) != 0)
        return -1;
    deadline.tv_sec += (time_t)(wait_nanos / 1000000000LL);
    deadline.tv_nsec += (long)(wait_nanos % 1000000000LL);
    if (deadline.tv_nsec >= 1000000000L) {
        deadline.tv_sec++;
        deadline.tv_nsec -= 1000000000L;
    }
    return pthread_cond_timedwait(condition, mutex, &deadline);
#endif
}
