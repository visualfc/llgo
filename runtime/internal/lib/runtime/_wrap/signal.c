#define _POSIX_C_SOURCE 200809L
#if defined(__APPLE__)
#define _DARWIN_C_SOURCE 1
#endif

#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <sched.h>
#include <signal.h>
#include <stddef.h>
#include <string.h>
#include <unistd.h>

_Static_assert(SIGPIPE == 13, "runtime signalPipe assumes SIGPIPE is 13");

static int llgo_signal_pipe[2] = {-1, -1};
static unsigned int llgo_signal_delivering;

static void llgo_signal_handler(int signum)
{
    int saved_errno = errno;

    /* signalWaitUntilIdle uses this like the Go runtime's sig.delivering:
     * a pipe marker must not overtake a handler that has already entered. */
    __atomic_add_fetch(&llgo_signal_delivering, 1, __ATOMIC_ACQUIRE);
    if (llgo_signal_pipe[1] >= 0) {
        ssize_t ignored = write(llgo_signal_pipe[1], &signum, sizeof(signum));
        (void)ignored;
    }
    __atomic_sub_fetch(&llgo_signal_delivering, 1, __ATOMIC_RELEASE);
    errno = saved_errno;
}

int llgo_signal_init(void)
{
    int flags;

    if (llgo_signal_pipe[0] >= 0)
        return llgo_signal_pipe[0];
    if (pipe(llgo_signal_pipe) != 0)
        return -errno;

    flags = fcntl(llgo_signal_pipe[1], F_GETFL, 0);
    if (flags < 0 || fcntl(llgo_signal_pipe[1], F_SETFL,
                           flags | O_NONBLOCK) != 0)
        goto fail;
    if (fcntl(llgo_signal_pipe[0], F_SETFD, FD_CLOEXEC) != 0 ||
        fcntl(llgo_signal_pipe[1], F_SETFD, FD_CLOEXEC) != 0)
        goto fail;
    return llgo_signal_pipe[0];

fail:
    flags = errno;
    close(llgo_signal_pipe[0]);
    close(llgo_signal_pipe[1]);
    llgo_signal_pipe[0] = -1;
    llgo_signal_pipe[1] = -1;
    return -flags;
}

int llgo_signal_enable(int signum)
{
    struct sigaction action;

    memset(&action, 0, sizeof(action));
    action.sa_handler = llgo_signal_handler;
    sigemptyset(&action.sa_mask);
    action.sa_flags = SA_RESTART;
    return sigaction(signum, &action, 0);
}

int llgo_signal_disable(int signum)
{
    struct sigaction action;

    memset(&action, 0, sizeof(action));
    action.sa_handler = SIG_DFL;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;
    return sigaction(signum, &action, 0);
}

int llgo_signal_ignore(int signum)
{
    struct sigaction action;

    memset(&action, 0, sizeof(action));
    action.sa_handler = SIG_IGN;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;
    return sigaction(signum, &action, 0);
}

int llgo_signal_ignore_pipe(void)
{
    struct sigaction action;
    struct sigaction previous;

    memset(&action, 0, sizeof(action));
    action.sa_handler = SIG_IGN;
    sigemptyset(&action.sa_mask);
    if (sigaction(SIGPIPE, &action, &previous) != 0)
        return -errno;
    return previous.sa_handler == SIG_IGN;
}

int llgo_signal_raise_pipe(void)
{
    return raise(SIGPIPE);
}

int llgo_signal_die_pipe(void)
{
    struct sigaction action;
    sigset_t mask;
    int code;

    memset(&action, 0, sizeof(action));
    action.sa_handler = SIG_DFL;
    sigemptyset(&action.sa_mask);
    if (sigaction(SIGPIPE, &action, 0) != 0)
        return errno;

    sigemptyset(&mask);
    sigaddset(&mask, SIGPIPE);
    code = pthread_sigmask(SIG_UNBLOCK, &mask, 0);
    if (code != 0)
        return code;
    if (raise(SIGPIPE) != 0)
        return errno;
    return EIO;
}

/* Place a marker after every signal already written to the pipe. The write
 * end must stay nonblocking for the signal handler, so an ordinary runtime
 * thread retries while the reader makes room. */
int llgo_signal_barrier(void)
{
    const int marker = 0;

    while (__atomic_load_n(&llgo_signal_delivering, __ATOMIC_ACQUIRE) != 0)
        sched_yield();
    for (;;) {
        ssize_t count = write(llgo_signal_pipe[1], &marker, sizeof(marker));
        if (count == (ssize_t)sizeof(marker))
            return 0;
        if (count < 0 && errno == EINTR)
            continue;
        if (count < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
            sched_yield();
            continue;
        }
        return count < 0 ? errno : EIO;
    }
}

int llgo_signal_recv(int fd, int *signum)
{
    unsigned char *out = (unsigned char *)signum;
    size_t offset = 0;

    if (signum == 0)
        return EINVAL;
    while (offset < sizeof(*signum)) {
        ssize_t count = read(fd, out + offset, sizeof(*signum) - offset);
        if (count > 0) {
            offset += (size_t)count;
            continue;
        }
        if (count < 0 && errno == EINTR)
            continue;
        return count == 0 ? EPIPE : errno;
    }
    return 0;
}
