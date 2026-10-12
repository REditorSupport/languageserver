#include "stdin.h"
#include <errno.h>
#include <math.h>
#include <limits.h>
#include <string.h>

#if defined(_WIN32) || defined(_WIN64)
#include <windows.h>
#else
#include <unistd.h>
#include <poll.h>
#endif

static SEXP read_eof(void) {
    SEXP out = PROTECT(Rf_allocVector(RAWSXP, 0));
    SEXP eof = PROTECT(Rf_ScalarLogical(TRUE));
    Rf_setAttrib(out, Rf_install("eof"), eof);
    UNPROTECT(2);
    return out;
}

static int wait_milliseconds(SEXP timeout) {
    double value = Rf_asReal(timeout);
    if (!R_FINITE(value) || value < 0 || value > INT_MAX)
        Rf_error("stdin wait timeout must be finite nonnegative milliseconds");
    return (int) ceil(value);
}

#if defined(_WIN32) || defined(_WIN64)
static int pipe_available(HANDLE input, DWORD *bytes) {
    if (PeekNamedPipe(input, NULL, 0, NULL, bytes, NULL)) return 1;
    DWORD error = GetLastError();
    if (error == ERROR_BROKEN_PIPE || error == ERROR_PIPE_NOT_CONNECTED ||
            error == ERROR_INVALID_HANDLE) return 0;
    Rf_error("cannot inspect stdin pipe (Windows error %lu)", (unsigned long) error);
    return 0;
}
#endif

SEXP stdin_wait(SEXP timeout) {
    int milliseconds = wait_milliseconds(timeout);
#if defined(_WIN32) || defined(_WIN64)
    HANDLE input = GetStdHandle(STD_INPUT_HANDLE);
    ULONGLONG started = GetTickCount64();
    for (;;) {
        DWORD available = 0;
        if (!pipe_available(input, &available) || available) return Rf_ScalarLogical(TRUE);
        ULONGLONG elapsed = GetTickCount64() - started;
        if (elapsed >= (ULONGLONG) milliseconds) return Rf_ScalarLogical(FALSE);
        DWORD remaining = milliseconds - (DWORD) elapsed;
        Sleep(remaining < 10 ? remaining : 10);
        R_CheckUserInterrupt();
    }
#else
    struct pollfd fd;
    fd.fd = STDIN_FILENO;
    fd.events = POLLIN;
    fd.revents = 0;
    int status;
    do {
        status = poll(&fd, 1, milliseconds);
        if (status < 0 && errno == EINTR) R_CheckUserInterrupt();
    } while (status < 0 && errno == EINTR);
    if (status < 0) Rf_error("cannot wait for stdin: %s", strerror(errno));
    return Rf_ScalarLogical(status > 0);
#endif
}

SEXP stdin_read_raw(SEXP count) {
    int requested = Rf_asInteger(count);
    if (requested == NA_INTEGER || requested < 1 || requested > 65536)
        Rf_error("stdin raw read size must be between 1 and 65536 bytes");
    unsigned char data[65536];
    size_t received;
#if defined(_WIN32) || defined(_WIN64)
    HANDLE input = GetStdHandle(STD_INPUT_HANDLE);
    DWORD available = 0, bytes_read = 0;
    if (!pipe_available(input, &available)) return read_eof();
    if (!available) return Rf_allocVector(RAWSXP, 0);
    DWORD n = available < (DWORD) requested ? available : (DWORD) requested;
    if (!ReadFile(input, data, n, &bytes_read, NULL)) {
        DWORD error = GetLastError();
        if (error == ERROR_BROKEN_PIPE || error == ERROR_PIPE_NOT_CONNECTED)
            return read_eof();
        Rf_error("cannot read stdin (Windows error %lu)", (unsigned long) error);
    }
    if (!bytes_read) return read_eof();
    received = bytes_read;
#else
    struct pollfd fd;
    fd.fd = STDIN_FILENO;
    fd.events = POLLIN;
    fd.revents = 0;
    int ready;
    do {
        ready = poll(&fd, 1, 0);
        if (ready < 0 && errno == EINTR) R_CheckUserInterrupt();
    } while (ready < 0 && errno == EINTR);
    if (ready < 0) Rf_error("cannot inspect stdin: %s", strerror(errno));
    if (!ready) return Rf_allocVector(RAWSXP, 0);
    ssize_t bytes_read;
    do {
        bytes_read = read(STDIN_FILENO, data, requested);
        if (bytes_read < 0 && errno == EINTR) R_CheckUserInterrupt();
    } while (bytes_read < 0 && errno == EINTR);
    if (!bytes_read) return read_eof();
    if (bytes_read < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) return Rf_allocVector(RAWSXP, 0);
        Rf_error("cannot read stdin: %s", strerror(errno));
    }
    received = (size_t) bytes_read;
#endif
    SEXP result = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t) received));
    memcpy(RAW(result), data, received);
    UNPROTECT(1);
    return result;
}
