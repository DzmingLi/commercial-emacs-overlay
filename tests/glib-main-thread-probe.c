/* Run GUI stress tests with this preload library in an isolated Emacs.
   Stop before dispatching an unsafe callback, without generating a core. */
#define _GNU_SOURCE
#include <dlfcn.h>
#include <execinfo.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static pthread_t main_thread;
static unsigned long dispatches;
static int active;
static pthread_once_t callbacks_once = PTHREAD_ONCE_INIT;
static void (*dispatch) (void *);
static void *(*default_context) (void);

static void resolve_callbacks (void)
{
  dispatch = dlsym (RTLD_NEXT, "g_main_context_dispatch");
  default_context = dlsym (RTLD_NEXT, "g_main_context_default");
  if (!dispatch || !default_context) _exit (77);
}

static void report (void)
{
  if (active)
    fprintf (stderr, "glib-main-thread-probe: %lu main-thread dispatches\n",
             dispatches);
}

__attribute__((constructor)) static void initialize (void)
{
  main_thread = pthread_self ();
  char executable[4096];
  ssize_t length = readlink ("/proc/self/exe", executable, sizeof executable - 1);
  if (length > 0)
    {
      executable[length] = '\0';
      const char *name = strrchr (executable, '/');
      active = strstr (name ? name + 1 : executable, "emacs") != NULL;
    }
  atexit (report);
}

void g_main_context_dispatch (void *context)
{
  pthread_once (&callbacks_once, resolve_callbacks);
  /* WebKit owns private GLib contexts on helper threads. Only the default
     GTK context is restricted to the Emacs GUI thread. */
  int gui_context = active && context == default_context ();
  if (gui_context && !pthread_equal (main_thread, pthread_self ()))
    {
      const char message[] = "glib-main-thread-probe: WORKER DISPATCH\n";
      write (STDERR_FILENO, message, sizeof message - 1);
      void *stack[24];
      int count = backtrace (stack, 24);
      backtrace_symbols_fd (stack, count, STDERR_FILENO);
      _exit (76);
    }
  if (gui_context && ++dispatches == 1)
    /* Emacs can use _exit, so report evidence before its shutdown path. */
    fprintf (stderr, "glib-main-thread-probe: 1 main-thread dispatches\n");
  dispatch (context);
}
