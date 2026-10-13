/* An always-ready GLib source reproduces a WebKit run-loop wakeup storm.
   Load only in an isolated graphical Emacs, never the user's daemon. */
#include <emacs-module.h>
#include <glib.h>
#include <signal.h>

int plugin_is_GPL_compatible;
static guint source_id;
static unsigned long dispatches;

static gboolean ready (gpointer unused)
{
  (void) unused;
  ++dispatches;
  return G_SOURCE_CONTINUE;
}

static emacs_value start (emacs_env *env, ptrdiff_t nargs,
                          emacs_value *args, void *data)
{
  (void) nargs; (void) args; (void) data;
  source_id = g_idle_add_full (G_PRIORITY_DEFAULT, ready, NULL, NULL);
  return env->intern (env, "t");
}

static emacs_value stop (emacs_env *env, ptrdiff_t nargs,
                         emacs_value *args, void *data)
{
  (void) nargs; (void) args; (void) data;
  if (source_id) g_source_remove (source_id);
  source_id = 0;
  return env->make_integer (env, dispatches);
}

static emacs_value fail (emacs_env *env, ptrdiff_t nargs,
                         emacs_value *args, void *data)
{
  (void) nargs; (void) args; (void) data;
  /* Fatal cleanup sees pending GUI input, as in the captured live stack. */
  raise (SIGIO);
  raise (SIGABRT);
  return env->intern (env, "nil");
}

int emacs_module_init (struct emacs_runtime *runtime)
{
  emacs_env *env = runtime->get_environment (runtime);
  emacs_value args[2];
  const char *names[] = {"glib-test-start", "glib-test-stop", "glib-test-fail"};
  emacs_value (*functions[]) (emacs_env *, ptrdiff_t, emacs_value *, void *)
    = {start, stop, fail};
  for (unsigned i = 0; i < 3; ++i)
    {
      args[0] = env->intern (env, names[i]);
      args[1] = env->make_function (env, 0, 0, functions[i], "Test fixture.", NULL);
      env->funcall (env, env->intern (env, "fset"), 2, args);
    }
  return 0;
}
