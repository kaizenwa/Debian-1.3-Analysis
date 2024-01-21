dnl Check for libreadline of proper version
AC_DEFUN(READLINE_CHECK,
[AC_MSG_CHECKING(for readline)
AC_CACHE_VAL(lftp_cv_precompiled_readline,
[
   old_LIBS="$LIBS"
   LIBS="-lreadline $LIBS"
   AC_TRY_LINK([extern int (*rl_getc_function)();],
      [rl_getc_function=0;],
      lftp_cv_precompiled_readline=yes,
      lftp_cv_precompiled_readline=no)
   LIBS="$old_LIBS"
])
if test $lftp_cv_precompiled_readline = yes; then
   AC_MSG_RESULT(yes)
   LIBS="-lreadline $LIBS"
   READLINE_DEPEND=""
else
   AC_MSG_RESULT(no - will compile own)
   READLINE_DEPEND='$(READLINE)'
fi
AC_SUBST(READLINE_DEPEND)
])

dnl Check type of signal routines (posix, 4.2bsd, 4.1bsd or v7)
AC_DEFUN(BASH_SIGNAL_CHECK,
[AC_REQUIRE([AC_TYPE_SIGNAL])
AC_MSG_CHECKING(for type of signal functions)
AC_CACHE_VAL(bash_cv_signal_vintage,
[
  AC_TRY_LINK([#include <signal.h>],[
    sigset_t ss;
    struct sigaction sa;
    sigemptyset(&ss); sigsuspend(&ss);
    sigaction(SIGINT, &sa, (struct sigaction *) 0);
    sigprocmask(SIG_BLOCK, &ss, (sigset_t *) 0);
  ], bash_cv_signal_vintage=posix,
  [
    AC_TRY_LINK([#include <signal.h>], [
	int mask = sigmask(SIGINT);
	sigsetmask(mask); sigblock(mask); sigpause(mask);
    ], bash_cv_signal_vintage=4.2bsd,
    [
      AC_TRY_LINK([
	#include <signal.h>
	RETSIGTYPE foo() { }], [
		int mask = sigmask(SIGINT);
		sigset(SIGINT, foo); sigrelse(SIGINT);
		sighold(SIGINT); sigpause(SIGINT);
        ], bash_cv_signal_vintage=svr3, bash_cv_signal_vintage=v7
    )]
  )]
)
])
AC_MSG_RESULT($bash_cv_signal_vintage)
if test "$bash_cv_signal_vintage" = posix; then
AC_DEFINE(HAVE_POSIX_SIGNALS)
elif test "$bash_cv_signal_vintage" = "4.2bsd"; then
AC_DEFINE(HAVE_BSD_SIGNALS)
elif test "$bash_cv_signal_vintage" = svr3; then
AC_DEFINE(HAVE_USG_SIGHOLD)
fi
])
