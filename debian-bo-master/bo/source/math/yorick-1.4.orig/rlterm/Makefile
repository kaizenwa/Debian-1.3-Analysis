# replace this with the directory containing libreadline.a
#   or blank if libreadline.a is visible to your compiler
LIBDIR = -L/home/munro/readline-2.0

# The following switches may be necessary in order for rlterm.c to
# find termio and signal headers on your system:
#   -DUSE_TERMIOS      to use termios.h (otherwise termio.h)
#   -DUSE_SYS_TERMIO   to use <sys/termio.h> or <sys/termios.h>
#   -DPOSIX_TERMIO     to use tc.etattr instead of ioctl (implies USE_TERMIOS)
#   -DNO_SIGACTION     if no sigaction or it doesn't take SA_NOCLDSTOP flag
RLDEFS =

LIBS = -lreadline -ltermcap
ALLFLAGS = $(CFLAGS) $(LDFLAGS) $(DEFS) $(RLDEFS)

rlterm: rlterm.c
	$(CC) $(ALLFLAGS) rlterm.c ${LIBDIR} ${LIBS} -o rlterm

clean::
	rm -f rlterm *~
