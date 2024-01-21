# Note: Each line beginning with `##' are reserved.

# Note for `##-conf' lines: They are displayed (without ##-conf.?) with
# ./MakeMakefile --help
##-conf Options for sunos 4:
##-conf.gccchecker: gccchecker.o
##-conf.gccchecker.so: gccchecker.so.1
##-conf.default: gccchecker

# Common part
ifcommon
@decl
### makefile fragment for sunos 4
RANLIB = ranlib
LDFLAGS_FOR_CHKR = -Bstatic -r
SYMCTL_C = $(srcdir)/utils/symctl_aout.c
C_OBJS=begin.o gnu.a.out.o chkr-string.o generic.o syscalls.o \
  startup.o parse-args.o error.o smalloc.o signal.o find-exec.o fd.o \
  time.o message.o version.o vprintf.o getenv.o mktemp.o qsort.o \
  malloc.o free.o realloc.o utils.o garbage.o maccess.o calloc.o memalign.o \
  sbrk.o attach.o detach.o sigset.o sigprocmask.o sigvec.o sigaction.o \
  sigtramp.o ffs.o sig.o _sbrk.o stubs-syscalls.o \
  end.o
  # open.o syscalls.o sigset.o opendir.o readdir.o closedir.o \
  # _dup2.o sysconf.o ftruncate.o __sbrk.o mmap.o sigaction.o 
STUB_OBJS= begin-stubs.o s-arpa_inet-inet_addr.o s-arpa_inet-inet_makeaddr.o \
s-arpa_inet-inet_network.o s-arpa_inet-inet_ntoa.o s-dirent-closedir.o \
s-dirent-opendir.o s-dirent-readdir.o s-dirent-rewinddir.o \
s-dirent-seekdir.o s-dirent-telldir.o s-dirent-scandir.o \
s-grp-getgrent.o s-grp-getgrgid.o \
s-grp-getgrnam.o s-math-acos.o s-math-acosh.o s-math-asin.o \
s-math-asinh.o s-math-atan.o s-math-atan2.o s-math-atanh.o s-math-cbrt.o \
s-math-ceil.o s-math-copysign.o s-math-cos.o s-math-cosh.o s-math-drem.o \
s-math-erf.o s-math-erfc.o s-math-exp.o s-math-expm1.o s-math-fabs.o \
s-math-finite.o s-math-floor.o s-math-fmod.o s-math-frexp.o \
s-math-hypot.o s-math-isinf.o s-math-isnan.o s-math-j0.o s-math-j1.o \
s-math-jn.o s-math-ldexp.o s-math-lgamma.o s-math-log.o s-math-log10.o \
s-math-log1p.o s-math-modf.o s-math-pow.o s-math-rint.o s-math-sin.o \
s-math-sinh.o s-math-sqrt.o s-math-tan.o s-math-tanh.o s-math-y0.o \
s-math-y1.o s-math-yn.o s-netdb-gethostbyaddr.o s-netdb-gethostbyname.o \
s-netdb-gethostent.o s-netdb-getnetbyaddr.o s-netdb-getnetbyname.o \
s-netdb-getnetent.o s-netdb-getprotobyname.o s-netdb-getprotobynumber.o \
s-netdb-getprotoent.o s-netdb-getrpcbyname.o s-netdb-getrpcbynumber.o \
s-netdb-getrpcent.o s-netdb-getservbyname.o s-netdb-getservbyport.o \
s-netdb-getservent.o s-pwd-getpwnam.o s-pwd-getpwuid.o s-stdio-fdopen.o \
s-stdio-setvbuf.o \
s-stdio-fgets.o s-stdio-fopen.o s-stdio-freopen.o s-stdio-ftell.o \
s-stdio-gets.o s-stdio-popen.o s-stdio-sprintf.o s-stdio-tempnam.o \
s-stdio-tmpfile.o s-stdio-tmpnam.o s-stdlib-abort.o s-stdlib-abs.o \
s-stdlib-atof.o s-stdlib-atoi.o s-stdlib-atol.o s-stdlib-bsearch.o \
s-stdlib-exit.o s-stdlib-getenv.o s-stdlib-qsort.o s-stdlib-rand.o \
s-stdlib-srand.o s-string-strcat.o s-string-strchr.o s-string-strcmp.o \
s-string-strcpy.o s-string-strcspn.o s-string-strdup.o s-string-strlen.o \
s-string-strncat.o s-string-strncmp.o s-string-strncpy.o \
s-string-strpbrk.o s-string-strrchr.o s-string-strspn.o \
s-string-strstr.o s-string-strtok.o s-sys_stat-chmod.o \
s-sys_stat-fstat.o s-sys_stat-mkdir.o s-sys_stat-mkfifo.o \
s-sys_stat-stat.o s-sys_stat-umask.o s-sys_utsname-uname.o \
s-sys_wait-wait.o s-sys_wait-waitpid.o s-termios-tcgetattr.o \
s-termios-tcsetattr.o s-time-asctime.o s-time-ctime.o s-time-gmtime.o \
s-time-localtime.o s-time-tzset.o s-unistd-_exit.o s-unistd-access.o \
s-unistd-alarm.o s-unistd-chdir.o s-unistd-chown.o s-unistd-close.o \
s-unistd-dup.o s-unistd-dup2.o s-unistd-execl.o s-unistd-execlp.o \
s-unistd-execv.o s-unistd-execve.o s-unistd-execvp.o s-unistd-fork.o \
s-unistd-fpathconf.o s-unistd-getcwd.o s-unistd-getegid.o \
s-unistd-geteuid.o s-unistd-getgid.o s-unistd-getgroups.o \
s-unistd-getlogin.o s-unistd-getpgrp.o s-unistd-getpid.o \
s-unistd-getppid.o s-unistd-getuid.o s-unistd-isatty.o s-unistd-link.o \
s-unistd-lseek.o s-unistd-pathconf.o s-unistd-pause.o s-unistd-pipe.o \
s-unistd-read.o s-unistd-rmdir.o s-unistd-setgid.o s-unistd-setpgid.o \
s-unistd-setsid.o s-unistd-setuid.o s-unistd-sleep.o s-unistd-sysconf.o \
s-unistd-tcgetpgrp.o s-unistd-tcsetpgrp.o s-unistd-ttyname.o \
s-unistd-unlink.o s-unistd-write.o s-utime-utime.o s-assert-__eprintf.o \
s-dirent-chkr_func.o s-grp-chkr_func.o s-sys_stat-chkr_func.o \
stubs-malloc.o check-printf.o \
s-stdio-ferror.o s-stdio-fwrite.o s-stdio-fputs.o s-stdio-fprintf.o \
s-stdio-getc.o s-stdio-ungetc.o s-stdio-fileno.o s-stdio-putc.o \
s-stdio-fclose.o s-string-index.o s-string-rindex.o \
s-string-bzero.o s-string-bcmp.o s-string-bcopy.o s-fcntl-open.o \
s-setjmp-setjmp.o s-setjmp-longjmp.o s-time-time.o s-stdio-fputc.o \
s-stdio-printf.o s-stdio-fflush.o s-stdio-perror.o s-stdio-puts.o \
s-ctype-isalnum.o s-ctype-isalpha.o s-ctype-iscntrl.o \
s-ctype-isdigit.o s-ctype-islower.o s-ctype-isgraph.o s-ctype-isprint.o \
s-ctype-ispunct.o s-ctype-isspace.o s-ctype-isupper.o s-ctype-isxdigit.o \
s-ctype-isblank.o s-ctype-tolower.o s-ctype-toupper.o s-ctype-isascii.o \
s-ctype-toascii.o s-ctype-tolower.o \
s-sys_resource-getrlimit.o s-sys_resource-setrlimit.o \
s-sys_resource-getrusage.o s-sys_time-chkr_func.o \
s-unistd-vfork.o s-unistd-mktemp.o \
s-signal-raise.o s-signal-sigblock.o s-signal-sigsetmask.o \
s-signal-sigaddset.o s-signal-sigdelset.o s-signal-sigemptyset.o \
s-signal-sigfillset.o s-signal-sigismember.o s-signal-siggetmask.o \
s-signal-psignal.o s-signal-sigpause.o s-signal-sigprocmask.o \
s-signal-sigaction.o \
s-string-memset.o s-stdlib-system.o s-unistd-getwd.o s-sys_mman-mmap.o \
s-sys_mman-munmap.o s-sys_mman-mprotect.o \
s-stdlib-strtol.o s-stdlib-strtoul.o s-string-memmove.o s-string-memcpy.o \
s-string-memcmp.o s-stdio-scanf.o \
s-termcap-tgetent.o s-termcap-tgetnum.o s-termcap-tgetflag.o \
s-termcap-tgetstr.o s-termcap-tputs.o s-termcap-tgoto.o \
end-stubs.o

@rules
endifcommon

# ########### GCCCHECKER
ifgccchecker
@decl
TARGET=libchecker.a
ALL_CFLAGS = -DGCCCHECKER $(CPPFLAGS) $(CFLAGS)
ALL_ASFLAGS = $(ACPPFLAGS)
STUB_CFLAGS = -O -Wall -fdollars-in-identifiers -c -g -I$(srcdir) -I.

@rules
symctl.in: $(srcdir)/common.symctl $(srcdir)/lib/stubs-syscalls.symctl \
	   $(srcdir)/config/sparc/sunos/gccchecker.symctl
	cat $(srcdir)/common.symctl $(srcdir)/lib/stubs-syscalls.symctl \
	    $(srcdir)/config/sparc/sunos/gccchecker.symctl > symctl.in

gccchecker.o: $(C_OBJS) symctl.in symctl
	$(LD) $(LDFLAGS_FOR_CHKR) -o gccchecker.o $(C_OBJS)
	./symctl symctl.in gccchecker.o

libchecker.a: gccchecker.o $(STUB_OBJS)
	$(AR) rcv libchecker.a gccchecker.o $(STUB_OBJS)
	$(RANLIB) libchecker.a

install: libchecker.a checker install.dir install.stubs
	$(INSTALL_DATA) libchecker.a $(libdir)
	$(INSTALL_PROGRAM) checker $(bindir)
	$(RM) -f $(bindir)/checkergcc $(bindir)/checkerg++
	ln -s $(bindir)/checker $(bindir)/checkergcc > /dev/null 2>&1 \
	  || cp $(bindir)/checker $(bindir)/checkergcc
	ln -s $(bindir)/checker $(bindir)/checkerg++ > /dev/null 2>&1 \
	  || cp $(bindir)/checker $(bindir)/checkerg++

uninstall: uninstall.stubs
	$(RM) -f $(libdir)/libchecker.a $(bindir)/checker
	$(RM) -f $(bindir)/checkergcc $(bindir)/checkerg++

update: libchecker.a
	$(INSTALL_DATA) libchecker.a $(libdir)

fast-update: gccchecker.o
	$(AR) rv $(libdir)/libchecker.a gccchecker.o
	$(RANLIB) $(libdir)/libchecker.a

endifgccchecker

# ########### GCCCHECKER.so
ifgccchecker.so
@decl
TARGET=libchecker.a
ALL_CFLAGS = -fpic -D__PIC__ $(CPPFLAGS) $(CFLAGS)
ALL_ASFLAGS = $(ACPPFLAGS) -D__PIC__ -fpic
STUB_CFLAGS = -fpic -O -Wall -fdollars-in-identifiers -c -g -I$(srcdir) -I.

@rules
symctl.in: $(srcdir)/common.symctl $(srcdir)/config/sparc/sunos/gccchecker.symctl
	cat $(srcdir)/common.symctl $(srcdir)/config/sparc/sunos/gccchecker.symctl > symctl.in

gccchecker.o: $(C_OBJS) symctl.in symctl
	$(LD) $(LDFLAGS_FOR_CHKR) -o gccchecker.o $(C_OBJS)
	./symctl symctl.in gccchecker.o

libchecker.a: gccchecker.o $(STUB_OBJS)
	$(AR) rcv libchecker.a gccchecker.o $(STUB_OBJS)
	$(RANLIB) libchecker.a

install: libchecker.a checker install.dir install.stubs
	$(INSTALL_DATA) libchecker.a $(libdir)
	$(INSTALL_PROGRAM) checker $(bindir)
	$(RM) -f $(bindir)/checkergcc $(bindir)/checkerg++
	ln -s $(bindir)/checker $(bindir)/checkergcc > /dev/null 2>&1 \
	  || cp $(bindir)/checker $(bindir)/checkergcc
	ln -s $(bindir)/checker $(bindir)/checkerg++ > /dev/null 2>&1 \
	  || cp $(bindir)/checker $(bindir)/checkerg++

uninstall: uninstall.stubs
	$(RM) -f $(libdir)/libchecker.a $(bindir)/checker
	$(RM) -f $(bindir)/checkergcc $(bindir)/checkerg++

update: libchecker.a
	$(INSTALL_DATA) libchecker.a $(libdir)

endifgccchecker.so
