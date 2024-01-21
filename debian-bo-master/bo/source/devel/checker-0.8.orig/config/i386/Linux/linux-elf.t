### makefile fragment for linux-elf
# Note: Each line beginning with `##' are reserved.

# Note for `##-conf' lines: They are displayed (without ##-conf.?) with
# ./MakeMakefile --help
##-conf Options for i386-linuxelf:
##-conf.aschecker: (libchecker.o)@*  Code Insertion in GAS.  Needs Checkered libs
##-conf.plchecker: (plchecker.o)@*  Pre-loading checker.  (Not tested)
##-conf.mdchecker: (libmdchecker.a)@*  A simple Malloc Debugger.  (Not tested)
##-conf.gccchecker: (gccchecker.o)@*  Checker with GCC.  Need a patched version of gcc-2.7.2
##-conf.gcccheckerso: (gccchecker.so.1)@*  Checker with GCC.  Shared Object.  (See above).
##-conf.default: aschecker

# Common declaration
ifcommon
@decl
RANLIB = ranlib
LDFLAGS_FOR_CHKR = -m elf_i386 -r
SYMCTL_C = $(srcdir)/utils/symctl_elf.c
@rules
endifcommon

# ##### for mdchecker
ifmdchecker
@decl
OBJS=parse-args.o error.o smalloc.o find-exec.o time.o message.o version.o \
  malloc.o free.o realloc.o utils.o garbage.o calloc.o memalign.o sbrk.o \
  attach.o detach.o maccess.o vprintf.o getcwd.o mktemp.o qsort.o opendir.o \
  closedir.o readdir.o linux.o startup.o __sbrk.o _exit.o __access.o \
  __close.o __dup2.o __fstat.o __getpid.o __kill.o __link.o __lseek.o \
  __lstat.o __open.o __read.o __stat.o __unlink.o __write.o __fcntl.o \
  __gettod.o __sigproc.o __ftruncate.o mmap.o munmap.o elf-stabs.o
ALL_CFLAGS = -DMDCHECKER $(CPPFLAGS) $(CFLAGS) -c
TARGET=libmdchecker.a

@rules
libmdchecker.a: $(OBJS)
	$(AR) rcvs libmdchecker.a $(OBJS)
	$(RANLIB) libmdchecker.a
endifmdchecker

# ###### For plchecker
ifplchecker
@decl
OBJS=parse-args.o error.o smalloc.o find-exec.o time.o message.o version.o \
  malloc.o free.o realloc.o utils.o garbage.o calloc.o memalign.o sbrk.o \
  attach.o detach.o maccess.o vprintf.o getcwd.o mktemp.o qsort.o getenv.o \
  opendir.o closedir.o readdir.o linux.o startup.o __sbrk.o _exit.o \
  __access.o __close.o __dup2.o __fstat.o __getpid.o __kill.o __link.o \
  __lseek.o __lstat.o __open.o __read.o __stat.o __unlink.o __write.o \
  __fcntl.o __gettod.o __ftruncate.o mmap.o munmap.o elf-stabs.o
ALL_CFLAGS = -DPLCHECKER -DMDCHECKER $(CPPFLAGS) $(CFLAGS) -c
TARGET=plchecker.o

@rules
plchecker.o: $(OBJS)
	$(checkerdir)/ld -x -Ttext 0x50000000 -o plchecker.o $(OBJS) -lc
endifplchecker


# ###### For aschecker
ifaschecker
@decl
OBJS=begin.o __access.o __close.o __dup2.o __fstat.o __getpid.o __kill.o \
  __link.o __lseek.o __lstat.o __open.o __read.o __stat.o __unlink.o \
  __write.o __fcntl.o __gettod.o __sigproc.o __ftruncate.o sigjumpto.o \
  sigpending.o _exit.o opendir.o closedir.o readdir.o linux-syscall.o linux.o \
  mmap.o munmap.o __sbrk.o __setjmp.o __sigact.o sigsend.o shmctl.o __ipc.o \
  startup.o elf-stabs.o codecheck.o parse-args.o error.o smalloc.o signal.o \
  find-exec.o fd.o time.o message.o version.o vprintf.o getenv.o getcwd.o \
  mktemp.o qsort.o open.o malloc.o free.o realloc.o utils.o garbage.o \
  maccess.o calloc.o memalign.o sbrk.o attach.o detach.o end.o
ALL_CFLAGS = -DASCHECKER $(CPPFLAGS) $(CFLAGS) -c -D__ELF__
ALL_ASFLAGS = $(ACPPFLAGS) -c -D__ELF__
TARGET=libchecker.o
# Where to put libchecker.a
checkerdir = $(libdir)/checker

@rules
symctl.in: $(srcdir)/common.symctl $(srcdir)/config/i386/Linux/elfchecker.symctl
	cat $(srcdir)/common.symctl $(srcdir)/config/i386/Linux/elfchecker.symctl > symctl.in

libchecker.o: $(OBJS) symctl symctl.in
	$(LD) $(LDFLAGS_FOR_CHKR) -o libchecker.o $(OBJS)
	./symctl -s symctl.in libchecker.o

chkrcrt1.o: $(srcdir)/patches/chkrcrt1.S
	$(CC) -o chkrcrt1.o -c $(srcdir)/patches/chkrcrt1.S

checkergcc: checkergcc.o
	$(CC) $(UTILS_LDFLAGS) -o checkergcc checkergcc.o

checkergcc.o: $(srcdir)/utils/checkergcc.c
	$(CC) $(UTILS_CFLAGS) -c $<

checkerg++: checkerg++.o
	$(CC) $(UTILS_LDFLAGS) -o checkerg++ checkerg++.o

checkerg++.o: $(srcdir)/utils/checkerg++.c
	$(CC) $(UTILS_CFLAGS) -c $<

install: install.dir libchecker.o checkergcc checkerg++ # setchkrfl
	$(INSTALL_DATA) libchecker.o $(checkerdir)
	sed s,@CHECKER_PATH@,$(checkerdir),g < $(srcdir)/bin/specs > specs.new
	$(INSTALL_DATA) specs.new $(checkerdir)/specs
	rm specs.new
#	$(INSTALL_DATA) $(srcdir)/bin/chkrcrt0.o $(checkerdir)/crt0.o
#	./setchkrfl --yes $(checkerdir)/crt0.o
#	$(INSTALL_PROGRAM) $(srcdir)/bin/as $(checkerdir)/as
	$(INSTALL_PROGRAM) checkergcc $(bindir)
	$(INSTALL_PROGRAM) checkerg++ $(bindir)
	@echo "Don't forget to install Checker-libs !"

# Should only be used by maintenainers to quickly install the binary.
update: 
	$(INSTALL_DATA) libchecker.o $(checkerdir)


uninstall: 
	rm -f $(checkerdir)/crt0.o $(checkerdir)/as $(checkerdir)/ld
	rm -f $(bindir)/checkergcc $(bindir)/checkerg++
	rm -f $(checkerdir)/specs $(checkerdir)/libchecker.o

endifaschecker


# ###### For GCC Checker
ifgccchecker
@decl
OBJS=begin.o __access.o __close.o __dup2.o __fstat.o __getpid.o __kill.o \
  __link.o __lseek.o __lstat.o __open.o __read.o __stat.o __unlink.o \
  __write.o __fcntl.o __gettod.o __sigproc.o __ftruncate.o sigjumpto.o \
  sigpending.o _exit.o opendir.o closedir.o readdir.o linux.o  munmap.o \
  mmap.o __sbrk.o __sigact.o shmctl.o __ipc.o startup.o elf-stabs.o \
  parse-args.o error.o smalloc.o signal.o find-exec.o fd.o time.o message.o \
  version.o vprintf.o getenv.o getcwd.o mktemp.o qsort.o malloc.o \
  free.o realloc.o utils.o garbage.o maccess.o calloc.o memalign.o sbrk.o \
  attach.o detach.o gcc-startup.o stubs-syscalls.o main-tramp.o end.o
ALL_CFLAGS = -DGCCCHECKER $(CPPFLAGS) $(CFLAGS)
# Stubs
STUB_OBJS= begin-stubs.o s-arpa_inet-inet_addr.o s-arpa_inet-inet_ntoa.o \
s-arpa_inet-inet_aton.o s-arpa_inet-inet_lnaof.o s-arpa_inet-inet_netof.o \
s-arpa_inet-inet_makeaddr.o s-arpa_inet-inet_network.o s-dirent-opendir.o \
s-dirent-closedir.o s-dirent-readdir.o s-dirent-rewinddir.o \
s-dirent-seekdir.o s-dirent-telldir.o s-fcntl-creat.o s-fcntl-open.o \
s-fcntl-lockf.o s-fcntl-fcntl.o s-fnmatch-fnmatch.o s-getopt-getopt.o \
s-getopt-getopt_long.o s-getopt-getopt_long_only.o s-grp-getgrgid.o \
s-grp-getgrent.o s-grp-setgrent.o s-grp-endgrent.o s-grp-getgrnam.o \
s-netdb-gethostbyname.o s-netdb-gethostbyaddr.o s-netdb-getservbyname.o \
s-netdb-herror.o s-netdb-endhostent.o s-netdb-endnetent.o \
s-netdb-endprotoent.o s-netdb-endservent.o s-netdb-endrpcent.o \
s-netdb-gethostent.o s-netdb-getnetbyaddr.o s-netdb-getnetbyname.o \
s-netdb-getnetent.o s-netdb-getprotobyname.o s-netdb-getprotobynumber.o \
s-netdb-getprotoent.o s-netdb-getservbyport.o s-netdb-getservent.o \
s-netdb-getrpcent.o s-netdb-getrpcbyname.o s-netdb-getrpcbynumber.o \
s-netdb-sethostent.o s-netdb-setnetent.o s-netdb-setprotoent.o \
s-netdb-setservent.o s-netdb-setrpcent.o s-netinet_in-ntohl.o \
s-netinet_in-ntohs.o s-netinet_in-htonl.o s-netinet_in-htons.o \
s-pwd-getpwuid.o s-pwd-getpwnam.o s-pwd-endpwent.o s-pwd-setpwent.o \
s-setjmp-longjmp.o s-setjmp-__setjmp.o s-signal-signal.o \
s-signal-kill.o s-signal-raise.o s-signal-sigblock.o s-signal-sigsetmask.o \
s-signal-sigaddset.o s-signal-sigdelset.o s-signal-sigemptyset.o \
s-signal-sigfillset.o s-signal-sigismember.o s-signal-siggetmask.o \
s-signal-psignal.o s-signal-sigpause.o s-signal-sigprocmask.o \
s-signal-sigaction.o \
s-stdio-printf.o s-stdio-fprintf.o s-stdio-scanf.o s-stdio-sscanf.o \
s-stdio-vasprintf.o s-stdio-asprintf.o s-stdio-vsnprintf.o \
s-stdio-snprintf.o s-stdio-vsscanf.o s-stdio-vscanf.o s-stdio-vfscanf.o \
s-stdio-vsprintf.o s-stdio-vprintf.o s-stdio-vfprintf.o s-stdio-sprintf.o \
s-stdio-fscanf.o s-stdio-fgets.o s-stdio-fputs.o s-stdio-puts.o \
s-stdio-vfprintf.o s-stdio-fflush.o s-stdio-fputc.o s-stdio-putc.o \
s-stdio-setbuf.o s-stdio-sprintf.o s-stdio-putchar.o s-stdio-fileno.o \
s-stdio-getc.o s-stdio-fopen.o s-stdio-ungetc.o s-stdio-fclose.o \
s-stdio-perror.o s-stdio-getchar.o s-stdio-fwrite.o s-stdio-fread.o \
s-stdio-freopen.o s-stdio-ferror.o s-stdio-ftell.o s-stdio-vsprintf.o \
s-stdio-popen.o s-stdio-pclose.o s-stdio-fdopen.o s-stdio-rewind.o \
s-stdio-rename.o s-stdio-remove.o s-stdio-gets.o s-stdio-fseek.o \
s-stdio-fgetc.o s-stdio-feof.o s-stdio-setlinebuf.o s-stdio-tmpnam.o \
s-stdio-tempnam.o s-stdio-putw.o s-stdio-getw.o s-stdio-tmpfile.o \
s-stdio-setvbuf.o s-stdlib-exit.o s-stdlib-abort.o s-stdlib-getenv.o \
s-stdlib-atoi.o s-stdlib-atol.o s-stdlib-atof.o s-stdlib-strtof.o \
s-stdlib-strtod.o s-stdlib-strtold.o s-stdlib-strtol.o s-stdlib-strtoul.o \
s-stdlib-strtoq.o s-stdlib-strtouq.o s-stdlib-qsort.o s-stdlib-bsearch.o \
s-stdlib-abs.o s-stdlib-labs.o s-stdlib-ldiv.o s-stdlib-div.o \
s-stdlib-unsetenv.o s-stdlib-setenv.o s-stdlib-putenv.o s-stdlib-drand48.o \
s-stdlib-erand48.o s-stdlib-lrand48.o s-stdlib-nrand48.o s-stdlib-mrand48.o \
s-stdlib-jrand48.o s-stdlib-srand48.o s-stdlib-seed48.o s-stdlib-lcong48.o \
s-stdlib-rand.o s-stdlib-srand.o s-stdlib-random.o s-stdlib-srandom.o \
s-stdlib-initstate.o s-stdlib-setstate.o s-stdlib-atexit.o \
s-stdlib-on_exit.o s-stdlib-system.o s-string-strrchr.o s-string-strcat.o \
s-string-memmove.o s-string-memcpy.o s-string-strncmp.o s-string-strncpy.o \
s-string-strcpy.o s-string-strlen.o s-string-rindex.o s-string-strcmp.o \
s-string-strchr.o s-string-strerror.o s-string-index.o s-string-bcopy.o \
s-string-bcmp.o s-string-bzero.o s-string-strstr.o s-string-memset.o \
s-string-memcmp.o s-string-strncat.o s-string-stpcpy.o s-string-strdup.o \
s-string-strcoll.o s-string-memchr.o s-string-strspn.o s-string-strpbrk.o \
s-string-strtok.o s-string-strcspn.o s-string-strncasecmp.o \
s-string-strcasecmp.o s-sys_resource-getrlimit.o s-sys_resource-setrlimit.o \
s-sys_resource-setpriority.o s-sys_resource-getpriority.o \
s-sys_resource-getrusage.o s-sys_socket-socket.o s-sys_socket-socketpair.o \
s-sys_socket-bind.o s-sys_socket-connect.o s-sys_socket-listen.o \
s-sys_socket-accept.o s-sys_socket-getsockopt.o s-sys_socket-setsockopt.o \
s-sys_socket-getsockname.o s-sys_socket-getpeername.o s-sys_socket-send.o \
s-sys_socket-recv.o s-sys_socket-sendto.o s-sys_socket-recvfrom.o \
s-sys_socket-shutdown.o s-sys_stat-_xstat.o s-sys_stat-_fxstat.o \
s-sys_stat-_lxstat.o s-sys_stat-_xmknod.o s-sys_stat-stat.o \
s-sys_stat-lstat.o s-sys_stat-fstat.o s-sys_stat-mknod.o s-sys_stat-umask.o \
s-sys_stat-mkfifo.o s-sys_stat-mkdir.o s-sys_stat-chmod.o \
s-sys_stat-fchmod.o s-sys_time-gettimeofday.o s-sys_time-settimeofday.o \
s-sys_time-getitimer.o s-sys_time-setitimer.o s-sys_time-select.o \
s-sys_time-FD_SET.o s-sys_time-FD_CLR.o s-sys_time-FD_ISSET.o \
s-sys_time-FD_ZERO.o s-sys_timeb-ftime.o s-sys_uio-readv.o \
s-sys_uio-writev.o s-sys_utsname-uname.o s-sys_wait-wait.o \
s-sys_wait-waitpid.o s-termios-tcgetattr.o s-termios-tcsetattr.o \
s-time-stime.o s-time-clock.o s-time-time.o s-time-difftime.o \
s-time-mktime.o s-time-asctime.o s-time-ctime.o s-time-strftime.o \
s-time-tzset.o s-time-gmtime.o s-time-localtime.o s-unistd-execl.o \
s-unistd-execvp.o s-unistd-execve.o s-unistd-read.o s-unistd-close.o \
s-unistd-write.o s-unistd-dup2.o s-unistd-getuid.o s-unistd-geteuid.o \
s-unistd-getgid.o s-unistd-getegid.o s-unistd-getgroups.o \
s-unistd-getlogin.o s-unistd-ttyname.o s-unistd-access.o s-unistd-alarm.o \
s-unistd-sleep.o s-unistd-pause.o s-unistd-lseek.o s-unistd-pipe.o \
s-unistd-chown.o s-unistd-fchown.o s-unistd-chdir.o s-unistd-link.o \
s-unistd-dup.o s-unistd-dup2.o s-unistd-getpid.o s-unistd-getppid.o \
s-unistd-unlink.o s-unistd-mktemp.o s-unistd-execv.o s-unistd-execlp.o \
s-unistd-fork.o s-unistd-vfork.o s-unistd-llseek.o s-unistd-fchdir.o \
s-unistd-getcwd.o s-unistd-get_current_dir_name.o s-unistd-getwd.o \
s-unistd-pathconf.o s-unistd-fpathconf.o s-unistd-sysconf.o \
s-unistd-confstr.o s-unistd-getpgrp.o s-unistd-setpgid.o s-unistd-setpgrp.o \
s-unistd-setsid.o s-unistd-setuid.o s-unistd-setreuid.o s-unistd-seteuid.o \
s-unistd-setgid.o s-unistd-setregid.o s-unistd-setegid.o \
s-unistd-setfsuid.o s-unistd-symlink.o s-unistd-readlink.o s-unistd-rmdir.o \
s-unistd-tcgetpgrp.o s-unistd-tcsetpgrp.o s-unistd-gethostid.o \
s-unistd-sethostid.o s-unistd-getpagesize.o s-unistd-getdtablesize.o \
s-unistd-fsync.o s-unistd-sync.o s-unistd-fsync.o s-unistd-vhangup.o \
s-unistd-acct.o s-unistd-ftruncate.o s-unistd-truncate.o s-unistd-nice.o \
s-unistd-usleep.o s-unistd-isatty.o s-unistd-gethostname.o s-unistd-_exit.o \
s-utime-utime.o s-ctype-isalnum.o s-ctype-isalpha.o s-ctype-iscntrl.o \
s-ctype-isdigit.o s-ctype-islower.o s-ctype-isgraph.o s-ctype-isprint.o \
s-ctype-ispunct.o s-ctype-isspace.o s-ctype-isupper.o s-ctype-isxdigit.o \
s-ctype-isblank.o s-ctype-tolower.o s-ctype-toupper.o s-ctype-isascii.o \
s-ctype-toascii.o s-ctype-_toupper.o s-ctype-tolower.o s-ctype-_tolower.o \
s-math-acos.o s-math-asin.o s-math-atan.o s-math-atan2.o s-math-cos.o \
s-math-sin.o s-math-tan.o s-math-cosh.o s-math-sinh.o s-math-tanh.o \
s-math-acosh.o s-math-asinh.o s-math-atanh.o s-math-exp.o s-math-frexp.o \
s-math-ldexp.o s-math-log.o s-math-log10.o s-math-expm1.o s-math-log1p.o \
s-math-modf.o s-math-pow.o s-math-sqrt.o s-math-cbrt.o s-math-ceil.o \
s-math-fabs.o s-math-floor.o s-math-fmod.o s-math-__isinf.o \
s-math-__isnan.o s-math-__finite.o s-math-__infnan.o s-math-__copysign.o \
s-math-__rint.o s-math-rint.o s-math-hypot.o s-math-isinf.o s-math-isnan.o \
s-math-finite.o s-math-infnan.o s-math-copysign.o s-math-drem.o \
s-math-pow2.o s-math-pow10.o s-math-erf.o s-math-erfc.o s-math-j0.o \
s-math-j1.o s-math-jn.o s-math-lgamma.o s-math-y0.o s-math-y1.o \
s-math-yn.o s-math-acosl.o s-math-asinl.o s-math-atanl.o s-math-cosl.o \
s-math-sinl.o s-math-tanl.o s-math-coshl.o s-math-sinhl.o s-math-tanhl.o \
s-math-acoshl.o s-math-asinhl.o s-math-atanhl.o s-math-expl.o \
s-math-frexpl.o s-math-ldexpl.o s-math-logl.o s-math-log10l.o \
s-math-expm1l.o s-math-log1pl.o s-math-modfl.o s-math-powl.o \
s-math-atan2l.o s-math-sqrtl.o s-math-cbrtl.o s-math-log2l.o \
s-math-ceill.o s-math-fabsl.o s-math-floorl.o s-math-fmodl.o \
s-math-__isinfl.o s-math-__isnanl.o s-math-hypotl.o s-math-pow2l.o \
s-math-pow10l.o s-math-erfl.o s-math-erfcl.o s-math-j0l.o s-math-j1l.o \
s-math-jnl.o s-math-lgammal.o s-math-y0l.o s-math-y1l.o s-math-ynl.o \
s-termcap-tgetent.o s-termcap-tgetnum.o s-termcap-tgetflag.o \
s-termcap-tgetstr.o s-termcap-tputs.o s-termcap-tgoto.o s-sys_mman-mmap.o \
s-sys_mman-munmap.o s-sys_mman-mprotect.o s-mntent-setmntent.o \
s-mntent-getmntent.o s-mntent-addmntent.o s-mntent-hasmntopt.o \
s-mntent-endmntent.o s-assert-__eprintf.o \
s-dirent-chkr_func.o s-grp-chkr_func.o \
s-sys_socket-chkr_func.o s-sys_stat-chkr_func.o s-sys_time-chkr_func.o \
stubs-ncurses.o stubs-malloc.o check-printf.o end-stubs.o

STUB_CFLAGS = -O2 -Wall -fdollars-in-identifiers -c -g $(CPPFLAGS)

TARGET=libchecker.a

@rules
symctl.in: $(srcdir)/common.symctl $(srcdir)/lib/stubs-syscalls.symctl \
	   $(srcdir)/config/i386/Linux/gccchecker.symctl
	cat $(srcdir)/common.symctl $(srcdir)/lib/stubs-syscalls.symctl \
	    $(srcdir)/config/i386/Linux/gccchecker.symctl > symctl.in

gccchecker.o: $(OBJS) symctl symctl.in
	$(LD) $(LDFLAGS_FOR_CHKR) -o gccchecker.o $(OBJS)
	./symctl -s symctl.in gccchecker.o

libchecker.a: gccchecker.o $(STUB_OBJS)
	$(AR) rcv libchecker.a gccchecker.o $(STUB_OBJS)

# Should only be used by maintenainers to quickly install the binary.
update: libchecker.a
	$(INSTALL_DATA) libchecker.a $(libdir)

# Should only be used by maintenainers to quickly install the binary.
fast-update: gccchecker.o
	$(AR) rv $(libdir)/libchecker.a gccchecker.o

install: libchecker.a checker install.dir install.stubs
	$(INSTALL_DATA) libchecker.a $(libdir)
	$(INSTALL_PROGRAM) checker $(bindir)
	ln -s $(bindir)/checker $(bindir)/checkergcc > /dev/null 2>&1 \
	  || cp $(bindir)/checker $(bindir)/checkergcc
	ln -s $(bindir)/checker $(bindir)/checkerg++ > /dev/null 2>&1 \
	  || cp $(bindir)/checker $(bindir)/checkerg++

uninstall: uninstall.stubs
	$(RM) -f $(libdir)/libchecker.a $(bindir)/checker
	$(RM) -f $(bindir)/checkergcc $(bindir)/checkerg++

#libstubs.a: $(STUB_OBJS)
#	$(AR) rcv libstubs.a $(STUB_OBJS)

endifgccchecker

# ###### For GCC Checker - Shared Object
ifgcccheckerso
@decl
OBJS=begin.o __access.o __close.o __dup2.o __fstat.o __getpid.o __kill.o \
  __link.o __lseek.o __lstat.o __open.o __read.o __stat.o __unlink.o \
  __write.o __fcntl.o __gettod.o __sigproc.o __ftruncate.o sigjumpto.o \
  sigpending.o _exit.o opendir.o closedir.o readdir.o munmap.o __sbrk.o \
  linux.o mmap.o __sigact.o sigsend.o shmctl.o __ipc.o startup.o elf-stabs.o \
  parse-args.o error.o smalloc.o signal.o find-exec.o fd.o time.o message.o \
  version.o vprintf.o getenv.o getcwd.o mktemp.o qsort.o malloc.o \
  free.o realloc.o utils.o garbage.o maccess.o calloc.o memalign.o sbrk.o \
  attach.o detach.o gcc-startup.o end.o
ALL_CFLAGS = -DGCCCHECKER $(CPPFLAGS) $(CFLAGS) -fPIC -D__PIC__
ALL_ASFLAGS = $(ACPPFLAGS) -D__PIC__ -fPIC
# Stubs
STUB_OBJS = libc-stubs.o chk-printf.o
STUB_CFLAGS = -O2 -Wall -fdollars-in-identifiers -c -g -I$(srcdir)

TARGET=gccchecker.so.1

@rules
symctl.in: $(srcdir)/common.symctl $(srcdir)/config/i386/Linux/gccchecker.symctl
	cat $(srcdir)/common.symctl $(srcdir)/config/i386/Linux/gccchecker.symctl > symctl.in

gccchecker.so.1: $(OBJS) symctl symctl.in
	$(CC) -shared -o gccchecker.so.1 $(OBJS)
	./symctl -s symctl.in gccchecker.so.1

#$(STUB_OBJS):
#	$(CC) $(STUB_CFLAGS) $(srcdir)/utils/$(subst .o,.c,$@) -o $@

#libstubs.a: $(STUB_OBJS)
#	$(AR) rcv libstubs.a $(STUB_OBJS)

endifgcccheckerso
