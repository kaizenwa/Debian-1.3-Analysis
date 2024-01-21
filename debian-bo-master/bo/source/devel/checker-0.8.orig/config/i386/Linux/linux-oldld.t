### makefile fragment for linux-aout
# Note: Each line beginning with `##' are reserved.

# Note for `##-conf' lines: They are displayed (without ##-conf.?) with
# ./MakeMakefile --help
##-conf Options for i386-linux-a.out
##-conf.checker: (libchecker.o) Code Insertion in GAS.  Needs Checkered libs
##-conf.plchecker: (plchecker.o) Pre-loading checker.
##-conf.mdchecker: (libmdchecker.a) Malloc Debugger.
##-conf.gccchecker: (gccchecker.o) Checker with GCC.
##-conf.default: checker

# Common declaration
ifcommon
@decl
RANLIB = ranlib
LDFLAGS_FOR_CHKR = -r -static
SYMCTL_C = $(srcdir)/utils/symctl_aout.c
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
  __gettod.o __sigproc.o __ftruncate.o mmap.o munmap.o gnu.a.out.o
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
  __fcntl.o __gettod.o __ftruncate.o mmap.o munmap.o gnu.a.out.o 
ALL_CFLAGS = -DPLCHECKER -DMDCHECKER $(CPPFLAGS) $(CFLAGS) -c
TARGET=plchecker.o

@rules
plchecker.o: $(OBJS)
	$(checkerdir)/ld -x -Ttext 0x50000000 -o plchecker.o $(OBJS) -lc
endifplchecker


# ###### For checker
ifchecker
@decl
OBJS=__access.o __close.o __dup2.o __fstat.o __getpid.o __kill.o __link.o \
  __lseek.o __lstat.o __open.o __read.o __stat.o __unlink.o __write.o \
  __fcntl.o __gettod.o __sigproc.o __ftruncate.o sigjumpto.o sigpending.o \
  _exit.o opendir.o closedir.o readdir.o linux-syscall.o linux.o mmap.o \
  munmap.o __sbrk.o __setjmp.o __sigact.o sigsend.o shmctl.o __ipc.o \
  startup.o gnu.a.out.o codecheck.o parse-args.o error.o smalloc.o signal.o \
  find-exec.o fd.o time.o message.o version.o vprintf.o getenv.o getcwd.o \
  mktemp.o qsort.o open.o malloc.o free.o realloc.o utils.o garbage.o \
  maccess.o calloc.o memalign.o sbrk.o attach.o detach.o imodf.o cvt.o
SYMCTLFILE = $(srcdir)/config/i386/Linux/checker.symctl
ALL_CFLAGS = -DASCHECKER -DGCCCHECKER $(CPPFLAGS) $(CFLAGS)
TARGET=libchecker.o
# Where to put libchecker.a
checkerdir = $(libdir)/checker

@rules
libchecker.o: $(OBJS) symctl $(SYMCTLFILE)
	$(LD) $(LDFLAGS_FOR_CHKR) -o libchecker.o $(OBJS)
	./symctl $(SYMCTLFILE) libchecker.o

setchkrfl.o: $(srcdir)/utils/setchkrfl.c
	$(CC) $(UTILS_CFLAGS) -c $<

setchkrfl: setchkrfl.o
	$(CC) $(UTILS_LDFLAGS) -o setchkrfl setchkrfl.o

chkrcrt0.o: $(srcdir)/patches/chkrcrt0.S setchkrfl
	$(CC) -o chkrcrt0.o -c $(srcdir)/patches/chkrcrt0.S
	./setchkrfl --yes chkrcrt0.o

install: install.dir libchecker.o checkergcc checkerg++ # setchkrfl
	$(INSTALL_DATA) $(LIBOBJ) $(checkerdir)
	sed s,@CHECKER_PATH@,$(checkerdir), < $(srcdir)/bin/specs > specs.new
	$(INSTALL_DATA) specs.new $(checkerdir)/specs
	rm specs.new
	$(INSTALL_DATA) $(srcdir)/bin/chkrcrt0.o $(checkerdir)/crt0.o
#	./setchkrfl --yes $(checkerdir)/crt0.o
	$(INSTALL_PROGRAM) $(srcdir)/bin/as $(checkerdir)/as
	$(INSTALL_PROGRAM) $(srcdir)/bin/ld $(checkerdir)/ld
	$(INSTALL_PROGRAM) checkergcc $(bindir)
	$(INSTALL_PROGRAM) checkerg++ $(bindir)
	@echo "Don't forget to install Checker-libs !"

uninstall:
	rm -f $(checkerdir)/crt0.o $(checkerdir)/as $(checkerdir)/ld
	rm -f $(bindir)/checkergcc $(bindir)/checkerg++
	rm -f $(checkerdir)/specs $(checkerdir)/libchecker.o

endifchecker


# ###### For GCC Checker
ifgccchecker
@decl
OBJS=__access.o __close.o __dup2.o __fstat.o __getpid.o __kill.o __link.o \
  __lseek.o __lstat.o __open.o __read.o __stat.o __unlink.o __write.o \
  __fcntl.o __gettod.o __sigproc.o __ftruncate.o sigjumpto.o sigpending.o \
  _exit.o opendir.o closedir.o readdir.o linux.o mmap.o munmap.o __sbrk.o \
  __sigact.o sigsend.o shmctl.o __ipc.o startup.o gnu.a.out.o \
  parse-args.o error.o smalloc.o signal.o find-exec.o fd.o time.o message.o \
  version.o vprintf.o getenv.o getcwd.o mktemp.o qsort.o malloc.o \
  free.o realloc.o utils.o garbage.o maccess.o calloc.o memalign.o sbrk.o \
  attach.o detach.o imodf.o cvt.o
SYMCTLFILE = $(srcdir)/config/i386/Linux/gccchecker.symctl
ALL_CFLAGS = -DGCCCHECKER $(CPPFLAGS) $(CFLAGS)
TARGET=gccchecker.o

@rules
gccchecker.o: $(OBJS) symctl $(SYMCTLFILE)
	$(LD) $(LDFLAGS_FOR_CHKR) -o gccchecker.o $(OBJS)
	./symctl $(SYMCTLFILE) gccchecker.o

gccchkrcrt0.o: $(srcdir)/patches/gccchkrcrt0.S
	$(CC) -o gccchkrcrt0.o -c $(srcdir)/patches/gccchkrcrt0.S
endifgccchecker
