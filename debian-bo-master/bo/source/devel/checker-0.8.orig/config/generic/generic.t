# Note: Each line beginning with `##' are reserved.

# Note for `##-conf' lines: They are displayed (without ##-conf.?) with
# ./MakeMakefile --help
##-conf Options for generic:
##-conf.gccchecker: gccchecker.o
##-conf.default: gccchecker

# Common part
ifcommon
@decl
### makefile fragment for generic
RANLIB = echo > /dev/null
LDFLAGS_FOR_CHKR = -r --static -nostdlib -nostartfiles -v
SYMCTL_C = 
@rules
endifcommon

# ########### GCCCHECKER
ifgccchecker
@decl
TARGET=libchecker.a
ALL_CFLAGS = -DGCCCHECKER $(CPPFLAGS) $(CFLAGS)
ALL_ASFLAGS = $(ACPPFLAGS)
STUB_CFLAGS = -O -Wall -fdollars-in-identifiers -c -g -I$(srcdir) -I.
C_OBJS=begin.o generic-symtab.o chkr-string.o generic.o \
  startup.o parse-args.o error.o smalloc.o signal.o find-exec.o fd.o \
  time.o message.o version.o vprintf.o getenv.o mktemp.o qsort.o \
  malloc.o free.o realloc.o utils.o garbage.o maccess.o calloc.o memalign.o \
  sbrk.o attach.o detach.o syscall.o end.o
# open.o syscalls.o sigset.o opendir.o readdir.o closedir.o \
# _dup2.o sysconf.o ftruncate.o __sbrk.o mmap.o sigaction.o 

@rules
gccchecker.o: $(C_OBJS) # $(srcdir)/config/sparc/solaris2/simchecker.symctl ./symctl
	$(CC) $(LDFLAGS_FOR_CHKR) -o gccchecker.o $(C_OBJS)
#	./symctl -s $(srcdir)/config/sparc/solaris2/simchecker.symctl checker.so.1

machine.h: $(srcdir)/config/generic/make-machine \
  $(srcdir)/config/generic/prospect.c $(srcdir)/config/generic/tm-generic.h
	$(SHELL) $(srcdir)/config/generic/make-machine $(srcdir)

header.aux confdefs.h: $(srcdir)/stubs/configure
	$(srcdir)/stubs/configure

available-stubs.h: machine.h header.aux confdefs.h mk-stubs
	cp confdefs.h available-stubs.h
#	./mk-stubs -t define < header.aux >> available-stubs.h

Makefile-stubs: header.aux mk-stubs $(srcdir)/stubs/written-stubs \
	 $(srcdir)/stubs/SubMakefile $(srcdir)/config/generic/generic.t
	$(srcdir)/config/generic/make-Makefile-stubs $(srcdir)

stubs-files: Makefile-stubs available-stubs.h
	$(MAKE) -f Makefile-stubs CC="$(CC)" STUB_CFLAGS="$(STUB_CFLAGS)" \
	 srcdir="$(srcdir)" stubs-files

mk-stubs.o: $(srcdir)/stubs/mk-stubs.c
	$(CC) -g -c -o $@ $(srcdir)/stubs/mk-stubs.c

mk-stubs: mk-stubs.o
	$(CC) -o mk-stubs mk-stubs.o

install.stubs: Makefile-stubs
	$(MAKE) -f Makefile-stubs CC="$(CC)" STUB_CFLAGS="$(STUB_CFLAGS)" \
	 srcdir="$(srcdir)" includedir="$(includedir)" \
	 INSTALL_DATA="$(INSTALL_DATA)" install.stubs

libchecker.a: gccchecker.o stubs-files
	$(AR) rcv libchecker.a gccchecker.o begin-stubs.o `cat stubs-files` end-stubs.o

install: libchecker.a checker install.dir install.stubs
	$(INSTALL_DATA) libchecker.a $(libdir)
	$(INSTALL_PROGRAM) checker $(bindir)
	$(RM) -f $(bindir)/checkergcc
	ln -s $(bindir)/checker $(bindir)/checkergcc > /dev/null 2>&1 \
	  || cp $(bindir)/checker $(bindir)/checkergcc
#	ln -s $(bindir)/checker $(bindir)/checkerg++ > /dev/null 2>&1 \
#	  || cp $(bindir)/checker $(bindir)/checkerg++

uninstall: uninstall.stubs
	$(RM) -f $(libdir)/libchecker.a $(bindir)/checker
	$(RM) -f $(bindir)/checkergcc $(bindir)/checkerg++

endifgccchecker

