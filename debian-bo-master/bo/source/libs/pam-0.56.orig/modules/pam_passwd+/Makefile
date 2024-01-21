# This Makefile controls a build process of the pam_passwd+ modules
# for Linux-PAM.

ifeq ($(OS),solaris)
include ../dont_makefile
else

LIBSESSSH =
LIBAUTHSH =
LIBPASSWDSH = pam_passwd+.so

ifdef STATIC
LIBSTATIC = libpam_passwd+.o
endif

LIBAUTHOBJ =
LIBAUTHSRC =
LIBSESSOBJ =
LIBSESSSRC =
LIBPASSWDSRC = load.c log.c pattern.c prog.c util.c verify.c \
		pam_passwd+.c test.y passwd.h sys.h
LIBPASSWDOBJ = load.o log.o pattern.o prog.o util.o verify.o \
		test.o pam_passwd+.o
LIBOBJ = $(LIBAUTHOBJ) $(LIBSESSOBJ) $(LIBPASSWDOBJ)
LIBSRC = $(LIBAUTHSRC) $(LIBSESSSRC) $(LIBPASSWDSRC)
LIBOBJD = $(addprefix dynamic/,$(LIBOBJ))
LIBOBJS = $(addprefix static/,$(LIBOBJ))

dynamic/%.o : %.c
	$(CC) $(CFLAGS) $(DYNAMIC) $(CPPFLAGS) $(TARGET_ARCH) -c $< -o $@

static/%.o : %.c
	$(CC) $(CFLAGS) $(STATIC) $(CPPFLAGS) $(TARGET_ARCH) -c $< -o $@


# when the other pieces are written, uncomment them
ifdef DYNAMIC
LIBSHARED = $(LIBPASSWDSH) # $(LIBSESSSH) $(LIBAUTHSH) 
endif

all:	dirs $(LIBSHARED) $(LIBSTATIC) register

dirs:
ifdef DYNAMIC
	$(MKDIR) ./dynamic
endif
ifdef STATIC
	$(MKDIR) ./static
endif

register:
ifdef STATIC
	( cd .. ; ./register_static pam_passwdPLUS pam_passwd+/$(LIBSTATIC) )
endif

ifdef DYNAMIC
$(LIBOBJD): $(LIBSRC)
endif

ifdef DYNAMIC
$(LIBSHARED):	$(LIBOBJD)
		$(LD_D) -o $@ $(LIBOBJD)
endif

ifdef STATIC
$(LIBOBJS): $(LIBSRC)
endif

ifdef STATIC
$(LIBSTATIC): $(LIBOBJS)
	$(LD) -r -o $@ $(LIBOBJS)
endif

install: all
	$(MKDIR) $(SECUREDIR)
ifdef DYNAMIC
	$(INSTALL) -m 644 $(LIBSHARED) $(SECUREDIR)
endif

remove:
	cd $(SECUREDIR) && rm -f $(LIBPASSWDSH) $(LIBSESSSH) $(LIBAUTHSH)

clean:
	rm -f $(LIBOBJD) $(LIBOBJS) a.out core *~

extraclean: clean
	rm -f *.a *.out *.o *.so *.bak

.c.o:	
	$(CC) -c $(CFLAGS) $<

# This is a list of the local include files so that Makefile will work properly
load.o:			load.c passwd.h sys.h extdecs.h
log.o:			log.c passwd.h sys.h extdecs.h
pam_passwd+_passwd.o:	pam_passwd+_passwd.c extdecs.h
pattern.o:		pattern.c passwd.h sys.h extdecs.h
prog.o:			prog.c passwd.h sys.h extdecs.h
util.o:			util.c passwd.h sys.h extdecs.h
verify.o:		verify.c passwd.h sys.h extdecs.h

# The 'sed' execution corrects a flaw in the bison.simple file
test.c:		test.y passwd.h sys.h extdecs.h
	bison -y test.y
	sed -e 's/yyparse (void)/yyparse (void *)/' <y.tab.c >test.c
	rm y.tab.c

test.o:	test.c

endif