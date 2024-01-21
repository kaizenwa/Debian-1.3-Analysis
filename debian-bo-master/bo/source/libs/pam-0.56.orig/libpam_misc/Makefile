# $Header: /home/morgan/pam/Linux-PAM-0.56/libpam_misc/RCS/Makefile,v 1.9 1997/02/15 15:46:56 morgan Exp $
#
# $Log: Makefile,v $
# Revision 1.9  1997/02/15 15:46:56  morgan
# inherit major and minor numbers from top level
#
# Revision 1.8  1997/01/04 20:20:11  morgan
# update for .55 and make -> $(MAKE)
#
# Revision 1.7  1996/12/01 03:28:11  morgan
# update for 0.54
#

dummy:
	@echo "*** This is not a top-level Makefile!"

# ///////////////////////////////////////////////////////////////////

# uncomment if you wnat libpam_misc to be made as a dynamic library
# AGM has had some segfaulting from libdl when I did this. I have not
# investigated the cause...

MAKE_DYNAMIC=yes

LIBNAME = pam_misc
LIBMAJOR=$(MAJOR_REL)
LIBMINOR=$(MINOR_REL)

FILES=xstrdup misc_conv help_env

#
# Probably no need to alter anything below here.
#

# build dynamic library names

LIBDYNAMIC=lib$(LIBNAME).so
LIBDYNMAJ=$(LIBDYNAMIC).$(LIBMAJOR)
LIBDYNMIN=$(LIBDYNMAJ).$(LIBMINOR)

# static library name

LIBSTATIC = lib$(LIBNAME).a

# sources and object files

LIBSRC = $(addsuffix .c,$(FILES))
LIBOBJ = $(addsuffix .o,$(FILES))

# rules

all: $(LIBSTATIC) $(LIBDYNAMIC)

$(LIBDYNAMIC): $(LIBOBJ)
ifdef MAKE_DYNAMIC
	$(LD) -soname $(LIBDYNMAJ) -x -shared -o $@ $(LIBOBJ)
endif

$(LIBSTATIC): $(LIBOBJ)
	$(AR) $@ $(LIBOBJ)
	$(RANLIB) $@

install: all
	$(MKDIR) $(INCLUDED)
	$(INSTALL) -m 644 ./pam_misc.h $(INCLUDED)
ifdef MAKE_DYNAMIC
	$(INSTALL) -m 644 $(LIBDYNAMIC) $(LIBDIR)/$(LIBDYNMIN)
	$(LDCONFIG)
	( cd $(LIBDIR) ; ln -sf $(LIBDYNMAJ) $(LIBDYNAMIC) )
endif
	$(INSTALL) -m 644 $(LIBSTATIC) $(LIBDIR)

clean:
	rm -f *.so *.a core a.out *~

remove:
	rm -f $(INCLUDED)/pam_misc.h
	rm -f $(LIBDIR)/$(LIBDYNAMIC).*
	rm -f $(LIBDIR)/$(LIBDYNAMIC)
	$(LDCONFIG)
	rm -f $(LIBDIR)/$(LIBSTATIC)
	rm -f $(INCLUDED)/chk_malloc.h

.c.o:
	$(CC) -c $(DEFS) $(CFLAGS) $<

extraclean:
	@$(MAKE) clean
	rm -f *.o *.bak

