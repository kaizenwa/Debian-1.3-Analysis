#
# $Id: Makefile,v 1.18 1997/02/15 15:56:09 morgan Exp $
#
# $Log: Makefile,v $
# Revision 1.18  1997/02/15 15:56:09  morgan
# inherit major and minor numbers
#
# Revision 1.17  1997/01/04 20:03:09  morgan
# update for .55
#
# Revision 1.16  1996/12/01 03:14:13  morgan
# update for .54
#
# Revision 1.15  1996/11/10 20:07:51  morgan
# updated for .53
#
# Revision 1.14  1996/09/05 06:06:53  morgan
# added local flag for locking, slight reorganization too.
#

# need to tell libpam about the default directory for PAMs
MOREFLAGS=-D"DEFAULT_MODULE_PATH=\"$(SECUREDIR)/\""

# you may uncomment the following to build libpam in modified ways

# lots of debugging information goes to /tmp/pam-debug.log
#MOREFLAGS += -D"DEBUG"

# pay attention to locked /etc/pam.conf file
#MOREFLAGS += -D"PAM_LOCKING"

LIBNAME=libpam
VERSION=.$(MAJOR_REL)
MODIFICATION=.$(MINOR_REL)

# ---------------------------------------------

dummy:
	@echo "*** This is not a top-level Makefile!"

# ---------------------------------------------

CFLAGS += $(DYNAMIC) $(STATIC) $(MOREFLAGS)

# dynamic library names

LIBPAM = $(LIBNAME).so
LIBPAMNAME = $(LIBPAM)$(VERSION)
LIBPAMFULL = $(LIBPAMNAME)$(MODIFICATION)

# static library name

LIBPAMSTATIC = $(LIBNAME).a

ifdef STATIC
MODULES = $(shell cat ../modules/_static_module_objects)
STATICOBJ = pam_static.o
endif

EXTRAS = pam_delay.o
ifdef MEMORY_DEBUG
EXTRAS += pam_malloc.o
endif

LIBOBJECTS = pam_item.o pam_strerror.o pam_end.o pam_start.o pam_data.o \
		pam_dispatch.o pam_handlers.o pam_misc.o pam_account.o \
		pam_auth.o pam_session.o pam_password.o pam_env.o \
		$(EXTRAS)

# ---------------------------------------------
## rules

all: $(LIBPAM) $(LIBPAMSTATIC)

$(LIBPAM): $(LIBOBJECTS)  $(STATICOBJ)
ifdef DYNAMIC_LIBPAM
	$(LD) -soname $(LIBPAMNAME) -x -shared -o $@ $(LIBOBJECTS)  \
                $(STATICOBJ) $(MODULES)
endif

$(LIBPAMSTATIC): $(LIBOBJECTS) $(STATICOBJ)
ifdef STATIC_LIBPAM
	$(AR) $@ $(LIBOBJECTS) $(STATICOBJ) $(MODULES)
	$(RANLIB) $@
endif

install: all
	$(MKDIR) $(INCLUDED)
	$(INSTALL) -m 644 include/security/pam_appl.h $(INCLUDED)
	$(INSTALL) -m 644 include/security/pam_modules.h $(INCLUDED)
	$(INSTALL) -m 644 include/security/_pam_macros.h $(INCLUDED)
	$(INSTALL) -m 644 include/security/_pam_types.h $(INCLUDED)
ifdef MEMORY_DEBUG
	$(INSTALL) -m 644 include/security/pam_malloc.h $(INCLUDED)
endif
ifdef DYNAMIC_LIBPAM
	$(INSTALL) -m 644 $(LIBPAM) $(LIBDIR)/$(LIBPAMFULL)
	$(LDCONFIG)
	( cd $(LIBDIR) ; ln -sf $(LIBPAMNAME) $(LIBPAM) )
endif
ifdef STATIC_LIBPAM
	$(INSTALL) -m644 $(LIBPAMSTATIC) $(LIBDIR)
endif

remove:
	rm -f $(INCLUDED)/_pam_types.h
	rm -f $(INCLUDED)/_pam_macros.h
	rm -f $(INCLUDED)/pam_appl.h
	rm -f $(INCLUDED)/pam_modules.h
	rm -f $(INCLUDED)/pam_malloc.h
	rm -f $(LIBDIR)/$(LIBPAM).*
	rm -f $(LIBDIR)/$(LIBPAM)
	$(LDCONFIG)
	rm -f $(LIBDIR)/$(LIBPAMSTATIC)

clean:
	rm -f $(LIBOBJECTS) pam_static.o a.out core *~

extraclean:	

	@$(MAKE) clean
	rm -f *.a *.out *.o *.so ./include/security/*~

ifdef STATIC_LIBPAM
$(STATICOBJ): pam_static.c ../modules/_static_module_objects
	$(CC) $(CFLAGS) -c pam_static.c
endif
