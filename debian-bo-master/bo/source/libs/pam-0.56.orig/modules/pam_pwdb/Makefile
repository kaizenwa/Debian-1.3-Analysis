# $Id: Makefile,v 1.6 1997/02/15 17:25:32 morgan Exp $
#
# This Makefile controls a build process of the pam_unix module
# for Linux-PAM. You should not modify this Makefile.
#
# rewritten to compile new module Andrew Morgan
# <morgan@parc.power.net> 1996/11/6
#

ifeq ($(HAVE_PWDBLIB),yes)

TITLE=pam_pwdb
CHKPWD=pwdb_chkpwd

# compilation flags
EXTRAS=
# extra object files
PLUS=
# extra files that may be needed to be created
CREATE=

# NOTE: this module links dynamically to the libpwdb library.
EXTRALS += -lpwdb
EXTRAS += -DCHKPWD_HELPER=\"$(SUPLEMENTED)/$(CHKPWD)\"

########################### don't edit below ##########################

LIBSRC = $(TITLE).c
LIBOBJ = $(TITLE).o
LIBOBJD = $(addprefix dynamic/,$(LIBOBJ))
LIBOBJS = $(addprefix static/,$(LIBOBJ))
LIBDEPS = pam_unix_acct.-c pam_unix_auth.-c pam_unix_passwd.-c \
	pam_unix_sess.-c pam_unix_pwupd.-c support.-c
PLUS += md5.o md5_crypt.o
CFLAGS += $(EXTRAS)

dummy:
	@echo "**** This is not a top-level Makefile "
	exit

dynamic/$(LIBOBJ) : $(LIBSRC) $(LIBDEPS)
	$(CC) $(CFLAGS) $(DYNAMIC) $(CPPFLAGS) $(TARGET_ARCH) -c $< -o $@

static/$(LIBOBJ) : $(LIBSRC) $(LIBDEPS)
	$(CC) $(CFLAGS) $(STATIC) $(CPPFLAGS) $(TARGET_ARCH) -c $< -o $@

ifdef DYNAMIC
LIBSHARED = $(TITLE).so
endif
ifdef STATIC
LIBSTATIC = lib$(TITLE).o
endif

all: info dirs $(PLUS) $(LIBSHARED) $(LIBSTATIC) register $(CHKPWD)

info:
	@echo
	@echo "*** Building PAM_pwdb module..."
	@echo

$(CHKPWD): pwdb_chkpwd.o md5.o md5_crypt.o
	$(CC) -o $(CHKPWD) pwdb_chkpwd.o md5.o md5_crypt.o -lpwdb

pwdb_chkpwd.o: pwdb_chkpwd.c pam_unix_md.-c

dirs:
ifdef DYNAMIC
	@$(MKDIR) ./dynamic
endif
ifdef STATIC
	@$(MKDIR) ./static
endif

register:
ifdef STATIC
	( cd .. ; ./register_static $(TITLE) $(TITLE)/$(LIBSTATIC) )
endif

ifdef DYNAMIC
$(LIBOBJD): $(LIBSRC)

$(LIBSHARED):	$(LIBOBJD)
	$(LD_D) -o $@ $(LIBOBJD) $(PLUS) $(EXTRALS)
endif

ifdef STATIC
$(LIBOBJS): $(LIBSRC)

$(LIBSTATIC): $(LIBOBJS)
	$(LD) -r -o $@ $(LIBOBJS) $(PLUS) $(EXTRALS)
endif

install: all
	$(MKDIR) $(SECUREDIR)
ifdef DYNAMIC
	$(INSTALL) -m 644 $(LIBSHARED) $(SECUREDIR)
endif
	$(INSTALL) -m 4555 -o root -g root $(CHKPWD) $(SUPLEMENTED)

remove:
	rm -f $(SECUREDIR)/$(TITLE).so
	rm -f $(SUPLEMENTED)/$(CHKPWD)

clean:
	rm -f $(CHKPWD) $(LIBOBJD) $(LIBOBJS) $(MOREDELS) core *~ *.o *.so

extraclean: clean
	rm -f *.a *.o *.so *.bak

else

include ../dont_makefile

endif

#####################################################################
# $Log: Makefile,v $
# Revision 1.6  1997/02/15 17:25:32  morgan
# update for .56 . extra commands for new helper binary
#
# Revision 1.5  1997/01/04 20:39:08  morgan
# conditional on having libpwdb
#
# Revision 1.4  1996/12/01 03:02:03  morgan
# changed banner, removed linking libraries
#
# Revision 1.3  1996/11/10 20:14:42  morgan
# cross platform support
#
# Revision 1.2  1996/09/05 06:36:49  morgan
# options added and use of LD altered
#
# Revision 1.1  1996/08/29 13:23:29  morgan
# Initial revision
#
#
