# $Header: /home/morgan/pam/Linux-PAM-0.55/modules/pam_unix/RCS/Makefile,v 1.2 1996/11/10 20:18:59 morgan Exp $
#
# This Makefile controls a build process of the pam_unix modules
# for Linux-PAM. You should not modify this Makefile.
#
# $Log: Makefile,v $
# Revision 1.2  1996/11/10 20:18:59  morgan
# cross platform support
#
# Revision 1.1  1996/11/09 19:44:16  morgan
# Initial revision
#
#

########################################################################
# some options... uncomment to take effect
########################################################################

# do you want shadow?
#USE_SHADOW=-D"HAVE_SHADOW_H"

# do you want cracklib?
USE_CRACKLIB=-D"USE_CRACKLIB"

# do you have lckpwdf?
USE_LCKPWDF=-D"USE_LCKPWDF"

########################################################################

CFLAGS += $(USE_SHADOW) $(USE_CRACKLIB) $(USE_LCKPWDF)

ifdef DYNAMIC
LIBSESSSH = pam_unix_session.so
LIBAUTHSH = pam_unix_auth.so
LIBPASSWDSH = pam_unix_passwd.so
LIBACCOUNT = pam_unix_acct.so
endif

ifdef STATIC
LIBSTATIC = libpam_unix.o
endif

ifdef USE_CRACKLIB
CRACKLIB = -lcrack
endif

LIBAUTHOBJ = pam_unix_auth.o support.o
LIBAUTHSRC = pam_unix_auth.c support.c
LIBSESSOBJ = pam_unix_sess.o
LIBSESSSRC = pam_unix_sess.c
LIBPASSWDSRC = pam_unix_passwd.c
LIBPASSWDOBJ = pam_unix_passwd.o
LIBACCOUNTSRC = pam_unix_acct.c
LIBACCOUNTOBJ = pam_unix_acct.o
LIBOBJ = $(LIBAUTHOBJ) $(LIBSESSOBJ) $(LIBPASSWDOBJ) $(LIBACCOUNTOBJ)
LIBSRC = $(LIBAUTHSRC) $(LIBSESSSRC) $(LIBPASSWDSRC) $(LIBACCOUNTSRC)

LIBSHARED = $(LIBSESSSH) $(LIBAUTHSH) $(LIBPASSWDSH) $(LIBACCOUNT)

LIBOBJD = $(addprefix dynamic/,$(LIBOBJ))
LIBOBJS = $(addprefix static/,$(LIBOBJ))

dynamic/%.o : %.c
	$(CC) $(CFLAGS) $(DYNAMIC) $(CPPFLAGS) -c $< -o $@

static/%.o: %.c
	$(CC) $(CFLAGS) $(STATIC) $(CPPFLAGS) -c $< -o $@


########################### don't edit below #######################

dummy:

	@echo "**** This is not a top-level Makefile "
	exit

info:
	@echo
	@echo "*** Building pam-unix(alpha) module of the framework..."
	@echo

all: dirs info $(LIBSHARED) $(LIBSTATIC) register

dirs:
ifdef DYNAMIC
	mkdir -p ./dynamic
endif
ifdef STATIC
	mkdir -p ./static
endif

register:
ifdef STATIC
	( cd .. ; \
	  ./register_static pam_unix_auth  pam_unix/$(LIBSTATIC) ; \
	  ./register_static pam_unix_acct  "" ; \
	)
endif

ifdef DYNAMIC
$(LIBOBJD): $(LIBSRC)

$(LIBAUTHSH):	$(LIBAUTHSRC) $(LIBOBJD)
		$(LD) -x --shared -o $@ $(addprefix dynamic/,$(LIBAUTHOBJ))

$(LIBSESSSH):	$(LIBSESSSRC) $(LIBOBJD)
		$(LD) -x --shared -o $@ $(addprefix dynamic/,$(LIBSESSOBJ))

$(LIBPASSWDSH):	$(LIBPASSWDSRC) $(LIBOBJD)
		$(LD) -x --shared -o $@ $(addprefix dynamic/,$(LIBPASSWDOBJ)) $(CRACKLIB)

$(LIBACCOUNT):	$(LIBACCOUNTSRC) $(LIBOBJD)
		$(LD) -x --shared -o $@ $(addprefix dynamic/,$(LIBACCOUNTOBJ))
endif


ifdef STATIC
$(LIBOBJS): $(LIBSRC)

$(LIBSTATIC): $(LIBOBJS)
	$(LD) -r -o $@ $(LIBOBJS)
endif

install: all
	mkdir -p $(SECUREDIR)
ifdef DYNAMIC
	install -m644 $(LIBSHARED) $(SECUREDIR)
endif

remove:
	cd $(SECUREDIR) && rm -f $(LIBSHARED)

clean:
	rm -f $(LIBOBJD) $(LIBOBJS) a.out core *~

extraclean: clean
	rm -f *.a *.out *.o *.so *.bak

.c.o:	
	$(CC) -c $(CFLAGS) $<

