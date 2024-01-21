#
# $Id: Makefile,v 1.5 1997/01/04 20:28:47 morgan Exp $
#
# This Makefile controls a build process of $(TITLE) module for
# Linux-PAM. You should not modify this Makefile (unless you know
# what you are doing!).
#
# $Log: Makefile,v $
# Revision 1.5  1997/01/04 20:28:47  morgan
# compile with and without libpwdb
#
# Revision 1.4  1996/11/10 20:13:18  morgan
# cross platform support
#
# Created by Andrew Morgan <morgan@parc.power.net> 1996/6/11
#

TITLE=pam_group
CONFD=$(CONFIGED)/security
export CONFD
CONFILE=$(CONFD)/group.conf
export CONFILE

#

LIBSRC = $(TITLE).c
LIBOBJ = $(TITLE).o
LIBOBJD = $(addprefix dynamic/,$(LIBOBJ))
LIBOBJS = $(addprefix static/,$(LIBOBJ))

DEFS=-DCONFILE=\"$(CONFILE)\"
ifeq ($(HAVE_PWDBLIB),yes)
 DEFS+=-DWANT_PWDB
 ELIBS=-lpwdb
endif

CFLAGS += $(DEFS)

dynamic/%.o : %.c
	$(CC) $(CFLAGS) $(DYNAMIC) $(CPPFLAGS) $(TARGET_ARCH) -c $< -o $@

static/%.o : %.c
	$(CC) $(CFLAGS) $(STATIC) $(CPPFLAGS) $(TARGET_ARCH) -c $< -o $@


ifdef DYNAMIC
LIBSHARED = $(TITLE).so
endif
ifdef STATIC
LIBSTATIC = lib$(TITLE).o
endif

####################### don't edit below #######################

dummy:
	@echo "**** This is not a top-level Makefile "
	exit

all: dirs $(LIBSHARED) $(LIBSTATIC) register

dirs:
ifdef DYNAMIC
	$(MKDIR) ./dynamic
endif
ifdef STATIC
	$(MKDIR) ./static
endif

register:
ifdef STATIC
	( cd .. ; ./register_static $(TITLE) $(TITLE)/$(LIBSTATIC) )
endif

ifdef DYNAMIC
$(LIBOBJD): $(LIBSRC)

$(LIBSHARED):	$(LIBOBJD)
		$(LD_D) -o $@ $(LIBOBJD) $(ELIBS)
endif

ifdef STATIC
$(LIBOBJS): $(LIBSRC)

$(LIBSTATIC): $(LIBOBJS)
	$(LD) -r -o $@ $(LIBOBJS) $(ELIBS)
endif

install: all
	$(MKDIR) $(SECUREDIR)
ifdef DYNAMIC
	$(INSTALL) -m 644 $(LIBSHARED) $(SECUREDIR)
endif
	bash -f ./install_conf

remove:
	rm -f $(SECUREDIR)/$(TITLE).so
	rm -f $(CONFILE)

clean:
	rm -f $(LIBOBJD) $(LIBOBJS) core *~
	rm -f ./.ignore_age

extraclean: clean
	rm -f *.a *.o *.so *.bak

.c.o:	
	$(CC) $(CFLAGS) -c $<

