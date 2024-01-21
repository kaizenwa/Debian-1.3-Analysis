#
# $Id: Makefile,v 1.7 1997/02/15 19:02:27 morgan Exp $
#
# This Makefile controls a build process of $(TITLE) module for
# Linux-PAM. You should not modify this Makefile (unless you know
# what you are doing!).
#
# $Log: Makefile,v $
# Revision 1.7  1997/02/15 19:02:27  morgan
# updated email address
#
# Revision 1.6  1996/11/10 20:14:34  morgan
# cross platform support
#
# Revision 1.5  1996/09/05 06:32:45  morgan
# ld --> gcc
#
# Revision 1.4  1996/05/26 15:49:25  morgan
# make dynamic and static dirs
#
# Revision 1.3  1996/05/26 04:04:26  morgan
# automated static support
#
# Revision 1.2  1996/03/16 17:56:38  morgan
# tidied up
#
#
# Created by Andrew Morgan <morgan@parc.power.net> 1996/3/11
#

TITLE=pam_permit

#

LIBSRC = $(TITLE).c
LIBOBJ = $(TITLE).o
LIBOBJD = $(addprefix dynamic/,$(LIBOBJ))
LIBOBJS = $(addprefix static/,$(LIBOBJ))

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
	rm -f $(SECUREDIR)/$(TITLE).so

clean:
	rm -f $(LIBOBJD) $(LIBOBJS) core *~

extraclean: clean
	rm -f *.a *.o *.so *.bak

.c.o:	
	$(CC) $(CFLAGS) -c $<

