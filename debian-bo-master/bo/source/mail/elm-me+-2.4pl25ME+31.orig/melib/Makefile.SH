case $CONFIG in
'')
    if test ! -f config.sh; then
	ln ../config.sh . || \
	ln ../../config.sh . || \
	ln ../../../config.sh . || \
	(echo "Can't find config.sh."; exit 1)
    fi
    . ./config.sh
    ;;
esac
case "$0" in
*/*) cd `expr X$0 : 'X\(.*\)/'` ;;
esac

echo "Extracting melib/Makefile (with variable substitutions)"
cat >Makefile <<!GROK!THIS!
#  Makefile for the ELM mail program.
#
# Variables
#	Variables established by Configure
AR		=	ar
CC		=	$cc
CCFLAGS		=	$ccflags $xencf
CHMOD		=	$chmod
LINT		=	$lint
OPTIMIZE	=	$optimize
RANLIB		=	$ranlib
RM		= 	$rm -f
TOUCH		=	$touch
TAGS		=	ctags

!GROK!THIS!

cat >>Makefile <<'!NO!SUBS!'
#	Other general variables
CFLAGS		=	-I$(INCLDIR) $(CCFLAGS) $(OPTIMIZE) $(DEBUG) $(DACSNET) 
INCLDIR		=	../hdrs
LINTFLAGS	=	-I$(INCLDIR)
SHELL		=	/bin/sh

# 	Lists
LIB_SRC		= 	parse_util.c	\
			pgp_decode.c	\
			mime.c		\
			mime_decode.c	\
			mime_parse.c	\
			state.c

LIB_OBJ		=	parse_util.o	\
			pgp_decode.o	\
			mime.o		\
			mime_decode.o	\
			mime_parse.o	\
			state.o

all:			Makefile libme.a

install:		Makefile libme.a

uninstall:		libme.a

Makefile: Makefile.SH ../config.sh
	@echo "You must run 'sh Configure -S' or 'sh Configure'"
	exit 1

libme.a:		$(LIB_OBJ)
			$(AR) r $@ $?
			$(RANLIB) $@

lint:		
		$(LINT) $(LINTFLAGS) $(LIB_SRC) > LINT.OUT

tags:
		$(TAGS) $(LIB_SRC)

clean:		
		$(RM) $(LIB_OBJ) libme.a

# Dependencies and rules
#	Dependencies of header files upon other header files they include
.PRECIOUS:		$(INCLDIR)/defs.h $(INCLDIR)/elm.h $(INCLDIR)/headers.h

$(INCLDIR)/defs.h:	$(INCLDIR)/../config.h $(INCLDIR)/sysdefs.h
			$(CHMOD) u+w $@
			$(TOUCH) $@

$(INCLDIR)/elm.h:	$(INCLDIR)/curses.h $(INCLDIR)/defs.h  $(INCLDIR)/melib.h
			$(CHMOD) u+w $@
			$(TOUCH) $@

$(INCLDIR)/headers.h:	$(INCLDIR)/curses.h $(INCLDIR)/defs.h  $(INCLDIR)/me.h $(INCLDIR)/elmlib.h
			$(CHMOD) u+w $@
			$(TOUCH) $@

#	Dependencies of C object files
pgp_decode.o:		$(INCLDIR)/headers.h $(INCLDIR)/melib.h
parse_util.o:   	$(INCLDIR)/headers.h $(INCLDIR)/melib.h
mime_parse.o:   	$(INCLDIR)/headers.h $(INCLDIR)/melib.h
mime_decode.o:  	$(INCLDIR)/headers.h $(INCLDIR)/me.h  
mime.o:			$(INCLDIR)/headers.h $(INCLDIR)/s_elm.h $(INCLDIR)/melib.h
state.o:		$(INCLDIR)/headers.h $(INCLDIR)/melib.h
!NO!SUBS!
