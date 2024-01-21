# $Id: gwm.shar,v 1.115 1995/12/08 07:51:55 colas Exp $
#
# $Log: gwm.shar,v $
# Revision 1.115  1995/12/08 07:51:55  colas
# ********************
# *** Version 1.8c ***
# ********************
#
 * Revision 1.100  1995/05/29  15:56:57  colas
 * simple-win.gwm: new parameters:
 *     label like simple-icon
 *     legend to place the label on sides of window
 *     lpad and rpad: number of () to pad the label with stretchable space
 * bar-max-wdths set by default to 1000
 *
 * John Carr <jfc@MIT.EDU>: patches to supress warnings on AIX/RS_6000/xlc
 * rxterm install fixed once more
 *
 * Revision 1.97  1995/05/16  16:16:36  colas
 * contrib/scripts/find-bar-nils
 *
# Revision 1.5  1995/05/15  22:29:34  colas
# bar can have abitrary shaped backgrounds (shaped tiles)
#
 * Revision 1.95  1995/05/11  17:06:56  colas
 * better spy
 *
 * Revision 1.93  1995/04/26  16:34:51  colas
 * Makefile added in distrib
 *
 * simple-icon.gwm:
 *
 *     - customize item "legend" can now be instead of () or t the strings:
 *       "top" "base" "right" "left" for the positions where you want the string
 *       to appear
 *       e.g: (customize simple-icon any XTerm "left")
 *
 *     - new customization item "label" to provide either a fixed string or a
 *       lambda which will be used to filter the label
 *       must return a non-empty string otherwise the unfiltered label is used
 *       e.g: to supress the Netscape: in netscape icon titles
 *       (customize simple-icon any Netscape
 *           label (lambdaq (s) (match "Netscape: \\(.*\\)$" s 1))
 *       )
 *
 * iconify a window doesnt not loose the window anymore in case of error in wool
 * code
 *
 * Revision 1.92  1995/04/25  14:31:09  colas
 * *** Version 1.7p_beta_2 ***
 *
# Revision 1.3  1994/09/30  18:55:08  uwe
# Distribution targets uses parent directory to keep
# emacs VC happy when making snapshots.
#
# Revision 1.2  1994/09/30  17:40:28  uwe
# New targets for creating distributions.
#
# Revision 1.1  1994/09/21  20:48:22  uwe
# Initial revision
#

PROG = gwmsh

all: $(PROG)

SRCS	= gwmsh.c version.c
OBJS	= gwmsh.o version.o

NONSRCS = Makefile rcshead ChangeLog PATCH-GWM README

THINGS_TO_RELEASE = $(SRCS) $(NONSRCS)

LN	= ln
TAR	= gtar -v
COMPRESS= gzip -9
SHAR	= shar -o

CC	= gcc
CDEBUG	= -ggdb3 -Wall -Wshadow
#CC	= cc
#CDEBUG	= -g

CINC	= -I/usr/openwin/include -I/usr/gnu/include
CDEFS	= -DUSE_READLINE # -DNO_LIBERTY
CFLAGS	= $(CDEBUG) $(CINC) $(CDEFS)

READLINE= -lreadline -ltermcap
LIBS	= $(READLINE) -liberty -lX11 -lm

LDFLAGS	= -L/usr/gnu/lib -L/usr/openwin/lib

$(PROG): $(OBJS)
	$(CC) -o $(PROG) $(OBJS) $(LDFLAGS) $(LIBS)

clean:
	-rm -f core $(PROG) $(OBJS)

new: clean $(PROG)

distdir: $(THINGS_TO_RELEASE)
	@RELEASE=$(PROG)-`./rcshead version.c`;\
	echo Creating "$$RELEASE" in parent directory;\
	rm -rf ../"$$RELEASE";\
	mkdir ../"$$RELEASE";\
	$(LN) $(THINGS_TO_RELEASE) ../"$$RELEASE"

tardist: distdir
	@RELEASE=$(PROG)-`./rcshead version.c`;\
	echo Making tar archive;\
	cd ..;\
	$(TAR) -c -v -f "$$RELEASE".tar $$RELEASE

tgzdist: tardist
	@RELEASE=$(PROG)-`./rcshead version.c`;\
	echo Compressing tar archive;\
	cd ..;\
	$(COMPRESS) "$$RELEASE".tar

shardist: distdir
	@RELEASE=$(PROG)-`./rcshead version.c`;\
	echo Making shar archive;\
	cd ..;\
	$(SHAR) "$$RELEASE".part "$$RELEASE"
