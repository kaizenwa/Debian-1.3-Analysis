# $Id: Makefile,v 1.2 1996/07/15 10:02:48 root Exp $

#-- Do not edit ----------------------------------------------------------#

CFLAGS		=	$(CCFLAGS) \
			-DSPOOLDIR="\"$(SPOOLDIR)\"" \
			-DVERSION="\"$(VERSION)\""

all:			vboxgetty \
			vbox \
			rmdcatheader \
			rmdcutheader \
			rmdgetheader

#-- rmdgetheader ---------------------------------------------------------#

GETHEADEROBJS	=	rmdgetheader.o

rmdgetheader:		$(GETHEADEROBJS)
			$(CC) $(GETHEADEROBJS) -o ../bin/rmdgetheader

#-- rmdcatheader ---------------------------------------------------------#

CATHEADEROBJS	=	rmdcatheader.o

rmdcatheader:		$(CATHEADEROBJS)
			$(CC) $(CATHEADEROBJS) -o ../bin/rmdcatheader

#-- rmdcutheader ---------------------------------------------------------#

CUTHEADEROBJS	=	rmdcutheader.o

rmdcutheader:		$(CUTHEADEROBJS)
			$(CC) $(CUTHEADEROBJS) -o ../bin/rmdcutheader

#-- vboxgetty ------------------------------------------------------------#

VBOXGETTYOBJS	=	vboxgetty.o modem.o log.o lock.o voice.o vboxrc.o \
			utils.o

vboxgetty:		$(VBOXGETTYOBJS)
			$(CC) $(VBOXGETTYOBJS) -o ../bin/vboxgetty

#-- VBox -----------------------------------------------------------------#

VBOXLIBS	=	-lpanel -lncurses

VBOXOBJS	=	vbox.o vboxrc.o utils.o

vbox:			$(VBOXOBJS)
			$(CC) $(VBOXOBJS) $(VBOXLIBS) -o ../bin/vbox

#-- install --------------------------------------------------------------#


#-- cleaning -------------------------------------------------------------#

clean:	ignore
		@rm -f *~

clear:	clean
		@rm -f *.[oas] ../bin/v* ../bin/rmd*

#-- end -------------------------------------------------------------------#

ignore:
