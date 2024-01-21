#
# mpage:	A program to reduce pages of print so that several pages
# 	  	of output appear on one sheet of paper.
#
# Copyright (c) 1988 Mark P. Hahn, Herndon, Virginia
# Copyright (c) 1994-1996 Marcel J.E. Mol, Rijswijk, The Netherlands
#                    marcel@mesa.nl
#     
# Written by:
#   ...!uunet!\                       Mark Hahn, Sr Systems Engineer
#              >pyrdc!mark            Pyramid Technology Corporation
# ...!pyramid!/                       Vienna, Va    (703)848-2050
#
#
#     Permission is granted to anyone to make or distribute verbatim
#     copies of this document as received, in any medium, provided
#     that this copyright notice is preserved, and that the
#     distributor grants the recipient permission for further
#     redistribution as permitted by this notice.
#
#  
#     Marcel Mol                     MESA Consulting
#     Karel Doormanlaan 58           P.o. box 9
#     2283 AT Rijswijk               2270 AA Voorburg
#     The Netherlands                The Netherlands
#     phone: 070-3361515             phone: +31-6-54724868
#     marcel@mesa.nl                 info@mesa.nl
#  

# Set this to an ANSI compatible C compiler (preferably gcc)
#CC=cc
CC=gcc


#
# Define your favorite page size:
#          PAGE_LETTER for US letter
#          PAGE_LEGAL for legal
#          PAGE_A4 for European A4
#
PAGESIZE=PAGE_A4

#
# Define your spooler type
#          ATT_SPOOLER for 'lp ...' type printing
#          BSD_SPOOLER for 'lpr ..' tpye printing
#
SPOOL_TYPE=BSD_SPOOLER

# PREFIX=e:/usr		# OS/2 type
PREFIX=/usr/local
BINDIR=$(PREFIX)/bin
LIBDIR=$(PREFIX)/lib
MANDIR=$(PREFIX)/man/man1


# Unix extensions
E = 
O = .o
# OS/2 and MSDOS extensions
# E = .exe
# O = .obj
# pattern rules (OS/2)
# %$(O): %.c
#       $(CC) $(CFLAGS) -c -o $@ $<

#
# A default encoding is given in encoding.h. Setting ENCODING=1 will
# activate this encoding by default. 0 switches default off.
# The -C option (without subargument) can be used to switch.
#
ENCODING=1


#
# Set the default sheet margin in 1/72 inches. 
#
SHEETMARGIN=20 


DEFS = -DPAGE_DEF=${PAGESIZE} -DLIBDIR=\"${LIBDIR}/mpage\" -DDEFAULT_ENCODING=${ENCODING} -DDEFAULTSMARGIN=${SHEETMARGIN} -DSPOOLER=${SPOOL_TYPE}

# Choose one of the following CFLAGS definitions:
# If you are using gcc, you probably don't need to change anything here.

# Linux:
CFLAGS = -O2 -s $(DEFS) -Wall

# AIX (xlC on aix 4):
#CFLAGS = -O2 -s $(DEFS)

# SunOS 4.1.3_U1 1 sun4m (gcc 2.7.2)
#CFLAGS = -O2 -s $(DEFS) -D__USE_FIXED_PROTOTYPES__ -Wall

# SunOS 4.1.3_U1 1 sun4m (standard C-compiler (NOTE: sample.c will not
#     compile, since the standard cc is not ANSI-standard)
#CFLAGS = -O2 -s $(DEFS)

# HP-UX A.09.01 A 9000/735 (standard cc)
#CFLAGS = -Aa +O3 -s $(DEFS) -D_INCLUDE_POSIX_SOURCE -Wall

#
# END OF CONFIGURATION OPTIONS
#


HEAD =  mpage.h
DENC =  encoding.h
SRCS =  mpage.c glob.c text.c post.c file.c page.c args.c
MOBJ =  mpage$(O) glob$(O) text$(O) post$(O) file$(O) page$(O) args$(O)
SMPL =  sample.c page.c glob.c args.c
SOBJ =  sample$(O) page$(O) glob$(O) args$(O)

default:	mpage$(E) msample$(E)
	@echo Done!

mpage$(E):	$(MOBJ)
	$(CC) $(CFLAGS) -o mpage$(E) $(MOBJ) ${LIBS}

msample$(E):	$(SOBJ)
	$(CC) $(CFLAGS) -o msample$(E) $(SOBJ)

clean:
	rm -rf $(MOBJ) mpage$(E) mpage.ps $(SOBJ) msample$(E) make.log core

mpage$(O):	$(HEAD) $(DENC) mpage.c
glob$(O):	$(HEAD) glob.c
text$(O):	$(HEAD) text.c
post$(O):	$(HEAD) post.c
file$(O):	$(HEAD) file.c
page$(O):	$(HEAD) page.c
args$(O):	$(HEAD) args.c
sample$(O):	$(HEAD) sample.c

mpage.ps:	mpage.1
	psroff -t -man mpage.1 > mpage.ps


#
# add your proper install stuff
#
install:
	cp mpage$(E) $(BINDIR)
	cp mpage.1 $(MANDIR)
	if [ ! -d $(LIBDIR)/mpage ] ; then mkdir $(LIBDIR)/mpage ; fi
	-cp Encodings/* $(LIBDIR)/mpage
