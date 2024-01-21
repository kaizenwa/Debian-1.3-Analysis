# Configuration variables

# They can also be stored in a file /etc/lilo.defines, e.g.
# -DIGNORECASE -DONE_SHOT

# The variables configured in this Makefile are ignored if
# /etc/lilo.defines exists

#   IGNORECASE    Image selection is case-insensitive. Passwords are still
#		  case-sensitive.
#   ONE_SHOT      Disable the command-line and password timeout if any key is
#		  hit at the boot prompt.
#   NODRAIN	  Don't drain keyboard buffer after booting.
#   READONLY	  Don't overwrite the default command line in the map file
#		  after reading it.
#   REWRITE_TABLE Enable rewriting the partition table at boot time.
#   FIX_TYPE	  Iff rewriting is enabled, also "unhide" DOS partitions.
#   HIDE_DOS	  Iff FIX_TYPE and rewriting are enabled, hide other DOS
#		  partitions.
#   NOINSTDEF     Don't install a new boot sector if INSTALL is not specified.
#   NO1STDIAG	  Don't show diagnostic on read errors in the first stage boot
#		  loader.
#   VARSETUP	  Enables use of variable-size setup segments.
#   XL_SECS=n	  Support for extra large (non-standard) floppies.
#   BEEP	  Beep after displaying "LILO".

CONFIG=-DIGNORECASE -DVARSETUP -DFIX_TYPE -DHIDE_DOS

# End of configuration variables

SBIN_DIR=/sbin
CFG_DIR=/etc
BOOT_DIR=/boot

PCONFIG=`( if [ -r $$ROOT/etc/lilo.defines ]; then \
  cat $$ROOT/etc/lilo.defines; else echo $(CONFIG); fi ) | \
  sed 's/-D/-DLCF_/g'`
GO=-DGO=0x`sed '/go/s/^.*go  0 \(....\) A.*$$/\1/p;d' first.lis`

SHELL=/bin/sh
CC=cc
CPP=$(CC) -E
AS86=as86 -0 -a
LD86=ld86 -0

CFLAGS=-Wall -Wno-parentheses -g $(PCONFIG)
LDFLAGS=#-Xlinker -qmagic

OBJS=lilo.o map.o geometry.o boot.o device.o common.o bsect.o cfg.o temp.o \
  partition.o identify.o

.SUFFIXES:	.img .b

all:		lilo boot.b any_b.b any_d.b dump.b os2_d.b chain.b dparam.com \
		activate

.c.o:
		$(CC) -c $(CFLAGS) $*.c

.s.o:
		$(AS86) -w -o $*.o $*.s

.o.img:
		$(LD86) -s -o $*.img $*.o

.img.b:
		dd if=$*.img of=$*.b bs=32 skip=1

activate:	activate.c
		$(CC) -Wall -s -O -o activate activate.c $(LDFLAGS)

dparam.com:	dparam.img
		dd if=dparam.img of=dparam.com bs=288 skip=1

lilo:		$(OBJS)
		$(CC) -o lilo $(LDFLAGS) $(OBJS)

boot.b:		first.b second.b
		(dd if=first.b bs=512 conv=sync; dd if=second.b) >boot.b

first.s:	first.S lilo.h lilo
		$(CPP) `./lilo -X` first.S -o first.s

second.s:	second.S lilo.h lilo
		$(CPP) $(PCONFIG) $(GO) `./lilo -X` second.S -o second.s

chain.s:	chain.S lilo.h
		$(CPP) $(PCONFIG) $(GO) chain.S -o chain.s

any_b.s:	chain.S lilo.h
		$(CPP) $(PCONFIG) $(GO) chain.S -DSWAP_FD -o any_b.s

any_d.s:	chain.S lilo.h
		$(CPP) $(PCONFIG) $(GO) chain.S -DSWAP_HD -o any_d.s

os2_d.s:	chain.S lilo.h
		$(CPP) $(PCONFIG) $(GO) chain.S -DDOS_D -o os2_d.s

#dos_d.s:	chain.S lilo.h first.lis
#		$(CPP) $(PCONFIG) $(GO) chain.S -DDOS_D -o dos_d.s

dump.s:		dump.S lilo.h first.lis
		$(CPP) `./lilo -X` dump.S -DDOS_D -o dump.s \
	          -DGO=0x`sed '/go/s/^.*go  0 \(....\) A.*$$/\1/p;d' first.lis`

xxx.s:		chain.S lilo.h
		$(CPP) chain.S -DXXX -o xxx.s

first.o first.lis:	first.s
		$(AS86) -w -o first.o -l first.lis first.s

second.lis:	second.s
		$(AS86) -w -l second.lis second.s

install:	all
		if [ ! -d $$ROOT$(SBIN_DIR) ]; then mkdir $$ROOT$(SBIN_DIR); fi
		if [ ! -d $$ROOT$(CFG_DIR) ]; then mkdir $$ROOT$(CFG_DIR); fi
		if [ ! -d $$ROOT$(BOOT_DIR) ]; then mkdir $$ROOT$(BOOT_DIR); fi
		if [ -f $$ROOT$(BOOT_DIR)/boot.b ]; then \
		  mv $$ROOT$(BOOT_DIR)/boot.b $$ROOT$(BOOT_DIR)/boot.old; fi
		if [ -f $$ROOT$(BOOT_DIR)/chain.b ]; then \
		  mv $$ROOT$(BOOT_DIR)/chain.b $$ROOT$(BOOT_DIR)/chain.old; fi
		if [ -f $$ROOT$(BOOT_DIR)/any_b.b ]; then \
		  mv $$ROOT$(BOOT_DIR)/any_b.b $$ROOT$(BOOT_DIR)/any_b.old; fi
		if [ -f $$ROOT$(BOOT_DIR)/any_d.b ]; then \
		  mv $$ROOT$(BOOT_DIR)/any_d.b $$ROOT$(BOOT_DIR)/any_d.old; fi
		if [ -f $$ROOT$(BOOT_DIR)/os2_d.b ]; then \
		  mv $$ROOT$(BOOT_DIR)/os2_d.b $$ROOT$(BOOT_DIR)/os2_d.old; fi
		cp boot.b any_b.b any_d.b chain.b os2_d.b $$ROOT$(BOOT_DIR)
		cp lilo $$ROOT$(SBIN_DIR)
		strip $$ROOT$(SBIN_DIR)/lilo
		cp activate $$ROOT$(SBIN_DIR)
		strip $$ROOT$(SBIN_DIR)/activate
		@if [ -e $$ROOT/etc/lilo/install ]; then echo; \
		  echo -n "$$ROOT/etc/lilo/install is obsolete. LILO is now ";\
		  echo "re-installed "; \
		  echo "by just invoking $(SBIN_DIR)/lilo"; echo; fi
		@echo "/sbin/lilo must now be run to complete the update."

dep:
		sed '/\#\#\# Dependencies/q' <Makefile >tmp_make
		$(CPP) $(CFLAGS) -MM *.c >>tmp_make
		mv tmp_make Makefile

version:
		[ -r VERSION ] && [ -d ../lilo -a ! -d ../lilo-`cat VERSION` ]\
		  && mv ../lilo ../lilo-`cat VERSION`

clean:
		rm -f *.o *.img *.lis *~ first.s second.s chain.s any_b.s \
		  any_d.s dos_d.s os2_d.s dump.s tmp_make \
		  first.b second.b dump.b *.orig

spotless:	clean
		rm -f lilo activate boot.b chain.b any_b.b any_d.b dos_d.b \
		  os2_d.b dparam.com

### Dependencies
activate.o : activate.c 
boot.o : boot.c config.h common.h lilo.h geometry.h cfg.h map.h partition.h \
  boot.h 
bsect.o : bsect.c config.h common.h lilo.h cfg.h device.h geometry.h map.h temp.h \
  partition.h boot.h bsect.h 
cfg.o : cfg.c common.h lilo.h temp.h cfg.h 
common.o : common.c common.h lilo.h 
device.o : device.c config.h common.h lilo.h temp.h device.h 
geometry.o : geometry.c config.h lilo.h common.h device.h geometry.h cfg.h 
identify.o : identify.c cfg.h 
lilo.o : lilo.c config.h common.h lilo.h temp.h geometry.h map.h bsect.h cfg.h \
  identify.h 
map.o : map.c lilo.h common.h geometry.h map.h 
partition.o : partition.c config.h lilo.h common.h cfg.h device.h geometry.h 
temp.o : temp.c common.h lilo.h temp.h 
