#
# man/Makefile 1.26 1997/04/23 15:50:22 (David Hinds)
#

# Include site dependent options
include ../config.mk

COFLAGS = -kv
MANDIR = $(PREFIX)/usr/man

man1 = cardinfo.1
man4 = pcmcia_core.4 tcic.4 i82365.4 tc589_cs.4 pcnet_cs.4 xircnw_cs.4 \
	serial_cs.4 pcmem_cs.4 ftl_cs.4 memory_cs.4 fmvj18x_cs.4 \
	wavelan_cs.4 aha152x_cs.4
man5 = pcmcia.5 stab.5
man8 = cardmgr.8 cardctl.8 ftl_format.8 ftl_check.8 scsi_info.8 \
	ifport.8 ide_info.8 pcinitrd.8

all: $(man1) $(man4) $(man5) $(man8)

dep:

clean:

install: $(man1) $(man4) $(man5) $(man8)
	mkdir -p $(MANDIR)/man1 $(MANDIR)/man4 $(MANDIR)/man5 $(MANDIR)/man8
	cp $(man1) $(MANDIR)/man1
	cp $(man4) $(MANDIR)/man4
	cp $(man5) $(MANDIR)/man5
	cp $(man8) $(MANDIR)/man8
