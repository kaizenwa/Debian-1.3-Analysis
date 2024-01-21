# src/Makefile for irqtune

SDIR = ../src

SBIN = /sbin
MODTAIL = _mod

MODULE = irqtune$(MODTAIL)
OBJS = load.o
TARGETLIST = $(MODULE).o irqtune
INSLIST = $(SBIN)/$(MODULE).o $(SBIN)/irqtune

CFLAGS = -O $(CFGINCLUDE) -I$(SDIR) -I../include -Wall -DMODTAIL=\"$(MODTAIL)\"
CKFLAGS = $(CFLAGS) -D__KERNEL__

sbin: $(TARGETLIST)

$(MODULE).o: $(SDIR)/load.c $(SDIR)/irqhigh.c
	cc -c $(CKFLAGS) $(SDIR)/load.c
	ld -r -o $(MODULE).o $(OBJS)
	rm -f $(OBJS)

irqtune: $(SDIR)/irqtune.c
	cc -o irqtune $(CFLAGS) -s $(SDIR)/irqtune.c

install:
	cp $(TARGETLIST) $(SBIN)

uninstall:
	rm -f $(INSLIST)

clean:
	rm -f $(TARGETLIST)
