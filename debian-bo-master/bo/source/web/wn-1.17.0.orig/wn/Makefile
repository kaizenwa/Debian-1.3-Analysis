# Makefile for wn/wn.c and wn/swn.c version 1.15.7


CC = cc
CFLAGS = 
CPPFLAGS = 
LDFLAGS = 
EXTRALIBS = 


BINDIR = ../bin

OBJS	= wn.o prequest.o init.o chkcntrl.o send.o parse.o util.o misc.o www.o\
	  csearch.o gsearch.o isearch.o regcomp.o regfind.o cgi.o mod.o\
	  tilde.o rfc931.o evalif.o image.o chkauth.o vhost.o

SWNOBJS	= swn.o prequest.o swninit.o chkcntrl.o send.o parse.o util.o\
	misc.o www.o csearch.o gsearch.o isearch.o regcomp.o regfind.o cgi.o\
	mod.o standalone.o tilde.o rfc931.o evalif.o image.o chkauth.o\
	vhost.o

HEADERS = wn.h common.h err.h extra.h ../config.h

all:	wn swn

wn:	$(OBJS)
	$(CC) $(LDFLAGS) -o wn $(OBJS) $(LIBS) $(EXTRALIBS)

swn:	$(SWNOBJS)
	$(CC) $(LDFLAGS) -o swn $(SWNOBJS) $(LIBS) $(EXTRALIBS)

install: wn swn
	@echo "Installing wn and swn in $(BINDIR)"
	@cp wn $(BINDIR)
	@strip $(BINDIR)/wn
	@cp swn $(BINDIR)
	@strip $(BINDIR)/swn

clean:
	@rm -f *.o *~ wn swn

wn.o:		wn.c version.h $(HEADERS)
swn.o:		wn.c version.h $(HEADERS)
prequest.o:	$(HEADERS)
init.o:		init.c version.h $(HEADERS)
swninit.o:	init.c $(HEADERS)
util.o:		util.c $(HEADERS)
acc.o:		acc.c $(HEADERS)
chkcntrl.o:	chkcntrl.c $(HEADERS) chkcntrl.h content.h access.h 
csearch.o:	csearch.c $(HEADERS) search.h 
gsearch.o:	gsearch.c $(HEADERS) search.h  parse.h
isearch.o:	isearch.c $(HEADERS) search.h 
misc.o:		misc.c $(HEADERS)
www.o:		www.c $(HEADERS)
send.o:		send.c $(HEADERS) version.h 
parse.o:	parse.c $(HEADERS)  parse.h
evalif.o:	evalif.c $(HEADERS) parse.h access.h 
cgi.o:		cgi.c $(HEADERS) version.h cgi.h 
mod.o:		mod.c $(HEADERS)
standalone.o:	standalone.c $(HEADERS)
tilde.o:	tilde.c $(HEADERS)
rfc931.o:	rfc931.c $(HEADERS)
chkauth.o:	chkauth.c $(HEADERS) auth.h
image.o:	image.c $(HEADERS) image.h 
vhost.o:	vhost.c $(HEADERS)
regcomp.o:	reg.h regi.h
regfind.o:	reg.h regi.h

