#
#  Answer the following questions and type 'make build'.
#
SHELL=/bin/sh

#
# Which compiler are you using?  This _must_ be a C++ compiler.
#
CC=g++

#
# Compiler flags? 
#
CFLAGS=-g -DUNIX -Wall -I. 

#
# Where is your Isearch distribution?
#
ISEARCH_DIR=../Isearch
ISEARCH_LIB=libIsearch.a

#
# Where do you want the binaries stored?
#
BIN_DIR=../bin

#
# Where are the CNIDR Search API (libsapi) include files located?
#
LS=../sapi
SAPI_LIB=libsapi.a

#
# Where are the libraries with which I need to link?
#
# This makefile requires that all required libraries are in one directory.
#
# The required libraries include: libsapi.a, libz3950.a and libIsearch.a
#
LIB_DIR=../bin

#
# STOP!!
#
# Hopefully, you didn't have to change anything above.  Chances are,
# you'll never have to change anything below.
#

#
# Full filename of the resulting library
#
LIB=$(LIB_DIR)/libz3950.a

#
# Borrowed from GNU gcc distribution
#
# How to invoke ranlib.
#
MYRANLIB = ranlib
MYRANLIB_TEST = [ -f /usr/bin/ranlib -o -f /bin/ranlib ]

EXTRA_LIBS=-lnsl -lsocket
EXTRA_LIBS_TEST=[ -f /usr/lib/libnsl.a -a -f /usr/lib/libsocket.a ]

LDFLAGS=
RM=rm -f
AR=ar
ARFLAGS=r
INC=-I$(ISEARCH_DIR)/src
LZ_OBJ=berutil.o ber.o zpdu.o zsession.o zsquery.o \
	unisock.o tcpbuf.o tcpstr.o tcpsock.o ipbuf.o zrecords.o userinfo.o \
	zclibase.o cgi.o httpbox.o 
#	marclib.o memcntl.o marc.o zclibase.o cgi.o httpbox.o 

LZ_H=berutil.h ber.hxx zpdu.hxx zsession.hxx unisock.hxx zsquery.hxx \
	tcpbuf.hxx tcpstr.hxx tcpsock.hxx ipbuf.hxx zrecords.hxx \
	zclibase.hxx userinfo.hxx ztags.hxx \
	cgi.h httpbox.h
#	marclib.h memcntl.h marc.hxx 

all:$(LZ_OBJ) $(LIB) zclient zserver zcon zgate izclient

memcntl.o:$(LZ_H) memcntl.c
	$(CC) $(CFLAGS) $(INC) -c memcntl.c

marc.o:$(LZ_H) marc.cxx
	$(CC) $(CFLAGS) $(INC) -c marc.cxx

marclib.o:$(LZ_H) marclib.c
	$(CC) $(CFLAGS) $(INC) -c marclib.c

berutil.o:$(LZ_H) berutil.c
	$(CC) $(CFLAGS) $(INC) -c berutil.c

ber.o:$(LZ_H) ber.cxx
	$(CC) $(CFLAGS) $(INC) -c ber.cxx
	
zpdu.o:$(LZ_H) zpdu.cxx
	$(CC) $(CFLAGS) $(INC) -c zpdu.cxx
	
zsession.o:$(LZ_H) zsession.cxx
	$(CC) $(CFLAGS) $(INC) -c zsession.cxx

zsquery.o:$(LZ_H) zsquery.cxx
	$(CC) $(CFLAGS) $(INC) -c zsquery.cxx

unisock.o:$(LZ_H) unisock.cxx
	$(CC) $(CFLAGS) $(INC) -c unisock.cxx

tcpbuf.o:$(LZ_H) tcpbuf.cxx
	$(CC) $(CFLAGS) $(INC) -c tcpbuf.cxx

tcpstr.o:$(LZ_H) tcpstr.cxx
	$(CC) $(CFLAGS) $(INC) -c tcpstr.cxx

tcpsock.o:$(LZ_H) tcpsock.cxx
	$(CC) $(CFLAGS) $(INC) -c tcpsock.cxx

ipbuf.o:$(LZ_H) ipbuf.cxx
	$(CC) $(CFLAGS) $(INC) -c ipbuf.cxx

zrecords.o:$(LZ_H) zrecords.cxx
	$(CC) $(CFLAGS) $(INC) -c zrecords.cxx
	
userinfo.o:$(LZ_H) userinfo.cxx
	$(CC) $(CFLAGS) $(INC) -c userinfo.cxx
	
zclibase.o:$(LZ_H) zclibase.cxx 
	$(CC) $(CFLAGS) $(INC) -c zclibase.cxx

cgi.o:cgi.c
	$(CC) $(CFLAGS) $(INC) -c cgi.c
 
httpbox.o:httpbox.c
	$(CC) $(CFLAGS) $(INC) -c httpbox.c

$(LIB):$(LIB_DIR)/$(LIB)
	@echo ""
 
$(LIB_DIR)/$(LIB):$(LZ_H) $(LZ_OBJ)
	$(AR) $(ARFLAGS) $(LIB_DIR)/$(LIB) $(LZ_OBJ)
	`if $(MYRANLIB_TEST) ; \
		then $(MYRANLIB) $(LIB_DIR)/$(LIB); else true; fi`

$(ISEARCH_LIB):$(LIB_DIR)/$(ISEARCH_LIB)
	@echo ""

$(LIB_DIR)/$(ISEARCH_LIB):
	cd $(ISEARCH_DIR);make

izclient:$(BIN_DIR)/izclient
	@echo ""

zclient:$(BIN_DIR)/zclient
	@echo ""

zserver:$(BIN_DIR)/zserver
	@echo ""

zcon:$(BIN_DIR)/zcon
	@echo ""

zgate:$(BIN_DIR)/zgate
	@echo ""

izclient.o:$(LZ_H) izclient.cxx 
	$(CC) $(CFLAGS) $(INC) -c izclient.cxx

zclient.o:$(LZ_H) zclient.cxx 
	$(CC) $(CFLAGS) $(INC) -c zclient.cxx

zserver.o:$(LZ_H) zserver.cxx zserver.hxx
	$(CC) $(CFLAGS) $(INC) -I$(LS) -c zserver.cxx

$(BIN_DIR)/zclient:$(LIB) zclient.o
	`if $(EXTRA_LIBS_TEST) ; \
	then $(CC) $(CFLAGS) $(LDFLAGS) -o $(BIN_DIR)/zclient zclient.o \
                -L$(LIB_DIR) -lz3950 -lIsearch -lm $(EXTRA_LIBS) ; \
	else $(CC) $(CFLAGS) $(LDFLAGS) -o $(BIN_DIR)/zclient zclient.o \
                -L$(LIB_DIR) -lz3950 -lIsearch -lm ; fi`
	
$(BIN_DIR)/izclient:$(LIB) izclient.o
	`if $(EXTRA_LIBS_TEST) ; \
	then $(CC) $(CFLAGS) $(LDFLAGS) -o $(BIN_DIR)/izclient izclient.o \
                -L$(LIB_DIR) -lz3950 -lIsearch -lm $(EXTRA_LIBS) ; \
	else $(CC) $(CFLAGS) $(LDFLAGS) -o $(BIN_DIR)/izclient izclient.o \
                -L$(LIB_DIR) -lz3950 -lIsearch -lm ; fi`
	
$(BIN_DIR)/zserver:$(LIB) zserver.o $(LIB_DIR)/$(ISEARCH_LIB)
	`if $(EXTRA_LIBS_TEST) ; \
	then $(CC) $(CFLAGS) $(LDFLAGS) -o $(BIN_DIR)/zserver zserver.o \
                -L$(LIB_DIR) -lsapi -lz3950 -lIsearch -lm $(EXTRA_LIBS) ; \
	else $(CC) $(CFLAGS) $(LDFLAGS) -o $(BIN_DIR)/zserver zserver.o \
		-L$(LIB_DIR) -lsapi -lz3950 -lIsearch -lm ; fi`

zcon.o:$(LZ_H) zcon.cxx 
	$(CC) $(CFLAGS) $(INC) -I$(LS) -c zcon.cxx

$(BIN_DIR)/zcon:$(LIB) zcon.o
	`if $(EXTRA_LIBS_TEST) ; \
	then $(CC) $(CFLAGS) $(LDFLAGS) -o $(BIN_DIR)/zcon zcon.o \
		-L$(LIB_DIR) -lz3950 -lIsearch -lm $(EXTRA_LIBS) ; \
	else $(CC) $(CFLAGS) $(LDFLAGS) -o $(BIN_DIR)/zcon zcon.o \
		-L$(LIB_DIR) -lz3950 -lIsearch -lm ; fi`

zgate.o:$(LZ_H) zgate.cxx
	$(CC) $(CFLAGS) $(INC) -c zgate.cxx

$(BIN_DIR)/zgate:$(LIB) zgate.o
	`if $(EXTRA_LIBS_TEST) ; \
	then $(CC) $(CFLAGS) $(LDFLAGS) -o $(BIN_DIR)/zgate zgate.o \
                -L$(LIB_DIR) -lz3950 -lIsearch -lm $(EXTRA_LIBS) ; \
	else $(CC) $(CFLAGS) $(LDFLAGS) -o $(BIN_DIR)/zgate zgate.o \
		-L$(LIB_DIR) -lz3950 -lIsearch -lm ; fi`

build:
	make -i clean;
	make

docs:
	doc++ -d ../doc *.cxx *.hxx

clean:
	$(RM) *.o core *~ $(LIB) $(BIN_DIR)/zclient $(BIN_DIR)/zserver \
		$(BIN_DIR)/zcon $(BIN_DIR)/zgate $(BIN_DIR)/izclient

