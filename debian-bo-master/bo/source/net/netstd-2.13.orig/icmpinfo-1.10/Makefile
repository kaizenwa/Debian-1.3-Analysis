#
# Minimal makefile for 'icmpinfo'
#   a tool to look at the icmp you receive
#   modified from BSD ping by Laurent Demailly
#				<dl@hplyot.obspm.fr>
#
# see man page & README (Installation section at least)

VERS = 1.10

# You may need to add libraries here :
# like for Solaris, uncomment next line :
#LDLIBS= -lsocket -lnsl

# To override default compiler flags :
#CFLAGS=-g

# To change default compiler
#CC=gcc

RM = rm -f

LDFLAGS= $(CFLAGS)

OBJECTS= recvping.o print.o err.o icmpinfo.o
TARGET = icmpinfo

$(TARGET):  $(OBJECTS)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) $(LDLIBS)

tgz: clean
	rm -f CHECKSUMS.asc
	md5sum * > ../CHECKSUMS
	mv ../CHECKSUMS .
	pgp -asw CHECKSUMS
	chmod 444 CHECKSUMS.asc
	cd .. ; tar cvf icmpinfo-$(VERS).tar icmpinfo-$(VERS) ; gzip icmpinfo-$(VERS).tar

clean:
	$(RM) $(OBJECTS) $(TARGET) core *~

