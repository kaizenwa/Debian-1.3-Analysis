#
# Makefile for SUN rpc functions
#
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

# very special flags. gcc is broken on the builtin ffs ().
VSCFLAGS=-fno-builtin

INC_CFLAGS=-I$(TOPDIR)/include
BASE_CFLAGS:= $(BASE_CFLAGS) $(VSCFLAGS)


DIRS:=

SRCS = 	auth_des.c authdes_prot.c key_call.c key_prot.c netname.c \
	svcauth_des.c openchild.c rtime.c xcrypt.c \
	auth_none.c auth_unix.c auth_prot.c \
	clnt_generic.c clnt_perror.c clnt_raw.c clnt_simple.c \
	clnt_tcp.c clnt_udp.c rpc_dtblsz.c get_myaddr.c \
	getrpcport.c pmap_clnt.c pmap_getmaps.c \
	pmap_getport.c pmap_prot.c pmap_prot2.c pmap_rmt.c \
	rpc_prot.c rpc_comdata.c rpc_callmsg.c svc.c svc_auth.c \
	svc_au_ux.c svc_raw.c svc_run.c svc_simple.c \
	svc_tcp.c svc_udp.c xdr.c xdr_array.c xdr_float.c xdr_mem.c \
	xdr_rec.c xdr_ref.c xdr_stdio.c

HDRS =	key_prot.h auth_des.h \
	auth.h auth_unix.h clnt.h netdb.h pmap_clnt.h pmap_prot.h \
	pmap_rmt.h rpc.h rpc_msg.h svc.h svc_auth.h types.h xdr.h

ifneq ($(NYS),true)
SRCS+=getrpcent.c publickey.c
endif

ASMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)
ALIASES= 

include $(TOPDIR)/Maketargets  
