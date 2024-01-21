#
# This is Makefile of inet lib for Linux
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

ifeq ($(NYS),true)
YPFLAGS=-DYP
endif

ifeq ($(YP),true)
VSCFLAGS=-DNIS
endif

INC_CFLAGS= -I. -I$(TOPDIR)
BASE_CFLAGS := $(BASE_CFLAGS) $(VSCFLAGS) -DUSE_OPTIONS_H=1 

DIRS:=
ifeq ($(NYS),)
SRCS = gethstnmad.c getnetbyad.c getnetbynm.c getnetent.c \
	getproto.c getprtent.c getprtname.c getservent.c \
	getsrvbynm.c getsrvbypt.c herror.c inet_addr.c \
	inet_lnaof.c inet_mkadr.c inet_net.c inet_netof.c \
	inet_ntoa.c rcmd.c res_comp.c res_debug.c res_init.c res_data.c \
	res_mkquery.c res_query.c res_send.c rexec.c ruserpass.c \
	sethostent.c bindresvport.c nsap_addr.c \
	__h_errno_loc.c __res_loc.c
else
SRCS= herror.c rexec.c rcmd.c ruserpass.c bindresvport.c \
	inet_lnaof.c inet_mkadr.c inet_net.c inet_netof.c inet_ntoa.c \
	inet_addr.c res_data.c \
	res_comp.c res_debug.c res_init.c res_mkquery.c res_send.c \
	res_query.c nsap_addr.c \
	__h_errno_loc.c __res_loc.c
endif
ASMS= $(SRCS:.c=.s) 
OBJS= $(SRCS:.c=.o)
ALIASES=

include $(TOPDIR)/Maketargets
