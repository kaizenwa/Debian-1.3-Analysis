#
# libbsd	A collection of library modules similar or equal to their
#		BSD UNIX equivalents, to make the job of porting BSD pro-
#		grams to LINUX easier.
#
# Author:	Rick Sladkey, <jrs@world.std.com>
#

override STATIC=true
override SHARED=false
override DEBUG=false
override PROFILE=false
#override CHECKER=false

ifeq ($(CHECKER),true)
override STATIC=false
endif

# That is fake for SUBDIR.
JUMP_LIB=libbsd

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

override STATIC_LIB=$(STATIC_DIR)/libbsd.a
override CHECKER_LIB=$(CHECKER_DIR)/libbsd.a

INC_CFLAGS= -I$(CROSS_INCDIR)/bsd -I$(CROSS_INCDIR)  \
	-include $(CROSS_INCDIR)/bsd/bsd.h

DIRS:=
SRCS= daemon.c logwtmp.c logout.c pty.c login_tty.c \
	  sgtty.c cfsetspeed.c revoke.c setpassent.c \
	  checkrhosts.c select.c
# snprintf.c strsep.c roundup.c getusersh.c
ASMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)
 
include $(TOPDIR)/Maketargets
