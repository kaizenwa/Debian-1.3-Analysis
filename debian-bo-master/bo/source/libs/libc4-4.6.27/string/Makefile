#
# Makefile for string functions
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS=-I. -I$(TOPDIR) \
	-I$(TOPDIR)/sysdeps/$(TARGET_ARCH) \
	-I$(TOPDIR)/sysdeps/generic

DIRS:=
SRC1S= __memccpy.c bcmp.c bcopy.c \
	memcpy.c memfrob.c memmem.c memmove.c strcasecmp.c \
	strcoll.c strcpy.c strdup.c strerror.c strfry.c \
	strncscmp.c strncat.c strncpy.c strsep.c strsignal.c \
	strtok.c strxfrm.c swab.c # __stpncpy.c stpcpy.c strstr.c
# bzero.c ffs.c memchr.c memcmp.c memset.c strcat.c strchr.c strcmp.c
# strcspn.c strlen.c strncmp.c strpbrk.c strrchr.c strspn.c strstr.c.
# 
SRC2S= index.c memccpy.c rindex.c errlist.c siglist.c stpncpy.c

SRCS= $(SRC1S) $(SRC2S)
ASMS= $(SRC1S:.c=.s) $(SRC2S:.c=.s)
OBJS= $(SRC1S:.c=.o)
ALIASES= $(SRC2S:.c=.o)

include $(TOPDIR)/Maketargets  
