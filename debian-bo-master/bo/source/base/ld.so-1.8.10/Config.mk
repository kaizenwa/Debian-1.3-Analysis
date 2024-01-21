ARCH = i386
#ARCH = m68k
#ARCH = sparc
#DEBUG = true

ifeq ($(ARCH),sparc)
AOUT_SUPPORT = false
else
AOUT_SUPPORT = true
endif

LDSO_ADDR = 62f00020
LDSO_ENTRY = "0x$(LDSO_ADDR)"

# Do NOT use -fomit-frame-pointer -- It won't work!
CFLAGS	= -Wall -O4 -g -DVERSION=\"$(VERSION)\"
ifeq ($(DEBUG),true)
CFLAGS  += -DDEBUG
endif

CC = gcc
AS = as
LD = ld
RANLIB = ranlib

ifeq ($(ARCH),i386)
AOUTCC = /usr/i486-linuxaout/bin/gcc
#AOUTCC = gcc -b i486-linuxaout
AOUTLD = /usr/i486-linuxaout/bin/ld -m i386linux
endif
ifeq ($(ARCH),m68k)
AOUTCC = /usr/m68k-linuxaout/bin/gcc
AOUTLD = /usr/m68k-linuxaout/bin/ld -m m68klinux
endif

