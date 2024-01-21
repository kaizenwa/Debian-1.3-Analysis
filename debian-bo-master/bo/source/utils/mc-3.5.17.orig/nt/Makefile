#
# This makefile is for making the distribution on Unix
# You won't be able to compile with this one the Midnight Commander
# on Windows NT, use Makefile.NT instead.
#
# Please read the file README.NT in the top directory of the distribution
#

include ../Make.common

FILES =	Makefile TODO dirent.c mc.ico bugs getopt.c mc.rc config.h \
	getopt.h dirent.h Makefile.NT chmod.nt.c drive.nt.c slint.nt.c \
	cons.handler.nt.c key.nt.c utilnt.c

all:

TAGS: $(FILES)
	etags $(FILES)

clean realclean cross:

install:

uninstall:

depend dep:

fastdeploc:

distcopy:
	$(CP) $(FILES) ../../mc-$(VERSION)/nt
