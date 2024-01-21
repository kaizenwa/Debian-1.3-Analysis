#! make -f 
#
# Makefile for cdda2wav, a sampling utility.
#
CDDA2WAVVERSION=0.8
KERNEL_SRCDIR=/usr/src/linux

RM=rm -f
#CC=c89
CC=gcc

# If you get compile errors and you have
# incorrect/missing links from /usr/include to the
# the kernel include subdirectories, here is an explicit hint.
# We need the links for the linux/asm/scsi subdirectories, that you
# would get when doing
#
#  'ln -s /usr/src/linux/include/linux /usr/include/linux'
#  'ln -s /usr/src/linux/include/asm /usr/include/asm'
#  'ln -s /usr/src/linux/include/scsi /usr/include/scsi'
# and maybe also
#  'ln -s /usr/src/linux/include/net /usr/include/net'
#
# (this assumes your kernel sources reside in /usr/src/linux).
#
# okay, I give in...
# the following lines try to adjust themselves

have_correct_link := $(shell \
   if [ -h /usr/include/scsi ]; then echo 1; else echo 0; fi)
ifneq ($(have_correct_link),1)
      KERNEL_INCLUDE_ROOT=-I$(KERNEL_SRCDIR)/include
else
      KERNEL_INCLUDE_ROOT=
endif


############# interface
# choose one of the following interfacing and device types.
# the first is for scsi cdrom devices.
# uncomment one INTERFACE and one DEF_DEVICE line
# (for details consult the README)

INTERFACE=\"generic_scsi\"

				# set the default cdrom device
DEF_DEVICE=\"/dev/sgd\"
				# matching device
                                # used for a MULTISESSION ioctl
AUX_DEVICE=\"/dev/cdrom\"

# for nonscsi cdrom drives with cdda kernel support ie atapi, cdu31a
# choose this if the kernel driver supports cdda reading
#INTERFACE=\"cooked_ioctl\"

				# set the default cdrom device
#DEF_DEVICE=\"/dev/cdrom\"
				# not actually used
#AUX_DEVICE=\"/dev/cdrom\"


############# Sound support
#to disable sound support comment out the next line
HAVE_SOUND=-DECHO_TO_SOUNDCARD
				# soundcard device when monitoring
SOUND_DEVICE=\"/dev/dsp\"

############# sound file defaults
# Defaults for wav/sun/raw output file format
				# default divider for 44.1 KHz
DEF_UNDERSAMPLING=1

				# 16, 12 or 8 bits per sample
DEF_BITS=16
				# 1 = mono, 2 = stereo
DEF_CHANNELS=2

############## output file type: wav or sun au ##############
# enable one DEF_TYPE and one DEF_FILE
# first are for wav sound files, second are for sun au pcm sound files,
# third are for headerless sound files (raw samples in lowendian format)
				# default audio file type
DEF_TYPE=\"wav\"
#DEF_TYPE=\"sun\"
#DEF_TYPE=\"raw\"

				# default file name for sound file
DEF_FILE=\"audio\"

############## installation directories #####################################
MAN_DIRECTORY=/usr/local/man
BIN_DIR=/usr/local/bin
ROOT=root

############## enable/disable info files ####################################
# Info files contain date/time, track, recording time, ISRC and optional MD5 
# signature each item in one line. These files are generated one per track.
# Comment out if you don't want this.
WANT_INFOFILES=-DINFOFILES

############## enable/disable MD5 signatures in info files ##################
# Comment out if you don't want this.
WANT_MD5=-DMD5_SIGNATURES
MD5OBJECTS=md5c.o

# End of Configuration ######################################################

DEFINES=-DCD_DEVICE=$(DEF_DEVICE) -DUNDERSAMPLING=$(DEF_UNDERSAMPLING) \
-DBITS_P_S=$(DEF_BITS) -DCHANNELS=$(DEF_CHANNELS) -DAUDIOTYPE=$(DEF_TYPE) \
-DDURATION=$(DEF_TIME) -DFILENAME=$(DEF_FILE) -DDEF_INTERFACE=$(INTERFACE) \
$(HAVE_SOUND) -DSOUND_DEV=$(SOUND_DEVICE) -DVERSION=\"$(CDDA2WAVVERSION)\" \
$(WANT_INFOFILES) $(WANT_MD5) -DAUX_DEVICE=$(AUX_DEVICE)

WARNINGS=-Wall -Wtraditional -Wshadow -Wpointer-arith -Wcast-qual \
-Wcast-align -Wwrite-strings -Wstrict-prototypes -Wnested-externs # -Wconversion -pedantic 
#CFLAGS=$(WARNINGS) -g -pipe $(KERNEL_INCLUDE_ROOT)
CFLAGS=$(WARNINGS) -O6 -s -fomit-frame-pointer -finline-functions -fcaller-saves\
       -funroll-loops -pipe $(KERNEL_INCLUDE_ROOT)

#LDFLAGS=-g -static
LDFLAGS=-s

MODULES=cdda2wav.o interface.o semshm.o resample.o indexscan.o wav.o sun.o raw.o md5c.o setuid.o

.PHONY:	install clean dist uuencoded lint_it

cdda2wav: $(MODULES)
	$(CC) $(LDFLAGS) $(MODULES) -o cdda2wav

install: cdda2wav.1 cdda2wav
	chown root cdda2wav
	chmod 555 cdda2wav
	install -m a+x -o $(ROOT) cdda2wav $(BIN_DIR)
	cp cdda2wav.1 $(MAN_DIRECTORY)/man1

# build distribution package
dist cdda2wav$(CDDA2WAVVERSION).src.tar.gz:
	mkdir cdda2wav$(CDDA2WAVVERSION)
	cp -a README Makefile scan_scsi uti.h \
	interface.h cdda2wav.h cdda2wav.c interface.c  semshm.c share.h \
	sun.c sun.h wav.c wav.h raw.c raw.h resample.c resample.h indexscan.c \
	indexscan.h md5.h md5c.c setuid.c setuid.h byteorder.h \
	GPL README.INSTALL HOWTOUSE THANKS \
	cdda2wav.1 cdda2wav.doc cdda2wav$(CDDA2WAVVERSION)
	tar cfvz cdda2wav$(CDDA2WAVVERSION).src.tar.gz \
	cdda2wav$(CDDA2WAVVERSION)

uuencoded: cdda2wav$(CDDA2WAVVERSION).src.tar.gz
	uuencode $< <$< >$<.uue

cdda2wav.o: Makefile cdda2wav.c wav.h sun.h uti.h interface.h cdda2wav.h \
	share.h resample.h indexscan.h md5.h setuid.h byteorder.h
	$(CC) $(CFLAGS) $(DEFINES)	-c cdda2wav.c

cdda2wav.s: Makefile cdda2wav.c wav.h sun.h uti.h interface.h cdda2wav.h \
	share.h resample.h indexscan.h md5.h setuid.h byteorder.h
	$(CC) $(CFLAGS) $(DEFINES)	-S cdda2wav.c

interface.o: Makefile interface.c interface.h cdda2wav.h share.h \
	setuid.h byteorder.h uti.h
	$(CC) $(CFLAGS) $(DEFINES)	-c interface.c

indexscan.o: Makefile indexscan.h indexscan.c cdda2wav.h
	$(CC) $(CFLAGS) $(DEFINES)	-c indexscan.c

semshm.o: Makefile semshm.c share.h
	$(CC) $(CFLAGS) $(DEFINES)	-c semshm.c

resample.o: Makefile byteorder.h resample.c interface.h cdda2wav.h
	$(CC) $(CFLAGS) $(DEFINES)	-c resample.c

wav.o: wav.c Makefile byteorder.h uti.h wav.h
	$(CC) $(CFLAGS) $(DEFINES)	-c wav.c

sun.o: sun.c Makefile byteorder.h uti.h sun.h
	$(CC) $(CFLAGS) $(DEFINES)	-c sun.c

raw.o: raw.c uti.h

md5c.o: md5c.c md5.h
	$(CC) $(CFLAGS)                 -c md5c.c

setuid.o: setuid.c setuid.h
	$(CC) $(CFLAGS)                 -c setuid.c

clean:
	$(RM) *.o *~ cdda2wav

lint_it:
	lclint $(DEFINES) +posixlib +skip-sys-headers +trytorecover +gnuextensions *.c

