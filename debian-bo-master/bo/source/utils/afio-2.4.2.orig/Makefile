SHELL=/bin/sh
##
##  I wrote this Makefile, based on comments in the source.  -rich $alz.
##  Define INDEX to use index() in place of strchr() (v7, BSD).
#1	= -UINDEX
##  Define MEMCPY when an efficient memcpy() exists (SysV).
2	= -DMEMCPY
##  Define MKDIR when a mkdir() system call is present (4.2BSD, SysVr3).
3	= -DMKDIR
##  Define NOVOID if your compiler doesn't like void casts.
#4	= -UNOVOID
##  Define SYSTIME to use <sys/time.h> rather than <time.h> (4.2BSD).
#5	= -USYSTIME
##  Define VOIDFIX to allow pointers to functions returning void (non-PCC).
6	= -DVOIDFIX
##  Define CTC3B2 to support AT&T 3B2 streaming cartridge tape.
#7	= -UCTC3B2
##  Define HAVEFCNTL if you have <fcntl.h>
8	= -DHAVEFCNTL
##  Define MYTEMPNAM if you don't have tempnam()
#a	= -UMYTEMPNAM
##  Define UNIXPC if you are on a 3b1, 7300, etc.
##	(problem is you can't write to a floppy from shared memory)
#b	= -UUNIXPC
##  Define HAVEMEMCMP if you have memcmp otherwise assumes bcmp
c	= -DHAVEMEMCMP
##  Define DEFFMTCMD to being how to format the media you use the most
##		This is the DEFault FoRMat CoManD. 
d	= -DDEFFMTCMD='"fdformat /dev/fd0H1440"'
##  Define LONGZFILE if you want .Z to be tagged on the end of a 14 char
##	file name (or longer for BSD) in the archive when the file is compressed
#e	= -DLONGZFILE

##  Define PRG_COMPRESS to get something other than `gzip'.
# you need to edit compfile.c (-G option) if you change this line.
f	= -DPRG_COMPRESS='"gzip"'

## Define HAVEFNMATCH if you want to use the gnu fnmatch() routine for
# -y -Y -w -W matching.
# If it is not defined, a primitive replacement match routine is used that
# only supports patterns of the form "cccc" and "cccc*".  Make sure that
# you change the manual page in this case.

g	= -DHAVEFNMATCH

# fnmatch() is in the gnu C library, so it is directly available on
# Linux.  If your system (e.g. SCO) does not have the gnu C library,
# unpack the archive gnu.fnmatch.tar.gz and uncomment the following
# two lines:

#M	 = fnmatch.o
#I	 = -I.

# Please read the COPYING.LIB file in this archive if you plan to
# redistribute afio executables with this library linked in.

CFLAGS1 = -s -O2 -fomit-frame-pointer
#CFLAGS1 =  -fomit-frame-pointer
#CFLAGS1 = -Wall  -fomit-frame-pointer

CC=gcc


CFLAGS = ${CFLAGS1} $1 $2 $3 $4 $5 $6 $7 $8 $9 $a $b $c $d $e $f $g $I
LDFLAGS = -s

afio : afio.o compfile.o exten.o match.o $M
	${CC} ${LDFLAGS} afio.o compfile.o exten.o match.o $M -o afio

clean:
	rm -f *.o afio

install: afio
	cp afio /usr/bin
	cp afio.1 /usr/man/man1

tarfile: clean
	{ cd .. ; tar zcvf /root/src/tbackup/afio.2.4.2.tgz afio.2.4.2 }

afio.o : afio.h patchlevel.h
compfile.o : afio.h
exten.o : afio.h
match.o : afio.h

