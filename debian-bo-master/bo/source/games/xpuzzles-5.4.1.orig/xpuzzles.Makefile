#
#       Makefile
#
###
#
#  Copyright (c) 1994 - 97	David Albert Bagley, bagleyd@bigfoot.com
#
#                   All Rights Reserved
#
#  Permission to use, copy, modify, and distribute this software and
#  its documentation for any purpose and without fee is hereby granted,
#  provided that the above copyright notice appear in all copies and
#  that both that copyright notice and this permission notice appear in
#  supporting documentation, and that the name of the author not be
#  used in advertising or publicity pertaining to distribution of the
#  software without specific, written prior permission.
#
#  This program is distributed in the hope that it will be "useful",
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#

# if this fails to build one may have to edit the individual Imakefiles
# Please consult the individual README's.
# Also remember to put the appropriate puzzle.ad file in $HOME/puzzle
# or where ever you normally put these preferences.

# After ftping all the *.tar.gz files you want (each program is independent)
#	make -f xpuzzles.Makefile gunzip  # beware: destroys the original gz files
#	make -f xpuzzles.Makefile configure
#	make -f xpuzzles.Makefile
#	make -f xpuzzles.Makefile run     # this cycles through all programs

#SHELL=/bin/sh

XLOCK=more
#STUFF=abacus dial threed
ROTATIONAL=rubik skewb dino pyraminx oct mball
SLIDING=cubes triangles hexagons panex mlink
PUZZLES=${ROTATIONAL} ${SLIDING} ${STUFF}
ALTRIS=tetris tertris hextris welltris

#PRE=xlock
#NAME=xlockmore.
PRE=x
NAME=xpuzzles.
#PRE=al
#NAME=altris.

PROGRAMS=${PUZZLES}

# Used by me to zip and write and read from my floppy drive as a user
UNIXDIR=${HOME}/net/pending
MOUNT=eject -q floppy
#MOUNT=mntflop -d
UMOUNT=eject floppy
#UMOUNT=mntflop -u ; if [ -x /usr/bin/eject ]; then ; /usr/bin/eject floppy; fi
#DOSDIR=/dosa
DOSDIR=/floppy/floppy0
# Solaris, and Linux.  If there is interest, I will make it publicly available.
# If there is something out there already, let me know.

all :
	for i in ${PROGRAMS}; do\
		if [ -d ${PRE}$${i} ]; then\
			cd ${PRE}$${i};\
			make;\
			cd ..;\
		fi;\
	done

all.xm :
	for i in ${PROGRAMS}; do\
		if [ -d ${PRE}$${i} ]; then\
			cd ${PRE}$${i};\
			make all.xm;\
			cd ..;\
		fi;\
	done

autoconf :
	for i in ${PROGRAMS}; do\
		if [ -d ${PRE}$${i} ]; then\
			cd ${PRE}$${i};\
			make -f Makefile.in distclean;\
			autoconf;\
			cd ..;\
		fi;\
	done

configure :
	for i in ${PROGRAMS}; do\
		if [ -d ${PRE}$${i} ]; then\
			cd ${PRE}$${i};\
			make -f Makefile.in distclean;\
			configure;\
			cd ..;\
		fi;\
	done

xmkmf :
	for i in ${PROGRAMS}; do\
		if [ -d ${PRE}$${i} ]; then\
			cd ${PRE}$${i};\
			make -f Makefile.in distclean;\
			xmkmf;\
			cd ..;\
		fi;\
	done

cc :
	for i in ${PROGRAMS}; do\
		if [ -d ${PRE}$${i} ]; then\
			cd ${PRE}$${i};\
			make -f Makefile.in distclean;\
			configure --without-gcc;\
			cd ..;\
		fi;\
	done

lint :
	for i in ${PROGRAMS}; do\
		if [ -d ${PRE}$${i} ]; then\
			cd ${PRE}$${i};\
			make lint;\
			cd ..;\
		fi;\
	done

run :
	for i in ${PROGRAMS}; do\
		if [ -d ${PRE}$${i} ]; then\
			cd ${PRE}$${i};\
			./${PRE}$${i};\
			cd ..;\
		fi;\
	done

run.xm :
	for i in ${PROGRAMS}; do\
		if [ -d ${PRE}$${i} ]; then\
			cd ${PRE}$${i};\
			./xm$${i};\
			cd ..;\
		fi;\
	done

clean :
	for i in ${PROGRAMS}; do\
		if [ -d ${PRE}$${i} ]; then\
			cd ${PRE}$${i};\
			make -f Makefile.in clean;\
			cd ..;\
		fi;\
	done


distclean :
	for i in ${PROGRAMS}; do\
		if [ -d ${PRE}$${i} ]; then\
			cd ${PRE}$${i};\
			make -f Makefile.in distclean;\
			cd ..;\
		fi;\
	done

clean.all : distclean

tar :
	for i in ${PROGRAMS}; do\
		if [ -d ${PRE}$${i} ]; then\
			cd ${PRE}$${i};\
			make -f Makefile.in tar;\
			cd ..;\
		fi;\
	done

compress :
	for i in ${PROGRAMS}; do\
		if [ -d ${PRE}$${i} ]; then\
			cd ${PRE}$${i};\
			make -f Makefile.in compress;\
			cd ..;\
		fi;\
	done

gzip :
	cp ${NAME}README ${UNIXDIR}/${NAME}README;\
	cp ${NAME}Makefile ${UNIXDIR}/${NAME}Makefile;\
	cp ${NAME}lsm ${UNIXDIR}/${NAME}lsm;\
	for i in ${PROGRAMS}; do\
		if [ -d ${PRE}$${i} ]; then\
			cd ${PRE}$${i};\
			make -f Makefile.in gzip;\
			cd ..;\
			mv ${PRE}$${i}.tar.gz ${UNIXDIR};\
		fi;\
	done

tgz :
	${MOUNT};\
	cp ${NAME}README ${DOSDIR}/${NAME}rea;\
	cp ${NAME}Makefile ${DOSDIR}/${NAME}mak;\
	cp ${NAME}lsm ${DOSDIR}/${NAME}lsm;\
	for i in ${PROGRAMS}; do\
		if [ -d ${PRE}$${i} ]; then\
			cd ${PRE}$${i};\
			make -f Makefile.in tgz;\
			cd ..;\
			cp `echo ${PRE}$${i} | cut -c1-8`.tgz ${DOSDIR};\
		if [ -w ${DOSDIR}/`echo ${PRE}$${i} | cut -c1-8`.tgz ]; then\
				rm -f `echo ${PRE}$${i} | cut -c1-8`.tgz;\
			fi;\
		fi;\
	done;\
	${UMOUNT}

utar :
	for i in ${PROGRAMS}; do\
		tar xvf ${PRE}$${i}.tar;\
		rm -f ${PRE}$${i}.tar;\
	done

uncompress :
	for i in ${PROGRAMS}; do\
		uncompress ${PRE}$${i}.tar.Z;\
		tar xvf ${PRE}$${i}.tar;\
		rm -f ${PRE}$${i}.tar;\
	done

gunzip :
	for i in ${PROGRAMS}; do\
		gunzip ${PRE}$${i}*.tar.gz;\
		tar xvf ${PRE}$${i}*.tar;\
		rm -f ${PRE}$${i}*.tar;\
	done

utgz :
	for i in ${PROGRAMS}; do\
		if [ -r `echo ${PRE}$${i} | cut -c1-8`.tgz ]; then\
			gunzip -fN `echo ${PRE}$${i} | cut -c1-8`.tgz;\
			tar xvf ${PRE}$${i}.tar;\
			rm -f ${PRE}$${i}.tar;\
		fi;\
	done

extract :
	${MOUNT};\
	cp ${DOSDIR}/${NAME}rea ${NAME}README;\
	cp ${DOSDIR}/${NAME}mak ${NAME}Makefile;\
	cp ${DOSDIR}/${NAME}lsm ${NAME}lsm;\
	chmod 600 ${NAME}README ${NAME}Makefile ${NAME}lsm;\
	for i in ${PROGRAMS}; do\
		if [ -r ${DOSDIR}/`echo ${PRE}$${i} | cut -c1-8`.tgz ]; then\
			cp ${DOSDIR}/`echo ${PRE}$${i} | cut -c1-8`.tgz .;\
		fi;\
	done;\
	${UMOUNT}

read :
	for i in ${PROGRAMS}; do\
		if [ -d ${PRE}$${i} ]; then\
			cd ${PRE}$${i};\
			more README;\
			cd ..;\
		fi;\
	done

man :
	for i in ${PROGRAMS}; do\
		if [ -d ${PRE}$${i} ]; then\
			cd ${PRE}$${i};\
			nroff -man ${PRE}$${i}.man | more;\
			cd ..;\
		fi;\
	done

#print :

#install :
