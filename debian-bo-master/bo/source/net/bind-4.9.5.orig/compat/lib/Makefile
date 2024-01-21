#
#	from @(#)Makefile	5.16 (Berkeley) 3/14/88
#	$Id: Makefile,v 8.5 1996/06/02 08:20:33 vixie Exp $
#

## ++Copyright++ 1988
## -
## Copyright (c) 1988
##    The Regents of the University of California.  All rights reserved.
## 
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions
## are met:
## 1. Redistributions of source code must retain the above copyright
##    notice, this list of conditions and the following disclaimer.
## 2. Redistributions in binary form must reproduce the above copyright
##    notice, this list of conditions and the following disclaimer in the
##    documentation and/or other materials provided with the distribution.
## 3. All advertising materials mentioning features or use of this software
##    must display the following acknowledgement:
## 	This product includes software developed by the University of
## 	California, Berkeley and its contributors.
## 4. Neither the name of the University nor the names of its contributors
##    may be used to endorse or promote products derived from this software
##    without specific prior written permission.
## 
## THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
## ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
## IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
## ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
## FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
## DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
## OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
## HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
## LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
## OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
## SUCH DAMAGE.
## -
## Portions Copyright (c) 1993 by Digital Equipment Corporation.
## 
## Permission to use, copy, modify, and distribute this software for any
## purpose with or without fee is hereby granted, provided that the above
## copyright notice and this permission notice appear in all copies, and that
## the name of Digital Equipment Corporation not be used in advertising or
## publicity pertaining to distribution of the document or software without
## specific, written prior permission.
## 
## THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS ALL
## WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES
## OF MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL EQUIPMENT
## CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
## DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
## PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
## ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
## SOFTWARE.
## -
## --Copyright--

DESTDIR =
DESTLIB = /usr/lib

CC= cc
SHELL= /bin/sh
CDEBUG= -O
INCL = ../../include
COMPINCL = ../../compat/include
DEFS=	
CDEBUG= -O
CFLAGS=	${CDEBUG} -I${INCL} -I${COMPINCL} ${DEFS}
AR= ar cru
RANLIB= ranlib

AROBJS= ${ARPREF} ${OBJS} ${ARSUFF}

SRCS=	mktemp.c strcasecmp.c strerror.c strpbrk.c strtoul.c strdup.c \
	putenv.c setenv.c setitimer.c writev.c ftruncate.c gettimeofday.c

OBJS=	mktemp.o strcasecmp.o strerror.o strpbrk.o strtoul.o strdup.o \
	putenv.o setenv.o setitimer.o writev.o ftruncate.o gettimeofday.o

all: lib44bsd.a

lib44bsd.a: ${OBJS}
	${AR} lib44bsd.a ${AROBJS}
	$(RANLIB) lib44bsd.a

install: ${DESTDIR}${DESTLIB}/lib44bsd.a

${DESTDIR}${DESTLIB}/lib44bsd.a: lib44bsd.a
	${INSTALL} -c -o bin -g bin -m 644 lib44bsd.a ${DESTDIR}${DESTLIB}/
	( cd ${DESTDIR}${DESTLIB} ; $(RANLIB) lib44bsd.a )

.c.o:
	${CC} ${CPPFLAGS} ${CFLAGS} -c $*.c
	-${LDS} ld -x -r $*.o
	${LDS} mv a.out $*.o

clean: FRC
	rm -f errs a.out core lib44bsd.a tags
	rm -f *.o *.BAK *.CKP *~

depend: FRC
	mkdep -I${INCL} -I${COMPINCL} ${DEFS} ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.
