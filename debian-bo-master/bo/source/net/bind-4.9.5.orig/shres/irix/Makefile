# $Id: Makefile,v 8.2 1996/09/22 00:13:32 vixie Exp $

CFLAGS=	-O -I../${INCL} -I../${COMPINCL} ${DEFS} ${LOCDEFS} 

LOCDEFS= -DUSE_OPTIONS_H

OBJS=	base64.o herror.o res_debug.o res_data.o \
	res_comp.o res_init.o res_mkquery.o res_query.o res_send.o \
	getnetbyaddr.o getnetbyname.o getnetent.o getnetnamadr.o \
	gethnamaddr.o sethostent.o nsap_addr.o inet_pton.o inet_ntop.o \
	inet_addr.o

all: libresolv.so

$(OBJS):
	${SHCC} ${CFLAGS} ${PIC} -D_REENTRANT -c $? -o $@

clean: FRC
	rm -f errs a.out core libresolv.so tags *.o *.BAK *.CKP *~ *.orig \
	so_locations

depend: 
	@echo No dependencies in `pwd`

install: ${DESTDIR}${DESTLIB}/libresolv.so.2

${DESTDIR}${DESTLIB}/libresolv.so.2: libresolv.so
	${INSTALL} -c -o bin -g bin -m 755 libresolv.so \
	 ${DESTDIR}${DESTLIB}/libresolv.so.2
	rm -f ${DESTDIR}${DESTLIB}/libresolv.so
	ln -s libresolv.so.2 ${DESTDIR}${DESTLIB}/libresolv.so

FRC:

libresolv.so: ${OBJS}
	LD_OPTIONS="-h libresolv.so.2" $(SHLD) -o $@ $(OBJS) $(LIBS)

base64.o: ../../res/base64.c
herror.o: ../../res/herror.c
res_comp.o: ../../res/res_comp.c
res_debug.o: ../../res/res_debug.c
res_data.o: ../../res/res_data.c
res_init.o: ../../res/res_init.c
res_mkquery.o: ../../res/res_mkquery.c
res_query.o: ../../res/res_query.c
res_send.o: ../../res/res_send.c
getnetbyaddr.o: ../../res/getnetbyaddr.c
getnetbyname.o: ../../res/getnetbyname.c
getnetent.o: ../../res/getnetent.c
getnetnamadr.o: ../../res/getnetnamadr.c
gethnamaddr.o: ../../res/gethnamaddr.c
sethostent.o: ../../res/sethostent.c
nsap_addr.o: ../../res/nsap_addr.c
inet_addr.o: ../../res/inet_addr.c
inet_ntop.o: ../../res/inet_ntop.c
inet_pton.o: ../../res/inet_pton.c
