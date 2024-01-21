#ident	   "@(#)bind/shres/linux:$Id: Makefile,v 8.5 1996/09/22 00:13:32 vixie Exp $"

CFLAGS=	${CDEBUG} -I../${INCL} -I../${COMPINCL} ${DEFS} ${LOCDEFS}
# What is USE_OPTIONS_H for? -u@q.net
LOCDEFS= -DUSE_OPTIONS_H
SRCS=	base64.c herror.c res_debug.c res_data.c \
	res_comp.c res_init.c res_mkquery.c res_query.c res_send.c \
	getnetbyaddr.c getnetbyname.c getnetent.c getnetnamadr.c \
	gethnamaddr.c sethostent.c nsap_addr.c inet_ntop.c inet_pton.c \
	inet_addr.c strerror.c

OBJS=	base64.o herror.o res_debug.o res_data.o \
	res_comp.o res_init.o res_mkquery.o res_query.o res_send.o \
	getnetbyaddr.o getnetbyname.o getnetent.o getnetnamadr.o \
	gethnamaddr.o sethostent.o nsap_addr.o inet_ntop.o inet_pton.o \
	inet_addr.o strerror.o

all: libresolv.so

$(OBJS):
	${SHCC} $(CPPFLAGS) ${CFLAGS} ${PIC} -c $? -o $@

clean: clean-makeshlib-stuff
	rm -f errs a.out core libresolv_pic.a tags *.o *.so *.BAK *.CKP *~ *.orig

clean-makeshlib-stuff: FRC
	rm -rf sysvtmp ucbtmp
	rm -f lorder.sysvtmp lorder.ucbtmp libc*.so.* libc*.sa.*

depend: 
	@echo No dependancies in `pwd`

install: ${DESTDIR}${DESTLIB}/libresolv.so.${SHVER}

${DESTDIR}${DESTLIB}/libresolv.so.$(SHVER): libresolv.so
	${INSTALL} -c -o bin -g bin -m 755 libresolv.so \
	 ${DESTDIR}${DESTLIB}/libresolv.so.$(SHVER)
	ldconfig

FRC:

libresolv.so: ${OBJS}
	$(SHLD) -o $@ $(OBJS)

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
strerror.o: ../../compat/lib/strerror.c
