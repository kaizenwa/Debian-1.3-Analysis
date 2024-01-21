include ./Makeconfig

include ./Makerules

LIBS=

ifndef OPTFLAGS
OPTFLAGS= -O2	
endif

ifndef DAEMON
DAEMON = -DDAEMON
endif

SUBDIR=	domainname ypwhich ypcat ypmatch yppoll ypset # ypbind yppasswd

all: subdir

subdir:
	for d in $(SUBDIR); do \
	  ($(MAKE) -C $$d $(TARGET)) || exit 1; \
	done

clean: FRC
	$(RM) -f *.a *.o *~ core
	$(MAKE) TARGET=clean subdir

install: subdir
	$(MAKE) TARGET=install subdir
	#install -o root -g root -m 0644 yp.conf.example ${DEBDIR}/etc/yp.conf 
	install -o root -g root -m 0755 -d ${DEBDIR}/var/yp


FRC:
