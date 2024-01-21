#
# etc/Makefile 1.26 1996/12/31 18:37:35 (David Hinds)
#

# Include site dependent options
include ../config.mk

scripts = config network pcmem memory serial ftl scsi fixed cdrom shared
options = config.opts network.opts pcmem.opts memory.opts \
	serial.opts ftl.opts scsi.opts fixed.opts cdrom.opts

ifdef SYSV_INIT
INST = install-sysv
else
INST = install-bsd
endif

# Don't change this without editing the device-specific scripts
ETC = /etc/pcmcia

all:	$(scripts) $(options)

dep:

clean:

R=$(PREFIX)$(RC_DIR)
RC_DIRS=$R/init.d $R/rc0.d $R/rc2.d $R/rc3.d $R/rc5.d $R/rc6.d
$(PREFIX)/etc/rc.d $(PREFIX)/etc/sysconfig $(PREFIX)/etc/pcmcia $(RC_DIRS):
	mkdir -p $@


install-bsd: $(PREFIX)/etc/rc.d
	RC=$(PREFIX)/etc/rc.d/rc.pcmcia ;			\
	if [ -r $$RC ] ; then RC=$$RC.N ; fi ;			\
	sed -e s/=i82365/=`../cardmgr/probe -m`/ rc.pcmcia > $$RC ;\
	chmod +x $$RC


install-sysv: $(PREFIX)/etc/sysconfig $(RC_DIRS)
	CFG=$(PREFIX)/etc/sysconfig/pcmcia ;			\
	if [ ! -f $$CFG ] ; then				\
	    echo PCMCIA=yes > $$CFG ;				\
	    echo PCIC=`../cardmgr/probe -m` >> $$CFG ;		\
	    echo PCIC_OPTS= >> $$CFG ;				\
	    echo CORE_OPTS= >> $$CFG ;				\
	fi
	cp rc.pcmcia $(PREFIX)$(RC_DIR)/init.d/pcmcia
	chmod +x $(PREFIX)$(RC_DIR)/init.d/pcmcia
	for RC in $(PREFIX)$(RC_DIR)/rc[06].d ; do		\
	    if [ ! -e $$RC/*pcmcia ] ; then			\
		ln -sf ../init.d/pcmcia $$RC/K52pcmcia ;	\
	    fi ;						\
	done
	for RC in $(PREFIX)$(RC_DIR)/rc[235].d ; do		\
	    if [ ! -e $$RC/*pcmcia ] ; then			\
		ln -sf ../init.d/pcmcia $$RC/S45pcmcia ;	\
	    fi ;						\
	done

install: $(INST) $(PREFIX)$(ETC)
	if [ -r $(PREFIX)/etc/sysconfig/pcmcia-scripts ] ; then	\
	    mv $(PREFIX)/etc/sysconfig/pcmcia-scripts $(PREFIX)$(ETC) ; \
	fi
	for f in $(scripts) ; do				\
	    DEST=$(PREFIX)$(ETC)/$$f ;				\
	    if [ -r $$DEST ] ; then				\
		cmp -s $$f $$DEST && continue ;			\
	    fi ;						\
	    cp -b -Vt $$f $$DEST ; 				\
	done
	for f in $(options) ; do				\
	    DEST=$(PREFIX)$(ETC)/$$f ;				\
	    test -r $$DEST || cp $$f $$DEST ;			\
	done
