HOWTO=/usr/doc/HOWTO/Module-HOWTO.gz

all:	descr.gz #eval.gz

install:
	cp modconf $(DESTDIR)/usr/sbin
	chmod 755 $(DESTDIR)/usr/sbin/modconf
	chown root.root $(DESTDIR)/usr/sbin/modconf
	mkdir -p -m 755 $(DESTDIR)/usr/lib/module_help
	cp {eval,descr.gz} $(DESTDIR)/usr/lib/module_help/

descr.gz:	descr.additional $(HOWTO) mkdescr.pl
	rm -f descr
	if [ -f descr.additional ]; then cp descr.additional descr; fi
	echo >>descr
	echo >>descr
	zcat $(HOWTO) | perl mkdescr.pl >> descr
	gzip -9f descr

#eval.gz:	eval
#	cat eval| sed -e 's/"$$/";/'| gzip -9 >eval.gz

clean:
	rm -f debian/*~ *~ descr.gz descr eval.gz
