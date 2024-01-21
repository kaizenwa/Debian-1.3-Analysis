# =()<@<INCLUDE>@>()=
.include "../include/config.make"

.SUFFIXES:	.fake .h

.fake.h:
	cp $< $@

ready:	$(HFAKE) dbz.h

dbz.h:	../libdbz/dbz.h
	cp ../libdbz/dbz.h $@

clean:
	rm -f `ls | sed -n '/\.fake$$/s//.h/p'`
	( cd sys ; rm -f `ls | sed -n '/\.fake$$/s//.h/p'` )
	rm -f dbz.h
