
SUBDIRS=	en_US.88591 fr_FR.88591
GENCATNEW=	gencat -new
GENCAT=		gencat

catalogs:
	@if [ -f ../config.h ]; then \
	        YESNO=`cat ../config.h | grep "[define][ ]*[NLS]" | awk '{ print $$3 }'`; \
		if [ "$$YESNO" = "1" ]; then \
			echo "Making catalogs ..."; \
			for i in ${SUBDIRS} ; \
			do \
			    echo "$$i"; \
			    cd $$i; \
			    cat *.m > ../nettools.m; \
			    if [ -f nettools.cat ]; then \
			    	${GENCAT} nettools.cat ../nettools.m; \
			    else \
				${GENCATNEW} nettools.cat ../nettools.m; \
			    fi; \
			    cd ..;\
			done; \
			echo "Making NLS header files ..."; \
			${GENCATNEW} /dev/null nettools.m -h ../nettools-nls.h; \
		fi \
	fi
clean:
	@for i in ${SUBDIRS} ; \
	do \
	    rm -f $$i/*~; \
	    rm -f $$i/nettools.cat nettools.m ; \
	done; \
	rm -f ../nettools-nls.h
