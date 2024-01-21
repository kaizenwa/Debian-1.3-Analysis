include ../Makeconfig

#----------------------------------------------------------------------

all install-bin clean realclean dep depend:

install install-man:
	for i in *.[1-8] ; do \
	  ext=`echo $$i | sed 's/.*\.//'`; \
	  $(INSTALL) -m 444 $$i $(MANDIR)/man$$ext/; \
	done
