#

TOPDIR=..

CHECKER_LIB_DIR=/usr/local/lib/checker/

include $(TOPDIR)/Makeconfig

libc.a: alwaysredo
	$(RM) -f libc.a; \
	$(MKDIR) tmpcopy; \
	cd tmpcopy; \
	$(AR) -x ../../libalias.a; \
	$(AR) ucv ../libc.a *.o; \
	cd ..; $(RM) -rf tmpcopy;
	cd libc; \
	$(AR) ucv ../libc.a *.o;

libm.a: alwaysredo
	$(RM) -f libm.a; \
	$(MKDIR) tmpcopy; \
	cd tmpcopy; \
	$(AR) -x ../../libmalias.a; \
	$(AR) ucv ../libm.a *.o; \
	cd ..; $(RM) -rf tmpcopy;
	cd libm; \
	$(AR) ucv ../libm.a *.o; 

prepare.inst: libm.a libc.a
	cp libgcc/libgcc3.a libgcc.a
	$(REALRANLIB) *.a

install: prepare.inst
	for i in *.a; do \
	 cp $$i $(CHECKER_LIB_DIR)/`basename $$i .a`.chkr.a ; \
	done

alwaysredo:

.PHONY: alwaysredo
