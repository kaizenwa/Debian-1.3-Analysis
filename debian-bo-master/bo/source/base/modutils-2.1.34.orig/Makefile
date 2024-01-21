all install-bin clean realclean dep depend:
	$(MAKE) -C util $@
	$(MAKE) -C obj $@
	$(MAKE) -C insmod $@
	$(MAKE) -C genksyms $@
	$(MAKE) -C depmod $@
	$(MAKE) -C kerneld $@

install-man:
	$(MAKE) -C man $@

install: install-bin install-man
