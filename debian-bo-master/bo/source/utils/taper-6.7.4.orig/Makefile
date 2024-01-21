#
#   Time-stamp: <96/08/05 20:29:13 yusuf>
#
#   $Id: Makefile,v 1.31 1996/08/05 19:34:04 yusuf Exp $	


#All the user config stuff, including directories etc..
#is found in Makefile.common
#
#
include version
include Makefile.common




all: bins docs 

bins: taper bg_backup bg_restore

docs: taper.8

version.h:version
	echo "#define CUR_VERSION \""$(VERSION)"\"" > version.h
	grep "MS_MGC_VAL" /usr/include/linux/fs.h >> version.h

taper: version.h .depend $(TAPER_OBJS) $(COMMON_LIB) check_sd
	$(CC) $(CFLAGS) $(DEFINES) $(INCLUDE) -o taper $(TAPER_OBJS) \
		$(COMMON_LIB) $(COMPRESS_DIR)/$(COMPRESS_LIB) $(LIBPATH) \
		$(LINKLIB) -l$(FORM) -l$(CURSES) 

bg_backup: version.h .depend $(BG_BACKUP_OBJS) $(COMMON_LIB) check_sd
	$(CC) $(CFLAGS) $(DEFINES) $(INCLUDE) -o bg_backup \
	        $(BG_BACKUP_OBJS) $(COMPRESS_DIR)/$(COMPRESS_LIB) \
		$(COMMON_LIB) $(LIBPATH) $(LINKLIB) -l$(FORM) -l$(CURSES) 

bg_restore: version.h .depend $(BG_RESTORE_OBJS) $(COMMON_LIB) check_sd
	$(CC) $(CFLAGS) $(DEFINES) $(INCLUDE) -o bg_restore \
	        $(BG_RESTORE_OBJS) $(COMPRESS_DIR)/$(COMPRESS_LIB) \
		$(COMMON_LIB) $(LIBPATH) $(LINKLIB) -l$(FORM) -l$(CURSES) \

taper-static: version.h .depend $(TAPER_OBJS) $(COMMON_LIB) check_sd
	touch bg_backup bg_restore
	rm bg_backup bg_restore
	make bg_backup-static
	make bg_restore-static
	$(CC) $(CFLAGS) $(DEFINES) $(INCLUDE) -o taper-static $(TAPER_OBJS) \
		$(COMMON_LIB) $(COMPRESS_DIR)/$(COMPRESS_LIB) $(LIBPATH) \
		$(LINKLIB) -l$(FORM) -l$(CURSES) -static

bg_backup-static: version.h .depend $(BG_BACKUP_OBJS) $(COMMON_LIB) check_sd
	$(CC) $(CFLAGS) $(DEFINES) $(INCLUDE) -o bg_backup \
	        $(BG_BACKUP_OBJS) $(COMPRESS_DIR)/$(COMPRESS_LIB) \
		$(COMMON_LIB) $(LIBPATH) $(LINKLIB) -l$(FORM) -l$(CURSES) -static

bg_restore-static: version.h .depend $(BG_RESTORE_OBJS) $(COMMON_LIB) check_sd
	$(CC) $(CFLAGS) $(DEFINES) $(INCLUDE) -o bg_restore \
	        $(BG_RESTORE_OBJS) $(COMPRESS_DIR)/$(COMPRESS_LIB) \
		$(COMMON_LIB) $(LIBPATH) $(LINKLIB) -l$(FORM) -l$(CURSES) -static

$(COMMON_LIB): version.h .depend $(COMMON_OBJS)
	ar rc $(COMMON_LIB) $(COMMON_OBJS)
	ranlib $(COMMON_LIB)


check_sd: 
	make -C $(COMPRESS_DIR) $(COMPRESS_LIB)

taper.8: taper.txt
	cp taper.txt taper.8

taper.txt: taper.sgml
	sgml2txt taper

.c.o:
	$(CC) $(CFLAGS) $(CFLAGS_NOLNK) $(DEFINES) $(INCLUDE) -o $@ -c $<

install:
	install --mode 755 --strip taper $(BINDIR) 
	install --mode 755 --strip bg_backup $(BINDIR)
	install --mode 755 --strip bg_restore $(BINDIR)
	install --mode 644 taper.8 $(MANDIR)

uninstall:
	rm -rf	$(BINDIR)/taper \
		$(BINDIR)/bg_backup \
		$(BINDIR)/bg_restore \
		$(MANDIR)/taper.8 \
		$(MANDIR)/taper.8.gz 

clean:
	rm -rf core *.o taper taper-static .depend *~ taper.8 version.h  \
	       bg_backup bg_restore $(COMMON_LIB)
	make -C $(COMPRESS_DIR) clean


.depend:
	make depend

depend:
	$(GCC) -M $(INCLUDE) $(TAPER_SRCS) $(BG_BACKUP_SRCS) \
	     $(BG_RESTORE_SRCS) > .depend
	make -C $(COMPRESS_DIR) depend


dep: depend

ifeq (.depend, $(wildcard .depend))
include .depend
endif
#
#
#--for making distributions only --------------------------------------
#
INSTALL:INSTALL.raw
	sed -e "s/NEEDS_VERSION/$(VERSION)/" \
	    -e "s/NEEDS_DATE/$(DATE)/" INSTALL.raw > INSTALL

taper.sgml:taper.sgml.raw
	sed -e "s/NEEDS_VERSION/$(VERSION)/" \
	    -e "s/NEEDS_DATE/$(DATE)/" taper.sgml.raw > taper.sgml 

taper-$(VERSION).lsm:taper.lsm.raw
	sed -e "s/NEEDS_VERSION/$(VERSION)/" \
            -e "s/NEEDS_DATE/$(DATE)/" taper.lsm.raw > taper-$(VERSION).lsm


distrib:
	make clean
#move the raw files away
	cp INSTALL.raw ../INSTALL.raw
	cp taper.sgml.raw ../taper.sgml.raw
	cp taper.lsm.raw ../taper.lsm.raw
#make the files with correct dates/version
	make INSTALL
	make taper.sgml
	make taper-$(VERSION).lsm
#for those that don't have the linuxdoc package
	make taper.txt
#remove excess files from distribution
	touch ttt
	rm ttt INSTALL.raw taper.sgml.raw taper.lsm.raw
	ln -s taper.sgml taper.sgml.raw
#change to production makefile
	sed 	-e "s/-fno-common/-O2/"  \
		-e "s/DEVELOPMENT = TRUE/DEVELOPMENT = FALSE/" \
		-e "s/AOUT = TRUE/AOUT = FALSE/" \
		Makefile.common > mc
	cp mc Makefile.common
	rm mc
#make tar archive
	ln -s /home/yusuf/taper /home/yusuf/taper-$(VERSION)
	cd .. ; tar czf taper-$(VERSION).tar.gz taper-$(VERSION)/*
	rm /home/yusuf/taper-$(VERSION)
	cp *.lsm ../
#remove the corrected files
	rm INSTALL
	rm taper.sgml
	rm taper-$(VERSION).lsm
	rm taper.sgml.raw
#bring back this for the next version
#bring back my raw files
	mv ../INSTALL.raw INSTALL.raw
	mv ../taper.sgml.raw taper.sgml.raw
	mv ../taper.lsm.raw taper.lsm.raw
#back to development makefile
	sed 	-e "s/-O2/-fno-common/"  \
		-e "s/DEVELOPMENT = FALSE/DEVELOPMENT = TRUE/" \
		-e "s/AOUT = FALSE/AOUT = TRUE/" \
		Makefile.common > mc
	cp mc Makefile.common
	rm mc

backup:
	@echo "Have you committed?  (CTRL-C if you haven't)"
	@sleep 5
	mount /drivea
	tar czf /drivea/taper.cvs.tar.gz ~/cvs/taper/*
	umount /drivea


