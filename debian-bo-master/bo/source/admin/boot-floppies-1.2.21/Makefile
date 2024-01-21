architecture=$(shell dpkg --print-architecture)

all::	build

# Edit the variables below this line

# Customize this to refer to the path of the Debian archive.
ftp_archive=/home/ftp/pub/debian
archive=$(ftp_archive)/stable/binary-$(architecture)
incoming=$(ftp_archive)/Incoming
tools_dir=$(ftp_archive)/tools
kernel_dir=$(ftp_archive)/project/other_kernels

kernel=$(archive)/base/kernel-image-2.0.29_2.0.29-7.deb
kerneltecra=$(kernel_dir)/kernel-image-2.0.29_2.0.29-7_tecrai386.deb
kernel_version=$(shell echo $(kernel) | sed -e 's/.*kernel-image-//' | \
			sed -e 's/_.*//')
pcmcia_package=$(shell ls $(archive)/admin/pcmcia-modules-$(kernel_version)_*.deb | tail -1)
pcmcia_package=/home/sr1/debian/Incoming/pcmcia-modules-2.0.29-7_2.9.6-2_i386.deb

packages_with_modules=$(pcmcia_package)

# disable this if you already have these files in the FTP mirror
localfiles: updates #$(archive)/base/modconf_0.2.12.deb

# dependency for the files that have to be copied
updates::
	mkdir -p updates
	cp -av $(ftp_archive)/bo-updates/*.deb \
		$(incoming)/pcmcia-modules-2.0.29-7_2.9.6-1_i386.deb \
		$(incoming)/pcmcia-modules-2.0.30-7_2.9.6-1_i386.deb \
		$(incoming)/REJECT/sysklogd_1.3-16_i386.deb \
		updates/

# don't edit below this line (but look for XXX)

debianversion=1.3

base_archive=base$(shell echo ${debianversion} | sed -e 's/\./_/').tgz

build::	localfiles
	$(MAKE) all_subdirs
	find . -name \*~ | xargs rm -f
	$(MAKE) resc1440.bin resc1200.bin \
		drv1440.bin drv1200.bin ${base_archive}
	$(MAKE) lmemroot.bin
	$(MAKE) tecra

all_subdirs:
	(cd utilities;make)
	(cd documentation;make)



release:: all
	rm -rf release
	mkdir -p release
	cp documentation/*.{txt,html} release
	rm release/*.in.html
	cp -r *.bin base*.tgz tecra release/
	mkdir /var/tmp/mnt.$$
	mount -o loop release/resc1440.bin /var/tmp/mnt.$$
	cp /var/tmp/mnt.$$/linux release/
	umount /var/tmp/mnt.$$
	rmdir /var/tmp/mnt.$$
	unix2dos <scripts/dos/install.bat >release/install.bat
	( cd release; unzip $(tools_dir)/rawrite2.zip rawrite2.{exe,txt} )
	( cd release; unzip -p $(tools_dir)/lodlin16.zip \
		LODLIN16/LOADLIN.EXE >loadlin.exe )
	( cd release; md5sum `find . -type f | grep -v md5sum.txt` \
			> md5sum.txt)


stamp-build: build

clean: distclean

install: stamp-build
	mkdir -m 755 -p $(prefix)/usr/src/boot-floppies
	tar cf - `echo * | sed 's:debian::'` \
	 | (cd $(prefix)/usr/src/boot-floppies;tar xf -)

distclean:
	(cd utilities;make distclean)
	(cd documentation;make distclean)
	rm -f *.bin *.tgz sys_map*.gz  linux* modcont* core *~
	rm -rf release tecra updates

tecra::	tecra/resc1440.bin tecra/drv1440.bin

modules.tgz sys_map.gz linux \
modcont: $(packages_with_modules) kernel.sh $(kernel) Makefile
	./kernel.sh "" $(kernel) $(packages_with_modules)

modulestecra.tgz sys_maptecra.gz linuxtecra \
modconttecra: $(packages_with_modules) kernel.sh $(kerneltecra) Makefile
	./kernel.sh tecra $(kerneltecra) $(packages_with_modules)

drv1440.bin: modules.tgz drivers.sh Makefile \
		$(shell find scripts/drivers -type f )
	./drivers.sh "" 1440 modules.tgz 

tecra/drv1440.bin: modulestecra.tgz drivers.sh Makefile \
		$(shell find scripts/drivers -type f )
	./drivers.sh tecra 1440 modulestecra.tgz 
	mkdir -p tecra
	mv -f drv1440tecra.bin tecra/drv1440.bin

drv1200.bin: modules.tgz drivers.sh Makefile \
		$(shell find scripts/drivers -type f )
	./drivers.sh "" 1200 modules.tgz

root.bin:	$(shell find scripts/rootdisk -type f ) \
		$(shell find utilities -type f ) \
		$(shell find $(archive)) \
		rootdisk.sh modcont Makefile
	./rootdisk.sh "" ${ftp_archive} ${archive} 1800 ${debianversion}

lmemroot.bin:	$(shell find scripts/rootdisk -type f ) \
		$(shell find utilities -type f ) \
		$(shell find $(archive)) \
		rootdisk.sh modcont Makefile
	./rootdisk.sh lowmem ${ftp_archive} ${archive} 1200 ${debianversion}
	mv -f rootlowmem.bin lmemroot.bin

resc1440.bin:	linux sys_map.gz rescue.sh Makefile \
		root.bin $(shell find scripts/rescue -type f )
	./rescue.sh "" $(kernel) root.bin 1440 ${debianversion}

tecra/resc1440.bin:	linuxtecra sys_maptecra.gz rescue.sh Makefile \
		root.bin $(shell find scripts/rescue -type f )
	./rescue.sh tecra $(kerneltecra) root.bin 1440 ${debianversion}
	mkdir -p tecra
	mv -f resc1440tecra.bin tecra/resc1440.bin

resc1200.bin:	linux sys_map.gz rescue.sh Makefile \
		$(shell find scripts/rescue -type f )
	./rescue.sh "" ${kernel} root.bin 1200 ${debianversion}

${base_archive}:	basedisks.sh \
			$(shell find scripts/basedisks -type f ) \
			$(shell find ${archive} -type f )
	./basedisks.sh ${archive} ${debianversion}

sign:
	pgp +clearsig=on -fast <release/md5sum.txt >release/md5sum.tmp
	mv release/md5sum.tmp release/md5sum.txt
