#! /bin/bash

# Verbose shell execution.
# set -x

revext="$1"
kernel="$2"
shift 2
packages_with_modules="$*"

extractdir=/var/tmp/extract-tmp-$$

rm -rf $extractdir
mkdir $extractdir

echo kernel: $kernel
for i in $kernel $packages_with_modules; do
	dpkg-deb --extract $i $extractdir; \
done

if [ "$revext" = lowmem ]; then
	(cd $extractdir/lib/modules/*; 
	rm -rf block ipv? misc net pcmcia fs *MODULES )
fi

# creating modcont

(cd $extractdir;
	for i in lib/modules/*/*; do
		if [ -d  $i ]; then
			echo -n "dir_content_`basename $i`=\"";
			for j in $i/*.o; do
				echo -n `basename $j .o` " " ;
			done ;
			echo \" ;
		fi ;
	done
) > modcont$revext

# creating modules.tgz

(cd $extractdir;tar clf - lib/modules|gzip -9) > modules$revext.tgz


# creating rescmods.tgz
# not used anymore

if [ "$revext" = "" ]; then
	(cd $extractdir/lib/modules/*;
		rm -rf block ipv? misc net pcmcia fs scsi *MODULES )

	(cd $extractdir;
		echo -n "rescue_content=\"" ;
		for i in `find lib/modules -name \*.o`; do
			echo -n `basename $i .o` " ";
		done ;
		echo \" ;
	) >> modcont$revext

#	(cd $extractdir;tar clf - lib/modules |gzip -9) > rescmods$revext.tgz
fi

#

cp $extractdir/boot/vmlinuz* linux$revext
cat $extractdir/boot/System.map-* | gzip -9 > sys_map$revext.gz
rm -rf $extractdir
