all:

install:
	(cd prototype; tar cf - .)|(umask 0; cd debian/tmp; tar xf -)
	(umask 0;mv debian/tmp/root/dot.bash_profile debian/tmp/root/.bash_profile)
	(umask 0;mv debian/tmp/root/dot.bashrc debian/tmp/root/.bashrc)

clean:
