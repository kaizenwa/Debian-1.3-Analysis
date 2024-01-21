Description: Midnight Commander visual shell.
Name: mc
Version: 3.2.9
Release: 1
Copyright: GPL
Group: Shells
Source:ftp.nuclecu.unam.mx:/Midnight/mc-3.2.9.tar.gz
BuildRoot: /tmp/mc-root
%description
Midnight Commanders is a visual shell much like a file manager, only
with way more features.  It is text mode, but also includes mouse
support if you are running GPM.  It's coolest feature is the ability
to ftp, view tar files and poke into RPMs for specific files.  :-)

%prep

%setup

%build
CFLAGS="$RPM_OPT_FLAGS" ./configure --prefix=/usr 

make

%install
mkdir -p $RPM_BUILD_ROOT/usr
make prefix=$RPM_BUILD_ROOT/usr install
strip $RPM_BUILD_ROOT/usr/bin/mc $RPM_BUILD_ROOT/usr/bin/mcserv

%post
if grep "# Midnight Commander invoking function" /etc/profile >/dev/null 2>&1; then
	:
else
	if grep "mc.*()" /etc/profile >/dev/null 2>&1; then
		:
	else
		echo >> /etc/profile
		echo "# Midnight Commander invoking function">> /etc/profile
		echo 'mc () { MC=`/usr/bin/mc -P "$@"`; [ -n "$MC" ] && cd "$MC"; unset MC }'>> /etc/profile
		echo >> /etc/profile
	fi
fi

if [ -f /sbin/pamconfig ]; then
    /sbin/pamconfig --add --service=mcserv --authlist='required /lib/security/pam_unix_auth.so ""'
fi

%postun
if [ -f /sbin/pamconfig ]; then
    /sbin/pamconfig --remove --service=mcserv --authlist='required /lib/security/pam_unix_auth.so ""'
fi

%files
/usr/bin/mc
/usr/bin/mcserv
/usr/man/man1/mc.1
/usr/man/man1/mcedit.1
/usr/man/man8/mcserv.8
/usr/lib/mc
%doc FAQ
