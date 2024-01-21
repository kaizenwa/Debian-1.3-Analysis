Summary: programs to access SMB network servers
Name: smbfs
Version: 2.0.1
Release: 1
Copyright: GPL
Group: Utilities/File
Source: ftp://sunsite.unc.edu/pub/linux/system/Filesystems/smbfs/smbfs-2.0.1.tgz
Patch: smbfs-make
ExclusiveArch: i386
BuildRoot: /tmp/ksmbfs-root
%description
This package includes the tools necessary to mount filesystems from SMB
servers.

%prep
%setup
%patch

%build
make

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/usr/sbin $RPM_BUILD_ROOT/usr/man/man8
make install BINDIR=$RPM_BUILD_ROOT/usr/sbin MANDIR=$RPM_BUILD_ROOT/usr/man

%clean
rm -rf $RPM_BUILD_ROOT

%files
/usr/sbin/smbmount
/usr/sbin/smbumount
/usr/man/man8/smbmount.8
/usr/man/man8/smbumount.8
%doc README COPYING Changes FAQ smbfs-2.0.1.lsm
%doc smbfs-2.0.28-2.diff

