Summary: GIT - GNU Interactive Tools
Name: git
Version: 4.3.16
Release: 1
Copyright: GPL
Group: Utilities/File
Source: prep.ai.mit.edu:/pub/gnu/git-4.3.16.tar.gz

%description
GIT is a set of interactive tools.  It contains an extensible file
system browser, an ascii/hex file viewer, a process viewer/killer and
some other related utilities and shell scripts.  It can be used to
increase the speed and efficiency of most of the daily tasks such as
copying and moving files and directories, invoking editors,
compressing and uncompressing files, creating and expanding archives,
compiling programs, sending mail, etc.

%prep
%setup

%build
CFLAGS="$RPM_OPT_FLAGS" ./configure --prefix=/usr
make
make info

%install
rm -f /usr/info/git.info*
umask 022
make install-strip
gzip -9nf /usr/info/git.info*

%files
/usr/bin/git
/usr/bin/gitps
/usr/bin/gitview
/usr/bin/gitkeys
/usr/bin/gitwipe
/usr/bin/gitmount
/usr/bin/gitaction
/usr/bin/gitrgrep
/usr/bin/gitxgrep
/usr/bin/gitregrep
/usr/bin/gitrfgrep
/usr/bin/.gitaction

/usr/man/man1/git.1
/usr/man/man1/gitaction.1
/usr/man/man1/gitmount.1
/usr/man/man1/gitkeys.1
/usr/man/man1/gitrgrep.1
/usr/man/man1/gitps.1
/usr/man/man1/gitview.1

/usr/info/git.info*

/usr/lib/git
