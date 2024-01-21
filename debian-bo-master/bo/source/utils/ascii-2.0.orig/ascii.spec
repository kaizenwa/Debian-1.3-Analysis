Description: interactive ASCII name and synonym chart
Name: ascii
Version: 2.0
Release: 1
Source: locke.ccil.org:/pub/esr/ascii-2.0.tar.gz
Copyright: distributable
Group: Utilities/Text

%prep
%setup

%build
make

%install
rm -f /usr/bin/ascii
cp ascii /usr/bin
cp ascii.1 /usr/man/man1/ascii.1

%files
/usr/man/man1/ascii.1
/usr/bin/ascii
