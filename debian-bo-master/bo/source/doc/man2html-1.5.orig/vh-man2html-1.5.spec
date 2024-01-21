Description:  Web browser Manual Page viewing.
Name: vh-man2html
Version: 1.5
Release: 1
Source: vh-man2html-1.5.tar.gz
Copyright: distributable
Vendor: Michael Hamilton <michael@actrix.gen.nz>.
Group: Networking/Utilities
Distribution: CND 1.0

%prep 
%setup

%build
make 

%install
make install

%postun

%files
%doc README
/home/httpd/html/man.html
/home/httpd/html/mansearch.html
/home/httpd/html/mansearchhelp.html
/home/httpd/cgi-bin/man2html
/home/httpd/cgi-bin/manwhatis
/home/httpd/cgi-bin/mansearch
/home/httpd/cgi-bin/mansec
/usr/bin/netscape-man
/usr/man/man8/man2html.8
/usr/man/man1/netscape-man.1
/var/man2html/.glimpse_filters

