This is CGI.pm 2.21, an easy-to-use Perl5 library for writing World
Wide Web CGI scripts.

To install this module, cd to the directory that contains this README
file and type the following:

	perl Makefile.PL
	make
	make install

If this doesn't work for you, try:

	cp CGI.pm /usr/local/lib/perl5

If you have trouble installing CGI.pm because you have insufficient
access privileges to add to the perl library directory, you can still
use CGI.pm.  See the docs for details.

This module works with NT, Windows, Macintosh and VMS servers, although
it hasn't been tested as extensively as it should be.  See the docs
for notes on your particular platform.

The documentation for these modules is part of the CGI.pm itself.  It
can be read using the pod2man and pod2html programs that come with
perl5.001.  To convert them into manual page format, type something
like the following:

	pod2man CGI.pm > CGI.man

For your convenience, there's a man page already compiled.  The very
verbose HTML document that is used for online documentation can also
be found in cgi_docs.html.

A collection of examples demonstrating various CGI features and
techniques are in the directory "examples".

Online documentation of for CGI.pm, and notifications of new versions
can be found at:

	http://www-genome.wi.mit.edu/ftp/pub/software/WWW/cgi_docs.html

Many examples of CGI scripts of various degrees of complexity can be
found at:

	http://www-genome.wi.mit.edu/WWW/examples/Ch9/

Have fun, and let me know how it turns out!

Lincoln D. Stein
lstein@genome.wi.mit.edu
