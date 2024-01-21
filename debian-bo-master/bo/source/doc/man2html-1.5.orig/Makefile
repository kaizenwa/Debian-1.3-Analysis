#
# vh-man2html
# -----------
#
# Makefile
#                                                             Michael Hamilton 
#                                                        michael@actrix.gen.nz
# ----------------------------------------------------------------------------
# Set these to your desires...
# They over ride the defaults built into the C code and scripts.
#
# CGIBASE is defined relative to the current server.
# If CGIBASE starts with a relative "http:..." address (ie not "http://.."),
# man2html will insert the httpd's server name (as reported by the 
# SERVER_NAME environment variable) into server any redirects it generates
#  ie a redirect of        location: http:/cgi-bin/...
#     will be returned as  location: http://server_name/cgi-bin/...
# This is so that remote browsers will receive the necessary server 
# address on redirects.
#
# Installation configuration:
#
CACHE_DIR  = /var/man2html
CGI_BIN    = cgi-bin
CGI_DIR    = $(HTTP_HOME)/$(CGI_BIN)
CGIBASE    = http:/$(CGI_BIN)/man2html
HTTP_HOME  = /home/httpd
HTML_DIR   = $(HTTP_HOME)/html
OWNER      = root.root
BIN_DIR    = /usr/bin
MAN_DIR1   = /usr/man/man1
MAN_DIR8   = /usr/man/man8

#
# Man config file as per Linux man-1.4g.
# Comment out if there is no man.config file - in which case you will
# have to edit man2html.c to alter the man path.
#
MAN_CONFIG = /etc/man.config

#
# Man2html Uses popen with man.config decompressor info to decompress man
# source.  The popen parameters are checked for possible hacking attempts,
# and strings passed to man2html are prevented from over-running memory.
# Uncomment the following to disable this feature.  This makes the cgi 
# program more secure - i.e. no popen() calls.
#
# DISABLE_ZCATS = TRUE

#
# If the following is defined the man2html -M option can be used to point
# man2html at man pages outside of the normal hierarchy.  By default (in 
# version 1.4 and above) -M only allows the user to select what part of 
# the hierarchy to search first - they cannot specify a path outside of
# the official hierarchy.
#
# UNSECURE_MANPATH = TRUE
#

#
# Programs used:
#
AWK_PROG   = /usr/bin/awk
MAN_PROG   = /usr/bin/man
FIND_PROG  = /usr/bin/find
EGREP_PROG = /usr/bin/egrep


#
# Compiler options:
# -fno-strength-reduce is in case this gcc has a bug.
#
DEFINES = -DTOPLEVELDOC='"$(TOPLEVELDOC)"' -DCGIBASE='"$(CGIBASE)"' -DMAN_CONFIG='"$(MAN_CONFIG)"'
#
CFLAGS = -O2 -fno-strength-reduce $(DEFINES)
#CFLAGS = -O0 -g -fno-strength-reduce $(DEFINES)

#
# Installer program:
#
INSTALL = install


# UNLESS YOU WANT TO CHANGE THE RPM CONFIG, NOTHING BELOW THIS LINE NEEDS
# EDITING.
# ----------------------------------------------------------------------------
#
# The name of the cwd must be PACKAGENAME in order to build an rpm
#
# The following are used to construct source and binary rpm's.
RPM_PROG = rpm-1.4
PACKAGENAME = vh-man2html-1.5
TARFILE =  $(PACKAGENAME).tar.gz
SPECFILE = $(PACKAGENAME).spec
SPECLOC = /usr/src/SPECS
SOURCELOC = /usr/src/SOURCES
#
# The following commands will edit the scripts - works on freshly untar'ed
# source only (ie can't be undone by reapplication).
#
SED_SCRIPT = \
 -e s?/home/http/cgi-bin?$(CGI_DIR)?g \
 -e s?/home/http/html?$(HTML_DIR)?g \
 -e s?/var/man2html?$(CACHE_DIR)?g \
 -e s?/usr/bin/awk?$(AWK_PROG)?g \
 -e s?/usr/bin/man?$(MAN_PROG)?g \
 -e s?/usr/bin/find?$(FIND_PROG)?g \
 -e s?/usr/bin/egrep?$(EGREP_PROG)?g
#
TOPLEVELDOC = $(HTML_DIR)/man.html
#
#
INCLUDES =
BROWSER = netscape-man
SCRIPTS = manwhatis mansec mansearch
HTML_FILES =  man.html mansearch.html mansearchhelp.html
SERVER_FILES = man2html $(SCRIPTS)
DIST_FILES = $(BROWSER) $(SCRIPTS) $(HTML_FILES) man2html.8 netscape-man.1 


targets: man2html $(DIST_FILES)

man2html: man2html.c
	$(CC) $(CFLAGS) $< -o $@

$(DIST_FILES): Makefile $(addprefix dist/,$(DIST_FILES))
	sed $(SED_SCRIPT) dist/$@ > $@

install: targets
	strip man2html; \
	for i in $(SERVER_FILES); do \
	  $(INSTALL) $$i $(CGI_DIR) ; \
	  chown $(OWNER) $(CGI_DIR)/$$i ; \
	done ; \
	for i in $(HTML_FILES); do \
	  $(INSTALL) $$i $(HTML_DIR) ; \
	  chown $(OWNER) $(HTML_DIR)/$$i ; \
	done ; \
	$(INSTALL) $(BROWSER)         $(BIN_DIR) ; \
	$(INSTALL) man2html.8         $(MAN_DIR8) ; \
	$(INSTALL) netscape-man.1     $(MAN_DIR1) ; \
	if [ \! -d $(CACHE_DIR) ] ; then \
		mkdir $(CACHE_DIR) ; chmod o=rwxt $(CACHE_DIR); \
	fi; \
	cp glimpse_filters $(CACHE_DIR)/.glimpse_filters

rpm: spotless
	(cd ..; tar cvzf $(SOURCELOC)/$(TARFILE) $(PACKAGENAME) ) ; \
	cp $(SPECFILE) $(SPECLOC)/$(SPECFILE)
	$(RPM_PROG) -ba -v $(SPECFILE)

clean:
	-rm *.o *~

spotless: clean
	-rm man2html
	-rm `ls dist`


