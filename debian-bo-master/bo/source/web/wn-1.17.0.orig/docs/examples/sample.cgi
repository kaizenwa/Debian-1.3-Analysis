#!/usr/local/bin/perl

# Simple example of CGI script.

print "Content-type: text/html\r\n"; # The first line must specify content type

print "\r\n";                        # A blank line ends the headers

# From now on everything goes to the client

print "<body>\n";
print "<h2>Here are some standard CGI environment variables:</h2>\n\n";

print "PATH_INFO = $ENV{PATH_INFO}<br>\n";
print "AUTH_TYPE = $ENV{AUTH_TYPE}<br>\n";
print "SERVER_SOFTWARE = $ENV{SERVER_SOFTWARE}<br>\n";
print "SERVER_NAME = $ENV{SERVER_NAME}<br>\n";
print "SERVER_PROTOCOL = $ENV{SERVER_PROTOCOL}<br>\n";
print "SERVER_PORT = $ENV{SERVER_PORT}<br>\n";
print "HTTP_ACCEPT = $ENV{HTTP_ACCEPT}<br>\n";
print "HTTP_ACCEPT_CHARSET = $ENV{HTTP_ACCEPT_CHARSET}<br>\n";
print "HTTP_ACCEPT_LANGUAGE = $ENV{HTTP_ACCEPT_LANGUAGE}<br>\n";
print "HTTP_RANGE = $ENV{HTTP_RANGE}<br>\n";
print "HTTP_REFERER = $ENV{HTTP_REFERER}<br>\n";
print "HTTP_USER_AGENT = $ENV{HTTP_USER_AGENT}<br>\n";
print "HTTP_FROM = $ENV{HTTP_FROM}<br>\n";
print "HTTP_HOST = $ENV{HTTP_HOST}<br>\n";
print "HTTP_COOKIE = $ENV{HTTP_COOKIE}<br>\n";
print "PATH_TRANSLATED = $ENV{PATH_TRANSLATED}<br>\n";
print "SCRIPT_NAME = $ENV{SCRIPT_NAME}<br>\n";
print "QUERY_STRING = $ENV{QUERY_STRING}<br>\n";
print "REMOTE_HOST = $ENV{REMOTE_HOST}<br>\n";
print "REMOTE_ADDR = $ENV{REMOTE_ADDR}<br>\n";
print "REQUEST_METHOD = $ENV{REQUEST_METHOD}<br>\n";
print "<p>\n";

print "<h2>Non-CGI variables provided by the WN server:</h3>\n";

print "URL_SCHEME = $ENV{URL_SCHEME}<br>\n";
print "WN_ROOT = $ENV{WN_ROOT}<br>\n";
print "WN_DIR_PATH = $ENV{WN_DIR_PATH}<br>\n";
print "HTTP_POST_FILE = $ENV{HTTP_POST_FILE}<br>\n";
print "</body>\n";
exit(0);