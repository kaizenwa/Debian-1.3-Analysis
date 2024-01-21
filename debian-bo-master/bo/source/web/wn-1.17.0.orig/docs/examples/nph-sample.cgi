#!/usr/local/bin/perl

# Simple example of CGI script.

print "HTTP/1.0 200 Request fulfilled\r\n";
print "Content-type: text/html\r\n"; # First line must specify content type
print "Pragma: no-cache\r\n";
print "Last-modified: Friday, 10-Sep-95 14:30:33 GMT\r\n";
print "\r\n";                         #Followed by a blank line

# From now on everything goes to the client

print "<body>\n";
print "<h2>Here are some standard CGI environment variables:</h2>\n\n";

print "PATH_INFO = $ENV{PATH_INFO}<br>\n";
print "SERVER_SOFTWARE = $ENV{SERVER_SOFTWARE}<br>\n";
print "SERVER_NAME = $ENV{SERVER_NAME}<br>\n";
print "SERVER_PROTOCOL = $ENV{SERVER_PROTOCOL}<br>\n";
print "SERVER_PORT = $ENV{SERVER_PORT}<br>\n";
print "HTTP_ACCEPT = $ENV{HTTP_ACCEPT}<br>\n";
print "HTTP_REFERER = $ENV{HTTP_REFERER}<br>\n";
print "HTTP_USER_AGENT = $ENV{HTTP_USER_AGENT}<br>\n";
print "HTTP_FROM = $ENV{HTTP_FROM}<br>\n";
print "HTTP_HOST = $ENV{HTTP_HOST}<br>\n";
print "PATH_TRANSLATED = $ENV{PATH_TRANSLATED}<br>\n";
print "SCRIPT_NAME = $ENV{SCRIPT_NAME}<br>\n";
print "QUERY_STRING = $ENV{QUERY_STRING}<br>\n";
print "REMOTE_HOST = $ENV{REMOTE_HOST}<br>\n";
print "REMOTE_ADDR = $ENV{REMOTE_ADDR}<br>\n";
print "<p>\n";

print "<h2>Non-CGI variables provided by the WN server:</h3>\n";

print "WN_ROOT = $ENV{WN_ROOT}<br>\n";
print "WN_DIR_PATH = $ENV{WN_DIR_PATH}<br>\n";
print "</body>\n";
