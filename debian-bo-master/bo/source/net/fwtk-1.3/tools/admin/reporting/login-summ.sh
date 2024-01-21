#!/bin/sh

echo 'Count    User      What/Where                Date'
echo '#######################################################'
last | sed -e '/wtmp begins/d' -e '/^$/d' | \
	awk '{ printf("    %s\n",substr($0,0,47)); }' | \
	sort -r | uniq -c
