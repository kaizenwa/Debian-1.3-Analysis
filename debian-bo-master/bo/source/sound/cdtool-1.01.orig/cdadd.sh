#!/bin/sh
# "cdadd" copyright 1994 thomas insel
/usr/bin/cdir -t > /tmp/cdadd-$$
vi /tmp/cdadd-$$
cat /tmp/cdadd-$$ >>~/.cdtooldb
rm /tmp/cdadd-$$
