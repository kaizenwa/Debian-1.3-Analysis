#!/bin/sh
cd /usr
ls include/*.h include/sys/*.h | sed -e "
s/\./_/g
s%/%_%g
y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/
s%INCLUDE_%#define HAVE_%g
"

# | tr [a-z] [A-Z]
