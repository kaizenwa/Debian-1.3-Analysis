#!/bin/sh
for i in *.tcl
do
	mv $i $i.old
	/bin/sed "s/$1//g" $i.old > $i
done
