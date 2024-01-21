#!/bin/sh
#
# Copyright (c) 1997 Thorsten Kukuk, Germany
#
# Author: Thorsten Kukuk <kukuk@vt.uni-paderborn.de>
#
# $Id: BUILD,v 1.4 1997/02/06 21:03:10 kukuk Exp $

if [ ! "$arch" -a ! "$os" -a ! "$rel" ]
then
        arch=`uname -m | sed -e 's/ //g'`
        os=`uname -s | sed -e 's/\//-/g' -e 's/ //g'`
        rel=`uname -r | sed -e 's/(/-/g' -e 's/)//g'`
else
	echo "ERROR: Could not determine system !"
fi

echo "Configuration: arch=$arch, os=$os, rel=$rel"

if [ ! -d $arch.$os-$rel ]
then
   mkdir $arch.$os-$rel
fi

cd $arch.$os-$rel

../configure --prefix=/usr --libexecdir=/usr/lib --sysconfdir=/etc

if make
then
	echo ""
	echo ""
	echo "ypserv is compiled successfully."
	echo ""
	echo "change in the directory $arch.$os-$rel, type make install for" 
	echo "installing and edit /var/yp/Makefile, if this is a NIS master" 
	echo "host"
else
	echo "Error when compiling ypserv ..."
fi
