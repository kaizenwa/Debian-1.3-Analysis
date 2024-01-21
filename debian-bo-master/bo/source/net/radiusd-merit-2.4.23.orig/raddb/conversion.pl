#! /usr/bin/perl
#
# [C] The Regents of the University of Michigan and Merit Network, Inc. 1992,
# 1993, 1994, 1995, 1996 All Rights Reserved
#
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted, provided
# that the above copyright notice and this permission notice appear in all
# copies of the software and derivative works or modified versions thereof,
# and that both the copyright notice and this permission and disclaimer
# notice appear in supporting documentation.
#
# THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER
# EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE REGENTS OF THE
# UNIVERSITY OF MICHIGAN AND MERIT NETWORK, INC. DO NOT WARRANT THAT THE
# FUNCTIONS CONTAINED IN THE SOFTWARE WILL MEET LICENSEE'S REQUIREMENTS OR
# THAT OPERATION WILL BE UNINTERRUPTED OR ERROR FREE.  The Regents of the
# University of Michigan and Merit Network, Inc. shall not be liable for any
# special, indirect, incidental or consequential damages with respect to any
# claim by Licensee or any third party arising from use of the software.
#
# Filename: conversion.pl
# Author:   Barry James

# Define some constants

$infile = "users.livingston";
$outfile = "users.merit";

#
# Read in Livingston style users file and change selected strings to
# match the DRAFT RADIUS RFC as implemented in Merit RADIUS.
#

open ( INFILE, $infile ) || die "Can't open $infile: $!\n";
open ( OFILE, ">$outfile" ) || die "Can't open $outfile: $!\n";

while ( <INFILE> )
{
	s/Challenge-State/State/;		# attribute 24 type string
	s/Client-Id/NAS-IP-Address/;		# attribute 4 type ipaddr
	s/Client-Port-Id/NAS-Port/;		# attribute 5 type int
	s/Dialback-Name/Framed-Callback-Id/;	# attribute 20 type string
	s/Dialback-No/Login-Callback-Number/;	# attribute 19 type string
	s/Framed-Address/Framed-IP-Address/;	# attribute 8 type ipaddr
	s/Jacobsen/Jacobson/;			# attribute 13 type int
	s/Framed-Netmask/Framed-IP-Netmask/;	# attribute 9 type ipaddr
	s/Framed-Filter-Id/Filter-Id/;		# attribute 11 type string
	s/Login-Host/Login-IP-Host/;		# attribute 14 type ipaddr
	s/Port-Message/Reply-Message/;		# attribute 18 type string
	s/User-Service-Type/Service-Type/;	# attribute 6 type int
	s/Login-User/Login/;
	s/Framed-User/Framed/;
	s/Dialback-Login-User/Callback-Login/;
	s/Dialback-Framed-User/Callback-Framed/;

	print OFILE "$_";

} # end while ( <INFILE> )
