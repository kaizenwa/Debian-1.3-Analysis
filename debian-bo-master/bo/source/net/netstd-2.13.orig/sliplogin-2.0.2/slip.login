#!/bin/sh -
#
#	@(#)slip.login	5.1 (Berkeley) 7/1/90

#
# generic login file for a slip line.  sliplogin invokes this with
# the parameters:
#     1        2      3      4         5          6        7     8-n
#  slipunit ttyspeed pid loginname local-addr remote-addr mask options
#
/sbin/ifconfig $1 $5 pointopoint $6 mtu 1500 arp -trailers up
/sbin/route add $6 $1
#in case you have an ethernet card this will announce the slip client
#xx:xx:xx:xx has to be replaced by your hardware address
#/sbin/arp -s $6 xx:xx:xx:xx pub
exit 0
