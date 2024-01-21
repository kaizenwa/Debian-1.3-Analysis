#!/bin/sh

# shell script to dial in using dip
# and place a dynamaically assigned IP address
# into /etc/hosts
# /tmp/local_name is a file with my local machine name
# /etc/hosts.back is the default /etc/hosts file with
# 127.0.0.1 localhost as the only entry
# host command gets name of ip address
#  - Blair Sandberg <bes38869@uxa.cso.uiuc.edu>

dip /sbin/dynamic-ip.dip 2>&1
sleep 5
ifconfig | fgrep P-t-P | cut -c21-35 > /tmp/ip
host `cat /tmp/ip` | fgrep Name | cut -c7- > /tmp/ip_name
paste /tmp/ip /tmp/ip_name /tmp/local_name > /tmp/host_bottom
cat /etc/hosts.bak /tmp/host_bottom > /etc/hosts
rm /tmp/ip
rm /tmp/ip_name
rm /tmp/host_bottom
