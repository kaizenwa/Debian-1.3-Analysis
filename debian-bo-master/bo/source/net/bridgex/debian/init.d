#! /bin/sh
# /etc/init.d/bridge: Start or Stop bridge operation
# This is a script for Debian style management of the bridge
# It will also work with any other distribution on its own.
# NO_RESTART_ON_UPGRADE

test -x /sbin/brcfg || exit 0

# Here all interfaces to be bridged should be listed
INTERFACES="eth0 eth1"

case "$1" in
  start)
	for i in $INTERFACES; do ifconfig $i up promisc; done
	brcfg start
# Use the following to just bridge strange protocols. Use the more
# efficient routing for localtalk and tcp/ip traffic
#	brcfg start exempt 802_3 802_2 ip arp
    ;;
  stop)
	brcfg stop
	for i in $INTERFACES; do ifconfig $i -promisc; done
    ;;
  *)
    echo "Usage: /etc/init.d/bridgex {start|stop}"
    exit 1
esac

exit 0
