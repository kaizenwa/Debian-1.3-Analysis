#! /bin/sh

if [ -x /etc/cron.daily/find ]; then
	echo -n "Updating locate database ... "
	/etc/cron.daily/find
	echo "done."
fi

rm -f /root/setup.sh
exit 0
