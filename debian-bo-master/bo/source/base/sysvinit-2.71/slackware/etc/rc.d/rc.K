#! /bin/sh
#
# rc.K 		This file is executed by init when it goes into runlevel
#		1, which is the administrative state. It kills all
#		deamons and then puts the system into single user mode.
#		Note that the file systems are kept mounted.
#
# Version:	@(#)/etc/rc.d/rc.K	1.50	1994-01-18
# Version:	@(#)/etc/rc.d/rc.K	1.60	1995-10-02 (PV)
#
# Author:	Miquel van Smoorenburg <miquels@cistron.nl>
# Modified by:  Patrick J. Volkerding <volkerdi@ftp.cdrom.com>
#

  # Set the path.
  PATH=/sbin:/etc:/bin:/usr/bin

  # Kill all processes.
  echo
  echo "Sending all processes the TERM signal."
  kill -15 -1
  echo -n "Waiting for processes to terminate"
  for loop in 0 1 2 3 4 5 6 7 ; do
    sleep 1
    echo -n "."
  done
  echo
  echo "Sending all processes the KILL signal."
  kill -9 -1

  # Try to turn off quota and accounting.
  if [ -x /usr/sbin/quotaoff ]
  then
	echo "Turning off quota.."
	/usr/sbin/quotaoff -a
  fi
  if [ -x /sbin/accton ]
  then
	echo "Turning off accounting.."
	/sbin/accton
  fi

  # Now go to the single user level
#  echo "Telling INIT to go to single user mode..."
#  exec init -t1 S
  echo "Single user mode."

