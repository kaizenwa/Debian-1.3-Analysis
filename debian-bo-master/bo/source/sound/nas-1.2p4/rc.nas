#! /bin/sh
# /etc/init.d/nas: start or stop the Network Audio System.

test -f /usr/X11R6/bin/auvoxware || exit 0

case "$1" in
  start)
    echo Starting the Network Audio System
    /usr/X11R6/bin/au &
    ;;
  stop)
    echo Stopping the Network Audio System
    start-stop-daemon --stop --quiet --exec /usr/X11R6/bin/au
    ;;
  *)
    echo "Usage: /etc/init.d/nas {start|stop}"
    exit 1
esac

exit 0
