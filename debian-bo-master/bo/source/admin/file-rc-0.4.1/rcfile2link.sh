#!/bin/sh
# rcfile2link.sh
# Convert the rc-file back into symlinks.
#
# Copyright (c) 1997, Tom Lees <tom@lpsg.demon.co.uk>.
#

# Hacked out of the file-based rc script.


CFGFILE="/etc/runlevel.conf"
BAKCFG="/etc/runlevel.fallback"
LOCKFILE="/var/lock/runlevel.lock"
TMPFILE="/tmp/runlevel.tmp"

i=0
while [ -f "$LOCKFILE" -a "$previous" != "N" ]
do
    read pid < "$LOCKFILE"
    if ! kill -0 $pid &> /dev/null
    then
	echo "$0: found stale lockfile '$LOCKFILE'. Ignoring it." >&2
# restriction on built-in functions ...
#        rm -f "$LOCKFILE"
        break
    fi
    if [ "$i" -gt "10" ]
    then
        echo "Process no. '$pid' is locking the configuration database. Terminating." >&2
        exit 1
    fi
    sleep 2
    let i+=1
done

cd /etc
for i in 0 1 2 3 4 5 6; do
    mkdir rc${i}.d
done

while read  SORT_NO  OFF_LEVELS  ON_LEVELS  CMD  OPTIONS
do
    case "$SORT_NO" in
	"#*" | "") continue ;;
    esac
    [ ! -x "$CMD" ] && continue

    NAME=`basename $CMD`

    OLDIFS="$IFS"
    IFS=,
    [ "$OFF_LEVELS" = "-" ] || for i in $OFF_LEVELS; do
	[ $i -ge 0 -a $i -le 6 ] && ln -s $CMD rc$i.d/K${SORT_NO}$NAME
    done
    [ "$ON_LEVELS" = "-" ] || for i in $ON_LEVELS; do
	[ $i -ge 0 -a $i -le 6 ] && ln -s $CMD rc$i.d/S${SORT_NO}$NAME 
    done
    IFS="$OLDIFS"
    unset OLDIFS

done < $CFGFILE

# End of file.
