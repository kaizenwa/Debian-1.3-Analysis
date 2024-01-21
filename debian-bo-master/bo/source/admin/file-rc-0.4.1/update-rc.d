#!/bin/sh
# This is the script "update-rc.d" used manipulating the runlevel setup.
# This version handles a configuration file for the SysV-init instead 
# of dealing with links in /etc/rc?.d/*
#
# Author: Winfried Trümper <winni@xpilot.org>
#
# Misc fixes to make "remove" work like the "real" one - should work if the
# file doesn't exist - Tom Lees <tom@lpsg.demon.co.uk>.
#
# 27.12.96 v0.2 (corresponds to v0.2 of "new-rc")

CFGFILE="/etc/runlevel.conf"
BAKCFG="/etc/runlevel.fallback"
LOCKFILE="/etc/runlevel.lock"
TMPFILE="/tmp/runlevel.tmp"

function print_usage() {
    cat <<EOF
Usage:   update-rc.d  <basename>  <action>

Actions:
	comment   <sort number>
	uncomment <sort number>
	remove    <sort number> <runlevel>,<runlevel>,<runlevel> 
        on        <sort number> <runlevel>,<runlevel>,<runlevel> 
        off       <sort number> <runlevel>,<runlevel>,<runlevel> 
        defaults  [<sort number> | <startcode> <stopcode>]
                  (eqivalent to   start <startcode> 2,3,4,5
                                  stop  <stopcode>  0,1,6
                   <sort number> defaults to 20)
EOF
}

if [ "$1" = "-h" -o "$1" = "--help" ]
then
    print usage
    exit 0
fi

if [ $# -lt 2 ]
then
    echo "update-rc.d: too few arguments." >&2
    print_usage >&2
    exit 1
fi

basename="$1"; shift

if [ ! "$1" = "remove" ]; then
    if ! [ -f "/etc/init.d/$basename" ]
    then
        echo "update-rc.d: warning /etc/init.d/$basename doesn't exist. Terminating" >&2
        exit 1
    fi
    
    if ! [ -x "/etc/init.d/$basename" ]
    then
        echo "update-rc.d: warning /etc/init.d/$basename is not executable. Terminating" >&2
        exit 1
    fi
fi

[ "$action" = "on" ] && action="start"
[ "$action" = "off" ] && action="stop"

function element() {
    local element list IFS

    element="$1"
    case "$element" in
	reboot | R) element=0 ;;
	single | S) element=1 ;;
	halt   | H) element=6 ;;
    esac
	
    [ "$2" = "in" ] && shift
    list="$2"
    [ "$list" = "-" ] && return 1
    [ "$list" = "*" ] && return 0

    IFS=","
    set -- $list
    case $element in
	"$1" | "$2" | "$3" | "$4" | "$5" | "$6" | "$7" | "$8" | "$9")
	    return 0
    esac
    return 1
}


function unique() {
    local IFS level_set
    level_set="$2"
    echo -n "$level_set"

    IFS=","
    set -- $1
    for i in $@
    do
	if ! element $i in $level_set
	then
	    echo ",$i"
	fi
    done
}


START_SORT_NO=""
STOP_SORT_NO=""
STARTLEVELS=""
STOPLEVELS=""

action="$1"; shift
case "$action" in
    defaults)
	STARTLEVELS="2,3,4,5"
	STOPLEVELS="0,1,6"
	case "$#" in
	    "0")
		START_SORT_NO="20"
		STOP_SORT_NO="20"
		;;
	    "1")
		START_SORT_NO="$1"
		STOP_SORT_NO="$1"
		;;
	    "2")
		START_SORT_NO="$1"
		STOP_SORT_NO="$2"
		;;
	esac
	;;
    start)
	START_SORT_NO="$1"; shift
	if [ "$#" -gt "1" ]
	then
	    for i in $@
	    do
		STARTLEVELS="$STARTLEVELS,$i"
	    done
	    STARTLEVELS=${STARTLEVELS#","}
	else
	    STARTLEVELS="$1"
	fi
	;;
    stop)
	STOP_SORT_NO="$1"; shift
	if [ "$#" -gt "1" ]
	then
	    for i in $@
	    do
		STOPLEVELS="$STOPLEVELS,$i"
	    done
	    STOPLEVELS=${STOPLEVELS#","}
	else
	    STOPLEVELS="$1"
	fi
	;;
    remove | comment | uncomment)
	START_SORT_NO="$1"
	STOP_SORT_NO="$1"
	;;
esac


function remove_lock() {
    rm -f "$LOCKFILE"
}

  # wait for any lock to vanish
i=0
while [ -f "$LOCKFILE" ]
do
    read pid < "$LOCKFILE"
    if ! kill -0 $pid &> /dev/null
    then
        remove_lock
        break
    fi
    if [ "$i" -gt "5" ]
    then
        echo "Process no. '$pid' is locking the configuration database. Terminating." >&2
        exit 1
    fi
    sleep 2
    let i+=1
done

  # lock the configuration file
echo -e "$$\n" > "$LOCKFILE"


skip=""
rm -f $TMPFILE
touch $TMPFILE

#
#  default -> remove und dann neu setzen.
#

while read LINE
do
    if [ "$skip" ]
    then 
	echo "$LINE" >> "$TMPFILE"
	continue
    fi

    case $LINE in
	\#* | "" )
	    echo "$LINE" >> "$TMPFILE"
#	    [ "$action" = "uncomment" ] || 
	    continue
    esac

    set -- $LINE
    SORT_NO="$1"; STOP="$2"; START="$3"; CMD="$4"

    if [ "$CMD" = "/etc/init.d/$basename" ] 
    then
	if [ "$SORT_NO" = "$START_SORT_NO" -o  "$SORT_NO" = "$STOP_SORT_NO" -o\
	     "$START_SORT_NO" = "*" -o "$STOP_SORT_NO" = "*" ]
	then
	    case "$action" in
		comment)
		    echo "#$LINE" >> "$TMPFILE"
		    modified="1"
		    continue
		    ;;
		remove)
		    modified="1"
		    continue
		    ;;
		defaults)
		    START="$STARTLEVELS"
		    STOP="$STOPLEVELS"
		    action=remove
		    START_SORT_NO="*"
		    ;;
		start)
		    if [ "$START" = "-" ]
		    then
			START="$STARTLEVELS"
		    else
			START=`unique "$STARTLEVELS" "$START"`
		    fi
		    skip=1
		    ;;
		stop)
		    if [ "$STOP" = "-" ]
		    then
			STOP="$STOPLEVELS"
		    else
			STOP=`unique "$STOPLEVELS" "$STOP"`
		    fi
		    skip=1
		    ;;
	    esac
	fi
	echo -e "$SORT_NO\t$STOP\t$START\t\t$CMD" >> "$TMPFILE"
	modified="1"
	continue
    fi

    NEW_STOP="-"
    NEW_START="-"
    NEW_SORT_NO=""
    if [ "$action" = "defaults" -o "$action" = "start" ]
    then
	if [ "$START_SORT_NO" -lt "$SORT_NO" ]
	then
	    NEW_SORT_NO="$START_SORT_NO"
	    NEW_START="$STARTLEVELS"
	    NEW_CMD="/etc/init.d/$basename"
	    [ "$action" = "start" ] && skip=1
	    action="stop"
	fi
    fi
    if [ "$action" = "defaults" -o "$action" = "stop" ]
    then
	if [ "$STOP_SORT_NO" -lt "$SORT_NO" ]
	then
	    NEW_SORT_NO="$STOP_SORT_NO"
	    NEW_STOP="$STOPLEVELS"
	    NEW_CMD="/etc/init.d/$basename"
	    [ "$action" = "stop" ] && skip=1
	    action="start"
	fi
    fi
    if [ -n "$NEW_SORT_NO" ]
    then
	echo -e "$NEW_SORT_NO\t$NEW_STOP\t$NEW_START\t\t$NEW_CMD" >> "$TMPFILE"
	modified="1"
    fi

    echo -e "$SORT_NO\t$STOP\t$START\t\t$CMD" >> "$TMPFILE"

done < "$CFGFILE"

remove_lock

if [ -z "$modified" ]
then
    echo "Nothing to do."
else
    umask=022
    mv "$TMPFILE" "$CFGFILE"
fi
exit 0
