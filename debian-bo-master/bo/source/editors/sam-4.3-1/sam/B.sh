#!/bin/sh

files=
line="none"
force=/bin/false

if [ $# = 0 ]; then
	echo 'usage: B [-f[a]] [-nnnn] files...'  1>&2
	exit 1
fi

case $1 in
	-f)  force=/bin/true; auto=/bin/false; shift;;
	-fa) force=/bin/true; auto=/bin/true;  shift;;
esac

dir=`/bin/pwd`
if [ "$USER" = "" ]; then
	USER=$LOGNAME
fi
pipe=/tmp/.sam.$USER

case "$DISPLAY" in
	*:[0-9]*\.[0-9]*)
			pipe=$pipe.$DISPLAY
			if [ ! -r $pipe ]; then
				pipe=`echo $pipe | sed 's/\.[0-9]*$//'`
			fi
			;;
	*:[0-9]*)
			pipe=$pipe.$DISPLAY
			if [ ! -r $pipe ]; then
				pipe=$pipe.0
			fi
			;;
	"")
			;;
	*)		pipe=$pipe.$DISPLAY
			;;
esac
if [ ! -r $pipe ]; then
	if $force; then
		if $auto; then
			case "$DISPLAY" in
				"") exec sam -d "$@";;
				*)  exec sam "$@" & ;;
			esac
		else
			exec sam "$@"
		fi
	else
		echo `basename $0`": No pipe \""$pipe"\" to sam." 1>&2
		exit 1
	fi
fi

for i in $*
do
	case "$i" in
		/*)	files="$files $i"
			;;
		-*)	line=`echo $i | sed 's/.//'`
			;;
		*)	files="$files $dir/$i"
			;;
	esac
done

echo "B $files" >> $pipe
if [ $line != "none" ]; then
	echo $line >> $pipe
fi

