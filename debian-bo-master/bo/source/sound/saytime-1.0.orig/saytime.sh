#!/bin/sh
#
# saytime.sh - shell version of saytime, by david@eng.sun.com

SDIR=${1-${SAYTIME-.}}
SAYFILES=

# n leadingzero
saynumber () {
	num=$1
	zero=$2
	set -$- `expr $num / 10 ; expr $num % 10`
	tens=$1
	unit=$2

	case $tens in
	0)
		case $zero in
		y) say oh ;;
		esac
		saydigit $unit
		;;
	1)
		say $num
		;;
	*)
		say ${tens}0
		saydigit $unit
	esac
}

saydigit () {
	case $1 in
	[1-9]) say $1 ;;
	esac
}

say () {
	file=$SDIR/$1.au
	if [ ! -f $file ] ; then
		echo "`basename $0`: cannot find $file"
		exit 1
	fi
	SAYFILES="$SAYFILES $file"
}

say the_time_is
set -$- `/bin/date '+%H %M %S'`
h=$1 m=$2 s=$3

case $h in
0)		h=12 ;;
1[3-9]|2[0-3])	h=`expr $h - 12` ;;
esac
saynumber $h n

case $m in
00)	say oclock ;;
*)	saynumber $m y
esac

case $s in
00)	say exactly ;;
01)	say and
	saynumber $s n
	say second
	;;
*)	say and
	saynumber $s n
	say seconds
esac

cat $SAYFILES > /dev/audio
