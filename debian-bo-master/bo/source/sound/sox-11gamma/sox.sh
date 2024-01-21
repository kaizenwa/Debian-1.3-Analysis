#!/bin/sh


SOX=/usr/local/bin/sox.bin

# SOX shell script to handle effect names as main name

# The shell is your friend.  Use it.

NAME=$0
switch $NAME in
	*/*)
		NAME=`echo $NAME | sed "s'^.*/''"`
	;;
esac

echo $NAME
RATE=8000
if [ "$1" = "-r" -a -n "$2" ] ; then
	RATE=$2
	shift
	shift
fi

switch $NAME in
	case *sox)
		exec $SOX $*
	;;
	case *echo|*avg|*pred|*stat|*vibro|*echo|*rate|*band|*lowp)
		$SOX -t sw -r $RATE - - $NAME $*
	;;
	case *kaiser)
		$SOX 
esac
