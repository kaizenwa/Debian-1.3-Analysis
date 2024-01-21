#! /bin/sh -f

AGENT=/etc/snmpd
CONF=/etc/snmpd.conf
INFO=/etc/snmpd.agentinfo
## default is configured: MIB=/usr/lib/snmpd.mib
MIB=/usr/lib/snmpd.mib


TMP=/tmp/snmpd.$$
trap "rm -f $TMP" 1 2 3 13 15

POSTURE= PASSWORD=

for A in "$@"
do
    case "$A" in
	-mini|-semi|-very)
		if [ ! -z "$POSTURE" ]; then
		    echo "use -mini, -semi, or -very exactly once" 1>&2
		    echo "usage: agent-boot.sh [-mini/-semi/-very] password" \
			 1>&2
		    exit 1
		fi
		POSTURE="$A"
		;;

	-*)	echo "unknown option: $A" 1>&2
		echo "usage: agent-boot.sh [-mini/-semi/-very] password" 1>&2
		exit 1
		;;

	*)	if [ ! -z "$PASSWORD" ]; then
		    echo "use password only once" 1>&2
		    echo "usage: agent-boot.sh [-mini/-semi/-very] password" \
			     1>&2
		    exit 1
		else
		    PASSWORD="$A"
		fi
		;;
    esac
done

if [ -z "$POSTURE" ]; then
    LEVEL=very
else
    LEVEL="`echo $POSTURE | sed -e s%-%%`"
fi

AGENTID=`netstat -in | awk '
/^Name /{ next }
/^lo/	{ next }
	{
	    split($4,quad,".")
	    printf "%02x%02x%02x%02x", 0, 0, 0, 35;
	    printf "%02x%02x%02x%02x", quad[1], quad[2], quad[3], quad[4];
	    printf "%02x%02x%02x%02x\n", 0, 0, 0, 0;

	    exit 0
	}
END	{ exit 1 }
'`
if [ -z "$AGENTID" ]; then
    echo "unable to determine IP-address" 1>&2
    exit 1
fi

if [ ! -f ../mib.txt -o ! -x authkey ]; then
    echo "need to run script from the apps/ directory" 1>&2
    exit 1
fi


umask 0022


if [ ! -f $AGENT ]; then
    if (cp snmpd $AGENT) 2>/dev/null; then
	echo "created $AGENT"
    else
	echo "unable to create $AGENT, you must copy snmpd by hand..."
    fi
fi


if [ ! -f $INFO ]; then
    if (echo 0 > $INFO) 2>/dev/null; then
	echo "created $INFO"
    else
	echo "unable to create $INFO, you must do so by hand..."
    fi
else
    echo "warning: file $INFO already exists"
fi


if [ ! -f $MIB ]; then
    if (cp ../mib.txt $MIB) 2>/dev/null; then
	echo "created $MIB"
    else
	echo "unable to create $MIB, you must copy ../mib.txt by hand..."
    fi
else
    echo "warning: file $MIB already exists"
fi


echo -n "generating key from password... "
KEY=`./authkey "$PASSWORD" "$AGENTID" | awk '{ print $NF }'`
if [ -z "$KEY" ]; then
    exit 1
fi
echo "done."


umask 0077


echo "\
#
# snmpd.conf - created `date`
#


#
#
# view configuration
#
#	viewName	OID			included/excluded
#

# internet
view	all		.1.3.6.1		included

# internet
view	mini		.1.3.6.1		included

# system, snmp, usecAgent, usecStats
view	semi		.1.3.6.1.2.1.1		included
view	semi		.1.3.6.1.2.1.11		included
view	semi		.1.3.6.1.6.3.6.1.1	included
view	semi		.1.3.6.1.6.3.6.1.2	included

# snmp, usecAgent, usecStats
view	semi		.1.3.6.1.2.1.11		included
view	semi		.1.3.6.1.6.3.6.1.1	included
view	semi		.1.3.6.1.6.3.6.1.2	included


#
#
# user configuration
#
#	noneRV	noneWV	authRV	authWV	userName[/authKey]
#
user	$LEVEL	-	all	all	public/0x$KEY


#
#
# community configuration
#
#	  commName	readV	writeV
#
community public	$LEVEL	-

" > $TMP

if [ ! -f $CONF ]; then
    if (mv $TMP $CONF) 2>/dev/null; then
	echo "created $CONF"
    else
	echo "unable to create $CONF, you must copy $TMP by hand..."
    fi
else
    echo "
*** file $CONF already exists, newly-generated configuration left in

    $TMP"
fi


echo "
*** to run agent, the file /etc/rc.local needs these lines:

    if [ -f $AGENT ]; then
	MIBFILE=$MIB $AGENT >/dev/null 2>&1 & echo -n ' snmpd'
    fi"


exit 0
