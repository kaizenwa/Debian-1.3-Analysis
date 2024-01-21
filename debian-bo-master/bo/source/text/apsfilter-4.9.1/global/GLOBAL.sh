# apsfilters version number
VERSION=491

# NOTIFY whon in case of printer fault
NOTIFY=root

# possible PATH's ... is that enough ?! :)
APS_PATH=$APS_BASEDIR/bin:$APS_BASEDIR/setup:$APS_BASEDIR/global
STD_PATH=/sbin:/bin:/usr/sbin:/usr/bin:/usr/contrib/bin
TEX_PATH_U=/usr/TeX/bin:/usr/local/TeX/bin:/TeX/bin
TEX_PATH_L=/usr/tex/bin:/usr/local/lib/{tex,mf}/bin:/tex/bin
SUN_PATH=/usr/ucb:/usr/openwin/bin:/usr/etc:/etc
GNU_PATH=/usr/gnu/bin:/gnu/bin:/usr/gnu
X11_PATH=/usr/bin/X11:/usr/X386/bin:/usr/X11/bin
WUP_PATH=/wup/bin
COMMON_PATH=/usr/local/bin:/local/bin
# HERE IS THE TEMPLATE FOR YOU TO ADD PATHS THAT ARE ADDITIONALLY NEEDED !!!
LOCAL_PATH=/foo/bar

PATH=$APS_PATH:$STD_PATH:$TEX_PATH_U:$TEX_PATH_L:\
$SUN_PATH:$GNU_PATH:$X11_PATH:$WUP_PATH:$COMMON_PATH:$LOCAL_PATH:.
export PATH

OLDCAP=/etc/printcap.old

# APS_BASEDIR is computed from /etc/printcap in apsfilter script
FILTERS_FOUND=$APS_BASEDIR/global/filters_found

SYSTEM=`uname -s`
case $SYSTEM in
	*SunOS*|*sunos*|*SUNOS*)	LP_OWNER=bin
					LP_GROUP=daemon
					SPOOL=/var/spool/lpd
					MAGIC=/etc/magic
					;;
	BSD/386)			LP_OWNER=root
					LP_GROUP=wheel
					SPOOL=/var/spool/lpd
					MAGIC=/usr/share/misc/magic
					;;
	*BSD)				LP_OWNER=bin
					LP_GROUP=daemon
					SPOOL=/var/spool/lpd
					MAGIC=/etc/magic
					;;
	*)				LP_OWNER=root
					LP_GROUP=lp
					SPOOL=/var/spool/lpd
					MAGIC=/etc/magic
					;;
esac

PAGER=zless
#
# how to suppress newlines on echo command for nicer display ...
#
c=''
n=''
# first determine how to suppress newline on echo command
(echo "hi there\c" ; echo " ") > /tmp/.echotmp
if grep c /tmp/.echotmp >/dev/null 2>&1 ; then
    n='-n'
    c=''
else
    n=''
    c='\c'
fi
rm -f /tmp/.echotmp
