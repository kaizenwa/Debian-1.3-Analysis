# Module: ExternalProgs.tcl
# 6.5.96 T.Niederreiter
#
# Defines all external called programs and icons.
#

# System-Programs:

set DF /bin/df
set MKE2FS /sbin/mke2fs
set LS /bin/ls
set DU /bin/du
set DU2 /usr/bin/du
set RM /bin/rm
set MOUNT /bin/mount
set UMOUNT /bin/umount
set DATE /bin/date

# Private-Programs:
 
set BINDIR "../bin"

set INQ $BINDIR/scsiinq
set FDISK $BINDIR/fdisk3.04
set GETPARTSIZE $BINDIR/getpartsize
set GETCDTOC $BINDIR/getcdtoc
set ISODETECT $BINDIR/isodetect
set RESETCD $BINDIR/resetcd
set CHECKCD $BINDIR/checkcd
set ISOSIZE $BINDIR/isosize
set MKISOFS $BINDIR/mymkisofs
set ISOINFO $BINDIR/isoinfo
set READCDDA $BINDIR/readcdda
set PLAYCDDA $BINDIR/playcdda
set MYDD $BINDIR/mydd
set MYCMP $BINDIR/mycmp
set CDWRITE $BINDIR/mycdwrite

# Icons:

set ICONDIR "../icons"

set CDRICO $ICONDIR/xcdricon.xbm
set LOGOICO $ICONDIR/xcdrlogo.gif
set YESOPTICO $ICONDIR/yes_opt.xbm
set NOOPTICO $ICONDIR/no_opt.xbm
set BARTOP $ICONDIR/topbar.xbm
set BARDOWN $ICONDIR/downbar.xbm
set BARMID $ICONDIR/midbar.xbm
set BAREMPTY $ICONDIR/emptybar.xbm
set MINIINFO $ICONDIR/miniinfo.xbm
set DELARROW $ICONDIR/delarrow.xbm
set ADDARROW $ICONDIR/addarrow.xbm
set PLAYICO $ICONDIR/play.xbm
set PAUSEICO $ICONDIR/pause.xbm
set QUITICO $ICONDIR/quit.xbm
set STOPICO $ICONDIR/stop.xbm
set WARNICO $ICONDIR/warn.xpm
set INFOICO $ICONDIR/info.xpm

