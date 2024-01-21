#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/upgrade.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
# Upgrade the rc file
#
proc upgradeRC {} {
    switch [tk_dialog .@upgrade Upgrade \
      {Zircon now stores its configuration files in a directory called \
	.zircon in your home directory. Upgrade?} {} 0 Upgrade Cancel ] {
 	0 { upgrade2 }
 	1 { }
    }
}
#
proc upgrade2 {} {
    global zircon
    set x $zircon(prefdir)
    if [catch {exec mkdir $x} msg] {
	puts stderr "**** Could not make directory $x - $msg"
	return
    }
    if [catch {exec mv [glob ~]/.zirconrc $x/preferences} msg] {
	puts stderr "**** Could not move .zirconrc to $x/preferences - $msg"
    }
}
