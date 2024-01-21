#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/Sound.tcl,v $
# $Date: 1996/07/01 09:11:33 $
# $Revision: 1.17.1.3 $
#
proc sendSound {net nk} {
    global FBFilter ztrans
    set v .@[newName snd]
    set msg "Play sound file for $nk"
    tkwait window [mkFileBox {} {.*\.[wW][aA][vV]} $ztrans(sound) $msg {} \
	"$ztrans(send) {doSendSound $net $nk}" "$ztrans(dismiss) {}"]
}
#
proc handleSound {net usr param} {
    global zircon
    if ![string match {} $zircon(wavplayer)] {
	set dirs [split $zircon(wavpath) :]
	lappend dirs $zircon(prefdir)/sounds $zircon(lib)/sounds
	set fl [lindex $param 1]
	if [string match {} [file extension $fl]] { append fl .wav }
	foreach x $dirs {
	    if [file exists $x/$fl] {set fl "$x/$fl" ; break }
	}
	if [catch {exec $zircon(wavplayer) $fl &} msg] {
	}
    }
}
#
proc doSendSound {net nk fl} {
    if ![string match {} $fl] {
	$net CTCP SOUND $nk "$fl <$fl>"
	handleSound $net [$net nickname] "SOUND $fl"
    }
}
#
proc addSoundMenu {net menu chid} {
    global zircon ztrans
    if ![string match {} $zircon(wavplayer)] {
	$menu add cascade -label $ztrans(sound) -menu $menu.sound
	menu $menu.sound -postcommand "buildSMenu $net $menu.sound $chid"
	catch {$menu.sound configure  -tearoffcommand "retitle {Sounds for [$chid name]}"}
    }
}
#
proc buildSMenu {net menu chid} {
    global zircon
    $menu delete 0 end
    set ptn {*.[wW][aA][vV]}
    set wavs {}
    foreach x $zircon(wavpath) {
	if ![string match {} [set wvs [glob -nocomplain $x/$ptn]]] {
	    foreach y $wvs { lappend wavs [file tail $y] }
	}
    }
    if [string match {} $wavs] {
	foreach y [glob -nocomplain $ptn] { lappend wavs [file tail $y] }
    }
    foreach x [lsort $wavs] {
	$menu add command -label [file tail [file rootname $x]] \
	  -command "doSendSound $net {[$chid name]} {$x}"
    }
}
