#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/saverc.tcl,v $
# $Date: 1996/06/20 20:01:32 $
# $Revision: 1.17.1.3 $
#
#
proc putProc {desc name list} {
    foreach l $list { puts $desc "$name {$l}" }
}
#
proc saverc {} {
    global zircon defaults cVars confData DEBUG OnCode trust \
      style current
    set rc $zircon(prefdir)/preferences
    if [file exist $rc] {
	file stat $rc st
	set mode $st(mode)
	set rc [glob $rc]
	exec mv $rc $rc.bak
	set bak 1
    } {
	if ![file exists $zircon(prefdir)] {
	    if [catch {exec mkdir $zircon(prefdir)} msg] {
		mkDialog SAVE {} Error \
		  "Error when creating .zircon directory - $msg" {}
		return 0
	    }
	}
	set mode 0600
	set bak 0
    }
    if [catch {open $rc w $mode} desc] {
	mkDialog SAVE {} Error \
	  "Error when opening preferences file for write - $desc" {}
	if $bak { exec mv $rc.bak $rc }
	return 0
    }
    if [catch {doSave $current(net) $desc} msg] {
	mkDialog SAVE {} Error \
	  "Error while saving preferences file - $msg" {}
	catch {close $desc}
	exec mv $rc $rc.tmp 
	if $bak {
	    exec mv $rc.bak $rc
	}
	return 0
    }
    return 1
}
#
proc doSave {net desc} {
    global zircon defaults cVars confData DEBUG OnCode trust
    puts $desc "#\n# Zircon preferences file saved - [getDate]\n#"
#
# Save things that are different from the default...
#
    foreach z [array names defaults] {
	if [string compare $zircon($z) $defaults($z)] {
	    switch $z {
	    ping { puts $desc "set zircon($z) {[expr $zircon($z) / 1000]}" }
	    defaul { puts $desc "set zircon($z) {$zircon($z)}" }
	    }
	}
    }
#
# Optional values
#
    foreach z {soundcmd cciport wwwclient} {
	if [info exists zircon($z)] {
	    puts $desc "set zircon($z) {$zircon($z)}"
	}
    }
    puts $desc "#\n# Nicknames\n#"
    putProc $desc nick [$net nicks]
    puts $desc "#\n# IRC Names\n#"
    putProc $desc ircname [$net ircnames]
    puts $desc "#\n# Server information\n#"
    Server :: save $desc $net
    puts $desc "#\n# Service information\n#"
    Service :: save $desc $net
    puts $desc "#\n# User information\n#"
    Friend :: save $desc $net
    puts $desc "#\n# Ignores\n#"
    foreach p [$net ignores] {
	if ![string match {} [lindex $p 1]] {
	    puts $desc "ignore {[lindex $p 0]} [lindex $p 1]"
	}
    }
    foreach x {Channel Message Notice Chat} {
	puts $desc "#\n# $x Information\n#"
	$x :: save $desc $net
    }
    puts $desc "#\n# Miscellaneous Control Values\n#"
    foreach x [array names cVars] {
	foreach v $cVars($x) {
	    global $v
	    switch $v {
	    ignores -
	    ircnames -
	    nicks { }
	    actions  { putProc $desc action [$net actions] }
	    leaves  { putProc $desc leave [$net leaves] }
	    signoffs  { putProc $desc signoff [$net signoffs] }
	    aways  { putProc $desc away [$net aways] }
	    default
		{
		    if {[lsearch $confData(single) $v] >= 0} {
			puts $desc "set $v {[$net $v]}"
		    } {
			puts $desc "set $v \{"
			foreach x [$net $v] { puts $desc "    {$x}" }
			puts $desc "\}"
		    }
		}
	    }
	}
    }
    if $DEBUG {
	global monitorOut monitorIn
	puts $desc {set DEBUG 1}
	if $monitorOut {puts $desc {set monitorOut 1}}
	if $monitorIn {puts $desc {set monitorIn 1}}
    }
    puts $desc "#\n# On Conditions\n#"
    foreach x [array names OnCode] {
	foreach on $OnCode($x) { puts $desc "on $x $on" }
    }
    if ![string match {} [$net bindings]] {
	puts $desc "#\n# Global Bindings\n#"
	foreach on [$net bindings] { puts $desc "zbind {} $on\n" }
    }
    puts $desc "#\n# Trust settings\n#"
    foreach x [array names trust] {
	puts $desc "set trust($x) {$trust($x)}"
    }
    close $desc
}
