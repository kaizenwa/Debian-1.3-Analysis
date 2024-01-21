#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/servers.tcl,v $
# $Date: 1996/06/24 11:07:13 $
# $Revision: 1.17.1.2 $
#
#
#  Server Operations
#
proc sendStats {net srv param var win} {
    global $var
    if ![string match {} $param] {
	set val $param
    } {
	set val [set ${var}($win)]
    }
    $net STATS $val $srv
    unset ${var}($win)
}
#
proc serverCmd {net cmd} {
    global zircon ztrans
    set srv [[set hst [$net hostid]] host]
    set def "(Default host is $srv)"
    switch $cmd {
    Oper {
	    set opStuff [$hst oper]
	    set nk [lindex $opStuff 0]
	    set pw [lindex $opStuff 1]
	    mkEntryBox .@oper Oper {Enter name and password:} \
	      "{Name [expr {$nk == {} ? [[$net myid] name] : $nk}]} \
	      {$ztrans(password) $pw}" "$ztrans(ok) {$net OPER}" \
	      "$ztrans(cancel) {}"
	}
    Rehash -
    Restart {
    	    set ucmd [string toupper $cmd]
	    mkDialog $ucmd .@$cmd $cmd "Really $cmd $srv?" \
	      {} "$ztrans(ok) {$net q1Send $ucmd}" "$ztrans(cancel) {}"
	}
    Stats {
	    global statsInfo
	    set statsInfo($net) {}
	    mkRadioBox .@Stats Stats "Enter Stats parameters $def:" \
	    {c h k i l L m o t u y z} c \
	    "{$ztrans(server) {}} {$ztrans(parameter) {}}" \
	    "$ztrans(ok) {sendStats $net}" "$ztrans(cancel) {}"
	}
    Links {
	    global linksInfo
	    set linksInfo($net) {}
	    mkEntryBox .@[newName Links] Links "Enter Links parameters $def:" \
	      "{$ztrans(server) {}} {Mask {}}" \
	      "$ztrans(ok) {$net LINKS}" "$ztrans(cancel) {}"
	}
    Connect {
	    mkEntryBox .@[newName Con] Connect "Enter Connect parameters $def:" \
	      "{$ztrans(server) {}} {$ztrans(port) {}} {Remote {}}" \
	      "$ztrans(ok) {$net CONNECT}" "$ztrans(cancel) {}"
	}
    Info {
	    global infoInfo
	    set infoInfo($net) {}
	    mkEntryBox .@[newName Inf] Info "Enter Server name $def:" \
	      "{$ztrans(server) {}}" "$ztrans(ok) {$net nsend INFO}" \
	      "$ztrans(cancel) {}"
	}
    Squit {
	    mkEntryBox .@[newName sq] Squit "Enter Server name :" \
	      "{$ztrans(server) {}}" "$ztrans(ok) {doSquit $net}" \
	      "$ztrans(cancel) {}"
	}
    Trace {
	    global traceInfo
	    set traceInfo {}
	    mkEntryBox .@[newName tr] Trace "Enter server name $def:" \
	      "{$ztrans(server) {}}" "$ztrans(ok) {$net nsend TRACE}" \
	      "$ztrans(cancel) {}"
	}
    Motd {
	    mkEntryBox .@[newName motd] MOTD "Enter server name $def:" \
	      "{Server {}}" "$ztrans(ok) {$net nsend MOTD}" \
	      "$ztrans(cancel) {}"
	}
    default {
	    set ucmd [string toupper $cmd]
	    mkEntryBox .@$cmd $cmd "Enter server pattern $def:" \
	      "{$ztrans(server) {}}" \
	      "$ztrans(ok) {$net nsend $ucmd}" "$ztrans(cancel) {}"
	}
    }
}
#
proc doSquit {net srv} {
    if ![string match {} $srv] {
	global ztrans
	set w .@[newName squit]
	mkDialog SQUIT $w Squit "Really Squit server \"$srv\"?" \
	  {} "$ztrans(ok) {$net qSend SQUIT :$srv}" "$ztrans(cancel) {}"
	tkwait window $w ;# This seems to be needed...
    }
}
#
proc kill {net who} {
    global ztrans
    mkDialog {} .@[newName kill] Kill "Really kill $who?" \
      "{$ztrans(message) {}}" "$ztrans(ok) {doKill $net $who}" \
      "$ztrans(cancel) {}"
}
#
proc doKill {net nk msg} {if ![string match {} $nk] { $net KILL $nk $msg }}
#
proc userKill {net} {
    global ztrans
    mkEntryBox {} "Kill" {Enter user name and message:} \
      "{$ztrans(user) {}} {$ztrans(message) {}}" \
      "$ztrans(ok) {doKill $net}" "$ztrans(cancel) {}"
}

proc statsProc {net prefix param pargs} {
    global statsInfo
    set p [string range $prefix 1 end]:
    foreach a [lrange $pargs 1 end] {
	if ![string match {} $a] {append p " $a"}
    }
    if ![string match {} $param] {append p " $param"}
    append statsInfo($net) "$p\n"
}

proc traceProc {net prefix param pargs} {
    set p [string range $prefix 1 end]:
    foreach a [lrange $pargs 1 end] {
	if ![string match {} $a] {append p " $a"}
    }
    if ![string match {} $param] {append p " $param"}
    $net inform $p
}
#
proc irc201 {net prefix param pargs} { traceProc $net $prefix $param $pargs }
proc irc202 {net prefix param pargs} { traceProc $net $prefix $param $pargs }
proc irc203 {net prefix param pargs} { traceProc $net $prefix $param $pargs }
proc irc204 {net prefix param pargs} { traceProc $net $prefix $param $pargs }
proc irc205 {net prefix param pargs} { traceProc $net $prefix $param $pargs }
proc irc208 {net prefix param pargs} { traceProc $net $prefix $param $pargs }
proc irc261 {net prefix param pargs} { traceProc $net $prefix $param $pargs }
#
proc irc211 {net prefix param pargs} { statsProc $net $prefix $param $pargs }
proc irc212 {net prefix param pargs} { statsProc $net $prefix $param $pargs }
proc irc213 {net prefix param pargs} { statsProc $net $prefix $param $pargs }
proc irc214 {net prefix param pargs} { statsProc $net $prefix $param $pargs }
proc irc215 {net prefix param pargs} { statsProc $net $prefix $param $pargs }
proc irc216 {net prefix param pargs} { statsProc $net $prefix $param $pargs }
proc irc218 {net prefix param pargs} { statsProc $net $prefix $param $pargs }
proc irc241 {net prefix param pargs} { statsProc $net $prefix $param $pargs }
proc irc242 {net prefix param pargs} { statsProc $net $prefix $param $pargs }
proc irc243 {net prefix param pargs} { statsProc $net $prefix $param $pargs }
proc irc244 {net prefix param pargs} { statsProc $net $prefix $param $pargs }
proc irc249 {net prefix param pargs} { statsProc $net $prefix $param $pargs }
#
proc tracewin {net prefix param pargs} {
    global wstat
    set prefix [string range $prefix 1 end]
    if ![info exists wstat($prefix)] {
	set w [toplevel .@[newName trace] -class Zircon]
	wm resizable $w 1 1
	wm protocol $w WM_DELETE_WINDOW "destroy $w ; unset wstat($prefix)"
	wm title $w "Trace for $prefix"
	set f [frame $w.f1]
	scrollbar $f.vs -command "$f.txt yview"
        text $f.txt -yscrollcommand "$f.vs set" -width 60 -height 10 \
	  -takefocus 0
	bindtags $f.txt ROText
	pack $f.vs -side right -fill y
	pack $f.txt -fill both -expand 1 -side left
	pack $f -fill both -expand 1
	pack [frame $w.f2] -fill x
	pack [button $w.f2.xit -text Dismiss -command "destroy $w ; \
	   unset wstat($prefix)" -width 10] -expand 1 -side left
	pack [button $w.f2.trc -text Trace -command "$net qSend TRACE :$prefix" \
	  -width 10] -expand 1 -side left
	set wstat($prefix) $w
    } {
	set w $wstat($prefix)
    }
    foreach a [lrange $pargs 1 end] {
	if ![string match {} $a] {$w.f1.txt insert end " $a"}
    }
    if ![string match {} $param] {$w.f1.txt insert end " $param"}
    $w.f1.txt insert end "\n"
}
#
proc irc200 {net prefix param pargs} {
    regsub -all {\\} $pargs {\\\\} pargs
    tracewin $net :[lindex $pargs 3] $param $pargs
}
#
proc irc206 {net prefix param pargs} { tracewin $net $prefix $param $pargs }
#
proc irc219 {net prefix param pargs} {
    global statsInfo
    if [info exists statsInfo($net)] {
	set w .@[newName stats]
	mkInfoBox STATS $w \
	  "[string range $prefix 1 end] Stats [getDate]" $statsInfo($net)
	unset statsInfo($net)
    }
}
#
proc irc364 {net prefix param pargs} {
    global linksInfo
    regsub -all {\\} $pargs {\\\\} pargs
    append linksInfo($net) "[lindex $pargs 1] [lindex $pargs 2] ${param}\n"
}
#
proc irc365 {net prefix param pargs} {
    global linksInfo
    if ![info exists linksInfo($net)] { set linksInfo($net) {No Links} }
    set w .@[newName links]
    mkInfoBox LINKS $w "[string range $prefix 1 end] Links [getDate]" $linksInfo($net)
    unset linksInfo($net)
}
#
proc irc371 {net prefix param pargs} {
    global infoInfo
    append infoInfo($net) "${param}\n"
}
#
proc irc374 {net prefix param pargs} {
    global infoInfo
    set w .@[newName info]
    mkInfoBox INFO $w "[string range $prefix 1 end] Info [getDate]" $infoInfo($net)
    unset infoInfo($net)
}
