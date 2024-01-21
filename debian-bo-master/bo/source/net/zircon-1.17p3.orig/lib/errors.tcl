#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/errors.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
proc irc401 {net prefix param pargs} {
    regsub -all {\\} $pargs {\\\\} pargs
    if [regexp -nocase {^zirconbot$} [set chan [lindex $pargs 1]]] return
    if [[set this [Message :: find $chan]] active] {
	$this addText @ERROR "*** $chan is not on IRC"
    } {
	$net errmsg "$param - $chan"
    }
}

proc irc404 {net prefix param pargs} {
    regsub -all {\\} $pargs {\\\\} pargs
    if [[set this [Channel :: find [set chan [lindex $pargs 1]]]] active] {
	$this addText @ERROR "*** $param"
    } {
	$net errmsg "Cannot send to channel $chan"
    }
}

proc irc406 {net prefix param pargs} {
    global whois
    regsub -all {\\} $pargs {\\\\} pargs
    set whois($net,err) [lindex $pargs 1]
}

proc resetNick {net} {
    entrySet [[$net control] window].nSFrm.nickname.entry [$net nickname]
}

proc irc432 {net prefix param pargs} {
    resetNick $net
    regsub -all {\\} $pargs {\\\\} pargs
    mkInfoBox ERROR .@nke$net {Nickname Error} "[lindex $pargs 1] : $param"
}
#
proc irc433 {net prefix param pargs} {
    regsub -all {\\} $pargs {\\\\} pargs
    set unk [lindex $pargs 1]
    if [$net startup] {
	global user ztrans
	$net fast
	set nk $user
	foreach x [$net nicks] {
	    if [string compare $param $x] { set nk $x ; break }
	}
	mkDialog SERROR {} {Nick in use} \
	  "The nickname \"$unk\" is in use. Try another." \
	  [list [list $ztrans(nickname) $nk [$net nicks]]] \
	  "$ztrans(ok) {$net NICK}" "$ztrans(cancel) {$net close}"
    } {
	resetNick $net
	mkInfoBox [expr {[$net startup] ? {SERROR} : {ERROR}}] .@nke$net \
	  {Nickname Error} "$unk : $param"
    }
}

proc irc442 {net prefix param pargs} {
    regsub -all {\\} $pargs {\\\\} pargs
    if [[set ch [Channel :: find [set nm [lindex $pargs 1]]]] active] {
	$ch delete
    } {
	mkInfoBox ERROR .@chaner {Channel Error} \
	  "$nm $param [lindex $pargs 2]"
    }
}

proc irc443 {net prefix param pargs} {
    regsub -all {\\} $pargs {\\\\} pargs
    set who [lindex $pargs 1]
    if [[set ch [Channel :: find [set cn [lindex $pargs 2]]]] active] {
	$ch addText @ERROR "*** $who $param $cn"
    } {
	mkInfoBox ERROR .@inver {Invite Error} \
	  "$who $param $cn"
    }
}

proc irc471 {net prefix param pargs} {
    global ztrans
    regsub -all {\\} $pargs {\\\\} pargs
    set chn [Channel :: find [set chan [lindex $pargs 1]]]
    mkDialog {} .@full {Channel Full} "Channel ${chan} is full!" \
      {} "$ztrans(ok) {}" "{Try Again} {$chn sendJoin}"
}

proc irc473 {net prefix param pargs} {
    regsub -all {\\} $pargs {\\\\} pargs
    if [[set ch [Channel :: find [lindex $pargs 1]]] active] {
	$ch addText @ERROR "*** \007Channel is invitation only!"
    } {
	mkInfoBox ERROR .@invonly {Invitation Only} \
	  "Channel [lindex $pargs 1] is invitation only!"
    }
}
#
proc irc474 {net prefix param pargs} {
    regsub -all {\\} $pargs {\\\\} pargs
    if [[set ch [Channel :: find [lindex $pargs 1]]] active] {
	$ch flag disabled
	$ch addText @ERROR "*** \007You are banned from this channel!"
    } {
	mkInfoBox ERROR .@banned$net Banned \
	  "You are banned from channel [lindex $pargs 1]!"
    }
}

proc irc475 {net prefix param pargs} {
    global ztrans
    regsub -all {\\} $pargs {\\\\} pargs
    if {[[set chn [Channel :: find [set chan [lindex $pargs 1]]]] key] == {}} {
	mkEntryBox .@key$net $ztrans(key) "Enter key for channel $chan:" \
	  "{$ztrans(key) {}}" "$ztrans(join) {$chn sendJoin}" \
	  "$ztrans(cancel) {}"
    } {
	mkDialog {} .@key$net "Bad Key" \
	  "Bad key for channel $chan!" [list [list Key [$chn key]]] \
	  "{Try Again} {$chn sendJoin}" "$ztrans(cancel) {}"
    }
}



