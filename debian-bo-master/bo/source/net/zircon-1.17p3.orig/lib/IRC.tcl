#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/IRC.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
#
#   Handle IRC cmds
#
proc ctcpAnswer {usr nk cp} {
    if {[llength [set ch [$usr channels]]] == 1 || 
      [string compare [set ch [find $nk]] nil]} {
	$ch addText $usr "*** CTCP Reply from $nk: $cp"
    } {
	mkInfoBox CTCP .@ctcp$usr {CTCP Reply} \
	  "CTCP Reply from $nk:\n$cp" {Dismiss {}}
    }
    handleOn CTCPREPLY [list $nk $cp]
}
#
proc ircERROR {net prefix param pargs} { $net error $prefix $param $pargs }
#
proc ircPONG {net prefix param pargs} { $net configure -pinged 0 }
#
proc handlePing {usr nk line} {
}
#
proc mungNotice {msg} {
    if [regexp \
      {Received KILL message for ([^ ]+). From ([^ ]+) Path: ([^ ]+) (.*)} \
      $msg match user from path rest] {
	return "*** KILL from $from for $user $rest"
    }
    return $msg
}
#
proc ircNOTICE {net prefix param pargs} {
    set nkinfo [mungPrefix $net $prefix]
    if [ignoreSet [lindex $nkinfo 3] notices] { return }
    if [string match {} [lindex $nkinfo 2]] {
	$net inform [mungNotice $param]
	set chan {}
    } {
	set nk [[set usr [lindex $nkinfo 0]] name]
	regsub -all {[\\{\"}]} $pargs {\\&} pargs
	set id [find [set chan [lindex $pargs 0]]]
	if [regexp "\001(\[^\001\]*)\001" $param sub cp] {
	    switch -glob -- $cp {
	    {ZIRCON Sorry*} { }
	    {PING *} {
		regexp {PING *(.*)} $cp m t
		set t [zping $t]
		ctcpAnswer $usr $nk "PING - $t secs"
	    }
	    default { ctcpAnswer $usr $nk $cp }
	    }
	} \
	elseif {[string match {[#$&]*} $chan]} {
	    $id addText $usr "-$nk- $param"
	} \
	elseif {[string match nil [set id [Notice :: find $nk]]] &&
	  [string match nil [set id [Message :: find $nk]]]} {
	    if [$net busy] {
		$net inform "Notice from $nk at [getDate] : $param"
	    } {
		handleOn POPUP [list $nk]
		set id [Notice :: make $nk]
		$id addText {} "[getDate]\n$param"
	    }
	} {
	    if ![$id active] {$id show}
	    $id addText $usr $param
	}
    }
    handleOn NOTICE [list $prefix $param $chan]
    ditchPrefix $nkinfo
}
#
proc ircMODE {net prefix param pargs} {
    global userFlags
    regsub -all {[\\{\"}]} $pargs {\\&} pargs
    set chan [lindex $pargs 0]
    if [string match {nil} [set id [Channel :: find $chan]]] {
	if [me $chan $net] {
	    if {[set md [lindex $pargs 1]] == {}} { set md $param }
	    foreach m [split $md {}] {
		switch -exact -- $m {
	 	- { set val 0 }
		+ { set val 1 }
		default { catch {$net configure -$userFlag($m) $val} }
		}
	    }
	}
	return
    }
    regsub -all {[{}]} $pargs {\\&} pargs
    $id mode [lrange $pargs 1 end]
    $id optText MODE "*** Mode change \"[string trim \
      [join [lrange $pargs 1 end]]]\" on channel $chan by\
      [lindex [mung1Prefix $net $prefix] 0]"
}
#
proc ircPRIVMSG {net prefix param pargs} {
    set nkinfo [mungPrefix $net $prefix]
    set usr [lindex $nkinfo 0]
    set nk [$usr name]
    set lnk [$usr lname]
    set ign [lindex $nkinfo 3]
    regsub -all {[\\{\"}]} $pargs {\\&} pargs
    set chan [lindex $pargs 0]
    if [regexp "(\[^\001\]*)\001(\[^\001\]*)\001(\[^\001\]*)" $param sub a cp b] {
    	set ctcp [split $cp]
	set value \
	  [handleCTCP $net [lindex $ctcp 0] $chan $usr $prefix $ign "$cp"]
	if [string match {} $value] return
	set param "${a}${value}$b"
    }
    set pfx "<$nk>"
    regsub -all "\[\x01-\x1f\]" $pfx {} pfx
    if [me $chan $net] {
	if [ignoreSet $ign notes] return
	if [string match nil [set where [Message :: find $nk]]] {
	    global zircon
	    if [$net busy] {
		$net qSend NOTICE $nk :$zircon(busymsg)
		$net inform "Message from $nk at [getDate] : $param"
	    } {
		handleOn POPUP [list $nk]
		[Message :: make $nk] addText $usr "[getDate]\n$pfx $param"
	    }
	    ditchPrefix $nkinfo
	    return
	} {
	    $where show
	    if ![$where isJoined $usr] {$where addUser $usr 0 0}
	}
	set chan $lnk
    } {
	if [ignoreSet $ign public] return
	set where [Channel :: find $chan]
    }
    if [string match nil $where] {
	set where [$net info]
	set pfx "<$nk/$chan>"
    }
    $where addText $usr "$pfx $param"
    foreach p [$where patterns] {
	set pt [lindex $p 0]
	if {[regexp -nocase [lindex $pt 0] $pfx] && \
	  [regexp [lindex $pt 1] $param]} {
	    if [catch {uplevel #0 [lindex $p 1]} msg] {
		mkDialog PATTERN .@pattern "Pattern Command Error" \
		  "Error when executing pattern command \"[lindex $p 1]\" : $msg" \
		  {} "Dismiss {}"
	    }
	}
    }
    ditchPrefix $nkinfo
}
#
proc ircJOIN {net prefix param pargs} {
    set nkinfo [mungPrefix $net $prefix]
    if [lindex $nkinfo 1] { [Channel :: make $param] show } {
	[Channel :: find $param] doJoin [lindex $nkinfo 0] \
	  [lindex $nkinfo 2] $prefix
    }
    handleOn JOIN [list $param $prefix]
    ditchPrefix $nkinfo
}
#
proc ircNICK {net prefix param pargs} {
    set usr [lindex [set nkinfo [mungPrefix $net $prefix]] 0]
    if [lindex $nkinfo 1] { $net configure -nickname $param } \
    elseif {[string compare nil [set orig [User :: find $param]]] &&
	[string compare $usr $orig]} {
	$usr substitute $orig
    } {
	foreach id [$net channels] {
	    if [$id isJoined $usr] { $id nickChange $usr $param }
	}
	foreach x {Message Notice Chat} {
	    if [string compare nil [set old [$x :: find [$usr lname]]]] {
		$old nickChange $usr $param
	    }
	}
	$usr rename $param
    }
    handleOn NICK [list $prefix $param]
    ditchPrefix $nkinfo
}
#
proc ircPART {net prefix param pargs} {
    set nkinfo [mungPrefix $net $prefix]
    regsub -all {[\\{\"}]} $pargs {\\&} pargs
    set chan [Channel :: find [set chn [lindex $pargs 0]]]
    if [lindex $nkinfo 1] {$chan delete} {
	set usr [lindex $nkinfo 0]
	$chan optText LEAVE "*** [$usr name] has left channel $chn"
	$chan killUser $usr
    }
    handleOn LEAVE [list $chn $prefix]
    ditchPrefix $nkinfo
}
#
proc ircKICK {net prefix param pargs} {
    set nkinfo [mungPrefix $net $prefix]
    regsub -all {[\\{\"}]} $pargs {\\&} pargs
    set chan [lindex $pargs 0]
    set nk [[set who [User :: make [lindex $pargs 1]]] name]
    set kicker [[lindex $nkinfo 0] name]
    set id [Channel :: find $chan]
    if [$net me $who] {
	$id quiesce
	mkDialog KICKED .@kick$id "Kicked from $chan"\
	  "You have been kicked off channel $chan by $kicker ($param)" \
	  {} "Rejoin {$id sendJoin}" "Dismiss {$id delete}"
    } {
	$id optText KICK \
	  "*** $nk has been kicked off channel $chan by $kicker ($param)"
	$id killUser $who
   }
   handleOn KICK [list $chan $prefix $nk $param]
   ditchPrefix $nkinfo
}
#
proc netsplit {string} {
    return [regexp -nocase \
      {^([a-z0-9*_-]+\.)+([a-z0-9_-]+) ([a-z0-9*_-]+\.)+([a-z0-9_-]+)$} $string]
}
#
proc ircQUIT {net prefix param pargs} {
    global zircon
    set nkinfo [mungPrefix $net $prefix]
    set nk [[set usr [lindex $nkinfo 0]] name]
    if {!$zircon(nosplit) && [netsplit $param]} {
	$usr split $param
    } {
	global toInfo
	if [string compare nil [set fobj [Friend :: find $nk]]] {
	    $fobj configure -ison 0
	}
	$usr off
	if {[set ti [expr {[lsearch $toInfo SIGNOFF] >=0}]]} {
	    $net display @QUIT "*** Signoff: $nk ($param)"
	    if [string compare [set m [Message :: find $nk]] nil] {
		$m addText @QUIT "*** $nk has signed off : $param"
	    }
	}
	set lnk [$usr lname]
	foreach x {channels messages notices chats} {
	    foreach id [$net $x] {
		if [$id isJoined $usr] {
		    if !$ti { $id optText QUIT "*** Signoff: $nk ($param)" }
		    $id killUser $usr
		} \
		elseif {[$id lname] == $lnk && [$id active]} {
		    $id addText @QUIT "*** $nk has signed off : $param"
		}
	    }
	}
	handleOn QUIT [list $prefix]
    }
    ditchPrefix $nkinfo
}
#
proc acceptInvite {net chan} {
   set chn [Channel :: make $chan]
   catch {destroy .@kick$chn}
   $chn sendJoin {}
}
#
proc ircINVITE {net prefix param pargs} {
    if ![ignoreSet [lindex [set nkinfo [mungPrefix $net $prefix]] 3] invites] {
	set name [[lindex $nkinfo 0] name]
	mkDialog {} {} "Invitation" \
	  "$name invites you to channel $param." {} \
	  "Join {acceptInvite $net $param}" {Ignore {}}
    }
    handleOn INVITE [list $prefix $param]
    ditchPrefix $nkinfo
}
#
proc ircKILL {net prefix param pargs} {
    set nkinfo [mung1Prefix $net $prefix]
    regsub -all {[\\{\"}]} $pargs {\\&} pargs
    [set who [User :: make [lindex $pargs 0]]] off
    $who ref
    set nk [$who name]
    if [$net me $who] {
	mkDialog KILLED {} "Killed"\
	  "You have been killed by [lindex $nkinfo 0] ($param)" \
	  {} {Dismiss {}}
    } {
	foreach x {channels notices messages} {
	    foreach id [$net $x] {
		if {[$id isJoined $who]} {
		    $id optText KILL \
		      "*** $nk has been killed by [lindex $nkinfo 0] ($param)"
		    $id killUser $who
		}
	    }
	}
    }
    handleOn KILL [list $prefix $nk]
    $who deref
}
#
proc ircTOPIC {net prefix param pargs} {
    regsub -all {[\\{\"}]} $pargs {\\&&} pargs
    set id [Channel :: find [set chan [lindex $pargs 0]]]
    $id setTopic $param
    set who [lindex [mung1Prefix $net $prefix] 0]
    $id optText TOPIC "*** $who has set the topic."
    $id log "*** $who has set the topic: $param"
    handleOn TOPIC [list $chan $prefix $param]
}
#
proc ircWALLOPS {net prefix param pargs} {
   $net display WALLOP "[getDate] $prefix (WALLOPS) - $param"
}
#
#	proc mungPrefix : breaks up the prefix to an IRC message
#	returns : {user object, me?, user@host, ignores}
#
proc mungPrefix {net prefix} {
    if ![regexp {:([^!]*)!(.*)} $prefix m1 nk nm] {
	set nk [string range $prefix 1 end]
	set nm {}
    }
    [set usr [User :: make $nk]] ref
    return [list $usr [$net me $usr] $nm [z_ignore $usr $nm]]
}
#
proc mung1Prefix {net prefix} {
    if ![regexp {:([^!]*)!(.*)} $prefix m1 nk nm] {
	set nk [string range $prefix 1 end]
	set nm {}
    }
    return [list $nk $nm]
}
#
proc ditchPrefix {mng} { [lindex $mng 0] deref }
