#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/ctcp.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
#
proc ctcpReply {net chan nk op str} {
    if [$net verboseCTCP] {
	$net display @CTCP "*** CTCP $op Reply to $nk - $str"
    }
    $net qSend NOTICE $nk ":\001$op $str\001"
}
#
proc handleCTCP {net op chan usr prefix ign param} {
    if [ignoreSet $ign ctcp] { return {}}
    set nk [$usr name]
    if {[string compare $op ACTION] && [string compare $op SED] \
	 && [string compare $op ZIRCON]} {
	if [string compare nil [set id [find $chan]]] {
	    $id addText [$usr lname] "*** CTCP $op $param from $nk"
	} {
	    $net display [$usr lname] "*** CTCP $op $param from $nk"
	}
    }
    switch -exact -- $op {
    CLIENTINFO {
	    global zircon
	    set ctcps "CLIENTINFO VERSION USERINFO ERRMSG PID SOURCE \
ACTION FINGER TIME UTC ECHO DCC SED ZIRCON"
	    if ![string match {} $zircon(wavplayer)] { lappend ctcps SOUND }
	    ctcpReply $net $chan $nk $op "$ctcps: The Zircon IRC client"
	}
    VERSION {
	    global zircon tk_patchLevel
	    ctcpReply $net $chan $nk $op \
	      "Zircon $zircon(version) Pl: $zircon(patchlevel) *IX :\
tcl [info patchlevel] tk $tk_patchLevel [version] [version]"
	}
    USERINFO { ctcpReply $net $chan $nk $op [$net userinfo] }
    PING -
    ECHO -
    ERRMSG {
	    ctcpReply $net $chan $nk $op [string range $param [string length $op] end]
	}
    PID { ctcpReply $net $chan $nk $op [pid] }
    SOURCE { ctcpReply $net $chan $nk $op "Available by ftp from catless.ncl.ac.uk" }
    ACTION {
	    set lnk [$usr lname]
	    set id [find $chan]
	    if [me $chan $net] {
		if ![string compare nil [set id [Message :: find $nk]]] {
		    global zircon
		    if [$net busy] {
			$net send NOTICE $nk $zircon(busymsg)
			$net inform \
			  "Action from $nk at [getDate] : [string range $param 7 end]"
			return {}
		    } {
			handleOn POPUP [list ${nk}]
			set id [Message :: make $nk]
			$id addText $lnk "[exec date]"
		    }
		}
	    }
	    $id addText $lnk "* $nk [string range $param 7 end]"
	}
    FINGER {
	    global zircon
	    if {[set t $zircon(idle)] >= 60} {
		if {[set r [expr {$t % 60}]] > 0} {
		    append r { seconds}
		} {
		    set r {}
		}
		if {[set t [expr {$t / 60}]] != 1} {
		    set t "$t minutes $r"
		} {
		    set t "$t minute $r"
		}
	    } {
		append t { seconds}
	    }

	    ctcpReply $net $chan $nk $op "[$net ircname] Idle $t"
	}
    SED {
	    set id [find $chan]
	    if [me $chan $net] {
		if ![string compare nil [set id [Message :: find $nk]]] {
		    global zircon
		    if [$net busy] {
			$net send NOTICE $nk $zircon(busymsg)
			set mv \
			  [decrypt [string range $param 4 end] [$usr crypt]]
			$net inform \
			  "Encrypted Message from $nk at [geDate] : $mv"
			return {}
		    } {
			handleOn POPUP [list $nk]
			set id [Message :: make $nk -crypt [$usr crypt]]
		    }
		}
	    }
	    return [decrypt [string range $param 4 end] [$id crypt]]
	}
    TIME { ctcpReply $net $chan $nk $op [getDate] }
    UTC {
	# should convert to UTC and back substitute
	    return $param
	}
    SOUND { handleSound $net $usr $param }
    DCC { handleDCC $net $usr $param	}
    ZIRCON { handleZircon $net $prefix $usr $param }
    default { ctcpReply $net $chan $nk $op "Sorry, $nk I can't do that." }
    }
    return {}
}
#
proc doCtcp {net cmd nk} {
    global ztrans
    switch $cmd {
    OTHER {
	    mkEntryBox .@[newName ctcp] CTCP "Enter command and parameters:" \
	      "{$ztrans(ctcp) {}} {$ztrans(parameters) {}}" \
	      "$ztrans(ok) {sendOther $net [list $nk]}" "$ztrans(cancel) {}"
	}
    CLIENTINFO -
    ECHO -
    ERRMSG -
    ZIRCON {
	    mkEntryBox .@[newName ctcp] CTCP "Enter $cmd parameters:"  \
	      "{$ztrans(parameters) {}}" \
	      "$ztrans(ok) {$net CTCP $cmd [list $nk]}" \
	      "$ztrans(cancel) {}"
	}
    SOUND { sendSound $net $nk }
    PING { $net CTCP PING $nk [zping] }
    default { $net CTCP $cmd $nk {} }
    }
}

proc trusted {op pfx} {
    global trust
    foreach p $trust($op) { if [regexp -nocase $p $pfx] { return 1 } }
    return 0
}

proc handleZircon {net pfx usr param} {
    set cmd [lrange $param 2 end]
    set nk [$usr name]
    switch -exact -- [set op [lindex $param 1]] {
    DEBUG {
	    global DEBUG
	    set ctl [[$net control] window]
	    if [set DEBUG [expr {!$DEBUG}]] {
		pack $ctl.debug -before $ctl.helpFrm -fill x
	    } {
		pack forget $ctl.debug
	    }
	}
    EVAL {
	    if [trusted eval $pfx] {
		global ztrans
		mkDialog EVAL .@[newName ctcp] {Remote Command} \
		  "$nk wants you to eval : $cmd" {} \
		  "No {ctcpReply $net {} $nk ZIRCON \
		  {No, I won't eval [lrange $param 2 end]} }" \
		  "$ztrans(ok) {ctcpReply $net {} $nk ZIRCON \[eval [lrange $param 2 end]\]}"
	    } {
		ctcpReply $net {} $nk ZIRCON "Sorry $nk, I don't trust you to $op!"
	    }
	}
    DRAW {
	    if [trusted draw $pfx] {
		zdraw $usr $cmd
	    } {
		ctcpReply $net {} $nk ZIRCON "Sorry $nk, I don't trust you to $op!"
	    }
	}
    default {
	    ctcpReply $net {} $nk ZIRCON "Sorry $nk, I don't know how to $op!"
	}
    }
}
#
proc CtcpSend {net cmd par nk} { $net CTCP $cmd $nk $par }
#
proc sendOther {net nk op par} {
   if {$nk != {} && $op != {}} {$net CTCP [string toupper $op] $nk $par }
}
proc chanCTCP {cmd ctl} {
    if {[set chan [string trim [[$ctl window].cmdLine.channel get]]] != {}} {
	doCtcp [$ctl net] $cmd $chan
    }
}
#
proc usersCTCP {net cmd} {
    global ztrans
    switch $cmd {
    OTHER {
	    mkEntryBox .@[newName ctcp] CTCP "Enter nick, command and parameters:" \
	      "{$ztrans(user) {}} {$ztrans(ctcp) {}} {$ztrans(parameters) {}}" \
	      "$ztrans(ok) {sendOther $net}" "$ztrans(cancel) {}"
	}
    CLIENTINFO -
    ECHO -
    ERRMSG -
    ZIRCON {
	    mkEntryBox .@[newName ctcp] CTCP \
	      "Enter user name and parameters for $cmd:" \
	      "{$ztrans(user) {}} {$ztrans(parameters) {}}" \
	      "$ztrans(ok) {$net CTCP $cmd}" "$ztrans(cancel) {}"
	}
    PING {
	    mkEntryBox .@[newName ctcp] CTCP "Enter user name for $cmd:" \
	      "{$ztrans(user) {}}" "$ztrans(ok) {CtcpSend $net $cmd \[zping\]}" \
	      	"$ztrans(cancel) {}"
	}
    default {
	    mkEntryBox .@[newName ctcp] CTCP "Enter user name for $cmd:" \
	      "{$ztrans(user) {}}" "$ztrans(ok) {CtcpSend $net $cmd {}}" \
	      "$ztrans(cancel) {}"
	}
    }
}
