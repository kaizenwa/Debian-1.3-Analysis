#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/configure.tcl,v $
# $Date: 1996/06/26 11:58:54 $
# $Revision: 1.17.1.4 $
#
#
# Adjust configuration and rc file.
#
#
proc confInit {net which} {
    global cVars new$net $net
    array set new$net [array get $net]
    switch -exact -- $which {
    People {
	    global newFrd delFrd
	    set newFrd($net) {}
	    set delFrd($net) {}
	    Friend :: pack $net
	}
    Channels {
	    global newChn delChn
	    set newChn($net) {}
	    set delChn($net) {}
	    Channel :: pack $net
	}
    IRC {
	    global newSv delSv svindx
	    set newSv($net) {}
	    set delSv($net) {}
	    set svindx($net) {}
	    Server :: pack $net
	}
    }
}
#
proc confCopyBack {net which} {
    global cVars defChan confChange zircon
    upvar #0 new$net newd
    set win .@cf$which$net
    switch $which {
    Channels {
	    global newChn delChn
	    foreach v $delChn($net) {
		if [$v active] {
		} {
		    $v delete
		    uplevel #0 unset new$v
		}
	    }
	    copybackChan $net
	}
    IRC {
	    global newSv delSv svd$net
	    confDirty $win
	    set newSv($net) {}
	    foreach v $delSv($net) {
# dont allow current server to be deleted!!
		if ![string compare [$net hostid] $v] {
		} {
		    $v unpack $net
		    $v delete
		}
	    }
	    set delSv($net) {}
	    Server :: unpack $net
	    if [info exists svd$net] {
		unset svd$net
	    }
	}
    People {
	    global newFrd delFrd
	    foreach v $delFrd($net) {
		$v unpack $net
		$v delete
	    }
	    Friend :: unpack $net
	}
    Info {
	    set newd(helpService) [.@cfInfo$net.misc1.help.entry get]
	    set newd(notifyInterval) \
	      [expr [.@cfInfo$net.misc1.ison.entry get] *1000]
	    set newd(listPattern) [.@cfInfo$net.filter2.entry get]
	    set newd(topicPattern) [.@cfInfo$net.filter3.entry get]
	}
    }
    foreach v $cVars($which) {
	if [string compare [$net $v] $newd($v)] {
	    $net configure -$v $newd($v)
	    confDirty $win
	}
    }
    unset newd
    if $confChange {
	switch -exact -- $which {
	People { $net setupUsers }
	IRC {
		set name [[$net control] window].nSFrm
		$name.nickname.label.menu delete 0 last
		foreach nn [lsort [$net nicks]] {
		    $name.nickname.label.menu add command -label "$nn" \
		      -command "$net changeNickname {$nn}"
		}
		$name.ircname.label.menu delete 0 last
		foreach nn [lsort [$net ircnames]] {
		    $name.ircname.label.menu add command -label "$nn" \
		      -command "$net changeIRCName {$nn}"
		}
		$name.server.label.menu delete 0 last
		foreach nn [lsort [$net servers]] {
		    $name.server.label.menu add command \
		       -label [$nn host] -command "$net changeServer $nn"
		}
	    }
	Info { }
	Channels {
		set cm [[$net control] window].bf2.channels.menu
		while {![catch {$cm entrycget last -label}]} {$cm delete last}
		foreach chn [$net channels] {
		    if [$chn menu] {
			$cm add command -label [$chn name] \
			  -command "$chn sendJoin"
		    }
		}
	    }
	    global defChan
	    [$net info] configure -history [$defChan history] \
	       -closetime [$defChan $closetime]
	}
    }
}
#
proc confApply {net which} {
    confCopyBack $net $which
    confInit $net $which
    confCleaner .@cf$which$net
}
#
proc confRevert {net which} {
    confDismiss $net $which
    conf$which $net
    confClean .@cf$which$net
}
#
proc confSave {net which} {
    if [string compare $which all] { confCopyBack $net $which }
    saverc
    confInit $net $which
    confClean .@cf$which$net
}
#
proc confDismiss {net which} {
    uplevel #0 unset new$net
    killWindow .@cf$which$net
    switch $which {
    IRC {
	    global newSv delSv svindx
	    foreach v $newSv($net) { $v delete }
	    unset newSv($net) delSv($net) svindx($net)
	    Server :: cleanup new
	}
    Channels {
	    global newChn delChn
	    foreach v $newChn($net) {
		if ![$v sys] { $v delete }
	    }
	    Channel :: cleanup net
	    unset newChn($net) delChn($net)
	}
    People {
	    global newFrd delFrd
	    foreach v $newFrd($net) { $v delete }
	    unset newFrd($net) delFrd($net)
	}
    }
}
#
proc doCAN {net pos name lst win dflt val} {
    if [string match {} $val] return
    upvar #0 new$net new
    set x [lsearch $new($name) "$val"]
    set lvl $val
    set edit [expr {$pos != ([$win size] - 1)}]
    if {$x >= 0}  {
	if {$edit && [string compare $x $pos]} return
	if $dflt {
	    $win delete $x
	    $win insert 0 [expr {$lst ? [lindex $val 0] : $val}]
	    listmove new($name) $x 0 $lvl
	}
    } {
	if $dflt {
	    if $edit { listdel new($name) $pos ; $win delete $pos }
	    set new($name) [linsert $new($name) 0 $lvl]
	    $win insert 0 [expr {$lst ? [lindex $val 0] : $val}]
	} {
	    if $edit {
		listupdate new($name) $pos $lvl
		$win delete $pos
		$win insert $pos [expr {$lst ? [lindex $val 0] : $val}]
	    } {
		lappend new($name) $lvl
		$win insert [expr {[$win size] - 1}] \
		  [expr {$lst ? [lindex $val 0] : $val}]
	    }
	}
    }
    confDirty .@cfIRC$net
}
#
proc confAddIt {net win y typ ltyp var} {
    global ztrans
    set val [$win get [set pos [$win nearest $y]]]
    if ![string compare *NEW* $val] {
	mkEntryBox .@cadd "New $typ" "Enter the new $ltyp:" \
	  "{$typ {}}" "$ztrans(ok) {doCAN $net $pos $var 0 $win 0}" \
	  "$ztrans(default) {doCAN $net $pos $var 0 $win 1}" \
	  "$ztrans(cancel) {}"
    } {
	$win selection set $pos
	mkEntryBox .@cadd "Edit $typ" "Edit the $ltyp:" \
	  "{[trans $typ] {$val}}" \
	  "$ztrans(ok) {doCAN $net $pos $var 0 $win 0}" \
	  "$ztrans(default) {doCAN $net $pos $var 0 $win 1}" \
	  "$ztrans(delete) {confDel $net $var $win}" "$ztrans(cancel) {}"
    }
    tkwait window .@cadd
}
#
proc confAddNickname {net win y} {
    confAddIt $net $win $y Nickname nickname nicks
}
#
proc confAddIRCName {net win y} {
    confAddIt $net $win $y IRCname {IRC name} ircnames
}
#
proc doCAS {net sid pos win dflt hst prt onk opw} {
    if [string match {} $hst] return
    if ![string match {} $sid] {
	set lnm [string tolower [set nm [$win get $pos]]]
	upvar #0 new$sid new
	if [string compare $lnm [string tolower $hst]] {
	    set new(host) $hst
	    $win delete $pos
	    $win insert $pos $hst
	    $win selection set $pos
	}
	if [string match {} $prt] { set prt 6667 }
	set new(port) $prt
	set new(oper) $onk
	set new(operpw) $opw
	if $dflt { global svd$net ; set svd$net $sid }
    } {
	global newSv svindx
	set id [Server [newName _srvr] -host $hst -oper $onk -operpw $opw]
	if [string match {} $prt] { setprt 6667 }
	$id configure -port $prt
	lappend newSv($net) $id
	lappend svindx($net) $id
	set last [expr [$win index end] - 1]
	$win insert $last $hst
	$win selection set $last
	$id pack $net
	if $dflt { global svd$net ; set svd$net $id }
    }
    confDirty .@cfIRC$net
}
#
proc confDel {net var win args} {
    upvar #0 new$net newd
    set size [expr {[$win size] - 1}]
    foreach l [set t [$win curselection]] {
	if ![string compare $l $size] break
	switch $var {
	servers {
	    global newSv delSv svindx
	    set lnm [string tolower [$win get $l]]
	    set id [lindex $svindx($net) $l]
	    if {[set x [lsearch $newSv($net) $id]] >= 0} {
		$id unpack $net
		$id delete
		listdel newSv($net) $x
	    } \
	    elseif {![string compare $id [$net hostid]]} {
		mkDialog {} {} {Delete error} \
		  {You cannot delete the active server!} {}
	    } {
		lappend delSv($net) $id
	    }
	}
	friends {
	    global newFrd delFrd
	    upvar #0 ${net}FTO newt
	    set id $newt([set lnm [string tolower [$win get $l]]])
	    $id unpack $net
	    if {[set x [lsearch $newFrd($net) $id]] >= 0} {
		$id delete
		listdel newFrd($net) $x
	    } {
		$id configure -menu 0
	    }
	}
	default {
		set cl [expr {[llength $t] - 1}]
		while {[set m [lindex $t $cl]] == $size} { incr cl -1 }
		set newd($var) [lreplace $newd($var) $l $m]
	    }
	}
    }
    switch $var {
    servers {
	    global svindx
	    if [string match {} $t] {
		set svindx($net) {}
	    } {
		set svindx($net) [lreplace $svindx($net) \
		  [lindex $t 0] [lindex $t end]]
	    }
	}
    }
    catch {$win delete [lindex $t 0] [lindex $t end]}
    confDirty [winfo toplevel $win]
}
#
proc confAddServer {net win y} {
    global ztrans
    set val [$win get [set pos [$win nearest $y]]]
    if ![string compare *NEW* $val] {
	mkEntryBox .@cas$net {New Server} {Enter the new server details:} \
	  "{$ztrans(hostname) {}} {$ztrans(port) 6667} {{Op Nick} {}} \
	  {{Op passwd} {}}" "$ztrans(ok) {doCAS $net {} $pos $win 0}" \
	  "$ztrans(default) {doCAS $net {} $pos $win 1}" "$ztrans(cancel) {}"
    } {
	global svindx
	set sid [lindex $svindx($net) $pos]
	upvar #0 new$sid new
	$win selection set $pos
	mkEntryBox .@cas$net {Edit Server} {Edit the server details:} \
	  "{$ztrans(hostname) $val} {$ztrans(port) $new(port)} \
	  {{Op Nick} $new(oper)} {{Op passwd} $new(operpw)}" \
	  "$ztrans(ok) {doCAS $net $sid $pos $win 0}" \
	  "$ztrans(default) {doCAS $net $sid $pos $win 1}" \
	  "$ztrans(delete) {confDel $net servers $win}" "$ztrans(cancel) {}"
    }
    tkwait window .@cas$net
}
#
proc confEnt {net win var title} {
    set name [string tolower $title]
    set winn $win.$name
    frame $winn -relief raised
    label $winn.label -text "${title}s"
    set winnl $winn.list
    makeLB $winnl -setgrid 1 -relief flat
    switch $title {
    Friend {foreach v [$net friends] {$winnl.l insert end [$v name]}	}
    Server {
	    global svindx
	    foreach v [$net servers] {
		if ![$v sys] {
		    $winnl.l insert end [$v host]
		    lappend svindx($net) $v
		}
	    }	
	}
    default {foreach v [$net $var] {$winnl.l insert end $v}}
    }
    $winnl.l insert end *NEW*
    bind $winnl.l <Double-Button-1> "confAdd${title} $net %W %y"
    bind $winnl.l <Delete> "confDel $net $var %W"
    bind $winnl.l <BackSpace> [bind $winnl.l <Delete>]
    bind $winnl.l <Control-h> [bind $winnl.l <Delete>]
    pack $winn.label
    pack $winn.list -expand 1 -fill both
    pack $winn -side left -expand 1 -fill both
    bind $winn <Enter> "focus $winnl.l"
}
#
proc confIRC {net} {
    set win .@cfIRC$net
    if [winfo exists $win] { popup $win ; return }
    toplevel $win -class Zircon
    wm title $win "IRC Configuration"
    wm protocol $win WM_DELETE_WINDOW "confDismiss $net IRC"
    confInit $net IRC
    frame $win.data
    confEnt $net $win.data nicks Nickname
    confEnt $net $win.data ircnames IRCName
    confEnt $net $win.data servers Server
    confMkBtn $net $win IRC
    pack $win.data -expand 1 -fill both
    pack $win.btn -fill x
}
#
proc doCAF {net pos win id ntfy unm uh} {
    if [string match {} $unm] return
    set unm [$net trimNick $unm]
    set last [expr [$win size] - 1]
    if [string match {} $id] { set id [Friend :: make $unm] }
    if [string compare $pos $last] {
	upvar #0 ${net}FTO newFTO new$id newu
	set nm [$win get $pos]
	set lnm [string tolower $nm]
	set id $newFTO($lnm)
	if {[string compare $lnm [set lu [string tolower $unm]]] ||
	  [string compare $uh $newu(id)]} {
	    set newu(name) $unm
	    set newu(lname) $lu
	    set newu(notify) $ntfy
	    set newu(menu) 1
	    set newu(id) $uh
	    unset newFTO($lnm)
	    set newFTO($lu) $id
	    $win delete $pos
	    $win insert $pos $unm
	    $win selection set $pos
	}
    } {
	global newFrd
	$id configure -menu 1 -notify $ntfy -id $uh
	$id pack $net
	lappend newFrd($net) $id
	$win insert $last $unm
	$win selection set $last
    }
    confDirty .@cfPeople$net
}
#
proc confAddFriend {net win y} {
    global ztrans
    set val [$win get [set pos [$win nearest $y]]]
    if ![string compare *NEW* $val] {
	mkEntryBox .@can$net {New Friend} {Enter the new friend's nickname:} \
	  "{$ztrans(nickname) {}} {{User@Host Pattern} {}}" \
	  "$ztrans(ok) {doCAF $net $pos $win {} 0}" \
	  "{Notify On} {doCAF $net $pos $win {} 1}" "$ztrans(cancel) {}"
    } {
	$win selection set $pos
	set id [Friend :: find $val]
	upvar #0 new$id newu
	set nf [$id notify]
	mkEntryBox .@can$net {Edit Friend} {Edit the Friend's nickname:} \
	  "{$ztrans(nickname) {$val}} {{User@Host Pattern} {$newu(id)}}" \
	  "$ztrans(ok) {doCAF $net $pos $win $id [expr {!$nf}]}" \
	  "{Notify [expr {$nf ? {On} : {Off}}]} {doCAF $net $pos $win $id $nf}" \
	  "$ztrans(delete) {confDel $net friends $win}" "$ztrans(cancel) {}"
    }
    tkwait window .@can$net
}
#
proc doCAI {net pos win val} {
    if {[set val [string trim [string tolower $val]]] == {}} { return }
    global confISel
    upvar #0 new$net new
    regexp {^([^!@]*)!?([^@]*)@?(.*)} $val m p1 p2 p3
    if [string match {} $p1] { set p1 * }
    if [string match {} $p2] { set p2 * }
    if [string match {} $p3] { set p3 * }
    set val $p1!$p2@$p3
    set x [listmatch $new(ignores) $val]
    if [set edit [expr {$pos != ([$win size] -1)}]] {
	if {$x >= 0 && $x != $pos} return
	set v [lindex $new(ignores) $pos]
	listupdate new(ignores) $pos [list $val [lindex $v 1]]
	$win delete $pos
	$win insert $pos $val
    } \
    elseif {$x < 0} {
	lappend new(ignores) [list $val {}]
	set pos [expr [$win size] - 1]
	$win insert $pos $val
	$win selection set $pos
	set confISel($net) $val
	setIB $net $val
    }
    confDirty .@cfPeople$net
}
#
proc doConfIgnore {net indx} {
    global confISel
    upvar #0 new$net new
    if ![string match {} $confISel($net)] {
	global confI zircon
	set dx [listmatch $new(ignores) $confISel($net)]
	set chin [lindex $new(ignores) $dx]
        set val [lindex $chin 1]
	set vdx [lsearch $val $indx]
	if !$confI($indx) {
	    if {$vdx >= 0} { listdel val $vdx }
	} \
	elseif {$vdx < 0} { lappend val $indx }
	if {$dx >= 0} {
	    listupdate new(ignores) $dx [list $confISel($net) $val]
	} {
	    lappend new(ignores) [list $confISel($net) $val]
	}
    }
    confDirty .@cfPeople$net
}
#
proc confAddIgnore {net win y} {
    global ztrans
    set val [$win get [set pos [$win nearest $y]]]
    if ![string compare *NEW* $val] {
	mkEntryBox .@ci {New ignore} {Enter the nickname/username to ignore:} \
	  "{$ztrans(nickname) {}}" "$ztrans(ok) {doCAI $net $pos $win}" "$ztrans(cancel) {}"
    } {
	$win selection set $pos
	mkEntryBox .@ci {Edit Nick} {Edit the nickname:} \
	  "{$ztrans(nickname) {$val}}" "$ztrans(ok) {doCAI $net $pos $win}" \
	  "Delete {confDel $net ignores $win}" "$ztrans(cancel) {}"
    }
}
#
proc changeIgnore {net dbl win y} {
    global confISel
    if {[set confISel($net) [$win get [set p [$win nearest $y]]]] == "*NEW*"} {
	if $dbl { confAddIgnore $net $win } {
	    global zircon
	    foreach b $zircon(ignore) {
		set l [string tolower $b]
		.@cfPeople$net.data.idata.ignore.ign2.$l configure -state disabled
	    }
	    if ![string match {} [set s [$win curselection]]] { $win selection clear $s }
	    set confISel($net) {}
	}
    } {
	$win selection set $p
	setIB $net $confISel($net)
    }
}
#
proc setIB {net nk} {
    global zircon confI
    upvar #0 new$net new
    set modes [lindex [lindex $new(ignores) [listmatch $new(ignores) $nk]] 1]
    foreach b $zircon(ignore) {
	set lb [string tolower $b]
	set confI($lb) [expr {[lsearch $modes $lb] >= 0}]
	.@cfPeople$net.data.idata.ignore.ign2.$lb configure -state normal
    }
}
#
proc confPeople {net} {
    upvar #0 new$net new
    set win .@cfPeople$net
    if [winfo exists $win] { popup $win ; return }
    toplevel $win -class Zircon
    wm title $win {People Configuration}
    wm protocol $win WM_DELETE_WINDOW "confDismiss $net People"
    confInit $net People
    global confISel zircon
    set confISel($net) {}
    frame $win.data
    frame $win.data.fdata -relief raised
    confEnt $net $win.data.fdata friends Friend
    set winn [frame $win.data.idata -relief raised]
    label $winn.label -text Ignores
    frame $winn.ignore
    set winn1 $winn.ignore.ign1
    makeLB $winn1 -setgrid 1 -relief flat
    foreach v [$net ignores] { $winn1.l insert end [lindex $v 0] }
    $winn1.l insert end *NEW*
    bind $winn1.l <ButtonPress-1> "changeIgnore $net 0 %W %y"
    bind $winn1.l <Double-Button-1> "confAddIgnore $net %W %y"
    bind $winn1.l <Delete> "confDel $net ignores %W"
    bind $winn1.l <BackSpace> [bind $winn1.l <Delete>]
    bind $winn1.l <Control-h> [bind $winn1.l <Delete>]
    bind $winn1 <Enter> {focus %W.l}
    frame $winn.ignore.ign2
    foreach v $zircon(ignore) {
	set lv [string tolower $v]
	checkbutton $winn.ignore.ign2.$lv -text $v -state disabled \
	  -variable confI($lv) -command "doConfIgnore $net $lv"
	pack  $winn.ignore.ign2.$lv -anchor w
    }
    pack $winn.ignore.ign1 $winn.ignore.ign2 -side left -expand 1 -fill both
    pack $winn.label $winn.ignore
    confMkBtn $net $win People
    pack $win.data.fdata $win.data.idata -side left -expand 1 -fill both
    pack $win.data -expand 1 -fill both
    frame $win.misc
    checkbutton $win.misc.on -text {Friends On} -variable new(friendsOn)
    checkbutton $win.misc.sf -text {Show Friends} -variable new(showFriends)
    pack $win.misc.on $win.misc.sf -expand 1 -fill x -side left
    pack $win.misc $win.btn -fill x
}
#
proc confMkBtn {net win type} {
    global confChange
    frame $win.btn
    foreach bt {Revert Apply Save Dismiss} {
	set lbt [string tolower $bt]
	button $win.btn.$lbt -command "conf$bt $net $type" -text $bt
	pack $win.btn.$lbt -side left -expand 1 -fill x
    }
    if !$confChange {confClean $win}
}
#
proc confDirty {win} {
    global confChange
    set confChange 1
    foreach bt {revert apply save} { $win.btn.$bt configure -state normal }
}
#
proc confCleaner {win} {
    global confChange
    set confChange 0
    foreach bt {apply} { $win.btn.$bt configure -state disabled }
}
#
proc confClean {win} {
    global confChange
    set confChange 0
    foreach bt {revert apply save} { $win.btn.$bt configure -state disabled }
}
