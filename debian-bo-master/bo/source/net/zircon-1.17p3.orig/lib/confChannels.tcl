#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/confChannels.tcl,v $
# $Date: 1996/07/01 09:25:47 $
# $Revision: 1.17.1.3 $
#
#
proc editChan {net pos win chan} {
    global selID
    upvar #0 new$selID($net) news
    if {[string match {} $chan] || \
      ![string compare [set lchan [string tolower $chan]] [$selID($net) lname]]} return
    $win delete $pos
    $win insert $pos $chan
    $win selection set $pos
    set news(name) $chan
    set news(lname) $lchan
}
#
proc changeChan {net dbl win y} {
    global selID defChan ztrans
    saveChan $net
    if ![string compare [set cnm [$win get [set p [$win nearest $y]]]] *NEW*] {
	if $dbl {
	    set selID($net) nil
	    setCCB $net {}
	    confAddChan $net $win
	} {
	    incr p -1
	    $win selection set $p
	    set selID($net) $defChan
	    setCCB $net *DEFAULT*
	}
    } {
	set selID($net) [Channel :: find $cnm]
	setCCB $net $cnm
	$win selection set $p
	if {$dbl && [string compare $selID($net) $defChan]} {
	    tkwait window [mkEntryBox {} {Edit Channel} \
	      {Edit the channel name:} "{$ztrans(channel) {$cnm}}" \
	      "$ztrans(ok) {editChan $net $p $win }" \
	      "$ztrans(delete) {confDelChan $net $win $y}" \
	      "$ztrans(cancel) {}"]
	}
    }
}
#
proc setCCB {net chan args} {
    global confData confB 
    set w .@cfChannels$net.chan
    if [string match {} $chan] {
	foreach wc [winfo children $w.options] {
	    if [string compare $wc $w.options.msg] { $wc conf -state disabled }
	}
	foreach i {history closetime icon1 icon2 logfile key} {
	   $w.values.$i.entry conf -state disabled
	}
	return
    }
    foreach wc [winfo children $w.options] {
	if [string compare $wc $w.options.msg] { $wc conf -state normal }
    }
    foreach wc [winfo children $w.values] {$wc.entry conf -state normal}
    set chan [Channel :: find $chan]
    upvar #0 new$chan new
    foreach v $confData(channel) {
	set tn [lindex $v 1]
	$w.options.$tn configure -variable new${chan}($tn)
    }
    foreach b $confData(msg) {
	set b [string toupper $b]
	set lb [string tolower $b]
	set confB($lb) [expr {[lsearch $new(msg) $b] < 0}]
    }
    entrySet $w.values.history.entry [$chan history]
    entrySet $w.values.closetime.entry [$chan closetime]
    set v $new(icon)
    entrySet $w.values.icon1.entry [lindex $v 0]
    entrySet $w.values.icon2.entry [lindex $v 1]
    entrySet $w.values.logfile.entry [$chan logfile]
    entrySet $w.values.key.entry [$chan key]
}
#
proc CancelCAC {net win args} {
    global selID defChan
    set selID($net) $defChan
    setCCB $net *DEFAULT*
    $win selection set [expr [$win size] - 2]
}
#
proc doCAC {net win chan} {
    if ![string match {} $chan] {
	if ![string match {[&#]*} $chan] {set chan "#$chan"}
	set chid [Channel :: make $chan]
	upvar #0 new$chid newc
	if ![array exists newc] {
	    set x [expr {[$win size] - 2}]
	    $win insert $x $chan
	    $chid pack new
	}
	uplevel #0 set selID($net) $chid
	uplevel #0 lappend newChn($net) $chid
	set newc(keep) 1
	set newc(sys) 0
	$win selection set $x
	setCCB $net $chan
	confDirty [winfo toplevel $win]
    }
}
#
proc confAddChan {net win} {
    global ztrans
    tkwait window [mkEntryBox {} {New Channel} {Enter the channel name:} \
      "{$ztrans(channel) {}}" "$ztrans(ok) {doCAC $net $win}" \
      "$ztrans(cancel) {CancelCAC $net $win}"]
}
#
proc confDelChan {net win y args} {
    if [string match {} [set dx [$win curselection]]] { set dx [$win nearest y] }
    if {$dx < [expr {[$win size] - 2}]} {
	$win delete $dx
	global selID newChn delChn
	uplevel #0 unset new$selID($net)
	$selID($net) configure -keep 0
	if {[set x [lsearch $newChn($net) $selID($net)]] >= 0} {
	    listdel newChn($net) $x
	    if ![$selID($net) active] {
		$selID($net) configure -keep 0
		$selID($net) delete
	    }
	} {
	    lappend delChn($net) $selID($net)
	}
	set cnm [string tolower [$win get $dx]]
	$win selection set $dx
	set selID($net) [Channel :: find $cnm]
	setCCB $net $cnm
    }
    confDirty [winfo toplevel $win]
}
#
proc confChannels {net} {
    set win .@cfChannels$net
    if [winfo exists $win] { popup $win ; return }
    confInit $net Channels
    toplevel $win -class Zircon
    wm title $win {Channel Configuration}
    wm iconname $win Channels
    wm protocol $win WM_DELETE_WINDOW "confDismiss $net Channels"
    set winc [frame $win.chan -relief raised]
    set wincn [frame $winc.nels]
    label $wincn.label -text Channels
    makeLB $wincn.list -setgrid 1 -selectmode single
    global confData selID defChan
    foreach c [$net channels] {
	if {[string compare $c $defChan] && ![$c sys] && [$c keep]} {
	    $wincn.list.l insert end [$c name]
	}
    }
    set selID($net) $defChan
    $wincn.list.l insert end *DEFAULT*
    $wincn.list.l insert end *NEW*
    $wincn.list.l selection set [expr {[$wincn.list.l size] - 2}]
    bind $wincn.list.l <Delete> "confDelChan $net %W %y"
    bind $wincn.list.l <BackSpace> "confDelChan $net %W %y"
    bind $wincn.list.l <Control-h> "confDelChan $net %W %y"
    bind $wincn.list.l <Button-1> "changeChan $net 0 %W %y"
    bind $wincn.list.l <Double-Button-1> "changeChan $net 1 %W %y"
    pack $wincn.label
    pack $wincn.list -expand 1 -fill both
    frame $winc.options
    foreach opt $confData(channel) {
	set tn [lindex $opt 1]
	checkbutton $winc.options.$tn -text [lindex $opt 0] \
	  -variable new${defChan}($tn) -command "confDirty .@cfChannels$net"
	pack $winc.options.$tn -anchor w
    }
    label $winc.options.msg -text Messages
    pack $winc.options.msg -anchor w
    foreach opt $confData(msg) {
	set val [string tolower $opt]
	checkbutton $winc.options.msg$val -text $opt \
	  -variable confB($val) -command "doConfButton $net $val"
	pack $winc.options.msg$val -anchor w
    }
    frame $winc.values
    labelEntry 0 $winc.values.history {-text History -width 12} {} {}
    set le $winc.values.history.entry
    bind $le <Return> {}
    bind $le <KeyPress> {
	switch -glob -- %A { [0-9+-] { %W insert insert %A } }
	break
    }
    labelEntry 0 $winc.values.closetime {-text {Close Time} -width 12} {} {}
    set le $winc.values.closetime.entry
    bind $le <Return> {}
    bind $le <KeyPress> {
	switch -glob -- %A { [0-9+-] { %W insert insert %A }}
	break
    }
    labelEntry 0 $winc.values.icon1 {-text Icon -width 12} {} {}
    labelEntry 0 $winc.values.icon2 {-text {Active Icon} -width 12} {} {}
    labelEntry 0 $winc.values.logfile {-text {Log File} -width 12} {} {}
    labelEntry 0 $winc.values.key {-text Key -width 12} {} {}
    pack $winc.values.history $winc.values.closetime $winc.values.icon1 \
      $winc.values.icon2 $winc.values.logfile $winc.values.key -expand 1 -fill x
    pack $winc.nels $winc.options $winc.values -side left -expand 1 -fill both
    setCCB $net *DEFAULT*
    bind $wincn <Enter> "focus $wincn.list.l"
    confMkBtn $net $win Channels
    pack $win.chan -expand 1 -fill both
    pack $win.btn -fill x
}
#
proc saveChan {net} {
    global selID
    if ![string compare nil $selID($net)] { return }
    upvar #0 new$selID($net) new
    set w .@cfChannels$net.chan.values
    foreach e {closetime history logfile key} {
	set v [$w.$e.entry get]
	if [string compare $new($e) $v] { set new($e) $v }
    }
    set v1 [list [$w.icon1.entry get] [$w.icon2.entry get]]
    if ![string compare $v1 {{} {}}] { set v1 {} }
    if [string compare $v1 $new(icon)] { set new(icon) $v1 }
}
#
proc doConfButton {net indx} {
    global selID defChan
    if [string compare nil $selID($net)] {
	global confB
	upvar #0 new$selID($net) new
	
	set vdx [lsearch $new(msg) [set v [string toupper $indx]]]
	if !$confB($indx) {
	    if {$vdx < 0} { lappend new(msg) $v }
	} {
	    if {$vdx >= 0} { listdel new(msg) $vdx }
	}
	confDirty .@cfChannels$net
    }
}
#
proc copybackChan {net} {
    global defChan
    saveChan $net
    foreach ch [info globals newchann*] { [string range $ch 3 end] unpack new }
    $net configure -closeTime [expr [$defChan closetime] * 1000]
    confDirty .@cfChannels$net
}
