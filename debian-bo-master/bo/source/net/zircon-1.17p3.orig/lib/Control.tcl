#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/Control.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
#
#
proc busyFlag {net} {if [$net busy] { } { } }
#
proc control_call {this op args} {
    upvar #0 $this cdata
    if [info exists cdata($op)] { return $cdata($op) }
    return [eval control_$op $this $args]
}
#
proc control_showServer {this srv} {
    entrySet [$this window].nSFrm.server.entry $srv
}
#
proc control_showNick {this nk} {
    entrySet [$this window].nSFrm.nickname.entry $nk
}
#
class Control {
    net		{}
    window	{}
    friends	{}
}
#
proc Control {cname args} {
    global zircon Ops DEBUG defChan monitorIn monitorOut OType ztrans
    set OType($cname) Control
    initObj $cname Control
    upvar #0 $cname cdata
    set ctl [set cdata(window) .$cname]
    proc $cname {args} "eval control_call $cname \$args"
    set net [set cdata(net) [lindex $args 1]] ;# hack alert.....
    toplevel $ctl -class Zircon -borderwidth 2
    wm title $ctl {Zircon Control Panel}
    wm iconname $ctl {Zircon Control}
    wm resizable $ctl 0 0
    wm protocol $ctl WM_DELETE_WINDOW "$net quit"

    frame $ctl.debug -borderwidth 0
    pack [frame $ctl.debug.l0 -background $zircon(sepColor) -borderwidth 2] -fill x \
      -pady 4 -side bottom
    checkbutton $ctl.debug.mo -text {Monitor Out} -variable monitorOut \
      -command zTrace
    checkbutton $ctl.debug.mi -text {Monitor In} -variable monitorIn \
      -command zTrace
    button $ctl.debug.dbg -text Debug -command zdebug
    pack $ctl.debug.mo $ctl.debug.mi $ctl.debug.dbg -side left -expand 1
    if $DEBUG { pack $ctl.debug -fill x }
    if {$monitorIn || $monitorOut} zTrace

    frame $ctl.helpFrm -borderwidth 0
    set om [makeMB $ctl.helpFrm.info "Zircon V$zircon(version)"]
    $om configure -tearoff 0
    $om add command -label "About Zircon" -command credits
    $om add cascade -label Configure -menu $om.conf
    menu $om.conf -tearoff 0
    foreach nn {IRC People Channels Windows Info} {
	$om.conf add command -label [trans $nn] \
	  -command "conf$nn $net"
    }
    foreach x {3} { $om.conf entryconfigure $x -state disabled}
    $om.conf add command -label {Save Current} -command "saveCurrent $net"
    $om.conf add command -label {Save Layout} -command "saveLayout $net"
    $om.conf add command -label {Reread rc} -command reread -state disabled
#    $om add cascade -label Register -menu $om.reg -state disabled
#    menu $om.reg -tearoff 0 -postcommand "register $net $om.reg"

    set om [makeMB $ctl.helpFrm.help Help]
    foreach x [glob -nocomplain $zircon(lib)/help/*] {
	set x [file tail $x]
	set nm [string tolower $x]
	$om add cascade -label $x -menu $om.$nm
	menu $om.$nm -tearoff 0 -postcommand "makeHelp $om.$nm $x"
    }
    if ![string match {} [$net helpService]] {
	$om add command -label "IRC Help Service" -command "getHelp $net"
    }

    pack $ctl.helpFrm.info $ctl.helpFrm.help -side left -expand 1
    pack $ctl.helpFrm -fill x

    pack [frame $ctl.l1 -background $zircon(sepColor) -borderwidth 2] -fill x -pady 4

    set oc [frame $ctl.cr -borderwidth 0]
    checkbutton $oc.busy -text $ztrans(busy) -command "busyFlag $net" \
      -variable ${net}(busy)
    checkbutton $oc.invis -text Invisible -command "$net setFlag invisible" \
      -variable ${net}(invisible)
    checkbutton $oc.wallop -text Wallop -command "$net setFlag wallops" \
      -variable ${net}(wallops)
    checkbutton $oc.srvmsg -text SrvMsg -command "$net setFlag srvmsg" \
      -variable ${net}(srvmsg)
    checkbutton $oc.ircop -text {IRC Op} -command "$net deIRCOp" \
      -variable ${net}(ircop) -state disabled -foreground red
    pack $oc.busy $oc.invis $oc.wallop $oc.srvmsg $oc.ircop -side left
    pack $oc -fill x
    pack [frame $ctl.l2 -background $zircon(sepColor) -borderwidth 2] -fill x -pady 4

    frame $ctl.nSFrm -borderwidth 0
    NNSBuild $ctl Nickname nickname [$net nicks] $net
    NNSBuild $ctl IRCName ircname [$net ircnames] $net

    frame $ctl.nSFrm.server -borderwidth 0
    set sm [makeMB $ctl.nSFrm.server.label Server]
    set sl {}
    foreach nn [$net servers] {
	set hst [$nn host]
	if [string compare 6667 [set prt [$nn port]]] { append hst :$prt }
	if {[set x [lsearch $sl ${hst}]] >= 0} { continue }
	lappend sl [list $hst $nn]
    }
    foreach nn [lsort $sl] {
	set lbl [lindex $sl 1]
	$sm add command -label [lindex $nn 0] \
	   -command "$net changeServer [lindex $nn 1]"
    }

    emacsEntry $ctl.nSFrm.server.entry -relief sunken

    bind $ctl.nSFrm.server.entry <Return> "
	$net changeServer \[srvName \[%W get\]\]
    "
    bind $ctl.nSFrm.server.entry <Escape> "
	set h \[%W get\]
	regexp {(.*):.*} \$h m h
	mkEntryBox {} {Port Number} \"Enter port number for \$h:\" \
	  {{$ztrans(port) 6667}} \
	  {$ztrans(ok) {$net changeServerPort \$h}} {$ztrans(cancel) {}}
    "

    pack $ctl.nSFrm.server.label -side left
    pack $ctl.nSFrm.server.entry -side left -expand 1 -fill x
    pack $ctl.nSFrm.server -expand 1 -fill x

    frame $ctl.bf2 -borderwidth 0
    if [info exists Ops(server)] {
	set mn [makeMB $ctl.bf2.servers Servers]
	foreach cmd $Ops(server) {
	    $mn add command -label [trans $cmd] -command "serverCmd $net $cmd"
	}
	if [info exists Ops(ircSrv)] {
	    foreach cmd $Ops(ircSrv) {
		$mn add command -label [trans $cmd] -foreground red \
		  -command "serverCmd $net $cmd"
	    }
	}

	if ![$net ircop] { setState $ctl.bf2.servers.menu ircSrv disabled }
	pack $ctl.bf2.servers -fill x -side left -expand 1
    }
    if [info exists Ops(user)] {
	set mn [makeMB $ctl.bf2.users Users]
	foreach cmd $Ops(user) {
	    switch -exact -- $cmd {
	    DCC {
		   $mn add cascade -label $ztrans(dcc) -menu $mn.dcc
		    menu $mn.dcc -tearoff 0
		    foreach nn {List Send Chat Close} {
			$mn.dcc add command -label [trans $nn] \
			  -command "usersDCC $net $nn"
		    }
		}
	    CTCP { addCTCPMenu $net $mn {{}} }
	    default {
		    $mn add command -label [trans $cmd] -command "userCmd $net $cmd"
		}
	    }
	}
	if [info exists Ops(ircop)] {
	    foreach cmd $Ops(ircop) {
		$mn add command -label [trans $cmd] -foreground red \
		  -command "userCmd $net $cmd"
	    }
	}
	if ![$net ircop] { setState $ctl.bf2.users.menu ircop disabled }
	pack $ctl.bf2.users -side left -fill x -expand 1
    }
    set cm [makeMB $ctl.bf2.channels Channels]
    $cm add command -label $ztrans(favourites) -command "faves $net"
    $cm add separator    
    foreach cmd "Join Who List Mode Names Notice Monitor" {
	$cm add command -label [trans $cmd] \
	  -command "channel$cmd $net \[string trim \[$ctl.cmdLine.channel get\]\]"
    }
    addChanCTCPMenu $cm $cname
    $cm add separator
    foreach chan [$net channels] {
	if {[string compare $chan $defChan] && [$chan menu]} {
	    $cm add command -label [$chan name] -command "$chan sendJoin"
	}
    }
    makeMB $ctl.bf2.services Services
    set i 3
    $ctl.bf2.services.menu add command -label $ztrans(exec) \
      -command "$net exec"
    $ctl.bf2.services.menu add cascade -label $ztrans(script) -state disabled
    $ctl.bf2.services.menu add cascade -label $ztrans(plugin) -state disabled
    $ctl.bf2.services.menu add separator
    foreach chn [$net services] {
	$ctl.bf2.services.menu add cascade -label [$chn name] \
	  -menu $ctl.bf2.services.menu.$i
	set m [menu $ctl.bf2.services.menu.$i -tearoff 0]
	foreach nn [$chn ops] {
	    $m add command -label $nn -command "$chn do $nn"
	}
	incr i
    }

    pack [frame $ctl.l3 -background $zircon(sepColor) -borderwidth 2] -fill x -pady 4

    frame $ctl.bf1 -borderwidth 0
    makeMB $ctl.bf1.away Away
    $ctl.bf1.away.menu add command -label $ztrans(back) -command "$net AWAY"
    $ctl.bf1.away.menu add command -label $ztrans(new) -command "$net getAway"
    $ctl.bf1.away.menu add separator

    foreach act [$net aways] {
	$ctl.bf1.away.menu add command \
	  -label "[prune $act 15]" -command "$net AWAY {$act}"
    }

    button $ctl.bf1.brb -command "$net doBRB" -width 10 -text $ztrans(brb)
    set cdata(friends) [Friends frnd$net -control $cname]
    if [$cdata(friends) menu] {
	menubutton $ctl.bf1.friends -width 10 -text Friends \
	    -menu $ctl.bf1.friends.menu
	$cdata(friends) show
    } {
	button $ctl.bf1.friends -width 10 -text Friends \
	  -command "$cdata(friends) show"
	if [$net showFriends] { $ctl.bf1.friends invoke }
    }
    set qm [buttonmenu $ctl.bf1.quit -command "$net quit" \
      -width 10 -text $ztrans(quit)]
    bind $qm <Shift-1> {tkButtonDown %W}
    bind $qm <Shift-ButtonRelease-1> "
	set sc \[%W cget -command\]
	%W configure -command \"$net close\"
	tkButtonUp %W
	%W configure -command \$sc
    "
    menu $qm.menu -tearoff 0
    $qm.menu add command -label $ztrans(new) -command "getQuit $cname"
    $qm.menu add separator
    foreach x [$net signoffs] {
	$qm.menu add command -label [prune $x 15] -command "$net doQuit {$x}"
    }

    pack $ctl.bf1.away $ctl.bf1.brb $ctl.bf1.friends $ctl.bf1.quit \
	-side left -fill x -expand 1
    pack $ctl.bf2.channels $ctl.bf2.services -side left -fill x -expand 1
    frame $ctl.cmdLine -borderwidth 0 -relief flat
    label $ctl.cmdLine.label -relief flat -text " $ztrans(channel) "
    emacsEntry $ctl.cmdLine.channel -relief sunken
    pack $ctl.cmdLine.label -side left
    pack $ctl.cmdLine.channel -side left -expand 1 -fill x
    pack $ctl.nSFrm $ctl.bf1 $ctl.bf2 -fill x
    pack [frame $ctl.l4 -background $zircon(sepColor) -borderwidth 2] \
      -fill x -pady 4
    pack $ctl.cmdLine -fill x

    bind $ctl.cmdLine.channel <Return> "channelJoin $net \[%W get\] {}"

    tkwait visibility $ctl
    return $cname
}
#
# Build the Nickname and Ircname entries for the control window
#
proc NNSBuild {ctl lbl var lst net} {
    set frm $ctl.nSFrm
    set name [string tolower $lbl]
    pack $frm [frame $frm.$name -borderwidth 0] -fill x -pady 2
    set name $frm.$name

    set mn [makeMB $name.label $lbl]
    foreach nn [lsort $lst] {
	$mn add command -label $nn -command "$net change$lbl {$nn}"
    }
    emacsEntry $name.entry -relief sunken
    bind $name.entry <Return> "$net change$lbl \[%W get\]"

    pack $name.label -side left
    pack $name.entry -side left -expand 1 -fill x
    $name.entry insert end [lindex $lst 0]
    $net configure -$var [lindex $lst 0]
}
#
proc saveLayout {net} {
    global zircon
    set rc $zircon(prefdir)/layout
    if [file exist $rc] {
	file stat $rc st
	set mode $st(mode)
	set rc [glob $rc]
	exec mv $rc $rc.bak
	set bak 1
    } {
	set mode 0600
	set bak 0
    }
    if [catch {open $rc w $mode} desc] {
	mkDialog SAVE {} Error \
	  "Error when opening layout file for write - $desc" {}
	if $bak { exec mv $rc.bak $rc }
	return 0
    }
    foreach nt $zircon(nets) {
	set nm [$nt name]
	puts $desc "#\n# Netspace $nm\n#"
	puts $desc "layout $nm info [wm geometry [[$nt info] window]]"
	puts $desc "layout $nm control [wm geometry [[$nt control] window]]"
    }
    if [winfo exists .@ztrace] {
	puts $desc "layout $nm trace [wm geometry .@ztrace]"
    }
    if [winfo exists .@zdbg] {
	puts $desc "layout $nm debug [wm geometry .@zdbg]"
    }
    close $desc
    return 1
}
#
proc saveCurrent {net} {
    global defChan defMsg
    foreach x [$net channels] { $x configure -keep 1 }
    $defChan configure -keep 0
    foreach x [$net messages] { $x configure -keep 1 }
    $defMsg configure -keep 0
    saverc
}
#
proc reread {} {
}
#
proc faves {net} {
    if [winfo exists [set w .@faves$net]] {popup $w ; return}
    global ztrans
    toplevel $w -class Zircon
    wm title $w Favourites
    wm resizable $w 0 1
    wm protocol $w WM_DELETE_WINDOW "killWindow $w"

    set win [frame $w.chans -relief raised]
    scrollbar $win.vscroller -command "$win.chList yview" 
    text $win.chList -yscrollcommand "bsSet $win.vscroller" -width 14 \
      -height 12
    bindtags $win.chList none
    pack $win.chList -side left -fill y
    button $win.ok -text $ztrans(dismiss) -command "killWindow $w"
    pack $win.ok -expand 1 -side right -fill y
    pack $w.chans -expand 1 -fill y
    foreach chan [$net channels] {
	if [$chan menu] {
	    set fm $win.chList.$chan
	    set cm [makeMB $fm [$chan name]]
	    $cm configure -postcommand "favecheck $cm $chan"
	    foreach cmd "Join Who Names Notice Monitor" {
		$cm add command -label $cmd \
		  -command "channel${cmd} $net [$chan name]"
	    }
	    addCTCPMenu $net $cm $chan
	    $win.chList window create end -window $fm
  	}
    }
}
#
proc favecheck {menu chan} {
    if [$chan active] {
	foreach e {4 6} { $menu entryconfigure $e -state normal }
	foreach e {1 3 5} { $menu entryconfigure $e -state disabled }
    } {
	foreach e {4 6} { $menu entryconfigure $e -state disabled }
	foreach e {1 3 5} { $menu entryconfigure $e -state normal }
    }
}
#
#
proc keepQuit {ctl v} {
    set net [$ctl net]
    [$ctl window].bf1.quit add command -label "[prune $v 15]" \
      -command "$net doQuit {$v}"
    $net configure +signoffs $v
    uplevel #0 set confChange 1
}
#
proc getQuit {ctl} {
    global ztrans
    mkEntryBox {} Quit "Enter your new signoff message:" \
      "{$ztrans(signoff) {}}" "$ztrans(keep) {keepQuit $ctl}" \
      "$ztrans(quit) {[$ctl net] doQuit}" "$ztrans(cancel) {}"
}
#
proc control_ircItems {this state} {
    foreach cid [[$this net] channels] { $cid ircOp $state }
    set ctl [$this window]
    setState $ctl.bf2.servers.menu ircSrv $state
    setState $ctl.bf2.users.menu ircop $state
    $ctl.cr.ircop configure -state $state
}
