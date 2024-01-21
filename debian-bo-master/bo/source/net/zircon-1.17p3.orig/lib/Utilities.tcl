#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/Utilities.tcl,v $
# $Date: 1996/06/07 12:00:31 $
# $Revision: 1.17.1.2 $
#
proc makeArray {args} {
    foreach x $args { global $x ; set ${x}(1) 1 ; unset ${x}(1) }
}
#
proc channel {w} {
    global Name
    if {![catch {set win [winfo toplevel $w]}]  && [info exists Name($win)]} {
	return $Name($win)
    }
    return nil
}
#
proc window {chan} { return [[find $chan] window] }
#
proc normal {w} {return [string match normal [$w cget -state]]}
#
proc capitalise {str} {return [string toupper [string index $str 0]][string range $str 1 end]}
#
proc getOption {var dflt} {
    global $var
    if [string match {} [set $var [option get . $var [capitalise $var]]]] {
	set $var $dflt
    }
}
#
# List utilities
#
proc listkill {list val} {
    upvar $list lst
    if {[set x [lsearch $lst $val]] >= 0} { set lst [lreplace $lst $x $x]}
}
#
proc listmatch {list val} {
    set i 0
    foreach item $list {
	if ![string compare [lindex $item 0] $val] { return $i }
	incr i
    }
    return -1
}
#
proc listdel {v item} {
    upvar $v lst
    catch {set lst [lreplace $lst $item $item]}
}
#
proc listupdate {list item val} {
    upvar $list lst
    while {[llength $lst] <= $item} {lappend lst {} }
    set lst [lreplace $lst $item $item $val]
}
#
proc listmove {list from to val} {
    upvar $list lst
    set lst [linsert [lreplace $lst $from $from] $to $val]
}
#
# Procedure used to shorten menu labels to 10 characters. Used
# when adding user provided items to menus
#
proc prune {name lng} {
    regsub -all "\[\002\017\026\037\]" $name {} name
    return [expr {[string length $name] > $lng ? \
      "[string range $name 0 [expr {$lng - 3}]]..." : $name}]
}
#
proc killWindow {win} {
    global Icon IconBM
    catch "unset Icon($win)"
    catch "unset IconBM($win)"
    catch "destroy $win"
}
#
#	proc me : Returns true if nk is this user
#		  Assumes that nk is in lower case!!!
#
proc me {nk net} {
    return [expr ![string compare [string tolower $nk] [[$net myid] lname]]]
}
#
set ctcpCmds  {Clientinfo Echo Errmsg Finger Pid Ping Source Time
Version Userinfo Other}
set specialCmds {Zircon}
#
proc addCTCPMenu {net name usr} {
    global ctcpCmds DEBUG specialCmds ztrans zircon

    $name add cascade -label $ztrans(ctcp) -menu $name.ctcp
    menu $name.ctcp 
    set cmdlst $ctcpCmds
    if ![string match {} $zircon(wavplayer)] { lappend cmdlst Sound }
    if $DEBUG { lappend cmdlst Zircon }
    foreach cmd $cmdlst {
	if {$usr == {{}}} {
	    set prc "usersCTCP $net [string toupper $cmd]"
	    $name.ctcp configure -tearoff 0
	} {
	    set prc "doCtcp $net [string toupper $cmd] \[$usr name\]"
	    catch {$name.ctcp configure -tearoffcommand "retitle \"CTCP menu for \[$usr name\]\""}
	}
	$name.ctcp add command -label [trans $cmd] -command $prc
    }
}
#
proc addChanCTCPMenu {name ctl} {
    global ctcpCmds DEBUG specialCmds ztrans
    $name add cascade -label $ztrans(ctcp) -menu $name.ctcp
    menu $name.ctcp -tearoff 0
    foreach cmd [expr {$DEBUG ? [concat $ctcpCmds $specialCmds] : $ctcpCmds}] {
	$name.ctcp add command -label [trans $cmd] \
	  -command "chanCTCP [string toupper $cmd] $ctl"
    }
}
#
proc addDCCMenu {name usr} {
    global ztrans
    $name add cascade -label $ztrans(dcc) -menu $name.dcc
    menu $name.dcc -tearoff 0
    foreach cmd {Send Chat} { $name.dcc add command -label [trans $cmd] \
      -command "doDCC [string toupper $cmd] \[$usr name\]" }
}
#
proc makeUserMenu {chid win usr} {
    if [winfo exists $win] {
	return $win
    } {
	return [menu $win -postcommand "postUM $chid $win $usr"]
    }
}
#
#
proc postUM {chid win usr} {
    global zircon ztrans ucmds Ops
    set w [winfo parent $win]
    set net [$usr net]
    set nrm [string compare nil $chid]
    array set ucmds "
	Whois	{$net WHOIS \[$usr lname\]}
	Message	{Message :: make \[$usr name\]}
	Notice	{channelNotice $net \[$usr lname\]}
	Time	{$net TIME \[$usr name\]}
	CTCP	{}
	DCC	{}
	Notify	{}
	Ignore	{}
	Finger	{}
	Speak	{}
	ChanOp	{}
	Kick	{$chid kick $usr}
	Ban	{doBan $net + $chid \[$usr name\]!*@*}
	BanKick {$chid banKick $usr}
	Kill	{}
    "
    foreach cmd $Ops(userMenu) {
	switch $cmd {
	CTCP { addCTCPMenu $net $win $usr }
	DCC { addDCCMenu $win $usr }
	Notify {
		$win add checkbutton -label $ztrans(notify) \
		  -variable ${usr}(notify) -command "$usr doNotify"
	    }
	Whois -
	Message -
	Notice -
	Time { $win add command -label [trans $cmd] -command $ucmds($cmd) }
	Ignore { if $nrm { addIgnoreMenu $win $usr } }
	Finger {
		if $nrm {
		    $win add command -label $ztrans(finger) \
		      -command "$usr finger"
		}
	   }
	}
    }
    if $nrm {
	set st [expr {[$chid operator] ? "normal" : "disabled"}]
	foreach cmd $Ops(chanop) {
	    switch $cmd {
	    Speak {
		$win add checkbutton -label $ztrans(speak) \
		  -variable ${chid}(Spk,$usr) \
		  -command "$chid userMode $usr v" -state $st
	      }
	    ChanOp {
		$win add checkbutton -label $ztrans(chanop) \
		  -variable ${chid}(Op,$usr) \
		  -command "$chid userMode $usr o" -state $st
	      }
	    Kick -
	    Ban -
	    BanKick {
		$win add command -label [trans $cmd] \
		  -command $ucmds($cmd) -state $st
	      }
	    }
        }
	foreach cmd $Ops(ircop) {
	    switch $cmd {
	    Kill {
		$win add command -label $ztrans(kill) -command "$usr kill" \
		  -state [expr {[$net ircop] ? {normal} : {disabled}}] \
		  -foreground red
	      }
	    }
	}
    }
    $win configure -postcommand {}
}
#
proc invert {button} {
    set fg [$button cget -foreground]
    set bg [$button cget -background]
    $button conf -foreground $bg -background $fg \
      -activeforeground $fg -activebackground $bg
}
#
proc makeLB {win args} {
    frame $win -relief raised
    frame $win.vs -borderwidth 0
    frame $win.hs -borderwidth 0
    scrollbar $win.vs.vscroller -command "$win.l yview"
    scrollbar $win.hs.hscroller -command "$win.l xview" -orient horizontal
    eval listbox $win.l -xscrollcommand "{hsSet $win}" \
      -yscrollcommand "{bsSet $win.vs.vscroller}" $args
    if ![string compare 7.5 [info tclversion]] {
	$win.l configure -selectmode single
    }
    pack $win.l -side left -expand 1 -fill both -in $win.vs
    frame $win.hs.pd
    pack $win.vs -expand 1 -fill both -side top
    return $win
}
#
proc utest {win x y cmd} {
    set name [lindex [$win tag names @$x,$y] 0]
    if [string match user* $name] { Message :: make [$name lname] }
    notIdle $win
}
#
proc who2 {net name} {$net WHOIS $name $name}
#
proc rebind {txt net} {
    $txt configure -state disabled -takefocus 0
    bind $txt <Any-KeyPress> {notIdle %W ; break}
    bind $txt <Double-Button-2> {utest %W %x %y {Message :: make}}
    bind $txt <Shift-Double-Button-2> "utest %W %x %y {who2 $net}"
    bind $txt <Control-Double-Button-2> "utest %W %x %y {finger $net}"
    bind $txt <Double-3> "findURL %W %x %y $net"
}
#
proc retitle {t w1 w2} { wm title $w2 $t }
