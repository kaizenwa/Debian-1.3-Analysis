#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/Friends.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
#
# Handle all stuff related to Friends menu/window
#
#
class Friend {
    nick	{}
    lnick	{}
    notify	1
    net		{}
    usr		nil
    id		{}
    menu	1
    ison	0
}
#
proc Friend_save {desc net} {
    foreach x [$net friends] {
	regsub -all {[][\\{\"}]} [$x name] {\\&} n
	set ln "Friend $n"
	if ![$x menu] { append ln " -menu 0"}
	if ![$x notify] { append ln " -notify 0" }
	if ![string match {} [$x id]] { append ln " -id {[$x id]}" }
	puts $desc $ln
    }
}
#
proc Friend_find {name} {
    global current
    upvar #0 FTO$current(net) FTO UTO$current(net) UTO
    set ln [string tolower $name]
    if [info exists FTO($ln)] { return $FTO($ln) }
    if [info exist UTO($ln)] { return [$UTO($ln) fobj] }
    return nil
}
#
proc Friend_make {name} {
    global current
    upvar #0 FTO$current(net) FTO
    set ln [string tolower $name]
    if [info exists FTO($ln)] { return $FTO($ln) }
    return [Friend $name]
}
#
proc Friend {name args} {
    if ![string compare :: $name] {
	return [eval Friend_[lindex $args 0] [lrange $args 1 end]]
    }
    set this [objName Friend]
    initObj $this Friend
    global current
    set net $current(net)
    upvar #0 FTO$net FTO $this fdata
    set fdata(nick) [set name [$net trimNick $name]]
    set fdata(net) $net
    $net register friends $this
    set FTO([set fdata(lnick) [string tolower $name]]) $this
    proc $this {args} "eval friend_call $this \$args"
    if ![string match {} $args] { eval $this configure $args }
    return $this
}
#
proc Friend_pack {net} {
    makeArray ${net}FTO
    foreach f [$net friends] { $f pack $net }
}
#
proc Friend_unpack {net} {
    upvar #0 ${net}FTO newFTO
    foreach u [array names newFTO] { $newFTO($u) unpack $net }
    unset newFTO
}
#
proc friend_doNotify {this} {
    upvar #0 $this fdata
    if [string compare nil $fdata(usr)] {
	$fdata(usr) configure -notify $fdata(notify)
    }
    if $fdata(notify) { [$this net] ISON } {
	if [$this ison] { [[$this net] finfo] mark $this {} }
    }
}
#
proc friend_pack {this net} {
    upvar #0 ${net}FTO newt
    uplevel #0 array set new$this \[array get $this\]
    set ln [$this lname]
    set newt($ln) $this
}
#
proc friend_unpack {this net} {
    upvar #0 new$this newu
    foreach v {nick notify menu id} {$this configure -$v $newu($v)}
    unset newu
    global ${net}FTO
    unset ${net}FTO([$this lname])
    $this configure -usr [User :: find [$this nick]]
}
#
proc friend_name {this} {
    upvar #0 $this fdata
    if [string compare nil $fdata(usr)] { return [$fdata(usr) name] }
    return $fdata(nick)
}
#
proc friend_lname {this} {
    upvar #0 $this fdata
    if [string compare nil $fdata(usr)] { return [$fdata(usr) lname] }
    return $fdata(lnick)
}
#
proc friend_delete {this} {
    $this configure -notify 0
    upvar #0 $this fdata
    set net $fdata(net)
    upvar #0 FTO$net FTO
    unset FTO($fdata(lnick)) fdata
    $net deregister friends $this
}
#
proc friend_configure {this args} {
    upvar #0 $this fdata
    while {![string match {} $args]} {
	set op [lindex $args 0]
	set val [lindex $args 1]
	switch -- $op {
	-usr {
		if [string compare nil $val] { $val configure -fobj $this }
	    }
	default { set fdata([string range $op 1 end]) $val }
	}
	set args [lrange $args 2 end]
    }
}
#
proc friend_call {this op args} {
    upvar #0 $this fdata    
    if [info exists fdata($op)] { return $fdata($op) }
    return [eval friend_$op $this $args]
}
#
# This is the code for the Friends window
#
proc friends_call {this op args} {
    switch $op {
    enable { eval friends_state $this normal $args }
    disable { eval friends_state $this disabled $args }
    default { eval friends_$op $this $args }
    }
}
#
proc friends_rename {this frd nnk} {
    if [$this menu] {
	global Fctl
	set ctl [$Fctl($this) window]
	if {[set x [indexHack $ctl.bf1.friends.menu [$frd name] 0]] >= 0} {
	    $ctl.bf1.friends.menu entryconfigure $x -label $nnk
	}
    } {
	if [winfo exists .@$this.users.userList.$frd] {
	    if [string compare $nnk [$frd nick]] {
		append nnk "\n([$frd nick])"
	    }
	    .@$this.users.userList.$frd config -text $nnk
	}
    }
}
#
proc friends_absent {this frd} {
    if [$this menu] {
	global Fctl
	set ctl [$Fctl($this) window]
	if {[set x [indexHack $ctl.bf1.friends.menu [$frd name] 0]] >= 0} {
	    set v [$ctl.bf1.friends.menu entrycget $x -state]
	    return [string compare normal $v]
	}
	return 0	
    } {
	return [expr {[winfo exists .@$this.users.userList.$frd] \
	      && ![normal .@$this.users.userList.$frd]}]
    }
}
#
proc friends_menu {this} {
    global Fctl
    return [string match {menu} [[$Fctl($this) net] friendsStyle]]
}
#
proc friends_mark {this frd what} {
    if [$this menu] {
# Dont do anything at the moment
    } \
    elseif [winfo exists .@$this.users.userList.$frd] {
	markButton .@$this.users.userList.$frd $what
	popup .@$this
    }
}
#
proc friends_delete {this} { catch {destroy .@$this} }
#
proc friends_state {this state args} {
    if [$this menu] {
	global Fctl
	set ctl [$Fctl($this) window]
	foreach id $args {
	    if {[set x [indexHack $ctl.bf1.friends.menu $id 0]] >= 0} {
		$ctl.bf1.friends.menu entryconfigure $x -state $state
	    }
	}
    } {
	foreach id $args {
	    if [winfo exists .@$this.users.userList.$id] {
		.@$this.users.userList.$id conf -state $state
	    }
	}
    }
}
#
proc Friends {name args} {
    global Fctl
    set Fctl($name) [lindex $args 1]
    proc $name {args} "eval friends_call $name \$args"
    return $name
}
#
proc friends_show {this} {
    if [$this menu] {
	makeFriendsMenu $this [$Fctl($this) window].bf1.friends.menu
    } elseif {[winfo exists .@$this]} {popup .@$this} {makeFriends $this}
}
#
proc makeFriendsMenu {this win} {menu $win ; insertFriends $this}
#
proc makeFriends {this} {
    global ztrans
    set w [toplevel .@$this -class Zircon]
    wm title $w "Friends"
    wm resizable $w 0 1
    wm protocol $w WM_DELETE_WINDOW "killWindow $w"

    set win [frame $w.users -relief raised]
    scrollbar $win.vscroller -command "$win.userList yview" 
    text $win.userList -yscrollcommand "bsSet $win.vscroller" \
      -width 14 -height 10 -relief flat -borderwidth 0
    bindtags $win.userList {null}
    button $win.ok -text $ztrans(dismiss) -command "killWindow $w"
    pack $win.ok -side right -fill y
    pack $win.userList -side left -fill y -expand 1
    pack $win -expand 1 -fill y
    insertFriends $this
}
#
proc insertFriends {this} {
    global Fctl
    set net [$Fctl($this) net]
    foreach frd [$net friends] {
	if {[$frd menu] && ![$net me [$frd usr]]} {
	    if [$net friendsOn] {
		$frd configure -notify 1
		if [$frd ison] { $this add $frd }
	    } {
		$this add $frd
	    }
	}
    }
    if [$net friendsOn] { $net ISON }
}
#
proc friends_add {this frd} {
    if [string compare nil $frd] {
	if [$this menu] {
	    global Fctl
	    set w [$Fctl($this) window].bf1.friends.menu
	    if ![winfo exists $w.$frd] {
		$w add cascade -label [$frd name] -menu $w.$frd
		makeUserMenu nil $w.$frd $frd
	    }
	} \
	elseif [winfo exists .@$this] {
	    set win .@$this.users.userList
	    set winb $win.$frd
	    if ![winfo exists $winb] {
		menubutton $winb -text [$frd name] -menu $winb.menu -width 12
		makeUserMenu nil $winb.menu $frd
		$win window create end -window $winb
		if [$frd ison] { markButton $winb ison }
	    }
	}
    }
}
#
proc friends_remove {this frd} {
    global Fctl
    if [$this menu] {
	set ctl [$Fctl($this) window]
	if {[set x [indexHack $ctl.bf1.friends.menu [$frd name] 0]] >= 0} {
	    $ctl.bf1.friends.menu delete $x
	}
    } \
    elseif [winfo exists .@$this.users.userList.$frd] {
	if [[$Fctl($this) net] friendsOn] {
	    if [normal .@$this.users.userList.$frd] {
		destroy .@$this.users.userList.$frd
	    }
	} {
	    $this mark $frd {}
	}
	popup .@$this
    }
}
