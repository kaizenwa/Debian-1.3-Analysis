#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/Notice.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
set defNotice {}
#
class Notice {
    buttons	0
    width	80
    height	10
    away	{}
}
#
proc Notice {name args} {
    if ![string compare {::} $name] {
	set op [lindex $args 0]
	if [string match {} [info procs Notice_$op]] {
	    return [eval Channel_$op [lrange $args 1 end] ]
	}
	return [eval Notice_$op [lrange $args 1 end] ]
    }
    if ![string compare [set id [Notice :: find $name]] nil] {set id [makeNotice $name]}
    if ![string match {} $args] { eval $id configure $args }
    return $id
}
#
proc notice_call {this op args} {
    if [string match {} [info procs notice_$op]] {
	return [eval channel_call $this $op $args]
    }
    return [eval notice_$op $this $args]
}
#
proc makeNotice {chan} {
    global defNotice defChan current
    set this [objName Notice]
    proc $this {args} " eval notice_call $this \$args "
    initObj $this Channel Notice
    set lchan [string tolower $chan]
    set net $current(net)
    upvar #0 $this ndata NTO$net NTO
    if [string compare $lchan *default*] { [User :: make $chan] join $this }
    if [string match {} [set def $defNotice]] {
	set def $defChan
	set b 0
	set d 0
    } {
	set b [$def buttons]
	set d [$def draw]
    }
    array set ndata [uplevel #0 array get $def]
    set ndata(keep) 0
    set ndata(buttons) $b
    set ndata(draw) $d
    set ndata(name) $chan
    set ndata(lname) $lchan
    set ndata(net) $net
    $net register notices $this
    set NTO($lchan) $this
    return $this
}
#
proc notice_nickChange {this usr nnk} {
    if ![string compare [$this lname] [$usr lname]] { $this nChange $nnk }
    channel_nickChange $this $usr $nnk
}
#
proc notice_nChange {this nnk} {
    if [$this active] {wm title [$this window] "IRC Notice from $nnk"}
    upvar #0 NTO[$this net] NTO
    upvar #0 $this cdata
    set ln [string tolower $nnk]
    unset NTO($cdata(lname))
    set NTO($ln) $this
    set cdata(lname) $ln
    set cdata(name) $nnk
}
#
proc notice_replace {this usr1 usr2} {
    $this nChange [$usr2 name]
    channel_replace $this $usr1 $usr2
}
#
proc notice_delete {this} { mcnDelete $this NTO[$this net] notices }
#
proc Notice_make {nk} {
    global current
    upvar #0 NTO$current(net) NTO
    set usr [User :: make $nk]
    set ln [string tolower $nk]
    if [info exists NTO($ln)] { set id $NTO($ln) } { set id [Notice $nk] }
    $id configure -crypt [$usr crypt]
    $id show -nofocus
    $id addUser $usr 0 0
    return $id
}
#
proc Notice_find {nk} {
    global current
    upvar #0 NTO$current(net) NTO
    set ln [string tolower $nk]
    return [expr {[info exists NTO($ln)] ? $NTO($ln) : {nil}}]
}
#
proc Notice_save {desc net} {uplevel #0 defSave $desc \$defNotice Notice}
#
proc notice_save {this desc} {
    global defNotice
    puts $desc [mncSave $this $defNotice]
    foreach  b [$this bindings] {
	puts $desc "zbind [$this name] [lindex $b 0] {[lindex $b 1]}"
    }
}
