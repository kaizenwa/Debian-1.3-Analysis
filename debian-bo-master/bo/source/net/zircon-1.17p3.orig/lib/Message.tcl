#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/Message.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
set defMsg {}
#
proc Message {name args} {
    if ![string compare :: $name] {
	set op [lindex $args 0]
	if [string match {} [info procs Message_$op]] {
	    return [eval Channel_$op [lrange $args 1 end] ]
	}
	return [eval Message_$op [lrange $args 1 end] ]
    }
    if ![string compare nil [set id [Message :: find $name]]] {
	set id [makeMessage $name]
    }
    if ![string match {} $args] { eval $id configure $args }
    return $id
}
#
proc message_call {this op args} {
    if [string match {} [info procs message_$op]] {
	return [eval channel_call $this $op $args]
    }
    return [eval message_$op $this $args]
}
#
class Message {
    buttons	0
    width	80
    height	10
    away	{}
}
#
proc makeMessage {chan} {
    global defMsg defChan current
    set this [objName Message]
    proc $this {args} "eval message_call $this \$args "
    initObj $this Channel Message
    set net $current(net)
    upvar #0 $this mdata MTO$net MTO
    if [string compare *default* [set lchan [string tolower $chan]]] {
	User :: make $chan
    }
    if [string match {} [set def $defMsg]] {
	set def $defChan
	set b 0
    } {
	set b [$def buttons]
    }
    array set mdata [uplevel #0 array get $def]
    set mdata(keep) 0
    set mdata(buttons) $b
    set mdata(name) $chan
    set mdata(lname) $lchan
    set mdata(net) $net
    $net register messages $this 
    set MTO($lchan) $this
    return $this
}
#
proc message_nickChange {this usr nnk} {
    if ![string compare [$this lname] [$usr lname]] { $this nChange $nnk }
    channel_nickChange $this $usr $nnk
}
#
proc message_nChange {this nnk} {
    if [$this active] {wm title [$this window] "IRC Conversation with $nnk"}
    upvar #0 MTO[$this net] MTO
    upvar #0 $this cdata
    set ln [string tolower $nnk]
    unset MTO($cdata(lname))
    set MTO($ln) $this
    set cdata(lname) $ln
    set cdata(name) $nnk
}
#
proc message_replace {this usr1 usr2} {
    $this nChange [$usr2 name]
    channel_replace $this $usr1 $usr2
}
#
proc message_delete {this} { mcnDelete $this MTO[$this net] messages}
#
proc Message_make {nk args} {
    global current
    upvar #0 MTO$current(net) MTO
    set nk [$current(net) trimNick [cleanup $nk]]
    set usr [User :: make $nk]
    set ln [string tolower $nk]
    if [info exists MTO($ln)] { set id $MTO($ln) } { set id [Message $nk] }
    $id configure -crypt [$usr crypt]
    $id show -nofocus
    $id addUser $usr 0 0
    if ![string match {} $args] { eval $id configure $args }
    return $id
}
#
proc Message_find {nk} {
    global current
    upvar #0 MTO$current(net) MTO
    set ln [string tolower $nk]
    return [expr {[info exists MTO($ln)] ? $MTO($ln) : {nil}}]
}
#
proc Message_save {desc net} {
    global defMsg
    defSave $desc $defMsg Message
    foreach ch [$net messages] {
	if {[string compare $ch $defMsg] && [$ch keep]} { $ch save $desc }
    }
}
#
proc message_save {this desc} {
    global defMsg
    puts $desc [mncSave $this $defMsg]
    foreach  b [$this bindings] {
	puts $desc "zbind [$this name] [lindex $b 0] {[lindex $b 1]}"
    }
}
#
proc message_awayMsg {this msg} {
    upvar #0 $this mdata
    if {$mdata(away) != $msg} {
	set mdata(away) $msg
	if [$this active] {
	    $this addText {} "*** [$this name] is away : $msg"
	}
    }
}
