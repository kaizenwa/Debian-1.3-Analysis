#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/Chat.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
set defChat {}
#
class DChat {
    buttons	0
    draw	0
    height	10
    width	80
    caller	{}
    sock	{}
}
#
proc Chat {name args} {
    if ![string compare $name ::] {
	set op [lindex $args 0]
	if [string match {} [info procs Chat_$op]] {
	    return [eval Channel_$op [lrange $args 1 end] ]
	}
	return [eval Chat_$op [lrange $args 1 end] ]
    }
    if ![string compare [set id [Chat :: find $name]] nil] { set id [makeChat $name] }
    if ![string match {} $args] { eval $id configure $args }
    return $id
}
#
proc chat_nickChange {this usr nnk} {
    if [string match {} [set w [$this window]]] return
    set net [$this net]
    $this optText NICK "*** [$usr name] is now known as $nnk"
    if [$net me $usr] {
	$w.cFrm.uFrm.userBtn.$usr configure -text $nnk
	$w.users.menu entryconfigure 3 -label $nnk
# Check for self abuse!!
	if ![string compare $usr [$this caller]] { $this nChange $nnk }
    } {
	$this nChange $nnk
    }
}
#
proc chat_nChange {this nnk} {
    catch {wm title [$this window] "DCC Chat with $nnk"}
    upvar #0 CHTO[$this net] CHTO
    upvar #0 $this cdata
    set ln [string tolower $nnk]
    unset CHTO($cdata(lname))
    set CHTO($ln) $this
    set cdata(lname) $ln
    set cdata(name) $nnk
}
#
proc chat_replace {this usr1 usr2} {$this nChange [$usr2 name]}
##
proc chat_action {this string} { $this send $string }
#
proc chat_send {this string args} {
    notIdle {}
    if ![string match {} $string] {
	global monitorOut
	upvar #0 $this cdata
	if ![string match {} $cdata(sock)] {
	    if [catch {puts $cdata(sock) $string} err] {
		$this addText {} "*** Error : $err"
	    } {
		flush $cdata(sock)
		$this addText @me "= $string"
	        if $monitorOut { zOut "= $string" }
	    }
	} {
	    $this addText {} {*** Connection is closed!!!!}
	}
    }
}
#
proc chat_call {this op args} {
    if [string match {} [info procs chat_$op]] {
	return [eval channel_call $this $op $args]
    }
    return [eval chat_$op $this $args]
}
#
proc makeChat {chan} {
    global defChat defChan current
    set this [objName Chat]
    proc $this {args} "eval chat_call $this \$args"
    initObj $this Channel DChat
    upvar #0 $this cdata CHTO$current(net) CHTO
    set lchan [string tolower $chan]
    if [string match {} [set def $defChat]] {
	set b 0
	set def $defChan
    } {
	set b [$def buttons]
    }
    array set cdata [uplevel #0 array get $def]
    set cdata(buttons) $b
    set cdata(name) $chan
    set cdata(lname) $lchan
    set cdata(net) $current(net)
    $current(net) register chats $this
    set CHTO($lchan) $this
    return $this
}
#
proc chat_configure {this args} {
    upvar #0 $this cdata
    while {![string match {} $args]} {
	set val [lindex $args 1]
	set name [lindex $args 0]
	switch -glob -- $name {
	-caller { set cdata(caller) $val ; $val ref}
	default { channel_configure $this $name $val }
	}
	set args [lrange $args 2 end]
    }
}
#
proc chat_isJoined {this usr} {
    return [expr {![string compare $usr [User :: find [$this name]]]}]
}
#
proc chat_delete {this} {
    global Name current
    upvar #0 $this cdata
    upvar #0 CHTO$current(net) CHTO
    if ![string match {} $cdata(logfd)] { close $cdata(logfd) }
    if ![string match {} [set win [$this window]]] {
	foreach x [winfo children $win.cFrm.uFrm.userBtn] {
	    [winfo name $x] leave $this
	}
    }
    catch {unset Name($win)}
    $this configure -window {}
    set chan $cdata(lname)
    $cdata(caller) deref
    rename $this {}
    if ![string match {} $cdata(sock)] { closeChat $this $chan $cdata(sock) }
    unset CHTO($chan) cdata
    $current(net) deregister chats $this
}
#
proc chat_leave {this} {
    global ztrans
    set chan [$this name]
    set msg "Really leave DCC chat with $chan?"
    mkDialog LEAVE .@$this "Leaving $chan" $msg {} \
      "$ztrans(ok) {$this doLeave {}}" "$ztrans(cancel) {}"
}
#
proc Chat_make {name} {
    global current
    upvar #0 CHTO$current(net) CHTO
    set ln [string tolower $name]
    if [info exists CHTO($ln)] { return $CHTO($ln) } { return [Chat $name]}
}
#
proc Chat_find {name} {
    global current
    upvar #0 CHTO$current(net) CHTO
    set ln [string tolower $name]
    return [expr {[info exists CHTO($ln)] ? $CHTO($ln) : {nil}}]
}
#
proc Chat_save {desc net} {uplevel #0 defSave $desc \$defChat DChat}
#
proc chat_save {this desc} {
    global defChat
    puts $desc [mncSave $this $defChat]
    foreach  b [$this bindings] {
	puts $desc "zbind [$this name] [lindex $b 0] {[lindex $b 1]}"
    }
}
