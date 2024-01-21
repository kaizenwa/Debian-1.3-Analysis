#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/User.tcl,v $
# $Date: 1996/06/11 16:35:29 $
# $Revision: 1.17.1.2 $
#
class User {
    name	{}
    lname	{}
    channels	{}
    refcount	0
    crypt	{}
    net		{}
    fobj	nil
    menu	0
    notify	0
    id		{}
}
#
proc User {name args} {
    if ![string compare :: $name] {
	return [eval User_[lindex $args 0] [lrange $args 1 end]]
    }
    set this [objName User]
    initObj $this User
    proc $this {args} "eval user_call $this \$args"
    global current
    set net $current(net)
    upvar #0 UTO$net UTO
    upvar #0 $this udata
    set name [$net trimNick $name]
    set udata(name) $name
    set udata(net) $net
    $net register users $this
    if [string compare nil [set fobj [Friend :: find $name]]] {
	$fobj configure -usr $this
	set udata(fobj) $fobj
    }
    set UTO([set udata(lname) [string tolower $name]]) $this
    if ![string match {} $args] { eval $this configure $args }
    return $this
}
#
proc user_configure {this args} {
    upvar #0 $this udata
    while {![string match {} $args]} {
	set op [lindex $args 0]
	set val [lindex $args 1]
	switch -- $op {
	-friend { set udata(menu) $val }
	default { set udata([string range $op 1 end]) $val }
	}
	set args [lrange $args 2 end]
    }
}
#
proc traceback {} {
    set x [info level]
    puts stderr {}
    while {$x > 0} { incr x -1 ; puts stderr [info level $x] }
}
#
proc user_call {this op args} {
    upvar #0 $this udata
    switch $op {
    ref { incr udata(refcount) ; return }
    deref { if {[incr udata(refcount) -1] <= 0} {$this delete} ; return }
    }
    if [info exists udata($op)] { return $udata($op) }
    return [eval user_$op $this $args]
}
#
proc user_delete {this} {
    global TFn TFa TFg TBg TAF TAB TBl Bl IFlag OType
    upvar #0 $this udata
    [set net $udata(net)] deregister users $this
    if [string compare nil $udata(fobj)] { $udata(fobj) configure -usr nil }
    upvar #0 UTO$net UTO
    unset UTO([$this lname]) OType($this) udata
    foreach z {TFn TFa TFg TBg TAF TAB TBl Bl IFlag} {
	foreach v [array names $z $this,*] { unset ${z}($v) }
    }
    rename $this {}
}
#
proc user_substitute {this orig} {
    $this ref
    foreach x [$this channels] { $x replace $this $orig }
    set ln [$this lname]
    foreach x {Message Notice Chat} {
	if [string compare nil [set old [$x :: find $ln]]] {
	    $old replace $this $orig
	}
    }
    global AChat
    if [info exists AChat($this)] { 
	set AChat($orig) $Achat($this)
	unset AChat($this)
    }
    $this deref
}
#
proc user_rename {this nk} {
    set net [$this net]
    upvar #0 UTO$net UTO
    set nk [$net trimNick $nk]
    upvar #0 $this udata
    if [string compare nil [set fobj [$this fobj]]] {
	[$net finfo] rename $fobj $nk
    }
    set udata(name) $nk
    unset UTO($udata(lname))
    set UTO([set udata(lname) [string tolower $nk]]) $this
}
#
proc user_join {this chan} {
    upvar #0 $this udata
    if {[lsearch $udata(channels) $chan] < 0} {
	lappend udata(channels) $chan
	$this ref
    }
}
#
proc user_leave {this chan} {
    global TAB TAF TBg TFg TBl TFa TFn
    upvar #0 $chan cdata
    upvar #0 $this udata
    set ln [$this lname]
    catch {unset cdata(Op,$this) cdata(Spk,$this)}
    foreach x {TAB TAF TBg TFg TBl TFa TFn} { catch {unset ${x}($chan,$ln)} }
    if {[set x [lsearch $udata(channels) $chan]] >= 0} {
	listdel udata(channels) $x
	$this deref
    }
}
#
proc user_doNotify {this} {
    if [$this notify] {
	set fobj [Friend :: make [$this name]]
	$fobj configure -notify 1
	$this configure -fobj $fobj
	[$this net] ISON
    } {
	if [string compare nil [set fobj [$this fobj]]] {
	    if [$fobj ison] {
		[[$this net] finfo] mark $fobj {}
	    }
	    $fobj configure -notify 0
	}
    }
}
#
proc user_off {this} {$this unChServ}
#
proc user_finger {this} { finger [$this net] [$this name] }
#
proc user_mode {this mode args} {
    [$this net] send MODE [$this name] $mode [lindex $args 0]
}
#
proc user_dcc {this cmd} {
    global ztrans
    set nk [$this name]
    switch $cmd {
    SEND {
	    mkFileBox {} .* "Send $nk" "File to send to $nk" {}\
	      "$ztrans(send) {DCCSend $this}" "$ztrans(cancel) {}"
	}
    CHAT {
	    global AChat
	    set net [$this net]
	    if [info exist AChat($this)] {
		mkDialog {} .@chat$this {Chat} \
		  "You already have a chat request pending for $nk." {} \
		  "$ztrans(close) {$this unChat}" "$ztrans(keep) {}"
	    } \
	    elseif {[string compare nil [Chat :: find $nk]]} {
		mkDialog {} .@chat$this {Chat} \
		  "You already have a chat session open to $nk." {} \
		  "$ztrans(keep) {}" "$ztrans(close) {$this unChat}"
	    } \
	    elseif {[catch {ChatServer $this $nk} msg]}  {
		[$this net] errmsg "[ipAddress] : $msg"
	    } \
	    elseif {[winfo exists .@dls$net ]} { buildDCCList $net }

	}
    }
}
#
proc user_heal {this} {
    global Split Heal TSplit
    set told 0
    set net [$this net]
    foreach sl [array names Split] {
	if {[set x [lsearch $Split($sl) $this]] >= 0} {
	    catch {after cancel $TSplit($sl) ; unset TSplit($sl)}
	    if ![info exists Heal($sl)] {
		set told 1
		[$net info] optText HEAL "*** Heal - $sl"
		handleOn HEAL $sl
	    }
	    set v $Split($sl)
	    listdel v $x
	    $this deref
	    if ![string match {} $v] {
		set Split($sl) $v
		catch {after cancel $Heal($sl)}
		set Heal($sl) [after 120000 $net cleanSplit "{$sl}"]
	    } {
		unset Split($sl)
		catch { after cancel Heal($sl) ; unset Heal($sl) }
	    }
	}
    }
    foreach x {Notice Message} {
	if {[string compare nil [set id [$x :: find [$this lname]]]] 
	   && [$id active]} {
	    $id addText {} {*** Heal}
	    $id flag normal
	}
    }
    foreach x [$net channels] {
	if {[$x monitor] && [winfo exists .@mon$x.users.userList.$this]} {
	    .@mon$x.users.userList.$this configure -state normal
	}
    }
}
#
proc user_unChServ {this} {
    global AChat
    if [info exist AChat($this)] {
	catch {uplevel #0 unset $AChat($this)}
	catch {shutdown $AChat($this) all}
	catch {close $AChat($this)}
	unset AChat($this)
	$this deref
    }
}
#
proc user_unChat {this} {
    $this unChServ
    if [string compare [set id [Chat :: find [$this name]]] nil] {$id delete}
}
#
proc user_kill {this} { kill [$this net] [$this name] }
#
proc user_pack {this net} {
    upvar #0 ${net}UTO newt
    uplevel #0 array set new$this \[array get $this\]
    set ln [$this lname]
    set newt($ln) $this
}
#
proc user_unpack {this net} {
    upvar #0 new$this newu
    foreach v {name notify menu id} {$this configure -$v $newu($v)}
    unset newu
    global ${net}UTO
    unset ${net}UTO([$this lname])
}
#
proc user_split {this split} {
    global Split
    set net [$this net]
    if ![info exists Split($split)] {
	$net newSplit $split
    }
    foreach id [$net channels] {
	if [$id isJoined $this] {
	    set w [$id window]
	    if {[set x [indexHack $w.users.menu [$this name] 3]] >=0} {
		$w.users.menu entryconfigure $x -state disabled
	    }
	    $w.cFrm.uFrm.userBtn.$this conf -state disabled
	} elseif {[$id monitor]} {
	     if [winfo exists .@mon$id.users.userList.$this] {
		.@mon$id.users.userList.$this configure -state disabled
	     }
	}
    }
    foreach x {Notice Message} {
	if {[string compare nil [set id [$x :: find [$this lname]]]] &&
	  [$id active]} {
	    $id addText {} "*** Netsplit - $split"
	    $id flag disabled
	}
    }
    if [string compare nil [set fobj [$this fobj]]] {
	[$net finfo] disable $fobj
	$fobj configure -limbo 1
    }
    lappend Split($split) $this
    $this ref
    handleOn USPLIT [list $split [$this lname]]
}
#
# Procs for the class User
#
#
proc User_find {nk} {
    global current
    upvar #0 UTO$current(net) UTO
    set name [string tolower [$current(net) trimNick $nk]]
    if [info exists UTO($name)] { return $UTO($name) } { return nil }
}
#
proc User_make {nk} {
    global current
    upvar #0 UTO$current(net) UTO
    set name [string tolower [$current(net) trimNick $nk]]
    if [info exists UTO($name)] { return $UTO($name) } { return [User $nk] }
}
