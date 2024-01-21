#
# $Source: /home/nlfm/Working/Zircon/Released/lib/interp/RCS/tcl7.5.tcl,v $
# $Date: 1996/06/10 09:41:12 $
# $Revision: 1.17.1.2 $
#
# ----------------------------------------------------------------------
#   AUTHOR:  Lindsay Marshall <lindsay.marshall@newcastle.ac.uk>
# ----------------------------------------------------------------------
# Copyright 1995 The University of Newcastle upon Tyne (see COPYRIGHT)
# ======================================================================
#
proc getDate {} { return [clock format [clock seconds]] }
#
proc convTime {t} { return [clock format $t] }
#
proc IPaccept {p1 p2 args} {
    global hostIPaddress
    set hostIPaddress $p2
    catch {close $p1}
}
#
proc ipAddress {} {
    global hostIPaddress
    if ![info exists hostIPaddress] {
	set hostIPaddress {}
	set port 5024
	while {[catch {socket -server IPaccept $port} sock]} {
	    incr port
	}
	set sock2 [socket [info hostname] $port]
	vwait hostIPaddress
	catch {close $sock}
	catch {close $sock2}
	if {$hostIPaddress == "0.0.0.0" || $hostIPaddress == "127.0.0.1"} {
	    mkDialog {} .@ip {IP Number} \
	      {Please enter the IP number for your host} {{{IP Number}{}}} \
	      "$ztrans(OK) {set hostIPaddress}"
	    tkwait .@ip
	}
    }
    return $hostIPaddress
}
#
proc ChatServer {usr nk} {
    global AChat
    set port 1024
    while {[catch {socket -server "acceptChat $usr" $port} sock]} {
	incr port
    }
    set AChat($usr) $sock
    $usr ref
    [$usr net] CTCP DCC $nk "CHAT chat [ipPack [ipAddress]] $port"
}
#
proc acceptChat {usr newc hst args} {
    global AChat
    [set cht [Chat [$usr name] -caller $usr]] show
    $cht addUser $usr 0 0
    upvar #0 $newc chdata
    $cht configure -sock $newc
    set chdata(who) $usr
    set chdata(obj) $cht
    fconfigure $newc -buffering none -blocking 0 -translation lf
    handler $newc re dccChat
    catch {close $AChat($usr)}
    catch {unset AChat($usr)}
    $usr deref
}
#
proc dbg_acceptChat {usr newc hst args} {
    global monitorIn
    nrm_acceptChat $usr $newc $hst $args
    if $monitorIn { zIn "Chat Accept : [$usr name] on $hst" }
}
#
proc thisHost {} { return [info hostname] }
#
proc connect {host port} {
    set sk [socket $host $port]
    sconf $sk
    return $sk
}
#
proc sconf {sk} {
    global hostIPaddress
    fconfigure $sk -buffering line -translation lf -blocking 0
    if ![info exists hostIPaddress] {
	if ![catch {fconfigure $sk -sockname} xx] {
	    set hostIPaddress [lindex $xx 0]
	}
    }
}
#
proc aconnect {host port} {
    set sk [socket -async $host $port]
    return $sk
}
#
proc chatBuffer {sok} { fconfigure $sok -translation lf }
#	
proc handler {sock what prc} { fileevent $sock readable "$prc r $sock" }
#
proc clearHandler {sock} { fileevent $sock readable {} }
#
proc atclose {args} { }
#
proc socketOption {args} { }
#
proc ircsend {sock what} { puts $sock $what }
#
proc lowsend {sock what} { puts -nonewline $sock $what }
#
proc shutdown {args} { }
#
set inexp1 "^(\[^ \]*) (\[^ \]*)(( (\[^:\]\[^ \]*))*)( :(\[^\r\]*))?\r?$"
set inexp2 "^(\[^ \]*) (\[^ \]*)(\[^\r\]*)?\r?$"
#
proc ircInput {mode conn} {
    global STN zircon inexp1 inexp2 current
    set net $STN($conn)
    if [catch {gets $conn} line] {
	if [string match {socket is not connected} $line] {
	    set line {connection request timed out}
 	}
	$net close $line
	return
    }
    
    if [eof $conn] { $net close } {
	if ![regexp $inexp1 $line match prefix cmd b c d e param] {
	    if ![regexp $inexp2 $line match prefix cmd b] {
		if ![string match {} $line] {
		    $net errmsg "Error on server connection - $line"
		}
		return
	    }
	    set param {}
	}
	switch -glob $prefix {
	:* { }
	PING {
		$net qSend PONG :[string range $cmd 1 end]
		return
	    }
	ERROR {
		regsub "\r" $line {} line
		$net error {} [string range $line 7 end] {}
		return
	    }
	default { set prefix :[$net host] }
	}
	if [catch {irc$cmd [set current(net) $net] $prefix $param \
	  [string range $b 1 end]} msg] {
	    zError $msg $cmd $prefix $param [string range $b 1 end]
	}
    }
}
#
proc dbg_ircInput {mode conn} {
    global STN monitorIn inData zircon fgo inexp1 inexp2 current
    set net $STN($conn)
    if [catch {gets $conn} line] {
	if $monitorIn { zIn "**** Error on input ($line) eof = [eof $conn]" }
	if [string match {socket is not connected} $line] {
	    set line {connection request timed out}
 	}
	$net close $line
	return
    }
    if [eof $conn] {
	if $monitorIn { zIn "**** EOF from server." }
	$net close
    } {
	if $monitorIn { zIn $line }
	if ![regexp $inexp1 $line match prefix cmd b c d e param] {
	    if ![regexp $inexp2 $line match prefix cmd b] {
		if ![string match {} $line] {
		    $net errmsg "Error on server connection - $line"
		}
		return
	    }
	    set param {}
	}
	switch -glob $prefix {
	:* { }
	PING {
		$net qSend PONG :[string range $cmd 1 end]
		return
	    }
	ERROR {
		regsub "\r" $line {} line
		$net error {} [string range $line 7 end] {}
		return
	    }
	default { set prefix :[$net host] }
	}
	if [catch {irc$cmd [set current(net) $net] $prefix $param \
	  [string range $b 1 end]} msg] {
	    zError $msg $cmd $prefix $param [string range $b 1 end]
	}
    }
}
#
proc version {} { return {Native} }
#
proc acceptCon {cmd p1 p2} { filevent $p1 readable $cmd }
#
proc server {cmd} {
    if [catch {socket -server "acceptCon {$cmd}" 0} fd] {
	error $fd
    }
    if ![catch {fconfigure $fd -sockname} xx] {
	return [list $fd [lindex $xx 2]]
    }
    set port 1000
    while {[catch {socket -server "acceptCon {$cmd}" $port} fd]} {
	if ![string match "*: address already in use" $fd] { error $fd }
	incr port
    }
    return [list $fd $port]
}
#
proc zping {args} {
    if [string match {} $args] { return [clock seconds] }
    set res [lindex $args 0]
    if ![regexp {([0-9]+)} $res] { return $res }
    return [expr [clock seconds] - $res]
}
