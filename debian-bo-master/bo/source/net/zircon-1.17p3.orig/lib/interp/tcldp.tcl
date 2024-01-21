#
# $Source: /home/nlfm/Working/Zircon/Released/lib/interp/RCS/tcldp.tcl,v $
# $Date: 1996/06/04 08:39:01 $
# $Revision: 1.17.1.1 $
#
# ----------------------------------------------------------------------
#   AUTHOR:  Lindsay Marshall <lindsay.marshall@newcastle.ac.uk>
# ----------------------------------------------------------------------
# Copyright 1995 The University of Newcastle upon Tyne (see COPYRIGHT)
# ======================================================================
#
proc getDate {} { return [exec date] }
#
proc convTime {t} { return $t }
#
proc ipAddress {} {
    global host hostIPaddress
    if [info exists hostIPaddress] { return $hostIPaddress }
    return [set hostIPaddress [getIP $host]]
}
#
proc getIP {host} {
    set a1 [dp_address create $host 0]
    set hip [lindex [dp_address info $a1] 0]
    dp_address delete $a1
    if {$hip == "0.0.0.0" || $hip == "127.0.0.1"} {
	global ztrans HIP
	mkDialog {} .@ip {IP Number} \
	  "Please enter the IP number for $host" {{{IP Number}{}}} \
	  "$ztrans(OK) {set HIP}"
	tkwait .@ip
	set hip $HIP
	unset HIP
    }
    return $hip
}
#
proc ChatServer {usr nk} {
    global AChat
    set sk [dp_connect -server 0]
    set sock [lindex $sk 0]
    upvar #0 $sock chdata
    set chdata(who) $usr
    set AChat($usr) $sock
    $usr ref
    handler $sock re acceptChat
    [$usr net] CTCP DCC $nk "CHAT chat [ipPack [ipAddress]] [lindex $sk 1]"
}
#
proc acceptChat {mode conn} {
    global monitorIn current
    switch $mode {
    r   {
	    if ![catch  {dp_accept $conn} cls] {
		global AChat
		upvar #0 $conn chdata
		set usr $chdata(who)
		[set cht [Chat [$usr name] -caller $usr]] show
		$cht addUser $usr 0 0
		set newc [$cht configure -sock [lindex $cls 0]]
		upvar #0 $newc ndata
		set ndata(who) $usr
		set ndata(obj) $cht
		handler $newc re dccChat
		catch {unset chdata AChat($usr)}
		$usr deref
		if $monitorIn { zIn "Chat Accept : [$usr name]" }
	    } {
		[$usr net] errmsg "Error on DCC Chat (accept). $cls"
		if $monitorIn { zIn "Error on Accept : $conn" }
	    }
	    catch {clearHandler $conn}
	    catch {close $conn}
	}
    e   { $current(net) errmsg {*** Error on DCC Chat accept}}
    }
}
#
proc thisHost {} {
    global env
    return [expr {[info exists env(HOSTNAME)] ? $env(HOSTNAME) : [exec hostname]}]
}
#
proc connect {host port} { return [lindex [dp_connect $host $port] 0] }
#
proc aconnect {host port} { return [lindex [dp_connect $host $port] 0] }
#
proc handler {sock what prc} { dp_filehandler $sock re $prc }
#
proc clearHandler {sock} { dp_filehandler $sock }
#
proc atclose {args} { eval dp_atclose $args }
#
proc socketOption {args} { eval dp_socketOption $args }
#
proc shutdown {args} { eval dp_shutdown $args }
#
proc ircsend {sock what} { dp_send $sock "$what\r\n" }
#
proc lowsend {sock what} { dp_send $sock $what }
#
proc ircInput {mode conn} {
    global STN monitorIn inData zircon current
    set net $STN($conn)
    switch $mode {
    r   {
	    if {[catch {dp_receive $conn} buffer] || $buffer == {}} {
		$net close
	    } {
#		regsub -all "\r" $buffer {} buffer
		append inData $buffer
		while {[regexp "^(\[^\r\n\]*)\r?\n(.*)" $inData match line inData]} {
		    if $monitorIn { zIn $line }
		    if ![regexp {^([^ ]*) ([^ ]*)(( ([^:][^ ]*))*)( :(.*))?$} \
		       $line match prefix cmd b c d e param] {
			if {![regexp {^([^ ]*) ([^ ]*)(.*)$} $line match \
			   prefix cmd b]} {
			    continue
			}
			set param {}
		    }
		    switch -glob $prefix {
		    :* { }
		    PING {
			    $net qSend PONG :[string range $cmd 1 end]
			    continue
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
	}
    e   { $net errmsg {*** Error on server connection} }
    }
}
#
proc version {} {
    global dp_version
    return "tcl-dp  V$dp_version"
}
#
proc zping {args} {
    global zircon
    return [eval exec $zircon(lib)/zping $args]
}
