#
# $Source: /home/nlfm/Working/Zircon/Released/lib/interp/RCS/tclx.tcl,v $
# $Date: 1996/06/04 08:39:01 $
# $Revision: 1.17.1.1 $
#
# ----------------------------------------------------------------------
#   AUTHOR:  Lindsay Marshall <lindsay.marshall@newcastle.ac.uk>
# ----------------------------------------------------------------------
# Copyright 1995 The University of Newcastle upon Tyne (see COPYRIGHT)
# ======================================================================
#
set tclx_errorHandler tkerror
proc getDate {} { return [fmtclock [getclock]] }
#
proc convTime {t} { return [fmtclock $t] }
#
proc ipAddress {} {
    global host hostIPaddress ztrans
    if [info exists hostIPaddress] { return $hostIPaddress }
    set hostIPaddress [lindex [server_info addresses $host] 0]
    if {$hostIPaddress == "0.0.0.0" || $hostIPaddress == "127.0.0.1"} {
	mkDialog {} .@ip {IP Number} \
	  {Please enter the IP number for your host} {{{IP Number}{}}} \
	  "$ztrans(OK) {set hostIPaddress}"
	tkwait .@ip
    }
    return $hostIPaddress
}
#
proc thisHost {} {
    global env
    return [expr {[info exists env(HOSTNAME)] ? $env(HOSTNAME) : [exec hostname]}]
}
#
proc ChatServer {usr nk} {
    global AChat
    set sock [server_create]
    set port [lindex [fstat $sock localhost] 2]
    set AChat($usr) $sock
    $usr ref
    fileevent $sock readable "acceptChat $usr $sock"
    [$usr net] CTCP DCC $nk "CHAT chat [ipPack [ipAddress]] $port"
}
#
proc acceptChat {usr conn} {
    global monitorIn
    if ![catch  {server_accept $conn} cls] {
	global AChat
	[set cht [Chat [$usr name] -caller $usr]] show
	$cht addUser $usr 0 0
	set newc [$cht configure -sock [lindex $cls 0]]
	upvar #0 $newc chdata
	set chdata(who) $usr
	set chdata(obj) $cht
	handler $newc re dccChat
	catch {unset AChat($usr)}
	$usr deref
	if $monitorIn { zIn "Chat Accept : [$usr name]" }
    } {
	[$usr net] errmsg "Error on DCC Chat (accept). $cls"
	if $monitorIn { zIn "Error on Accept : $conn" }
    }
    catch {fileevent $conn readable {}}
    catch {close $conn}
}
#
proc connect {host port} { return [server_connect -buf $host $port] }
#
proc aconnect {host port} { return [server_connect -buf $host $port] }
#	
proc handler {sock what prc} { fileevent $sock readable "$prc r $sock" }
#
proc clearHandler {sock} { fileevent $sock readable {} }
#
proc atclose {args} { }
#
proc socketOption {args} { }
#
proc ircsend {sock what} { server_send $sock "$what\r\n" }
#
proc lowsend {sock what} { server_send $sock $what }
#
proc dp_receive {sock} { return [gets $sock] }
#
proc shutdown {args} { }
#
proc ircInput {mode conn} {
    global STN monitorIn inData zircon current
    set net $STN($conn)
    if {[catch {gets $conn} line] || [string match {} $line]} {
	close $conn
	$net close
    } {
	regsub -all "\r" $line {} line
	if $monitorIn { zIn $line }
	if ![regexp {^([^ ]*) ([^ ]*)(( ([^:][^ ]*))*)( :(.*))?$} \
	   $line match prefix cmd b c d e param] {
	    if ![regexp {^([^ ]*) ([^ ]*)(.*)$} $line match \
	      prefix cmd b] {
	        $net errmsg "Error on server connection - $line"
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
proc version {} { return "tclX V[infox version] patch [infox patchlevel]" }
#
proc zping {args} {
    if [string match {} $args] {return [getclock]}
    set res [lindex $args 0]
    if ![regexp {([0-9]+)} $res] { return $res }
    return [expr [getclock] - $res]
}
