
proc dp_MakeRPCClient {host port} {
    global socks
    set sock [socket $host $port]
    set socks($sock::state) idle
    set socks($sock::closehooks) ""
    set socks($sock::checkhook) ""
    fconfigure $sock -blocking no -buffering none
    fileevent $sock readable "_myrpc_readable $sock"
    return $sock
}

proc dp_MakeRPCServer {port {checkcmd ""}} {
    global socks
    set sock [socket -server "_myrpc_accept $port" $port]
    set socks(listen$port::checkhook) $checkcmd
    set port [lindex [fconfigure $sock -sockname] 2]
    return $port
}

proc dp_RDO {sock args} {
#    debugmsg "SEND: [string range $args 0 65]"
    puts -nonewline $sock [format "RDO%05d" [string length $args]]
    puts -nonewline $sock $args
}

proc dp_atclose {sock op cmd} {
    global socks
    if {$op=="append"} {
	lappend socks($sock::closehooks) $cmd
    }
}

proc dp_SetCheckCmd {sock args} {
    global socks
    set socks($sock::checkhook) $args
}


proc _myrpc_accept {listener sock addr port} {
    global socks
    set socks($sock::state) idle
    set socks($sock::closehooks) ""
    set socks($sock::checkhook) $socks(listen$listener::checkhook)
    fconfigure $sock -blocking no -buffering none
    fileevent $sock readable "_myrpc_readable $sock"
}

proc rpcFile {} {
    global myrpc_channel
    if [info exists myrpc_channel] {
	return $myrpc_channel
    } else {
	return ""
    }
}

proc _myrpc_readable {sock} {
    global socks
    switch $socks($sock::state) {
	"idle" {
	    set socks($sock::state) readhdr
	    set socks($sock::buffer) ""
	    set socks($sock::toread) 8
	    _myrpc_readhdr $sock
	}
	"readhdr" {
	    _myrpc_readhdr $sock
	}
	"readmsg" {
	    _myrpc_readmsg $sock
	}
    }
}

proc _myrpc_readhdr {sock} {
    global socks
    set result [read $sock $socks($sock::toread)]
    if {$result==""} {   
	_myrpc_closed $sock
	return
    }
    append socks($sock::buffer) $result
    incr socks($sock::toread) [expr -[string length $result]]
    if {$socks($sock::toread)==0} {
	if {[string range $socks($sock::buffer) 0 2]=="RDO"} {
	    set socks($sock::state) readmsg
	    scan [string range $socks($sock::buffer) 3 end] "%d" len 
	    set socks($sock::toread) $len
	    set socks($sock::buffer) ""
	} else {
	    set socks($sock::state) idle
	}
    }
}

proc _myrpc_readmsg {sock} {
    global socks myrpc_channel
    set result [read $sock $socks($sock::toread)]
    if {$result==""} {   
	_myrpc_closed $sock
	return
    }
    append socks($sock::buffer) $result
    incr socks($sock::toread) [expr -[string length $result]]
    if {$socks($sock::toread)==0} {
	set myrpc_channel $sock
	set socks($sock::state) idle
	set cmd $socks($sock::buffer)
#	debugmsg "RECV: [string range $cmd 0 70]"
	if {($socks($sock::checkhook)=="") \
		|| ([catch {$socks($sock::checkhook) $sock $cmd}]==0)} {
#	    catch {uplevel #0 $cmd}
            catch {_myrpc_eval [lindex $cmd 0] [lrange $cmd 1 end]}
	} else {
	    puts "VIOLATION: [string range $cmd 0 70]"
	}
	catch {unset myrpc_channel}
    }
}

proc _myrpc_eval {cmd args} {
    set args [lindex $args 0]
    return [eval [list $cmd] $args]
}

proc _myrpc_closed {sock} {
#    debugmsg "closed on $sock, eof=[eof $sock]"
    global socks
    if [eof $sock] {
        fileevent $sock readable ""
	foreach i $socks($sock::closehooks) {
	    catch {uplevel #0 $i}
	}
    }
}
