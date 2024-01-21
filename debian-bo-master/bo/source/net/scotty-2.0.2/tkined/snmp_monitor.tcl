#! /usr/local/bin/scotty -inf
##
## Simple SNMP monitoring utilities for tkined.
##
## Copyright (c) 1993, 1994, 1995
##
## J. Schoenwaelder
## TU Braunschweig, Germany
## Institute for Operating Systems and Computer Networks
##
## Permission to use, copy, modify, and distribute this
## software and its documentation for any purpose and without
## fee is hereby granted, provided that this copyright
## notice appears in all copies.  The University of Braunschweig
## makes no representations about the suitability of this
## software for any purpose.  It is provided "as is" without
## express or implied warranty.
##

LoadDefaults snmp monitor

SnmpInit SNMP-Monitor

if {[info exists default(interval)]} {
    set interval $default(interval)
} else {
    set interval 60
}
if {![info exists default(graph)]} {
    set default(graph) false
}

##
## Create a chart. Honours the monitor.graph default definition.
##

proc CreateChart {id x y} {
    global default
    if {$default(graph) != "true"} {
	set id [CloneNode $id [ined create STRIPCHART] $x $y]
    } else {
	set id [CloneNode $id [ined create GRAPH]]
    }
    return $id
}

##
## Tell tkined to restart a command when a saved map gets loaded.
##

proc save { cmd } {
    set cmdList [ined restart]
    if {[lsearch $cmdList $cmd] >= 0} return
    lappend cmdList $cmd
    ined restart $cmdList
}

##
## Remove a command from the list of commands saved in the tkined map.
##

proc forget { cmd } {
    set cmdList ""
    foreach c [ined restart] {
	if {$c != $cmd} {
	    lappend cmdList $c
	}
    }
    ined restart $cmdList
}

##
## Restart a monitor job by calling cmd. Build the list of objects
## from tkined using ined retrieve commands.
##

proc restart { cmd args } {

    global interval

    if {! [catch {expr [lindex $args 0] + 0}]} {
	set itv  [lindex $args 0]
	set jid  [lindex $args 1]
	set args [lrange $args 2 end]
	set old_interval $interval
	set interval [expr int($itv)]
    }

    foreach id $args {
	catch {ined -noupdate clear $id}
	catch {ined -noupdate clear $id}
    }
    catch {eval $cmd $args}

    if {[info exists old_interval]} {
	set interval $old_interval
    }
}

##
## Monitor a SNMP variable in a stripchart. This version uses two
## procedures. The ShowVariable proc is called by the job scheduler
## to send SNMP requests at regular intervals. SNMP responses are
## processed by ShowVariableProc.
##

proc ShowVariableProc {id status vbl} {
    global cx
    if {$status != "noError"} {
	ined -noupdate attribute $id $cx(descr,$id) \
		"$cx(descr,$id) [lindex $vbl 0]"
	return
    }
    set now [mib scan sysUpTime [lindex [lindex $vbl 0] 2]]
    set syntax [lindex [lindex $vbl 1] 1]
    set value  [lindex [lindex $vbl 1] 2]
    set val 0
    switch -glob $syntax {
	Gauge -
	INTEGER {
	    set val $value
	}
	Counter* {
	    if {$now > $cx(time,$id)} {
		set val [expr $value - $cx(value,$id)]
		set val [expr $val / ( ($now-$cx(time,$id)) / 100.0 )]
	    } else {
		set val 0
	    }
	}
    }
    ined -noupdate values $id $val
    ined -noupdate attribute $id $cx(descr,$id) "$cx(descr,$id) $val"
    set cx(value,$id) $value
    set cx(time,$id) $now
    set txt "[ined -noupdate name $id] $cx(descr,$id)"
    MoJoCheckThreshold $id $txt $val
}

proc ShowVariable { ids } {
    global cx
    foreach id [MoJoCheckIds ShowVariable $ids] {
	$cx(snmp,$id) get "sysUpTime.0 $cx(name,$id)" \
		"ShowVariableProc $id %E \"%V\""
    }
}

##
## Start a new monitoring job for the device given by ip. The args
## list contains node ids with the snmp variable oid to display.
##

proc start_snmp_monitor { ip args } {
    global cx interval

    set ids ""
    set re_args ""

    while {$args != ""} {
	set id [lindex $args 0]
	set var [lindex $args 1]

	if {$id != ""} {
	    set s [SnmpOpen $id $ip]
	    if {[catch {$s get "sysUpTime.0 $var"} vbl]} {
		writeln "SNMP get failed while starting monitoring job:\n$vbl"
		$s destroy
		set args [lrange $args 2 end]
		ined delete $id
		continue
	    }
	    set cx(time,$id)  [mib scan sysUpTime [lindex [lindex $vbl 0] 2]]
	    set cx(value,$id) [lindex [lindex $vbl 1] 2]
	    set cx(snmp,$id) $s
	    set cx(name,$id) $var
	    set cx(descr,$id) [mib name [mib oid $var]]
	    lappend ids $id

	    ined -noupdate attribute $id $cx(descr,$id) $cx(descr,$id)
	    ined -noupdate label $id $cx(descr,$id)
	}
	
	set args [lrange $args 2 end]
	append re_args " \$$id $var"
    }

    if {$ids != ""} {
	set jid [job create [list ShowVariable $ids] [expr $interval * 1000]]
	save "restart start_snmp_monitor $interval $jid $ip $re_args"
    }
}

##
## Prepare a monitoring job for the objects in list and the SNMP variable
## given by varname. This proc is also called by other scripts so changes
## must be done very carefully...
##

proc MonitorVariable {list varname} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	set syntaxList "Gauge Counter Counter32 Counter64 INTEGER"
	set vars ""
	
	set err [catch {$s get $varname} vbl]
	if {$err || [lindex [lindex $vbl 0] 1] == "noSuchInstance"} {
	    if {[catch {
		$s walk vbl $varname {
		    set syntax [lindex [lindex $vbl 0] 1]
		    if {[lsearch $syntaxList $syntax] >= 0} {
			lappend vars [lindex [lindex $vbl 0] 0]
		    }
		}
	    } msg]} {
		ined acknowledge "Monitor Variable $varname on $ip: $msg"
		$s destroy
		continue
	    }
	} else {
	    set syntax [lindex [lindex $vbl 0] 1]
	    if {[lsearch $syntaxList $syntax] >= 0} {
		lappend vars [lindex [lindex $vbl 0] 0]
	    }
	}
	
	if {$vars == ""} {
	    ined acknowledge \
		    "$varname is either not available or of wrong type."
	    continue
	}
	
	set args $ip
	set i 0
	foreach var $vars {
	    set nid [CreateChart $id [expr 30+$i] [expr 30+$i]]
	    ined -noupdate attribute $nid "SNMP:Config" [$s configure]
	    lappend args $nid
	    lappend args $var
	    incr i
	}
	$s destroy
	
	eval start_snmp_monitor $args
    }
}

##
## This is the command interface to the generic SNMP variable
## monitor.
##

proc "Monitor Variable" { list } {

    static varname

    if {![info exists varname]} {
	set varname ip.ipForwDatagrams
    }

    set res [ined request "Tell me which variable I should show you:" \
	    [list [list "SNMP Variable:" $varname] ] \
	    [list start cancel] ]
    if {[lindex $res 0] == "cancel"} return
    
    set varname ""
    foreach var [lindex $res 1] {
	if {[catch {mib oid $var}]} {
	    ined acknowledge "Unknown SNMP variable \"$var\"."
	    continue
	}
	lappend varname $var
    }

    foreach var $varname {
	MonitorVariable $list $var
    }
}

##
## Monitor the interface load as reported by the interface groups 
## of MIB II.
##

##
## Calculate the interface utilisation. This is done using the formula
##
## util = ( 8 * ( delta (ifInOctets, t1, t0) 
##              + delta (ifOutOctets, t1, t0) ) / (t1 - t0) ) / ifSpeed
##
## This formula returns incorrect results for full-duplex point to point
## links. In this case, the following formula should be used:
##
## util = ( 8 * max ( delta (ifInOctets, t1, t0) ,
##                    delta (ifOutOctets, t1, t0) ) / (t1 - t0) ) / ifSpeed
##
## See Simple Times, 1(5), November/December, 1992 for more details.
##

proc ShowIfLoadProc {id status vbl} {
    global cx

    if {$status != "noError"} {
	ined -noupdate attribute $id "interface load" [lindex $vbl 0]
	return
    }

    ## Saved ifOperStatus currently unused but should be used to 
    ## call the MoJoAction on changing (flashing, writing, etc.).

    set now   [mib scan sysUpTime [lindex [lindex $vbl 0] 2]]
    set ifIn  [lindex [lindex $vbl 1] 2]
    set ifOut [lindex [lindex $vbl 2] 2]
    set ifOS  [lindex [lindex $vbl 3] 2]
    
    # Be careful with Tcl's broken arithmetic.
    
    if {[catch {expr $ifIn - $cx(ifInOctets,$id)} deltaIn]} {
	set deltaIn  [expr double($ifIn) - $cx(ifInOctets,$id)]
    }
    if {[catch {expr $ifOut - $cx(ifOutOctets,$id)} deltaOut]} {
	set deltaOut [expr double($ifOut) - $cx(ifOutOctets,$id)]
    }
    
    if {$cx(duplex,$id)} {
	set delta [expr $deltaIn > $deltaOut ? $deltaIn : $deltaOut]
    } else {
	set delta [expr $deltaIn + $deltaOut]
    }
    
    set speed [ined -noupdate attribute $id SNMP-Monitor:IfSpeed]
    if {$speed == ""} {
	set speed $cx(ifSpeed,$id)
    }
    
    if {$now > $cx(sysUpTime,$id)} {
	set secs [expr ($now - $cx(sysUpTime,$id)) / 100.0]
	set val [expr ( 8.0 * $delta / $secs ) / $speed * 100]
    } else {
	set val 0
    }
    ined values $id $val
    if {$ifOS != "up" && $ifOS != "1"} {
	ined -noupdate attribute $id "interface load" \
		"$cx(ifDescr,$id) $ifOS"
	ined -noupdate color $id red
    } else {
	ined -noupdate color $id blue
	ined -noupdate attribute $id "interface load" \
		[format "$cx(ifDescr,$id) %.2f %%" $val]
    }
    
    set txt "[ined name $id] $cx(ifDescr,$id) interface load"
    MoJoCheckThreshold $id $txt $val "%%"
    
    set cx(sysUpTime,$id)    $now
    set cx(ifInOctets,$id)   $ifIn
    set cx(ifOutOctets,$id)  $ifOut
    set cx(ifOperStatus,$id) $ifOS
}

proc ShowIfLoad { ids } {
    global cx
    foreach id [MoJoCheckIds ShowIfLoad $ids] {
	set i $cx(idx,$id)
	set vbl "sysUpTime.0 ifInOctets.$i ifOutOctets.$i ifOperStatus.$i"
	$cx(snmp,$id) get $vbl "ShowIfLoadProc $id %E \"%V\""
    }
}

##
## Start a new interface load monitoring job for the device given by ip. 
## The args list contains node ids with the interface index to display.
##

proc start_ifload_monitor { ip args } {

    global cx interval

    # Note, IANAifType (RFC 1573) has somewhat different encodings
    # than RFC 1213. We use RFC 1213 style.

    set full_duplex {
	regular1822 hdh1822 ddn-x25 rfc877-x25 lapb sdlc ds1 e1 
	basicISDN primaryISDN propPointToPointSerial ppp slip ds3 sip 
	frame-relay
    }

    set ids ""
    set re_args ""

    while {$args != ""} {
	set id [lindex $args 0]
	set idx [lindex $args 1]
	
	if {$id != ""} {
	    
	    set sh [SnmpOpen $id $ip]

	    if {[catch {$sh get [list sysUpTime.0 \
		    ifInOctets.$idx ifOutOctets.$idx \
		    ifSpeed.$idx ifDescr.$idx ifType.$idx \
		    ifOperStatus.$idx]} value]} {
		ined -noupdate attribute $id "interface load" [lindex $value 0]
		ined -noupdate label $id "interface load"
		set args [lrange $args 2 end]
		continue
	    }
	    
	    set cx(sysUpTime,$id)    [lindex [lindex $value 0] 2]
	    set cx(sysUpTime,$id)    [mib scan sysUpTime $cx(sysUpTime,$id)]
	    set cx(ifInOctets,$id)   [lindex [lindex $value 1] 2]
	    set cx(ifOutOctets,$id)  [lindex [lindex $value 2] 2]
	    set cx(ifSpeed,$id)      [lindex [lindex $value 3] 2]
	    set cx(ifDescr,$id)      [lindex [lindex $value 4] 2]
	    set cx(ifType,$id)       [lindex [lindex $value 5] 2]
	    set cx(ifOperStatus,$id) [lindex [lindex $value 6] 2]
	    set cx(snmp,$id) $sh
	    set cx(idx,$id) $idx
	    if {[lsearch $full_duplex $cx(ifType,$id)] < 0} {
		set cx(duplex,$id) 0
	    } else {
		set cx(duplex,$id) 1
	    }
	    lappend ids $id

	    ined -noupdate attribute $id "interface load" \
		"ifLoad $cx(ifDescr,$id)"
	    ined -noupdate label $id "interface load"
	}
	
	set args [lrange $args 2 end]
	append re_args " \$$id $idx"
    }

    if {$ids != ""} {
	set jid [job create [list ShowIfLoad $ids] [expr $interval * 1000]]
	save "restart start_ifload_monitor $interval $jid $ip $re_args"
    }
}

proc "Interface Load" { list } {
    ForeachIpNode id ip host $list {
	set sh [SnmpOpen $id $ip]
	
	# test which interfaces might be of interest	
	set iflist ""
	if {[catch {
	    $sh walk x "ifIndex ifOperStatus ifSpeed ifInOctets ifOutOctets" {
		set idx    [lindex [lindex $x 0] 2]
		set status [lindex [lindex $x 1] 2]
		set speed  [lindex [lindex $x 2] 2]
		
#		if {$status != "up" && $status != "1"} {
#		    writeln "Interface $idx on $host \[$ip\] ignored (ifOperStatus = $status)"
#		    continue
#		}
		
		if {$speed == 0} {
		    writeln "Interface $idx on $host \[$ip\] ignored (ifSpeed = $speed)"
		    continue
		}
		
		lappend iflist $idx
	    }
	}]} {
	    $sh destroy
	    continue
	}

	set args $ip
	set i 0
	foreach if $iflist {
	    lappend args [CreateChart $id [expr 30+$i] [expr 30+$i]]
	    lappend args $if
	    incr i
	}
	
	eval start_ifload_monitor $args
	
	$sh destroy
    }
}

##
## Count the number of connections to a TCP service and display the
## result in a stripchart.
##

proc tcp_service_user { ids } {
    global cx
    
    set ids [MoJoCheckIds tcp_service_user $ids]
    if {$ids == ""} return

    foreach id $ids {
	set count 0
	foreach ipaddr $cx(addr,$id) {
	    if {[catch {
		$cx(snmp,$id) walk x tcpConnState.$ipaddr.$cx(port,$id) {
		    set tcpConnState [lindex [lindex $x 0] 2]
		    if {$tcpConnState == "established"} { incr count }
		}
	    }]} continue
	}
	ined -noupdate attribute $id "tcp established" "$count $cx(name,$id)"
	ined values $id $count
    }
}

proc start_tcp_service_user { ip id port } {
    global cx interval

    set cx(snmp,$id) [SnmpOpen $id $ip]
    set cx(port,$id) $port
    set cx(addr,$id) ""
    if [catch {netdb services name "$cx(port,$id) tcp"} cx(name,$id)] {
	set cx(name,$id) "port $cx(port,$id)"
    }

    if [catch {
	$cx(snmp,$id) walk x ipAdEntAddr { 
	    lappend cx(addr,$id) [lindex [lindex $x 0] 2] 
	}
    } msg] {
	writeln "Can not read IP address table of host $ip: $msg"
	catch {$cx(snmp,$id) destroy}
	return
    }
    
    ined -noupdate attribute $id "tcp established" $cx(name,$id)
    ined -noupdate label $id "tcp established"

    set jid [job create [list tcp_service_user $id] [expr $interval * 1000]]
    save "restart start_tcp_service_user $interval $jid $ip \$$id $port"
}

proc "TCP Service User" { list } {

    set service [IpService tcp]
    if {$service == ""} return

    set name [lindex $service 0]
    set port [lindex $service 1]

    set args ""
    ForeachIpNode id ip host $list {
	set nid [CreateChart $id -20 20]
	catch {ined -noupdate scale $nid 1}
	start_tcp_service_user $ip $nid $port
    }
}

##
## Receive SNMP traps and do something useful. The later must is left
## as an exercise for the reader. :-)
##

proc TrapSinkProc {s src vbl} {
    writeln "[getdate] [$s cget -version] trap from \[$src\]:"
    foreach vb $vbl {
	writeln "  [mib name [lindex $vb 0]] = [lindex $vb 2]"
    }
    writeln
    ForeachIpNode id ip host [ined -noupdate retrieve] {
	if {$ip == $src} {
	    set trap [lindex [lindex $vbl 1] 2]
	    set name "[$s cget -version] Trap"
	    ined -noupdate attribute $id $name "$trap at [getdate]"
	    ined -noupdate label $id $name
	    ined -noupdate flash $id 10
	}
    }
}

proc TrapSink {args} {
    static ts
    if {![info exists ts]} {
	set ts [snmp session -port 162]
    }
    switch $args {
	"" {
	    if {[$ts bind "" trap] == ""} {
		return ignore
	    } else {
		return listen
	    }
	}
	listen {
	    $ts bind "" trap {
		TrapSinkProc %S %A "%V"
	    }
	    save "TrapSink listen"
	}
	ignore {
	    $ts destroy
	    unset ts
	    forget "TrapSink listen"
	}
    }
}

proc "Trap Sink" {list} {
    set res [ined request "Listen for SNMP trap messages?" \
	 [list [list SNMPv1: [TrapSink] radio listen ignore]] "accept cancel"]
    if {[lindex $res 0] == "cancel"} return
    TrapSink [lindex $res 1]
}

##
## Display the jobs currently running.
##

proc "Monitor Job Info" { list } {
    MoJoInfo
}

##
## Modify the state or the interval of a running job.
##

proc "Modify Monitor Job" { list } {
    MoJoModify
}

##
## Set the parameters (community, timeout, retry) for snmp requests.
##

proc "Set Parameter" {list} {

    global snmp_community snmp_timeout snmp_retries snmp_port
    global snmp_protocol snmp_context
    global interval
    global default

    set result [ined request "SNMP Parameter" \
	[list [list "Community:" $snmp_community entry 20] \
          [list "UDP Port:" $snmp_port entry 10] \
          [list "Timeout \[s\]:" $snmp_timeout entry 10] \
          [list "Retries:" $snmp_retries scale 1 8] \
	  [list "Protocol:" $snmp_protocol radio SNMPv1 SNMPv2] \
	  [list "Context:" $snmp_context entry 10] \
          [list "Interval \[s\]:" $interval entry 10] \
	  [list "Use Graph Diagram:" $default(graph) radio true false ] ] \
	[list "set values" cancel] ]

    if {[lindex $result 0] == "cancel"} return

    set snmp_community [lindex $result 1]
    set snmp_port      [lindex $result 2]
    set snmp_timeout   [lindex $result 3]
    set snmp_retries   [lindex $result 4]
    set snmp_protocol  [lindex $result 5]
    set snmp_context   [lindex $result 6]
    set interval       [lindex $result 7]
    set default(graph) [lindex $result 8]
}

##
## Display some help about this tool.
##

proc "Help SNMP-Monitor" {list} {
    ined browse "Help about SNMP-Monitor" {
	"The SNMP-Monitor contains a set of utilities to monitor hosts" 
	"using SNMP requests. You can define thresholds by setting the" 
	"attributes Monitor:RisingThreshold or Monitor:FallingThreshold." 
	"The attribute Monitor:ThresholdAction controls what action will" 
	"be taken if the rising threshold or the falling threshold is" 
	"exceeded. The action types are syslog, flash and write." 
	"" 
	"Monitor Variable:" 
	"    Monitor a SNMP variable in a stripchart. Variables of Counter" 
	"    type are displayed as the delta value per second." 
	"" 
	"Interface Load:" 
	"    Compute the load of an interface and display it in a stripchart." 
	"    The load is calculated using Leinwand's formula:" 
	"" 
	"    util = ( 8 * ( delta (ifInOctets, t1, t0)" 
	"           + delta (ifOutOctets, t1, t0) ) / (t1 - t0) ) / ifSpeed" 
	"" 
	"    This formula returns incorrect results for full-duplex point" 
	"    to point links. In this case, the following formula is used:" 
	"" 
	"    util = ( 8 * max ( delta (ifInOctets, t1, t0) ," 
	"             delta (ifOutOctets, t1, t0) ) / (t1 - t0) ) / ifSpeed" 
	"" 
	"    See Simple Times 1(5), November/December, 1992 for more details." 
	"    The interface speed is usually retrieved from MIB-II but you can" 
	"    overwrite it by setting the SNMP-Monitor:IfSpeed attribute." 
	"" 
	"TCP Service User" 
	"    Show the number of users of a given TCP service. This can be" 
	"    useful to monitor NNTP connections." 
	"" 
	"Monitor Job Info:" 
	"    This command display information about all monitoring jobs" 
	"    started by this monitor script." 
	"" 
	"Modify Monitor Job:" 
	"    Select one of the monitoring jobs and modify it. You can change" 
	"    the sampling interval and switch the state from active (which" 
	"    means running) to suspended." 
	"" 
	"Set Parameter:" 
	"    This dialog allows you to set the sampling interval and " 
	"    SNMP related parameter like retries and community names." 
    }
}

##
## Delete the menus created by this interpreter.
##

proc "Delete SNMP-Monitor" {list} {

    global menus

    if {[job info] != ""} {
	set res [ined confirm "Kill running monitoring jobs?" \
		 [list "kill & exit" cancel] ]
	if {$res == "cancel"} return
    }

    DeleteClones

    foreach id $menus { ined delete $id }
    exit
}

set menus [ ined create MENU "SNMP-Monitor" \
	   "Monitor Variable" "" \
	   "Interface Load" "TCP Service User" "" \
	   "Trap Sink" "" \
	   "Monitor Job Info" "Modify Monitor Job" "" \
	   "Set Parameter" "" \
	   "Help SNMP-Monitor" "Delete SNMP-Monitor" ]
