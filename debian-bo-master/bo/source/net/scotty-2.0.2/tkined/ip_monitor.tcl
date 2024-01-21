#! /usr/local/bin/scotty -inf
##
## Simple IP-Monitor Tools for TKINED.
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

LoadDefaults icmp monitor

IpInit IP-Monitor

##
## Set up some default parameters.
##

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
    if {[ined type $id] != "STRIPCHART"} {
	if {$default(graph) != "true"} {
	    set id [CloneNode $id [ined create STRIPCHART] $x $y]
	} else {
	    set id [CloneNode $id [ined create GRAPH]]
	}
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

    set ids $args
    foreach id $ids {
	catch {ined -noupdate clear $id}
	catch {ined -noupdate clear $id}
	lappend list [ined retrieve $id]
    }
    catch { $cmd $list } err

    if {[info exists old_interval]} {
	set interval $old_interval
    }
}

##
## Extract all nodes of the given object list with a valid IP
## address that are reachable. Return a list of id/ip pairs.
##

proc extract { list } {

    set id_ip_list ""

    foreach comp $list {
	set type [ined type $comp]
        if {[lsearch "NODE STRIPCHART BARCHART GRAPH" $type] >= 0} {
            set id [ined id $comp]
            set host [lindex [ined name $comp] 0]
            set ip [GetIpAddress $comp]
            if {$ip == ""} {
                ined acknowledge "Can not lookup IP Address for $host."
                continue
            }
	    lappend id_ip_list [list $id $ip]
        }
    }

    return $id_ip_list
}

##
## Compute the diff between two rstat calls.
##

proc rstat_diff {l1 l2 period} {
    set len [llength $l1]
    set res ""
    for {set i "0"} {$i < $len} {incr i} {
        set el1 [lindex $l1 $i]
        set el2 [lindex $l2 $i]
        set tmp [lindex $el1 2]
        if {[lindex $el1 1] == "Counter"} {
            set tmp [expr {[lindex $el1 2]-[lindex $el2 2]}]
	    if {$period <= 0} {
		set tmp 0
	    } else {
		set tmp [expr {"$tmp.0" / $period}]
	    }
	}
        if {[lindex $el1 1] == "Gauge"} {
            set tmp [expr {[lindex $el1 2]/256.0}]
        }
        lappend res [format "%-12s %-12s %16.3f" \
		     [lindex $el1 0] [lindex $el1 1] $tmp]
    }
    return $res
}

##
## Send an ICMP request periodically to test if the selected nodes
## are reachable. This routine makes use of the multi threaded ping 
## mechanism.
##

proc reachability {ids} {

    global reachability_ip reachability_color reachability_label

    set ids [MoJoCheckIds reachability $ids]
    if {$ids == ""} return

    set id_list ""
    set ip_list ""

    foreach id $ids {
	set ip $reachability_ip($id)
	lappend id_list $id
	lappend ip_list $ip
    }
	
    if {$ip_list != ""} {
	set icmp_result [icmp echo $ip_list]
	set i 0
	foreach id $id_list {
	    set rtt [lindex [lindex $icmp_result $i] 1]
	    set ip [lindex $ip_list $i]
	    if {$rtt < 0} {
		if {$reachability_color($id) == ""} {
		    set reachability_color($id) [ined -noupdate color $id]
		    set reachability_label($id) [ined -noupdate label $id]
		    ined -noupdate color $id red
		}
		ined flash $id [expr {[[job current] interval] / 1000}]
		ined -noupdate attribute $id \
		    "round trip time" "[ined name $id]\nunreachable"
		ined -noupdate label $id "round trip time"
		MoJoAction $id "[ined name $id] \[$ip\] unreachable"
	    } else {
		if {$reachability_color($id) != ""} {
		    ined -noupdate color $id $reachability_color($id)
		    set reachability_color($id) ""
		}
		if {$reachability_label($id) != ""} {
		    ined -noupdate label $id $reachability_label($id)
		    set reachability_label($id) ""
		}
		ined -noupdate attribute $id \
		    "round trip time" "rtt $rtt ms"
		set txt "[ined name $id] \[$ip\] round trip time"
		MoJoCheckThreshold $id $txt $rtt "ms"
	    }
	    incr i
	}
    }
}

##
## Send an ICMP request periodically to test if the selected nodes
## are reachable. This s the actual proc that is invoked from the 
## user interface.
##

proc "Check Reachability" {list} {

    global interval
    global reachability_ip reachability_color reachability_label
    global reachability_ids

    if {![info exists reachability_ids]} { set reachability_ids "" }

    foreach id_ip [extract $list] {
	set id [lindex $id_ip 0]
	set ip [lindex $id_ip 1]

	set reachability_ip($id)   $ip
	set reachability_color($id) ""
	set reachability_label($id) ""
	lappend ids $id
	if {[lsearch $reachability_ids $id] < 0} {
	    lappend reachability_ids $id
	}
    }

    if {[info exists ids]} {
	set jid [job create [list reachability $ids] [expr {$interval * 1000}]]
	foreach id $ids { append rsids "\$$id " }
	save "restart {Check Reachability} $interval $jid $rsids"
    }
}

##
## Display the round trip time in a strip chart.
##

proc pingtime {ids} {

    global pingtime_ip

    set ids [MoJoCheckIds pingtime $ids]
    if {$ids == ""} return

    set id_list ""
    set ip_list ""

    foreach id $ids {
	set ip $pingtime_ip($id)
	lappend id_list $id
	lappend ip_list $ip
    }
	
    if {$ip_list != ""} {
	set icmp_result [icmp echo $ip_list]
	set i 0
	foreach id $id_list {
	    set ip  [lindex [lindex $icmp_result $i] 0]
	    set rtt [lindex [lindex $icmp_result $i] 1]
	    if {$rtt < 0} {
		ined values $id 0
		ined -noupdate attribute $id \
		    "round trip time" "[ined name $id]\nunreachable"
		MoJoAction $id "[ined name $id] \[$ip\] unreachable"
	    } else {
		ined values $id [expr {$rtt/10.0}]
		ined -noupdate attribute $id \
		    "round trip time" "rtt $rtt ms"
		set txt "[ined name $id] \[$ip\] round trip time"
		MoJoCheckThreshold $id $txt $rtt "ms"
	    }
	    incr i
	}
    }
}

##
## Display the round trip time in a strip chart. This 
## is the actual proc that is invoked from the user interface.
##

proc "Round Trip Time" {list} {

    global interval
    global pingtime_ip

    foreach id_ip [extract $list] {
	set id [lindex $id_ip 0]
	set ip [lindex $id_ip 1]

	set id [CreateChart $id 15 15]
	ined -noupdate attribute $id "round trip time" "rtt"
	ined -noupdate label $id "round trip time"

	set pingtime_ip($id) $ip
	lappend ids $id
    }

    if {[info exists ids]} {
	set jid [job create [list pingtime $ids] [expr {$interval * 1000}]]
	foreach id $ids { append rsids "\$$id " }
	save "restart {Round Trip Time} $interval $jid $rsids"
    }
}


##
## Display the ntp offset of a peer:
##

proc ntpoffset {ids} {

    global ntp_offset_ip

    set ids [MoJoCheckIds ntpoffset $ids]
    if {$ids == ""} return

    foreach id $ids {
	set ip $ntp_offset_ip($id)
	
	if {[catch {ntp $ip ntp} err]} {
	    ined -noupdate attribute $id "ntp offset" "$err"
	    ined values $id 0
	} else {
	    if {[info exists ntp(peer.offset)]} {
		set offset $ntp(peer.offset)
		ined -noupdate attribute $id "ntp offset" "$offset ms"
		set val [expr abs($offset)]
		ined values $id $val
		set txt "[ined name $id] \[$ip\] ntp offset"
		MoJoCheckThreshold $id $txt $val "ms"
	    }
	    if {[info exists ntp(peer.srcadr)]} {
		set srcadr [nslook $ntp(peer.srcadr)]
		ined -noupdate attribute $id "ntp peer" $srcadr"
	    } else {
		ined -noupdate attribute $id "ntp offset" "no ntp peer"
	    }
	}
    }
}

##
## Display the round trip time in a strip chart. This 
## is the actual proc that is invoked from the user interface.
##

proc "NTP Offset" {list} {

    global interval
    global ntp_offset_ip

    foreach id_ip [extract $list] {
	set id [lindex $id_ip 0]
	set ip [lindex $id_ip 1]

	set id [CreateChart $id 15 15]
	ined -noupdate attribute $id "ntp offset" "offset"
	ined -noupdate label $id "ntp offset"

	set ntp_offset_ip($id) $ip
	lappend ids $id
    }

    if {[info exists ids]} {
	set jid [job create [list ntpoffset $ids] [expr {$interval * 1000}]]
	foreach id $ids { append rsids "\$$id " }
	save "restart {NTP Offset} $interval $jid $rsids"
    }
}


##
## Show the load given by the rstat RPC in a stripchart.
##

proc sysload {ids} {

    global sysload_ip
    global sysload_time
    global sysload_stat

    set ids [MoJoCheckIds sysload $ids]
    if {$ids == ""} return

    foreach id $ids {
	set ip $sysload_ip($id)

	if {[catch {sunrpc stat $ip} res]} {
	    ined -noupdate values $id 0
	    ined -noupdate attribute $id "system load" $res
	    continue
	}
	
	set now [getclock]
	set time_diff [expr {$now - $sysload_time($id)}]
	set rstat [rstat_diff $res $sysload_stat($id) $time_diff]

	set sysload_stat($id) $res
	set sysload_time($id) $now
	set load [lindex [lindex $rstat 18] 2]
	ined -noupdate values $id $load
	ined -noupdate attribute $id "system load" "load $load"
	set txt "[ined name $id] \[$ip\] system load"
	MoJoCheckThreshold $id $txt $load
    }
}

##
## Show the load given by the rstat RPC in a stripchart. This 
## is the actual proc that is invoked from the user interface.
##

proc "System Load" {list} {

    global interval
    global sysload_ip
    global sysload_time
    global sysload_stat

    foreach id_ip [extract $list] {
	set id [lindex $id_ip 0]
	set ip [lindex $id_ip 1]

	if {[catch {sunrpc stat $ip} res]} {
	    ined acknowledge "[ined name $id] \[$ip\]: $res"
	    continue
	}

        set id [CreateChart $id 35 25]
	catch {ined -noupdate scale $id 1}
	ined -noupdate attribute $id "system load" "load"
	ined -noupdate label $id "system load"

	set sysload_ip($id)   $ip
	set sysload_stat($id) $res
	set sysload_time($id) [expr {[getclock]-$interval}]

	lappend ids $id
    }

    if {[info exists ids]} {
	set jid [job create [list sysload $ids] [expr {$interval * 1000}]]
	foreach id $ids { append rsids "\$$id " }
	save "restart {System Load} $interval $jid $rsids"
    }
}

##
## Show the cpu time split given by the rstat RPC in a barchart.
##

proc cpusplit {ids} {

    global cpusplit_ip
    global cpusplit_time
    global cpusplit_stat

    set ids [MoJoCheckIds cpusplit $ids]
    if {$ids == ""} return

    foreach id $ids {
	set ip $cpusplit_ip($id)

	if {[catch {sunrpc stat $ip} res]} {
	    ined -noupdate values $id 0
	    ined -noupdate attribute $id "cpu activity" $res
	    continue
	}
	
	set now [getclock]
	set time_diff [expr {$now - $cpusplit_time($id)}]
	set rstat [rstat_diff $res $cpusplit_stat($id) $time_diff]

	set cpusplit_stat($id) $res
	set cpusplit_time($id) $now

	set load ""
	foreach idx "0 1 2 3" {
	    lappend load [lindex [lindex $rstat $idx] 2]
	}
	ined -noupdate values $id $load
	ined -noupdate attribute $id "cpu activity" \
	    [eval format {"user %.1f nice %.1f\nsystem %.1f idle %.1f"} $load]
	set txt "[ined name $id] \[$ip\] cpu split"
	MoJoCheckThreshold $id $txt $load
    }
}

##
## Show the cpu time split given by the rstat RPC in a barchart. This
## is the actual proc that is invoked from the user interface.
##

proc "CPU Activity" {list} {

    global interval
    global cpusplit_ip
    global cpusplit_time
    global cpusplit_stat

    foreach id_ip [extract $list] {
	set id [lindex $id_ip 0]
	set ip [lindex $id_ip 1]

	if {[catch {sunrpc stat $ip} res]} {
	    ined acknowledge "[ined name $id] \[$ip\]: $res"
	    continue
	}

	if {[ined type $id] != "BARCHART"} {
	    set id [CloneNode $id [ined create BARCHART] 35 -35]
	}
	ined -noupdate attribute $id "cpu activity" "user nice system idle"
	ined -noupdate label $id "cpu activity"

	set cpusplit_ip($id)   $ip
	set cpusplit_stat($id) $res
	set cpusplit_time($id) [expr {[getclock]-$interval}]

	lappend ids $id
    }

    if {[info exists ids]} {
	set jid [job create [list cpusplit $ids] [expr {$interval * 1000}]]
	foreach id $ids { append rsids "\$$id " }
	save "restart {CPU Activity} $interval $jid $rsids"
    }
}

##
## Show the disk activity given by the rstat RPC in a barchart.
##

proc diskload {ids} {

    global diskload_ip
    global diskload_time
    global diskload_stat

    set ids [MoJoCheckIds diskload $ids]
    if {$ids == ""} return

    foreach id $ids {
	set ip $diskload_ip($id)

	if {[catch {sunrpc stat $ip} res]} {
	    ined -noupdate values $id 0
	    continue
	}
    
	set now [getclock]
	set time_diff [expr {$now - $diskload_time($id)}]
	set rstat [rstat_diff $res $diskload_stat($id) $time_diff]

	set diskload_stat($id) $res
	set diskload_time($id) $now

	set load ""
	foreach idx "4 5 6 7 8 9 10 11" {
	    lappend load [expr {2*[lindex [lindex $rstat $idx] 2]}]
	}
	ined values $id $load
	set txt "[ined name $id] \[$ip\] disk load"
	MoJoCheckThreshold $id $txt $load
    }
}

##
## Show the disk activity given by the rstat RPC in a barchart. This
## is the actual proc that is invoked from the user interface.
##

proc "Disk Activity" {list} {

    global interval
    global diskload_ip
    global diskload_time
    global diskload_stat

    foreach id_ip [extract $list] {
	set id [lindex $id_ip 0]
	set ip [lindex $id_ip 1]

	if {[catch {sunrpc stat $ip} res]} {
	    ined acknowledge "[ined name $id] \[$ip\]: $res"
	    continue
	}

        if {[ined type $id] != "BARCHART"} {
	    set id [CloneNode $id [ined create BARCHART] -35 40]
	}
	ined -noupdate attribute $id "disk activity" "d0 d1 d2 d3 pi po si so"
	ined -noupdate label $id "disk activity"

	set diskload_ip($id)   $ip
	set diskload_stat($id) $res
	set diskload_time($id) [expr {[getclock]-$interval}]

	lappend ids $id
    }

    if {[info exists ids]} {
	set jid [job create [list diskload $ids] [expr {$interval * 1000}]]
	foreach id $ids { append rsids "\$$id " }
	save "restart {Disk Activity} $interval $jid $rsids"
    }
}

##
## Show the disk activity given by the rstat RPC in a barchart.
##

proc ifload {ids} {

    global ifload_ip
    global ifload_time
    global ifload_stat

    set ids [MoJoCheckIds ifload $ids]
    if {$ids == ""} return

    foreach id $ids {
	set ip $ifload_ip($id)

	if {[catch {sunrpc stat $ip} res]} {
	    ined -noupdate values $id 0
	    continue
	}
	
	set now [getclock]
	set time_diff [expr {$now - $ifload_time($id)}]
	set rstat [rstat_diff $res $ifload_stat($id) $time_diff]

	set ifload_stat($id) $res
	set ifload_time($id) $now

	set load ""
	foreach idx "14 15 16 17" {
	    lappend load [expr {[lindex [lindex $rstat $idx] 2]}]
	}
	ined values $id $load
	ined -noupdate attribute $id "interface activity" \
	    [eval format {"in %.1f error %.1f\nout %.1f error %.1f"} $load]
	set txt "[ined name $id] \[$ip\] interface load"
	MoJoCheckThreshold $id $txt $load
    }
}

##
## Show the interface activity given by the rstat RPC in a barchart. This
## is the actual proc that is invoked from the user interface.
##

proc "Interface Activity" {list} {

    global interval
    global ifload_ip
    global ifload_time
    global ifload_stat

    foreach id_ip [extract $list] {
	set id [lindex $id_ip 0]
	set ip [lindex $id_ip 1]

	if {[catch {sunrpc stat $ip} res]} {
	    ined acknowledge "[ined name $id] \[$ip\]: $res"
	    continue
	}

        if {[ined type $id] != "BARCHART"} {
	    set id [CloneNode $id [ined create BARCHART] -35 -40]
	}
	ined -noupdate attribute $id "interface activity" "in ierr out oerr"
	ined -noupdate label $id "interface activity"

	set ifload_ip($id)   $ip
	set ifload_stat($id) $res
	set ifload_time($id) [expr {[getclock]-$interval}]

	lappend ids $id
    }

    if {[info exists ids]} {
	set jid [job create [list ifload $ids] [expr {$interval * 1000}]]
	foreach id $ids { append rsids "\$$id " }
	save "restart {Interface Activity} $interval $jid $rsids"
    }
}

##
## Show the ethernet load as reported by an etherstatd.
##

proc etherload {ids} {

    global etherload_ip

    set ids [MoJoCheckIds etherload $ids]
    if {$ids == ""} return

    foreach id $ids {
	set ip $etherload_ip($id)

	if {[catch {sunrpc ether $ip} load]} {
	    ined -noupdate attribute $id "ethernet load" $load
	    ined -noupdate values $id 0
	    continue
	}

	set time_ms [lindex [lindex $load 0] 2]
	set bytes   [lindex [lindex $load 1] 2]
	set packets [lindex [lindex $load 2] 2]
	set mbits   [expr {$bytes*8/1000000.0}]
	set time_s  [expr {$time_ms/1000.0}]
	set mbits_per_s   [expr {$mbits/$time_s}]
	set packets_per_s [expr {$packets/$time_s}]
	set bytes_per_s   [expr {$bytes*1.0/$packets}]
	ined -noupdate attribute $id "ethernet load" \
	    [format "%3.2f MBit/s\\n%4.1f Packet/s\\n %4.1f Bytes/Packet" \
	     $mbits_per_s $packets_per_s $bytes_per_s]
	set load [list [expr {$mbits_per_s*12.5}] \
		       [expr {$packets_per_s/20.0}] \
		       [expr {$bytes_per_s/10.0}] ]
	ined values $id $load
	set txt "[ined name $id] \[$ip\] ethernet load"
	MoJoCheckThreshold $id $txt $load
    }
}

##
## Show the ethernet load as reported by an etherstatd. This
## is the actual proc that is invoked from the user interface.
##

proc "Ethernet Load" {list} {

    global interval
    global etherload_ip

    foreach id_ip [extract $list] {
	set id [lindex $id_ip 0]
	set ip [lindex $id_ip 1]

	if {[catch {sunrpc ether $ip open} res]} {
	    if {[ined type $id] != "BARCHART"} {
		ined -noupdate attribute $id ""
            }
	    ined acknowledge "[ined name $id] \[$ip\]: $res"
            continue
	} else {

	    if {[ined type $id] != "BARCHART"} {
		set id [CloneNode $id [ined create BARCHART] -40 -25]
	    }
	    ined -noupdate attribute $id "ethernet load" "ethernet load"
	    ined -noupdate label $id "ethernet load"
	    set etherload_ip($id)   $ip
	    lappend ids $id
	}
    }

    if {[info exists ids]} {
	set jid [job create [list etherload $ids] [expr {$interval * 1000}]]
	foreach id $ids { append rsids "\$$id " }
	save "restart {Ethernet Load} $interval $jid $rsids"
    }
}

##
## Show the user connected to a host using the finger protocol.
##

proc users {ids} {

    global users_ip

    set ids [MoJoCheckIds users $ids]
    if {$ids == ""} return

    foreach id $ids {
        set ip $users_ip($id)
	if {[catch {tcp connect $ip finger} f]} {
	    continue
	}
	if {[catch {puts $f ""}]} {
	    catch {tcp close $f}
	    continue
	}
	catch {unset users}
	while {! [eof $f]} {
	    set user [string trimright [gets $f] "\r\n"]
	    if {$user == ""} continue
	    if {[string match "Login*Name*" $user]} continue
	    if {[string match "No*one*" $user]} continue
	    if {[string match "*TTY*" $user]} continue
	    set user [lindex $user 0]
	    if {[info exists users($user)]} {
		incr users($user)
	    } else {
		set users($user) 1
	    }
	}
	tcp close $f

	set txt ""
	foreach user [array names users] {
	    lappend txt "$user/$users($user)"
	}
	if {$txt == ""} {
	    set txt "no users"
	}

	ined -noupdate attribute $id "active users" $txt
    }
}

proc "Active Users" { list } {

    global interval
    global users_ip

    foreach id_ip [extract $list] {
	set id [lindex $id_ip 0]
	set ip [lindex $id_ip 1]

	if {[catch {tcp connect $ip finger} f]} continue
	tcp close $f

	ined -noupdate attribute $id "active users" "no users"
	ined label $id "active users"

	set users_ip($id) $ip

	lappend ids $id
    }

    if {[info exists ids]} {
        set jid [job create [list users $ids] [expr {$interval * 1000}]]
        foreach id $ids { append rsids "\$$id " }
        save "restart {Active Users} $interval $jid $rsids"
    }
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
## This simple dialog allows us to modify the monitor parameters.
##

proc "Set Parameter" {list} {

    global icmp_retries icmp_timeout icmp_delay
    global interval
    global default

    set result [ined request "Monitor Parameter" \
        [list [list "# of ICMP retries:" $icmp_retries scale 1 10] \
          [list "ICMP timeout \[s\]:" $icmp_timeout scale 1 42] \
          [list "Delay between ICMP packets \[ms\]:" $icmp_delay scale 1 100] \
	  [list "Interval \[s\]:" $interval entry 8] \
	  [list "Use Graph Diagram:" $default(graph) radio true false ] ] \
	[list "set values" cancel] ]

    if {[lindex $result 0] == "cancel"} return

    set icmp_retries   [lindex $result 1]
    set icmp_timeout   [lindex $result 2]
    set icmp_delay     [lindex $result 3]
    set interval       [lindex $result 4]
    set default(graph) [lindex $result 5]

    if {$interval<1} { set interval 1 }

    icmp -retries $icmp_retries
    icmp -timeout $icmp_timeout
    icmp -delay   $icmp_delay
}

##
## Show the defaults as loaded from the tkined.defaults files.
##

proc "Show Defaults" {list} {
    ShowDefaults
}

##
## Display some help about this tool.
##

proc "Help IP-Monitor" {list} {

    ined browse "Help about IP-Monitor" {
	"The IP-Monitor contains a set of utilities to monitor hosts" 
	"using simple ICMP messages or rstat/etherd RPCs. You can define" 
	"thresholds by setting the attributes Monitor:RisingThreshold" 
	"or Monitor:FallingThreshold. The attribute Monitor:ThresholdAction" 
	"controls what action will be taken if the rising threshold or" 
	"the falling threshold is exceeded. The action types are syslog," 
	"flash and write." 
	"" 
	"Check Reachability:" 
	"    This command will periodically send an echo ICMP request to the" 
	"    selected hosts and takes action when a host gets unreachable." 
	"" 
	"Round Trip Time:" 
	"    Send an ICMP echo request to the selected hosts and display the" 
	"    round trip time in a stripchart." 
	"" 
	"NTP offset:" 
	"    Send an NTP mode 6 request to the selected hosts and display the" 
	"    referenz peer offset in a stripchart." 
	"" 
	"System Load:" 
	"    Query the rstat daemon of the selected hosts and display the" 
	"    system load in stripchart." 
	"" 
	"CPU Split:" 
	"    Query the rstat daemon of the selected hosts and display the" 
	"    the cpu split (user nice system idle) in a bar chart." 
	"" 
	"Disk Activity:" 
	"    Query the rstat daemon of the selected hosts and display the" 
        "    the disk activity in a bar chart. The first four bars show the" 
	"    activity of disk 0 to 3. The fifth and sixth bar display the " 
	"    paging activity and the last two bars show the swapping load." 
	"" 
	"Ethernet Load:" 
	"    Query the etherstat daemon of the selected hosts and display" 
	"    the ethernet load, the number of packets and the average packet" 
	"    size over the selected time interval." 
	"" 
	"Active Users:" 
	"    Display the number of active users as reported by the finger" 
	"    daemon in the 'active users' attribute. Note, there might be" 
	"    active users that are not displayed because finger output" 
	"    is quite untrustworthy." 
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
	"    some other parameters that control the monitoring commands. " 
	"" 
	"Show Defaults:" 
	"    Show the defaults that may be defined in the tkined.defaults" 
	"    files. This script makes use of definitions in the form:" 
	"" 
	"        monitor.interval:      <number>" 
	"" 
    }
}

##
## Delete the menus created by this interpreter.
##

proc "Delete IP-Monitor" {list} {

    global menus
    global reachability_ids

    if {[job info] != ""} {
	set res [ined confirm "Kill running monitoring jobs?" \
		 [list "kill & exit" cancel] ]
	if {$res == "cancel"} return
    }

    if {[info exists reachability_ids]} {
	foreach id $reachability_ids {
	    ined -noupdate attribute $id "round trip time" ""
	}
    }

    DeleteClones

    foreach id $menus { ined delete $id }    
    exit
}

set menus [ ined create MENU "IP-Monitor" \
	    "Check Reachability" "Round Trip Time" "" \
	    "NTP Offset" "" \
	    "System Load" "CPU Activity" \
	    "Disk Activity" "Interface Activity" "" \
	    "Ethernet Load" "" \
	    "Active Users" "" \
            "Monitor Job Info" "Modify Monitor Job" "" \
	    "Set Parameter" "Show Defaults" "" \
	    "Help IP-Monitor" "Delete IP-Monitor" ]
