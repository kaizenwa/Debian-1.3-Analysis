#! /usr/local/bin/scotty -inf
##
## This script is the beginning of a visual test quite for
## tkined and scotty. Its main purpose is to run on all the
## test systems before we make a new distribution. The script
## simply calls all tool commands without any user interaction.
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

proc Node { ip x y } {
    global list
    set id [ined create NODE]
    ined address $id $ip
    ined label $id address
    ined move $id $x $y
    ined select $id
    set list [list [ined retrieve $id]]
    return $id
}

proc Start { name } {
    global auto_path

    foreach dir $auto_path {
	if {[file exists $dir/$name]} {
	    set interp [ined create INTERPRETER $dir/$name]
	    # give 'em time to start up -- bad assumption
	    after 3000
	    return $interp
	}
    }

    ined acknowledge "Can not find $name." "Check your installation."
    exit
}

proc Call { interp cmd } {
    global list
    ined send $interp $cmd $list
}

proc Wait { interp } {
    while {[ined retrieve $interp] != ""} {
	after 1000
    }
}

##
## IP-Trouble
##

proc test_ip_trouble {} {

    set ok [ined confirm "Test IP Troubleshooter?" [list test skip cancel] ]
    if {$ok == "cancel"} exit
    if {$ok == "skip"} return

    set ip [ined request "Please enter a vaild IP address for this test:" \
	    [list [list "IP address:" 134.169.34.15] ] [list start cancel] ]
    if {[lindex $ip 0] == "cancel"} return
    set id [Node [lindex $ip 1] 60 60]

    set trouble [Start ip_trouble.tcl]

    Call $trouble "Set Parameter"
    Call $trouble "Ping"
    Call $trouble "Multi Ping"
    Call $trouble "Netmask"
    Call $trouble "Trace Route"

    Call $trouble "Telnet"
    Call $trouble "Rlogin"
    Call $trouble "Daytime"
    Call $trouble "Finger"
    Call $trouble "DNS Info"

    Call $trouble "TCP Services"
    Call $trouble "RPC Services"
    Call $trouble "NFS Exports"
    Call $trouble "NFS Mounts"

    Call $trouble "Delete IP-Trouble"

    Wait $trouble
    ined delete $id
}

##
## IP-Monitor
##

proc test_ip_monitor {} {

    set ok [ined confirm "Test IP Monitor?" [list test skip cancel] ]
    if {$ok == "cancel"} exit
    if {$ok == "skip"} return

    set ip [ined request "Please enter a vaild IP address for this test:" \
	    [list [list "IP address:" 134.169.34.15] ] [list start cancel] ]
    if {[lindex $ip 0] == "cancel"} return
    set id [Node [lindex $ip 1] 120 60]

    set monitor [Start ip_monitor.tcl]

    Call $monitor "Set Parameter"
    Call $monitor "Check Reachability"
    Call $monitor "Round Trip Time"

    Call $monitor "System Load"
    Call $monitor "CPU Activity"
    Call $monitor "Disk Activity"
    Call $monitor "Interface Activity"

    Call $monitor "Ethernet Load"

    Call $monitor "Delete IP-Monitor"

    Wait $monitor
    ined delete $id
}

##
## SNMP-Trouble
##

proc test_snmp_trouble {} {

    set ok [ined confirm "Test SNMP Troubleshooter?" [list test skip cancel] ]
    if {$ok == "cancel"} exit
    if {$ok == "skip"} return

    set ip [ined request "Please enter an IP address of an SNMP agent:" \
	    [list [list "IP address:" 134.169.34.3] ] [list start cancel] ]
    if {[lindex $ip 0] == "cancel"} return
    if {$ip == ""} return
    set id [Node [lindex $ip 1] 60 60]

    set trouble [Start snmp_trouble.tcl]

    Call $trouble "Show Defaults"
    Call $trouble "Set Parameter"

    Call $trouble "Show SNMP Devices"
    Call $trouble "System Information"

    Call $trouble "Interface Status"
    Call $trouble "Interface Parameter"
    Call $trouble "Interface Statistic"
    Call $trouble "Interface Quality"

    Call $trouble "Routing Table"
    Call $trouble "Arp Table"
    Call $trouble "Tcp Connections"
    Call $trouble "Udp Listener"

    Call $trouble "Walk MIB Tree"

    Call $trouble "Delete SNMP-Trouble"

    Wait $trouble
    ined delete $id
}

##
## SNMP-Monitor
##

proc test_snmp_monitor {} {

    set ok [ined confirm "Test SNMP Monitor?" [list test skip cancel] ]
    if {$ok == "cancel"} exit
    if {$ok == "skip"} return

    set ip [ined request "Please enter an IP address of an SNMP agent:" \
	    [list [list "IP address:" 134.169.34.3] ] [list start cancel] ]
    if {[lindex $ip 0] == "cancel"} return
    set id [Node [lindex $ip 1] 60 60]

    set monitor [Start snmp_monitor.tcl]

    Call $monitor "Show Defaults"
    Call $monitor "Set Parameter"

    Call $monitor "Monitor Variable"
    Call $monitor "Interface Load"

    Call $monitor "Delete SNMP-Monitor"

    Wait $monitor
    ined delete $id
}

##
## Here we start the test suite.
##

test_ip_trouble
test_ip_monitor

test_snmp_trouble
test_snmp_monitor
