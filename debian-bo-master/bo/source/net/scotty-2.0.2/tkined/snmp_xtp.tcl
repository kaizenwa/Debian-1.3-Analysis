#! /usr/local/bin/scotty -inf
##
## Some XTP specific procs to show how it works.
##
## Copyright (c) 1995
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

LoadDefaults snmp

SnmpInit SNMP-XTP

set snmp_timeout 1

if {![catch {netdb services number "snmp-xtp udp"}]} {
    set snmp_port snmp-xtp
}

##
## Load the mib modules required by this script. This will prevent to load 
## all mib modules and reduce memory requirements and statup time.
##

mib load tubs.mib
mib load xtp.mib

##
## List all scalars of a group for the node objects in list.
##

proc ShowScalars {list group} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
        SNMP_ShowScalars $s $group
        $s destroy
    }
}

##
## Show a complete MIB table for the node objects in list.
##

proc ShowTable {list table} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	SNMP_ShowTable $s $table
	$s destroy
    }
}

##
## Callback for the monitor button.
##

proc StartMonitor { prefix } {

    static monitor
    global snmp_port

    foreach id [ined -noupdate select] {
	lappend list [ined -noupdate retrieve $id]
    }
    
    if {![info exists list]} return
    
    if {![info exists monitor]} {
	set monitor [ined create INTERPRETER snmp_monitor.tcl]
    } else {
	if {[ined retrieve $monitor] == ""} {
	    set monitor [ined create INTERPRETER snmp_monitor.tcl]
	}
    }

    ined send $monitor set snmp_port $snmp_port
    ined send $monitor MonitorVariable $list $prefix
}

##
## Try to figure which devices responds to XTP related SNMP requests.
##

proc XtpDeviceCallback {id ip host session error args} {
    if {$error == "noError"} {
	write   [format "%-32s \[" $host ]
	write   $ip "IpFlash $ip"
	set oid [lindex [lindex $args 0] 0]
	if {[string match [mib oid xtp].* $oid]} {
	    writeln "\]\t"
	} else {
	    writeln "\]\tSNMP but no XTP"
	}
    } else {
	writeln [format "%-32s \[%s\]\t%s" $host $ip $error]
    }
    $session destroy
}

proc "XTP SNMP Devices" {list} {
    writeln "XTP SNMP devices:"
    ForeachIpNode id ip host $list {
	[SnmpOpen $id $ip] getnext xtp \
		[list XtpDeviceCallback $id $ip $host %S %E %V]
    }
    snmp wait
    writeln
}

##
## Display information about the xtp daemon.
##

proc "Daemon Information" {list} {
    ShowScalars $list xtpDaemon
}

##
## Display information about the xtp context manager.
##

proc "Context Information" {list} {
    ShowScalars $list xtpContextManager
}

##
## Display the context table.
##

proc "Context Table" {list} {
    ShowTable $list xtpContextTable
}

##
## Monitor an XTP context. Uses the snmp_monitor.tcl script.
##

proc "Monitor Context" {list} {
    set lst ""
    foreach oid [mib suc xtpContextEntry] {
	if {[string match Counter* [mib syntax $oid]]} {
	    lappend lst [list $oid off radio on off]
	}
    }
    set res [ined request "Select the variables of interest:" \
	    $lst "accept cancel"]
    if {[lindex $res 0] == "cancel"} return
    set i 1
    set vbl ""
    foreach elem $lst {
	if {[lindex $res $i] == "on"} {
	    lappend vbl [lindex $elem 0]
	}
	incr i
    }
    if {$vbl == ""} return
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	set lst ""
	if {[catch {
	    $s walk x "xtpContextKey" {
		set idx [lindex [lindex $x 0] 2]
		lappend lst $idx
	    }
	} msg]} {
	    ined acknowledge "Failed to get contexts from $host: $msg"
	    continue
	}
	$s destroy
	if {$lst == ""} {
	    ined acknowledge "Sorry, no XTP contexts on $host."
	    continue
	}
	set res [ined list "Select a context identifier:" $lst "select cancel"]
	if {[lindex $res 0] == "cancel"} continue
	set instance [lindex $res 1]
	foreach oid $vbl {
	    StartMonitor $oid:$instance
	}
    }
}

##
## Monitor an XTP variable for all contexts.
##

proc "Monitor Variable" {list} {
    set lst [mib suc xtpContextEntry]
    set res [ined list "Select a variable:" $lst "select cancel"]
    if {[lindex $res 0] == "cancel"} return
    StartMonitor [lindex $res 1]
}

##
## Set the parameters (community, timeout, retry) for snmp requests.
##

proc "Set Parameter" {list} {
    SnmpParameter
}

##
## Delete the menus created by this interpreter.
##

proc "Delete SNMP-XTP" {list} {
    global menus
    foreach id $menus { ined delete $id }
    exit
}

##
## Display some help about this tool.
##

proc "Help SNMP-XTP" {list} {
    ined browse "NO Help about SNMP-XTP :-)" {
	"Set Parameter:" 
	"    This dialog allows you to set SNMP parameters like retries, " 
	"    timeouts, community name and port number. " 
    }
}

set menus [ ined create MENU "SNMP-XTP" \
	"XTP SNMP Devices" "" \
	"Daemon Information" "Context Information" "Context Table" "" \
	"Monitor Context" "Monitor Variable" "" \
	"Set Parameter" "" \
	"Help SNMP-XTP" "Delete SNMP-XTP"]
