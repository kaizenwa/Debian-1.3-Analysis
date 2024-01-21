#! /usr/local/bin/scotty -inf
##
## Some experimental RMON-MIB (rfc-1757) specific procs.
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

SnmpInit SNMP-RMON

##
## Load the mib modules required by this script. This will prevent to load 
## all mib modules and reduce memory requirements and statup time.
##

mib load rfc1757.mib

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
## Show the hrSystem information of MIB II.
##

proc "History Control" {list} {
    ShowTable $list historyControlTable
}

proc "Ethernet History" {list} {
    ShowTable $list etherHistoryTable
}

proc "Host Control" {list} {
    ShowTable $list hostControlTable
}

proc "Host Table" {list} {
    ShowTable $list hostTable
}

proc "Host Time Table" {list} {
    ShowTable $list hostTimeTable
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

proc "Delete SNMP-RMON" {list} {
    global menus
    foreach id $menus { ined delete $id }
    exit
}

##
## Display some help about this tool.
##

proc "Help SNMP-RMON" {list} {
    ined browse "Help about SNMP-RMON" {
	"" 
	"Not yet available, sorry." 
	"" 
	"Set Parameter:" 
	"    This dialog allows you to set the sampling interval and " 
	"    SNMP related parameter like retries and community names." 
    }
}

set menus [ ined create MENU "SNMP-RMON" \
        "History Control" "Ethernet History" "" \
        "Host Control" "Host Table" "Host Time Table" "" \
	"Set Parameter" "" \
	"Help SNMP-RMON" "Delete SNMP-RMON"]

