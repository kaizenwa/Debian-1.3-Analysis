#! /usr/local/bin/scotty -inf
##
## Some experimental extensions for the accounting meter NeTraMet.
##
## Copyright (c) 1994, 1995
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

SnmpInit SNMP-ACCT

##
## Load the mib modules required by this script. This will prevent to load 
## all mib modules and reduce memory requirements and statup time.
##

mib load netramet.mib

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
## Display the Information in the first group which controls the meter.
##

proc "Meter Information" {list} {
    ShowScalars $list acctMIB.acctControl
}

proc "Rule Set Info" {list} {
    ShowTable $list acctRuleSetInfoTable
}

proc "Flow Sampling Table" {list} {
    ShowTable $list acctFlowSamplingTable
}

proc "Collector Info Table" {list} {
    ShowTable $list acctCollectorInfoTable
}

##
## Display the Rule Table itself.
##

proc "Rule Table" {list} {
catch {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	try {
	    catch {unset ruleSize}
	    $s walk x "acctRuleInfoIndex acctRuleInfoRuleSize" {
		set index [lindex [lindex $x 0] 2]
		set size  [lindex [lindex $x 1] 2]
		writeln "$index $size"
		if {$size > 0} {
		    set ruleSize($index) $size
		}
	    }
	} msg {
	    writeln "$msg\n"
	    $s destroy
	    continue
	}
	writeln "howee"
	if {![info exists ruleSize]} continue
	ined list "Select a rule set:" [array names ruleSize] "select cancel"
    }
} err ; puts $err
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

proc "Delete SNMP-ACCT" {list} {
    global menus
    foreach id $menus { ined delete $id }
    exit
}

##
## Display some help about this tool.
##

proc "Help SNMP-ACCT" {list} {
    ined browse "Help about SNMP-ACCT" {
	"" 
	"Set Parameter:" 
	"    This dialog allows you to set SNMP parameters like retries, " 
	"    timeouts, community name and port number. " 
    }
}

set menus [ ined create MENU "SNMP-ACCT" "Meter Information" \
	    "Rule Set Info" "Flow Sampling Table" "Collector Info Table" "" \
	    "Rule Table" "" \
	    "Set Parameter" "" \
	    "Help SNMP-ACCT" "Delete SNMP-ACCT"]
