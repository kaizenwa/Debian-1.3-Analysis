#! /usr/local/bin/scotty -inf
##
## Some CISCO specific procs to dump the accounting table.
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

LoadDefaults snmp

SnmpInit SNMP-CISCO

##
## Load the mib modules required by this script. This will prevent to load 
## all mib modules and reduce memory requirements and statup time.
##

mib load cisco.mib

##
## Sort a list in numerical order. The first argument is the list
## to be sorted and the second argument gives the index of the 
## element that will be used as the key.
##

proc numcmp {field a b} {
    return [expr {[lindex $a $field] < [lindex $b $field]}]
}

proc numsort {list field} {
    lsort -command "numcmp $field" $list 
}

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
## Display the system MIB of CISCO devices.
##

proc "Cisco Information" {list} {
    ShowScalars $list cisco.local.lsystem
}

##
## This callback is invoked to show host names for IP addresses or
## to create NODE objects for IP addresses shown in an accounting table.
##

proc ShowName { dst pkts byts } {

    static x y

    if {![info exists x]} { set x 50 }
    if {![info exists y]} { set y 50 }

    set name ""
    if {[catch {dns ptr $dst} name]} {
	if {[catch {nslook $dst} name]} {
	    set result [ined confirm "Can not look up name for $dst." "" \
			"Create a NODE for it?" [list create cancel] ]
	    if {$result != "create"} return
	}
    }

    if {$name != ""} {
	set result [ined confirm "$name \[$dst\]" "" "Create a NODE for it?" \
		   [list create cancel] ]
    }

    if {$result == "create"} {
	set id [ined create NODE]
	ined -noupdate name $id $name
	ined -noupdate address $id $dst
	ined -noupdate attribute $id "ip accounting" "$pkts pkts $byts byte"
	ined -noupdate label $id name
	ined -noupdate move $id [incr x 20] $y
	if {[incr y 20] > 500} { set y 50 }
    }
}

##
## Show the result of an accounting table of the cisco MIB.
##

proc ShowAccountTable { table } {
    writeln
    writeln [format "%16s  %16s  %12s %12s" \
	    Source Destination Packets Bytes]
    writeln "-------------------------------------------------------------"
    foreach x [numsort $table 3] {
	set src  [lindex $x 0]
	set dst  [lindex $x 1]
	set pkts [lindex $x 2]
	set byts [lindex $x 3]
	write [format "%16s " $src]
	write [format "%16s" $dst] "ShowName $dst $pkts $byts"
	writeln [format " %12s %12s" $pkts $byts]
    }
    writeln
}

##
## Retrieve an accounting table of the cisco MIB.
##

proc GetAccountingTable { list {ck 0} {src ""} } {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	if {$ck} {
	    set actAge cisco.local.lip.ckactAge.0
	    if {$src == ""} {
		set varlist "ckactSrc ckactDst ckactPkts ckactByts"
	    } else {
		set varlist "ckactSrc.$src ckactDst.$src ckactPkts.$src ckactByts.$src"
	    }
	} else {
	    set actAge cisco.local.lip.actAge.0
	    if {$src == ""} {
		set varlist "actSrc actDst actPkts actByts"
	    } else {
		set varlist "actSrc.$src actDst.$src actPkts.$src actByts.$src"
	    }
	}
	if {[catch {$s get $actAge} age]} {
	    ined acknowledge "Can not get accounting information of $host."
	    continue
	}
	set age [lindex [lindex $age 0] 2]
	if {$ck} {
	    writeln "Checkpoint accounting statistics of $host ([join $age]):"
	} else {
	    writeln "Accounting statistics of $host ([join $age]):"
	}
	set table ""
	$s walk row $varlist {
	    set Src  [lindex [lindex $row 0] 2]
	    set Dst  [lindex [lindex $row 1] 2]
	    set Pkts [lindex [lindex $row 2] 2]
	    set Byts [lindex [lindex $row 3] 2]
	    lappend table "$Src $Dst $Pkts $Byts"
	}
	ShowAccountTable $table
	$s destroy
    }    
}

proc "Accounting Table" {list} {
    GetAccountingTable $list
}

proc "Checkpoint Accounting Table" {list} {
    GetAccountingTable $list 1
}

##
## Only retrieve those table elements that start with a given IP
## address prefix.
##

proc "Bytes send from Host" {list} {

    static src
    if {![info exists src]} { set src "" }

    set result [ined request "Set filter to source IP address:" \
	       [list [list IP: $src] \
	             [list Table: "actual" radio actual checkpointed] ] \
	       [list start cancel] ]
    if {[lindex $result 0] == "cancel"} return
    set src [lindex $result 1]
    set ck  [expr {[lindex $result 2] == "checkpointed"}]

    # should convert hostnames to ip dot notation

    if {![regexp "^\[0-9\]+\.\[0-9\]+\.\[0-9\]+\.\[0-9\]+$" $src]} {
	if {[catch {nslook $src} src]} {
	    ined acknowledge "Can not convert to IP address."
	    return
	}
    }
    set src [lindex $src 0]

    GetAccountingTable $list $ck $src
}

##
## Display active connections on terminal server lines.
##

proc "Active Sessions" {list} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]

	set txt "Active terminal sessions of $host \[$ip\]:\n"
	append txt "Type Direction Active Idle Address         Name\n"
	$s walk x {
	    tslineSesType tslineSesDir tslineSesAddr tslineSesName
	    tslineSesCur tslineSesIdle
	} {
	    set SesType [lindex [lindex $x 0] 2]
	    set SesDir  [lindex [lindex $x 1] 2]
	    set SesAddr [lindex [lindex $x 2] 2]
	    set SesName [lindex [lindex $x 3] 2]
	    set SesCur  [lindex [lindex $x 4] 2]
	    set SesIdle [lindex [lindex $x 5] 2]
	    append txt [format "%3s   %4s     %4s %5s  %-15s %s\n" \
			$SesType $SesDir $SesCur $SesIdle $SesAddr $SesName]
	}
	$s destroy
	writeln $txt
    }
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

proc "Delete SNMP-CISCO" {list} {
    global menus
    foreach id $menus { ined delete $id }
    exit
}

##
## Display some help about this tool.
##

proc "Help SNMP-CISCO" {list} {
    ined browse "Help about SNMP-CISCO" {
	"Cisco Information:" 
	"    Display some interesting system specific information for a" 
	"    CISCO device." 
	"" 
	"Active Sessions:" 
	"    List the currently active terminal server sessions." 
	"" 
	"Set Parameter:" 
	"    This dialog allows you to set SNMP parameters like retries, " 
	"    timeouts, community name and port number. " 
    }
}

set menus [ ined create MENU "SNMP-CISCO" "Cisco Information" "" \
	    "Accounting Table" "Checkpoint Accounting Table" \
	    "Bytes send from Host" "" \
	    "Active Sessions" "" \
	    "Set Parameter" "" \
	    "Help SNMP-CISCO" "Delete SNMP-CISCO"]
