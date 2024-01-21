#! /usr/local/bin/scotty -inf
##
## Some experimental HOST-MIB (rfc-1514) specific procs.
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

SnmpInit SNMP-Host

##
## Load the mib modules required by this script. This will prevent to load 
## all mib modules and reduce memory requirements and statup time.
##

mib load rfc1414.mib
mib load rfc1514.mib

##
## Show the hrSystem information of MIB II.
##

proc "System Information" {list} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
        SNMP_ShowScalars $s hrSystem
        $s destroy
    }
}

proc "Storage Information" {list} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	writeln "SNMP table hrStorageTable of $host \[$ip\]:"
	set txt "Index   Description       Unitsize      Total       Used   Allocated\n"
	try {
	    $s walk x "hrStorageIndex hrStorageDescr hrStorageAllocationUnits hrStorageSize hrStorageUsed" {
		set hridx   [lindex [lindex $x 0] 2]
		set hrdesc  [lindex [lindex $x 1] 2]
		set hrunits [lindex [lindex $x 2] 2]
		set hrtotal [lindex [lindex $x 3] 2]
		set hrused  [lindex [lindex $x 4] 2]
		set alloc   [expr $hrused * 100.0 / $hrtotal]
		append txt  [format " %-5s  %-15s %10d %10d %10d     %5.2f %%\n" \
			$hridx $hrdesc $hrunits $hrtotal $hrused $alloc]
	    }
	} msg {
	    append txt "$msg\n"
	}
	writeln $txt
	$s destroy
    }
}

proc "Device Information" {list} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	writeln "SNMP table hrDeviceTable of $host \[$ip\]:"
	set txt " Status  ProductID  Description\n"
	try {
	    $s walk x "hrDeviceStatus hrDeviceID hrDeviceDescr" {
		set hrstat [lindex [lindex $x 0] 2]
		set hrid   [lindex [lindex $x 1] 2]
		set hrdesc [lindex [lindex $x 2] 2]
		append txt [format "%-10s %-8s %s\n" \
			$hrstat $hrid $hrdesc]
	    }
	} msg {
	    append txt "$msg\n"
	}
	writeln $txt
	$s destroy
    }       
}

proc "Filesystem Information" {list} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	writeln "SNMP table hrFSTable of $host \[$ip\]:"
	set txt "Index    Mountpoint        Remote          StorageIndex\n"
	try {
	    $s walk x "hrFSIndex hrFSRemoteMountPoint hrFSMountPoint hrFSStorageIndex" {
		set fsidx   [lindex [lindex $x 0] 2]
		set fsremnt [lindex [lindex $x 1] 2]
		set fsmnt   [lindex [lindex $x 2] 2]
		set fssto   [lindex [lindex $x 3] 2]
		## XXX hack: format octets to a printable string:
		set fsmnt   [mib format sysDescr $fsmnt]
		set fsremnt [mib format sysDescr $fsremnt]
		append txt [format " %-8s %-16s %-16s %-8s\n" \
			$fsidx $fsremnt $fsmnt $fssto]
	    }
	} msg {
	    append txt "$msg\n"
	}
	writeln $txt
	$s destroy
    }       
}


proc "Process Information" {list} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	writeln "SNMP table hrSWRunTable of $host \[$ip\]:"
	set txt "ID         Status     Memory   CPU-Time    Command\n"
	try  {
	    $s walk x "hrSWRunIndex hrSWRunStatus hrSWRunParameters hrSWRunPerfCPU hrSWRunPerfMem" {
		set pid  [lindex [lindex $x 0] 2]
		set stat [lindex [lindex $x 1] 2]
		set cmd  [lindex [lindex $x 2] 2]
		set cpu  [lindex [lindex $x 3] 2]
		set mem  [lindex [lindex $x 4] 2]
		## XXX hack: format cmd from octets to a printable string:
		set cmd  [mib format sysDescr $cmd]
		## format cpu time to something readable:
		set cputime [format "%d:%02d.%02d" [expr $cpu / 6000] [expr ($cpu % 6000) / 100] [expr $cpu % 100]]
		append txt [format " %-8s %-10s %6sk %10s    %-20s\n" $pid $stat $mem $cputime $cmd]
	    }
	} msg {
	    append txt "$msg\n"
        }
        writeln $txt
	$s destroy
    }
}

proc "Ident Information" {list} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
        writeln "SNMP table identTable of $host \[$ip\]:"
        set txt " Source                 Destination            User-Identity\n"
	try {
	    $s walk x "identStatus identUserid identMisc" {
		set status [lindex [lindex $x 0] 2]
                set userid [mib format sysDescr [lindex [lindex $x 1] 2]]
                set misc   [mib format sysDescr [lindex [lindex $x 2] 2]]
		set index [split [lindex [lindex $x 0] 0] .]
		set src	[join [lrange $index 11 15] .]
		set dst [join [lrange $index 16 end] .]
		if {$status != "noError"} {
		    set user ""
		} else {
		    set user "$misc ($userid)"
		}
		append txt [format " %-22s %-22s %s\n" $src $dst $user]
	    }
	} msg {
            append txt "$msg\n"
	}
	writeln $txt
        $s destroy
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

proc "Delete SNMP-HOST" {list} {
    global menus
    foreach id $menus { ined delete $id }
    exit
}

##
## Display some help about this tool.
##

proc "Help SNMP-HOST" {list} {
    ined browse "Help about SNMP-HOST" {
	"System Information:" 
	"    Display the system information of the host mib variables." 
	"" 
	"Storage Information" 
	"    Display information about storage devices on the host." 
	"" 
	"Device Information" 
	"    Display information about devices connected to the host." 
	"" 
	"Filesystem Information" 
	"    Display information about the filesystems on this host." 
	"" 
	"Process Information" 
	"    Display information about running software on the host." 
	"" 
	"Ident Information" 
	"    Display information about the identity of TCP connections" 
	"    from/to this host." 
	"" 
	"Set Parameter:" 
	"    This dialog allows you to set SNMP parameters like retries, " 
	"    timeouts, community name and port number. " 
    }
}

set menus [ ined create MENU "SNMP-Host" \
	"System Information" \
	"Storage Information" \
	"Device Information" \
	"Filesystem Information" \
	"Process Information" "" \
	"Ident Information" "" \
	"Set Parameter" "" \
	"Help SNMP-HOST" "Delete SNMP-HOST"]

## end of snmp_host.tcl
