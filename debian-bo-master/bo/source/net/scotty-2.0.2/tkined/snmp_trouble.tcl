#! /usr/local/bin/scotty -inf
##
## Simple SNMP-Troubleshooting utilities for tkined.
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

SnmpInit SNMP-Trouble

##
## Load the mib modules required by this script. This will prevent to load 
## all mib modules and reduce memory requirements and statup time.
##

# mib load rfc1447.mib
# mib load rfc1450.mib
# mib load rfc1451.mib

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
## Try to figure out if the devices responds to snmp requests.
##

proc SnmpDeviceCallback {id ip host session error} {
    if {$error == "noError" } {
	write   [format "%-32s \[" $host ]
	write   $ip "IpFlash $ip"
	writeln "\] "
	ined -noupdate attribute $id "SNMP:Config" [$session configure]
    } else {
	writeln [format "%-32s \[%s\] %s" $host $ip $error]
    }
    $session destroy
}

proc "SNMP Devices" {list} {
    writeln "SNMP devices:"
    ForeachIpNode id ip host $list {
	[SnmpOpen $id $ip] getnext 1.0 \
		[list SnmpDeviceCallback $id $ip $host %S %E]
    }
    snmp wait
    writeln
}

##
## Show the system information of MIB II.
##

proc "System Information" {list} {
    ShowScalars $list system
}

##
## Get the list of interfaces and their status.
##

proc "IF Status" {list} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	set txt "Interface status of $host \[$ip\]:\n"
	append txt "ifIndex ifDescr            ifAdminStatus ifOperStatus (ifType)\n"
	try {
	    $s walk x "ifIndex ifDescr ifAdminStatus ifOperStatus ifType" {
		set ifIndex [lindex [lindex $x 0] 2]
		set ifDescr [lindex [lindex [lindex [lindex $x 1] 2] 0] 0]
		set ifAdmin [lindex [lindex $x 2] 2]
		set ifOper  [lindex [lindex $x 3] 2]
		set ifType  [lindex [lindex $x 4] 2]
		append txt [format "%5s   %-16s %12s %12s    (%s)\n" \
			$ifIndex $ifDescr $ifAdmin $ifOper $ifType]
	    }
	} msg { 
	    append txt "$msg\n" 
	}
	writeln $txt
	$s destroy
    }
}

##
## Get the list of interfaces and their status.
##

proc "IF Parameter" {list} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	set txt "Interface parameter of $host \[$ip\]:\n"
	append txt "ifIndex ifDescr              ifSpeed      ifMtu   ifPhysAddress\n"
	try {
	    $s walk x "ifIndex ifDescr ifSpeed ifMtu ifPhysAddress" {
		set ifIndex [lindex [lindex $x 0] 2]
		set ifDescr [lindex [lindex [lindex [lindex $x 1] 2] 0] 0]
		set ifSpeed [lindex [lindex $x 2] 2]
		set ifMtu   [lindex [lindex $x 3] 2]
		set ifPhys  [lindex [lindex $x 4] 2]
		append txt [format " %5s  %-16s %11s %10s   %s\n" \
			$ifIndex $ifDescr $ifSpeed $ifMtu $ifPhys]
	    }
	} msg { 
	    append txt "$msg\n" 
	}
	writeln $txt
	$s destroy
    }
}

##
## We should check which interfaces are up and running.
##

proc "IF Usage Statistics" {list} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	set txt "Interface usage of $host \[$ip\]:\n"
        append txt "ifIndex ifDescr           ifInOctets  ifInUcastPkts  ifOutOctets ifOutUcastPkts\n"
	try {
	    $s walk x "ifIndex ifDescr ifInOctets ifInUcastPkts \
		    ifOutOctets ifOutUcastPkts" {
		set ifIndex   [lindex [lindex $x 0] 2]
		set ifDescr   [lindex [lindex [lindex [lindex $x 1] 2] 0] 0]
		set ifInOct   [lindex [lindex $x 2] 2]
		set ifInUOct  [lindex [lindex $x 3] 2]
		set ifOutOct  [lindex [lindex $x 4] 2]
		set ifOutUOct [lindex [lindex $x 5] 2]
		append txt [format " %5s  %-15s %12s %14s %12s %14s\n" \
			$ifIndex $ifDescr $ifInOct $ifInUOct \
			$ifOutOct $ifOutUOct]
	    }
	} msg {
            append txt "$msg\n"
        }
	writeln $txt
	$s destroy
    }
}


proc "IF Error Statistics" {list} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	set txt "Interface errors of $host \[$ip\]:\n"
	append txt "ifIndex ifDescr           ifInErrors   ifInDiscards  ifOutErrors  ifOutDiscards\n"
	try {
	    $s walk x "ifIndex ifDescr ifInErrors ifInDiscards \
		    ifOutErrors ifOutDiscards" {
		set ifIndex   [lindex [lindex $x 0] 2]
		set ifDescr   [lindex [lindex [lindex [lindex $x 1] 2] 0] 0]
		set ifInErr   [lindex [lindex $x 2] 2]
		set ifInDisc  [lindex [lindex $x 3] 2]
		set ifOutErr  [lindex [lindex $x 4] 2]
		set ifOutDisc [lindex [lindex $x 5] 2]
		append txt [format " %5s  %-15s %12s %14s %12s %14s\n" \
			$ifIndex $ifDescr $ifInErr $ifInDisc \
			$ifOutErr $ifOutDisc]
	    }
        } msg {
            append txt "$msg\n"
        }
	writeln $txt
	$s destroy
    }
}

##
## Display relative error statistics.
##

proc "IF Quality" {list} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	set txt "Interface quality of $host \[$ip\]:\n"
	append txt "(may be invalid if counter have wrapped)\n"
	append txt "ifIndex ifDescr        Error Rate   Discard Rate\n"
	try {
	    $s walk x "ifIndex ifDescr ifInUcastPkts ifInNUcastPkts \
		    ifOutUcastPkts ifOutNUcastPkts ifInErrors ifOutErrors \
		    ifInDiscards ifOutDiscards" {
		set ifIndex   [lindex [lindex $x 0] 2]
		set ifDescr   [lindex [lindex [lindex [lindex $x 1] 2] 0] 0]
		set in  [expr [lindex [lindex $x 2] 2] + [lindex [lindex $x 3] 2]]
		set out [expr [lindex [lindex $x 4] 2] + [lindex [lindex $x 5] 2]]
		set pkts [expr {$in + $out}]
		if {$in == 0} {
		    set err 0
		    set dis 0
		} else {
		    set err [expr {[lindex [lindex $x 6] 2] \
			    + [lindex [lindex $x 7] 2]}]
		    set err [expr {100 * double($err) / $pkts}]
		}
		if {$out == 0} {
		    set err 0
		    set dis 0
		} else {
		    set dis [expr {[lindex [lindex $x 8] 2] \
			    + [lindex [lindex $x 9] 2]}]
		    set dis [expr {100 * double($dis) / $pkts}]
		}
		append txt \
			[format "%5s   %-15s  %5.2f %%        %5.2f %%\n" \
			$ifIndex $ifDescr $err $dis]
	    }
	} msg {
            append txt "$msg\n"
        }
	writeln $txt
	$s destroy
    }
}

##
## Show the IP Scalars.
##

proc "IP Statistics" {list} {
    ShowScalars $list ip
}

##
## Get the IP addresses belonging to this device.
##

proc "IP Addresses" {list} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	set txt "IP addresses of $host \[$ip\]:\n"
	append txt "ifIndex ipAdEntAddr     ipAdEntNetMask ipAdEntBcastAddr\n"
	try {
	    set list ""
	    $s walk x {ipAdEntAddr ipAdEntNetMask 
	               ipAdEntBcastAddr ipAdEntIfIndex} {
		set ipAdEntAddr      [lindex [lindex $x 0] 2]
		set ipAdEntNetMask   [lindex [lindex $x 1] 2]
		set ipAdEntBcastAddr [lindex [lindex $x 2] 2]
		set ipAdEntIfIndex   [lindex [lindex $x 3] 2]
		lappend list [format "%5s   %-15s %-15s %5s" $ipAdEntIfIndex \
			$ipAdEntAddr $ipAdEntNetMask $ipAdEntBcastAddr]
	    }
	    append txt "[join [lsort $list] "\n"]\n"
	} msg { 
	    append txt "$msg\n" 
	}
	writeln $txt
	$s destroy
    }
}

##
## Show the routing tables of the agents.
##

proc "IP Routing Table" {list} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	set txt "Routing Table of $host \[$ip\]:\n"
	append txt "ipRouteDest      ipRouteNextHop   ipRouteMask      IfIndex    Type     Proto\n"
	try {
	    $s walk x {ipRouteDest ipRouteNextHop ipRouteMask ipRouteIfIndex
	    ipRouteType ipRouteProto} {
		set ipRouteDest    [lindex [lindex $x 0] 2]
		set ipRouteNext    [lindex [lindex $x 1] 2]
		set ipRouteMask    [lindex [lindex $x 2] 2]
		set ipRouteIfIndex [lindex [lindex $x 3] 2]
		set ipRouteType    [lindex [lindex $x 4] 2]
		set ipRouteProto   [lindex [lindex $x 5] 2]
		append txt [format "%-16s %-16s %-16s %5d %10s %8s\n" \
			$ipRouteDest $ipRouteNext $ipRouteMask \
			$ipRouteIfIndex $ipRouteType $ipRouteProto]
	    }
	} msg { 
	    append txt "$msg\n" 
	}
	writeln $txt
	$s destroy
    }
}

##
## Show the routing tables of the agents.
##

proc "IP ARP Table" {list} {
    global vendor
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	set txt "ARP Table of $host \[$ip\]:\n"
	append txt "ifIndex PhysAddress        NetAddress         Type       Vendor\n"
	set done 0
	$s walk x {ipNetToMediaIfIndex ipNetToMediaPhysAddress 
	ipNetToMediaNetAddress ipNetToMediaType} {
	    set ipNetToMediaIfIndex	    [lindex [lindex $x 0] 2]
	    set ipNetToMediaPhysAddress [lindex [lindex $x 1] 2]
	    set ipNetToMediaNetAddress  [lindex [lindex $x 2] 2]
	    set ipNetToMediaType        [lindex [lindex $x 3] 2]
	    set done 1
	    set l [join [lrange [split $ipNetToMediaPhysAddress :] 0 2] {}]
	    set l [string toupper $l]
	    if {[info exists vendor($l)]} {
		set vend $vendor($l)
	    } else {
		set vend ""
	    }
	    append txt [format " %5s  %-18s %-18s %-10s %s\n" \
			$ipNetToMediaIfIndex $ipNetToMediaPhysAddress \
			$ipNetToMediaNetAddress $ipNetToMediaType $vend]
	}
	if {!$done} {
	    $s walk x {atIfIndex atPhysAddress atNetAddress} {
		set atIfIndex     [lindex [lindex $x 0] 2]
		set atPhysAddress [lindex [lindex $x 1] 2]
		set atNetAddress  [lindex [lindex $x 2] 2]
		set l [join [lrange [split $atPhysAddress :] 0 2] {}]
		set l [string toupper $l]
		if {[info exists vendor($l)]} {
		    set vend $vendor($l)
		} else {
		    set vend ""
		}
		append txt [format " %5s  %-18s %-18s %s\n" \
			    $atIfIndex $atPhysAddress $atNetAddress $vend]
	    }
	}
	writeln $txt
	$s destroy
    }
}

##
## Show the TCP Scalars.
##

proc "TCP Statistics" {list} {
    ShowScalars $list tcp
}

##
## Show the tcp connection tables of the agents.
##

proc "TCP Connections" {list} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	set percent -1
	if {![catch {$s get "tcp.tcpMaxConn.0 tcp.tcpCurrEstab.0"} vbl]} {
	    set tcpMaxConn   [lindex [lindex $vbl 0] 2]
	    set tcpCurrEstab [lindex [lindex $vbl 1] 2]
	    if {$tcpMaxConn > 0} {
		set percent [expr {$tcpCurrEstab/double($tcpMaxConn)*100}]
	    }
	}
	    
	if {$percent > 0} {
	    set txt "TCP Connections of $host \[$ip\] ($percent% established):\n"
	} else {
	    set txt "TCP Connections of $host \[$ip\]:\n"
	}
	append txt "State            LocalAddress       LocalPort   RemoteAddress       RemotePort\n"
	try {
	    $s walk x "tcpConnState tcpConnLocalAddress tcpConnLocalPort \
		    tcpConnRemAddress tcpConnRemPort" {
		set tcpConnState        [lindex [lindex $x 0] 2]
		set tcpConnLocalAddress [lindex [lindex $x 1] 2]
		set tcpConnLocalPort    [lindex [lindex $x 2] 2]
		set tcpConnRemAddress   [lindex [lindex $x 3] 2]
		set tcpConnRemPort      [lindex [lindex $x 4] 2]

		if {![catch {netdb services name "$tcpConnLocalPort tcp"} n]} {
		    set tcpConnLocalPort "$n"
		}
		if {![catch {netdb services name "$tcpConnRemPort tcp"} n]} {
		    set tcpConnRemPort "$n"
		}

		append txt [format "%-12s %16s %15s %16s %15s\n" \
			$tcpConnState \
			$tcpConnLocalAddress $tcpConnLocalPort \
			$tcpConnRemAddress $tcpConnRemPort]
	    }
	} msg {
	    append txt "$msg\n"
	}
	writeln $txt
	$s destroy
    }
}

##
## Show the UDP Scalars.
##

proc "UDP Statistics" {list} {
    ShowScalars $list udp
}

##
## Show the udp listener table of the agents.
##

proc "UDP Listener" {list} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	set txt "UDP Listener of $host \[$ip\]:\n"
	append txt "      LocalAddress       LocalPort\n"
	try {
	    $s walk x "udpLocalAddress udpLocalPort" {
		set udpLocalAddress	[lindex [lindex $x 0] 2]
		set udpLocalPort    [lindex [lindex $x 1] 2]
		if {![catch {netdb services name "$udpLocalPort udp"} n]} {
		    set udpLocalPort "$n"
		}
		append txt [format "  %16s %15s\n" \
			$udpLocalAddress $udpLocalPort]
	    }
	} msg {
	    append txt "$msg\n"
	}
	writeln $txt
	$s destroy
    }
}

##
## Show the SNMP Scalars.
##

proc "SNMP Statistics" {list} {
    ShowScalars $list snmp
}

##
## Show the ICMP Scalars.
##

proc "ICMP Statistics" {list} {
    ShowScalars $list icmp
}

##
## Show the important stuff in the party database.
##

proc "Parties" {list} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	set txt "SNMPv2 Parties of $host \[$ip\]:\n"
	append txt "Index  Transport-Address          AuthProtocol       PrivProtocol \n"
	try {
	    $s walk x "partyIndex partyTDomain partyTAddress \
		    partyAuthProtocol partyPrivProtocol" {
		set Index    [lindex [lindex $x 0] 2]
		set TDomain  [lindex [lindex $x 1] 2]
		set TAddress [lindex [lindex $x 2] 2]
		set AuthPro  [lindex [lindex $x 3] 2]
		set PrivPro  [lindex [lindex $x 4] 2]
		if {[mib oid $TDomain] == "1.3.6.1.6.1.1"} {
		    scan $TAddress "%x:%x:%x:%x:%x:%x" i1 i2 i3 i4 p1 p2
		    set TAddress [format "%d.%d.%d.%d/%d" \
			    $i1 $i2 $i3 $i4 [expr $p1 + $p2]]
		}
		append txt [format "%4s   %-20s %18s %18s\n" \
			$Index $TAddress $AuthPro $PrivPro]
	    }
	} msg {
	    append txt "$msg\n"
	}
	writeln $txt
	$s destroy
    }
}

##
## Show the context defined at the selected agents.
##

proc "Contexts" {list} {
    ForeachIpNode id ip host $list {
        set s [SnmpOpen $id $ip]
        set txt "SNMPv2 Contexts of $host \[$ip\]:\n"
        append txt "Index  ViewIndex Local       Storage \n"
        try {
	    $s walk x "contextIndex contextViewIndex contextLocal \
		    contextStorageType" {
		set Index    [lindex [lindex $x 0] 2]
		set ViewIdx  [lindex [lindex $x 1] 2]
		set Local    [lindex [lindex $x 2] 2]
		set Storage  [lindex [lindex $x 3] 2]
		append txt [format "%4s  %6s %8s %14s\n" \
                        $Index $ViewIdx $Local $Storage]
	    }
	} msg {
	    append txt "$msg\n"
	}
	writeln $txt
        $s destroy
    }
}

##
## Show the access control table.
##

proc "Views" {list} {
    ForeachIpNode id ip host $list {
        set s [SnmpOpen $id $ip]
        set txt "SNMPv2 Views of $host \[$ip\]:\n"
        append txt "Index Tree Mask Type Storage \n"
        try {
	    $s walk x "viewIndex viewSubtree viewMask viewType viewStorageType" {
		set Index    [lindex [lindex $x 0] 2]
		set Tree     [lindex [lindex $x 1] 2]
		set Mask     [lindex [lindex $x 2] 2]
		set Type     [lindex [lindex $x 3] 2]
		set Storage  [lindex [lindex $x 4] 2]
		append txt [format "%4s  %6s %8s %14s %14s\n" \
                        $Index $Tree $Mask $Type $Storage]
	    }
	} msg {
	    append txt "$msg\n"
	}
	writeln $txt
        $s destroy
    }
}

##
## Show the SNMPv2 statistics.
## 

proc "Statistics" {list} {
    ShowScalars $list snmpStats
}

##
## Show the access control table.
##

proc "Access Control" {list} {
    ForeachIpNode id ip host $list {
        set s [SnmpOpen $id $ip]
        set txt "SNMPv2 Access Control of $host \[$ip\]:\n"
        append txt "Target Subject Context Privileges Storage \n"
	try {
	    $s walk x "aclTarget aclSubject aclResources aclPrivileges \
		    aclStorageType" {
		set Target     [lindex [lindex $x 0] 2]
		set Subject    [lindex [lindex $x 1] 2]
		set Resources  [lindex [lindex $x 2] 2]
		set Privileges [lindex [lindex $x 3] 2]
		set Storage    [lindex [lindex $x 4] 2]
		append txt [format "%4s  %6s %8s %14s %14s\n" \
                        $Target $Subject $Resources $Privileges $Storage]
	    }
	} msg {
            append txt "$msg\n"
	 }
        writeln $txt
        $s destroy
    }
}

##
## Dump a complete hierarchy. The user may choose the hierarchy
## before we start our action. We store the last selected hierarchy
## in a static variable called dump_mib_tree_path.
##

proc "Walk MIB Tree" {list} {

    static dump_mib_tree_path

    if {![info exists dump_mib_tree_path]} {
        set dump_mib_tree_path "mib-2"
    }

    set path [ined request "Walk MIB Tree:" \
	        [list [list "MIB path:" $dump_mib_tree_path] ] \
		[list walk cancel] ]

    if {[lindex $path 0]== "cancel"} return

    set dump_mib_tree_path [lindex $path 1]

    ForeachIpNode id ip host $list {
	write   "MIB Tree Walk for $host \[$ip\] "
	writeln "starting at $dump_mib_tree_path:"
	set s [SnmpOpen $id $ip]
        SNMP_Walk $s $dump_mib_tree_path
        $s destroy
	writeln
    }
}

##
## Set the parameters (community, timeout, retry) for snmp requests.
##

proc "Set Parameter" {list} {
    SnmpParameter
}

##
## Display some help about this tool.
##

proc "Help SNMP-Trouble" {list} {
    ined browse "Help about SNMP-Trouble" {
	"SNMP Devices:" 
	"    Test which of the selected nodes respond to SNMP requests." 
	"" 
	"System Information:" 
	"    Display the information of the system group." 
	"" 
	"Interface -> IF Status:" 
	"    Display status information of the interfaces." 
	"" 
	"Interface -> IF Parameter:" 
	"    Display interface parameter like speed and MTU." 
	"" 
	"Interface -> IF Usage Statistics:" 
	"    Display interface statistics." 
	"" 
	"Interface -> IF Error Statistics:" 
	"    Display interface statistics." 
	"" 
	"Interface -> IF Quality:" 
	"    Show the error and discard rate per received packed for" 
	"    each interface. The output is only valid if your counters" 
	"    have not wrapped!" 
	"" 
	"" 
	"IP -> IP Statistics" 
	"    Displays some statistics and parameter of the IP layer." 
	"" 
	"IP -> IP Addresses:" 
	"    Show the IP addresses used by this device." 
	"" 
	"IP -> IP Routing Table:" 
	"    Display the routing table." 
	"" 
	"IP -> IP ARP Table:" 
	"    Display the ipNetToMedia Table of the selected hosts." 
	"    Ethernet addresses are converted to vendor names using" 
	"    the Ethernet-Code-List available from MIT." 
	"" 
	"TCP -> TCP Statistics" 
	"    Statistics and parameter of the TCP layer." 
	"" 
	"TCP -> TCP Connections:" 
	"    Display the status of existing TCP connections." 
	"" 
	"UDP -> UDP Statistics" 
	"    Statistics and parameter of the UDP layer." 
	"" 
	"UDP -> UDP Listener:" 
	"    Display the status of existing UDP listener." 
	"" 
	"ICMP Statistics:" 
	"    Statistics and parameter of the ICMP layer." 
	"" 
	"SNMP Statistics:" 
	"    Statistics and parameter of the SNMP layer." 
	"" 
	"Walk MIB Tree:" 
	"    Walk through the MIB tree and print the object values." 
	"" 
	"Set Parameter:" 
	"    This dialog allows you to set SNMP parameters like retries, " 
	"    timeouts, community name and port number. " 
    }
}

##
## Delete the menus created by this interpreter.
##

proc "Delete SNMP-Trouble" {list} {
    global menus
    foreach id $menus { ined delete $id }
    exit
}

set menus [ ined create MENU "SNMP-Trouble" \
    "SNMP Devices" "" \
    "System Information" "" \
    "Interfaces:IF Status" \
    "Interfaces:IF Parameter" \
    "Interfaces:IF Usage Statistics" \
    "Interfaces:IF Error Statistics" \
    "Interfaces:IF Quality" \
    "IP:IP Statistics" \
    "IP:IP Addresses" \
    "IP:IP Routing Table" \
    "IP:IP ARP Table" \
    "TCP:TCP Statistics" \
    "TCP:TCP Connections" \
    "UDP:UDP Statistics" \
    "UDP:UDP Listener" "" \
    "ICMP Statistics" \
    "SNMP Statistics" "" \
    "SNMPv2:Parties" "SNMPv2:Contexts" "SNMPv2:Views" \
    "SNMPv2:Access Control" "SNMPv2:Statistics" "" \
    "Walk MIB Tree" "" \
    "Set Parameter" "" \
    "Help SNMP-Trouble" "Delete SNMP-Trouble" ]

##
## The following ethernet address list is take from the ethernet code 
## list maintained by Michael A. Patton, MIT as of 8-Apr-93.
##
## It is available via anonymous FTP from FTP.LCS.MIT.EDU with the name
## pub/map/EtherNet-codes. Thanks to Michael A. Patton and all the 
## contributorsfor making this list available.
##

set vendor(000002) {BBN (was internal usage only, no longer used)}
set vendor(00000C) {Cisco}
set vendor(00000E) {Fujitsu}
set vendor(00000F) {NeXT}
set vendor(000010) {Hughes LAN Systems (formerly Sytek)}
set vendor(000011) {Tektronix}
set vendor(000015) {Datapoint Corporation }
set vendor(000018) {Webster Computer Corporation Appletalk/Ethernet Gateway}
set vendor(00001A) {AMD (?)}
set vendor(00001B) {Novell >> now Eagle Technology}
set vendor(00001D) {Cabletron}
set vendor(000020) {DIAB (Data Intdustrier AB)}
set vendor(000021) {SC&C}
set vendor(000022) {Visual Technology}
set vendor(000023) {ABB Automation AB, Dept. Q}
set vendor(000029) {IMC}
set vendor(00002A) {TRW}
set vendor(00003C) {Auspex}
set vendor(00003D) {AT&T}
set vendor(000044) {Castelle}
set vendor(000046) {ISC-Bunker Ramo, An Olivetti Company}
set vendor(000049) {Apricot Ltd.}
set vendor(00004B) {APT  A.P.T. Appletalk WAN router}
set vendor(00004F) {Logicraft 386-Ware P.C. Emulator}
set vendor(000051) {Hob Electronic Gmbh & Co. KG}
set vendor(000052) {ODS}
set vendor(000055) {AT&T}
set vendor(00005A) {SK (Schneider & Koch in Europe and Syskonnect outside of Europe)}
set vendor(00005A) {Xerox 806 (unregistered)}
set vendor(00005D) {RCE}
set vendor(00005E) {U.S. Department of Defense (IANA)}
set vendor(00005F) {Sumitomo (?)}
set vendor(000061) {Gateway Communications}
set vendor(000062) {Honeywell}
set vendor(000065) {Network General}
set vendor(000069) {Silicon Graphics(?)}
set vendor(00006B) {MIPS}
set vendor(00006E) {Artisoft, Inc.}
set vendor(000077) {Interphase  (Used in other systems, e.g. MIPS, Motorola)}
set vendor(000078) {Labtam Australia}
set vendor(000079) {Net Ware (?)}
set vendor(00007A) {Ardent}
set vendor(00007B) {Research Machines}
set vendor(00007D) {Cray Research Superservers,Inc (Also Harris (3M) (old))}
set vendor(00007F) {Linotronic}
set vendor(000080) {Dowty Network Services (Also shows as "Harris (3M) (new)" and/or "Imagen(?)" elsewhere)}
set vendor(000081) {Synoptics}
set vendor(000084) {Aquila (?), ADI Systems Inc.(?)}
set vendor(000086) {Gateway Communications Inc. (also Megahertz Corporation?)}
set vendor(000089) {Cayman Systems Gatorbox}
set vendor(00008A) {Datahouse Information Systems}
set vendor(00008E) {Jupiter(?), Solbourne(?)}
set vendor(000093) {Proteon}
set vendor(000094) {Asante MAC}
set vendor(000095) {Sony/Tektronix}
set vendor(000097) {Epoch}
set vendor(000098) {Cross Com}
set vendor(00009F) {Ameristar Technology}
set vendor(0000A0) {Sanyo Electronics}
set vendor(0000A2) {Wellfleet}
set vendor(0000A3) {Network Application Technology (NAT)}
set vendor(0000A4) {Acorn}
set vendor(0000A5) {Compatible Systems Corporation}
set vendor(0000A6) {Network General (internal assignment, not for products)}
set vendor(0000A7) {Network Computing Devices (NCD) X-terminals}
set vendor(0000A8) {Stratus Computer, Inc.}
set vendor(0000A9) {Network Systems}
set vendor(0000AA) {Xerox Xerox machines}
set vendor(0000AC) {Apollo}
set vendor(0000AF) {Nuclear Data Acquisition Interface Modules (AIM)}
set vendor(0000B0) {RND (RAD Network Devices)}
set vendor(0000B1) {Alpha Microsystems Inc.}
set vendor(0000B3) {CIMLinc}
set vendor(0000B4) {Edimax}
set vendor(0000B5) {Datability Terminal Servers}
set vendor(0000B7) {Dove Fastnet}
set vendor(0000BB) {TRI-DATA Systems Inc. Netway products, 3274 emulators}
set vendor(0000BC) {Allen-Bradley}
set vendor(0000C0) {Western Digital now SMC (Std. Microsystems Corp.)}
set vendor(0000C6) {HP Intelligent Networks Operation (formerly Eon Systems)}
set vendor(0000C8) {Altos}
set vendor(0000C9) {Emulex Terminal Servers}
set vendor(0000CC) {Densan Co., Ltd.}
set vendor(0000D0) {Develcon Electronics, Ltd.}
set vendor(0000D1) {Adaptec, Inc. "Nodem" product}
set vendor(0000D3) {Wang Labs}
set vendor(0000D4) {PureData}
set vendor(0000D7) {Dartmouth College (NED Router)}
set vendor(0000D8) {old Novell NE1000's ~<=1987 ? (maybe also 3Com?)}
set vendor(0000DD) {Gould}
set vendor(0000DE) {Unigraph}
set vendor(0000E2) {Acer Counterpoint}
set vendor(0000E3) {Integrated Micro Products Ltd}
set vendor(0000E6) {Aptor Produits De Comm Indust}
set vendor(0000E8) {Accton Technology Corporation}
set vendor(0000E9) {ISICAD, Inc.}
set vendor(0000ED) {April}
set vendor(0000EE) {Network Designers Limited(?)}
set vendor(0000EF) {Alantec}
set vendor(0000F0) {Samsung}
set vendor(0000F3) {Gandalf Data Ltd. - Canada}
set vendor(0000F4) {Allied Telesis, Inc.}
set vendor(0000F6) {A.M.C. (Applied Microsystems Corp.)}
set vendor(0000F8) {DEC (?)}
set vendor(0000FD) {High Level Hardware (Orion, UK)}
set vendor(000102) {BBN (Bolt Beranek and Newman, Inc.) internal usage (not registered)}
set vendor(000143) {IEEE 802}
set vendor(000163) {NDC  (National Datacomm Corporation)}
set vendor(000168) {W&G  (Wandel & Goltermann)}
set vendor(0001C8) {Thomas Conrad Corp.}
set vendor(000852) {Technically Elite Concepts}
set vendor(000855) {Fermilab}
set vendor(001700) {Kabel}
set vendor(004088) {Mobuis NuBus (Mac) combination video/EtherTalk}
set vendor(00400B) {Crescendo (?)}
set vendor(00400C) {General Micro Systems, Inc.}
set vendor(00400D) {LANNET Data Communications}
set vendor(004010) {Sonic Mac Ethernet interfaces}
set vendor(004014) {Comsoft Gmbh}
set vendor(004015) {Ascom (?)}
set vendor(00401F) {Colorgraph Ltd}
set vendor(004027) {Sigma (?)}
set vendor(00402A) {Canoga-Perkins}
set vendor(00402B) {TriGem}
set vendor(00402F) {XDI (?)}
set vendor(004030) {GK Computer}
set vendor(004033) {Addtron Technology Co., Ltd.}
set vendor(00403C) {Forks, Inc.}
set vendor(004041) {Fujikura Ltd.}
set vendor(00404C) {Hypertec Pty Ltd.}
set vendor(004050) {Ironics, Incorporated}
set vendor(00405B) {Funasset Limited}
set vendor(004066) {Hitachi Cable, Ltd.}
set vendor(004068) {Extended Systems}
set vendor(00406E) {Corollary, Inc.}
set vendor(004074) {Cable and Wireless}
set vendor(004076) {AMP Incorporated}
set vendor(00407F) {Agema Infrared Systems AB}
set vendor(00408C) {Axis Communications AB}
set vendor(00408E) {CXR/Digilog}
set vendor(004092) {ASP Computer Products, Inc.}
set vendor(004095) {Eagle Technologies}
set vendor(00409D) {DigiBoard Ethernet-ISDN bridges}
set vendor(00409E) {Concurrent Technologies  Ltd.}
set vendor(0040A6) {Cray Research Inc.}
set vendor(0040AE) {Delta Controls, Inc.}
set vendor(0040B4) {3COM K.K.}
set vendor(0040B6) {Computerm Corporation}
set vendor(0040C1) {Bizerba-Werke Wilheim Kraut}
set vendor(0040C2) {Applied Computing Devices}
set vendor(0040C3) {Fischer and Porter Co.}
set vendor(0040C5) {Micom Communications Corp.}
set vendor(0040C6) {Fibernet Research, Inc.}
set vendor(0040C8) {Milan Technology Corp.}
set vendor(0040D4) {Gage Talker Corp.}
set vendor(0040DF) {Digalog Systems, Inc.}
set vendor(0040E7) {Arnos Instruments & Computer}
set vendor(0040E9) {Accord Systems, Inc.}
set vendor(0040F1) {Chuo Electronics Co., Ltd.}
set vendor(0040F4) {Cameo Communications, Inc.}
set vendor(0040F9) {Combinet}
set vendor(0040FB) {Cascade Communications Corp.}
set vendor(00608C) {3Com (1990 onwards)}
set vendor(008004) {Antlow Computers, Ltd.}
set vendor(008005) {Cactus Computer Inc.}
set vendor(008006) {Compuadd Corporation}
set vendor(008007) {Dlog NC-Systeme}
set vendor(00800F) {SMC (Standard Microsystem Corp.)}
set vendor(008010) {Commodore}
set vendor(008017) {PFU}
set vendor(008019) {Dayna Communications "Etherprint" product}
set vendor(00801A) {Bell Atlantic}
set vendor(00801B) {Kodiak Technology}
set vendor(008021) {Newbridge Networks Corporation}
set vendor(008023) {Integrated Business Networks}
set vendor(008024) {Kalpana}
set vendor(008029) {Microdyne Corporation}
set vendor(00802D) {Xylogics, Inc. Annex terminal servers}
set vendor(00802E) {Plexcom, Inc.}
set vendor(008033) {Formation (?)}
set vendor(008034) {SMT-Goupil}
set vendor(008035) {Technology Works}
set vendor(008037) {Ericsson Business Comm.}
set vendor(008038) {Data Research & Applications}
set vendor(00803B) {APT Communications, Inc.}
set vendor(00803E) {Synernetics}
set vendor(00803F) {Hyundai Electronics}
set vendor(008042) {Force Computers}
set vendor(00804C) {Contec Co., Ltd.}
set vendor(00804D) {Cyclone Microsystems, Inc.}
set vendor(008051) {ADC Fibermux}
set vendor(008052) {Network Professor}
set vendor(00805B) {Condor Systems, Inc.}
set vendor(00805C) {Agilis(?)}
set vendor(008060) {Network Interface Corporation}
set vendor(008062) {Interface Co.}
set vendor(008069) {Computone Systems}
set vendor(00806A) {ERI (Empac Research Inc.)}
set vendor(00806C) {Cegelec Projects Ltd}
set vendor(00806D) {Century Systems Corp.}
set vendor(008074) {Fisher Controls}
set vendor(00807B) {Artel Communications Corp.}
set vendor(00807C) {FiberCom}
set vendor(008086) {Computer Generation Inc.}
set vendor(008087) {Okidata}
set vendor(00808A) {Summit (?)}
set vendor(00808B) {Dacoll Limited}
set vendor(00808C) {Frontier Software Development}
set vendor(008092) {Japan Computer Industry, Inc.}
set vendor(008096) {HDS (Human Designed Systems) X terminals}
set vendor(00809D) {Datacraft Manufactur'g Pty Ltd}
set vendor(00809F) {Alcatel Business Systems}
set vendor(0080A1) {Microtest}
set vendor(0080A3) {Lantronix}
set vendor(0080AD) {Telebit}
set vendor(0080AE) {Hughes Network Systems}
set vendor(0080AF) {Allumer Co., Ltd.}
set vendor(0080B2) {NET (Network Equipment Technologies)}
set vendor(0080C0) {Penril (?)}
set vendor(0080C2) {IEEE 802.1 Committee}
set vendor(0080C7) {Xircom, Inc.}
set vendor(0080C8) {D-Link (also Solectek Pocket Adapters)}
set vendor(0080C9) {Alberta Microelectronic Centre}
set vendor(0080CE) {Broadcast Television Systems}
set vendor(0080D0) {Computer Products International}
set vendor(0080D3) {Shiva Appletalk-Ethernet interface}
set vendor(0080D4) {Chase Limited}
set vendor(0080D6) {Apple Mac Portable(?)}
set vendor(0080D7) {Fantum Electronics}
set vendor(0080D8) {Network Peripherals}
set vendor(0080DA) {Bruel & Kjaer}
set vendor(0080E3) {Coral (?)}
set vendor(0080F1) {Opus}
set vendor(0080F7) {Zenith Communications Products}
set vendor(0080FB) {BVM Limited}
set vendor(00AA00) {Intel}
set vendor(00B0D0) {Computer Products International}
set vendor(00C001) {Diatek Patient Managment}
set vendor(00C004) {Japan Business Computer Co.Ltd}
set vendor(00C016) {Electronic Theatre Controls}
set vendor(00C01A) {Corometrics Medical Systems}
set vendor(00C01C) {Interlink Communications Ltd.}
set vendor(00C01D) {Grand Junction Networks, Inc.}
set vendor(00C020) {Arco Electronic, Control Ltd.}
set vendor(00C024) {Eden Sistemas De Computacao SA}
set vendor(00C025) {Dataproducts Corporation}
set vendor(00C027) {Cipher Systems, Inc.}
set vendor(00C028) {Jasco Corporation}
set vendor(00C02B) {Gerloff Gesellschaft Fur}
set vendor(00C02C) {Centrum Communications, Inc.}
set vendor(00C02D) {Fuji Photo Film Co., Ltd.}
set vendor(00C030) {Integrated Engineering B. V.}
set vendor(00C031) {Design Research Systems, Inc.}
set vendor(00C032) {I-Cubed Limited}
set vendor(00C034) {Dale Computer Corporation}
set vendor(00C040) {ECCI}
set vendor(00C042) {Datalux Corp.}
set vendor(00C044) {Emcom Corporation}
set vendor(00C048) {Bay Technical Associates}
set vendor(00C04E) {Comtrol Corporation}
set vendor(00C051) {Advanced Integration Research}
set vendor(00C05C) {Elonex PLC}
set vendor(00C066) {Docupoint, Inc.}
set vendor(00C06D) {Boca Research, Inc.}
set vendor(00C071) {Areanex Communications, Inc.}
set vendor(00C078) {Computer Systems Engineering}
set vendor(00C091) {Jabil Circuit, Inc.}
set vendor(00C093) {Alta Research Corp.}
set vendor(00C097) {Archipel SA}
set vendor(00C098) {Chuntex Electronic Co., Ltd.}
set vendor(00C09D) {Distributed Systems Int'l, Inc}
set vendor(00C0A0) {Advance Micro Research, Inc.}
set vendor(00C0A2) {Intermedium A/S}
set vendor(00C0A8) {GVC Corporation}
set vendor(00C0AC) {Gambit Computer Communications}
set vendor(00C0AD) {Computer Communication Systems}
set vendor(00C0B0) {GCC Technologies,Inc.}
set vendor(00C0B8) {Fraser's Hill Ltd.}
set vendor(00C0BD) {Inex Technologies, Inc.}
set vendor(00C0BE) {Alcatel - Sel}
set vendor(00C0C2) {Infinite Networks Ltd.}
set vendor(00C0C4) {Computer Operational}
set vendor(00C0CA) {Alfa, Inc.}
set vendor(00C0CB) {Control Technology Corporation}
set vendor(00C0D1) {Comtree Technology Corporation}
set vendor(00C0D6) {J1 Systems, Inc.}
set vendor(00C0DC) {EOS Technologies, Inc.}
set vendor(00C0E2) {Calcomp, Inc.}
set vendor(00C0E7) {Fiberdata AB}
set vendor(00C0EA) {Array Technology Ltd.}
set vendor(00C0EC) {Dauphin Technology}
set vendor(00C0EF) {Abit Corporation}
set vendor(00C0F4) {Interlink System Co., Ltd.}
set vendor(00C0F6) {Celan Technology Inc.}
set vendor(00C0F7) {Engage Communication, Inc.}
set vendor(00C0F8) {About Computing Inc.}
set vendor(00C0FB) {Advanced Technology Labs}
set vendor(00DD00) {Ungermann-Bass              IBM RT}
set vendor(00DD01) {Ungermann-Bass}
set vendor(00DD08) {Ungermann-Bass}
set vendor(020406) {BBN (Bolt Beranek and Newman, Inc.) internal usage (not registered)}
set vendor(020701) {MICOM/Interlan DEC (UNIBUS or QBUS), Apollo, Cisco}
set vendor(026060) {3Com}
set vendor(026086) {Satelcom MegaPac (UK)}
set vendor(02608C) {3Com IBM PC; Imagen; Valid; Cisco; Macintosh}
set vendor(02CF1F) {CMC Masscomp; Silicon Graphics; Prime EXL}
set vendor(02E6D3) {BTI (Bus-Tech, Inc.) IBM Mainframes}
set vendor(080001) {Computer Vision}
set vendor(080002) {3Com (formerly Bridge)}
set vendor(080003) {ACC (Advanced Computer Communications)}
set vendor(080005) {Symbolics Symbolics LISP machines}
set vendor(080006) {Siemens Nixdorf PC clone}
set vendor(080007) {Apple}
set vendor(080008) {BBN (Bolt Beranek and Newman, Inc.)}
set vendor(080009) {Hewlett-Packard}
set vendor(08000A) {Nestar Systems}
set vendor(08000B) {Unisys}
set vendor(08000D) {ICL (International Computers, Ltd.)}
set vendor(08000E) {NCR/AT&T}
set vendor(08000F) {SMC (Standard Microsystems Corp.)}
set vendor(080010) {AT&T (misrepresentation of 800010?)}
set vendor(080011) {Tektronix, Inc.}
set vendor(080014) {Excelan BBN Butterfly, Masscomp, Silicon Graphics}
set vendor(080017) {NSC (Network System Corp.)}
set vendor(08001A) {Tiara? (used to have Data General)}
set vendor(08001B) {Data General}
set vendor(08001E) {Apollo}
set vendor(08001F) {Sharp}
set vendor(080020) {Sun}
set vendor(080022) {NBI (Nothing But Initials)}
set vendor(080023) {Matsushita Denso}
set vendor(080025) {CDC}
set vendor(080026) {Norsk Data (Nord)}
set vendor(080027) {PCS Computer Systems GmbH}
set vendor(080028) {TI Explorer}
set vendor(08002B) {DEC}
set vendor(08002E) {Metaphor}
set vendor(08002F) {Prime Computer Prime 50-Series LHC300}
set vendor(080030) {CERN}
set vendor(080036) {Intergraph CAE stations}
set vendor(080037) {Fujitsu-Xerox}
set vendor(080038) {Bull}
set vendor(080039) {Spider Systems}
set vendor(08003B) {Torus Systems}
set vendor(08003E) {Motorola VME bus processor modules}
set vendor(080041) {DCA (Digital Comm. Assoc.)}
set vendor(080044) {DSI (DAVID Systems, Inc.)}
set vendor(080045) {???? (maybe Xylogics, but they claim not to know this number)}
set vendor(080046) {Sony}
set vendor(080047) {Sequent}
set vendor(080048) {Eurotherm Gauging Systems}
set vendor(080049) {Univation}
set vendor(08004C) {Encore}
set vendor(08004E) {BICC}
set vendor(080051) {Experdata}
set vendor(080056) {Stanford University}
set vendor(080057) {Evans & Sutherland (?)}
set vendor(080058) {??? DECsystem-20}
set vendor(08005A) {IBM}
set vendor(080067) {Comdesign}
set vendor(080068) {Ridge}
set vendor(080069) {Silicon Graphics}
set vendor(08006A) {ATTst (?)}
set vendor(08006E) {Excelan}
set vendor(080070) {Mitsubishi}
set vendor(080074) {Casio}
set vendor(080075) {DDE (Danish Data Elektronik A/S)}
set vendor(080077) {TSL (now Retix)}
set vendor(080079) {Silicon Graphics}
set vendor(08007C) {Vitalink TransLAN III}
set vendor(080080) {XIOS}
set vendor(080081) {Crosfield Electronics}
set vendor(080083) {Seiko Denshi }
set vendor(080086) {Imagen/QMS}
set vendor(080087) {Xyplex terminal servers}
set vendor(080089) {Kinetics AppleTalk-Ethernet interface}
set vendor(08008B) {Pyramid}
set vendor(08008D) {XyVision XyVision machines}
set vendor(08008E) {Tandem / Solbourne Computer ?}
set vendor(08008F) {Chipcom Corp.}
set vendor(080090) {Retix, Inc. Bridges}
set vendor(10005A) {IBM}
set vendor(1000D4) {DEC}
set vendor(1000E0) {Apple A/UX (modified addresses for licensing)}
set vendor(400003) {Net Ware (?)}
set vendor(444649) {DFI (Diamond Flower Industries)}
set vendor(475443) {GTC (Not registered!) (This number is a multicast!)}
set vendor(484453) {HDS ???}
set vendor(800010) {AT&T (misrepresented as 080010? One source claims this is correct)}
set vendor(80AD00) {CNET Technology Inc.}
set vendor(AA0000) {DEC obsolete}
set vendor(AA0001) {DEC obsolete}
set vendor(AA0002) {DEC obsolete}
set vendor(AA0003) {DEC Global physical address for some DEC machines}
set vendor(AA0004) {DEC Local logical address for systems running DECNET}
set vendor(C00000) {Western Digital (may be reversed 00 00 C0?)}
set vendor(EC1000) {Enance Source Co., Ltd. PC clones(?)}
