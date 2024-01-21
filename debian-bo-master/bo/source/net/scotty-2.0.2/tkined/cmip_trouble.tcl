#! /usr/local/bin/scotty -inf
##
## Simple CMIP-Troubleshooting utilities for tkined.
##
## Copyright (c) 1994, 1995
##
## M. Kernchen
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

LoadDefaults cmip
## none at this time maybe the agent???

##
## =========================================================================
## ========== C M I P   related subroutines ================================
## =========================================================================
##
##  CmipInit { toolname }
##  CmipParameter
##  CmipOpen  { host }
##  CmipGet { s class inst attrlist }			eventuell noch mehr
##  CmipGetBulk { s class inst varlist varname body }	hier auch???
##  CmipClose { s }
##  CmipShowAttrs { host class inst }			und hier???
##  CmipDump { host class inst scope filter attrs }	attrlist -atomic ???
##
## =========================================================================
##


##
## Initialize global variables that are used by these cmip procs.
##

proc CmipInit { toolname } {

    global tool_name

    set tool_name $toolname

    if {[lsearch [info commands] cmip] < 0} {
        ined acknowledge "Sorry, this version of scotty has no cmip support."
        exit
    }
}

##
## Set the parameters () for cmip requests. at this time set nothing
##

proc CmipParameter {} {
## none at this time

}

##
## Start a cmip session by connecting to a remote agent (at this time OIM-SMA)
##

proc CmipOpen { host } {

    set isohost [lindex [split $host "."] 0]
    return [cmip connect OIM-SMA $isohost]
## hier noch fest verdrahtet OIM-SMA ???
}

##
## Release a cmip association and delete its cmip handle.
## Catch all errors. Why bother about it.
##

proc CmipClose { s } {
    catch {$s release}
}

##
## Send a cmip get request and return the retrieved values.
##

proc CmipGet { s class inst attrlist } {
    set result ""
    foreach attresult \
	    [lindex [lindex [$s get $class $inst -attributes $attrlist] 0] 4] {
        lappend result [lindex $attresult 1]
    }
    return $result
}

##
## Send a cmip get request and set the variable varname to the
## retrieved values. Evaluate the body foreach object that is in
## our subtree, i.e. wholesubtree ???
##

## das ganze hier ist noch nicht ausgereift!!!

proc CmipGetBulk { s class inst varlist varname body } {
    upvar $varname result
    writeln "** $s get $class $inst -attributes $varlist -scope wholesubtree"
    if {[catch {$s get $class $inst -attributes $varlist -scope wholesubtree} \
	    reply]} {
    } else {
	foreach item $reply {
	    if {[lindex $item 3] != "getListError"} {
		set result ""
		foreach attresult [lindex $item 4] {
		    lappend result [lindex $attresult 1]
		}
		uplevel 1 $body
	    }
	}
    }
}

##
## Show all scalar variables that are successors of path.
##

proc CmipShowAttrs {host class inst} {

    if {[catch {nslook $host} ip]==0} {
	set ip [lindex $ip 0]
    } else {
	set ip ""
    }
    set txt "Attributes in class $class, instance $inst of $host \[$ip\]:\n"
    if {[catch {CmipOpen $host} s]} {
	append txt "$s\n"
    } else {
	if {[catch {$s get $class $inst} result]} {
	    append txt "$result\n"
	} else {
	    foreach attresult [lindex [lindex $result 0] 4] {
		append txt \
			[format "  %-24s %s\n" \
			[lindex $attresult 0] [lindex $attresult 1]]
	    }
	}
	CmipClose $s
    }
    writeln $txt
}

##
## Dump a hierarchy of the MIB.
##

proc CmipDump {host class inst scope filter attrs} {

    if {[catch {nslook $host} ip]==0} {
	set ip [lindex $ip 0]
    } else {
	set ip ""
    }
    if {$inst == ""} {
	set inst sysName=[lindex [split $host "."] 0]
    } else {
	set inst sysName=[lindex [split $host "."] 0]@$inst
    }
    if {$filter == ""} {
	set filter "(NULL)"
    }
##    writeln "$host \[$ip\]:"
    if {[catch {CmipOpen $host} s]} {
	writeln "$s\n"
	return
    }
    if {$attrs == "all"} {
	if {[catch {$s get $class $inst -scope $scope -filter $filter} var]} {
	    writeln "$var\n"
	    CmipClose $s
	    return
	}
    } else {
	if {[catch {
	    $s get $class $inst -scope $scope -filter $filter -attributes $attrs
	} var]} {
	    writeln "$var\n"
	    CmipClose $s
	    return
	}
    }
    foreach elem $var {
	writeln "[lindex $elem 0], [lindex $elem 1] :"
	foreach attresult [lindex $elem 4] {
	    writeln "  [lindex $attresult 0] = [lindex $attresult 1]"
	}
    }
    writeln

    CmipClose $s
}

CmipInit CMIP-Trouble

##
## List all scalars of a group for the node objects in list.
##

proc ShowAttrs {list class inst} {
    ForeachIpNode id ip host $list {
	if {$inst == ""} {
	    CmipShowAttrs $host $class sysName=[lindex [split $host "."] 0]
	} else {
	    CmipShowAttrs $host $class \
		    sysName=[lindex [split $host "."] 0]@$inst
	}
    }
}

##
## Try to figure out if the devices responds to cmip requests.
##

proc "CMIP Devices" {list} {
    set first 1

    ForeachIpNode id ip host $list {
	if {![catch {CmipOpen $host} s]} {
	    if {$first} {
		writeln "CMIP devices:"
		set first 0
	    }
	    write   [format "%-32s \[" $host ]
	    write   $ip "IpFlash $ip"
	    writeln "\]"
	    CmipClose $s
	}
    }
    writeln
}

##
## Show the system information of MIB II.
##

proc "System Information" {list} {
    ShowAttrs $list system ""
}

##
## Get the list of interfaces and their status.
##

proc "Status" {list} {
    ForeachIpNode id ip host $list {
	set txt "Interface status of $host \[$ip\]:\n"
	append txt "ifIndex ifDescr            ifAdminStatus ifOperStatus (ifType)\n"
	if {![catch {CmipOpen $host} s]} {
	    CmipGetBulk $s ifTable \
		sysName=[lindex [split $host "."] 0]@ifId=\"\"@ifTableId=\"\" {
		ifIndex
		ifDescr
		ifAdminStatus
		ifOperStatus
		ifType
	    } x {
		set ifIndex [lindex $x 0]
		set ifDescr [lindex [lindex [lindex $x 1] 0] 0]
		set ifAdmin [lindex $x 2]
		set ifOper  [lindex $x 3]
		set ifType  [lindex $x 4]
		append txt [format "%5s   %-16s %12s %12s    (%s)\n" \
			$ifIndex $ifDescr $ifAdmin $ifOper $ifType]
	    }
	    CmipClose $s
	}
	writeln $txt
    }
}

##
## Get the list of interfaces and their status.
##

proc "Parameter" {list} {
    ForeachIpNode id ip host $list {
	set txt "Interface parameter of $host \[$ip\]:\n"
	append txt "ifIndex ifDescr              ifSpeed      ifMtu   ifPhysAddress\n"
	if {![catch {CmipOpen $host} s]} {
	    CmipGetBulk $s ifTable \
		sysName=[lindex [split $host "."] 0]@ifId=\"\"@ifTableId=\"\" {
		ifIndex
		ifDescr
		ifSpeed
		ifMtu
		ifPhysAddress
	    } x {
		set ifIndex [lindex $x 0]
		set ifDescr [lindex [lindex [lindex $x 1] 0] 0]
		set ifSpeed [lindex $x 2]
		set ifMtu   [lindex $x 3]
		set ifPhys  [lindex $x 4]
		append txt [format " %5s  %-16s %11s %10s   %s\n" \
			$ifIndex $ifDescr $ifSpeed $ifMtu $ifPhys]
	    }
	    CmipClose $s
	}
	writeln $txt
    }
}

##
## We should check which interfaces are up and running.
##

proc "Usage Statistics" {list} {
    ForeachIpNode id ip host $list {
	set txt "Interface usage of $host \[$ip\]:\n"
        append txt "ifIndex ifDescr           ifInOctets  ifInUcastPkts  ifOutOctets ifOutUcastPkts\n"
	if {![catch {CmipOpen $host} s]} {
	    CmipGetBulk $s ifTable \
		sysName=[lindex [split $host "."] 0]@ifId=\"\"@ifTableId=\"\" {
		ifIndex
		ifDescr
		ifInOctets
		ifInUcastPkts
		ifOutOctets
		ifOutUcastPkts
	    } x {
		set ifIndex   [lindex $x 0]
		set ifDescr   [lindex [lindex [lindex $x 1] 0] 0]
		set ifInOct   [lindex $x 2]
		set ifInUOct  [lindex $x 3]
		set ifOutOct  [lindex $x 4]
		set ifOutUOct [lindex $x 5]
		append txt [format " %5s  %-15s %12s %14s %12s %14s\n" \
			$ifIndex $ifDescr $ifInOct $ifInUOct \
			$ifOutOct $ifOutUOct]
	    }
	    CmipClose $s
	}
	writeln $txt
    }
}


proc "Error Statistics" {list} {
    ForeachIpNode id ip host $list {
	set txt "Interface errors of $host \[$ip\]:\n"
	append txt "ifIndex ifDescr           ifInErrors   ifInDiscards  ifOutErrors  ifOutDiscards\n"
	if {![catch {CmipOpen $host} s]} {
	    CmipGetBulk $s ifTable \
		sysName=[lindex [split $host "."] 0]@ifId=\"\"@ifTableId=\"\" {
		ifIndex
		ifDescr
		ifInErrors
		ifInDiscards
		ifOutErrors
		ifOutDiscards
	    } x {
		set ifIndex   [lindex $x 0]
		set ifDescr   [lindex [lindex [lindex $x 1] 0] 0]
		set ifInErr   [lindex $x 2]
		set ifInDisc  [lindex $x 3]
		set ifOutErr  [lindex $x 4]
		set ifOutDisc [lindex $x 5]
		append txt [format " %5s  %-15s %12s %14s %12s %14s\n" \
			$ifIndex $ifDescr $ifInErr $ifInDisc \
			$ifOutErr $ifOutDisc]
	    }
	    CmipClose $s
	}
	writeln $txt
    }
}

##
## Display relative error statistics.
##

proc "Quality" {list} {
    ForeachIpNode id ip host $list {
	set txt "Interface quality of $host \[$ip\]:\n"
	append txt "(may be invalid if counter have wrapped)\n"
	append txt "ifIndex ifDescr        Error Rate   Discard Rate\n"
	if {![catch {CmipOpen $host} s]} {
	    CmipGetBulk $s ifTable \
		sysName=[lindex [split $host "."] 0]@ifId=\"\"@ifTableId=\"\" {
		ifIndex
		ifDescr
		ifInUcastPkts
		ifInNUcastPkts
		ifOutUcastPkts
		ifOutNUcastPkts
		ifInErrors
		ifOutErrors
		ifInDiscards
		ifOutDiscards
	    } x {
		set ifIndex   [lindex $x 0]
		set ifDescr   [lindex [lindex [lindex $x 1] 0] 0]
		set in  [expr {[lindex $x 2] +  [lindex $x 3]}]
		set out [expr {[lindex $x 4] +  [lindex $x 5]}]
		set pkts [expr {$in + $out}]
		if {$in == 0} {
		    set err 0
		    set dis 0
		} else {
		    set err [expr {[lindex $x 6] + [lindex $x 7]}]
		    set err [expr {100 * double($err) / $pkts}]
		}
		if {$out == 0} {
		    set err 0
		    set dis 0
		} else {
		    set dis [expr {[lindex $x 8] + [lindex $x 9]}]
		    set dis [expr {100 * double($dis) / $pkts}]
		}
		append txt \
			[format "%5s   %-15s  %5.2f %%        %5.2f %%\n" \
			$ifIndex $ifDescr $err $dis]
	    }
	    CmipClose $s
	}
	writeln $txt
    }
}

##
## Show the IP Attributes.
##

proc "IP Statistics" {list} {
    ShowAttrs $list ip ipId=\"\"
}

##
## Show the routing tables of the agents.
##

proc "IP Routing Table" {list} {
    ForeachIpNode id ip host $list {
	set txt "Routing Table of $host \[$ip\]:\n"
	append txt "ipRouteDest      ipRouteNextHop   ipRouteMask      IfIndex    Type      Proto\n"
	if {![catch {CmipOpen $host} s]} {
	    CmipGetBulk $s ipRoutingTable \
	sysName=[lindex [split $host "."] 0]@ipId=\"\"@ipRoutingTableId=\"\" {
		ipRouteDest
		ipRouteNextHop
		ipRouteMask
		ipRouteIfIndex
		ipRouteType
		ipRouteProto
	    } x {
		set ipRouteDest    [lindex $x 0]
		set ipRouteNext    [lindex $x 1]
		set ipRouteMask    [lindex $x 2]
		set ipRouteIfIndex [lindex $x 3]
		set ipRouteType    [lindex $x 4]
		set ipRouteProto   [lindex $x 5]
		append txt [format "%-16s %-16s %-16s %5d %8s %8s\n" \
			$ipRouteDest $ipRouteNext $ipRouteMask \
			$ipRouteIfIndex $ipRouteType $ipRouteProto]
	    }
	    CmipClose $s
	}
	writeln $txt
    }
}

##
## Show the routing tables of the agents.
##

## ADD missing ZEROs: 8:0:20:e:3b:6 -> 08:00:20:0e:3b:06
proc "addzero" {elem} {
  return [eval "format {%02s:%02s:%02s:%02s:%02s:%02s} [join [split $elem :]]"]
}

proc "IP ARP Table" {list} {
    global vendor
    ForeachIpNode id ip host $list {
	set txt "ARP Table of $host \[$ip\]:\n"
	append txt "ifIndex PhysAddress        NetAddress         Type       Vendor\n"
	if {![catch {CmipOpen $host} s]} {
	    set done 0
	    CmipGetBulk $s ipNetToMediaTable \
    sysName=[lindex [split $host "."] 0]@ipId=\"\"@ipNetToMediaTableId=\"\" {
		ipNetToMediaIfIndex
		ipNetToMediaPhysAddress
		ipNetToMediaNetAddress
		ipNetToMediaType
	    } x {
		set done 1
		set l [join [lrange [split [addzero [lindex $x 1]] :] 0 2] {}]
		set l [string toupper $l]
		if {[info exists vendor($l)]} {
		    set vend $vendor($l)
		} else {
		    set vend ""
		}
		set aa "format { %5s  %-18s %-18s %-10s} [join $x]"
		append txt "[eval $aa] $vend\n"
	    }
	    if {!$done} {
		CmipGetBulk $s atTable \
			sysName=[lindex [split $host "."] 0]@atTableId=\"\" {
		    atIfIndex
		    atPhysAddress
		    atNetAddress
		} x {
		    set l [join [lrange [split [addzero [lindex $x 1]] :] 0 2] {}]
		    set l [string toupper $l]
		    if {[info exists vendor($l)]} {
			set vend $vendor($l)
		    } else {
			set vend ""
		    }
		    set aa "format { %5s  %-18s %-18s } [join $x]"
		    append txt "[eval $aa] $vend\n"
		}
	    }
	    CmipClose $s
	}
	writeln $txt
    }
}

##
## Show the TCP Attributes.
##

proc "TCP Statistics" {list} {
    ShowAttrs $list tcp tcpId=\"\"
}

##
## Show the tcp connection tables of the agents.
##

proc "TCP Connections" {list} {
    ForeachIpNode id ip host $list {
	if {[catch {CmipOpen $host} s]} {
	    set txt "TCP Connections of $host \[$ip\]:\n"
	    append txt "State            LocalAddress LocalPort RemoteAddress RemotePort\n"
	} else {
	    set percent -1
	    if {![catch {CmipGet $s tcp \
		    sysName=[lindex [split $host "."] 0]@tcpId=\"\" \
		    {tcpMaxConn tcpCurrEstab}} res]} {
		set tcpMaxConn   [lindex $res 0]
		set tcpCurrEstab [lindex $res 1]
		if {$tcpMaxConn > 0} {
		    set percent [expr {$tcpCurrEstab/double($tcpMaxConn)*100}]
		}
	    }
	    
	    if {$percent > 0} {
		set txt "TCP Connections of $host \[$ip\] ($percent% established):\n"
	    } else {
		set txt "TCP Connections of $host \[$ip\]:\n"
	    }
	    append txt "State            LocalAddress LocalPort RemoteAddress RemotePort\n"
	    CmipGetBulk $s \
		    tcp sysName=[lindex [split $host "."] 0]@tcpId=\"\" {
		tcpConnState
		tcpConnLocalAddress
		tcpConnLocalPort
		tcpConnRemAddress
		tcpConnRemPort
	    } x {
		set tcpConnState        [lindex $x 0]
		set tcpConnLocalAddress [lindex $x 1]
		set tcpConnLocalPort    [lindex $x 2]
		set tcpConnRemAddress   [lindex $x 3]
		set tcpConnRemPort      [lindex $x 4]
		append txt [format "%-12s %16s %6s %16s %6s\n" $tcpConnState \
			$tcpConnLocalAddress $tcpConnLocalPort \
			$tcpConnRemAddress $tcpConnRemPort]
	    }
	    CmipClose $s
	}
	writeln $txt
    }
}

##
## Show the UDP Attributes.
##

proc "UDP Statistics" {list} {
    ShowAttrs $list udp udpId=\"\"
}

##
## Show the udp listener table of the agents.
##

proc "UDP Listener" {list} {
    ForeachIpNode id ip host $list {
	set txt "UDP Listener of $host \[$ip\]:\n"
	append txt "  LocalAddress       LocalPort\n"
	if {![catch {CmipOpen $host} s]} {
	    CmipGetBulk $s \
		    udp sysName=[lindex [split $host "."] 0]@udpId=\"\" {
		udpLocalAddress
		udpLocalPort
	    } x {
		set udpLocalAddress	[lindex $x 0]
		set udpLocalPort    [lindex $x 1]
		append txt [format "  %-16s    %6s\n" \
			$udpLocalAddress $udpLocalPort]
	    }
	    CmipClose $s
	}
	writeln $txt
    }
}

##
## Show the SNMP Attributes.
##

proc "SNMP Statistics" {list} {
## none in cmip
}

##
## Show the ICMP Attributes.
##

proc "ICMP Statistics" {list} {
    ShowAttrs $list icmp icmpId=\"\"
}

##
## Dump a complete hierarchy. The user may choose the hierarchy
## before we start our action. We store the last selected hierarchy
## in a static variable called dump_mib_tree_path.
##

proc "Dump MIB Tree" {list} {

    static dump_mib_tree_class
    static dump_mib_tree_inst
    static dump_mib_tree_scope
    static dump_mib_tree_filter
    static dump_mib_tree_attrs

    if {![info exists dump_mib_tree_class]} {
        set dump_mib_tree_class "system"
    }
    if {![info exists dump_mib_tree_inst]} {
        set dump_mib_tree_inst ""
    }
    if {![info exists dump_mib_tree_scope]} {
        set dump_mib_tree_scope "wholeSubtree"
    }
    if {![info exists dump_mib_tree_filter]} {
        set dump_mib_tree_filter ""
    }
    if {![info exists dump_mib_tree_attrs]} {
        set dump_mib_tree_attrs "all"
    }

    set result [ined request "Dump MIB Tree:" [list \
	    [list "MO class  :" $dump_mib_tree_class] \
	    [list "MO inst   :" $dump_mib_tree_inst] \
	    [list "scope     :" $dump_mib_tree_scope] \
	    [list "filter    :" $dump_mib_tree_filter] \
	    [list "attributes:" $dump_mib_tree_attrs] \
	    ] \
	    [list dump cancel] ]

    if {[lindex $result 0]== "cancel"} return
    if {[lindex $result 1] == ""} {
	ined acknowledge "missing MIB class"
	return
    }

    set dump_mib_tree_class [lindex $result 1]
    set dump_mib_tree_inst [lindex $result 2]
    set dump_mib_tree_scope [lindex $result 3]
    set dump_mib_tree_filter [lindex $result 4]
    set dump_mib_tree_attrs [lindex $result 5]

    ForeachIpNode id ip host $list {
	writeln "MIB Tree Dump for $host \[$ip\] "
	writeln "starting at $dump_mib_tree_class $dump_mib_tree_inst"
	CmipDump $host $dump_mib_tree_class $dump_mib_tree_inst \
		$dump_mib_tree_scope $dump_mib_tree_filter $dump_mib_tree_attrs
	writeln
    }
}

##
## Set the parameters (community, timeout, retry) for snmp requests.
##

proc "Set Parameter" {list} {
    CmipParameter
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

proc "Help CMIP-Trouble" {list} {
    ined browse "Help about CMIP-Trouble" {
	"CMIP Devices:" 
	"    Test which of the selected nodes respond to CMIP requests." 
	"" 
	"System Information:" 
	"    Display the information of the system group." 
	"" 
	"Interface -> Status:" 
	"    Display status information of the interfaces." 
	"" 
	"Interface -> Parameter:" 
	"    Display interface parameter like speed and MTU." 
	"" 
	"Interface -> Usage Statistics:" 
	"    Display interface statistics." 
	"" 
	"Interface -> Error Statistics:" 
	"    Display interface statistics." 
	"" 
	"Interface -> Quality:" 
	"    Show the error and discard rate per received packed for" 
	"    each interface. The output is only valid if your counters" 
	"    have not wrapped!" 
	"" 
	"" 
	"IP -> IP Statistics" 
	"    Displays some statistics and parameter of the IP layer." 
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
	"Dump MIB Tree:" 
	"    Walk through the MIB tree and print the attribute values." 
	"" 
    }
##???	"SNMP Statistics:" 
##???	"    Statistics and parameter of the SNMP layer." 
##???	"" 
##???	"Set Parameter:" 
##???	"    This dialog allows you to set SNMP parameters like retries, " 
##???	"    timeouts, community name and port number. " 
##???	"" 
##???	"Show Defaults:" 
##???	"    Show the defaults that may be defined in the tkined.defaults" 
##???	"    files. This script makes use of definitions in the form" 
##???	"" 
##???	"        snmp.community:      <string>" 
##???	"        snmp.port:           <number>" 
##???	"        snmp.retries:        <number>" 
##???	"        snmp.timeout:        <number>" 
##???	"" 
##???	"        snmp.community.<ip>: <string>" 
##???	"        snmp.port.<ip>:      <number>" 
##???	"        snmp.retries.<ip>:   <number>" 
##???	"        snmp.timeout.<ip>:   <number>" 
##???	"" 
##???	"    where <ip> is an IP address in dot notation." 
}

##
## Delete the menus created by this interpreter.
##

proc "Delete CMIP-Trouble" {list} {
    global menus
    foreach id $menus { ined delete $id }
    exit
}

set menus [ ined create MENU "CMIP-Trouble" \
    "CMIP Devices" "" \
    "System Information" "" \
    "Interfaces:Status" \
    "Interfaces:Parameter" \
    "Interfaces:Usage Statistics" \
    "Interfaces:Error Statistics" \
    "Interfaces:Quality" \
    "IP:IP Statistics" \
    "IP:IP Routing Table" \
    "IP:IP ARP Table" \
    "TCP:TCP Statistics" \
    "TCP:TCP Connections" \
    "UDP:UDP Statistics" \
    "UDP:UDP Listener" "" \
    "ICMP Statistics" "" \
    "Dump MIB Tree" "" \
    "Help CMIP-Trouble" "Delete CMIP-Trouble" ]
##???    "SNMP Statistics" "" \
##???    "Set Parameter" "Show Defaults" "" \

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
