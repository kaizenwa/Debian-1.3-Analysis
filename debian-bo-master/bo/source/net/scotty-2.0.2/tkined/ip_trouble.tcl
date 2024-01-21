#! /usr/local/bin/scotty -inf
##
## Simple Troubleshooting interpreter for [TK]INED.
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

LoadDefaults icmp

IpInit IP-Trouble

##
## Send a icmp echo request to all addresses on a given network.
##

proc netping {network} {
    if {[regexp "^\[0-9\]+\.\[0-9\]+\.\[0-9\]+$" $network]>0} {
	set hosts ""
	for {set a4 1} {$a4<255} {incr a4} {
	    lappend hosts $network.$a4
	}
	if {[catch {icmp -timeout 15 echo $hosts} res]} {
	    set res ""
	}
	return $res
    }
    if {[regexp "^\[0-9\]+\.\[0-9\]+$" $network]>0} {
	set result ""
	for {set a3 0} {$a3<256} {incr a3} {
	    lappend result [netping $network.$a3]
	}
	return "[join $result]"
    }
}

##
## IP Troubleshooter Tool for INED
##

proc Ping {list} {

    set nodes ""
    set networks ""
    foreach comp $list {
	set type [ined type $comp]
        if {$type=="NODE"}    { lappend nodes $comp }
	if {$type=="NETWORK"} { lappend networks $comp }
    }

    if {[llength $nodes]>0} {
	foreach comp $nodes {
	    set host [lindex [ined name $comp] 0]
	    set ip [GetIpAddress $comp]
	    if {$ip==""} {
		ined acknowledge "Can not lookup IP Address for $host."
		continue
	    }
	    if {[catch {icmp -size 0 -timeout 5 echo $ip} rtt]} {
		writeln "Could not send icmp packet: $rtt"
		writeln
		return
	    }
	    set rtt [lindex [join $rtt] 1]
	    if {$rtt >= 0} {
		write   "Round trip time for $host \["
		write   $ip "IpFlash $ip"
		writeln "\]: $rtt ms"
	    } else {
		write   "$host \["
		write   $ip "IpFlash $ip"
		writeln "\] not reachable."
	    }
	}
	writeln
    }

    if {[llength $networks]>0} {
	foreach comp $networks {
	    set net [ined name $comp]
	    set ip [ined address $comp]
	    if {![regexp "^\[0-9\]+\.\[0-9\]+\.\[0-9\]+$" $ip]} {
		ined acknowledge "Can not lookup IP Address for $net."
		continue
	    }
	    writeln "Network ping for $net \[$ip\]:"
	    foreach pr [netping $ip] {
		set pr_ip [lindex $pr 0]
		set pr_time [lindex $pr 1]
		if {$pr_time>=0} {
		    if [catch {nslook $pr_ip} pr_host] {
			set pr_host $pr_ip
		    }
		    write   "  Round trip time for [lindex $pr_host 0] \["
		    write   $pr_ip "IpFlash $pr_ip"
		    writeln "\]: $pr_time ms"
		}
	    }
	    writeln
	}
    }
}

##
## Ping a host with a series of packets with varying packet sizes.
##

proc "Multi Ping" {list} {
    static increment
    static maxsize
    
    if ![info exists increment] { set increment 128 }
    if ![info exists maxsize]   { set maxsize 2048 }
    
    set result [ined request "Send icmp Packets with varying sizes." \
	    [list [list "Increment \[bytes\]:" $increment scale 1 512] \
	    [list "Max. packet size \[bytes\]:" $maxsize scale 64 2048] \
	    ] [list start cancel] ]
    
    if {[lindex $result 0] == "cancel"} return
    
    set increment [lindex $result 1]
    set maxsize   [lindex $result 2]

    foreach comp $list {
        if {[ined type $comp]=="NODE"} {
	    set host [lindex [ined name $comp] 0]
            set ip [GetIpAddress $comp]
            if {$ip==""} {
                ined acknowledge "Can not lookup IP Address for $host."
                continue;
            }
	    writeln "Multi ping for $host \[$ip\]:"
	    for {set size 0} {$size <= $maxsize} {incr size $increment} {
		if {[catch {icmp -size $size echo $ip} time]} {
		    writeln "Could not send icmp packet: $time"
		    writeln
		    return
		}
		set time [lindex [lindex $time 0] 1]
		if {$time < 0} {
		    writeln [format "  No answer for   (%5s bytes)" $size]
		} else {
		    writeln [format "  Round trip time (%5s bytes) %5s ms" \
			     $size $time]
		}
	    }
	    writeln
	}
    }
}

##
## Trace the route to a host using the van Jakobsen method.
##

proc "Trace Route" {list} {
    global icmp_routelength

    ForeachIpNode id ip host $list {
	writeln "Routing trace for $host \[$ip\]:"
	set ttl 1
	set trace ""
	set trace_ip ""
	while {$trace_ip != $ip} {
	    write [format "%2d  " $ttl]
	    set last_ip ""
	    for {set i 0} {$i < 3} {incr i} {
		if {[catch {icmp trace $ttl $ip} trace]} {
		    writeln "Could not send icmp packet: $trace"
		    writeln
		    return
		}
		set trace [lindex $trace 0]
		set trace_ip [lindex $trace 0]
		if {[catch {nslook $trace_ip} trace_name]} {
		    set trace_name ""
		}
		set trace_time [lindex $trace 1]
		if {$trace_time < 0} {
		    write " ***"
		} else {
		    if {$last_ip != $trace_ip} {
			set last_ip $trace_ip
			write " [lindex $trace_name 0] \["
			write $trace_ip "IpFlash $trace_ip"
			write "\] $trace_time ms"
		    } else {
			write " $trace_time ms"
		    }
		}
	    }
	    writeln
	    incr ttl
	    if {$ttl > $icmp_routelength} break;
	}
	writeln
    }
}

##
## Get the netmask using a special icmp request.
##

proc "Netmask" {list} {
    ForeachIpNode id ip host $list {
	if {[catch {icmp mask $ip} result]} {
	    writeln "Could not send icmp packet: $result"
	    writeln
	    return
	}
	set result [lindex [join $result] 1]
	writeln "Netmask for $host \[$ip\]: $result"
    }
    writeln
}

##
## Open an xterm with a telnet for every selected node.
##

proc Telnet {list} {
    global env

    set xterm "/usr/bin/X11/xterm"

    if {[info exists env(PATH)]} {
	foreach d [split $env(PATH) :] {
	    if {[file exists $d/xterm]} {
		set xterm $d/xterm
	    }
	}
    }
    
    ForeachIpNode id ip host $list {
	set port [ined attribute $id IP-Trouble:TelnetPort]
	if {$port != ""} {
	    catch {exec $xterm -title $host -e telnet $ip $port &}
	} else {
	    catch {exec $xterm -title $host -e telnet $ip &}
	}
    }
}

##
## Open an xterm with a rlogin for every selected node.
##

proc Rlogin {list} {
    global env

    set xterm "/usr/bin/X11/xterm"

    if {[info exists env(PATH)]} {
	foreach d [split $env(PATH) :] {
	    if {[file exists $d/xterm]} {
		set xterm $d/xterm
	    }
	}
    }
    
    ForeachIpNode id ip host $list {
	catch {exec $xterm -title $host -e rlogin $ip &}
    }
}

##
## Ask the inetd of the selected nodes for the daytime.
##

proc Daytime {list} {
    ForeachIpNode id ip host $list {
	if {[catch {tcp connect $ip daytime} f]} {
	    writeln "Can not connect to $host."
	} else {
	    set time [string trimright [gets $f]]
	    writeln "Daytime for $host \[$ip\]: $time"
	    tcp shutdown $f all
	    tcp close $f
	}
    }
    writeln
}

##
## Connect to the finger port and get what they are willing to tell us.
## This is also nice for cisco routers.
##

proc Finger {list} {
    ForeachIpNode id ip host $list {
	if {[catch {tcp connect $ip finger} f]} {
	    writeln "Can not connect to $host."
	    continue
	}
	puts $f ""
	writeln "Finger for $host \[$ip\]:"
	while {! [eof $f]} {
	    writeln [string trimright [gets $f] "\r\n"]
	}
	tcp shutdown $f all
	tcp close $f
	writeln
    }
}

##
## Get the host information from the name server. This should really
## go to the troubleshooter. And it should use the buildin hostname
## lookup command.
##

proc "DNS Info" {list} {
    ForeachIpNode id ip host $list {
	if {[catch {dns ptr $ip} name]} { set name $host }
        set name [lindex $name 0]
	if {[catch {dns address $name} a]}       { set a "" }
	if {[catch {dns ptr [lindex $a 0]} ptr]} { set ptr "" }
	if {[catch {dns hinfo $name} hinfo]}     { set hinfo "" }
	if {[catch {dns mx $name} mx]}           { set mx "" }

	set soa ""
	while {([llength [split $name .]]>0) && ($soa=="")} {
	    if {[catch {dns soa $name} soa]} { set soa "" }
	    set name [join [lrange [split $name .] 1 end] .]
	}
	
	writeln "Domain Name Server information for $host \[$ip\]:"
	if {$ptr != ""} { writeln "Name: $ptr" }
	if {$a   != ""} { writeln "Address: $a" }
	if {[lindex $hinfo 0] != ""} { writeln "Machine: [lindex $hinfo 0]" }
	if {[lindex $hinfo 1] != ""} { writeln "OS: [lindex $hinfo 1]" }
	foreach m $mx {
	    writeln "MX: $m"
	}
	if {$soa != ""} { writeln "SOA: $soa" }
	
	writeln
    }
}

##
## Whois information directly from whois.internic.net. The static
## array whois is used to cache whois information to reduce server
## load. The magic regular expression is used to strip away unwanted
## comments send by the whois server.
##

proc "Whois Info" { list } {

    static whois

    set server whois.internic.net
		
    foreach comp $list {
	set type [ined type $comp]
	if {$type == "NETWORK" || $type == "NODE"} {

	    if {$type == "NODE"} {
		set ip [GetIpAddress $comp]
	    } else {
		set ip [lindex [ined address $comp] 0]
	    }
	    if {$ip == ""} continue

	    set bytes [split $ip .]

	    if {[lindex $bytes 0] == 127} {
		continue
	    } elseif {[lindex $bytes 0] < 127} {
		set ip "[lindex $bytes 0].0.0.0"
	    } elseif {[lindex $bytes 0] < 192} {
		set ip "[lindex $bytes 0].[lindex $bytes 1].0.0"
	    } elseif {[lindex $bytes 0] < 224} {
		set ip "[lindex $bytes 0].[lindex $bytes 1].[lindex $bytes 2].0"
	    }

	    if {! [info exists whois($ip)]} {

		if {[catch {tcp connect $server whois} s]} {
		    writeln "Can not connect to whois server $server."
		    continue
		}
		
		puts $s $ip
		set answer ""
		while {![eof $s]} {
		    set line [string trimright [gets $s] "\r\n"]
		    if {![regexp {^The InterNIC|^\(Netw|^Please} $line]} {
			lappend answer $line
		    }
		}
		tcp close $s

		set whois($ip) $answer
	    }

	    foreach line $whois($ip) {
		writeln $line
	    }
	}
    }
}

##
## Show some information retrieved via the network time protocol.
##

proc "NTP Info" { list } {

    if {[lsearch [info commands] ntp] < 0} {
	ined acknowledge "Sorry, ntp not supported by this scotty version."
	return
    }

    ForeachIpNode id ip host $list {
	if {[catch {ntp $ip result} err]} {
	    writeln "$host \[$ip\]: $err"
	} else {
	    writeln "NTP information for $host \[$ip\]:"
	    foreach name [lsort [array names result]] {
		writeln "  $name = $result($name)"
	    }
	}
	writeln
	catch {unset result}
    }
}

##
## Scan the services file and try to connect to all ports
## listed there. This will not really tell us, if we could you
## a service but it may be a hint.
##

proc "TCP Services" {list} {

    ForeachIpNode id ip host $list {
	writeln "TCP services on $host \[$ip\]:"

	foreach service [netdb services] {
	    if {[lindex $service 2] == "tcp"} {
		lappend serviceList $service
	    }
	}
	lappend serviceList "X11 6000 tcp"
	foreach serv $serviceList {
	    set name [lindex $serv 0]
	    if {[catch {tcp connect $ip $name} f]} {
		continue
	    }
	    tcp shutdown $f all
	    tcp close $f
	    writeln [format "  %-12s %6s/%s" \
		     $name [lindex $serv 1] [lindex $serv 2]]
	}
	writeln
    }
}

##
## Ask the portmapper of registered RPC services for the selected nodes.
##

proc "RPC Services" {list} {
    ForeachIpNode id ip host $list {
	if {[catch {sunrpc info $ip} server]} {
	    ined acknowledge "Can not connect to $host."
	    continue
	}
	if {$server == "" } {
	    ined acknowledge "No RPC servers registered for $host"
	    continue
	}
	
	writeln "Registered RPC server for $host \[$ip\]:"
	foreach probe [lsort -ascii $server] {
	    set probe [eval format "{%8s %2d %s %5d %-16s}" $probe]
	    if {[catch {eval sunrpc probe $ip $probe} res]} {
		writeln "$probe (probe failed)"
	    } else {
		writeln [format "%s (probe successful) %6d ms" \
			 $probe [lindex $res 0] ]
	    }
	}
	writeln
    }
}

##
## Get the NFS exports list by querying the mountd of the selected nodes.
##

proc "NFS Exports" {list} {
    ForeachIpNode id ip host $list {
	if {[catch {sunrpc exports $ip} inf]} {
	    ined acknowledge "Can not connect to $host."
	    continue
	}
	if {$inf == ""} {
	    ined acknowledge "No filesystems exported by $host."
	    continue
	}
	catch {unset fstab}
	writeln "Exported NFS Filesystems for $host \[$ip\]:"
	foreach fs $inf {
	    writeln "  [lindex $fs 0] -> [join [lrange $fs 1 end]]"
	}
	writeln
    }
}

##
## Show who has mounted partitions of the selected nodes.
##

proc "NFS Mounts" {list} {
    ForeachIpNode id ip host $list {
	if {[catch {sunrpc mount $ip} inf]} {
	    ined acknowledge "Can not connect to $host."
	    continue
	}
	if {$inf == ""} {
	    ined acknowledge "No filesystems mounted from $host."
	    continue
	}
	catch {unset fstab}
	foreach fs $inf {
	    set fsname [lindex $fs 0]
	    set mname  [join [lrange $fs 1 end]]
	    if {![info exists fstab($fsname)]} {
		set fstab($fsname) $mname
	    } else {
		lappend fstab($fsname) $mname
	    }
	}
	writeln "Mounted NFS Filesystems from $host \[$ip\]:"
	foreach fsname [lsort -ascii [array names fstab]] {
	    writeln "  $fsname -> $fstab($fsname)"
	}
	writeln
    }
}

##
## Make a call to the rstat daemon and report the results for
## a time intervall which is queried from the user.
##

proc rstat_diff {l1 l2 period} {
    set len [llength $l1]
    set res ""
    for {set i "0"} {$i<$len} {incr i} {
        set el1 [lindex $l1 $i]
        set el2 [lindex $l2 $i]
        set tmp [lindex $el1 2]
        if {[lindex $el1 1]=="Counter"} {
            set tmp [expr {[lindex $el1 2]-[lindex $el2 2]}]
            set tmp [expr {"$tmp.0" / $period}]
	}
        if {[lindex $el1 1]=="Gauge"} {
            set tmp [expr {[lindex $el1 2]/256.0}]
        }
        lappend res [format "%-12s %-12s %16.3f" \
		     [lindex $el1 0] [lindex $el1 1] $tmp]
    }
    return $res
}

proc rstat {host period} {
    set stat [sunrpc stat $host]
    after [expr $period * 1000]
    set newstat [sunrpc stat $host]
    return [rstat_diff $newstat $stat $period]
}

proc "rstat RPC" {list} {

    static interval
    if {![info exists interval]} { set interval 1 }

    set interval [ined request "Show rstat results (rstat)" \
		   [list [list "Interval \[s\]:" $interval scale 1 10] ] \
		   [list start cancel] ]

    if {[lindex $interval 0] == "cancel"} return

    set interval [lindex $interval 1]
    ForeachIpNode id ip host $list {
	if {[catch {rstat $ip $interval} result]==0} {
	    writeln "rstat result for $host ($interval seconds):"
	    foreach l $result {
		writeln "  $l"
	    }
	    writeln
	} else {
	    ined acknowledge "rstat RPC failed for $host: $result"
	}
    }
}

##
## Make a call to the ether daemon and report the results for
## a time interval which is queried from the user.
##

proc "ether RPC" {list} {

    static interval
    if {![info exists interval]} { set interval 1 }

    set interval [ined request "Show ether results (etherd)" \
		   [list [list "Interval \[s\]:" $interval scale 1 10] ] \
		   [list start cancel] ]

    if {[lindex $interval 0] == "cancel"} return

    set interval [lindex $interval 1]
    ForeachIpNode id ip host $list {
	if {[catch {
	    sunrpc ether $ip open
	    after [expr $interval * 1000]
	    set res [sunrpc ether $ip]
	    sunrpc ether $ip close
	    set result ""
	    foreach elem $res {
		lappend result [eval "format {%-12s %-12s %12d} $elem"]
	    }
	}]} {
	    ined acknowledge "ether RPC failed for $host"
	} else {
	    writeln "ether result for $host ($interval seconds):"
	    foreach l $result {
		writeln "  $l"
	    }
	    writeln
	}
    }
}

##
## Set some constants that control the tool.
##

proc "Set Parameter" {list} {
    global icmp_retries icmp_timeout icmp_delay
    global icmp_routelength

    set result [ined request "IP Trouble Parameter" \
	[list [list "# of ICMP retries:" $icmp_retries scale 1 10] \
          [list "ICMP timeout \[s\]:" $icmp_timeout scale 1 42] \
          [list "Delay between ICMP packets \[ms\]:" $icmp_delay scale 1 100] \
          [list "Max. length of a route:" $icmp_routelength scale 1 42] ] \
	[list "set values" cancel] ]

    if {[lindex $result 0] == "cancel"} return

    set icmp_retries     [lindex $result 1]
    set icmp_timeout     [lindex $result 2]
    set icmp_delay       [lindex $result 3]
    set icmp_routelength [lindex $result 4]

    icmp -retries $icmp_retries
    icmp -timeout $icmp_timeout
    icmp -delay   $icmp_delay
}

##
## Display some help about this tool.
##

proc "Help IP-Trouble" {list} {
    ined browse "Help about IP-Trouble" {
	"Ping:" 
	"    Send an ICMP echo request to all selected objects. If you" 
	"    select a network with a valid address, then all address of" 
	"    address space will be queried (be careful!)." 
	"" 
	"Multi Ping:" 
	"    Send ICMP echo request with varying packet size." 
	"" 
	"Netmask:" 
	"    Send ICMP netmask request to all selected nodes." 
	"" 
	"Trace Route:" 
	"    Trace the route to the selected nodes." 
	"" 
	"Telnet:" 
	"    Open a telnet session to the selected nodes. You can" 
	"    define an alternate port number by setting the attribute" 
	"    IP-Trouble:TelnetPort." 
	"" 
	"Rlogin:" 
	"    Open a rlogin session to the selected nodes." 
	"" 
	"Daytime:" 
	"    Get the daytime of all selected nodes." 
	"" 
	"Finger:" 
	"    Finger all selected nodes." 
	"" 
	"DNS Info:" 
	"    Show information stored in the Domain Name Service." 
	"" 
	"Whois Info:" 
	"    Query the whois server whois.internic.net for information" 
	"    about a network." 
	"" 
	"NTP Info:" 
	"    Display some status information about the time on the selected" 
	"    hosts. Uses the network time protocol (ntp)." 
	"" 
	"TCP Services:" 
	"    Find available TCP services on all selected nodes." 
	"" 
	"RPC Services:" 
	"    Find available RPC services on all selected nodes." 
	"" 
	"NFS Exports:" 
	"    List the NFS export list of all selected nodes." 
	"" 
	"NFS Mounts:" 
	"    Show who has mounted the selected nodes." 
	"" 
	"rstat RPC:" 
	"    Display the result of a call to the rstat RPC service." 
	"" 
	"ether RPC:" 
	"    Display the result of a call to the etherstat RPC service." 
	"" 
	"Set Parameter:" 
	"    Display and change parameters of the tools." 
    }
}

##
## Delete the menus created by this interpreter.
##

proc "Delete IP-Trouble" {list} {
    global menus
    foreach id $menus { ined delete $id }
    exit
}

set menus [ ined create MENU "IP-Trouble" \
	    Ping "Multi Ping" "Netmask" "Trace Route" "" \
	    Telnet Rlogin Daytime Finger \
	    "DNS Info" "Whois Info" "NTP Info" "" \
	    "TCP Services" "RPC Services" "" \
	    "NFS Exports" "NFS Mounts" "" \
	    "rstat RPC" "ether RPC" "" \
	    "Set Parameter" "" \
	    "Help IP-Trouble" "Delete IP-Trouble" ]
