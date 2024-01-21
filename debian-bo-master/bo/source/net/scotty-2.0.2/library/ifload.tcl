##
## ifload.tcl
##
## This file contains the implementation of the Tcl proc
## MonitorIfLoad which starts jobs to monitor the interface 
## load.
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

##
## Calculate the interface utilisation. This is done using the formula
##
## util = ( 8 * ( delta (ifInOctets, t1, t0) 
##              + delta (ifOutOctets, t1, t0) ) / (t1 - t0) ) / ifSpeed
##
## This formula returns incorrect results for full-duplex point to point
## links. In this case, the following formula should be used:
##
## util = ( 8 * max ( delta (ifInOctets, t1, t0) ,
##                    delta (ifOutOctets, t1, t0) ) / (t1 - t0) ) / ifSpeed
##
## See Simple Times, 1(5), November/December, 1992 for more details.
##

proc GetIfLoad {} {
    set job [job current]
    array set cx [$job attribute status]

    set ifIndex $cx(ifIndex)
    set vbl [$cx(session) get "sysUpTime.0 ifOperStatus.$ifIndex \
	ifInOctets.$ifIndex ifOutOctets.$ifIndex"]
    
    set sysUpTime    [mib scan sysUpTime [lindex [lindex $vbl 0] 2]]
    set ifOperStatus [lindex [lindex $vbl 1] 2]
    set ifInOctets   [lindex [lindex $vbl 2] 2]
    set ifOutOctets  [lindex [lindex $vbl 3] 2]
    
    # be careful with Tcl's broken arithmetic
    
    if {[catch {expr $ifInOctets - $cx(ifInOctets)} deltaIn]} {
	set deltaIn  [expr double($ifInOctets) - $cx(ifInOctets)]
    }
    if {[catch {expr $ifOutOctets - $cx(ifOutOctets)} deltaOut]} {
	set deltaOut [expr double($ifOutOctets) - $cx(ifOutOctets)]
    }
    
    if {$cx(fullduplex)} {
	set delta [expr $deltaIn > $deltaOut ? $deltaIn : $deltaOut]
    } else {
	set delta [expr $deltaIn + $deltaOut]
    }
    
    if {$sysUpTime > $cx(sysUpTime) && $cx(ifSpeed) > 0} {
	set secs [expr ($sysUpTime - $cx(sysUpTime)) / 100.0]
	set val  [expr (8.0 * $delta / $secs) / $cx(ifSpeed) * 100]
    } else {
	set val 0
    }

    event raise ifLoad $ifIndex $cx(ifDescr) $ifOperStatus $val
    if {$ifOperStatus != $cx(ifOperStatus)} {
	event raise ifStatusChange $ifIndex $cx(ifDescr) $ifOperStatus
    }

    set cx(sysUpTime)    $sysUpTime
    set cx(ifInOctets)   $ifInOctets
    set cx(ifOutOctets)  $ifOutOctets
    set cx(ifOperStatus) $ifOperStatus
    $job attribute status [array get cx]
}

##
## The following procedure walks the ifTable and starts an interface 
## load monitoring procedure for every interface. We retrieve some 
## initial status information from the agent to initialize the monitor
## jobs. We store the job parameters in the job attribute status.
##

proc MonitorIfLoad {s interval {iterations {}}} {

    set result ""

    # The list of full duplex interface types. Note, IANAifType 
    # (RFC 1573) uses slightly different encodings than RFC 1213. 
    # We use RFC 1213 style here.
    
    set fullDuplex {
	regular1822 hdh1822 ddn-x25 rfc877-x25 lapb sdlc ds1 e1 
	basicISDN primaryISDN propPointToPointSerial ppp slip ds3 sip 
	frame-relay
    }

    $s walk vbl ifIndex {
	set ifIndex [lindex [lindex $vbl 0] 2]

	set vbl [$s get [list sysUpTime.0 \
                          ifInOctets.$ifIndex ifOutOctets.$ifIndex \
                          ifSpeed.$ifIndex ifDescr.$ifIndex \
			  ifType.$ifIndex ifOperStatus.$ifIndex]]

	set cx(session)      [eval snmp session [$s configure]]
	set cx(ifIndex)      $ifIndex
	set cx(sysUpTime)    [lindex [lindex $vbl 0] 2]
	set cx(sysUpTime)    [mib scan sysUpTime $cx(sysUpTime)]
	set cx(ifInOctets)   [lindex [lindex $vbl 1] 2]
	set cx(ifOutOctets)  [lindex [lindex $vbl 2] 2]
	set cx(ifSpeed)      [lindex [lindex $vbl 3] 2]
	set cx(ifDescr)      [lindex [lindex $vbl 4] 2]
	set cx(ifType)       [lindex [lindex $vbl 5] 2]
	set cx(ifOperStatus) [lindex [lindex $vbl 6] 2]
	set cx(fullduplex)   [expr [lsearch $fullDuplex $cx(ifType)] < 0]

	if {$iterations != ""} {
	    set job [job create GetIfLoad [expr $interval * 1000] $iterations]
	} else {
	    set job [job create GetIfLoad [expr $interval * 1000]]
	}

	$job attribute status [array get cx]
	lappend result $job
    }

    return $result
}

