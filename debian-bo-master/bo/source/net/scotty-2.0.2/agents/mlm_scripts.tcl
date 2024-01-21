##
## Some sample scripts for the experimenal MLM agent.
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


proc SNMP_MLMScriptEcho {args} {
    return [join $args]
}

proc SNMP_MLMScriptCheckICMP {args} {
    return [join [icmp echo [join $args]]]
}

proc SNMP_MLMScriptCheckIfStatus {args} {
    set result ""
    foreach host [join $args] {
	catch {
	    set s [snmp session -address $host]
	    $s walk x "ifDescr ifAdminStatus ifOperStatus" {
		set descr [lindex [lindex $x 0] 2]
		set admin [lindex [lindex $x 1] 2]
		set oper  [lindex [lindex $x 2] 2]
		if {$oper != $admin} {
		    append result "$host: Interface $descr:"
		    append result " AdminStatus=$admin != OperStatus=$oper !\n"
		}
	    }
	    $s destroy
	}
    }
    return $result
}

proc SNMP_MLMScriptCountTcpServiceUser {s port} {
    set count 0
    $s walk x "tcpConnState tcpConnLocalPort" {
        set tcpConnState     [lindex [lindex $x 0] 2]
        set tcpConnLocalPort [lindex [lindex $x 1] 2]
        if {$tcpConnState == "established" && $tcpConnLocalPort == $port} {
            incr count
        }
    }
    return $count
}

proc SNMP_MLMScriptTcpServiceUser {port args} {
    set result ""
    foreach host [join $args] {
        catch {
            set s [snmp session -address $host]
            lappend result "$host/[SNMP_MLMScriptCountTcpServiceUser $s $port]"
            $s destroy
        }
    }
    return $result
}

##
##

proc SNMP_MLMScriptRegisterSamples {s} {
    SNMP_MLMRegister SNMP_MLMScriptEcho {
	This procedure simply echos its arguments.
    }
    SNMP_MLMRegister SNMP_MLMSCriptCheckICMP {
	Send an ICMP echo request to the arguments.
    }
    SNMP_MLMRegister SNMP_MLMScriptCheckIfStatus {
	Test the ifAdmin and ifOper status.
    }
    SNMP_MLMRegister SNMP_MLMScriptTcpServiceUser {
	Count the number of established TCP connections to a given port.
    }
}

