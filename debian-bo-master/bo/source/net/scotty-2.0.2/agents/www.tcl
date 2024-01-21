##
## Here starts an experimental http interface to the 
## experimental snmp agent.
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

proc SNMP_HttpdLinkToSBrowser {oid} {
    set url http://www.cs.tu-bs.de/ibr/cgi-bin/sbrowser.cgi
    set name [lindex [split [mib name $oid] .] 0]
    return "<A HREF=\"$url?HOST=&OID=$name\">[mib name $oid]</A>"
}

proc SNMP_HttpdLinkToBindingBrowser {oid msg} {
    return "<A HREF=\"/binding?$oid\">$msg</A>"
}

proc SNMP_HttpdInit {s} {
    if {[catch {http server 1701} msg]} {
	puts stderr $msg
	exit 1
    }
}

http bind /binding?* get {
    set txt "<TITLE>Scotty SNMP agent HTTP interface.</TITLE>"
    set oid "%S"
    foreach operation "get create set" {
	set binding [snmp bind $oid $operation]
	if {$binding != ""} {
	    append txt "<H2>Binding for [mib name $oid] $operation:</H2>"
	    append txt "<LISTING>$binding</LISTING>"
	}
    }
    return $txt
}

http bind / get {
    set txt "<TITLE>Scotty SNMP agent HTTP interface.</TITLE>"
    append txt "<H2>SNMP instances:</H2>"
    snmp walk instance {
	if {[llength $instance] == 1} continue
	set oid [lindex $instance 0]
	set var [lindex $instance 1]
	append txt <BR>
	foreach operation "get create set" {
	    if {[snmp bind $oid $operation] != ""} {
		append txt " [SNMP_HttpdLinkToBindingBrowser $oid $operation] |"
	    } else {
		append txt " $operation |"
	    }
	}
	append txt " [SNMP_HttpdLinkToSBrowser $oid] "
 	if {! [catch {set $var} value]} {
	    append txt " = $value"
	}
    }
    return $txt
}
