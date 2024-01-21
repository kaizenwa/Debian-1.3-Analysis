##
## snmp.tcl
##
## This file useful utilities for programming SNMP based applications
## in Tcl. Previous versions were written for the tkined(1) editor.
## These versions contain no tkined specific code anymore.
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


# SNMP_Walk --
#
# Walk through a MIB tree and show the contents of the MIB variables.
#
# Arguments:
# s -		The SNMP session to use.
# oid -		The object identifier of the MIB subtree.

proc SNMP_Walk {s oid} {

    set ip [SNMPGetIP $s]
    if {[catch {nslook $ip} host]} { set host "" }
    set host [lindex $host 0]

    writeln "$host \[$ip\] \[[getdate]\]:"
    if {[catch {
	$s walk vbl $oid {
	    set vb [lindex $vbl 0]
	    set oid [lindex $vb 0]
	    set val [lindex $vb 2]
	    writeln [format "  %-24s %s" "[mib name $oid]:" $val]
	}
    } msg]} {
	writeln $msg
    }
    writeln
}


# SNMP_Scalars --
#
# Retrieve all scalar variables of a MIB group into an array.
#
# Arguments:
# s -		The SNMP session to use.
# path -	The object identifier of the MIB group.
# name -	The name of an array where we store the values.

proc SNMP_Scalars {s path name} {

    upvar $name value
    catch {unset value}

    # Make sure value is an array and the path is a valid object
    # identifier and not an instance or a table. This is a hack.

    set value(foo) bar
    unset value(foo)
    if {[mib syntax $path] != "OBJECT IDENTIFIER"} return

    set exception(noSuchObject)   1
    set exception(noSuchInstance) 1
    set exception(endOfMibView)   1

    # Build a list of all variables in this group that do not have any
    # sucessors (to avoid embedded tables).

    set varlist ""
    foreach var [mib successor [mib oid $path]] {
	if {[mib access $var] != "not-accessible"} {
	    lappend varlist "$var.0"
	}
    }

    # Try to retrieve the varlist in one get request. If this fails,
    # we use a get request to retrieve variables individually.
    # Note, I would like to honor noSuchName or tooBig errors, but
    # unfortunately some agents do not send this error code.

    if {[catch {$s get $varlist} vbl]} {
	set vbl ""
	foreach var $varlist {
	    if {[catch {$s get $var} v]} continue
	    lappend vbl [lindex $v 0]
	}
    }

    # This commented version below does it asynchonously. Fast on 
    # fast reliable networks but problematic if the net has problems.

#    if {[catch {$s get $varlist} vbl]} {
#	global _vbl_
#	foreach var $varlist {
#	    $s get $var {
#		if {"%E" == "noError"} {
#		    lappend _vbl_ [lindex "%V" 0]
#		}
#	    }
#	}
#	$s wait
#	set vbl $_vbl_
#	unset _vbl_
#    }

    set result ""
    foreach vb $vbl {
	set oid [lindex $vb 0]
	set syn [lindex $vb 1]
	set val [lindex $vb 2]
	if {[info exists exception($syn)]} continue
	set name [mib name $oid]
	set value($name) $val
	lappend result $name
    }
    return $result
}


# SNMP_ShowScalars --
#
# Show all scalar variables of a MIB group.
#
# Arguments:
# s -		The SNMP session to use.
# path -	The object identifier of the MIB group.

proc SNMP_ShowScalars {s path} {

    set ip [SNMPGetIP $s]
    if {[catch {nslook $ip} host]} { set host "" }
    set host [lindex $host 0]

    set txt "$host \[$ip\] \[[getdate]\]:\n"

    if {[catch {SNMP_Scalars $s $path values} msg]} {
	append txt "$msg\n"
	writeln $txt
	return
    }

    foreach name $msg {
	append txt [format "  %-24s %s\n" "$name:" $values($name)]
    }
    writeln $txt
}


# SNMP_ShowTable --
#
# Show a complete MIB table. First determine which rows are supported
# by an agent and then walk through the table. The formating code is
# ugly and should be replaced by a better version.
#
# Arguments:
# s -		The SNMP session to use.
# table -	The object identifier of an snmp table.

proc SNMP_ShowTable {s table} {

    set ip [SNMPGetIP $s]
    if {[catch {nslook $ip} host]} { set host "" }
    set host [lindex $host 0]

    set table [mib oid $table]
    if {[mib syntax $table] == "SEQUENCE"} {
	set list [split $table .]
	set len  [expr [llength $list] - 2]
	set table [join [lrange $list 0 $len] .]
    }

    if {[mib syntax $table] != "SEQUENCE OF"} return

    set exception(noSuchObject)   1
    set exception(noSuchInstance) 1
    set exception(endOfMibView)   1

    writeln "$host \[$ip\] \[[getdate]\]:"

    # Check if we can walk the rows simultaneously. We start a walk and
    # if we get a result, we will be happy. Otherwise, we will start a
    # very silly walk to collect the table element by element.

    set rows ""
    foreach var [mib suc [mib suc $table]] {
	if {[mib access $var] != "not-accessible"} {
	    lappend rows $var
	}
    }
    catch {
	$s walk vbl $rows {
	    set table $rows
	    break
	}
    }

    # Now walk through the table and fill the array named value,
    # indexed by row name : instance identifier, e.g.
    #	value(ifType:1) "le0"
    #	value(ifType:2) "le1"
    # The lists xorder and yorder contain the order of the row
    # names (xorder) and the instance identifier (yorder).

    set xorder ""
    set yorder ""
    if [catch {
	$s walk vbl $table {
	    foreach vb $vbl {
		set oid  [lindex $vb 0]
		set syn  [lindex $vb 1]
		set val  [lindex $vb 2]
		if {[info exists exception($syn)]} continue
		
		set lname [split [mib name $oid] .]
		set pfx [lindex $lname 0]
		set idx [join [lrange $lname 1 end] .]
		
		if {[lsearch $xorder $pfx] < 0} {
		    lappend xorder $pfx
		}
		if {[lsearch $yorder $idx] < 0} {
		    lappend yorder $idx
		}
		set value($pfx:$idx) $val
	    }
	}
    } msg] {
	writeln $msg
	return
    }

    if {![info exists xorder] || ![info exists yorder]} {
	return
    }
    
    # Calculate the max. length of all values for every row.

    foreach pfx $xorder {
	set xindex($pfx) [string length $pfx]
	foreach n [array names value $pfx:*] {
	    set len [string length $value($n)]
	    if {$len > $xindex($pfx)} {
		set xindex($pfx) $len
	    }
	}
    }

    # Try to display the table as a table. This is still no good solution... 

    while {[array names xindex] != ""} {
	set foo ""
	set total 0
	set fmt ""
	set txt ""
	foreach pfx $xorder {
	    incr total $xindex($pfx)
	    incr total 2
	    set fmt "  %$xindex($pfx)s"
	    append txt [format $fmt $pfx]
	    lappend foo $pfx
	}
	writeln $txt
	foreach idx $yorder {
	    set txt ""
	    foreach pfx $foo {
		set fmt "  %$xindex($pfx)s"
		# note, large table may be inconsistend
		if [catch {format $fmt $value($pfx:$idx)} xxx] {
		    set xxx [format $fmt "?"]
		}
		append txt $xxx
	    }
	    writeln $txt
	}
	foreach pfx $foo {
	    unset xindex($pfx) 
	}
    }

    writeln
}


# SNMP_SetValue --
#
# Set a value of a MIB variable.
#
# Arguments:
# s -		The SNMP session to use.
# oid -		The object identifier of the variable to set.
# w -		The root widget name or empty if running with tkined.

proc SNMP_SetValue {s oid {w ""}} {
    $s walk x $oid {
	set x [lindex $x 0]
	set value([mib name [lindex $x 0]]) [lindex $x 2]
    }
    set name [array names value]
    if {[llength $name] > 1} {
	if {$w != ""} {
	    set res [Dialog_Select $w.l questhead "Select an instance:" \
		    [lsort [array names value]] "select cancel"]
	} else {
	    set res [ined list "Select an instance:" \
		    [lsort [array names value]] "select cancel"]
	}
	if {[lindex $res 0] != "select"} return
	set name [lindex $res 1]
    }
    if {$name == ""} return
    if {$w != ""} {
	set res [Dialog_Request $w.r questhead \
		"Enter new value for $name:" $value($name) "ok cancel"]
    } else {
	set res [ined request "Enter new value for $name:" \
		[list [list $name $value($name) entry 20]] "ok cancel"]
    }
    if {[lindex $res 0] != "ok"} return
    set new [lindex $res 1]
    if [catch {$s set [list [list $name $new]]} msg] {
	error $msg
    }
}


# SNMP_CreateInstance --
#
# Set a value of a MIB variable.
#
# Arguments:
# s -		The SNMP session to use.
# oid -		The object identifier of the variable to set.
# w -		The root widget name or empty if running with tkined.

proc SNMP_CreateInstance {s oid {w ""}} {
    if {$w != ""} {
	set res [Dialog_Request $w.l questhead \
		"Enter an instance identifier:" [mib name $oid]. \
		"create cancel"]
    } else {
	set res [ined request "Enter an instance identifier:" \
		[list [list Instance [mib name $oid] entry 20]] \
		"create cancel"]
    }
    if {[lindex $res 0] != "create"} return
    set name [lindex $res 1]
    if {$w != ""} {
	set res [Dialog_Request $w.r questhead \
		"Enter new value for $name:" "" "ok cancel"]
    } else {
	set res [ined request "Enter new value for $name:" \
		[list [list Value "" entry 20]] "ok cancel"]
    }
    if {[lindex $res 0] != "ok"} return
    set new [lindex $res 1]
    if [catch {$s set [list [list $name $new]]} msg] {
	error $msg
    }
}


# SNMP_DescribeMibNode --
#
# Give a description of a node in the MIB tree.
#
# Arguments:
# oid -		The oid of the MIB node to describe.

proc SNMP_DescribeMibNode oid {

    if {[info proc writeln] == ""} {
        proc writeln args { puts [join $args] }
    }
    
    writeln "Object Type:\t\t[mib name $oid]"
    writeln "Object Identifier:\t[mib oid $oid]"
    set idx [mib index $oid]
    if {$idx != ""} {
	writeln "Table Index:\t\t$idx"
    }
    writeln "Access:\t\t\t[mib access $oid]"
    set tc [mib tc $oid]
    if {$tc == ""} {
	writeln "Syntax:\t\t\t[mib syntax $oid]"
    } else {
	writeln "Syntax:\t\t\t[lindex $tc 1]"
	if {[lindex $tc 2] != ""} {
	    writeln "Textual Convention:\t[lindex $tc 0]"
	    writeln "Format:\t\t\t[lindex $tc 2]"
	} elseif {[lindex $tc 3] != ""} {
	    writeln "Enumeration:\t\t[join [lindex $tc 3] {, }]"
	} else {
	    writeln "Textual Convention:\t[lindex $tc 0]"
        }
    }
    writeln "File:\t\t\t[mib file $oid]"
    set description [mib description $oid]
    if {$description != ""} {
	writeln "\n$description"
    }
    writeln
}


# SNMPGetIP --
#
# Return the IP destination address of a SNMP session. We also define 
# the writeln proc to a reasonable default if it is not defined yet.
#
# Arguments:
# s -		The SNMP session handle

proc SNMPGetIP {s} {
    if {[catch {$s cget -address} ip]} {
	set ip [lindex [$s cget -dstparty] 2]
    }
    if {[info proc writeln] == ""} {
	proc writeln args { puts [join $args] }
    }
    return $ip
}
