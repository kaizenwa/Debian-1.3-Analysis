#! /usr/local/bin/scotty -inf
##
## Simple SNMP MIB browser for tkined.
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

SnmpInit SNMP-Browser

##
## Callback for walk button.
##

proc mb_walk { browser prefix } {
    foreach id [ined -noupdate select] {
	ForeachIpNode id ip host [list [ined -noupdate retrieve $id] ] {
	    set s [SnmpOpen $id $ip]
	    SNMP_Walk $s $prefix
	    $s destroy
	}
    }
}

##
## Callback for the monitor button.
##

proc mb_monitor { browser prefix } {

    static monitor

    foreach id [ined -noupdate select] {
	lappend list [ined -noupdate retrieve $id]
    }

    if {![info exists list]} return

    if {![info exists monitor]} {
	set monitor [ined create INTERPRETER snmp_monitor.tcl]
    } else {
	if {[ined retrieve $monitor] == ""} {
	    set monitor [ined create INTERPRETER snmp_monitor.tcl]
	}
    }

    ined send $monitor MonitorVariable $list $prefix
}

##
## Callback for the set button.
##

proc mb_set { browser prefix } {

    ined size
    after 1000

    foreach id [ined -noupdate select] {
	ForeachIpNode id ip host [list [ined -noupdate retrieve $id] ] {
	    set s [SnmpOpen $id $ip]
	    set name [split $prefix .]
	    set len [llength $name]
	    incr len -1
	    set name [lindex $name $len]
	    set query ""
	    $s walk x $prefix {
		set name  [mib name [lindex [lindex $x 0] 0]]
		set value [lindex [lindex $x 0] 2]
		set pfx [lindex [split $name .] 0]
		set idx [join [lrange [split $name .] 1 end] .]
		lappend query [list $pfx.$idx $value]
		lappend oidlist "$pfx.$idx"
	    }
	    if {$query == ""} {
		$s destroy
		continue
	    }
	    set result [ined request "Set SNMP variable on $host \[$ip\]:" \
		    $query [list "set value" cancel]]
	    if {[lindex $result 0] == "cancel"} {
		$s destroy
                continue
	    }
	    set i 0
            foreach value [lrange $result 1 end] {
                lappend varbind [list [lindex $oidlist $i] $value]
                incr i
            }
            if {[catch {$s set $varbind} error]} {
                ined acknowledge "Set operation failed:" "" $error
            }
	    $s destroy
	}
    }
}

##
## Callback for show button (everything not in the first line).
##

proc mb_showit { browser prefix } {

    set subs [mib successor $prefix]
    
    ined -noupdate clear $browser
    ined -noupdate hyperlink $browser "mb_walk $browser $prefix" --walk--
    ined -noupdate append $browser " "

    set last [split $prefix .]
    set this [lindex $last [expr [llength $last]-1]]
    set newlength [expr [llength $last]-2]
    set last [join [lrange $last 0 $newlength] .]
    if {$last != ""} {
	ined -noupdate hyperlink $browser "mb_showit $browser $last" ---up---
	ined -noupdate append $browser " "
    }

    if {$last != ""} {
	set brothers [mib successor $last]
	
	set idx [lsearch $brothers $this]
	incr idx -1
	set previous [lindex $brothers $idx]
	incr idx 2
	set next [lindex $brothers $idx]
	if {$previous != ""} {
	    ined -noupdate hyperlink $browser \
		    "mb_showit $browser $last.$previous" -previous-
	    ined -noupdate append $browser " "
	}
	if {$next != ""} {
	    ined -noupdate hyperlink $browser \
		    "mb_showit $browser $last.$next" --next--
	    ined -noupdate append $browser " "
	}
    }

    if {$subs == ""} {
	set oid [mib oid $prefix]
	set type [string toupper [mib syntax $oid]]
	# check if we have a textual convention here
	set tmp [lindex [mib tc $oid] 1]
	if {$tmp != ""} {
	    set type $tmp
	}
	if {[lsearch "GAUGE COUNTER COUNTER32 INTEGER" $type] >= 0} {
	    ined -noupdate hyperlink $browser \
		    "mb_monitor $browser $prefix" -monitor-
	    ined -noupdate append $browser " "
	}
	set access [mib access $prefix]
	if {[string match *write* $access]} {
	    ined -noupdate hyperlink $browser \
		    "mb_set $browser $prefix" --set--
	}
    }

    ined -noupdate append $browser "\n\n"
    ined -noupdate append $browser "Path:        $prefix\n"

    if {$subs == ""} {
        set oid [mib oid $prefix]
	ined -noupdate append $browser "Syntax:      [mib syntax $prefix]\n"
	ined -noupdate append $browser "Access:      [mib access $prefix]\n\n"
	ined -noupdate append $browser [mib desc $oid]
	mb_walk $browser $prefix
    } else {
	set toggle 0
	ined -noupdate append $browser "\n"
	foreach elem $subs {
	    ined -noupdate hyperlink \
		$browser "mb_showit $browser $prefix.$elem" $elem
	    set len [string length $elem]
	    incr len +2
	    if {$toggle} { 
		set toggle 0
		ined -noupdate append $browser "\n" 
	    } else {
		set toggle 1
		if {$len < 8} {
		    ined -noupdate append $browser "\t\t\t\t"
		} elseif {$len < 16} {
		    ined -noupdate append $browser "\t\t\t"
		} elseif {$len < 24} {
		    ined -noupdate append $browser "\t\t"
		} else {
		    ined -noupdate append $browser "\t"
		}
	    }
	}
    }
}

##
## This recursive proc creates menu entries by appending to the 
## global var mibcmds.
##

proc makemenu { top level } {
    global mibcmds
    set scalars ""
    set subs ""
    set writeable ""
    set level [mib oid $level]
    set name  [mib name $level]
    foreach suc [mib successor $level] {
	if {[mib successor $suc] == ""} {
	    set access [mib access $suc]
	    if {$access == "not-accessible"} 	continue
	    lappend scalars $suc
	    if {[string match *write* $access]} {
		lappend writeable $suc
	    }
	} else {
	    lappend subs $suc
	}
    }
    if {$scalars != ""} {
	set cmd "$name ([lindex [split $top :] 0])"
	lappend mibcmds $top:$name:$cmd
	proc $cmd { list } "ShowScalars \$list $name"
	if {$writeable != ""} {
	    set cmd "edit $name ([lindex [split $top :] 0])"
	    lappend mibcmds $top:$name:$cmd
	    proc $cmd { list } "EditScalars \$list $name"
	}
    }
    foreach suc $subs {
	if {[mib syntax $suc] == "SEQUENCE OF"} {
	    set writeable ""
	    set entries [mib successor [mib successor $suc]]
	    foreach entry $entries {
		set access [mib access $entry]
		if {[string match *write* $access]} {
		    lappend writeable $suc
		}
	    }
	    set cmd "[mib name $suc] ([lindex [split $top :] 0])"
	    lappend mibcmds $top:$name:$cmd
	    proc $cmd { list } "ShowTable \$list $suc"
	    if {$writeable != ""} {
		set cmd "edit [mib name $suc] ([lindex [split $top :] 0])"
		lappend mibcmds $top:$name:$cmd
		proc $cmd { list } "EditTable \$list $suc"
	    }
	} else {
	    makemenu $top:$name $suc
	}
    }
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
## Edit all scalars of a group that are writeable.
##

proc EditScalars {list group} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	SnmpEditScalars $s $group
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
## Edit a complete table for the node objects in list.
##

proc EditTable {list table args} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	SnmpEditTable $s $table $args
	$s destroy
    }
}

##
## Start a MIB browser for this device.
##

proc "MIB Browser" { list } {

    global snmp_browser
    
    set browser [ined -noupdate create LOG]
    lappend snmp_browser $browser
    ined -noupdate name $browser "SNMP MIB Browser"
    mb_showit $browser internet
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

proc "Help SNMP-Browser" {list} {
    ined browse "Help about SNMP-Browser" {
	"MIB Browser:" 
	"    Open a new window with a MIB browser. You can step through" 
	"    the MIB similar to a file selector. When reaching a leaf," 
	"    the selected nodes are queried for the selected object and" 
	"    a description of the objects will be displayed." 
	"" 
	"<MIB MENUS>:" 
	"    These menus allow you to access MIB scalars and tables very" 
	"    once you know what you are looking for. All menus are created" 
	"    from the MIB database. New submenus should automatically appear" 
	"    if you add new MIB definitions to the MIB database." 
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

proc "Delete SNMP-Browser" {list} {
    global menus snmp_browser
    foreach id $snmp_browser { 
	catch {ined -noupdate delete $id}
    }
    foreach id $menus { ined delete $id }
    exit
}

##
## Create the MENU which will contain submenus to access the MIB
## information quickly (good if you know what you are searching for). 
##

set mibcmds [list "MIB Browser" ""]

catch {
    foreach suc [mib successor [mib oid mib-2]] {
	makemenu [string toupper mib-2] $suc
    }
}

catch {
    foreach suc [mib successor [mib oid snmpV2]] {
	makemenu [string toupper snmpV2] $suc
    }
}

lappend mibcmds ""

catch {
    foreach suc [mib successor [mib oid private.enterprises]] {
	foreach moresuc [mib successor $suc] {
	    makemenu [string toupper [mib name $suc]] $moresuc
	}
    }
}

catch {
    foreach suc [mib successor experimental] {
	foreach moresuc [mib successor [mib oid $suc]] {
	    makemenu [string toupper $suc] $moresuc
	}
    }
}

lappend mibcmds "" \
    "Walk MIB Tree" "" \
    "Set Parameter" "" \
    "Help SNMP-Browser" "Delete SNMP-Browser"

set menus [eval ined create MENU "SNMP-Browser" $mibcmds]
