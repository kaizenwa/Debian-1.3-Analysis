#! /usr/local/bin/scotty -inf
##
## Some experimental TUBS  specific procs.
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

LoadDefaults snmp

SnmpInit SNMP-TUBS

set snmp_port 1701
set snmp_timeout 10

##
## Load the mib modules required by this script. This will prevent to load 
## all mib modules and reduce memory requirements and statup time.
##

mib load rfc1447.mib
mib load tubs.mib
mib load mlm.mib

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
## Show the system information of MIB II.
##

proc "System Information" {list} {
    ShowScalars $list system
}

proc "Scotty Information" {list} {
    ShowScalars $list scottyStatus
    ShowScalars $list scottyDownload
}

proc "Scotty Download" {list} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	if {[catch {$s get "scottyHttpProxy.0 scottyHttpSource.0"} msg]} {
	    writeln $msg
	    $s destroy
	    continue
	}
	set proxy [lindex [lindex $msg 0] 2]
	set url   [lindex [lindex $msg 1] 2]
	set msg [ined request "Download and source a scotty script via http:" \
		[list [list Proxy: $proxy] \
		      [list URL: $url] ] \
		[list "download" cancel] ]

	if {[lindex $msg 0] == "download"} {
	    set proxy [lindex $msg 1]
	    set url   [lindex $msg 2]
	    if {[catch {
		$s set [list [list scottyHttpProxy.0  $proxy] \
			     [list scottyHttpSource.0 $url]]
		$s get scottyHttpError.0
	    } msg]} {
		writeln $msg
		$s destroy
		continue
	    }
	    set msg [lindex [lindex $msg 0] 2]
	    if {$msg != ""} {
		ined acknowledge $msg
	    }
	}
	$s destroy
    }
}

proc "SNMP Statistics" {list} {
    ShowScalars $list snmp
}

##
## Retrieve the NFS statistics from the agent.
##

proc "NFS Server Statistics" {list} {
    ShowScalars $list nfsServer
}

proc "NFS Client Statistics" {list} {
    ShowScalars $list nfsClient
}

##
## Retrieve WWW server statistics.
##

proc "WWW Server Statistics" {list} {
    ShowScalars $list www
}

##
## Retrieve the Process Table.
##

proc "Running Processes" {list} {
    ForeachIpNode id ip host $list {
        set s [SnmpOpen $id $ip]
	if {[catch {$s set {{procReload.0 ""}}} date]} {
	    writeln $date
	    continue
	}
	set date [lindex [lindex $date 0] 2]
        writeln "Running processes for $host \[$ip\]: ($date)"
	set txt ""
	try {
	    $s walk x "procEntry.procID procEntry.procCmd" {
		set id  [lindex [lindex $x 0] 2]
		set cmd [lindex [lindex $x 1] 2]
		append txt [format "%6d  %s\n" $id $cmd]
	    }
	} msg {
	    append txt "$msg\n"
	}
        writeln $txt
        $s destroy
    }
}

##
## Display the scottyEvalTable.
##

proc "Show Eval Table" {list} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	set txt "Tcl Evaluation Table of host \[$ip\]:\n"
	append txt "Index       Status     Command\n"
	try {
	    $s walk x "scottyEvalIndex scottyEvalStatus scottyEvalString" {
		set scottyEvalIndex  [lindex [lindex $x 0] 2]
		set scottyEvalStatus [lindex [lindex $x 1] 2]
		set scottyEvalString [lindex [lindex $x 2] 2]
		append txt [format " %3d  %12s     %s\n" \
                        $scottyEvalIndex $scottyEvalStatus $scottyEvalString]
	    }
	} msg {
	    append txt "$msg\n"
	}
	writeln $txt
        $s destroy
    }
}

proc SelectEvalRow {host ip s} {
    set rowList ""
    try {
	$s walk x "scottyEvalIndex scottyEvalStatus scottyEvalString" {
	    set scottyEvalIndex  [lindex [lindex $x 0] 2]
	    set scottyEvalStatus [lindex [lindex $x 1] 2]
	    set scottyEvalString [lindex [lindex $x 2] 2]
	    lappend rowList "$scottyEvalIndex $scottyEvalStatus $scottyEvalString"
	}
    } msg {
	writeln $msg
	return
    }
    set res [ined list "Select an eval row on $host:" \
	     $rowList "eval edit new cancel"]
    set cmd [lindex $res 0]
    if {$cmd == ""} { set cmd eval }
    switch $cmd {
	cancel { return }
	eval {
	    set idx [lindex [lindex $res 1] 0]
	    set txt "Evaluating row $idx of host $host \[$ip\]:\n"
	    try {
		set value [lindex [lindex [$s get scottyEvalValue.$idx] 0] 2]
		append txt "$value\n"
	    } msg {
		append txt "$msg\n"
	    }
	    writeln $txt
	    return
	}
	new {
	    try {
		set idx [lindex [lindex [$s get scottyEvalSlot.0] 0] 2]
		$s set [list [list scottyEvalStatus.$idx INTEGER createAndWait] \
			[list scottyEvalString.$idx "getdate"] ]
	    } msg {
		writeln $msg
		return
	    }
	    set scottyEvalStatus.$idx active
	}
	edit {
	    set idx [lindex [lindex $res 1] 0]
	}
    }
    try {
	set aa [$s get [list scottyEvalStatus.$idx scottyEvalString.$idx]]
	set scottyEvalStatus [lindex [lindex $aa 0] 2]
	set scottyEvalString [lindex [lindex $aa 1] 2]
    }
    set res [ined request "Enter a new Tcl command for entry $idx:" \
	    [list [list Command: $scottyEvalString] \
            [list Status: $scottyEvalStatus radio active notInService destroy] ] \
            [list "set values" cancel] ]
    switch [lindex $res 0] {
	"set values" {
	    try {
		$s set [list \
			[list scottyEvalString.$idx [lindex $res 1]] \
			    [list scottyEvalStatus.$idx [lindex $res 2]] \
			   ]
	    } msg {
		writeln $msg
	    }
	}
    }
}

proc "Use Eval Table" {list} {
    ForeachIpNode id ip host $list {
	set s [SnmpOpen $id $ip]
	SelectEvalRow $host $ip $s
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

proc "Delete SNMP-TUBS" {list} {
    global menus
    foreach id $menus { ined delete $id }
    exit
}

##
## Display some help about this tool.
##

proc "Help SNMP-TUBS" {list} {
    ined browse "Help about SNMP-TUBS" {
	"System Information:" 
	"    Display the system information of the tubs SNMP agent." 
	"" 
	"Scotty Information:" 
	"    Display some information about the scotty midlevel SNMP agent." 
	"" 
	"NFS Server Statistics:" 
	"    Display nfsstat like NFS server statistics." 
	"" 
	"NFS Server Statistics:" 
	"    Display nfsstat like NFS client statistics." 
	"" 
	"WWW Server Statistics:" 
	"    Display some interesting WWW server statistics." 
	"" 
	"Running Processes:" 
	"    Display the process running on the agents host." 
	"" 
	"Set Parameter:" 
	"    This dialog allows you to set SNMP parameters like retries, " 
	"    timeouts, community name and port number. " 
    }
}

set menus [ ined create MENU "SNMP-TUBS" \
	    "System Information" "SNMP Statistics" "" \
	    "Scotty Information" "Scotty Download" "" \
	    "NFS Server Statistics" "NFS Client Statistics" "" \
	    "WWW Server Statistics" "" \
	    "Running Processes" "" \
	    "Show Eval Table" "Use Eval Table" "" \
	    "TCL MLM:Start Script" "TCL MLM:Delete Script" \
	    "TCL MLM:Script Results" "" \
	    "MLM MIB:ScriptTable" "MLM MIB:SourceTable" \
	    "MLM MIB:ExecutionTable" "MLM MIB:ResultTable" "" \
	    "Set Parameter" "" \
	    "Help SNMP-TUBS" "Delete SNMP-TUBS"]

##
## Some debugging MLM stuff. Should work with any MLM MIB.
##

proc "ScriptTable" {list} {
    ShowTable $list mlmScriptTable
}

proc "SourceTable" {list} {
    ShowTable $list mlmSourceTable
}

proc "ExecutionTable" {list} {
    ShowTable $list mlmExecutionTable
}

proc "ResultTable" {list} {
    ShowTable $list mlmResultTable
}

##
## Somewhat nicer commands...
##

proc SelectMLMSript {s txt} {
    catch {unset runable}
    $s walk x "mlmScriptIndex mlmScriptName mlmScriptDescr mlmScriptStatus" {
	set index  [lindex [lindex $x 0] 2]
	set name   [lindex [lindex $x 1] 2]
	set status [lindex [lindex $x 3] 2]
	if {$status == "active"} {
	    set descr($index)  [lindex [lindex $x 2] 2]
	    lappend runable [list $index $name]
	}
    }
    if {[info exists runable]} {
	while {1} {
	    set result [ined list "Select a script to $txt:" [lsort $runable] \
		    [list select describe cancel]]
	    set idx [lindex [lindex $result 1] 0]
	    if {[lindex $result 0] == "cancel"} return
	    if {$idx != ""} { 
		if {[lindex $result 0] == "describe"} {
		    ined acknowledge $descr($idx)
		} else {
		    return $idx 
		}
	    }
	}
    }
    return ""
}

proc "Start Script" {list} {
    static intv reps args

    if {![info exists intv]} { set intv 100 }
    if {![info exists reps]} { set reps 10 }
    if {![info exists args]} { set args "" }

    ForeachIpNode id ip host $list {
        set s [SnmpOpen $id $ip]
	set script [SelectMLMSript $s start]
	if {$script == ""} {
	    $s describe
	    continue
	}

	if {[catch {
	    set idx [$s get mlmNextExecution.0]
	    $s set $idx
	    set idx [lindex [lindex $idx 0] 2]
	    $s set [list [list mlmExecutionStatus.$idx createAndWait] ]
	} err]} {
	    writeln $err
	    $s destroy
            continue
	}

	set r [ined request "Start MLM script:" \
                [list [list Arguments: $args ] \
                      [list Interval: $intv ] \
                      [list Repetitions: $reps ] ] [list start cancel] ]

	if {[lindex $r 0] == "cancel"} {
	    catch {$s set [list [list mlmExecutionStatus.$idx destroy]]}
	    $s destroy
	    continue
	}

	set args [lindex $r 1]
	set intv [lindex $r 2]
	set reps [lindex $r 3]

	if {[catch {$s set [list \
		[list mlmExecutionScript.$idx $script] \
		[list mlmExecutionArguments.$idx $args] \
		[list mlmExecutionInterval.$idx $intv] \
		[list mlmExecutionRepetitions.$idx $reps] \
		]} err]} {
            writeln $err
        }

	if {[catch {$s set [list \
                [list mlmExecutionStatus.$idx active] \
		]} err]} {
            writeln $err
        }

	$s destroy
    }
}

proc "Delete Script" {list} {
    ForeachIpNode id ip host $list {
        set s [SnmpOpen $id $ip]
	set idx [SelectMLMSript $s delete]
	if {$idx == ""} {
	    $s destroy
	    continue
	}
	$s set [list [list mlmScriptStatus.$idx destroy]]
	$s destroy
    }
}

proc "Script Results" {list} {
    ForeachIpNode id ip host $list {
        set s [SnmpOpen $id $ip]
	set idx [SelectMLMSript $s "show results"]
	if {$idx == ""} {
	    $s destroy
	    continue
	}

	set idxlist ""
	$s walk x "mlmExecutionIndex mlmExecutionScript" {
	    set scr   [lindex [lindex $x 1] 2]
	    if {$scr == $idx} {
		lappend idxlist [lindex [lindex $x 0] 2]
	    }
	}

	if {$idxlist == ""} {
	    ined acknowledge "No results for script $idx on $host \[$ip\]!"
	    continue
	}

	foreach idx $idxlist {
	    set boottime [lindex [lindex [$s get scottyDate.0] 0] 2]
	    writeln "MLM Script Results $idx on $host \[$ip\]:"
	    writeln "$boottime (agent restart)"
	    $s walk x [list mlmResultStatus.$idx mlmResultTimeStamp.$idx \
		    mlmOctetStringValue.$idx ] {
		set status [lindex [lindex $x 0] 2]
		set time   [lindex [lindex $x 1] 2]
		set result [mib format sysDescr [lindex [lindex $x 2] 2]]
		writeln " + $time: $result ($status)"
	    }
	    writeln
	}
	$s destroy
    }
}

