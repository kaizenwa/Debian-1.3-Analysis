##
## Here starts the experimental implementation of the MLM MIB. 
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
## The script table contains currently only static script definitions.
## The MLM_Register procedure can be used to register new scripts.
##

proc SNMP_MLMRegister {s name description {status active}} {

    global mlmNextScript
    global mlmScriptIndex mlmScriptName mlmScriptDescr mlmScriptStorageType
    global mlmSourceScript mlmSourceLine mlmSourceCode

    set i $mlmNextScript
    incr mlmNextScript

    set description [string trim $description]

    $s instance mlmScriptIndex.$i	mlmScriptIndex($i)	$i
    $s instance mlmScriptName.$i	mlmScriptName($i)	$name
    $s instance mlmScriptDescr.$i	mlmScriptDescr($i)	$description
    $s instance mlmScriptStorageType.$i mlmScriptStorageType($i) permanent
    $s instance mlmScriptStatus.$i	mlmScriptStatus($i)     $status
}

##
## Remove a complete row given by $idx from a table.
##

proc SNMP_MLMDestroyTable { table idx } {
    if {[mib syntax $table] == "SEQUENCE OF"} {
	set table [mib successor $table]
    }
    foreach elem [mib successor $table] {
	upvar #0 $elem var
	catch {unset var($idx)}
    }
}

##
## Handling of the mlm execution table.
##

proc SNMP_MLMResult {s script result error} {
    global mlmResultLastIndex sysUpTime
    if {![info exists mlmResultLastIndex($script)]} {
	set mlmResultLastIndex($script) 0
    }
    set idx [incr mlmResultLastIndex($script)]
    $s instance mlmResultExecution.$script.$idx \
	    mlmResultExecution($script,$idx) $script
    $s instance mlmResultIndex.$script.$idx \
	    mlmResultIndex($script,$idx) $idx
    $s instance mlmResultTimeStamp.$script.$idx \
            mlmResultTimeStamp($script,$idx) $sysUpTime
    $s instance mlmResultType.$script.$idx \
	    mlmResultType($script,$idx) octetstring
    if {$result != ""} {
	$s instance mlmOctetStringValue.$script.$idx \
		mlmOctetStringValue($script,$idx) [mib scan sysDescr $result]
	$s instance mlmResultStatus.$script.$idx \
		mlmResultStatus($script,$idx) ok
    } else {
	$s instance mlmOctetStringValue.$script.$idx \
		mlmOctetStringValue($script,$idx) [mib scan sysDescr $error]
	$s instance mlmResultStatus.$script.$idx \
		mlmResultStatus($script,$idx) error
    }
}

proc SNMP_MLMExecute { s idx } {
    global mlmExecutionScript mlmExecutionArguments
    global mlmScriptName
    puts stderr "SNMP_MLMExecute $idx"
    set sidx $mlmExecutionScript($idx)
    set cmd [list $mlmScriptName($sidx) $mlmExecutionArguments($idx)]
    set cmd $mlmScriptName($sidx) ; append cmd " $mlmExecutionArguments($idx)"
    set cmd [concat $mlmScriptName($sidx) $mlmExecutionArguments($idx)]
    if {[eval [list catch $cmd result]]} {
	SNMP_MLMResult $s "" $result
    } else {
	SNMP_MLMResult $s $idx $result ""
    }
    puts stderr "SNMP_MLMExecute $cmd -> $result"
}

proc SNMP_MLMStartExecution {s idx {status active}} {

    global mlmExecutionIndex
    global mlmExecutionScript mlmExecutionArguments
    global mlmExecutionInterval mlmExecutionRepetitions
    global mlmExecutionPermanence mlmExecutionStatus

    $s instance mlmExecutionIndex.$idx mlmExecutionIndex($idx) $idx
    $s instance mlmExecutionScript.$idx mlmExecutionScript($idx) "0"
    $s instance mlmExecutionArguments.$idx mlmExecutionArguments($idx) ""
    $s instance mlmExecutionInterval.$idx mlmExecutionInterval($idx) 0
    $s instance mlmExecutionRepetitions.$idx mlmExecutionRepetitions($idx) 1
    $s instance mlmExecutionPermanence.$idx mlmExecutionPermanence($idx) temporary
    set mlmExecutionStatus($idx) $status
}

###
###

proc SNMP_MLMInit {s} {
    $s instance mlmLock.0 mlmLock 1

    $s instance mlmNextScript.0 mlmNextScript 1
    $s bind mlmNextScript.0 set {
	incr mlmNextScript
	break
    }

    $s bind mlmScriptStatus create {
	switch "%v" {
	    createAndGo {
		SNMP_MLMRegister "%S" "" "" active
	    }
	    createAndWait {
		SNMP_MLMRegister "%S" "" "" notReady
	    }
	    destroy {
	    }
	    default {
		error inconsistentValue
	    }
	}
    }

    $s bind mlmScriptStatus set {
	switch "%v" {
	    active {
	    }
	    notInService {
	    }
	    notReady {
	    }
	    createAndGo {
		error inconsistentValue
	    }
	    createAndWait {
		error inconsistentValue
	    }
	    destroy {
		SNMP_MLMDestroyTable mlmScriptTable "%i"
	    }
	}
    }

    $s instance mlmNextExecution.0 mlmNextExecution 1
    $s bind mlmNextExecution.0 set { 
	incr mlmNextExecution
	break
    }

    $s bind mlmExecutionStatus create {
	switch "%v" {
	    createAndGo {
		SNMP_MLMStartExecution "%S" "%i" active
	    }
	    createAndWait {
		SNMP_MLMStartExecution "%S" "%i" notReady
	    }
	    destroy {
	    }
	    default {
		error inconsistentValue
	    }
	}
    }

    $s bind mlmExecutionStatus set {
	global mlmExecutionInterval mlmExecutionRepetitions
	static mlmExecutionJob
	set idx "%i"
	switch "%v" {
	    active {
		set interval [expr $mlmExecutionInterval($idx) * 10]
		set times $mlmExecutionRepetitions($idx)
		set mlmExecutionJob($idx) \
		    [job create "SNMP_MLMExecute "%S" $idx" $interval $times]
	    }
	    notInService {
	    }
	    notReady {
	    }
	    createAndGo {
		error inconsistentValue
	    }
	    createAndWait {
		error inconsistentValue
	    }
	    destroy {
		after 1 "SNMP_MLMDestroyTable mlmExecutionTable $idx"
		unset mlmExecutionJob($idx)
	    }
	}
    }
}

