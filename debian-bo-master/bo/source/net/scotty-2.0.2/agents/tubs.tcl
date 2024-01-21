##
## Simple and first SNMP Agent. Provides some TUBS extensions. See the
## MIB definition tubs.mib for more details.
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
## Return the actual date and time in the textual convention format.
##

proc DateAndTime {} {
    set months "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec"
    set date [getdate]
    set y [lindex $date 4]
    set m [lsearch $months [lindex $date 1]]; incr m
    set d [lindex $date 2]
    return "$y-$m-$d-[lindex $date 3]"
}

##
## Create the scalars in the scotty group.
##

proc SNMP_ScottyInit {s} {
    global scotty_version
    $s instance scottyDate.0 scottyDate [DateAndTime]
    $s instance scottyTrapDst.0 scottyTrapDst localhost
    $s instance scottyTrapMsg.0 scottyTrapMsg ""
    $s instance scottyVersion.0 scottyVersion [set scotty_version]
    $s instance scottyTclVersion.0 scottyTclVersion [info tclversion]
    $s instance scottyTclCmdCount.0 scottyTclCmdCount
    $s bind scottyTclCmdCount.0 get { 
	set scottyTclCmdCount [info cmdcount] 
    }
}

##
## Create the http group variables.
##

proc SNMP_HttpInit {s} {
    $s instance scottyHttpProxy.0 scottyHttpProxy
    $s instance scottyHttpSource.0 scottyHttpSource
    $s instance scottyHttpError.0 scottyHttpError
    
    $s bind scottyHttpProxy.0 set {
	catch {http proxy "%v"}
	set scottyHttpProxy [http proxy]
    }

    $s bind scottyHttpSource.0 set {
	set scottyHttpSource ""
	set scottyHttpError ""
	set file /tmp/http.[pid]
	catch { exec rm -f $file }
	set rc [catch { http get "%v" $file } msg]
	if {$rc} {
	    set scottyHttpError $msg
	    return
	} 
	set rc [catch { source $file } msg]
	if {$rc} {
	    set scottyHttpError $msg
	    return
	}
	catch { exec rm -f $file }
	set scottyHttpSource "%v"
    }
}

##
## Create the scalars in the nfsstat group. A get request to
## the variables nfsstatScalls or nfsstatCcalls will refresh
## all values by calling nfsstat and parsing the output.
##

proc SNMP_NfsGetStats {what} {
    switch $what {
	client { set arg -cn }
	server { set arg -sn }
	default {
	    error "unknown option $what given to SNMP_NfsGetStats"
	}
    }
    if {[catch {open "|/usr/etc/nfsstat $arg"} f]} {
	return
    }
    gets $f line; gets $f line; gets $f line; gets $f line
    scan $line "%d %d" nfs(calls) nfs(bad)
    gets $f line; gets $f line
    scan $line "%d %*s %d %*s %d %*s %d %*s %d %*s %d %*s %d %*s" \
	    nfs(null) nfs(getattr) nfs(setattr) nfs(root) \
	    nfs(lookup) nfs(readlink) nfs(read)
    gets $f line; gets $f line
    scan $line "%d %*s %d %*s %d %*s %d %*s %d %*s %d %*s %d %*s" \
	    nfs(wrcache) nfs(write) nfs(create) \
	    nfs(remove) nfs(rename) nfs(link) nfs(symlink)
    gets $f line; gets $f line
    scan $line "%d %*s %d %*s %d %*s %d %*s" \
	    nfs(mkdir) nfs(rmdir) nfs(readdir) nfs(fsstat)
    close $f
    return [array get nfs]
}


proc SNMP_NfsInit {s} {
    set interp [$s cget -agent]
    $interp alias SNMP_NfsGetStats SNMP_NfsGetStats
    foreach n [mib successor nfsServer] {
	$s instance nfsServer.$n.0 nfsServerStats($n) 0
    }
    foreach n [mib successor nfsClient] {
	$s instance nfsClient.$n.0 nfsClientStats($n) 0
    }
    $s bind nfsServer.calls get {
	array set nfsServerStats [SNMP_NfsGetStats server]
    }
    $s bind nfsClient.calls get {
	array set nfsClientStats [SNMP_NfsGetStats client]
    }
}


##
## The scottyEval group contains a table of variables that can store
## tcl code for evaluation. This is only useful if you use security
## mechanisms in the protocol exchange.
##

proc SMMP_EvalInit {s} {

    $s instance scottyEvalSlot.0 scottyEvalSlot 0
    $s bind scottyEvalSlot.0 get { 
	incr scottyEvalSlot
    }
    
    $s bind scottyEvalValue get {
	global scottyEvalStatus scottyEvalString
	set idx "%i"
	if {$scottyEvalStatus($idx) == "active"} {
	    if [catch {eval $scottyEvalString($idx)} result] {
		error "scottyEvaluation of $scottyEvalString($idx) failed"
	    }
	    set scottyEvalValue($idx) $result
	}
    }
    
    $s bind scottyEvalStatus create {
	set idx "%i"
	switch "%v" {
	    createAndGo {
		%S instance scottyEvalIndex.$idx  scottyEvalIndex($idx) $idx
		%S instance scottyEvalString.$idx scottyEvalString($idx)
		%S instance scottyEvalValue.$idx  scottyEvalValue($idx)
		%S instance scottyEvalStatus.$idx scottyEvalStatus($idx) active
	    }
	    createAndWait {
		%S instance scottyEvalIndex.$idx  scottyEvalIndex($idx) $idx
		%S instance scottyEvalString.$idx scottyEvalString($idx)
		%S instance scottyEvalValue.$idx  scottyEvalValue($idx)
		%S instance scottyEvalStatus.$idx scottyEvalStatus($idx) notReady
	    }
	    destroy {
	    }
	    default {
		error inconsistentValue
	    }
	}
    }

    $s bind scottyEvalStatus set {
	set idx "%i"
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
		after idle "unset scottyEvalIndex($idx)"
		after idle "unset scottyEvalString($idx)"
		after idle "unset scottyEvalValue($idx)"
		after idle "unset scottyEvalStatus($idx)"
	    }
	}
    }
}

##
## The proc group contains a table of actually running processes.
## This is much the same as the running software group of the Host
## Resources MIB, but it was here much earlier and it has a mechanism
## to explicitly reload the proc table.
##

proc SNMP_ProcInit {s} {
    set interp [$s cget -agent]
    $interp alias DateAndTime DateAndTime
    if {! [catch SNMP_ProcPSAX]} {
	$interp alias SNMP_GetPS SNMP_ProcPSAX
    } elseif {! [catch SNMP_ProcPSE result]} {
	$interp alias SNMP_GetPS SNMP_ProcPSE
    } else {
	$interp alias SNMP_GetPS join ""
    }

    $s instance procReload.0 procReload
    $s bind procReload.0 set {
	global procReload procID procCmd
	foreach id [array names proc] {
	    unset proc($id) 
	}
	catch {unset cmd}
	array set cmd [SNMP_GetPS]
	foreach pid [array names cmd] {
	    %S instance procID.$pid	proc(id,$pid)	$pid
	    %S instance procCmd.$pid	proc(cmd,$pid)	$cmd($pid)
	}
	set procReload [DateAndTime]
    }
}

##
## Two implementations to read the process list for BSD and SYSV machines. 
## SNMP_ProcInit above installs the right one simply by checking which
## of these procs does not fail.
##

proc SNMP_ProcPSAX {} {
    set ps [open "| ps -axc"]
    gets $ps line
    while {![eof $ps]} {
        set pid [lindex $line 0]
        if {[scan $pid "%d" dummy] == 1} {
            regsub {^[^:]*:[0-9][0-9]} $line "" cmd
	    set foo($pid) [string trim $cmd]
        }
        gets $ps line
    }
    close $ps
    return [array get foo]
}

proc SNMP_ProcPSE {} {
    set ps [open "| ps -e"]
    gets $ps line
    while {![eof $ps]} {
        set pid [lindex $line 0]
        if {[scan $pid "%d" dummy] == 1} {
            regsub {^[^:]*:[0-9][0-9]} $line "" cmd
	    set foo($pid) [string trim $cmd]
        }
        gets $ps line
    }
    close $ps
    return [array get foo]
}
