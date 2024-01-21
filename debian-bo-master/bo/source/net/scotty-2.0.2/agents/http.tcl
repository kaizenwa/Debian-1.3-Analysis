##
## Here starts an experimental interface to the CERN HTTP daemon.
##
## TODO: - Need to detect if the http server has been restarted.
##       - There is more information in the error log available.
##	 - A table indexed by URL would be nice (# of request,
##	   last request etc.)
##	 - A table indexed by host address would be nice.
##	 - A table indexed by domain name would be nice.
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

# The path to the log files in common log file format.

set AccessLog /usr/local/WWW/log/httpd-access.log
set ErrorLog  /usr/local/WWW/log/httpd-errors.log

##
## Scan a line from a CERN log file (CERN common log format).
##

proc SNMP_HTTPCernScanLine {line interp} {
    set n [scan $line {%s - - [%d/%[^/]/%d:%d:%d:%d %d] "%s %[^"]" %d %s} \
	    request(host) d mo y h m s gmt \
	    request(method) url request(code) request(size)]
    if {$n != 12} return
    set request(date) [getclock "- $mo $d $h:$m:$s $y"]
    set request(url) [lindex $url 0]
    set request(version) [lindex $url 1]
    $interp eval [list SNMP_HTTPCountRequest [array get request]]
}


##
## Scan a line from a CERN error log file.
##

proc SNMP_HTTPCernScanErrorLine {line interp} {
    set date [string trim [lindex $line 0] {[]}]
    scan [split $date "/:"] "%d %s %d %d %d %d" day month year h m s
    set request(date) [getclock "- $month $day $h:$m:$s $year"]
    set request(gmt)  [string trim [lindex $line 1] {[]}]
    $interp eval [list SNMP_HTTPCountError [array get request]]
}


##
## Find the newest file given a prefix value.
##

proc SNMP_HTTPFindNewestFile { prefix } {
    if {[catch {glob $prefix*} filelist]} {
	return
    }
    foreach file $filelist {
	if {![info exists mtime]} {
	    set active $file
	    set mtime [file mtime $file]
	} else {
	    if {[file mtime $file] > $mtime} {
		set active $file
		set mtime [file mtime $file]
	    }
	}
    }
    return $active
}


##
## This a simple minded implementation of tail(1).
##

proc SNMP_HTTPTail {f interp cmd} {
    while {[gets $f line] != -1} {
	if {$line != ""} {
	    $cmd $line $interp
	}
    }
    after 2000 [list SNMP_HTTPTail $f $interp $cmd]
}

##
## Restart the HTTP agent. This will re-initialize all variables.
##

proc SNMP_HTTPRestart {interp} {

    global AccessLog ErrorLog log err

    # Reset all variables that keep state information.

    catch {close $log}
    catch {close $err}

    # Reset the variables that keep the HTTP statistics.

    $interp eval {
	catch {unset httpStartTime}
	foreach var [array names  wwwStats] {
	    uplevel #0 "set wwwStats($var) 0"
	}
    }

    # Find the newest log and error files and start a tail emulation
    # to process log and error messages.
    
    set name [SNMP_HTTPFindNewestFile $AccessLog]
    if {$name != ""} {
	set log [open $name r]
	SNMP_HTTPTail $log $interp SNMP_HTTPCernScanLine
    }

    set name [SNMP_HTTPFindNewestFile $ErrorLog]
    if {$name != ""} {
	set err [open $name r]
	SNMP_HTTPTail $err $interp SNMP_HTTPCernScanErrorLine
    }
}


##
## Create the SNMP variables that provide access to the statistics.
## See the MIB definition in tubs.mib for more details.
##

proc SNMP_HTTPInit {s} {

    global AccessLog ErrorLog

    #if {! [file readable $AccessLog] || ! [file readable $ErrorLog]} {
	#puts stderr "Can not read HTTP log files - skipping HTTP agent."
	#return
    #}

    if [catch {tcp connect 127.0.0.1 http} server] {
	puts stderr "No http server running on this host - skipping HTTP agent."
	return
    }
    catch {tcp close $server}

    set interp [$s cget -agent]

    $s instance wwwGetRequests.0    wwwStats(GET)    0
    $s instance wwwHeadRequests.0   wwwStats(HEAD)   0
    $s instance wwwPostRequests.0   wwwStats(POST)   0
    $s instance wwwPutRequests.0    wwwStats(PUT)    0
    $s instance wwwDeleteRequests.0 wwwStats(DELETE) 0
    $s instance wwwLinkRequests.0   wwwStats(LINK)   0
    $s instance wwwUnlinkRequests.0 wwwStats(UNLINK) 0
    $s instance wwwOtherRequests.0  wwwStats(OTHER)  0
    $s instance wwwBadRequests.0    wwwStats(BAD)    0
    $s instance wwwOutBytes.0       wwwStats(BYTES)  0
    $s instance wwwUpTime.0	    wwwUpTime

    $s bind wwwUpTime.0 get {
	set wwwUpTime [expr ([getclock] - $httpStartTime) * 100]
    }

    $interp eval {

	##
	## Callback to read another log entry from a CERN log file.
	##
	
	proc SNMP_HTTPCountRequest {rl} {
	    global wwwStats
	    array set request $rl
	    switch -- $request(method) {
		GET    -
		HEAD   -
		POST   -
		PUT    -
		DELETE -
		LINK   -
		UNLINK {
		    incr wwwStats($request(method))
		}
		default {
		    incr wwwStats(OTHER)
		}
	    }
	    catch {incr wwwwStats(OUT) $request(size)}
	}
	
	
	##
	## Callback to count a failed error request. The first line defines 
	## the start time of the HTTP server. All other lines describes one 
	## error request.
	##
	
	proc SNMP_HTTPCountError {rl} {
	    global httpStartTime wwwStats
	    array set request $rl
	    if {![info exists httpStartTime]} {
		set httpStartTime $request(date)
	    } else {
		incr wwwStats(BAD)
	    }
	}
    }

    SNMP_HTTPRestart $interp
}
