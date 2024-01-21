#! /usr/local/bin/scotty -inf
##
## Tool Manager for TKINED.
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

LoadDefaults remote manager

##
## Set up some default parameters.
##

if {[info exists default(listen)]} {
    set dolist $default(listen)
} else {
    set dolist false
}

if {[info exists default(silent)]} {
    set doaccept $default(silent)
} else {
    set doaccept false
}

##
## Test if we are at home.
##

proc local {} {

    if {[catch {dns ptr [nslook [exec hostname]]} name]} { return 0 }

    if {[string match "*cs.tu-bs.de" $name]} { return 1 }

    if {[string match "*CeBit.dfn.de" $name]} { return 1 }

    return 0
}

##
## Search for the interpreter. Return the absolute path or an
## empty string if not found.
##

proc FindScript { fname } {

    global auto_path

    if [file exists $fname] { 
	return [file dirname $fname]/[file tail $fname]
    }

    foreach dir $auto_path {
        if [file exists $dir/$fname] {
            return [file dirname $dir/$fname]/$fname
        }
    }

    return ""
}

##
## Start the interpreter given by fname (searching the path).
## Be smart and check accept names without a tcl extension.
##

proc StartScript { fname } {

    set base [lindex $fname 0]
    set args [lrange $fname 1 end]
    set fullname [FindScript $base]
    if {$fullname == ""} {
	set fullname [FindScript $base.tcl]
    }

    if {$fullname == ""} {
	ined acknowledge "Unable to find $base nor $base.tcl."
    } else {
	ined create INTERPRETER [concat $fullname $args]
    }
}

##
## Find a program searching along the environment variable PATH.
##

proc FindProgram { fname } {

    global env

    if {[info exists env(PATH)]} {
	set path [split $env(PATH) :]
    } else {
	set path "/bin /usr/bin /usr/local/bin"
    }

    if [file exists $fname] {
        return [file dirname $fname]/[file tail $fname]
    }

    foreach dir $path {
        if [file exists $dir/$fname] {
	    if [file executable $dir/$fname] {
		return [file dirname $dir/$fname]/$fname
	    }
        }
    }

    return ""
}

##
## Start the program given by fname (searching the path).
##

proc StartProgram { fname } {

    set fullname [FindProgram $fname]

    if {$fullname == ""} {
	ined acknowledge "Unable to find $fname."
    } else {
	exec "$fullname" "&"
    }
}

##
## Fire up various tcl scripts for different tasks. These procs
## are registered in the Tools menu below and called from the 
## tkined editor.
##

proc "IP Trouble" { list } {
    StartScript ip_trouble.tcl
}

proc "IP Monitor" { list } {
    StartScript ip_monitor.tcl
}

proc "IP Layout" { list } {
    StartScript ip_layout.tcl
}

proc "IP Discover" { list } {
    StartScript ip_discover.tcl
}

proc "IP World" { list } {
    StartScript ip_world.tcl
}

proc "SNMP Trouble" { list } {
    StartScript snmp_trouble.tcl
}

proc "SNMP Host & Ident" { list } {
    StartScript snmp_host.tcl
}

proc "SNMP Monitor" { list } {
    StartScript snmp_monitor.tcl
}

proc "SNMP Browser" { list } {
    StartScript snmp_browser.tcl
}

proc "SNMP Tree" { list } {
    StartScript "mibtree -i"
}

proc "SNMP CISCO" { list } {
    StartScript snmp_cisco.tcl
}

proc "SNMP HP" { list } {
    StartScript snmp_hp.tcl
}

proc "SNMP TUBS" { list } {
    StartScript snmp_tubs.tcl
}

proc "SNMP ACCT" { list } {
    StartScript snmp_acct.tcl
}

proc "CMIP Trouble" { list } {
    StartScript cmip_trouble.tcl
}

proc "User Client" { list } {
    StartScript netguard_usr.tcl
}

proc "Admin Client" { list } {
    StartScript netguard_adm.tcl
}

proc "Tkgraphs Client" { list } {
    StartProgram tkgraphs
}

proc "Event Filter" { list } {
    StartScript event.tcl
}

proc "GAME" { list } {
    StartScript game.tcl
}

proc "Bones" { list } {
    StartScript bones.tcl
}

proc "SimuLan" { list } {
    StartScript /usr/local/lib/simuLan/simulan_tool.tcl
}

##
## Show a clipboard that can be used to enter ined commands for
## interactive debugging purposes.
##

proc "Show Clipboard" { list } {

    static log

    if {[info exists log]} {
	if {[ined -noupdate retrieve $log] != ""} {
	    set res [ined confirm "Replace previous clipboard?" \
		     [list replace cancel] ]
	    if {$res != "replace"} return
	}
	ined -noupdate delete $log
    }

    set log [ined -noupdate -notrace create LOG]
    ined -noupdate name $log "tkined clipboard"
}

##
## Let the user select a script file and start it as a tkined
## interpreter.
##

proc "Start Script" { list } {

    static dir
    
    if {![info exists dir]} { set dir "" }

    set file [ined fileselect "Please select a script file:" $dir]
    if {$file == ""} return
    
    set dir [file dirname $file]
    StartScript $file
}

##
## Kill an application form the tkined editor.
##

proc "Kill Interpreter" { list } {

    set interps ""
    set tools ""
    foreach comp [ined retrieve] {
	switch [ined type $comp] {
	    INTERPRETER {
		lappend interps $comp
	    }
	    MENU {
		lappend tools $comp
	    }
	}
    }

    set list ""
    foreach interp $interps {
	set id [ined id $interp]
	set  tlist ""
	foreach tool $tools {
	    if {[ined interpreter [ined id $tool]] == $id} {
		lappend tlist [lindex $tool 2]
	    }
	}
	set name [file tail [ined name $id]]
	lappend list [format "%-14s(%s) %s" $id $name $tlist]
    }

    set result [ined list "Select an interpreter to kill:" $list \
		[list kill cancel] ]
    if {[lindex $result 0] == "cancel"} return

    ined delete [lindex [lindex $result 1] 0]
}

##
## Display some help about this tool.
##

proc "Help Tools" { list } {
    ined browse "Help about Tool Manager" {
	"The Tool Manager is responsible to dynamically load new tools" 
	"into tkined. It currently knows about the following tools. (Note" 
	"that not all of them may be available on your site.)" 
	"" 
	"IP Trouble:" 
	"    A set of commands to find out why something is broken." 
	"" 
	"IP Monitor:" 
	"    Some simple monitoring commands. They allow you to sit down" 
	"    and watch whats going on." 
	"" 
	"IP Layout:" 
	"    These commands help you to layout your network." 
	"" 
	"IP Discover:" 
	"    Discover the IP structure of you network. This saves a lot" 
        "    of time when starting INED without a useable network map." 
	"" 
	"SNMP Trouble:" 
	"    Commands to query and monitor your SNMP devices." 
	"" 
	"SNMP Monitor:" 
	"    Monitor SNMP variables. Needs more work to be serious." 
	"" 
	"SNMP Browser:" 
	"    A MIB browser to manually inspect a MIB." 
	"" 
	"SNMP Private:" 
	"    SNMP commands specific to some private MIB extensions." 
	"" 
	"Remote tkined -> Listen:"  
	"    Listen for connections to this tkined. This may be used to" 
	"    allow access to the current map over the Internet." 
	"Remote tkined -> Connect:" 
	"    Connect to a remote tkined over the Internet. May be useful" 
	"    to load the current view of a running tkined from a remote" 
	"    site." 
	"Remote tkined -> Info:" 
	"    Display some information about current connections." 
	"" 
	"Start Script:" 
	"    Prompt the user for the file name of a script to execute." 
	"" 
	"Kill Interpreter:" 
	"    Kill an interpreter. This may be useful if a tool seems." 
	"    to hang in a loop. Note, it may be difficult to find the" 
	"    right interpreter if you have one tool running more than" 
	"    once." 
	"" 
	"Show Clipboard:" 
	"    Open a clipboard where you may save some notices." 
    }
}

##
## Delete the TOOL Manager and exit this interpreter.
##

proc "Delete Tools" { list } {
    global menus
    foreach id $menus {	ined delete $id }
    exit
}

##
## Set up the menu. This is ugly because we have features locally
## that should not appear on other sites.
##

set cmds [list "IP Trouble" "IP Monitor" "IP Layout" "IP Discover" "IP World" ""]
if {[lsearch [info commands] snmp] >= 0} {
    lappend cmds "SNMP Trouble"
    lappend cmds "SNMP Host & Ident"
    lappend cmds "SNMP Monitor"
    lappend cmds "SNMP Browser"
    if {[FindScript mibtree] != ""} {
        lappend cmds "SNMP Tree"
    }
    lappend cmds "SNMP Private:SNMP CISCO"
    lappend cmds "SNMP Private:SNMP HP"
    lappend cmds "SNMP Private:SNMP TUBS"
    lappend cmds "SNMP Private:SNMP ACCT"
    lappend cmds ""
}
if {[lsearch [info commands] cmip] >= 0} {
    lappend cmds "CMIP Trouble"
    lappend cmds ""
}
lappend cmds "Event Filter"
if {[info commands msqlconnect] != ""} {
    lappend cmds "Bones"
}
lappend cmds ""

if {[FindScript netguard_adm.tcl] != ""} {
    lappend cmds "NetGuard:User Client"
}
if {[FindScript netguard_adm.tcl] != ""} {
    lappend cmds "NetGuard:Admin Client"
}
if {[FindProgram tkgraphs] != ""} {
    lappend cmds "NetGuard:Tkgraphs Client"
}

lappend cmds "Remote tkined:Connect"
lappend cmds "Remote tkined:Listen"
lappend cmds "Remote tkined:Info"

if {[local]} {
    lappend cmds "IBR STUFF:SimuLan"
    lappend cmds "IBR STUFF:GAME"
}

set done 0
if {[info exists default]} {
    foreach name [array names default] {
	set l [split $name .]
	if {[lindex $l 0] == "tool"} {
	    
	    if {$done == 0} { lappend cmds "" ; set done 1}
	    set toolname [lindex $l 1]
	    set toolcmd $default($name)
	    
	    # define a proc for the callback
	    
	    proc $toolname [list list] [list StartScript $toolcmd]
	    
	    lappend cmds $toolname
	}
    }
}


lappend cmds ""
lappend cmds "Start Script" 
lappend cmds "Kill Interpreter"
lappend cmds "Show Clipboard" 
lappend cmds ""
lappend cmds "Help Tools" 
lappend cmds "Delete Tools"

set menus [eval ined create MENU Tools $cmds]

############################################################################
##
## Here starts the communication module. It is used to create connections
## between tkined interpreters.
##
############################################################################

##
## Dump one tkined object into a tcl string that rebuilds the
## object when evaluated. This is the same algorithm as tkined
## uses in editor.c in do_dump().
##

proc ComDumpComp { comp } {

    global done
    set id [ined id $comp]

    if {[info exists done($id)]} return

    set done($id) $id
    switch [ined type $comp] {
	NODE {
	    return [ined dump $id]
	}
	NETWORK {
	    return [ined dump $id]
	}
	REFERENCE {
	    return [ined dump $id]
	}
	TEXT {
	    return [ined dump $id]
	}
	IMAGE {
	    return [ined dump $id]
	}
	LINK {
	    set res ""
	    append res "[ComDumpComp [ined src $comp]]"
	    append res "[ComDumpComp [ined dst $comp]]"
	    append res "[ined dump $id]"
	    return $res 
	}
	GROUP {
	    set res ""
	    foreach m [ined member $comp] {
		append res "[ComDumpComp $m]"
	    }
	    append res "[ined dump $id]"
            return $res
	}
	LOG {
	    return [ined dump $id]
	}
	STRIPCHART {
	    return [ined dump $id]
	}
	BARCHART {
	    return [ined dump $id]
	}
    }
}

##
## Download the complete dump of the tkined editor. This proc may be 
## called by clients to get the initial image of the server map.
##

proc ComDownload {} {

    global done
    catch {unset done}

    set result "ined page [ined page]; "
    foreach comp [ined retrieve] {
	append result [ComDumpComp $comp]
    }

    return $result
}

##
## This is the callback that absorbs the tkined trace. It simply 
## forwards the trace to all the clients in client_list.
##

proc ComTraceCallback { args } {

    global client_list

    foreach client $client_list {

	if {[catch {$client -async ComDo $args} err]} {
	    set client_list [ldelete client_list $client]
	    rpc delete $client
	    if {$client_list == ""} {
		ined trace ""
	    }
	}
    }
}

##
## Accept an incoming connection and create an RPC client.
##

proc ComAccept { host port } {

    global client_list doaccept

    if {![info exists client_list]} { set client_list "" }
    if {![info exists doaccept]}    { set doaccept false }

    if {$doaccept != "true"} {
	set result [ined confirm "Accept connection from $host?" \
		   [list accept ignore] ]
	if {$result == "ignore"} return
    }

    if {[catch {rpc client $host $port} client]} {
	ined acknowledge "Can not connect to server:" $client
	return
    }

    lappend client_list $client

    ined trace ComTraceCallback

    return $client
}

##
## Start the listening RPC Server. It will wait for incoming connections
## and start itself a client connection by exporting the command 
## ComAccept. If the port argument is empty, we shut down the server
## and disallow further requests.
##

proc ComListen { {port ""} } {

    static server server_port

    if {$port == ""} {
	if {![info exists server]} return
	rpc delete $server
	unset server
	return
    }

    if {[info exists server]} {
	return $server_port
    }

    set server ""
    for {set i $port} {$i < $port+10} {incr i} {
	if {! [catch {rpc server $i} server]} break
    }

    if {$server == ""} {
	ined acknowledge "Can not start server!"
	return
    }

    rpc register $server ComAccept
    rpc register $server ComDownload

    return [set server_port $i]
}

##
## Connect to the master. If successful, setup our own server
## that will be registered by the master using its ComAccept proc.
##

proc ComConnect { host port {download 1} {clear 1} } {

    global myid
    global server

    # first connect to the master tkined editor

    if {[catch {rpc client $host $port} client]} {
	ined acknowledge "Can not connect to remote tkined."
	return
    }

    # now set up a rpc server that will handle forwarded traces

    set server ""
    for {set i [expr {$port+10}]} {$i < $port+20} {incr i} {
	if {! [catch {rpc server $i} server]} break
    }

    if {$server == ""} {
	ined acknowledge "Can not start server!"
	rpc delete $client
	unset server
	return
    }

    rpc register $server ComDo

    if {[catch {nslook [exec hostname]} ip]} {
	if {[catch {dns aaddress [exec hostname]} ip]} {
	    ined acknowledge "Can not lookup [exec hostname]."
	    rpc delete $client
	    rpc delete $server
	    unset server
	    return
	}
    }

    if {[catch {$client ComAccept $ip $i} err]} {
	ined acknowledge "Communication error to $host port $port:" $err
	rpc delete $client
	rpc delete $server
	unset server
	return
    }

    if {$err == ""} {
	ined acknowledge "tkined on $host port $port refuses connection!"
        rpc delete $client
        rpc delete $server
	unset server
        return
    }

    if {$clear} {
	foreach comp [ined -noupdate retrieve] {
	    if {[lsearch "INTERPRETER MENU LOG" [ined type $comp]] < 0} {
		ined -noupdate delete [ined id $comp]
	    }
	}
    }

    if {$download} {
	if {[catch {$client ComDownload} dump]} {
	    ined acknowledge "Failed to download from server:" $dump
	} else {
	    foreach elem [split $dump ";"] {
		if {$elem == ""} continue
		if {[catch {eval $elem} res]} {
		    writeln "*download* $res :: $elem"
		}
		if {[lindex $elem 0] == "set"} {
		    set myid([lindex $elem 1]) $res
		}
	    }
	}
    }

    rpc delete $client

    return $server
}

##
## This is the hard part. Here we must parse all commands to eliminate
## simple query commands and to map the ids to the ids used by our slave
## tkined.
##

proc ComDo { args } {

    global myid

    debug "*ComDo-1* $args"

    set idx [llength $args]
    incr idx -1
    set result [lindex $args $idx]
    incr idx -1
    if {[lindex $args $idx] == ">"} {
	incr idx -1
	set cmd [lrange $args 0 $idx]
    } else {
	set cmd $args
	set result ""
    }

    if {[llength $cmd] <= 2} return

    # first handle create and delete commands to update the myid table

    if {[lindex $cmd 0] != "ined"} {
	ined acknowledge "Retrieved unknown command from server:" $cmd
	return
    }

    debug "*ComDo-2* $cmd"

    if {[lindex $cmd 2] == "page"} {
	debug "*PAGE* $cmd"
	catch {eval $cmd}
	return
    }

    switch [lindex $cmd 1] {
	create {
	    set id [string trim $result]
	    switch [lindex $cmd 2] {
		LINK {
		    set ida [lindex $cmd 3]
		    if {![info exists myid($ida)]} return
		    set cmd [lreplace $cmd 3 3 $myid($ida)]
		    set idb [lindex $cmd 4]
		    if {![info exists myid($idb)]} return
		    set cmd [lreplace $cmd 4 4 $myid($idb)]
		}
		GROUP {
		    set member [lrange $cmd 3 end]
		    set cmd [lrange $cmd 0 2]
		    foreach guy $member {
			if {![info exists myid($guy)]} return
			lappend cmd $myid($guy)
		    }
		}
		INTERPRETER {
		    return
		}
		MENU {
		    return
		}
	    }
	    set cmd "ined -noupdate -notrace [string range $cmd 4 end]"
	    debug "*ComDo-3* $cmd"
	    if {[catch {eval $cmd} res]} return
	    set myid($id) $res
	    return
	}
	delete {
	    set ids [join [lrange $cmd 2 end]]
	    set myids ""
	    foreach id $ids {
		if {[info exists myid($id)]} {
		    lappend myids $myid($id)
		}
	    }
	    set cmd [lreplace $cmd 2 end $myids]
	    set cmd "ined -noupdate -notrace [string range $cmd 4 end]"
	    debug "*ComDo-4* $cmd"
	    catch {eval $cmd}
	    return
	}
    }

    # still here? check for a normal command and do it

    set normal [list retrieve move icon font color label name address oid \
		collapse expand ungroup lower raise append flash \
                scale size jump points clear attribute values ]

    if {[lsearch $normal [lindex $cmd 1]] < 0} return

    set id [lindex $cmd 2]
    if {![info exists myid($id)]} return
    set cmd [lreplace $cmd 2 2 $myid($id)]
    set cmd "ined -noupdate -notrace [string range $cmd 4 end]"
    debug "*ComDo-5* $cmd"
    debug "*ComDo-6* [concat $cmd]"
    catch {eval $cmd}
}

##
## A command to control if we are listening for connections from
## other tkineds.
##

proc "Listen" { list } {

    global dolist doaccept

    set result [ined request "Set the parameter for remote tkined access:" \
		  [list [list "Listening:" $dolist radio true false] \
		        [list "Silent Accept:" $doaccept radio true false ] ] \
		  [list ok cancel] ]
    
    if {[lindex $result 0] == "cancel"} return

    set dolist   [lindex $result 1]
    set doaccept [lindex $result 2]

    if {$dolist == "true"} {
	set port [ComListen 6660]
	if {[catch {exec hostname} host]} {
	    set host "localhost"
        }
	if {$port != ""} {
	    ined acknowledge \
		"Listening for remote access on $host port $port."
	}
    } else {
	ComListen
	ined acknowledge "Stopped listening for remote access."
    }
}

##
## This is the connect command as invoked from the tkined editor.
##

proc "Connect" { list } {

    global debug
    static host port down new todo

    global server

#    if {[info exists server]} {
#	ined acknowledge "You are already connected!"
#	return
#    }

    if {![info exists host]}  { set host localhost }
    if {![info exists port]}  { set port 6660 }
    if {![info exists down]}  { set down true }
    if {![info exists new ]}  { set new  true }
    if {![info exists debug]} { set debug false }
    if {![info exists todo]} { 
	if {[info exists server]} {
	    set todo disconnect
	} else {
	    set todo connect
	}
    }

    set port 6660

    while {1} {
	
	set result [ined request "Connect to a remote tkined editor." \
		     [list [list Action: $todo radio connect disconnect] \
		           [list Host: $host] \
                           [list Port: $port scale 6660 6670] \
		           [list Download: $down radio true false ] \
		           [list Clear: $new radio true false ] \
                           [list Debug: $debug radio true false ] ] \
		     [list ok cancel] ]

	if {[lindex $result 0] == "cancel"} return

	set todo   [lindex $result 1]
	set host   [lindex $result 2]
	set port   [lindex $result 3]
	set down   [lindex $result 4]
	set new    [lindex $result 5]
	set debug  [lindex $result 6]

	if {$todo == "connect"} {

	    if {[info exists server]} {
		catch {rpc delete $server}
                unset server
	    }

	    set success [ ComConnect $host $port \
			 [expr {$down == "true"}] [expr {$new == "true"}] ]
       
	    if {$success != ""} {
		set todo disconnect
		break
	    }
	} else {
	    if {[info exists server]} {
		catch {rpc delete $server}
		unset server
	    }
	    set todo connect
	    break
	}

	incr port
    }
}

proc "Info" { list } {

    if {[catch {exec hostname} host]} {
	set host "localhost"
    }

    set txt ""

    foreach r [rpc info] {
	set ri [rpc info $r]
	if {[lsearch [lindex $ri 1] ComAccept] < 0} continue
	set port [lindex [tcp info [lindex $ri 0]] 1]
	lappend txt "Listening for remote connections on $host port $port."
	foreach file [lindex $ri 2] {
	    set ti [tcp info $file]
	    lappend txt "Connection established from [lindex $ti 2] port [lindex $ti 3]."
	}
    }

    if {$txt != ""} { lappend txt "" }

    foreach r [rpc info] {
        set ri [rpc info $r]
	if {[lsearch [lindex $ri 1] ComDo] < 0} continue
        set port [lindex [tcp info [lindex $ri 0]] 1]
	foreach file [lindex $ri 2] {
	    set ti [tcp info $file]
            lappend txt "Connected to [lindex $ti 0] port [lindex $ti 1]."
        }
    }

    if {$txt == ""} {
	set txt \
	    [list "Not connected and not listening for connection requests."]
    }
    
    eval ined acknowledge $txt
}

if {$dolist == "true"} {
    ComListen 6660
}
