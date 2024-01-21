#! /usr/local/bin/scotty -nf
##
## A CGI script for the CERN httpd to browse SNMP MIBS through WWW.
##
## Copyright (c) 1994, 1995
##
## J. Schoenwaelder
## TU Braunschweig, Germany
## Institute for Operating Systems and Computer Networks
##
## Permission to use, copy, modify, and distribute this
## software and its documentation for any purpose and without
## fee is hereby granted, provided that the above copyright
## notice appear in all copies.  The University of Braunschweig
## makes no representations about the suitability of this
## software for any purpose.  It is provided "as is" without
## express or implied warranty.
##

set sbrowser /ibr/cgi-bin/sbrowser.cgi

set icon(dir) \
	"<IMG ALT=\"\" SRC=\"/httpd-internal-icons/directory.xbm\">"
set icon(txt) \
	"<IMG ALT=\"\" SRC=\"/httpd-internal-icons/text.xbm\">"

##
## The mibfile array below is used as an index to select the MIB files 
## that are loaded when the browser displays a particular OID. The 
## table can be created automatically with the following script:
##

proc MibFiles { node } {
    mib walk x [mib oid $node] {
        set file([file tail [mib file $x]]) dummy
    }
    return [lsort [array names file]]
}

proc ListMibFiles {} {
    foreach subtree [concat [mib suc mib-2] [mib suc enterprises]] {
	set subtree [mib oid $subtree]
	puts "set mibfile($subtree) \"[MibFiles $subtree]\""
    }
}

##
## And here comes the output:
##

set mibfile(1.3.6.1.2.1.2) "rfc1213.mib"
set mibfile(1.3.6.1.2.1.3) "rfc1213.mib"
set mibfile(1.3.6.1.2.1.4) "rfc1213.mib"
set mibfile(1.3.6.1.2.1.5) "rfc1213.mib"
set mibfile(1.3.6.1.2.1.6) "rfc1213.mib"
set mibfile(1.3.6.1.2.1.7) "rfc1213.mib"
set mibfile(1.3.6.1.2.1.8) "rfc1213.mib"
set mibfile(1.3.6.1.2.1.10) "rfc1213.mib rfc1512.mib rfc1650.mib rfc1659.mib rfc1660.mib rfc1748.mib"
set mibfile(1.3.6.1.2.1.11) "rfc1213.mib"
set mibfile(1.3.6.1.2.1.16) "rfc1513.mib rfc1757.mib"
set mibfile(1.3.6.1.2.1.17) "rfc1493.mib"
set mibfile(1.3.6.1.2.1.19) "rfc1658.mib"
set mibfile(1.3.6.1.2.1.22) "rfc1516.mib"
set mibfile(1.3.6.1.2.1.23) "rfc1724.mib"
set mibfile(1.3.6.1.2.1.24) "rfc1414.mib"
set mibfile(1.3.6.1.2.1.25) "rfc1514.mib"
set mibfile(1.3.6.1.2.1.26) "rfc1515.mib"
set mibfile(1.3.6.1.2.1.27) "rfc1565.mib"
set mibfile(1.3.6.1.2.1.28) "rfc1566.mib"
set mibfile(1.3.6.1.2.1.29) "rfc1567.mib"
set mibfile(1.3.6.1.2.1.30) "rfc1573.smi"
set mibfile(1.3.6.1.2.1.31) "rfc1573.mib"
set mibfile(1.3.6.1.2.1.32) "rfc1611.mib rfc1612.mib"
set mibfile(1.3.6.1.2.1.35) "rfc1650.mib"
set mibfile(1.3.6.1.2.1.38) "rfc1696.mib"
set mibfile(1.3.6.1.2.1.39) "rfc1697.mib"
set mibfile(1.3.6.1.2.1.40) "netramet.mib"
set mibfile(1.3.6.1.2.1.42) "rfc1749.mib"
set mibfile(1.3.6.1.4.1.4) "unix.mib"
set mibfile(1.3.6.1.4.1.9) "cisco.mib"
set mibfile(1.3.6.1.4.1.11) "hp-unix.mib hpnp.mib"
set mibfile(1.3.6.1.4.1.18) "wellfleet.mib"
set mibfile(1.3.6.1.4.1.23) "rfc1792.mib"
set mibfile(1.3.6.1.4.1.45) "synoptics.mib"
set mibfile(1.3.6.1.4.1.72) "retix.mib"
set mibfile(1.3.6.1.4.1.1701) "mlm.mib tubs.mib"

##
## The following proc is used to load only the MIB definitions that
## are required to process the request. This saves a lot of time as
## MIB are growing and growing and growing...
##

proc LoadMib { oid } {
    global mibfile didprefix
    if {$oid == ""} return
    set oid [mib oid $oid]
    foreach prefix [array names mibfile] {
	if {[string match "$prefix.*" $oid.] 
		&& [info exists didprefix($prefix)] == 0} {
	    set didprefix($prefix) 1
	    foreach mib $mibfile($prefix) {
		mib load $mib
		append msg "<P>loading mib file $mib"
	    }
	}
    }
}

##
## Start a new html page. Writes a suitable header section.
##

proc StartPage { {title {}} } {

    puts "Content-type: text/html"
    puts ""

    puts "<HTML><HEAD><title>SNMP MIB Browser"
    if {$title != ""} {
	puts " ($title)"
    }
    puts "</title></HEAD><BODY>"

    # make sure to write the header before we get error messages
    flush stdout
}

##
## End a page and the script.
##

proc EndPage {} {
    puts "</BODY></HTML>"
    exit
}

##
## Some utility procs.
##

proc ListChildren { oid host {level 1}} {
    global sbrowser icon
    incr level -1
    puts "<DL>"
    foreach s [mib successor $oid] {
	puts "<DT>"
	if {[mib successor $s] == ""} {
	    puts "$icon(txt)"
	} else {
	    puts "$icon(dir)"
	}
	puts "<A HREF=\"$sbrowser?HOST=$host&OID=$oid.$s\">$s</A>"
	if {$level > 0} {
	    ListChildren $oid.$s $host $level
	} else {
	    puts "<BR>"
	}
    }
    puts "</DL>"
}

##
## List the elements of the current path.
##

proc ListPath { oid host {action BROWSE} } {
    global sbrowser
    foreach aa [split $oid .] {
	if {[info exists foo]} { puts -nonewline "." }
	lappend foo $aa
	set s [join $foo .]
	puts "<A HREF=\"$sbrowser?HOST=$host&OID=$s&ACTION=$action\">$aa</A>"
    }
}

##
## Walk a MIB subtree and print the result as HTML
##

proc WalkTree { oid host } {
    foreach host [split $host +] {
	puts "<H3>$host:</H3><BLOCKQUOTE>"
	if {[catch {
	    set host [split $host :]
	    set s [snmp session -address [lindex $host 0]]
	    if {[lindex $host 1] != ""} {
		$s configure -port [lindex $host 1]
	    }
	    if {[lindex $host 2] != ""} {
		$s configure -community [lindex $host 2]
	    }
	    $s walk x $oid {
		set x [lindex $x 0]
		puts "[mib name [lindex $x 0]] : [lindex $x 2]"
		puts "<BR>"
	    }
	    $s destroy
	} err]} {
	    puts "\[$host\] : $err"
	}
	puts "</BLOCKQUOTE><P>"
    }
}

##
## Issue a form dialog to set the hosts to query.
##

proc GetHost { oid host } {
    global sbrowser
    StartPage
    puts "<FORM ACTION=\"$sbrowser\" METHOD=PUT>"    
    puts "<H2>Set SNMP agent addresses:</H2>" 
    puts "<INPUT TYPE=\"text\"   NAME=HOST VALUE=\"[split $host +]\" SIZE=60>"
    puts "<P>Agent addresses are either host names or internet addresses in"
    puts "decimal dot notation like 134.169.2.1 . Multiple addresses "
    puts "should be separated using white spaces. SNMP queries are send"
    puts "to the well known SNMP port 161 using community public. Every"
    puts "request will use a timeout of 5 seconds and 3 retries.<P>"
    puts "You can specify alternate port numbers or community strings other"
    puts "than public by using the syntax "
    puts "&lt;address&gt;:&lt;port&gt;:&lt;community&gt;<P>"
    puts "<INPUT TYPE=\"hidden\" NAME=OID  VALUE=$oid>"
    if {$oid == ""} {
	puts "<INPUT TYPE=\"hidden\" NAME=ACTION  VALUE=WELCOME>"
    }
    puts "<INPUT TYPE=\"submit\" VALUE=\"set and return\">"
    puts "</FORM>"
    EndPage
}

##
## And here we start. The main program at the end of this file calls
## the following procs whenever a request is received without any
## further arguments. This is the toplevel dialog page.
##

proc Welcome { oid host } {
    global sbrowser
    StartPage

    puts "<H1>WWW SNMP MIB Browser</H1>"

    puts "Welcome to the WWW SNMP MIB Brower. This browser is a simple"
    puts "<A HREF=\"http://www.cs.tu-bs.de/ibr/bin/sbrowser.cgi\">CGI "
    puts "script</A> written using the <A HREF=\""
    puts "http://www.cs.tu-bs.de/ibr/projects/nm/scotty/\">scotty</A>"
    puts "tcl interpreter. Report bugs or any other comments to "
    puts "<TT>schoenw@ibr.cs.tu-bs.de</TT>"
    
    puts "<HR>"

    puts "<P><A HREF=\"$sbrowser?ACTION=GETHOST&OID=$oid&HOST=$host\">"
    puts "<B>Hosts:</B></A> <TT>[join [split $host +]]</TT><P><HR>"

    puts "<H2>Official Internet MIBs:</H2>"
    ListChildren mib-2 $host

    puts "<H2>Enterprise MIBs:</H2>"
    ListChildren enterprises $host

    EndPage
}

##
## Browse the mib level given by oid.
##

proc Browse { oid host } {
    global sbrowser
    StartPage [mib name $oid]

    puts "<A HREF=\"$sbrowser?HOST=$host&OID=$oid&ACTION=WELCOME\">"
    puts "<B>Goto:</B></A> "    

    ListPath $oid $host BROWSE

    puts "<P><B>Walk:</B> "

    ListPath $oid $host WALK

    puts "<P><A HREF=\"$sbrowser?ACTION=GETHOST&OID=$oid&HOST=$host\">"
    puts "<B>Hosts:</B></A> <TT>[join [split $host +]]</TT><P><HR>"

    set sucs [mib successor $oid]

    if {$sucs != ""} {
	ListChildren $oid $host 4
    } else {
	puts "<P>"
	puts "<DL>"
	puts "<DT><B>Object Type:</B><DD>[mib name $oid]"
	puts "<DT><B>Object Identifier:</B><DD>[mib oid $oid]"
	puts "<DT><B>Access:</B><DD>[mib access $oid]"
	set tc [mib tc $oid]
	if {$tc == ""} {
	    puts "<DT><B>Syntax:</B><DD>[mib syntax $oid]"
	} else {
	    puts "<DT><B>Syntax:</B><DD>[lindex $tc 1]"
	    if {[lindex $tc 2] != ""} {
		puts "<DT><B>Textual Convention:</B><DD>[lindex $tc 0]"
		puts "<DT><B>Format:</B><DD>[lindex $tc 2]"
	    } elseif {[lindex $tc 3] != ""} {
		puts "<DT><B>Enumeration:</B><DD>[join [lindex $tc 3] {, }]"
	    } else {
		puts "<DT><B>Textual Convention:</B><DD>[lindex $tc 0]"
	    }
	}
	set description [mib description $oid]
	if {$description != ""} {
	    puts "<DT><B>Description:</B><DD>$description"
	}
	puts "<DT><B>File:</B><DD>[mib file $oid]</DL><HR>"
	WalkTree $oid $host
    }

    EndPage
}

##
## Walk a MIB tree and write the result to the HTML page.
##

proc Walk { oid host } {
    global sbrowser
    StartPage [mib name $oid]

    puts "<A HREF=\"$sbrowser?HOST=$host&OID=$oid&ACTION=WELCOME\">"
    puts "<B>Goto:</B></A> "    

    ListPath $oid $host BROWSE

    puts "<P><B>Walk:</B> "

    ListPath $oid $host WALK

    puts "<P><A HREF=\"$sbrowser?ACTION=GETHOST&OID=$oid&HOST=$host\">"
    puts "<B>Hosts:</B></A> <TT>[join [split $host +]]</TT><P><HR>"

    WalkTree $oid $host

    EndPage
}

##
## The main program starts here. It checks the environment variable
## QUERY_STRING to decide what should be done. After parsing and checking 
## the parameters, we call the appropriate proc to do the job.
##

if {![info exists env(QUERY_STRING)] || ($env(QUERY_STRING) == "")} {

    Welcome "" ""

} else {

    set query [split $env(QUERY_STRING) &]
    set action ""
    set oid ""
    set host ""
    set port 161
    foreach av $query {
	set a [lindex [split $av =] 0]
	set v [lindex [split $av =] 1]
	switch $a {
	    ACTION { set action $v }
	    HOST   { regsub -all "%20" [string trim $v +] "+" host
		     regsub -all "%3A" $host ":" host
		     split $host :
		     if {[llength $host] > 1} { 
			set port [lindex $host 1] 
			set host [lindex $host 0]
		     }
		   }
	    OID    { set oid $v }
	    *      { lappend args [list $a $v] }
	}
    }

    LoadMib $oid

    set o ""
    foreach o1 [split $oid .] {
	if [string length $o] {
	    set o "$o.$o1"
	} else {
            set o $o1
	}
	LoadMib $o
    }

    switch $action {
	GETHOST {
	    GetHost $oid $host
	}
	WELCOME {
	    Welcome $oid $host
	}
	WALK {
	    Walk $oid $host
	}
	default {
	    Browse $oid $host
	}
    }
}

exit
