# url.tcl,v 1.4 1995/11/17 00:42:14 steve Exp
#
#	PASTIME Project
#	Cooperative Research Centre for Advanced Computational Systems
#	COPYRIGHT NOTICE AND DISCLAIMER.
#
#	Copyright (c) 1995 ANU and CSIRO
#	on behalf of the participants in
#	the CRC for Advanced Computational Systems (ACSys)
#
# This software and all associated data and documentation ("Software")
# was developed for research purposes and ACSys does not warrant that 
# it is error free or fit for any purpose.  ACSys disclaims all liability
# for all claims, expenses, losses, damages and costs any user may incur 
# as a result of using, copying or modifying the Software.
#
# You may make copies of the Software but you must include all of this
# notice on any copy.
###
# url.tcl
#
# Routines for parsing and loading a URL
#
# All procedures are prefixed by "URL"



# Parsing a URL

# This is a more tolerant version.  It returns empty strings for pieces it cannot find.
# It is then up to the caller to decide whether the URL was valid.

proc URL_parse {url} {
    set protocol {}
    set host {}
    set port {}
    set path {}
    set key {}
    set name {}

    # Extract out the components piece-by-piece, ignoring errors
    regexp {^([^:]*):} $url all protocol
    regexp {^[^:]*:(//)?([^/:]*)(/|:)?} $url all h host p
    regexp {^[^:]*://[^:]*:([^/]*)/} $url all port
    regexp {^[^/]*//[^/]*(/[^#?]*)} $url all path
    regexp {^[^#]*#(.*)} $url all name
    regexp {^[^?]*\?(.*)} $url all key

    return [list $protocol $host $port $path $key $name]
}

# Construct URI's, etc, from the pieces

# Reconstitute the path
proc URL_makeURIpath {path {key {}}} {
    if {[set what $path] == {}} {set what /}
    if {$key != {}} {
	append what "?" $key
    }
    return $what
}

proc URL_makeURI {protocol host port path {key {}}} {
    # Check arguments
    if {($protocol == "http" || $protocol == "ftp" || $protocol == "file") && \
	$path == {}} {set path /}
    if {$protocol == "file"} {return $protocol://$path}
#   if {$protocol == {} || $host == {} || $path == {}} {return {}}

    if {$port != {}} {set port ":$port"}
    if {$key != {} && ![string match {\?*} $key] && ![string match #* $key]} \
	{set key "?$key"}
    return $protocol://$host$port$path$key
}

# canonicalise takes a URL and a URL specifying a possibly relative 
# reference and returns an absolute URL for the referred URI
proc URL_canonicalise {base rel} {
    if {[regexp {^[^:]*://} $rel]} {return $rel} ;# rel was absolute after all
    if {[string match /* $rel]} {
	set s [split $base /]
	return "[lindex $s 0]//[lindex $s 2]$rel"
    }
##    lassign [URL_parse $base] protocol host port path key name
    
    set foo [URL_parse $base]
    set protocol [lindex $foo 0]
    set host [lindex $foo 1]
    set port [lindex $foo 2]
    set path [lindex $foo 3]
    set key [lindex $foo 4]
    set name [lindex $foo 5]

    if {[string match #* $rel]} {
	# Special case: relative destination anchor
	return [URL_makeURI $protocol $host $port $path $key$rel]
    } elseif {[string match {\?*} $rel]} {
	# Special case: relative search query
	return [URL_makeURI $protocol $host $port $path $rel]
    } else {
	if {$port != ""} {set port ":$port"}
	if {$path != "/"} {
	    set lastchar [expr [string length $path]-1]
	    set last [string range $path $lastchar $lastchar]
	    if {$last!="/"} {
		set path [file dirname $path]
	    } else {
		set path [string range $path 0 [expr $lastchar-1]]
	    }
	} else {set path ""}
	return "$protocol://$host$port$path/$rel"
    }
}

# urltype checks whether a given URL is absolute or relative
proc URL_type {a} {
    if {[regexp -nocase :// $a]} {return abs} else {return rel}
}

