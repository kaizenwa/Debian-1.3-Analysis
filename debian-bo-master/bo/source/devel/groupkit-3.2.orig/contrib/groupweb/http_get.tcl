########################################################################
##
## URL fetching code
##   - this is based on code originally by brent welch
##
########################################################################

if {[info commands "unsupported0"] == "unsupported0"} {
    rename unsupported0 copychannel
}

set http(counter) 0
proc http_uniqid {} {
    if {[info commands gk_uniqid]=="gk_uniqid"} {
	return [gk_uniqid]
    } else {
	global http
	incr http(counter)
	return $http(counter)
    }
}

proc http_get {url type desc callback} {
    global http
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

    if {$protocol!="http"} {error "not http protocol"}
    if {$port=={}} {set port 80}
    
    set id http[http_uniqid]
    set s [socket $host $port]
    set http($id::socket) $s
    set http($id::state) header
    set http($id::disposition) $type
    if {$type=="variable"} {
	set http($id::variable) $desc
	global $desc
	set $desc ""
    } elseif {$type=="file"} {
	set http($id::filedesc) [open $desc w]
    } else {
	error "type should be \"variable\" or \"file\""
    }
    set http($id::callback) $callback
    fconfigure $s -translation crlf
    puts $s "GET $path HTTP/1.0"
    puts $s "Accept: */*"
    puts $s "User-Agent: TeamRooms"
    puts $s ""
    flush $s
    fconfigure $s -translation auto
    fileevent $s readable "httpEvent $id"
}

proc httpEvent id {
    global http
    set s $http($id::socket)
    if {$http($id::state)=="header"} {
	set n [gets $s line]
	if {$n<0} {
	    eval "$http($id::callback) failed"
	    close $s
	    if {$http($id::disposition)=="file"} {
		close $http($id::filedesc)
	    }
	} elseif {$n==0} {
	    set http($id::state) body
	    if ![regexp -nocase ^text $http($id::type)] {
		fconfigure $s -translation binary
		if {$http($id::disposition)=="file"} {
			fconfigure $http($id::filedesc) -translation binary
		}
	    } else {
		fconfigure $s -translation auto
		if {$http($id::disposition)=="file"} {
			fconfigure $http($id::filedesc) -translation auto
		}
	    }
	} else {
	    if [regexp -nocase {^content-type: (.+)$} $line x type] {
		set http($id::type) $type
	    }
	}
    } else {
	if {$http($id::disposition)=="file"} {
	    set n [copychannel $s $http($id::filedesc) 4096]
	    if {($n<=0) && ([eof $s]==1)} {
		close $s
		close $http($id::filedesc)
		eval "$http($id::callback) complete"
	    } else {
		# do nothing
	    }
	} else {
	    # variable
	    set data [read $s 4096]
	    if {([string length $data]<=0) && ([eof $s]==1)} {
		close $s
		eval "$http($id::callback) complete"
	    } else {
		global $http($id::variable)
		append $http($id::variable) $data
	    }
	}
    }
}

