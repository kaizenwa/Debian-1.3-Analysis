if {$argc != 3} {
    puts stderr "usage: ... headers-file body-file text-flag"
    exit 1
}

set textflag [lindex $argv 2]

set hdrsfile [open [lindex $argv 0] w]

set mime ""
foreach header [SafeTcl_getheaders] {
    if {[set name [string tolower [set field [lindex $header 0]]]] \
	    == "mime-version"} {
	continue
    }
    if {[string first content- $name] == 0} {
	append mime "$field: [lindex $header 1]\n"
	continue
    }

    puts $hdrsfile "$field: [lindex $header 1]"
}
puts $hdrsfile "MIME-Version: 1.0"
if {$mime != ""} {
    puts $hdrsfile "Content-Type: application/pgp; format=mime\n"
} elseif {$textflag == "T"} {
    puts $hdrsfile "Content-Type: text/pgp\n"
} else {
    puts $hdrsfile "Content-Type: application/pgp\n"
}

close $hdrsfile


set bodyfile [open [lindex $argv 1] w]

if {$mime != ""} {
    puts $bodyfile "$mime"
}
puts -nonewline $bodyfile [SafeTcl_getbodyprop 1 value]

close $bodyfile


if {[SafeTcl_getheader Resent-To] != ""} {
    set prefix Resent-
} else {
    set prefix ""
}

set addrs ""
foreach header [list Reply-To From Sender To cc Bcc Dcc From] {
    foreach addr [SafeTcl_getaddrs [ SafeTcl_getheader $prefix$header]] {
	catch { if {[string first @ $addr] < 0} {
		    set addr [exec ali $addr]
		}
		set addr [SafeTcl_getaddrprop $addr address]
		if {[lsearch -exact $addrs $addr] < 0} {
		    lappend addrs $addr
		}
	      }
    }
}

if {[set from [SafeTcl_getheader From]] == ""} {
    set addr [id effective user]
} else {
    set addr [lindex [SafeTcl_getaddrs $from] 0]
}
catch {
    if {[string first @ $addr] < 0} {
	set addr [exec ali $addr]
    }
    set addr [SafeTcl_getaddrprop $addr address]
}
if {[lsearch -exact $addrs $addr] < 0} {
    lappend addrs $addr
}

puts stdout "$addrs -u $addr"
