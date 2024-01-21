# report.tcl
#
# Bug reporting and user registration
#
# Copyright (c) 1994 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Report_Bug {} {
    global mhProfile exmh tk_version

    set draft [Mh_Path $mhProfile(draft-folder) new]
    if [catch {open $draft w} out] {
	Exmh_Status "Cannot write $draft"
	return
    }
    puts $out "To: $exmh(maintainer)"
    puts $out "Subject: exmh bug"
    puts $out "------"
    puts $out "$exmh(version)"
    catch {puts $out [exec uname -a]}
    puts $out "Tk $tk_version Tcl [info tclversion]"
    close $out
    Edit_DraftID [file tail $draft]
}
proc Report_Registration {} {
    global mhProfile exmh env

    set draft [Mh_Path $mhProfile(draft-folder) new]
    if [catch {open $draft w} out] {
	Exmh_Status "Cannot write $draft"
	return
    }
    puts $out \
"To: Brent.Welch@eng.sun.com
Subject: Register exmh user
-----
$exmh(version) $env(USER)
[exec uname -a]

>   Please register as an exmh user so I can more accurately
>   track the usage of exmh.  I will not use your email address
>   for any purpose, unless you join one of the mailing lists
>   listed below.  Any and all comments are appreciated.
>
>   If you have registered for an earlier release you need not
>   register again, unless you want to, of course.
>
>	Brent Welch - Sun Labs - Brent.Welch@eng.sun.com


Please comment on exmh:

I like exmh because...

I don't really like...
"
    close $out
    Edit_DraftID [file tail $draft]
}
proc Report_Subscribe {list what} {
    global mhProfile exmh tk_version

    set draft [Mh_Path $mhProfile(draft-folder) new]
    if [catch {open $draft w} out] {
	Exmh_Status "Cannot write $draft"
	return
    }
    puts $out "To: majordomo@sunlabs.sun.com"
    puts $out "Subject: exmh mailing list"
    puts $out "------"
    puts $out "$what $list"
    puts $out "--"
    puts $out "$exmh(version)"
    close $out
    Edit_DraftID [file tail $draft]
}

