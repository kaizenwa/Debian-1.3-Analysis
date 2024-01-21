proc About {} {
    return "Testing portable filename handling"
}

# This is mainly an API testing.
#
#
proc Test {} {
    global tixPriv

    if ![info exists tixPriv(isWindows)] {
	# Unix
	#
	set home [glob ~]
	if {$home == "/"} {
	    set homeprefix {}
	} else {
	    set homeprefix $home
	}

	# it shouldn't do filename substitution
	#
	Assert {[tixFileIntName *] == "*"}
	Assert {[tixFileIntName ~/*] == "$homeprefix/*"}

	Assert {[tixFileIntName /home/ioi/../foo/bar/..] == "/home/foo"}

    }

}


## MAIN_BEGIN

source ../library/TestLib.tcl
Test
Done

## MAIN_END
