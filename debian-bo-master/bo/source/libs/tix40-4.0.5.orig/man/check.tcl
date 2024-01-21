#!/usr/local/bin/tclsh

set code 0
foreach file [lsort [glob *.n]] {
    if {![file exists [file root $file].html]} {
	puts stderr "[file root $file].html missing"
	set code 1
    }
}

exit $code
