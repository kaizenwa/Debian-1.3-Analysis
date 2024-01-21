#!/usr/local/bin/tclsh

foreach file [lsort [glob -nocomplain *.1 *.3 *.n]] {
    if ![file exists [file rootname $file].html] {
	puts $file doesn't have html file.
    }
}
