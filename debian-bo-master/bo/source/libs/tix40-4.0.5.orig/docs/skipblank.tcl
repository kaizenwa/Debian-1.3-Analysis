#!/bin/sh
# the next line restarts using tixwish \
exec tclsh "$0" "$@"

set empty 0
while {![eof stdin]} {
    set line [gets stdin]
    if {[string trim $line] == {}} {
	if {$empty == 0} {
	    puts ""
	    set empty 1
	}
    } else {
	set empty 0
	puts $line
    }
}
      
