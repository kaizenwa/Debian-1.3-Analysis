#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

if {$argv == {}} {
    puts "usage: dosstrip.tcl \[-n\] file"
    exit 1
}

if {[llength $argv] > 1 && [lindex $argv 0] == "-n"} {
    set test 1
    set files [lrange $argv 1 end]
} else {
    set test 0
    set files $argv
}

foreach file $files {
    if [catch {set data [exec cat $file]}] {
	puts stderr "Cannot open $file"
	continue
    }
    set ctrlM [format %s \r]
    if {[regsub -all $ctrlM $data "" data]} {
	if {$test} {
	    puts "$file contains ^M"
	} else {
	    set fd [open $file {WRONLY TRUNC}]
	    puts $fd $data
	    close $fd
	}
    } else {
	if {$test} {
#	    puts "$file does not contains ^M"
	} 
    }
}
