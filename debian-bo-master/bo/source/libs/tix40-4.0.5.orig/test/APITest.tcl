# APITest.tcl --
#
# 	This file performs API testing on all the Tix widgets and commands
#
# Copyright (c) 1996, Expert Interface Technologies
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#
#

set test(init)	       ""
set info(init)         "Initialization, find out all the widget classes"
set test(test0)        "init"
set into(test0)        "Try to create each widget"
set test(config-state) "init"
set info(config-state) "Configuring -state of widgets"

set all [array names test]

set test(all)		$all
set info(all)		"All tests"


proc Test::init {requested} {
    global widCmd cmdNames auto_index

    # (1) All the Tix commands are stores in the associative array cmdNames
    #

    foreach cmd [info commands tix*] {
	if [regexp :: $cmd] {
	    continue
	}
	set cmdNames($cmd) ""
    }

    foreach name [array names auto_index "tix*::AutoLoad"] {
	if [regsub {::AutoLoad} $name "" cmd] {
	    set cmdNames($cmd) ""
	}
    }


    # (2) Find out the names of the widget creation commands
    #
    foreach cmd [lsort [array names cmdNames]] {
	if [info exists $cmd\(superClass\)] {
	    if {[set $cmd\(superClass\)] == ""} {
		continue
	    }
	}
	switch -regexp -- $cmd {
	    {(DoWhenIdle)|(:)} {
		continue
	    }
	}

	if [info exists err] {
	    unset err
	}

	auto_load $cmd
	catch {
	    if {[uplevel #0 set $cmd\(isWidget\)] == 1} {
		if {$requested} {
		    puts "Found widget class: $cmd"
		}
		set widCmd($cmd) ""
	    }
	}
    }
}

proc Test::test0 {requested} {
    global widCmd

    foreach w [lsort [array names widCmd]] {
	if {[uplevel #0 set $w\(virtual\)] == 1} {
	    continue
	}

	if {$requested} {
	    puts "Creating widget of class: $w"
	}
	$w .c

	catch {
	    destroy .c
	}
    }
}

proc Test::config-state {requested} {
    global widCmd

    foreach w [lsort [array names widCmd]] {
	if {[uplevel #0 set $w\(virtual\)] == 1} {
	    continue
	}

	$w .c
	catch {
	    pack .c
	}
	if [catch {.c config -state normal}] {
	    destroy .c
	    update
	    continue
	}

	if {$requested} {
	    puts "Testing: class $w,  configure -state"
	}

	.c config -state disabled
	update
	.c config -state normal
	update
	.c config -state disabled
	update
	.c config -state normal
	update

	destroy .c
	update
    }
}

proc Test {t {level 0} {requested 1}} {
    global test tested


    if {$level > 300} {
	error "possibly circular dependency"
    }

    set tested(none)  1

    if [info exist tested($t)] {
	return
    }
    foreach dep $test($t) {
	if {![info exists tested($dep)]} {
	    Test $dep [expr $level + 1] 0
	}
    }

    if {$t == "all"} {
	set tested($t) 1
	return
    } else {
	puts "executing test \"$t\""
	eval Test::$t $requested
	set tested($t) 1
    }
}


wm geometry . +100+100
Test $argv
puts ---------------OK---------------
exit 0
