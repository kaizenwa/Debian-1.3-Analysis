#----------------------------------------------------------------------
# The widgets to test .... (currently not used)
#
#
#----------------------------------------------------------------------

set testapp(tix,w,normal) {
    tixButtonBox tixComboBox tixControl tixDirList tixDirTree
    tixExDirSelectBox tixExFileSelectBox tixFileSelectBox tixFileEntry
    tixLabelEntry tixLabelFrame tixNoteBook tixOptionMenu
    tixPanedWindow tixScrolledHList tixScrolledListBox
    tixScrolledTList tixScrolledText tixScrolledWindow tixSelect
    tixStdButtonBox tixTree
}
set testapp(tix,w,shell) {
    tixBalloon tixDialogShell tixExFileSelectDialog tixFileSelectDialog
    tixPopupMenu tixStdDialogShell
}
set testapp(tix,w,base) {
    tixLabelWidget 
    tixPrimitive 
    tixScrolledWidget 
    tixShell 
    tixStackWindow 
    tixVResize tixVStack tixVTree 
}
set testapp(tix,w,unsupported) {
    tixMDIMenuBar 
    tixMDIWindow 
    tixMwmClient 
    tixResizeHandle 
    tixSimpleDialog 
    tixStatusBar 
}

#----------------------------------------------------------------------
#
#	Message routines
#
#----------------------------------------------------------------------
proc UserMessage {args} {
    puts $args
}

#----------------------------------------------------------------------
#
#	General assertion and evaluation
#
#----------------------------------------------------------------------

# Assert --
#
#	Evaulates an assertion. Output error message if the assertion is false
#
proc Assert {cond} {
    uplevel 1 [list \
	if !($cond) [list \
	    error "Failed Assertion \"$cond\"\n   evaluated as [uplevel 1 subst [list $cond]]"
	] \
    ]
}

# test --
#
#	Try to evaluate a command.
#
proc test {cmd {result {}} {ret {}}} {
    if [catch {set ret [uplevel 1 $cmd]} err] {
	set done 0
	foreach r $result {
	    if [regexp $r $err] {
		puts "Passed (Error message is expected):"
		puts " command        = \"$cmd\""
		puts " expected error = \"$result\""
		puts " actual error   = $err"
		set done 1
		break
	    }
	}
	if {!$done} {
	    error $err
	}
    } else {
	puts "Passed (Execution OK):\n command = \"$cmd\""
    }
    return $ret
}

#----------------------------------------------------------------------
#
#	Mouse event emulation routines
#
#----------------------------------------------------------------------
proc GetRoot {w x y} {
    upvar X X
    upvar Y Y

    set x0 [winfo rootx $w]
    set y0 [winfo rooty $w]

    set X [expr $x0 + $x]
    set Y [expr $y0 + $y]
}

proc MouseEvent {w type x y args} {
    set tags [bindtags $w]
    GetRoot $w $x $y

    lappend args %q
    lappend args $w
    lappend args %W
    lappend args $w
    lappend args %x
    lappend args $x
    lappend args %y
    lappend args $y
    lappend args %X
    lappend args $X
    lappend args %Y
    lappend args $Y

    set found 0
    foreach t $tags {
	set cmd [string trim [bind $t $type]]

	if {$cmd != ""} {
	    set found 1
	}
	tixForEach {sub val} $args {
	    regsub -all $sub $cmd $val cmd
	}
	uplevel #0 $cmd
    }
    if {$found == 0} {
	puts "warning: widget $w has no bindings for $type"
    }
    return $found
}

proc Event-Initialize {} {
    global app

    set app(X)      -1000
    set app(Y)      -1000
    set app(curWid) {}
}

proc InWidget {w} {
    global app

    return [tixWithinWindow $w $app(X) $app(Y)]
}

proc Leave {w {x -10} {y -10} args} {
    global app

    eval MouseEvent $w <Leave> $x $y $args
}

proc B1-Leave {w {x -10} {y -10} args} {
    global app

    eval MouseEvent $w <Leave> $x $y $args
}

proc RecordRoot {w x y} {
    global app

    GetRoot $w $x $y
    set app(X) $X
    set app(Y) $Y
}

proc Enter {w {x -1} {y -1} args} {
    global app

    if {$y == -1} {
	set x [expr [winfo width  $w] / 2]
	set y [expr [winfo height $w] / 2]
    }

    if {$app(curWid) != {}} {
	Leave $app(curWid)
    }
    RecordRoot $w $x $y

    eval MouseEvent $w <Enter> $x $y $args
    set app(curWid) $w
}

proc Drag {w {x -1} {y -1} args} {
    global app

    if {$y == -1} {
	set x [expr [winfo width  $w] / 2]
	set y [expr [winfo height $w] / 2]
    }

    if {![InWidget $w]} {
	B1-Leave $w $x $y
    }

    eval MouseEvent $w <B1-Motion> $x $y $args
}

proc Release  {w {x -1} {y -1} args} {
    global app

    if {$y == -1} {
	set x [expr [winfo width  $w] / 2]
	set y [expr [winfo height $w] / 2]
    }
    eval MouseEvent $w <ButtonRelease-1> $x $y $args
}

# Assumming the button was not originally down
#
proc HoldDown {w {x -1} {y -1} args} {
    global app

    if {$y == -1} {
	set x [expr [winfo width  $w] / 2]
	set y [expr [winfo height $w] / 2]
    }
    if {![InWidget $w]} {
	Enter $w $x $y
    }
    
    if {![eval MouseEvent $w <ButtonPress-1> $x $y $args]} {
	eval MouseEvent $w <1> $x $y $args
    }
}

proc Click {w {x -1} {y -1} args} {
    global app

    if {$y == -1} {
	set x [expr [winfo width  $w] / 2]
	set y [expr [winfo height $w] / 2]
    }
    eval HoldDown $w $x $y $args
    eval MouseEvent $w <ButtonRelease-1> $x $y $args
}

proc Double {w {x -1} {y -1} args} {
    global app

    if {$y == -1} {
	set x [expr [winfo width  $w] / 2]
	set y [expr [winfo height $w] / 2]
    }
    eval MouseEvent $w <Double-1> $x $y $args
}

#----------------------------------------------------------------------
#
#			main routines
#
#----------------------------------------------------------------------

proc Done {args} {
    global env
    if {![info exists env(WAITTIME)]} {
	set env(WAITTIME) 100000
    }
    if {$args == "forced"} {
	puts "------------------------done--------------------------------"
	exit 0
    } else {
	Wait $env(WAITTIME)
    }
}

proc Wait {msecs} {
    global Test:timer
    set Test:timer 0
    after $msecs set Test:timer 1
    tkwait variable Test:timer
}

#----------------------------------------------------------------------
#
#			general initialization
#
#----------------------------------------------------------------------

# init the event emulation
#
Event-Initialize

# some window managers don't put the main window at a default place, this
# may be quite annoying for the user
#
wm geometry . +100+100

catch {
    puts "  [About]"
    puts "---------------------starting-------------------------------"
}

if [file exists pkginit.tcl] {
    source pkginit.tcl
}

if [file exists ../library/$argv-init.tcl] {
    source ../library/$argv-init.tcl
}

