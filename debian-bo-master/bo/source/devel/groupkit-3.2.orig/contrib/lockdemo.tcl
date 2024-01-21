# simple demo of locking features

gk_initConf $argv
pack [gk_defaultMenu .menubar] -side top -fill x

pack [button .lock -text "Request Lock" -command lock_request] -side top
pack [button .delay -text "Set Delay" -command set_delay] -side top
pack [canvas .c]
set id [.c create rectangle 100 100 120 120 -fill red]
.c bind $id <1> "startDrag $id %x %y"
.c bind $id <B1-Motion> "continueDrag $id %x %y"

gk_initializeTelepointers
gk_specializeWidgetTreeTelepointer .c

## Create the lock manager.
gk_lockmanager locks

proc startDrag {id x y} {
    global drag
    set drag(x) [expr $x-[lindex [.c coords $id] 0]]
    set drag(y) [expr $y-[lindex [.c coords $id] 1]]
}

proc continueDrag {id x y} {
    if {[locks owner myLock] == [users local.usernum]} {
	global drag

	set newx [expr $x-$drag(x)]
	set newy [expr $y-$drag(y)]
	gk_toAll .c coords $id $newx $newy [expr $newx+20] [expr $newy+20]
    }
}

proc lock_request {} {
    locks request myLock "lock_callback"
    .lock configure -text "Lock Pending..." -command ""
}

proc lock_callback {state} {
    if {$state=="Succeeded"} {
	.lock configure -text "Got Lock.. Press to Release" -command lock_release
    } else {
	.lock configure -text "Lock Failed.. Request Again" -command lock_request
    }
}

proc lock_release {} {
    .lock configure -text "Released.. Request Again" -command lock_request
    locks release myLock
}

proc set_delay {} {
    .delay configure -text "Reset Delay" -command reset_delay
    locks delay 5000
}

proc reset_delay {} {
    .delay configure -text "Set Delay" -command set_delay
    locks delay ""
}
