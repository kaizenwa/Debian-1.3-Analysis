## use groupkit environments to manipulate a deck of cards on a
## table -- rules?  who needs no steenking rules!
##    original version by mark roseman

## This version by Terrence Asgar-Deen.
## This code now has the Locking layer built into it to arbitrate between
## various clients requesting access to the shared objects, in this case,
## the deck of cards.  In addition, this version of card table uses an
## optimistic locking scheme.

gk_initConf $argv
gk_defaultMenu .menubar

canvas .c -bg ForestGreen -width 500 -height 500
pack append . .menubar {top fillx} .c top

gk_initializeTelepointers
gk_specializeWidgetTreeTelepointer .c

gk_newenv -bind -share deck
gk_newenv cardinfo

# Create the lock manager.
gk_lockmanager locks

# Define a global variable that holds whether or not I currently have the lock.
set HaveLock 0

deck bind addEnvInfo {
    set pieces [split %K .]
    if {[lindex $pieces 0]=="cards"} {
	set card [lindex $pieces 1]
	set attrs [deck keys cards.$card]
	if {([member posn $attrs]) && ([member state $attrs])} {
	    createCard $card
	}
    }
}

deck bind changeEnvInfo {
    set pieces [split %K .]
    if {[lindex $pieces 0]=="cards"} {
	set card [lindex $pieces 1]
	if {[lindex $pieces 2]=="posn"} {
	    set coords [deck %K]; 
	    set x [lindex $coords 0]; set y [lindex $coords 1]
	    .c coords [cardinfo cardid.$card] $x $y
	}
	if {[lindex $pieces 2]=="state"} {
	    .c itemconfigure [cardinfo cardid.$card] -bitmap [cardBitmap $card]
	}
    }
}

proc createCard card {    
    scan [deck cards.$card.posn] "%d %d" x y
    set id [.c create bitmap $x $y -bitmap [cardBitmap $card] -background white -foreground blue]
    cardinfo cardid.$card $id  
    
    .c bind $id <1> "startDrag $card %x %y"
    .c bind $id <B1-ButtonRelease> "ReleaseLock $card"
    
    .c bind $id <B1-Motion> "continueDrag $card %x %y"
    .c bind $id <2> "flipCard $card"
    .c bind $id <3> "peekCard $card"
    .c bind $id <B3-ButtonRelease> "unpeekCard $card"
}

## This code is the callback for the lock request made in the procedure
## call startDrag.  On success, it should set a local variable to indicate
## that this client now has the lock.
proc SetLockVar {card value} {
    global HaveLock
    
    if {$value == "Succeeded"} {
	set HaveLock 1
	if {[userprefs debugevents] == 1} {
	    puts "CLIENT:  Get lock succeeded."
	}
    } else {
	set HaveLock -1

	## Update the display to the current position in ``deck''.
	set posn [deck cards.$card.posn]
	scan $posn "%d %d" x0 y0  
	.c coords [cardinfo cardid.$card] $x0 $y0

	if {[userprefs debugevents] == 1} {
	    puts "CLIENT:  Get lock failed."
	}
    }
}

## Release the lock on some card.  If this client does not own the lock,
## the command is ignored.  Conversely, if we do own the lock, it is released.
## In addition, since we are forced to hold update information locally, we
## need to propagate these changes to the other users.
proc ReleaseLock {card} {
    global HaveLock
    
    if { $HaveLock == 1 } {
	deck cards.$card.posn [cardinfo misc.posn]
    } else {
	## This code is slightly problematic.  If we do not have the lock,
	## use what is currently in the deck environment to reset the card's
	## position.  This seems ackward from an HCI perspective since two
	## cases must be handled:
	## 1.  User A has the lock.  User B grabs the same card, moves it and
	##     releases it without the lock.  It will snap back to its orginal
	##     location ALL clients, including user A.  User A now moves the
	##     card and it will snap back to the old holding position and
	##     continue as expected.
	## 2.  User A has the lock.  User B moves the card.  User A releases
	##     the card (and lock).  User B releases the card.  This works
	##     as expected.
	set posn [deck cards.$card.posn]
	scan $posn "%d %d" x0 y0  
	.c coords [cardinfo cardid.$card] $x0 $y0
    }

    set HaveLock 0
    locks release $card
}

## Callback when mouse button1 is pressed.  It will generate a lock request
## and the locking mechanism is will take the approiate actions.  Since
## this is an optimistic scheme, the screen will update, however, the deck
## environment will not be changed until the state of the lock request is
## known.
proc startDrag {card x y} {
    cardinfo misc.startx $x
    cardinfo misc.starty $y

    ## Load the current position into the cardinfo environment.  All
    ## manipulation proceeds on this data UNTIL the state of the lock
    ## request is known.
    cardinfo misc.posn [deck cards.$card.posn]
    
    # Generate a lock for the card.
    set HaveLock 0
    locks request $card "SetLockVar $card"
}

proc continueDrag {card x y} {
    global HaveLock

    ## If we don't have the lock, do nothing for drag event.
    if {$HaveLock == -1} { return }
    
    set dX [expr $x-[cardinfo misc.startx]]
    set dY [expr $y-[cardinfo misc.starty]]
    cardinfo misc.startx $x
    cardinfo misc.starty $y

    set posn [cardinfo misc.posn]
    scan $posn "%d %d" x0 y0  
    incr x0 $dX; incr y0 $dY
    
    ## Set some if $HaveLock and then begin updating deck if we have the lock.
    ## If we find say a -1, we could abort at this point and do NOTHING.
    ## Theoretically, the shared deck will fix things up for me.

    if { $HaveLock == 1 } {
	cardinfo misc.posn "$x0 $y0"
	deck cards.$card.posn [cardinfo misc.posn]
    } else {
	if { $HaveLock != -1 } {
	    .c coords [cardinfo cardid.$card] $x0 $y0

	    update idletasks
	    cardinfo misc.posn "$x0 $y0"
	}
    }	

    update idletasks
}

proc flipCard card {
    if {[deck cards.$card.state]=="up"} {
	deck cards.$card.state down
    } else {
	deck cards.$card.state up
    }
}

deck bind envReceived {
    foreach i [deck keys cards] {
	createCard $i
    }
}

proc cardBitmap card {
    global gk_library
    set bmpdir $gk_library/library/tksol-0.9
    if {[deck cards.$card.state]=="up"} {
	return @$bmpdir/cardbitmaps/$card
    } else {
	return @$bmpdir/downbitmaps/down
    }
}

proc peekCard card {
    global gk_library
    set bmpdir $gk_library/library/tksol-0.9
    set bitmap @$bmpdir/cardbitmaps/$card  
    .c itemconfigure [cardinfo cardid.$card] -bitmap $bitmap
}

proc unpeekCard card {
    .c itemconfigure [cardinfo cardid.$card] -bitmap [cardBitmap $card]
}

if {[gk_amOriginator]} {
    foreach suit {c d h s} {
	foreach card {1 2 3 4 5 6 7 8 9 10 11 12 13} {
	    set name $suit$card
	    deck cards.$name.posn "100 100"
	    deck cards.$name.state down
	}
    }
}
