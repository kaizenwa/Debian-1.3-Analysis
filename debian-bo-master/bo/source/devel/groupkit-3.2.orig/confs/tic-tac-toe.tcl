# A Tic-Tac-Toe  Game
# ====================
# Code Written & Copyrite by Saul Greenberg,
# Based on Ralph Hill's Rendezvous(tm) interface to Tic-Tac-Toe (BellCore)
# See Help descriptions below for details

# Initialize conference
gk_initConf $argv



# Help description to be added to the help menu
#----------------------------------------------

set xo(why_bother_title) "Why another Tic-Tac-Toe?"

set xo(why_bother_help) {
 {normal} {This }
 {normalbold} {tic tac toe } 
 {normal} {game was designed to mimic the one built in the }
 {normalitalic} {Rendezvous } 
 {normal} {groupware toolkit. I wanted to see\
 what it would be like to follow their specs as closely as\
 possible. As far as I can see, it does everything theirs\
 does, including:
  -floor control
  -customized views
  -replicated views
  -relaxed or selective floor control
  -simultaneous action
  -customized views
  -direct manipulation

We also added:
  -telepointers

Implementing this game was easy. As you should\
 be able to see from the code, almost all of it concerns\
 the user interface. The groupware part is straight forward.

This game was implemented by } 
 {normalitalic} {Saul Greenberg. }
 {normal} {Turn taking protocol was corrected by }
 {normalitalic} {Shannon Jaeger. }
 {normal} {The interface design is copied from }
 {normalitalic} {Ralph Hill's}
 {normal} {ACM TOCHI paper on }
 {normalbold} {Rendezvous.}
}

set xo(how_to_play_title) "Playing Tic-Tac-Toe"

set xo(how_to_play_help) {
{normalbold} {Roles. 
}
{normal} {The first player will become X, and the second will be O.\
 Other joiners will be kibbitzers who can only watch.

}

{normalbold} {Playing.
}
{normal} {You will only be allowed to move when its your turn,\
 which you do by pressing the }
 {normalitalic} {mouse left button }
 {normal} {on the desired square. If you move when it's not your\
 turn or on an occupied square, the game will beep at you.

}
 {normalbold} {New Games.
}
 {normal} {Start a new game any time by selecting the }
 {normalitalic} {New Game }
 {normal} {option in the file menu.

}

 {normalbold} {Moving The Board.
}
 {normal} {You can also drag the board around by pressing and moving the }
 {normalitalic} {mouse middle button!}
}


# Global values used by the game
#--------------------------------

# The canvas, its size, and the border to leave around the playing board
set xo(canvas)         .c
set xo(canvas_size)    250
set xo(border)         50

# The playing board, its size, where to draw the big X & O, 
set xo(board)          $xo(canvas).board
set xo(board_size)     150
set xo(graphic_border) 10

# The actual names of the playing board squares, as a row/colum number
set xo(squares)        {11 12 13 21 22 23 31 32 33}  

# The designated X, Y and Players are initially not defined.
set xo(X) -1
set xo(O) -1
set xo(kibbitzers) {}

# Values for True and false
set xo(true)           1
set xo(false)          0

# Fonts we use
set xo(bold_font)      -Adobe-times-bold-r-normal--*-240*
set xo(grey_font)      -Adobe-times-medium-i-normal--*-240*
set xo(small_font)     -Adobe-times-medium-r-normal--*-180*
set xo(help_font)      -Adobe-times-medium-r-normal--*-140*

set xo(window_title)   "Tic Tac Toe"

# The current move
set move               X 



# Proceedures
#------------
# Create the Tic-Tac-Toe board and a set of 9 buttons bound to callbacks
proc make_board {w} {global xo
    for {set i 0} {$i < 9} {set i [expr $i+1]} {
	set square [lindex $xo(squares) $i]
	set xpos($square) [expr ($i%3)*.33]
	set ypos($square) [expr ($i/3)*.33]
	button $w.$square -relief sunken -command "take_turn $w $square"
	place $w.$square -relx $xpos($square) -rely $ypos($square) \
	    -relwidth .33 -relheight .33
	bind $w.$square <Enter> "highlight $square enter"
	bind $w.$square <Leave> "highlight $square leave"
	bind_for_motion $w.$square
    }
    set xpos(space) .75
    set ypos(space) .75
}


# The type of player I am: both, playerX, playerY, or kibbitzer
proc decide_players_role {} {global xo
    if { [gk_amOriginator] } {
	set xo(X) [users local.usernum]
	set xo(O) [users local.usernum]
	kibbitzer_to_X
    } 
    decide_turn_label
    $xo(canvas) delete shape
}    

# Bind the widget so a button2 action will move the board
proc bind_for_motion {w} {
    bind $w <ButtonPress-2> {
	global lastX lastY
	set lastX %x; set lastY %y; 

    }
    bind $w <B2-Motion> {global lastX lastY xo
	$xo(canvas) move board [expr %x-$lastX] [expr %y-$lastY]
	# Don't let the board go outside the canvas
	set xy [$xo(canvas) coords board]
	set outer [expr $xo(canvas_size)-$xo(board_size)]
	set x [lindex $xy 0]
	set y [lindex $xy 1]
	if {$x < 1 } {set x 1}
	if {$x > $outer} {set x $outer}
	if {$y < 1 } {set y 1}
	if {$y > $outer} {set y $outer}
	$xo(canvas) coords board $x $y
    }
    # A hack, because for some reason some buttons change their state
    bind $w <ButtonRelease-2> {global xo
	foreach square $xo(squares) {
	    $xo(board).$square configure -state normal
	}
    }
}

#=== Boolean utility functions 

# Returns true if the square has no X or 0 in it, else false
proc square_empty {square} { global xo
    set contents [lindex [$xo(board).$square configure -text] 4]
    if {$contents == "X" || $contents == "O"} {return $xo(false)}
    return $xo(true)
}

# Returns true if its this player's move, else false
proc players_move {} { global xo move
    if { ( $move == "X" ) && ( $xo(X) == [users local.usernum] ) } {
	return $xo(true)
    } elseif { ( $move == "O" ) && ( $xo(O) == [users local.usernum] ) } \
	    { return $xo(true) } { return $xo(false) }
}

#=== Callbacks

# If its this players turn and they are over an empty square,
# display a tentative move (the italicized letter) in the square
proc highlight {square type} {global move xo
    if {![players_move] || ![square_empty $square]} {return}
    if {$type == "enter"} {
	$xo(board).$square configure -text [format " %s " $move] \
	    -font $xo(grey_font)
    } else {
	$xo(board).$square configure -text " "
    }
}

# The player has clicked into a square. If its their turn, do the move
proc take_turn {w square} {global move xo
    if {[players_move] && [square_empty $square]} {
	gk_toAll do_turn $square $move
    } else {
	puts -nonewline ""; flush stdout
    }
}

#=== Handle arrival, departure, and updating of participants 

# Update the entering participant with the state of the board
gk_bind updateEntrant "updateEntrant %U"
proc updateEntrant newUsernum { global xo

    foreach square $xo(squares) {
	set contents [lindex [$xo(board).$square configure -text] 4]
	if {$contents == "X" || $contents == "O"} {
	    gk_toUserNum $newUsernum do_turn $square $contents
	}
	gk_toUserNum $newUsernum set xo(X) $xo(X)	
	gk_toUserNum $newUsernum set xo(O) $xo(O)
	gk_toUserNum $newUsernum set xo(kibbitzers) $xo(kibbitzers)
    }
    if { $xo(X) == -1 } { 
	gk_toAll set xo(X) $newUsernum
	gk_toUserNum $newUsernum kibbitzer_to_X
    } elseif { ( $xo(O) == -1 ) || ( $xo(X) == $xo(O) ) } {
	gk_toAll set xo(O) $newUsernum
	gk_toUserNum $newUsernum kibbitzer_to_O
    } else { 
	gk_toAll lappend xo(kibbitzers) $newUsernum
    }
}

#update roles of users when one of the x or o players leaves.
gk_bind userDeleted "deleteUser %U"
proc deleteUser delUsernum {global xo
   set local [users local.usernum]

   if { ( $xo(X) != $delUsernum ) && ( $xo(O) != $delUsernum ) } { 
       set index [lsearch $xo(kibbitzers) $delUsernum]
       set xo(kibbitzers) [lreplace $xo(kibbitzers) $index $index]
       return }
  
   if { $delUsernum == $xo(O) } { 
       if { $xo(kibbitzers) != "" } { 
	   set xo(O) [lindex $xo(kibbitzers) 0]
	   set xo(kibbitzers) [lreplace $xo(kibbitzers) 0 0]
           if { $local == $xo(O) } { kibbitzer_to_O }
       } else { 
	   set xo(O) $xo(X)
           if { $local == $xo(X) } { kibbitzer_to_X }
       }
   } else { 
       if { $xo(kibbitzers) != "" } { 
	   set xo(X) [lindex $xo(kibbitzers) 0]
	   set xo(kibbitzers) [lreplace $xo(kibbitzers) 0 0]
       } else { 
	   set xo(X) $xo(O)
       }
       if { $xo(X) == $local } { kibbitzer_to_X }
   }
}

# === The Protocol: This is how the boards communicate to each other

# Set the given square to the given move on all boards
proc do_turn {square this_move} {global move xo
    $xo(board).$square configure -text $this_move \
	-font $xo(bold_font)
    if {$this_move == "X"} {set move "O"} {set move "X"}
    decide_turn_label
}

# Give the player some feedback over what they are expected to do
proc decide_turn_label {} {global xo turn_label move
    if { [lsearch -exact $xo(kibbitzers) [users local.usernum]] != -1} {
	set turn_label "'s turn. You're kibbitzing."	
    } elseif { [players_move ]}  {
	set turn_label "'s turn. That's you."
    } else {
	set turn_label "'s turn. That's them."
    }
}

# Clear all tic tac toe boards, and reset the move to an X
proc do_new_game {} {global move xo 
    foreach square $xo(squares) {
	$xo(board).$square configure -text ""
    }
    set move X
    decide_turn_label
}

# Make a kibbitzer into an "O" player
proc kibbitzer_to_O {} {global xo
    $xo(canvas) delete shape
    set top [expr $xo(canvas_size)-$xo(graphic_border)]
    set bot $xo(graphic_border)
    $xo(canvas) create oval $bot $bot $top $top -width 3m -outline Orange \
	    -tag shape
    decide_turn_label
}

# Make a kibbitzer into an "X" player
proc kibbitzer_to_X {} {global xo
    $xo(canvas) delete shape
    set top [expr $xo(canvas_size)-$xo(graphic_border)]
    set bot $xo(graphic_border)
    $xo(canvas) create line $bot $bot $top $top -width 3m -fill Orange \
	    -tag shape
    $xo(canvas) create line $top $bot $bot $top -width 3m -fill Orange \
	    -tag shape
    decide_turn_label
}

# The Main Program
#-----------------

# Decorate the window
wm title . $xo(window_title) 

# Create the default groupkit menu bar
# Add a new game option to the file menu, and help options to the Help menu
gk_defaultMenu .menubar

.menubar itemcommand 0 insert 1 command -label "New Game" \
    -command "gk_toAll do_new_game"
.menubar itemcommand 0 insert 2 separator

.menubar itemcommand 2 add command -label "$xo(how_to_play_title)" \
                          -command "gk_topicWindow .helpWindow \
                                    -title \"$xo(how_to_play_title)\" \
                                    -text \" $xo(how_to_play_help)\""
.menubar itemcommand 2 add command -label "$xo(why_bother_title)" \
                          -command "gk_topicWindow .helpWindow \
                                    -title \"$xo(why_bother_title)\" \
                                    -text \" $xo(why_bother_help)\""

# Create a background canvas, the playing board frame and board, 
# and then place the board inside the canvas
canvas $xo(canvas) -width $xo(canvas_size) -height $xo(canvas_size) -relief ridge


frame $xo(board) \
    -width $xo(board_size) -height $xo(board_size) \
    -borderwidth 2 -bg Bisque3
bind_for_motion $xo(board)

make_board $xo(board)

$xo(canvas) create window $xo(border) $xo(border) -window $xo(board) \
    -anchor nw -tag board

# Now create some labels to show whose turn and what turn it is 
label .whose_move -textvariable move       -relief ridge -font $xo(bold_font)
label .turn_label -textvariable turn_label -font $xo(small_font)

# Fit it all together
pack .menubar -side top -fill x 
pack $xo(canvas) -side top -padx 5 -pady 5
pack .whose_move -side left
pack .turn_label -side left

# And decide what the player's role is
decide_players_role

# Attach the GroupKit telepointers to the board
gk_initializeTelepointers
gk_specializeWidgetTreeTelepointer $xo(board)
