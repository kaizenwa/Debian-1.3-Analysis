# Noughts and Crosses v.2
#
# by Keith McAlpine
#
# A simple game containing the basics for environmentally-aware software
#
# New version to run under GroupKit 3.1 properly


# ------- Utility routine to send a msg to all participants except a list of exceptions -------------------
proc gk_toAllExcept { exceptions args } {
	set all [concat [users keys remote] [users local.usernum]]
	foreach i $exceptions {
		set idx [lsearch $all $i]
		catch { set all [lreplace $all $idx $idx] } irrelevantMsg
	}

	foreach i $all { gk_toUserNum $i eval $args }
}


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Basic initialisation routines
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# ------- Initialise conference application ---------------------------------------------------------------
gk_newenv tempenv
tempenv import data $argv
set thisconfname [tempenv data.confname]

tempenv destroy


gk_initConf $argv
# gk_initializeTelepointers -- telepointers don't seem to work reliably (on a 1 user linux box, anyway!)

# ------- Create the game environment ---------------------------------------------------------------------
# The noughts and crosses world is stored in an environment variable called envXOWorld which is shared
# amongst all the conference participants.  It contains the board data structure, and a list of what
# the various participants can do.
#
# Another environment variable holds the local user's view on the world.  In this case, different users
# may have different game board sizes.

gk_newenv -bind -share envXOWorld
gk_newenv -bind envXOView

envXOView set lastX	-1
envXOView set lastY	-1

# ------- A global array for the collaborators menu -------------------------------------------------------
# xoMenu(gametype & turn & x & o & mediator)

# ------- Constants for menu positions of menu items ------------------------------------------------------
set kMenuPos(x)		5
set kMenuPos(o)		6
set kMenuPos(med)	7
set kMenuPos(state)	8


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Participant Join/Leave routines
# This code is designed so that the application can adapt to its surroundings
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# ------- When somebody joins the conference, change our state to be a 1-2-3 player game ------------------
proc TheWorldGrows { usernum } {

	# we need to ensure that a new user only gets joined once
	if { [envXOWorld players.x] == "" } { return }
	set playerList [concat [envXOWorld players.watching] [envXOWorld players.x]			\
			[envXOWorld players.o] [envXOWorld players.mediator]]
	if { [lsearch $playerList $usernum] != -1 } { return }

	if { [envXOWorld gametype] == "single-user" } {
		# we've moved from a single-user game to a two-player game.
		ClearHighlight -1 -1
		if { [envXOWorld turn] == "X" } {
			envXOWorld set players.x $usernum
		} else {
			# if the game is over, still make the new player O by default
			envXOWorld set players.o $usernum
		}
		envXOWorld set players.mediator [envXOWorld players.x]
		envXOWorld set gametype two-player
	} elseif { [envXOWorld gametype] == "two-player" } {
		# the new member is the mediator for the game
		envXOWorld set players.mediator $usernum
		envXOWorld set gametype mediated
	} else {
		# all other arrivals can simply watch the game
		envXOWorld set players.watching [concat [envXOWorld players.watching] $usernum]
	}
}


# ------- When somebody leaves the conference, get our world recalculated to allow for their departure ----
proc TheWorldShrinks { usernum } {

	# We only want this code called by ONE of the other conference members so set up a variable that
	# will be either blank or irrelevant (eg a past user) and use as a flag to determine if this
	# recalculation procedure has already been carried out elsewhere
	if { [envXOWorld deletedUser] == $usernum } { return }

	envXOWorld set deletedUser $usernum

	# Find out what kind of player the departing user was
	set whatDeparterWas [WhatAmI $usernum]
	switch $whatDeparterWas										\
	x		{ set otherPlayer o }								\
	o		{ set otherPlayer x }								\
	watching	{
			  # the departer was only watching, so just needs to be removed from the watcher
			  # list (he doesn't affect the actual game in progress)
			  set i [lsearch [envXOWorld players.watching] $usernum]
			  envXOWorld set players.watching [lreplace [envXOWorld players.watching] $i $i]
			  return
			}


	# Let's find a replacement player/mediator if necessary
	set mediatorReplacement ""
	if {[envXOWorld gametype] == "mediated" } { set mediatorReplacement [envXOWorld players.mediator] }
	set watchingReplacement [lindex [envXOWorld players.watching] 0]

	# Re-adjust our game world depending on who is available in the world, and who has left
	if { ($mediatorReplacement == "") && ($watchingReplacement == "") } {
		# there are no replacement mediators or watchers, so we'll need to adjust ourselves
		# to be a 'lesser' application if necessary

		if { $whatDeparterWas == "mediator" } {
			# we need to become a two-player game, with no independent mediator
			envXOWorld set players.mediator [envXOWorld players.x]
			envXOWorld set gametype two-player
		} else {
			# we need to become a one-player game, as the departer was a player
			envXOWorld set players.$whatDeparterWas [envXOWorld players.$otherPlayer]
			envXOWorld set players.mediator [envXOWorld players.x]
			envXOWorld set gametype single-user
		}
	} else {
		# We've got a spare watcher, or we're in a mediated game, so the game state may not change
		if { $watchingReplacement != "" } {
			# There's a spare watcher, so change them into whatever the departer is and take
			# the watcher off the watching list
			envXOWorld set players.$whatDeparterWas $watchingReplacement
			envXOWorld set players.watching [lrange [envXOWorld players.watching] 1 end]
		} else {
			# There's only the mediator to take the place of the departing player, so the
			# game changes into a two-player game
			envXOWorld set players.$whatDeparterWas $mediatorReplacement
			envXOWorld set players.mediator [envXOWorld players.x]
			envXOWorld set gametype two-player
		}
	}		
}


# ------- A new user needs their settings initialised -----------------------------------------------------
proc SortMeOut {} {	

	# This procedure is called from an envReceived event when the envXOWorld environment gets copied to
	# the joining member process.  This is in preference to an 'updateEntrant' event, as there's no
	# other info to send apart from the environment data.

	for {set i 0} {$i < 3} {incr i} {
		for {set j 0} {$j < 3} {incr j} {
			AllowedToPlaceTile $i $j [envXOWorld board.$i.$j]
		}
	}

	# if the game is over, we need to get the winning line highlighted (force variable to update)
	if { [envXOWorld turn] == "gameover" } {
		envXOWorld set gameover.winner [envXOWorld gameover.winner]
	}

	# get the menus kicked into life
	envXOWorld set players.mediator [envXOWorld players.mediator]
	envXOWorld set players.x [envXOWorld players.x]
	envXOWorld set players.o [envXOWorld players.o]
	envXOWorld set players.watching [envXOWorld players.watching]
	envXOWorld set gametype [envXOWorld gametype]
	envXOWorld set turn [envXOWorld turn]
}


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Code relating to the local view
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# ------- Given an x/y coordinate, return its bounding box ------------------------------------------------
proc CalcCoords { i j slack width } {
	set leftx [expr ($i * $width) + $slack + 4]
	set rightx [expr $leftx + $width - ($slack * 2) - 4]
	set topy [expr ($j * $width) + $slack + 4]
	set bottomy [expr $topy + $width - ($slack * 2) - 4]

	return "$leftx $topy $rightx $bottomy"
}


# ------- Place an X at position i,j ----------------------------------------------------------------------
proc PlaceX { i j colour } {
	set coords [CalcCoords $i $j 8 [envXOView boardSize]]
	eval " .board.c create line $coords -width 6 -fill $colour -tags ShapeAt$i.$j "
	.board.c create line [lindex $coords 0] [lindex $coords 3] [lindex $coords 2]			\
			[lindex $coords 1] -width 6 -fill $colour -tags ShapeAt$i.$j
}


# ------- Place an O at position i,j ----------------------------------------------------------------------
proc PlaceO { i j colour } {
	eval " .board.c create oval [CalcCoords $i $j 8 [envXOView boardSize]]				\
			-width 6 -outline $colour -tags ShapeAt$i.$j "
}


# ------- Draw an X or O, set up out view environment afterwards ------------------------------------------
proc DoPlaceTile { i j type colour } {
	.board.c delete "ShapeAt$i.$j"

	if { $type != "blank" } {
		eval "Place$type $i $j $colour"
		envXOView set lastX $i
		envXOView set lastY $j
	}
}


# ------- If we can go, put either an X or O at position i,j ----------------------------------------------
proc AllowedToPlaceTile { i j type } {
	DoPlaceTile $i $j $type black
}


# ------- See if we can do a tentative move on the board --------------------------------------------------
proc Highlight { i j } {
	ClearHighlight $i $j
	if { ([MyTurn [users local.usernum]] == 1) && ([envXOWorld board.$i.$j] == "blank") } {
		DoPlaceTile $i $j [envXOWorld turn] [userprefs color]
	}
}


# ------- Clear any previous highlights which may be on the board -----------------------------------------
proc ClearHighlight { i j } {
	set lastx [envXOView lastX]
	set lasty [envXOView lastY]
	if { (($lastx != $i) || ($lasty != $j)) && (($lastx != -1) && ($lasty != -1)) &&		\
		([envXOWorld board.$lastx.$lasty] == "blank") } {
		.board.c delete "ShapeAt$lastx.$lasty"
	}
}


# ------- Make the game board -----------------------------------------------------------------------------
proc MakeBoard {} {
	set width [envXOView boardSize]

	catch { destroy .board } irrelevantMsg
	pack forget .label

	frame .board -width [expr $width*3] -height [expr $width*3] -relief groove
	canvas .board.c -width [expr $width*3] -height [expr $width*3]

	pack .menubar .board .label -side top -fill x
	pack .board.c -in .board

	for {set i 0} {$i < 3} {incr i} {
		for {set j 0} {$j < 3} {incr j} {
			eval " .board.c create rectangle [CalcCoords $i $j 0 $width]			\
				-outline blue -fill cyan -width 4 -tags {place$i,$j square$i,$j}"
			.board.c bind place$i,$j <Enter> "Highlight $i $j"
			.board.c bind place$i,$j <1> "PlaceTile $i $j"
		}
	}

	bind .board.c <Leave> "ClearHighlight -1 -1"
}


# ------- Game over condition, so get the winning squares highlighted -------------------------------------
proc GameOver { turnLetter winningSquares } {
	if { $turnLetter == "O" } { set config -outline } else { set config -fill }
	foreach i $winningSquares {
		.board.c itemconfigure ShapeAt$i $config white
	}
}


# ------- Callback from mouse press.  See if we can place a tile at the square ----------------------------
proc PlaceTile { i j } {
	gk_toUserNum [envXOWorld players.mediator] RequestToPlaceTile $i $j [users local.usernum]
}


# ------- Print a message at the bottom of the display saying the state of the game -----------------------
proc SetStateMsg {} {
	if { [envXOWorld turn] == "gameover" } {
		if { [envXOWorld gameover.winner] == "draw" } { set msg "Game Over - A Draw!"
		} else { set msg "Game Over - [envXOWorld gameover.winner] won." }
	} else {
		set whatAmI [WhatAmI [users local.usernum]]

		if { [MyTurn [users local.usernum]] == 1 } {
			if { [envXOWorld turn] == "O" } { set whatAmI o }   ; # fudge for single-user game
			set gamestate "and it's my turn"
		} elseif { ($whatAmI == "x") || ($whatAmI == "o") } {
			set gamestate "and it's not my go"
		} else {
			set gamestate "so I don't play"
		}
		set msg "I'm $whatAmI, $gamestate"
	}

	.label configure -text "$msg"
}


# ------- If the game is over, pop up a dialog saying that ------------------------------------------------
proc GameOverMsg { msg } {
	catch { destroy .gameover } irrelevantMsg

	toplevel .gameover
	message .gameover.msg -width 5c -justify center -relief raised -bd 2 -text $msg			\
			-font -Adobe-Helvetica-Medium-R-Normal--*-180-*
	button .gameover.dismiss -text "Dismiss" -command {destroy .gameover}
	button .gameover.restart -text "New Game" -command {
		NewGame
		gk_toAll catch { destroy .gameover } irrelevantMsg
	}

	# doesn't work??? gk_specializeWidgetTreeTelepointer .gameover

	pack .gameover.msg -side top
	pack .gameover.dismiss .gameover.restart -side left -padx 2m -pady 2m
}


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Mediator code
# It is possible to factor off this portion of the code to a separate application, so that the game is
# unable to be played unless 'somebody' plays as the mediator.  The mediator knows the rules, and knows
# when somebody has won the game
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# ------- Code that sees if a person can place a tile and, if they can, places it and checks for a game ---
# ------- over situation ----------------------------------------------------------------------------------
proc RequestToPlaceTile { i j requestor } {
	if { ([MyTurn $requestor] == 1) && ([envXOWorld board.$i.$j] == "blank") } {
		set turnLetter [envXOWorld turn]
		envXOWorld set board.$i.$j $turnLetter

		# find out if the game is over.  If it is, the apps will be told through the environment
		if { [CheckGameOver $turnLetter $requestor] == 0 } {
			if { $turnLetter == "X" } {
				envXOWorld set turn O
			} elseif { $turnLetter == "O" } {
				envXOWorld set turn X
			}
		}
	}
}


# ------- There's only 8 possible winning combinations, so see if we've got any of them -------------------
proc CheckGameOver { player playerNum } {
	set gameDrawed 1
	foreach i {{0.0 0.1 0.2} {1.0 1.1 1.2} {2.0 2.1 2.2} {0.0 1.0 2.0} {0.1 1.1 2.1} {0.2 1.2 2.2}	\
		   {0.0 1.1 2.2} {0.2 1.1 2.0}} {

		set done 1
		foreach item $i {
			if { [envXOWorld board.$item] != $player } { set done 0 }
			if { [envXOWorld board.$item] == "blank" } { set gameDrawed 0 }
		}

		# if the game is over, set up our game over condition variables
		if { $done == 1 } {
			envXOWorld set turn gameover
			envXOWorld set gameover.winningSquares $i
			envXOWorld set gameover.winner $player

			# fire a message to the loser, one to the winner, and one to everybody else
			if { $player == "X" } {
				set opp [envXOWorld players.o] } else { set opp [envXOWorld players.x] }
			gk_toUserNum $opp GameOverMsg "Ya Boo - You Lost!!"
			gk_toUserNum $playerNum GameOverMsg "YOU WON!"
			gk_toAllExcept [list $playerNum $opp] GameOverMsg "$player won."
			return 1
		 }
	}

	# if there's no more empty spaces, the game is a draw
	if { $gameDrawed == 1 } {
		envXOWorld set turn gameover
		envXOWorld set gameover.winningSquares {}
		envXOWorld set gameover.winner draw

		gk_toAll GameOverMsg "It's a Draw"
		return 1
	}

	return 0
}


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Miscellaneous routines
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# ------- Return a description of what user ?? is ---------------------------------------------------------
proc WhatAmI { usernum } {
	if { [envXOWorld players.x] == $usernum } { return "x"
	} elseif { [envXOWorld players.o] == $usernum } { return "o"
	} elseif { [envXOWorld players.mediator] == $usernum } { return "mediator"
	} else { return "watching" }
}


# ------- Return 1/0 if it is my turn or not --------------------------------------------------------------
proc MyTurn { me } {
	if { (([envXOWorld turn] == "X") && ([envXOWorld players.x] == $me)) ||
	     (([envXOWorld turn] == "O") && ([envXOWorld players.o] == $me)) } {
		return 1
	}
	return 0
}


# ------- Initialise the environments for a new game ------------------------------------------------------
proc NewGame {} {
	envXOWorld set turn X

	# Blank out the board elements
	for {set i 0} {$i < 3} {incr i} {
		for {set j 0} {$j < 3} {incr j} {
			envXOWorld set board.$i.$j blank
		}
	}
}


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Bindings
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# ------- Set up our bindings for entering and leaving conferences ----------------------------------------
gk_bind newUserArrived	"TheWorldGrows %U"
gk_bind userDeleted	"TheWorldShrinks %U"
# gk_bind updateEntrant	"SortMeOut %U"		; # not needed with auto-sharing environments


# ------- Set up our bindings for manipulating the view ---------------------------------------------------
proc EnvXOBindings { key } {
	global xoMenu	; # the last three switch items are to get the menus updated as required

	set variable [split $key .]

	switch [lindex $variable 0]									\
	board {
		# the board has changed, so get our display updated to allow for it
		AllowedToPlaceTile [lindex $variable 1] [lindex $variable 2] [envXOWorld $key]
	}												\

	gameover {
		if { [lindex $variable 1] == "winner" } {
			# the game is over, so get the winning run highlighted
			GameOver [envXOWorld gameover.winner] [envXOWorld gameover.winningSquares]
			SetStateMsg
		}
	}												\

	boardSize {
		# resize the board as it has changed
		MakeBoard
		SortMeOut
	}												\

	gametype { set xoMenu(gametype) [envXOWorld gametype] }						\
	players  {
		   if { [lindex $variable 1] != "watching" } {
		     set xoMenu([lindex $variable 1]) [expr [envXOWorld $key] == [users local.usernum]]
		     after 1000 "RebuildMenu [lindex $variable 1] [expr [envXOWorld $key]]"
		   } else {
			 after 1000 "RebuildMenu watching [list [envXOWorld $key]]"
		   }
		   SetStateMsg	}									\
	turn	 { set xoMenu(turn) [envXOWorld turn]
		   SetStateMsg	}			
}

envXOWorld bind changeEnvInfo	{ EnvXOBindings %K }
envXOWorld bind addEnvInfo	{ EnvXOBindings %K }
envXOView  bind changeEnvInfo	{ EnvXOBindings %K }
envXOView  bind addEnvInfo	{ EnvXOBindings %K }

# If we are joining a conference, all the info we need will be automatically copied to our environment.
# We just need to get the screen display forced to update instead
envXOWorld bind envReceived	{ SortMeOut }


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Basic screen display with menus
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ------- Return the username based on the usernum id.  It's possible that the environment detailing it ---
# ------- hasn't been set yet, so return an ERROR msg if the username does not (yet) exist ----------------
proc MakeMsgWithUserName { premsg usernum postmsg } {
	set user [users remote.$usernum.username]
	if { $user == "" } {
		if { [users local.usernum] == $usernum } {
			set user [users local.username]
		} else {
			# the user has not been defined yet, so return an error.  This can be picked up
			# by the calling procedure and dealt with accordingly
			return "**ERROR**"
		}
	}
	return "$premsg$user$postmsg"
}

# ------- Build up half of the menu containing the game state ---------------------------------------------
proc RebuildMenu { what usernum } {
	global kMenuPos
	set usr [MakeMsgWithUserName "" $usernum ""]
	if { ($usr == "**ERROR**") && ($what != "watching") } {
		after 2000 RebuildMenu $what $usernum
	} else {
	switch $what											\
	x	 { .menubar.collaboration.menu entryconfigure $kMenuPos(x) -label "$usr is X"	}	\
	o	 { .menubar.collaboration.menu entryconfigure $kMenuPos(o) -label "$usr is O" }		\
	mediator { .menubar.collaboration.menu entryconfigure $kMenuPos(med) -label "$usr is Mediator" } \
	watching {	
			set numWatchers [llength $usernum]
			set msg "Watching is "
			if { $numWatchers > 1 } {
				for { set i 0 } { $i < [expr $numWatchers -1] } { incr i } {
					set msg [MakeMsgWithUserName $msg [lindex $usernum $i] ", "]
					if {$msg == "**ERROR**"} {after 2000 SortMeOut; return}
				}
				set msg "$msg and "
			}
			set m2 "Nobody else watching"
			if { $numWatchers != 0 } {
			     set m2 [MakeMsgWithUserName $msg [lindex $usernum [expr $numWatchers -1]] ""]
			     if {$m2 == "**ERROR**"} {after 2000 RebuildMenu $what [list $usernum]; return}
			}
			.menubar.collaboration.menu entryconfigure $kMenuPos(state) -label "$m2"
		 }
	}				
}


# ------- Create the basic HCI display with menubar along the top -----------------------------------------
wm title . "$thisconfname ([users local.username])"

gk_defaultMenu .menubar
label .label -text "Noughts and Crosses" -font -b&h-lucida-medium-i-normal-sans-*-100-*

.menubar.file.menu insert 0 cascade -label "Board" -menu .menubar.file.menu.width
.menubar.file.menu insert 2 command -label "New Game" -command NewGame

menu .menubar.file.menu.width
for {set i 25} {$i < 150} {incr i 25} {
	.menubar.file.menu.width add radiobutton -label "Size $i" -command "envXOView set boardSize $i"
}

.menubar.collaboration.menu configure -tearoff 0  ; # don't make this a tear-off, otherwise updates wrong
.menubar.collaboration.menu add separator
.menubar.collaboration.menu add radiobutton -label "It's Xs turn" 				\
		-variable xoMenu(turn) -value X -state disabled
.menubar.collaboration.menu add radiobutton -label "It's Os turn"					\
		-variable xoMenu(turn) -value O -state disabled
.menubar.collaboration.menu add separator
.menubar.collaboration.menu add checkbutton -label "X?" -variable xoMenu(x) -state disabled
.menubar.collaboration.menu add checkbutton -label "O?" -variable xoMenu(o) -state disabled
.menubar.collaboration.menu add checkbutton -label "Mediator?" -variable xoMenu(mediator) -state disabled
.menubar.collaboration.menu add command -label "Watching?" -state disabled
.menubar.collaboration.menu add separator
.menubar.collaboration.menu add radiobutton -label "Single-user game"					\
		-variable xoMenu(gametype) -value single-user -state disabled
.menubar.collaboration.menu add radiobutton -label "Two-player game"					\
		-variable xoMenu(gametype) -value two-player -state disabled
.menubar.collaboration.menu add radiobutton -label "Mediated two-player"				\
		-variable xoMenu(gametype) -value mediated -state disabled


# ------- Set up our help message -------------------------------------------------------------------------
set help_title "About Noughts & Crosses"
set help_text {
{normal} {A simple game of tic-tac-toe for groups of 1 or more!

The game shows the basic concept of } {normalitalic} {aware} {normal} { software in that each node knows \
who is present and adjusts itself accordingly.  Each node can be:

}
{normalitalic} {Playing X, O and mediating in a 1 player game.
Playing X or O, and mediating in a 2 player game.
Playing X, O, or mediating in a mediated game.
Watching a game.

}
{normal} {As users enter and leave the world, so the roles of the other players change.

by Keith McAlpine (mcalpink@uk.ac.aston)

}
{normalitalic} {This version also uses shared environments and views as in GroupKit 3.1}
}

.menubar itemcommand help add command -label "$help_title"						\
	-command "gk_topicWindow .helpWindow -title {$help_title} -text {$help_text} -width 61 -height 16"


# ------- Initialise the game if we're the first in the conference ----------------------------------------
if [gk_amOriginator] {
	envXOWorld set players.x	[users local.usernum]
	envXOWorld set players.o	[envXOWorld players.x]
	envXOWorld set players.mediator	[envXOWorld players.x]
	envXOWorld set players.watching	{}
	envXOWorld set gametype		single-user
	envXOWorld set deletedUser	-1
	NewGame
}
RebuildMenu "watching" "[envXOWorld players.watching]" 
.menubar.file.menu.width invoke 3	; # this gets the board drawn for the first time

