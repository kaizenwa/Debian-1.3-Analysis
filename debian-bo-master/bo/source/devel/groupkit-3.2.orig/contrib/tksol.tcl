#
# TkSol - Version 0.9 Beta 1/5/94
#
# Copyright (c) 1993 by Bao Trinh.
# 
# Permission to use, copy, modify and distribute this software
# for any purpose and without fee is hereby granted, provided that the
# above copyright notice appear in all copies and that both that
# copyright notice and this permission notice appear in supporting
# documentation.
# 
# The author makes no representations about the suitability of
# this software for any purpose.  The software is provided "as is"
# without express or implied warranty.

gk_initConf $argv

###########################################################################
# Set the default values for the variables and read in user customization #
###########################################################################
proc Customize {} {

    # global array of default values
    global tksol argv
    global gk_library

    set tksol(version) 0.9;	# don't change this!
    set tksol(debug) 0
    set tksol(canvas.background) ForestGreen
    set tksol(flip) 1
    set tksol(root_directory) $gk_library/library/tksol-0.9
    # the directory where the faced up card bitmaps are stored
    set tksol(bitmap.card_directory) $tksol(root_directory)/cardbitmaps
    # the directory where the faced down card bitmaps are stored
    set tksol(bitmap.down_directory) $tksol(root_directory)/downbitmaps

    set tksol(bitmap) down;		# bitmap of faced down card
    set tksol(bitmap.foreground) Blue;	# colors of the cards when facing down
    set tksol(bitmap.background) White

    # I can't imagine why you would want anything other than black and red,
    # but just in case you do, here's your chance to customize it
    set tksol(card.blackForeground) Black
    set tksol(card.blackBackground) White
    set tksol(card.redForeground) Red
    set tksol(card.redBackground) White

    set tksol(card.width) 80;		# the padded width of the card
    set tksol(card.height) 100;		# the padded height of the card
    set tksol(card.realWidth) 72;	# the actual width of the card
    set tksol(card.realHeight) 99;	# the actual height of the card

    set tksol(stack.spacing) 10;	# spacing between card stacks
    set tksol(stack.up_spacing) 15;	# spacing between faced up cards
    set tksol(stack.down_spacing) 5;	# spacing between faced down cards
    set tksol(stack.side_spacing) 15;	# horizontal spacing between cards

    set tksol(side_margin) 15;		# left and right margins
    set tksol(top_level) 15;		# position of top level stacks
    set tksol(bottom_level) 150;	# position of bottom level stacks

    set tksol(button.font) 9x15
    set tksol(button.foreground) White
    set tksol(button.background) DeepSkyBlue1
    set tksol(button.activeForeground) Black
    set tksol(button.activeBackground) Bisque2

    # read in the user customizations
    if [file readable ~/.tksolrc] {source ~/.tksolrc}

    # parse the command line (courtesy of ical)
    while {[llength $argv] != 0} {

	set arg [lindex $argv 0]
	set argv [lrange $argv 1 end]
	set start 0

 	case $arg in {
	    "-background" {
		set tksol(canvas.background) [lindex $argv 0]
		set start 1
   	    }
	    "-bitmap" {
		set tksol(bitmap) [lindex $argv 0]
		set start 1
   	    }
	    "-card_dir" {
		set tksol(bitmap.card_directory) [lindex $argv 0]
		set start 1
   	    }
	    "-debug" {
		set tksol(debug) 1
	    }
	    "-down_dir" {
		set tksol(bitmap.down_directory) [lindex $argv 0]
		set start 1
   	    }
	    "-flip" {
		set tksol(flip) [lindex $argv 0]
		if [$tksol(flip) > 8] {set tksol(flip) 8}
		set start 1
	    }
	    "-default" {
		puts stderr "Usage: tksol \[options\]
	-background <background>	; canvas background color
	-bitmap <bitmap>]		; facedown cards bitmap
	-card_dir <card_dir>		; faceup cards directory
	-down_dir <down_dir>		; facedown cards directory
	-flip <no_card>			; number of card to flip"        
		exit -1
	    }
	}
	set argv [lrange $argv $start end]
    }
}



##########################################################
# Setup the table top by creating the canvas and bitmaps #
##########################################################
proc SetupTable {} {

    global tksol Score
    global ItemToCard

    set canvas_width [expr (7 * $tksol(card.width)) + \
			   (2 * $tksol(side_margin)) + \
			   (6 * $tksol(stack.spacing))]
    set tksol(canvas.width) $canvas_width
    # the height is, to a certain extend, really arbitrary
    set canvas_height [expr $tksol(card.height) * 4.5]
    set tksol(canvas.height) $canvas_height

    # create the canvas and the dividing line
    canvas .canvas -width $canvas_width -height $canvas_height \
	-background $tksol(canvas.background) -relief raise
    set midpt [expr ($tksol(top_level) + $tksol(bottom_level) + \
		     $tksol(card.height)) / 2]
    .canvas create line 0 $midpt $canvas_width $midpt

    # create the top level placeholders
    .canvas create rectangle $tksol(side_margin) $tksol(top_level) \
	[expr $tksol(side_margin) + $tksol(card.realWidth)] \
	[expr $tksol(top_level) + $tksol(card.realHeight)] \
	-fill white -stipple gray25 -tags {flip_Tag}
    set x [expr $tksol(side_margin) + $tksol(card.width) + \
	   $tksol(stack.spacing)]
    .canvas create rectangle $x $tksol(top_level) \
        [expr $x + $tksol(card.realWidth)] \
	[expr $tksol(top_level) + $tksol(card.realHeight)] \
	-outline {} -fill $tksol(canvas.background) -tags {unflip_Tag}
    set skip [expr $tksol(card.width) + $tksol(stack.spacing)]
    set x [expr $tksol(side_margin) + (3 * $tksol(stack.spacing)) + \
           (3 * $tksol(card.width))]
    for {set stk_no 0} {$stk_no < 4} {incr stk_no} {
	.canvas create rectangle $x $tksol(top_level) \
	    [expr $x + $tksol(card.realWidth)] \
	    [expr $tksol(top_level) + $tksol(card.realHeight)] \
	    -fill white -stipple gray25 -tags [list endspot${stk_no}_Tag]
	incr x $skip
    }
    # create the bottom level placeholders, which are invisible
    set x $tksol(side_margin)
    for {set stk_no 0} {$stk_no < 7} {incr stk_no} {
	.canvas create rectangle $x $tksol(bottom_level) \
	    [expr $x + $tksol(card.realWidth)] \
	    [expr $tksol(bottom_level) + $tksol(card.realHeight)] \
	    -outline {} -fill $tksol(canvas.background) \
	    -tags [list playspot${stk_no}_Tag]
	incr x $skip
    }

    # give the window manager some info
    wm sizefrom . user
    wm iconname . "TkSol"
    wm title . "TkSolitaire $tksol(version)"

    # now create a few buttons
    frame .button -background $tksol(canvas.background) -relief raise -bd 2
    button .button.quit -foreground $tksol(button.foreground) \
	-background $tksol(button.background) \
	-activeforeground $tksol(button.activeForeground) \
	-activebackground $tksol(button.activeBackground) \
	-text "QUIT" -font $tksol(button.font) -command "destroy ."
    button .button.deal -foreground $tksol(button.foreground) \
	-background $tksol(button.background) \
	-activeforeground $tksol(button.activeForeground) \
	-activebackground $tksol(button.activeBackground) \
	-text "DEAL" -font $tksol(button.font) -command "gk_toAll NewDeal"
    button .button.undo -foreground $tksol(button.foreground) \
	-background $tksol(button.background) \
	-activeforeground $tksol(button.activeForeground) \
	-activebackground $tksol(button.activeBackground) \
	-text "UNDO" -font $tksol(button.font) -command "gk_toAll Undo"
    button .button.finish -foreground $tksol(button.foreground) \
	-background $tksol(button.background) \
	-activeforeground $tksol(button.activeForeground) \
	-activebackground $tksol(button.activeBackground) \
	-text "FINISH" -font $tksol(button.font) -command "gk_toAll AutoFinish"

    # now create the score label
    label .button.score -width 4 -font $tksol(button.font) -text "0" \
	-textvariable Score -anchor e -relief sunken

    # now create the card bitmaps, make them invisible and temporarily
    # place them at the top left corner
    set index 0
    for {set card_no 1} {$card_no < 14} {incr card_no} {
	# create heart first, then spade, then diamond, then club
	set item [.canvas create bitmap $tksol(side_margin) $tksol(top_level) \
		  -bitmap @$tksol(bitmap.card_directory)/h${card_no} \
		  -background $tksol(canvas.background) \
		  -foreground $tksol(canvas.background) -anchor nw]
	set ItemToCard($item) h${card_no}
	incr index
	set item [.canvas create bitmap $tksol(side_margin) $tksol(top_level) \
		  -bitmap @$tksol(bitmap.card_directory)/s${card_no} \
		  -background $tksol(canvas.background) \
		  -foreground $tksol(canvas.background) -anchor nw]
	set ItemToCard($item) s${card_no}
	incr index
	set item [.canvas create bitmap $tksol(side_margin) $tksol(top_level) \
		  -bitmap @$tksol(bitmap.card_directory)/d${card_no} \
		  -background $tksol(canvas.background) \
		  -foreground $tksol(canvas.background) -anchor nw]
	set ItemToCard($item) d${card_no}
	incr index
	set item [.canvas create bitmap $tksol(side_margin) $tksol(top_level) \
		  -bitmap @$tksol(bitmap.card_directory)/c${card_no} \
		  -background $tksol(canvas.background) \
		  -foreground $tksol(canvas.background) -anchor nw]
	set ItemToCard($item) c${card_no}
	incr index
    }

    # Finally, Abracadabra ...
    pack .button.quit .button.deal -side left -anchor w
    pack .button.score -side right -anchor e
    # .button.undo is packed by NewDeal (ie. since EndGame unpack it)
    # .button.finish is packed when autofinish is enabled
    pack .button -side top -fill x
    pack .canvas
}



##############################################
# Deal the cards out onto the various stacks #
##############################################
proc DealCards {} {

    global PlayStk0 PlayStk1 PlayStk2 PlayStk3 PlayStk4 PlayStk5 PlayStk6
    global tksol
    global ItemToCard

    # find out the index of the lowest entry in the ItemToCard array
    set offset [lindex [lsort -integer [array names ItemToCard]] 0]
    set index 0
    foreach item_no [ShuffleDeck] {
	# add in offset since ItemToCard doesn't start at 0!
	set UnsortDeck($index) [expr $item_no + $offset]
	incr index
    }

    #
    # layout the cards on the table
    #

    set card_no 0
    set x $tksol(side_margin)

    for {set stk_no 0} {$stk_no < 7} {incr stk_no} {

	set y $tksol(bottom_level)
	set index 0

	# first the face down cards
	for {set down 0} {$down < $stk_no} {incr down} {
	    set item $UnsortDeck($card_no)
	    DisplayCard $item FaceDown $x $y \
		[list playstk${stk_no}_Tag down_Tag]
	    set PlayStk${stk_no}($index) $item
	    incr y $tksol(stack.down_spacing)
	    incr card_no
	    incr index
	}
	# then the single face up card
	set item $UnsortDeck($card_no)
	DisplayCard $item FaceUp $x $y [list playstk${stk_no}_Tag up_Tag]

	set PlayStk${stk_no}($index) $item
	set PlayStk${stk_no}(size) [expr $index + 1];	# index starts at 0

	incr x [expr $tksol(card.width) + $tksol(stack.spacing)]
	incr card_no
    }

    # now put the remaining cards face down on the draw_left stack
    for {} {$card_no < 52} {incr card_no} {
	DisplayCard $UnsortDeck($card_no) FaceDown $tksol(side_margin) \
	    $tksol(top_level) {drawleft_Tag down_Tag}
    }
}



######################################################
# Display a card on the table (invoked by DealCards) #
######################################################
proc DisplayCard {item state x y tag_list} {

    global tksol
    global ItemToCard

    # see if the card is face down or up
    if {$state == "FaceDown"} {
	set fgcolor $tksol(bitmap.foreground)
	set bgcolor $tksol(bitmap.background)
	set bitmap $tksol(bitmap)
	set bitmap_dir $tksol(bitmap.down_directory)
    } else {
	# find out the correct foreground and background color
	set bitmap $ItemToCard($item)
	set bitmap_dir $tksol(bitmap.card_directory)
	set suit [string index $ItemToCard($item) 0]
	if {$suit == "h" || $suit == "d"} {
	    set fgcolor $tksol(card.redForeground)
	    set bgcolor $tksol(card.redBackground)
	} else {
	    set fgcolor $tksol(card.blackForeground)
	    set bgcolor $tksol(card.blackBackground)
	}
    }
    # raise the current card above the rest because of the shuffling
    .canvas raise $item all
    # now move it to the proper position and display it
    .canvas coord $item $x $y
    .canvas itemconfig $item -bitmap @$bitmap_dir/$bitmap \
	-foreground $fgcolor -background $bgcolor -tags $tag_list
}



#######################################################
# Flip the card up and move it to the drawright stack #
#######################################################
proc MoveRight {{offset 0} {card_no 1}} {

    global tksol
    global WorkStk
    global ItemToCard

    #
    # why are we looking at flip_Tag instead of drawleft_Tag????????????
    # (see ptksol for the bug fix)
    set flip_coord [.canvas coord flip_Tag]
    set item [.canvas find closest [lindex $flip_coord 0] \
	      [lindex $flip_coord 1]]
    set card $ItemToCard($item)
    scan $card "%c%d" suit number
    set suit [format "%c" $suit]

    # the tags get reset in DisplayCard so there's no need to delete them here
    DisplayCard $item FaceUp \
	[expr ($tksol(side_margin) + $tksol(card.width) + \
	       $tksol(stack.spacing) + $offset)] \
	$tksol(top_level) {up_Tag drawright_Tag}
    PushUndoStack $item drawleft_Tag drawright_Tag [list $card_no]

    # prevent double clicking on a face down card from blowing up [bug]
    # find out if there is a better way to handle this!!!!!!!!!!!!!!!!
    # how about putting the current card on the working stack?????????
    set WorkStk(size) 0
}


##################################################################
# Flip up more than one card and move them to the drawrigh stack #
##################################################################
proc MoveRightMany {} {

    global tksol

    # first, reset the drawright stack
    set card_list [.canvas find withtag drawright_Tag]
    set coords [.canvas coord drawright_Tag]
    foreach card $card_list {
	.canvas coord $card [lindex $coords 0] [lindex $coords 1]
    }
    set how_many $tksol(flip)
    # invoke MoveRight up to "how_many" times, passing in a different 
    # offset each time
    set cards [llength [.canvas find withtag drawleft_Tag]]
    # there will be at least one because we are clicking on it ...
    set offset 0
    for {set card_no 1} {$card_no <= $how_many && $cards > 0} {incr card_no} {
	MoveRight $offset $card_no
	incr offset $tksol(stack.side_spacing)
	incr cards -1
    }
}



################################################################
# Flip the cards down and move them back to the drawleft stack #
################################################################
proc MoveLeft {} {

    global tksol

    # can we trust "find" to return a list in order? [bug]
    foreach item [.canvas find withtag drawright_Tag] {
	# note that drawleft_Tag has to come *BEFORE* down_Tag in the
	# taglist; see key binding for reasoning behind this restriction
	DisplayCard $item FaceDown $tksol(side_margin) $tksol(top_level) \
	    {drawleft_Tag down_Tag}
	# note that the above find return the bottom most card first;
	# DisplayCard does a raise which will raise the last card in
	# the foreach item list above all others; we want it to be the
	# other way around so let's invoke a "lower" here
	.canvas lower $item drawleft_Tag
    }
    # push onto the undostack a special undo command
    PushUndoStack 0 MoveLeft MoveLeft
}



#########################################################
# Flip a facedown card up if it's on top of a playstack #
#########################################################
proc FlipUp {tag_list item} { 

    global PlayStk0 PlayStk1 PlayStk2 PlayStk3 PlayStk4 PlayStk5 PlayStk6
    global WorkStk
    global AutoFinishState
    global tksol
    # DEBUG [bug]
    global ItemToCard

#    set item [.canvas find withtag current]
#    puts "FlipUp> Flipping up item $item"

    # first, find out which playstack we are located in
 #   set tag_list [.canvas gettags current]
    set tag_index [lsearch -glob $tag_list playstk?_Tag]
    if {$tag_index == -1} {	# this should not be happening!
#	puts "Error FlipUp> Can't find stack we are flipping up from!"
	return
    }
    # now see if the card is the topmost
    scan [lindex $tag_list $tag_index] "playstk%d_Tag" stk_no
    eval set top_index [expr \$PlayStk${stk_no}(size) - 1]
    eval set top_item \$PlayStk${stk_no}($top_index)
    if {$top_item == $item} {
	# find out the current location
	set item_coord [.canvas coords $item]
#	puts "Flipup: coord is $item_coord"
	DisplayCard $item FaceUp [lindex $item_coord 0] \
	    [lindex $item_coord 1] [list playstk${stk_no}_Tag up_Tag]
	PushUndoStack $item playstk${stk_no}_Tag playstk${stk_no}_Tag

	# prevent double clicking on a face down card from blowing up [bug]
	# find out if there is a better way to handle this!!!!!!!!!!!!!!!!
	# how about putting the current card on the working stack?????????
	set WorkStk(size) 0

	# if this is the last face down card on all the playstacks, allow
	# the user to autofinish
	if {!${AutoFinishState} && $tksol(flip) == 1} {
	    set down_size [llength [.canvas find withtag down_Tag]]
	    set drawleft_size [llength [.canvas find withtag drawleft_Tag]]
	    if {$down_size == $drawleft_size} {
		set AutoFinishState 1
		pack .button.finish -side left -anchor w
	    }
	}
    }
}



#############################################################################
# Automatically finish the game once all the cards have been turned face up #
#############################################################################
proc AutoFinish {} {

    global PlayStk0 PlayStk1 PlayStk2 PlayStk3 PlayStk4 PlayStk5 PlayStk6
    global EndStk0 EndStk1 EndStk2 EndStk3
    global UndoStk
    global ItemToCard

    # if this is the last face down card on all the playstacks, autofinish
    # there still may be some face down cards on the drawright stack however
    # [this is not really necessary as the button will not be packed when
    # autofinish is disabled; however, just in case ...]
    set down_size [llength [.canvas find withtag down_Tag]]
    set drawleft_size [llength [.canvas find withtag drawleft_Tag]]
    set drawright_size [llength [.canvas find withtag drawright_Tag]]
    if {$down_size != $drawleft_size} {	# nice try!
	return
    }

    # change the cursor to a watch
    .button config -cursor watch
    update

    # find out the current card number of each endstack
    set orphan_list {}
    for {set stk_no 0} {$stk_no < 4} {incr stk_no} {
	eval set suit \$EndStk${stk_no}(suit)
	if {![string match empty $suit]} {
	    eval set number \$EndStk${stk_no}(number)
	    set ${suit}(number) $number
	    set ${suit}(stack) EndStk${stk_no}
	    lappend suit_list $suit
	} else {	# mark this endstack as unassigned
	    lappend orphan_list $stk_no
	}
    }

    # the section below will be difficult to debug ... [bug]

    # assigned any orphan suit if needed
    if {[string length $orphan_list] > 0} {
	set index 0
	foreach suit {h s d c} {
	    if {![info exists ${suit}(number)]} {
		set ${suit}(number) 0
		set orphan_endstk [lindex $orphan_list $index]
		set ${suit}(stack) EndStk${orphan_endstk}
		incr index
	    }
	}
    }

    # once an empty stack, always an empty stack
    set empty_stack 0
    for {set stk_no 0} {$stk_no < 7} {incr stk_no} {
	eval set stk_size \$PlayStk${stk_no}(size)
	if {$stk_size == 0} {
	    incr empty_stack
	}
    }

    # cycle through the playstacks and drawstacks until all the cards have
    # been moved up to the endstacks; there are other ways to do this, and
    # this probably is not the optimal way ...

    set playcard [expr 52 - $h(number) - $d(number) - $s(number) - $c(number)]
    set drawright_coord [.canvas coord unflip_Tag]
    set drawright_x [lindex $drawright_coord 0]
    set drawright_y [lindex $drawright_coord 1]

    while {$playcard > 0} {

	set found_top 0
	set found_bottom init
	set topbottom 1
	set top 1
	if {$empty_stack < 7} {
	    set bottom 1
	} else { set bottom 0}

	# first scan through the playstacks and drawright stack
	while {$topbottom == 1} {

	    # move as many of the cards on the drawright stack as possible
	    while {$top == 1} {
		# move a card from the drawleft stack over if needed
		if {$drawright_size == 0} {
		    if {$drawleft_size == 0} {	# no card left on drawstacks
			break;		# get out of "top" loop
		    } else {
			MoveRight
			update idletasks
			incr drawright_size
			incr drawleft_size -1
		    }
		}
		# this _has_ to be the topmost card on the drawright stack
		set item [.canvas find closest $drawright_x $drawright_y]
		set card $ItemToCard($item)
		scan $card "%c%d" card_suit card_number
		set card_suit [format "%c" $card_suit]
		eval set suit_number \$${card_suit}(number)
		eval set endstk \$${card_suit}(stack)

		if {[ClearCard $item $card_number $suit_number \
		     drawright_Tag $endstk]} {
		    incr ${card_suit}(number)
		    incr drawright_size -1
		    set found_top 1
		    incr playcard -1
		} else {
		    break;		# get out of "top" loop
		}
	    }

	    # stop if we can't find anything at the top and this is not the
	    # first iteration of the loop, ie. we have to go into the bottom
	    # loop at least once ...
	    if {$found_top == 0 && $found_bottom != "init"} {
		break;			# get out of "topbottom" loop
	    }

	    set found_bottom 0
	    while {$bottom == 1} {

		set bottom 0
		for {set stk_no 0} {$stk_no < 7} {incr stk_no} {

		    eval set stk_size \$PlayStk${stk_no}(size)
		    if {$stk_size > 0} {
			set top_index [expr $stk_size - 1]
			eval set item \$PlayStk${stk_no}($top_index)
			set card $ItemToCard($item)
			scan $card "%c%d" card_suit card_number
			set card_suit [format "%c" $card_suit]
			eval set suit_number \$${card_suit}(number)
			eval set endstk \$${card_suit}(stack)

			if {[ClearCard $item $card_number $suit_number \
			     playstk${stk_no}_Tag $endstk]} {
				 incr ${card_suit}(number)
				 incr playcard -1
				 set found_bottom 1
				 set bottom 1
				 if {$stk_size == 1} {
				     incr empty_stack
				 }
			     }
		    }
		}
	    }

	    # stop if we can't find anything at the bottom
	    if {$found_bottom == 0} {
		break;			# get out of topbottom loop
	    }
	}

	# now flip over a card from the drawleft stack, move all the cards
	# from the drawright stack over if needed
	if {$drawleft_size > 0} {
	    MoveRight
	    update idletasks
	    incr drawright_size
	    incr drawleft_size -1
	} else {	# flip over all the drawright stack cards first
	    if {$drawright_size > 1} {
		MoveLeft
		update idletasks
		MoveRight
		update idletasks
		set temp drawleft_size
		set drawleft_size [expr $drawright_size - 1]
		set drawright_size 1
	    }
	}
    }

    # change the cursor back to a normal pointer
    .button config -cursor left_ptr

    # don't invoke EndGame here since it is done automatically when all
    # the cards have been moved up to the endstacks (see MoveWorkStack)
}



#######################################################################
# Do something graphically related to reward the player for finishing #
#######################################################################
proc EndGame {} {

    global tksol
    global UndoStk WorkStk

    # don't let the user do something strange while the endgame is running ...
    set UndoStk(top) 0
    set WorkStk(size) 0
    pack forget .button.undo .button.finish

    set beginstk [.canvas coord playspot0_Tag]
    set endstk [.canvas coord playspot6_Tag]
    set xdistance [expr [lindex $endstk 0] - [lindex $beginstk 0]]
    # note that we divide by 12, not 13, since the last card is positioned
    # on the coordinates of the last playstack, ie. playspot6
    set x_offset [expr $xdistance / 12]
    set ydistance [expr $tksol(canvas.height) - [lindex $beginstk 1]]
    set y_offset [expr $ydistance / 4]
    set x [expr int([lindex $beginstk 0])]
    set y [expr int([lindex $beginstk 1])]
    for {set endstk_no 0} {$endstk_no < 4} {incr endstk_no} {
	# layout each suit on the canvas
	.canvas raise endstk${endstk_no}_Tag all
	set card_list [.canvas find withtag endstk${endstk_no}_Tag]
	# the find returns the bottom most card first (eg. the Ace)
	for {set card [expr [llength $card_list] - 1]} \
	    {$card >= 0} \
	    {incr card -1} {
	    set card_no [lindex $card_list $card]
	    .canvas raise $card_no endstk${endstk_no}_Tag
	    .canvas coord $card_no $x $y
	    update idletasks
	    incr x [expr round(floor($x_offset))]
	}
	set x [expr int([lindex $beginstk 0])]
	incr y [expr round(floor($y_offset))]
    }

    # we may have to remove all bindings or tags so that the player won't
    # attempt to move the cards around after the endgame has concluded ...
    .canvas dtag up_Tag up_Tag
}



####################################################
# Move a card to the endstack while autofinish'ing #
####################################################
proc ClearCard {item card_number suit_number fromstk_Tag endstk} {

    global WorkStk

    set rtn_val 0
    if {$suit_number == [expr $card_number - 1]} {
	set WorkStk(size) 1
	set WorkStk(0) $item
	set WorkStk(stack) $fromstk_Tag
	.canvas addtag workstk_Tag withtag $item
	eval MoveWorkStack $endstk MoveSlide 10
	update idletasks
	set rtn_val 1
    }
    return $rtn_val
}



##################################################################
# Setup the working stack for either dragging or double clicking #
##################################################################
proc SetupWorkStack {x y} {

    global PlayStk0 PlayStk1 PlayStk2 PlayStk3 PlayStk4 PlayStk5 PlayStk6
    global EndStk0 EndStk1 EndStk2 EndStk3
    global WorkStk
    global lastX lastY oldX oldY
    global SetupWorkStk DragWorkStk
    global tksol
    # DEBUG [bug]
    global ItemToCard

    #set current_item [.canvas find withtag current]
    set current_item [.canvas find closest $x $y]
    if {$current_item == ""} {
	puts "Error SetupWorkStack: Cannot find the card at $x $y!"
	return
    }

    # first, clear the working stack
    .canvas dtag workstk_Tag workstk_Tag

    set SetupWorkStk 1;		# these are for the key release routine [bug]
    set DragWorkStk 0

    set lastX [.canvas canvasx $x]
    set lastY [.canvas canvasy $y]
    # save the current position in case we don't go anywhere lateron
    set oldX $lastX
    set oldY $lastY

    # find out which stack we are located in first
    set tag_list [.canvas gettags $current_item]
    if {[lsearch -exact $tag_list drawright_Tag] != -1} {
	set WorkStk(info) [.canvas coord $current_item]
	if {$tksol(flip) > 1} {
	    # make sure this is the topmost card on the drawright stack
	    # [bug] is there a quicker way to do this? [bug]
	    set card_list [.canvas find withtag drawright_Tag]
	    set list_length [llength $card_list]
	    if {$current_item != [lindex $card_list [expr $list_length - 1]]} {
		set SetupWorkStk 0
		return
	    }
	}
	set current_stk drawright_Tag
    } else {
	set index [lsearch -glob $tag_list playstk?_Tag]
	if {$index != -1} {
	    set current_stk [lindex $tag_list $index]
	} else {
	    set index [lsearch -glob $tag_list endstk?_Tag]
	    # no checking necessary since it has to be in an endstack
	    set current_stk [lindex $tag_list $index]
	}
    }

    set WorkStk(stack) $current_stk
    set WorkStk(0) $current_item
    set WorkStk(size) 1
    .canvas addtag workstk_Tag withtag $current_item

    # see if we have to add more items to the working stack, ie. playstack
    if {[string match playstk?_Tag $current_stk]} {
	# first find the index of the "current" item on the playstack
	scan $current_stk "playstk%d_Tag" stk_no
	eval set bound \$PlayStk${stk_no}(size)
	for {set index 0} {$index < $bound} {incr index} {
	    eval set item \$PlayStk${stk_no}($index)
	    if {$item == $current_item} {
		set WorkStk(size) [expr $bound - $index]
		incr index;	# we have already assigned the first item
		break
	    }
	}
	# now get all the cards that are on top of the "current" item
	for {set stk_index 1} {$index < $bound} {incr index; incr stk_index} {
	    eval set item \$PlayStk${stk_no}($index)
	    set WorkStk($stk_index) $item
	    .canvas addtag workstk_Tag withtag $item
	}
    }

    # note that we don't need to raise the cards in the working stack since
    # they will be raise when they are displayed or moved around ...

    if {$tksol(debug)} {
	puts stderr "\[SetupWorkStack\]"
	puts stderr "\tWorkStk(size) is $WorkStk(size)"
	for {set i 0} {$i < $WorkStk(size)} {incr i} {
	    set item $WorkStk($i)
	    puts stderr "\tWorkStk($i) is $ItemToCard($item)"
	}
	puts stderr "\n"
    }
}



#################################
# Drag the working stack around #
#################################
proc DragWorkStack {x y} {

    global lastX lastY
    global SetupWorkStk DragWorkStk
    global tksol
    # [bug]
    global WorkStk ItemToCard

    if {$SetupWorkStk == 0} {
	return
    }

#    if {$tksol(debug)} {
#	puts stderr "\[DragWorkStack\]"
#	puts stderr "\tWorkStk(size) is $WorkStk(size)"
#	for {set i 0} {$i < $WorkStk(size)} {incr i} {
#	    set item $WorkStk($i)
#	    puts stderr "\tWorkStk($i) is $ItemToCard($item)"
#	}
#	puts stderr "\n"
#    }


    set DragWorkStk 1
    set x [.canvas canvasx $x]
    set y [.canvas canvasy $y]
    .canvas raise workstk_Tag all
    .canvas move workstk_Tag [expr $x - $lastX] [expr $y - $lastY]
    set lastX $x
    set lastY $y
}


#########################################################
# Move the working stack to a new location, if possible #
#########################################################
proc ProcessWorkStack {x y} {

    global PlayStk0 PlayStk1 PlayStk2 PlayStk3 PlayStk4 PlayStk5 PlayStk6
    global EndStk0 EndStk1 EndStk2 EndStk3
    global WorkStk
    global ItemToCard
    global DragWorkStk SetupWorkStk
    global oldX oldY
    global tksol

    # ignore if we click but don't move anywhere or got invoke by double blick
    if {$DragWorkStk == 0 || $SetupWorkStk == 0} {
	set SetupWorkStk 0;		# reset [just in case]
	set DragWorkStk 0
	return
    } else {
	set DragWorkStk 0;		# reset [just in case]
	set SetupWorkStk 0
    }

    # if the working stack is empty, probably because it is explicitly set
    # by the routines that flip up a down card, then don't do anything
    if {$WorkStk(size) < 1} {
	return
    }

    if {$tksol(debug)} {
	puts stderr "\[ProcessWorkStack\]"
	puts stderr "\tWorkStk(size) is $WorkStk(size)"
	for {set i 0} {$i < $WorkStk(size)} {incr i} {
	    set item $WorkStk($i)
	    puts stderr "\tWorkStk($i) is $ItemToCard($item)"
	}
	puts stderr "\n"
    }

    #set item_no [.canvas find withtag current]
    #set item_no [.canvas find closest $x $y]

    # use the working stack to hopefully get around that silly "element
    # does not exist" error ... also makes it a wee bit faster too
    set item_no $WorkStk(0)

    set card $ItemToCard($item_no)
    scan $card "%c%d" suit number
    set suit [format "%c" $suit]

    set x [.canvas canvasx $x]
    set y [.canvas canvasx $y]
    # we could have use workstk_Tag instead of current also ...
    set workstk_coord [.canvas bbox $item_no]

    # see if we overlap any endstack, in order from left to right
    if {$WorkStk(size) == 1} {
	for {set endstk_no 0} {$endstk_no < 4} {incr endstk_no} {
	    # make sure we don't match ourselves
	    if {[string match endstk${endstk_no}_Tag $WorkStk(stack)]} {
		continue
	    }
	    set endstk_coord [.canvas bbox endspot${endstk_no}_Tag]
	    if {[StackOverlap $workstk_coord $endstk_coord]} {
		# check to see if it is the next card to go there
		eval set endstk_suit \$EndStk${endstk_no}(suit)
		eval set endstk_card \$EndStk${endstk_no}(number)
		if {$endstk_suit == $suit && \
		    $endstk_card == [expr $number - 1]} {
		    # set EndStk${endstk_no}(number) $number
		    MoveWorkStack EndStk${endstk_no} MoveJump
		    PushUndoStack $WorkStk(0) $WorkStk(stack) \
			endstk${endstk_no}_Tag $WorkStk(info)
		    return
		} elseif {$number == 1 && $endstk_suit == "empty"} {
		    # setup endstack info here, not in MoveWorkStack!
		    # Bzzzt, now done in MoveWorkStack instead!!!
		    # set EndStk${endstk_no}(suit) $suit
		    # set EndStk${endstk_no}(number) 1
		    MoveWorkStack EndStk${endstk_no} MoveJump
		    PushUndoStack $WorkStk(0) $WorkStk(stack) \
			endstk${endstk_no}_Tag $WorkStk(info)
		    return
		}
	    }
	}
    }

    # now see if we overlap any playstack, in order from left to right
    for {set playstk_no 0} {$playstk_no < 7} {incr playstk_no} {
	# make sure we don't match ourselves
	if {[string match playstk${playstk_no}_Tag $WorkStk(stack)]} {
	    continue
	}

	eval set playstk_size \$PlayStk${playstk_no}(size)
	if {$playstk_size > 0} {

	    # find the coordinate of the topmost item in the playstack
	    eval set top_index [expr \$PlayStk${playstk_no}(size) - 1]
	    eval set top_item \$PlayStk${playstk_no}($top_index)
	    set playstk_coord [.canvas bbox $top_item]

	    if {[StackOverlap $workstk_coord $playstk_coord]} {

		# see if we have both a number and color (mis)match

		set top_card $ItemToCard($top_item)
		scan $top_card "%c%d" top_suit top_number
		set top_suit [format "%c" $top_suit]

#		puts "topnumber $top_number number $number";	# [bug]
		if {$top_number == [expr $number + 1]} {

		    if {$suit == "h" || $suit == "d"} {
			set suit_color red
		    } else { set suit_color black }
		    if {$top_suit == "h" || $top_suit == "d"} {
			set topsuit_color red
		    } else { set topsuit_color black }

		    if {$suit_color != $topsuit_color} {
			MoveWorkStack PlayStk${playstk_no} MoveJump
			PushUndoStack $WorkStk(0) $WorkStk(stack) \
			    playstk${playstk_no}_Tag $WorkStk(info)
			return
		    }
		}
	    }
	} else { # see if we are dragging a King to an empty playstack
	    if {$number == 13} {
		set playstk_coord [.canvas coords playspot${playstk_no}_Tag]
		if {[StackOverlap $workstk_coord $playstk_coord]} {
		    MoveWorkStack PlayStk${playstk_no} MoveJump
		    PushUndoStack $WorkStk(0) $WorkStk(stack) \
			playstk${playstk_no}_Tag $WorkStk(info)
		    return;
		}
	    }
	}
    }


    # if we get down to here, then we can't go anywhere so warp back to
    # our original location
    set move_increment 3;		# just for the effect
    set x_increment [expr ($oldX - $x) / $move_increment]
    set y_increment [expr ($oldY - $y) / $move_increment]
    for {set move 1} {$move < $move_increment} {incr move} {
	.canvas move workstk_Tag $x_increment $y_increment
	update idletasks
    }
    set total_x [expr ($move_increment - 1) * $x_increment]
    set total_y [expr ($move_increment - 1) * $y_increment]
    .canvas move workstk_Tag [expr ($oldX - $x - $total_x)] \
	[expr $oldY - $y - $total_y]

    # do we have to do anything else to cleanup the working stack/label? [bug]
    # note that this will not mess up double click because it will abort above
    .canvas dtag workstk_Tag workstk_Tag
}



####################################################################
# See if we can move the card on the working stack to an end stack #
####################################################################
proc ClearWorkStack {x y} {

    global EndStk0 EndStk1 EndStk2 EndStk3
    global WorkStk
    global tksol
    # [bug]
    global ItemToCard

    # note that a double click will always generate a single click first,
    # so the working stack already contains the cards we are interested in
    # (except for the case where we double click on a facedown card [bug])

    if {$tksol(debug)} {
	puts stderr "\[ClearWorkStack\]"
	puts stderr "\tWorkStk(size) is $WorkStk(size)"
	for {set i 0} {$i < $WorkStk(size)} {incr i} {
	    set item $WorkStk($i)
	    puts stderr "\tWorkStk($i) is $ItemToCard($item)"
	}
	puts stderr "\n"
    }

    # if there is more than one card on the working stack, then the "current"
    # card that we double click on can't be the one on top so ignore
    if {$WorkStk(size) != 1} {
	return
    }

    #set item_no [.canvas find withtag current]
    #set item_no [.canvas find closest $x $y]
    #puts "(x,y) is $x $y"
    set item_no $WorkStk(0)
    set card $ItemToCard($item_no)
    scan $card "%c%d" suit number
    set suit [format "%c" $suit]

    # if the card is an ace, find an empty end stack and move it there
    if {$number == 1} {
	if {$EndStk0(suit) == "empty"} {
	    set endstk_no 0
	} elseif {$EndStk1(suit) == "empty"} {
	    set endstk_no 1
	} elseif {$EndStk2(suit) == "empty"} {
	    set endstk_no 2
	} else { set endstk_no 3 }
#	set EndStk${endstk_no}(suit) $suit
#	set EndStk${endstk_no}(number) 1
    } else {
	set endstk_no -1
	# see if the end stack for the current suit exists
	for {set stk_no 0} {$stk_no < 4} {incr stk_no} {
	    eval set stk_suit \$EndStk${stk_no}(suit)
	    if {$stk_suit == $suit} {
		eval set stk_card \$EndStk${stk_no}(number)
		if {$stk_card != [expr $number - 1]} {
#		    puts "Current number is $stk_card"
#		    puts "Found endstack but no match"
                    return
		} else {
#		    puts "Found endstack and match"
		    set endstk_no $stk_no
#                   incr EndStk${endstk_no}(number)
                    break
		}
	    }
	}
	if {$endstk_no == -1} {
#	    puts "Found no endstack"
	    return
	}
    }

    # now simply move the card on the working stack to the end stack
    MoveWorkStack EndStk${endstk_no} MoveSlide 10
    PushUndoStack $WorkStk(0) $WorkStk(stack) endstk${endstk_no}_Tag \
	$WorkStk(info)
}


##############################################################
# Move the cards on the working stack to the given new stack #
##############################################################
proc MoveWorkStack {newstk type {move_increment 10} {info {}}} {

    global PlayStk0 PlayStk1 PlayStk2 PlayStk3 PlayStk4 PlayStk5 PlayStk6
    global EndStk0 EndStk1 EndStk2 EndStk3
    global WorkStk
    global tksol Score
    # [bug]
    global ItemToCard

    if {$WorkStk(size) < 1} { 	# [bug]
	puts "Error MoveWorkStack: WorkStk(size) is\
			$WorkStk(size) in MoveWorkStack!"
	return
    }

    if {$tksol(debug)} {
	puts stderr "\[MoveWorkStack\]"
	puts stderr "\tWorkStk(size) is $WorkStk(size)"
	for {set i 0} {$i < $WorkStk(size)} {incr i} {
	    set item $WorkStk($i)
	    puts stderr "\tWorkStk($i) is $ItemToCard($item)"
	}
	puts stderr "\n"
    }

    # raise the card we are moving to the top
    .canvas raise workstk_Tag all

    if {[string match PlayStk? $newstk]} {	# moving to a playstack
	scan $newstk "PlayStk%d" newstk_no

	if {![string compare $WorkStk(stack) drawright_Tag]} {
	    incr Score 5
	} else {
	    incr Score -1
	}

	# find the coordinates of the topmost item in the destination playstack
	eval set newstk_size \$${newstk}(size)
	if {$newstk_size == 0} {	# playstack is empty
	    # find the coordinates of the placeholder instead
	    set newcoords [.canvas coords playspot${newstk_no}_Tag]
	    set top_index -1;		# increment it below
	    set newy [lindex $newcoords 1]
	} else {
	    # find the coordinates of the topmost item in the stack
	    eval set top_index [expr \$${newstk}(size) - 1]
	    eval set top_item \$${newstk}($top_index)
	    set newcoords [.canvas coord $top_item]
	    if {[lsearch -exact [.canvas gettags $top_item] up_Tag] != -1} {
		set newy [expr [lindex $newcoords 1] + \
			  $tksol(stack.up_spacing)]
	    } else { 
		set newy [expr [lindex $newcoords 1] + \
			  $tksol(stack.down_spacing)] 
	    }
	}
	set newx [lindex $newcoords 0]

	# move all the cards on the working stack over to the playstack
	#set oldcoords [.canvas coord current]
	set oldcoords [.canvas coord $WorkStk(0)]
	set oldx [lindex $oldcoords 0]
	set oldy [lindex $oldcoords 1]
	if {$type == "MoveSlide"} {
	    set x_increment [expr ($newx - $oldx) / $move_increment]
	    set y_increment [expr ($newy - $oldy) / $move_increment]
	    for {set move 1} {$move < $move_increment} {incr move} {
		.canvas move workstk_Tag $x_increment $y_increment
		update idletasks
	    }
	    set total_x [expr ($move_increment - 1) * $x_increment]
	    set total_y [expr ($move_increment - 1) * $y_increment]
	    .canvas move workstk_Tag [expr ($newx - $oldx - $total_x)] \
		[expr ($newy - $oldy - $total_y)]
	} else {	# jump in one step to the new location
	    .canvas move workstk_Tag [expr $newx - $oldx] [expr $newy - $oldy]
	}

	# update the playstack to include the new cards
	for {set newcard 0} {$newcard < $WorkStk(size)} {incr newcard} {
	    incr top_index
	    set ${newstk}($top_index) $WorkStk($newcard)
	}
	incr ${newstk}(size) $WorkStk(size)

	# update the tags of the cards we have just moved over
	.canvas itemconfig workstk_Tag -tags \
	    [list playstk${newstk_no}_Tag up_Tag]

    } elseif {[string match EndStk? $newstk]} {
	incr Score 10
	# find the coordinates of the destination endstack
	##### [debug]
	set card $ItemToCard($WorkStk(0))
	scan $card "%c%d" suit number
	set suit [format "%c" $suit]
	eval set endstk_suit \$${newstk}(suit)
	if {$endstk_suit == "empty"} {
	    set ${newstk}(suit) $suit
	}
	set ${newstk}(number) $number
	##### [debug]
	scan $newstk "EndStk%d" newstk_no
	set coords [.canvas coords endspot${newstk_no}_Tag]
	# move the single card over
	if {$type == "MoveSlide"} {

	    # we do the same thing above, should combine it [bug]
	    set oldcoords [.canvas coord $WorkStk(0)]
	    set oldx [lindex $oldcoords 0]
	    set oldy [lindex $oldcoords 1]

	    set newx [lindex $coords 0]
	    set newy [lindex $coords 1]
	    set x_increment [expr ($newx - $oldx) / $move_increment]
	    set y_increment [expr ($newy - $oldy) / $move_increment]
	    for {set move 1} {$move < $move_increment} {incr move} {
		.canvas move $WorkStk(0) $x_increment $y_increment
		update idletasks
	    }
	}
	# again, we do this just to be on the safe side
	#.canvas coords current [lindex $coords 0] [lindex $coords 1]
	.canvas coords $WorkStk(0) [lindex $coords 0] [lindex $coords 1]

	# update the tags of the card
	#.canvas itemconfig current -tags [list endstk${newstk_no}_Tag up_Tag]
	.canvas itemconfig $WorkStk(0) \
	    -tags [list endstk${newstk_no}_Tag up_Tag]
	# update of the endstack top item is done somewhere else [bug]
	# look at ClearWorkStack and ProcessWorkStack, should move here [bug]
	# -- this is already done (see above)! [bug]

	# we should really keep a number of playing cards somewhere
	if {$EndStk0(number) == 13 && $EndStk1(number) == 13 && \
	    $EndStk2(number) == 13 && $EndStk3(number) == 13} {
		EndGame
	}

    } elseif {[string match RightStk $newstk]} {
	# change it to a "sliding" instead of a "jump" after testing [bug]
	.canvas coord $WorkStk(0) [lindex $info 0] [lindex $info 1]
	.canvas itemconfig $WorkStk(0) -tags {drawright_Tag up_Tag}

    } elseif {[string match LeftStk $newstk]} {
	# change it to a "sliding" instead of a "jump" after testing [bug]
	set flip_coord [.canvas coord flip_Tag]
	# flip the card over
	DisplayCard $WorkStk(0) FaceDown [lindex $flip_coord 0] \
	    [lindex $flip_coord 1] {drawleft_Tag down_Tag}

    } else {
	puts "Error MoveWorkStack> Moving to an invalid stack\
		 		location \($newstk\)"
	return
    }

    # if we are dragging from an endstack, update its top card number
    if {[string match endstk?_Tag $WorkStk(stack)]} {
	scan $WorkStk(stack) "endstk%d_Tag" endstk_no
	eval set endstk_number \$EndStk${endstk_no}(number)
	if {$endstk_number == 1} {
	    set EndStk${endstk_no}(suit) empty
	}
	incr EndStk${endstk_no}(number) -1
	eval set endstk_number \$EndStk${endstk_no}(number)
#	puts "EndStk${endstk_no}(number) is $endstk_number"
    } elseif {[string match playstk?_Tag $WorkStk(stack)]} {
	# update the playstack size
	scan $WorkStk(stack) "playstk%d_Tag" playstk_no
	incr PlayStk${playstk_no}(size) [expr -1 * $WorkStk(size)]
	# [debug]
#	eval set newsize \$PlayStk${playstk_no}(size)
#	puts "The new Playstk${playstk_no}(size) is $newsize"
	# [debug]
    }

    # do we want to reset the working stack size here?  what about undo? [bug]
    # yes, for double clicking cases where the events are somewhat confusing
    set WorkStk(size) 0
}



####################################
# Check for rectangle intersection #
####################################
proc StackOverlap {coord1 coord2} {

    # find the stack on the left
    set x1 [lindex $coord1 0]
    set x2 [lindex $coord2 0]
    if {$x1 < $x2} {
	set left 1
	set right 2
    } else {
	set right 1
	set left 2
    }
    # if right edge of left stack is less than left edge of right stack, no go
    if {[eval lindex \$coord${left} 2] < [eval lindex \$coord${right} 0]} {
	return 0
    }

    # now we know the stacks overlap vertically, need to check horizontally

    # find the stack on the top
    set y1 [lindex $coord1 1]
    set y2 [lindex $coord2 1]
    if {$y1 < $y2} {
	set top 1
	set bottom 2
    } else {
	set top 2
	set bottom 1
    }
    # if bottom edge of top stack is less than top edge of bottom stack, no go
    if {[eval lindex \$coord${top} 3] < [eval lindex \$coord${bottom} 1]} {
	return 0
    }

    # if we get this far, the stacks overlap
    return 1
}



###################################
# Push a move onto the undo stack #
###################################
proc PushUndoStack {item origin destination {info {}}} {

    global UndoStk

    # note that to undo, the item will go from destination to origin
    incr UndoStk(top)
    set UndoStk($UndoStk(top)) [list $item $origin $destination $info]
}



###############
# Undo a move #
###############
proc Undo {} {

    global PlayStk0 PlayStk1 PlayStk2 PlayStk3 PlayStk4 PlayStk5 PlayStk6
    global WorkStk
    global UndoStk
    global AutoFinishState
    # [bug]
    global ItemToCard

    if {$UndoStk(top) == 0} {
	return
    }

    # double check to make sure that both destination and origin are tags [bug]

    set top $UndoStk(top)
    set undo_cmd $UndoStk($top)
    incr UndoStk(top) -1
    set WorkStk(0) [lindex $undo_cmd 0]
    .canvas addtag workstk_Tag withtag $WorkStk(0)
    set origin [lindex $undo_cmd 1]
    set destination [lindex $undo_cmd 2]
    set info [lindex $undo_cmd 3]
    set WorkStk(stack) $destination
    set WorkStk(size) 1

    if {[string match MoveLeft $origin]} {	# unflip the drawleft stack
	# invoke MoveRight for each card on the drawleft stack
	# yes, this is ugly but it's the easiest way to do it, for now ...
	set drawleft_size [llength [.canvas find withtag drawleft_Tag]]
	for {set card 0} {$card < $drawleft_size} {incr card} {
	    MoveRight
	}
    } elseif {[string match drawleft_Tag $origin]} {
	set flip_coord [.canvas coord flip_Tag]
	# flip one or more cards over
	MoveWorkStack LeftStk MoveSlide 5
	if {[lindex $info 0] > 1} {
	    Undo
	}
    } elseif {[string match drawright_Tag $origin]} {
	MoveWorkStack RightStk MoveSlide 5 $info
    } elseif {[string match endstk?_Tag $origin]} {
	scan $origin "endstk%d_Tag" endstk_no
	MoveWorkStack EndStk${endstk_no} MoveSlide 5
    } elseif {[string match playstk?_Tag $origin]} {
	if {[string match $origin $destination]} {
	    # same playstack so simply flip the card facedown
	    set item $WorkStk(0)
	    set coord [.canvas coord $item]
	    DisplayCard $item FaceDown [lindex $coord 0] [lindex $coord 1] \
		[list $destination down_Tag]
	    # disable autofinish if it is currently enabled ...
	    if {$AutoFinishState} {
		set AutoFinishState 0
		pack forget .button.finish
	    }
	} else {	# fill up the working stack if needed
	    if {[string match playstk?_Tag $destination]} {
		set current_item $WorkStk(0)
		# look in the destination, _not_ the origin, playstack
		scan $destination "playstk%d_Tag" playstk_no
		eval set bound \$PlayStk${playstk_no}(size)
		for {set index 0} {$index < $bound} {incr index} {
		    eval set item \$PlayStk${playstk_no}($index)
		    if {$item == $current_item} {
			set WorkStk(size) [expr $bound - $index]
			incr index;	# first item already assigned above
			break
		    }
		}
		# now get all the cards that are on top of the "current" item
		for {set stk_index 1} {$index < $bound} {incr stk_index} {
		    eval set item \$PlayStk${playstk_no}($index)
		    set WorkStk($stk_index) $item
		    .canvas addtag workstk_Tag withtag $item
		    incr index
		}
	    }
	    scan $origin "playstk%d_Tag" playstk_no
	    MoveWorkStack PlayStk${playstk_no} MoveSlide 5
	}
    } else {
	puts "Error Undo> unknown original stack \($origin\)"
	return
    }

    # just to be on the safe side ...
    set WorkStk(size) 0
}



####################
# Setup a new game #
####################
proc NewDeal {} {

    # need to find out exactly which variables need to be reset [bug]

    # first reinitialize all the global variables and arrays
    Initialize
    # then, just to be on the safe side, reset all the tags [bug]
    .canvas dtag workstk_Tag workstk_Tag
    .canvas dtag drawright_Tag drawright_Tag
    .canvas dtag drawleft_Tag drawleft_Tag
    .canvas dtag up_Tag up_Tag
    .canvas dtag down_Tag down_Tag
    for {set stk_no 0} {$stk_no < 4} {incr stk_no} {
	.canvas dtag endstk${stk_no}_Tag endstk${stk_no}_Tag
    }
    for {set stk_no 0} {$stk_no < 7} {incr stk_no} {
	.canvas dtag playstk${stk_no}_Tag playstk${stk_no}_Tag
    }

    # pack the Undo button
    pack .button.undo -side left -anchor w
    # finally, start the new game
    DealCards
}

proc temp {} {
# This is needed for groupware version since
# "gettags" and "find withtag" use local cursor informations
  set tag_list [.canvas gettags current]
  set item [.canvas find withtag current]
 gk_toAll FlipUp $tag_list $item
}


#################################################
# Setup the various keyboard and mouse bindings #
################################################# 
proc BindKeys {} {

    global tksol

    # single click on drawleft stack flip the card over to drawright
    if {$tksol(flip) > 1} {
	.canvas bind drawleft_Tag <Any-ButtonPress-1> "gk_toAll MoveRightMany"
    } else {
	.canvas bind drawleft_Tag <Any-ButtonPress-1> "gk_toAll MoveRight 0"
    }

    # single click on flip move all the cards from drawright over
    .canvas bind flip_Tag <Any-ButtonPress-1> "gk_toAll MoveLeft"

    # single click on a facedown card on top of a stack flip it
     .canvas bind down_Tag <Any-ButtonPress-1> "temp"

    # press of button-1 on a faceup card setup the current working stack
     .canvas bind up_Tag <Any-ButtonPress-1> "gk_toAll SetupWorkStack %x %y"

    # motion drags the current working stack around
    .canvas bind up_Tag <Any-B1-Motion> "gk_toAll DragWorkStack %x %y"

    # release of button-1 moves the working stack to a new location (maybe)
    .canvas bind up_Tag <Any-ButtonRelease-1> "gk_toAll ProcessWorkStack %x %y"

    # double click on an up card attempts to move it to the end stack
    .canvas bind up_Tag <Any-Double-ButtonPress-1> "gk_toAll ClearWorkStack %x %y"
}



#######################################
# Initialize all the global variables #
#######################################
proc Initialize {} {

    global PlayStk0 PlayStk1 PlayStk2 PlayStk3 PlayStk4 PlayStk5 PlayStk6
    global EndStk0 EndStk1 EndStk2 EndStk3
    global SetupWorkStk DragWorkStk
    global WorkStk
    global UndoStk
    global AutoFinishState
    global NextRandom Score

    # set the global variables to default values
    set SetupWorkStk 0
    set DragWorkStk 0
    set WorkStk(size) 0
    set WorkStk(info) {}
    set UndoStk(top) 0
    set AutoFinishState 0
    set Score 0
    if {![info exists NextRandom]} {
	#set NextRandom [pid]
         set NextRandom 1    }

    # set all the end stacks to empty, we only need to keep a single card here
    for {set stk_no 0} {$stk_no < 4} {incr stk_no} {
	set EndStk${stk_no}(suit) empty
	set EndStk${stk_no}(number) 0
    }
    # setup the play stacks
    for {set stk_no 0} {$stk_no < 7} {incr stk_no} {
	set PlayStk${stk_no}(size) 0
    }
}



###################################
# "Shuffle" a sorted deck of card #
###################################
proc ShuffleDeck {} {

    # first create the sorted deck
    for {set card 0} {$card < 52} {incr card} {
	set card_struct $card
	lappend card_struct [Random]
	lappend sorted_deck $card_struct
    }

    # now shuffle the deck by sorting it based on the random number field
    set shuffled_deck [lsort -command SortCard $sorted_deck]
    foreach i $shuffled_deck {
	lappend rtn_val [lindex $i 0]
    }

    return $rtn_val
}



############################################################################
# Sort two card structures based on the random number associated with each #
############################################################################
proc SortCard {a b} {

    set num_a [lindex $a 1]
    set num_b [lindex $b 1]
    if {$num_a > $num_b} { return 1 }
    if {$num_a < $num_b} { return -1 }
    return 0
}



##########################################################
# Return a "random" number                               #
##########################################################
proc Random {} {
    return [random 10000]
}



####################################################################
# MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN #
####################################################################

# set the default values and read in user customization
Customize

# setup the table top by creating the canvas and bitmaps
SetupTable

# deal the cards
NewDeal

# setup key bindings and party time ...
BindKeys

# Now put the GroupKit telepointers, which will only appear on the canvas
gk_initializeTelepointers
gk_specializeWidgetTreeTelepointer . 

