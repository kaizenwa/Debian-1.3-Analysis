# Two-layer radar
# ============================
# Last modified June 28 by Carl Gutwin

# A radar view that shows local detail in the front plane, and a full view
# of the entire workspace in the back plane

# Help Description to be added to the help menu
#----------------------------------------------
set help_title "About Two-layer radar"

set help_text  {
    {normal} {PROTOTYPE - USE AT YOUR OWN RISK.
This conference demonstrates a two-layer view of a workspace. It shows local detail in the front plane, and a full view of the entire workspace in the back plane. You can move nodes with the left mouse button.}
}

gk_initConf $argv

gk_defaultMenu .menu
.menu itemcommand 2 add command \
     -label "$help_title" \
     -command "gk_topicWindow .helpWindow \
          -title \"$help_title\" \
          -text \"$help_text\""

pack .menu -side top -fill x

wm geometry . 440x450
wm minsize . 50 50 
wm title . "Head-Up Radar"

#-------------Globals----------------#

set canvasWidth 800
set canvasHeight 800
set viewWidth 400
set viewHeight 400
set viewOutlineWidth [expr $viewWidth.0 * $viewWidth / $canvasWidth] 
set viewOutlineHeight [expr $viewHeight.0 * $viewHeight / $canvasHeight] 
set lastLeftEdge 0
set lastTopEdge 0

set xRatio [expr $viewWidth.0 / $canvasWidth]
set yRatio [expr $viewHeight.0 / $canvasHeight]

set nodeRadius 20
set backXRadius [expr $nodeRadius * $xRatio]
set backYRadius [expr $nodeRadius * $yRatio]



set me [users get local.usernum]


set lightfill grey90
set lightoutline grey85
set boxoutline grey85

# If a monochrome display, change the colours

if {[lindex [lindex [winfo visualsavailable .] 0] 0] == "staticgray"} {
	set lightfill white
	set lightoutline black
	set boxoutline black
}


#-------------Setup------------------#


canvas .c \
    -scrollregion [list 0 0 $canvasWidth $canvasHeight] \
    -width $viewWidth -height $viewHeight \
    -bg linen \
	-borderwidth 1 \
	-relief raised \
	-yscrollcommand ".vertical set" \
	-xscrollcommand ".horizontal set"

set leftedge [lindex [.c xview]  0]
set topedge [lindex [.c yview] 0]


.c create rectangle 0 0 $viewOutlineWidth $viewOutlineHeight \
	-width 2 \
	-tags viewOutline$me \
	-outline seaGreen

scrollbar .vertical \
	-command "scroll yview"

scrollbar .horizontal \
	-orient horiz \
	-command "scroll xview"

pack .vertical -side right -fill y
pack .horizontal -side bottom -fill x
pack .c -side top -anchor nw 

# Set up event bindings for canvas:

bind .c <2> ".c scan mark %x %y"
bind .c <B2-Motion> ".c scan dragto %x %y;moveBackground;update idletasks"
bind .c <Motion> "moveMiniPointer %x %y"

# Now put the GroupKit telepointers, which will only appear on the canvas
#gk_initializeTelepointers
#gk_specializeWidgetTreeTelepointer .c

# Groupkit Enter and Leaves
#---------------------------

# Add a callback so new conference entrants can be updated,
# and departing ones destroyed

gk_on {[gk_event type]=="updateEntrant"} {updateEntrant [gk_event usernum]}

proc updateEntrant {usernum} {

	global me

	# send my view rectangle
	gk_toUserNum $usernum createRemoteView $me
	#gk_toUserNum $usernum updateRemoteView $me [.c coords viewOutline$me]

	# create a view for the new entrant
	createRemoteView $usernum
}


set totalNodes 0
set lastX 0
set lastY 0


# environment

gk_newenv -bind -share nodes

proc createNode {id x y name edges} {

    nodes set $id.coords [list $x $y]
    nodes set $id.name $name
    nodes set $id.edges $edges
}

nodes bind addEnvInfo {
    set pieces [split %K .]
    if {[lindex $pieces 1] == "edges"} {
	createScreenNode [lindex $pieces 0]
    }
}

nodes bind changeEnvInfo {
    set pieces [split %K .]
    if {[lindex $pieces 1] == "coords"} {
	set id [lindex $pieces 0]
	updateScreenNode $id
    }
}


# procs

proc updateScreenNode {id} {
    global nodeRadius

    set x [lindex [nodes get $id.coords] 0]
    set y [lindex [nodes get $id.coords] 1]
    set edges [nodes get $id.edges]

    .c coords $id [expr $x - $nodeRadius] [expr $y - $nodeRadius] \
	    [expr $x + $nodeRadius] [expr $y + $nodeRadius]

    foreach edge $edges {
	if {$edge > 0} {
	    set inId "node$edge"
	    set inCoords [nodes get $inId.coords]
	    set inX [lindex $inCoords 0]
	    set inY [lindex $inCoords 1]
	    set blah "edge$id"
	    set edgeId "$blah$edge"
	    .c coords $edgeId $x $y $inX $inY
	}
    }

    #.c coords "name$id" $x $y
    
    update idletasks

    updateBackgroundNode $id
}

proc mapX {x} {
    global xRatio canvasWidth leftedge

    set newX [expr $x * $xRatio + $canvasWidth * $leftedge]
    return $newX
}

proc mapY {y} {
    global yRatio canvasHeight topedge

    set newY [expr $y * $yRatio + $canvasHeight * $topedge]
    return $newY
}

proc updateBackgroundNode {id} {
    
    global canvasWidth canvasHeight lightfill lightoutline \
	    viewWidth viewHeight leftedge topedge \
	    backXRadius backYRadius

    set x [lindex [nodes get $id.coords] 0]
    set y [lindex [nodes get $id.coords] 1]
    set newX [mapX $x]
    set newY [mapY $y]
    
    .c coords back$id [expr $newX - $backXRadius] [expr $newY - $backYRadius]\
	[expr $newX + $backXRadius] [expr $newY + $backYRadius]

    set edges [nodes get $id.edges]
    foreach edge $edges {
	if {$edge > 0} {
	    set inId "node$edge"
	    set inCoords [nodes get $inId.coords]
	    set inX [lindex $inCoords 0]
	    set inY [lindex $inCoords 1]
	    set newInX [mapX $inX]
	    set newInY [mapY $inY]

	    set edgeId back$id$inId
	    .c coords $edgeId $newX $newY $newInX $newInY
	}
    }
    # make sure the back stays in the back
    
    .c lower back front
    .c lower edge node
    .c raise mini    
}

proc createScreenNode {id} {
    global nodeRadius

    set x [lindex [nodes get $id.coords] 0]
    set y [lindex [nodes get $id.coords] 1]
    set edges [nodes get $id.edges]
    set name [nodes get $id.name]

    .c create oval [expr $x - $nodeRadius] [expr $y - $nodeRadius] \
	[expr $x + $nodeRadius] [expr $y + $nodeRadius] \
	-fill blue -tags [list $id front node] -stipple gray50

    .c bind $id <1> "startDrag $id %x %y"
    .c bind $id <B1-Motion> "continueDrag $id %x %y"

    #.c create text $x $y -text $name -tags "name$id" -fill white

    foreach edge $edges {
	if {$edge > 0} {
	    set inId "node$edge"
	    set inCoords [nodes get $inId.coords]
	    set inX [lindex $inCoords 0]
	    set inY [lindex $inCoords 1]
	    
	    set blah "edge$id"
	    set edgeId "$blah$edge"
	    .c create line $x $y $inX $inY -width 5 -fill orange \
		    -tags [list $edgeId edge] -stipple gray50
	    .c lower edge
	}
    }
    createBackgroundNode $id
}

proc createBackgroundNode {id} {
    
    global canvasWidth canvasHeight lightfill lightoutline \
	viewWidth viewHeight topedge leftedge \
	backXRadius backYRadius

    set x [lindex [nodes get $id.coords] 0]
    set y [lindex [nodes get $id.coords] 1]

    set newX [mapX $x]
    set newY [mapY $y]
    
    .c create oval [expr $newX - $backXRadius] [expr $newY - $backYRadius]\
	[expr $newX + $backXRadius] [expr $newY + $backYRadius] \
	-fill $lightfill \
	-outline $lightoutline \
	-tags [list back back$id backnode] \
	-width 2

    set edges [nodes get $id.edges]
    foreach edge $edges {
	if {$edge > 0} {
	    set inId "node$edge"
	    set inCoords [nodes get $inId.coords]
	    set inX [lindex $inCoords 0]
	    set inY [lindex $inCoords 1]
	    set newInX [mapX $inX]
	    set newInY [mapY $inY]

	    set edgeId back$id$inId
	    .c create line $newX $newY $newInX $newInY -width 4 \
		    -fill $lightoutline -tags [list $edgeId back backedge]
	}
    }
    # make sure the back stays in the back
    
    .c lower back
    .c lower backedge backnode
    .c raise mini    
}

proc startDrag {id x y} {
    global offsetDragX offsetDragY

    set x [.c canvasx $x]; set y [.c canvasy $y]
    set offsetDragX [expr $x-[lindex [nodes get $id.coords] 0]]
    set offsetDragY [expr $y-[lindex [nodes get $id.coords] 1]]
}

proc continueDrag {id x y} {
    global offsetDragX offsetDragY

    set x [.c canvasx $x]; set y [.c canvasy $y]
    nodes set $id.coords [list [expr $x-$offsetDragX] \
	    [expr $y-$offsetDragY]]
}

proc loadGraph {filename} {
    global totalNodes
    
    set fd [open $filename]
    gets $fd node
    while {$node != ""} {
	set x [lindex $node 0]
	set y [lindex $node 1]
	set name [lindex $node 2]
	set edges [lrange $node 3 end]
	incr totalNodes
	createNode "node$totalNodes" $x $y $name $edges
	gets $fd node
    }
    close $fd
}

loadGraph "[userprefs scriptpath]/test.graph"



proc scroll {which cmd amount {unit ""}} {
    global leftedge topedge
    
    if {$unit == ""} {
	.c $which $cmd $amount
    } else {
	.c $which $cmd $amount $unit
    }
    
    set leftedge [lindex [.c xview]  0]
    set topedge [lindex [.c yview] 0]
    
    moveBackground
    update idletasks
}



proc moveBackground {} {

	global lastLeftEdge lastTopEdge canvasWidth canvasHeight me \
		leftedge topedge

	# first, move the objects in the background

	set xcoords [.c xview]
	set ycoords [.c yview]

	set rightedge [lindex $xcoords 1]
	set bottomedge [lindex $ycoords 1]

	set xoffset [expr ($leftedge - $lastLeftEdge) * $canvasWidth]
	set yoffset [expr ($topedge - $lastTopEdge) * $canvasHeight]

	.c move back $xoffset $yoffset

	# second, move the view outline

	set lastLeftEdge $leftedge
	set lastTopEdge $topedge

	set l [expr $leftedge * 1.5 * $canvasWidth]
	set r [expr ($rightedge / 2.0 + $leftedge) * $canvasWidth]
	set t [expr $topedge * 1.5 * $canvasHeight]
	set b [expr ($bottomedge / 2.0 + $topedge) * $canvasHeight]

	.c coords viewOutline$me $l $t $r $b
	gk_toOthers updateRemoteView $me $leftedge $topedge \
		$rightedge $bottomedge

	# third, move the remote view outline
	
	.c move you $xoffset $yoffset
}

proc moveMiniPointer {x y} {

    set x [.c canvasx $x]
    set y [.c canvasy $y]
    
    gk_toAll reallyMovePointer $x $y

    set tl [expr $x - 3]
    set tr [expr $x + 3]
    set tt [expr $y - 3]
    set tb [expr $y + 3]
    
    gk_toOthers .c coords tele $tl $tt $tr $tb
}

proc reallyMovePointer {x y} {

    set newX [mapX $x]
    set newY [mapY $y]

    .c coords mini [expr $newX - 2] [expr $newY -2] \
	    [expr $newX + 2] [expr $newY + 2]
    
}

proc createRemoteView {user} {
	global viewOutlineWidth viewOutlineHeight lastMiniX lastMiniY

	.c create rectangle 0 0 $viewOutlineWidth $viewOutlineHeight \
		-width 2 \
		-tags you \
		-outline grey75
	
	.c create oval 0 0 4 4 \
		-width 1 \
		-fill grey85 \
		-tags mini

	.c create oval 0 0 6 6 \
		-width 1 \
		-fill black \
		-tags tele
	set lastMiniX 0
	set lastMiniY 0
		
}

proc updateRemoteView {user rle rte rre rbe} {

	global canvasWidth canvasHeight

	set xcoords [.c xview]
	set ycoords [.c yview]
	set lle [lindex $xcoords 0]
	set lte [lindex $ycoords 0]
	set lre [lindex $xcoords 1]
	set lbe [lindex $ycoords 1]
	
	set lvw [expr $lre - $lle]
	set lvh [expr $lbe - $lte]

	set hoffset [expr $lle * $canvasWidth]
	set voffset [expr $lte * $canvasHeight]
	
	set rl [expr $rle * $lvw * $canvasWidth + $hoffset]
	set rt [expr $rte * $lvh * $canvasWidth + $voffset]
	set rr [expr $rre * $lvw * $canvasWidth + $hoffset]
	set rb [expr $rbe * $lvh * $canvasWidth + $voffset]

	set tag "viewOutline$user"
	.c coords you $rl $rt $rr $rb
}





