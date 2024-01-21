# Initialize the groupkit conference
gk_initConf $argv

######################### Help Information #############################

set about(title) "About Mine Sweeper"
set about(description) {
{normal} {This mine sweeper conference is based on }
{normalitalic} {Joel A. Fine's}
{normal} { tkmines version .  The person responsible for doing\
 the groupkit version is }
{normalitalic} {Shannon Jaeger}
{normal } {.  

For playing instructions see the }
{normalbold} {Gk Mine Sweeper}
{normal} { help information.}
}

set app(title) "Gk Mine Sweeper"
set app(description) {
{largebold} {Welcome to gkmines!}
{normal} {

In this game, you are a minefield inspector, \
trying to identify the unsafe spots on a large grid. If you step on \
a square with a bomb underneath, you }
{normalbold} {lose}
{normal} { the game. If, on the other \
hand, the square you step on has no bomb beneath it, the number of \
neighboring squares that }
{normalbold} {do}
{normal} { have bombs is etched in the square. 

You must identify all of the unmined squares in order to escape the \
minefield and }
{normalbold} {win}
{normal} { the game. You can step on any unknown square at any time. 

Press the }
{normalitalic} {left mouse button}
{normal} { over a square to step on it. Use the \
information revealed as you step on non-mined squares to deduce which \
other squares have mines. If you believe that a particular \
square has a mine, press the }
{normalitalic} {right mouse button}
{normal} { over it to mark it \
"mined." You are then safe from accidentally stepping on the square. \
Of course, there is no guarantee that your guess is right! 

You have an assistant who will help you step on several squares at \
once, but he is very cautious. If you point out a "known" square (one \
that you have already stepped on) to him, he will step on all of the \
"non-mined" squares adjacent to the square you show him, under one \
condition: that you have marked enough squares "mined" that you can \
reasonably believe that none of the other neighbor squares are also \
mined. You can ask him to do this by pressing the }
{normalitalic} {middle-mouse button}
{normal} { over any known square.

For example, if you }
{normalitalic} {middle-mouse} 
{normal} { over a square with a "1" in it, and \
you have marked one of its neighbors with a mine-flag, your assistant \
will step on all of the other squares around the "1." If your flag is \
misplaced, though, then your assistant will probably blow you both up! 

You can also mark a square with a "?" by pressing the }
{normalitalic} {right-mouse}
{normal} { button an extra time. The "?" has no effect, other than to help \
you mark a square as a memory aid.

}

{largebold} {Good luck, and tread lightly.}
}

######################### Board Construction  #############################

# $f is frame/top window (already created)
# $w is width
# $h is height
# $m is number of mines  

proc makeBoard {f w h m} {
    global about app

    # if f is empty than the window must be the root window
    if {$f != "."} {set f $f. }
    
    # Add lots of goodies to the groupkit default menu!!
    gk_defaultMenu ${f}menubar
    ${f}menubar itemcommand 0 insert 1 command -label "Play Again" \
	    -command "askResetBoard $f"
    ${f}menubar itemcommand 0 insert 2 cascade -label "Resize" \
	    -menu "${f}menubar.file.menu.mr"
    ${f}menubar itemcommand 0 insert 3 command -label "Give Up" \
	    -command "askShowAll $f" 
    ${f}menubar itemcommand 0 insert 4 separator

    ${f}menubar itemcommand help insert 2 separator
    ${f}menubar itemcommand help add command \
	    -label "$about(title)" \
	    -command "gk_topicWindow .helpWindow \
	           -title {$about(title)} \
		   -text {$about(description)}"
    ${f}menubar itemcommand help add command \
	    -label "$app(title)" \
	    -command "gk_topicWindow .helpWindow \
	           -title {$app(title)} \
		   -text {$app(description)}"

    # A BIG HACK - since the defualt menubar has no means of return
    #   the pathname of its submenu we are hard-coding it here until
    #   the default menubar changes
    set submenu ${f}menubar.file.menu.mr
    menu $submenu
    $submenu add command -label "Small" -command "askResizeBoard $f small"
    $submenu add command -label "Medium" -command "askResizeBoard $f medium"
    $submenu add command -label "Big" -command "askResizeBoard $f big"
    $submenu add command -label "Max" -command "askResizeBoard $f max"
    $submenu add command -label "Custom..." -command "askResizeBoard $f custom"

    pack ${f}menubar -fill x -side top
    # Status stuff - Mines found and the total number of mines
    frame ${f}st
    
    frame ${f}st.m
    frame ${f}st.t
    pack append ${f}st ${f}st.m {left frame w padx 20} \
	    ${f}st.t {left frame e padx 20}
    
    # Mine counts
    frame ${f}st.m.l
    frame ${f}st.m.n
    pack append ${f}st.m ${f}st.m.l {left fillx} ${f}st.m.n {left fillx}
    
    label ${f}st.m.l.found -text "Mines found:"
    label ${f}st.m.l.mines -text "Mines:"
    pack append ${f}st.m.l ${f}st.m.l.found {top frame w} \
	    ${f}st.m.l.mines {top frame w}
    
    label ${f}st.m.n.found -relief sunken -width 3
    label ${f}st.m.n.mines -relief sunken -width 3
    pack append ${f}st.m.n ${f}st.m.n.found {top frame e} \
	    ${f}st.m.n.mines {top frame e}
    
    # Now make those buttons and find where those mines are.
    if { [users local.usernum] == [mines controller] } {
	makeButtons $f $w $h $m
    }
    resetBoard $f
}

proc makeButtons {f w h m} {
    
    mines width $w
    mines height $h
    mines number $m

    # Buttons
    if [winfo exists ${f}b] { destroy ${f}b }
    frame ${f}b 

    pack append $f ${f}st {top pady 20} ${f}b top
    for {set row 0} {$row < $h} {incr row} {
	frame ${f}b.$row
	pack append ${f}b ${f}b.$row top
	for {set col 0} {$col < $w} {incr col} {
	    button ${f}b.$row.$col -highlightthickness 0 -bg lightgray \
		    -activebackground grey -relief raised \
		    -bitmap [getBitmap empty]
	    pack append ${f}b.$row ${f}b.$row.$col left
	    ${f}b.$row.$col config -command "showSquare $f $row $col" 
	    bind ${f}b.$row.$col <3> {
		set which [lowToLol %W right 2]
		eval markSquare [lindex $which 0] [lindex $which 1]
	    }
	    bind ${f}b.$row.$col <2> {
		set row [lindex [lowToLol %W right 2] 0]
		set col [lindex [lowToLol %W right 2] 1]
		set last [expr [string last .b.$row.$col %W] - 1]
		if { $last < 0 } { set win .
	        } else { set win [string range %W 0 $last] }
		eval testSquare $win $row $col
	    }
	}
    }
    if ![winfo exists ${f}status] {
	createStatusInfo $f
    }
    gk_specializeWidgetTreeTelepointer .
}

proc createStatusInfo {f} {
    set cont [mines controllerName]
    set next [lindex [mines nextControllerName] 0]
    set local [users local.username]
    
    frame ${f}status -relief groove -bd 2 
    label ${f}status.label -text "Controller: $cont   Next: $next"
    if { [users local.usernum] == [mines controller] } {
	set action "Give control to next user"
    } else {
	set action "Request Turn"
    }
    button ${f}status.button -text "$action" -command "changeStatus $f"

    pack ${f}status.label -fill x
    pack ${f}status.button
    pack ${f}status -pady 3 -side bottom
}

proc changeStatus {f} {
    set local [users local.usernum]
    if { [mines controller] == $local } {
	if { [lindex [mines nextController] 0] != -1 } {
	    mines controller [lindex [mines nextController] 0]
	    mines nextController [lreplace [mines nextController] 0 0]
	    mines controllerName [lindex [mines nextControllerName] 0]
	    mines nextControllerName [lreplace [mines nextControllerName] 0 0]
	}
    } elseif { [mines nextController] == -1 } {
	mines nextController $local
	mines nextControllerName [list [users local.username]]
    } elseif { [lsearch [mines nextController] $local] == -1 } {
	mines nextController [linsert [mines nextController] end $local]
	mines nextControllerName [linsert [mines nextControllerName] end \
		[users local.username]]
    }
    puts "CONT: [mines controllerName] NEXT: [mines nextControllerName]"
}

##################### Resetting Board ##########################

proc resetBoard {f} {
    if { [users local.usernum] != [mines controller] } {
	return
    }

  set h [mines height]
  set w [mines width]
  set m [mines number]
  for {set row 0} {$row < $h} {incr row} {
    for {set col 0} {$col < $w} {incr col} {
      initSquare $row $col
    }
  }
  mines guessed 0
  mines squaresknown {}
  mines visited {}
  mines done 0
  mines started 0

  setMines $f $m
  mines l.Row 0
  mines l.Col 0
  lazyUpdate
  gk_toAll unbusy
}

proc askResetBoard {f} {

  if {[mines done]} {
      gk_toAll busy
      resetBoard $f
      return
  }

  gk_toAll doChoice "Reset Mines Board" \
	   {msgLabel "Are you sure you want to reset the board?"} \
	   [list [list "yes" "busy; resetBoard $f"] {no {}}]
}

proc askResizeBoard {f type} {

    set done [mines done]
    set started [mines started]

    case $type {
	"small"  { set width 10;         set height 10 }
	"medium" { set width 20;         set height 10 }
	"big"    { set width 40;         set height 20 }
	"max"    { set width [maxwidth]; set height [maxheight] }
	"custom" { customResize $f; return }
    }

    set mines [stdmines $width $height]
    if {$done || !$started} {
	resizeBoard $f $width $height $mines
	return
    }

    gk_toAll doChoice "Reset Board Size" \
	    [list msgLabel "Are you sure you want to reset the board to \
	         $width x $height ($mines mines)?"] \
	    [list [list "Yes" "resizeBoard $f $width $height $mines"] {No {}}]
}

proc askShowAll {f} {

  if {[mines done]} {
      gk_toAll doChoice Information \
	      {msgBox "You can't give up; the game is already over!" 1000} \
	      {{dismiss {}}}
  } else {
      gk_toAll doChoice "Give Up Mines" \
	      {msgLabel "Are you sure you want to give up the game?"} \
	      [list [list "yes" "showAll $f"] {no {}}]
  }
}

proc setMines {f m} {


    set squareList {}
    foreach row [winfo children ${f}b] {
	set squareList [concat $squareList \
		[lowToLol [winfo children $row] right 2]]
    }
    set mineList [choose $m $squareList]
    foreach square $mineList {
	mines [lindex $square 0].[lindex $square 1].mined 1
    }
}

proc initSquare {row col} {

    if { [mines $row.$col.value] != "" } {
	mines delete $row.$col.value
    }
    mines $row.$col.nbrguessed 0
    mines $row.$col.guess empty
    mines $row.$col.mined 0
}

# Every so often, count up the neighbor-mines of another square.
proc lazyUpdate {} {
  if {[mines done]} {return}
  set row [mines l.Row]
  set col [mines l.Col]

  mines $row.$col.value [countNeighborMines $row $col]
  incr col
  if {$col == [mines width]} {
    set col 0
    incr row
  }
  mines l.Row $row
  mines l.Col $col
  # Have we reached the end?
  if {$row != [mines height]} {
    if {[mines started]} {
      set nextTime 50
    } else {
      set nextTime 1
    }
    after $nextTime lazyUpdate
  }
}

##################### Button Display  ##########################

proc showAll {f} {

  set w [mines width]
  set h [mines height]
  mines done 1
  for {set row 0} {$row < $h} {incr row} {
    for {set col 0} {$col < $w} {incr col}  {
      set guess [mines $row.$col.guess]
      if { [lsearch [mines squaresknown] [list $row $col]] != -1} {
        continue
      }

      # We don't know for sure what this is
      if {$guess == "mine"} {
        # We think it's a mine
        if {[mines $row.$col.mined]} {
          continue
        }
        # Wrong!
        gk_toAll ${f}b.$row.$col config -bitmap [getBitmap wrongMine]
        continue
      }
      if {[mines $row.$col.mined]} {
        gk_toAll ${f}b.$row.$col config -bitmap [getBitmap unfoundMine]
      }
    }
    gk_toAll update
  }
  gk_toAll update idletasks
}

proc visitSquare {f row col} {

  if {[mines done]} {return}
  if { [lsearch [mines squaresknown] [list $row $col]] != -1 } {
    return
  }
  if {[mines $row.$col.guess] == "mine"} {
    return
  }
  if {[mines $row.$col.mined]} {
    # LOSE THE GAME
    showAll $f
    gk_toAll doChoice Loser! {msgLabel "Sorry, you lost!"} \
	    [list {OK {}} [list {Play Again} "busy; resetBoard $f"] \
	    {Quit _gk_properQuit}]
  }
  if {![mines started]} {
    mines started 1
  }

  mines squaresknown [linsert [mines squaresknown] end [list $row $col]]
  mines $row.$col.guess [countNeighborMines $row $col]
  update
}

proc showSquare {f row col} {

  if {[mines done]} {return}
  visitSquare $f $row $col
  set count [countNeighborMines $row $col]
  if {$count == 0} {
    set zero {}
    while 1 {
	foreach square [getNeighbors $row $col] {
        set nr [lindex $square 0]
        set nc [lindex $square 1]
        if ![countNeighborMines $nr $nc] {
           if {[lsearch [mines squaresknown] $square] < 0} {
	       lappend zero $square
	   }
        }
        visitSquare $f $nr $nc
      }
      if {[llength $zero] != 0} {
        set row [lindex [lindex $zero 0] 0]
        set col [lindex [lindex $zero 0] 1]
        set zero [lrange $zero 1 end]
      } else {
        return
      }
    }
  }
}

proc markSquare {row col} {
  set guess [mines $row.$col.guess]
  if { [lsearch [mines squaresknown] [list $row $col]] != -1 } {
    return
  }

  case $guess {
    "empty" {
      set guess mine
      set incr 1
    }
    "mine" {
      set guess mark
      set incr -1
    }
    "mark" {
      set guess empty
      set incr 0
    }
  }
  mines $row.$col.guess $guess
  foreach square [getNeighbors $row $col] {
    set nr [lindex $square 0]
    set nc [lindex $square 1]
    mines $nr.$nc.nbrguessed [expr [mines $nr.$nc.nbrguessed] + 1]
  }
  mines guessed [expr [mines guessed] + 1]
}

proc getActiveForeground {value} {
  case $value {
    "mine" {return red}
    "mark" {return black}
    "empty" {return black}
  }
}

proc getForeground {value} {
  case $value {
    "mine" {return red}
    "mark" {return black}
    "empty" {return black}
    "0" {return lightgrey}
    "1" {return blue}
    "2" {return seagreen}
    "3" {return red}
    "4" {return white}
    "5" {return black}
    "6" {return yellow}
    "7" {return purple}
    "8" {return orange}
  }
}

# ${f}$row.$col.nbrguessed = number of neighbors guessed to be bombs.

# If I already know the value of this button, and I've already marked
# that many mines around it, expose the rest of its neighbors.
proc testSquare {f row col} {

  # If the square isn't known, testSquare does nothing.
  if { ([lsearch [mines squaresknown] [list $row $col]] == -1) \
      && ( [mines $row.$col.guess] != "mine") } {
    return
  }

  foreach square [getNeighbors $row $col] {
      set nr [lindex $square 0]
      set nc [lindex $square 1]
      showSquare $f $nr $nc
  }
}

##################### Custom Board ##########################

proc customResize {f} {

    gk_toAll doChoice "Custom Board Size" "customMsg $f" \
	    [list [list "Resize Board" "resizeBoardCustom $f .choice"]\
	    {Cancel {}}]
}

proc customMineScale {w num} {
  gk_toAll $w.h set [$w.h get]
  gk_toAll $w.w set [$w.w get]
  set min [minmines ]
  set max [maxmines [$w.w get] [$w.h get]]
  set std [stdmines [$w.w get] [$w.h get]]
  gk_toAll $w.m.scale config -from $min
  gk_toAll $w.m.scale config -to   $max
  gk_toAll $w.m.scale config -tickinterval [expr {$max - $min}]
  gk_toAll $w.m.stdl config -text $std
}

proc resizeBoardCustom {f win} {
    global useStdMineCount
    global scaleWidth scaleHeight scaleMines
    
    if { $scaleWidth == ""} {set w [minwidth] } else { set w $scaleWidth }
    if { $scaleHeight== ""} {set h [minheight] } else { set h $scaleHeight }

    if {$useStdMineCount} {
	set m [stdmines $w $h]
    } else {
	set m $scaleMines
    }
    resizeBoard $f $w $h $m
}

proc resizeBoard {f w h m} {

    if {$w > [maxwidth] || $h > [maxheight]} {
	gk_toAll doChoice "Warning!" \
		{msgBox "That board is too big! It wouldn't fit on the screen." 1000} \
		{{ok {}}}
    return
  }
  gk_toAll busy
  gk_toAll doChoice Information \
	  {msgBox "Resizing board. Please stand by." 1000} \
	  {{dismiss {}}}
  gk_toAll update
puts "MAKE BUTTON $f $w $h $m"
  gk_toAll makeButtons $f $w $h $m
  gk_toUserNum [mines controller] resetBoard $f
  gk_toAll catch {destroy .choice}
  gk_toAll unbusy
}

##################### Handling Neighbors  ##########################

# How many neighbors are mines?
proc countNeighborMines {row col} {

  # Just in case I've computed it already
  if {[mines $row.$col.value] != ""} {
    return [mines $row.$col.value]
  }
  set count 0
  foreach square [getNeighbors $row $col] {
    set nr [lindex $square 0]
    set nc [lindex $square 1]
    incr count [mines $nr.$nc.mined]
  }

  return $count
}

proc getNeighbors {row col} {
    
    set nl {}
    for {set nrow [expr $row-1]} {$nrow <= [expr $row+1]} {incr nrow} {
	for {set ncol [expr $col-1]} {$ncol <= [expr $col+1]} {incr ncol} {
	    if [inBoard $nrow $ncol] {
		if { ($row!=$nrow) || ($col!=$ncol) } {
		    lappend nl [list $nrow $ncol]
		}
	    }
	}
    }
    return $nl
}

proc inBoard {row col} {
    if { ($row < 0) || ($row >= [mines height]) } {
	return 0
    } elseif { ($col < 0) || ($col >= [mines width]) }  {
	return 0
    } else {
	return 1
    }
}

##################### Pop-up windows  ##########################

proc msgBox {text aspect w} {
    message $w.message -text $text -aspect $aspect
    pack $w.message -fill both -expand y -side top
}

proc msgLabel {text w} {
    label $w.label -text $text
    pack $w.label -fill both -expand y -side top
}

proc customMsg {f w} {

    frame $w.m
    label $w.m.l -text "Mine Count:"
    radiobutton $w.m.std -text "Standard" -variable useStdMineCount \
	    -value 1 -command "gk_toAll $w.m.scale config -state disabled 
	                       gk_toAll $w.m.std select 
			       gk_toAll $w.m.custom deselect" 
    label $w.m.stdl -relief sunken -width 3
    radiobutton $w.m.custom -text "Custom" -variable useStdMineCount \
	    -value 0 -command "gk_toAll $w.m.scale config -state normal
	                       gk_toAll $w.m.custom select 
			       gk_toAll $w.m.std deselect" 
    scale $w.m.scale -orient horizontal -length 200 -variable scaleMines
            -command "gk_toAll $w.m.scale set [$w.m.scale get]"
    $w.m.scale set [mines number]
    $w.m.std invoke

    pack append $w.m $w.m.l left $w.m.std left $w.m.stdl left \
	    $w.m.custom left $w.m.scale left
    pack $w.m -side bottom

    scale $w.w -label "Width:" -orient horizontal -length 200 \
	    -from [minwidth] -to [maxwidth] -variable scaleWidth \
	    -command "customMineScale $w" 
    scale $w.h -label "Height:" -orient horizontal -length 200 \
	    -from [minheight] -to [maxheight]  -variable scaleHeight \
	    -command "customMineScale $w" 
    
    $w.w set [mines width]
    $w.h set [mines height]

    pack append $w $w.w top $w.h top
}

######################### Support Routines #############################

proc minwidth {} {
  return 3
}

proc minheight {} {
  return 3
}

proc maxwidth {} {
    set buttonWidth 24
    set rslt [expr {[winfo screenwidth .] / $buttonWidth}]
    return $rslt
}

proc maxheight {} {
  set buttonHeight 24
  set rslt [expr {[winfo screenheight .] / $buttonHeight - 5}]
  return $rslt
}

proc stdmines {width height} {
  set std [toint [expr {($width * $height) * .15}]]
  if {$std < [minmines ]} {
    set std [minmines]
  }
  return $std
}

proc minmines {} {
  return 2
}

proc maxmines {width height} {
  return [expr {$width * $height - 2}]
}

proc getBitmap {name} {
  global gk_library
  return "@$gk_library/library/bitmaps/mines/$name"
}

proc lowToLol {winList side number} {
    set list {}
    
    foreach window $winList {
	set window [split $window .]
	set max [llength $window]
	if { [expr $max -1] < $number } {
	    error "Bad number, $number, supplied to lowToLol"
	}
	if { [string first $side right] != -1} {
	    set newItem {}
	    for {set count [expr $max - $number]} {$count < $max} \
		    {incr count} {
		lappend newItem [lindex $window $count]
	    }
	} elseif { [string first $side left] != -1 } {
	    set newItem {}
	    for {set count 1} {$count < [expr $number+1] && $count < $max} \
		    {incr count} {
		lappend newItem [lindex $window $count]
	    }
	} else {
	    error "Incorrect side, $side, for lowToLol: must be left or right."
	}
	lappend list $newItem
    }
    return $list
}

########################## Event Handlers  #####################
     
proc updateEnvironment {} {
    gk_toAll busy
    gk_toAll doChoice {New user} \
	   {msgLabel "New user joining. Please wait"} \
	   {{dismiss {}}}
    gk_toAll update
    makeButtons . [mines width] [mines height] [mines number]
    foreach key [mines keys] {
	updateKey $key
    }
    gk_toAll unbusy
}

proc updateKey {key} {
    if { [llength [mines $key]] == 1 } {
	mines $key [mines $key]
    } else {
	foreach newKey [mines keys $key] {
	    updateKey $key.$newKey
	}
    }
}

proc envUpdate {f key} {

    set key [split $key .]
    set area [expr [mines width] * [mines height]]

    switch -exact [lindex $key 0] {
        controller {
	    if { [mines controller] == [users local.usernum] } {
		${f}status.button config -text "Give turn to next user"
		
	    } else { 
		${f}status.button config -text "Request Turn"
	    }
	}
	controllerName {
	    ${f}status.label config -text "Controller: [mines controllerName]\
		    Next: [lindex [mines nextControllerName] 0]"
        }
	nextControllerName {
	    if { [mines nextControllerName] == "" } {
		mines nextController -1
		mines nextControllerName {nobody}
	    }
	    ${f}status.label config -text "Controller: [mines controllerName] \
		    Next: [lindex [mines nextControllerName] 0]"
	}
	guessed { 
	    ${f}st.m.n.found config -text [mines guessed]
	    ${f}st.m.l.found config -text "Mines found:"	
	    set known [llength [mines squaresknown]]
	    if {[expr $known + [mines guessed]]==$area \
		    && ![winfo exists .choice]} {
		# WIN THE GAME
		gk_toAll doChoice Winner! \
			{msgLabel "Congratulations, you won!"} \
			[list {OK {}} \
			[list "Play Again" "gk_toAll busy; resetBoard $f"] \
			{Quit _gk_properQuit}]
	    }
	}
	squaresknown { 
	    set known [llength [mines squaresknown]]
	    if {[expr $known + [mines guessed]]==$area \
		&& ![winfo exists .choice]} {
		# WIN THE GAME
		gk_toAll doChoice Winner! \
			{msgLabel "Congratulations, you won!"} \
			[list {OK {}} \
			[list "Play Again" "gk_toAll busy; resetBoard $f"] \
			{Quit _gk_properQuit}]
	    }
	}  
	number { ${f}st.m.n.mines config -text [mines number] }
	default {
	    set row [lindex $key 0]
	    set col [lindex $key 1]
	    switch -exact [lindex $key 2] {
		nbrguessed {
		    if { [mines $row.$col.nbrguessed] == 0} {
			${f}b.$row.$col config -bg lightgray \
				-activebackground grey -relief raised \
				-bitmap [getBitmap empty]
		    }
		}
		guess {
		    set guess [mines $row.$col.guess]
		    if [regexp "^0$|^1$|^2$|^3$|^4$|^5$|^6$|^7$|^8$" $guess] {
			${f}b.$row.$col config -bitmap [getBitmap $guess] 
			${f}b.$row.$col config -fg [getForeground $guess] 
			${f}b.$row.$col config -activeforeground \
				[getForeground $guess] 
			${f}b.$row.$col config -relief sunken \
				-activeBackground lightgrey
		    } else {
			${f}b.$row.$col config -bitmap [getBitmap $guess] 
			${f}b.$row.$col config -fg [getForeground $guess] 
			${f}b.$row.$col config -activeforeground \
				[getActiveForeground $guess]
		    }
		}
		default {
#		    puts "WARNING: Change in env $key not handled"
		}
	    }
	}
    
    }
}

proc userLeft {f who} {
puts "userLeft $f $who"
    if { $who == [mines controller] } {
	if { [mines nextController] == -1 } {
	   mines controller
	   mines controllerName
	} else {
	    mines controller [lindex [mines nextController] 0]
	    mines controllerName [lindex [mines nextControllerName] 0]
# If nextController != -1 do this
	    mines nextController [lreplace [mines nextController] 0 0]
	    mines nextControllerName [lreplace [mines nextControllerName] 0 0]
# Else use noone?
	}
    } elseif { [lsearch [mines nextController] $who] != -1 } {
	while { [set idx [lsearch [mines nextController] $who]] != -1 } {
	    mines nextController [lreplace [mines nextController] $idx $idx]
	    mines nextControllerName [lreplace [mines nextControllerName] \
		    $idx $idx]
	}
	
    }
}

proc doChoice {title msg choices} {

  if { [users local.usernum] == [mines controller] } {  
      set state normal
  } else {  
      set state disabled 
  }

  foreach choice $choices {
      lappend newChoices [list [lindex $choice 0] \
	      "gk_toAll destroy .choice; [lindex $choice 1]"]
  }
  if [winfo exists .choice] { destroy .choice }
  gk_choice .choice -title $title -msg $msg -choices $newChoices -state $state
  update
}

######### random routines

# Choose $num elements at random from $l
proc choose {num l} {
  random seed
  set nl {}
  for {set i 1} {$i <= $num} {incr i} {
    set index [random [llength $l]]
    lappend nl [lindex $l $index]
    set l [lreplace $l $index $index]
  }
  return $nl
}

proc chooseOld {num l} {
  lrange [randOrder $l] 0 [expr {$num - 1}]
}

# Put $l (a list) into random order
proc randOrder {l} {
  set newlist {}
  while {! [lempty $l]} {
    set x [extractRandomElement $l]
    lappend newlist [lindex $x 0]
    set l [lindex $x 1]
  }
  return $newlist
}

# Extract a single element from $l at random. Return the element, followed
# by the list, extracting the element from the list.
proc extractRandomElement {list} {
  random seed
  set index [random [llength $list]]
  set el [lindex $list $index]
  set newlist [concat [lrange $list 0 [expr {$index - 1}]] [lrange $list [expr {$index + 1}] [llength $list]]]
  return [list $el $newlist]
}



##########################  Okay now lets play!! #####################

# Create an environment to store all of the needed information
# and bind to the appropriate events.
gk_newenv -share -bind mines
mines bind envReceived "updateEnvironment"
mines bind addEnvInfo "envUpdate . %K"
mines bind changeEnvInfo "envUpdate . %K"
bind userDeleted "userLeft . %U"

# Now put the GroupKit telepointers
gk_initializeTelepointers

# Finally make the board if you are the originator only!!
# Only the originator of the conference can be intialized as the
# controller!
if  [gk_amOriginator] {
    mines controller [users local.usernum]
    mines controllerName [users local.username]
    mines nextController -1
    mines nextControllerName {nobody}
}

makeBoard . 10 10 [stdmines 10 10]





