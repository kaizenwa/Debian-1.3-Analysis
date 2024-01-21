gk_initConf $argv
gk_defaultMenu .menubar

# node_info environment has all the graph info in it.

gk_newenv -notify -share node_info

wm minsize . 70 70
set nodes 0; set lines 0

pack .menubar -side top -fill x

toplevel .sliders 
frame .sliders.scaling -relief raised -borderwidth 3
pack .sliders.scaling -fill x	
wm title .sliders "Lens Configuration"
frame .viewer 
pack .viewer -side bottom

.menubar itemcommand 0 insert 1 command -label Save -command save
.menubar itemcommand 0 insert 2 command -label Load -command load
.menubar itemcommand 0 insert 3 separator

.menubar itemcommand 2 insert 1 command -label "About Offset Lens..."  \
	-command about
.menubar itemcommand 2 insert 2 command -label "Using the Offset Lens..."  \
	-command help

canvas .sliders.scaling.magwidg -relief ridge -height 200 -width 300 -bd 4
pack .sliders.scaling.magwidg

scale .sliders.scaling.grey -length 300 -from 0 -to 3 \
	-label "Background greyness" -orient horizontal -command grey
pack .sliders.scaling.grey -pady 5

checkbutton .sliders.scaling.clearbox -text "Filled Mag Box" \
	-variable filled_box
pack .sliders.scaling.clearbox

label .sliders.scaling.lock -text "Lock OFF"
pack .sliders.scaling.lock

canvas .viewer.net -width 700 -height 500 -background white
pack .viewer.net
.viewer.net create rectangle 0 0 800 600 -outline "" \
	-stipple gray25 -fill black -tag bgrect

bind .viewer.net <ButtonPress-1> {addnode %x %y [node_info get num]}
bind .viewer.net <Button2-Motion> {dragline %x %y}
bind .viewer.net <ButtonRelease-2> {endline %x %y}
bind .viewer.net <Motion> {mag %x %y}
bind .viewer.net <ButtonPress-3> {hold_mag %x %y}

set line_in_progress 0
gk_on {([gk_event type]=="addEnvInfo") && ([gk_event env]=="node_info")} {
  set pieces [split [gk_event key] .]
  set second [lindex $pieces 1]
  set name [lindex $pieces 0]
  if {$second=="text"} {
    makenode $name
  }
}

node_info set num 1
node_info set current_line 1

gk_on {([gk_event type]=="changeEnvInfo") && ([gk_event env]=="node_info")} {
  set pieces [split [gk_event key] .]
  set second [lindex $pieces 1]
  set name [lindex $pieces 0]
  if {$second=="end_line"} {
    drawline [node_info $name.start_line] [node_info $name.end_line]
  }
}

gk_on {[gk_event type] == "envReceived"} {
  new_comer
}

# magwidg_setup: setup the magnification widget.
# c is for canvas. x is for canvas height; y, width.

proc magwidg_setup { c x y} {
  global v

  set v(width) $x
  set v(height) $y

  # starting coordinates for smaller rectangle
  set v(x1) 10
  set v(y1) 10
  set v(x2) 35
  set v(y2) 35

  # starting coordinates for larger rectangle

  set v(X1) 45
  set v(Y1) 35
  set v(X2) 145
  set v(Y2) 135

  # create the rectangles and the magnification lines....

  $c create rectan $v(x1) $v(y1) $v(x2) $v(y2) -tag box_small
  $c create line $v(x1) $v(y1) $v(X1) $v(Y1)\
      -fill gray -tags line_tl -arrow last
  $c create line $v(x1) $v(y2) $v(X1) $v(Y2)\
      -fill gray -tags line_tr -arrow last
  $c create line $v(x2) $v(y1) $v(X2) $v(Y1)\
      -fill gray -tags line_bl -arrow last
  $c create line $v(x2) $v(y2) $v(X2) $v(Y2)\
      -fill gray -tags line_br -arrow last
  $c create rectan $v(X1) $v(Y1) $v(X2) $v(Y2) -tag box_large

  # create the little boxes used for changing the size of the big boxes.

  $c create rectan [expr $v(x2) - 3] [expr $v(y1) - 3] [expr $v(x2) + 3] [expr $v(y1) + 3]\
      -tags adj_small
  $c create rectan [expr $v(X2) - 3] [expr $v(Y1) - 3] [expr $v(X2) + 3] [expr $v(Y1) + 3]\
      -tag adj_large

  # bind the visual cues when entering/leaving something.

  $c bind box_small <Enter> "$c itemconfigure box_small -width 3"
  $c bind box_small <Leave> "$c itemconfigure box_small -width 1" 
  $c bind box_large <Enter> "$c itemconfigure box_large -width 3"
  $c bind box_large <Leave> "$c itemconfigure box_large -width 1"

  $c bind adj_small <Enter> "$c itemconfigure adj_small -fill red"
  $c bind adj_small <Leave> "$c itemconfigure adj_small -fill {}"
  $c bind adj_large <Enter> "$c itemconfigure adj_large -fill red"
  $c bind adj_large <Leave> "$c itemconfigure adj_large -fill {}"

  # bind the actions for moving

  $c bind box_small <ButtonPress-1> "start_drag_box small %x %y"
  $c bind box_large <ButtonPress-1> "start_drag_box large %x %y"

  $c bind box_small <B1-Motion> "do_drag_box $c small %x %y"
  $c bind box_large <B1-Motion> "do_drag_box $c large %x %y"

  # bind the actions for resizing

  $c bind adj_small <ButtonPress-1> "start_resize_box small %x %y"
  $c bind adj_large <ButtonPress-1> "start_resize_box large %x %y"

  $c bind adj_small <B1-Motion> "do_resize_box $c small %x %y"
  $c bind adj_large <B1-Motion> "do_resize_box $c large %x %y"
}

# start_drag_box
# box: "large" for enlarged box. "small" for the other box
# x,y point at which the drag started

proc start_drag_box {box x y} {
  global v

  set v(start_drag_${box}_x) $x
  set v(start_drag_${box}_y) $y
}

# do_drag_box: Nice dynamic display of moving box.
# c is for canvas. box is "large"|"small". x,y is position of mouse.

proc do_drag_box { c box x y} {
  global v

  set xdiff [expr $x - $v(start_drag_${box}_x)]
  set ydiff [expr $y - $v(start_drag_${box}_y)]
  set v(start_drag_${box}_x) $x
  set v(start_drag_${box}_y) $y


  if {$box=="small"} {
    set v(x1) [expr $v(x1) + $xdiff]
    set v(x2) [expr $v(x2) + $xdiff]
    set v(y1) [expr $v(y1) + $ydiff]
    set v(y2) [expr $v(y2) + $ydiff]
    check_in_bounds_small
    update_mag_box $c
  } elseif {$box=="large"} {
    set v(X1) [expr $v(X1) + $xdiff]
    set v(X2) [expr $v(X2) + $xdiff]
    set v(Y1) [expr $v(Y1) + $ydiff]
    set v(Y2) [expr $v(Y2) + $ydiff]
    check_in_bounds_large
    update_mag_box $c
  } else {
    puts stderr "Shouldn't ever get here: do_drag_box $box $x $y"
    exit 1
  }
}

# check_in_bounds_small

proc check_in_bounds_small {} {
  global v

  # resize check

  set l [expr $v(x2) - $v(x1)]
  if {$l > ($v(height) - 6)} {
    set l [expr $v(height) - 6]
    set v(x2) [expr $v(x1) + $l]
    set v(y2) [expr $v(y1) + $l]
  }

  # move check

  if {$v(x1) < 6} {
    set v(x1) 6
    set v(x2) [expr 6 + $l]
  }
  if {$v(y1) < 6} {
    set v(y1) 6
    set v(y2) [expr 6 + $l]
  }
  if {$v(y2) > $v(height)} {
    set v(y2) $v(height)
    set v(y1) [expr $v(height) - $l]
  }
  if {$v(x2) > $v(width)} {
    set v(x2) $v(width)
    set v(x1) [expr $v(width) - $l]
  }
}

# check_in_bounds_large

proc check_in_bounds_large {} {
  global v

  # resize check

  set l [expr $v(X2) - $v(X1)]
  if {$l > ($v(height) - 6)} {
    set l [expr $v(height) - 6]
    set v(X2) [expr $v(X1) + $l]
    set v(Y2) [expr $v(Y1) + $l]
  }
  # move check

  if {$v(X1) < 6} {
    set v(X1) 6
    set v(X2) [expr 6 + $l]
  }
  if {$v(Y1) < 6} {
    set v(Y1) 6
    set v(Y2) [expr 6 + $l]
  }
  if {$v(Y2) > $v(height)} {
    set v(Y2) $v(height)
    set v(Y1) [expr $v(height) - $l]
  }
  if {$v(X2) > $v(width)} {
    set v(X2) $v(width)
    set v(X1) [expr $v(width) - $l]
  }
}

# start_resize_box
# box = "large"|"small"
# x,y is position of start to resize.

proc start_resize_box {box x y} {
  global v

  set v(start_resize_${box}_x) $x
  set v(start_resize_${box}_y) $y
}

# do_resize_box: nice dynamic display of resizing.

proc do_resize_box {c box x y} {
  global v 

  # work out changes in x. Force a square box, so ydiff = xdiff.

  set xdiff [expr $x - $v(start_resize_${box}_x)]
  set ydiff -$xdiff
  set v(start_resize_${box}_x) $x
  set v(start_resize_${box}_y) $y

  if {$box=="small"} {
    set v(x2) [expr $v(x2) + $xdiff]
    set v(y1) [expr $v(y1) + $ydiff]
    if {[expr $v(x2) - $v(x1)] < 10} {
      set v(x2) [expr $v(x1) + 10]
    }
    if {[expr $v(y2) - $v(y1)] < 10} {
      set v(y1) [expr $v(y2) - 10]
    }
    check_in_bounds_small
    update_mag_box $c
  } elseif {$box=="large"} {
    set v(X2) [expr $v(X2) + $xdiff]
    set v(Y1) [expr $v(Y1) + $ydiff]
    if {[expr $v(X2) - $v(X1)] < 10} {
      set v(X2) [expr $v(X1) + 10]
    }
    if {[expr $v(Y2) - $v(Y1)] < 10} {
      set v(Y1) [expr $v(Y2) - 10]
    }
    check_in_bounds_large
    update_mag_box $c
  } else {
    puts stderr "Shouldn't ever get here: do_resize_box $box $x $y"
    exit 1
  }
}

# update_mag_box: Update the display with the new information from move/resize
# c is for canvas

proc update_mag_box { c } {
  global v sfactor_mag sfactor_back startx starty movex movey bbox
  
  unmag

  # update small and large boxes of mag widget.

  $c coords box_small $v(x1) $v(y1) $v(x2) $v(y2)
  $c coords adj_small\
      [expr $v(x2) - 3] [expr $v(y1) - 3] [expr $v(x2) + 3] [expr $v(y1) + 3]
  $c coords box_large $v(X1) $v(Y1) $v(X2) $v(Y2)
  $c coords adj_large\
      [expr $v(X2) - 3] [expr $v(Y1) - 3] [expr $v(X2) + 3] [expr $v(Y1) + 3]

  # redraw mag widget lines

  $c coords line_tl $v(x1) $v(y1) $v(X1) $v(Y1)
  $c coords line_tr $v(x1) $v(y2) $v(X1) $v(Y2)
  $c coords line_bl $v(x2) $v(y1) $v(X2) $v(Y1)
  $c coords line_br $v(x2) $v(y2) $v(X2) $v(Y2)

  # calculate scalefactor

  set sfactor_mag [expr ($v(X2) - $v(X1).) /  ($v(x2) - $v(x1))]
  set sfactor_back [expr 1.0/$sfactor_mag]

  # calculate the x and y distance needed to move the magnified box so
  # it is in the right place relative to the smaller box, as shown in
  # the magnification widget.

  set movex [expr $v(X2) + $v(X1) - $v(x2) - $v(x1) + 0]
  set movey [expr $v(Y2) + $v(Y1) - $v(y2) - $v(y1) + 0]

  set bbox [expr ($v(x2) - $v(x1))]
  mag $startx $starty
}

# addnode
# Adds the node to the node_info environment.
# This will trigger the makenode procedure for all participants.

proc addnode {x y text} {
  global me_id orig is_held unmag_x unmag_y

#   if {$is_held} {
#     set x $unmag_x
#     set y $unmag_y
#   }

  if {[node_at_point $x $y] != "nix"} {
    return
  }
  set nodes [node_info get num]
  node_info set num [expr $nodes + 1]

  node_info set node$nodes.whoby [users local.usernum]
  node_info set node$nodes.x $x
  node_info set node$nodes.y $y
  node_info set node$nodes.num_links 0
  node_info set node$nodes.text $text

}

# makenode: draw the node on the screen

proc makenode {node} {
  global is_held sfactor_mag startx starty sfactor_back me_id unmag_x unmag_y
  global movex movey

  # get node info from node_info environment

  set id [string range $node 4 end]
  set x [node_info get $node.x]
  set y [node_info get $node.y]
  set text [node_info get $node.text]

  # need different actions for lock on/off

  if {$is_held && ([node_info get $node.whoby] == [users local.usernum])} {
    set x1 [expr $unmag_x-10]; set y1 [expr $unmag_y-10]
    set x2 [expr $unmag_x+10]; set y2 [expr $unmag_y+10]

    .viewer.net create oval $x1 $y1 $x2 $y2\
        -fill red -tags node$id
    .viewer.net addtag mag_bit withtag node$id
    .viewer.net create text $unmag_x $unmag_y -anchor c -text $text\
        -font -adobe-helvetica-*-r-*-*-8-*-*-*-*-*-*-* -tag text$id
    .viewer.net addtag mag_bit withtag text$id

    .viewer.net create oval $x1 $y1 $x2 $y2 -tag cunning$id \
        -fill white
    .viewer.net create text $unmag_x $unmag_y -anchor c -text $text \
        -tag cunningt$id -font -adobe-helvetica-*-r-*-*-8-*-*-*-*-*-*-*

    .viewer.net scale node$id $startx $starty $sfactor_mag $sfactor_mag
    .viewer.net scale text$id $startx $starty $sfactor_mag $sfactor_mag
    .viewer.net move node$id $movex $movey
    .viewer.net move text$id $movex $movey

    node_info $node.x $unmag_x
    node_info $node.y $unmag_y
  } else {
    set x1 [expr $x-10]; set y1 [expr $y-10]
    set x2 [expr $x+10]; set y2 [expr $y+10]
    
    .viewer.net create oval $x1 $y1 $x2 $y2 -tag node$id \
        -fill red
    .viewer.net create text $x $y -anchor c -text $text \
        -tag text$id -font -adobe-helvetica-*-r-*-*-8-*-*-*-*-*-*-*

    .viewer.net create oval $x1 $y1 $x2 $y2 -tag cunning$id \
        -fill white
    .viewer.net create text $x $y -anchor c -text $text \
        -tag cunningt$id -font -adobe-helvetica-*-r-*-*-8-*-*-*-*-*-*-*
  }

  .viewer.net raise text$id node$id
  .viewer.net raise cunningt$id cunning$id 

  # do the bindy thing.

  .viewer.net bind cunning$id <ButtonPress-2> "startline $id %x %y"
  .viewer.net bind cunningt$id <ButtonPress-2> "startline $id %x %y"
  .viewer.net bind node$id <ButtonPress-2> "startline $id %x %y"
  .viewer.net bind text$id <ButtonPress-2> "startline $id %x %y"

  .viewer.net bind cunning$id <Enter> \
      ".viewer.net itemconfigure cunning$id -width 5"
  .viewer.net bind cunning$id <Leave> \
      ".viewer.net itemconfigure cunning$id -width 1"
  .viewer.net bind cunningt$id <Enter> \
      ".viewer.net itemconfigure cunning$id -width 5"
  .viewer.net bind cunningt$id <Leave> \
      ".viewer.net itemconfigure cunning$id -width 1"

}

# node_at_point
# find out what node (if any) is at a point (x,y)

proc node_at_point {x y} {
  set tags [.viewer.net find overlapping $x $y $x $y]]

  foreach tag $tags {
    set t [.viewer.net gettags $tag]
    foreach subtag $t {
      if {[string match "cunning*" $subtag] && 
	  ![string match "cunning\[lt\]*" $subtag]} {
	return [string range $subtag 7 end]
      }
      if {[string match "node*" $subtag]} {
	return [string range $subtag 4 end]
      }
    }
  }
  return "nix"
}

# startline: start a line at xpos,ypos from a node "node".

proc startline {node xpos ypos} {
  global linex1 liney1 line_in_progress lines me_id is_held unmag_x unmag-y

  set linex1 [node_info get node$node.x] 
  set liney1 [node_info get node$node.y]
  set biglinex1 $xpos
  set bigliney1 $ypos
  incr lines
  .viewer.net create line $linex1 $liney1 $linex1 $liney1 -tag line$lines -fill red
  .viewer.net create line $linex1 $liney1 $linex1 $liney1 -tag cunningli$lines
  gk_toOthers .viewer.net create line $linex1 $liney1 $linex1 $liney1 \
      -tag temp_line$me_id

  set line_in_progress $node
}

# dragline: nice dynamic rubberbandy stuff.

proc dragline {x y} {
  global lines linex1 liney1 is_held startx starty sfactor_back line_in_progress
  global me_id unmag_x unmag_y sfactor_mag movex movey

  if {!$line_in_progress} {
    return
  }

  if {!$is_held} {
    .viewer.net coords line$lines $linex1 $liney1 $x $y
    .viewer.net coords cunningli$lines $linex1 $liney1 $x $y
    gk_toOthers .viewer.net coords temp_line$me_id $linex1 $liney1 $x $y
  } else {
    draw_mini_cursor $x $y 0
    .viewer.net coords line$lines $linex1 $liney1 $unmag_x $unmag_y
    .viewer.net coords cunningli$lines $linex1 $liney1 $unmag_x $unmag_y
    gk_toOthers .viewer.net coords temp_line$me_id $linex1 $liney1 $unmag_x $unmag_y
    .viewer.net scale cunningli$lines $startx  $starty $sfactor_mag $sfactor_mag
    .viewer.net move cunningli$lines $movex $movey
    .viewer.net addtag mag_bit withtag line$lines
  }

  update idletasks
}

# endline: finish line at real_x real_y. translate this to end node and
# put the info on the node_info environment. This triggers the drawline proc


proc endline {real_x real_y} {
  global lines linex1 liney1 startx starty sfactor_back line_in_progress
  global newline me_id is_held unmag_x unmag_y

  if {!$line_in_progress} {
    return
  }
  .viewer.net delete line$lines
  .viewer.net delete cunningli$lines
  gk_toOthers .viewer.net delete temp_line$me_id

  if {$is_held} {
    set end_node [node_at_point $unmag_x $unmag_y]
  } else {
    set end_node [node_at_point $real_x $real_y]
  }

  if {$end_node == "nix"} {
    return
  }

  set newline [node_info get current_line]
  node_info set current_line [expr $newline + 1]

  # update links information in node_info environment.
  # first, update start node

  set num_links_start [node_info get node$line_in_progress.num_links]
  incr num_links_start
  node_info set node$line_in_progress.$num_links_start $end_node
  node_info set node$line_in_progress.num_links $num_links_start
  node_info set user$me_id.start_line node$line_in_progress

  #then, update end node

  set num_links_end [node_info get node$end_node.num_links]
  incr num_links_end
  node_info set node$end_node.$num_links_end $line_in_progress
  node_info set node$end_node.num_links $num_links_end
  node_info set user$me_id.end_line node$end_node
  
  set line_in_progress 0
}

# drawline. draws line from one node to another.

proc drawline {from to} {
  global lines linex1 liney1 is_held startx starty sfactor_mag me_id
  global movex movey

  if {$from == 0 && $to == 0} {return}

  set lines [node_info get current_line]

  set to_x [node_info get $to.x]
  set to_y [node_info get $to.y]

  set from_x [node_info get $from.x]
  set from_y [node_info get $from.y]

  .viewer.net create line $from_x $from_y $to_x $to_y -tag line$lines
  .viewer.net create line $from_x $from_y $to_x $to_y -tag cunningli$lines
  .viewer.net raise line$lines

  if $is_held {
    .viewer.net scale cunningli$lines $startx  $starty $sfactor_mag $sfactor_mag
    .viewer.net move cunningli$lines $movex $movey
    .viewer.net addtag mag_bit withtag cunningli$lines
  }
  set details [.viewer.net coords cunningli$lines]
}

# make_rect: make rectangle for $who at $x,$y, width $bbox, colour $my_colour

proc make_rect {who x y bbox my_colour} {
  .viewer.net create rectangle [expr $x - $bbox] [expr $y - $bbox] \
		[expr $x + $bbox] [expr $y + $bbox] -tag rect$who \
		 -outline $my_colour 
}

# unmag: unmags everything magged.

proc unmag {} {
  global startx starty sfactor_mag sfactor_back me_id unmag_which
  global movex movey oldx oldy

  .viewer.net move mag_bit [expr $movex * -1] [expr $movey * -1]
  .viewer.net scale mag_bit $startx $starty $sfactor_back $sfactor_back
  .viewer.net lower mag_bit bgrect
  .viewer.net dtag mag_bit
  .viewer.net dtag back
  .viewer.net dtag raise_this
  .viewer.net delete biggun
  gk_toAll .viewer.net delete rect$me_id
}

# mag: mags everything.

proc mag {x y} {
  global first startx starty sfactor_mag sfactor_back bbox xoff yoff me_id
  global last_which mag_lines filled_box my_colour unmag_which bbox_bound
  global xdiff ydiff oldx oldy movex movey
  
  if {$x < $bbox_bound} {set x $bbox_bound}
  if {$y < $bbox_bound} {set y $bbox_bound}

  if !$first { 
        unmag
  } else {
        set first 0
  }

  # first, figure out what to magnify

  gk_toAll make_rect $me_id $x $y $bbox $my_colour
  .viewer.net create rectangle [expr $x - $bbox] [expr $y - $bbox] \
                [expr $x + $bbox] [expr $y + $bbox] -tag biggun \
                 -outline $my_colour

  set unmag_which [.viewer.net find overlapping [expr $x - $bbox] \
		[expr $y - $bbox] [expr $x + $bbox] [expr $y + $bbox]]
  foreach obj $unmag_which {
    set all_tags [.viewer.net gettags $obj]
    if {[string match "node*" $all_tags]} {
      .viewer.net addtag mag_bit withtag $obj
    }
    if {[string match "text*" $all_tags]} {
      .viewer.net addtag mag_bit withtag $obj
      .viewer.net addtag raise_this withtag $obj
    }
  }

  .viewer.net raise mag_bit biggun
  .viewer.net addtag mag_bit withtag biggun

  # magnify everything, then move it to the right place as shown in mag widget

  .viewer.net scale mag_bit $x $y $sfactor_mag $sfactor_mag
  .viewer.net move mag_bit $movex $movey
  .viewer.net raise raise_this
  if $filled_box {
      .viewer.net itemconfigure biggun -fill yellow
  }
  foreach i $unmag_which {
    set all_tags [.viewer.net gettags $i]
    if ![string match "*cunningl*" $all_tags] {
      catch {.viewer.net lower $i [expr $i + 1] }
    }
  }

  set startx $x;set starty $y

  # draw maglines bewtween magboxes

  set Coords [.viewer.net coords biggun]
  set coords [.viewer.net coords rect$me_id]

  set x1 [lindex $coords 0]
  set y1 [lindex $coords 1]
  set x2 [lindex $coords 2]
  set y2 [lindex $coords 3]
  set X1 [lindex $Coords 0]
  set Y1 [lindex $Coords 1]
  set X2 [lindex $Coords 2]
  set Y2 [lindex $Coords 3]
  .viewer.net delete magline
  .viewer.net create line $x1 $y1 $X1 $Y1 -tag magline -arrow last -fill grey
  .viewer.net create line $x1 $y2 $X1 $Y2 -tag magline -arrow last -fill grey
  .viewer.net create line $x2 $y1 $X2 $Y1 -tag magline -arrow last -fill grey
  .viewer.net create line $x2 $y2 $X2 $Y2 -tag magline -arrow last -fill grey
  update idletasks
}


# draw_mini_cursor: when mag lock is on, draw the minicursor.
# x,y position of mouse pointer.
# gkit - porpogate this information to others?

proc draw_mini_cursor {x y gkit} {
  global me_id my_colour startx starty sfactor_back unmag_x unmag_y
  global movex movey oldx oldy

  .viewer.net delete glob_curs$me_id
  if $gkit { gk_toAll .viewer.net delete glob_curs$me_id }
  .viewer.net create oval [expr $x -6] [expr $y -6] \
	[expr $x +6]  [expr $y + 6] -fill $my_colour \
	-tag glob_curs$me_id
  .viewer.net move glob_curs$me_id [expr $movex * -1] [expr $movey * -1]
  .viewer.net scale glob_curs$me_id $startx $starty \
 	$sfactor_back $sfactor_back

  set curs_where [.viewer.net coords glob_curs$me_id]
  set unmag_x [expr ([lindex $curs_where 0] + [lindex $curs_where 2])/2.0]
  set unmag_y [expr ([lindex $curs_where 1] + [lindex $curs_where 3])/2.0]

  if $gkit {
	gk_toOthers .viewer.net create oval [lindex $curs_where 0] \
	[lindex $curs_where 1]  [lindex $curs_where 2]  \
	[lindex $curs_where 3] -tag glob_curs$me_id
  }
}

# hold_mag: (un)lock mag at x,y

proc hold_mag {x y} {
  global is_held me_id 

  set is_held [expr !$is_held]
  if $is_held {
      .sliders.scaling.lock configure -text "Lock ON"
     bind .viewer.net <Motion> {draw_mini_cursor %x %y 1}

  } else {
    .sliders.scaling.lock configure -text "Lock OFF"
    .viewer.net delete glob_curs$me_id
    bind .viewer.net <Motion> {mag %x %y}
    gk_toAll .viewer.net delete glob_curs$me_id
    mag $x $y
  }
}

# save graph structure. Doesn't save mag coordinates. maybe in the next version

proc save {} {
  set filename [FSBox "File-name to save"]
  if {$filename ==""} return

  set keys [node_info keys]
  set num_nodes [expr [llength $keys] - 1]

  set fid [open $filename w]

  puts $fid "\# mag.tcl file format version 1.0"
  
  foreach key $keys {
    if {[string match "node*" $key]} {
      set node_id [string range $key 4 end]
      set x_pos [node_info get $key.x]
      set y_pos [node_info get $key.y]
      set text [node_info get $key.text]
      set num_links [node_info get $key.num_links]
      puts $fid "$key $x_pos $y_pos $text $num_links"
      for {set links 1} {$links <= $num_links} {incr links} {
	set other_id [node_info get $key.$links]
	if {![info exists linkdb($other_id,$node_id)]} {
	  set linkdb($node_id,$other_id) "link_info"
	}
      }
    }
  }
  puts $fid "links"
  set links [array names linkdb]
  foreach link $links {
    puts $fid $link
  }
  puts $fid "end"
  close $fid
}

# load. Load graph structure from file.

proc load {} {
  global me_id

  set filename [FSBox "File-name to load"]
  if {$filename ==""} return

  set fid [open $filename r]

  set node_line [next_line $fid 1]
  while {$node_line != "links"} {
    set node [lindex $node_line 0]
    node_info set $node.x [lindex $node_line 1]
    node_info set $node.y [lindex $node_line 2]
    node_info set $node.num_links 0
    node_info set $node.text [lindex $node_line 3]
    set num [node_info get num]
    node_info set num [expr $num + 1]
    set node_line [next_line $fid 1]
  }

  set link_line [next_line $fid 1]
  
  while {$link_line != "end"} {
    set links [split $link_line ,]
    set from [lindex $links 0]
    set to [lindex $links 1]
    
    set num_links_from [node_info get node$from.num_links]
    incr num_links_from
    node_info set node$from.$num_links_from $to
    node_info set node$from.num_links $num_links_from
    node_info set user$me_id.start_line node$from
    
    set num_links_end [node_info get node$to.num_links]
    incr num_links_end
    node_info set node$to.$num_links_end $from
    node_info set node$to.num_links $num_links_end
    node_info set user$me_id.end_line node$to
    set link_line [next_line $fid 1]
  }
  close $fid

}

# next_line: get next_line of file $fid.

proc next_line { fid no_eof_expected } {

  set line "\#dummy"

  while {[string index $line 0] == "\#"} {
    set err [gets $fid line]
    if {$err == -1} {
      if {$no_eof_expected} {
	puts stderr "Unexpected eof in file $fid"
	close $fid
	exit 1
      }
      return "eof"
    }
  }
  return $line
}

# new_comer. Update the newcomer with what the others see.

proc new_comer {} {

  set keys [node_info keys]
  foreach key $keys {
    if {[string match "node*" $key]} {
      set node_id [string range $key 4 end]
      set num_links [node_info get $key.num_links]
      makenode node$node_id
      for {set links 1} {$links <= $num_links} {incr links} {
	set other_id [node_info get $key.$links]
	if {![info exists linkdb($other_id,$node_id)]} {
	  set linkdb($node_id,$other_id) "link_info"
	}
      }
    }
  }
  set links [array names linkdb]
  foreach link $links {
    set s [split $link ,]
    drawline node[lindex $s 0] node[lindex $s 1]
  }
}

# grey: greyscale of background.

proc grey { val } {

  switch $val {
    0 {
       .viewer.net itemconfigure bgrect -fill ""
      }
    1 {
       .viewer.net itemconfigure bgrect -fill black -stipple gray25
      }
    2 {
       .viewer.net itemconfigure bgrect -fill black -stipple gray50
      }
    3 {
       .viewer.net itemconfigure bgrect -fill black -stipple ""
      }
  }
}

proc about {} {
  toplevel .about
  message .about.mess -text "Principal investigator: \nAndy \
Cockburn, University of Canterbury, Christchurch, New Zealand.\n\n
Implemented by: \nAndy Cockburn and Carl Cerecke. \n\n
Design contributions by: \nCarl Gutwin, Saul Greenberg, and Mark Roseman.\n\n
Comments and queries to andy@cosc.canterbury.ac.nz"
pack .about.mess
button .about.b -text "Dismiss" -command {destroy .about}
pack .about.b -fill x
  wm title .about "About the Offset Lens"
}

proc help {} {
  toplevel .help
  message .help.mess -font "-adobe-times-roman-r-*-*-*-160-*-*-*-*-*-*" \
	 -text "The offset lens is a graphical node editor and viewer.\n
The left mouse button creates numbered nodes. \nThe middle mouse button creates lines between nodes (by dragging).  \n
A magnifying glass follows your cursor, and any nodes overlapping with the lens will be magnified within the magnified region (the square with arrows pointing towards it).  \n
The size and magnificaiton power of lens can be configured through the \"Lens Configuration\" window.  The relative positions of the Lens and the magnified region can also be configured.  \n
Clicking the right hand mouse button locks the magnification lens allowing you to edit and create magnified nodes.  When doing this, a small cursor shows you (and all other users) the position of your action on the unmagnified region... \n
To get the hand of it, give it a go!"
  pack .help.mess
  button .help.b -text "Dismiss" -command "destroy .help"
  pack .help.b -fill x
  wm title .help "Using the Offset Lens"
}

set first 1


set xoff -50; set yoff -50
set bbox 30
set bbox_bound [expr 1.5 * $bbox + 5]
set sfactor_mag 4
set sfactor_back 0.25
.sliders.scaling.grey set 1

magwidg_setup .sliders.scaling.magwidg 300 200

set me [gk_getLocalAttrib username]
set is_space [string first " " $me]
if {$is_space != -1} {
   set me [string range $me 0 [expr $is_space - 1]]
}
set last_which ""
set mag_lines 0
set filled_box 0
set my_colour [userprefs color]
set is_held 0
set movex 50
set movey 50


if  [gk_amOriginator] {
    set orig [users local.usernum]
}

gk_on {[gk_event type] == "updateEntrant"} {
   if [gk_amOriginator] {
      gk_toUserNum [gk_event usernum] set orig  [users local.usernum]
   }
}
set me_id [users local.usernum]

mag 300 300
update_mag_box .sliders.scaling.magwidg

node_info set user$me_id.start_line 0
node_info set user$me_id.end_line 0
