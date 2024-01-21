## use groupkit environments to manipulate a deck of cards on a
## table -- rules?  who needs no steenking rules!
##    this version by mark roseman

gk_initConf $argv
gk_defaultMenu .menubar

canvas .c -bg ForestGreen -width 500 -height 500
pack append . .menubar {top fillx} .c top

gk_initializeTelepointers
gk_specializeWidgetTreeTelepointer .c

gk_newenv -bind -share deck
gk_newenv cardinfo

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
  .c bind $id <1> "startDrag %x %y"
  .c bind $id <B1-Motion> "continueDrag $card %x %y"
  .c bind $id <2> "flipCard $card"
  .c bind $id <3> "peekCard $card"
  .c bind $id <B3-ButtonRelease> "unpeekCard $card"
}

proc startDrag {x y} {
  cardinfo misc.startx $x
  cardinfo misc.starty $y
}

proc continueDrag {card x y} {
  set dX [expr $x-[cardinfo misc.startx]]
  set dY [expr $y-[cardinfo misc.starty]]
  cardinfo misc.startx $x
  cardinfo misc.starty $y
  set posn [deck cards.$card.posn]
  scan $posn "%d %d" x0 y0  
  incr x0 $dX; incr y0 $dY
  deck cards.$card.posn "$x0 $y0"
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


proc cardBitmap card {  global gk_library
  set bmpdir $gk_library/library/tksol-0.9
  if {[deck cards.$card.state]=="up"} {
    return @$bmpdir/cardbitmaps/$card
  } else {
    return @$bmpdir/downbitmaps/down
  }
}

proc peekCard card {  global gk_library
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
