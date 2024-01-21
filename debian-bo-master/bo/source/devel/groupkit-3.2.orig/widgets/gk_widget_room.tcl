


proc gk_room {w args} {
    eval gkInt_CreateWidget $w gkRoom GkRoom $args
    return $w
}

proc gkRoom_CreateClassRec {} {
    global gkRoom

    set gkRoom(inherit) {label}
    set gkRoom(methods) {adduser deluser addtool deltool component}
    set gkRoom(options) {-roomname -state -door}
    set gkRoom(-roomname) {-roomname roomname RoomName <room>}
    set gkRoom(-state) {-state state State normal}
    set gkRoom(-door) {-door door Door open}
}

proc gkRoom_InitWidgetRec {w class className args} {
    upvar #0 $w gk_room
    set gk_Room(occupants) {}
}

proc gkRoom_ConstructWidget {w} {
    global gkRoom gk_library
    upvar #0 $w gk_room

    $w configure -relief raised -bg grey -borderwidth 2
    label $w.roomname -text [lindex $gkRoom(-roomname) 3] -bg grey
    pack $w.roomname 
    label $w.door -bitmap @$gk_library/library/bitmaps/open.xbm -bg grey
    pack $w.door
    pack [frame $w.users] -expand false
    pack [frame $w.tools] -expand false
}

proc gkRoom_Config {w option args} {
     upvar #0 $w gk_room
     switch -exact [string range $option 1 end] {
       roomname {$w.roomname configure -text [lindex $args 0]}
       state {gkRoom_setState $w $args}
       door {gkRoom_setDoor $w $args}
       default {$gk_room(rootCmd) configure $option $args}
     }
}

proc gkRoom_Methods {w command args} {
  upvar #0 $w gk_room
  set args [lindex $args 0]
  switch -exact $command {
    adduser { gkRoom_addUser $w $args }
    deluser { gkRoom_delUser $w $args }
    addtool { gkRoom_addTool $w $args }
    deltool { gkRoom_delTool $w $args }
    component { gkRoom_component $w $args }
    default {$gk_room(rootCmd) $command $args}
  }
}

proc gkRoom_addUser {w stuff} {
  upvar #0 $w gk_room
  if {[llength $stuff]!=2} {error "wrong # args"}
  set id [lindex $stuff 0]; set name [lindex $stuff 1]
  if {[info commands $w.users$id]!=""} {error "user exists with same id"}
  pack [label $w.users$id -text $name -bg grey -font 6x9] -before $w.tools
}

proc gkRoom_delUser {w stuff} {
  upvar #0 $w gk_room
  if {[llength $stuff]!=1} {error "wrong # args"}
  set id [lindex $stuff 0]
  if {[info commands $w.users$id]==""} {error "no such user"}
  destroy $w.users$id
}

proc gkRoom_addTool {w stuff} {
  upvar #0 $w gk_room
  if {[llength $stuff]!=2} {error "wrong # args"}
  set id [lindex $stuff 0]; set name [lindex $stuff 1]
  if {[info commands $w.tools$id]!=""} {error "tool exists with same id"}
  pack [label $w.tools$id -text $name -bg grey -font 6x9 -fg red]
}

proc gkRoom_delTool {w stuff} {
  upvar #0 $w gk_room
  if {[llength $stuff]!=1} {error "wrong # args"}
  set id [lindex $stuff 0]
  if {[info commands $w.tools$id]==""} {error "no such tool"}
  destroy $w.tools$id
  
}

proc gkRoom_setState {w state} {
  if {$state=="active"} {
    $w configure -borderwidth 6
  } else {
    $w configure -borderwidth 2
  }
}

proc gkRoom_setDoor {w state} {
  global gk_library
  switch $state {
    open {$w.door configure -bitmap @$gk_library/library/bitmaps/open.xbm}
    shut {$w.door configure -bitmap @$gk_library/library/bitmaps/shut.xbm}
    ajar {$w.door configure -bitmap @$gk_library/library/bitmaps/ajar.xbm}
    board {$w.door configure -bitmap @$gk_library/library/bitmaps/boarded.xbm}
  }
}

proc gkRoom_component {w stuff} {
  switch [lindex $stuff 0] {
    tool {set id [lindex $stuff 1]; return $w.tools$id}
    user {set id [lindex $stuff 1]; return $w.users$id}
    roomname {return $w.roomname}
    default {error "no such component"}
  }
}
