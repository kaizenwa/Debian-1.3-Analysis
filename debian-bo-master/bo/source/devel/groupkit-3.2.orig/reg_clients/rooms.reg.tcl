gk_initGenericRC $argv


###################################################################
# routines to react to changes in the rooms environment and dispatch
#


# information added to the environment
#    - new room, new attribute for a room, new user in a room
#
gk_on {([gk_event type]=="addEnvInfo")&&([gk_event env]=="rooms")} {
  room_newInfo [gk_event key]
}

proc room_newInfo key {
  set pieces [split $key .]
  if {[lindex $pieces 0]=="room"} {
    if {[llength $pieces]>2} {
      set id [lindex $pieces 1]
      if {[viewinfo exists room.$id]==0} {
	room_addRoom $id
      }
      if {[llength $pieces]==3} {
	set item [lindex $pieces 2]
	room_changeAttribute $id $item [rooms get room.$id.$item]
      } elseif {([llength $pieces]==4)&&([lindex $pieces 2]=="users")} {
	set user [lindex $pieces 3]
	room_addUser $id $user [rooms get room.$id.users.$user]
      } elseif {([llength $pieces]==4)&&([lindex $pieces 2]=="tools")} {
	set tool [lindex $pieces 3]
	room_addTool $id $tool [rooms get room.$id.tools.$tool] 
      }
    }
  }
}


# information removed from the environment
#    - deleted user or room
#
gk_on {([gk_event type]=="deleteEnvInfo")&&([gk_event env]=="rooms")} {
  room_delInfo [gk_event key]
}

proc room_delInfo key {
  set pieces [split $key .]
    if {[llength $pieces]==2} {
      room_deleteRoom [lindex $pieces 1]
    } elseif {([llength $pieces]==4) && ([lindex $pieces 2]=="users")} {
      set id [lindex $pieces 1]
      set user [lindex $pieces 3]
      room_delUser $id $user
    } elseif {([llength $pieces]==4) && ([lindex $pieces 2]=="tools")} {
      set id [lindex $pieces 1]
      set tool [lindex $pieces 3]
      room_delTool $id $tool
    }
}


# information changed in the environment
#    - attribute of a room
#
gk_on {([gk_event type]=="changeEnvInfo")&&([gk_event env]=="rooms")} {
  room_changeInfo [gk_event key]
}

proc room_changeInfo key {
  set pieces [split $key .]
  if {([llength $pieces]==3)&&([lindex $pieces 0]=="room")} {
    set id [lindex $pieces 1]
    set item [lindex $pieces 2]
    room_changeAttribute $id $item [rooms get room.$id.$item]
  }
}


###################################################################
# routines to add, delete and change info for rooms; these are not
# called directly, but as a result of changes in the "rooms"
# environment
#


# a new room has been added; create and configure a room widget for it
#
proc room_addRoom room {
  set w .f.c.room$room
  gk_room $w
  set x [rooms get room.$room.x]; set y [rooms get room.$room.y]
  if {$x==""} {set x 100}; if {$y==""} {set y 100}
  viewinfo set room.$room.canvasid [.f.c create window $x $y -window $w]
  bind $w <3> "room_startdrag $room %x %y"
  bind $w <B3-Motion> "room_keepdragging $room %x %y"
  bind $w <1> "room_moveTo $room"
  bind [$w component roomname] <3> "room_startdrag $room %x %y"
  bind [$w component roomname] <B3-Motion> "room_keepdragging $room %x %y"
  bind [$w component roomname] <1> "room_moveTo $room"
}


# an attribute of a room has changed; adjust the widget accordingly
#
proc room_changeAttribute {room attr val} {
  set w .f.c.room$room
  switch $attr {
    name { $w configure -roomname $val}
    description { }
    x { .f.c coords [viewinfo room.$room.canvasid] $val [rooms room.$room.y]}
    y { .f.c coords [viewinfo room.$room.canvasid] [rooms room.$room.x] $val}
    door { $w configure -door $val}
    default {  }
  }
}


# a room has been deleted; destroy the widget
#
proc room_deleteRoom room {
  destroy .f.c.room$room
  if {$room==[viewinfo misc.current_room]} {
    viewinfo misc.current_room ""
  }
  viewinfo delete room.$room
}


# used for starting a room drag
#
proc room_startdrag {room x y} {
}


# used for continuing a drag; change the room's position in the environment
#
proc room_keepdragging {room x y} {
	set cx [expr 1000*[lindex [.f.c xview] 0]]
	set cy [expr 1000*[lindex [.f.c yview] 0]]
	set newx [expr $x+[winfo x .f.c.room$room]+$cx]
	set newy [expr $y+[winfo y .f.c.room$room]+$cy]
	rooms set room.$room.x $newx
	rooms set room.$room.y $newy
}


###################################################################
# routines to react to adds and deletions of users in the
# rooms environment (not called directly)
#


# a user has been added to a room; inform the widget, and if the
# user is us, make the widget appear "active"
#
proc room_addUser {room id name} {
  set w .f.c.room$room
  $w adduser $id $name
  bind [$w component user $id] <1> "room_makeUserMenu $w $room $id %X %Y"
  if {$id==[room_getID]} {$w configure -state active}
}

proc room_makeUserMenu {w room id x y} {
	if [winfo exists $w.userpopup] {
		destroy $w.userpopup
	}
	menu $w.userpopup
	$w.userpopup add command -label "Delete Phantom" -command "room_userDelete $room $id $w"
	tk_popup $w.userpopup $x $y
}

proc room_userDelete {room id w} {
  rooms delete room.$room.users.$id
}

# a user has left a room; inform the widget, and if it was us,
# return the room to the "normal" appearance
#
proc room_delUser {room id} {
  set w .f.c.room$room
  $w deluser $id
  if {$id==[room_getID]} {$w configure -state normal}
}



###################################################################
# routines to react to adds and deletions of tools in the
# rooms environment (not called directly)
#


# a tool has been added to a room; inform the widget, and if this
# is the room we are in, create the tool
#
proc room_addTool {room id name} {
  set w .f.c.room$room
  $w addtool $id $name
  bind [$w component tool $id] <1> "room_makeToolMenu $w $id %X %Y"
  if {$room==[viewinfo misc.current_room]} {
      gk_callJoinConference $id
      gk_pollUsers $id
  }
}

proc room_makeToolMenu {w id x y} {
	if [winfo exists $w.toolpopup] {
		destroy $w.toolpopup
	}
    menu $w.toolpopup
    $w.toolpopup add command -label "Delete" -command "room_toolDelete $id $w"
	tk_popup $w.toolpopup $x $y
}

proc room_unpost w {
  catch {$w invoke active}
  destroy $w
}

proc room_toolDelete {id w} {
  gk_callDeleteConference $id
  gk_pollConferences
}

# a tool has been removed from the  room; inform the widget, and if 
# this is the room we are in, delete the tool
#
proc room_delTool {room id} {
  set w .f.c.room$room
  $w deltool $id
  if {$id==[room_getID]} {}
}



#
# routines to inject changes
#

proc room_createView {} {
  gk_newenv -notify viewinfo
  viewinfo misc.current_room ""
  gk_on {([gk_event type]=="changeEnvInfo")&&([gk_event env]=="viewinfo")} {
    room_viewInfoChanged [gk_event key]
  }
  viewinfo set misc.roomid 0
  wm minsize . 100 100
  wm title . "GroupKit Rooms Browser"
  gk_defaultMenu .menu
  .menu delete collaboration
  .menu add roomMenu 1 -text Rooms
  .menu itemcommand roomMenu add command -label "New Room" \
    -command room_makeNewRoom
  .menu itemcommand roomMenu add command -label "Delete" \
    -command room_deleteCurrentRoom
  .menu itemcommand roomMenu add separator
  .menu itemcommand roomMenu add radiobutton -label "Open Door" \
    -command "room_adjustDoor open" -value open -variable room_door
  .menu itemcommand roomMenu add radiobutton -label "Leave Door Ajar" \
    -command "room_adjustDoor ajar" -value ajar -variable room_door
  .menu itemcommand roomMenu add radiobutton -label "Shut Door" \
    -command "room_adjustDoor shut" -value shut -variable room_door
  .menu itemcommand roomMenu add radiobutton -label "Board Door" \
    -command "room_adjustDoor board" -value board -variable room_door
  .menu add toolsMenu 2 -text Tools
  .menu itemconfigure toolsMenu \
    -postcommand "buildConferenceMenu .menu.toolsMenu.menu"
  pack append . .menu {top fillx}
  frame .f
  canvas .f.c -width 400 -height 400 -yscrollcommand ".scrV set" \
		  -xscrollcommand ".scrH set" -scrollregion "0 0 1000 1000"
  scrollbar .scrV -orient vertical -command ".f.c yview"
  scrollbar .scrH -orient horizontal -command ".f.c xview"
  pack .scrV -side right -fill y
  pack .scrH -side bottom -fill x
  pack .f -side top -fill both -expand yes
  pack .f.c -side top -fill both -expand yes
  .menu itemcommand 0 entryconfigure 1 \
          -command room_quit
}

proc room_viewInfoChanged key {
  global room_door
  if {$key=="misc.current_room"} {
    set room [viewinfo misc.current_room]
    if {$room!=""} {
      .menu itemcommand roomMenu entryconfigure "Open Door" -state active
      .menu itemcommand roomMenu entryconfigure "Board Door" -state active
      .menu itemcommand roomMenu entryconfigure "Shut Door" -state active
      .menu itemcommand roomMenu entryconfigure "Leave Door Ajar" -state active
      .menu itemcommand roomMenu entryconfigure "Delete" -state active
      .menu toolsMenu configure -state normal
      set room_door [rooms room.$room.door]
    } else {
      .menu itemcommand roomMenu entryconfigure "Open Door" -state disabled
      .menu itemcommand roomMenu entryconfigure "Board Door" -state disabled
      .menu itemcommand roomMenu entryconfigure "Shut Door" -state disabled
      .menu itemcommand roomMenu entryconfigure "Leave Door Ajar" -state disabled
      .menu itemcommand roomMenu entryconfigure "Delete" -state disabled
      .menu toolsMenu configure -state disabled
      set room_door none
    }
  }
}

proc room_createRoom {name {description "Empty..."} {x 100} {y 100} {door open}} {
  set suffix [viewinfo misc.roomid]
  incr suffix
  viewinfo misc.roomid $suffix
  set id [room_getID]x[viewinfo misc.roomid]
  rooms set room.$id.name $name
  rooms set room.$id.description $description
  rooms set room.$id.x $x
  rooms set room.$id.y $y
  rooms set room.$id.door $door
  return $id
}

proc room_enterRoom room {
  viewinfo misc.current_room $room
  rooms set room.$room.users.[room_getID] [userprefs name]
  foreach i [rooms keys room.$room.tools] {
    gk_callJoinConference $i
    gk_pollUsers $i
  }
}

proc room_leaveRoom room {
  rooms delete room.$room.users.[room_getID]
  foreach i [rooms keys room.$room.tools] {
    gk_toConf $i _gk_QuitConference
  }
}

proc room_moveTo room {
  if {$room!=[viewinfo get misc.current_room]} {
    room_leaveRoom [viewinfo get misc.current_room]
    room_enterRoom $room
  }
}


proc room_quit {} {
     keylset event type regClientQuitting 
     room_leaveRoom [viewinfo get misc.current_room]
     gk_postEvent $event
     _gk_properQuit
}

proc room_addToolToCurrentRoom confnum {
    rooms set room.[viewinfo get misc.current_room].tools.$confnum \
       [confs c.$confnum.confname]
}

proc room_removeToolFromRoom confnum {
   foreach i [rooms keys room] {
     if {[member $confnum [rooms keys room.$i.tools]]} {
       rooms delete room.$i.tools.$confnum
     }
   }
}

proc room_makeNewRoom {} {
  global roomName
  if {[info commands .newroom]==".newroom"} return
  toplevel .newroom
  wm title .newroom "New Room"
  set roomName "New Room"
  pack append [frame .newroom.buttons] \
    [button .newroom.buttons.cancel -text Cancel \
       -command "destroy .newroom"] right \
    [button .newroom.buttons.ok -text Ok \
       -command {room_createRoom $roomName; destroy .newroom}] right

    pack append .newroom \
        [label .newroom.lbl -text $roomName] top \
        [entry .newroom.name -relief sunken -textvariable roomName] \
                   {top pady 10 fillx} \
        .newroom.buttons {bottom fillx pady 20} 
    bind .newroom.name <Return> {room_createRoom $roomName;destroy .newroom}

}

proc room_deleteCurrentRoom {} {
  set current [viewinfo misc.current_room]
  if {$current!=""} {
#### delete tools once we have any
    rooms delete room.$current
  }
}


proc room_adjustDoor state {
  set current [viewinfo misc.current_room]
  if {$current!=""} {
    rooms room.$current.door $state
  }
}


proc room_getID {} {
  while {[viewinfo exists misc.uniqid]==0} {
    update
    after 200
  }
  return [viewinfo get misc.uniqid]
}

proc room_setUniqID id {
  viewinfo set misc.uniqid $id
}

# start it all going


room_createView

gk_newenv -notify -client rooms

keylset info name [userprefs name]
set host [userprefs rooms.host]
if {$host==""} {
    set host [registrar host]
}

set fd [gk_connectToServer $host 9000 $info]
rooms server $fd
dp_RDO $fd gk_serverQueryID "room_setUniqID"
gk_pollConferences

# room_createRoom corridor

gk_on {([gk_event type]=="envReceived")&&([gk_event env]=="rooms")} {
  room_startup 
}

proc room_startup {} {

#
# walk tree, create all necessary rooms

  foreach i [rooms keys room] {
    room_addRoom $i
    foreach j [rooms keys room.$i] {
      if {$j!="users"} {room_changeAttribute $i $j [rooms get room.$i.$j]}
    }
    foreach j [rooms keys room.$i.users] {
      room_addUser $i $j [rooms get room.$i.users.$j]
    }
    foreach j [rooms keys room.$i.tools] {
      room_addTool $i $j [rooms get room.$i.tools.$j]
    }
  }
  
#
# enter into default room, creating it if necessary

  set target corridor
  set roomid ""    

  foreach i [rooms keys room] {
    if {[rooms get room.$i.name]==$target} {set roomid $i}
  }

  if {$roomid==""} {set roomid [room_createRoom $target]}
  room_enterRoom $roomid
}

# bindings for registration things

# all new conferences are valid, and we add them to our list
gk_on {[gk_event type]=="foundNewConf"} {gk_addConfToList}


# all new users are valid, and we add them to our list
gk_on {[gk_event type]=="foundNewUser"} {gk_addUserToList}


# when we find a new conference we created, add it to our current room;
# this will filter down and cause us to join to it
gk_on {([gk_event type]=="foundNewConf")&&([gk_event originator]==[gk_uniqprogid])}     { 
      room_addToolToCurrentRoom [gk_event confnum]
    }


# when we find ourselves added to a conference, create the process
#
gk_on {  ([gk_event type]=="foundNewUser") && 
         ([gk_event host]==[userprefs host]) &&
         ([gk_event port]==[userprefs port])} { 
             confs c.[gk_event confnum].persistenceStyle always
	     gk_createConference [gk_event confnum] [gk_event usernum]; 
	     gk_joinToOthers [gk_event confnum] [gk_event usernum]
	 }


# all deleted users are removed from the lists and rooms
gk_on {[gk_event type]=="foundDeletedUser"} {gk_removeUserFromList }

# when the last user of a conference leaves, we shut down the conference
gk_on {[gk_event type]=="lastUserLeftConf"} \
      {gk_callDeleteConference [gk_event confnum]; gk_pollConferences}


# when a conference connected to us died, report the user left
gk_on {[gk_event type]=="conferenceDied"} \
      {gk_userLeft [gk_event conf] [gk_event user] [gk_event filedesc]}

# all deleted conferences are removed from lists
# all deleted conferences are removed from rooms
gk_on {[gk_event type]=="foundDeletedConf"} {gk_removeConfFromList}
gk_on {[gk_event type]=="foundDeletedConf"} {
  room_removeToolFromRoom [gk_event confnum]
}

# if our user is removed from the conference we are no longer joined
gk_on {([gk_event type]=="foundDeletedUser")&&\
        ([gk_event usernum]==[spawned c.[gk_event confnum].localuser])} \
    {gk_noLongerJoined [gk_event confnum]}


# when a user asks to create a new conference, create it as requested
gk_on {[gk_event type]=="userRequestedNewConf"} {gk_createRequestedConf}


