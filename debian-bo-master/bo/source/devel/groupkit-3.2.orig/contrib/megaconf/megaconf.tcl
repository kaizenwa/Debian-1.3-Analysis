gk_newenv stuff
stuff set counter 1

proc buildRoomWindow {} {
    wm title . [users local.confname]
    wm minsize . 500 500
    pack [gk_defaultMenu .menu] -side top -fill x
    pack [makeRoomContents .contents] -side top -expand yes -fill both
    pack [makeRoomControls .controls] -side top -fill x
    tk_focusFollowsMouse
}

proc makeRoomContents {win} {
  frame $win -borderwidth 4 -relief sunken
  frame $win.x
  canvas $win.room -yscrollcommand "$win.vs set" \
	  -xscrollcommand "$win.hs set" -scrollregion "0 0 1000 1000"
  stuff canvas $win.room
  scrollbar $win.vs -command "$win.room yview"
  scrollbar $win.hs -command "$win.room xview" -orient h
  pack $win.vs -side right -fill y -in $win.x
  pack $win.room -fill both -expand yes -side left -in $win.x
  pack $win.x -side top -expand yes -fill both
  pack $win.hs -side top -fill x
  return $win
}

proc makeRoomControls {win} {  global gk_library
  frame $win  
  pack [makeToolButton $win.sketch sketch-tool @$gk_library/library/bitmaps/megaconf/sketch.xbm] \
	  -side left -padx 2 -pady 2
  pack [makeToolButton $win.postit postit-tool @$gk_library/library/bitmaps/megaconf/postit.xbm] \
	  -side left -padx 2 -pady 2
  pack [makeToolButton $win.chat chat-tool @$gk_library/library/bitmaps/megaconf/chat.xbm] \
	  -side left -padx 2 -pady 2
  return $win
}

proc makeToolButton {win type bitmap} {
    button $win -bitmap $bitmap -relief raised -borderwidth 2 \
	    -command "addTool $type"
    return $win
}

proc addTool {type} {
  set id [stuff counter]
  stuff counter [expr $id+1]
  set id obj[users local.usernum]x$id
  objects $id.type $type
  objects $id.coords "100 100"
}

proc objChkAdd key {
    set parts [split $key .]
    set id [lindex $parts 0]
    if {[objects exists $id.type]&&[objects exists $id.coords]} {
	doAddObj $id
    }
}

proc objChkChange key {
    set parts [split $key .]
    set objid [lindex $parts 0]
    if {[lindex $parts 1]=="coords"} {
	eval [stuff canvas] coords $objid [objects $key]
    }
}

proc objReceivedAll args {
    foreach id [objects keys] {
	doAddObj $id
    }
}

proc doAddObj {objid} {
    set w [stuff canvas].$objid
    set x [lindex [objects $objid.coords] 0]
    set y [lindex [objects $objid.coords] 1] 
    createObjFrame $w $objid
    createObjInternals $w $objid [objects $objid.type]
    [stuff canvas] create window $x $y -window $w -anchor nw \
	    -tags [list roomObject $objid]
}

proc createObjFrame {w objid} {  global gk_library
  frame $w -borderwidth 2 -bg black
  pack [frame $w.tools -borderwidth 0 -bg white -height 10] -fill x
#  pack [label $w.tools.move -bitmap @$gk_library/library/bitmaps/megaconf/handle.xbm -bg white] -padx 5 -anchor w -side left  
  pack [frame $w.spacer -height 3 -bg black] -fill x
  bind $w.tools <1> "raise $w; drag_start $objid %X %Y"
  bind $w.tools <B1-Motion> "drag_continue $objid %X %Y"
}


proc drag_start {objid X Y} {
  set coords [[stuff canvas] coords $objid]
  set objx [lindex $coords 0]; set objy [lindex $coords 1]
  stuff offsetx [expr $X-$objx-[winfo rootx [stuff canvas]]]
  stuff offsety [expr $Y-$objy-[winfo rooty [stuff canvas]]]
}

proc drag_continue {objid X Y} {
  set x [expr $X-[winfo rootx [stuff canvas]]-[stuff offsetx]]
  set y [expr $Y-[winfo rooty [stuff canvas]]-[stuff offsety]]
  objects $objid.coords [list $x $y]
}


proc createObjInternals {w objid type} {
    if {$type=="sketch-tool"} {
	SketchTool $objid $w $objid
    } elseif {$type=="postit-tool"} {
	PostitTool $objid $w $objid
    } elseif {$type=="chat-tool"} {
	ChatTool $objid $w $objid
    }
}




#### base level object

itcl_class GKTool {
  constructor {w id} {
    set win $w
    set objid $id

    lappend bindings [gk_bind newUserArrived "$this event_newuser %U"]
    lappend bindings [gk_bind userDeleted "$this event_userleft %U"]
    lappend bindings [gk_bind updateEntrant "$this event_update_entrant %U"]
    $this makeWindow
  }

  destructor {
    foreach i $bindings {
      gk_delbind $i
    }
  }

  method event_object_deleted {id} {
    if {$objid==$id} {
      $this objectDelete
    }
  }

  method objectDelete {} {
    destroy $win
    $this delete
  }

  method event_newuser {usernum} {
    $this newuser $usernum
  }
  method newuser {usernum} {  }

  method event_userleft {usernum} {
    $this userleft $usernum
  }
  method userleft {usernum} {  }

  method event_update_entrant usernum {
    $this save "gk_toUserNum $usernum $this"
  }

  method event_save_state {} {
    send_clear_object_state $objid
    $this save "send_object_state $objid $this"
  }

  method save prefix {

  }
  
  protected win
  protected objid
  protected bindings
}



############

gk_initConf $argv

gk_newenv -bind -serialize objects


objects bind addEnvInfo "objChkAdd %K"
objects bind changeEnvInfo "objChkChange %K"
objects bind envReceived "objReceivedAll %K"

###########
# load up the tools
#

source [userprefs scriptpath]/sketch_tool.tcl
source [userprefs scriptpath]/postit_tool.tcl
source [userprefs scriptpath]/chat_tool.tcl

buildRoomWindow


