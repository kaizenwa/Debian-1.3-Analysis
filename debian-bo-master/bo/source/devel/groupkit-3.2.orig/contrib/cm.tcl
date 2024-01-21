# concept map editor

gk_initConf $argv

set application(title) "Concept Map Editor"
set application(description) {
{normal} {
This is a demonstration application that lets you create
concept maps, which are graphs where each node has a type.
This application lets you create and manipulate concept
map nodes and links, but isn't complete, so you can't yet
do useful things like deleting them. :-(

Use the \"Edit Types...\" menu item to manipulate the list of
node types, which lets you associate visual information with
nodes of different types.  Use \"New Node..\" to create a node
of a certain type.

The program also demonstrates the use of a radar view, which
is a widget showing the entire concept map display in miniature,
and highlighting the area where different people are working
in it.  Note that the scrollbars for the main full-size display
are actually gk_scrollbar widgets that aren't displaying the
multi-user color indicator bars (though they could be displayed).

Things of interest from an application developer's view are
the use of environments to model all the data sharing (you'll
find a rather suspicious absence of gk_toAll calls), and how
the radar view is enhanced to deal with application-specific
data display.  The dialog manager code to display the various
dialog boxes is also a generally useful component.
}}

# create our data structures

gk_newenv -share -bind types
gk_newenv -share -bind nodes
gk_newenv -share -bind arcs


# create the interface

wm title . $application(title)
pack [gk_defaultMenu .menubar] -fill x
.menubar itemcommand help add command \
	-label "About $application(title)..." \
	-command "gk_topicWindow .help -title \"$application(title)\" \
	             -width 55 -height 28 \
	             -text \"$application(description)\""
pack [frame .radar -borderwidth 2 -relief sunken] -side right -anchor nw
pack [frame .f1] -fill both -expand yes
pack [gk_scrollbar .scrx -orient horizontal -multiuser no \
	-command ".c xview" ] -in .f1 -fill x -side bottom
pack [gk_scrollbar .scry -multiuser no -command ".c yview"] -in .f1 \
	-fill y -side right 
pack [canvas .c -scrollregion "0 0 2000 2000" -yscrollcommand ".scry set" \
	-xscrollcommand ".scrx set" -width 600 -height 500] -in .f1 \
	-fill both -expand yes -side left
.menubar itemcommand 0 insert 1 command -label "New Node..." -command "newNode"
.menubar itemcommand 0 insert 2 command -label "Edit Types..." \
	-command "nodetype_editor"
pack [gk_radar .radar.r] 


###### stuff about nodetypes

set cmpriv(node_shapes) [list rectangle oval]

types set thingy.shape rectangle
types "other thing.shape" oval

####### node types editor

proc nodetype_editor {} {
    set w .nodetypes
    toplevel $w
    wm title $w "Node Types"
    pack [frame $w.f1] -side left -fill both -expand yes
    pack [listbox $w.list -yscrollcommand "$w.scr set"] -in $w.f1 -side left \
	    -fill both -expand yes
    pack [scrollbar $w.scr -command "$w.list yview"] -in $w.f1 -side left \
	    -fill y
    pack [frame $w.f2] -side left -fill y 
    pack [button $w.new -text New -command "newType"] -in $w.f2 -side top -\
	    fill x 
    pack [button $w.change -text Change -command "changeType"] -in $w.f2 \
	    -side top -fill x
    pack [button $w.delete -text Delete -command "deleteType"] -in $w.f2 \
	    -side top -fill x
    pack [button $w.dismiss -text Dismiss -command "destroy $w"] -in $w.f2 \
	    -side top -fill x -pady 10
    nodetype_editor_rebuildlist
}

proc nodetype_editor_rebuildlist {} {
    set w .nodetypes
    $w.list delete 0 end
    foreach type [types keys] {
	$w.list insert end $type
    }
}

types bind addEnvInfo {
    if [winfo exists .nodetypes] {
	after 1 nodetype_editor_rebuildlist
    }
}


proc changeType {} {
    global cmpriv
    set sel [.nodetypes.list curselection]
    if {[llength $sel]==1} {
	set type [.nodetypes.list get $sel]
	dialog_create .chgtype "Change Node Type" "" Change \
		"request_change_type" 25 \
		[list \
		    [list name static "Type Name" $type] \
		    [list shape list "Shape" [types get $type.shape] \
		           $cmpriv(node_shapes)] \
		    [list display_type boolean "Display Node Type" \
		           [types get $type.display_type]] \
		    [list display_name boolean "Display Node Name" \
		           [types get $type.display_name]] \
		 ]
    }
}


proc request_change_type {} {
    set type [dialog_getfield .chgtype name]
    set shape [dialog_getfield .chgtype shape]
    if {$shape!=[types get $type.shape]} {
	types set $type.shape $shape
    }
    set display_type [dialog_getfield .chgtype display_type]
    if {$display_type!=[types get $type.display_type]} {
	types set $type.display_type $display_type
    }
    set display_name [dialog_getfield .chgtype display_name]
    if {$display_name!=[types get $type.display_name]} {
	types set $type.display_name $display_name
    }
    dialog_destroy .chgtype
}

proc newType {} {
    global cmpriv
    dialog_create .newtype "New Node Type" "" Create "request_new_type" 25 \
	    [list \
  	      [list name entry "Type Name" ""] \
	      [list shape list "Shape" "" $cmpriv(node_shapes)] \
	      [list display_type boolean "Display Node Type" 0] \
	      [list display_name boolean "Display Node Name" 1] \
	    ]
}


proc request_new_type {} {
    set name [dialog_getfield .newtype name]
    if [member $name [types keys]] {
	dialog_alert "A type already exists with the name \"$name\".  You'll have to choose another name."
	return
    } else {
	set shape [dialog_getfield .newtype shape]
	types set $name.shape $shape
	types set $name.display_name [dialog_getfield .newtype display_name]
	types set $name.display_type [dialog_getfield .newtype display_type]
	dialog_destroy .newtype
    }
}


####### stuff about nodes

proc newNode {} {
    dialog_create .newnode "New Node" "" "Create" "request_new_node" 25 \
	    [list \
	       [list name entry "Node Name" ""] \
	       [list type list "Node Type" "" [types keys]] \
	       [list url entry "URL" ""] \
	    ]
}

proc request_new_node {} {
    set name [dialog_getfield .newnode name]
    set type [dialog_getfield .newnode type]
    set url [dialog_getfield .newnode url]
    set cx [.c canvasx 100]
    set cy [.c canvasy 100]
    set id node[uniqid]
    nodes $id.name $name
    nodes $id.type $type
    nodes $id.position [list $cx $cy [expr $cx+100] [expr $cy+100]]
    nodes $id.url $url
    dialog_destroy .newnode
}

nodes bind addEnvInfo "post_create_screen_node %K"
proc post_create_screen_node key {
    global cmpriv
    set id [lindex [split $key .] 0]
    if ![member postcreate_$id [array names cmpriv]] {
	set cmpriv(postcreate_$id) 1
	after 1 do_create_screen_node $id
    }
}


types bind changeEnvInfo {
    do_change_screen_nodes [lindex [split %K .] 0]
}

proc do_change_screen_nodes type {
    foreach node [nodes keys] {
	if {[nodes get $node.type]==$type} {
	    post_create_screen_node $node
	}
    }
}



nodes bind envReceived {
    foreach i [nodes keys] {
	do_create_screen_node $i
    }
}

proc do_create_screen_node id {
    global cmpriv
    catch {.c delete $id}
    set type [nodes get $id.type]
    set shape [types get $type.shape]
    set posn [nodes get $id.position]
    set x0 [lindex $posn 0]; set y0 [lindex $posn 1]
    set x1 [lindex $posn 2]; set y1 [lindex $posn 3]
    set name [nodes get $id.name]
    .c create $shape $x0 $y0 $x1 $y1 -fill white -tags [list $id frame]
    if {[types get $type.display_name]!=0} {
	.c create text $x0 $y0 -text $name -anchor nw -tags [list $id name]
    }
    if {[types get $type.display_type]!=0} {
	.c create text $x0 [expr $y0+20] -text $type -anchor nw \
		-tags [list $id type]
    }
    .c bind $id <1> "startMoveNode $id %x %y"
    .c bind $id <B1-Motion> "continueMoveNode $id %x %y"
    .c bind $id <3> "startArc $id %x %y"
    .c bind $id <B3-Motion> "continueArc $id %x %y"
    .c bind $id <B3-ButtonRelease> "endArc $id %x %y"
    .c bind $id <Double-1> "invokeNode $id"

    ##### radar view now too
    catch {[.radar.r component canvas] delete $id}
    scan [.c cget -scrollregion] "%f %f %f %f" extx0 exty0 extx1 exty1
    set width [expr $extx1-$extx0]; set height [expr $exty1-$exty0]
    [.radar.r component canvas] create $shape \
	    [expr $x0/$width*[winfo width [.radar.r component canvas]]] \
	    [expr $y0/$height*[winfo height [.radar.r component canvas]]] \
	    [expr $x1/$width*[winfo width [.radar.r component canvas]]] \
	    [expr $y1/$height*[winfo height [.radar.r component canvas]]] \
	    -fill white -tags [list $id]

    catch {unset cmpriv(postcreate_$id)}
}


proc invokeNode node {
    set url [nodes get $node.url]
    if {$url!=""} {
	if {[catch {exec netscape -remote openURL($url) } tmp]} {
	    if { [string first "not running on" $tmp] != -1} {
		exec netscape $url &
	    }
	}
    }
}

nodes bind changeEnvInfo {
    if {[lindex [split %K .] 1]=="position"} {
	doChangeNodePosition [lindex [split %K .] 0]
    }
}

proc doChangeNodePosition id {
    update idletasks
    set posn [nodes get $id.position]
    foreach item [.c find withtag $id] {
	if [member frame [.c gettags $item]] {
	    eval .c coords $item $posn
	}
	if [member name [.c gettags $item]] {
	    .c coords $item [lindex $posn 0] [lindex $posn 1]
	}
	if [member type [.c gettags $item]] {
	    .c coords $item [lindex $posn 0] [expr [lindex $posn 1]+20]
	}
    }
    
    ##### radar view now
    scan [.c cget -scrollregion] "%f %f %f %f" extx0 exty0 extx1 exty1
    set width [expr $extx1-$extx0]; set height [expr $exty1-$exty0]
    scan $posn "%f %f %f %f" x0 y0 x1 y1
    [.radar.r component canvas] coords $id \
    	    [expr $x0/$width*[winfo width [.radar.r component canvas]]] \
	    [expr $y0/$height*[winfo height [.radar.r component canvas]]] \
	    [expr $x1/$width*[winfo width [.radar.r component canvas]]] \
	    [expr $y1/$height*[winfo height [.radar.r component canvas]]] 

}

proc startMoveNode {id x y} {
    global cmpriv
    set cmpriv(move_offsetx) [expr [.c canvasx $x]-[lindex \
	    [nodes get $id.position] 0]]
    set cmpriv(move_offsety) [expr [.c canvasy $y]-[lindex \
	    [nodes get $id.position] 1]]
}

proc continueMoveNode {id x y} {
    global cmpriv
    set oldposn [nodes get $id.position]
    set newx0 [expr [.c canvasx $x]-$cmpriv(move_offsetx)]
    set newy0 [expr [.c canvasy $y]-$cmpriv(move_offsety)]
    set newx1 [expr $newx0+([lindex $oldposn 2]-[lindex $oldposn 0])]
    set newy1 [expr $newy0+([lindex $oldposn 3]-[lindex $oldposn 1])]
    nodes set $id.position [list $newx0 $newy0 $newx1 $newy1]
}


####### arcs

proc startArc {src x y} {
    global cmpriv
    set arcid arc[uniqid]
    arcs set $arcid.src $src
    arcs set $arcid.dragpt [list [.c canvasx $x] [.c canvasy $y]]
    set cmpriv(newarc_id) $arcid
}

proc continueArc {src x y} {
    global cmpriv
    arcs set $cmpriv(newarc_id).dragpt [list [.c canvasx $x] [.c canvasy $y]]
}

proc endArc {src x y} {
    global cmpriv
    set enditem [lindex [.c find overlapping $x $y $x $y] 0]
    if {$enditem!=""} {
	foreach i [.c gettags $enditem] {
	    if {[string range $i 0 3]=="node"} {
		arcs delete $cmpriv(newarc_id).dragpt
		arcs set $cmpriv(newarc_id).dest $i
		return
	    }
	}
    }
    arcs delete $cmpriv(newarc_id)
}


###### screen stuff for arcs

arcs bind deleteEnvInfo {
    if {[llength [split %K .]]==1} {
	catch {.c delete %K}
### radar view stuff
       catch {[.radar.r component canvas] delete %K}
    }
}

arcs bind envReceived {
    foreach arc [arcs keys] {
	post_create_screen_arc $arc
    }
}

arcs bind addEnvInfo {post_create_screen_arc [lindex [split %K .] 0]}
proc post_create_screen_arc arcid {
    if ![member postcreate_$arcid [array names cmpriv]] {
	set cmpriv(postcreate_$arcid) 1
	after 1 do_create_screen_arc $arcid
    }
} 

arcs bind changeEnvInfo {
    post_create_screen_arc [lindex [split %K .] 0]
    update idletasks
}

proc do_create_screen_arc arcid {
    catch {.c delete $arcid}
    set srcposn [nodes get [arcs get $arcid.src].position]
    set x0 [expr ([lindex $srcposn 0]+[lindex $srcposn 2])/2]
    set y0 [expr ([lindex $srcposn 1]+[lindex $srcposn 3])/2]
    if [member dragpt [arcs keys $arcid]] {
	set x1 [lindex [arcs get $arcid.dragpt] 0]
	set y1 [lindex [arcs get $arcid.dragpt] 1]
    } else {
	set destposn [nodes get [arcs get $arcid.dest].position]
	set x1 [expr ([lindex $destposn 0]+[lindex $destposn 2])/2]
	set y1 [expr ([lindex $destposn 1]+[lindex $destposn 3])/2]
    }
    .c create line $x0 $y0 $x1 $y1 -tags $arcid
    catch {.c raise [arcs get $arcid.src]}
    catch {.c raise [arcs get $arcid.dest]}

### radar view stuff now
    catch {[.radar.r component canvas] delete $arcid}
    scan [.c cget -scrollregion] "%f %f %f %f" extx0 exty0 extx1 exty1
    set width [expr $extx1-$extx0]; set height [expr $exty1-$exty0]
    [.radar.r component canvas] create line \
	    [expr $x0/$width*[winfo width [.radar.r component canvas]]] \
	    [expr $y0/$height*[winfo height [.radar.r component canvas]]] \
	    [expr $x1/$width*[winfo width [.radar.r component canvas]]] \
	    [expr $y1/$height*[winfo height [.radar.r component canvas]]] \
	    -tags $arcid
    catch {[.radar.r component canvas] raise [arcs get $arcid.src]}
    catch {[.radar.r component canvas] raise [arcs get $arcid.dest]}
}

nodes bind changeEnvInfo {
    if {[lindex [split %K .] 1]=="position"} {
	do_move_arcs [lindex [split %K .] 0]
    }
}

proc do_move_arcs node {
    foreach i [arcs keys] {
	if {([arcs get $i.src]==$node) || ([arcs get $i.dest]==$node)} {
	    post_create_screen_arc $i
	}
    }
}


####### dialog manager stuff

proc dialog_create {w title bitmap ok_label ok_cmd label_width fields} {
  global cmpriv
  toplevel $w
  wm title $w $title
  set first ""
  if {$bitmap!=""} {
    pack [label $w.icon -bitmap $bitmap] -side left -anchor n -padx 7 -pady 7
  }
  pack [frame $w.frm] -side right -expand yes -fill both
  pack [label $w.frm.hdr -text $title] -anchor sw -pady 15 -padx 2
  foreach i $fields {
      set id [lindex $i 0]
      set type [lindex $i 1]
      set label [lindex $i 2]
      set default [lindex $i 3]
      pack [frame $w.frm.$id] -side top -fill x -pady 4 -padx 2
      if [member $type "password static entry list"] {
        pack [label $w.frm.$id.lbl -text $label -width $label_width] -side left
      } else {
	pack [label $w.frm.$id.lbl -text "" -width $label_width] -side left
      }
      if [member $type "password static entry"] { 
        pack [entry $w.frm.$id.entry -textvariable cmpriv(dlgval::$w::$id)] \
		-side left -expand yes -fill x
        $w.frm.$id.entry insert 0 $default
      }
      if {$type=="boolean"} {
	  pack [checkbutton $w.frm.$id.checkbox -text $label -variable \
		  cmpriv(dlgval::$w::$id)] -side left -expand yes -fill x
	  if {$default=="1"} {$w.frm.$id.checkbox select}
      }
      if {$type=="list"} {
	  if {$default==""} {
	      set itemlist [lindex $i 4]
	  } else {
	      set itemlist $default
	      foreach opt [lindex $i 4] {
		  if {$opt!=$default} {lappend itemlist $opt}
	      }
	  }
	  eval tk_optionMenu $w.frm.$id.menu cmpriv(dlgval::$w::$id) $itemlist
	  pack $w.frm.$id.menu -side left -expand yes -fill x
      }
      if {$type=="password"} {$w.frm.$id.entry config -show *}
      if {$type=="static"} {
	  $w.frm.$id.entry config -state disabled
	  set cmpriv(dlgval::$w::$id) $default
      }
      if {$first==""} {
	  if {($type=="entry")||($type=="password")} {
	      set first $id
	  }
      }
  }
  pack [frame $w.frm.buttons] -side right -fill x -pady 4 -padx 2
  pack [button $w.frm.buttons.ok -text $ok_label -command $ok_cmd] -side left
  pack [button $w.frm.buttons.cancel -text Cancel \
	  -command "dialog_destroy $w"] -side left
  if {$first!=""} {
      focus $w.frm.$first.entry
  }
  grab $w
}


proc dialog_getfield {w attr} {
    global cmpriv
    return $cmpriv(dlgval::$w::$attr)
}


proc dialog_destroy {w} {
  destroy $w
}


proc dialog_alert {msg} {
   toplevel .dialog_alert
   pack [message .dialog_alert.msg -aspect 600 -text $msg] -side top
   pack [button .dialog_alert.ok -text Dismiss \
	   -command "destroy .dialog_alert"] -side top
   grab .dialog_alert
   tkwait window .dialog_alert
}

####### utility stuff

set cmpriv(uniqid_counter) 0
proc uniqid {} {  
    global cmpriv
    incr cmpriv(uniqid_counter)
    return [users local.usernum]x$cmpriv(uniqid_counter)
}
