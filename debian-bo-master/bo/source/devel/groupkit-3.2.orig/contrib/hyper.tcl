

set application(title) "Hyper Node Editor"


set application(description) {
{normal} {Draw and manipulate connected nodes.  
Use the left button to sweep out a node, and the
right button to move existing nodes around.  Use
the middle button to connect nodes with lines.

You'll find that when you move nodes, any lines
connected to the node will automatically follow.}}

gk_initConf $argv

wm title . $application(title)

gk_defaultMenu .menubar
.menubar itemcommand help add command -label "$application(title)" \
                          -command "gk_topicWindow .helpWindow \
                                    -height 12 \
                                    -title \"$application(title)\" \
                                    -text \"$application(description)\""



canvas .c
pack append . .menubar {top fillx} .c top


gk_newenv -bind -share graph
gk_newenv graphinfo
graphinfo misc.nextid 0


# multiple cursors
#

gk_initializeTelepointers
gk_specializeWidgetTreeTelepointer .c


# initial drawing of nodes
#

bind .c <1> {startNode %x %y}
bind .c <B1-Motion> {continueNode %x %y}

proc startNode {x y} {
    graphinfo misc.theid [nextId]
    graphinfo misc.startx $x
    graphinfo misc.starty $y
    graph nodes.[graphinfo misc.theid].coords [list $x $y $x $y]
}

proc continueNode {x y} {
    graph nodes.[graphinfo misc.theid].coords \
	[list [graphinfo misc.startx] [graphinfo misc.starty] $x $y]
}


# resizing of nodes
#

proc startDrag {id x y} {
    graphinfo misc.startx $x
    graphinfo misc.starty $y
}

proc continueDrag {id x y} {
    set dX [expr $x-[graphinfo misc.startx]]
    set dY [expr $y-[graphinfo misc.starty]]
    graphinfo misc.startx $x
    graphinfo misc.starty $y
    scan [graph nodes.$id.coords] "%d %d %d %d" x0 y0 x1 y1
    incr x0 $dX; incr y0 $dY; incr x1 $dX; incr y1 $dY
    graph nodes.$id.coords [list $x0 $y0 $x1 $y1]
}

# drawing of edges
#

bind .c <2> {startEdge %x %y}
bind .c <B2-Motion> {continueEdge %x %y}
bind .c <ButtonRelease-2> {endEdge %x %y}

proc startEdge {x y} {
    graphinfo misc.theid [nextId]
    graphinfo misc.startx $x
    graphinfo misc.starty $y
    graphinfo misc.startnode [closestnode $x $y]
    graph edges.[graphinfo misc.theid].coords [list $x $y $x $y]
}

proc continueEdge {x y} {
    graph edges.[graphinfo misc.theid].coords \
	[list [graphinfo misc.startx] [graphinfo misc.starty] $x $y]
}

proc endEdge {x y} {
    set endnode [closestnode $x $y]
    if {$endnode!=[graphinfo misc.startnode]} {
	addEdge [graphinfo misc.startnode] $endnode [graphinfo misc.theid]
    }
    graph delete edges.[graphinfo misc.theid]
}

proc addEdge {startnode endnode id} {
    graph nodes.$startnode.arcs.$id.target $endnode
    graph nodes.$startnode.arcs.$id.direction away
    graph nodes.$endnode.arcs.$id.target $startnode
    graph nodes.$endnode.arcs.$id.direction towards
}


# trap changes to underlying data structure and pass to graphic routines
#

graph bind envReceived {
   foreach i [graph keys nodes] {
      newNode $i
      foreach j [graph keys nodes.$i.arcs] {
        if {[graph nodes.$i.arcs.$j.direction]=="away"} {
           newArc $i $j
        }
      }
   }
}

graph bind addEnvInfo {
    set id [parseArcId %K]
    if {$id!=""} {
        set nodeid [lindex $id 0]; set arcid [lindex $id 1]
	newArc $nodeid $arcid
    } else { 
	set id [parseNodeId %K]
	if {$id!=""} {newNode $id} 
	set id [parseEdgeId %K]
	if {$id!=""} {newEdge $id}
    }
}

graph bind changeEnvInfo {
    set id [parseNodeId %K]
    if {$id!=""} {moveNode $id}
    set id [parseEdgeId %K]
    if {$id!=""} {moveEdge $id}
}

graph bind deleteEnvInfo {
    set id [parseArcId %K]
    if {$id!=""} {
	scan $id "%s %s" nodeid arcid
	delArc $nodeid $arcid
    } else {
	set id [parseNodeId %K]
	if {$id!=""} {delNode $id}
	set id [parseEdgeId %K]
	if {$id!=""} {delEdge $id}
    }
}


# changes to node data structure get reflected on screen
#

proc newNode id {
    scan [graph nodes.$id.coords] "%d %d %d %d" x0 y0 x1 y1
    graphinfo nodes.$id.canvasid [.c create oval $x0 $y0 $x1 $y1 -tags {node}]
    .c bind [graphinfo nodes.$id.canvasid] <3> "startDrag $id %x %y"
    .c bind [graphinfo nodes.$id.canvasid] <B3-Motion> "continueDrag $id %x %y"
}

proc moveNode id {
    scan [graph nodes.$id.coords] "%d %d %d %d" x0 y0 x1 y1
    .c coords [graphinfo nodes.$id.canvasid] $x0 $y0 $x1 $y1
    foreach i [graph keys nodes.$id.arcs] {
	moveArc $id $i
    }
    update idletasks
}

proc delNode id {
    .c delete [graphinfo nodes.$id.canvasid]
    graphinfo delete nodes.$id
}


# changes to node data structure get reflected on screen
#

proc newEdge id {
    scan [graph edges.$id.coords] "%d %d %d %d" x0 y0 x1 y1
    graphinfo edges.$id.canvasid [.c create line $x0 $y0 $x1 $y1 \
				  -tags {edge} -arrow last]
}

proc moveEdge id {
    scan [graph edges.$id.coords] "%d %d %d %d" x0 y0 x1 y1
    .c coords [graphinfo edges.$id.canvasid] $x0 $y0 $x1 $y1
    update idletasks
}

proc delEdge id {
    .c delete [graphinfo edges.$id.canvasid]
    graphinfo delete edges.$id
}

# change to arcs get reflected on screen
#

proc newArc {nodeid arcid} {
    set target [graph nodes.$nodeid.arcs.$arcid.target]
    set direction [graph nodes.$nodeid.arcs.$arcid.direction]
    if {($target!="")&&($direction!="")&&([graphinfo arcs.$arcid.canvasid]=="")} {
	graphinfo arcs.$arcid.canvasid \
	    [eval ".c create line [center [graphinfo nodes.$nodeid.canvasid]] \
            [center [graphinfo nodes.$target.canvasid]] -tags {arc} -arrow last"]
    }
}

proc moveArc {nodeid arcid} {
    set target [graph nodes.$nodeid.arcs.$arcid.target]
    set direction [graph nodes.$nodeid.arcs.$arcid.direction]
    if {$direction=="away"} {
	set first [graphinfo nodes.$nodeid.canvasid]
	set second [graphinfo nodes.$target.canvasid]
    } else {
	set first [graphinfo nodes.$target.canvasid]
	set second [graphinfo nodes.$nodeid.canvasid]
    }
    eval ".c coords [graphinfo arcs.$arcid.canvasid] [center $first] [center $second]"
}

proc delArc {nodeid arcid} {
    if {[graphinfo arcs.$arcid.canvasid]!=""} {
	.c delete [graphinfo arcs.$arcid.canvasid]
	graphinfo delete arcs.$arcid
    }
}


# utility stuff
#

proc parseNodeId key {
    set pieces [split $key .]
    if {[lindex $pieces 0]=="nodes"} {
	return [lindex $pieces 1]
    } else {return ""}
}

proc parseArcId key {
    set nodeid [parseNodeId $key]
    if {$nodeid!=""} {
	set pieces [split $key .]
	if {([llength $pieces]>2)&&([lindex $pieces 2]=="arcs")} {
	    return [list $nodeid [lindex $pieces 3]]
	}
    }
    return ""
}

proc parseEdgeId key {
    set pieces [split $key .]
    if {[lindex $pieces 0]=="edges"} {
	return [lindex $pieces 1]
    } else {return ""}
}

proc nextId {} {
    set i [graphinfo misc.nextid]
    set res [users local.usernum]x$i
    incr i
    graphinfo misc.nextid $i
    return $res
}

proc center widget {
    scan [.c coords $widget] "%f %f %f %f" x0 y0 x1 y1
    return [list [expr ($x0+$x1)*0.5] [expr ($y0+$y1)*0.5]]
}

proc closestnode {x y} {
    set l [eval ".c find withtag node"]
    set min 9999999
    set item 0
    foreach i $l {
	scan [center $i] "%f %f" x0 y0
	set dist [expr {($x-$x0)*($x-$x0)+($y-$y0)*($y-$y0)}]
	if {$dist<$min} {
	    set min $dist
	    set item $i
	}
    }
    foreach i [graphinfo keys nodes] {
	if {[graphinfo nodes.$i.canvasid]==$item} {
	    return $i
	}
    }
    return ""
}
