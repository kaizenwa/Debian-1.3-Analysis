gk_initConf $argv

pack [gk_defaultMenu .menubar] -side top -fill x
pack [entry .e] -side bottom -fill x
bind .e <Return> enterItem
pack [gk_scrollbar .s -command ".t yview"] -side right -fill y
pack [text .t -yscrollcommand ".s set" -state disabled -cursor ""] \
	-side right -fill both -expand yes
focus .t

wm title . "Outlining Tool"
set help {
{normal} {
This application is an outlining tool that allows users
to create and manipulate an outline.  The outline is
displayed in a manner similar to the Mac finder, where
\"little triangles\" (the official name!) are used to
change whether items within another item are displayed
or not.  Carl would want me to mention that the name 
for this technique is \"holophrasting\".

To add new items to the outliner, type them into the
entry at the bottom of the window and hit Return.
You can select items by clicking on their text with
the left mouse button, and delete them with the backspace
key.

To reposition items in the outline, click and drag the
source item with the left mouse button.  To make the
source item a child of another item, release the mouse
button somewhere on the target item's text.  To make
the source item a sibling of another item, release the
mouse button either just to the left or just to the
right of the target item.
}
}
.menubar itemcommand help add command -label "Outlining Tool" \
	-command "gk_topicWindow .help -title \"Outlining Tool\" \
	              -text \"$help\" -width 50"

gk_initializeTelepointers
gk_specializeWidgetTreeTelepointer .t

bindtags .t {all .t}

bind .t <BackSpace> "delete_selected_item"

gk_newenv items

proc item_bitmap {which} {
    global gk_library
    case $which {
	open   {return @$gk_library/library/bitmaps/open-triangle.xbm}
	closed {return @$gk_library/library/bitmaps/closed-triangle.xbm}
	nokids {return @$gk_library/library/bitmaps/nokids.xbm}
    }
}

## add an item at the appropriate level, starting at the index "where"
proc addItem {item level where} {
    .t configure -state normal
    .t insert $where " " space$item
    if {[items get $item.kids]!=""}  {
	if {[items get $item.state]=="open"} {
	    set icon [item_bitmap open]
	} else {
	    set icon [item_bitmap closed]
	}
    } else {
	set icon [item_bitmap nokids]
    }
    .t window create $where+1c -window [label .icon$item -bitmap $icon]
    bind .icon$item <1> "gk_serialize toggleState $item $level"
    .t tag configure space$item -tabs [expr 1.5+$level]c
    .t insert $where+2c "\t" start$item
    set contents [items get $item.contents]
    set len [string length $contents]
    .t insert $where+3c $contents text$item
    .t insert $where+[expr 3+$len]c "\n" end$item
    .t tag configure text$item -tabs [expr 1.5+$level]c \
	    -lmargin2 [expr 1.5+$level]c
    .t tag bind text$item <1> "select_item $item"
#    .t tag bind text$item <B1-Motion> {puts "moving [.t index @%x,%y]" }
    .t tag bind text$item <B1-ButtonRelease> "release_button $item %x %y"
    if {[items get $item.state]=="open"} {
	set where end$item.last
	foreach i [items get $item.kids] {
	    set where [addItem $i [expr $level+1] $where]
	}
	.t configure -state disabled
	return $where
    } else {
	.t configure -state disabled
	return end$item.last
    }
}


### muck with the triangles
proc toggleState {item level} {
    if {[items get $item.kids]==""} {return}
    if {[items get $item.state]=="closed"} {
	setState $item open
    } else {
	setState $item closed
    }
}

proc setState {item state} {
    items set $item.state $state
    if {[items get $item.kids]==""} {return}
    if {$state=="open"} {
	.icon$item configure -bitmap [item_bitmap open]
	set where end$item.last
	set level [tree_level $item]
	foreach i [items get $item.kids] {
	    set where [addItem $i [expr $level+1] $where]
	}
    } else {
	.icon$item configure -bitmap [item_bitmap closed]
	foreach i [items get $item.kids] {
	    removeItem $i
	}
    }
}

### remove items from tree display
proc removeItem {item} {
    .t configure -state normal
    .t delete space$item.first end$item.last
    if {[items get $item.state]=="open"} {
	foreach i [items get $item.kids] {
	    removeItem $i
	}
    }
    .t configure -state disabled
}

set selected_item ""
proc select_item {item} {
    .t configure -cursor hand2
    focus .t
    global selected_item
    if {$selected_item!=""} {
	catch {
	    .t tag configure text$selected_item -relief flat \
		    -background [.t cget -background] -borderwidth 0
	}
    }
    set selected_item $item
    .t tag configure text$item -background [.t cget -selectbackground] \
	    -relief raised -borderwidth 2
}

proc delete_selected_item {} {
    global selected_item
    if {$selected_item!=""} {
	gk_serialize tree_delete $selected_item
	set selected_item ""
    }
}

proc release_button {item x y} {
    .t configure -cursor ""
    set where [.t index @$x,$y]
    set tags [.t tag names $where]
    if {$tags!=""} {
	if {[string range $tags 0 3]=="text"} {
	    set target [string range $tags 4 end]
	    if {$item==$target} return
	    # dropped somewhere in middle of string
	    gk_serialize tree_unlink $item
	    gk_serialize tree_appendChild $item \
		    [items get $item.contents] $target \
		    [items get $item.state]
	} elseif {[string range $tags 0 4]=="start"} {
	    set target [string range $tags 5 end]
	    if {$item==$target} return
	    # dropped at start of string
	    set parent [items get $target.parent]
	    set kids [items get $parent.kids]
	    set posn [lsearch $kids $target]
	    if {$posn==0} {
		gk_serialize tree_unlink $item
		gk_serialize tree_appendChild $item \
			[items get $item.contents] $parent \
			[items get $item.state]
	    } else {
		set sibling [lindex $kids [expr $posn-1]]
		gk_serialize tree_unlink $item
		gk_serialize tree_appendSibling $item \
			[items get $item.contents] $sibling \
			[items get $item.state]
	    }
	} elseif {[string range $tags 0 2]=="end"} {
	    # dropped at end of string
	    set target [string range $tags 3 end]
	    if {$item==$target} return
	    gk_serialize tree_unlink $item
	    gk_serialize tree_appendSibling $item [items get $item.contents] \
		    $target [items get $item.state]
	}
    }
}

proc tree_appendSibling {item contents sibling state} {
    set parent [items get $sibling.parent]
    if {$parent==""} {error "could not get parent of $sibling"}
    items set $item.contents $contents
    items set $item.parent $parent
    items set $item.state $state
    set kids [items get $parent.kids]
    set posn [lsearch $kids $sibling]
    if {$posn==-1} {error "$parent does not know about child $sibling"}
    items set $parent.kids [linsert $kids [expr $posn+1] $item]
    ### now update the display
    if [tree_displayed $item] {
	### the position is wrong, have to take into account kids
	addItem $item [tree_level $item] [endof_item $sibling]
    }
}

proc endof_item {item} {
    if {[items get $item.state]=="closed"} {return end$item.last}
    set kids [items get $item.kids]
    if {$kids==""} {return end$item.last}
    return [endof_item [lindex $kids [expr [llength $kids]-1]]]
}

proc tree_appendChild {item contents parent state} {
    items set $item.contents $contents
    items set $item.parent $parent
    items set $item.state $state
    set kids [items get $parent.kids]
    items set $parent.kids [linsert $kids 0 $item]
    ### change state of parent node if necessary
    if {($kids=="")&&($parent!="root")&&([tree_displayed $parent])} {
	if {[items get $parent.state]=="open"} {
	    .icon$parent configure -bitmap [item_bitmap open]
	} else {
	    .icon$parent configure -bitmap [item_bitmap closed]
	}
    }
    ### now update the display
    if [tree_displayed $item] {
	if {$parent=="root"} {
	    addItem $item 0 end
	} else {
	    addItem $item [tree_level $item] end$parent.last
	}
    }
}

proc tree_unlink {item} {
    set parent [items get $item.parent]
    if {$parent==""} {error "could not get parent of $item"}
    set kids [items get $parent.kids]
    set posn [lsearch $kids $item]
    if {$posn==-1} {error "$parent does not know about child $item"}
    set visibility [tree_displayed $item]
    items set $parent.kids [lreplace $kids $posn $posn]
    ### change state of parent node if necessary
    if {([items get $parent.kids]=="")&&($parent!="root")} {
	.icon$parent configure -bitmap [item_bitmap nokids]
    }
    ### now update the display
    if $visibility {
	removeItem $item
    }
}

proc tree_delete {item} {
    catch {tree_unlink $item}
    foreach i [items get $item.kids] {
	tree_delete $i
    }
}

proc tree_displayed {item} {
    set parent [items get $item.parent]
    if {$parent=="root"} {
	return 1
    } else {
	if {[items get $parent.state]=="closed"} {
	    return 0
	} else {
	    return [tree_displayed $parent]
	}
    }
}

proc tree_level {item} {
    if {$item=="root"} {
	return -1
    } else {
	return [expr 1+[tree_level [items get $item.parent]]]
    }
}

### just some initial data.. feel free to eliminate this
if [gk_amOriginator] {
    gk_serialize tree_appendChild item1 "this is item1" root open
    gk_serialize tree_appendSibling item2 "this is item2" item1 open
    gk_serialize tree_appendSibling item3 "this is item3" item2 open
    gk_serialize tree_appendSibling item7 "this is item7" item3 closed
    gk_serialize tree_appendSibling item8 "this is item8" item7 open
    gk_serialize tree_appendSibling item9 "this is item9" item8 open

    gk_serialize tree_appendChild item4 "this is item4" item1 open
    gk_serialize tree_appendSibling item5 "this is item5" item4 open

    gk_serialize tree_appendChild item6 "this is item6" item5 open

    gk_serialize tree_appendChild item10 "this is item10" item7 open
    gk_serialize tree_appendSibling item11 "this is item11" item10 open
    gk_serialize tree_appendSibling item12 "this is item12" item11 open
    gk_serialize tree_appendSibling item13 "this is item13" item12 open
    gk_serialize tree_appendSibling item14 "this is item14" item13 open

    gk_serialize tree_appendChild item15 "this is item15" item2 closed
    gk_serialize tree_appendChild item16 "this is item16" item15 open
    gk_serialize tree_appendSibling item17 "this is item17" item16 open
    gk_serialize tree_appendSibling item18 "this is item18" item17 open
}

gk_bind updateEntrant {
    sendTree root %U
}

proc sendTree {node usernum} {
    set last ""
    foreach i [items get $node.kids] {
	if {$last==""} {
	    gk_toUserNum $usernum tree_appendChild $i [items get $i.contents] \
		    $node [items get $i.state]
	} else {
	    gk_toUserNum $usernum tree_appendSibling $i \
		    [items get $i.contents] $last [items get $i.state]
	}
	set last $i
	sendTree $i $usernum
    }
}


set counter 1
proc enterItem {} {
    global counter
    set item [.e get]
    set rootkids [items get root.kids]
    if {$rootkids==""} {
	gk_serialize tree_appendChild [users local.usernum]x$counter \
		$item root open
    } else {
	set sib [lindex $rootkids [expr [llength $rootkids]-1]]
	gk_serialize tree_appendSibling [users local.usernum]x$counter \
		$item $sib open
    }
    incr counter
    .e delete 0 end
}
