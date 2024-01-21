##
## Editor.tcl
##
## This file contains the definition of a Tkined editor. The
## procs in this file are mostly callbacks that are called from
## an editor object, which can be found in editor.c.
##
## Copyright (c) 1993, 1994, 1995
##
## J. Schoenwaelder
## TU Braunschweig, Germany
## Institute for Operating Systems and Computer Networks
##
## Permission to use, copy, modify, and distribute this
## software and its documentation for any purpose and without
## fee is hereby granted, provided that this copyright
## notice appears in all copies.  The University of Braunschweig
## makes no representations about the suitability of this
## software for any purpose.  It is provided "as is" without
## express or implied warranty.
##


##
## Set up a new editor. Create all the menus and the canvas with
## the scollbars.
##

proc Editor::create { editor } { 
    $editor toplevel ".$editor"
}

proc Editor::delete { editor } {
    destroy [$editor toplevel]
}

proc Editor::graph { editor } {
    set w [$editor toplevel]
    set top $w.diagram
    if {! [winfo exists $top]} { 
	Diagram::create $editor
    }
    return $top
}

proc Editor::toplevel { editor } {

    global tkined_version

    set w [$editor toplevel]

    catch {destroy $w}
    toplevel $w -class tkined

    wm withdraw $w
    wm iconbitmap $w icon
    wm iconname $w "tkined"
    wm minsize $w 500 300

    set width [$editor attribute width]
    if {$width == ""} { set width 600 }
    set height [$editor attribute height]
    if {$height == ""} { set height 400 }

    # set up the canvas for the graphic
    canvas $w.canvas -borderwidth 1 -relief raised -highlightthickness 0 \
	-width $width -height $height
    
    # set up the tool box
    frame $w.tools -borderwidth 1 -relief raised
    
    button $w.tools.select -text "Select" -relief flat \
	-command "Tool::Select $editor"
    button $w.tools.resize -text "Resize" -relief flat \
	-command "Tool::Resize $editor"
    button $w.tools.text -text "Text" -relief flat \
	-command "Tool::Text $editor"
    button $w.tools.node -bitmap machine -relief flat \
	-command "Tool::Node $editor"
    button $w.tools.network -bitmap network -relief flat \
	-command "Tool::Network $editor"
    button $w.tools.link -bitmap link -relief flat \
	-command "Tool::Link $editor"
    button $w.tools.group -bitmap group -relief flat  \
	-command "Tool::Group $editor"
    button $w.tools.reference -bitmap reference -relief flat  \
	-command "Tool::Reference $editor"
    Tool::Select $editor

    global newToolbox$w
    set newToolbox$w 0

    label $w.tools.dummy -text "tkined $tkined_version"
    pack $w.tools.dummy -side top -pady 1 -ipadx 5
    pack $w.tools.select    -fill x
    pack $w.tools.resize    -fill x
    pack $w.tools.text      -fill x
    pack $w.tools.node      -fill x
    pack $w.tools.network   -fill x
    pack $w.tools.link      -fill x
    pack $w.tools.group     -fill x
    pack $w.tools.reference -fill x

    if {[$editor attribute zoom] != ""} {
	button $w.tools.up   -bitmap zoomin   -relief flat \
		-command "ScaleTree $w.canvas 0.8"
	button $w.tools.down -bitmap zoomout -relief flat \
		-command "ScaleTree $w.canvas 1.25"
	pack $w.tools.up $w.tools.down -side bottom -fill x
	
	proc ScaleTree {c factor} {
	    global zoom
	    if {![info exists zoom($c)]} {
		set zoom($c) 1.0
	    }
	    set n [expr $zoom($c) * $factor]
	    if {$n > 1 || $n < 0.2} return
	    set zoom($c) $n
	    $c scale all 0 0 $factor $factor
	}
    }

    # set up the menu bar
    frame $w.menu -borderwidth 1 -relief raised
    
    # Add some scrollbars to the canvas. We put put the vscrollbar 
    # into a frame with an extra label to get rid of the overlapping
    # scrollbars in the lower right corner. Ugly, but it works.

    scrollbar $w.hscroll -orient horiz -relief sunken \
	-command "$w.canvas xview"
    frame $w.vscroll
    scrollbar $w.vscroll.sc -relief sunken -command "$w.canvas yview"
    button $w.vscroll.l -bitmap corner -relief sunken \
	    -padx 1 -pady 1 -command "Editor::UselessButton $w.canvas"
    pack  $w.vscroll.l -side bottom -ipadx 1 -ipady 1
    pack  $w.vscroll.sc -side top -fill y -expand yes

    # set up the application
    pack $w.tools   -side left -fill y
    pack $w.menu    -side top -fill x
    pack $w.vscroll -side right -fill y
    pack $w.hscroll -side bottom -fill x
    pack $w.canvas  -side left -fill both -expand true -padx 0 -pady 0

    $w.canvas config \
	-xscrollcommand "$w.hscroll set" -yscrollcommand "$w.vscroll.sc set"

    # set up the file menu
    menubutton $w.menu.file -text "File" -menu $w.menu.file.m
    menu $w.menu.file.m
    $w.menu.file.m add command -label "Clear" \
	-accelerator "  Alt+C" \
	-command "$w.menu.file configure -state disabled; update idletasks; \
                  Command::Clear $editor; \
                  $w.menu.file configure -state normal"
    bind $w <Alt-c>  "$w.menu.file.m invoke Clear"
    bind $w <Alt-C>  "$w.menu.file.m invoke Clear"
    bind $w <Meta-c> "$w.menu.file.m invoke Clear"
    bind $w <Meta-C> "$w.menu.file.m invoke Clear"
    $w.menu.file.m add command -label "Open..." \
	-accelerator "  Alt+0" \
	-command "$w.menu.file configure -state disabled; update idletasks; \
                  Command::Open $editor; \
                  $w.menu.file configure -state normal"
    bind $w <Alt-o>  "$w.menu.file.m invoke Open..."
    bind $w <Alt-O>  "$w.menu.file.m invoke Open..."
    bind $w <Meta-o> "$w.menu.file.m invoke Open..."
    bind $w <Meta-O> "$w.menu.file.m invoke Open..."
    $w.menu.file.m add command -label "Merge..." \
	-accelerator "  Alt+M" \
	-command "$w.menu.file configure -state disabled; update idletasks; \
                  Command::Merge $editor; \
                  $w.menu.file configure -state normal"
    bind $w <Alt-m>      "$w.menu.file.m invoke Merge..."
    bind $w <Alt-Key-M>  "$w.menu.file.m invoke Merge..."
    bind $w <Meta-m>     "$w.menu.file.m invoke Merge..."
    bind $w <Meta-Key-M> "$w.menu.file.m invoke Merge..."
    $w.menu.file.m add command -label "Save..." \
	-accelerator "  Alt+S" \
	-command "$w.menu.file configure -state disabled; update idletasks; \
                  Command::Save $editor; \
                  $w.menu.file configure -state normal"
    bind $w <Alt-s>  "$w.menu.file.m invoke Save..."
    bind $w <Alt-S>  "$w.menu.file.m invoke Save..."
    bind $w <Meta-s> "$w.menu.file.m invoke Save..."
    bind $w <Meta-S> "$w.menu.file.m invoke Save..."
    $w.menu.file.m add command -label "Save As..." \
	-accelerator "  Alt+A" \
	-command "$w.menu.file configure -state disabled; update idletasks; \
                  Command::SaveAs $editor; \
                  $w.menu.file configure -state normal"
    bind $w <Alt-a>  "$w.menu.file.m invoke {Save As...}"
    bind $w <Alt-A>  "$w.menu.file.m invoke {Save As...}"
    bind $w <Meta-a> "$w.menu.file.m invoke {Save As...}"
    bind $w <Meta-A> "$w.menu.file.m invoke {Save As...}"
    $w.menu.file.m add sep
    $w.menu.file.m add command -label "Print..." \
	-accelerator "  Alt+P" \
	-command "$w.menu.file configure -state disabled; update idletasks; \
                  Command::Print $editor; \
                  $w.menu.file configure -state normal"
    bind $w <Alt-p>  "$w.menu.file.m invoke Print..."
    bind $w <Alt-P>  "$w.menu.file.m invoke Print..."
    bind $w <Meta-p> "$w.menu.file.m invoke Print..."
    bind $w <Meta-P> "$w.menu.file.m invoke Print..."
    $w.menu.file.m add command -label "Import..." \
	-accelerator "  Alt+I" \
	-command "$w.menu.file configure -state disabled; update idletasks; \
                  Command::Import $editor; \
                  $w.menu.file configure -state normal"
    bind $w <Alt-i>  "$w.menu.file.m invoke Import..."
    bind $w <Alt-I>  "$w.menu.file.m invoke Import..."
    bind $w <Meta-i> "$w.menu.file.m invoke Import..."
    bind $w <Meta-I> "$w.menu.file.m invoke Import..."
    $w.menu.file.m add command -label "History..." \
	-accelerator "  Alt+H" \
	-command "Command::History $editor"
    bind $w <Alt-h>  "$w.menu.file.m invoke History..."
    bind $w <Alt-H>  "$w.menu.file.m invoke History..."
    bind $w <Meta-h> "$w.menu.file.m invoke History..."
    bind $w <Meta-H> "$w.menu.file.m invoke History..."
    $w.menu.file.m add sep
    $w.menu.file.m add command -label "New View" \
	-accelerator "  Alt+N" \
	-command "$w.menu.file configure -state disabled; update idletasks; \
                  EDITOR; \
                  $w.menu.file configure -state normal"
    bind $w <Alt-n>  "$w.menu.file.m invoke {New View}"
    bind $w <Alt-N>  "$w.menu.file.m invoke {New View}"
    bind $w <Meta-n> "$w.menu.file.m invoke {New View}"
    bind $w <Meta-N> "$w.menu.file.m invoke {New View}"
    $w.menu.file.m add command -label "Close View" \
	-accelerator "  Alt+W" \
	-command "$w.menu.file configure -state disabled; update idletasks; \
                  Command::Close $editor; \
                  catch \"$w.menu.file configure -state normal\""
    bind $w <Alt-w>  "$w.menu.file.m invoke {Close View}"
    bind $w <Alt-W>  "$w.menu.file.m invoke {Close View}"
    bind $w <Meta-w> "$w.menu.file.m invoke {Close View}"
    bind $w <Meta-W> "$w.menu.file.m invoke {Close View}"
    
    # set up the edit menu
    menubutton $w.menu.edit -text "Edit" \
	-menu $w.menu.edit.m
    menu $w.menu.edit.m
    $w.menu.edit.m add cascade -label "Attribute" \
	-menu $w.menu.edit.m.a
    $w.menu.edit.m add command -label "Set Label..." \
	-accelerator "  S" \
	-command "$w.menu.edit configure -state disabled; update idletasks; \
	          Command::Label $editor; \
		  $w.menu.edit configure -state normal"
    bind $w <S> "$w.menu.edit.m invoke {Set Label...}"
    $w.menu.edit.m add command -label "Scale Factor..." \
	-accelerator "  F" \
	-command "$w.menu.edit configure -state disabled; update idletasks; \
                  Command::Scale $editor; \
                  $w.menu.edit configure -state normal"
    bind $w <F> "$w.menu.edit.m invoke {Scale Factor...}"
    $w.menu.edit.m add sep
    $w.menu.edit.m add command -label "Grid Spacing..." \
        -accelerator "  s" \
        -command "$w.menu.edit configure -state disabled; \
                  update idletasks; \
                  Command::GridSpacing $editor; \
                  $w.menu.edit configure -state normal"
    bind $w <s> "$w.menu.edit.m invoke {Grid Spacing...}"
    $w.menu.edit.m add command -label "Align to Grid" \
        -accelerator "  t" \
        -command "$w.menu.edit configure -state disabled; \
                  update idletasks; \
                  Command::Grid $editor; \
                  $w.menu.edit configure -state normal"
    bind $w <t> "$w.menu.edit.m invoke {Align to Grid}"
    $w.menu.edit.m add sep
    $w.menu.edit.m add command -label "Undo" \
	-accelerator "  U" \
	-command "$w.menu.edit configure -state disabled; update idletasks; \
                  Command::Undo $editor; \
                  $w.menu.edit configure -state normal"
    bind $w <U> "$w.menu.edit.m invoke Undo"
    $w.menu.edit.m add command -label "Redo" \
	-accelerator "  R" \
	-command "$w.menu.edit configure -state disabled; update idletasks; \
                  Command::Redo $editor; \
                  $w.menu.edit configure -state normal" 
    bind $w <R> "$w.menu.edit.m invoke Redo"
    $w.menu.edit.m add sep
    $w.menu.edit.m add command -label "Delete" \
	-accelerator " ^D" \
	-command "$w.menu.edit configure -state disabled; update idletasks; \
                  Command::Delete $editor; \
                  $w.menu.edit configure -state normal"
    bind $w <Control-d> "$w.menu.edit.m invoke Delete"
    $w.menu.edit.m add command -label "Cut" \
	-accelerator "  x" \
	-command "$w.menu.edit configure -state disabled; update idletasks; \
                  $editor cut; \
                  $w.menu.edit configure -state normal"
    bind $w <x> "$w.menu.edit.m invoke Cut"
    $w.menu.edit.m add command -label "Copy" \
	-accelerator "  w" \
	-command "$w.menu.edit configure -state disabled; update idletasks; \
                  $editor copy; \
                  $w.menu.edit configure -state normal"
    bind $w <w> "$w.menu.edit.m invoke Copy"
    $w.menu.edit.m add command -label "Paste" \
	-accelerator "  v" \
	-command "$w.menu.edit configure -state disabled; update idletasks; \
                  $editor paste; \
                  $w.menu.edit configure -state normal"
    bind $w <v> "$w.menu.edit.m invoke Paste"

    # set up the select menu
    menubutton $w.menu.select -text "Select" \
        -menu $w.menu.select.m
    menu $w.menu.select.m
    $w.menu.select.m add command -label "Select All" \
	-accelerator "  a" \
	-command "$w.menu.select configure -state disabled; update idletasks; \
                  Command::SelectAll $editor; \
                  $w.menu.select configure -state normal"
    bind $w <a> "$w.menu.select.m invoke {Select All}"
    $w.menu.select.m add sep
    $w.menu.select.m add command -label "Select Neighbours" \
	-accelerator "  n" \
	-command "$w.menu.select configure -state disabled; update idletasks; \
                  Command::SelectNeighbours $editor; \
                  $w.menu.select configure -state normal"
    bind $w <n> "$w.menu.select.m invoke {Select Neighbours}"
    $w.menu.select.m add command -label "Select Member" \
	-accelerator "  m" \
	-command "$w.menu.select configure -state disabled; update idletasks; \
                  Command::SelectMember $editor; \
                  $w.menu.select configure -state normal"
    bind $w <m> "$w.menu.select.m invoke {Select Member}"
    $w.menu.select.m add sep
    $w.menu.select.m add command -label "Select by Type..." \
	-accelerator "  T" \
	-command "$w.menu.select configure -state disabled; update idletasks; \
                  Command::SelectType $editor; \
                  $w.menu.select configure -state normal"
    bind $w <T> "$w.menu.select.m invoke {Select by Type...}"
    $w.menu.select.m add command -label "Select by Name..." \
	-accelerator "  N" \
	-command "$w.menu.select configure -state disabled; update idletasks; \
                  Command::SelectName $editor; \
                  $w.menu.select configure -state normal"
    bind $w <N> "$w.menu.select.m invoke {Select by Name...}"
    $w.menu.select.m add command -label "Select by Address..." \
	-accelerator "  A" \
	-command "$w.menu.select configure -state disabled; update idletasks; \
                  Command::SelectAddress $editor; \
                  $w.menu.select configure -state normal"
    bind $w <A> "$w.menu.select.m invoke {Select by Address...}"
    $w.menu.select.m add command -label "Select by Label..." \
        -accelerator "  L" \
        -command "$w.menu.select configure -state disabled; update idletasks; \
                  Command::SelectLabel $editor; \
                  $w.menu.select configure -state normal"
    bind $w <L> "$w.menu.select.m invoke {Select by Label...}"

    # set up the structure menu
    menubutton $w.menu.structure -text "Structure" \
        -menu $w.menu.structure.m
    menu $w.menu.structure.m
    $w.menu.structure.m add command -label "Bring to Front" \
	-accelerator "  f" \
        -command "$w.menu.structure configure -state disabled; \
                  update idletasks; \
                  Command::Front $editor; \
                  $w.menu.structure configure -state normal"
    bind $w <f> "$w.menu.structure.m invoke {Bring to Front}"
    $w.menu.structure.m add command -label "Send to Back" \
        -accelerator "  b" \
        -command "$w.menu.structure configure -state disabled; \
                  update idletasks; \
                  Command::Back $editor; \
                  $w.menu.structure configure -state normal"
    bind $w <b> "$w.menu.structure.m invoke {Send to Back}"
    $w.menu.structure.m add sep
    $w.menu.structure.m add command -label "Group" \
        -accelerator "  g" \
        -command "$w.menu.structure configure -state disabled; \
                  update idletasks; \
                  Command::Group $editor; \
                  $w.menu.structure configure -state normal"
    bind $w <g> "$w.menu.structure.m invoke Group"
    $w.menu.structure.m add command -label "Ungroup" \
        -accelerator "  u" \
        -command "$w.menu.structure configure -state disabled; \
                  update idletasks; \
                  Command::Ungroup $editor; \
                  $w.menu.structure configure -state normal"
    bind $w <u> "$w.menu.structure.m invoke Ungroup"
    $w.menu.structure.m add command -label "Collapse" \
        -accelerator "  c" \
        -command "$w.menu.structure configure -state disabled; \
                  update idletasks; \
                  Command::Collapse $editor; \
                  $w.menu.structure configure -state normal"
    bind $w <c> "$w.menu.structure.m invoke Collapse"
    $w.menu.structure.m add command -label "Expand" \
        -accelerator "  e" \
        -command "$w.menu.structure configure -state disabled; \
                  update idletasks; \
                  Command::Expand $editor; \
                  $w.menu.structure configure -state normal"
    bind $w <e> "$w.menu.structure.m invoke Expand"
    $w.menu.structure.m add sep
    $w.menu.structure.m add command -label "Join Group" \
        -accelerator "  j" \
        -command "$w.menu.structure configure -state disabled; \
                  update idletasks; \
                  Command::Join $editor; \
                  $w.menu.structure configure -state normal"
    bind $w <j> "$w.menu.structure.m invoke {Join Group}"
    $w.menu.structure.m add command -label "Leave Group" \
        -accelerator "  l" \
        -command "$w.menu.structure configure -state disabled; \
                  update idletasks; \
                  Command::RemoveGroup $editor; \
                  $w.menu.structure configure -state normal"
    bind $w <l> "$w.menu.structure.m invoke {Leave Group}"

    menu $w.menu.edit.m.a -tearoff false
    $w.menu.edit.m.a add command -label Create... \
	-command "Command::Attribute $editor create"
    $w.menu.edit.m.a add command -label Delete... \
	-command "Command::Attribute $editor delete"
    $w.menu.edit.m.a add command -label Edit... \
	-command "Command::Attribute $editor edit"
    
#    menu $w.menu.edit.m.l -tearoff false
#    $w.menu.edit.m.l add command -label name \
#	-command "Command::Label $editor name"
#    $w.menu.edit.m.l add command -label address \
#	-command "Command::Label $editor address"
#    $w.menu.edit.m.l add command -label attribute \
#	-command "Command::Label $editor attribute"
#    $w.menu.edit.m.l add sep
#    $w.menu.edit.m.l add command -label clear \
#	-command "Command::Label $editor clear"
    
    # set up the icon menu
    menubutton $w.menu.icon -text "Icon" \
        -menu $w.menu.icon.m
    menu $w.menu.icon.m
    $w.menu.icon.m add cascade -label "Node" \
	-menu $w.menu.icon.m.node
    $w.menu.icon.m add cascade -label "Network" \
	-menu $w.menu.icon.m.network
    $w.menu.icon.m add cascade -label "Group" \
	-menu $w.menu.icon.m.group
    $w.menu.icon.m add cascade -label "Reference" \
	-menu $w.menu.icon.m.reference
    $w.menu.icon.m add cascade -label "Font" \
	-menu $w.menu.icon.m.font
#    if {[$editor color]} { 
	$w.menu.icon.m add cascade -label "Color" \
	    -menu $w.menu.icon.m.color
#    }

    # set up the node menu
    menu $w.menu.icon.m.node
    set i 1
    while {[set val [$editor attribute node$i]]!=""} {
	set name [lrange $val 1 end]
	if {$name == ""} {
            $w.menu.icon.m.node add sep
        } else {
	    tkined_makemenu $w.menu.icon.m.node $name last name
	    $last add radio -label " $name" \
		-variable node$w -value $last$name \
		-command "$w.menu.icon configure -state disabled; \
                      update idletasks; \
                      Command::Icon $editor NODE {$name}; \
                      $w.menu.icon configure -state normal"
	}
	incr i
    }
    $w.menu.icon.m.node add radio -label " Machine" \
	-variable node$w -value machine \
	-command "$w.menu.icon configure -state disabled; \
                  update idletasks; \
                  Command::Icon $editor NODE machine; \
                  $w.menu.icon configure -state normal"
    $w.menu.icon.m.node invoke $i

    # set up the network menu
    menu $w.menu.icon.m.network
    set i 1
    while {[set val [$editor attribute network$i]]!=""} {
	set name [lrange $val 1 end]
	if {$name == ""} {
            $w.menu.icon.m.network add sep
        } else {
	    tkined_makemenu $w.menu.icon.m.network $name last name
	    $last add radio -label " $name" \
		-variable network$w -value $last$name \
		-command "$w.menu.icon configure -state disabled; \
                      update idletasks; \
                      Command::Icon $editor NETWORK {$name}; \
                      $w.menu.icon configure -state normal"
	}
        incr i
    }
    $w.menu.icon.m.network add radio -label " Network" \
	-variable network$w -value network \
	-command "$w.menu.icon configure -state disabled;
                  update idletasks; \
                  Command::Icon $editor NETWORK network; \
                  $w.menu.icon configure -state normal"
    $w.menu.icon.m.network invoke $i

    # set up the group menu
    menu $w.menu.icon.m.group
    set i 1
    while {[set val [$editor attribute group$i]]!=""} {
	set name [lrange $val 1 end]
	if {$name == ""} {
            $w.menu.icon.m.group add sep
        } else {
	    tkined_makemenu $w.menu.icon.m.group $name last name
	    $last add radio -label " $name" \
		-variable group$w -value $last$name \
		-command "$w.menu.icon configure -state disabled;
                      update idletasks; \
                      Command::Icon $editor GROUP {$name}; \
                      $w.menu.icon configure -state normal"
	}
        incr i
    }
    $w.menu.icon.m.group add radio -label " Group" \
	-variable group$w -value group \
	-command "$w.menu.icon configure -state disabled;
                  update idletasks; \
                  Command::Icon $editor GROUP group; \
                  $w.menu.icon configure -state normal"
    $w.menu.icon.m.group invoke $i

    # set up the reference menu
    menu $w.menu.icon.m.reference
    set i 1
    while {[set val [$editor attribute reference$i]]!=""} {
	set name [lrange $val 1 end]
	if {$name == ""} {
            $w.menu.icon.m.reference add sep
        } else {
	    tkined_makemenu $w.menu.icon.m.reference $name last name
	    $last add radio -label " $name" \
		-variable reference$w -value $last$name \
		-command "$w.menu.icon configure -state disabled;
                      update idletasks; \
                      Command::Icon $editor REFERENCE {$name}; \
                      $w.menu.icon configure -state normal"
	}
        incr i
    }
    $w.menu.icon.m.reference add radio -label " Reference" \
	-variable reference$w -value reference \
	-command "$w.menu.icon configure -state disabled;
                  update idletasks; \
                  Command::Icon $editor REFERENCE reference; \
                  $w.menu.icon configure -state normal"
    $w.menu.icon.m.reference invoke $i

    # set up the font menu
    menu $w.menu.icon.m.font
    set i 1
    while {[set val [$editor attribute font$i]]!=""} {
	set name [lrange $val 1 end]
	if {$name == ""} {
	    $w.menu.icon.m.font add sep
	} else {
	    tkined_makemenu $w.menu.icon.m.font $name last name
	    $last add radio -label " $name" \
		-variable font$w -value $last$name \
		-command "$w.menu.icon configure -state disabled;
                          update idletasks; \
                          Command::Font $editor {$name}; \
                          $w.menu.icon configure -state normal"
	    $editor attribute font-$name [lindex $val 0]
	}
	incr i
    }
    $w.menu.icon.m.font add radio -label " Fixed" \
	-variable font$w -value default \
	-command "$w.menu.icon configure -state disabled;
                  update idletasks; \
                  Command::Font $editor fixed; \
                  $w.menu.icon configure -state normal"
    $w.menu.icon.m.font invoke $i

    # set up the color menu
#    if {[$editor color]} { 
	menu $w.menu.icon.m.color
	set i 1
	while {[set val [$editor attribute color$i]]!=""} {
	    set name [lrange $val 1 end]
	    if {$name == ""} {
		$w.menu.icon.m.color add sep
	    } else {
		tkined_makemenu $w.menu.icon.m.color $name last name
		$last add radio -label " $name" \
		    -variable color$w -value $name \
		    -command "$w.menu.icon configure -state disabled;
                          update idletasks; \
                          Command::Color $editor {$name}; \
                          $w.menu.icon configure -state normal"
		$editor attribute color-$name [lindex $val 0]
	    }
	    incr i
	}
	$w.menu.icon.m.color add radio -label " Black" \
	    -variable color$w -value Black \
	    -command "$w.menu.icon configure -state disabled;
                  update idletasks; \
                  Command::Color $editor Black; \
                  $w.menu.icon configure -state normal"
	$editor attribute color-Black black
	$w.menu.icon.m.color invoke $i
#    }

    # set up the option menu
    menubutton $w.menu.opts -text "Options" \
	-menu $w.menu.opts.m
    menu $w.menu.opts.m
    $w.menu.opts.m add cascade -label "Media" \
        -menu $w.menu.opts.m.page
    $w.menu.opts.m add cascade -label "Orientation" \
        -menu $w.menu.opts.m.orient
    $w.menu.opts.m add sep
    $w.menu.opts.m add checkbutton -label "Flip Toolbox" \
	-accelerator "  P" \
        -command "Command::ToggleToolBox $editor" \
	-offvalue 0 -onvalue 1 -variable newToolbox$w
    bind $w <P> "$w.menu.opts.m invoke {Flip Toolbox}"
    $w.menu.opts.m add checkbutton -label "Show Toolbox" \
	-accelerator "  H" \
        -command "Command::ToggleToolBox $editor" \
	-offvalue 0 -onvalue 1 -variable showToolbox$w
    bind $w <H> "$w.menu.opts.m invoke {Show Toolbox}"
    $w.menu.opts.m add checkbutton -label "Lock Editor" \
	-accelerator "  C" \
        -command "Command::LockEditor $editor" \
	-offvalue 0 -onvalue 1 -variable lockEditor$w
    bind $w <C> "$w.menu.opts.m invoke {Lock Editor}"
    $w.menu.opts.m invoke "Show Toolbox"
    $w.menu.opts.m add sep
    $w.menu.opts.m add checkbutton -label "Strict Motif" \
	-offvalue 0 -onvalue 1 -variable tk_strictMotif
    if {[info commands memory] != ""} {
	$w.menu.opts.m add sep
	$w.menu.opts.m add checkbutton -label "Memory Validation" \
		-command {memory validate $memval} \
		-offvalue off -onvalue on -variable memval
        set memval off
	$w.menu.opts.m add checkbutton -label "Memory Trace" \
		-command {memory trace $memtrace} \
		-offvalue off -onvalue on -variable memtrace
        set memtrace off
	$w.menu.opts.m add command -label "Memory Info" \
	    -command "memory info"
    }

    menu $w.menu.opts.m.page -tearoff false
    global tkined_pageSize$w
    $w.menu.opts.m.page add radio -label "Letter" \
	-variable tkined_pageSize$w -value Letter \
	-command "$w.menu.opts configure -state disabled; update idletasks; \
                  $editor pagesize Letter; \
                  $w.menu.opts configure -state normal"
    $w.menu.opts.m.page add radio -label "Legal" \
	-variable tkined_pageSize$w -value Legal \
	-command "$w.menu.opts configure -state disabled; update idletasks; \
                  $editor pagesize Legal; \
                  $w.menu.opts configure -state normal"
    $w.menu.opts.m.page add radio -label "DIN A4" \
	-variable tkined_pageSize$w -value A4 \
	-command "$w.menu.opts configure -state disabled; update idletasks; \
                  $editor pagesize A4; \
                  $w.menu.opts configure -state normal"
    $w.menu.opts.m.page add radio -label "DIN A3" \
	-variable tkined_pageSize$w -value A3 \
	-command "$w.menu.opts configure -state disabled; update idletasks; \
                  $editor pagesize A3; \
                  $w.menu.opts configure -state normal"
    $w.menu.opts.m.page add radio -label "DIN A2" \
	-variable tkined_pageSize$w -value A2 \
	-command "$w.menu.opts configure -state disabled; update idletasks; \
                  $editor pagesize A2; \
                  $w.menu.opts configure -state normal"
    $w.menu.opts.m.page add radio -label "DIN A1" \
	-variable tkined_pageSize$w -value A1 \
	-command "$w.menu.opts configure -state disabled; update idletasks; \
                  $editor pagesize A1; \
                  $w.menu.opts configure -state normal"
    $w.menu.opts.m.page invoke "DIN A4"

    menu $w.menu.opts.m.orient -tearoff false
    global tkined_orientation$w
    $w.menu.opts.m.orient add radio -label "Portrait" \
	-variable tkined_orientation$w -value portrait \
	-command "$w.menu.opts configure -state disabled; update idletasks; \
                  $editor orientation portrait; \
                  $w.menu.opts configure -state normal"
    $w.menu.opts.m.orient add radio -label "Landscape" \
	-variable tkined_orientation$w -value landscape \
	-command "$w.menu.opts configure -state disabled; update idletasks; \
                  $editor orientation landscape; \
                  $w.menu.opts configure -state normal"
    $w.menu.opts.m.orient invoke Landscape

    # set up the help menu
    menubutton $w.menu.help -text "Help" \
	-menu $w.menu.help.m
    menu $w.menu.help.m
    $w.menu.help.m add command -label "General" \
	-command "$w.menu.help configure -state disabled; update idletasks; \
                  Help::general $editor; \
	          $w.menu.help configure -state normal"
    $w.menu.help.m add command -label "Status" \
	-command "$w.menu.help configure -state disabled; update idletasks; \
                  Help::status $editor; \
                  $w.menu.help configure -state normal"
    $w.menu.help.m add command -label "Key Bindings" \
	-command "$w.menu.help configure -state disabled; update idletasks; \
                  Help::key_bindings $editor; \
                  $w.menu.help configure -state normal"
    $w.menu.help.m add command -label "Man Page (tkined)" \
	-command "$w.menu.help configure -state disabled; update idletasks; \
                  Help::manpage $editor tkined; \
                  $w.menu.help configure -state normal"
    $w.menu.help.m add command -label "Man Page (scotty)" \
	-command "$w.menu.help configure -state disabled; update idletasks; \
                  Help::manpage $editor scotty; \
                  $w.menu.help configure -state normal"
    
    pack $w.menu.file -side left
    pack $w.menu.select -side left
    pack $w.menu.opts -side left
    pack $w.menu.help -side right

    focus $w

    # bind the popup menu (alter tool) on the right mouse button

    bind $w.canvas <3> "Tool::AlterMark $editor \
	    \[%W canvasx %x\] \[%W canvasy %y\] %X %Y"
    Editor::unlock $editor

    if {[Editor::BooleanAttribute $editor optionFlipToolbox]} {
	$w.menu.opts.m invoke {Flip Toolbox}
    }
    if {! [Editor::BooleanAttribute $editor optionShowToolbox]} {
	$w.menu.opts.m invoke {Show Toolbox}
    }
    if {[Editor::BooleanAttribute $editor optionLockEditor]} {
	$w.menu.opts.m invoke {Lock Editor}
    }
    if {[Editor::BooleanAttribute $editor optionStrictMotif]} {
	$w.menu.opts.m invoke {Strict Motif}
    }

    # Key binding to turn on debugging mode
    bind $w <D> "set tkined_debug 1"
    bind $w <Q> "set tkined_debug 0"

    # A very special binding to kill an editor if the toplevel 
    # receives a destroy event.

    bind $w <Destroy> "yoyo %W $editor"

    proc yoyo {w editor} {
	if {$w == [$editor toplevel]} { 
	    after idle [list catch "$editor delete"]
	}
    }

    # now its time to map the whole thing on the screen
    update
    global geometry
    if {[info exists geometry]} {
        wm geometry $w $geometry
    }
    wm deiconify $w
    update

    # fire up all interpreters for this editor
    set i 1
    while {[set interp [$editor attribute interpreter$i]]!=""} {
        incr i
	if {[catch {INTERPRETER create $interp} interpreter]} {
	    if {$interp != "manager.tcl"} {
		Dialog::acknowledge $w.canvas \
		    "Sorry, $interp not found !!" "" \
		    "Check your tkined.defaults and your TKINED_PATH."
	    }
	} else {
	    $interpreter editor $editor
	    $interpreter canvas $w.canvas
	}
    }
}

##
## Lock the editor. This proc removed all menus and bindings that
## could change the current map.
##

proc Editor::lock { editor } {
    set w [$editor toplevel]
    foreach menu "$w.menu.edit $w.menu.structure $w.menu.icon" {
	pack forget $menu
	foreach child [winfo children $menu] {
	    set last [$child index last]
	    for {set i 0} {$i <= $last} {incr i} {
		catch {$child entryconfigure $i -state disabled}
	    }
	}
    }
    
    # remove the move bindings on the middle mouse button
    bind $w.canvas <2> ""
    bind $w.canvas <B2-Motion> ""
    bind $w.canvas <ButtonRelease-2> ""
}

##
## Unlock the editor. This proc adds all menus and bindings that
## allow to edit the current map.
##

proc Editor::unlock { editor } {
    set w [$editor toplevel]
    set last $w.menu.select
    foreach menu "$w.menu.edit $w.menu.structure $w.menu.icon" {
	pack $menu -after $last -side left
	foreach child [winfo children $menu] {
	    set last [$child index last]
	    for {set i 0} {$i <= $last} {incr i} {
		catch {$child entryconfigure $i -state normal}
	    }
	}
	set last $menu
    }
    
    Tool::Move $editor
}


##
## Change the title of the toplevel window to show the new filename.
##

proc Editor::filename { editor } { 
    wm title [$editor toplevel] "$editor: [$editor filename]"
}

##
## Set the canvas to the new width and size. Set the global
## variables tkined_orientation$w and tkined_pageSize$w to
## update the menu.
##

proc Editor::pagesize { editor width height } {

    set c [$editor toplevel].canvas
    $c configure -scrollregion "0 0 $width $height"

    set w [$editor toplevel]
    global tkined_orientation$w tkined_pageSize$w
    set tkined_orientation$w [$editor orientation]
    set tkined_pageSize$w    [$editor pagesize]
}

##
## Create a PostScript dump of the canvas. We need to do some magic here
## to get the background right. I think tk is a bit broken here...
##

proc Editor::postscript { editor } {

    global tkined_ps_map
    set w [$editor toplevel]
    set tkined_ps_map(fixed) [list Courier 10]

    set orientation [$editor orientation]
    set region [$w.canvas cget -scrollregion]
    set width  [lindex $region 2]
    set height [lindex $region 3]

    foreach item [$w.canvas find all] {
	switch [$w.canvas type $item] {
	    bitmap -
	    stripchart -
	    barchart {
		$w.canvas itemconfigure $item -background White
	    }
	}
	$w.canvas itemconfigure clip$item -fill White -outline White
    }

    # make all selection marks invisible

    foreach id [$editor selection] {
	foreach item [$id items] {
	    $w.canvas itemconfigure mark$item -outline "" -fill ""
	}
    }

    update

    if {[catch {$w.canvas postscript -colormode color \
            -fontmap tkined_ps_map \
	    -x 0 -y 0 -height $height -width $width \
            -rotate [expr {$orientation == "landscape"}]} result]} {
	Dialog::acknowledge $w.canvas "Generating PostScript failed:" $result
    }

    set color [$w.canvas cget -background]
    foreach item [$w.canvas find all] {
	switch [$w.canvas type $item] {
	    bitmap -
	    stripchart -
	    barchart {
		$w.canvas itemconfigure $item -background $color
	    }
	}
	$w.canvas itemconfigure clip$item -fill $color -outline $color
    }

    # restore the selection marks

    foreach id [$editor selection] {
	foreach item [$id items] {
	    $w.canvas itemconfigure mark$item -outline black -fill black
	}
    }

    return $result
}

##
## Print a given file on a printer by running it through a print 
## command (like lpr) or save it to a file.
##

proc Editor::print {editor w srcFile} {
    global env

    set lpr [$editor attribute printcmd]
    if {$lpr == ""} {
	foreach dir [split $env(PATH) ":"] {
	    if {[file executable $dir/lpr]} {
		set lpr $dir/lpr
		break
	    }
	}
    }

    set res [Dialog::request $w "Saved to temporary file $srcFile." \
	     [list [list "Print Command:" "$lpr" entry 30] ] \
	     [list print "save to file" cancel]]

    if {[lindex $res 0] == "cancel"} return

    if {[lindex $res 0] == "print"} {
	set cmd [lindex $res 1]
	$editor attribute printcmd $cmd
	if {[catch {eval exec $cmd $srcFile} err]} {
	    Dialog::acknowledge $w "$lpr $srcFile failed:" "" $err
	}
    } else {
	set dir [$editor dirname]
	
	set dstFile [Dialog::fileselect $w "Save to file:" $dir]
	if {$dstFile == ""} return
	
	while {[file exists $dstFile] && ![file writable $dstFile]} {
	    set dstFile [Dialog::fileselect $w "Save to file:" $dir]
	    if {$dstFile == ""} return
	}
	
	if {[file exists $dstFile]} {
	    if {![file writable $dstFile]} {
		Dialog::acknowledge $w "Can not write $dstFile"
		return
	    }
	    set res [Dialog::confirm $w "Replace file $dstFile?" \
		     [list replace cancel]]
	    if {$res == "cancel"} return
	}
	
	if {[catch {exec cp $srcFile $dstFile} err]} {
	    Dialog::acknowledge $w "Failed to write to $dstFile:" "" $err
	}
    }
}

##
## I do not know what to do with this button.
##

proc Editor::UselessButton { c } {
    if {[catch {exec /bin/sh -c "( fortune || yow )" 2> /dev/null} txt]} {
	set txt "You do not have fortune or yow on your system? Unbelievable!"
    }
    Dialog::acknowledge $c $txt
}

##
## Check whether a boolean editor attribute exists and return the
## boolean value as 1 or 0.
##

proc Editor::BooleanAttribute { editor name {default 0} } {
    set val [$editor attribute $name]
    if {$val == ""} {
	return $default
    }
    return [expr [lsearch "yes true on 1" $val] >= 0]
}

##
## Make a menu entry. The ultimate test for this proc is to create a
## menu entry like: ined create MENU asdf "aa:bb:cc bb:cc:dd cc:bb:cc"
##

proc tkined_makemenu {w path rlast rname} {

    static widgetpath
    static count

    upvar $rlast last
    upvar $rname name 

    if {![info exists count]} { set count 0 }
    incr count

    set path [split $path :]
    set len  [llength $path]
    set name [lindex $path [incr len -1]]
    set last $w

    for {set i 0} {$i < $len} {incr i} {
	set elem [join [lrange $path 0 $i] :]
	if {![info exists widgetpath($w$elem)]} {
	    set widgetpath($w$elem) $last.$count
	    menu $widgetpath($w$elem)
	    $last add cascade -label " [lindex $path $i]" \
		-menu $widgetpath($w$elem)
	}

	set last $widgetpath($w$elem)
    }
}
