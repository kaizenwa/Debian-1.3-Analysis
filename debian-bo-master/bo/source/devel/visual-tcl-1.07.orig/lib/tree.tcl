##############################################################################
#
# tree.tcl - widget tree browser and associated procedures
#
# Copyright (C) 1996-1997 Stewart Allen
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

##############################################################################
#

proc vTcl:show_wtree {} {
    global vTcl
    Window show .vTcl.tree
}

proc vTcl:clear_wtree {} {
set b .vTcl.tree.fra4.can8
foreach i [winfo children $b] {
    destroy $i
}
$b delete TEXT LINE
$b configure -scrollregion "0 0 0 0"
}

proc vTcl:init_wtree {} {
global vTcl
vTcl:destroy_handles
vTcl:clear_wtree
set b .vTcl.tree.fra4.can8
set y 10
set tree [vTcl:list_widget_tree .]
foreach i $tree {
    if {$i == "."} {
        set depth 1
    } else {
        set depth [llength [split $i "."]]
    }
    set x [expr $depth * 30 - 15]
    set x2 [expr $x + 40]
    set y2 [expr $y + 15]
    set j [vTcl:rename $i]
    if {$i == "."} {
        set c toplevel
    } else {
        set c [string tolower [vTcl:get_class $i]]
        if {$c == "scrollbar" || $c == "scale"} {
            append c "_[string index [$i cget -orient] 0]"
        }
    }
    if {![winfo exists $b.$j]} {
        set l($depth) [llength [vTcl:get_children $i]]
        if {$i == "."} {
            incr l($depth) -1
        }
        if {$depth > 1} {
            incr l([expr $depth - 1]) -1
        }
        button $b.$j -image ctl_$c -command "
			vTcl:show $i
			vTcl:active_widget $i
		"
        vTcl:set_balloon $b.$j $i
        $b create window $x $y -window $b.$j -anchor nw -tags $b.$j
        switch $c {
            toplevel {set t [wm title $i]}
            label      -
            button     -
            menubutton {set t [$i cget -text]}
            default    {set t ""}
        }
        $b create text $x2 $y2 -text $t -anchor w -tags TEXT
		set d2 [expr $depth - 1]
        for {set k 1} {$k <= $d2} {incr k} {
            if {$depth > 1} {
                set xx2 [expr $k * 30 + 15]
                set xx1 [expr $k * 30]
                set yy1 [expr $y + 30]
                set yy2 [expr $y + 30 - 15]
				if {$k == $d2} {
					if {$l($k) > 0} {
						$b create line $xx1 $y $xx1 $yy1 -tags LINE
						$b create line $xx1 $yy2 $xx2 $yy2 -tags LINE
					} else {
						$b create line $xx1 $y $xx1 $yy2 -tags LINE
						$b create line $xx1 $yy2 $xx2 $yy2 -tags LINE
					}
				} elseif {$l($k) > 0} {
                    $b create line $xx1 $y $xx1 $yy1 -tags LINE
				}
            }
        }
    } else {
        $b coords $b.$j $x $y
    }
    incr y 30
}
$b configure -scrollregion "0 0 [expr $x + 200] $y"
}

proc vTclWindow.vTcl.tree {args} {
    set base .vTcl.tree
    if {[winfo exists .vTcl.tree]} {
        wm deiconify .vTcl.tree; return
    }
    toplevel .vTcl.tree -class vTcl
    wm focusmodel .vTcl.tree passive
    wm geometry .vTcl.tree 296x243+75+142
    wm maxsize .vTcl.tree 1137 870
    wm minsize .vTcl.tree 1 1
    wm overrideredirect .vTcl.tree 0
    wm resizable .vTcl.tree 1 1
    wm deiconify .vTcl.tree
    wm title .vTcl.tree "Widget Browser"
    frame .vTcl.tree.fra4 \
        -height 30 -width 30 
    pack .vTcl.tree.fra4 \
        -in .vTcl.tree -anchor center -expand 1 -fill both -ipadx 0 -ipady 0 \
        -padx 0 -pady 0 -side top 
    canvas .vTcl.tree.fra4.can8 \
        -borderwidth 2 -height 0 -highlightthickness 0 -relief ridge \
        -scrollregion {0 0 0 0} -width 0 \
        -xscrollcommand {.vTcl.tree.fra6.scr7 set} \
        -yscrollcommand {.vTcl.tree.fra4.scr9 set} 
    pack .vTcl.tree.fra4.can8 \
        -in .vTcl.tree.fra4 -anchor center -expand 1 -fill both -ipadx 0 \
        -ipady 0 -padx 2 -pady 2 -side left 
    scrollbar .vTcl.tree.fra4.scr9 \
        -borderwidth 1 -command {.vTcl.tree.fra4.can8 yview} \
        -highlightthickness 0 -width 10 
    pack .vTcl.tree.fra4.scr9 \
        -in .vTcl.tree.fra4 -anchor center -expand 0 -fill y -ipadx 0 \
        -ipady 0 -padx 0 -pady 2 -side right 
    frame .vTcl.tree.fra6 \
        -height 30 -width 30 
    pack .vTcl.tree.fra6 \
        -in .vTcl.tree -anchor center -expand 0 -fill x -ipadx 0 -ipady 0 \
        -padx 2 -pady 0 -side top 
    scrollbar .vTcl.tree.fra6.scr7 \
        -borderwidth 1 -command {.vTcl.tree.fra4.can8 xview} \
        -highlightthickness 0 -orient horizontal -width 10 
    pack .vTcl.tree.fra6.scr7 \
        -in .vTcl.tree.fra6 -anchor center -expand 1 -fill both -ipadx 0 \
        -ipady 0 -padx 2 -pady 0 -side left 
    frame .vTcl.tree.fra6.fra10 \
        -borderwidth 1 -height 12 -relief raised -width 12 
    pack .vTcl.tree.fra6.fra10 \
        -in .vTcl.tree.fra6 -anchor center -expand 0 -fill none -ipadx 0 \
        -ipady 0 -padx 0 -pady 0 -side right 
    frame .vTcl.tree.fra11 \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    pack .vTcl.tree.fra11 \
        -in .vTcl.tree -anchor center -expand 0 -fill both -ipadx 0 -ipady 0 \
        -padx 2 -pady 2 -side top 
    button .vTcl.tree.fra11.but3 \
        -command vTcl:init_wtree \
         \
        -highlightthickness 0 -padx 5 -pady 2 -text Reload -width 5 
    pack .vTcl.tree.fra11.but3 \
        -in .vTcl.tree.fra11 -anchor center -expand 1 -fill both -ipadx 0 \
        -ipady 0 -padx 2 -pady 2 -side left 
    button .vTcl.tree.fra11.but1 \
        -command {destroy .vTcl.tree} \
         \
        -highlightthickness 0 -padx 5 -pady 2 -text Done -width 5 
    pack .vTcl.tree.fra11.but1 \
        -in .vTcl.tree.fra11 -anchor center -expand 1 -fill both -ipadx 0 \
        -ipady 0 -padx 2 -pady 2 -side left 
}

proc vTclWindow(post).vTcl.tree {args} {
    vTcl:init_wtree
    vTcl:setup_vTcl:bind .vTcl.tree
}

