##############################################################################
#
# dragsize.tcl - procedures to handle widget sizing and movement
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

proc vTcl:bind_button_1 {target x y} {
	global vTcl
	if { [lindex [split %W .] 1] != "vTcl" } {
		if {$target != "." && [winfo class $target] != "Toplevel"} {
			vTcl:active_widget $target
			vTcl:grab $target $x $y
			set vTcl(cursor,last) [$target cget -cursor]
			$target configure -cursor fleur
		} else {
			vTcl:active_widget $target
		}
	}
}

proc vTcl:bind_button_2 {target x y} {
	global vTcl
	if {$vTcl(w,widget) != "." && \
		[winfo class $vTcl(w,widget)] != "Toplevel" && \
		$vTcl(w,widget) != ""} {
			vTcl:grab $vTcl(w,widget) $x $y
			set vTcl(cursor,last) [$vTcl(w,widget) cget -cursor]
			$vTcl(w,widget) configure -cursor fleur
	}
}

proc vTcl:bind_motion {x y} {
	global vTcl
	if {$vTcl(w,widget) != "." && $vTcl(w,class) != "Toplevel"} {
		vTcl:grab_motion $vTcl(w,widget) $x $y
	}
}

proc vTcl:bind_release {x y} {
	global vTcl
	$vTcl(w,widget) configure -cursor $vTcl(cursor,last)
	vTcl:place_handles $vTcl(w,widget)
	vTcl:grab_release $vTcl(w,widget)
	vTcl:update_widget_info $vTcl(w,widget)
}

proc vTcl:grab {widget absX absY} {
    global vTcl
    grab $widget
    set vTcl(w,didmove) 0
    set vTcl(grab,startX) [vTcl:grid_snap x $absX]
    set vTcl(grab,startY) [vTcl:grid_snap y $absY]
}

proc vTcl:grab_motion {widget absX absY} {
    global vTcl
    set vTcl(w,didmove) 1
    if { $vTcl(w,manager) == "place" } {
        place $widget \
            -x [vTcl:grid_snap x \
                [expr $absX-$vTcl(grab,startX)+$vTcl(w,x)]] \
            -y [vTcl:grid_snap y \
                [expr $absY-$vTcl(grab,startY)+$vTcl(w,y)]]
    }
    vTcl:place_handles $widget
}

proc vTcl:grab_release {widget} {
    global vTcl
    grab release $widget
    if { $vTcl(w,didmove) == 1 } {
        set vTcl(undo) [vTcl:dump_widget_quick $vTcl(w,widget)]
        vTcl:passive_push_action $vTcl(undo) $vTcl(redo)
    }
}

proc vTcl:grab_resize {absX absY handle} {
    global vTcl
    set vTcl(w,didmove) 1
    set widget $vTcl(w,widget)
    set X [vTcl:grid_snap x $absX]
    set Y [vTcl:grid_snap y $absY]
    set deltaX [expr $X - $vTcl(grab,startX)]
    set deltaY [expr $Y - $vTcl(grab,startY)]
    switch $vTcl(w,manager) {
        place {
            switch $handle {
                n {
                    set newX $vTcl(w,x)
                    set newY [expr $vTcl(w,y) + $deltaY]
                    set newW $vTcl(w,width)
                    set newH [expr $vTcl(w,height) - $deltaY]
                }
                e {
                    set newX $vTcl(w,x)
                    set newY $vTcl(w,y)
                    set newW [expr $vTcl(w,width) + $deltaX]
                    set newH $vTcl(w,height)
                }
                s {
                    set newX $vTcl(w,x)
                    set newY $vTcl(w,y)
                    set newW $vTcl(w,width)
                    set newH [expr $vTcl(w,height) + $deltaY]
                }
                w {
                    set newX [expr $vTcl(w,x) + $deltaX]
                    set newY $vTcl(w,y)
                    set newW [expr $vTcl(w,width) - $deltaX]
                    set newH $vTcl(w,height)
                }
                nw {
                    set newX [expr $vTcl(w,x) + $deltaX]
                    set newY [expr $vTcl(w,y) + $deltaY]
                    set newW [expr $vTcl(w,width) - $deltaX]
                    set newH [expr $vTcl(w,height) - $deltaY]
                }
                ne {
                    set newX $vTcl(w,x)
                    set newY [expr $vTcl(w,y) + $deltaY]
                    set newW [expr $vTcl(w,width) + $deltaX]
                    set newH [expr $vTcl(w,height) - $deltaY]
                }
                se {
                    set newX $vTcl(w,x)
                    set newY $vTcl(w,y)
                    set newW [expr $vTcl(w,width) + $deltaX]
                    set newH [expr $vTcl(w,height) + $deltaY]
                }
                sw {
                    set newX [expr $vTcl(w,x) + $deltaX]
                    set newY $vTcl(w,y)
                    set newW [expr $vTcl(w,width) - $deltaX]
                    set newH [expr $vTcl(w,height) + $deltaY]
                }
            }
            place $widget -x $newX -y $newY -width $newW -height $newH
        }
        grid -
        pack {
            switch $vTcl(w,class) {
                Label -
                Entry -
                Message -
                Scrollbar -
                Scale {
                    set vTcl(w,opt,-height) ""
                }
            }
            switch $handle {
                n {
                    set newW $vTcl(w,opt,-width)
                    set newH [expr $vTcl(w,opt,-height) - $deltaY]
                }
                e {
                    set newW [expr $vTcl(w,opt,-width) + $deltaX]
                    set newH $vTcl(w,opt,-height)
                }
                s {
                    set newW $vTcl(w,opt,-width)
                    set newH [expr $vTcl(w,opt,-height) + $deltaY]
                }
                w {
                    set newW [expr $vTcl(w,opt,-width) - $deltaX]
                    set newH $vTcl(w,opt,-height)
                }
                nw {
                    set newW [expr $vTcl(w,opt,-width) - $deltaX]
                    set newH [expr $vTcl(w,opt,-height) - $deltaY]
                }
                ne {
                    set newW [expr $vTcl(w,opt,-width) + $deltaX]
                    set newH [expr $vTcl(w,opt,-height) - $deltaY]
                }
                se {
                    set newW [expr $vTcl(w,opt,-width) + $deltaX]
                    set newH [expr $vTcl(w,opt,-height) + $deltaY]
                }
                sw {
                    set newW [expr $vTcl(w,opt,-width) - $deltaX]
                    set newH [expr $vTcl(w,opt,-height) + $deltaY]
                }
            }
            if { $newW < 0 } { set newW 0 }
            if { $newH < 0 } { set newH 0 }
            switch $vTcl(w,class) {
                Label -
                Entry -
                Message -
                Scrollbar -
                Scale {
                    $widget configure -width $newW
                }
                default {
                    $widget configure -width $newW -height $newH
                }
            }
        }
    }
    vTcl:place_handles $widget
}

