##############################################################################
#
# toolbar.tcl - widget toolbar
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

proc vTcl:toolbar_create {args} {
    global vTcl
    set base .vTcl.toolbar
    if [winfo exists $base] {return}
    toplevel $base -width 0 -height 0 -class vTcl
    wm withdraw $base
    wm title $base "Widget Toolbar"
    wm grid $base 1 1 20 20
    wm geometry $base +0+110
    catch {wm geometry .vTcl.toolbar $vTcl(geometry,.vTcl.toolbar)}
}

proc vTcl:toolbar_add {type name image cmd_add} {
    global vTcl
    if ![winfo exists .vTcl.toolbar] {
        vTcl:toolbar_create
    }
    set base .vTcl.toolbar
    set f [vTcl:new_widget_name tb $base]
    button $f -bd 1 -image $image -comm "vTcl:new_widget $type \"$cmd_add\""
    vTcl:set_balloon $f $name
    lappend vTcl(tool,list) $f
}

proc vTclWindow.vTcl.toolbar {} {
    vTcl:toolbar_reflow
}

proc vTcl:toolbar_reflow {} {
    global vTcl
    set base .vTcl.toolbar
    set num [llength [winfo children $base]]
    set w [expr [winfo width $base] / 20]
    if {$w <= 0} {set w [expr $num / 7]}
    if {$w <= 0} {set w 2}
    set h [expr $num / $w]
    set x 0
    set gr ""
    foreach i $vTcl(tool,list) {
        append gr "$i "
        incr x
        if {$x >= $w} {
            eval "grid $gr"
            set x 0
            set gr ""
        }
    }
    if {$gr != ""} {
        eval "grid $gr"
    }
	update idletasks
    wm deiconify $base
}

