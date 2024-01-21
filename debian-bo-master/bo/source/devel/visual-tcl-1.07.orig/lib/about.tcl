##############################################################################
#
# abour.tcl - dialog "about Visual Tcl"
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

proc vTclWindow.vTcl.about {args} {
    set base .vTcl.about
    if {[winfo exists .vTcl.about]} {
        wm deiconify .vTcl.about; return
    }
    toplevel .vTcl.about -class vTcl
    wm resizable .vTcl.about 0 0
    wm withdraw .vTcl.about
    wm title .vTcl.about "About Visual Tcl"
    label .vTcl.about.lab6 \
        -image title -relief raised -text label 
    pack .vTcl.about.lab6 \
        -in .vTcl.about -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 \
        -padx 5 -pady 5 -side top 
    label .vTcl.about.lab7 \
        -borderwidth 0 \
        -text {Copyright (C) 1996-1997 Stewart Allen} 
    pack .vTcl.about.lab7 \
        -in .vTcl.about -anchor center -expand 1 -fill both -ipadx 0 -ipady 0 \
        -padx 1 -pady 1 -side top 
    label .vTcl.about.lab2 \
        -borderwidth 0 \
        -text (stewart@neuron.com) 
    pack .vTcl.about.lab2 \
        -in .vTcl.about -anchor center -expand 1 -fill both -ipadx 0 -ipady 0 \
        -padx 0 -pady 0 -side top 
    frame .vTcl.about.fra8 \
        -height 30 -width 30 
    pack .vTcl.about.fra8 \
        -in .vTcl.about -anchor center -expand 1 -fill y -ipadx 0 -ipady 0 \
        -padx 2 -pady 2 -side top 
    label .vTcl.about.fra8.lab9 \
        -borderwidth 0 \
        -text Version 
    pack .vTcl.about.fra8.lab9 \
        -in .vTcl.about.fra8 -anchor center -expand 0 -fill none -ipadx 0 \
        -ipady 0 -padx 0 -pady 0 -side left 
    label .vTcl.about.fra8.lab10 \
        -borderwidth 0 \
         -text 1.04 \
        -textvariable vTcl(version) 
    pack .vTcl.about.fra8.lab10 \
        -in .vTcl.about.fra8 -anchor center -expand 0 -fill none -ipadx 0 \
        -ipady 0 -padx 0 -pady 0 -side left 
    button .vTcl.about.but11 \
        -command {destroy .vTcl.about} \
         -padx 11 \
        -pady 4 -text OK 
    pack .vTcl.about.but11 \
        -in .vTcl.about -anchor center -expand 0 -fill x -ipadx 0 -ipady 0 \
        -padx 0 -pady 0 -side top 
    update idletasks
    set sw [winfo screenwidth .]
    set sh [winfo screenheight .]
    set w [winfo reqwidth .vTcl.about]
    set h [winfo reqheight .vTcl.about]
    set x [expr ($sw - $w)/2]
    set y [expr ($sh - $h)/2]
    wm geometry .vTcl.about ${w}x${h}+$x+$y
    wm deiconify .vTcl.about
}

