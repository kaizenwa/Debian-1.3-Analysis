##############################################################################
#
# status.tcl - small widget status window
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

proc vTcl:widget_status {} {
    global vTcl

    set tmp $vTcl(gui,main).widgets
    if { [winfo exists $tmp] == 1 } { wm deiconify $tmp; return }
    toplevel $tmp -class vTcl
    wm resizable $tmp 1 0
    wm minsize $tmp 210 0
    wm title $tmp "Widget Info"
    frame $tmp.f1 -relief flat -bd 0
    frame $tmp.f2 -relief flat -bd 0
    frame $tmp.f3 -relief flat -bd 0
    frame $tmp.f4 -relief flat -bd 0
    pack $tmp.f1 $tmp.f2 $tmp.f3 $tmp.f4 -side top -expand 1 -fill both

    label $tmp.f1.l -text "Widget" -width 8 -relief groove -bd 2 -anchor w
    entry $tmp.f1.e \
        -textvar vTcl(w,widget) -width 8 \
        -relief sunken -bd 1 -state disabled
    pack $tmp.f1.l -side left
    pack $tmp.f1.e -side left -expand 1 -fill x

    label $tmp.f2.l -text "Insert" -width 8 -relief groove -bd 2 -anchor w
    entry $tmp.f2.e \
        -textvar vTcl(w,insert) -width 8 \
        -relief sunken -bd 1 -state disabled
    pack $tmp.f2.l -side left
    pack $tmp.f2.e -side left -expand 1 -fill x

    label $tmp.f3.l -text "Class" -width 8 -relief groove -bd 2 -anchor w
    entry $tmp.f3.e \
        -textvar vTcl(w,class) -width 8 \
        -relief sunken -bd 1 -state disabled
    pack $tmp.f3.l -side left
    pack $tmp.f3.e -side left -expand 1 -fill x

    label $tmp.f4.l -text "Alias" -width 8 -relief groove -bd 2 -anchor w
    entry $tmp.f4.e \
        -textvar vTcl(w,alias) -width 8 \
        -relief sunken -bd 1 -state disabled
    pack $tmp.f4.l -side left
    pack $tmp.f4.e -side left -expand 1 -fill x

	vTcl:setup_vTcl:bind $tmp
}

