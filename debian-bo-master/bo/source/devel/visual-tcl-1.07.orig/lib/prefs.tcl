##############################################################################
#
# prefs.tcl - procedures for editing application preferences
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

proc vTclWindow.vTcl.prefs {args} {
    set base .vTcl.prefs
    if {[winfo exists .vTcl.prefs]} {
        wm deiconify .vTcl.prefs; return
    }
    toplevel .vTcl.prefs -class vTcl
    wm focusmodel .vTcl.prefs passive
#    wm geometry .vTcl.prefs 250x189+239+285
    wm resizable .vTcl.prefs 0 0
    wm title .vTcl.prefs "Preferences"
    frame .vTcl.prefs.fra20 \
        -borderwidth 1 -height 6 -relief sunken -width 30 
    label .vTcl.prefs.fra20.lab23 \
        -borderwidth 1 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -relief raised -text {The Basics} 
    checkbutton .vTcl.prefs.fra20.che24 \
        -anchor w -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -text {Ask for Widget name on insert} \
        -variable vTcl(pr,getname) 
    checkbutton .vTcl.prefs.fra20.che25 \
        -anchor w -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -text {Short automatic widget names} \
        -variable vTcl(pr,shortname) 
    checkbutton .vTcl.prefs.fra20.che26 \
        -anchor w -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -text {Save verbose widget configuration} \
        -variable vTcl(pr,fullcfg) 
    label .vTcl.prefs.fra20.lab22 \
        -borderwidth 1 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -relief raised -text {Default Geometry Manager} 
    frame .vTcl.prefs.fra20.fra17 \
        -borderwidth 1 -height 30 -width 30 
    radiobutton .vTcl.prefs.fra20.fra17.rad24 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* -text Grid \
        -value grid -variable vTcl(pr,manager) -width 5 
    radiobutton .vTcl.prefs.fra20.fra17.rad25 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* -text Pack \
        -value pack -variable vTcl(pr,manager) -width 5 
    radiobutton .vTcl.prefs.fra20.fra17.rad26 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -text Place -value place -variable vTcl(pr,manager) -width 5 
    frame .vTcl.prefs.fra22 \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    button .vTcl.prefs.fra22.but23 \
        -command {wm withdraw .vTcl.prefs} \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -padx 9 -pady 3 -text OK 
    pack .vTcl.prefs.fra20 \
        -anchor center -expand 1 -fill both -padx 5 -pady 5 -side top 
    pack .vTcl.prefs.fra20.lab23 \
        -anchor center -expand 0 -fill x -padx 2 -pady 2 -side top 
    pack .vTcl.prefs.fra20.che24 \
        -anchor center -expand 0 -fill x -padx 1 -pady 1 -side top 
    pack .vTcl.prefs.fra20.che25 \
        -anchor center -expand 0 -fill x -padx 1 -pady 1 -side top 
    pack .vTcl.prefs.fra20.che26 \
        -anchor center -expand 0 -fill x -padx 1 -pady 1 -side top 
    pack .vTcl.prefs.fra20.lab22 \
        -anchor center -expand 0 -fill x -padx 2 -pady 2 -side top 
    pack .vTcl.prefs.fra20.fra17 \
        -anchor center -expand 1 -fill both -side top 
    pack .vTcl.prefs.fra20.fra17.rad24 \
        -anchor center -expand 1 -fill none -side left 
    pack .vTcl.prefs.fra20.fra17.rad25 \
        -anchor center -expand 1 -fill none -side left 
    pack .vTcl.prefs.fra20.fra17.rad26 \
        -anchor center -expand 1 -fill none -side left 
    pack .vTcl.prefs.fra22 \
        -anchor center -expand 0 -fill both -padx 5 -pady 5 -side top 
    pack .vTcl.prefs.fra22.but23 \
        -anchor center -expand 0 -fill both -padx 2 -pady 2 -side top 
}

