##############################################################################
#
# console.tcl - console procedures
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

proc vTcl:show_console {} {
    global vTcl tcl_platform
    if $vTcl(console) {
        console show
    } else {
        Window show .vTcl.con
    }
}

proc vTclWindow.vTcl.con {args} {
    set base .vTcl.con
    if {[winfo exists .vTcl.con]} {
        wm deiconify .vTcl.con; return
    }
    toplevel .vTcl.con -class vTcl
    wm minsize .vTcl.con 375 80
    wm title .vTcl.con "Visual Tcl Console"
    frame .vTcl.con.fra5 \
        -height 30 -width 30 
    pack .vTcl.con.fra5 \
        -anchor center -expand 1 -fill both -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side top 
    text .vTcl.con.fra5.tex7 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* -height 6 \
        -highlightthickness 0 -state disabled -width 50 \
        -yscrollcommand {.vTcl.con.fra5.scr8 set} 
    pack .vTcl.con.fra5.tex7 \
        -anchor center -expand 1 -fill both -ipadx 0 -ipady 0 -padx 0 -pady 0 \
        -side left 
    scrollbar .vTcl.con.fra5.scr8 \
        -command {.vTcl.con.fra5.tex7 yview} -highlightthickness 0
    pack .vTcl.con.fra5.scr8 \
        -anchor center -expand 0 -fill y -ipadx 0 -ipady 0 -padx 0 -pady 0 \
        -side right 
    frame .vTcl.con.fra6 \
        -height 30 -width 30 
    pack .vTcl.con.fra6 \
        -anchor center -expand 0 -fill both -ipadx 0 -ipady 0 -padx 0 -pady 0 \
        -side top 
    entry .vTcl.con.fra6.ent10 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 
    pack .vTcl.con.fra6.ent10 \
        -anchor center -expand 0 -fill x -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side top 
    bind .vTcl.con.fra6.ent10 <Key-Return> {
        .vTcl.con.fra5.tex7 insert end "\n[.vTcl.con.fra6.ent10 get]"
        if { [catch [.vTcl.con.fra6.ent10 get] vTcl(err)] == 1 } {
            .vTcl.con.fra5.tex7 conf -state normal
            .vTcl.con.fra5.tex7 insert end "\n$vTcl(err)"
            .vTcl.con.fra5.tex7 conf -state disabled
            .vTcl.con.fra5.tex7 yview end
        }
        .vTcl.con.fra6.ent10 delete 0 end
    }
}

