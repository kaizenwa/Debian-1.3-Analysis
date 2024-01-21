##############################################################################
#
# compounds.tcl - bundled system compound widgets
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

set vTcl(syscmpd,list) "{Menu Bar} {Scrollable Text} {Scrollable Listbox} {Label and Entry} {Scrollable Canvas}"

set {vTcl(syscmpd:Menu Bar)} {{frame {-borderwidth 1 -height 30 -relief sunken -width 30} {pack {-anchor center -expand 0 -fill x -ipadx 0 -ipady 0 -padx 0 -pady 0 -side top}} {} {} {{menubutton {-anchor w  -menu .top1.fra3.men4.m -padx 4 -pady 3 -text File -width 4} {pack {-anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 0 -pady 0 -side left}} {} {} {{menu { -tearoff 0} {{} {}} {} {{command {-accelerator Ctrl-O -label Open}} {command {-accelerator Ctrl-W -label Close}}} {} .01.02 {}} } .01 {}} {menubutton {-anchor w  -menu .top1.fra3.men5.01 -padx 4 -pady 3 -text Edit -width 4} {pack {-anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 0 -pady 0 -side left}} {} {} {{menu { -tearoff 0} {{} {}} {} {} {} .03.04 {}} } .03 {}} {menubutton {-anchor w  -menu .top1.fra3.men6.01 -padx 4 -pady 3 -text Help -width 4} {pack {-anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 0 -pady 0 -side right}} {} {} {{menu { -tearoff 0} {{} {}} {} {} {} .05.06 {}} } .05 {}} } {} {}} {{.top1.fra3.men4.m .01.02} {.top1.fra3.men4 .01} {.top1.fra3.men5.01 .03.04} {.top1.fra3.men5 .03} {.top1.fra3.men6.01 .05.06} {.top1.fra3.men6 .05} {.top1.fra3 }}}

set {vTcl(syscmpd:Label and Entry)} {{frame {-borderwidth 1 -height 30 -relief raised -width 30} {place {-x 10 -relx 0 -y 10 -rely 0 -width 245 -relwidth {} -height 25 -relheight {} -anchor nw -bordermode ignore}} {} {} {{label {-anchor w  -relief groove -text label} {pack {-anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 2 -pady 2 -side left}} {} {} {} .01 {}} {entry {-cursor {}  -highlightthickness 0} {pack {-anchor center -expand 1 -fill x -ipadx 0 -ipady 0 -padx 2 -pady 2 -side right}} {} {} {} .02 {}} } {} {}} {{.top1.fra7.lab8 .01} {.top1.fra7.ent9 .02} {.top1.fra7 }}}

set {vTcl(syscmpd:Scrollable Listbox)} {{frame {-borderwidth 1 -height 30 -relief raised -width 30} {place {-x 5 -y 5 -width 135 -height 125 -anchor nw -bordermode ignore}} {} {} {{listbox {-font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* -xscrollcommand {.top16.fra17.scr19 set} -yscrollcommand {.top16.fra17.scr20 set}} {grid {-column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw}} {} {} {} .01 {} {} {}} {scrollbar {-borderwidth 1 -command {.top16.fra17.lis18 xview} -orient horiz -width 10} {grid {-column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew}} {} {} {} .02 {} {} {}} {scrollbar {-borderwidth 1 -command {.top16.fra17.lis18 yview} -orient vert -width 10} {grid {-column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns}} {} {} {} .03 {} {} {}} } {} {} {{columnconf 0 -weight 1} {rowconf 0 -weight 1}} {}} {{.top16.fra17.lis18 .01} {.top16.fra17.scr19 .02} {.top16.fra17.scr20 .03} {.top16.fra17 }}}

set {vTcl(syscmpd:Scrollable Text)} {{frame {-borderwidth 1 -height 30 -relief raised -width 30} {place {-x 5 -y 5 -width 143 -height 126 -anchor nw}} {} {} {{scrollbar {-borderwidth 1 -command {.top16.cpd27.03 xview} -orient horiz -width 10} {grid {-column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew}} {} {} {} .01 {} {} {}} {scrollbar {-borderwidth 1 -command {.top16.cpd27.03 yview} -orient vert -width 10} {grid {-column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns}} {} {} {} .02 {} {} {}} {text {-font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* -height 1 -width 8 -xscrollcommand {.top16.cpd27.01 set} -yscrollcommand {.top16.cpd27.02 set}} {grid {-column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw}} {} {} {} .03 {} {} {}} } {} {} {{columnconf 0 -weight 1} {rowconf 0 -weight 1}} {}} {{.top16.cpd27.01 .01} {.top16.cpd27.02 .02} {.top16.cpd27.03 .03} {.top16.cpd27 }}}

set {vTcl(syscmpd:Scrollable Canvas)} {{frame {-borderwidth 1 -height 30 -relief raised -width 30} {place {-x 5 -y 5 -width 143 -height 126 -anchor nw}} {} {} {{scrollbar {-borderwidth 1 -command {.top16.fra29.can30 xview} -orient horiz -width 10} {grid {-column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew}} {} {} {} .01 {} {} {}} {scrollbar {-borderwidth 1 -command {.top16.fra29.can30 yview} -orient vert -width 10} {grid {-column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns}} {} {} {} .02 {} {} {}} {canvas {-borderwidth 2 -height 100 -relief ridge -width 100 -xscrollcommand {.top16.fra29.01 set} -yscrollcommand {.top16.fra29.02 set}} {grid {-column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw}} {} {} {} .03 {} {} {}} } {} {} {{columnconf 0 -weight 1} {rowconf 0 -weight 1}} {}} {{.top16.fra29.01 .01} {.top16.fra29.02 .02} {.top16.fra29.can30 .03} {.top16.fra29 }}}

