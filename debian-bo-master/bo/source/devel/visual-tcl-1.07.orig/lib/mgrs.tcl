##############################################################################
#
# mgrs.tcl - procedures used by the Geometry Information window
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

proc vTclWindow(post).vTcl.mgr {args} {
global vTcl
    wm withdraw .vTcl.mgr
    vTcl:setup_vTcl:bind .vTcl.mgr
    catch {wm geometry .vTcl.mgr $vTcl(geometry,.vTcl.mgr)}
    update idletasks
    wm deiconify .vTcl.mgr
    vTcl:show_mgr $vTcl(w,manager)
}

proc vTcl:widget_gridconf {rc opt} {
global vTcl
    set p [winfo parent $vTcl(w,widget)]
    set name [string range $rc 0 2]
    if {$rc == "column"} {
        set num $vTcl(w,grid,-column)
    } else {
        set num $vTcl(w,grid,-row)
    }
    if {$opt == "weight"} {
        grid ${rc}configure $p $num -weight \
            [expr round($vTcl(w,grid,$name,$opt))]
    } else {
        grid ${rc}configure $p $num -minsize \
            [expr round($vTcl(w,grid,$name,$opt))]
    }
    vTcl:manager_update grid
}

proc vTcl:manager_update {mgr} {
global vTcl
    if {$mgr == ""} {return}
    set options ""
    if {$vTcl(w,manager) != "$mgr"} {return}
    update idletasks
    if {$mgr != "wm" } {
        if {$mgr == "grid"} {
            vTcl:set_grid_stickies
        }
        foreach i $vTcl($mgr,attr) {
            set value $vTcl(w,$mgr,-$i)
            if { $value == "" } { set value {{}} }
            append options "-$i $value "
        }
        set vTcl(var_update) "no"
        set undo [vTcl:dump_widget_quick $vTcl(w,widget)]
        set do "$mgr configure $vTcl(w,widget) $options"
        vTcl:push_action $do $undo
        set vTcl(var_update) "yes"
    } else {
        set    vTcl(w,wm,geometry) \
            "$vTcl(w,wm,geometry,w)x$vTcl(w,wm,geometry,h)"
        append vTcl(w,wm,geometry) \
            "+$vTcl(w,wm,geometry,x)+$vTcl(w,wm,geometry,y)"
        set    vTcl(w,wm,minsize) \
            "$vTcl(w,wm,minsize,x) $vTcl(w,wm,minsize,y)"
        set    vTcl(w,wm,maxsize) \
            "$vTcl(w,wm,maxsize,x) $vTcl(w,wm,maxsize,y)"
        set    vTcl(w,wm,aspect) \
            "$vTcl(w,wm,aspect,minnum) $vTcl(w,wm,aspect,minden)"
        append vTcl(w,wm,aspect) \
            "+$vTcl(w,wm,aspect,maxnum)+$vTcl(w,wm,aspect,maxden)"
        set    vTcl(w,wm,resizable) \
            "$vTcl(w,wm,resizable,w) $vTcl(w,wm,resizable,h)"
#            set    do "$mgr geometry $vTcl(w,widget) $vTcl(w,wm,geometry); "
        append do "$mgr minsize $vTcl(w,widget) $vTcl(w,wm,minsize); "
        append do "$mgr maxsize $vTcl(w,widget) $vTcl(w,wm,maxsize); "
        append do "$mgr focusmodel $vTcl(w,widget) $vTcl(w,wm,focusmodel);"
        append do "$mgr resizable $vTcl(w,widget) $vTcl(w,wm,resizable); "
        append do "$mgr title $vTcl(w,widget) \"$vTcl(w,wm,title)\"; "
        switch $vTcl(w,wm,state) {
            withdrawn { append do "$mgr withdraw $vTcl(w,widget); " }
            iconic { append do "$mgr iconify $vTcl(w,widget); " }
            normal { append do "$mgr deiconify $vTcl(w,widget); " }
        }
        eval $do
        vTcl:wm_button_update
    }
    vTcl:place_handles $vTcl(w,widget)
    vTcl:update_top_list
}

proc vTcl:pack_after {target} {
if {[winfo manager $target] != "pack" || $target == "."} {return}
    set l [pack slaves [winfo parent $target]]
    set i [lsearch $l $target]
    set n [lindex $l [expr $i + 1]]
    if {$n != ""} {
        pack conf $target -after $n
    }
    vTcl:place_handles $target
}

proc vTcl:pack_before {target} {
if {[winfo manager $target] != "pack" || $target == "."} {return}
    set l [pack slaves [winfo parent $target]]
    set i [lsearch $l $target]
    set n [lindex $l [expr $i - 1]]
    if {$n != ""} {
        pack conf $target -before $n
    }
    vTcl:place_handles $target
}

proc vTcl:set_grid_stickies {} {
global vTcl
    set vTcl(w,grid,-sticky) ""
    foreach i {n s e w} {
        append vTcl(w,grid,-sticky) $vTcl(grid,sticky,$i)
    }
}

proc vTcl:show_mgr {mgr} {
global vTcl
    update idletasks
    if {[winfo exists .vTcl.mgr] == 0 || $vTcl(w,class) == "Menu"} {return}
    foreach i $vTcl(w,mgrs) {
        if { $i == $mgr } {
            pack .vTcl.mgr.xx.$i -expand 1 -fill both
            .vTcl.mgr.x.$i configure -bg yellow
        } else {
            pack forget .vTcl.mgr.xx.$i
            .vTcl.mgr.x.$i configure -bg gray
        }
    }
    if {$mgr == "wm"} {vTcl:wm_button_update}
}

proc vTcl:wm_button_switch {wh widget} {
global vTcl
    if {[winfo exists .vTcl.mgr] == 1} {
        switch $vTcl(w,wm,resizable,$wh) {
            1 {
                set vTcl(w,wm,resizable,$wh) 0
            }
            0 {
                set vTcl(w,wm,resizable,$wh) 1
            }
        }
        vTcl:wm_button_update
    }
}

proc vTcl:wm_button_update {} {
global vTcl
    set w .vTcl.mgr.xx.wm.fra1.but6
    set h .vTcl.mgr.xx.wm.fra1.but7
    if {$vTcl(w,widget) == ""} {return}
    if {[winfo exists .vTcl.mgr] == 1} {
        if {$vTcl(w,wm,resizable,h)} {
            $h conf -relief raised -bg gray
        } else {
            $h conf -relief sunken -bg #ed7291
        }
        if {$vTcl(w,wm,resizable,w)} {
            $w conf -relief raised -bg gray
        } else {
            $w conf -relief sunken -bg #ed7291
        }
        wm resizable $vTcl(w,widget) \
            $vTcl(w,wm,resizable,w) $vTcl(w,wm,resizable,h)
    }
}

proc vTclWindow.vTcl.mgr {args} {
    set base .vTcl.mgr
    if {[winfo exists .vTcl.mgr]} {
        wm deiconify .vTcl.mgr; return
    }
    toplevel .vTcl.mgr -class vTcl
    wm focusmodel .vTcl.mgr passive
    wm geometry .vTcl.mgr 236x271+412+141
    wm maxsize .vTcl.mgr 1137 870
    wm minsize .vTcl.mgr 1 1
    wm overrideredirect .vTcl.mgr 0
    wm resizable .vTcl.mgr 0 0
    wm deiconify .vTcl.mgr
    wm title .vTcl.mgr "Geometry Information"
    bind .vTcl.mgr <Key-Return> {
        vTcl:manager_update $vTcl(w,manager)
    }
    frame .vTcl.mgr.x \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    pack .vTcl.mgr.x \
        -anchor center -expand 0 -fill x -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side top 
    button .vTcl.mgr.x.grid \
        -background gray \
        -highlightthickness 0 -padx 9 -pady 1 -text Grid -width 4 
    pack .vTcl.mgr.x.grid \
        -anchor center -expand 1 -fill x -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    button .vTcl.mgr.x.pack \
        -background gray \
        -highlightthickness 0 -padx 9 -pady 1 -text Pack -width 4 
    pack .vTcl.mgr.x.pack \
        -anchor center -expand 1 -fill x -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    button .vTcl.mgr.x.place \
        -background gray \
        -highlightthickness 0 -padx 9 -pady 1 -text Place -width 4 
    pack .vTcl.mgr.x.place \
        -anchor center -expand 1 -fill x -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    button .vTcl.mgr.x.wm \
        -background yellow \
        -highlightthickness 0 -padx 9 -pady 1 -text Window -width 4 
    pack .vTcl.mgr.x.wm \
        -anchor center -expand 1 -fill x -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    frame .vTcl.mgr.xx \
        -borderwidth 1 -relief sunken 
    pack .vTcl.mgr.xx \
        -anchor center -expand 1 -fill both -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side top 
    frame .vTcl.mgr.xx.grid \
        -height 30 -width 30 
    frame .vTcl.mgr.xx.grid.fra3 \
        -borderwidth 1 -height 30 -width 30 
    place .vTcl.mgr.xx.grid.fra3 \
        -x 0 -relx 0 -y 0 -rely 0 -width 170 -height 65 -anchor nw \
        -bordermode ignore 
    label .vTcl.mgr.xx.grid.fra3.lab4 \
        -anchor w  \
        -relief groove -text {Internal Pad} -width 11 
    grid .vTcl.mgr.xx.grid.fra3.lab4 \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 2 \
        -pady 0 
    label .vTcl.mgr.xx.grid.fra3.lab5 \
        -anchor w  \
        -relief groove -text {External Pad} -width 11 
    grid .vTcl.mgr.xx.grid.fra3.lab5 \
        -column 0 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 2 \
        -pady 0 
    label .vTcl.mgr.xx.grid.fra3.lab6 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-100-*-*-*-*-*-* -text x 
    grid .vTcl.mgr.xx.grid.fra3.lab6 \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 0 \
        -pady 0 
    label .vTcl.mgr.xx.grid.fra3.lab7 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-100-*-*-*-*-*-* -text y 
    grid .vTcl.mgr.xx.grid.fra3.lab7 \
        -column 2 -row 0 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.grid.fra3.ent8 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,grid,-ipadx) -width 4 
    grid .vTcl.mgr.xx.grid.fra3.ent8 \
        -column 1 -row 1 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.grid.fra3.ent9 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,grid,-ipady) -width 4 
    grid .vTcl.mgr.xx.grid.fra3.ent9 \
        -column 2 -row 1 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.grid.fra3.ent10 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,grid,-padx) -width 4 
    grid .vTcl.mgr.xx.grid.fra3.ent10 \
        -column 1 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.grid.fra3.ent11 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,grid,-pady) -width 4 
    grid .vTcl.mgr.xx.grid.fra3.ent11 \
        -column 2 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    frame .vTcl.mgr.xx.grid.fra13 \
        -borderwidth 1 -height 30 -width 30 
    place .vTcl.mgr.xx.grid.fra13 \
        -x 0 -relx 0 -y 65 -rely 0 -width 235 -height 65 -anchor nw \
        -bordermode ignore 
    label .vTcl.mgr.xx.grid.fra13.lab14 \
        -anchor w  \
        -relief groove -text Absolute -width 11 
    grid .vTcl.mgr.xx.grid.fra13.lab14 \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 2 \
        -pady 0 
    label .vTcl.mgr.xx.grid.fra13.lab15 \
        -anchor w  \
        -relief groove -text {Weight / Min} -width 11 
    grid .vTcl.mgr.xx.grid.fra13.lab15 \
        -column 0 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 2 \
        -pady 0 
    label .vTcl.mgr.xx.grid.fra13.lab16 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-100-*-*-*-*-*-* -text col 
    grid .vTcl.mgr.xx.grid.fra13.lab16 \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 0 \
        -pady 0 
    label .vTcl.mgr.xx.grid.fra13.lab17 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-100-*-*-*-*-*-* -text row 
    grid .vTcl.mgr.xx.grid.fra13.lab17 \
        -column 2 -row 0 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 0 \
        -pady 0 
    label .vTcl.mgr.xx.grid.fra13.lab18 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-100-*-*-*-*-*-* \
        -text width 
    grid .vTcl.mgr.xx.grid.fra13.lab18 \
        -column 3 -row 0 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 0 \
        -pady 0 
    label .vTcl.mgr.xx.grid.fra13.lab19 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-100-*-*-*-*-*-* \
        -text height 
    grid .vTcl.mgr.xx.grid.fra13.lab19 \
        -column 4 -row 0 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.grid.fra13.ent20 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,grid,-column) -width 4 
    grid .vTcl.mgr.xx.grid.fra13.ent20 \
        -column 1 -row 1 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.grid.fra13.ent21 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,grid,-row) -width 4 
    grid .vTcl.mgr.xx.grid.fra13.ent21 \
        -column 2 -row 1 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.grid.fra13.ent22 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,grid,-columnspan) -width 4 
    grid .vTcl.mgr.xx.grid.fra13.ent22 \
        -column 3 -row 1 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.grid.fra13.ent23 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,grid,-rowspan) -width 4 
    grid .vTcl.mgr.xx.grid.fra13.ent23 \
        -column 4 -row 1 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.grid.fra13.ent24 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,grid,col,weight) -width 4 
    bind .vTcl.mgr.xx.grid.fra13.ent24 <Return> {
        vTcl:widget_gridconf column weight
    }
    grid .vTcl.mgr.xx.grid.fra13.ent24 \
        -column 1 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.grid.fra13.ent25 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,grid,row,weight) -width 4 
    bind .vTcl.mgr.xx.grid.fra13.ent25 <Return> {
        vTcl:widget_gridconf row weight
    }
    grid .vTcl.mgr.xx.grid.fra13.ent25 \
        -column 2 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.grid.fra13.ent26 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,grid,col,width) -width 4 
    bind .vTcl.mgr.xx.grid.fra13.ent26 <Return> {
        vTcl:widget_gridconf column width
    }
    grid .vTcl.mgr.xx.grid.fra13.ent26 \
        -column 3 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.grid.fra13.ent27 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,grid,row,width) -width 4 
    bind .vTcl.mgr.xx.grid.fra13.ent27 <Return> {
        vTcl:widget_gridconf row width
    }
    grid .vTcl.mgr.xx.grid.fra13.ent27 \
        -column 4 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    label .vTcl.mgr.xx.grid.lab29 \
        -borderwidth 1 \
        -relief raised -text Sticky 
    place .vTcl.mgr.xx.grid.lab29 \
        -x 90 -relx 0 -y 155 -rely 0 -width 50 -height 20 -anchor nw \
        -bordermode ignore 
    checkbutton .vTcl.mgr.xx.grid.che1 \
        -borderwidth 1 -command {vTcl:manager_update grid} \
        -highlightthickness 0 -indicatoron 0 -offvalue {} -onvalue n \
        -relief raised -selectcolor #ed7291 -text north \
        -variable vTcl(grid,sticky,n) 
    place .vTcl.mgr.xx.grid.che1 \
        -x 90 -relx 0 -y 134 -rely 0 -width 50 -height 20 -anchor nw \
        -bordermode ignore 
    checkbutton .vTcl.mgr.xx.grid.che3 \
        -borderwidth 1 -command {vTcl:manager_update grid} \
        -highlightthickness 0 -indicatoron 0 -offvalue {} -onvalue s \
        -relief raised -selectcolor #ed7291 -text south \
        -variable vTcl(grid,sticky,s) 
    place .vTcl.mgr.xx.grid.che3 \
        -x 90 -relx 0 -y 176 -rely 0 -width 50 -height 20 -anchor nw \
        -bordermode ignore 
    checkbutton .vTcl.mgr.xx.grid.che4 \
        -borderwidth 1 -command {vTcl:manager_update grid} \
        -highlightthickness 0 -indicatoron 0 -offvalue {} -onvalue e \
        -relief raised -selectcolor #ed7291 -text east \
        -variable vTcl(grid,sticky,e) 
    place .vTcl.mgr.xx.grid.che4 \
        -x 142 -relx 0 -y 155 -rely 0 -width 50 -height 20 -anchor nw \
        -bordermode ignore 
    checkbutton .vTcl.mgr.xx.grid.che5 \
        -borderwidth 1 -command {vTcl:manager_update grid} \
        -highlightthickness 0 -indicatoron 0 -offvalue {} -onvalue w \
        -relief raised -selectcolor #ed7291 -text west \
        -variable vTcl(grid,sticky,w) 
    place .vTcl.mgr.xx.grid.che5 \
        -x 38 -relx 0 -y 155 -rely 0 -width 50 -height 20 -anchor nw \
        -bordermode ignore 
    frame .vTcl.mgr.xx.pack \
        -height 30 -width 30 
    frame .vTcl.mgr.xx.pack.fra1 \
        -borderwidth 1 -height 30 -width 30 
    place .vTcl.mgr.xx.pack.fra1 \
        -x 0 -relx 0 -y 0 -rely 0 -width 175 -height 65 -anchor nw \
        -bordermode ignore 
    label .vTcl.mgr.xx.pack.fra1.lab2 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-100-*-*-*-*-*-* -text x 
    grid .vTcl.mgr.xx.pack.fra1.lab2 \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 0 \
        -pady 0 
    label .vTcl.mgr.xx.pack.fra1.lab3 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-100-*-*-*-*-*-* -text y 
    grid .vTcl.mgr.xx.pack.fra1.lab3 \
        -column 2 -row 0 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 0 \
        -pady 0 
    label .vTcl.mgr.xx.pack.fra1.lab4 \
        -anchor w  \
        -relief groove -text {Internal Pad} -width 11 
    grid .vTcl.mgr.xx.pack.fra1.lab4 \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 2 \
        -pady 0 
    label .vTcl.mgr.xx.pack.fra1.lab5 \
        -anchor w  \
        -relief groove -text {External Pad} -width 11 
    grid .vTcl.mgr.xx.pack.fra1.lab5 \
        -column 0 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 2 \
        -pady 0 
    entry .vTcl.mgr.xx.pack.fra1.ent6 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,pack,-ipadx) -width 4 
    grid .vTcl.mgr.xx.pack.fra1.ent6 \
        -column 1 -row 1 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.pack.fra1.ent7 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,pack,-ipady) -width 4 
    grid .vTcl.mgr.xx.pack.fra1.ent7 \
        -column 2 -row 1 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.pack.fra1.ent8 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,pack,-padx) -width 4 
    grid .vTcl.mgr.xx.pack.fra1.ent8 \
        -column 1 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.pack.fra1.ent9 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,pack,-pady) -width 4 
    grid .vTcl.mgr.xx.pack.fra1.ent9 \
        -column 2 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    checkbutton .vTcl.mgr.xx.pack.che10 \
        -command {vTcl:manager_update pack} \
        -highlightthickness 0 -text {Expand widget to fill space} \
        -variable vTcl(w,pack,-expand) 
    place .vTcl.mgr.xx.pack.che10 \
        -x 35 -relx 0 -y 70 -rely 0 -width 173 -height 20 -anchor nw \
        -bordermode ignore 
    frame .vTcl.mgr.xx.pack.fra11 \
        -borderwidth 1 -height 30 -width 30 
    place .vTcl.mgr.xx.pack.fra11 \
        -x 0 -relx 0 -y 95 -rely 0 -width 175 -height 70 -anchor nw \
        -bordermode ignore 
    label .vTcl.mgr.xx.pack.fra11.lab16 \
        -anchor w  \
        -relief groove -text {X/Y Fill} -width 11 
    grid .vTcl.mgr.xx.pack.fra11.lab16 \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 2 \
        -pady 0 
    label .vTcl.mgr.xx.pack.fra11.lab17 \
        -anchor w  \
        -relief groove -text Anchor -width 11 
    grid .vTcl.mgr.xx.pack.fra11.lab17 \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 2 \
        -pady 0 
    label .vTcl.mgr.xx.pack.fra11.lab18 \
        -anchor w  \
        -relief groove -text {Pack Side} -width 11 
    grid .vTcl.mgr.xx.pack.fra11.lab18 \
        -column 0 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 2 \
        -pady 0 
    menubutton .vTcl.mgr.xx.pack.fra11.men19 \
        -borderwidth 1 \
        -menu .vTcl.mgr.xx.pack.fra11.men19.m -padx 4 -pady 2 -relief raised \
        -text both -textvariable vTcl(w,pack,-fill) -width 8 
    grid .vTcl.mgr.xx.pack.fra11.men19 \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 0 \
        -pady 0 
    menu .vTcl.mgr.xx.pack.fra11.men19.m \
         -tearoff 0 
    .vTcl.mgr.xx.pack.fra11.men19.m add command \
        -command { set vTcl(w,pack,-fill) x; vTcl:manager_update pack } \
        -label x 
    .vTcl.mgr.xx.pack.fra11.men19.m add command \
        -command { set vTcl(w,pack,-fill) y; vTcl:manager_update pack } \
        -label y 
    .vTcl.mgr.xx.pack.fra11.men19.m add command \
        -command { set vTcl(w,pack,-fill) both; vTcl:manager_update pack } \
        -label both 
    .vTcl.mgr.xx.pack.fra11.men19.m add command \
        -command { set vTcl(w,pack,-fill) none; vTcl:manager_update pack } \
        -label none 
    menubutton .vTcl.mgr.xx.pack.fra11.men20 \
        -borderwidth 1 \
        -menu .vTcl.mgr.xx.pack.fra11.men20.m -padx 4 -pady 2 -relief raised \
        -text center -textvariable vTcl(w,pack,-anchor) -width 8 
    grid .vTcl.mgr.xx.pack.fra11.men20 \
        -column 1 -row 1 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 0 \
        -pady 0 
    menu .vTcl.mgr.xx.pack.fra11.men20.m \
         -tearoff 0 
    .vTcl.mgr.xx.pack.fra11.men20.m add command \
        -command { set vTcl(w,pack,-anchor) center; vTcl:manager_update pack } \
        -label center 
    .vTcl.mgr.xx.pack.fra11.men20.m add command \
        -command { set vTcl(w,pack,-anchor) n; vTcl:manager_update pack } \
        -label n 
    .vTcl.mgr.xx.pack.fra11.men20.m add command \
        -command { set vTcl(w,pack,-anchor) s; vTcl:manager_update pack } \
        -label s 
    .vTcl.mgr.xx.pack.fra11.men20.m add command \
        -command { set vTcl(w,pack,-anchor) e; vTcl:manager_update pack } \
        -label e 
    .vTcl.mgr.xx.pack.fra11.men20.m add command \
        -command { set vTcl(w,pack,-anchor) w; vTcl:manager_update pack } \
        -label w 
    .vTcl.mgr.xx.pack.fra11.men20.m add command \
        -command { set vTcl(w,pack,-anchor) nw; vTcl:manager_update pack } \
        -label nw 
    .vTcl.mgr.xx.pack.fra11.men20.m add command \
        -command { set vTcl(w,pack,-anchor) ne; vTcl:manager_update pack } \
        -label ne 
    .vTcl.mgr.xx.pack.fra11.men20.m add command \
        -command { set vTcl(w,pack,-anchor) sw; vTcl:manager_update pack } \
        -label sw 
    .vTcl.mgr.xx.pack.fra11.men20.m add command \
        -command { set vTcl(w,pack,-anchor) se; vTcl:manager_update pack } \
        -label se 
    menubutton .vTcl.mgr.xx.pack.fra11.men21 \
        -borderwidth 1 \
        -menu .vTcl.mgr.xx.pack.fra11.men21.m -padx 4 -pady 2 -relief raised \
        -text top -textvariable vTcl(w,pack,-side) -width 8 
    grid .vTcl.mgr.xx.pack.fra11.men21 \
        -column 1 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 0 \
        -pady 0 
    menu .vTcl.mgr.xx.pack.fra11.men21.m \
         -tearoff 0 
    .vTcl.mgr.xx.pack.fra11.men21.m add command \
        -command { set vTcl(w,pack,-side) top; vTcl:manager_update pack } \
        -label top 
    .vTcl.mgr.xx.pack.fra11.men21.m add command \
        -command { set vTcl(w,pack,-side) left; vTcl:manager_update pack } \
        -label left 
    .vTcl.mgr.xx.pack.fra11.men21.m add command \
        -command { set vTcl(w,pack,-side) right; vTcl:manager_update pack } \
        -label right 
    .vTcl.mgr.xx.pack.fra11.men21.m add command \
        -command { set vTcl(w,pack,-side) bottom; vTcl:manager_update pack } \
        -label bottom 
    button .vTcl.mgr.xx.pack.but6 \
        -borderwidth 1 -command {vTcl:pack_before $vTcl(w,widget)} \
        -highlightthickness 0 -padx 9 -pady 3 -text {<< Pack Before} 
    place .vTcl.mgr.xx.pack.but6 \
        -x 11 -relx 0 -y 171 -rely 0 -width 101 -height 20 -anchor nw \
        -bordermode ignore 
    button .vTcl.mgr.xx.pack.but7 \
        -borderwidth 1 -command {vTcl:pack_after $vTcl(w,widget)} \
        -highlightthickness 0 -padx 9 -pady 3 -text {Pack After >>} 
    place .vTcl.mgr.xx.pack.but7 \
        -x 118 -relx 0 -y 171 -rely 0 -width 101 -height 20 -anchor nw \
        -bordermode ignore 
    frame .vTcl.mgr.xx.place \
        -height 30 -width 30 
    frame .vTcl.mgr.xx.place.fra1 \
        -borderwidth 1 -height 30 -width 30 
    place .vTcl.mgr.xx.place.fra1 \
        -x 0 -relx 0 -y 0 -rely 0 -width 215 -height 65 -anchor nw \
        -bordermode ignore 
    label .vTcl.mgr.xx.place.fra1.lab2 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-100-*-*-*-*-*-* -text x 
    grid .vTcl.mgr.xx.place.fra1.lab2 \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 0 \
        -pady 0 
    label .vTcl.mgr.xx.place.fra1.lab3 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-100-*-*-*-*-*-* -text y 
    grid .vTcl.mgr.xx.place.fra1.lab3 \
        -column 2 -row 0 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 0 \
        -pady 0 
    label .vTcl.mgr.xx.place.fra1.lab4 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-100-*-*-*-*-*-* \
        -text width 
    grid .vTcl.mgr.xx.place.fra1.lab4 \
        -column 3 -row 0 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 0 \
        -pady 0 
    label .vTcl.mgr.xx.place.fra1.lab5 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-100-*-*-*-*-*-* \
        -text height 
    grid .vTcl.mgr.xx.place.fra1.lab5 \
        -column 4 -row 0 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 0 \
        -pady 0 
    label .vTcl.mgr.xx.place.fra1.lab6 \
        -anchor w  \
        -relief groove -text Absolute -width 8 
    grid .vTcl.mgr.xx.place.fra1.lab6 \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 2 \
        -pady 0 
    label .vTcl.mgr.xx.place.fra1.lab7 \
        -anchor w  \
        -relief groove -text Relative -width 8 
    grid .vTcl.mgr.xx.place.fra1.lab7 \
        -column 0 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 2 \
        -pady 0 
    entry .vTcl.mgr.xx.place.fra1.ent8 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,place,-x) -width 4 
    grid .vTcl.mgr.xx.place.fra1.ent8 \
        -column 1 -row 1 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.place.fra1.ent9 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,place,-y) -width 4 
    grid .vTcl.mgr.xx.place.fra1.ent9 \
        -column 2 -row 1 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.place.fra1.ent10 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,place,-width) -width 4 
    grid .vTcl.mgr.xx.place.fra1.ent10 \
        -column 3 -row 1 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.place.fra1.ent11 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,place,-height) -width 4 
    grid .vTcl.mgr.xx.place.fra1.ent11 \
        -column 4 -row 1 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.place.fra1.ent12 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,place,-relx) -width 4 
    grid .vTcl.mgr.xx.place.fra1.ent12 \
        -column 1 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.place.fra1.ent13 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,place,-rely) -width 4 
    grid .vTcl.mgr.xx.place.fra1.ent13 \
        -column 2 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.place.fra1.ent14 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,place,-relwidth) -width 4 
    grid .vTcl.mgr.xx.place.fra1.ent14 \
        -column 3 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.place.fra1.ent15 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,place,-relheight) -width 4 
    grid .vTcl.mgr.xx.place.fra1.ent15 \
        -column 4 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    frame .vTcl.mgr.xx.place.fra18 \
        -borderwidth 1 -height 30 -width 30 
    place .vTcl.mgr.xx.place.fra18 \
        -x 0 -relx 0 -y 65 -rely 0 -width 215 -height 65 -anchor nw \
        -bordermode ignore 
    label .vTcl.mgr.xx.place.fra18.lab19 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-100-*-*-*-*-*-* -text x 
    grid .vTcl.mgr.xx.place.fra18.lab19 \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 0 \
        -pady 0 
    label .vTcl.mgr.xx.place.fra18.lab20 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-100-*-*-*-*-*-* -text y 
    grid .vTcl.mgr.xx.place.fra18.lab20 \
        -column 2 -row 0 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 0 \
        -pady 0 
    label .vTcl.mgr.xx.place.fra18.lab21 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-100-*-*-*-*-*-* \
        -text width 
    grid .vTcl.mgr.xx.place.fra18.lab21 \
        -column 3 -row 0 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 0 \
        -pady 0 
    label .vTcl.mgr.xx.place.fra18.lab22 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-100-*-*-*-*-*-* \
        -text height 
    grid .vTcl.mgr.xx.place.fra18.lab22 \
        -column 4 -row 0 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 0 \
        -pady 0 
    label .vTcl.mgr.xx.place.fra18.lab23 \
        -anchor w  \
        -relief groove -text Grid -width 8 
    grid .vTcl.mgr.xx.place.fra18.lab23 \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 2 \
        -pady 0 
    label .vTcl.mgr.xx.place.fra18.lab24 \
        -anchor w  \
        -relief groove -text {Key Incr.} -width 8 
    grid .vTcl.mgr.xx.place.fra18.lab24 \
        -column 0 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 0 -padx 2 \
        -pady 0 
    entry .vTcl.mgr.xx.place.fra18.ent25 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(grid,x) -width 4 
    grid .vTcl.mgr.xx.place.fra18.ent25 \
        -column 1 -row 1 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.place.fra18.ent26 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(grid,y) -width 4 
    grid .vTcl.mgr.xx.place.fra18.ent26 \
        -column 2 -row 1 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.place.fra18.ent27 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(key,x) -width 4 
    grid .vTcl.mgr.xx.place.fra18.ent27 \
        -column 1 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.place.fra18.ent28 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(key,y) -width 4 
    grid .vTcl.mgr.xx.place.fra18.ent28 \
        -column 2 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.place.fra18.ent29 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(key,w) -width 4 
    grid .vTcl.mgr.xx.place.fra18.ent29 \
        -column 3 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    entry .vTcl.mgr.xx.place.fra18.ent30 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(key,h) -width 4 
    grid .vTcl.mgr.xx.place.fra18.ent30 \
        -column 4 -row 2 -columnspan 1 -rowspan 1 -ipadx 0 -ipady 1 -padx 0 \
        -pady 0 
    label .vTcl.mgr.xx.place.lab32 \
        -anchor w  \
        -relief groove -text Anchor 
    place .vTcl.mgr.xx.place.lab32 \
        -x 10 -relx 0 -y 135 -rely 0 -width 75 -height 20 -anchor nw \
        -bordermode ignore 
    label .vTcl.mgr.xx.place.lab33 \
        -anchor w  \
        -relief groove -text BorderMode 
    place .vTcl.mgr.xx.place.lab33 \
        -x 10 -relx 0 -y 155 -rely 0 -width 75 -height 20 -anchor nw \
        -bordermode ignore 
    menubutton .vTcl.mgr.xx.place.men34 \
        -borderwidth 1 \
        -menu .vTcl.mgr.xx.place.men34.m -padx 4 -pady 3 -relief raised \
        -text nw -textvariable vTcl(w,place,-anchor) 
    place .vTcl.mgr.xx.place.men34 \
        -x 90 -relx 0 -y 135 -rely 0 -width 55 -height 19 -anchor nw \
        -bordermode ignore 
    menu .vTcl.mgr.xx.place.men34.m \
         -tearoff 0 
    .vTcl.mgr.xx.place.men34.m add command \
        -command {set vTcl(w,place,-anchor) center; vTcl:manager_update place} \
        -label center 
    .vTcl.mgr.xx.place.men34.m add command \
        -command { set vTcl(w,place,-anchor) n; vTcl:manager_update place } \
        -label n 
    .vTcl.mgr.xx.place.men34.m add command \
        -command { set vTcl(w,place,-anchor) s; vTcl:manager_update place } \
        -label s 
    .vTcl.mgr.xx.place.men34.m add command \
        -command { set vTcl(w,place,-anchor) e; vTcl:manager_update place } \
        -label e 
    .vTcl.mgr.xx.place.men34.m add command \
        -command { set vTcl(w,place,-anchor) w; vTcl:manager_update place } \
        -label w 
    .vTcl.mgr.xx.place.men34.m add command \
        -command { set vTcl(w,place,-anchor) nw; vTcl:manager_update place } \
        -label nw 
    .vTcl.mgr.xx.place.men34.m add command \
        -command { set vTcl(w,place,-anchor) ne; vTcl:manager_update place } \
        -label ne 
    .vTcl.mgr.xx.place.men34.m add command \
        -command { set vTcl(w,place,-anchor) sw; vTcl:manager_update place } \
        -label sw 
    .vTcl.mgr.xx.place.men34.m add command \
        -command { set vTcl(w,place,-anchor) se; vTcl:manager_update place } \
        -label se 
    menubutton .vTcl.mgr.xx.place.men35 \
        -borderwidth 1 \
        -menu .vTcl.mgr.xx.place.men35.m -padx 4 -pady 3 -relief raised \
        -text ignore -textvariable vTcl(w,place,-bordermode) 
    place .vTcl.mgr.xx.place.men35 \
        -x 90 -relx 0 -y 155 -rely 0 -width 55 -height 19 -anchor nw \
        -bordermode ignore 
    menu .vTcl.mgr.xx.place.men35.m \
         -tearoff 0 
    .vTcl.mgr.xx.place.men35.m add command \
        -command {set vTcl(w,place,-bordermode) ignore; vTcl:manager_update place} \
        -label ignore 
    .vTcl.mgr.xx.place.men35.m add command \
        -command {set vTcl(w,place,-bordermode) inside; vTcl:manager_update place} \
        -label inside 
    .vTcl.mgr.xx.place.men35.m add command \
        -command {set vTcl(w,place,-bordermode) outside; vTcl:manager_update place} \
        -label outside 
    frame .vTcl.mgr.xx.wm \
        -height 30 -width 30 
    label .vTcl.mgr.xx.wm.lab1 \
        -anchor w  \
        -relief groove -text Title 
    place .vTcl.mgr.xx.wm.lab1 \
        -x 5 -relx 0 -y 5 -rely 0 -width 60 -height 20 -anchor nw \
        -bordermode ignore 
    entry .vTcl.mgr.xx.wm.ent2 \
        -highlightthickness 0 -textvariable vTcl(w,wm,title) 
    place .vTcl.mgr.xx.wm.ent2 \
        -x 70 -relx 0 -y 5 -rely 0 -width 150 -height 20 -anchor nw \
        -bordermode ignore 
    label .vTcl.mgr.xx.wm.lab39 \
        -anchor w  \
        -relief groove -text State 
    place .vTcl.mgr.xx.wm.lab39 \
        -x 5 -relx 0 -y 155 -rely 0 -width 65 -height 20 -anchor nw \
        -bordermode ignore 
    menubutton .vTcl.mgr.xx.wm.men40 \
        -borderwidth 1 \
        -menu .vTcl.mgr.xx.wm.men40.m -padx 4 -pady 3 -relief raised \
        -text normal -textvariable vTcl(w,wm,state) 
    place .vTcl.mgr.xx.wm.men40 \
        -x 75 -relx 0 -y 155 -rely 0 -width 70 -height 20 -anchor nw \
        -bordermode ignore 
    menu .vTcl.mgr.xx.wm.men40.m \
         -tearoff 0 
    .vTcl.mgr.xx.wm.men40.m add command \
        -command { set vTcl(w,wm,state) normal; vTcl:manager_update wm } \
        -label normal 
    .vTcl.mgr.xx.wm.men40.m add command \
        -command { set vTcl(w,wm,state) iconic; vTcl:manager_update wm } \
        -label iconify 
    .vTcl.mgr.xx.wm.men40.m add command \
        -command { set vTcl(w,wm,state) withdrawn; vTcl:manager_update wm } \
        -label withdraw 
    label .vTcl.mgr.xx.wm.lab41 \
        -anchor w  \
        -relief groove -text Focus 
    place .vTcl.mgr.xx.wm.lab41 \
        -x 5 -relx 0 -y 175 -rely 0 -width 65 -height 20 -anchor nw \
        -bordermode ignore 
    menubutton .vTcl.mgr.xx.wm.men42 \
        -borderwidth 1 \
        -menu .vTcl.mgr.xx.wm.men42.m -padx 4 -pady 3 -relief raised \
        -text passive -textvariable vTcl(w,wm,focusmodel) 
    place .vTcl.mgr.xx.wm.men42 \
        -x 75 -relx 0 -y 175 -rely 0 -width 70 -height 20 -anchor nw \
        -bordermode ignore 
    menu .vTcl.mgr.xx.wm.men42.m \
         -tearoff 0 
    .vTcl.mgr.xx.wm.men42.m add command \
        -command { set vTcl(w,wm,focusmodel) active; vTcl:manager_update wm } \
        -label active 
    .vTcl.mgr.xx.wm.men42.m add command \
        -command { set vTcl(w,wm,focusmodel) passive; vTcl:manager_update wm } \
        -label passive 
    frame .vTcl.mgr.xx.wm.fra1 \
        -borderwidth 1 -height 30 -width 30 
    place .vTcl.mgr.xx.wm.fra1 \
        -x 0 -relx 0 -y 30 -rely 0 -width 245 -height 65 -anchor nw \
        -bordermode ignore 
    label .vTcl.mgr.xx.wm.fra1.lab2 \
        -anchor w  \
        -relief groove -text Geometry -width 8 
    place .vTcl.mgr.xx.wm.fra1.lab2 \
        -x 5 -relx 0 -y 20 -rely 0 -anchor nw -bordermode ignore 
    label .vTcl.mgr.xx.wm.fra1.lab3 \
        -anchor w  \
        -relief groove -text Min/Max -width 8 
    place .vTcl.mgr.xx.wm.fra1.lab3 \
        -x 5 -relx 0 -y 40 -rely 0 -anchor nw -bordermode ignore 
    label .vTcl.mgr.xx.wm.fra1.lab4 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-100-*-*-*-*-*-* -text x 
    place .vTcl.mgr.xx.wm.fra1.lab4 \
        -x 70 -relx 0 -y 5 -rely 0 -width 35 -height 13 -anchor nw \
        -bordermode ignore 
    label .vTcl.mgr.xx.wm.fra1.lab5 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-100-*-*-*-*-*-* -text y 
    place .vTcl.mgr.xx.wm.fra1.lab5 \
        -x 105 -relx 0 -y 5 -rely 0 -width 35 -height 13 -anchor nw \
        -bordermode ignore 
    button .vTcl.mgr.xx.wm.fra1.but6 \
        -background gray -borderwidth 1 \
        -command {vTcl:wm_button_switch w .vTcl.mgr.xx.wm.fra1.but6} \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-100-*-*-*-*-*-* \
        -highlightthickness 0 -padx 1 -pady 1 -takefocus 0 -text width 
    place .vTcl.mgr.xx.wm.fra1.but6 \
        -x 150 -relx 0 -y 3 -rely 0 -width 35 -height 16 -anchor nw \
        -bordermode ignore 
    button .vTcl.mgr.xx.wm.fra1.but7 \
        -background gray -borderwidth 1 \
        -command {vTcl:wm_button_switch h .vTcl.mgr.xx.wm.fra1.but7} \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-100-*-*-*-*-*-* \
        -highlightthickness 0 -padx 1 -pady 1 -takefocus 0 -text height 
    place .vTcl.mgr.xx.wm.fra1.but7 \
        -x 185 -relx 0 -y 3 -rely 0 -width 35 -height 16 -anchor nw \
        -bordermode ignore 
    entry .vTcl.mgr.xx.wm.fra1.ent8 \
        -borderwidth 1 \
        -highlightthickness 0 \
        -textvariable vTcl(w,wm,geometry,x) -width 4 
    place .vTcl.mgr.xx.wm.fra1.ent8 \
        -x 70 -relx 0 -y 20 -rely 0 -width 35 -height 19 -anchor nw \
        -bordermode ignore 
    entry .vTcl.mgr.xx.wm.fra1.ent9 \
        -borderwidth 1 \
        -highlightthickness 0 \
        -textvariable vTcl(w,wm,geometry,y) -width 4 
    place .vTcl.mgr.xx.wm.fra1.ent9 \
        -x 105 -relx 0 -y 20 -rely 0 -width 35 -height 19 -anchor nw \
        -bordermode ignore 
    entry .vTcl.mgr.xx.wm.fra1.ent10 \
        -borderwidth 1 \
        -highlightthickness 0 \
        -textvariable vTcl(w,wm,geometry,w) -width 4 
    place .vTcl.mgr.xx.wm.fra1.ent10 \
        -x 150 -relx 0 -y 20 -rely 0 -width 35 -height 19 -anchor nw \
        -bordermode ignore 
    entry .vTcl.mgr.xx.wm.fra1.ent11 \
        -borderwidth 1 \
        -highlightthickness 0 \
        -textvariable vTcl(w,wm,geometry,h) -width 4 
    place .vTcl.mgr.xx.wm.fra1.ent11 \
        -x 185 -relx 0 -y 20 -rely 0 -width 35 -height 19 -anchor nw \
        -bordermode ignore 
    entry .vTcl.mgr.xx.wm.fra1.ent12 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,wm,minsize,x) -width 4 
    place .vTcl.mgr.xx.wm.fra1.ent12 \
        -x 70 -relx 0 -y 40 -rely 0 -width 35 -height 19 -anchor nw \
        -bordermode ignore 
    entry .vTcl.mgr.xx.wm.fra1.ent13 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,wm,minsize,y) -width 4 
    place .vTcl.mgr.xx.wm.fra1.ent13 \
        -x 105 -relx 0 -y 40 -rely 0 -width 35 -height 19 -anchor nw \
        -bordermode ignore 
    entry .vTcl.mgr.xx.wm.fra1.ent14 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,wm,maxsize,x) -width 4 
    place .vTcl.mgr.xx.wm.fra1.ent14 \
        -x 150 -relx 0 -y 40 -rely 0 -width 35 -height 19 -anchor nw \
        -bordermode ignore 
    entry .vTcl.mgr.xx.wm.fra1.ent15 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,wm,maxsize,y) -width 4 
    place .vTcl.mgr.xx.wm.fra1.ent15 \
        -x 185 -relx 0 -y 40 -rely 0 -width 35 -height 19 -anchor nw \
        -bordermode ignore 
    label .vTcl.mgr.xx.wm.fra1.lab16 \
        -borderwidth 0 \
         -text / 
    place .vTcl.mgr.xx.wm.fra1.lab16 \
        -x 143 -relx 0 -y 42 -rely 0 -width 5 -height 16 -anchor nw \
        -bordermode ignore 
    frame .vTcl.mgr.xx.wm.fra17 \
        -borderwidth 1 -height 30 -width 30 
    place .vTcl.mgr.xx.wm.fra17 \
        -x 0 -relx 0 -y 95 -rely 0 -width 245 -height 45 -anchor nw \
        -bordermode ignore 
    label .vTcl.mgr.xx.wm.fra17.lab18 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-100-*-*-*-*-*-* \
        -text minimum 
    place .vTcl.mgr.xx.wm.fra17.lab18 \
        -x 70 -relx 0 -y 5 -rely 0 -width 70 -height 13 -anchor nw \
        -bordermode ignore 
    label .vTcl.mgr.xx.wm.fra17.lab19 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-100-*-*-*-*-*-* \
        -text maximum 
    place .vTcl.mgr.xx.wm.fra17.lab19 \
        -x 150 -relx 0 -y 5 -rely 0 -width 68 -height 13 -anchor nw \
        -bordermode ignore 
    label .vTcl.mgr.xx.wm.fra17.lab20 \
        -anchor w  \
        -relief groove -text Aspect -width 8 
    place .vTcl.mgr.xx.wm.fra17.lab20 \
        -x 5 -relx 0 -y 20 -rely 0 -anchor nw -bordermode ignore 
    entry .vTcl.mgr.xx.wm.fra17.ent21 \
        -borderwidth 1 \
        -highlightthickness 0 -justify right \
        -textvariable vTcl(w,wm,aspect,minnum) -width 4 
    place .vTcl.mgr.xx.wm.fra17.ent21 \
        -x 70 -relx 0 -y 20 -rely 0 -width 30 -height 19 -anchor nw \
        -bordermode ignore 
    label .vTcl.mgr.xx.wm.fra17.lab22 \
        -borderwidth 0 \
         -text / 
    place .vTcl.mgr.xx.wm.fra17.lab22 \
        -x 102 -relx 0 -y 23 -rely 0 -width 6 -height 16 -anchor nw \
        -bordermode ignore 
    entry .vTcl.mgr.xx.wm.fra17.ent23 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,wm,aspect,minden) -width 4 
    place .vTcl.mgr.xx.wm.fra17.ent23 \
        -x 110 -relx 0 -y 20 -rely 0 -width 30 -height 19 -anchor nw \
        -bordermode ignore 
    entry .vTcl.mgr.xx.wm.fra17.ent24 \
        -borderwidth 1 \
        -highlightthickness 0 -justify right \
        -textvariable vTcl(w,wm,aspect,maxnum) -width 4 
    place .vTcl.mgr.xx.wm.fra17.ent24 \
        -x 150 -relx 0 -y 20 -rely 0 -width 30 -height 19 -anchor nw \
        -bordermode ignore 
    entry .vTcl.mgr.xx.wm.fra17.ent25 \
        -borderwidth 1 \
        -highlightthickness 0 -textvariable vTcl(w,wm,aspect,maxden) -width 4 
    place .vTcl.mgr.xx.wm.fra17.ent25 \
        -x 190 -relx 0 -y 20 -rely 0 -width 30 -height 19 -anchor nw \
        -bordermode ignore 
    label .vTcl.mgr.xx.wm.fra17.lab26 \
        -borderwidth 0 \
         -text / 
    place .vTcl.mgr.xx.wm.fra17.lab26 \
        -x 182 -relx 0 -y 22 -rely 0 -width 6 -height 16 -anchor nw \
        -bordermode ignore 
    frame .vTcl.mgr.xx.wm.fra3 \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    place .vTcl.mgr.xx.wm.fra3 \
        -x 155 -relx 0 -y 145 -rely 0 -width 65 -height 50 -anchor nw \
        -bordermode ignore 
    button .vTcl.mgr.xx.wm.fra3.but4 \
        -command {vTcl:edit_wincmd pre} \
        -highlightthickness 0 -padx 9 -pady 3 -text PreCmd -width 5 
    pack .vTcl.mgr.xx.wm.fra3.but4 \
        -anchor center -expand 1 -fill both -ipadx 0 -ipady 0 -padx 0 -pady 0 \
        -side top 
    button .vTcl.mgr.xx.wm.fra3.but5 \
        -command {vTcl:edit_wincmd post} \
        -highlightthickness 0 -padx 9 -pady 3 -text PostCmd -width 5 
    pack .vTcl.mgr.xx.wm.fra3.but5 \
        -anchor center -expand 1 -fill both -ipadx 0 -ipady 0 -padx 0 -pady 0 \
        -side top 
    frame .vTcl.mgr.fra38 \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    pack .vTcl.mgr.fra38 \
        -anchor center -expand 0 -fill x -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side top 
    button .vTcl.mgr.fra38.but39 \
        -command {vTcl:manager_update $vTcl(w,manager)} \
        -highlightthickness 0 -padx 9 -pady 3 -text Update 
    pack .vTcl.mgr.fra38.but39 \
        -anchor center -expand 0 -fill x -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side top 
}

