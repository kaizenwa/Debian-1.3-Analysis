##############################################################################
#
# command.tcl - procedures to update widget commands
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

proc vTcl:edit_wincmd {which} {
    global vTcl
    set target $vTcl(w,widget)
    set base ".vTcl.com_${which}_[vTcl:rename $target]"
    if [catch {set cmd [info body vTclWindow($which)$target]}] {
        set cmd ""
    }
    set r [vTcl:get_command "Window ${which}Command for $target" $cmd $base]
    if {$r == -1} {
        return
    } else {
        proc vTclWindow($which)$target {args} $r
    }
}

proc vTcl:set_command {target} {
    global vTcl
    set base ".vTcl.com_[vTcl:rename $target]"
    if {[catch {set cmd [$target cget -command]}] == 1} {
        return
    }
    set r [vTcl:get_command "Command for $target" $cmd $base]
    if {$r == -1} {
        return
    } else {
        $target conf -command [string trim $r]
    }
}

proc vTcl:get_command {title initial base} {
    global vTcl
    if [winfo exists $base] {wm deiconify $base; return}
    set vTcl(x,$base) -1
    toplevel $base -class vTcl
    wm geometry $base 350x150
    wm resizable $base 1 1
    wm title $base $title
    frame $base.f \
        -borderwidth 2 -height 30 -relief groove -width 30 
    pack $base.f \
        -in $base -anchor center -expand 1 -fill both -ipadx 0 -ipady 0 \
        -padx 3 -pady 3 -side top 
    text $base.f.text \
        -height 2 -width 2 -wrap none \
        -yscrollcommand {$base.f.scrollbar16 set} 
    pack $base.f.text \
        -in $base.f -anchor center -expand 1 -fill both -ipadx 0 \
        -ipady 0 -padx 0 -pady 0 -side left 
    scrollbar $base.f.scrollbar16 \
        -borderwidth 1 -command "$base.f.text yview" -width 10 
    pack $base.f.scrollbar16 \
        -in $base.f -anchor center -expand 0 -fill y -ipadx 0 -ipady 0 \
        -padx 0 -pady 0 -side left 
    frame $base.f21 \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    pack $base.f21 \
        -in $base -anchor center -expand 0 -fill x -ipadx 0 -ipady 0 \
        -padx 3 -pady 3 -side top 
    button $base.f21.button22 \
        -command "
            set vTcl(x,$base) \[$base.f.text get 0.0 end\]
            destroy $base
        " \
         -padx 9 \
        -pady 3 -text OK -width 5 
    pack $base.f21.button22 \
        -in $base.f21 -anchor center -expand 1 -fill x -ipadx 0 -ipady 0 \
        -padx 0 -pady 0 -side left 
    button $base.f21.button23 \
        -command "
            set vTcl(x,$base) -1
            destroy $base
        " \
         -padx 9 \
        -pady 3 -text Cancel -width 5 
    pack $base.f21.button23 \
        -in $base.f21 -anchor center -expand 1 -fill x -ipadx 0 -ipady 0 \
        -padx 0 -pady 0 -side left 
    update idletasks
    bind $base <Key-Escape> "
        set vTcl(x,$base) \[$base.f.text get 0.0 end\]
        destroy $base
        break
    "
    $base.f.text delete 0.0 end
    $base.f.text insert end $initial
    focus $base.f.text
    tkwait window $base
    update idletasks
    switch -- $vTcl(x,$base) {
        "-1"      {return -1}
        default "return $vTcl(x,$base)"
    }
}

