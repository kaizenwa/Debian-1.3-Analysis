##############################################################################
#
# proc.tcl - procedures for manipulating proctions and the proction browser
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

proc vTclWindow(post).vTcl.proc {argv} {
global vTcl
    set base $vTcl(gui,proc)
    set procname $base.f2.f8.procname
    set procargs $base.f2.f9.args
    set procbody $base.f3.text
    $procname delete 0 end
    $procargs delete 0 end
    $procbody delete 0.0 end
    $procname insert end [string trim [lindex $argv 0]]
    $procargs insert end [string trim [lindex $argv 1]]
    $procbody insert end [lindex $argv 2]
    if { [lindex $argv 1] == "" } {
        focus $procname
    } else {
        focus $procbody
    }
}

proc vTclWindow(post).vTcl.proclist {args} {
global vTcl
    wm withdraw $vTcl(gui,proclist)
    vTcl:setup_vTcl:bind $vTcl(gui,proclist)
    catch {wm geometry $vTcl(gui,proclist) $vTcl(geometry,$vTcl(gui,proclist))}
    update idletasks
    wm deiconify $vTcl(gui,proclist)
}

proc vTcl:delete_proc {name} {
global vTcl
    if {$name != ""} {
        rename $name ""
		vTcl:list delete $name vTcl(procs)
        vTcl:update_proc_list
    }
}

proc vTcl:find_new_procs {} {
global vTcl
    return [vTcl:diff_list $vTcl(start,procs) [info procs]]
}

proc vTcl:show_proc {name} {
global vTcl
    if { $name != "" } {
        set args {}
        foreach j [info args $name] {
            if {[info default $name $j def]} {
                lappend args "$j $def"
            } else {
                lappend args $j
            }
        }
        set body [string trim [info body $name]]
        Window show $vTcl(gui,proc) $name $args $body
    } else {
        Window show $vTcl(gui,proc) "" "" ""
    }
}

proc vTcl:show_proc_list {} {
global vTcl
    Window show $vTcl(gui,proclist)
    vTcl:update_proc_list
}

proc vTcl:update_proc {} {
global vTcl
    set name [.vTcl.proc.f2.f8.procname get]
    set args [.vTcl.proc.f2.f9.args get]
    set body [string trim [.vTcl.proc.f3.text get 0.0 end]]
    if {$name != ""} {
        proc $name $args $body
    }
	vTcl:list add $name vTcl(procs)
    grab release .vTcl.proc
    destroy .vTcl.proc
    vTcl:update_proc_list
}

proc vTcl:update_proc_list {} {
global vTcl
    if { [winfo exists $vTcl(gui,proclist)] == 0 } { return }
    $vTcl(gui,proclist).f2.list delete 0 end
    foreach i [lsort $vTcl(procs)] {
        if {[vTcl:valid_procname $i] == 1} {
            if {[info body $i] != "" || $i == "main" || $i == "init"} {
                $vTcl(gui,proclist).f2.list insert end $i
            }
        }
    }
}

proc vTcl:valid_procname {name} {
global vTcl
    set len [string length $vTcl(winname)]
    if { [string range $name 0 $len] != "$vTcl(winname)." &&  [string range $name 0 4] != "auto_" &&  [string range $name 0 2] != "tcl" &&  [string range $name 0 1] != "tk" &&  $name != "bgerror" } {
        return 1
    } else {
        return -1
    }
}

proc vTclWindow.vTcl.proc {args} {
	global vTcl
    set base .vTcl.proc
    if {[winfo exists .vTcl.proc]} {
        wm deiconify .vTcl.proc; return
    }
    toplevel .vTcl.proc -class vTcl
    wm focusmodel .vTcl.proc passive
    wm geometry .vTcl.proc 516x357+129+142
    wm maxsize .vTcl.proc 1137 870
    wm minsize .vTcl.proc 1 1
    wm overrideredirect .vTcl.proc 0
    wm resizable .vTcl.proc 1 1
    wm deiconify .vTcl.proc
    wm title .vTcl.proc "Function Editor"
    bind .vTcl.proc <Key-Escape> {
        vTcl:update_proc
    }
    frame .vTcl.proc.f2 \
        -height 30 -width 30 
    pack .vTcl.proc.f2 \
        -anchor center -expand 0 -fill x -ipadx 0 -ipady 0 -padx 3 -pady 3 \
        -side top 
    frame .vTcl.proc.f2.f8 \
        -height 30 -width 30 
    pack .vTcl.proc.f2.f8 \
        -anchor center -expand 1 -fill both -ipadx 0 -ipady 0 -padx 0 -pady 0 \
        -side top 
    label .vTcl.proc.f2.f8.label10 \
        -anchor w  \
        -relief groove -text Function -width 9 
    pack .vTcl.proc.f2.f8.label10 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 2 -pady 0 \
        -side left 
    entry .vTcl.proc.f2.f8.procname \
        -cursor {}  \
        -highlightthickness 0 
    pack .vTcl.proc.f2.f8.procname \
        -anchor center -expand 1 -fill x -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    frame .vTcl.proc.f2.f9 \
        -height 30 -width 30 
    pack .vTcl.proc.f2.f9 \
        -anchor center -expand 0 -fill both -ipadx 0 -ipady 0 -padx 0 -pady 0 \
        -side top 
    label .vTcl.proc.f2.f9.label12 \
        -anchor w  \
        -relief groove -text Arguments -width 9 
    pack .vTcl.proc.f2.f9.label12 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 2 -pady 0 \
        -side left 
    entry .vTcl.proc.f2.f9.args \
        -cursor {}  \
        -highlightthickness 0 
    pack .vTcl.proc.f2.f9.args \
        -anchor center -expand 1 -fill x -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    frame .vTcl.proc.f3 \
        -borderwidth 2 -height 30 -relief groove -width 30 
    pack .vTcl.proc.f3 \
        -anchor center -expand 1 -fill both -ipadx 0 -ipady 0 -padx 3 -pady 3 \
        -side top 
    text .vTcl.proc.f3.text \
        -height 7 -highlightthickness 0 -width 16 \
        -wrap none -yscrollcommand {.vTcl.proc.f3.scrollbar4 set} 
    pack .vTcl.proc.f3.text \
        -anchor center -expand 1 -fill both -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    scrollbar .vTcl.proc.f3.scrollbar4 \
        -command {.vTcl.proc.f3.text yview}
    pack .vTcl.proc.f3.scrollbar4 \
        -anchor center -expand 0 -fill y -ipadx 0 -ipady 0 -padx 0 -pady 0 \
        -side left 
    frame .vTcl.proc.frame14 \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    pack .vTcl.proc.frame14 \
        -anchor center -expand 0 -fill x -ipadx 0 -ipady 0 -padx 3 -pady 3 \
        -side top 
    button .vTcl.proc.frame14.button15 \
        -command {
            vTcl:update_proc
        } \
         -padx 9 \
        -pady 3 -text OK -width 5 
    pack .vTcl.proc.frame14.button15 \
        -anchor center -expand 1 -fill x -ipadx 0 -ipady 0 -padx 0 -pady 0 \
        -side left 
    button .vTcl.proc.frame14.button16 \
        -command {grab release .vTcl.proc; destroy .vTcl.proc} \
         -padx 9 \
        -pady 3 -text Cancel -width 5 
    pack .vTcl.proc.frame14.button16 \
        -anchor center -expand 1 -fill x -ipadx 0 -ipady 0 -padx 0 -pady 0 \
        -side left 
}

proc vTclWindow.vTcl.proclist {args} {
    set base .vTcl.proclist
    if {[winfo exists .vTcl.proclist]} {
        wm deiconify .vTcl.proclist; return
    }
    toplevel .vTcl.proclist -class vTcl
    wm focusmodel .vTcl.proclist passive
    wm geometry .vTcl.proclist 200x200+48+237
    wm maxsize .vTcl.proclist 1137 870
    wm minsize .vTcl.proclist 200 200
    wm overrideredirect .vTcl.proclist 0
    wm resizable .vTcl.proclist 1 1
    wm deiconify .vTcl.proclist
    wm title .vTcl.proclist "Functions"
    bind .vTcl.proclist <Double-Button-1> {
        vTcl:show_proc [.vTcl.proclist.f2.list get  [.vTcl.proclist.f2.list curselection]]
    }
    frame .vTcl.proclist.frame7 \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    pack .vTcl.proclist.frame7 \
        -anchor center -expand 0 -fill x -ipadx 0 -ipady 0 -padx 0 -pady 0 \
        -side bottom 
    button .vTcl.proclist.frame7.button8 \
        -command {vTcl:show_proc ""} \
         -padx 9 \
        -pady 3 -text Add -width 5 
    pack .vTcl.proclist.frame7.button8 \
        -anchor center -expand 1 -fill x -ipadx 0 -ipady 0 -padx 0 -pady 0 \
        -side left 
    button .vTcl.proclist.frame7.button9 \
        \
        -command {
            set vTcl(x) [.vTcl.proclist.f2.list curselection]
            if {$vTcl(x) != ""} {
                vTcl:show_proc [.vTcl.proclist.f2.list get $vTcl(x)]
            }
        } \
         -padx 9 \
        -pady 3 -text Edit -width 5 
    pack .vTcl.proclist.frame7.button9 \
        -anchor center -expand 1 -fill x -ipadx 0 -ipady 0 -padx 0 -pady 0 \
        -side left 
    button .vTcl.proclist.frame7.button10 \
        \
        -command {
            set vTcl(x) [.vTcl.proclist.f2.list curselection]
            if {$vTcl(x) != ""} {
                vTcl:delete_proc [.vTcl.proclist.f2.list get $vTcl(x)]
            }
        } \
         -padx 9 \
        -pady 3 -text Delete -width 5 
    pack .vTcl.proclist.frame7.button10 \
        -anchor center -expand 1 -fill x -ipadx 0 -ipady 0 -padx 0 -pady 0 \
        -side left 
    frame .vTcl.proclist.f2 \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    pack .vTcl.proclist.f2 \
        -anchor center -expand 1 -fill both -ipadx 0 -ipady 0 -padx 0 -pady 0 \
        -side top 
    listbox .vTcl.proclist.f2.list \
         \
        -yscrollcommand {.vTcl.proclist.f2.sb4  set} 
    pack .vTcl.proclist.f2.list \
        -anchor center -expand 1 -fill both -ipadx 0 -ipady 0 -padx 0 -pady 0 \
        -side left 
    scrollbar .vTcl.proclist.f2.sb4 \
        -borderwidth 1 -command {.vTcl.proclist.f2.list yview} -width 10 
    pack .vTcl.proclist.f2.sb4 \
        -anchor center -expand 0 -fill y -ipadx 0 -ipady 0 -padx 0 -pady 0 \
        -side right 
}

