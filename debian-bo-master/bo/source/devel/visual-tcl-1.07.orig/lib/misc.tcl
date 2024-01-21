##############################################################################
#
# misc.tcl - leftover uncategorized procedures
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

proc vTcl:load_images {} {
    global vTcl

    foreach i {l c r j} {
        image create photo "align_$i" \
            -file [file join $vTcl(VTCL_HOME) images align_$i.ppm]
    }
    foreach i {grid pack place} {
        image create photo "mgr_$i" \
            -file [file join $vTcl(VTCL_HOME) images mgr_$i.ppm]
    }
    foreach i {x y both none} {
        image create photo "fill_$i" \
            -file [file join $vTcl(VTCL_HOME) images fill_$i.ppm]
    }
    foreach i {n s e w nw ne sw se c} {
        image create photo "anchor_$i" \
            -file [file join $vTcl(VTCL_HOME) images anchor_$i.ppm]
    }
    image create bitmap "file_down" \
        -file [file join $vTcl(VTCL_HOME) images down.xbm]
}

proc vTcl:list {cmd elements list} {
    upvar $list nlist
    switch $cmd {
        add {
            foreach i $elements {
                if {[lsearch -exact $nlist $i] < 0} {
                    lappend nlist $i
                }
            }
        }
        delete {
            foreach i $elements {
                set n [lsearch -exact $nlist $i]
                if {$n > -1} {
                    set nlist [lreplace $nlist $n $n]
                }
            }
        }
    }
    return $nlist
}

proc vTcl:diff_list {oldlist newlist} {
    set output ""
    set index 0
    set oldlist2 [lsort $oldlist]
    set oldlen [llength $oldlist2]
    set newlen [llength $newlist]
    foreach i [lsort $newlist] {
        while { 1 } {
            if { $index > $oldlen } {
                lappend output $i
                break
            }
            set result [string compare $i [lindex $oldlist2 $index]]
            if { $result == -1 } {
                lappend output $i
                break
            } elseif { $result == 0 } {
                incr index
                break
            } else {
                incr index
                if { $index > $oldlen && $index > $newlen } {
                    break
                }
            }
        }
    }
    return $output
}

proc vTcl:clean_pairs {list {indent 8}} {
    set tab [string range "                " 0 [expr $indent - 1]]
    set index $indent
    set output $tab
    set last ""
    foreach i $list {
        if {$i == ""} {
            set i {{}}
        } elseif {[llength [split $i]] > 1} {
            set i "\{$i\}"
        }
        if {$last == ""} {
            set last $i
        } else {
            set i "$last $i "
            set last ""
            set len [string length $i]
            if { [expr $index + $len] > 78 } {
                append output "\\\n${tab}${i}"
                set index [expr $indent + $len]
            } else {
                append output "$i"
                incr index $len
            }
        }
    }
    return $output
}

#############################
# Setting Widget Properties #
#############################
proc vTcl:bounded_incr {var delta} {
    upvar $var newvar
    set newval [expr $newvar + $delta]
    if {$newval < 0} {
        set newvar 0
    } else {
        set newvar $newval
    }
}

proc vTcl:pos_neg {num} {
    if {$num > 0} {return 1}
    if {$num < 0} {return -1}
    return 0
}

proc vTcl:widget_delta {widget x y w h} {
    global vTcl
    switch $vTcl(w,manager) {
        grid {
            vTcl:bounded_incr vTcl(w,grid,-column) [vTcl:pos_neg $x]
            vTcl:bounded_incr vTcl(w,grid,-row) [vTcl:pos_neg $y]
            vTcl:bounded_incr vTcl(w,grid,-columnspan) [vTcl:pos_neg $w]
            vTcl:bounded_incr vTcl(w,grid,-rowspan) [vTcl:pos_neg $h]
            vTcl:manager_update grid
        }
        pack {
            if {$x < 0 || $y < 0} {vTcl:pack_before $vTcl(w,widget)}
            if {$x > 0 || $y > 0} {vTcl:pack_after $vTcl(w,widget)}
        }
        place {
            set newX [expr [winfo x $widget] + $x]
            set newY [expr [winfo y $widget] + $y]
            set newW [expr [winfo width $widget] + $w]
            set newH [expr [winfo height $widget] + $h]
            set do "place $vTcl(w,widget) -x $newX -y $newY \
                -width $newW -height $newH -bordermode ignore"
            set undo [vTcl:dump_widget_quick $widget]
            vTcl:push_action $do $undo
        }
    }
    vTcl:place_handles $widget
}

##############################################################################
# OTHER PROCEDURES
##############################################################################
proc vTcl:hex {num} {
    if {$num == ""} {set num 0}
    set textnum [format "%x" $num]
    if { $num < 16 } { set textnum "0${textnum}" }
    return $textnum
}

proc vTcl:grid_snap {xy pos} {
    global vTcl
    if { $vTcl(w,manager) != "place" } { return $pos }
    set off [expr $pos % $vTcl(grid,$xy)]
    if { $off > 0 } {
        return [expr $pos - $off]
    } else {
        return $pos
    }
}

proc vTcl:status {message} {
    global vTcl
    set vTcl(status) $message
    update idletasks
}

proc vTcl:right_click {widget x y} {
    global vTcl
    $vTcl(gui,rc_menu) post $x $y
    grab $vTcl(gui,rc_menu)
    bind $vTcl(gui,rc_menu) <ButtonRelease> {
        grab release $vTcl(gui,rc_menu)
        $vTcl(gui,rc_menu) unpost
    }
}

proc vTcl:statbar {value} {
    global vTcl
    if {$value == 0} {
        place forget $vTcl(gui,statbar)
    } else {
        place $vTcl(gui,statbar) -x 1 -y 2 -width [expr $value * 1.5] -height 12
    }
    update idletasks
}

proc vTcl:show_bindings {} {
    global vTcl
    if {$vTcl(w,widget) != ""} {
        Window show .vTcl.bind
        vTcl:get_bind $vTcl(w,widget)
    } else {
        vTcl:dialog "No widget selected!"
    }
}

proc vTcl:rename {name} {
    regsub -all "\\." $name "_" ret
    return $ret
}

proc vTcl:cmp_user_menu {} {
    global vTcl
    set m $vTcl(gui,main).menu.c.m.m.u
    catch {destroy $m}
    menu $m -tearoff 0
    foreach i [lsort $vTcl(cmpd,list)] {
        $m add comm -label $i -comm "
            vTcl:put_compound \$vTcl(cmpd:$i)
        "
    }
}

proc vTcl:cmp_sys_menu {} {
    global vTcl
    set m $vTcl(gui,main).menu.c.m.m.s
    catch {destroy $m}
    menu $m -tearoff 0
    foreach i [lsort $vTcl(syscmpd,list)] {
        $m add comm -label $i -comm "
            vTcl:put_compound \$vTcl(syscmpd:$i)
        "
    }
}

proc vTcl:get_children {target} {
    global vTcl
    set r ""
    set all [winfo children $target]
    set n [pack slaves $target]
    if {$n != ""} {
        foreach i $all {
            if {[lsearch -exact $n $i] < 0} {
                lappend n $i
            }
        }
    } else {
        set n $all
    }
    foreach i $n {
        if ![string match ".__tk*" $i] {
            lappend r $i
        }
    }
    return $r
}

proc vTcl:find_new_tops {} {
    global vTcl
    set new ""
    foreach i $vTcl(procs) {
        if [string match vTclWindow* $i] {
            set n [string range $i 10 end]
            if {$n != "."} {
                lappend new [string range $i 10 end]
            }
        }
    }
    foreach i [vTcl:list_widget_tree .] {
        if {[winfo class $i] == "Toplevel"} {
            if {[lsearch $new $i] < 0} {
                lappend new $i
            }
        }
    }
    return $new
}

proc vTcl:error {mesg} {
    vTcl:dialog $mesg
}

proc vTcl:dialog {mesg {options Ok} {root 0}} {
    global vTcl
    if {$root == 0} {
        set base .vTcl.message
    } else {
        set base .vTcl:message
    }
    set sw [winfo screenwidth .]
    set sh [winfo screenheight .]
    toplevel $base -class vTcl
    frame $base.f -bd 2 -relief groove
    label $base.f.t -bd 0 -relief flat -text $mesg -justify left \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-180-*-*-*-*-*-*
    frame $base.o -bd 1 -relief sunken
    foreach i $options {
        set n [string tolower $i]
        button $base.o.$n -text $i -width 5 -highlightthickness 0 \
        -command "
            set vTcl(x_mesg) $i
            destroy $base
        "
        pack $base.o.$n -side left -expand 1 -fill x -padx 2 -pady 2
    }
    pack $base.f.t -side top -expand 1 -fill both -ipadx 5 -ipady 5
    pack $base.f -side top -expand 1 -fill both -padx 2 -pady 2
    pack $base.o -side top -fill x -padx 2 -pady 2
    wm withdraw $base
    update idletasks
    set w [winfo reqwidth $base]
    set h [winfo reqheight $base]
    set x [expr ($sw - $w)/2]
    set y [expr ($sh - $h)/2]
    wm geometry $base ${w}x${h}+$x+$y
    wm deiconify $base
    grab $base
    tkwait window $base
    grab release $base
    return $vTcl(x_mesg)
}

