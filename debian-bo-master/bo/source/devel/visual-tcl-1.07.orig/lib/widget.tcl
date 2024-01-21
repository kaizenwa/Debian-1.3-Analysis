##############################################################################
#
# widget.tcl - procedures for manipulating widget information
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

proc vTcl:properties {target} {
    vTcl:status "Properties not implemented"
}

proc vTcl:bind_scrollbar {t1 t2} {
    global vTcl
    set c1 [winfo class $t1]
    set c2 [winfo class $t2]
    if { $c1 == "Scrollbar" } {
        set t3 $t1; set t1 $t2; set t2 $t3
        set c3 $c1; set c1 $c2; set c2 $c3
    } elseif { $c2 == "Scrollbar" } {
    } else {
        return
    }
    switch [lindex [$t2 conf -orient] 4] {
        vert -
        vertical { set scr_cmd -yscrollcommand; set v_cmd yview }
        default  { set scr_cmd -xscrollcommand; set v_cmd xview }
    }
    switch $c1 {
        Listbox -
        Canvas -
        Text {
            $t1 conf $scr_cmd "$t2 set"
            $t2 conf -command "$t1 $v_cmd"
        }
        Entry {
            if {$v_cmd == "xview"} {
                $t1 conf $scr_cmd "$t2 set"
                $t2 conf -command "$t1 $v_cmd"
            }
        }
    }
}

proc vTcl:show {target} {
    global vTcl
    if {![winfo viewable $target]} {
        catch {eval $vTcl(hide,$target,m) $target $vTcl(hide,$target,i)}
    }
}

proc vTcl:hide {} {
    global vTcl
    if { $vTcl(w,manager) != "wm" && $vTcl(w,widget) != "" } {
        lappend vTcl(hide) $vTcl(w,widget)
        set vTcl(hide,$vTcl(w,widget),m) $vTcl(w,manager)
        set vTcl(hide,$vTcl(w,widget),i) [$vTcl(w,manager) info $vTcl(w,widget)]
        $vTcl(w,manager) forget $vTcl(w,widget)
        vTcl:destroy_handles
    }
}

proc vTcl:set_insert {} {
    global vTcl
    set vTcl(w,insert) $vTcl(w,widget)
}

proc vTcl:select_parent {} {
    global vTcl
    vTcl:active_widget [winfo parent $vTcl(w,widget)]
    vTcl:set_insert
}

proc vTcl:select_toplevel {} {
    global vTcl
    vTcl:active_widget [winfo toplevel $vTcl(w,widget)]
    vTcl:set_insert
}

proc vTcl:active_widget {target} {
    global vTcl
    if { $vTcl(w,widget) != "$target" } {
        vTcl:select_widget $target
        vTcl:attrbar_color $target
        vTcl:attrbar_anchor $target
        set vTcl(redo) [vTcl:dump_widget_quick $target]
        if {$vTcl(w,class) == "Toplevel"} {
            vTcl:destroy_handles
            set vTcl(w,insert) $target
        } else {
            vTcl:create_handles $target
            vTcl:place_handles $target
            if { $vTcl(w,class) == "Frame" } {
                set vTcl(w,insert) $target
            } else {
                set vTcl(w,insert) [winfo parent $target]
            }
        }
    }
}

proc vTcl:select_widget {target} {
    global vTcl
    if {$target == $vTcl(w,widget)} {return}
    set vTcl(w,last_class) $vTcl(w,class)
    set vTcl(w,last_widget) $vTcl(w,widget)
    set vTcl(w,last_manager) $vTcl(w,manager)
    vTcl:update_widget_info $target
    vTcl:gui_update_widget_info
    vTcl:show_mgr $vTcl(w,manager)
    vTcl:get_bind $target
}

#
# Recurses a widget tree ignoring toplevels
#
proc vTcl:widget_tree {target} {
    if {$target == ".vTcl" || [string range $target 0 4] == ".__tk"} {
        return
    }
    set output "$target "
    set c [vTcl:get_children $target]
    foreach i $c {
        set mgr [winfo manager $i]
        set class [vTcl:get_class $i]
        if {$class != "Toplevel"} {
            append output [vTcl:widget_tree $i]
        }
    }
    return $output
}

#
# Recurses a widget tree with the option of not ignoring built-ins
#
proc vTcl:list_widget_tree {target {which ""}} {
    if {$which == ""} {
        if {$target == ".vTcl" || [string range $target 0 4] == ".__tk"} {
            return
        }
    }
    set w_tree "$target "
    set children [vTcl:get_children $target]
    if {[winfo class $target] == "Canvas"} {
        return $w_tree
    }
    foreach i $children {
        append w_tree "[vTcl:list_widget_tree $i $which] "
    }
    return $w_tree
}

##############################################################################
# WIDGET INFO ROUTINES
##############################################################################
proc vTcl:split_info {target} {
    global vTcl
    set index 0
    set mgr $vTcl(w,manager)
    set mgr_info [$mgr info $target]
    set vTcl(w,info) $mgr_info
    if { $vTcl(var_update) == "yes" } {
        set index a
        foreach i $mgr_info {
            if { $index == "a" } {
                set var vTcl(w,$mgr,$i)
                set last $i
                set index b
            } else {
                set $var $i
                if {$last == "-sticky"} {
                    vTcl:get_grid_stickies $i
                }
                set index a
            }
        }
    }
    if {$mgr == "grid"} {
        set p [winfo parent $target]
        set col $vTcl(w,grid,-column)
        set row $vTcl(w,grid,-row)
        set vTcl(w,grid,col,weight) \
            [expr round([grid columnconf $p $col -weight])]
        set vTcl(w,grid,col,width)  \
            [expr round([grid columnconf $p $col -minsize])]
        set vTcl(w,grid,row,weight) \
            [expr round([grid rowconf $p $row -weight])]
        set vTcl(w,grid,row,width)  \
            [expr round([grid rowconf $p $row -minsize])]
    }
}

proc vTcl:split_wm_info {target} {
    global vTcl
    set vTcl(w,info) ""
    foreach i $vTcl(attr,tops) {
        if {$i == "geometry"} {
            #
            # because window managers behave unpredictably with wm and
            # winfo, one is used for editing and the other for saving
            #
            if {$vTcl(mode) == "EDIT"} {
                set vTcl(w,wm,$i) [winfo $i $target]
            } else {
                set vTcl(w,wm,$i) [wm $i $target]
            }
        } else {
            set vTcl(w,wm,$i) [wm $i $target]
        }
    }
    set vTcl(w,wm,class) [winfo class $target]
    if { $vTcl(var_update) == "yes" } {
        set geo [split $vTcl(w,wm,geometry) "+"]
        set geo_hw [split [lindex $geo 0] x]
        set vTcl(w,wm,geometry,w)    [lindex $geo_hw 0]
        set vTcl(w,wm,geometry,h)    [lindex $geo_hw 1]
        set vTcl(w,wm,geometry,x)    [lindex $geo 1]
        set vTcl(w,wm,geometry,y)    [lindex $geo 2]
        set vTcl(w,wm,minsize,x)     [lindex $vTcl(w,wm,minsize) 0]
        set vTcl(w,wm,minsize,y)     [lindex $vTcl(w,wm,minsize) 1]
        set vTcl(w,wm,maxsize,x)     [lindex $vTcl(w,wm,maxsize) 0]
        set vTcl(w,wm,maxsize,y)     [lindex $vTcl(w,wm,maxsize) 1]
        set vTcl(w,wm,aspect,minnum) [lindex $vTcl(w,wm,aspect) 0]
        set vTcl(w,wm,aspect,minden) [lindex $vTcl(w,wm,aspect) 1]
        set vTcl(w,wm,aspect,maxnum) [lindex $vTcl(w,wm,aspect) 2]
        set vTcl(w,wm,aspect,maxden) [lindex $vTcl(w,wm,aspect) 3]
        set vTcl(w,wm,resizable,w)   [lindex $vTcl(w,wm,resizable) 0]
        set vTcl(w,wm,resizable,h)   [lindex $vTcl(w,wm,resizable) 1]
    }
}

proc vTcl:get_grid_stickies {sticky} {
    global vTcl
    set len [string length $sticky]
    foreach i {n s e w} {
        set vTcl(grid,sticky,$i) ""
    }
    for {set i 0} {$i < $len} {incr i} {
        set val [string index $sticky $i]
        set vTcl(grid,sticky,$val) $val
    }
}

proc vTcl:update_widget_info {target} {
    global vTcl widget
    update idletasks
    set vTcl(w,widget) $target
    set vTcl(w,didmove) 0
    set vTcl(w,options) ""
    set vTcl(w,optlist) ""
    if {![winfo exists $target]} {return}
    foreach i $vTcl(attr,winfo) {
        set vTcl(w,$i) [winfo $i $target]
    }
    set vTcl(w,class) [vTcl:get_class $target]
    set vTcl(w,r_class) [winfo class $target]
    set vTcl(w,conf) [$target configure]
    switch $vTcl(w,manager) {
        {} {}
        grid -
        pack -
        place {
            vTcl:split_info $target
        }
        wm {
            if { $vTcl(w,class) != "Menu" } {
                vTcl:split_wm_info $target
            }
        }
    }
    set vTcl(w,options) [vTcl:conf_to_pairs $vTcl(w,conf) set]
    if {[catch {set vTcl(w,alias) $widget(rev,$target)}]} {
        set vTcl(w,alias) ""
    }
}

proc vTcl:conf_to_pairs {conf opt} {
    global vTcl
    set pairs ""
    set vTcl(w,opt,-text) ""
    foreach i $conf {
        set option [lindex $i 0]
        set def [lindex $i 3]
        set value [lindex $i 4]
        if { $value != $def && $option != "-class" } {
            lappend pairs $option $value
        }
        if {$opt == "set"} {
            lappend vTcl(w,optlist) $option
            set vTcl(w,opt,$option) $value
        }
    }
    return $pairs
}

proc vTcl:new_widget_name {type base} {
    global vTcl
    while { 1 } {
        if $vTcl(pr,shortname) {
            set num "[string range $type 0 2]$vTcl(item_num)"
        } else {
            set num "$type$vTcl(item_num)"
        }
        incr vTcl(item_num)
        if {$base != "." && $type != "toplevel"} {
            set new_widg $base.$num
        } else {
            set new_widg .$num
        }
        if { ![winfo exists $new_widg] } { break }
    }
    return $new_widg
}

proc vTcl:setup_vTcl:bind {target} {
    global vTcl
    set bindlist [vTcl:list_widget_tree $target all]
    update idletasks
    foreach i $bindlist {
        if { [lsearch [bindtags $target] vTcl(a)] < 0 } {
            set tmp [bindtags $target]
            bindtags $target "vTcl(a) $tmp"
        }
    }
}

proc vTcl:setup_bind {target} {
    global vTcl
    if {[lsearch [bindtags $target] vTcl(b)] < 0} {
        set vTcl(bindtags,$target) [bindtags $target]
        bindtags $target vTcl(b)
        if {[vTcl:get_class $target] == "Toplevel"} {
            wm protocol $target WM_DELETE_WINDOW "vTcl:hide_top $target"
            wm protocol $target WM_TAKE_FOCUS "vTcl:wm_take_focus $target"
        }
    }
}

proc vTcl:switch_mode {} {
    global vTcl
    if {$vTcl(mode) == "EDIT"} {
        vTcl:setup_unbind_tree .
    } else {
        vTcl:setup_bind_tree .
    }
}

proc vTcl:setup_bind_tree {target} {
    global vTcl
    set bindlist [vTcl:list_widget_tree $target]
    update idletasks
    foreach i $bindlist {
        vTcl:setup_bind $i
    }
    set vTcl(mode) "EDIT"
}

proc vTcl:setup_unbind {target} {
    global vTcl
    if { [lsearch [bindtags $target] vTcl(b)] >= 0 } {
        bindtags $target $vTcl(bindtags,$target)
    }
}

proc vTcl:setup_unbind_tree {target} {
    global vTcl
    vTcl:select_widget .
    vTcl:destroy_handles
    set bindlist [vTcl:list_widget_tree $target]
    update idletasks
    foreach i $bindlist {
        vTcl:setup_unbind $i
    }
    set vTcl(mode) "TEST"
}

##############################################################################
# INSERT NEW WIDGET ROUTINE
##############################################################################
proc vTcl:new_widget {type {options ""}} {
    global vTcl
    if {$vTcl(mode) == "TEST"} {
        vTcl:error "Inserting widgets is not\nallowed in Test mode."
        return
    }
    global vTcl
    if { ($vTcl(w,insert) == "." && $type != "toplevel") ||
         ([winfo exists $vTcl(w,insert)] == 0 && $type != "toplevel")} {
        vTcl:dialog "No insertion point set!"
        return
    }
    set vTcl(mgrs,update) no
    if $vTcl(pr,getname) {
        set new_widg [vTcl:get_name $type]
    } else {
        set new_widg [vTcl:new_widget_name $type $vTcl(w,insert)]
    }
    if {$new_widg != ""} {
        vTcl:create_widget $type $options $new_widg
    }
}

proc vTcl:create_widget {type options new_widg} {
    global vTcl
    set do ""
    set undo ""
    if {$vTcl(pr,getname) == 1} {
        if { $vTcl(w,insert) == "." } {
            set new_widg ".$new_widg"
        } else {
            set new_widg "$vTcl(w,insert).$new_widg"
        }
    }
    append do "$type $new_widg $vTcl($type,insert) $options;"
    if {[info procs vTcl:widget:$type:inscmd] != ""} {
        append do "[vTcl:widget:$type:inscmd $new_widg];"
    }
    if {$type != "toplevel"} {
        append do "$vTcl(w,def_mgr) $new_widg $vTcl($vTcl(w,def_mgr),insert);"
    }
    append do "vTcl:setup_bind_tree $new_widg"
    if {$undo == ""} {
        set undo "destroy $new_widg"
    }
    vTcl:push_action $do $undo
    update idletasks
    vTcl:active_widget $new_widg
    set vTcl(mgrs,update) yes
    return $new_widg
}

proc vTcl:wm_take_focus {target} {
    global vTcl
    if {$vTcl(w,manager) == "wm"} {
        vTcl:select_widget $target
        vTcl:wm_button_update
    } else {
        vTcl:place_handles $vTcl(w,widget)
    }
}

proc vTcl:set_alias {target} {
    global vTcl widget
    if {$target == ""} {return}
    set was ""
    if {[catch {set alias $widget(rev,$target)}]} {
        set alias [vTcl:get_string "Widget alias for $vTcl(w,class)" $target]
    } else {
        set was $alias
        set alias \
            [vTcl:get_string "Widget alias for $vTcl(w,class)" $target $alias]
    }
    if {$alias == "" && $was != ""} {
        catch {
            unset widget($was)
            unset widget(rev,$target)
        }
    } else {
        set widget($alias) $target
        set widget(rev,$target) $alias
    }
}

proc vTcl:set_label {t} {
    global vTcl
    if {$t == ""} {return}
    if [catch {set txt [$t cget -text]}] {
        return
    }
    set label [vTcl:get_string "Setting label" $t $txt]
    $t conf -text $label
    vTcl:place_handles $t
    set vTcl(w,opt,-text) $label
}

proc vTcl:set_textvar {t} {
    global vTcl
    if {$t == ""} {return}
    set label [vTcl:get_string "Setting label" $t [$t cget -textvar]]
    $t conf -textvar $label
    vTcl:place_handles $t
}

proc vTcl:name_compound {t} {
    global vTcl
    if {$t == "" || ![winfo exists $t]} {return}
    set name [vTcl:get_string "Name Compound" $t]
    if {$name == ""} {return}
    if {[lsearch $vTcl(cmpd,list) $name] < 0} {lappend vTcl(cmpd,list) $name}
    set vTcl(cmpd:$name) [vTcl:create_compound $t $name]
    vTcl:cmp_user_menu
}

proc vTcl:widget_dblclick {target} {
    global vTcl
    set c [string tolower [vTcl:get_class $target]]
    if {[info procs vTcl:widget:$c:dblclick] != ""} {
        eval vTcl:widget:$c:dblclick $target
    }
}

