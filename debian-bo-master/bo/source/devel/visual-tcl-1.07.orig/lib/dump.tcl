##############################################################################
#
# dump.tcl - procedures to export widget information
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

proc vTcl:save_vars {} {
    global vTcl
    set output ""
	set list $vTcl(vars)
	lappend list widget
    foreach i $list {
        catch {
            if {$i != "auto_index" && $i != "auto_oldpath" &&
                [string range $i 0 4] != ".__tk"} {
                global $i
                append output "global $i; "
                if {[array exists $i] == 1} {
                    append output "\n"
                    set names [array names $i]
                    foreach j $names {
                        set value "[subst $$i\($j\)]"
                        append output "$vTcl(tab)set $i\($j\) \{$value\}\n"
                    }
                } else {
                    append output "set $i \{[subst $\{$i\}]\}\n"
                }
            }
        }
    }
    return "$output"
}

proc vTcl:save_procs {} {
    global vTcl
    set output ""
    set list $vTcl(procs)
    lappend list Window
    foreach i $list {
        if {[vTcl:valid_procname $i] == 1} {
            set args ""
            foreach j [info args $i] {
                if {[info default $i $j value]} {
                    lappend args "$j $value"
                } else {
                    lappend args $j
                }
            }
            set body [string trim [info body $i]]
            if {($body != "" || $i == "main") && $i != "init"} {
                append output "\nproc $i \{$args\} \{\n$body\n\}\n"
            }
        }
    }
    return $output
}

proc vTcl:save_tree {target} {
    global vTcl
    set output ""
    set vTcl(dumptops) ""
    set vTcl(showtops) ""
    set vTcl(var_update) "no"
    set vTcl(num,index) 0
    set tops ". $vTcl(tops)"
    vTcl:status "Saving: collecting data"
    set vTcl(num,total) [llength [vTcl:list_widget_tree $target]]
    foreach i $tops {
        append output [vTcl:dump_top $i]
    }
    append output "\n"
    vTcl:status "Saving: collecting options"
    foreach i $vTcl(showtops) {
        append output "Window show $i\n"
    }
    set vTcl(var_update) "yes"
    vTcl:statbar 0
    vTcl:status "Saving: writing data"
    return $output
}

proc vTcl:valid_class {class} {
    global vTcl
    if {[lsearch $vTcl(classes) $class] >= 0} {
        return 1
    } else {
        return 0
    }
}

proc vTcl:get_class {target} {
    set class [winfo class $target]
    if {![vTcl:valid_class $class]} {
        return Toplevel
    } else {
        return $class
    }
}

proc vTcl:get_mgropts {opts} {
    if {[lindex $opts 0] == "-in"} {
        set opts [lrange $opts 2 end]
    }
    set nopts ""
    set spot a
    foreach i $opts {
        if {$spot == "a"} {
            set o $i
            set spot b
        } else {
            set v $i
            switch -- $o {
                -ipadx -
                -ipady -
                -padx -
                -pady -
                -relx -
                -rely {
                    if {$v != "" && $v != 0} {
                        lappend nopts $o $v
                    }
                }
                default {
                    if {$v != ""} {
                        lappend nopts $o $v
                    }
                }
            }
            set spot a
        }
    }
    return $nopts
}

proc vTcl:get_opts {opts} {
    set ret ""
    foreach i $opts {
        set o [lindex $i 0]
        set v [lindex $i 4]
        if {$o != "-class" && $v != [lindex $i 3]} {
            lappend ret $o $v
        }
    }
    return $ret
}

proc vTcl:dump_widget_quick {target} {
    global vTcl
    vTcl:update_widget_info $target
    set result "$target conf $vTcl(w,options)\n"
    append result "$vTcl(w,manager) $target $vTcl(w,info)\n"
    return $result
}

proc vTcl:dump_widget_opt {target} {
    global vTcl
    set mgr [winfo manager $target]
    if {$mgr == ""} {return}
    set result ""
    set class [vTcl:get_class $target]
    set opt [$target conf]
    if {$target != "."} {
        set result "$vTcl(tab)[string tolower $class] $target"
        if {$mgr == "wm" && $class != "Menu"} {
            append result " -class [winfo class $target]"
        }
        set p [vTcl:get_opts $opt]
        if {$p != ""} {
            append result " \\\n[vTcl:clean_pairs $p]\n"
        } else {
            append result "\n"
        }
    }
    if {$mgr == "wm"} {
        if {$class == "Menu"} {
            append result [vTcl:dump_menu_widget $target]
        } else {
            append result [vTcl:dump_top_widget $target]
        }
    }
    append result [vTcl:dump_widget_bind $target]
    return $result
}

proc vTcl:dump_widget_geom {target} {
    global vTcl
    set mgr [winfo manager $target]
    if {$mgr == ""} {return}
    set class [winfo class $target]
    set result ""
    if {$mgr != "wm"} {
        set opts [$mgr info $target]
        set result "$vTcl(tab)$mgr $target \\\n"
        append result "[vTcl:clean_pairs [vTcl:get_mgropts $opts]]\n"
    }
    set pre g
    set gcolumn [lindex [grid size $target] 0]
    set grow [lindex [grid size $target] 1]
    foreach a {column row} {
        foreach b {weight minsize} {
            set num [subst $$pre$a]
            for {set i 0} {$i < $num} {incr i} {
                set x [expr round([grid ${a}conf $target $i -$b])]
                if $x {
                    append result "$vTcl(tab)grid ${a}conf $target $i -$b $x\n"
                }
            }
        }
    }
    return $result
}

proc vTcl:dump_widget_bind {target} {
    global vTcl
    set result ""
    if {[catch {bindtags $target \{$vTcl(bindtags,$target)\}}]} {
        return ""
    }
    set bindlist [bind $target]
    foreach i $bindlist {
        set command [bind $target $i]
        append result "$vTcl(tab)bind $target $i \{\n"
        append result "$vTcl(tab2)[string trim $command]\n    \}\n"
    }
    bindtags $target vTcl(b)
    return $result
}

proc vTcl:dump_menu_widget {target} {
    global vTcl
    set entries [$target index end]
    if {$entries == "none"} {return}
    set result ""
    for {set index 0} {$index <= $entries} {incr index} {
        set conf [$target entryconf $index]
        set type [$target type $index]
        switch $type {
            tearoff {}
            separator {
                append result "$vTcl(tab)$target add separator\n"
            }
            default {
                set pairs [vTcl:conf_to_pairs $conf ""]
                append result "$vTcl(tab)$target add $type \\\n"
                append result "[vTcl:clean_pairs $pairs]\n"
            }
        }
    }
    return $result
}

proc vTcl:dump_top_widget {target} {
    global vTcl
    set result ""
    foreach i $vTcl(attr,tops) {
        if { $vTcl(w,wm,$i) != "" } {
            switch $i {
                class {}
                title {
                    append result "$vTcl(tab)wm $i $target"
                    append result " \"$vTcl(w,wm,$i)\"\n"
                }
                state {
                    switch $vTcl(w,wm,state) {
                        withdrawn {
                            append result "$vTcl(tab)wm withdraw $target\n"
                        }
                        iconic {
                            append result "$vTcl(tab)wm iconify $target\n"
                        }
                        normal {
                            append result "$vTcl(tab)wm deiconify $target\n"
                        }
                    }
                }
                default {
                    append result "$vTcl(tab)wm $i $target $vTcl(w,wm,$i)\n"
                }
            }
        }
    }
    return $result
}

proc vTcl:dump_top {target} {
    global vTcl
    if {[winfo class $target] != "Toplevel" && $target != "."} {
        return
    }
    vTcl:update_widget_info $target
    append output "\nproc $vTcl(winname)$target \{args\} \{\n"
    append output "$vTcl(tab)set base $target\n"
    if { $target != "." } {
        append output "$vTcl(tab)if \{\[winfo exists $target\]\} \{\n"
        append output "$vTcl(tab2)wm deiconify $target; return\n"
        append output "$vTcl(tab)\}\n"
    }
    if {[wm state $target] == "normal" ||
        [wm state $target] == "iconic" ||
        $target == "."} {
        lappend vTcl(showtops) $target
    }
    incr vTcl(num,index)
    vTcl:statbar [expr ($vTcl(num,index) * 100) / $vTcl(num,total)]
    set tree [vTcl:widget_tree $target]
    append output $vTcl(head,proc,widgets)
    foreach i $tree {
        append output [vTcl:dump_widget_opt $i]
        incr vTcl(num,index)
        vTcl:statbar [expr ($vTcl(num,index) * 100) / $vTcl(num,total)]
    }
    append output $vTcl(head,proc,geometry)
    foreach i $tree {
        append output [vTcl:dump_widget_geom $i]
    }
    append output "\}\n"
    return $output
}

