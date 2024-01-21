##############################################################################
#
# compound.tcl - procedures for creating and inserting compound-widgets
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

##########################################################################
# Compound Widgets
#
# compound   = type options mgr-info bind-info children
# mgr-info   = geom-manager-name geom-info
# bind-info  = list of: {event} {command}
# menu-info  = list of: {type} {options}
# children   = list-of-compound-widgets (recursive)
#

proc vTcl:save_compounds {} {
    global vTcl
    set file [vTcl:get_file save [pwd] "Save Compound Library"]
    if {$file == ""} {return}
    set f [open $file w]
    puts $f "set vTcl(cmpd,list) \"$vTcl(cmpd,list)\"\n"
    set index 0
    set num [llength $vTcl(cmpd,list)]
    foreach i $vTcl(cmpd,list) {
        puts $f "set \{vTcl(cmpd:$i)\} \{$vTcl(cmpd:$i)\}\n"
        incr index
        vTcl:statbar [expr ($index * 100) / $num]
    }
    close $f
    vTcl:statbar 0
}

proc vTcl:load_compounds {{file ""}} {
    global vTcl
    set file [vTcl:get_file open [pwd] "Load Compound Library"]
    if {$file == ""} {return}
    vTcl:statbar 10
    source $file
    vTcl:statbar 80
    vTcl:cmp_user_menu
    vTcl:statbar 0
}

proc vTcl:put_compound {compound} {
    global vTcl
    set name [vTcl:new_widget_name cpd $vTcl(w,insert)]
    vTcl:insert_compound $name $compound $vTcl(w,def_mgr)
    vTcl:setup_bind_tree $name
    vTcl:active_widget $name
    vTcl:update_proc_list
}

proc vTcl:name_replace {name s} {
    global vTcl
    foreach i $vTcl(cmp,alias) {
        set s [vTcl:replace [lindex $i 0] $name[lindex $i 1] $s]
    }
    return $s
}

proc vTcl:insert_compound {name compound {gmgr pack} {gopt ""}} {
    global vTcl
    set cpd \{[lindex $compound 0]\}
    set alias [lindex $compound 1]
    set vTcl(cmp,alias) [lsort -decreasing -command vTcl:sort_cmd $alias]
    set cmd [vTcl:extract_compound $name $name $cpd 0 $gmgr $gopt]
    set do "$cmd"
    set undo "destroy $name"
    vTcl:push_action $do $undo
}

proc vTcl:extract_compound {base name compound {level 0} {gmgr ""} {gopt ""}} {
    global vTcl widget
    set todo ""
    foreach i $compound {
        set type [string trim [lindex $i 0]]
        set opts [string trim [lindex $i 1]]
        set mgr  [string trim [lindex $i 2]]
        set mgrt [string trim [lindex $mgr 0]]
        set mgri [string trim [lindex $mgr 1]]
        set bind [string trim [lindex $i 3]]
        set menu [string trim [lindex $i 4]]
        set chld [string trim [lindex $i 5]]
        set wdgt [string trim [lindex $i 6]]
        set alis [string trim [lindex $i 7]]
        set grid [string trim [lindex $i 8]]
        set proc [string trim [lindex $i 9]]
        set cmpdname [string trim [lindex $i 10]]
        #
        # process procs first in case of dependancies (init)
        #
        foreach j $proc {
            set nme [lindex $j 0]
            set arg [lindex $j 1]
            set bdy [lindex $j 2]
            proc $nme $arg $bdy
            vTcl:list add $nme vTcl(procs)
            if {$nme == "${cmpdname}:init"} {
                eval $nme
            }
        }
        if {$mgrt == "wm" || $base == "."} {
            set base $name
        } elseif {$level == 0 && $gmgr != ""} {
            if {$gmgr != $mgrt || $gopt != ""}  {
                set mgrt $gmgr
                set mgri $gopt
            }
        }
        if {$level > 0} {
            set name "$base$wdgt"
        }
        append todo "$type $name [vTcl:name_replace $base $opts]; "
        if {$mgrt != "" && $mgrt != "wm"} {
            if {$mgrt == "place" && $mgri == ""} {
                set mgri "-x 5 -y 5"
            }
            append todo "$mgrt $name $mgri; "
        } elseif {$mgrt == "wm"} {
        } else {
            set ret $name
        }
        set index 0
        incr level
        foreach j $bind {
            set e [lindex $j 0]
            set c [vTcl:name_replace $base [lindex $j 1]]
            append todo "bind $name $e \"$c\"; "
        }
        foreach j $menu {
            set t [lindex $j 0]
            set o [lindex $j 1]
            if {$t != "tearoff"} {
                append todo "$name add $t $o; "
            }
        }
        foreach j $chld {
            append todo "[vTcl:extract_compound $base $name \{$j\} $level]; "
            incr index
        }
        if {$alis != ""} {
            set widget($alis) $name
            set widget(rev,$name) "$alis"
        }
        foreach j $grid {
            set cmd [lindex $j 0]
            set num [lindex $j 1]
            set prop [lindex $j 2]
            set val [lindex $j 3]
            append todo "grid $cmd $name $num $prop $val; "
        }
        if {[info procs "${cmpdname}:main"] != ""} {
            eval ${cmpdname}:main
        }
    }
    return $todo
}

proc vTcl:create_compound {target {cmpdname ""}} {
    global vTcl
    set vTcl(cmp,alias) ""
    set vTcl(cmp,index) 0
    set ret [vTcl:gen_compound $target "" $cmpdname]
    lappend ret $vTcl(cmp,alias)
    return $ret
}

proc vTcl:gen_compound {target {name ""} {cmpdname ""}} {
    global vTcl widget
    set ret ""
    set mgr ""
    set bind ""
    set menu ""
    set chld ""
    set alias ""
    set grid ""
    set proc ""
    if {![winfo exists $target]} {
        return ""
    }
    set type [string tolower [vTcl:get_class $target]]
    set opts [vTcl:get_opts [$target conf]]
    if {$type == "menu"} {
        set mnum [$target index end]
        if {$mnum != "none"} {
            for {set i 0} {$i <= $mnum} {incr i} {
                set t [$target type $i]
                set c [vTcl:get_opts [$target entryconf $i]]
                lappend menu "$t \{$c\}"
            }
        }
        set mgrt {}
        set mgri {}
    } elseif {$type == "toplevel"} {
        set mgrt "wm"
        set mgri ""
    } else {
        set mgrt [winfo manager $target]
        set mgri [vTcl:get_mgropts [$mgrt info $target]]
    }
    lappend mgr $mgrt $mgri
    set blst [bind $target]
    foreach i $blst {
        lappend bind "$i \{[bind $target $i]\}"
    }
    foreach i [vTcl:get_children $target] {
        incr vTcl(cmp,index)
        append chld "[vTcl:gen_compound $i $name.0$vTcl(cmp,index)] "
    }
    catch {set alias $widget(rev,$target)}
    set pre g
    set gcolumn [lindex [grid size $target] 0]
    set grow [lindex [grid size $target] 1]
    foreach a {column row} {
        foreach b {weight minsize} {
            set num [subst $$pre$a]
            for {set i 0} {$i < $num} {incr i} {
                set x [expr round([grid ${a}conf $target $i -$b])]
                if {$x > 0} {
                    lappend grid "${a}conf $i -$b $x"
                }
            }
        }
    }
    if {$cmpdname != ""} {
        foreach i $vTcl(procs) {
            if [string match ${cmpdname}:* $i] {
                lappend proc [list $i [info args $i] [info body $i]]
            }
        }
    }
    lappend ret $type $opts $mgr $bind $menu $chld $name $alias $grid $proc $cmpdname
    vTcl:append_alias $target $name
    return \{$ret\}
}


proc vTcl:append_alias {name alias} {
    global vTcl
    lappend vTcl(cmp,alias) "$name $alias"
}

proc vTcl:sort_cmd {el1 el2} {
    set l1 [string length [lindex $el1 0]]
    set l2 [string length [lindex $el2 0]]
    return [expr $l1 - $l2]
}

proc vTcl:replace {target replace source} {
    set ret ""
    set where [string first $target $source]
    if {$where < 0} {return $source}
    set len [string length $target]
    set before [string range $source 0 [expr $where - 1]]
    set after [string range $source [expr $where + $len] end]
    return "$before$replace$after"
}

