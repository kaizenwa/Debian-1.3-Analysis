##############################################################################
#
# file.tcl - procedures to open, close and save applications
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

proc vTcl:new {} {
    global vTcl
    vTcl:close
    vTcl:new_widget toplevel
    set vTcl(project,name) "unknown.tcl"
    wm title $vTcl(gui,main) "Visual Tcl - $vTcl(project,name)"
}

proc vTcl:file_source {} {
    set file [vTcl:get_file open [pwd] "Source File"]
    if {$file != ""} {
        vTcl:source $file
    }
}

proc vTcl:source {file} {
    global vTcl
    set ov [uplevel #0 info vars];           vTcl:statbar 15
    set op [uplevel #0 info procs];          vTcl:statbar 20
    catch {uplevel #0 [list source $file]};  vTcl:statbar 35
    vTcl:list add [vTcl:diff_list $ov [uplevel #0 info vars]] vTcl(vars)
    vTcl:list add [vTcl:diff_list $op [uplevel #0 info procs]] vTcl(procs)
    vTcl:statbar 45
    set vTcl(tops) [vTcl:find_new_tops];     vTcl:statbar 0
}

proc vTcl:open {{file ""}} {
    global vTcl argc argv
    vTcl:close
    if {$file == ""} {
        set file [vTcl:get_file open [pwd] "Open Project"]
    } else {
        if ![file exists $file] {return}
    }
    if {$file != ""} {
        set vTcl(file,mode) ""
        proc exit {args} {}
        proc init {argc argv} {}
        proc main {argc argv} {}
        vTcl:load_lib vtclib.tcl;            vTcl:statbar 10
        set vTcl(tops) ""
        set vTcl(vars) ""
        set vTcl(procs) ""
        vTcl:source $file;                   vTcl:statbar 55
        vTcl:list add "init main" vTcl(procs)
        vTcl:setup_bind_tree .;              vTcl:statbar 65
        vTcl:update_top_list;                vTcl:statbar 75
        vTcl:update_var_list;                vTcl:statbar 85
        vTcl:update_proc_list;               vTcl:statbar 95
        set vTcl(project,file) $file
        set vTcl(project,name) [lindex [file split $file] end]
        wm title .vTcl "Visual Tcl - $vTcl(project,name)"
        vTcl:status "Done Loading"
        vTcl:statbar 0
    }
}

proc vTcl:close {} {
    global vTcl
    if {$vTcl(change) > 0} {
        switch [vTcl:dialog "Your application has unsaved changes.\nDo you wish to save?" "Yes No Cancel"] {
            Yes {
                if {[vTcl:save_as] == -1} {
                    return -1
                }
            }
            Cancel {
                return -1
            }
        }
    }
    set tops [winfo children .]
    foreach i $tops {
        if {$i != ".vTcl"} {destroy $i}
    }
    set vTcl(tops) ""
    vTcl:update_top_list
    foreach i $vTcl(vars) {
        catch {global $i; unset $i}
    }
    set vTcl(vars) ""
    vTcl:update_var_list
    foreach i $vTcl(procs) {
        catch {rename $i {}}
    }
    proc exit {args} {}
    proc init {argc argv} {}
    proc main {argc argv} {}
    set vTcl(procs) "init main"
    vTcl:update_proc_list
    set vTcl(project,file) ""
    set vTcl(project,name) ""
    set vTcl(w,widget) ""
    set vTcl(w,save) ""
    wm title $vTcl(gui,main) "Visual Tcl"
    set vTcl(change) 0
}

proc vTcl:save {} {
    global vTcl
    set vTcl(save) all
    set vTcl(w,save) $vTcl(w,widget)
    if {$vTcl(project,file) == ""} {
        set file [vTcl:get_file save [pwd] "Save Project"]
    }
    vTcl:destroy_handles
    vTcl:save2 $vTcl(project,file)
}

proc vTcl:save_as {} {
    global vTcl
    set vTcl(save) all
    set vTcl(w,save) $vTcl(w,widget)
    set file [vTcl:get_file save [pwd] "Save Project"]
    if {$file != ""} {
        vTcl:destroy_handles
        vTcl:save2 $file
    } else {
        return -1
    }
}

proc vTcl:save2 {file} {
    global vTcl
    set vTcl(project,name) [lindex [file split $file] end]
    set vTcl(project,file) $file
    wm title $vTcl(gui,main) "Visual Tcl - $vTcl(project,name)"
    set output [open $file w]
    puts $output "[subst $vTcl(head,proj)]\n"
    if { $vTcl(save) == "all" } {
        puts $output $vTcl(head,vars)
        puts $output [vTcl:save_vars]
        set body [string trim [info body init]]
        puts $output $vTcl(head,procs)
        puts $output "proc init \{argc argv\} \{\n$body\n\}\n"
        puts $output "init \$argc \$argv\n"
        puts $output [vTcl:save_procs]
        puts $output $vTcl(head,gui)
        puts $output [vTcl:save_tree .]
        puts $output "main \$argc \$argv"
    } else {
        puts $output [vTcl:save_tree $vTcl(w,widget)]
    }
    close $output
    vTcl:status "Done Saving"
    set vTcl(file,mode) ""
    if {$vTcl(w,save) != ""} {
        if {$vTcl(w,widget) != $vTcl(w,save)} {
            vTcl:active_widget $vTcl(w,save)
        }
        vTcl:create_handles $vTcl(w,save)
    }
    set vTcl(change) 0
}

proc vTcl:wm_quit {} {
    global vTcl
    if {$vTcl(change) > 0} {
        if {[vTcl:dialog "Your application has unsaved changes.\nDo you wish to save?" "Yes No" 1] == "Yes"} {
            vTcl:save_as
        }
    }
    vTcl:exit
}

proc vTcl:quit {} {
    global vTcl
    if {[vTcl:close] == -1} {return}
    if {$vTcl(quit)} {
        if {[vTcl:dialog "Are you sure\nyou want to quit?" "Yes No"] == "No"} {
            return
        }
    }
    set vTcl(quit) 0
    set vTcl(change) 0
    vTcl:save_prefs
    vTcl:exit
}

proc vTcl:save_prefs {} {
    global vTcl
    set output ""
    set showlist ""
    foreach i ".vTcl .vTcl.toolbar .vTcl.mgr .vTcl.ae
               .vTcl.proclist .vTcl.varlist .vTcl.toplist" {
        if {[winfo exists $i]} {
            append output "set vTcl(geometry,${i}) [wm geometry $i]\n"
            lappend showlist $i
        } else {
            catch {
                append output "set vTcl(geometry,${i}) $vTcl(geometry,${i})\n"
            }
        }
    }
    append output "set vTcl(gui,showlist) \"$showlist\"\n"
    foreach i {pr,getname pr,shortname pr,fullcfg pr,manager grid,x grid,y
               key,x key,y key,w key,h} {
        append output "set vTcl($i) $vTcl($i)\n"
    }
    append output "catch \{cd \{[pwd]\}\}\n"
    set file [open $vTcl(CONF_FILE) w]
    puts $file $output
    close $file
}

proc vTcl:find_files {base pattern} {
    global vTcl
    set dirs ""
    set match ""
    set files [lsort [glob -nocomplain [file join $base *]]]
    if {$pattern == ""} {set pattern "*"}
    foreach i $files {
        if {[file isdir $i]} {
            lappend dirs $i
        } elseif {[string match $pattern $i]} {
            lappend match $i
        }
    }
    return "$dirs $match"
}

