##############################################################################
#
# attredit.tcl - procedures used by the attribute editor
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

proc vTclWindow.vTcl.ae {args} {
    global vTcl
    set ae $vTcl(gui,ae)
    set fr $vTcl(gui,ae).fr
    if {[winfo exists $ae]} {wm deiconify $ae; return}
    toplevel $ae -class vTcl
    wm title $ae "Attribute Editor"
    wm geometry $ae 206x325-1+0
    wm resizable $ae 0 1
    frame  $fr -relief groove -bd 2
    scrollbar $fr.s
    pack $fr -side top -fill both -padx 2 -pady 2 -expand 1
    pack $fr.s -side right -fill y
}

proc vTclWindow(post).vTcl.ae {args} {
    global vTcl tcl_platform
    wm withdraw .vTcl.ae
    if { $vTcl(w,widget) != "" } {
        vTcl:gui_update_widget_info
    }
    vTcl:setup_vTcl:bind $vTcl(gui,ae)
    if {$tcl_platform(platform) == "macintosh"} {
        set w [expr [winfo vrootwidth .] - 206]
        wm geometry $vTcl(gui,ae) 206x325+$w+20
    }
    catch {wm geometry .vTcl.ae $vTcl(geometry,.vTcl.ae)}
    update idletasks
    wm deiconify .vTcl.ae
}

proc vTcl:gui_update_widget_info {} {
    global vTcl
    if {$vTcl(var_update) == "no"} {
        return
    }
    vTcl:update_widget_info $vTcl(w,widget)
    set fr $vTcl(gui,ae).fr
    set ca $fr._$vTcl(w,class)
    set top $ca.f
    update idletasks
    if {[winfo exists $top]} {
        if {$vTcl(w,class) != $vTcl(w,last_class)} {
            catch {pack forget $fr._$vTcl(w,last_class)}
            pack $ca -side left -fill both -expand 1
            $fr.s conf -command "$ca yview"
        }
        foreach i $vTcl(opt,list) {
            if {[lsearch $vTcl(w,optlist) $i] >= 0} {
                if { [lindex $vTcl(opt,$i) 2] == "color" } {
                    $top.t$i configure -bg $vTcl(w,opt,$i)
                }
            }
        }
    } elseif [winfo exists $fr] {
        catch {pack forget $fr._$vTcl(w,last_class)}
        canvas $ca \
            -width 175 -height 150 -yscrollcommand "$fr.s set" \
            -yscrollincrement 10 -highlightthickness 0
        frame $top
        $ca create window 0 0 -window $top -anchor nw
        grid columnconfigure $top 1 -weight 1
        $ca configure -scrollregion "0 0 175 200"
        pack $ca -side left -fill both -expand 1
        $fr.s conf -command "$ca yview"
        foreach i $vTcl(opt,list) {
            if {[lsearch $vTcl(w,optlist) $i] >= 0} {
                set variable "vTcl(w,opt,$i)"
                set config_cmd "\$vTcl(w,widget) configure $i \$$variable; "
                append config_cmd "vTcl:place_handles \$vTcl(w,widget)"
                vTcl:new_gui_option $top $i $variable $config_cmd
            }
        }
    } else {
        return
    }
    update idletasks
    $ca configure -scrollregion "0 0 175 [winfo reqheight $top]"
}

proc vTcl:new_gui_option { top option variable config_cmd } {
    global vTcl
    label $top.$option \
        -text "[lindex $vTcl(opt,$option) 0]" -anchor w -width 11 -fg black
    switch [lindex $vTcl(opt,$option) 2] {
        choice {
            menubutton $top.t$option \
                -textvariable $variable -bd 1 -width 11 -menu $top.t$option.m \
                -highlightthickness 1 -relief sunken -anchor w -fg black
            menu $top.t$option.m -tearoff 0
            foreach i [lindex $vTcl(opt,$option) 3] {
                $top.t$option.m add command -label "$i" -command \
                    "set $variable $i; $config_cmd; "
            }
        }
        color {
            button $top.t$option \
                -relief sunken -bd 1 -width 11 -highlightthickness 1 -fg black \
                -bg [subst $$variable] -command \
                "vTcl:show_color $top.t$option $option $variable"
        }
        command {
            button $top.t$option \
                -text "<edit>" -relief sunken -bd 1 -width 11 \
                -highlightthickness 1 -fg black \
                -command {vTcl:set_command $vTcl(w,widget)} -anchor w
        }
        default {
            entry $top.t$option \
                -textvariable $variable -relief sunken -bd 1 -width 11 \
                -highlightthickness 1 -fg black
        }
    }
    bind $top.t$option <KeyRelease-Return> $config_cmd
    grid $top.$option $top.t$option -sticky we
}

