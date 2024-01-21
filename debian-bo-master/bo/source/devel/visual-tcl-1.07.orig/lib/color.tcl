##############################################################################
#
# color.tcl - color browser
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

proc vTcl:get_color {color} {
    global vTcl tk_version
    set oldcolor $color

    if {$tk_version >= 4.2} {
        set newcolor [tk_chooseColor -initialcolor $color]
        if {$newcolor != ""} {
            return $newcolor
        } else {
            return $oldcolor
        }
    }
    if {[string length $color] > 7} {
        set vTcl(color,red)   [format "%d" "0x[string range $color 1 2]"]
        set vTcl(color,green) [format "%d" "0x[string range $color 5 6]"]
        set vTcl(color,blue)  [format "%d" "0x[string range $color 9 10]"]
    } else {
        set vTcl(color,red)   [format "%d" "0x[string range $color 1 2]"]
        set vTcl(color,green) [format "%d" "0x[string range $color 3 4]"]
        set vTcl(color,blue)  [format "%d" "0x[string range $color 5 6]"]
    }
    Window show .vTcl.color
    vTcl:color_update
    tkwait window .vTcl.color
    if {$vTcl(color) == -1} {
        return $oldcolor
    } else {
        return $vTcl(color)
    }
}

proc vTcl:color_update {{num 0}} {
global vTcl
    set red      "#[vTcl:hex $vTcl(color,red)]0000"
    set green    "#00[vTcl:hex $vTcl(color,green)]00"
    set blue     "#0000[vTcl:hex $vTcl(color,blue)]"
    set color    "#[vTcl:hex $vTcl(color,red)]"
    append color "[vTcl:hex $vTcl(color,green)]"
    append color "[vTcl:hex $vTcl(color,blue)]"
    set vTcl(color) $color
    .vTcl.color.frame9 configure -bg $color
    .vTcl.color.frame11.frame5 configure -bg $red
    .vTcl.color.frame11.frame7 configure -bg $green
    .vTcl.color.frame11.frame6 configure -bg $blue
}

proc vTcl:show_color {widget option variable} {
global vTcl
    set vTcl(color,widget)   $widget
    set vTcl(color,option)   $option
    set vTcl(color,variable) $variable
    set color [subst $$variable]
    if {$color == ""} {
        set color "#000000" 
    } elseif {[string range $color 0 0] != "#" } {
        set clist [winfo rgb . $color]
        set r [lindex $clist 0]
        set g [lindex $clist 1]
        set b [lindex $clist 2]
        set color "#[vTcl:hex $r][vTcl:hex $g][vTcl:hex $b]"
    }
    set vTcl(color) [vTcl:get_color $color]
    set $vTcl(color,variable) $vTcl(color)
    $vTcl(color,widget) configure -bg $vTcl(color)
    $vTcl(w,widget) configure $vTcl(color,option) $vTcl(color)
}

proc vTclWindow.vTcl.color {args} {
    if {[winfo exists .vTcl.color]} {return}
    toplevel .vTcl.color -class vTcl
    wm focusmodel .vTcl.color passive
    wm geometry .vTcl.color 313x149+298+248
    wm maxsize .vTcl.color 1137 870
    wm minsize .vTcl.color 1 1
    wm overrideredirect .vTcl.color 0
    wm resizable .vTcl.color 0 0
    wm deiconify .vTcl.color
    wm title .vTcl.color "Color Picker"
    bind .vTcl.color <KeyRelease> {vTcl:color_update}
    frame .vTcl.color.frame9 \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    place .vTcl.color.frame9 \
        -x 5 -relx 0 -y 5 -rely 0 -width 300 -relwidth {} -height 25 \
        -relheight {} -anchor nw -bordermode ignore 
    frame .vTcl.color.frame11 \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    place .vTcl.color.frame11 \
        -x 5 -relx 0 -y 35 -rely 0 -width 300 -relwidth {} -height 80 \
        -relheight {} -anchor nw -bordermode ignore 
    scale .vTcl.color.frame11.scale1 \
        -borderwidth 1 -command vTcl:color_update \
        -highlightthickness 0 -orient horizontal -showvalue 0 \
        -sliderlength 25 -to 255.0 -troughcolor gray \
        -variable vTcl(color,blue) -width 10 
    place .vTcl.color.frame11.scale1 \
        -x 30 -relx 0 -y 58 -rely 0 -width 215 -relwidth {} -height 15 \
        -relheight {} -anchor nw -bordermode ignore 
    scale .vTcl.color.frame11.scale2 \
        -borderwidth 1 -command vTcl:color_update \
        -highlightthickness 0 -orient horizontal -showvalue 0 \
        -sliderlength 25 -to 255.0 -troughcolor gray \
        -variable vTcl(color,green) -width 10 
    place .vTcl.color.frame11.scale2 \
        -x 30 -relx 0 -y 33 -rely 0 -width 215 -relwidth {} -height 15 \
        -relheight {} -anchor nw -bordermode ignore 
    scale .vTcl.color.frame11.scale3 \
        -borderwidth 1 -command vTcl:color_update \
        -highlightthickness 0 -orient horizontal -showvalue 0 \
        -sliderlength 25 -to 255.0 -troughcolor gray \
        -variable vTcl(color,red) -width 10 
    place .vTcl.color.frame11.scale3 \
        -x 30 -relx 0 -y 8 -rely 0 -width 215 -relwidth {} -height 15 \
        -relheight {} -anchor nw -bordermode ignore 
    frame .vTcl.color.frame11.frame5 \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    place .vTcl.color.frame11.frame5 \
        -x 5 -relx 0 -y 5 -rely 0 -width 20 -relwidth {} -height 19 \
        -relheight {} -anchor nw -bordermode ignore 
    frame .vTcl.color.frame11.frame6 \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    place .vTcl.color.frame11.frame6 \
        -x 5 -relx 0 -y 55 -rely 0 -width 20 -relwidth {} -height 19 \
        -relheight {} -anchor nw -bordermode ignore 
    frame .vTcl.color.frame11.frame7 \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    place .vTcl.color.frame11.frame7 \
        -x 5 -relx 0 -y 30 -rely 0 -width 20 -relwidth {} -height 19 \
        -relheight {} -anchor nw -bordermode ignore 
    entry .vTcl.color.frame11.entry8 \
        -highlightthickness 0 -textvariable vTcl(color,red) 
    place .vTcl.color.frame11.entry8 \
        -x 250 -relx 0 -y 5 -rely 0 -width 45 -relwidth {} -height 19 \
        -relheight {} -anchor nw -bordermode ignore 
    entry .vTcl.color.frame11.entry9 \
        -highlightthickness 0 -textvariable vTcl(color,blue) 
    place .vTcl.color.frame11.entry9 \
        -x 250 -relx 0 -y 55 -rely 0 -width 45 -relwidth {} -height 19 \
        -relheight {} -anchor nw -bordermode ignore 
    entry .vTcl.color.frame11.entry10 \
         \
        -highlightthickness 0 -textvariable vTcl(color,green) 
    place .vTcl.color.frame11.entry10 \
        -x 250 -relx 0 -y 30 -rely 0 -width 45 -relwidth {} -height 19 \
        -relheight {} -anchor nw -bordermode ignore 
    button .vTcl.color.button11 \
        -command {
            destroy .vTcl.color
        } \
         \
        -highlightthickness 0 -padx 9 -pady 3 -text OK 
    place .vTcl.color.button11 \
        -x 5 -relx 0 -y 120 -rely 0 -width 146 -relwidth {} -height 24 \
        -relheight {} -anchor nw -bordermode ignore 
    button .vTcl.color.button12 \
        -command {
            set vTcl(color) -1
            destroy .vTcl.color
        } \
        -highlightthickness 0 -padx 9 -pady 3 -text Cancel 
    place .vTcl.color.button12 \
        -x 160 -relx 0 -y 120 -rely 0 -width 146 -relwidth {} -height 24 \
        -relheight {} -anchor nw -bordermode ignore 
}

