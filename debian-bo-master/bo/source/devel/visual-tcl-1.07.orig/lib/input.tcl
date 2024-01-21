##############################################################################
#
# input.tcl - procedures for prompting windowed string input
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

proc vTcl:get_string {title target {value ""}} {
    global vTcl
    set tmpname .vTcl.[vTcl:rename $target]
    set vTcl(x,$tmpname) ""
    vTcl:string_window $title $tmpname $value
    tkwait window $tmpname
    return $vTcl(x,$tmpname)
}

proc vTcl:set_string {base str} {
    global vTcl
    set vTcl(x,$base) $str
    grab release $base
    destroy $base
}

proc vTcl:snarf_string {base} {
    global vTcl
    vTcl:set_string $base "[$base.ent18 get]"
}

proc vTcl:string_window {title base {value ""}} {
    toplevel $base
    wm focusmodel $base passive
    wm geometry $base 225x49+288+216
    wm maxsize $base 500 870
    wm minsize $base 225 1
    wm overrideredirect $base 0
    wm resizable $base 1 0
    wm deiconify $base
    wm title $base "$title"
    entry $base.ent18 \
        -cursor {}  
    pack $base.ent18 \
        -in $base -anchor center -expand 0 -fill x -ipadx 0 -ipady 0 \
        -padx 0 -pady 0 -side top 
    frame $base.fra19 \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    pack $base.fra19 \
        -in $base -anchor center -expand 1 -fill both -ipadx 0 -ipady 0 \
        -padx 0 -pady 0 -side top 
    button $base.fra19.but20 \
        -command "vTcl:snarf_string \{$base\}" \
         -padx 9 \
        -pady 3 -text OK -width 5 
    pack $base.fra19.but20 \
        -in $base.fra19 -anchor center -expand 1 -fill x -ipadx 0 \
        -ipady 0 -padx 0 -pady 0 -side left 
    button $base.fra19.but21 \
        -command "
            $base.ent18 delete 0 end
            vTcl:set_string \{$base\} \{$value\}
        " \
         -padx 9 \
        -pady 3 -text Cancel -width 5 
    pack $base.fra19.but21 \
        -in $base.fra19 -anchor center -expand 1 -fill x -ipadx 0 \
        -ipady 0 -padx 0 -pady 0 -side left 
    bind $base <Key-Return> "vTcl:snarf_string \{$base\}; break"
    $base.ent18 insert end $value
    update idletasks
    focus $base.ent18
    grab $base
}

