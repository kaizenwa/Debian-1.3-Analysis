##############################################################################
#
# bind.tcl - procedures to control widget bindings
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

proc vTclWindow(post).vTcl.bind {args} {
    vTcl:setup_vTcl:bind .vTcl.bind
}

proc vTcl:add_bind {} {
global vTcl
    bind $vTcl(w,widget) $vTcl(bind,event) [string trimright [.vTcl.bind.fra9.fra16.tex21 get 0.0 end]]
    vTcl:get_bind $vTcl(w,widget)
}

proc vTcl:delete_bind {} {
global vTcl
    if {$vTcl(bind,event) != ""} {
        bind $vTcl(w,widget) $vTcl(bind,event) ""
    }
    vTcl:get_bind $vTcl(w,widget)
}

proc vTcl:get_bind {target} {
global vTcl
    if {[winfo exists .vTcl.bind] == 1} {
        set vTcl(bind,list) [bind $target]
        .vTcl.bind.fra9.ent14 delete 0 end
        .vTcl.bind.fra9.fra16.tex21 delete 0.0 end
        .vTcl.bind.fra9.fra9.lis10 delete 0 end
        foreach i $vTcl(bind,list) {
            .vTcl.bind.fra9.fra9.lis10 insert end $i
        }
        set vTcl(bind,tags) $vTcl(bindtags,$target)
        set vTcl(bind,event) ""
    }
}

proc vTcl:select_bind {} {
global vTcl
    .vTcl.bind.fra9.fra16.tex21 delete 0.0 end
    set listnum [.vTcl.bind.fra9.fra9.lis10 curselection]
    if {$listnum != ""} {
        set vTcl(bind,event) [.vTcl.bind.fra9.fra9.lis10 get $listnum]
        .vTcl.bind.fra9.fra16.tex21 insert end [bind $vTcl(w,widget) $vTcl(bind,event)]
    }
}

proc vTcl:update_bind {} {
global vTcl
    set listnum [.vTcl.bind.fra9.fra9.lis10 curselection]
    if {$listnum != ""} {
        bind $vTcl(w,widget) $vTcl(bind,event) [string trimright [.vTcl.bind.fra9.fra16.tex21 get 0.0 end]]
    }
    vTcl:get_bind $vTcl(w,widget)
}

proc vTclWindow.vTcl.bind {args} {
    set base .vTcl.bind
    if {[winfo exists .vTcl.bind]} {
        wm deiconify .vTcl.bind; return
    }
    toplevel .vTcl.bind -class vTcl
    wm focusmodel .vTcl.bind passive
    wm geometry .vTcl.bind 416x351+79+162
    wm maxsize .vTcl.bind 1137 870
    wm minsize .vTcl.bind 415 350
    wm overrideredirect .vTcl.bind 0
    wm resizable .vTcl.bind 0 0
    wm deiconify .vTcl.bind
    wm title .vTcl.bind "Widget Bindings"
    frame .vTcl.bind.fra9 \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    place .vTcl.bind.fra9 \
        -x 5 -relx 0 -y 5 -rely 0 -width 405 -relwidth {} -height 340 \
        -relheight {} -anchor nw -bordermode ignore 
    frame .vTcl.bind.fra9.fra12 \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    place .vTcl.bind.fra9.fra12 \
        -x 5 -relx 0 -y 190 -rely 0 -width 395 -relwidth {} -height 30 \
        -relheight {} -anchor nw -bordermode ignore 
    button .vTcl.bind.fra9.fra12.but13 \
        -command vTcl:add_bind \
         \
        -highlightthickness 0 -padx 9 -pady 3 -text Add -width 5 
    pack .vTcl.bind.fra9.fra12.but13 \
        -in .vTcl.bind.fra9.fra12 -anchor center -expand 1 -fill both \
        -ipadx 0 -ipady 0 -padx 2 -pady 2 -side left 
    button .vTcl.bind.fra9.fra12.but14 \
        -command vTcl:update_bind \
         \
        -highlightthickness 0 -padx 9 -pady 3 -text Update -width 5 
    pack .vTcl.bind.fra9.fra12.but14 \
        -in .vTcl.bind.fra9.fra12 -anchor center -expand 1 -fill both \
        -ipadx 0 -ipady 0 -padx 2 -pady 2 -side left 
    button .vTcl.bind.fra9.fra12.but15 \
        -command vTcl:delete_bind \
         \
        -highlightthickness 0 -padx 9 -pady 3 -text Delete -width 5 
    pack .vTcl.bind.fra9.fra12.but15 \
        -in .vTcl.bind.fra9.fra12 -anchor center -expand 1 -fill both \
        -ipadx 0 -ipady 0 -padx 2 -pady 2 -side left 
    button .vTcl.bind.fra9.fra12.but16 \
        -command {destroy .vTcl.bind} \
        -highlightthickness 0 -padx 9 -pady 3 -text Done -width 5 
    pack .vTcl.bind.fra9.fra12.but16 \
        -in .vTcl.bind.fra9.fra12 -anchor center -expand 1 -fill both \
        -ipadx 0 -ipady 0 -padx 2 -pady 2 -side left 
    frame .vTcl.bind.fra9.fra9 \
        -height 30 -width 30 
    place .vTcl.bind.fra9.fra9 \
        -x 5 -relx 0 -y 225 -rely 0 -width 395 -relwidth {} -height 110 \
        -relheight {} -anchor nw -bordermode ignore 
    listbox .vTcl.bind.fra9.fra9.lis10 \
         \
        -highlightthickness 0 \
        -yscrollcommand {.vTcl.bind.fra9.fra9.scr11 set} 
    pack .vTcl.bind.fra9.fra9.lis10 \
        -in .vTcl.bind.fra9.fra9 -anchor center -expand 1 -fill both -ipadx 0 \
        -ipady 0 -padx 0 -pady 0 -side left 
    bind .vTcl.bind.fra9.fra9.lis10 <Double-Button-1> {
       vTcl:select_bind
    }
    scrollbar .vTcl.bind.fra9.fra9.scr11 \
        -borderwidth 1 -command {.vTcl.bind.fra9.fra9.lis10 yview} \
        -highlightthickness 0 -width 10 
    pack .vTcl.bind.fra9.fra9.scr11 \
        -in .vTcl.bind.fra9.fra9 -anchor center -expand 0 -fill y -ipadx 0 \
        -ipady 0 -padx 0 -pady 0 -side right 
    entry .vTcl.bind.fra9.ent14 \
         \
        -highlightthickness 0 -textvariable vTcl(bind,scratch) 
    bind .vTcl.bind.fra9.ent14 <KeyPress> {
        append vTcl(bind,event) "<Key-%K>"
    }
    place .vTcl.bind.fra9.ent14 \
        -x 75 -relx 0 -y 5 -rely 0 -width 60 -relwidth {} -height 20 \
        -relheight {} -anchor nw -bordermode ignore 
    entry .vTcl.bind.fra9.ent15 \
         \
        -highlightthickness 0 -textvariable vTcl(bind,event) 
    place .vTcl.bind.fra9.ent15 \
        -x 60 -relx 0 -y 143 -rely 0 -width 340 -relwidth {} -height 20 \
        -relheight {} -anchor nw -bordermode ignore 
    frame .vTcl.bind.fra9.fra16 \
        -height 30 -width 30 
    place .vTcl.bind.fra9.fra16 \
        -x 5 -relx 0 -y 50 -rely 0 -width 395 -relwidth {} -height 89 \
        -relheight {} -anchor nw -bordermode ignore 
    scrollbar .vTcl.bind.fra9.fra16.scr20 \
        -borderwidth 1 -command {.vTcl.bind.fra9.fra16.tex21 yview} \
        -highlightthickness 0 -width 10 
    pack .vTcl.bind.fra9.fra16.scr20 \
        -in .vTcl.bind.fra9.fra16 -anchor center -expand 0 -fill y -ipadx 0 \
        -ipady 0 -padx 0 -pady 0 -side right 
    text .vTcl.bind.fra9.fra16.tex21 \
         \
        -highlightthickness 0 \
        -yscrollcommand {.vTcl.bind.fra9.fra16.scr20 set} 
    pack .vTcl.bind.fra9.fra16.tex21 \
        -in .vTcl.bind.fra9.fra16 -anchor center -expand 1 -fill both \
        -ipadx 0 -ipady 0 -padx 0 -pady 0 -side top 
    label .vTcl.bind.fra9.lab23 \
         \
        -relief groove -text {Type -->} 
    place .vTcl.bind.fra9.lab23 \
        -x 5 -relx 0 -y 5 -rely 0 -width 65 -relwidth {} -height 20 \
        -relheight {} -anchor nw -bordermode ignore 
    label .vTcl.bind.fra9.lab24 \
         \
        -relief groove -text Command 
    place .vTcl.bind.fra9.lab24 \
        -x 5 -relx 0 -y 28 -rely 0 -width 395 -relwidth {} -height 20 \
        -relheight {} -anchor nw -bordermode ignore 
    label .vTcl.bind.fra9.lab25 \
         \
        -relief groove -text Event 
    place .vTcl.bind.fra9.lab25 \
        -x 5 -relx 0 -y 143 -rely 0 -width 50 -relwidth {} -height 20 \
        -relheight {} -anchor nw -bordermode ignore 
    label .vTcl.bind.fra9.lab26 \
         \
        -relief groove -text Tags 
    place .vTcl.bind.fra9.lab26 \
        -x 5 -relx 0 -y 166 -rely 0 -width 50 -relwidth {} -height 20 \
        -relheight {} -anchor nw -bordermode ignore 
    entry .vTcl.bind.fra9.ent27 \
         \
        -highlightthickness 0 -textvariable vTcl(bind,tags) 
    place .vTcl.bind.fra9.ent27 \
        -x 60 -relx 0 -y 166 -rely 0 -width 340 -relwidth {} -height 20 \
        -relheight {} -anchor nw -bordermode ignore 
    menubutton .vTcl.bind.fra9.men1 \
        -borderwidth 1 \
         \
        -menu .vTcl.bind.fra9.men1.m -padx 5 -pady 4 -relief raised \
        -text Mouse 
    place .vTcl.bind.fra9.men1 \
        -x 145 -relx 0 -y 5 -rely 0 -width 60 -relwidth {} -height 20 \
        -relheight {} -anchor nw -bordermode ignore 
    menu .vTcl.bind.fra9.men1.m \
         -tearoff 0 
    .vTcl.bind.fra9.men1.m add cascade \
        -label Press -menu .vTcl.bind.fra9.men1.m.press -state normal \
        -underline -1 
    .vTcl.bind.fra9.men1.m add cascade \
        -label Release -menu .vTcl.bind.fra9.men1.m.release -state normal \
        -underline -1 
    .vTcl.bind.fra9.men1.m add command \
        -command {append vTcl(bind,event) "<Motion>"} -label Motion \
        -state normal -underline -1 
    menu .vTcl.bind.fra9.men1.m.press \
         -tearoff 0 
    .vTcl.bind.fra9.men1.m.press add command \
        -command {append vTcl(bind,event) "<Button-1>"} -label 1 \
        -state normal -underline -1 
    .vTcl.bind.fra9.men1.m.press add command \
        -command {append vTcl(bind,event) "<Button-2>"} -label 2 \
        -state normal -underline -1 
    .vTcl.bind.fra9.men1.m.press add command \
        -command {append vTcl(bind,event) "<Button-3>"} -label 3 \
        -state normal -underline -1 
    .vTcl.bind.fra9.men1.m.press add command \
        -command {append vTcl(bind,event) "<Button>"} -label any \
        -state normal -underline -1 
    menu .vTcl.bind.fra9.men1.m.release \
         -tearoff 0 
    .vTcl.bind.fra9.men1.m.release add command \
        -command {append vTcl(bind,event) "<ButtonRelease-1>"} -label 1 \
        -state normal -underline -1 
    .vTcl.bind.fra9.men1.m.release add command \
        -command {append vTcl(bind,event) "<ButtonRelease-2>"} -label 2 \
        -state normal -underline -1 
    .vTcl.bind.fra9.men1.m.release add command \
        -command {append vTcl(bind,event) "<ButtonRelease-3>"} -label 3 \
        -state normal -underline -1 
    .vTcl.bind.fra9.men1.m.release add command \
        -command {append vTcl(bind,event) "<ButtonRelease>"} -label any \
        -state normal -underline -1 
    menubutton .vTcl.bind.fra9.men2 \
        -borderwidth 1 \
         \
        -menu .vTcl.bind.fra9.men2.m -padx 5 -pady 4 -relief raised -text Key 
    place .vTcl.bind.fra9.men2 \
        -x 210 -relx 0 -y 5 -rely 0 -width 60 -relwidth {} -height 20 \
        -relheight {} -anchor nw -bordermode ignore 
    menu .vTcl.bind.fra9.men2.m \
         -tearoff 0 
    .vTcl.bind.fra9.men2.m add command \
        -label Press -state normal -underline -1 -command {
            append vTcl(bind,event) "<KeyPress>"
        }
    .vTcl.bind.fra9.men2.m add command \
        -label Release -state normal -underline -1 -command {
            append vTcl(bind,event) "<KeyRelease>"
        }
    menubutton .vTcl.bind.fra9.men3 \
        -borderwidth 1 \
         -padx 5 \
        -pady 4 -relief raised -text Mod 
    place .vTcl.bind.fra9.men3 \
        -x 275 -relx 0 -y 5 -rely 0 -width 60 -relwidth {} -height 20 \
        -relheight {} -anchor nw -bordermode ignore 
    menubutton .vTcl.bind.fra9.men4 \
        -borderwidth 1 \
         -padx 5 \
        -pady 4 -relief raised -text Window 
    place .vTcl.bind.fra9.men4 \
        -x 340 -relx 0 -y 5 -rely 0 -width 60 -relwidth {} -height 20 \
        -relheight {} -anchor nw -bordermode ignore 
}
