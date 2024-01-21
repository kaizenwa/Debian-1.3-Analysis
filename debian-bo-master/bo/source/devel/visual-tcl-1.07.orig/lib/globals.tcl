##############################################################################
#
# globals.tcl - global variables
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

global vTcl

set vTcl(action)         ""
set vTcl(action_index)   -1
set vTcl(action_limit)   -1
set vTcl(balloon,first)  0
set vTcl(balloon,on)     1
set vTcl(balloon,set)    0
set vTcl(balloon,soon)   0
set vTcl(change)         0
set vTcl(console)        0
set vTcl(cursor,last)    ""
set vTcl(procs)          "init main"
set vTcl(file,base)      [pwd]
set vTcl(file,mode)      ""
set vTcl(file,type)      "*.tcl"
set vTcl(grid,x)         5
set vTcl(grid,y)         5
set vTcl(gui,main)       ".vTcl"
set vTcl(gui,ae)         "$vTcl(gui,main).ae"
set vTcl(gui,command)    "$vTcl(gui,main).comm"
set vTcl(gui,console)    "$vTcl(gui,main).con"
set vTcl(gui,proc)       "$vTcl(gui,main).proc"
set vTcl(gui,proclist)   "$vTcl(gui,main).proclist"
set vTcl(gui,mgr)        "$vTcl(gui,main).mgr"
set vTcl(gui,prefs)      "$vTcl(gui,main).prefs"
set vTcl(gui,rc_menu)    "$vTcl(gui,main).rc"
set vTcl(gui,varlist)    "$vTcl(gui,main).varlist"
set vTcl(gui,statbar)    "$vTcl(gui,main).stat.f.bar"
set vTcl(gui,showlist)   ".vTcl.toolbar .vTcl.mgr .vTcl.ae"
set vTcl(h,exist)        no
set vTcl(h,size)         3
set vTcl(hide)           ""
set vTcl(item_num)       1
set vTcl(key,x)          1
set vTcl(key,y)          1
set vTcl(key,w)          1
set vTcl(key,h)          1
set vTcl(mgrs,update)    yes
set vTcl(pr,fullcfg)     0
set vTcl(pr,getname)     0
set vTcl(pr,manager)     place
set vTcl(pr,shortname)   1
set vTcl(proc,name)      ""
set vTcl(proc,args)      ""
set vTcl(proc,body)      ""
set vTcl(project,name)   ""
set vTcl(project,file)   ""
set vTcl(quit)           1
set vTcl(tool,list)      ""
set vTcl(tool,last)      ""
set vTcl(tops)           ""
set vTcl(undo)           ""
set vTcl(vars)           ""
set vTcl(var,name)       ""
set vTcl(var,value)      ""
set vTcl(var_update)     "yes"
set vTcl(w,alias)        ""
set vTcl(w,class)        ""
set vTcl(w,def_mgr)      $vTcl(pr,manager)
set vTcl(w,info)         ""
set vTcl(w,insert)       .
set vTcl(w,libs)         ""
set vTcl(w,manager)      ""
set vTcl(w,mgrs)         "grid pack place wm"
set vTcl(w,options)      ""
set vTcl(w,widget)       ""
set vTcl(winname)        "vTclWindow"

set vTcl(mode)           "EDIT"
set vTcl(pwd)            [pwd]
set vTcl(redo)           ""
set vTcl(save)           ""
set vTcl(tab)            "    "
set vTcl(tab2)           "$vTcl(tab)$vTcl(tab)"

set vTcl(cmpd,list)      ""
set vTcl(syscmpd,list)   ""

set vTcl(attr,tops)     "aspect command focusmodel geometry grid 
                         iconbitmap iconmask iconname iconposition 
                         iconwindow maxsize minsize overrideredirect 
                         resizable sizefrom state title"
set vTcl(attr,winfo)    "children class geometry height ismapped 
                         manager name parent rootx rooty toplevel 
                         width x y"

set vTcl(grid,insert)   ""  
set vTcl(pack,insert)   ""
set vTcl(place,insert)  "-x 5 -y 5 -bordermode ignore"

set vTcl(grid,attr)     "column row columnspan rowspan ipadx ipady
                         padx pady sticky"
set vTcl(pack,attr)     "anchor expand fill ipadx ipady padx pady side"
set vTcl(place,attr)    "x y width height relx rely relwidth relheight
                         anchor bordermode"

set vTcl(opt,list) "-activebackground -activeforeground -activerelief
    -anchor -aspect -borderwidth -background -bitmap -bordermode -class
    -closeenough -colormap -command -confine -cursor -disabledforeground
    -elementborderwidth -exportselection -foreground -font -from -height
    -highlightbackground -highlightcolor -highlightthickness -image
    -indicatoron -insertbackground -insertborderwidth -insertofftime
    -insertontime -insertwidth -jump -justify -menu -offvalue -onvalue -orient
    -padx -pady -relief -repeatdelay -repeatinterval -screen -scrollregion
    -selectbackground -selectborderwidth -selectcolor -selectforeground
    -selectimage -selectmode -setgrid -show -showvalue -sliderlength
    -sliderrelief -spacing1 -spacing2 -spacing3 -state -tabs -takefocus
    -text -textvariable -tickinterval -to -troughcolor -underline -value
    -variable -visual -width -wrap -wraplength -xscrollcommand 
    -xscrollincrement -yscrollcommand -yscrollincrement"

set vTcl(opt,-activebackground)    { {Active BG}     longname   color   {} }
set vTcl(opt,-activeforeground)    { {Active FG}     longname   color   {} }
set vTcl(opt,-activerelief)        { {Act. Relief}   longname   choice
    {flat groove raised ridge sunken} }
set vTcl(opt,-anchor)              { Anchor          longname   choice
    {n ne e se s sw w nw center} }
set vTcl(opt,-aspect)              { Aspect          longname   type    {} }
set vTcl(opt,-bd)                  { BorderWidth     longname   type    {} }
set vTcl(opt,-borderwidth)         { BorderWidth     longname   type    {} }
set vTcl(opt,-bg)                  { Background      longname   color   {} }
set vTcl(opt,-background)          { Background      longname   color   {} }
set vTcl(opt,-bitmap)              { Bitmap          longname   type    {} }
set vTcl(opt,-bordermode)          { BorderMode      longname   choice
    {inside ignore outside} }
set vTcl(opt,-class)               { Class           longname   type    {} }
set vTcl(opt,-closeenough)         { Closeness       longname   type    {} }
set vTcl(opt,-colormap)            { Colormap        longname   type    {} }
set vTcl(opt,-command)             { Command         longname   command {} }
set vTcl(opt,-confine)             { Confine         longname   choice
    {0 1} }
set vTcl(opt,-cursor)              { Cursor          longname   type    {} }
set vTcl(opt,-disabledforeground)  { {Disabled FG}   longname   color   {} }
set vTcl(opt,-elementborderwidth)  { {Element BW}    longname   type    {} }
set vTcl(opt,-exportselection)     { Export          longname   choice
    {0 1} }
set vTcl(opt,-fg)                  { Foreground      longname   color   {} }
set vTcl(opt,-foreground)          { Foreground      longname   color   {} }
set vTcl(opt,-font)                { Font            longname   type    {} }
set vTcl(opt,-from)                { From            longname   type    {} }
set vTcl(opt,-height)              { Height          longname   type    {} }
set vTcl(opt,-highlightbackground) { {Hi BG}         longname   color   {} }
set vTcl(opt,-highlightcolor)      { {Hi Color}      longname   color   {} }
set vTcl(opt,-highlightthickness)  { {Hi Thickns}    longname   type    {} }
set vTcl(opt,-image)               { Image           longname   type    {} }
set vTcl(opt,-indicatoron)         { Indicator       longname   choice
    {0 1} }
set vTcl(opt,-insertbackground)    { {Insert BG}     longname   color   {} }
set vTcl(opt,-insertborderwidth)   { {Insert BW}     longname   type    {} }
set vTcl(opt,-insertofftime)       { {Insert OffT}   longname   type    {} }
set vTcl(opt,-insertontime)        { {Insert OnT}    longname   type    {} }
set vTcl(opt,-insertwidth)         { {Insert WD}     longname   type    {} }
set vTcl(opt,-jump)                { Jump            longname   choice
    {0 1} }
set vTcl(opt,-justify)             { Justify         longname   choice
    {left right center} }
set vTcl(opt,-menu)                { Menu            longname   type    {} }
set vTcl(opt,-offvalue)            { {Off Value}     longname   type    {} }
set vTcl(opt,-onvalue)             { {On Value}      longname   type    {} }
set vTcl(opt,-orient)              { Orient          longname   choice
    {vertical horizontal} }
set vTcl(opt,-padx)                { {Pad X}         longname   type    {} }
set vTcl(opt,-pady)                { {Pad Y}         longname   type    {} }
set vTcl(opt,-relief)              { Relief          longname   choice
    {flat groove raised ridge sunken} }
set vTcl(opt,-repeatdelay)         { {Repeat Delay}  longname   type    {} }
set vTcl(opt,-repeatinterval)      { {Repeat Intvl}  longname   type    {} }
set vTcl(opt,-screen)              { Screen          longname   type    {} }
set vTcl(opt,-scrollregion)        { {Scroll Regn}   longname   type    {} }
set vTcl(opt,-selectbackground)    { {Sel. BG}       longname   color   {} }
set vTcl(opt,-selectborderwidth)   { {Sel. BW}       longname   type    {} }
set vTcl(opt,-selectcolor)         { {Sel. Color}    longname   color   {} }
set vTcl(opt,-selectforeground)    { {Sel. FG}       longname   color   {} }
set vTcl(opt,-selectimage)         { {Sel. Image}    longname   type    {} }
set vTcl(opt,-selectmode)          { {Sel. Mode}     longname   type    {} }
set vTcl(opt,-setgrid)             { {Set Grid}      longname   choice
    {0 1} }
set vTcl(opt,-show)                { Show            longname   type    {} }
set vTcl(opt,-showvalue)           { {Show Value}    longname   choice
    {0 1} }
set vTcl(opt,-sliderlength)        { SliderLen       longname   type    {} }
set vTcl(opt,-sliderrelief)        { SliderRelf      longname   choice
    {flat groove raised ridge sunken} }
set vTcl(opt,-spacing1)            { Spacing1        longname   type    {} }
set vTcl(opt,-spacing2)            { Spacing2        longname   type    {} }
set vTcl(opt,-spacing3)            { Spacing3        longname   type    {} }
set vTcl(opt,-state)               { State           longname   choice
    {normal active disabled} }
set vTcl(opt,-tabs)                { Tabs            longname   type    {} }
set vTcl(opt,-takefocus)           { {Take Focus}    longname   type    {} }
set vTcl(opt,-text)                { Text            longname   type    {} }
set vTcl(opt,-textvariable)        { {Text Var}      longname   type    {} }
set vTcl(opt,-tickinterval)        { {Tic Interval}  longname   type    {} }
set vTcl(opt,-to)                  { To              longname   type    {} }
set vTcl(opt,-troughcolor)         { {Trough Color}  longname   color   {} }
set vTcl(opt,-underline)           { Underline       longname   type    {} }
set vTcl(opt,-value)               { Value           longname   type    {} }
set vTcl(opt,-variable)            { Variable        longname   type    {} }
set vTcl(opt,-visual)              { Visual          longname   type    {} }
set vTcl(opt,-width)               { Width           longname   type    {} }
set vTcl(opt,-wrap)                { Wrap            longname   choice
    {char none word} }
set vTcl(opt,-wraplength)          { {Wrap Length}   longname   type    {} }
set vTcl(opt,-xscrollcommand)      { {X Scroll Cmd}  longname   type    {} }
set vTcl(opt,-xscrollincrement)    { {X Increment}   longname   type    {} }
set vTcl(opt,-yscrollcommand)      { {Y Scroll Cmd}  longname   type    {} }
set vTcl(opt,-yscrollincrement)    { {Y Increment}   longname   type    {} }

set vTcl(head,proj) [string trim {
#############################################################################
# Visual Tcl v$vTcl(version) Project
#
}]

set vTcl(head,vars) [string trim {
#################################
# GLOBAL VARIABLES
#
}]

set vTcl(head,procs) [string trim {
#################################
# USER DEFINED PROCEDURES
#
}]

set vTcl(head,gui) [string trim {
#################################
# VTCL GENERATED GUI PROCEDURES
#
}]

set vTcl(head,proc,widgets) "$vTcl(tab)###################
$vTcl(tab)# CREATING WIDGETS
$vTcl(tab)###################
"

set vTcl(head,proc,geometry) "$vTcl(tab)###################
$vTcl(tab)# SETTING GEOMETRY
$vTcl(tab)###################
"

