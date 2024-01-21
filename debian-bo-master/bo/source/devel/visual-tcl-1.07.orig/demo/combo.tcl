#############################################################################
# Visual Tcl v1.07 Project
#

#################################
# GLOBAL VARIABLES
#
global x_accel; set x_accel {}
global x_label; set x_label {}
global widget; 
#################################
# USER DEFINED PROCEDURES
#
proc init {argc argv} {

}

init $argc $argv


proc main {argc argv} {

}

proc Window {args} {
global vTcl
    set cmd [lindex $args 0]
    set name [lindex $args 1]
    set rest [lrange $args 2 end]
    if {$name == "" || $cmd == ""} {return}
    set exists [winfo exists $name]
    switch $cmd {
        show {
            if {[info procs vTclWindow(pre)$name] != ""} {
                vTclWindow(pre)$name $rest
            }
            if {[info procs vTclWindow$name] != ""} {
                vTclWindow$name
            }
            if {[info procs vTclWindow(post)$name] != ""} {
                vTclWindow(post)$name $rest
            }
        }
        hide    { if $exists {wm withdraw $name; return} }
        iconify { if $exists {wm iconify $name; return} }
        destroy { if $exists {destroy $name; return} }
    }
}

#################################
# VTCL GENERATED GUI PROCEDURES
#

proc vTclWindow. {args} {
    set base .
    ###################
    # CREATING WIDGETS
    ###################
    wm focusmodel . passive
    wm geometry . 1x1+0+0
    wm maxsize . 1137 870
    wm minsize . 1 1
    wm overrideredirect . 0
    wm resizable . 1 1
    wm withdraw .
    wm title . "Combo Demo Project"
    ###################
    # SETTING GEOMETRY
    ###################
}

proc vTclWindow.top22 {args} {
    set base .top22
    if {[winfo exists .top22]} {
        wm deiconify .top22; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel .top22 -class Toplevel
    wm focusmodel .top22 passive
    wm geometry .top22 239x203+130+159
    wm maxsize .top22 1137 870
    wm minsize .top22 1 1
    wm overrideredirect .top22 0
    wm resizable .top22 1 1
    wm deiconify .top22
    wm title .top22 "Geometry Combo"
    frame .top22.fra23 \
        -background #a0d9d9 -borderwidth 1 -height 108 -relief sunken \
        -width 93 
    button .top22.fra23.01 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -padx 9 -pady 3 -text We 
    button .top22.fra23.02 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -padx 9 -pady 3 -text are 
    button .top22.fra23.03 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -padx 9 -pady 3 -text placed 
    frame .top22.fra24 \
        -background #d9a0d9 -borderwidth 1 -height 30 -relief sunken \
        -width 30 
    button .top22.fra24.01 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -padx 9 -pady 3 -text We're 
    button .top22.fra24.02 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -padx 9 -pady 3 -text packed 
    frame .top22.fra25 \
        -background #d9d9a0 -borderwidth 1 -relief sunken -width 30 
    button .top22.fra25.01 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -padx 9 -pady 3 -text And -width 5 
    button .top22.fra25.02 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -padx 9 -pady 3 -text a -width 5 
    button .top22.fra25.03 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -padx 9 -pady 3 -text grid -width 5 
    button .top22.fra25.04 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -padx 9 -pady 3 -text this -width 5 
    button .top22.fra25.05 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -padx 9 -pady 3 -text is -width 5 
    button .top22.fra25.06 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -padx 9 -pady 3 -text layout -width 5 
    ###################
    # SETTING GEOMETRY
    ###################
    grid columnconf .top22 0 -weight 1
    grid columnconf .top22 1 -weight 1
    grid rowconf .top22 1 -weight 1
    grid .top22.fra23 \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -padx 5 -pady 5 \
        -sticky nesw 
    place .top22.fra23.01 \
        -x 10 -y 10 -anchor nw -bordermode ignore 
    place .top22.fra23.02 \
        -x 40 -y 40 -width 55 -height 24 -anchor nw -bordermode ignore 
    place .top22.fra23.03 \
        -x 20 -y 75 -anchor nw -bordermode ignore 
    grid .top22.fra24 \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -padx 5 -pady 5 \
        -sticky nesw 
    pack .top22.fra24.01 \
        -anchor center -expand 1 -fill both -padx 2 -pady 2 -side top 
    pack .top22.fra24.02 \
        -anchor center -expand 0 -fill x -padx 2 -pady 2 -side top 
    grid .top22.fra25 \
        -column 0 -row 1 -columnspan 2 -rowspan 1 -padx 5 -pady 5 \
        -sticky nesw 
    grid .top22.fra25.01 \
        -column 0 -row 0 -columnspan 1 -rowspan 1 -padx 2 -pady 2 
    grid .top22.fra25.02 \
        -column 0 -row 1 -columnspan 1 -rowspan 1 -padx 2 -pady 2 
    grid .top22.fra25.03 \
        -column 1 -row 1 -columnspan 1 -rowspan 1 -padx 2 -pady 2 
    grid .top22.fra25.04 \
        -column 1 -row 0 -columnspan 1 -rowspan 1 -padx 2 -pady 2 
    grid .top22.fra25.05 \
        -column 2 -row 0 -columnspan 1 -rowspan 1 -padx 2 -pady 2 
    grid .top22.fra25.06 \
        -column 2 -row 1 -columnspan 1 -rowspan 1 -padx 2 -pady 2 
}

Window show .
Window show .top22

main $argc $argv
