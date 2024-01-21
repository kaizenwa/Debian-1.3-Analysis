proc vTcl:widget_set_relief {relief} {
    global vTcl
    if {$vTcl(w,widget) == ""} {return}
    if [catch {$vTcl(w,widget) cget -relief}] {return}
    $vTcl(w,widget) conf -relief $relief
}

proc vTcl:widget_add_anchor {anchor} {
    global vTcl
    if {$vTcl(w,widget) == ""} {return}
    if [catch {set a [$vTcl(w,widget) cget -anchor]}] {return}
    if {$a == "center"} {
        $vTcl(w,widget) conf -anchor $anchor
    } else {
        foreach i {n s e w} {
            if {[string first $i $a] >= 0} {
                set $i $i
            } else {
                set $i ""
            }
        }
        if {[string first $anchor $a] >= 0} {
            set na ""
        } else {
            set na $anchor
        }
        switch $anchor {
            n -
            s { set new "${na}${e}${w}" }
            e -
            w { set new "${n}${s}${na}" }
        }
        if {$new == ""} {
            set new center
        }
        $vTcl(w,widget) conf -anchor $new
    }
    vTcl:attrbar_anchor $vTcl(w,widget)
}

proc vTcl:widget_set_fg {target} {
    global vTcl
    if {$vTcl(w,widget) == ""} {return}
    if [catch {set fg [$vTcl(w,widget) cget -foreground]}] {return}
    set vTcl(w,opt,-foreground) $fg
    vTcl:show_color $target -foreground vTcl(w,opt,-foreground)
}

proc vTcl:widget_set_bg {target} {
    global vTcl
    if {$vTcl(w,widget) == ""} {return}
    if [catch {set bg [$vTcl(w,widget) cget -background]}] {return}
    set vTcl(w,opt,-background) $bg
    vTcl:show_color $target -background vTcl(w,opt,-background)
}

proc vTcl:set_manager {mgr} {
    global vTcl
    foreach i $vTcl(w,mgrs) {
        if { $i == $mgr } { 
            $vTcl(mgrs,$i,widget) configure -relief sunken
        } else {
            $vTcl(mgrs,$i,widget) configure -relief raised
        }
    }
    set vTcl(w,def_mgr) $mgr
}

proc vTcl:attrbar_anchor {target} {
    if {[catch {set anch [$target cget -anchor]}] == 1} {
        set anch ""
    }
    if {$anch == "center"} {
        set anch ""
    }
    foreach i {"w .vTcl.attr.011.012" "e .vTcl.attr.011.013"
               "n .vTcl.attr.011.014" "s .vTcl.attr.011.015"} {
        set dir [lindex $i 0]
        set wdg [lindex $i 1]
        if {[string first $dir $anch] >= 0} {
            $wdg conf -relief sunken
        } else {
            $wdg conf -relief raised
        }
    }
}

proc vTcl:attrbar_color {target} {
    set dbg [lindex [. conf -bg] 3]
    if {[catch {set fg [$target cget -fg]}] == 1} {
        set fg $dbg
    } else {
        set fg [$target cget -fg]
    }
    if {[catch {set bg [$target cget -bg]}] == 1} {
        set fg $dbg
    } else {
        set bg [$target cget -bg]
    }
    catch {
        .vTcl.attr.010.lab41 conf -bg $fg
        .vTcl.attr.010.lab42 conf -bg $bg
    }
}

proc vTcl:attrbar {args} {
    global vTcl
    set base .vTcl
    frame .vTcl.attr \
        -borderwidth 1 -height 30 -relief sunken -width 30 
    pack .vTcl.attr \
        -anchor center -expand 1 -fill x -ipadx 0 -ipady 0 -padx 0 -pady 0 \
        -side top 
    frame .vTcl.attr.01 \
        -borderwidth 1 -height 20 -relief raised -width 20 
    pack .vTcl.attr.01 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 3 -pady 2 \
        -side left 
    label .vTcl.attr.01.02 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -relief sunken -width 14 -textvariable vTcl(w,opt,-text) \
        -anchor w
    bind .vTcl.attr.01.02 <ButtonPress> {
        vTcl:set_label $vTcl(w,widget)
    }
    vTcl:set_balloon .vTcl.attr.01.02 "label"
    pack .vTcl.attr.01.02 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    button .vTcl.attr.01.03 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -padx 4 -pady 1 -text ... -command {
            vTcl:set_command $vTcl(w,widget)
        }
    vTcl:set_balloon .vTcl.attr.01.03 "command"
    pack .vTcl.attr.01.03 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    frame .vTcl.attr.04 \
        -borderwidth 1 -height 20 -relief raised -width 20 
    pack .vTcl.attr.04 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 3 -pady 2 \
        -side left 
    frame .vTcl.attr.04.05 \
        -borderwidth 1 -height 20 -relief raised -width 20 
    pack .vTcl.attr.04.05 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    bind .vTcl.attr.04.05 <Button-1> {
        vTcl:widget_set_relief raised
    }
    frame .vTcl.attr.04.06 \
        -borderwidth 1 -height 20 -relief sunken -width 20 
    pack .vTcl.attr.04.06 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    bind .vTcl.attr.04.06 <Button-1> {
        vTcl:widget_set_relief sunken
    }
    frame .vTcl.attr.04.07 \
        -borderwidth 2 -height 20 -relief groove -width 20 
    pack .vTcl.attr.04.07 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    bind .vTcl.attr.04.07 <Button-1> {
        vTcl:widget_set_relief groove
    }
    frame .vTcl.attr.04.08 \
        -borderwidth 2 -height 20 -relief ridge -width 20 
    pack .vTcl.attr.04.08 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    bind .vTcl.attr.04.08 <Button-1> {
        vTcl:widget_set_relief raised
    }
    label .vTcl.attr.04.09 \
        -borderwidth 0 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* -text none 
    pack .vTcl.attr.04.09 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    bind .vTcl.attr.04.09 <Button-1> {
        vTcl:widget_set_relief flat
    }
    frame .vTcl.attr.010 \
        -borderwidth 1 -height 20 -relief raised -width 20 
    pack .vTcl.attr.010 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 3 -pady 2 \
        -side left 
    label .vTcl.attr.010.lab41 \
        -borderwidth 0 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* -padx 2 \
        -pady 3 -text FG 
    vTcl:set_balloon .vTcl.attr.010.lab41 "foreground"
    pack .vTcl.attr.010.lab41 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    bind .vTcl.attr.010.lab41 <Button-1> {
        vTcl:widget_set_fg %W
    }
    label .vTcl.attr.010.lab42 \
        -borderwidth 0 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* -padx 2 \
        -pady 3 -text BG 
    vTcl:set_balloon .vTcl.attr.010.lab42 "background"
    pack .vTcl.attr.010.lab42 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    bind .vTcl.attr.010.lab42 <Button-1> {
        vTcl:widget_set_bg %W
    }
    frame .vTcl.attr.011 \
        -borderwidth 1 -height 20 -relief raised -width 20 
    pack .vTcl.attr.011 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 3 -pady 2 \
        -side left 
    button .vTcl.attr.011.012 \
        -command {vTcl:widget_add_anchor w} \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -image anchor_w -padx 0 -pady 0 
    pack .vTcl.attr.011.012 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    button .vTcl.attr.011.013 \
        -command {vTcl:widget_add_anchor e} \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -image anchor_e -padx 0 -pady 0 
    pack .vTcl.attr.011.013 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    button .vTcl.attr.011.014 \
        -command {vTcl:widget_add_anchor n} \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -image anchor_n -padx 0 -pady 0 
    pack .vTcl.attr.011.014 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    button .vTcl.attr.011.015 \
        -command {vTcl:widget_add_anchor s} \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -image anchor_s -padx 0 -pady 0 
    pack .vTcl.attr.011.015 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    frame .vTcl.attr.016 \
        -borderwidth 1 -height 20 -relief raised -width 20 
    pack .vTcl.attr.016 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 3 -pady 2 \
        -side left 
    set vTcl(mgrs,grid,widget) .vTcl.attr.016.017
    button .vTcl.attr.016.017 \
        -command {vTcl:set_manager grid} \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -image mgr_grid -padx 0 -pady 0 
    pack .vTcl.attr.016.017 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    set vTcl(mgrs,pack,widget) .vTcl.attr.016.018
    button .vTcl.attr.016.018 \
        -command {vTcl:set_manager pack} \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -image mgr_pack -padx 0 -pady 0 
    pack .vTcl.attr.016.018 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    set vTcl(mgrs,place,widget) .vTcl.attr.016.019
    button .vTcl.attr.016.019 \
        -command {vTcl:set_manager place} \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* \
        -highlightthickness 0 -image mgr_place -padx 0 -pady 0 
    pack .vTcl.attr.016.019 \
        -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 -padx 2 -pady 2 \
        -side left 
    set vTcl(mgrs,wm,widget) .vTcl.attr.016.020
    button .vTcl.attr.016.020
}

