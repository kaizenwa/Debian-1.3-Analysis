proc gk_radar {w args} {
    eval gkInt_CreateWidget $w gkRadar gkRadar $args
    return $w
}

proc gkRadar_CreateClassRec {} {
    global gkRadar
    set gkRadar(inherit) {frame}
    set gkRadar(methods) {adduser attributechanged component deleteuser moveuser}
    set gkRadar(options) {-scrollid -color}
    set gkRadar(-scrollid) {-scrollid scrollId ScrollId gkGroupScroll}
    set gkRadar(-color) {-color color Color {}}
}

proc gkRadar_ConstructWidget {w} {
    upvar #0 $w gkradar
    
    set gkradar(local) [users local.usernum]

    # use the gk_views widget (environment) and initializes this users
    # coordinates
    gk_views $w $gkradar(-scrollid)

    # Make the widget   
    pack [canvas $w.canvas] -side right -fill y
}

proc gkRadar_Config {w option args} {
    upvar #0 $w gkradar
    switch -exact [string range $option 1 end] {
	scrollid { gk_viewsCopy $w $gkradar(-scrollid) $args }
	color {gk_viewsSetAttribute $gkradar(-scrollid) color $args}
	default {$w.canvas config $option $args }
    }
}

proc gkRadar_Methods {w command args} {
  upvar #0 $w gkradar
  set args [lindex $args 0]
  switch -exact $command {
      adduser { 
	  set id [lindex $args 0]
	  set who [lindex $args 1]
	  set coords [lindex $args 2]
	  eval gkRadar_add $w $id $who $coords
      }
      attributechanged { eval gkRadar_attrchanged $w $args }
      deleteuser { eval gkRadar_delete $w $args }
      moveuser { 
	  set id [lindex $args 0]
	  set who [lindex $args 1]
	  set coords [lindex $args 2]
	  eval gkRadar_move $w $id $who $coords
      }
      component {gkRadar_component $w $args}
      default {$gkradar(rootCmd) $command $args}
  }
}

proc gkRadar_add {w id who x0 y0 x1 y1} {
    upvar #0 $w gkradar
    if {$x1>1} {set x1 1}
    set width [winfo width $w.canvas]
    if {$width==1} {set width [winfo reqwidth $w.canvas]}
    set height [winfo height $w.canvas]
    if {$height==1} {set height [winfo reqheight $w.canvas]}
    set x0 [expr $x0*$width]
    set y0 [expr $y0*$height]
    set x1 [expr $x1*$width]
    set y1 [expr $y1*$height]
    set color [gk_viewsGetAttribute $id $who color]
    if {$color==""} {set color black}
    $w.canvas create rectangle $x0 $y0 $x1 $y1 -tags tag$who \
	    -fill $color -stipple gray25
}

proc gkRadar_move {w id who x0 y0 x1 y1} {
    upvar #0 $w gkradar
    if {[$w.canvas find withtag tag$who]==""} {
	gkRadar_add $w $id $who $x0 $y0 $x1 $y1
    } else {
	if {$x1>1} {set x1 1}
	set width [winfo width $w.canvas]
	if {$width==1} {set width [winfo reqwidth $w.canvas]}
	set height [winfo height $w.canvas]
	if {$height==1} {set height [winfo reqheight $w.canvas]}
	set x0 [expr $x0*$width]
	set y0 [expr $y0*$height]
	set x1 [expr $x1*$width]
	set y1 [expr $y1*$height]
	$w.canvas coords tag$who $x0 $y0 $x1 $y1
    }
}

proc gkRadar_attrchanged {w id who attr color} {
    upvar #0 $w gkradar
    if {$attr!="color"} {return}
    if {[$w.canvas find withtag tag$who]!=""} {
	$w.canvas itemconfig tag$who -fill $color
    }
}

proc gkRadar_delete {w id who} {
    upvar #0 $w gkradar
    if {[$w.canvas find withtag tag$who]!=""} {
	$w.canvas delete tag$who
    }
}


proc gkRadar_component {w stuff} {
    upvar #0 $w gkradar

    switch [lindex $stuff 0] {
	canvas {return $w.canvas}
	default {error "no such component"}
    }
}






