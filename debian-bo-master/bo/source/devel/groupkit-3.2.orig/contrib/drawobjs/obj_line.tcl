proc stdline_start {view type x y} {  
    set objid [[$view model] addobj $type]
    [$view model] setattr $objid startpt [list $x $y]
    [$view model] setattr $objid endpt [list $x $y]
    return $objid
}

proc stdline_sweep {view objid x y} {
    [$view model] setattr $objid endpt [list $x $y]
}

proc stdline_newview {view objid} {
    stdline_getcoords $view $objid x0 y0 x1 y1
    [$view canvas] create line $x0 $y0 $x1 $y1 -tags [list $objid object]
}

proc stdline_createHandles {view objid} {
    stdline_getcoords $view $objid x0 y0 x1 y1
    $view addhandle $objid startpt $x0 $y0
    $view addhandle $objid endpt $x1 $y1
} 

proc stdline_repositionHandles {view objid} {
    stdline_getcoords $view $objid x0 y0 x1 y1
    $view movehandle $objid startpt $x0 $y0
    $view movehandle $objid endpt $x1 $y1
}

proc stdline_handleDragged {view objid handle x y} {
    switch $handle {
	startpt {[$view model] setattr $objid startpt [list $x $y]}
	endpt   {[$view model] setattr $objid endpt [list $x $y]}
    }
}

proc stdline_attributeChanged {view objid attr val} {
    if {($attr=="startpt")||($attr=="endpt")} {
	stdline_getcoords $view $objid x0 y0 x1 y1
	[$view canvas] coords $objid $x0 $y0 $x1 $y1
	$view objectmoved $objid
    }
}

proc stdline_getcoords {view objid _x0 _y0 _x1 _y1} {
  upvar $_x0 x0 $_y0 y0 $_x1 x1 $_y1 y1
  set startpt [[$view model] getattr $objid startpt]
  set endpt [[$view model] getattr $objid endpt]
  if {$startpt==""} {
      set x0 0; set y0 0
  } else {
      set x0 [lindex $startpt 0]; set y0 [lindex $startpt 1]
  }
  if {$endpt==""} {
      set x1 0; set y1 0
  } else {
      set x1 [lindex $endpt 0]; set y1 [lindex $endpt 1]
  }
}
