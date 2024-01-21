proc stdrect_start {view type x y} {  
    set objid [[$view model] addobj $type]
    [$view model] setattr $objid x0 $x
    [$view model] setattr $objid y0 $y
    [$view model] setattr $objid x1 $x
    [$view model] setattr $objid y1 $y
    return $objid
}

proc stdrect_sweep {view objid x y} {
    [$view model] setattr $objid x1 $x
    [$view model] setattr $objid y1 $y
}

proc stdrect_newview {view objid} {
    stdrect_getcoords $view $objid x0 y0 x1 y1
    [$view canvas] create rectangle $x0 $y0 $x1 $y1 -tags [list $objid object]
}

proc stdrect_createHandles {view objid} {
    stdrect_getcoords $view $objid x0 y0 x1 y1
    $view addhandle $objid tl $x0 $y0
    $view addhandle $objid bl $x0 $y1
    $view addhandle $objid tr $x1 $y0
    $view addhandle $objid br $x1 $y1
} 

proc stdrect_repositionHandles {view objid} {
    stdrect_getcoords $view $objid x0 y0 x1 y1
    $view movehandle $objid tl $x0 $y0
    $view movehandle $objid bl $x0 $y1
    $view movehandle $objid tr $x1 $y0
    $view movehandle $objid br $x1 $y1
}

proc stdrect_handleDragged {view objid handle x y} {
    set model [$view model]
    switch $handle {
	tl {$model setattr $objid x0 $x; $model setattr $objid y0 $y}
	bl {$model setattr $objid x0 $x; $model setattr $objid y1 $y}
	tr {$model setattr $objid x1 $x; $model setattr $objid y0 $y}
	br {$model setattr $objid x1 $x; $model setattr $objid y1 $y}
    }
}

proc stdrect_attributeChanged {view objid attr val} {
    if {[member $attr [list x0 y0 x1 y1]]} {
	stdrect_getcoords $view $objid x0 y0 x1 y1
	[$view canvas] coords $objid $x0 $y0 $x1 $y1
	$view objectmoved $objid
    }
}

proc stdrect_getcoords {view objid _x0 _y0 _x1 _y1} {
  upvar $_x0 x0 $_y0 y0 $_x1 x1 $_y1 y1
  set model [$view model]
  set x0 [$model getattr $objid x0]; if {$x0==""} {set x0 0}
  set y0 [$model getattr $objid y0]; if {$y0==""} {set y0 0}
  set x1 [$model getattr $objid x1]; if {$x1==""} {set x1 0}
  set y1 [$model getattr $objid y1]; if {$y1==""} {set y1 0}
}
