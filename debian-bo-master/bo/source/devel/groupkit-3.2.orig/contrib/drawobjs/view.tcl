
proc gk_objview {view canvas model} {
  gk_newenv $view
  $view model $model
  $view canvas $canvas

  $view command set _newobj "_gk_objview_new"
  $view command set _create "_gk_objview_create"
  $view command set _setbindings "_gk_objview_setbindings"
  $view command set _delobj "_gk_objview_del"
  $view command set _attrchanged "_gk_objview_changeattr"
  $view command set addtype "_gk_objview_addtype"
  $view command set _startsweep _gk_objview_startsweep
  $view command set _continuesweep _gk_objview_continuesweep
  $view command set _endsweep _gk_objview_endsweep
  $view command set newobjtype _gk_objview_newobjtype
  $view command set select _gk_objview_select
  $view command set addhandle _gk_objview_addhandle
  $view command set movehandle _gk_objview_movehandle
  $view command set _draghandle _gk_objview_draghandle
  $view command set objectmoved _gk_objview_objmoved

  $model bind newObject "$view _newobj %I %T"
  $model bind deletedObject "$view _delobj %I"
  $model bind attrChanged "$view _attrchanged %I %A %V"

  bind $canvas <1> "$view _startsweep %x %y"
  bind $canvas <B1-Motion> "$view _continuesweep %x %y"
  bind $canvas <B1-ButtonRelease> "$view _endsweep %x %y"
}

proc _gk_objview_newobjtype {view cmd type} {
    $view set new_object_type $type
}

proc _gk_objview_startsweep {view cmd x y} {
    if {[$view am_dragging]=="yes"} {return}
    set canvas [$view canvas]
    set _x [$canvas canvasx $x]; set _y [$canvas canvasy $y]
    if {[$canvas find overlapping [expr $_x-3] [expr $_y-3] [expr $_x+3] [expr $_y+3]]!=""} {return}
    set type [$view new_object_type]
    set prefix [$view types.$type.prefix]
    $view drag_objid ""
    if {$prefix!=""} {
	$view drag_objid [${prefix}_start $view $type $_x $_y]
    }
}
proc _gk_objview_continuesweep {view cmd x y} {
    if {([$view am_dragging]=="yes")||([$view drag_objid]=="")} {return}
    set objid [$view drag_objid]
    set canvas [$view canvas]
    set prefix [$view obj.$objid.prefix]
    if {$prefix!=""} {
	${prefix}_sweep $view $objid [$canvas canvasx $x] [$canvas canvasy $y]
    }
}
proc _gk_objview_endsweep {view cmd x y} {
    if {([$view am_dragging]=="yes")||([$view drag_objid]=="")} {return}
    $view select replace [$view drag_objid]
    $view drag_objid ""
}

proc _gk_objview_new {view cmd id type} {
  set c [$view canvas]
  $view obj.$id.type $type
  $view obj.$id.prefix [$view types.$type.prefix]
  set canvasid [$view _create $c $id $type]
  $view obj.$id.canvasid $canvasid
  $view _setbindings $c $id $canvasid
}

proc _gk_objview_create {view cmd canvas id type} {
   set prefix [$view types.$type.prefix]
   if {$prefix!=""} {
       ${prefix}_newview $view $id
   }
   [$view canvas] bind $id <1> "$view select replace $id"
   return $id
}

proc _gk_objview_setbindings {view cmd canvas id canvasid} {
  # no default bindings
}

proc _gk_objview_del {view cmd id} {
  set c [$view canvas]
  $c delete [$view obj.$id.canvasid]
  $view delete obj.$id
}

proc _gk_objview_changeattr {view cmd objid attr val} {
    set prefix [$view obj.$objid.prefix]
    if {$prefix!=""} {
	${prefix}_attributeChanged $view $objid $attr $val
    }
}

proc _gk_objview_addtype {view cmd type prefix} {
    $view types.$type.prefix $prefix
}

proc _gk_objview_select {view cmd qualifier objid} {
    switch $qualifier {
	"replace"   {
	    $view $cmd removeall dummy
	    $view $cmd append $objid
	}
	"removeall" {
	    foreach i [$view selected_objects] {
		$view $cmd deselect $i
	    }
	}
	"deselect" {
	    foreach i [$view handles.$objid] {
		[$view canvas] delete $i
	    }
	    $view delete handles.$objid
	    set sel [$view selected_objects]
	    set posn [lsearch $sel $objid]
	    if {$posn!=-1} {
		$view selected_objects [lreplace $sel $posn $posn]
	    }
	}
	"append" {
	    set prefix [$view obj.$objid.prefix]
	    if {$prefix!=""} {
		${prefix}_createHandles $view $objid
	    }
	    set sel [$view selected_objects]
	    lappend sel $objid
	    $view selected_objects $sel
	}
    }
}


proc _gk_objview_addhandle {view cmd objid specifier x y} {
    set canvasid [[$view canvas] create rectangle [expr $x-3] [expr $y-3] \
	    [expr $x+3] [expr $y+3] -fill black \
	    -tags [list selection ${objid}_$specifier]]
    [$view canvas] bind $canvasid <1> "$view am_dragging yes"
    [$view canvas] bind $canvasid <B1-Motion> \
	    "$view _draghandle $objid $specifier %x %y"
    [$view canvas] bind $canvasid <B1-ButtonRelease> "$view am_dragging no"
    set handles [$view handles.$objid]
    lappend handles $canvasid
    $view handles.$objid $handles
}

proc _gk_objview_draghandle {view cmd objid specifier x y} {
    set prefix [$view obj.$objid.prefix]
    if {$prefix!=""} {
	${prefix}_handleDragged $view $objid $specifier \
		[[$view canvas] canvasx $x] [[$view canvas] canvasy $y]
    }
}

proc _gk_objview_movehandle {view cmd objid specifier x y} {
    set cid [[$view canvas] find withtag ${objid}_$specifier]
    [$view canvas] coords $cid [expr $x-3] [expr $y-3] [expr $x+3] [expr $y+3]
}

proc _gk_objview_objmoved {view cmd objid} {
    set prefix [$view obj.$objid.prefix]
    if {$prefix!=""} {
	${prefix}_repositionHandles $view $objid
    }
    schedule_refresh
}

set _idleset 0
proc schedule_refresh {} {  global _idleset
  if {$_idleset==0} {
      set _idleset 1
      after 100 "dorefresh"
  }
}
proc dorefresh {} {  global _idleset
    update idletasks
    set _idleset 0
}