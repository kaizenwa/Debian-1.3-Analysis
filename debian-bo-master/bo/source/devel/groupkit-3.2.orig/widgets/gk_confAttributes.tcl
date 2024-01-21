set _gk_conf_debug_attributes [list filedesc userid confnum usernum regport reghost originator conftype confname will_persist color]

proc gk_confAttributes {w user args} {
  eval gk_avlist $w $args
  global _gk_confpart_debugging _gk_conf_debug_attributes
  if {$user==[users local.usernum]} {
    foreach i [users keys local] {
      if {[users keys local.$i]==""} {
	if {($_gk_confpart_debugging) || (![member $i $_gk_conf_debug_attributes])} {
          $w addattr $i [users local.$i]
        }
      } else {
	foreach j [users keys local.$i] {
	  $w addattr $j [users local.$i.$j]
        }
      }
    }
  } else {
    foreach i [users keys remote.$user] {
      if {[users keys remote.$user.$i]==""} {
        if {($_gk_confpart_debugging) || (![member $i $_gk_conf_debug_attributes])} {
          $w addattr $i [users remote.$user.$i]
	}
      } else {
	foreach j [users keys remote.$user.$i] {
	  $w addattr $j [users remote.$user.$i.$j]
	}
      }
    }
  }
  return $w  
}

proc gk_toplevelConfAttributes {w usernum} {
  if {[winfo exists $w]} {
    destroy $w
  }
  toplevel $w
  wm title $w "Information on [gk_getUserAttrib $usernum username]"
  pack [label $w.lbl -text "Information on [gk_getUserAttrib $usernum username]"]
  pack [gk_confAttributes $w.users $usernum]
  pack [frame $w.line -height 4 -borderwidth 2 -relief sunken] -fill x -pady 4
  pack [button $w.ok -text Dismiss -command "destroy $w"]
}
