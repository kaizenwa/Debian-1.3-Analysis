set _gk_rc_debug_attributes [list filedesc]

proc gk_regAttributes {w user args} {
  eval gk_avlist $w $args
  global _gk_regpart_debugging _gk_rc_debug_attributes
  foreach i [regclients keys remote.$user] {
    if {($_gk_regpart_debugging) || (![member $i $_gk_rc_debug_attributes])} {
      $w addattr $i [regclients remote.$user.$i]
    }
  }
  return $w  
}

proc gk_toplevelRegAttributes {w usernum} {
  if {[winfo exists $w]} {
    destroy $w
  }
  toplevel $w
  wm title $w "Information on [regclients remote.$usernum.name]"
  pack [label $w.lbl -text "Information on [regclients remote.$usernum.name]"]
  pack [gk_regAttributes $w.users $usernum]
  pack [frame $w.line -height 4 -borderwidth 2 -relief sunken] -fill x -pady 4
  pack [button $w.ok -text Dismiss -command "destroy $w"]
}
