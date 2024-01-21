proc gk_objmodel name {
  gk_newenv -bind -share $name
  $name command set addobj "_gk_drawobjs_addobj"
  $name command set delobj "_gk_drawobjs_delobj"
  $name command set setattr "_gk_drawobjs_setattr"
  $name command set getattr "_gk_drawobjs_getattr"
  $name misc.nextid 0
  $name bind addEnvInfo "_gk_drawobjs_infoAdded $name %K"
  $name bind changeEnvInfo "_gk_drawobjs_infoChanged $name %K"
  $name bind deleteEnvInfo "_gk_drawobjs_infoDeleted $name %K"
  $name bind envReceived "_gk_drawobjs_infoReceived $name"
  return $name
}

proc _gk_drawobjs_addobj {env cmd type} {
  set id [$env misc.nextid];  $env set misc.nextid [expr $id+1]
  set objid [users local.usernum]x$id
  $env obj.$objid.type $type
  return $objid
}

proc _gk_drawobjs_delobj {env cmd objid} {
  $env delete obj.$objid
}

proc _gk_drawobjs_setattr {env cmd objid attr val} {
  $env obj.$objid.$attr $val
}

proc _gk_drawobjs_getattr {env cmd objid attr} {
  return [$env obj.$objid.$attr]
}

proc _gk_drawobjs_infoAdded {env key} {
  set pieces [split $key .]
  if {([llength $pieces]==3)&&([lindex $pieces 0]=="obj")} {
    set id [lindex $pieces 1]
    set notifier [$env option get binding_table]
    if {[lindex $pieces 2]=="type"} {
      set type [$env obj.$id.type]
      $notifier notify newObject [list [list I $id] [list T $type]]
    } else {
      set attr [lindex $pieces 2]
      set val [$env $key]
      $notifier notify attrChanged [list [list I $id] [list A $attr] \
				    [list V $val]]
    }
  }
}

proc _gk_drawobjs_infoChanged {env key} {
  set pieces [split $key .]
  if {([llength $pieces]==3)&&([lindex $pieces 0]=="obj")} {
    set id [lindex $pieces 1]
    set notifier [$env option get binding_table]
    set attr [lindex $pieces 2]
    set val [$env $key]
    $notifier notify attrChanged [list [list I $id] [list A $attr] \
				    [list V $val]]
  }
}

proc _gk_drawobjs_infoDeleted {env key} {
  set pieces [split $key .]
  if {([llength $pieces]==2)&&([lindex $pieces 0]=="obj")} {
    set id [lindex $pieces 1]
    set notifier [$env option get binding_table]
    $notifier notify deletedObject [list [list I $id]]
  }
}

proc _gk_drawobjs_infoReceived {env} {
  foreach i [$env keys obj] {
    set type [$env obj.$i.type]
    set notifier [$env option get binding_table]
    $notifier notify newObject \
      [list [list I $i] [list T $type]]
  }
}
