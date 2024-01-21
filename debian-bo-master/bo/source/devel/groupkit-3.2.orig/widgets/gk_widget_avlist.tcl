


proc gk_avlist {w args} {
    eval gkInt_CreateWidget $w gkAVlist gkAVlist $args
    return $w
}

proc gkAVlist_CreateClassRec {} {
    global gkAVlist

    set gkAVlist(inherit) {label}
    set gkAVlist(methods) {addattr delattr changeattr component attributes}
}

proc gkAVlist_InitWidgetRec {w class className args} {
    upvar #0 $w gk_avlist
    set gk_avlist(attrs) {}
    set gk_avlist(nextid) 1
}

proc gkAVlist_ConstructWidget {w} {
    global gkAVlist gk_library
    upvar #0 $w gk_avlist
}

proc gkAVlist_Config {w option args} {
     upvar #0 $w gk_avlist
     switch -exact [string range $option 1 end] {
       default {$gk_avlist(rootCmd) configure $option $args}
     }
}

proc gkAVlist_Methods {w command args} {
  upvar #0 $w gk_avlist
  set args [lindex $args 0]
  switch -exact $command {
    addattr { gkAVlist_addAttr $w $args }
    delattr { gkAVlist_delAttr $w $args }
    changeattr {gkAVlist_changeAttr $w $args}
    attributes {gkAVlist_attributes $w $args}
    component { gkAVlist_component $w $args }
    default {$gk_avlist(rootCmd) $command $args}
  }
}

proc gkAVlist_addAttr {w stuff} {
  upvar #0 $w gk_avlist
  if {[llength $stuff]!=2} {error "wrong # args"}
  set attr [lindex $stuff 0]; set val [lindex $stuff 1];  
  foreach i $gk_avlist(attrs) {
    if {[keylget i attr]==$attr} {error "attribute already exists"}
  }
  set id $gk_avlist(nextid); incr gk_avlist(nextid)
  keylset entry attr $attr val $val id $id
  lappend gk_avlist(attrs) $entry
  frame $w.$id; pack $w.$id -in $w -side top
  label $w.$id.attr -text $attr -width 15 -anchor w
  label $w.$id.val -text $val -width 25 -anchor w
  pack $w.$id.attr -side left 
  pack $w.$id.val -side left
}

proc gkAVlist_delAttr {w stuff} {
  upvar #0 $w gk_avlist
  if {[llength $stuff]!=1} {error "wrong # args"}
  set attr [lindex $stuff 0]
  set idx 0; foreach i $gk_avlist(attrs) {
    if {[keylget i attr]==$attr} {
      set id [keylget i id]
      destroy $w.$id
      set gk_avlist(attrs) [lreplace $gk_avlist(attrs) $idx $idx]
      return
    }
    incr idx
  }
  error "no such attribute"
}

proc gkAVlist_changeAttr {w stuff} {
  upvar #0 $w gk_avlist
  if {[llength $stuff]!=2} {error "wrong # args"}
  set attr [lindex $stuff 0]; set val [lindex $stuff 1]
  foreach i $gk_avlist(attrs) {
    if {[keylget i attr]==$attr} {
      set id [keylget i id]
      $w.$id.val configure -text $val
      return
    }
  }
  error "no such attribute"
}

proc gkAVlist_component {w stuff} {
  switch [lindex $stuff 0] {
    default {error "no such component"}
  }
}

proc gkAVlist_attributes {w stuff} {
  upvar #0 $w gk_avlist
  set result ""
  foreach i $gk_avlist(attrs) {
    lappend result [keylget i attr]
  }
  return $result
}
