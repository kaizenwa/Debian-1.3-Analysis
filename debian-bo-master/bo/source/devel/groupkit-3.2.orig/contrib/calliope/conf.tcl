# new routines to add to conf.tcl

# serialize to others
proc gk_serializeToOthers args {
  if {[users local.usernum]==[_gk_getUniqueUser]} {
    eval gk_toOthers $args
  } else {
    eval gk_toUserNum [_gk_getUniqueUser] gk_toAllBut [users local.usernum] \
      $args
  }
}

proc gk_toAllBut {usernum args} { 
    foreach i [users keys remote] { 
      if {$i != $usernum} {
        _gk_queueSend [users remote.$i.filedesc] $args
      }
    }
    if {[users local.usernum]!=$usernum} {
      uplevel #0 $args 
    }
}
