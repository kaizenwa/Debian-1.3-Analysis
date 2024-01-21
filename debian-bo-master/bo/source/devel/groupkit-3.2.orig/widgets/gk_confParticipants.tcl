set _gk_confpart_widgets ""
set _gk_confpart_debugging 0

proc gk_confParticipants {w args} {   global _gk_confpart_widgets
  eval gk_userlist $w $args
  _gk_confParticipant_addUser $w [users local.usernum]
  foreach i [users keys remote] {
    _gk_confParticipant_addUser $w $i
  }
  if {![member $w $_gk_confpart_widgets]} {
    gk_bind newUserArrived "_gk_confParticipant_addUser $w %U"
    gk_bind userDeleted "_gk_confParticipant_delUser $w %U"
    lappend _gk_confpart_widgets $w
  }
  return $w  
}

proc _gk_confParticipant_Attributes {user} {
    global _gk_confpart_debugging
    if {$_gk_confpart_debugging} {
	gk_toplevelConfAttributes .attrs $user
    } else {
	gk_confMakeCard $user
    }
}

proc _gk_confParticipant_addUser {w user} {
  if {[winfo exists $w]} {
    set name [gk_getUserAttrib $user username]
    button $w.userlbl$user -text $name -borderwidth 2 \
	-command "_gk_confParticipant_Attributes $user"
    $w adduser $user $w.userlbl$user 
  }
}

proc _gk_confParticipant_delUser {w user} {
  if {[winfo exists $w]} {
    $w deluser $user
  }
}

proc gk_toplevelConfParticipants {{w .gk_participants}} {
  if {[winfo exists $w]} {
    wm deiconify $w
    raise $w
  } else {
    toplevel $w
    wm title $w "Users in [users local.confname]"
    pack [label $w.lbl -text "Users in [users local.confname]"]
    pack [gk_confParticipants $w.users]
    pack [frame $w.line -height 4 -borderwidth 2 -relief sunken] -fill x -pady 4
    pack [checkbutton $w.debug -text "Include debugging" -variable _gk_confpart_debugging] -side top
    pack [button $w.ok -text Dismiss -command "destroy $w"]
  }
}
