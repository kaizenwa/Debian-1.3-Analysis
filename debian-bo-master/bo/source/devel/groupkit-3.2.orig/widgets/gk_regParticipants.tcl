set _gk_regpart_widgets ""
set _gk_regpart_debugging 0

proc gk_regParticipants {w args} {  global _gk_regpart_widgets
  eval gk_userlist $w $args
  set myid [gk_rc_getMyID]
  foreach i [regclients keys remote] {
      _gk_regParticipant_addUser $w $i
  }
  if {![member $w $_gk_regpart_widgets]} {
    set cmd1 "_gk_regParticipant_checkadd $w"
    set cmd2 "_gk_regParticipant_checkdel $w"
    gk_on {([gk_event type]=="addEnvInfo")&&([gk_event env]=="regclients")} $cmd1
    gk_on {([gk_event type]=="deleteEnvInfo")&&([gk_event env]=="regclients")} $cmd2
    lappend _gk_regpart_widgets $w
  }
  return $w  
}

proc _gk_regParticipant_checkadd w {
  if {[winfo exists $w]} {
    set key [gk_event key]; set split [split $key .]
    if {([llength $split]==3)&&([lindex $split 0]=="remote")&&([lindex $split 2]=="name")} {
      _gk_regParticipant_addUser $w [lindex $split 1]
    }
  }
}

proc _gk_regParticipant_checkdel w {
  if {[winfo exists $w]} {
    set key [gk_event key]; set split [split $key .]
    if {([llength $split]==2)&&([lindex $split 0]=="remote")} {
      _gk_regParticipant_delUser $w [lindex $split 1]
    }
  }
}

proc _gk_regParticipant_Attributes {user} {
    global _gk_regpart_debugging
    if {$_gk_regpart_debugging} {
	gk_toplevelRegAttributes .attrs $user
    } else {
	gk_regMakeCard $user
    }
}

proc _gk_regParticipant_addUser {w user} {
  set name [regclients remote.$user.name]
  button $w.userlbl$user -text $name -borderwidth 2 \
	  -command "_gk_regParticipant_Attributes $user"
  $w adduser $user $w.userlbl$user
}

proc _gk_regParticipant_delUser {w user} {
  $w deluser $user
}

proc gk_toplevelRegParticipants {{w .gk_participants}} {
  if {[winfo exists $w]} {
    wm deiconify $w
    raise $w
  } else {
    toplevel $w
    wm title $w "Logged in Users"
    pack [label $w.lbl -text "Logged in Users"]
    pack [gk_regParticipants $w.users]
    pack [frame $w.line -height 4 -borderwidth 2 -relief sunken] -fill x -pady 4
    pack [checkbutton $w.debug -text "Include debugging" -variable _gk_regpart_debugging] -side top
    pack [button $w.ok -text Dismiss -command "destroy $w"]
  }
}

