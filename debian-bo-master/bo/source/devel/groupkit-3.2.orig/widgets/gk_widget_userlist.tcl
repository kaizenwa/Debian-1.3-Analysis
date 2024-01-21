


proc gk_userlist {w args} {
    eval gkInt_CreateWidget $w gkUserlist gkUserlist $args
    return $w
}

proc gkUserlist_CreateClassRec {} {
    global gkUserlist

    set gkUserlist(inherit) {label}
    set gkUserlist(methods) {adduser deluser component}
    set gkUserlist(options) {-orient}
    set gkUserlist(-orient) {-orient orient Orient vertical}
}

proc gkUserlist_InitWidgetRec {w class className args} {
    upvar #0 $w gk_userlist
    set gk_userlist(users) {}
    set gk_userlist(side) top
}

proc gkUserlist_ConstructWidget {w} {
    global gkUserlist gk_library
    upvar #0 $w gk_userlist
}

proc gkUserlist_Config {w option args} {
     upvar #0 $w gk_userlist
     switch -exact [string range $option 1 end] {
       orient {gkUserlist_setOrientation $w $args}
       default {$gk_userlist(rootCmd) configure $option $args}
     }
}

proc gkUserlist_Methods {w command args} {
  upvar #0 $w gk_userlist
  set args [lindex $args 0]
  switch -exact $command {
    adduser { gkUserlist_addUser $w $args }
    deluser { gkUserlist_delUser $w $args }
    component { gkUserlist_component $w $args }
    default {$gk_userlist(rootCmd) $command $args}
  }
}

proc gkUserlist_addUser {w stuff} {
  upvar #0 $w gk_userlist
  if {[llength $stuff]!=2} {error "wrong # args"}
  set id [lindex $stuff 0]; set win [lindex $stuff 1]; 
  foreach i $gk_userlist(users) {
    if {[keylget i id]==$id} {error "user exists with same id"}
  }
  keylset entry id $id win $win
  lappend gk_userlist(users) $entry
  if {$gk_userlist(side)=="top"} {
    pack $win -in $w -side $gk_userlist(side) -fill x 
  } else {
    pack $win -in $w -side $gk_userlist(side) 
  }
}

proc gkUserlist_delUser {w stuff} {
  upvar #0 $w gk_userlist
  if {[llength $stuff]!=1} {error "wrong # args"}
  set id [lindex $stuff 0]
  set idx 0; foreach i $gk_userlist(users) {
    if {[keylget i id]==$id} {
      destroy [keylget i win] 
      set gk_userlist(users) [lreplace $gk_userlist(users) $idx $idx]
      return
    }
    incr idx
  }
  error "no such user"
}


proc gkUserlist_setOrientation {w orient} {
  upvar #0 $w gk_userlist
  if {$orient=="horizontal"} {
    set gk_userlist(side) left
  } else {
    set gk_userlist(side) top
  }
}

proc gkUserlist_component {w stuff} {
  upvar #0 $w gk_userlist
  switch [lindex $stuff 0] {
    user { set id [lindex $stuff 1]
	   foreach i $gk_userlist(users) {
	     if {[keylget i id]==$id} {return [keylget i win]}
           }
           error "no such user"
	 }
    default {error "no such component"}
  }
}
