set peep_bitmaps(active) ~saul/development/contact/p_active.xbm
set peep_bitmaps(idle) ~saul/development/contact/p_question.xbm
set peep_bitmaps(noval) ~saul/development/contact/p_connecting.xbm



proc gk_peephole {w args} {
    eval gkInt_CreateWidget $w gkPeephole GkPeephole $args
    return $w
}

proc gkPeephole_CreateClassRec {} {
    global gkPeephole

    set gkPeephole(inherit) {label}
    set gkPeephole(methods) {}
    set gkPeephole(options) {-user -time}
    set gkPeephole(-user) {-user user User {}}
    set gkPeephole(-time) {-time time Time {}}
}

proc gkPeephole_InitWidgetRec {w class className args} {
    upvar #0 $w gk_peephole

}

proc gkPeephole_ConstructWidget {w} {
    global gkPeephole
    upvar #0 $w gk_peephole

    label $w.pic 
    _peep_chooseBitmap $w.pic [lindex $gkPeephole(-time) 3]
    label $w.user -text [lindex $gkPeephole(-user) 3]
    pack $w.pic
    pack $w.user
}

proc gkPeephole_Config {w option args} {
     upvar #0 $w gk_peephole
     switch -exact [string range $option 1 end] {
       user { $w.user config -text $args}
       time { 
	 _peep_chooseBitmap $w.pic $args
       }
       default {$w config $option $args}
     }
}

proc gkPeephole_Methods {w command args} {
}

proc _peep_chooseBitmap {w idletime} {
  global peep_bitmaps
  if {$idletime=={}} {
    $w config -bitmap @$peep_bitmaps(noval)
  } elseif {$idletime<5} {
    $w config -bitmap @$peep_bitmaps(active)
    switch $idletime {
      0 {$w config -foreground black}
      1 {$w config -foreground gray40}
      2 {$w config -foreground gray50}
      3 {$w config -foreground gray60}
      4 {$w config -foreground gray70}
    }
  } elseif {$idletime>=5} {
    $w config -bitmap @$peep_bitmaps(idle)
  } else {
    $w config -bitmap @$peep_bitmaps(noval)
  }
 
}



