
proc gk_card {w args} {
    eval gkInt_CreateWidget $w gkCard gkCard $args
    return $w
}

proc gkCard_CreateClassRec {} {
    global gkCard

    set gkCard(inherit) {frame}
    set gkCard(methods) {component}
    set gkCard(options) {-name -image -title -dept -company -phone -fax -email -www -office -idle -namefont -deptfont -companyfont -infofont -locnfont -currfont -command}
    set gkCard(-name) {-name name Name ""}
    set gkCard(-image) {-image image Image ""}
    set gkCard(-title) {-title title Title ""}
    set gkCard(-dept) {-dept dept Dept ""}
    set gkCard(-company) {-company company Company ""}
    set gkCard(-phone) {-phone phone Phone ""}
    set gkCard(-fax) {-fax fax Fax ""}
    set gkCard(-email) {-email email Email ""}
    set gkCard(-www) {-www www Www ""}
    set gkCard(-office) {-office office Office ""}
    set gkCard(-idle) {-idle idle Idle ""}
    set gkCard(-command) {-command command Command ""}
    set gkCard(-namefont) {-namefont nameFont NameFont -*-helvetica-bold-o-*--18*}
    set gkCard(-deptfont) {-deptfont deptFont DeptFont -*-helvetica-medium-r-*--10*}
    set gkCard(-companyfont) {-companyfont companyFont CompanyFont -*-helvetica-bold-r-*--12*}
    set gkCard(-infofont) {-infofont infoFont InfoFont -*-helvetica-medium-r-*--10*}
    set gkCard(-locnfont) {-locnfont locnFont LocnFont -*-helvetica-bold-r-*--10*}
    set gkCard(-currfont) {-currfont currFont CurrFont -*-helvetica-bold-o-*--12*}
}

proc gkCard_InitWidgetRec {w class className args} {
    upvar #0 $w gk_card
}

proc gkCard_ConstructWidget {w} {
    global gkCard gk_library
    upvar #0 $w gk_card
    
    $w configure -width 290 -height 175
    place [label $w.name -text [lindex $gkCard(-name) 3] \
	    -font [lindex $gkCard(-namefont) 3]] -in $w -x 100 -y 10
    if {![member unknown_picture [image names]]} {
	image create photo unknown_picture -file $gk_library/library/images/unknown.gif
    }
    place [label $w.pic -borderwidth 2 -bg black -image unknown_picture] \
	    -in $w -x 20 -y 15
    place [label $w.title -text [lindex $gkCard(-title) 3] \
	    -font [lindex $gkCard(-deptfont) 3]] -in $w -x 100 -y 35
    place [label $w.dept -text [lindex $gkCard(-dept) 3] \
	    -font [lindex $gkCard(-deptfont) 3]] -in $w -x 100 -y 49
    place [label $w.company -text [lindex $gkCard(-company) 3] \
	    -font [lindex $gkCard(-companyfont) 3]] -in $w -x 100 -y 63
    place [label $w.phone -text "Telephone [lindex $gkCard(-phone) 3]" \
	    -font [lindex $gkCard(-infofont) 3]] -in $w -x 10 -y 109
    place [label $w.fax -text "FAX [lindex $gkCard(-fax) 3]" \
	    -font [lindex $gkCard(-infofont) 3]] -in $w -x 10 -y 124
    place [label $w.email -text [lindex $gkCard(-email) 3] \
	    -font [lindex $gkCard(-infofont) 3]] -in $w -x 10 -y 139
    place [label $w.www -text [lindex $gkCard(-www) 3] \
	    -font [lindex $gkCard(-infofont) 3]] -in $w -x 10 -y 154
    place [label $w.cur -text "Current Information" \
	    -font [lindex $gkCard(-currfont) 3]] -in $w -x 285 -y 109 -anchor ne
    place [label $w.office -text "Location [lindex $gkCard(-office) 3]" \
	    -font [lindex $gkCard(-locnfont) 3]] -in $w -x 285 -y 123 -anchor ne
    place [label $w.idle -text [lindex $gkCard(-idle) 3] \
	    -font [lindex $gkCard(-locnfont) 3]] -in $w -x 285 -y 137 -anchor ne
    place [frame $w.div -height 2 -relief ridge -width 275 -bg black] \
	    -in $w -x 10 -y 100
    place [button $w.ok -font 6x10 -text Okay -padx 1 -pady 1] -in $w -x 285 -y 151 -anchor ne
}

proc gkCard_Config {w option args} {
     upvar #0 $w gk_card
     switch -exact [string range $option 1 end] {
	 name { $w.name configure -text [lindex $args 0] }
	 image { $w.pic configure -image [lindex $args 0]}
	 title { $w.title configure -text [lindex $args 0] }
	 dept { $w.dept configure -text [lindex $args 0] }
	 company { $w.company configure -text [lindex $args 0]}
	 phone { $w.phone configure -text "Telephone [lindex $args 0]"}
	 fax { $w.fax configure -text "FAX [lindex $args 0]"}
	 email { $w.email configure -text [lindex $args 0]}
	 www { $w.www configure -text [lindex $args 0]}
	 office { $w.office configure -text "Location [lindex $args 0]"}
	 idle { $w.idle configure -text [lindex $args 0]}
	 namefont { }
	 deptfont { }
	 companyfont { }
	 infofont { }
	 locnfont { }
	 currfont { }
	 command { $w.ok configure -command [lindex $args 0]}
         default {$gk_card(rootCmd) configure $option $args}
     }
}

proc gkCard_Methods {w command args} {
  upvar #0 $w gk_card
  set args [lindex $args 0]
  switch -exact $command {
     component { gkCard_component $w $args }
    default {$gk_card(rootCmd) $command $args}
  }
}


proc gkCard_component {w stuff} {
  switch [lindex $stuff 0] {
    tool {set id [lindex $stuff 1]; return $w.tools$id}
    user {set id [lindex $stuff 1]; return $w.users$id}
    roomname {return $w.roomname}
    default {error "no such component"}
  }
}
