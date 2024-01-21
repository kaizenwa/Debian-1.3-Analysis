wm withdraw .

gk_initGenericRC $argv


gk_pollConferences


#############################################################################
# policy section
#

# all new conferences are valid, and we add them to our list
gk_on {[gk_event type]=="foundNewConf"} {gk_addConfToList}


# for conferences that we originated, join to them right away
gk_on {([gk_event type]=="foundNewConf")&& \
    ([gk_event originator]==[gk_uniqprogid])} \
    {facil_ourConference [gk_event confnum] [gk_event conftype]}


# all new users are valid, and we add them to our list
gk_on {[gk_event type]=="foundNewUser"} {gk_addUserToList}

gk_on { ([gk_event type]=="foundNewUser") && 
    ([gk_event confnum]==[confs mtg.confnum]) &&
    (([gk_event host]!=[userprefs host])||([gk_event port]!=[userprefs port]))} {
	facil_add_user [gk_event usernum]
    }

# when we are new to a conference, we create it and join to existing users
gk_on {  ([gk_event type]=="foundNewUser") && 
         ([gk_event host]==[userprefs host]) &&
         ([gk_event port]==[userprefs port]) &&
         ([confs c.[gk_event confnum].conftype]!="Facilitated Meeting")} { 
	     gk_createConference [gk_event confnum] [gk_event usernum]; 
	     gk_joinToOthers [gk_event confnum] [gk_event usernum]
	 }


# all deleted users are removed from the lists
gk_on {[gk_event type]=="foundDeletedUser"} {gk_removeUserFromList}


# when the last user of a conference leaves, we shut down the conference
gk_on {[gk_event type]=="lastUserLeftConf"} \
      {gk_callDeleteConference [gk_event confnum]; gk_pollConferences}


# when a conference connected to us died, report the user left
gk_on {[gk_event type]=="conferenceDied"} \
      {gk_userLeft [gk_event conf] [gk_event user] [gk_event filedesc]}


# all deleted conferences are removed from lists
gk_on {[gk_event type]=="foundDeletedConf"} {gk_removeConfFromList}


# if our user is removed from the conference we are no longer joined
gk_on {([gk_event type]=="foundDeletedUser")&&\
        ([gk_event usernum]==[spawned c.[gk_event confnum].localuser])} \
    {gk_noLongerJoined [gk_event confnum]}


# if one of our users leaves the facil conference, update the display
gk_on {[gk_event type]=="facilSlaveQuitting" } \
    { facil_remove_user [gk_event usernum] }
gk_on {([gk_event type]=="foundDeletedUser") && \
       ([gk_event confnum]==[confs mtg.confnum])} {
	   facil_remove_user [gk_event usernum]
       }

# when a user asks to create a new conference, create it as requested
gk_on {[gk_event type]=="userRequestedNewConf"} {gk_createRequestedConf}


# when a new user is approved, update the display
gk_on {[gk_event type]=="newUserApproved"} {
    facil_in [gk_event confnum] [user_in_facil_conf [gk_event confnum] [gk_event usernum]]
}

# when a leaving user is approved, update the display
gk_on {[gk_event type]=="deleteUserApproved"} {#puts BOO}
gk_on {[gk_event type]=="deleteUserApproved"} {
    facil_out [gk_event confnum] [user_in_facil_conf [gk_event confnum] [gk_event usernum]]
}

# when the conference dies, update the display
gk_on {[gk_event type]=="deleteConfApproved"} {facil_remove_conf [gk_event confnum]}

#############################################################################
# interface section
#


# globals
#
set bitmap(boot) @$gk_library/library/bitmaps/boot.xbm
set bitmap(checkmark) @$gk_library/library/bitmaps/checkmark.xbm
set bitmap(blank) @$gk_library/library/bitmaps/blank.xbm
set bitmap(x) @$gk_library/library/bitmaps/x.xbm

set fp(window) ""
set fp(users) {}
set fp(confs) {}


# create the user interface for the facilitator
#
proc facil_create_ui {w name} { global fp
   toplevel $w
   wm title $w "$name"
   set fp(window) $w
   gk_defaultMenu $w.menu 

   # Add a new item to the menubar
   $w.menu add toolsMenu 1 \
     -text Tools 
   $w.menu itemconfigure toolsMenu \
       -postcommand "buildConferenceMenu $w.menu.toolsMenu.menu"

   # Remove the Collaboration menu from the default menubar
   $w.menu delete collaboration

   # Change command for Quit on File menu
   $w.menu itemcommand file entryconfigure 1 -command facil_quit
   label $w.l -anchor center -text "Facilitator Panel"
   frame $w.names -relief ridge -borderwidth 2 -width 100
   frame $w.confs -relief flat -width 50
   label $w.names.x -width 20 -height 2
   facil_repack
}

# Post event that facilitator client is quitting
proc facil_quit {} {
     gk_killAllMyConfs
     keylset event type facilClientQuitting 
     gk_postEvent $event
     _gk_properQuit
}


# add a user to the facilitator's display
#
proc facil_add_user user { global fp bitmap
     if { ![winfo exists $fp(window)] } {
	 error "Must create facilitator panel  before you can add \
	        users to it."
     }
     frame $fp(window).names.$user -width 20
     button $fp(window).names.$user.b  -borderwidth 2 -pady 2 -padx 10 \
	        -relief raised -bitmap $bitmap(boot) -width 30 \
		-command "facil_remove_user_from_mtg $user" -anchor center
     label $fp(window).names.$user.l -text [confs c.[confs mtg.confnum].users.$user.username] -width [expr 20 - 5] \
		-anchor w
     lappend fp(users) $user
     foreach conf $fp(confs) {
	 facil_add_user_to_conf $conf $user
     }
     facil_repack
}


# remove a user from the facilitator's display
#
proc facil_remove_user user { global fp
     if {[set idx [lsearch -exact $fp(users) $user]] == -1} {
	error "bad user number \"$user\""
     } else {
	destroy $fp(window).names.$user
        foreach conf $fp(confs) { facil_remove_user_from_conf $conf $user }
        set fp(users) [lreplace $fp(users) $idx $idx]
    }
}


# add a conference to the facilitator's display
#
proc facil_add_conf conf { global fp
      if { ![winfo exists $fp(window)] } {
	 error "Must create facilitator panel before you can add \
	        conferences to it."
      }
      frame $fp(window).confs.$conf -width 50 -relief ridge -borderwidth 2 
      label $fp(window).confs.$conf.l -text [confs c.$conf.confname] -width 20 -height 2 \
		-anchor center 
      lappend fp(confs) $conf
      foreach user $fp(users) {
         facil_add_user_to_conf $conf $user
      }
      facil_repack
}


# remove a conference from the facilitator's display
#
proc facil_remove_conf conf { global fp
     if { [set idx [lsearch -exact $fp(confs) $conf]] == -1} {
	error "bad conference number \"$conf\""
     } else {
        destroy $fp(window).confs.$conf
        set fp(confs) [lreplace $fp(confs) $idx $idx]
    }
}


# join a user to a conference on the display
#
proc facil_add_user_to_conf {conf user} { global fp bitmap
     button $fp(window).confs.$conf.$user -borderwidth 2 -anchor center \
            -command "facil_in/out $fp(window).confs.$conf.$user"  -relief raised \
	    -width 30 -padx 0 -pady 3 
     set fp($conf,$user) 0
}


# remove a user from a conference on the display
#
proc facil_remove_user_from_conf {conf user} { global fp
     destroy $fp(window).confs.$conf.$user
}


# repack the display when conferences or users are added or deleted
#
proc facil_repack {} { global fp 
   pack forget $fp(window) $fp(window).names $fp(window).confs
   wm minsize $fp(window) 50 50
#   pack $fp(window).l -side top -fill x 
   pack append $fp(window) $fp(window).menu {top fillx} 
   pack $fp(window).names.x -fill x
   foreach user $fp(users) {
       pack $fp(window).names.$user.l -side right -fill x -expand t
       pack $fp(window).names.$user.b -pady 5 -padx 3 -side right 
       pack $fp(window).names.$user -fill x
   }
   pack $fp(window).names -side left -pady 2 -expand t -fill both
  
   foreach conf $fp(confs) {
      pack $fp(window).confs.$conf.l -side top 
      pack $fp(window).confs.$conf -side left -pady 2 -fill y
      foreach user $fp(users) {
         pack $fp(window).confs.$conf.$user -side top -pady 5 
      }
   }
   pack $fp(window).confs -fill y -expand t

   #fix the minsize of of the window so that everything is shown
   #This is a bit of a hack.  however the width and height are calculated
   #as follows:
   # width = width of the frame of names + 
   #             (the number of conferences * the width of a conference frame)
   # heigth = height of "facilitator Panel" label + 8 + 
   #          ( (the number of users + 1) * the height of a user frame)
   #
   # note: for the height the 8 is need, i think because of the padding.
   #       also for the height you need to add one to the number of users
   #       since there is one blank one at the top.

   update
   if {[set firstconf [lindex $fp(confs) 0]] != ""} {
	set width [expr [winfo width $fp(window).names] + \
	  ([llength $fp(confs)] * [winfo width $fp(window).confs.$firstconf])]
   } else {
	set width [expr [winfo width $fp(window).names]]
   }
   if {[set firstuser [lindex $fp(users) 0]] != ""} {
	set height [expr [winfo height $fp(window).l] +  8 + \
 (([llength $fp(users)] + 1) * [winfo height $fp(window).names.$firstuser])]
   } else {
	set height [expr [winfo height $fp(window).l] +  8]
   }
   wm minsize $fp(window) $width $height
}


# request toggle if a user is in or out of a conference
#
proc facil_in/out { which } { global fp bitmap
#    puts "DEBUG IO which=$which"
      set user [string range $which [expr [string last . $which] + 1] end]
      set conf [string range [winfo parent $which] \
                   [expr [string last . [winfo parent $which] ] + 1] end]

      if  { $fp($conf,$user) == 1 } { remove_a_user [user_in_real_conf $conf $user] $conf
      } else { join_a_user $user $conf}
}


# make a user in a conference
#
proc facil_in {conf user} {
    global bitmap fp
    set which $fp(window).confs.$conf.$user
#    puts "facil in which=$which"
    if {[winfo exists $which]} {
#	puts " ... exists"
	$which configure -bitmap $bitmap(checkmark) -padx 10 -pady 2
	set fp($conf,$user) 1
    } else {
#	puts " ... does not exist"
    }
}


# make a user out of a confernce
proc facil_out {conf user} {
    global bitmap fp
    set which $fp(window).confs.$conf.$user
#    puts "facil out which=$which"
    if {[winfo exists $which]} {
	$which configure -bitmap $bitmap(blank) -padx 10 -pady 2 
	set fp($conf,$user) 0
    }
}



#############################################################################
# get everything started by creating an over-seeing conference
#

# dialogue to get a facilitated conference name
#
proc facil_getMeetingName {} {
    set w .mtgname
    if {[info commands $w] == $w} return
 
    toplevel $w
    wm title $w "New Facilitated Meeting"
    set mtgname "New Meeting"
 
    pack append [frame $w.buttons] \
        [button $w.buttons.cancel -text Cancel \
                -command "destroy ."] right \
        [button $w.buttons.ok -text Ok \
                -command {createFacilMtg $mtgname;destroy .mtgname}] right
 
    pack append $w \
        [label $w.lbl -text $mtgname] top \
        [entry $w.name -relief sunken -textvariable mtgname] \
                   {top pady 10 fillx} \
        $w.buttons {bottom fillx pady 20} 
    bind $w.name <Return> {createFacilMtg $mtgname;destroy .mtgname}
}


# create the facilitated meeting given its name
# 
proc createFacilMtg name {
    keylset conf confname $name conftype "Facilitated Meeting" \
	originator [gk_uniqprogid]
    gk_callNewConference $conf
    gk_pollConferences
}


# we get called when a conference we created shows up from the registrar
#
proc facil_ourConference {confnum type} {
    if {$type=="Facilitated Meeting"} {
	confs mtg.confnum $confnum
	facil_create_ui .panel [confs c.$confnum.confname]
	gk_callJoinConference $confnum
	gk_pollUsers $confnum
    } else {
	facil_add_conf $confnum
	gk_callJoinConference $confnum
	gk_pollUsers $confnum
    }
}

proc facil_remove_user_from_mtg user {
    gk_callLeaveConference [confs mtg.confnum] $user
    gk_pollUsers [confs mtg.confnum]
}


proc join_a_user {user conf} {
    set userinfo [confs c.[confs mtg.confnum].users.$user]
    keylset userinfo confnum $conf
    gk_callJoinConfWithKeys $conf $userinfo
    gk_pollUsers $conf
}

proc remove_a_user {user conf} {
    gk_callLeaveConference $conf $user
    gk_pollUsers $conf
}

# given a user number from another conference, we find the corresponding
# user number in the facilitated conference; we do this by finding the
# matching registrar client
#
proc user_in_facil_conf {conf user} {
    set host [confs c.$conf.users.$user.host]
    set port [confs c.$conf.users.$user.port]
    set mtgconf [confs mtg.confnum]
    foreach i [confs keys c.$mtgconf.users] {
	if {([confs c.$mtgconf.users.$i.host]==$host) &&
	    ([confs c.$mtgconf.users.$i.port]==$port)} {
#		puts " .. found it"
		return $i
	    }
    }
#    puts "  .. did not find"
    return ""
}

# the same in reverse, i.e. map facilitated user num to real user num
#
proc user_in_real_conf {conf user} {
    set mtgconf [confs mtg.confnum]
    set host [confs c.$mtgconf.users.$user.host]
    set port [confs c.$mtgconf.users.$user.port]
    foreach i [confs keys c.$conf.users] {
	if {([confs c.$conf.users.$i.host]==$host) &&
	    ([confs c.$conf.users.$i.port]==$port)} {
		return $i
	    }
    }
    return ""
}


facil_getMeetingName 
