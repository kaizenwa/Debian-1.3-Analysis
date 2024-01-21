#
# 
# MODULE: rc.tcl (Generic registrar client support)
#
#


#########################################################################
# initialization stuff and some utility routines
#


# utility proc -- get a unique name for us (host+port)
#
proc gk_uniqprogid {} {
     set host [userprefs host]
     set port [userprefs port]
     return $host$port
}


# this routine initializes the registrar client, and should be the first
# thing called.  it parses some command line stuff out, sets up all the
# environments we need, connects to the central registrar, and sets up
# a server for conferences we create to connect to us
#
proc gk_initGenericRC {argv} {
    gk_initApplication
    gk_newenv confs
    gk_newenv spawned

    foreach i $argv {
	if {[string range $i 0 1] == "-p"} {
	    registrar port [string range $i 2 [expr [string length $i]-1]]
	} elseif {[string range $i 0 1] == "-h"} {
	    registrar host [string range $i 2 [expr [string length $i]-1]]
	}
    }

    if {[userprefs name]==""} {userprefs name [gk_info userid]}
    if {[registrar port]==""} {registrar port 9068}
    if {[registrar host]==""} {
	if {[userprefs host_override]!=""} {
	    registrar host [userprefs host_override]
	} else {
	    registrar host [gk_info host]
	}
    }

    keylset myinfo name [userprefs name]

    foreach i [userprefs keys personal] {
	keylset myinfo $i [userprefs personal.$i]
    }

#       note: later on put in more stuff including our listener, info
#             about the rc type (which will be passed in), etc.

    registrar fd [gk_connectToServer [registrar host] [registrar port] $myinfo]

# find out my id number
    _gk_toReg gk_serverQueryID "_gk_rc_setMyID"

    gk_on {[gk_event type]=="clientConnected"} {_gk_confConnected [gk_event key]}  
    gk_on {[gk_event type]=="clientDisconnected"} {_gk_confDisconnected [gk_event key]}  

    gk_newenv -notify -client regclients
    regclients server [registrar fd]
    userprefs port [gk_createServer spawned 0 confnum] 
    userprefs host [_gk_getHostname]
}

# callback from registrar, which tells me my id number in regclients

proc _gk_rc_setMyID id {
  registrar myid $id
}

proc gk_rc_getMyID {} {
  while {[registrar exists myid]==0} {
    update
    after 200
  }
  return [registrar get myid]
}


#########################################################################
# handle announcements of new and deleted conferences from the registrar
#


# here is the newest conference list; find out what the differences are
# between the new list and what we're holding internally.  check for new
# entries (not in old list) or removed entries (not in new list)
#
proc _gk_confList newlist {
    set oldlist ""; catch {set oldlist [confs keys c]}
    foreach i [keylkeys newlist] {
      if {![member $i $oldlist]} {
		  gk_foundNewConf $i [keylget newlist $i]
	  }
    }
    foreach i $oldlist {
      if {![member $i [keylkeys newlist]]} {gk_foundDeletedConf $i}
    }
    keylset event type confListProcessed
    gk_postEvent $event
}


# post a foundNewConf event on new conferences, and cache the conference
# info in the newconf environment (see gk_addConfToList down below)
#
proc gk_foundNewConf {conf confinfo} {
    gk_newenv newconf
    newconf id $conf; newconf info $confinfo
    keylset confinfo type foundNewConf confnum $conf
    gk_postEvent $confinfo
    catch {newconf destroy} tmp
}


# post a foundDeletedConf event on deleted conferences, and cache its
# info in the delconf environment (see gk_removeConfFromList down below)
#
proc gk_foundDeletedConf confnum {
    gk_newenv delconf
    delconf id $confnum
    keylset event confnum $confnum type foundDeletedConf
    gk_postEvent $event
    catch {delconf destroy}
}



#########################################################################
# handle announcements of new and deleted users from the registrar
#


# here is the newest list of users for one conference; find out what the
# differences are between the new list and what we're holding internally.
# check for new entries (not in old list) or removed entries (not in new list)
#
proc _gk_userList {conf newlist} {
  set oldlist ""; catch {set oldlist [confs keys c.$conf.users]}
  foreach i [keylkeys newlist] {
    if {![member $i $oldlist]} {gk_foundNewUser $conf $i [keylget newlist $i]}
  }
  foreach i $oldlist {
    if {![member $i [keylkeys newlist]]} {gk_foundDeletedUser $conf $i}
  }
  keylset event type userListProcessed confnum $conf
  gk_postEvent $event
}


# post a foundNewUser event on new users, and cache the user info in the
# newuser environment (see gk_addUserToList down below)
#
proc gk_foundNewUser {conf user userinfo} {
    gk_newenv newuser
    newuser conf $conf; newuser id $user; newuser import info $userinfo
    keylset userinfo type foundNewUser confnum $conf usernum $user
    gk_postEvent $userinfo
    catch {newuser destroy}
}


# post a foundDeletedUser event on deleted users, and cache the user
# info in the deluser environment (see gk_removeUserFromList down below)
#
proc gk_foundDeletedUser {confnum usernum} {
    gk_newenv deluser
    deluser conf $confnum; deluser user $usernum
    keylset delevent confnum $confnum usernum $usernum type foundDeletedUser
    gk_postEvent $delevent
    catch {deluser destroy}
}


#########################################################################
# send various registration commands to the central registrar
#
# send any single command to the registrar
#

proc _gk_toReg args {dp_RDO [registrar fd] eval $args}


# send the commands for new/delete/poll conferences and users
#
proc gk_callJoinConfWithKeys {conf user} {_gk_toReg _gk_addUser $conf $user }
proc gk_callLeaveConference {conf user} {_gk_toReg _gk_deleteUser $conf $user}
proc gk_callNewConference conf { _gk_toReg _gk_newConference $conf }
proc gk_callDeleteConference confnum {_gk_toReg _gk_deleteConference $confnum}
proc gk_pollConferences {} {after 100 _gk_toReg _gk_dispConference }
proc gk_pollUsers confnum {after 100 _gk_toReg _gk_dispUsers $confnum }
proc gk_provideUserInfo {key val} {_gk_toReg _gk_provideRCinfo $key $val}
proc gk_sendToRC {rc args} {eval _gk_toReg _gk_relayToRC $rc $args}

# front end to more conveniently allow the local user to join a conference
#
proc gk_callJoinConference confnum {
  global env
  keylset user userid [gk_info userid] host [userprefs host] \
     port [userprefs port]  username [userprefs name] display $env(DISPLAY)
  _gk_toReg _gk_addUser $confnum $user
}



#########################################################################
# routines to create and deal with attached conference processes
#


# create a new conference process; create the command line so as to pass
# all necessary information (about itself, its local user, its registrar
# client) and then fork a new process
#
proc gk_createConference {confnum usernum} {

    set type [confs c.$confnum.conftype]
    if {[userprefs prog.$type.originator_only]=="yes"} {
	set originator [confs c.$confnum.originator]
	if {($originator!="[userprefs host][userprefs port]") \
		|| ([spawned joined.$confnum]=="yes")} {
	    return
	}
    }

    spawned delete remote.$confnum
    spawned remote.$confnum.pending ""
    spawned remote.$confnum.stillCheck yes
    spawned remote.$confnum.localuser $usernum

    set cmdline [userprefs prog.$type.cmd]
    if {([llength $cmdline]>=2) && ([lindex $cmdline 1]=="gkwish")} {
	set filename [lindex $cmdline [expr [llength $cmdline]-1]]
	if {![file exists $filename]} {
	    _gk_confNotThereDlg $filename [confs c.$confnum.confname]
	    _gk_checkConnection $confnum $usernum
	    return
	}
    }

    set conf [confs c.$confnum]
    keyldel conf users
    keylset conf reghost [userprefs host] regport [userprefs port] \
	usernum $usernum confnum $confnum persisthost [registrar host] \
	userid [gk_info userid] username [userprefs name]
    keylset conf personal [userprefs personal]
    if {[userprefs color]!=""} {
	keylset conf color [userprefs color]
    }
    if {[spawned joined.$confnum]=="yes"} {
	keylset conf first_time no
    } else {
	keylset conf first_time yes
	spawned joined.$confnum yes
    }
    
    set cmd [concat $cmdline " " $conf " &"]
    after 10000 "_gk_checkConnection $confnum $usernum"
    eval $cmd

    foreach i [confs keys c.$confnum.users] {
	set name [confs c.$confnum.users.$i.username]
	set display [confs c.$confnum.users.$i.display]
	gk_toConf $confnum "_gksm_adduser $i <$name> <$display>"
    }

    _gk_reqChangeConfInfo $confnum will_persist no
}

# put up a dialog when we can't find the conference program to execute
#
proc _gk_confNotThereDlg {filename confname} { 
    set w .confnotthere
    if {[winfo exists $w]} {return}
    toplevel $w
    wm title $w "Oops!"
    pack [message $w.msg -aspect 500 -text "Could not find the file \"$filename\" while trying to create the conference \"$confname\"."] -side top
    pack [button $w.ok -text Dismiss -command "destroy $w"] -side top
}

# in case there are problems creating a conference, gk_createConference
# sets a timer so that after several seconds we are called to check if
# the conference has connected back up to us.  if not we make it look 
# like the user has left the conference
#
# NOTE: (MR 2/24/94) This should be replaced by a "confDidNotConnect"
#       trigger.
#
proc _gk_checkConnection {confnum usernum} {
  if {[spawned remote.$confnum.stillCheck]=="no"} {return}
  if {![member $confnum [spawned keys c]]} {gk_userLeft $confnum $usernum no}
}

# one of the first things the new conference does is connect back to us
# and call this method, so we can associate the socket with the conference.
# we send any queued messages for the conference (see gk_toConf)
#

proc _gk_confConnected confnum {
  foreach i [spawned remote.$confnum.pending] {
    eval [concat dp_RDO [spawned remote.$confnum.filedesc] $i]
  }
  spawned remote.$confnum.stillCheck no
}



# send a message to a conference; if the conference has not connected
# back to us, we queue the message until it does connect
#
proc gk_toConf {confnum msg} { 
    if {[spawned remote.$confnum.filedesc]==""} {
	set p [spawned remote.$confnum.pending]
	lappend p $msg
	spawned remote.$confnum.pending $p
    } else {
	set fd [spawned remote.$confnum.filedesc]
	catch {eval [concat dp_RDO $fd $msg]}
    }
}


# called when one of our conferences dies.  find the conference and 
# post a "conferenceDied" event
#  note the special case for "originator_only" conferences
#

proc _gk_confDisconnected confnum {
  set type [confs c.$confnum.conftype]
  if {[userprefs prog.$type.originator_only]=="yes"} {
      gk_callDeleteConference $confnum
  } else {
      keylset event conf $confnum user [spawned remote.$confnum.localuser] \
	      filedesc [spawned remote.$confnum.filedesc] type conferenceDied
      gk_postEvent $event    
  }
}


#########################################################################
# routines to tell conferences to connect to each other
#


# this routine tells one of our conference's to join to another (e.g.
# because that user just joined the conference
#
# CONFUSION ALERT:  the host and port of the user are actually not of the
#                   other user's conference process but their registrar
#                   client.  we therefore ask the other registrar client
#                   for the host and port of the conference itself.  among
#                   other things, this is a good hook for future security.
#                   we may need to retry if the other conference is not
#                   yet hooked up to its registrar client.  after getting
#                   the right host and port we tell our conference to connect.
#
proc gk_joinTo {usernum confnum} {
    set remote [dp_MakeRPCClient [confs c.$confnum.users.$usernum.host] \
	    [confs c.$confnum.users.$usernum.port]]
    dp_RDO $remote _gk_reqConfAddress $confnum
}

# return the host and port number of a connected conference (see gk_joinTo)
#
proc _gk_reqConfAddress confnum {
  if {[spawned remote.$confnum.host]==""} {set addr ""} \
  else {
    keylset addr host [spawned remote.$confnum.host] \
	    port [spawned remote.$confnum.port]
  }
  dp_RDO [rpcFile] _gk_receiveConfAddress $confnum $addr
}

proc _gk_receiveConfAddress {confnum addr} {
    close [rpcFile]
    if   {$addr == ""} {
	after 1000 gk_joinTo $usernum $confnum
    } else {
	global env
	gk_toConf $confnum [concat _gk_connectTo \{$addr\}]
	set unum [spawned remote.$confnum.localuser]
	set name [userprefs name]
	set display $env(DISPLAY)
	gk_toConf $confnum "_gksm_adduser $unum <$name> <$display>"
    }
    
}

# join to all others in a conference; a convenience
#
proc gk_joinToOthers {confnum myusernum} {
    foreach i [confs keys c.$confnum.users] {
	if {$i<$myusernum} {gk_joinTo $i $confnum}
    }
}




#########################################################################
# provide some "standard"  handlers for registration events
#


# after a conference is killed (gracefully or otherwise), tell the
# central registrar that the user has left the conference.
# good situations to call this:
#      - when a user fails to hook up to the registrar client
#      - when a conference process has died ("conferenceDied" event)
#
proc gk_userLeft {confid userid fd} {
    gk_callLeaveConference $confid $userid
    gk_pollUsers $confid
}
 

# approve a new conference and add it to our internal lists; we rely on the 
# newconf environment set up in gk_foundNewConf
# good situations to call this:
#	- when we detect a new conference ("foundNewConf" event)
#
proc gk_addConfToList {} {
    confs import c.[newconf id] [newconf info]
    keylset ev type newConfApproved confnum [newconf id]
    gk_postEvent $ev
    gk_pollUsers [newconf id]
}


# approve a new user and add them to our internal lists; we rely on the 
# newuser environment set up in gk_foundNewUser
# good situations to call this:
#	- when we detect a new user ("foundNewUser" event)
#
proc gk_addUserToList {} {
    confs import c.[newuser conf].users.[newuser id] [newuser info]
    keylset ev type newUserApproved confnum [newuser conf] \
	usernum [newuser id]
    gk_postEvent $ev
    set name [confs c.[newuser conf].users.[newuser id].username]
    set display [confs c.[newuser conf].users.[newuser id].display]
    gk_toConf [newuser conf] "_gksm_adduser [newuser id] <$name> <$display>"
}


# approve a deleted user and remove them from our lists; we rely on the 
# deluser environment set up in gk_foundDeletedUser.  we also generate 
# a "lastUserLeftConf" event if the last user is removed from a conference
# good situations to call this:
#	- when we detect a deleted user ("foundDeletedUser" event)
#

proc gk_removeUserFromList {} {
    set conf [deluser conf]; set user [deluser user]
    keylset ev type deleteUserApproved confnum $conf usernum $user
    gk_postEvent $ev

    if {[member $user [confs keys c.$conf.users]]} {
	catch {gk_toConf $conf "_gk_removeUser $user"}
	confs delete c.$conf.users.$user
	if {[llength [confs keys c.$conf.users]]==0} {
            set pers [confs c.$conf.will_persist]
            if {$pers=="yes"} {
	      set type lastUserLeftPeristentConf
	    } else {
	      set type lastUserLeftConf
	    }
	    keylset event type $type confnum $conf
	    gk_postEvent $event
	}
    }
   gk_toConf $conf "_gksm_removeuser $user"
    set type [confs c.$conf.conftype]
    if {[userprefs prog.$type.originator_only]!="yes"} {
	if {[spawned remote.$conf.localuser]==$user} {
	    spawned delete remote.$conf
	}
    }
}


# approve a deleted conference and remove it from our lists; we rely on
# the delconf environment set up in gk_foundDeletedConf
# good situations to call this:
#	- when we detect a deleted conference ("foundDeletedConf" event)
#
proc gk_removeConfFromList {} {
    set conf [delconf id]
    keylset ev type deleteConfApproved confnum $conf
    gk_postEvent $ev
    if {[lsearch [confs keys c] $conf]!=-1} {
	catch {gk_toConf $conf _gk_deleteConf}
	confs delete c.$conf
    }
    set fd [spawned remote.$conf.filedesc]
    if {$fd!=""} {
	close $fd
    }
}



#########################################################################
# we still need to implement the following events
#


#gk_on {[gk_event type]=="confDidNotConnect"} \
#      {gk_userLeft [gk_event conf] [gk_event user] no}
#gk_on {[gk_event type]=="reqConfAddress"} \
#      {dp_RDO _gk_addrResp [gk_event conf] [gk_event host] [gk_event port]}
#gk_on {[gk_event type]=="receivedConfAddress"} \
#      {gk_toConf [gk_event conf] [concat _gk_connectTo [gk_event host] [gk_event port]]}



#########################################################################
# optionally help keep track of which conferences we're joined to
# (this is different than spawned, given the time delay in creation)
#


gk_newenv joinedConfs


# we are now joined to the given conference
#
proc gk_conferenceJoined confnum {
    joinedConfs c.$confnum yes
}


# we are now not joined to the given conference
#
proc gk_noLongerJoined confnum {
    joinedConfs delete c.$confnum
}


# return the list of the conferences we're joined to
#
proc gk_confsJoined {} {
    return [joinedConfs keys c]
}


# check if we're already joined to a particular conference
#
proc gk_alreadyJoined confnum {
  return [member $confnum [gk_confsJoined]]
}



#########################################################################
# some interface bits for registrar clients to create a conference menu
#


# create the menu and fill with conference types from user preferences
#
proc buildConferenceMenu {menu} {
    $menu delete 0 last
    set conferences [lsort [userprefs keys prog]]
    set grouplist ""
    set maingroup ""
    foreach conference $conferences {
      set groupname [userprefs prog.$conference.group]
      if {$groupname!=""} {
        if {![member $groupname $grouplist]} {
          lappend grouplist $groupname
          set group($groupname) ""
        }
        lappend group($groupname) $conference
      } else {
        lappend maingroup $conference
      }
    }
    
    set counter 1
    foreach groupname $grouplist {
      catch {destroy $menu.sub$counter}
      menu $menu.sub$counter
      foreach prog $group($groupname) {
        set confname $prog
        if {[userprefs prog.$prog.name]!=""} {
          set confname [userprefs prog.$prog.name]
        }
        $menu.sub$counter add command -label $confname \
           -command "_gk_ConfName [list $prog] [list $confname]"
      }
      $menu add cascade -label $groupname -menu $menu.sub$counter
      incr counter
    }
    foreach prog $maingroup {
      set confname $prog
      if {[userprefs prog.$prog.name]!=""} {
        set confname [userprefs prog.$prog.name]
      }
      $menu add command -label $confname \
         -command "_gk_ConfName [list $prog] [list $confname]"
    }
}


# invoked after a user chooses a conference from the conference menu.
# pop up a dialog box showing the user the default conference name.
# allow them to change the conference name and/or cancel the conference.
#
proc _gk_ConfName {conference_type conference_name} {  global conftype confName

    set conftype $conference_type
    set confName $conference_name
    if {[userprefs conference_prompt_name]=="no"} {
        _gk_doNewConf
	return
    }

    toplevel .dlg
    wm title .dlg "Conference Name"

    # OK/Cancel buttons
    frame .dlg.buttons 
    button .dlg.buttons.cancel -text Cancel \
	-command "grab release .dlg; destroy .dlg"
    button .dlg.buttons.ok -text Create \
	-command "grab release .dlg; destroy .dlg; _gk_doNewConf"
    pack .dlg.buttons.cancel .dlg.buttons.ok  -side right

    # Descriptive label and changeable conference name entry
    label .dlg.lbl -text "Conference name" 
    entry .dlg.name -relief sunken -textvariable confName
    # Put it all together
    pack .dlg.lbl -side top 
    pack .dlg.name -side top -fill x
    pack .dlg.buttons -side bottom -fill x
    focus .dlg.name
    .dlg.name select from 0
    .dlg.name select to end
    bind .dlg.name <Return> "grab release .dlg; destroy .dlg; _gk_doNewConf"
    bind .dlg.name <Escape> "grab release .dlg; destroy .dlg" 
    grab .dlg
}


# this command is called in response to pressing the "Ok" button in the
# new conferences dialog box.  create the desired conference.
#
proc _gk_doNewConf {} {	global confName conftype
    set host [userprefs host]
    set myport [userprefs myport]
    keylset conf confname $confName conftype $conftype originator [gk_uniqprogid]

    if {[string compare $confName "Conference name"] == 0} {
	keylset conf confname [keylget conf conftype]}

    keylset conf type userRequestedNewConf
    gk_postEvent $conf
}


# when the user requests a conference to be created, create it
#
proc gk_createRequestedConf {} {
    keylset conf confname [gk_event confname] conftype [gk_event conftype] \
	originator [gk_event originator]
    gk_callNewConference $conf
    gk_pollConferences
}

#########################################################################
# routine to remove a user from all of the conferences
#
# remove a user from all of the conference they are currently in.
proc gk_killAllMyConfs {} {
     foreach conf [spawned keys remote.] {
         gk_toConf $conf _gk_deleteConf
         spawned delete c.$conf
     }
}


# routines called from the conferences
#

proc _gk_reqChangeConfInfo {confnum info val} {
  _gk_toReg _gk_changeConfInfo $confnum $info $val
}

proc _gk_dochangeConfInfo {confnum info val} {
  if {[member $confnum [confs keys c]]} {
    confs c.$confnum.$info $val
  }
}


##################################################################

#
# invitation protocol
#

gk_newenv invitations
invitations set misc.nextid 1

proc gk_inviteToConf {conf} {
  if {[winfo exists .invite]} {
    return
  }
  invitations set misc.forconf $conf
  toplevel .invite
  wm title .invite "Invitation for [confs c.$conf.confname]"
  label .invite.lbl -text "Select user to invite into [confs c.$conf.confname]"
  pack .invite.lbl -side top -padx 5 -pady 5
  frame .invite.users
  pack [listbox .invite.users.list -yscroll ".invite.users.scroll set" \
        -selectmode single]  -side left
  pack [scrollbar .invite.users.scroll -command ".invite.users.list yscroll"] \
    -side left -fill y
  pack .invite.users -side left -padx 5 -pady 5
  frame .invite.buttons
  pack [button .invite.buttons.ok -text Invite -command "_gk_do_invite" -state disabled] \
    -side top -padx 5 -pady 5 -fill x
  pack [button .invite.buttons.cancel -text Cancel -command "_gk_cancel_invite"] \
    -side top -padx 5 -pady 5 -fill x
  pack .invite.buttons -side left -padx 5 -pady 5
  foreach i [regclients keys remote] {
    if {$i!=[gk_rc_getMyID]} {
      .invite.users.list insert end "[regclients remote.$i.name]                                             $i"
    }
  }
  bind .invite.users.list <ButtonRelease-1> "_gk_invite_fixbutton"
  bind .invite.users.list <Double-1> "_gk_do_invite"
  grab .invite
}

proc _gk_invite_fixbutton {} {
  set useridx [.invite.users.list curselection]
  if {$useridx==""} {
    .invite.buttons.ok configure -state disabled
  } else {
    .invite.buttons.ok configure -state normal
  }
}

proc _gk_do_invite {} {
  grab release .invite
  set useridx [.invite.users.list curselection]
  if {$useridx!=""} {
    set username [.invite.users.list get $useridx]
    set user [lindex $username [expr [llength $username]-1]]
    gk_sendInvitation $user [invitations get misc.forconf]
  }
  destroy .invite    
}

proc _gk_cancel_invite {} {
  grab release .invite
  destroy .invite
}

proc gk_sendInvitation {to conf} {
  set suffix [invitations misc.nextid]
  invitations misc.nextid [expr $suffix+1]
  set myid [gk_rc_getMyID]
  set invitation $myid-$suffix
  invitations sent.$invitation.to $to
  invitations sent.$invitation.conf $conf
  gk_sendToRC $to gk_receiveInvitation $invitation $myid $conf  
}

proc gk_receiveInvitation {invitation from conf} {
  invitations received.$invitation.from $from
  invitations received.$invitation.conf $conf
  keylset event type invitationReceived invitation $invitation
  gk_postEvent $event
}

gk_on {[gk_event type]=="invitationReceived"} {
  gk_handleReceivedInvitation [gk_event invitation]
}

proc gk_handleReceivedInvitation {invitation} {
  set response [userprefs invitationResponse]
  if {$response==""} {set response prompt}
  if {$response=="accept"} {
    gk_acceptInvitation $invitation
  } elseif {$response=="reject"} {
    gk_rejectInvitation $invitation
  } elseif {[gk_alreadyJoined [invitations received.$invitation.conf]]} {
    gk_acceptInvitation $invitation
  } else {
    set w .invite$invitation
    toplevel $w
    wm title $w Invitation
    set fromid [invitations received.$invitation.from]
    set fromname [regclients remote.$fromid.name]
    set confid [invitations received.$invitation.conf]
    set confname [confs c.$confid.confname]
    message $w.msg -aspect 600 -text "$fromname has invited you to join $confname."
    frame $w.buttons
    button $w.buttons.accept -text "Accept" -command "gk_acceptInvitation $invitation"
    button $w.buttons.deny -text "Send Regrets" -command "gk_rejectInvitation $invitation"
    pack append $w.buttons \
      $w.buttons.accept {left padx 10 pady 10} \
      $w.buttons.deny {left padx 10 pady 10}
    pack append $w \
      $w.msg {top padx 10 pady 10} \
      $w.buttons {top padx 10 pady 10}
  }
}

proc gk_acceptInvitation {invitation} {
   gk_sendToRC [invitations received.$invitation.from] gk_rsvp $invitation accept
   if {[winfo exists .invite$invitation]} {
     destroy .invite$invitation
   }
   gk_callJoinConference [invitations received.$invitation.conf]
   gk_pollUsers [invitations received.$invitation.conf]
   invitations delete received.$invitation
}

proc gk_rejectInvitation {invitation} {
  gk_sendToRC [invitations received.$invitation.from] gk_rsvp $invitation deny
   if {[winfo exists .invite$invitation]} {
     destroy .invite$invitation
   }
   invitations delete received.$invitation
}

proc gk_rsvp {invitation response} {
   invitations delete sent.$invitation
}

