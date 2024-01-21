#
# Open Registration Client
# ------------------------
#
# This module presents one possible Registrar Client (see rc.tcl for 
# information on Registrar Clients in general).  Within an open Registrar
# Client, any user can create a conference, and users can join any 
# conference.  Users can only delete themselves from the conference,
# and the last user leaving a conference closes down the entire conference
# (default behavior for Registrar Clients; see the gk_userLeft proc in rc.tcl).
#
# There are two parts to this module, the underlying protocol part, and a
# Tk interface to interact with the protocol.
#

gk_initGenericRC $argv

gk_pollConferences


#############################################################################
##                                                                         ##
#                                                                           #
#  join to an existing conference (called in response to a double-click     #
#  in the conferences listbox)                                              #
#                                                                           #
#  we find which conference was clicked on, and if we're not already        #
#  joined to it, we join now                                                #
#                                                                           #
##                                                                         ##
#############################################################################

proc openrc_doJoin confIndex {  global openrc
    set confnum [openrc_confFromListbox $confIndex]
    if {$confnum!=""} {
	if {![gk_alreadyJoined $confnum]} {
	    if {[userprefs prog.[confs c.$confnum.conftype].cmd]!=""} {
		gk_conferenceJoined $confnum
		gk_callJoinConference $confnum
		gk_pollUsers $confnum
	    } else {
		set w .notype
		if {[winfo exists $w]} {return}
		toplevel $w
		wm title $w "Oops!"
		pack [message $w.msg -aspect 500 -text "I can't find a GroupKit program to associate with conference \"[confs c.$confnum.confname]\" (which is of type \"[confs c.$confnum.conftype]\".  You may need to update your .groupkitrc file."] -side top
		pack [button $w.ok -text Dismiss -command "destroy $w"] -side top
	    }
	}
    }
}


#############################################################################
##                                                                         ##
#                                                                           #
#  specify all the policy of this registrar client, by setting triggers     #
#  on the necessary events.                                                 #
#                                                                           #
##                                                                         ##
#############################################################################

# Remove me from all of my conferences.
gk_on {[gk_event type]=="regClientQuitting"} {gk_killAllMyConfs}

# all new conferences are valid, and we add them to our list
gk_on {[gk_event type]=="foundNewConf"} {gk_addConfToList}


# for conferences that we originated, join to them right away
gk_on {([gk_event type]=="foundNewConf")&& \
    ([gk_event originator]==[gk_uniqprogid])} \
    { gk_conferenceJoined [gk_event confnum]; \
      gk_callJoinConference [gk_event confnum]; \
	gk_pollUsers [gk_event confnum] }


# all new users are valid, and we add them to our list
gk_on {[gk_event type]=="foundNewUser"} {gk_addUserToList}


# when we are new to a conference, we create it and join to existing users
gk_on {  ([gk_event type]=="foundNewUser") && 
         ([gk_event host]==[userprefs host]) &&
         ([gk_event port]==[userprefs port])} { 
	     set confnum [gk_event confnum]; set usernum [gk_event usernum]
	     gk_createConference $confnum $usernum
	     gk_joinToOthers $confnum $usernum
	 }

# if our user is removed from the conference we are no longer joined
gk_on {([gk_event type]=="foundDeletedUser")&&\
        ([gk_event usernum]==[spawned c.[gk_event confnum].localuser])} \
    {gk_noLongerJoined [gk_event confnum]}

# all deleted users are removed from the lists
gk_on {[gk_event type]=="foundDeletedUser"} {gk_removeUserFromList}

# when the last user of a conference leaves, we shut down the conference
gk_on {[gk_event type]=="lastUserLeftConf"} \
      {gk_callDeleteConference [gk_event confnum]; gk_pollConferences}


gk_on {[gk_event type]=="conferenceDied"} \
    {gk_noLongerJoined [gk_event conf]}

# when a conference connected to us died, report the user left
gk_on {[gk_event type]=="conferenceDied"} \
      {gk_userLeft [gk_event conf] [gk_event user] [gk_event filedesc]}


# all deleted conferences are removed from lists
gk_on {[gk_event type]=="foundDeletedConf"} {gk_removeConfFromList}



# when a user asks to create a new conference, create it as requested
gk_on {[gk_event type]=="userRequestedNewConf"} {gk_createRequestedConf}




# these still have to be built on the rc.tcl side of things

#gk_on {[gk_event type]=="confDidNotConnect"} \
#      {gk_userLeft [gk_event conf] [gk_event user] no}
#gk_on {[gk_event type]=="reqConfAddress"} \
#      {dp_RDO _gk_addrResp [gk_event conf] [gk_event host] [gk_event port]}
#gk_on {[gk_event type]=="receivedConfAddress"} \
#      {gk_toConf [gk_event conf] [concat _gk_connectTo [gk_event host] [gk_event port]]}


#================================================================
# The remainder of this module specifies the Tk interface for the Open
# Registrar Client.  
#
# This first part specifies the main window.  It contains two scrollable
# lists, one containing a list of all the conferences (their names actually)
# and the second a list of users for a particular conference (selected in
# the first list).  Double clicking on a conference allows you to join
# the conference.  A Conferences menu lets you create new conferences
# and assign them a name
#


set openrc(help_title) "Open Registration"

set openrc(help_content) {
{normal} {This is an example of a }
{normalitalic} {registration }
{normal} {system that lets you \
create and join conference applications.

}
{normalbold} {Seeing conferences and participants.
} 
{normal} {The Conference pane shows all conferences on the system,\
 whether created by you or by others. 

Selecting a conference from the Conferences pane will list the people\
 in it in the Participants pane.

}
{normalbold} {Creating a conference application.
}
{normal} {Select a conference application from the Conferences menu.

A dialog box then gives you the opportunity to rename\
 the conference if you wish, to proceed, or to cancel the operation.  

A window containing that conference application\
 will appear, and its name is added to the Conferences pane.

}
{normalbold} {Joining an existing conference.
}
{normal} {Double-click a conference listed in the Conferences pane. You\
will automatically join that conference if you are not already in it.\
The conference application will appear on your screen.

}
{normalbold} {Changing your name.
}
{normal} {The bottom field lets you change your name if you wish. However,\
 it will not affect conferences\
 that you have already joined.

}
{normalbold} {Seeing who is around.
}
{normal} {The \"Show Logged-in Users\" item in the Collaboration menu\
 will show you other users who are logged into the system and running\
 their own Open Registration client.  

}
{normalbold} {Inviting users into a conference.
}
{normal} {To invite someone else who's logged in to join a conference,\
 first select the conference in the pane on the left, and then choose\
 the \"Invite...\" item in the Collaboration menu.  You can then pick\
 the user to invite; they will receive a message on their screen\
 asking them if they'd like to join the conference.

}
{normalbold} {Reloading the .groupkitrc
}
{normal} {Selecting Re-initialize  from the File menu will re-load\
 the .groupkitrc file. This is usually done only\
 if you have changed the contents of your .groupkitrc on the fly.
}}

wm title . $openrc(help_title)
set italic -Adobe-helvetica-bold-o-normal--*-140-*
frame .f2
pack append [frame .f2.conflist -relief flat] \
    [label     .f2.conflist.label -text Conferences -font $italic] top \
    [scrollbar .f2.conflist.scroll -relief ridge \
	       -command ".f2.conflist.list yview"] {right filly} \
    [listbox   .f2.conflist.list -yscroll ".f2.conflist.scroll set" \
	       -relief ridge -width 20 -height 6 -exportselection false \
               -setgrid 1 -selectmode single] {left expand fill}

pack append [frame .f2.userlist -relief flat] \
    [label     .f2.userlist.label -text Participants -font $italic] top \
    [scrollbar .f2.userlist.scroll -relief flat  \
               -command ".f2.userlist.list yview"] {right filly} \
    [listbox   .f2.userlist.list -yscroll ".f2.userlist.scroll set" \
               -relief ridge  -width 15 -height 6 -exportselection false \
               -setgrid 1] {left expand fill}

set openrc(user_name) [userprefs name]
pack append [frame .username -relief flat] \
    [label .username.label -text "You are: " -font $italic] left \
    [entry .username.entry -relief sunken -textvariable openrc(user_name) \
               ] {left fillx expand} 


trace variable openrc(user_name) w {openrc_changeUserName}

proc openrc_changeUserName args { 
   global openrc
   userprefs name $openrc(user_name)
   regclients remote.[gk_rc_getMyID].name $openrc(user_name)
}


#================================================================
# Make The Menu Bar Containing Pulldown Menus


# The default menu
gk_defaultMenu .menubar 

# Add an item to the help menu
.menubar itemcommand help add command \
         -label "$openrc(help_title)" \
         -command "gk_topicWindow .helpWindow \
                      -title \"$openrc(help_title)\" \
                      -text \"$openrc(help_content)\""

# Add an item to the file menu that re-reads the .groupkitrc file
.menubar itemcommand file insert 1 command -label "Re-initialize" \
    -command openrc_reinit
.menubar itemcommand file insert 2 command -label "Pick Color..." \
    -command openrc_pickcolor
.menubar itemcommand file insert 3 separator
.menubar itemcommand file entryconfigure Quit -command openrc_quit

# Constructs the pulldown conference menu on the fly...
.menubar add confmenu 1 \
    -text Conferences 
.menubar itemconfigure confmenu \
    -postcommand "buildConferenceMenu .menubar.confmenu.menu"


.menubar itemcommand collaboration add command -label "Invite..." \
  -command {gk_inviteToConf [openrc_selectedConference]} -state disabled
#================================================================

#Build the screen
pack  .menubar -side top -fill x
pack  .f2 -side top
pack  .f2.conflist -side left
pack  .f2.userlist -side right
pack  .username -side left -fill x -expand yes

bindtags .f2.conflist.list {Listbox . all .f2.conflist.list}
bind .f2.conflist.list <1> {openrc_updateUserList blat}
bind .f2.conflist.list <Double-1> {openrc_doJoin [.f2.conflist.list curselection]}


#############################################################################
##                                                                         ##
#                                                                           #
#  we need to watch when new or leaving users and conferences are           #
#  approved so that we can update our lists                                 #
#                                                                           #
##                                                                         ##
#############################################################################

gk_on {[gk_event type]=="newConfApproved"} \
    {openrc_addConfToListbox [gk_event confnum]}
gk_on {[gk_event type]=="deleteConfApproved"} \
    {openrc_delConfFromListbox [gk_event confnum]}
gk_on {[gk_event type]=="newUserApproved"} \
    {openrc_addUserToListbox [gk_event confnum] [gk_event usernum]}
gk_on {[gk_event type]=="deleteUserApproved"} \
    {openrc_delUserFromListbox [gk_event confnum] [gk_event usernum]}


#############################################################################
##                                                                         ##
#                                                                           #
#  add and delete conferences and users;                                    #
#  also, keep a list mapping lines in our conference listbox to confnums    #
#                                                                           #
##                                                                         ##
#############################################################################

set openrc(listboxconfs) ""


# given a line number in the listbox, what conference is it?
proc openrc_confFromListbox index {  global openrc
    if {[catch {set urgh [lindex $openrc(listboxconfs) $index]}]==0} {
	return $urgh
    }
    return ""
}


# what is currently selected conference?
proc openrc_selectedConference {} { 
    return [openrc_confFromListbox [.f2.conflist.list curselection]]
}


# add a conference to the list
proc openrc_addConfToListbox confnum {    global openrc
    lappend openrc(listboxconfs) $confnum
    .f2.conflist.list insert end [confs c.$confnum.confname]
    openrc_updateUserList
}


# delete a conference from the list
proc openrc_delConfFromListbox confnum {  global openrc
    set idx [lsearch $openrc(listboxconfs) $confnum]
    set openrc(listboxconfs) [lreplace $openrc(listboxconfs) $idx $idx]
    .f2.conflist.list delete $idx
    openrc_updateUserList
}


# add a user
proc openrc_addUserToListbox {confnum usernum} {  
    if {$confnum==[openrc_selectedConference]} {openrc_updateUserList}
}


# delete a user
proc openrc_delUserFromListbox {confnum usernum} {  
    if {$confnum==[openrc_selectedConference]} {after 1 openrc_updateUserList}
}


# update the display of users for the currently selected conference
proc openrc_updateUserList args {      global openrc
    set confnum [openrc_selectedConference]
    .menubar itemcommand collaboration entryconfigure Invite... -state disabled
    .f2.userlist.list delete 0 end
    if {$confnum!=""} {
	foreach i [confs keys c.$confnum.users] {
	    .f2.userlist.list insert end [confs c.$confnum.users.$i.username]
	}
	.menubar itemcommand collaboration entryconfigure Invite... -state normal
    }
}



# Default Quit procedure.  It posts the event "regClientQuitting" and
# destroys the window.
proc openrc_quit {} {
     keylset event type regClientQuitting 
     gk_postEvent $event
     _gk_properQuit
}

# reinitialize from the .groupkitrc
proc openrc_reinit {} {
    userprefs delete prog
    uplevel #0 {source ~/.groupkitrc}
    global openrc; set openrc(user_name) [userprefs name]
}

# pick a color
proc openrc_pickcolor {} {
    gk_colorPicker "userprefs color"
}
