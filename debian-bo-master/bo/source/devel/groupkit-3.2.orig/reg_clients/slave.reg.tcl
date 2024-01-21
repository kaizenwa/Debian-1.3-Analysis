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
wm withdraw .

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

proc slave_doJoin confIndex {  global openrc
    set confnum [slave_confFromListbox $confIndex]
    if {$confnum!=""} {
	if {![gk_alreadyJoined $confnum]} {
	    gk_conferenceJoined $confnum
	    gk_callJoinConference $confnum
	    gk_pollUsers $confnum
            confs mtg.facilitator $confIndex
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

# all new conferences are valid, and we add them to our list
gk_on {[gk_event type]=="foundNewConf"} {gk_addConfToList}


# all new users are valid, and we add them to our list
gk_on {[gk_event type]=="foundNewUser"} {gk_addUserToList}


# when we are new to a conference, we create it and join to existing users
gk_on {([gk_event type]=="foundNewUser") && 
       ([gk_event host]==[userprefs host]) &&
       ([gk_event port]==[userprefs port])} { 
           if {[confs c.[gk_event confnum].conftype]!="Facilitated Meeting"} {
	     gk_createConference [gk_event confnum] [gk_event usernum]; 
	     gk_joinToOthers [gk_event confnum] [gk_event usernum]
   	    } else {slave_joinedFacilMeeting [gk_event confnum] [gk_event usernum]         }
}

# all deleted users are removed from the lists
gk_on {[gk_event type]=="foundDeletedUser"} {gk_removeUserFromList}


# when a conference connected to us died, report the user left
gk_on {[gk_event type]=="conferenceDied"} \
      {gk_userLeft [gk_event conf] [gk_event user] [gk_event filedesc]}


# all deleted conferences are removed from lists
gk_on {[gk_event type]=="foundDeletedConf"} {gk_removeConfFromList}


# if we are removed from the facilitated conference, just die
gk_on {([gk_event type]=="foundDeletedUser")&&\
        ([gk_event usernum]==[confs mtg.usernum]) && \
	([gk_event confnum]==[confs mtg.confnum])} \
    {destroy .}

# if our user is removed from the conference we are no longer joined
gk_on {([gk_event type]=="foundDeletedUser")&&\
        ([gk_event usernum]==[spawned c.[gk_event confnum].localuser])} \
    {gk_noLongerJoined [gk_event confnum]}

#================================================================
# This portion lets the individual chose a meeting to join.
#
#

toplevel .pick
wm title .pick "Meeting Gateway"
set italic -Adobe-helvetica-bold-o-normal--*-140-*
frame .pick.f2
pack append [frame .pick.f2.conflist -relief flat] \
    [label     .pick.f2.conflist.label -text "Choose a Meeting" -font $italic] top \
    [scrollbar .pick.f2.conflist.scroll -relief ridge \
	       -command ".pick.f2.conflist.list yview"] {right filly} \
    [listbox   .pick.f2.conflist.list -yscroll ".pick.f2.conflist.scroll set" \
	       -relief ridge -width 20 -height 6 -exportselection false \
               -setgrid 1] {left expand fill}

#Build the screen
pack  .pick.f2 -side top
pack  .pick.f2.conflist -side left

#bind .pick.f2.conflist.list <1> {%W select from [%W nearest %y] }
bind .pick.f2.conflist.list <Double-1> {slave_doJoin [.pick.f2.conflist.list curselection]}
##tk_listboxSingleSelect .pick.f2.conflist.list

#############################################################################
##                                                                         ##
#                                                                           #
#  we need to watch when new or leaving users and conferences are           #
#  approved so that we can update our lists                                 #
#                                                                           #
##                                                                         ##
#############################################################################

gk_on {[gk_event type]=="newConfApproved"} \
    {slave_addConfToListbox [gk_event confnum]}
gk_on {[gk_event type]=="deleteConfApproved"} \
    {slave_delConfFromListbox [gk_event confnum]}


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
proc slave_confFromListbox index {  global openrc
    if {[catch {set urgh [lindex $openrc(listboxconfs) $index]}]==0} {
        return $urgh
    }
    return ""
}


# what is currently selected conference?
proc slave_selectedConference {} { 
    return [slave_confFromListbox [.pick.f2.conflist.list curselection]]
}


# add a conference to the list
proc slave_addConfToListbox confnum {    global openrc
    if {[confs c.$confnum.conftype]=="Facilitated Meeting"} {
        lappend openrc(listboxconfs) $confnum
        .pick.f2.conflist.list insert end [confs c.$confnum.confname]
    }
}

# delete a conference from the list
proc slave_delConfFromListbox confnum {  global openrc
    set idx [lsearch $openrc(listboxconfs) $confnum]
    set openrc(listboxconfs) [lreplace $openrc(listboxconfs) $idx $idx]
    catch {.pick.f2.conflist.list delete $idx}
}

#================================================================
# This portion add the individual to a particular meeting.
#
# Help information for the facilitated meeting
set facilSlave(help_title) "Facilitated Meeting"

set facilSlave(help_content) {
{normal} {This is an example of a }
{normalitalic} {facilitated meeting }
{normal} {system.  It lets you join a particular meeting and participate \
in it.  However, the meeting must be initiated by the facilitator first.

}
{normalbold} {Participation

} 
{normal} {Once you have joined a meeting you are entered on the \
facilitator's control panel.  Then the facilitator can select which \
tools you can join in on as well as remove you from ones you are in. \
So the facilitator has full control on what you can and can not do. 

}
{normalbold} {Menubar 

}
{normal} {In the file menu you can choose }
{normalitalic} {Quit }
{normal} {to quit the meeting.  Note that the facilitator can \
remove you from the facilitated meeting at anytime.  In the help menu \
choose one of the items to get more information on that topic. 

}}

# What to execute when the slave quits the facilitated meeting.
proc slave_quit {} {
     gk_toConf [confs mtg.confnum] \
                  [list facil_remove_user_from_mtg [confs mtg.usernum]]
     keylset event type facilSlaveQuitting usernum [confs mtg.usernum] \
          confnum [confs mtg.confnum]
     gk_postEvent $event
     _gk_properQuit
}

# Make sure slave is removed from everything when they quit.
# if either the facilitator or i quit the metting then kill off all my
# conferences
gk_on {[gk_event type]=="facilClientQuitting"} {gk_killAllMyConfs}
gk_on {[gk_event type]=="facilSlaveQuitting"} {gk_killAllMyConfs}

proc slave_joinedFacilMeeting {confnum usernum} {global facilSlave
  destroy .pick
  toplevel .mtg
  wm title .mtg "Meeting Notice"
  confs mtg.confnum $confnum
  confs mtg.usernum $usernum
  gk_defaultMenu .mtg.menubar 

  # Remove the Collaboration menu from the default menubar
  .mtg.menubar delete collaboration

   # Change the command executed by Quit in the file menu
   .mtg.menubar itemcommand file entryconfigure 1 -command slave_quit

   # Add to the help menu
   .mtg.menubar itemcommand help add command \
         -label $facilSlave(help_title) \
         -command "gk_topicWindow .helpWindow \
                 -title {$facilSlave(help_title)} \
                 -text {$facilSlave(help_content)}"

  label .mtg.lbl -text "You are in meeting [confs c.$confnum.confname]."
  pack append .mtg .mtg.menubar {top fillx}
  pack .mtg.lbl

}
