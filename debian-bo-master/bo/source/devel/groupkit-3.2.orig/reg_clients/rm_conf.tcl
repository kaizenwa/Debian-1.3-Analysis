
gk_initGenericRC $argv

gk_pollConferences


proc rmconf_doDelete confIndex {  
    set confnum [rmconf_confFromListbox $confIndex]
    if {$confnum!=""} {
        gk_callDeleteConference $confnum
        gk_pollConferences
    }
}



# all new conferences are valid, and we add them to our list
gk_on {[gk_event type]=="foundNewConf"} {gk_addConfToList}

# all new users are valid, and we add them to our list
gk_on {[gk_event type]=="foundNewUser"} {gk_addUserToList}

# all deleted users are removed from the lists
gk_on {[gk_event type]=="foundDeletedUser"} {gk_removeUserFromList}

# all deleted conferences are removed from lists
gk_on {[gk_event type]=="foundDeletedConf"} {gk_removeConfFromList}


#================================================================
# The remainder of this module specifies the Tk interface for the Open
# Registrar Client.  


set rmconf(help_title) "Delete Zombie Conferences"

set rmconf(help_content) {
{normal} {This registration system lets you delete \"zombie\" \
conferences, which are sometimes created when GroupKit programs \
exit improperly.  

Selecting a conference will display the list of users in it, and \
allows you to make sure you're deleting the right conference.  To \
delete a conference, double click on its name.
}
}

option add *background grey

wm title . $rmconf(help_title)
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



#================================================================
# Make The Menu Bar Containing Pulldown Menus


# The default menu
gk_defaultMenu .menubar 

# Add an item to the help menu
.menubar itemcommand help add command \
         -label "$rmconf(help_title)" \
         -command "gk_topicWindow .helpWindow \
                      -title \"$rmconf(help_title)\" \
                      -text \"$rmconf(help_content)\""


# Remove the Collaboration menu from the default menubar
.menubar delete collaboration
#================================================================

#Build the screen
pack  .menubar -side top -fill x
pack  .f2 -side top
pack  .f2.conflist -side left
pack  .f2.userlist -side right

bindtags .f2.conflist.list {Listbox . all .f2.conflist.list}
bind .f2.conflist.list <1> {rmconf_updateUserList blat}
bind .f2.conflist.list <Double-1> {rmconf_doDelete [.f2.conflist.list curselection]}


#############################################################################
##                                                                         ##
#                                                                           #
#  we need to watch when new or leaving users and conferences are           #
#  approved so that we can update our lists                                 #
#                                                                           #
##                                                                         ##
#############################################################################

gk_on {[gk_event type]=="newConfApproved"} \
    {rmconf_addConfToListbox [gk_event confnum]}
gk_on {[gk_event type]=="deleteConfApproved"} \
    {rmconf_delConfFromListbox [gk_event confnum]}
gk_on {[gk_event type]=="newUserApproved"} \
    {rmconf_addUserToListbox [gk_event confnum] [gk_event usernum]}
gk_on {[gk_event type]=="deleteUserApproved"} \
    {rmconf_delUserFromListbox [gk_event confnum] [gk_event usernum]}


#############################################################################
##                                                                         ##
#                                                                           #
#  add and delete conferences and users;                                    #
#  also, keep a list mapping lines in our conference listbox to confnums    #
#                                                                           #
##                                                                         ##
#############################################################################

set rmconf(listboxconfs) ""


# given a line number in the listbox, what conference is it?
proc rmconf_confFromListbox index {  global rmconf
    if {[catch {set urgh [lindex $rmconf(listboxconfs) $index]}]==0} {
	return $urgh
    }
    return ""
}


# what is currently selected conference?
proc rmconf_selectedConference {} { 
    return [rmconf_confFromListbox [.f2.conflist.list curselection]]
}


# add a conference to the list
proc rmconf_addConfToListbox confnum {    global rmconf
    lappend rmconf(listboxconfs) $confnum
    .f2.conflist.list insert end [confs c.$confnum.confname]
    rmconf_updateUserList
}


# delete a conference from the list
proc rmconf_delConfFromListbox confnum {  global rmconf
    set idx [lsearch $rmconf(listboxconfs) $confnum]
    set rmconf(listboxconfs) [lreplace $rmconf(listboxconfs) $idx $idx]
    .f2.conflist.list delete $idx
    rmconf_updateUserList
}


# add a user
proc rmconf_addUserToListbox {confnum usernum} {  
    if {$confnum==[rmconf_selectedConference]} {rmconf_updateUserList}
}


# delete a user
proc rmconf_delUserFromListbox {confnum usernum} {  
    if {$confnum==[rmconf_selectedConference]} {after 1 rmconf_updateUserList}
}


# update the display of users for the currently selected conference
proc rmconf_updateUserList args {      global rmconf
    set confnum [rmconf_selectedConference]
    .f2.userlist.list delete 0 end
    if {$confnum!=""} {
	foreach i [confs keys c.$confnum.users] {
	    .f2.userlist.list insert end [confs c.$confnum.users.$i.username]
	}
    }
}



# Default Quit procedure.  It posts the event "regClientQuitting" and
# destroys the window.
proc rmconf_quit {} {
     keylset event type regClientQuitting 
     gk_postEvent $event
     _gk_properQuit
}
