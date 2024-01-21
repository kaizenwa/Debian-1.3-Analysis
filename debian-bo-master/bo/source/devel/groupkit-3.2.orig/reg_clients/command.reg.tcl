# command registrar client

wm withdraw .
gk_initGenericRC $argv

##
## policy section
##

# all new conferences are valid, and we add them to our list
gk_on {[gk_event type]=="foundNewConf"} {gk_addConfToList}

# for conferences that we originated, join to them right away
gk_on {([gk_event type]=="foundNewConf")&& \
    ([gk_event originator]==[gk_uniqprogid])} \
    { set our_conf [gk_event confnum]; gk_callJoinConference $our_conf; \
	gk_pollUsers $our_conf }

# all new users are valid, and we add them to our list
gk_on {[gk_event type]=="foundNewUser"} {gk_addUserToList}

# when we are new to a conference, we create it and join to existing users
gk_on {  ([gk_event type]=="foundNewUser") && 
         ([gk_event host]==[userprefs host]) &&
         ([gk_event port]==[userprefs port])} { 
	     set our_conf [gk_event confnum]; set usernum [gk_event usernum]
	     gk_createConference $our_conf $usernum
	     gk_joinToOthers $our_conf $usernum
	 }

# if our user is removed from the conference we are no longer joined
gk_on {([gk_event type]=="foundDeletedUser")&&\
        ([gk_event usernum]==[spawned c.[gk_event confnum].localuser])} \
    {after 1 "destroy ."}

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

gk_on {[gk_event type]=="foundDeletedConf"} {
  if {[gk_event confnum]==$our_conf} {
    after 1 "destroy ."
  }
}

##
## real code here
##

set target_name ""
set first_run 1
if {$argv!=""} {
	set target_name ""
	set i [expr [llength $argv]-1]
	while {($i>=0)&&([string range [lindex $argv $i] 0 0]!="-")} {
		if {$target_name==""} {
			set target_name [lindex $argv $i]
		} else {
			set target_name "[lindex $argv $i] $target_name"
		}
		incr i -1
	}
}

# now we have the list
gk_on {[gk_event type]=="confListProcessed"} {ConfsListStable}


proc ConfsListStable {} {
  global target_name first_run

# only execute this first time through

  if {$first_run==0} {return}
  set first_run 0

# if no target, dump out a list of existing names
  if {$target_name==""} {
    set names ""
    foreach i [confs keys c] {
      lappend names [confs c.$i.confname]
    }
    if {$names==""} {
      puts "No current conferences"
    } else {
      puts "Current conferences: $names"
    }
    after 1 "destroy ."

# target specified; try to find and join the conference

  } else {
    set found 0
    foreach i [confs keys c] {
      if {[confs c.$i.confname]==$target_name} {
	gk_callJoinConference $i
        set found 1
      }
    }

# if can't find it, create it

    if {$found==0} {
      keylset conf confname $target_name conftype $target_name \
	originator [gk_uniqprogid]
      gk_callNewConference $conf
      gk_pollConferences
    }
  }
}

gk_pollConferences

