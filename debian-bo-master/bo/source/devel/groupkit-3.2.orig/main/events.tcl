######################################################################
#
# event handling
#
#   See the user documentation for details.
#
######################################################################

#
# __gk_eventdata is a keyed list which holds info about the current event
# being executed; it is used by gk_event
#

set __gk_eventdata ""


#
# __gk_triggerlist is a list of triggers, where each list element in
# the list is a keyed list containing keys "pattern" and "action"
#

set __gk_triggerlist ""


#
# add a new trigger to the end of the trigger list
#

proc gk_on {pattern action} {
	global __gk_triggerlist
	keylset elt pattern $pattern action $action
	lappend __gk_triggerlist $elt
}


#
# while an event is "active", i.e. has been posted, return its fields
# by interrogating __gk_eventdata
#

proc gk_event field {
	global __gk_eventdata
	return [keylget __gk_eventdata $field]
}


#
# generate an event by passing in the keyed list describing the event;
# set the data into __gk_eventdata so it can be retrieved by triggers
# (by the gk_event function above), and then loop through each trigger,
# and execute the actions of those that match the event.
#

set __gk_pattern ""
set __gk_action ""
proc gk_postEvent event {
    catch {
	gk_debug "EVENT([keylget event type]) is $event"
    }
	global __gk_eventdata __gk_triggerlist __gk_pattern __gk_action
	foreach i $__gk_triggerlist {
		set __gk_pattern [keylget i pattern]
		set __gk_action [keylget i action]
		set __gk_eventdata $event
#		catch {
		   uplevel #0 {
			if $__gk_pattern {
				eval $__gk_action
			} 
		   }
#		}
	}
}


