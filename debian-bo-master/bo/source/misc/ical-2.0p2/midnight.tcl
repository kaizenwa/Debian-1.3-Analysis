# Copyright (c) 1993 by Sanjay Ghemawat
###############################################################################
# Fire midnight signals

# effects Startup a thread that will do the right thing at each midnight.
proc start_midnight_thread {} {
    _midnight_schedule_thread
}

###############################################################################
# Internal procedures

# effects Send a midnight signal and schedule the thread to run at next
#	  midnight.
proc _midnight_thread {} {
    _midnight_fire
    _midnight_schedule_thread
}

# effects Send a midnight signal
proc _midnight_fire {} {
    trigger fire midnight
}

# effects Schedule _midnight_thread to run at next midnight
proc _midnight_schedule_thread {} {
    set now [time now]
    set split [time split $now]
    set offset [expr "([lindex $split 0]*60*60 +\
		       [lindex $split 1]*60 +\
		       [lindex $split 2])"]

    # Find time remaining today and schedule an event after that much time.
    # Note: the "after" command takes time in milliseconds.
    after [expr (24*60*60 - $offset)*1000] _midnight_thread
}
