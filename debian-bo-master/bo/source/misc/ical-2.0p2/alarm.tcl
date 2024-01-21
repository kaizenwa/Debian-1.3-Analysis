# Copyright (c) 1993 by Sanjay Ghemawat
#######################################
#
# Alarms
#
# Rep
#	alarms		list of alarm times in minutes
#	pending		list of pending alarms; each element is list --
#				{<item> <fire time> <appt start time>}

class Alarmer {} {
    set slot(alarms)	[list 0 5 10 15]
    set slot(pending)	""

    # Consider more alarms once every six hours
    thread_periodic [expr 6*60*60*1000] [list $self recompute]

    # Check new alarms on minute boundaries
    thread_minute [list $self fire]

    trigger on add	[list $self add]
    trigger on delete	[list $self remove]
    trigger on change	[list $self change]
    trigger on flush	[list $self recompute]

    # Initial recomputation
    $self recompute
}

# effects - Recompute alarm list
method Alarmer recompute {} {
    # Incorporate new items
    set slot(alarms) [cal option DefaultAlarms]
    set slot(pending) ""

    set now [time now]
    set today [date today]
    set midnight [$self midnight]

    cal query $today $today i d {
	if [$i is appt] {
	    $self appt $i [expr $midnight+[$i starttime]*60] $now
	}
    }

    cal query [expr $today+1] [expr $today+1] i d {
	if [$i is appt] {
	    $self appt $i [expr $midnight+(24*60*60)+[$i starttime]*60] $now
	}
    }
}

# effects - Merge newly added item into pending list
method Alarmer add {item} {
    if ![$item is appt] return

    set now [time now]
    set midnight [$self midnight]

    set added 0
    set today [date today]
    if [$item contains $today] {
	$self appt $item [expr $midnight+[$item starttime]*60] $now
	set added 1
    }

    if [$item contains [expr $today+1]] {
	$self appt $item [expr $midnight+(24*60*60)+[$item starttime]*60] $now
	set added 1
    }
}

# effects - Modify pending list to reflect change in specified item.
method Alarmer change {item} {
    $self remove $item
    $self add $item
}

# effects - Get time today started
method Alarmer midnight {} {
    set now [time now]
    set split [time split $now]
    set offset [expr "([lindex $split 0]*60*60 +\
		       [lindex $split 1]*60 +\
		       [lindex $split 2])"]

    return [expr $now-$offset]
}

# effects - Add appt to pending list
#	appt		appointment handle
#	time		time of occurrence
#	now		current time
method Alarmer appt {appt time now} {
    if [catch {set alarms [$appt alarms]}] {
	set alarms $slot(alarms)
    }

    foreach a $alarms {
	set t [expr $time-($a*60)]
	if {$t < $now} {
	    continue
	}

	lappend slot(pending) [list $appt $t $time]
    }
}

# effects - Remove pending entries for specified item
method Alarmer remove {item} {
    set pending ""
    foreach x $slot(pending) {
	if {[lindex $x 0] != $item} {
	    lappend pending $x
	}
    }
    set slot(pending) $pending
}

# effects - Fire pending alarms
method Alarmer fire {} {
    set pending ""

    # Fire alarms a little early (helps bypass round-off errors)
    set now [expr [time now]+5]

    foreach x $slot(pending) {
	if {[lindex $x 1] > $now} {
	    # Not time to fire yet
	    lappend pending $x
	    continue
	}

	# Close active notices
	trigger fire kill_alarm [lindex $x 0]

	# Create alarm
	AlarmNotice [lindex $x 0] [lindex $x 2]
	run-hook alarm-fire [lindex $x 0]
    }

    set slot(pending) $pending

    # Update clocks in notices
    trigger fire update_alarms
}

#### Alarm notices ####

class AlarmNotice {item starttime} {
    set slot(item) $item
    set slot(starttime) $starttime

    toplevel .$self -class Reminder

    wm title .$self Reminder
    wm iconname .$self Reminder
    wm protocol .$self WM_DELETE_WINDOW [list class_kill $self]

    set g [option get .$self geometry Geometry]
    if {$g != ""} {
	catch {wm geometry .$self $g}
    }

    # Buttons
    make_buttons .$self.bot 0\
	[list\
	     [list {Snooze}		[list class_kill $self]]\
	     [list {No More Alarms}	[list AN_shutup $self]]]
				   
    # Display
    set str [$item text]
    regsub -all "\n\$" $str "" str
    set lines [llength [split $str "\n"]]
    if {$lines < 4} {set lines 4}

    set st [time2text [$item starttime]]
    set fi [time2text [expr [$item starttime]+[$item length]]]


    frame .$self.top -class Pane
    label .$self.head -text "Appointment from $st to $fi"
    label .$self.foot -text ""

    text .$self.text -width 50 -height $lines -wrap word -relief groove
    .$self.text insert insert $str
    .$self.text configure -state disabled

    # Pack everything
    pack .$self.head -in .$self.top -side top -fill x
    pack .$self.text -in .$self.top -side top -padx 10m -pady 10m
    pack .$self.foot -in .$self.top -side bottom -fill x

    pack .$self.top -side top -expand 1 -fill x
    pack .$self.bot -side bottom -fill x

    # Key bindings
    bind .$self <Control-c> [list class_kill $self]
    bind .$self <Return> [list class_kill $self]

    # Triggers
    trigger on delete		[list AN_item_kill $self]
    trigger on change		[list AN_item_kill $self]
    trigger on kill_alarm	[list AN_item_kill $self]
    trigger on flush		[list AN_check_kill $self]
    trigger on update_alarms	[list $self countdown]

    bell
}

method AlarmNotice destructor {} {
    trigger remove delete		[list AN_item_kill $self]
    trigger remove change		[list AN_item_kill $self]
    trigger remove kill_alarm		[list AN_item_kill $self]
    trigger remove flush		[list AN_check_kill $self]
    trigger remove update_alarms	[list $self countdown]

    destroy .$self
}

# These are not methods because they need to delete the notice

# effects  Kill the specified alarm notice and also remove any
#	   pending alarms for the same item.
proc AN_shutup {object} {
    alarmer remove [$object item]
    class_kill $object
}

# effects  Kill the specified alarm notice if it belongs to the
#	   specified item.
proc AN_item_kill {object item} {
    if ![string compare [$object item] $item] {
	class_kill $object
    }
}

# effects  Kill the specified alarm notice if its corresponding item
#	   no longer exists.
proc AN_check_kill {object} {
    if ![string compare [info commands [$object item]] {}] {
	class_kill $object
    }
}

method AlarmNotice item {} {
    return $slot(item)
}

method AlarmNotice countdown {} {
    set now [time now]
    if {$now > $slot(starttime)} {
	catch {.$self.foot configure -text "Late for Appointment"}

	# No more need for countdown triggers
	catch {trigger remove update_alarms [list $self countdown]}
    } else {
	set min [format "%.f" [expr ($slot(starttime)-$now)/60]]
	catch {.$self.foot configure -text "in $min minutes"}
    }
}

#### Create alarmer instance ####

proc start_alarmer {} {
    Alarmer-with-name alarmer
}

#### Thread that runs at minute boundaries ####

proc thread_minute {cmd} {
    set id [_thread_alloc]
    _thread_minute_schedule $id $cmd
    return $id
}

proc _thread_minute_schedule {id cmd} {
    # Schedule next firing (at minute boundary)
    set msec [expr "(60-[time second [time now]])*1000"]
    if {$msec <= 0} {set msec 1}
    after $msec [list _thread_minute_fire $id $cmd]
}

proc _thread_minute_fire {id cmd} {
    global _thread
    if ![info exists _thread(run:$id)] {return}

    eval $cmd
    _thread_minute_schedule $id $cmd
}

#### Test list ####

# - Alarms go off
# - Alarm notices count down properly
# - Item changes/deletions kill alarms
# - Item deletions by other calendar instances kills alarms
# - Alarm notices aren't duplicated
#*- Shutup disables alarms
