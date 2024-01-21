# Copyright (c) 1993 by Sanjay Ghemawat
#############################################################################
# Dialog for getting range of time from a day.
#
# Commands
#
#	get_time_range <leader> <msg> <varstart> <varfinish>
#
#	Allow user to edit time range.  The initially displayed range
#	is taken from the value of the variables named by <varstart>
#	and <varfinish>.  If user confirms the dialog, set <varstart>
#	and <varfinish> to the new values and return 1.  Else
#	return 0.
#
#	<varstart> and <varfinish> take integral values in the range 0..24
#	to represent hours.

class TimeRange {w gvar} {
    set slot(window) $w
    set slot(gvar) $gvar

    toplevel $w -class Dialog
    wm title $w "Set Time Range"
    wm protocol $w WM_DELETE_WINDOW [list set $gvar 0]
    
    frame $w.mid -class Pane

    make_buttons $w.bot 1 {
	{Cancel		{set tr_done 0}}
	{Okay		{set tr_done 1}}
    }

    frame $w.mid.top
    frame $w.mid.bot
    label $w.mid.mid -text to

    scale $w.start -from 0 -to 24 -tickinterval 0\
	-orient horizontal -length 3i\
	-showvalue 0 -command [list $self start]

    label $w.slabel -text "" -width 8

    scale $w.finish -from 0 -to 24 -tickinterval 0\
	-orient horizontal -length 3i\
	-showvalue 0 -command [list $self finish]

    label $w.flabel -text "" -width 8

    message $w.msg -aspect 400 -text {Edit time range} -relief raised -bd 1

    pack $w.slabel -in $w.mid.top -side left
    pack $w.start -in $w.mid.top -side right -fill x

    pack $w.flabel -in $w.mid.bot -side left
    pack $w.finish -in $w.mid.bot -side right -fill x

    pack $w.mid.top -side top -fill x -padx 3m
    pack $w.mid.mid -side top -fill x
    pack $w.mid.bot -side top -fill x -padx 3m

    pack $w.msg -side top -fill x
    pack $w.mid -side top -fill x
    pack $w.bot -side top -fill x

    bind $w <Control-c> {set tr_done 0}
    bind $w <Return>    {set tr_done 1}

    $self edit 0 24

    wm withdraw $w
    update
}

method TimeRange start {value} {
    if {$value >= [$slot(window).finish get]} {
	set value [expr [$slot(window).finish get] - 1]
	$slot(window).start set $value
    }
    
    $slot(window).slabel config -text [time2text [expr $value*60]]
}
    
method TimeRange finish {value} {
    if {$value <= [$slot(window).start get]} {
	set value [expr [$slot(window).start get] + 1]
	$slot(window).finish set $value
    }
    
    $slot(window).flabel config -text [time2text [expr $value*60]]
}

method TimeRange edit {start finish } {
    $slot(window).start set $start
    $slot(window).finish set $finish
    $self start $start
    $self finish $finish
}

proc get_time_range {leader msg varstart varfinish} {
    upvar $varstart start
    upvar $varfinish finish

    global tr_done
    set tr_done -1

    if ![winfo exists .tr] {
	TimeRange-with-name time_range_dialog .tr tr_done
    }

    time_range_dialog edit $start $finish
    .tr.msg configure -text $msg
    dialog_run $leader .tr tr_done
    if $tr_done {
	set start [.tr.start get]
	set finish [.tr.finish get]
    }
    return $tr_done
}
