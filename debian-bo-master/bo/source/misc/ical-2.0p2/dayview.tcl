# Copyright (c) 1993 by Sanjay Ghemawat
##############################################################################
# DayView
#
# DESCRIPTION
# ===========
# A DayView shows the notices and appointments for one day.

set dayview_id 0

class DayView {} {
    # Generate new id for window
    global dayview_id
    incr dayview_id
    set n .dayview$dayview_id
    set slot(window) $n
    set slot(sel)  {}

    global ical_state ical_view
    lappend ical_state(views) $self
    set ical_view($n) $self

    toplevel $n -class Dayview

    set slot(apptlist) [ApptList $n.al $self]
    set slot(notelist) [NoteList $n.nl $self]
    set slot(dateeditor) [DateEditor $n.de [date today] $self]

    frame $n.status -class Pane
    label $n.cal -text ""
    label $n.rep -text ""
    frame $n.menu -class Pane

    $self build_menu

    # Pack windows
    pack $n.cal		-in $n.status -side left
    pack $n.rep		-in $n.status -side right
    pack $n.menu	-side top -fill x
    pack $n.status	-side bottom -fill x
    pack $n.al		-side right -expand 1 -fill both
    pack $n.nl		-side bottom -expand 1 -fill both
    pack $n.de		-side top -fill x

    $self reconfig

    wm title $n Calendar
    wm iconname $n ical
    wm protocol $n WM_DELETE_WINDOW [list ical_close]

    # Set-up geometry
    set g [option get $n geometry Geometry]
    if {$g != ""} {
	catch {wm geometry $n $g}
    }

    $self set_date [date today]

    # Set-up triggers.
    #
    # Save is not useful because dayview never keeps unsaved changes.
    # Add/delete/flush are not useful because we are only
    # interested in the currently selected item, and one of our
    # subwindows is going to tell us about it anyway.
    #
    # Therefore, we just need to listen for "change" so that we can
    # update the status window, and to "reconfig" so that we
    # can obey option changes.  We also need to listen for "midnight"
    # to automatically switch to next day.

    trigger on change	[list $self change]
    trigger on reconfig	[list $self reconfig]
    trigger on midnight [list $self midnight]
    trigger on keybind  [list $self rebind]
    trigger on select   [list $self check_selection]

    # Set-up key bindings
    bindtags $n [list $n IcalCommand Dayview all]
    $self rebind

    # User customization
    ical_with_view $self {run-hook dayview-startup $self}
}

method DayView destructor {} {
    ical_with_view $self {run-hook dayview-close [ical_view]}

    # Remove from list of registered views
    global ical_state ical_view
    lremove ical_state(views) $self
    catch {unset ical_view($slot(window))}

    trigger remove change   [list $self change]
    trigger remove reconfig [list $self reconfig]
    trigger remove midnight [list $self midnight]
    trigger remove keybind  [list $self rebind]
    trigger remove select   [list $self check_selection]

    class_kill $slot(apptlist)
    class_kill $slot(notelist)
    class_kill $slot(dateeditor)
    destroy $slot(window)
}

##############################################################################
# Routines needed by action procs.

# Return the toplevel window for the view
method DayView window {} {
    return $slot(window)
}

# Return the displayed date
method DayView date {} {
    return $slot(date)
}

# Set the displayed date
method DayView set_date {date} {
    set slot(date) $date
    $slot(dateeditor) set_date $date
    $slot(apptlist) set_date $date
    $slot(notelist) set_date $date

    ical_with_view $self {run-hook dayview-set-date $self $date}
}

method DayView check_selection {args} {
    if [string compare [ical_view] $self] {
	# This is not the current view
	$self clear_selection
	return
    }

    if [catch {set i [ical_find_selection]}] {
	# No current selection
	$self clear_selection
	return
    }

    if ![string compare $i $slot(sel)] {
	# Already selected
	return
    }

    # Need to clear old selection
    $self clear_selection

    if [$i contains $slot(date)] {
	# Item exists on current date
	$self set_selection $i
    }
}

method DayView set_selection {item} {
    set slot(sel) $item
    $self config_status
}

method DayView clear_selection {} {
    set slot(sel) {}
    $self config_status
}

method DayView appt_list {} {return $slot(apptlist)}
method DayView note_list {} {return $slot(notelist)}

##############################################################################
# Trigger callbacks

# Called at midnight by a trigger.  Advance to today if appropriate.
method DayView midnight {} {
    # Only advance if at previous date
    set today [date today]
    if {$slot(date) == ($today-1)} {$self set_date $today}
}

method DayView change {item} {
    if {$slot(sel) == $item} {
	$self config_status
    }
}

method DayView reconfig {} {
    set name $slot(window)

    # Geometry management
    set width [winfo pixels $name "[cal option ItemWidth]c"]

    set start [cal option DayviewTimeStart]
    set finish [cal option DayviewTimeFinish]
    wm grid $name\
	1\
	[expr ($finish - $start) * 2]\
	$width\
	[$slot(apptlist) line_height]
    wm minsize $name 1 10
    wm maxsize $name 1 48
}

##############################################################################
# Internal helper procs

method DayView config_status {} {
    set item $slot(sel)
    if {$item == ""} {
	$slot(window).cal configure -text ""
	$slot(window).rep configure -text ""
    } else {
	set disp [ical_title [$item calendar]]

	if {[$item hilite] == "holiday"} {
	    set disp [format {%s Holiday} $disp]
	}

	set owner [$item owner]
	if {$owner != ""} {
	    set disp [format {%s [Owner %s]} $disp $owner]
	}

	set type ""
	if [string compare [$item type] ""] {
	    set type [$item describe_repeat]
	    if {[string length $type] > 30} {
		set type "[string range $type 0 26]..."
	    }
	}

	$slot(window).cal configure -text $disp
	$slot(window).rep configure -text $type
    }
}

# Rebind keys
method DayView rebind {} {
    # Update menu accelerator keys
    global keymap
    set klist [concat $keymap(command) $keymap(item)]

    foreach m [winfo children $slot(window).menu] {
	set last [$m.m index last]
	for {set i 0} {$i <= $last} {incr i} {
	    catch {
		set act [lindex [$m.m entrycget $i -command] 0]
		$m.m entryconfig $i -acc "  [key_find_command $act $klist]"
	    }
	}
    }
}

# Build the menu
method DayView build_menu {} {
    set b $slot(window).menu

    menu-entry	$b File	Save			{ical_save}
    menu-entry	$b File	Re-Read			{ical_reread}
    menu-entry	$b File	Print			{ical_print}
    menu-sep	$b File
    menu-entry	$b File	{Include Calendar}	{ical_addinclude}
    menu-pull	$b File	{Remove Include}	{ical_fill_reminc}
    menu-sep	$b File
    menu-entry	$b File	{New Window}		{ical_newview}
    menu-entry	$b File	{Close Window}		{ical_close}
    menu-sep	$b File
    menu-entry	$b File	Exit			{ical_exit}

    menu-entry	$b Edit	Cut			{ical_cut_or_hide}
    menu-entry	$b Edit	Copy			{ical_copy}
    menu-entry	$b Edit	Paste			{ical_paste}
    #menu-sep	$b Edit
    #menu-entry	$b Edit	{Delete Text}		{ical_delete_selection}
    #menu-entry	$b Edit	{Insert Text}		{ical_insert_selection}
    menu-sep	$b Edit
    menu-entry	$b Edit	{Import Text}		{ical_import}
    menu-sep	$b Edit
    menu-entry	$b Edit	{Search Forward}	{ical_search_forward}
    menu-entry	$b Edit	{Search Backard}	{ical_search_backward}

    menu-bool	$b Item	Todo			{ical_toggle_todo}\
	dv_state(state:todo)
    menu-sep	$b Item
    $self fill_hilite $b Item
    #menu-sep	$b Item
    #menu-entry  $b Item {Link to Web Document}	{ical_link_to_uri}
    #menu-entry  $b Item {Link to File}		{ical_link_to_file}
    #menu-entry  $b Item {Remove Link}		{ical_remove_link}
    menu-sep	$b Item
    menu-entry	$b Item	{Change Alarms...}	{ical_alarms}
    menu-entry	$b Item	{Early Warning...}	{ical_set_remind}
    #menu-pull	$b Item	{Move Item To}		{ical_fill_move}
    menu-sep	$b Item
    menu-entry	$b Item	{Properties...}		{ical_edit_item}

    menu-entry	$b Repeat {Don't Repeat}	{ical_norepeat}
    menu-sep	$b Repeat
    menu-entry	$b Repeat {Daily}		{ical_daily}
    menu-entry	$b Repeat {Weekly}		{ical_weekly}
    menu-entry  $b Repeat {Monthly}		{ical_monthly}
    menu-entry  $b Repeat {Annually}		{ical_annual}
    menu-sep	$b Repeat
    menu-entry	$b Repeat {Edit Weekly...}	{ical_edit_weekly}
    menu-entry  $b Repeat {Edit Monthly...}	{ical_edit_monthly}
    menu-entry	$b Repeat {Set Range...}	{ical_set_range}
    menu-sep	$b Repeat
    menu-entry	$b Repeat {Last Occurrence}	{ical_last_date}
    menu-entry	$b Repeat {Make Unique}		{ical_makeunique}

    menu-entry	$b List	{One Day}		{ical_list 1}
    menu-entry	$b List	{Seven Days}		{ical_list 7}
    menu-entry	$b List	{Ten Days}		{ical_list 10}
    menu-entry	$b List	{Thirty Days}		{ical_list 30}
    menu-sep	$b List
    menu-entry	$b List	{Week}			{ical_list week}
    menu-entry	$b List	{Month}			{ical_list month}
    menu-entry	$b List	{Year}			{ical_list year}
    menu-sep	$b List
    menu-pull	$b List	{From Calendar}		{ical_fill_listinc}

    menu-entry	$b Options {Appointment Range}	  {ical_timerange}
    menu-entry	$b Options {Notice Window Height} {ical_noticeheight}
    menu-entry	$b Options {Item Width}		  {ical_itemwidth}
    menu-sep	$b Options
    menu-bool	$b Options {Allow Text Overflow}  {ical_toggle_overflow}\
	dv_state(state:overflow)
    menu-bool	$b Options {Display Am/Pm}	  {ical_toggle_ampm}\
	dv_state(state:ampm)
    menu-bool	$b Options {Start Week On Monday} {ical_toggle_monday}\
	dv_state(state:mondayfirst)
    menu-sep	$b Options
    menu-entry	$b Options {Default Alarms...}	  {ical_defalarms}
    menu-entry	$b Options {Default Listings...}  {ical_deflistings}

    # XXX Disable this now because I am not sure about the interface
    #menu-sep	$b Options
    #menu-entry	$b Options {Define a Command Key} {ical_cmdkey}

    menu-entry	$b Help	{About Ical}		  {ical_about}
    menu-entry	$b Help {User Guide}		  {ical_help}
}

#############################################################################
# Commands to fill cascading menus

# effects - Fill menu with calendar names.
#	    Invoke "<action> <calendar file>" when
#	    menu entry is selected.

proc ical_fill_includes {menu action} {
    set list {}
    cal forincludes file {
	lappend list $file
    }

    # Add menu separator whenever directory changes.
    set last_dir {}
    foreach f [lsort $list] {
	set d [file dirname $f]
	if [string compare $last_dir $d] {
	    if [string compare $last_dir {}] {$menu add separator}
	    set last_dir $d
	}
	$menu add command -label [ical_title $f]\
	    -command [list $action $f]
    }
}

# effects - Fill remove-include menu
proc ical_fill_reminc {menu} {
    $menu delete 0 last
    ical_fill_includes $menu ical_removeinc
}

# effects - Fill move-to-include menu
proc ical_fill_move {menu} {
    $menu delete 0 last
    $menu add command -label {Main Calendar}\
	-command [list ical_moveitem [cal main]]
    $menu add separator
    ical_fill_includes $menu ical_moveitem
}

# effects - Fill list-include menu
proc ical_fill_listinc {menu} {
    $menu delete 0 last
    $menu add command -label {Main Calendar}\
	-command [list ical_viewitems [cal main]]
    $menu add separator
    ical_fill_includes $menu ical_viewitems
}

# effects Fill hilite menu entries
method DayView fill_hilite {b m} {
    set entries {
	{ {Always Highlight}	{always}	}
	{ {Never Highlight}	{never}		}
	{ {Highlight Future}	{expire}	}
	{ {Holiday}		{holiday}	}
    }

    foreach e $entries {
	menu-oneof $b $m\
	    [lindex $e 0]\
	    [list ical_hilite [lindex $e 1]]\
	    dv_state(state:hilite)\
	    [lindex $e 1]
    }
}

#### Special code to set enablers for cascade menus ####
global ical_action_enabler
set ical_action_enabler(ical_fill_reminc)	writable
set ical_action_enabler(ical_fill_move)		witem
set ical_action_enabler(ical_fill_listinc)	always
