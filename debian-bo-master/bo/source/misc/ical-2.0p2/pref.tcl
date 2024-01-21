# Copyright (c) 1993 by Sanjay Ghemawat
###############################################################################
# User Preferences

# Autload support
proc pref {} {}
proc Preference-with-name {} {}

class Preference {} {
    $self reload
}

method Preference load_common {} {
    option add *fontFamily		times		startupFile
    option add *Font			fixed		startupFile
    option add *fontSize		normal		startupFile

    option add *saveSeconds		30		startupFile
    option add *pollSeconds		120		startupFile

    option add *itemPad			2		startupFile
    option add *Reminder.geometry	+400+0		startupFile
    option add *Listing.geometry	+400+0		startupFile

    option add *Dayview*takeFocus	0		startupFile

    option add *ApptList.Canvas.BorderWidth		1	startupFile
    option add *ApptList.Canvas.Relief			raised	startupFile
    option add *ApptList.Canvas.highlightThickness	0	startupFile

    option add *NoteList.Canvas.BorderWidth		1	startupFile
    option add *NoteList.Canvas.Relief			raised	startupFile
    option add *NoteList.Canvas.highlightThickness	0	startupFile
}

method Preference load_color {} {
    set fg	[$self findcolor black]
    set bg	[$self findcolor gray80 white]
    set wday	[$self findcolor black $fg]
    set wend	[$self findcolor red $fg]
    set int	[$self findcolor blue $fg]
    set wendint	[$self findcolor purple $fg]
    set line	$fg
    set ifg	$fg
    set ibg	[$self findcolor gray75 $bg white]
    set isfg	$fg
    set isbg	[$self findcolor LightSkyBlue $bg white]
    set over	[$self findcolor DeepSkyBlue $bg white]
    set csbg	[$self findcolor khaki $fg]

    if [catch {set disabled [$self findcolor gray60]}] {
	set disabled ""
    }

    option add *weekdayColor		$wday		startupFile
    option add *weekendColor		$wend		startupFile
    option add *interestColor		$int		startupFile
    option add *weekendInterestColor	$wendint	startupFile
    option add *apptLineColor		$line		startupFile

    option add *itemFg			$ifg		startupFile
    option add *itemBg			$ibg		startupFile
    option add *itemSelectFg		$isfg		startupFile
    option add *itemSelectBg		$isbg		startupFile
    option add *itemSelectWidth		0		startupFile
    option add *itemOverflowColor	$over		startupFile
    option add *itemOverflowStipple	{}		startupFile
    option add *Canvas*selectBackground	$csbg		startupFile
}

method Preference load_mono {} {
    option add *Foreground		black		startupFile
    option add *Background		white		startupFile

    option add *weekdayColor		black		startupFile
    option add *weekendColor		black		startupFile
    option add *interestColor		black		startupFile
    option add *weekendInterestColor	black		startupFile
    option add *apptLineColor		black		startupFile

    option add *itemFg			black		startupFile
    option add *itemBg			white		startupFile
    option add *itemSelectFg		black		startupFile
    option add *itemSelectBg		white		startupFile
    option add *itemSelectWidth		4		startupFile
    option add *itemOverflowColor	black		startupFile
    option add *itemOverflowStipple	gray50		startupFile
}

method Preference reload {} {
    option clear
    tcllib_load_options
    $self load_common

    # Load default options
    global ical
    $self load_common
    if [string match *color* [winfo screenvisual .]] {
	$self load_color
    } else {
	$self load_mono
    }

    $self fixfonts

    # Cache various entries
    set slot(itemPad)		[winfo pixels . [option get . itemPad Size]]

    # Use command-line geometry specification (if any)
    global geometry
    if ![catch {set geometry}] {
	# Specified on command line
	option add Ical.Dayview.geometry $geometry
    }

    # XXX People do not seem to like the motif-style popup behavior
    global tk_strictMotif
    if {!$tk_strictMotif} {
	bind Menubutton <Any-ButtonRelease-1> {tkMenuUnpost {}}
    }

    # Handle command line preferences
    foreach pref $ical(prefs) {eval $pref}
}

# Fix fonts in option database
method Preference fixfonts {} {
    set slot(fontfamilies) [list\
			    [option get . fontFamily String]\
			    times\
			    charter\
			    {new century schoolbook}\
			    courier\
			    helvetica\
			   ]

    switch -exact -- [option get . fontSize String] {
	small {
	    set size1	120
	    set size2	120
	    set size3	140
	    set size4	180
	}
	default {
	    set size1	120
	    set size2	140
	    set size3	180
	    set size4	240
	}
    }

    # Find fonts
    set canvas [canvas .preffonts]
    set norm120		[$self findfont $canvas medium r $size1]
    set norm140		[$self findfont $canvas medium r $size2]
    set ital140		[$self findfont $canvas medium i $size2]
    set bold140		[$self findfont $canvas bold   r $size2]
    set blit140		[$self findfont $canvas bold   i $size2]
    set norm180		[$self findfont $canvas medium r $size3]
    set bold180		[$self findfont $canvas bold   r $size3]
    set norm240		[$self findfont $canvas medium r $size4]
    destroy $canvas

    # Set option database
    option add *weekdayFont		$norm140 startupFile
    option add *weekendFont		$ital140 startupFile
    option add *interestFont		$bold140 startupFile
    option add *weekendInterestFont	$blit140 startupFile
    option add *itemFont		$norm140 startupFile

    option add *normFont		$norm140 startupFile
    option add *boldFont		$bold140 startupFile
    option add *italFont		$ital140 startupFile

    option add *smallHeadingFont	$bold140 startupFile
    option add *largeHeadingFont	$bold180 startupFile

    # General preferences
    option add *Dialog*font		$norm180 startupFile
    option add *Dialog*Message*font	$norm140 startupFile
    option add *Button*font		$bold180 startupFile
    option add *Dayview*Button*font	$bold140 startupFile
    option add *Label*font		$norm180 startupFile
    option add *Menubutton*font		$norm140 startupFile
    option add *Menu*font		$norm140 startupFile
    option add *Listbox*font		$norm140 startupFile
    option add *Text*font		$norm140 startupFile
    option add *Scale*font		$norm140 startupFile

    option add *editkeys*Entry*font	$norm120 startupFile

    # Dialogs with lots of stuff
    option add *Bigdialog*font		$norm140 startupFile
    option add *Bigdialog*Button*font	$bold180 startupFile

    # Date editor preferences
    option add *Dayview*Dateeditor*Label*font	$norm240 startupFile
    option add *Dialog*Dateeditor*Label*font	$norm180 startupFile
    option add *Dialog*Dateeditor*Button*font	$norm180 startupFile
}

# effects - Find font matching given specification
method Preference findfont {canvas weight style size} {
    # Search for this size in families
    foreach family $slot(fontfamilies) {
	set f "-*-$family-$weight-$style-normal-*-*-$size-*"
	if ![catch {$canvas create text 0 0 -text XXX -font $f}] {
	    $canvas delete all
	    return $f
	}
    }

    # XXX - More searching?

    # Return default font
    return fixed
}

# effects - Find first legal color in named list and return it.
method Preference findcolor {args} {
    foreach c $args {
	if [color_exists $c] {return $c}
    }
    error "could not find any of the following colors: \"[join $args]\""
}

# To force autoload
method Preference init {} {
}

# Methods for obtaining various preferences.

method Preference weekdayFont {} {
    return [option get . weekdayFont Font]
}

method Preference weekendFont {} {
    return [option get . weekendFont Font]
}

method Preference interestFont {} {
    return [option get . interestFont Font]
}

method Preference weekendInterestFont {} {
    return [option get . weekendInterestFont Font]
}

method Preference itemFont {} {
    return [option get . itemFont Font]
}

method Preference normFont {} {return [option get . normFont Font]}
method Preference boldFont {} {return [option get . boldFont Font]}
method Preference italFont {} {return [option get . italFont Font]}

method Preference smallHeadingFont {} {
    return [option get . smallHeadingFont Font]
}

method Preference largeHeadingFont {} {
    return [option get . largeHeadingFont Font]
}

method Preference weekdayColor {} {
    return [option get . weekdayColor Foreground]
}

method Preference weekendColor {} {
    return [option get . weekendColor Foreground]
}

method Preference interestColor {} {
    return [option get . interestColor Foreground]
}

method Preference weekendInterestColor {} {
    return [option get . weekendInterestColor Foreground]
}

method Preference itemFg {} {
    return [option get . itemFg Foreground]
}

method Preference itemBg {} {
    return [option get . itemBg Background]
}

method Preference itemSelectFg {} {
    return [option get . itemSelectFg Background]
}

method Preference itemSelectBg {} {
    return [option get . itemSelectBg Foreground]
}

method Preference itemSelectWidth {} {
    return [option get . itemSelectWidth Size]
}

method Preference itemOverflowColor {} {
    return [option get . itemOverflowColor Foreground]
}

method Preference itemOverflowStipple {} {
    return [option get . itemOverflowStipple Bitmap]
}

method Preference apptLineColor {} {
    return [option get . apptLineColor Foreground]
}

method Preference itemPad {} {
    return $slot(itemPad)
}

method Preference pollSeconds {} {
    return [option get . pollSeconds Time]
}

method Preference saveSeconds {} {
    return [option get . saveSeconds Time]
}
