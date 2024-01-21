#
# A File Selector Box for multiple files in Tk
#
# Frank Pilhofer Feb/Mar 1996
# $Id: tk_MFSbox.tcl,v 1.1.1.1 1996/06/06 19:41:11 fp Exp $
#

#
# Default Path and Pattern
#

set MFSLoadFilterPath [pwd]
set MFSLoadFilterPattern "*"

#
# directory separator
#

if { $tcl_platform(platform) == "windows" } {
    set ds "\\"
    set parent ".."
} elseif { $tcl_platform(platform) == "macintosh" } {
    set ds ":"
    set parent ":"
} else {
    set ds "/"
    set parent ".."
}

##############################################################################
## General helper functions
##############################################################################

#
# Canonicalize a path: compress multiple slashes, remove slash at end,
#                      expand double-dots and perform tilde expansion
#

proc CompressSlashes { Path } {
    global ds parent

    set thepath [ file split $Path ]
    set lastel  [ expr [ llength $thepath ] - 1 ]
    set newpat  {}
    set ignore  0

    set element "."
    for { set index $lastel } { $index >= 0 } { incr index -1 } {
	set element [ lindex $thepath $index ]

	if { $element == {} } {
	} elseif { $element == "." } {
	} elseif { $element == $parent } {
	    incr ignore
	} elseif { $index == 0 && [ string range $element 0 0 ] == "~" } {
	    set hopath [ file split [ glob -nocomplain $element ] ]

	    if { $hopath == {} } {
		tk_dialog ._Dialog { User does not exist } "This user does\
			not exist." {} 0 OK
	    } elseif { $ignore } {
		if { $ignore > [ llength $hopath ] } {
		    set newpat [ linsert $newpat 0 {} ]
		} else {
		    set holen [ llength $hopath ]
		    set newpat [ concat [ lrange $hopath 0 \
			    [ expr $holen - $ignore - 1 ] ] \
			    $newpat ]
		}
	    } else {
		set newpat [ concat $hopath $newpat ]
	    }
	} elseif { $ignore } {
	    incr ignore -1
	} else {
	    set newpat [ linsert $newpat 0 $element ]
	}
    }
    if { $element == {} } {
	set newpat [ linsert $newpat 0 {} ]
    } elseif { $element == $ds } {
    } elseif { $element == "." || $element == $parent } {
	if { $ignore } {
	    set curdir [ file split [ pwd ] ]
	    if { $ignore > [ llength $curdir ] } {
		set newpat [ linsert $newpat 0 {} ]
	    } else {
		set cdlen [ llength $curdir ]
		set newpat [ concat [ lrange $curdir 0 \
			[ expr $cdlen - $ignore - 1 ] ] \
			$newpat ]
	    }
	} else {
	    set newpat [ linsert $newpat 0 "." ]
	}
    } else {
	set newpat [ linsert $newpat 0 "." ]
    }
    set ThePath [ eval file join $newpat ]

#    if { $ThePath == {} } {
#	set ThePath [ file join "" ]
#    }

    return $ThePath
}

#
# Canonize our search pattern
#

proc CanonPattern { Pattern } {
    global MFSLoadFilterPath
    global MFSLoadFilterPattern
    global ds parent

    set ThePath [ CompressSlashes $Pattern ]
    set TheDir  [ file dirname $ThePath ]
    set TheFile [ file tail $ThePath ]

    # split up by directory and pattern

    if { $TheDir == {} } {
	set MFSLoadFilterPath "."
	set MFSLoadFilterPattern "*"
    } elseif { [ file exists $ThePath ] && [ file isdirectory $ThePath ] } {
	set MFSLoadFilterPath $ThePath
	if { $MFSLoadFilterPattern == {} } {
	    set MFSLoadFilterPattern "*"
	}
    } else {
	set MFSLoadFilterPath $TheDir
	set MFSLoadFilterPattern $TheFile
    }
    set MFSCurPattern [ file join $MFSLoadFilterPath $MFSLoadFilterPattern ]

    return $MFSCurPattern
}

#
# Add new value to a listbox if it's not already there
#

proc AddToLb { lb value } {
    set count [ $lb size ]
    for { set index 0 ; set found 0 } { $index < $count } { incr index } {
	if { [ $lb get $index ] == $value } {
	    set found 1
	    break
	}
    }
    if { $found == 0 } {
	$lb insert end $value
    }
}

#
# Update elements in Listboxes after directory or filter change
#

proc MFSSelectShow { havefiles } {
    global MFSLoadFilterPath
    global MFSLoadFilterPattern
    global ds parent

    if { $havefiles } {
	global MFSlbf
    }
    global MFSlbd

    if { $havefiles } {
	$MFSlbf delete 0 end
    }
    $MFSlbd delete 0 end

    if { ! [ file readable $MFSLoadFilterPath ] } {
	tk_dialog ._Dialog { File Error } "You do not have the proper\
		permissions to browse this Directory: $MFSLoadFilterPath" \
		{} 0 OK
	if { [ file pathtype $MFSLoadFilterPath ] == "absolute" } {
	    if { [ llength [ file split $MFSLoadFilterPath ] ] > 1 } {
		$MFSlbd insert 0 $parent$ds
	    }
	}
	return
    }
    #
    # insert files into file list
    #
    if { $havefiles } {
	set pat [ file join $MFSLoadFilterPath $MFSLoadFilterPattern ]
	foreach file [ lsort [ glob -nocomplain -- $pat ] ] {
	    set basename [ file tail $file ]
	
	    if { ! [ file isdirectory $file ] } {
		$MFSlbf insert end $basename
	    }
	}
    }
    #
    # insert directories into directory list
    #
    set pat [ file join $MFSLoadFilterPath * ]
    foreach file [ lsort [ glob -nocomplain -- $pat ] ] {
	set basename [ file tail $file ]
	
	if { [ file isdirectory $file ] } {
	    append basename /
	    $MFSlbd insert end $basename
	}
    }
    if { [ file pathtype $MFSLoadFilterPath ] == "absolute" } {
	if { [ llength [ file split $MFSLoadFilterPath ] ] > 1 } {
	    $MFSlbd insert 0 $parent$ds
	}
    }
    $MFSlbd insert 0 "./"
}

proc SelectAddFromList { lb } {
    global MFSLoadFilterPath
    global MFSLoadFilterPattern
    global ds parent
    global MFSlbi
    
    set Selection [ $lb curselection ]
    set SelItems  [ llength $Selection ]
    
    for { set index 0 } { $index < $SelItems } { incr index } {
	set SelFile [ $lb get [ lindex $Selection $index ] ]
	set TheFile [ file join $MFSLoadFilterPath $SelFile ]
	
	if { ! [ file readable $TheFile ] } {
	    tk_dialog ._Dialog { File Error } "You do not have the proper\
		    permissions to read this file or directory: $TheFile" \
		    {} 0 OK
	} else {
	    set TheFile [ CompressSlashes $TheFile ]
	    if { [ file isdirectory $TheFile ] && $TheFile != $ds } {
		append TheFile $ds
	    }
	    AddToLb $MFSlbi $TheFile
	}
    }
    return 0
}

##############################################################################
# Main function
##############################################################################
#
# title:      Title of this dialog box. Printed in the title bar
# multifile:  whether we allow to select multiple files
# allowdir:   Whether we allow directories to be selected:
#             0: no directories may be added to the selection
#             1: directories may be selected
#             2: only directories may be selected
# startpath:  the path where we want the selection to start
#

proc tk_SelectFiles { title multifile allowdir { startpath "" } } {
    global MFSLoadFilterPath
    global MFSLoadFilterPattern
    global MFSSelectFinish
    global MFSCurPattern
    global MFSCurSelection
    global MFSlbf
    global MFSlbd
    global MFSlbi
    global ds parent

    set MFSLoadFilterPath [ CompressSlashes $MFSLoadFilterPath ]
    set OldFilterPath $MFSLoadFilterPath
    set OldFilterPattern $MFSLoadFilterPattern
    if { $startpath != "" } {
	set MFSLoadFilterPath [ CompressSlashes $startpath ]
    }
    set MFSCurPattern [ file join $MFSLoadFilterPath $MFSLoadFilterPattern ]
    set MFSSelectFinish {}
    set MFSCurSelection ""

    set MFSlbf ._Selector._Top._FileList.sub.box
    set MFSlbd ._Selector._Top._DirList.sub.box
    set MFSlbi ._Selector._SelList._FileList._List

    toplevel ._Selector
    wm title ._Selector $title
    wm minsize ._Selector 300 200

    frame ._Selector._Filter -relief raised -bd 1
    label ._Selector._Filter._Label -text "Filter:"
    entry ._Selector._Filter._Filter -relief sunken -textvariable MFSCurPattern
    pack ._Selector._Filter._Label -side left -padx 4 -pady 4
    pack ._Selector._Filter._Filter -side right -padx 8 -pady 4 \
	    -fill x -expand true
    pack ._Selector._Filter -side top -fill x

    frame ._Selector._Top -relief groove -bd 1
    frame ._Selector._Top._DirList
    frame ._Selector._Top._DirList.sub

    label ._Selector._Top._DirList.label -text "Directories"
    listbox ._Selector._Top._DirList.sub.box -relief sunken \
	    -xscrollcommand "._Selector._Top._DirList.sub.xsb set" \
	    -yscrollcommand "._Selector._Top._DirList.sub.ysb set" \
	    -selectmode normal -height 8
    scrollbar ._Selector._Top._DirList.sub.xsb -orient horizontal \
	    -command "._Selector._Top._DirList.sub.box xview"
    scrollbar ._Selector._Top._DirList.sub.ysb \
	    -command "._Selector._Top._DirList.sub.box yview"

    pack ._Selector._Top._DirList.sub.xsb -side bottom -fill x
    pack ._Selector._Top._DirList.sub.ysb -side right  -fill y
    pack ._Selector._Top._DirList.sub.box -side left -expand true -fill both

    pack ._Selector._Top._DirList.label -side top -anchor w -padx 4
    pack ._Selector._Top._DirList.sub -side bottom -expand true -fill both


    if { $allowdir != 2 } {
	frame ._Selector._Top._FileList
	frame ._Selector._Top._FileList.sub

	label ._Selector._Top._FileList.label -text "Files"

	if { $multifile } {
	    listbox ._Selector._Top._FileList.sub.box -relief sunken \
		    -xscrollcommand "._Selector._Top._FileList.sub.xsb set" \
		    -yscrollcommand "._Selector._Top._FileList.sub.ysb set" \
		    -selectmode extended -height 8
	} else {
	    listbox ._Selector._Top._FileList.sub.box -relief sunken \
		    -xscrollcommand "._Selector._Top._FileList.sub.xsb set" \
		    -yscrollcommand "._Selector._Top._FileList.sub.ysb set" \
		    -selectmode normal -height 8
	}
	scrollbar ._Selector._Top._FileList.sub.xsb -orient horizontal \
		-command "._Selector._Top._FileList.sub.box xview"
	scrollbar ._Selector._Top._FileList.sub.ysb \
		-command "._Selector._Top._FileList.sub.box yview"

	pack ._Selector._Top._FileList.sub.xsb -side bottom -fill x
	pack ._Selector._Top._FileList.sub.ysb -side right  -fill y
	pack ._Selector._Top._FileList.sub.box -side left \
		-expand true -fill both

	pack ._Selector._Top._FileList.label -side top -anchor w -padx 4
	pack ._Selector._Top._FileList.sub -side bottom \
		-expand true -fill both
    }

    if { $multifile } {
	frame ._Selector._Top._Buttons
	frame ._Selector._Top._Buttons.b

	if { $allowdir == 2 } {
	    button ._Selector._Top._Buttons.b._Add -text "Add" -width 8 \
		    -command { SelectAddFromList $MFSlbd }
	} else {
	    button ._Selector._Top._Buttons.b._Add -text "Add" -width 8 \
		    -command { SelectAddFromList $MFSlbf }

	    if { $allowdir == 1 } {
		button ._Selector._Top._Buttons.b._AddPath -text "Add Path" \
			-width 8 -command { SelectAddFromList $MFSlbd }
	    }
	}
	if { $allowdir != 2 } {
	    button ._Selector._Top._Buttons.b._AddAll -text "Add All" \
		    -width 8 -command {
		set ThePath $MFSLoadFilterPath
		set count [ $MFSlbf size ]
		if { $ThePath != $ds } { append ThePath $ds }
		for { set index 0 } { $index < $count} { incr index } {
		    set TheFile $ThePath
		    append TheFile [ $MFSlbf get $index ]
		    AddToLb $MFSlbi $TheFile
		}
	    }
	}

	if { $allowdir == 1 } {
	    pack ._Selector._Top._Buttons.b._Add \
		    ._Selector._Top._Buttons.b._AddPath \
		    ._Selector._Top._Buttons.b._AddAll \
		    -side left -ipadx 4 -ipady 4 -fill x -padx 2 -pady 2
	} elseif { $allowdir == 2 } {
	    pack ._Selector._Top._Buttons.b._Add \
		    -side left -ipadx 4 -ipady 4 -fill x -padx 2 -pady 2
	} else {
	    pack ._Selector._Top._Buttons.b._Add \
		    ._Selector._Top._Buttons.b._AddAll \
		    -side left -ipadx 4 -ipady 4 -fill x -padx 2 -pady 2
	}

	pack ._Selector._Top._Buttons.b
    }

    if { $multifile } {
	pack ._Selector._Top._Buttons -side bottom -fill x -padx 4 -pady 4
    }

    pack ._Selector._Top._DirList -side left -expand true -fill both -padx 4

    if { $allowdir != 2 } {
	pack ._Selector._Top._FileList -side right -expand true \
		-fill both -padx 4
    }

    if { $multifile } {
	frame ._Selector._SelList -relief groove -bd 1
	frame ._Selector._SelList._FileList
	frame ._Selector._SelList._Buttons -bd 4

	label ._Selector._SelList._Label -text "Selected Files" -relief groove

	listbox ._Selector._SelList._FileList._List -relief sunken \
		-xscrollcommand "._Selector._SelList._FileList._XScrollbar set" \
		-yscrollcommand "._Selector._SelList._FileList._YScrollbar set" \
		-selectmode extended -height 4
	scrollbar ._Selector._SelList._FileList._XScrollbar -orient horizontal \
		-command "._Selector._SelList._FileList._List xview"
	scrollbar ._Selector._SelList._FileList._YScrollbar \
		-command "._Selector._SelList._FileList._List yview"

	pack ._Selector._SelList._FileList._XScrollbar -side bottom -fill x
	pack ._Selector._SelList._FileList._YScrollbar -side right  -fill y
	pack ._Selector._SelList._FileList._List -side left -expand true \
		-fill both

	button ._Selector._SelList._Buttons._Remove -text "Remove" -width 8 \
		-command {
	    set Selection [ $MFSlbi curselection ]
	    set count 0
	    foreach index $Selection {
		$MFSlbi delete [ expr $index - $count ]
		incr count
	    }
	}

	pack ._Selector._SelList._Buttons._Remove -ipadx 4 -ipady 4 -fill x \
		-padx 2 -pady 2 -anchor center

	pack ._Selector._SelList._Label -side top -fill x -padx 4 -pady 4
	pack ._Selector._SelList._FileList -side left -expand true -fill both \
		-padx 4 -pady 4
	pack ._Selector._SelList._Buttons -side right -padx 4 -pady 4
    } else {
	frame ._Selector._Selection -relief groove -bd 1
	label ._Selector._Selection.lab -text "Selection"
	entry ._Selector._Selection.ent -relief sunken \
		-textvariable MFSCurSelection
	pack ._Selector._Selection.ent -side bottom -padx 4 -fill x
	pack ._Selector._Selection.lab -side bottom -anchor w -padx 4
    }

    frame ._Selector._Buttons -relief raised -bd 1
    frame ._Selector._Buttons.b

    if { $multifile } {
	button ._Selector._Buttons.b._Ok -text "Ok" -width 8 -command { 
	    set MFSSelectFinish ok 
	}
    } else {
	if { $allowdir == 0 } {
	    button ._Selector._Buttons.b._Ok -text "Ok" -width 8 -command {
		if { ! [ file exists $MFSCurSelection ] || \
			! [ file readable $MFSCurSelection ] || \
			[ file isdirectory $MFSCurSelection ] } {
		    tk_dialog ._Dialog { Invalid Choice } "$MFSCurSelection\
			    does not exist, is not readable or is a\
			    directory" {} 0 OK
		} else {
		    set MFSSelectFinish ok
		}
	    }
	} elseif { $allowdir == 2 } {
	    button ._Selector._Buttons.b._Ok -text "Ok" -width 8 -command {
		if { ! [ file exists $MFSCurSelection ] || \
			! [ file readable $MFSCurSelection ] || \
			! [ file isdirectory $MFSCurSelection ] } {
		    tk_dialog ._Dialog { Invalid Choice } "$MFSCurSelection\
			    does not exist, is not readable or is not a\
			    directory" {} 0 OK
		} else {
		    set MFSSelectFinish ok
		}
	    }
	} else {
	    button ._Selector._Buttons.b._Ok -text "Ok" -width 8 -command {
		if { ! [ file exists $MFSCurSelection ] || \
			! [ file readable $MFSCurSelection ] } {
		    tk_dialog ._Dialog { Invalid Choice } "$MFSCurSelection\
			    does not exist or is not readable" {} 0 OK
		} else {
		    set MFSSelectFinish ok
		}
	    }
	}
    }
    if { $allowdir != 2 } {
	button ._Selector._Buttons.b._Filter -text "Filter" -width 8 -command {
	    set MFSCurPattern [ CanonPattern $MFSCurPattern ]
	    MFSSelectShow 1
	}
    } else {
	button ._Selector._Buttons.b._Filter -text "Filter" -width 8 -command {
	    set MFSCurPattern [ CanonPattern $MFSCurPattern ]
	    MFSSelectShow 0
	}
    }
    button ._Selector._Buttons.b._Cancel -text "Cancel" -width 8 \
	    -command { set MFSSelectFinish cancel }
    
    pack ._Selector._Buttons.b._Ok \
	    ._Selector._Buttons.b._Filter \
	    ._Selector._Buttons.b._Cancel \
	    -side left -ipadx 4 -ipady 4 -padx 4 -pady 4
    pack ._Selector._Buttons.b

    pack ._Selector._Top -side top -expand true -fill both -ipadx 8 -ipady 8

    if { $multifile } {
	pack ._Selector._SelList -side top -expand true -fill both \
		-ipadx 8 -ipady 8
    } else {
	pack ._Selector._Selection -side top -fill both -ipadx 8 -ipady 4
    }
    pack ._Selector._Buttons -side bottom -fill x

    #
    # the items are up on screen. define bindings
    # 

    if { $allowdir != 2 } {
	bind ._Selector._Filter._Filter <Return> {
	    set MFSCurPattern [ CanonPattern $MFSCurPattern ]
	    MFSSelectShow 1
	}
    } else {
	bind ._Selector._Filter._Filter <Return> {
	    set MFSCurPattern [ CanonPattern $MFSCurPattern ]
	    MFSSelectShow 0
	}
    }
    if { $multifile } {
	if { $allowdir != 2 } {
	    bind $MFSlbd <Double-Button-1> {
		set Selection [ lindex [ $MFSlbd curselection ] 0 ]

		if { $Selection != "" } {
		    set TheFile [ $MFSlbd get $Selection ]
		    set MFSLoadFilterPath [ CompressSlashes \
			    [ file join $MFSLoadFilterPath $TheFile ] ]
		    set MFSCurPattern [ \
			    file join $MFSLoadFilterPath $MFSLoadFilterPattern]
		    MFSSelectShow 1
		}
	    }
	} else {
	    bind $MFSlbd <Double-Button-1> {
		set Selection [ lindex [ $MFSlbd curselection ] 0 ]

		if { $Selection != "" } {
		    set TheFile [ $MFSlbd get $Selection ]
		    set MFSLoadFilterPath [ CompressSlashes \
			    [ file join $MFSLoadFilterPath $TheFile ] ]
		    set MFSCurPattern [ \
			    file join $MFSLoadFilterPath $MFSLoadFilterPattern]
		    MFSSelectShow 0
		}
	    }
	}
	if { $allowdir != 2 } {
	    bind $MFSlbf <Double-Button-1> {
		SelectAddFromList $MFSlbf
	    }
	}
    } else {
	if { $allowdir != 2 } {
	    bind $MFSlbd <Double-Button-1> {
		set Selection [ lindex [ $MFSlbd curselection ] 0 ]

		if { $Selection != "" } {
		    set TheFile [ $MFSlbd get $Selection ]
		    set MFSLoadFilterPath [ CompressSlashes \
			    [ file join $MFSLoadFilterPath $TheFile ] ]
		    set MFSCurPattern [ \
			    file join $MFSLoadFilterPath $MFSLoadFilterPattern]
		    MFSSelectShow 1
		}
	    }
	} else {
	    bind $MFSlbd <Double-Button-1> {
		set Selection [ lindex [ $MFSlbd curselection ] 0 ]

		if { $Selection != "" } {
		    set TheFile [ $MFSlbd get $Selection ]
		    set MFSLoadFilterPath [ CompressSlashes \
			    [ file join $MFSLoadFilterPath $TheFile ] ]
		    set MFSCurPattern [ \
			    file join $MFSLoadFilterPath $MFSLoadFilterPattern]
		    MFSSelectShow 0
		}
	    }
	}
    }
    if { ! $multifile } {
	if { $allowdir } {
	    bind $MFSlbd <Button-1> {
		set Selection [ $MFSlbd index  @%x,%y ]
		set TheFile   [ $MFSlbd get $Selection ]

		if { $TheFile != "" } {
		    set MFSCurSelection [ \
			    file join $MFSLoadFilterPath $TheFile ]$ds
		}
	    }
	}
	if { $allowdir != 2 } {
	    bind $MFSlbf <Button-1> {
		set Selection [ $MFSlbf index  @%x,%y ]
		set TheFile   [ $MFSlbf get $Selection ]

		if { $TheFile != "" } {
		    set MFSCurSelection [ \
			    file join $MFSLoadFilterPath $TheFile ]
		}
	    }
	    bind $MFSlbf <Double-Button-1> {
		set Selection [ lindex [ $MFSlbf curselection ] 0 ]

		if { $Selection != "" } {
		    set TheFile [ $MFSlbf get $Selection ]

		    set MFSCurSelection [ \
			    file join $MFSLoadFilterPath $TheFile ]
		    set MFSSelectFinish ok
		}
	    }
	}
	if { $allowdir == 0 } {
	    bind ._Selector._Selection.ent <Return> {
		set MFSCurSelection [ CompressSlashes $MFSCurSelection ]
		if { ! [ file exists $MFSCurSelection ] || \
			! [ file readable $MFSCurSelection ] || \
			[ file isdirectory $MFSCurSelection ] } {
		    tk_dialog ._Dialog { Invalid Choice } "$MFSCurSelection\
			    does not exist, is not readable or is a\
			    directory" {} 0 OK
		} else {
		    set MFSSelectFinish ok
		}
	    }
	} elseif { $allowdir == 2 } {
	    bind ._Selector._Selection.ent <Return> {
		set MFSCurSelection [ CompressSlashes $MFSCurSelection ]
		if { ! [ file exists $MFSCurSelection ] || \
			! [ file readable $MFSCurSelection ] || \
			! [ file isdirectory $MFSCurSelection ] } {
		    tk_dialog ._Dialog { Invalid Choice } "$MFSCurSelection\
			    does not exist, is not readable or is not a\
			    directory" {} 0 OK
		} else {
		    set MFSSelectFinish ok
		}
	    }
	} else {
	    bind ._Selector._Selection.ent <Return> {
		set MFSCurSelection [ CompressSlashes $MFSCurSelection ]
		if { ! [ file exists $MFSCurSelection ] || \
			! [ file readable $MFSCurSelection ] } {
		    tk_dialog ._Dialog { Invalid Choice } "$MFSCurSelection\
			    does not exist or is not readable" {} 0 OK
		} else {
		    set MFSSelectFinish ok
		}
	    }
	}
    }

    if { $allowdir != 2 } {
	MFSSelectShow 1
    } else {
	MFSSelectShow 0
    }

    set oldFocus [ focus ]
    tkwait visibility ._Selector
    grab set ._Selector
    focus ._Selector
    tkwait variable MFSSelectFinish

    set FileList {}

    if { $MFSSelectFinish == "cancel" } {
	set MFSLoadFilterPath $OldFilterPath
	set MFSLoadFilterPattern $OldFilterPattern
    } else {
	if { $multifile } {
	    set count [ ._Selector._SelList._FileList._List size ]

	    for { set index 0 } { $index < $count } { incr index } {
		lappend FileList \
			[ ._Selector._SelList._FileList._List get $index ]
	    }
	} else {
	    set FileList $MFSCurSelection
	}
    }

    destroy ._Selector
    focus $oldFocus
    return $FileList
}
