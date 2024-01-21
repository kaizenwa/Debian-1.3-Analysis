#!/usr/local/bin/wish
#
#	System: Sgtool, a frontend to sgrep.
#	Module: sgtool
#	Author: Pekka Kilpeläinen & Jani Jaakkola
#	Description: Implements a X interface to sgrep with tcl/tk
#	Version history: Original version July 1995 by JJ & PK
#	Copyright: University of Helsinki, Dept. of Computer Science
#		   Distributed under GNU General Public Lisence
#		   See file COPYING for details

# Here is some definitions you might like to check out

# The version number
set sgtversion 0.90

# Default output style is short
set opt_out "-s"
# Default preprocessor is m4 ( only one we can get macros from )
set preprocessor "m4"
set ostyle ""
set outstylefile ""

# Default preferences
set pref_input 1
set pref_macrofiles 1
set pref_macros 1
set pref_ver 1
set pref_status 1

# default filters
set filter_save *
set filter_input *
set filter_macro *.macros
set filter_ostyle *

# default directories
set input_dir .
set macro_dir .

# Macrofiles default to /usr/lib/sgreprc and ~/.sgreprc
catch { glob ~/.sgreprc } macrofiles
if { ! [ file isfile $macrofiles ] } { set macrofiles "/usr/local/lib/sgreprc" }
if { ! [ file isfile $macrofiles ] } { set macrofiles "" }

# The actual program starts here

set macros ""
set body_array(0) ""
set macro_file_array(0) ""

# How many macro editors is active ?
set macro_editors 0

# fetch the sgrep version string. Error means that we couldn't exec sgrep.
# in which case there is no point to continue.
if { [ catch { exec sgrep -V } sgrepver ] } { puts $sgrepver ; exit 1 }

# Check for command line arguments ( input files )
if { $argv == "" } {
	set input_files ""
} else {
	set input_files $argv
}

# Using this variable to give individual number to textwindows
set textwnum 0
# Using this var to give individual save file numbers
set sfilenum 0

set sgrep_define { "define" not in ( inner("#".."\n") or ("("..")") ) }
 
# Procedure which fetches macro names from given macrofiles using sgrep
# Macro names have a "\n" between them
# Returns macronames or error in variable var
# returs nonzero when error occurred
proc fetch_macro_names { files var } {
        global sgrep_define
	upvar $var macros

        if { $files == "" } {
	    set macros ""
	    return 0
	}

        set sgexpr { .. "(" __ "," }
	set sgexpr "$sgrep_define $sgexpr"
	if { [
	catch { eval exec sgrep -n -p - -o { "%r " } { $sgexpr } $files } macros
	     ] } {
		errwin .macroerr "Could not fetch macro names" $macros { }
		centerwin .macroerr .
		return 1
	}
	return 0
}

# Procedure which fetches macro bodies from given macrofiles using sgrep
# Macro bodies are stored in array starting with index 0
# Array name is given in variable var
# return nonzero when error occurred
proc fetch_macro_bodies { files body_var file_var } {
        global sgrep_define
	upvar $body_var bodies
        upvar $file_var macro_file_array
        
        if { $files == "" } {
	    array set bodies { }
	    return 0
	}
	# Open a pipe and try to catch errors
	set sgexpr { .. "(" .. "," _. ( "(" .. ")" ) }
        set sgexpr "$sgrep_define $sgexpr"
	if { [ catch { 
		open "|sgrep -n -p - -o %f\\n%r\\n!@£$%&/\\n \{$sgexpr\} $files" r
		} openerr ] } {
		# Error when creating pipe
		errwin .bodyerr "Couldn't create sgrep pipe" $openerr { }
		centerwin .bodyerr .
		return 1
	}
	# Read from pipe. Function bodies are separated with output style
	# !@£$%/ . Bodies are stored in array a
	set bodynum 0
	set body ""
	set f(0) ""
	while { [gets $openerr line] != -1 } {
		if { "$line" == "!@£$%&/" } {
			set a($bodynum) $body
			set body ""
			incr bodynum
		        set f($bodynum) ""
		} else {
		    if { $f($bodynum)=="" } {
			    set f($bodynum) $line
		    } else {
			set body "$body$line\n" 
		    }
		}
	}
	if { [catch { close $openerr } closeerr] } {
		errwin .bodyerr "Could not fetch macro bodies" $closeerr { }
		centerwin .bodyerr .
		return 1
	}
	# If bodies won't exists unset returns error, which is okay
	catch { unset bodies }
	array set bodies [array get a]
	catch { unser files }
	array set macro_file_array [array get f]
	return 0
}

# Procedure witch fetches macros. Returns 0 if fetch was ok
proc fetch_macros { } {
	global macrofiles
	global macros
	global body_array
	global mlist
        global macro_file_array
        global macro_editors

        # If there are macro editors around, we won't fetch macros
        if { $macro_editors > 0 } {
	    errwin .fetcherr "Macro fetching error" \
"Close all macro editors before scanning
for macros. Otherwise all changes would
be lost." { }
            return
        }

	.state configure -text "Fetching macros"
	update

	set m ""
	if { [fetch_macro_names $macrofiles m ] } {
		# Fetching macros was not ok 
		return 1
	}
	if { [fetch_macro_bodies $macrofiles body_array macro_file_array ] } {
		# Fetching macro bodies was not ok
		return 1
	} 
	set macros $m
	update_macro_list
}

# Updates macrolistbox macro list
proc update_macro_list { } {
    global mlist
    global macros

    $mlist delete 0 end
    foreach i [set macros] {
	$mlist insert end $i
    }

    .state configure -text "Ready" 
    return 0
}

# Procedure which returns a m4 macro file from given macro names
proc generate_macro_file { names } {
    global macros
    global body_array

    .state configure -text "Generating macrofile"
    update

    # r is the result variable
    set r \
"# sgrep macrofile for m4 preprocessor
# This file was automatically generated by sgreptool
"
    foreach i "$names" {
	set j [lsearch -exact "$macros" $i]
	set m $body_array($j)
	set r "$r\ndefine($i,$m)"
    }
    .state configure -text "Ready"
    return "$r\n"
}

# Procedure which is invoked instead of scrollbar set to enable or disable
# when they are needed or aren't needed
proc doset { sbar packopt first last } {
    if { $first=="0" && $last=="1" } {
	pack forget $sbar
    } else {
	eval "pack $sbar $packopt"
	$sbar set $first $last
    }
}

# Centers a window to given parent ( for popups and such )
proc centerwin { win dad } {
	set dx [ winfo rootx $dad ]
	set dy [ winfo rooty $dad ]
	set x [ expr 25+$dx ]
	set y [ expr 25+$dy ]
	wm geometry $win +$x+$y
}

# Creates given error window, with given label and error text.
# sets .status to error, and when error window is closed sets it to ready.
# Grabs input focus
proc errwin { errw label text dest_com } {
	# Create error window
	toplevel $errw
	# This is how this window gets killed
	set destroy_c ".state configure -text Ready; destroy $errw;
			focus [focus] ; $dest_com"
	# Information for window manages
	wm protocol $errw WM_DELETE_WINDOW $destroy_c 
	wm transient $errw [winfo toplevel [winfo parent $errw]]
	# How we look like
	button $errw.errokbutton -text "OK" -command $destroy_c
	pack $errw.errokbutton -side bottom
	.state configure -text "Error"
	label $errw.state -text $label
	pack $errw.state -side top -fill x
        pack [ label $errw.bitmap -bitmap error ] -side left -padx 10 -pady 10
	message $errw.msg \
        	-relief raised -width 500 \
        	-borderwidth 1 \
		-text $text
	pack $errw.msg -fill both -expand 1
	# Escape and enter also kills this window
	bind $errw <Escape> $destroy_c
	centerwin $errw [winfo toplevel [winfo parent $errw]]
	# This window must die before anything else can be done
	focus [winfo toplevel [winfo parent $errw]]
	focus $errw.errokbutton
	update
	grab $errw.errokbutton
	return 0
}

# Creates window with yes and no buttons executing script yes_com on yes
# button and no_com on no button. Establshes a grab, so that nothing will
# be done before query is answered
proc yesno { win text yes no } {
	set w $win
	# Create error window
	toplevel $w
	# This is how this window gets killed
	set destroy_c ".state configure -text Ready; destroy $w
		focus [focus]"
	# This window must be answered
	wm protocol $w WM_DELETE_WINDOW { } 
	wm transient $w [winfo toplevel [winfo parent $w]]
	# How we look like
	.state configure -text "Yes or No"
	pack [label $w.question -bitmap question ] -side left -padx 10
	message $w.msg -font -Adobe-times-medium-r-normal--*-180-*-*-*-*-*-* \
        	-relief raised -width 500 \
        	-borderwidth 1 \
		-text $text
	pack $w.msg -fill x
	# Create buttons
	frame $w.bf
	pack $w.bf -side bottom
	foreach i { yes no } {
		button $w.bf.$i -width 8 -text $i\
			-command "$destroy_c ; [set $i]"
		pack $w.bf.$i -side left -padx 5 -pady 5
	}
	# y and n are valid answers
	bind $w <y> "focus $w.bf.yes"
	bind $w <n> "focus $w.bf.no"
	bind $w <Return> { [focus] invoke }
	centerwin $w [winfo toplevel [winfo parent $w]]
	# This window must die before anything else can be done
	focus . 
	focus $w.bf.yes
	update
	grab $w
	return 0
}

# Checks if it is ok save file with given name. If it's ok, destroys $win,
# exec $savecommand giving filename as parameter. $file is name of global
# variable containing filename
proc oktosave { win savecommand file dir} {
	global $file
	global $dir

	set f [make_name [set $dir] [set $file] ]

	set c "
		destroy $win
		eval $savecommand \"$f\"
		unset $file
		unset $dir
	"
	#if we dont't have a file name, we do nothing
	if { [set $file] == ""} { return }

	# Make sure that it's okay to overwrite existing files
	if { [ file exists "$f" ] } {
		yesno $win.yesnowin "File $f exists. OK to overwrite ?" "$c" { }
	} else "$c"
}

# Window for asking filename for saving. Will create it into window win, and
# exec script savecommand when file is selected
proc savefile { win savecommand filtervar } {
	global sfilenum
	global filter_save

	incr sfilenum

	set file sfile$sfilenum
	set dir sdir$sfilenum
	set filter $filtervar

	global $file
	global $dir
	global $filter

	set $dir .
	set $file ""

	# Button frame
	frame $win.bf
	pack $win.bf -side bottom
	foreach i { save cancel } {
		button $win.bf.$i -text $i -width 8
		pack $win.bf.$i -side left -padx 5
	}
	# Command to execute when save button is pressed
	set c_save "oktosave $win \{ $savecommand \} $file $dir"
	$win.bf.save configure -command "$c_save"
	chooser $win $file $dir $filter "$c_save"

	# Command to execute when cancel is requested
	set c_cancel "destroy $win;unset $file; unset $dir"
	wm protocol $win WM_DELETE "unset $file;unset $dir"
	$win.bf.cancel configure -command $c_cancel
}

# Window for asking file name.
# exec script selectcommand when file is selected. 
# File must exist and be a regular file (that's checked)
proc selectfile { win selectcommand default_file default_filter } {
	set file selfile$win
	set dir seldir$win
	set filter $default_filter

	global $file
	global $dir
	global $filter

	set $dir [file dirname $default_file]
	if { $dir == "" } { set dir "." }
	set $file [file tail $default_file]

	# Button frame
	frame $win.bf
	pack $win.bf -side bottom
	foreach i { ok cancel } {
		button $win.bf.$i -text $i -width 8
		pack $win.bf.$i -side left -padx 5
	}
	# Command to execute when ok button is pressed
	proc file_exists { win selectcommand filevar dirvar } {
		global $filevar
		global $dirvar

		set f [ make_name [ set $dirvar ] [ set $filevar ]  ]
		if { [file isfile $f] } {
			destroy $win
			eval $selectcommand $f
		} else {
			errwin $win.errw "File selection error:" \
				"Selected file isn't regular file" { }
		}
	}
	set c_sel "file_exists $win \
		\{ unset $file; unset $dir; $selectcommand \} $file $dir"
	$win.bf.ok configure -command "$c_sel"
	
	chooser $win $file $dir $filter "$c_sel"

	# Command to execute when cancel is requested
	set c_cancel "destroy $win;unset $file; unset $dir"
	wm protocol $win WM_DELETE "unset $file;unset $dir"
	$win.bf.cancel configure -command $c_cancel
}

# Window for selecting outputstylefile
proc select_outfile { } {
	# If window exists do nothing
	if { [ winfo exists .outstylefilewin ] } { return }
	# Create window
	set w .outstylefilewin
	toplevel $w
	centerwin $w .
	wm title $w "sgreptool - Output style file"
	global outstylefile
	selectfile $w "global outstylefile; set outstylefile" $outstylefile filter_ostyle
	return
}

# Saves given textwindow to filename
proc textsave { twin filename } {
	# Status window tells what we are doing
	.state configure -text "Saving .."
	update
	# Open file, and catch errors
	if { [catch { open $filename w } f] } {
		errwin $twin.fileerror "Error opening file '$filename' for saving" $f {}
		return
	}
	if { [catch { puts -nonewline $f [$twin get 1.0 end] } err] } {
		catch { close $f }
		errwin $twin.fileerror "Error writing file '$filename'" \
			$err { }
		return 
	}
	if { [catch { close $f } err ] } {
		errwin $twin.fileerror "Error closing file '$filename'" \
			$err { }
		return
	}
	.state configure -text "Ready"
}

# Creates a window for saving text windows
proc textsavewindow { twin twid} {
	savefile $twin "textsave $twid" filter_save
	return
}

# Figures out the command line switches to be given to sgrep
proc sgrep_options {} {
	set o ""
	
	global opt_filter
	if { $opt_filter } { set o "-a" }
	global opt_count
	if { $opt_count } { set o "$o -c" }
	global opt_concat
	if { $opt_concat } { set o "$o -d" }
	global opt_nl
	if { $opt_nl } { set o "$o -N" }
	global opt_preproexpr
	if { $opt_preproexpr } { set o "$o -P" }
	global opt_stream
	if { $opt_stream } { set o "$o -S" }
	global opt_job
	if { $opt_job } { set o "$o -T" }
	global opt_time
	if { $opt_time } { set  o "$o -t " }
	global opt_out
	set o "$o $opt_out"
	# we have custom output style
	global ostyle
	if { $opt_out == "-o" } { set o "$o $ostyle" }
	# we have style file
	global outstylefile
	if { $opt_out == "-O" } { set o "$o $outstylefile"}
	# set preprocessor
	global preprocessor
	set o "$o -p $preprocessor"
	return $o
}

# Procedure for executing sgrep
proc execsgrep { sexpr } {
	global textwnum
	global errorCode
	global input_files
        global macros
        global body_array

	if { [llength $input_files] == 0 } {
		# no input files is an error
		errwin .sgerr "sgtool error" "No input files" { }
		return 0
	}

	set m [generate_macro_file $macros]

	.state configure -text "Executing query..."
	update

	# Execute sgrep
	set e [catch { 
	    eval exec sgrep -n -f - -e {$sexpr} [ sgrep_options ] $input_files << {$m}
	} errstr ]
	.state configure -text "Ready"

	incr textwnum
	set t .texttop$textwnum

	#create top level window
	toplevel $t 
	
	# baptizing windows
	wm title $t "sgreptool - query #$textwnum"
	wm iconname $t "query #$textwnum"

	# Create state label
	label $t.state -relief ridge -width 80
	pack $t.state -side top -fill x

	# If exit status != 1 it means sgrep error
	if { [lindex $errorCode 0] == "CHILDSTATUS" && $e != 0 } {
		if { [lindex $errorCode 2] != "1" } {
			destroy $t
			errwin .sgerr "sgrep error" $errstr { }
 			return 0
		}
		# We had empty output file
		# Label with the actual query
		$t.state config -text "Output from '$sexpr'"
		label $t.empty -text "No matching regions found." -relief ridge
		pack $t.empty -side top -fill x
		button $t.okbutton -text "OK" -command "destroy $t"
		pack $t.okbutton -side top
		focus $t.okbutton
		return 0
	} elseif { [lindex $errorCode 0] != "NONE"  && $e != 0 } {
		# Using default error handling
		destroy $t
		error $errstr
	}
	
	#create bottom byttons
	frame $t.bf
	pack $t.bf -side bottom
	foreach i "ok save edit wrap" {
		button $t.bf.$i -text "$i" -width 8 -underline 0
		pack $t.bf.$i -side left -padx 5
	}

	# ok button
	$t.bf.ok configure -command "destroy $t" -underline -1
	bind $t <Escape> "destroy $t"
	
	# wrap button
	proc wrap_b { dad } {
		if { [$dad.bf.wrap cget -relief] == "raised" } {
			$dad.bf.wrap configure -relief sunken
			$dad.text configure -wrap char
		} else {
			$dad.bf.wrap configure -relief raised
			$dad.text configure -wrap none
		}
	}
	$t.bf.wrap configure -relief sunken -command "wrap_b $t"
	bind $t <Alt-w> "$t.bf.wrap invoke"

	# edit button
	proc edit_b { dad } {
		if { [$dad.bf.edit cget -relief] == "raised" } {
			$dad.bf.edit configure -relief sunken
			$dad.text configure -state normal
		} else {
			$dad.bf.edit configure -relief raised
			$dad.text configure -state disabled
		}
	}
	$t.bf.edit configure -relief raised -command "edit_b $t"
	bind $t <Alt-e> "$t.bf.edit invoke"

	# save button
	proc save_b { dad num } {
		# Do nothing if save window already exists
		if { [winfo exists $dad.savewin] } { return }
		# Create save window
		toplevel $dad.savewin
		centerwin $dad.savewin $dad
		wm title $dad.savewin "save result #$num"
		textsavewindow $dad.savewin $dad.text
	}
	$t.bf.save configure -command "save_b $t $textwnum"
	bind $t <Alt-s> "$t.bf.save invoke"

	#create text scrollbars
	scrollbar $t.vscroll -orient vertical \
		-command "$t.text yview "
	scrollbar $t.hscroll -orient horizontal \
		-command "$t.text xview "

	text $t.text \
		-xscrollcommand "doset $t.hscroll \"-before $t.text -side bottom -fill x\" " \
		-yscrollcommand "doset $t.vscroll \"-before $t.text -side right -fill y\" " \
		-wrap char
	pack $t.text -side top -fill both -expand 1
	$t.text insert 1.0 $errstr
	$t.text configure -state disabled
	# Label with the actual query
	$t.state config -text "Output from '$sexpr'"

	focus $t.text
	update
	pack propagate $t 0 
	return 0
}

# Creates a listbox to given parent frame using hor and ver scrollbars
proc listb { pwin } {
	#create scrollbars
	scrollbar $pwin.vscroll -orient vertical \
		-command "$pwin.lb yview "
	scrollbar $pwin.hscroll -orient horizontal \
		-command "$pwin.lb xview "
	
	#create listbox
	listbox $pwin.lb \
		-xscrollcommand "doset $pwin.hscroll \"-side bottom -fill x -before $pwin.lb\" " \
		-yscrollcommand "doset $pwin.vscroll \"-side right -fill y -after $pwin.lb\" "
	pack $pwin.lb -side left -fill both -expand 1
	return $pwin.lb
}

# Fills file chooser window directory and files listboxes with
# filenames using globbing.
# Arguments:
#	dir	name of global variable containing directory name
#	filter	name of global variable containing filter
#	dwin	name of directory listbox window
#	fwin	name of file listbox window
proc globber { gl_dir gl_filter dwin fwin } {
	global $gl_dir
	global $gl_filter

	set d [ set $gl_dir ]
	set f [ set $gl_filter ]

	if { [string index $d [string length $d] ] != "/" } { set d $d/ }

	# empty listboxes
	$dwin delete 0 end
	$fwin delete 0 end

	# glob returns error for unreadable dirs ( that's wrong IMHO )
	if { [catch { glob -nocomplain --  $d$f } files] } {
		errwin [winfo toplevel $dwin].globerror \
			"Glob failed:" "$files" { }
		return
	}		
	foreach i [lsort $files] {
		if { [string first $d $i] == 0 } {
			set n [string range $i [string length $d] end ]
		} else { set n $i }
		if { [file isfile $i] } {
			$fwin insert end $n
		}
	}
	# Directories need their own glob, so that every directory will
	# be shown
	set files [ glob -nocomplain -- $d.* $d* ]
	foreach i [lsort $files] {
		if { [string first $d $i] == 0 } {
			set n [string range $i [string length $d] end ]
		} else { set n $i }
		if { [file isdirectory $i] } {
			$dwin insert end $n
		}
	}
}

# Removes one directory from dir and calls globber	
proc upglobber { gl_dir gl_filter dwin fwin } {
	global $gl_dir
	
	# gl_dir one up
	set $gl_dir [file dirname [set $gl_dir] ]
	globber $gl_dir $gl_filter $dwin $fwin
}

proc downglobber { gl_dir gl_filter dwin fwin } {
	global $gl_dir
	
	# if there is no selection do nothing
	if { [$dwin curselection] == "" } { return }

	set p [ set $gl_dir ]
	# use chosen directory
	set d [ $dwin get [ $dwin curselection ] ]
	if { "$d" == ".." } { 
		upglobber $gl_dir $gl_filter $dwin $fwin 
		return
	}
	if { "$d" == "." } { 
		globber $gl_dir $gl_filter $dwin $fwin 
		return
	}
	if { $p == "/" } {
		set $gl_dir /$d
	} else {
		set $gl_dir [set $gl_dir]/$d
	}
	globber $gl_dir $gl_filter $dwin $fwin
}

# Set filechooser selection	
proc setselection { se_dir se_filter dwin fwin ewin } {
	global $se_dir $se_filter
	
	# if there is no selection do nothing
	if { [$fwin curselection] == "" } { return }
	
	$ewin delete 0 end
	$ewin insert 0 [$fwin get [$fwin curselection]]
}
	
# Creates file chooser gadget to given frame. Uses fsvar as textvariable 
# for chosen file name. dir contains default directory.
# Returns name of the entry window containing file name
proc chooser { parent fsvar dir filter okcommand } {
	global $dir
	global $filter

	if { $fsvar != "" } { global $fsvar }

	# Expand directory
	if { [set $dir] == "." } {
		set $dir [ pwd ]
	}

	# Filter line
	if { [set $filter] == "" } { set $filter * }

	frame $parent.1
	pack $parent.1 -side top -fill x
	label $parent.1.l -width 10 -text "Filter:"
	pack $parent.1.l -side left -anchor e
	entry $parent.1.e -textvariable $filter
	pack $parent.1.e -side top -fill x

	# Directory line
	frame $parent.2
	pack $parent.2 -side top -fill x
	label $parent.2.l -width 10 -text "Directory:"
	pack $parent.2.l -side left -anchor e
	label $parent.2.e -textvariable "$dir" -relief ridge
	pack $parent.2.e -side top -fill x

	# Button line
	frame $parent.3
	pack $parent.3 -side top
	button $parent.3.ap -text "rescan" -width 8
	button $parent.3.up -text "up dir" -width 8
	button $parent.3.go -text "go dir" -width 8 
	button $parent.3.home -text "home" -width 8
	pack $parent.3.ap $parent.3.up $parent.3.go $parent.3.home -side left 

	# File name line
	frame $parent.4
	pack $parent.4 -side bottom -fill x
	label $parent.4.l -width 10 -text "Selection:"
	pack $parent.4.l -side left -anchor e
	pack [ entry $parent.4.e ] -side top -fill x
	if { $fsvar != "" } { $parent.4.e configure -textvariable "$fsvar" }
	focus $parent.4.e

	# Directories window
	frame $parent.dirf
	pack $parent.dirf -side left -expand 1 -fill both
	label $parent.dirf.dlabel -text Directories -anchor w
	pack $parent.dirf.dlabel -side top -fill x
	set dwin [ listb $parent.dirf ]
	# Files window
	frame $parent.filef
	pack $parent.filef -side left -expand 1 -fill both
	label $parent.filef.flabel -text Files -anchor w
	pack $parent.filef.flabel -side top -fill x
	set fwin [ listb $parent.filef ]

	# Glob the files to windows
	globber $dir $filter $dwin $fwin
	
	# set apply button to do globbing
	$parent.3.ap configure \
		-command "globber $dir $filter $dwin $fwin"
	# bind enter in filter window to do globbind
	bind $parent.1.e <Return> "globber $dir $filter $dwin $fwin"

	# set updir button to remove one directory and glob
	$parent.3.up configure -command \
		"upglobber $dir $filter $dwin $fwin"
	# set godir button to go to selected directory
	$parent.3.go configure -command \
		"downglobber $dir $filter $dwin $fwin"
	# home button to go to hom dir
	global env
	set h $env(HOME)
	$parent.3.home configure \
		-command "set $dir $h ; globber $dir $filter $dwin $fwin"
	# bind double click on dir window to go down hierarchy
	bind $dwin <Double-Button-1> "downglobber $dir $filter $dwin $fwin"
	bind $dwin <Return> "downglobber $dir $filter $dwin $fwin"
	# bindings for selecting file
	bind $fwin <ButtonRelease-1> "setselection $dir $filter $dwin $fwin $parent.4.e"
	bind $fwin <Return> "setselection $dir $filter $dwin $fwin $parent.4.e"
	# Bind double click on file name and enter in selection window
	# to execute ok command
	bind $fwin <Double-Button-1> \
		"setselection $dir $filter $dwin $fwin $parent.4.e ; $okcommand"
	bind $parent.4.e <Return> "$okcommand"
}

# When given dir name and file name, computes a real file name from
# them using algrithm below
proc make_name { dir file } {
	# No null files
	if { $file == "" } { return }
	# When dir == pwd, just add file name
	if { $dir == [pwd] } {
		return $file
	}
	# When filename starts with / just return file name
	if { [string index "$file" 0] == "/" } { return $file }
	set r $dir/$file
	# If dir path starts with pwd, remove it from filename
	if { [string first [pwd]/ $r] == 0 } {
		set r [string range $r [string length [pwd]/ ] end ]
	}
	return $r
}

# Sorts a given listbox windows lines
proc sort_listbox {lbox} {
	set sl [lsort [$lbox get 0 end] ]
	$lbox delete 0 end
	foreach i "$sl" {
		$lbox insert end $i
	}
}

# Removes active entry from listbox
proc remove_active { lbox } {
	if { [$lbox curselection] == "" } { return } 
	$lbox delete active
}

# Window for selecting multiple files
proc select_files { win text dirvar filesvar filtervar okcommand cancelcommand } {
	global $dirvar
	global $filesvar
	global $filtervar

	# Everything inside this window
	set w $win

	# Create file selection window
	frame $w.fsf
	pack $w.fsf -side left -fill both -expand 1

	# The files
	frame $w.ifiles
	pack $w.ifiles -side right -fill both -expand 1
	label $w.ifiles.l -text $text
	pack $w.ifiles.l  -side top -anchor w
	set iwin [ listb $w.ifiles ]

	# The buttons
	frame $w.buttons
	pack $w.buttons -side left
	foreach i "add remove clear sort ok cancel" {
		button $w.buttons.$i -text $i -width 8
		pack $w.buttons.$i -side top -pady 5 -padx 5
	}

	# Fill iwin with default input files
	foreach i [ set $filesvar ] {
		$iwin insert end $i
	}

	set apply "set $filesvar \[$iwin get 0 end\] ; $okcommand"

	# ok and cancel bindings
	$w.buttons.ok configure -command "$apply"
	$w.buttons.cancel configure -command "$cancelcommand"
	bind $w <Escape> "$cancelcommand"

	# remove button
	$w.buttons.remove configure -command "remove_active $iwin"
	# clear button
	$w.buttons.clear configure -command "$iwin delete 0 end"
	# sort button
	$w.buttons.sort configure -command "sort_listbox $iwin"

	# add button
	proc add_entry { iwin dirvar ewin} {
		global $dirvar
		set f [ $ewin get ]
		# No null files
		if { $f == "" } { return }
		$iwin insert end [make_name [set $dirvar] $f]
	}
	set add "add_entry $iwin $dirvar $w.fsf.4.e"
	$w.buttons.add configure -command "$add"
	# Create choose gadget
	chooser $w.fsf "" $dirvar $filtervar "$add"
	return $iwin
}

# Creates macro file selection window
proc macrocreate { } {
        global macro_editors

        # If there are macro editors around, we won't reselect macrofiles
        if { $macro_editors > 0 } {
	    errwin .fetcherr "Macro fetching error" \
"Close all macro editors before selecting
new macro files. Otherwise all changes would
be lost." { }
            return
        }

	# If we already have macrofile selection window, we don't start new
	# one
	if { [winfo exists .mfiles] } { return }
	
	global tmp_macros
	global macrofiles
	set tmp_macros $macrofiles

	# Create toplevel
	toplevel .mfiles
	wm title .mfiles "sgreptool - macrofiles"
	wm iconname .mfiles "macro files"
	centerwin .mfiles .

	wm protocol .mfiles WM_DELETE_WINDOW "destroy .mfiles"	
	set selw [ select_files .mfiles "Macro files:" macro_dir tmp_macros filter_macro \
		"okeido" "destroy .mfiles" ]
}

# Checks if macro files selected were okay
proc okeido { } {
    global tmp_macros
    global macrofiles

    set n ""
    foreach i [lsort $tmp_macros] {
	if { "$i"=="$n" } {
	    errwin .mfiles.errw "error - macro files" \
"You can use one macrofile only once.
File '$i' was selected twice" ""
            return
        }
	set n $i
	if { ! [file isfile $i] } {
	    errwin .mfiles.errw "error - macro files" \
"Selected file '$i' wasn't a regular file.
Please use only ordinary files as macro files." ""
            return
        }
    }
    set macrofiles $tmp_macros
    destroy .mfiles
    fetch_macros
}

# Creates the input file window.
proc ifcreate { } {
	# if input file window already exists do nothing
	if { [winfo exists .ifiles] } { return }

	# Create input file window
	toplevel .ifiles
	centerwin .ifiles .
	# Baptize window
	wm title .ifiles "sgreptool - input files" 
	wm iconname .ifiles "input files"

	set dest "destroy .ifiles"
	wm protocol .ifiles WM_DELETE_WINDOW "$dest"
	select_files .ifiles "Input files:" input_dir input_files filter_input \
		"$dest" "$dest"
}

# Destroys the input file window. ok for accept cancel for discard
proc destroy_ifwindow { how } {
	global input_files
	# if parameter was window name, use changes from window
	if  { "$how" != "cancel" } {
		set input_files [$how get 0 end]
	}
	destroy .ifwindow
}

# Creates a toplevel window for selecting output style
proc select_outstyle { } {
	# If output style window already exists do nothing
	if { [winfo exist .outstylewin] } { return }
	toplevel .outstylewin
	centerwin .outstylewin .
	wm title .outstylewin "sgreptool - output style"

	set o .outstylewin

	# buttons
	pack [ frame $o.bf ] -side bottom
	button $o.bf.ok -text "ok" -width 8 -command {
		set ostyle [.outstylewin.st.entry get]
		set opt_out "-o"
		destroy .outstylewin
	}		
	pack $o.bf.ok -side left -padx 5
	button $o.bf.cancel -text "cancel" -width 8 -command "
		set opt_out -s
		destroy $o"
	pack $o.bf.cancel -side left -padx 5
	# style
	pack [ frame $o.st ] -side top -fill x
	pack [ label $o.st.label -text "style:" -anchor e ] -side left
	global ostyle
	pack [ entry $o.st.entry ] -fill x
	$o.st.entry insert insert $ostyle
	bind $o.st.entry <Return> "$o.bf.ok invoke"
	focus $o.st.entry
	# stylebuttons
	pack [ frame $o.stbf ] -side top
	foreach i { 
		{ filename %f }
		{ start %s }
		{ end %e }
		{ length %l }
		{ file_start %i }	
		{ file_end %j }
		{ region %r } 
		{ number %n }
		{ % %% } } {
		set name [ lindex $i 0 ]
		set str [ lindex $i 1 ]
		pack [ 
			button $o.stbf.$name -text $name -width 5 \
				-command "$o.st.entry insert insert $str"
			] -side left
	}
}

# Creates a window for selecting preprocessor
proc precreate { } {
	set w .preprowin
	# We do nothing if prepro win already exists
	if { [winfo exists .preprowin] } { return }
	global newpreprocessor
	global preprocessor
	set newpreprocessor $preprocessor
	# Create error window
	toplevel $w
	centerwin $w .
	wm title $w "sgreptool - select preprocessor"
	# This is how this window gets killed
	set destroy_c "destroy $w"
	# How we look like
	pack [ entry $w.entry -textvariable newpreprocessor ] -side top -fill x
	focus $w.entry
	bind $w.entry <Return> "$w.bf.ok invoke"
	# Create buttons
	frame $w.bf
	pack $w.bf -side bottom
	foreach i { ok cancel m4 } {
		button $w.bf.$i -width 8 -text $i
		pack $w.bf.$i -side left -padx 5 -pady 5
	}
	$w.bf.ok configure -command {
		global preprocessor
		set preprocessor $newpreprocessor
		destroy .preprowin}
	set dest "
		destroy .preprowin"
	wm protocol .preprowin WM_DELETE_WINDOW "$dest"
	$w.bf.cancel configure -command "$dest"
	$w.bf.m4 configure -command "
		set preprocessor m4
		$dest"
}

# Destroys popupwindows for selecting outputstyle
proc del_stylepop {} {
	if { [winfo exists .outstylewin ] } { destroy .outstylewin }
	if { [winfo exists .outstylefilewin ] } { destroy .outstylefilewin }
}

# Applys user selected preferences by packing or unpacking main window frames
proc apply_preferences { } {
	global pref_input
	global pref_macros
	global pref_macrofiles
	global pref_ver
	global pref_status
	
        # Focus to expr window, so that it will never disappear
        focus .expr

	foreach i { .comm .macros .input .macro .up2 .verw .state } {
		pack forget $i
	}
	if { $pref_macros } {
		pack .macros -side top -expand 1 -fill both
	}
	if { $pref_input } {
		 pack .input -side top -fill x
	} 
	if { $pref_macrofiles } {
		pack .macro -side top -fill x
	}
	if { $pref_ver } {
		 pack .verw -side top -fill x
	} 
	if { $pref_status } {
 		pack .state -side top -fill x
	} 
}

# Puts the macro highlighted in given listbox to given textwindow
# Puts the macro files name to given label window
proc body_to_text { mlist t lw } {
	global body_array
	global macro_edit
        global macro_file_array
        global macros
    
        set m [$mlist curselection]
        if {$m==""} { return }
	set i [lsearch "$macros" [$mlist get $m]]
	if {$i==-1} { return }
	$t configure -state normal
	$t delete 1.0 end
	$t insert end $body_array($i)
	$t configure -state disabled
	# If no label window was given use none
	if { "$lw"=="" } { return }

	set f $macro_file_array([$mlist curselection])
        $lw configure -text $f
}

# Puts wrapping on or off in given textwindow according to given global variable
proc macro_proc_wrap { textw var } {
        global $var
        set macro_wrap [set $var]
	if { $macro_wrap } {
		# It was turned on
		$textw configure -wrap char
	} else {
		$textw configure -wrap none
	}
}

# Creates a menu with given name, with all sgrep commands
# When item is selected invokes com with text to be inserted and
# number indicating cursor movement
proc sgrep_menu { m com } {
    menu $m
    set c "in {not in} containing {not containing} equal {not equal} or extracting .. ._ _. __ quote _quote quote_ _quote_"
    foreach i "$c" {
	$m add command -label $i -command "$com \{ $i \} 0"
    }
    $m add separator
    set c "outer inner concat join"
    foreach i "$c" {
	$m add command -label "$i\( \)" -command "$com \{ $i\(  \) \} -3"
    }
    $m add separator
    set c "start end chars"
    foreach i "$c" {
	$m add command -label "$i" -command "$com \{ $i \} 0"
    }
    $m add separator
    $m add command -label "( )" -command "$com {(  )} -2"
    $m add command -label "\" \"" -command "$com {\"\"} -1"
    $m add command -label {[ ]} -command "$com \{\[\]\} -1"
}
    
# This command is executed when macro editor is spawned
proc macro_editor { winname macrofile } { 
    global macros
    global body_array
    global macro_file_array
    global macro_editors
    
    if { [winfo exists .mfiles] } {
	errwin .editerr "Macro editor error" \
"Close your macrofile selection window
Before editing macros." { }
        return
    }

    # If window already exists, bringt it to front
    if { [winfo exists $winname] } {
	wm withdraw $winname
	centerwin $winname .
	wm deiconify $winname
	return
    }
    
    # We have now one macro editor more
    incr macro_editors

    # Variables of one instance of macro editor
    global macros_edited$winname
    set macros_edited$winname 0
    global macro_file$winname
    set macro_file$winname $macrofile
    global macro_num$winname
    set macro_num$winname -1

    # Toplevel window
    toplevel $winname
    wm title $winname "Macro editor - $macrofile"
    centerwin $winname .

    # Menubar
    pack [
         frame $winname.menu -relief raised -borderwidth 2p
         ] -side top -fill x
    menubutton $winname.menu.file -menu $winname.menu.file.m -text "File" \
	    -underline 0
    menubutton $winname.menu.macros -menu $winname.menu.macros.m -text "Macros" \
	    -underline 0
    menubutton $winname.menu.sgrep -menu $winname.menu.sgrep.m -text "Operators" \
	    -underline 3 -state disabled

    pack $winname.menu.file -side left
    menu $winname.menu.file.m
    $winname.menu.file.m add command -label "Save" \
	    -command "me_save $winname"
    $winname.menu.file.m add command -label "Save as.." \
	    -command "me_save_as $winname"
    $winname.menu.file.m add separator
    $winname.menu.file.m add command -label "Close" -accelerator "Esc" \
	    -command "me_cancel $winname"

    pack $winname.menu.macros -side left
    menu $winname.menu.macros.m
    $winname.menu.macros.m add command -label "Rename" \
	    -command "me_rename_macro $winname"
    $winname.menu.macros.m add command -label "Insert" \
	    -command "me_insert_macro $winname"
    $winname.menu.macros.m add command -label "Remove" \
	    -command "me_remove_macro $winname"
    $winname.menu.macros.m add separator
    $winname.menu.macros.m add command -label "Execute" \
	    -command "me_execute_macro $winname"
    pack $winname.menu.sgrep -side left
    sgrep_menu $winname.menu.sgrep.m "me_insertsgrep $winname"

    #Macro list
    pack [ frame $winname.ml ] -side left -fill y
    pack [
           label $winname.ml.label -text "Macro names" -anchor w
         ] -side top -fill x
    set ml [listb $winname.ml]
    $ml configure -height 25
    set i [llength $macros]
    while { $i>0 } {
	incr i -1
	if { "$macro_file_array($i)"=="$macrofile" } {
	    $ml insert 0 [lindex $macros $i]
	}
    }
    $ml activate 0

    #Editor window
    pack [frame $winname.edit] -side right -fill both -expand 1
    pack [ frame $winname.edit.df ] -side top -fill x
    pack [ 
          label $winname.edit.df.label -text "Macro Editor - macro:" -anchor w 
         ] -side left
    pack [
          entry $winname.edit.df.macro -relief sunken
         ] -side top -fill x
    set t $winname.edit
    
    #Editor buttons
    pack [ frame $t.bf ] -side bottom -fill x
    foreach i "apply cancel" {
	pack [ 
	     button $t.bf.$i -text "$i" -width 6 -state disabled \
		     -command "me_ebutton_$i $winname" \
		     -underline 0
	] -side left
    }
    # Editor checkbuttons
    foreach i "edit wrap" {
	pack [
	     checkbutton $t.bf.$i -text "$i" -width 6 \
		     -command "me_ebutton_$i $winname" \
		     -variable "$i$winname" -underline 0
	] -side right
	global $i$winname
    }
    $t.bf.wrap configure -command "macro_proc_wrap $t.text wrap$winname"
    set wrap$winname 1
    set edit$winname 0

    # Textwindow
    pack [ text $t.text -width 50 -state disabled \
		-xscrollcommand "doset $t.hscroll \"-before $t.text -side bottom -fill x\" " \
		-yscrollcommand "doset $t.vscroll \"-before $t.text -side right -fill y\" " \
		-wrap char ] -side right -fill both -expand 1
    scrollbar $t.vscroll -orient vertical \
	    -command "$t.text yview "
    scrollbar $t.hscroll -orient horizontal \
	    -command "$t.text xview "
    
    # Bindings
    set bt "me_body_to_text $winname"
    bind $ml "<Double-Button-1>" "$bt"
    bind $ml "<ButtonRelease-1>" "me_listbutton1 $winname"
    bind $ml "<space>" "$bt"
    bind $ml "<Return>" "$bt"

    bind $winname "<Alt-e>" "$winname.edit.bf.edit invoke"
    bind $winname "<Alt-w>" "$winname.edit.bf.wrap invoke"
    bind $winname "<Alt-c>" "$winname.edit.bf.cancel invoke"
    bind $winname "<Alt-a>" "$winname.edit.bf.apply invoke"
    bind $winname "<Escape>" "me_cancel $winname"
    bind $winname.edit.df.macro "<Return>" "me_macro_enter $winname"
    bind $winname.edit.text "<Button-3>" "me_postmenu $winname"

    wm protocol $winname WM_DELETE_WINDOW "me_cancel $winname"
    
    # Focus to macro list
    focus $ml
    # No spontaneous resizing
    update
    pack propagate $winname 0
}

# Procedures starting with me_ are macro editor procedures, which are given
# macro editor window name as first parameter

# Close macro editor
proc me_cancel { winname } {
    global macros_edited$winname
    global macro_file$winname
    global macro_editors

    if { [set macros_edited$winname] } {
	yesno $winname.close \
		"Are you sure you want to close editor \nwithout saving changes ?" \
		"global macros_edited$winname 
                 set macros_edited$winname false
	         me_cancel $winname" { }
        return
    }
    unset macros_edited$winname
    unset macro_file$winname
    destroy $winname
    incr macro_editors -1
}

# Saves macro names to default macro file
proc me_save { winname } {
    global macro_file$winname
    global macros_edited$winname

    set smacros [$winname.ml.lb get 0 end]
    set file [set macro_file$winname]

    set result [do_save_macros "$smacros" "$file" "$winname.saveerr"]
    set macros_edited$winname $result
    return $result
}

# Asks for new macro file name, then invokes do_save_as
proc me_save_as { winname } {
    global macro_file$winname
    
    if { [winfo exists $winname.saveas] } { return }
    toplevel $winname.saveas
    wm title $winname.saveas "Save macrofile [set macro_file$winname] as"
    savefile $winname.saveas "me_do_save_as $winname" filter_macro
}

# Saves macros to new file
proc me_do_save_as { winname newfile } {
    global macro_file$winname
    global macrofiles
    global macros
    global macro_file_array

    if { [lsearch -exact "$macrofiles" $newfile]!=-1 } {
	errwin $winname.saverr "Macrofile saving error" \
"macro file with name \"$newfile\"\n is already in use. Choose another name" \
             "me_save_as $winname"
       return
    }
    set oldfile [set macro_file$winname]
    set macro_file$winname $newfile

    if { ![me_save $winname] } {
	#Saving was ok
	wm title $winname "Macro editor - $newfile"
	set i [lsearch -exact "$macrofiles" $oldfile]
	set macrofiles [lreplace "$macrofiles" $i $i $newfile]
	set j [llength "$macros"]
	while { "$j">0 } {
	    incr j -1
	    if { "$macro_file_array($j)"=="$oldfile" } {
		set macro_file_array($j) $newfile
	    }
	}
    } else {
	#Saving failed
	set macro_file$winname $oldfile
    }
}


#Macro editor edit checkbutton
proc me_ebutton_edit { winname } {
    global edit$winname
    global macros_edited$winname

    if { [set edit$winname] } {
	$winname.edit.text configure -state normal
	$winname.edit.bf.apply configure -state normal
	$winname.edit.bf.cancel configure -state normal
	$winname.menu.sgrep configure -state normal
	$winname.menu.macros configure -state disabled
	focus $winname.edit.text
    } else {
	$winname.edit.text configure -state disabled
	$winname.edit.bf.apply configure -state disabled
	$winname.edit.bf.cancel configure -state disabled
	$winname.menu.sgrep configure -state disabled
	$winname.menu.macros configure -state normal
	focus $winname.ml.lb
    }
}

# Macro editor apply button
proc me_ebutton_apply { winname } {
    global macro_num$winname
    global macro_file$winname
    global macros_edited$winname
    global edit$winname
    global body_array
    global macro_file_array
    global macros

    # If for some reason this proc is invoked when not editing
    if { ! [set edit$winname] } { return }
   
    set num [set macro_num$winname]
    set m [$winname.edit.df.macro get]
    set l [$winname.ml.lb get 0 end]
    set i [ lsearch -glob "$l" $m]
    set m [$winname.ml.lb get $i]
    if { $i!=$num || $num==-1 } {
	if {$i==-1} {
	    # We had new macro name to be saved
	    set last [ lsearch -exact "$macros" [lindex "$l" 0]]
	    set last [expr $last + [llength "$l"]]
	    macro_space $last

	    # insert macro to global macro arrays & lists
	    set body_array($last) [$winname.edit.text get 0.0 end]
	    set macro_file_array($last) [set macro_file$winname]
	    set macros [linsert "$macros" $last [$winname.edit.df.macro get]]
	    # insert macro to macro listbox
	    $winname.ml.lb insert end [$winname.edit.df.macro get]

	    me_changes $winname
	    $winname.edit.bf.edit invoke
	    return
	}
	# We overwrite old macro
	set i [ lsearch -exact "$macros" $m]
	yesno $winname.replace "Overwrite old macro $m" "
	set body_array($i) \{[$winname.edit.text get 0.0 end]\}
	me_changes $winname
	$winname.edit.bf.edit invoke" ""
	return
    }
    # Replace old macro with newly edited
    set i [lsearch -exact "$macros" $m]
    set body_array($i) [$winname.edit.text get 0.0 end]

    me_changes $winname
    #Turn of editing
    $winname.edit.bf.edit invoke
}   

# Macro editor cancel editing button
proc me_ebutton_cancel { winname } {
    global edit$winname
    global macro_num$winname

    # If for some reason this proc is invoked when not editing
    if { ! [set edit$winname] } { return }
    
    $winname.edit.text delete 0.0 end
    set macro_num$winname -1
 
    # Turn off editing
    $winname.edit.bf.edit invoke
}

# Macro body to text window button
proc me_body_to_text { winname } {
    global edit$winname
    global macro_num$winname
    
    #If invoked when not editing we fetch macro body to window
    if { ! [set edit$winname] } {
	set macro_num$winname [$winname.ml.lb curselection]
	if { [set macro_num$winname]=="" } {
	    set macro_num$winname 0
	}
	# set macro name entry
	$winname.edit.df.macro delete 0 end
	$winname.edit.df.macro insert 0 [$winname.ml.lb get [set macro_num$winname]]
	body_to_text $winname.ml.lb $winname.edit.text {}
	return
    }

    #If invoked when editing insert macro name from listbox
    set mn [$winname.ml.lb get active]
    $winname.edit.text insert insert " $mn "
}

# Post command menu to mouse position
proc me_postmenu { winname } {
    global edit$winname

    # If for some reason this proc is invoked when not editing
    if { ! [set edit$winname] } { return }
    
    set y [winfo pointery $winname]
    set x [winfo pointerx $winname]
    set y [expr $y - 20]
    set x [expr $x - 40]

    if { "$y"<0 } { set y 0 }
    if { "$x"<0 } { set x 0 }
    $winname.menu.sgrep.m post $x $y
    focus $winname.menu.sgrep.m
}

# Inserts a given text to textwindow of given macroeditor and moves cursor
proc me_insertsgrep { winname t cinc } {
    $winname.edit.text insert insert "$t"
    set s [$winname.edit.text index insert]
    # File command isn't made for this purpose, but it works well
    set r [file rootname $s]
    set c [expr [string trim [file extension $s] .] + $cinc ]
    $winname.edit.text mark set insert $r.$c
}

# When not editing button 1 in listbox fetches macros text
proc me_listbutton1 { winname } {
    global edit$winname
    if { [set edit$winname] } { return }
    me_body_to_text $winname
}

# Procedure which is invoked when enter is pressed in macro name entry
proc me_macro_enter { winname } {
    global edit$winname
    global macro_num$winname
    
    # if not editing macro file, fetch the macro text
    if { ! [set edit$winname] } {
	set m [$winname.edit.df.macro get]
	set l [$winname.ml.lb get 0 end]
	set i [ lsearch -glob "$l" $m]
	if { $i==-1 } {
	    $winname.edit.text delete 0 end
	    set macro_num$winname -1
	    return
	}
	$winname.ml.lb see $i
	$winname.ml.lb selection clear 0 end
	$winname.ml.lb selection set $i
	$winname.ml.lb activate $i
	set macro_num$winname $i
	me_body_to_text $winname
    }
    # Macro has new name, switch focus to edit window
    focus $winname.edit.text
}

# Macro editor execute macro
proc me_execute_macro { winname } {
    set i [$winname.ml.lb curselection]
    if { "$i"=="" } { set i 0 }
    set m [$winname.ml.lb get $i]
    if { "$m"=="" } {return }
    execsgrep "$m"
}

# Macro editor insert button
proc me_remove_macro { winname } {
    global edit$winname
   
    if { [set edit$winname] } { return }
    if { [ winfo exists $winname.insert] } { return }

    set i [$winname.ml.lb curselection]
    if { "$i"==""} { set i 0 }
    set m [$winname.ml.lb get $i]
    if { "$m"==""} { return }
    
    yesno $winname.remove \
"Are you sure you want to remove macro $m" "me_do_remove $winname" { }
}

proc me_do_remove { winname } {
    global macros
 
    set r $winname.remove
  
    set i [$winname.ml.lb curselection]
    if { "$i"==""} { set i 0 }
    set m [$winname.ml.lb get $i]
    if { "$m"==""} { return }
 
    macro_remove $i
    me_changes $winname
    $winname.ml.lb delete $i
}

# Macro editor insert button
proc me_insert_macro { winname } {
    global edit$winname
   
    if { [set edit$winname] } { return }
    if { [ winfo exists $winname.insert] } { return }

    set i [$winname.ml.lb curselection]
    if { "$i"==""} { set i 0 }
    set m [$winname.ml.lb get $i]
    if { "$m"==""} { return }
    
    set r [toplevel $winname.insert]
    wm transient $r $winname

    pack [ frame $r.bottom ] -side bottom
    pack [ frame $r.left ] -side left
    pack [ frame $r.right] -side right

    # buttons
    button $r.bottom.cancel -text "Cancel" -command "destroy $r" -width 8
    button $r.bottom.ok -text "Insert" -command "me_do_insert $winname" \
	    -width 8
    pack $r.bottom.cancel $r.bottom.ok -side left

    # Left labels
    label $r.left.from -text "Insert before:" -anchor e
    pack $r.left.from -fill x -side top
    label $r.left.to -text "macro name:" -anchor e
    pack $r.left.to -fill x -side top
    # Right label & entry
    pack [ label $r.right.from -text "$m" -relief ridge -anchor w] -side top -fill x
    pack [ entry $r.right.entry -width 12 -relief sunken ] -side top -fill x

    bind $r "<Escape>" "destroy $r"
    bind $r.right.entry "<Return>" "me_do_insert $winname"

    centerwin $r $winname
    grab $r
    focus $r.right.entry
}

proc me_do_insert { winname } {
    global macros
    global body_array

    set r $winname.insert
    
    set new_name [$r.right.entry get]
    if { "$new_name"=="" } {
	# Empty macro name, just destroy window
	destroy $r
	return
    }
    set i [lsearch -exact "$macros" $new_name] 
    if { "$i"!="-1" } {
	# Macro with given name already exists, we give error message
	errwin $r.renerror "Macro insert error:" \
"Macro with given name already existed.
Use some other name." "grab $r;focus $r.right.entry"
        return
    }
    set i [lsearch -exact "$macros" [$r.right.from cget -text]]

    # Now we do inserting
    macro_space $i
    # Whatever is in the text box is used as macro body
    set body_array($i) [$winname.edit.text get 0.0 end]
    set macros [linsert "$macros" $i $new_name]
    set j [$winname.ml.lb curselection]
    if { "$j"=="" } { set j 0 }
    $winname.ml.lb insert $j $new_name
    $winname.ml.lb activate $j
    $winname.ml.lb selection clear 0 end
    $winname.ml.lb selection set $j
    $winname.edit.df.macro delete 0 end
    $winname.edit.df.macro insert 0 $new_name
    destroy $r
    me_changes $winname
}

# Macro editor rename button
proc me_rename_macro { winname } {
    global edit$winname
   
    if { [set edit$winname] } { return }
    if { [ winfo exists $winname.rename] } { return }
    
    set i [$winname.ml.lb curselection]
    if { "$i"==""} { set i 0 }
    set m [$winname.ml.lb get $i]
    if { "$m"==""} { return }

    set r [toplevel $winname.rename]
    wm transient $r $winname

    pack [ frame $r.bottom ] -side bottom
    pack [ frame $r.left ] -side left
    pack [ frame $r.right] -side right

    # buttons
    button $r.bottom.cancel -text "Cancel" -command "destroy $r" -width 8
    button $r.bottom.ok -text "Rename" -command "me_do_rename $winname" \
	    -width 8
    pack $r.bottom.cancel $r.bottom.ok -side left

    # Left labels
    label $r.left.from -text "Rename macro:" -anchor e
    pack $r.left.from -fill x -side top
    label $r.left.to -text "to macro:" -anchor e
    pack $r.left.to -fill x -side top
    # Right label & entry
    pack [ label $r.right.from -text "$m" -relief ridge -anchor w] -side top -fill x
    pack [ entry $r.right.entry -width 12 -relief sunken ] -side top -fill x

    bind $r "<Escape>" "destroy $r"
    bind $r.right.entry "<Return>" "me_do_rename $winname"

    centerwin $r $winname
    grab $r
    focus $r.right.entry
}

proc me_do_rename { winname } {
    global macros

    set r $winname.rename
    
    set new_name [$r.right.entry get]
    if { "$new_name"=="" } {
	# Empty macro name, just destroy window
	destroy $r
	return
    }
    set i [lsearch -exact "$macros" $new_name] 
    if { "$i"!="-1" } {
	# Macro with given name already exists, we give error message
	errwin $r.renerror "Macro renaming error:" \
"Macro with given name already existed.
Use some other name." "grab $r;focus $r.right.entry"
        return
    }
    set i [lsearch -exact "$macros" [$r.right.from cget -text]]
    # Now we do renaming
    set macros [lreplace "$macros" $i $i $new_name]
    set j [$winname.ml.lb curselection]
    if { "$j"=="" } { set j 0 }
    $winname.ml.lb delete $j
    $winname.ml.lb insert $j $new_name
    $winname.ml.lb activate $j
    $winname.ml.lb selection set $j
    destroy $r
    me_changes $winname
}

# Changes have been done to macros.
proc me_changes { winname } {
    global macros_edited$winname
    global mlist

    # Macros of this window have now been edited
    set macros_edited$winname 1
    # We update main windows macro list
    update_macro_list
    # We do body_to_text in main window
    body_to_text $mlist .macros.text .macros.tbf.filename
}
    
# Makes space in macro array for one macro
proc macro_space { ind } {
    global macros
    global body_array
    global macro_file_array

    set l [llength $macros]
    set p [expr $l - 1]
    while { "$ind" < "$l" } {
	set body_array($l) $body_array($p)
	set macro_file_array($l) $macro_file_array($p)
	incr p -1
	incr l -1
    }
}

# Removes one macro from macro array
proc macro_remove { ind } {
    global macros
    global body_array
    global macro_file_array

    set l [llength $macros]
    set macros [lreplace "$macros" $ind $ind]
    set next [expr $ind + 1]
    while { $next < $l } {
	set body_array($ind) $body_array($next)
	set macro_file_array($ind) $macro_file_array($ind)
	incr ind
	incr next
    }
}

# This command is executed when macro editing is asked
# It asks for macro file to be edited. If only one macrofile is used it
# spawns editor immediately
proc edit_macros { } {
    global macrofiles
    # No macrofiles, no editing
    if { [llength $macrofiles] == 0 } return
    if { [llength $macrofiles] > 1 } {
	# Here will be macro file chooser system some day
	if { [winfo exists .editch] } {
	    return
	}

	toplevel .editch
	wm title .editch "Choose macrofile"
	wm transient .editch .
	centerwin .editch .

	pack [ label .editch.label -relief raised -text "Which macrofile you wish to edit ?"
	     ] -side top -fill x
	pack [ frame .editch.bf ] -side bottom
	foreach i "edit cancel" {
	    pack [
	      button .editch.bf.$i -text $i -width 6
	    ] -side left -pady 5
	}

	set lb [listb .editch]
	eval $lb insert end $macrofiles
	set i [llength $macrofiles]

	set edit_me {
	    macro_editor .macroedit[.editch.lb curselection] \
		    [lindex $macrofiles [.editch.lb curselection]]
	    destroy .editch
	}
	.editch.bf.cancel configure -command "destroy .editch"
	.editch.bf.edit configure -command "$edit_me"
	focus $lb
	bind .editch "<Escape>" "destroy .editch"
	bind .editch "<Return>" "$edit_me"
	bind .editch "<space>" "$edit_me"
	bind .editch "<Double-Button-1>" "$edit_me"
	grab .editch
    } else {
	macro_editor .macroedit0 [lindex $macrofiles 0 ]
    }
}

# Inserts sgrep command to entry
proc insert_entry { cmd move } {
    .expr insert insert "$cmd"
    set t [.expr index insert]
    set t [ expr $t + $move]
    .expr icursor $t
}

# Post sgrep command menu to cursor
# position
proc postsgrepmenu { } {
    set y [winfo pointery .]
    set x [winfo pointerx .]
    set y [expr $y - 20]
    set x [expr $x - 40]

    if { "$y"<0 } { set y 0 }
    if { "$x"<0 } { set x 0 }
    .menu.sgrep.m post $x $y
    focus .menu.sgrep.m
}

proc do_exit { } {
    global macro_editors

    if { $macro_editors>0 } {
	yesno .dying "There are still macro editors open\nExit anyway ?" \
		{ exit } { }
	return
    }
    exit
}

# Window for ascing file name for all macros
proc save_all_macros { } {
    if { [winfo exists .saveall] } { return }
    toplevel .saveall
    wm title .saveall "Save all macros as"
    centerwin .saveall .
    savefile .saveall "do_save_all" filter_macro
}

# Saves all macros to given file
proc do_save_all { file } {
    global macros

    if { [llength "$macros"]==0 } {
	# No macros, nothing to save
	errwin .saveall "Macro saving error" "No macros to save!" { }
	return
    }
    do_save_macros "$macros" $file .saveerr
}

# Saves given macros to given file using given errorwindow
# Returns immediately returning 1 on error
proc do_save_macros { smacros file errtop } {
    set f [generate_macro_file "$smacros"]
    .state configure -text "Saving macrofile.."
    update
    if { [catch { set fd [open $file w] } err] } {
	errwin $errtop "Macro saving error" \
"Could not open macro file for saving.
Reason given:\n$err" { }
        return 1
    }
    if { [catch { puts -nonewline $fd "$f" } err] } {
	errwin $errtop "Macro saving error" \
"Writing macro file failed.
Data may have corrupt.
Reason given:\n$err" { }
        close $fd
        return 1
    }
    close $fd
    .state configure -text "Ready"
    return 0
}

#
# Creates an about toplevel window
proc aboutcreate {  } {
    global sgtversion
    if { [winfo exists .about] } {
	wm withdraw .about
	wm deiconify .about
	centerwin .about .
	return
    }
    toplevel .about
    wm title .about "sgreptool -about"
    pack [ button .about.ok -text "ok" -command "destroy .about" -width 10] -side bottom
    message .about.msg -font -Adobe-times-medium-r-normal--*-180-*-*-*-*-*-* \
        -relief raised -width 500 \
        -borderwidth 1 -justify center \
	-text "
Sgreptool $sgtversion - A frontend to structured text retrieval tool sgrep

Sgreptool and sgrep were made by:
Jani Jaakkola, Jani.Jaakkola@cc.helsinki.fi
Pekka Kilpeläinen, Pekka.Kilpelainen@cc.helsinki.fi

Copyright University of Helsinki, Dept. of Computer Science
Distributed under GNU General Public Lisence
See file COPYING for details
" 
    pack .about.msg -fill x
    focus .about.ok
    centerwin .about .
}

# Our name is sgreptool
wm title . "sgreptool"

# Pull down menus
frame .menu -relief raised -borderwidth 2p
pack .menu -side top -fill x

menubutton .menu.file -menu .menu.file.m -text File -underline 0
menu .menu.file.m
.menu.file.m add command -label "Execute query" -command {execsgrep $sgrepexpr}
.menu.file.m add separator
.menu.file.m add command -label "Input files .." -command "ifcreate"
.menu.file.m add command -label "Macrofiles .." -command "macrocreate"
.menu.file.m add command -label "Preprocessor .." -command "precreate"
.menu.file.m add separator
.menu.file.m add command -label "Rescan macrofiles" -command "fetch_macros"
.menu.file.m add command -label "Edit macrofile.." -command "edit_macros"
.menu.file.m add command -label "Save all macros as.." \
	-command "save_all_macros"
.menu.file.m add separator
.menu.file.m add command -label "About .." -command "aboutcreate"
.menu.file.m add separator
.menu.file.m add command -label "Exit" -command "do_exit" -underline 1

menubutton .menu.options -menu .menu.options.m -text Options -underline 0
menu .menu.options.m
.menu.options.m add checkbutton -label "Filter mode" -variable opt_filter
.menu.options.m add checkbutton -label "Only count regions" -variable opt_count
.menu.options.m add checkbutton -label "No concat" -variable opt_concat
.menu.options.m add checkbutton -label "No trailing newline" -variable opt_nl
.menu.options.m add checkbutton -label "Show preprocessed expression" -variable opt_preproexpr
.menu.options.m add checkbutton -label "Stream mode" -variable opt_stream
.menu.options.m add separator
.menu.options.m add radiobutton -label "Short output style"  \
	 -variable opt_out -value "-s" -command "del_stylepop"
.menu.options.m add radiobutton -label "Long output style" \
	-variable opt_out -value "-l" -command "del_stylepop"
.menu.options.m add radiobutton -label "Custom output style .." \
	-variable opt_out -value "-o" -command "del_stylepop;select_outstyle"
.menu.options.m add radiobutton -label "Output style file .." \
	-variable opt_out -value "-O" -command "del_stylepop;select_outfile"

.menu.options.m add separator
.menu.options.m add checkbutton -label "Job statistics" -variable opt_job
.menu.options.m add checkbutton -label "Time statistics" -variable opt_time

menubutton .menu.pref -menu .menu.pref.m -text Preferences -underline 0
menu .menu.pref.m
.menu.pref.m add checkbutton -label "Show macros" -variable pref_macros -command apply_preferences
.menu.pref.m add checkbutton -label "Show input files" -variable pref_input -command apply_preferences
.menu.pref.m add checkbutton -label "Show macro files" -variable pref_macrofiles -command apply_preferences
.menu.pref.m add checkbutton -label "Show sgrep version" -variable pref_ver -command apply_preferences
.menu.pref.m add checkbutton -label "Show status line" -variable pref_status -command apply_preferences

menubutton .menu.sgrep -menu .menu.sgrep.m -text Operators -underline 3
sgrep_menu .menu.sgrep.m insert_entry

pack .menu.file .menu.options .menu.pref .menu.sgrep -side left

# These frames unpacked and packed when user selects preferences
 # Frame for expression related widgets
 frame .up1 -relief ridge
 pack .up1 -side top -fill x 
 
 # Frame for sgrep macros
 frame .macros
 # Frame for input files
 frame .input
 # Frame for macro files
 frame .macro
 # Window containing version text
 label .verw -relief ridge -text "using $sgrepver"
 # Window containing state
 label .state -relief ridge -text "Ready" -width 15

# Expression label & nearby buttons
pack [frame .exprstuff] -in .up1 -side left -fill y
pack [label .exprstuff.label -text expression: -width 10 ] -side top -anchor ne -pady 2
pack [ 
     button .exprstuff.clear -text "Clear" -command { set sgrepexpr "" } 
     ] -side top  -fill x
# Expression entry. Switch focus by default to this.
entry .expr -textvariable sgrepexpr -width 50 \
	-xscrollcommand { .exprscroll set }
pack  .expr -side top -in .up1 -fill x -ipadx 10p -ipady 2p
focus .expr
# Enter executes query
bind .expr <Return> {execsgrep $sgrepexpr}
# Button 3 gives sgrep commands menu
bind .expr <Button-3> "postsgrepmenu"

# Expression scrollbar
scrollbar .exprscroll -orient horizontal -command { .expr xview }
pack .exprscroll -side bottom -in .up1 -fill x

# label for input files
label .input.label -text "Input files:" -width 10 -anchor ne
pack .input.label -side left
label .input.text -textvariable input_files -relief ridge -anchor w
pack .input.text -fill x

# label for macro files
pack [label .macro.label -text "Macro files:" -width 10 -anchor ne] -side left
pack [label .macro.text -textvariable macrofiles -relief ridge -anchor w ] -fill x
# Button frame
frame .bf
pack .bf -side bottom

# Macro window
pack [frame .macros.l] -side left -fill y
pack [label .macros.l.label -text Macros -anchor w] -side top -fill x
set mlist [listb .macros.l]

set bt "body_to_text $mlist .macros.text .macros.tbf.filename"
bind $mlist <space> "$bt"
bind $mlist <ButtonRelease-1> "$bt"
bind $mlist <Double-Button-1> {.expr insert insert " [$mlist get active] "}
bind $mlist <Return> {.expr insert insert " [$mlist get active] "}
$mlist configure -height 6 -width 20
pack [ frame .macros.tbf ] -side top -fill x
pack [ label .macros.tbf.tlabel -text "Macro text - From file:" -anchor w ] -side left
pack [ checkbutton .macros.tbf.wrap -text wrap -variable macro_wrap \
		-command "macro_proc_wrap .macros.text macro_wrap" ] -side right

set macro_wrap 1
pack [ label .macros.tbf.filename -relief ridge -width 25 -anchor w
     ] -side left -fill x -expand 1
# create macro text scrollbars
scrollbar .macros.vscroll -orient vertical \
	-command ".macros.text yview "
scrollbar .macros.hscroll -orient horizontal \
	-command ".macros.text xview "
# Create macro text
text .macros.text \
	-xscrollcommand "doset .macros.hscroll { -after .macros.text -side bottom -fill x } " \
	-yscrollcommand "doset .macros.vscroll { -before .macros.text -side right -fill y } " \
	-wrap char -height 2 -width 10
pack .macros.text -fill both -expand 1
.macros.text configure -state disabled

bind . "<Alt-x>" "do_exit"
bind . "<Alt-e>" {execsgrep $sgrepexpr}
wm protocol . WM_DELETE_WINDOW "do_exit"

apply_preferences

fetch_macros

# The bitmap
#image create bitmap icon_bmp -file sgtool.xbm
#wm iconbitmap . @./sgtool.xbm

# I don't like the main window resizing itself
update
pack propagate . 0
$mlist configure -height 2

















