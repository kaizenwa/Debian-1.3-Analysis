# =============================================================================
#
# File:		action.tcl
# Project:	TkDesk
#
# Started:	14.10.94
# Changed:	17.10.94
# Author:	cb
#
# Description:	Implements procs for opening & executing files and for popups.
#
# Copyright (C) 1996  Christian Bolik
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
# See the file "COPYING" in the base directory of this distribution
# for more.
#
# -----------------------------------------------------------------------------
#
# Sections:
#	proc dsk_open {viewer file}
#	proc dsk_openall {{files ""}}
#	proc dsk_exec {args}
#	proc dsk_ask_exec {}
#	proc dsk_ask_dir {}
#	proc dsk_cd {dir}
#	proc dsk_open_dir {dir}
#	proc dsk_popup {lbox file mx my}
#	proc dsk_build_popup {poplist file lbox mx my}
#	proc _expand_pc {cmd {file ""}}
#	itcl_class dsk_Periodic
#	proc dsk_periodic {}
#	proc dsk_jobs {}
#	proc dsk_jobs_fill {}
#	proc dsk_jobs_sig {signal}
#       proc dsk_edit {args}
#       proc dsk_view {args}
#
# =============================================================================

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_open
# Args:		viewer		name of calling file viewer window
#		file		name of file
# Returns: 	
# Desc:		Performs the default action for $file. Gets called after
#		a double click in a file listbox. Or from the Bookmarks
#               menu.
# Side-FX:	none
#

set dsk_open(lastcmd) ""
set tkdesk(dont_add_cmd_to_history) 0

proc dsk_open {viewer file} {
    global tkdesk cmd dsk_open env

    if ![winfo exists $viewer] {
	set viewer ""
    }
    
    if ![file exists $file] {
	if {$viewer != ""} {
	    $viewer config -dir [$viewer curdir]
	}
	return
    }

    if ![file readable $file] {
	dsk_errbell
	cb_error "Permission denied."
	return
    }
    
    #set file [subst -nocommands -novariables [_make_fname_safe $file]]
    #set file [_make_fname_safe $file]

    if [file isdirectory $file] {
	# DIRECTORY

	if ![file executable $file] {
	    dsk_errbell
	    cb_error "Permission denied (directory not executable)."
	    return
	}
	
	set file [subst -nocommands -novariables [_make_fname_safe $file]]
	set dname [file tail $file]
	if {$dname == "."} {
	    set file [file dirname $file]
	} elseif {$dname == ".."} {
	    set file [file dirname $file]
	    set file [file dirname $file]
	}

	if $tkdesk(file_lb,control) {
	    if $tkdesk(in_browser) {
		dsk_FileViewer .fv[dsk_FileViewer :: id] \
			-dir "$file" -num_lbs $tkdesk(num_lbs)
	    } else {
		dsk_FileList .dfl[dsk_FileList :: id] -dir "$file"
	    }
	    set tkdesk(file_lb,control) 0
	} elseif {$viewer != ""} {
	    #$viewer config -dir "$file"
	    set action [dsk_default_action directories $file]
	    if {$action != ""} {
		eval [_expand_pc $action $file]
	    }
	}

    } elseif [file_executable $file] {
	# EXECUTABLE

	if !$tkdesk(file_lb,control) {
	    dsk_busy
	    if {[file extension $file] != ""} {
		set action [dsk_default_action regulars $file pat]
		set isf 1
	    } else {
		set action [dsk_default_action executables $file]
		set isf 0
	    }
	    if {$action != ""} {
		if {$isf} {
		    dsk_history_file $file
		}
		cd [dsk_active dir]
		eval [_expand_pc $action $file]
		cd ~
	    }
	    dsk_lazy
	} else {
	    set cmd [file tail $file]
	    dsk_ask_exec $cmd
	    set tkdesk(file_lb,control) 0
	}

    } else {
	# REGULAR FILE

	if $tkdesk(file_lb,control) {
	    #set ft [file tail $file]
	    set ft [_make_fname_safe $file]
	    if {[llength $ft] == 1} {
		set cmd "$dsk_open(lastcmd) $ft"
	    } else {
		set cmd "$dsk_open(lastcmd) \"$ft\""
	    }
	    set cmd [dsk_ask_exec $cmd]
	    if {$cmd != ""} {
		set dsk_open(lastcmd) [string range $cmd 0 \
			[expr [string last " " $cmd] - 1]]
		if {[set og [string first \" $dsk_open(lastcmd)]] > -1} {
		    set dsk_open(lastcmd) [string range $dsk_open(lastcmd) \
			    0 [expr $og - 2]]
		    
		}
	    }
	    set tkdesk(file_lb,control) 0
	} else {
	    dsk_busy
	    set action [dsk_default_action regulars $file]
	    
	    if {$action != "" && $action != "-"} {
		# add file to file history
		dsk_history_file $file
		
		cd [dsk_active dir]
		eval [_expand_pc $action $file]
		cd ~
	    }
	    dsk_lazy
	}

    }
}

# ----------------------------------------------------------------------------
# dsk_default_action popuplist file ?matchedvar?
# Returns the default action for $file in list $tkdesk(popup,$popuplist).
# If matchedvar is specified it's the name of a variable that will be
# set to the matching pattern.
#

proc dsk_default_action {popuplist file {matchedvar ""}} {
    global tkdesk
    
    if ![info exists tkdesk(popup,$popuplist)] return
    if {$matchedvar != ""} {
	upvar 1 $matchedvar mpat
    }

    set action_found 0
    set action ""
    set fname [file tail $file]
    foreach entry $tkdesk(popup,$popuplist) {
	set patlist [lindex $entry 0]

	foreach pat $patlist {
	    if [string match $pat $fname] {
		set elist [lindex $entry 1]
		set action [lindex [lindex $elist 0] 1]
		set action_found 1
		set mpat $pat
		break
	    }
	}

	if $action_found  break
    }

    return $action
}

proc dsk_history_file {file} {
    global tkdesk env
    
    set tkdesk(dont_add_cmd_to_history) 1
    if {[string first $env(HOME) $file] == 0} {
	file_history add [string_replace $file $env(HOME) ~]
    } else {
	file_history add $file
    }
    set tkdesk(dont_add_cmd_to_history) 0
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_openall
# Args:		files		(opt.) list of files
# Returns: 	""
# Desc:		Opens all selected files (or $files). "Open" means it
#		performs the default action for each file.
# Side-FX:	
#

proc dsk_openall {args} {
    global tkdesk

    set files $args
    if {$files == ""} {
    	set files [dsk_active sel]
    }

    if {$files == ""} {
	dsk_bell
	cb_info "Please select one or more files first."
	return
    }

    dsk_busy
    foreach file $files {
	dsk_open [dsk_active viewer] $file
    }
    dsk_lazy

    return
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_exec
# Args:		args		command line to execute
# Returns: 	pid or ""
# Desc:		Executes command $args in the background.
#               If $args matches dsk_*, $args is evaluated.
# Side-FX:	none
#

set tkdesk(dsk_exec,pids) ""
set tkdesk(dsk_exec,cmds) ""
set dsk_exec(bgcnt) 0
set dsk_exec(shell) 0

proc dsk_exec {args} {
    global tkdesk env dsk_exec

    if {$args == ""} {
	return
    }

    set cmd $args
    set p [lindex $cmd 0]
    if [string match dsk_* $p] {
	if {[info procs $p] != ""} {
	    set err [catch {eval $cmd} errmsg]
	    if $err {
		dsk_errbell
		cb_error "Malformed internal command.\n($errmsg)"
	    }
	    return
	} else {
	    dsk_errbell
	    cb_alert "Not an internal command: $p"
	    return
	}
    } else {
	if ![dsk_auto_execok $p] {
	    dsk_errbell
	    cb_alert "Can't execute: $p"
	    return
	}
    }

    dsk_busy
    if !$tkdesk(dont_add_cmd_to_history) {
	if {[string first $env(HOME) $cmd] == 0} {
	    exec_history add [string_replace $cmd $env(HOME) ~]
	} else {
	    exec_history add $cmd
	}
    }
    set cnt [incr dsk_exec(bgcnt)]
    dsk_sound dsk_exec_launch
    if [file_executable $cmd] {
	# then dsk_exec is called from dsk_open

    	cd [file dirname $cmd]
    	set err [catch {set pid \
		[blt_bgexec dsk_exec(bgvar,$cnt) \
		$cmd >@stdout </dev/null &]} errmsg]
    	cd ~
	if $err {
	    dsk_lazy
	    dsk_errbell
	    cb_error $errmsg
	    return 0
	}
    } else {
	set shell_args "</dev/null"
	set cmd ""

	foreach token $args {
	    if [string match {<*} $token] {
		lappend shell_args $token
	    } elseif [string match {>*} $token] {
		lappend shell_args $token
	    } elseif [string match {2>*} $token] {
		lappend shell_args $token
	    } elseif [string match {&} $token] {
		continue
	    } else {
		lappend cmd $token
	    }
	}

	set cmd [string_replace $cmd \{ \"]
	set cmd [string_replace $cmd \} \"]
	if {[string first " ~" $cmd] > -1} {
	    set cmd [string_replace $cmd " ~" " $env(HOME)"]
	}
	if [info exists dsk_exec(dir)] {
	    set dsk_exec(bgdir,$cnt) $dsk_exec(dir)
	    cd $dsk_exec(dir)
	    unset dsk_exec(dir)
	} else {
	    cd [dsk_active dir]
	}
	dsk_debug "Executing: sh -c \"exec $cmd\" $shell_args &"
	set err 0
	if $dsk_exec(shell) {
	    set dsk_exec(shell) 0
	    set err [catch {set pid [eval blt_bgexec dsk_exec(bgvar,$cnt) \
		    sh -c [list "exec $cmd"] >@stdout $shell_args &]} errmsg]
	} else {
	    if !$tkdesk(in_development) {
		set err [catch {set pid [eval blt_bgexec dsk_exec(bgvar,$cnt) \
			$cmd >@stdout $shell_args &]} errmsg]
	    } else {
		set pid [eval blt_bgexec dsk_exec(bgvar,$cnt) \
			$cmd >@stdout $shell_args &]
	    }
	}
	cd ~
	if {$err && !$tkdesk(in_development)} {
	    dsk_errbell
	    dsk_lazy
	    cb_error $errmsg
	    return
	}
	#set pid [exec sh -c \"exec $cmd\" $shell_args &]
    }
    set dsk_exec(bgcmd,$cnt) $cmd
    trace variable dsk_exec(bgvar,$cnt) w dsk_exec_trace
    dsk_status "Launched:  $cmd"
    dsk_lazy

    lappend tkdesk(dsk_exec,pids) $pid
    if {[llength $args] > 1} {
    	lappend tkdesk(dsk_exec,cmds) "$args"
    } else {
    	lappend tkdesk(dsk_exec,cmds) $args
    }

    if [winfo exists .dsk_jobs] {
	dsk_jobs_fill
    }

    return $pid
}

# this proc will be invoked when a dsk_exec'ed command exits:
proc dsk_exec_trace {arr idx op} {
    global dsk_exec

    set num [lindex [split $idx ,] 1]
    set cmd $dsk_exec(bgcmd,$num)
    set exit_code [lindex $dsk_exec(bgvar,$num) 2]
    unset dsk_exec(bgvar,$num)
    unset dsk_exec(bgcmd,$num)

    dsk_sound dsk_exec_exit
    dsk_status "Exit ($exit_code):  $cmd"

    if [info exists dsk_exec(bgdir,$num)] {
	dsk_refresh $dsk_exec(bgdir,$num)
	unset dsk_exec(bgdir,$num)
    }
}

# -----------------------------------------------------------------------------
# dsk_path_exec path args:
# Execute $args in $path.
#

proc dsk_path_exec {path args} {
    global dsk_exec

    set dsk_exec(dir) $path
    dsk_exec $args
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_ask_exec
# Args:		none
# Returns: 	pid
# Desc:		Asks the user which command to execute.
# Side-FX:	none
#

if ![info exists tkdesk(geometry,dsk_ask_exec)] {
    set tkdesk(geometry,dsk_ask_exec) ""
}
if ![info exists tkdesk(cmd_history)] {
    set tkdesk(cmd_history) ""
}

proc dsk_ask_exec {{cmd ""}} {
    global tkdesk dsk_ask_exec

    set t .dsk_ask_exec
    catch {destroy $t}

    toplevel $t
    wm withdraw $t

    frame $t.f -bd 1 -relief raised
    pack $t.f -fill both -expand yes

    frame $t.fl
    pack $t.fl -in $t.f -fill x

    label $t.label -text "Command to execute:"
    pack $t.label -in $t.fl -side left -padx $tkdesk(pad) -pady $tkdesk(pad)

    set dsk_ask_exec(view) 0
    checkbutton $t.cbView -text "View Output" -variable dsk_ask_exec(view)
    pack $t.cbView -in $t.fl -side right \
	    -padx $tkdesk(pad) -pady $tkdesk(pad)

    frame $t.fe
    pack $t.fe -in $t.f -fill both -expand yes

    entry $t.entry -width 40 -bd 2 -relief sunken
    pack $t.entry -in $t.fe -fill x -expand yes -side left \
		-padx $tkdesk(pad) -pady $tkdesk(pad) -ipady 2
    bind $t.entry <Return> "$t.bOK.button flash ; $t.bOK.button invoke"
    bind $t.entry <Escape> "$t.bCancel.button flash ; $t.bCancel.button invoke"
    bind $t.entry <Control-c> "$t.bCancel.button flash
				$t.bCancel.button invoke"
    cb_bindForCompletion $t.entry <Control-Tab>
    if {$cmd != ""} {
	$t.entry insert end $cmd
	$t.entry icursor 0
    }

    blt_drag&drop target $t.entry handler text "dd_handle_text $t.entry"

    menubutton $t.mbHist -bd 2 -relief raised \
		-bitmap @$tkdesk(library)/cb_tools/bitmaps/combo.xbm \
		-menu $t.mbHist.menu
    pack $t.mbHist -in $t.fe -side right \
		-padx $tkdesk(pad) -pady $tkdesk(pad) -ipadx 2 -ipady 2

    menu $t.mbHist.menu \
		-postcommand "cmd_history buildmenu $t.mbHist.menu"
    # add dummy entry to work around bug in pre Tk 4.0p2:
    $t.mbHist.menu add command -label "dummy"
    cmd_history changed

    frame $t.fb -bd 1 -relief raised
    pack $t.fb -fill x

    set dsk_ask_exec(cmd) ""
    cb_button $t.bOK -text "   OK   " -default 1 -command {
		set tmpcmd [string trimright [.dsk_ask_exec.entry get] &]
	        set dsk_ask_exec(cmd) $tmpcmd
		if {$tmpcmd == ""} {
		    dsk_bell
		    unset tmpcmd
		    break
		}
		.dsk_ask_exec.bCancel.button invoke
		cmd_history add $tmpcmd
		cd [dsk_active dir]
		if [file isdirectory $tmpcmd] {
		    dsk_open_dir $tmpcmd
		} elseif {[file_executable $tmpcmd] || \
			[dsk_auto_execok [lindex $tmpcmd 0]]} {
		    if $dsk_ask_exec(view) {
			eval dsk_view $tmpcmd
		    } else {
			eval dsk_exec $tmpcmd
		    }
		} elseif [file readable $tmpcmd] {
		    dsk_open "" $tmpcmd
		} else {
		    dsk_errbell
		    cb_error "Couldn't open/execute $tmpcmd."
		}
		cd ~
	}
    cb_button $t.bCancel -text " Cancel " -command {
		set tkdesk(geometry,dsk_ask_exec) [wm geometry .dsk_ask_exec]
		destroy .dsk_ask_exec }

    pack $t.bOK $t.bCancel -in $t.fb -side left \
		-padx $tkdesk(pad) -pady $tkdesk(pad)

    bind $t <Any-Enter> "focus $t.entry"

    wm minsize $t 402 118
    wm title $t "Execute"
    wm protocol $t WM_DELETE_WINDOW {
	set tkdesk(geometry,dsk_ask_exec) [wm geometry .dsk_ask_exec]
	destroy .dsk_ask_exec
    }

    dsk_place_window $t dsk_ask_exec "" 0 1
    wm deiconify $t

    grab $t
    set old_focus [focus]
    focus -force $t.entry
    tkwait window $t
    focus -force $old_focus
    return $dsk_ask_exec(cmd)
}

proc dsk_ask_exec_cb {t cmd} {
    $t.entry delete 0 end
    $t.entry insert end $cmd
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_ask_dir
# Args:		none
# Returns: 	""
# Desc:		Asks the user which directory to open.
# Side-FX:	none
#

if ![info exists tkdesk(geometry,dsk_ask_dir)] {
    set tkdesk(geometry,dsk_ask_dir) ""
}

proc dsk_ask_dir {{browser ""}} {
    global tkdesk dsk_ask_dir

    set t .dsk_ask_dir
    if [winfo exists $t] {
	cb_raise $t
	return
    }

    toplevel $t
    wm withdraw $t

    frame $t.f -bd 1 -relief raised
    pack $t.f -fill both -expand yes

    frame $t.fl
    pack $t.fl -in $t.f -fill x

    label $t.label -text "Directory to open:"
    pack $t.label -in $t.fl -side left -padx $tkdesk(pad) -pady $tkdesk(pad)

    set dsk_ask_dir(browser) [expr [string match "browser" $browser] || \
	    $tkdesk(in_browser)]
    checkbutton $t.cbBrowser -text "In Browser" -variable dsk_ask_dir(browser)
    pack $t.cbBrowser -in $t.fl -side right \
	    -padx $tkdesk(pad) -pady $tkdesk(pad)

    frame $t.fe
    pack $t.fe -in $t.f -fill both -expand yes

    entry $t.entry -width 40 -bd 2 -relief sunken
    pack $t.entry -in $t.fe -fill x -expand yes -side left \
		-padx $tkdesk(pad) -pady $tkdesk(pad) -ipady 2
    bind $t.entry <Return> "$t.bOK.button flash ; $t.bOK.button invoke"
    bind $t.entry <Escape> "$t.bCancel.button flash ; $t.bCancel.button invoke"
    bind $t.entry <Control-c> "$t.bCancel.button flash
				$t.bCancel.button invoke"
    cb_bindForCompletion $t.entry <Control-Tab>

    blt_drag&drop target $t.entry handler text "dd_handle_text $t.entry"

    menubutton $t.mbHist -bd 2 -relief raised \
		-bitmap @$tkdesk(library)/cb_tools/bitmaps/combo.xbm \
		-menu $t.mbHist.menu
    pack $t.mbHist -in $t.fe -side right \
		-padx $tkdesk(pad) -pady $tkdesk(pad) -ipadx 2 -ipady 2

    menu $t.mbHist.menu -postcommand "dsk_ask_dir_hmenu"
    # add dummy entry to work around bug in pre Tk 4.0p2:
    $t.mbHist.menu add command -label "dummy"
    history changed

    frame $t.fb -bd 1 -relief raised
    pack $t.fb -fill x

    cb_button $t.bOK -text "   OK   " -default 1 -command \
	    "dsk_ask_dir_ok"
    cb_button $t.bCancel -text " Cancel " -command {
		set tkdesk(geometry,dsk_ask_dir) [wm geometry .dsk_ask_dir]
		destroy .dsk_ask_dir
    }

    pack $t.bOK $t.bCancel -in $t.fb -side left \
	    -padx $tkdesk(pad) -pady $tkdesk(pad)

    bind $t <Any-Enter> "focus $t.entry"

    wm minsize $t 326 117
    wm title $t "Open Directory"
    wm protocol $t WM_DELETE_WINDOW {.dsk_ask_dir.bCancel.button invoke}

    dsk_place_window $t dsk_ask_dir "" 0 1
    wm deiconify $t

    grab $t
    set old_focus [focus]
    focus -force $t.entry
    tkwait window $t
    focus -force $old_focus
}

proc dsk_ask_dir_ok {} {
    global tkdesk dsk_ask_dir
    
    set tmpdir [.dsk_ask_dir.entry get]
    .dsk_ask_dir.bCancel.button invoke
    update idletasks
    
    if {$tmpdir != ""} {
	if ![file exists $tmpdir] {
	    dsk_bell
	    cb_alert "The path you specified is not completely valid."
	    set tmpdir [_make_path_valid $tmpdir]
	}
	if [file readable $tmpdir] {
	    dsk_busy
	    if $dsk_ask_dir(browser) {
		dsk_FileViewer .fv[dsk_FileViewer :: id] \
			-dir $tmpdir -num_lbs $tkdesk(num_lbs)
	    } else {
		dsk_FileList .dfl[dsk_FileList :: id] -dir $tmpdir
	    }
	    dsk_lazy
	} else {
	    dsk_errbell
	    cb_error "Permission denied."
	}
    }
}

proc dsk_ask_dir_hmenu {} {
    global tkdesk

    set t .dsk_ask_dir
    catch "$t.mbHist.menu delete 0 last"
    if $tkdesk(sort_history) {
	set l [lsort [history get]]
    } else {
	set l [history get]
    }
    foreach dir $l {
	$t.mbHist.menu add command -label $dir \
			-command "$t.entry delete 0 end
				$t.entry insert end $dir" \
			-font $tkdesk(font,entries)
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_cd
# Args:		dir	name of directory
# Returns: 	""
# Desc:		Display directory $dir in the active file viewer.
# Side-FX:	none
#

proc dsk_cd {{dir ""}} {
    global tkdesk env

    if {$dir == ""} return

    if {$dir == ".."} {
	set ad [dsk_active dir]
	if {$ad == "/"} {
	    dsk_bell
	    return
	}
	set dir [file dirname [string trimright $ad /]]
    } elseif ![file isdirectory $dir] {
	set dir [file dirname $dir]
    }

    if {$dir != "" && [winfo exists $tkdesk(active_viewer)]} {
	if ![file isdirectory $dir] {
	    set dir [_make_path_valid $dir]
	    catch {dsk_bell}
	    cb_alert "The path you specified is not completely valid."
	}
    	$tkdesk(active_viewer) config -directory $dir
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_open_dir
# Args:		dir		directory to open
# Returns: 	""
# Desc:		Opens a window for directory $dir
# Side-FX:	none
#

proc dsk_open_dir {dir} {
    global tkdesk

    if [file readable $dir] {
	if $tkdesk(in_browser) {
	    dsk_FileViewer .fv[dsk_FileViewer :: id] \
		    -directory "$dir" -num_lbs $tkdesk(num_lbs)
	} else {
	    dsk_FileList .dfl[dsk_FileList :: id] -directory "$dir"
	}
    } else {
	dsk_errbell
	cb_error "Permission denied."
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_open_browser
# Args:		dir		directory to open
# Returns: 	""
# Desc:		Opens a browser window for directory $dir
# Side-FX:	none
#

proc dsk_open_browser {dir} {
    global tkdesk
    
    if [file readable $dir] {
	dsk_FileViewer .fv[dsk_FileViewer :: id] -dir $dir \
		-num_lbs $tkdesk(num_lbs)
    } else {
	dsk_errbell
	cb_error "Permission denied."
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_popup
# Args:		lbox		name of calling listbox
#		file		name of file
#		mx		mouse x position on root window
#		my		mouse y position on root window
#               opt             additional options
# Returns: 	
# Desc:		Shows the appropriate popup menu for $file.
# Side-FX:	none
#

proc dsk_popup {lbox file mx my {opt ""}} {
    global tkdesk

    if $tkdesk(append_type_char) {
	set file [dsk_striptc $file]
    }
    #dsk_debug "dsk_popup: $file, $viewer, x: $mx, y: $my"

    if [file isdirectory $file] {
	# DIRECTORY
	dsk_build_popup tkdesk(popup,directories) $file $lbox $mx $my $opt
    } elseif [file_executable $file] {
	# EXECUTABLE
	if {[file extension $file] != ""} {
	    dsk_build_popup tkdesk(popup,regulars) $file $lbox $mx $my $opt
	} else {
	    dsk_build_popup tkdesk(popup,executables) $file $lbox $mx $my $opt
	}
    } else {
	# REGULAR (and others)
	dsk_build_popup tkdesk(popup,regulars) $file $lbox $mx $my $opt
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_build_popup
# Args:		poplist - list of popup entries
#		file - path and name of file
#		lbox - name of listbox where mousebutton was pressed
#		mx		mouse x position on root window
#		my		mouse y position on root window
#               opt             additional options
# Returns: 	""
# Desc:		builds the appropriate popup menu and brings it on-screen
# Side-FX:	none
#

proc dsk_build_popup {poplist file lbox mx my {opt ""}} {
    global tkdesk

    if ![info exists $poplist] {
	dsk_debug "$poplist does not exist!"
    }

    set fname [file tail $file]
    set dirname [file dirname $file]
    set sfile [_make_fname_safe $file]

    catch "destroy .dsk_popup"
    foreach entry [set $poplist] {
	#
	# the first element of each entry is a list of glob patterns
	#
	set patlist [lindex $entry 0]
	foreach pat $patlist {
	    if [string match $pat $fname] {
		menu .dsk_popup
		if [file isdirectory $file] {
		    set ffnt $tkdesk(font,directories)
		    set ffg $tkdesk(color,directories)
		} elseif [file_executable $file] {
		    set ffnt $tkdesk(font,executables)
		    set ffg $tkdesk(color,executables)
		} else {
		    set ffnt $tkdesk(font,file_lbs)
		    set ffg black
		}
		#.dsk_popup add command -label "$fname " \
			#	 -command "catch {destroy .dsk_popup} ;\
			#	     dsk_fileinfo [_make_fname_safe $file]" \
			#	 -font $ffnt -foreground $ffg \
			#        -activeforeground $ffg
		.dsk_popup add cascade -label "$fname " -menu .dsk_popup.fm \
			-font $ffnt -foreground $ffg -activeforeground $ffg
		menu [set m .dsk_popup.fm]
		$m add command -label "Info " -command \
			"catch {destroy .dsk_popup} ;\
			dsk_fileinfo \"$sfile\""
		$m add command -label "Bookmark " -command \
			"catch {destroy .dsk_popup} ;\
			dsk_bookmark add \"$sfile\""
		if {$poplist != "tkdesk(popup,directories)"} {
		    $m add command -label "Open with... " -command \
			    "catch {destroy .dsk_popup} ;\
			    set tkdesk(file_lb,control) 1 ;\
			    dsk_open $tkdesk(active_viewer) \"$sfile\""
		}
		$m add separator
		$m add command -label "Copy, Move, ... " -command \
			"catch {destroy .dsk_popup} ;\
			dsk_copy \"$sfile\""
		$m add command -label "Rename... " -command \
			"catch {destroy .dsk_popup} ;\
			dsk_rename \"$sfile\""
		$m add command -label "Delete " -command \
			"catch {destroy .dsk_popup} ;\
			dsk_delete \"$sfile\""

		
		if {$poplist == "tkdesk(popup,directories)"} {
		    .dsk_popup add cascade -label "Traverse" \
			    -menu [set m .dsk_popup.mc]
		    menu $m -postcommand "dsk_casdirs [_make_fname_safe $file] $m 1"
		    $m add command -label "dummy"
		}

		if {$opt != ""} {
		    switch -glob $opt {
			"dir *" {
			    .dsk_popup add command -label "Open Directory " \
				    -command "catch {destroy .dsk_popup}
			          dsk_open_dir [file dirname $file]"
			}
			"deskitem*" {
			    .dsk_popup add command -label "Remove Item " \
				    -command "catch \{[lindex $opt 1] delete\}"
			}
		    }
		}

		.dsk_popup add separator

		set num_entries 2
		set max_chars [expr [string length $fname] + 2]
		set menu_entries [lindex $entry 1]
		foreach me $menu_entries {
		    if {$poplist == "tkdesk(popup,executables)"} {
			#
			# Skip the "Edit" entry if file is not a script:
			#
			if [string match "Edit" [lindex $me 0]] {
			    set sig ""
			    set err [catch {set fd [open $file]}]
			    set err [catch {set sig [read $fd 2]}]
			    set err [catch {close $fd}]
			    if {($sig != "#!") || $err} {
				continue
			    }
			}
		    }

		    if {$me != "-"} {
			set l [lindex $me 0]
			set ll [string length $l]
			if {$ll > $max_chars} {
			    set max_chars $ll
			}
			.dsk_popup add command -label $l \
				-command "catch {destroy .dsk_popup} ;\
				dsk_history_file \"$sfile\" ;\
				cd \[dsk_active dir\] ;\
			[_expand_pc [dsk_esc [lindex $me 1] \$] $file] ;\
				cd ~"
		    } else {
			.dsk_popup add separator
		    }
		    incr num_entries
		}

		break
	    }
	}
	if [winfo exists .dsk_popup] {
	    break
	}
    }

    if [winfo exists .dsk_popup] {
	cd $dirname
	#
	# Post the popup
	#
	#cb_MenuPopupAdd $lbox 3 .dsk_popup "" "" 1 $mx $my
	update
	catch {tk_popup .dsk_popup $mx $my}
	catch {tkwait window .dsk_popup}
	update idletasks
	cd ~
    }

    return ""
}

#
# -----------------------------------------------------------------------------
#
# Proc:		_expand_pc
# Args:		cmd	command line with %?
#		file	(opt.) name of selected file (with path)
# Returns: 	cmd with %? expanded
# Desc:		Expands all %? shortcuts in the given command line.
#		Does also some preprocessing of the command.
# Side-FX:	none
#

proc _expand_pc {cmd {file ""}} {
    global tkdesk

    dsk_debug -nonewline "Expanding \"$cmd\" to "

    if {$file == ""} {
	set files [_make_fnames_safe]
	set file [lindex $files 0]
    } else {
	set file [_make_fname_safe $file]
	set files $file
    }

    if {$file == ""} {
	if {[string first "%A" $cmd] > -1} {
	    dsk_bell
	    cb_info "Please select one or more files first."
	    return ""
	} elseif {[string first "%" $cmd] > -1} {
	    if {[string first "%B" $cmd] == -1 && \
		[string first "%D" $cmd] == -1 && \
		[string first "%x" $cmd] == -1} {
	    	cb_info "Please select a file first."
	    	return ""
	    }
	}
    }

    if {[string first "%x" $cmd] > -1} {
	set err [catch {set xsel [selection get]}]
	if $err {
	    cb_info "The X-selection is empty."
	    return ""
	}
    }

    if {[string first % $file] > -1} {
	# temporarily replace % in the filename:
	set file [string_replace $file "%" "_!percent!_"]
	set percent_in_file 1
    } else {
	set percent_in_file 0
    }
    if {[string first % $files] > -1} {
	# temporarily replace % in the filenames:
	set files [string_replace $files "%" "_!percent!_"]
	set percent_in_files 1
    } else {
	set percent_in_files 0
    }

    # (I disabled the following cause it crashes with auto-loading.)
    # Enabled it again because TkDesk no longer uses auto-loading.
    if {[info commands [lindex $cmd 0]] == ""} {
	if {[info procs [lindex $cmd 0]] == ""} {
	    # use "dsk_exec" as the default command
	    set cmd "dsk_exec $cmd"
	}
    }

    set dir [string trimright [dsk_active dir] /]
    set ocmd $cmd
    set pcmd ""
    foreach cmd [split $ocmd \n] {
	if {[string first "%s" $cmd] > -1} {
	    set cmd [string_replace $cmd %s [list $file]]
	}
	if {[string first "%d" $cmd] > -1} {
	    set cmd [string_replace $cmd %d [list [file dirname $file]]]
	}
	if {[string first "%f" $cmd] > -1} {
	    set cmd [string_replace $cmd %f [list [file tail $file]]]
	}
	if {[string first "%b" $cmd] > -1} {
	    set cmd [string_replace $cmd %b [list [string range $file 0 \
		    [expr [string last "." $file] - 1 ]]]]
	}
 	if {[string first "%c" $cmd] > -1} {
 	    set cmd [string_replace $cmd %c [list [file tail [list [string \
		    range $file 0 [expr [string last "." $file] - 1 ]]]]]]
 	}
	if {[string first "%A" $cmd] > -1} {
	    #set files [string trimleft $files \{]
	    #set files [string trimright $files \}]
	    #set cmd [string_replace $cmd %A \
	    #	     [dsk_esc [split $files] {\"[]{}$;}]]
	    set cmd [string_replace $cmd %A $files]
	}
	if {[string first "%B" $cmd] > -1} {
	    if {$files != ""} {
		set cmd [string_replace $cmd %B [split $files]]
	    } else {
		set cmd [string_replace $cmd %B ""]
	    }
	}
	if {[string first "%D" $cmd] > -1} {
	    set cmd [string_replace $cmd %D [list $dir]]
	}
	if {[string first "%x" $cmd] > -1} {
	    set cmd [string_replace $cmd %x [string trimright $xsel \;]]
	}

	if {$percent_in_file || $percent_in_files} {
	    set cmd [string_replace $cmd "_!percent!_" "%"]
	}

	append pcmd "$cmd\n"
    }

    dsk_debug "\"$pcmd\""
    return [subst -nocommands -novariables $pcmd]
}

#
# -----------------------------------------------------------------------------
#
# Proc:		_make_fnames_safe
# Args:		none
# Returns: 	Names of currently selected files with all Tcl-special chars
#               ([ etc.) backslashed. Calls _make_fname_safe.
# Desc:		...
# Side-FX:	none
#

proc _make_fnames_safe {{flist ""}} {
    global tkdesk

    if {$flist == ""} {
	set flist [dsk_active sel]
    }
    if {$flist == ""} {return ""}

    foreach file $flist {
	#if {[string first "\\" $file] > -1} continue

	set nfile [_make_fname_safe $file]
	if {$nfile != ""} {
	    lappend rlist $nfile
	}
    }

    #puts $rlist
    #if {[llength $rlist] == 1} {
    #	 return [lindex $rlist 0]
    #} else {
    #	 return $rlist
    #}
    return $rlist
}

# -----------------------------------------------------------------------------
#
# Proc:		_make_fname_safe
# Args:		file - filename
# Returns: 	filename with all Tcl-special chars ([ etc.) backslashed
# Desc:		...
# Side-FX:	none
#

proc _make_fname_safe {{file ""}} {

    if {$file == ""} {return ""}

    set nfile [dsk_esc $file { \"[]{}$;}]
    #set nfile [string_replace $file \[ \\\[]
    #set nfile [string_replace $nfile \] \\\]]
    #set nfile [string_replace $nfile \{ \\\{]
    #set nfile [string_replace $nfile \} \\\}]
    #set nfile [string_replace $nfile \$ \\\$]
    #set nfile [string_replace $nfile \" \\\"]
    #set nfile [string_replace $nfile " " "\\ "]
    #set nfile [string_replace $nfile "&" "\\&"]
    #set nfile [string_replace $nfile "(" "\\("]
    #set nfile [string_replace $nfile ")" "\\)"]

    #puts $nfile
    return $nfile
}



#
# =============================================================================
#
# Class:	dsk_Periodic
# Desc:		Implements a window that periodically executes a shell
#		command and collects its output in a text widget.
#
# Methods:	
# Procs:	
# Publics:
#

itcl_class dsk_Periodic {

    constructor {args} {
	global tkdesk [set this]

	#
	# Create a toplevel with this object's name
	# (later accessible as $this-top):
	#
        set class [$this info class]
        ::rename $this $this-tmp-
        ::toplevel $this -class $class
	wm withdraw $this
        ::rename $this $this-top
        ::rename $this-tmp- $this

	frame $this.fe -bd 1 -relief raised
	pack $this.fe -fill x

	label $this.lCmd -text "Command:"
	entry $this.eCmd -bd 2 -relief sunken -width 10
	bind $this.eCmd <Return> "$this.bExec.button flash
				$this.bExec.button invoke"
	bind $this.eCmd <Tab> "focus $this.eSec"
    	menubutton $this.mbHist -bd 2 -relief raised \
		-bitmap @$tkdesk(library)/cb_tools/bitmaps/combo.xbm \
		-menu $this.mbHist.menu
    	menu $this.mbHist.menu \
		-postcommand "pcmd_history buildmenu $this.mbHist.menu"
	# add dummy entry to work around bug in pre Tk 4.0p2:
	$this.mbHist.menu add command -label "dummy"
	pcmd_history changed

	frame $this.fSep -width 16
	label $this.lu1 -text "Exec every"
	entry $this.eSec -bd 2 -relief sunken -width 4
	bind $this.eSec <Return> "$this.bExec.button flash
				$this.bExec.button invoke"
	bind $this.eSec <Tab> "focus $this.eCmd"
	$this.eSec insert end $period
	label $this.lu2 -text "seconds"

	pack $this.lCmd $this.eCmd $this.mbHist $this.fSep $this.lu1 \
		$this.eSec $this.lu2 \
		-in $this.fe -side left -ipady 2 \
		-padx $tkdesk(pad) -pady $tkdesk(pad)
	pack configure $this.eCmd -fill x -expand yes
	pack configure $this.mbHist  -ipadx 2 -ipady 2

	frame $this.ft -bd 1 -relief raised
	pack $this.ft -fill both -expand yes

	cb_text $this.ftext -vscroll 1 -lborder 1 -wrap none \
		-pad $tkdesk(pad) -width 20 -height 2 -state disabled
	pack $this.ftext -in $this.ft -fill both -expand yes \
		-pady $tkdesk(pad)

	frame $this.fb -bd 1 -relief raised
	pack $this.fb -fill x

	cb_button $this.bExec -text "  Exec  " -default 1 \
		-command "$this config -period \[$this.eSec get\]
				$this config -command \[$this.eCmd get\]
				focus $this
				pcmd_history add \[$this.eCmd get\]"
	cb_button $this.bClose -text "  Close  " -command "$this delete"
	pack $this.bExec $this.bClose -in $this.fb -side left \
		-padx $tkdesk(pad) -pady $tkdesk(pad)
	set [set this](dont_exec) 0
	checkbutton $this.cbExec -text "Don't execute" \
		-variable [set this](dont_exec) \
		-command "if !\[set [set this](dont_exec)\] \
		             \{$this.bExec.button invoke\}"
	pack $this.cbExec -in $this.fb -side left \
		-padx $tkdesk(pad) -pady $tkdesk(pad)

	bind $this <Any-Enter> "+focus $this"
	bind $this <Tab> "focus $this.eCmd"

    	wm minsize $this 354 124
	wm geometry $this 592x278
	wm title $this "Periodic Execution"
	wm protocol $this WM_DELETE_WINDOW "$this delete"

	eval config $args
	wm deiconify $this
    }

    destructor {
        #after 10 rename $this-top {}		;# delete this name
        catch {destroy $this}		;# destroy associated window
    }

    #
    # ----- Methods and Procs -------------------------------------------------
    #

    method config {config} {
    }

    method execute {} {
	global [set this] tkdesk
	
	if [set [set this](dont_exec)] return
	if ![winfo exists $this] return
	
	if {[grab current] == ""} {
	    dsk_busy ;# dsk_Periodic
	    set err [catch {eval blt_bgexec [set this](var) \
		    -output [set this](result) \
		    -error [set this](error) \
		    $command </dev/null &} m]
	    if $err {
		dsk_lazy dsk_Periodic
	    	set update_started 0
		dsk_errbell
		if {[string length $m] < 100} {
	    	    cb_error "Couldn't execute $command! ($m)"
		} else {
	    	    cb_error \
			"Couldn't execute $command for some strange reason..."
		}
	    	return
	    }

	    tkwait variable [set this](var)
	    set result [set [set this](result)]
	    set errout [set [set this](error)]
	    
	    set oldy [lindex [cb_old_sb_get $this.ftext.vscroll] 2]
	    $this.ftext.text config -state normal
	    $this.ftext.text delete 1.0 end
	    if {$errout == ""} {
		$this.ftext.text insert end $result
	    } else {
		$this.ftext.text insert end $errout
	    }
	    $this.ftext.text yview $oldy
	    $this.ftext.text config -state disabled
	    dsk_lazy ;# dsk_Periodic
	}
    }

    method update {} {
	$this execute

	if $period {
	    set update_started 1
	    after [expr $period * 1000] "catch \"$this update\""
	} else {
	    set update_started 0
	}
    }

    proc id {} {
	set i $id
	incr id
	return $i
    }

    #
    # ----- Variables ---------------------------------------------------------
    #

    public command "" {
	if {$command != ""} {
	    if !$update_started {
	    	$this update
	    } else {
	    	$this execute
	    }
	}
	$this.eCmd delete 0 end
	$this.eCmd insert end $command
    }

    public period 10 {
	$this.eSec delete 0 end
	$this.eSec insert end $period
    }

    protected update_started 0

    common id 0
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_periodic (~_cb is the callback for the history menu)
# Args:		none
# Returns: 	""
# Desc:		Creates a window for periodic execution of shell commands.
# Side-FX:	none (hopefully)
#

proc dsk_periodic {{cmd ""} {period 10}} {

    if {$cmd != ""} {
    	dsk_Periodic .pe[dsk_Periodic :: id] -command $cmd -period $period
    } else {
    	dsk_Periodic .pe[dsk_Periodic :: id] -period $period
    }
    return
}

proc dsk_periodic_cb {t cmd} {
    $t.eCmd delete 0 end
    $t.eCmd insert end $cmd
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_jobs
# Args:		none
# Returns: 	""
# Desc:		Displays a listing of all started background processes
#		that were started by TkDesk.
# Side-FX:	none
#

set dsk_jobs(pids) ""
if ![info exists tkdesk(geometry,dsk_jobs)] {
    set tkdesk(geometry,dsk_jobs) ""
}

proc dsk_jobs {} {
    global tkdesk dsk_jobs

    set t .dsk_jobs
    if [winfo exists $t] {
	cb_raise $t
	return
    }

    dsk_busy 

    toplevel $t
    wm withdraw $t

    frame $t.fl -bd 1 -relief raised
    pack $t.fl -fill x

    label $t.label -text "Background Jobs of TkDesk:"
    pack $t.label -in $t.fl -side left -padx $tkdesk(pad) -pady $tkdesk(pad)

    frame $t.fj
    pack $t.fj -fill both -expand yes

    cb_listbox $t.flb -vscroll 1 -hscroll 1 -lborder 1 -uborder 1 \
		-pad $tkdesk(pad) -font $tkdesk(font,file_lbs) \
		-width 20 -height 5
    $t.flb config -bd 1 -relief raised
    pack $t.flb -in $t.fj -side left -fill both -expand yes

    frame $t.fjb -bd 1 -relief raised
    pack $t.fjb -in $t.fj -side left -fill y

    button $t.bTerm -text "Terminate" -width 9 -command "dsk_jobs_sig term"
    pack $t.bTerm -in $t.fjb -padx $tkdesk(pad) -pady $tkdesk(pad) -ipady 2

    button $t.bHang -text "Hang up" -width 9 -command "dsk_jobs_sig hangup"
    pack $t.bHang -in $t.fjb -padx $tkdesk(pad) -pady $tkdesk(pad) -ipady 2

    button $t.bKill -text "Kill!" -width 9 -command "dsk_jobs_sig kill"
    pack $t.bKill -in $t.fjb -padx $tkdesk(pad) -pady $tkdesk(pad) -ipady 2

    button $t.bStop -text "Stop" -width 9 -command "dsk_jobs_sig stop"
    pack $t.bStop -in $t.fjb -padx $tkdesk(pad) -pady $tkdesk(pad) -ipady 2

    button $t.bCont -text "Continue" -width 9 -command "dsk_jobs_sig cont"
    pack $t.bCont -in $t.fjb -padx $tkdesk(pad) -pady $tkdesk(pad) -ipady 2

    frame $t.fb -bd 1 -relief raised
    pack $t.fb -fill x

    button $t.bClose -text "  Close  " -command {
		set tkdesk(geometry,dsk_jobs) [wm geometry .dsk_jobs]
		destroy .dsk_jobs }
    pack $t.bClose -in $t.fb -side left \
		-padx $tkdesk(pad) -pady $tkdesk(pad) -ipady 2

    button $t.bUpdate -text " Update " -command "dsk_jobs_fill"
    pack $t.bUpdate -in $t.fb -side left \
		-padx $tkdesk(pad) -pady $tkdesk(pad) -ipady 2

    wm protocol $t WM_DELETE_WINDOW {
	set tkdesk(geometry,dsk_jobs) [wm geometry .dsk_jobs]
	destroy .dsk_jobs
    }
    wm minsize $t 276 278
    wm title $t "Job Control"
    wm deiconify $t

    dsk_place_window $t dsk_jobs 422x278

    dsk_jobs_update
    dsk_lazy
}


proc dsk_jobs_fill {} {
    global tkdesk dsk_jobs

    #dsk_busy

    set err [catch {set pslist [split [exec ps] \n]} errmsg]
    if $err {
	dsk_errbell
	cb_error $errmsg
	return
    }
    
    set dsk_jobs(pids) ""
    set t .dsk_jobs
    set csel [$t.flb.lbox curselection]
    $t.flb.lbox delete 0 end

    set i 0
    foreach cmd $tkdesk(dsk_exec,cmds) {
	set pid [lindex $tkdesk(dsk_exec,pids) $i]
	if {[lsearch -regexp $pslist "^ *$pid +"] > -1} {
	    lappend dsk_jobs(pids) $pid
	    $t.flb.lbox insert end $cmd
	} else {
	    lreplace $tkdesk(dsk_exec,cmds) $i $i
	    lreplace $tkdesk(dsk_exec,pids) $i $i
	}
	incr i
    }
    if {$csel != ""} {
	$t.flb.lbox selection set [lindex $csel 0]
    }

    set t .dsk_jobs

    #dsk_lazy
}


proc dsk_jobs_sig {signal} {
    global dsk_jobs tkdesk

    switch $signal {
	term	{set signum 15}
	hangup	{set signum 1}
	kill	{set signum 9}
	stop	{set signum 19}
	cont	{set signum 18}
	default {cb_error "Unknown signal $signal!" ; return}
    }

    set sel [.dsk_jobs.flb.lbox curselection]
    if {$sel != ""} {
	foreach i $sel {
	    set pid [lindex $dsk_jobs(pids) $i]
	    set err [catch "exec kill -$signum $pid" m]
	    dsk_debug "exec kill -$signum $pid"
	    if {$err && $tkdesk(in_development)} {
		dsk_errbell
		cb_error "kill $signum failed! ($m)"
	    }
#	    if {$signal == "stop"} {
#		set j [lsearch $tkdesk(dsk_exec,pids) $pid]
#		if {$j > -1} {
#		    set tkdesk(dsk_exec,cmds) [lreplace \
#			$tkdesk(dsk_exec,cmds) $j $j \
#			"[lindex $tkdesk(dsk_exec,cmds) $j] (stopped)"]
#		}
#	    }
	}

	after 500 dsk_jobs_fill
    } else {
	dsk_bell
	cb_info "Please select one or more jobs first."
    }
}


proc dsk_jobs_update {} {
    global tkdesk

    if [winfo exists .dsk_jobs] {
    	dsk_jobs_fill
	after [expr $tkdesk(update,jobs) * 1000] dsk_jobs_update
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_edit
# Args:		args		a list of filenames to edit
# Returns: 	""
# Desc:		Calls the in "System" specified editor on the given files.
#		The editor will run in the background.
# Side-FX:	none
#

proc dsk_edit {args} {
    global tkdesk

    set files ""
    foreach f $args {
	if ![file isdirectory $f] {
	    lappend files $f
	}
    }

    if {$files == "{New File}"} {
	if {$tkdesk(editor,cmd) == "builtin"} {
	    set files ""
	} else {
	    set files "new_file"
	}
    } elseif {$files == ""} {
	set files [cb_fileSelector \
		-filter [string trimright [dsk_active dir] /]/* \
		-label "File to edit:" -showall 1]
	if {$files == ""} {
	    return
	}
    }
    
    if $tkdesk(editor,mfiles) {
	if {$tkdesk(editor,cmd) != "builtin"} {
	    eval dsk_exec $tkdesk(editor,cmd) $files
	} else {
	    if {$files != ""} {
		dsk_Editor .de[dsk_Editor :: id] -files $files
	    } else {
		dsk_editor new
	    }
	}
    } else {
	foreach file $files {
	    if {$tkdesk(editor,cmd) != "builtin"} {
		dsk_exec $tkdesk(editor,cmd) $file
	    } else {
		dsk_Editor .de[dsk_Editor :: id] -files $file
	    }

	}
    }

    return
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_view
# Args:		args - shell command
# Returns: 	""
# Desc:		Displays the standard output of command $args in the builtin
#               editor.
# Side-FX:	none
#

proc dsk_view {args} {

    set p [lindex $args 0]
    if ![dsk_auto_execok $p] {
	dsk_errbell
	cb_alert "Can't execute: $p"
	return
    }
    eval dsk_editor cmd $args
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_confirm
# Args:         msg - message to display in the dialog box
#               args - tcl script
# Returns: 	""
# Desc:		Evaluates $args after a positive confirmation.
# Side-FX:	none
#

proc dsk_confirm {msg script} {

    if ![cb_okcancel $msg] {
	eval $script
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_read_string
# Args:         msg - message to display in the dialog box
#               args - tcl script
# Returns: 	""
# Desc:		Evaluates $args if the entered string is != "".
# Side-FX:	none
#

proc dsk_read_string {msg script args} {
    global dsk_read_string tmpvar

    set dontpaste 0
    foreach a $args {
	switch $a {
	    "dontpaste" {
		set dontpaste 1
	    }
	}
    }

    set tmpvar ""
    if !$dontpaste {
	catch {set tmpvar [selection get]}
    }
    if {[string first " " $tmpvar] > -1} {
	set tmpvar ""
    }
    set dsk_read_string [cb_readString $msg tmpvar]
    update
    if {$dsk_read_string != ""} {
	eval $script
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_cbhelp
# Args:		file, (opt.) mode/regexp
# Returns: 	""
# Desc:		Invokes the cb_Help::show class procedure.
# Side-FX:	none
#

proc dsk_cbhelp {file {regexp ""}} {
    global tkdesk

    dsk_busy
    cb_help show $file $regexp
    dsk_lazy
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_sound
# Args:		snd: name of sound event, e.g. welcome, launch.
#               The soundfile is accessed by $tkdesk(sound,$snd).
# Returns: 	""
# Desc:		Plays a sound file via $tkdesk(soundcmd).
# Side-FX:	none
#

proc dsk_sound {snd {opts ""}} {
    global tkdesk

    if !$tkdesk(use_sound) return
    if ![info exists tkdesk(soundcmd)] {
	if {$opts == "beep"} bell
	return
    }
    if {$tkdesk(soundcmd) == ""} return
    if ![dsk_auto_execok [lindex $tkdesk(soundcmd) 0]] {
	unset tkdesk(soundcmd)
	if {$opts == "beep"} bell
	return
    }
    
    if ![info exists tkdesk(sound,$snd)] {
	#puts stderr "tkdesk: no associated sound file for $snd"
	return
    } else {
	if {$tkdesk(sound,$snd) == ""} return

	set sound $tkdesk(sound,$snd)
	if {[string index $sound 0] == "/" || \
		[string index $sound 0] == "~"} {
	    set sound $sound
	} else {
	    foreach p [split $tkdesk(path,sounds) ":"] {
		if [file exists $p/$sound] {
		    set sound $p/$sound
		    break
		}
	    }
	}
	if ![file exists $sound] {
	    cb_error "Can't find sound: [file tail $tkdesk(sound,$snd)]."
	    set tkdesk(sound,$snd) ""
	    return
	} else {
	    set tkdesk(sound,$snd) $sound
	}
	
	#update
	set errmsg ""
	set cmd [format "$tkdesk(soundcmd)" \
		[cb_tilde $tkdesk(sound,$snd) expand]]
	catch {eval exec $cmd} errmsg
	# prevent the pid in $errmsg from being printed
	if {$errmsg != "" && [string first " " $errmsg] > -1} {
	    catch {puts stderr $errmsg}
	}
    }
}

# ---------------------------------------------------------------------------
# dsk_bell:
# Rings the bell or plays the corresponding sound file:
#
proc dsk_bell {} {
    global tkdesk

    if {![info exists tkdesk(soundcmd)] || \
	    ![info exists tkdesk(sound,dsk_bell)]} {
	bell
    } else {
	dsk_sound dsk_bell
    }
}

# ---------------------------------------------------------------------------
# dsk_errbell:
# Rings the bell or plays the corresponding sound file:
#
proc dsk_errbell {} {
    global tkdesk

    if {![info exists tkdesk(soundcmd)] || \
	    ![info exists tkdesk(sound,dsk_error)]} {
	bell
    } else {
	dsk_sound dsk_error
    }
}

# ---------------------------------------------------------------------------
# dsk_print:
# Asks for the command to use when printing files.
#
proc dsk_print {args} {
    global tkdesk dsk_print_cmd

    set files $args
    if {$files == ""} {
    	set files [_make_fnames_safe]
    }

    if {$files == ""} {
	dsk_bell
	cb_info "Please select one or more files first."
	return
    }

    if [info exists tkdesk(cmd,print)] {
	set dsk_print_cmd $tkdesk(cmd,print)
    } else {
	set dsk_print_cmd "lpr"
    }

    cb_readString "Print command (file names will be appended):" \
	    dsk_print_cmd "Print Command"

    if {$dsk_print_cmd != ""} {
	eval exec "$dsk_print_cmd $files" &
	set tkdesk(cmd,print) $dsk_print_cmd
    }
}

# ---------------------------------------------------------------------------
# dsk_netscape:
# Makes use of Ken Hornstein's netscape-remote extension to communicate
# with Netscape.  If Netscape has not been started yet, TkDesk starts it.
#
proc dsk_netscape {type {loc ""} {args ""}} {
    global tkdesk

    if {[lsearch "window" $args] > -1} {set win 1} {set win 0}
    if {[lsearch "raise" $args] > -1} {set raise "raise"} {set raise "noraise"}
    
    if {$type == "file"} {
	set url "file:$loc"
    } elseif {$type == "url"} {
	set url "$loc"
    } else {
	error "dsk_netscape: unknown type $type"
    }
    if {$url != ""} {
	if $win {
	    set cmd "openURL($url,new-window,$raise)"
	} else {
	    set cmd "openURL($url,$raise)"
	}
    }

    #puts $cmd
    set err [catch {send-netscape $cmd}]
    if $err {
	if {[cb_yesno "Netscape is not yet running on your display. Start it now?"] == 0} {
	    # start new netscape
	    if {$url != ""} {
		eval dsk_exec $tkdesk(cmd,netscape) $url
	    } else {
		eval dsk_exec $tkdesk(cmd,netscape)
	    }
	}
    }
}
