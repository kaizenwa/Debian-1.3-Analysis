# =============================================================================
#
# File:		util.tcl
# Project:	TkDesk
#
# Started:	11.10.94
# Changed:	11.10.94
# Author:	cb
#
# Description:	Misc. utility procs.
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
#	proc dsk_debug {str}
#	proc dsk_busy {}
#	proc dsk_lazy {}
#	proc dsk_status {str}
#	proc dsk_logname {}
#
# =============================================================================

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_debug
# Args:		args		Optional -nonewline and string to print.
# Returns: 	""
# Desc:		Prints an arbitrary string on stderr if tkdesk(debug) is != 0.
# Side-FX:	none
#

proc dsk_debug {args} {
    global tkdesk

    if !$tkdesk(debug) return

    if {[llength $args] < 1} {
	error "too few arguments to dsk_debug"
    }

    set str [join $args]
    set nonewline 0
    if [string match "-nonew*" [lindex $args 0]] {
	set nonewline 1
	set str [lreplace $str 0 0]
    }

    if $nonewline {
    	catch {puts -nonewline stderr $str}
    } else {
    	catch {puts stderr $str}
    }
    return
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_busy
# Args:		what		(opt.) list of busy itcl classes 
# Returns: 	""
# Desc:		Displays the cursor in busy state.
# Side-FX:	none
#

set dsk_busy(still_busy) 0
set dsk_busy(ms) 200

proc dsk_busy {{what ""}} {
    global cb_tools tkdesk dsk_busy cb_balloonHelp

    incr dsk_busy(still_busy)
    
    #if {$tkdesk(status) == "busy"} {
	#return
    #}
    set tkdesk(status) busy

    set dsk_busy(windows) ""
    
    if {$what != ""} {
    	foreach class $what {
	    foreach obj [itcl_info objects -class $class] {
		lappend dsk_busy(windows) $obj
	    }
    	}
    } else {
    	foreach class {dsk_FileViewer dsk_FileList dsk_DiskUsage dsk_FileInfo
		dsk_Periodic dsk_Editor dsk_DeskItem} {
	    foreach obj [itcl_info objects -class $class] {
		lappend dsk_busy(windows) $obj
	    }
    	}
    	foreach w [winfo children .] {
	    if {[winfo class $w] == "Toplevel"} {
		lappend dsk_busy(windows) $w
	    }
    	}
    }

    set cb_balloonHelp(bltbusy) 1
    foreach win $dsk_busy(windows) {
	catch "blt_busy hold $win \
		-cursor \"@$cb_tools(path)/bitmaps/timer1.xbm \
		$cb_tools(path)/bitmaps/timer.mask.xbm black white\" "
    }

    #if ![info exists dsk_busy(afterid)] {
    #	 set dsk_busy(nr) 1
    #	 set dsk_busy(afterid) [after $dsk_busy(ms) dsk_busy_config]
    #}

    # Let the <Leave> event be transferred first:
    update
    set cb_balloonHelp(bltbusy) 0
}

# this is useless...
proc dsk_busy_config {} {
    global tkdesk dsk_busy cb_tools

    if {$tkdesk(status) == "busy"} {
	incr dsk_busy(nr)
	if {$dsk_busy(nr) > 8} {
	    set dsk_busy(nr) 1
	}
	foreach win $dsk_busy(windows) {
	    #puts "$win timer$dsk_busy(nr).xbm"
	    blt_busy configure $win \
		    -cursor "@$cb_tools(path)/bitmaps/timer$dsk_busy(nr).xbm \
		    $cb_tools(path)/bitmaps/timer.mask.xbm black white"
	}
	#update idletasks
	set dsk_busy(afterid) [after $dsk_busy(ms) dsk_busy_config]
    } else {
	unset dsk_busy(afterid)
    }
}


#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_lazy
# Args:		what		(opt.) list of now lazy itcl classes 
# Returns: 	""
# Desc:		Displays the cursor in normal state.
# Side-FX:	none
#

proc dsk_lazy {{what ""} {force 0}} {
    global tkdesk dsk_busy

    if {$tkdesk(status) == "lazy"} {
	return
    }

    if $force {
	set dsk_busy(still_busy) 0
    } else {
	incr dsk_busy(still_busy) -1
	if {$dsk_busy(still_busy) > 0} {
	    return
	}
    }
    
    set tkdesk(status) lazy
    update idletasks

    foreach win $dsk_busy(windows) {
	catch "blt_busy release $win"
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_status
# Args:		str	string to display
# Returns: 	""
# Desc:		Displays $str in the status bar of all file viewers.
# Side-FX:	none
#

proc dsk_status {str} {

    foreach fv [itcl_info objects -class dsk_FileViewer] {
	catch {$fv status $str}
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_fs_status
# Args:		file - opt. name of file
# Returns: 	A string that contains the available disk space.
# Desc:		...
# Side-FX:	none
#

proc dsk_fs_status {{file ""}} {
    global tkdesk

    # This proc is currently only enabled for Linux.  This is because
    # the output of df is different on virtually every platform.
    # Should add system-specific parsing at some point.
    if {$tkdesk(systype) != "Linux"} {
	return ""
    }

    if {$tkdesk(cmd,df) == ""} {
	return ""
    }

    if {$file == ""} {
	set file [dsk_active dir]
    }

    #dsk_busy
    set err [catch {set dfout [lindex [split [eval exec $tkdesk(cmd,df) \
	    [_make_fname_safe $file]] \n] 1]} errmsg]
    if $err {
	catch {puts stderr $errmsg}
	return ""
    }
    
    set avail [lindex $dfout 3]
    set cap [string trimright [lindex $dfout 4] %]
    if {[regexp {^[1-90]+$} $avail] && [regexp {^[1-90]+$} $cap]} {
	set fsstr " [format %.1f [expr $avail./1000]] MB available ([expr 100 - $cap]%)."
    } else {
	# for instance NFS-mounted file-systems
	set fsstr ""
    }

    #dsk_lazy
    return $fsstr
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_logname
# Args:		none
# Returns: 	The user's login name.
# Desc:		
# Side-FX:	none
#

set dsk_logname(name) ""

proc dsk_logname {} {
    global tkdesk dsk_logname

    if {$dsk_logname(name) == ""} {
	set dsk_logname(name) [exec $tkdesk(cmd,whoami)]
    }
    return $dsk_logname(name)
}

# ----------------------------------------------------------------------------
# dsk_hostname:
# Returns the name of the machine TkDesk is running on.

set dsk_hostname(name) ""

proc dsk_hostname {} {
    global dsk_hostname

    if {$dsk_hostname(name) == ""} {
	set err [catch {set dsk_hostname(name) [exec hostname]}]
	if $err {
	    set err [catch {set dsk_hostname(name) [exec uname -n]}]
	    if $err {
		set dsk_hostname(name) "unknown"
	    }
	}
    }
    return $dsk_hostname(name)
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dd_handle_text
# Args:		w		widget name
#		replace 	(opt.) set to 1 if ex. text should be deleted
# Returns: 	""
# Desc:		Drop handler for entry and text widgets.
# Side-FX:	none
#

proc dd_handle_text {w {replace 0}} {
    global DragDrop

    if !$replace {
    	$w insert insert $DragDrop(text)
    } else {
	$w delete 0 end
	$w insert end $DragDrop(text)
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		_make_path_valid
# Args:		path		path to shorten to a valid path
#		fallback	fallback path if $path is totally broken
# Returns: 	a valid path
# Desc:		see above
# Side-FX:	none
#

set _make_path_valid(barrier) 0

proc _make_path_valid {path {fallback ""}} {
    global tkdesk env _make_path_valid

    if {$fallback == ""} {
	set fallback $env(HOME)
	if ![file isdirectory $fallback] {
	    set fallback "/"
	}
    }

    catch {set path [cb_tilde $path collapse]}
    if {[string index $path 0] != "/" && [string index $path 0] != "~"} {
	if !$_make_path_valid(barrier) {
	    set _make_path_valid(barrier) 1
	    set path [dsk_active dir]/$path
	} else {
	    set _make_path_valid(barrier) 0
	    set path $fallback
	}
    }

    if [file isdirectory $path] {
	return $path
    }

    set vpath ""
    for {set i 1} {$i < [string length $path]} {incr i} {
	if {[string index $path $i] == "/"} {
	    set tpath [string range $path 0 [expr $i - 1]]
	    if [file isdirectory $tpath] {
		set vpath $tpath
	    } else {
		break
	    }
	}
    }

    if {$vpath == ""} {
	set vpath $fallback
    }

    return $vpath
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_maplist
# Args:		list - a tcl list
#               args - a list of variable name that will be set to the elements
#                      of $list
# Returns: 	""
# Desc:		see Args
# Side-FX:	none
#

proc dsk_maplist {list args} {

    set i 0
    foreach var $args {
	upvar $var v
	set v [lindex $list $i]
	incr i
    }

    return ""
}

# ----------------------------------------------------------------------------
# dsk_active what:
# what can be one of: directory/dir, selection/sel, viewer.
# Returns the current directory, selection, number of viewer/list windows.
# Also set the environment variable TKDESKDIR if $what is dir.
#
proc dsk_active {what {arg ""}} {
    global tkdesk env

    if ![winfo exists $tkdesk(active_viewer)] {
	set fvl "[itcl_info objects -class dsk_FileViewer] \
		[itcl_info objects -class dsk_FileList]"
	set tkdesk(active_viewer) [lindex $fvl 0]
    }
    if ![winfo exists $tkdesk(active_viewer)] {
	set tkdesk(active_viewer) ""
    }
    #puts $tkdesk(active_viewer)

    switch -glob $what {
	dir* {
	    if {$tkdesk(active_viewer) != ""} {
		if {$arg == ""} {
		    set adir [$tkdesk(active_viewer) curdir]
		} else {
		    $tkdesk(active_viewer) config -dir $arg
		    return
		}
	    } else {
		set adir [pwd]
	    }
	    if ![file isdirectory $adir] {
		set adir [_make_path_valid $adir]
	    }
	    return $adir
	}
	sel* {
	    if {$tkdesk(active_viewer) != ""} {
		return [$tkdesk(active_viewer) select get]
	    } else {
		return ""
	    }
	}
	viewer {
	    return [llength "[itcl_info objects -class dsk_FileViewer] \
		[itcl_info objects -class dsk_FileList]"]
	}
    }
}

# ----------------------------------------------------------------------------
# dsk_place_window top name default
# Positions the window $top on the screen. $default is the default
# geometry of the form <width>x<height>. $name will be used for accessing
# the windows saved geometry.
#
proc dsk_place_window {top name defgeom {grid 0} {center 0}} {
    global tkdesk

    if {![info exists tkdesk(geometry,$name)]} {
	set tkdesk(geometry,$name) ""
    }

    if {$defgeom == ""} {
	update idletasks
	set defgeom "[winfo reqwidth $top]x[winfo reqheight $top]"
    }
    
    if {$tkdesk(at_pointer)} {
	set x [winfo pointerx $top]
	set y [winfo pointery $top]
	if $grid {
	    update idletasks
	    set l "[winfo reqwidth $top] [winfo reqheight $top]"
	} else {
	    if {$tkdesk(geometry,$name) == ""} {
		set l [split $defgeom x]
	    } else {
		set l [split $tkdesk(geometry,$name) x+]
	    }
	}
	set w [lindex $l 0]
	set h [lindex $l 1]
	set sw [winfo screenwidth $top]
	set sh [winfo screenheight $top]
	incr x -[expr $w / 2]
	incr y -[expr $h / 2]
	if {$x < 0} {set x 0}
	if {$x + $w > $sw} {set x [expr $sw - $w - 6]}
	if {$y < 0} {set y 0}
	if {$y + $h > $sh} {set y [expr $sh - $h - 34]}
	if $grid {
	    wm geometry $top ${defgeom}+${x}+${y}
	} else {
	    wm geometry $top ${w}x${h}+${x}+${y}
	}
    } else {
	if {$tkdesk(geometry,$name) == ""} {
	    if $center {
		cb_centerToplevel $top
	    } else {
		wm geometry $top $defgeom
	    }
	} else {
	    wm geometry $top $tkdesk(geometry,$name)
	}
    }
}

# ----------------------------------------------------------------------------
# dsk_image:
# Front end for cb_image, looks in $tkdesk(path,images) for $file.
#
proc dsk_image {file args} {
    global tkdesk

    if {[string index $file 0] == "/" || \
	    [string index $file 0] == "~"} {
	set file $file
    } else {
	foreach p [split $tkdesk(path,images) ":"] {
	    if [file exists $p/$file] {
		set file $p/$file
		break
	    }
	}
    }
    if ![file exists $file] {
	set file $tkdesk(library)/images/xlogo16.xpm
    }

    return [eval cb_image $file $args]
}

# ----------------------------------------------------------------------------
# dsk_auto_execok cmd
# Returns 1 if $cmd is in $PATH, 0 otherwise.  "Virtualizes" the Tcl
# version-specific auto_execok.
#
proc dsk_auto_execok {cmd} {
    global tcl_version

    if {$tcl_version > 7.5} {
	if {[auto_execok $cmd] != ""} {
	    return 1
	} else {
	    return 0
	}
    } else {
	return [auto_execok $cmd]
    }
}
