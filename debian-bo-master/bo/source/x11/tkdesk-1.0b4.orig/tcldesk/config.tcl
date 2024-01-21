# =============================================================================
#
# File:		config.tcl
# Project:	TkDesk
#
# Started:	11.10.94
# Changed:	22.11.94
# Author:	cb
#
# Description:	Reads the config files.
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
#	proc dsk_read_config {}
#	proc dsk_edit_configs {}
#	proc read_System {}
#	proc read_ButtonBar {}
#	proc read_FileTags {}
#	proc read_Preferences {}
#	proc read_Directories {}
#	proc read_Popups {}
#	proc read_AppBar {}
#	proc read__history {}
#	proc read__layout {}
#	proc dsk_save_config {}
#	proc save__history {}
#	proc save__layout {}
#
# =============================================================================


#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_read_config
# Args:		none
# Returns: 	""
# Desc:		Determines the config dir and reads all of the config files.
# Side-FX:	Exits TkDesk if an error occured. Sets tkdesk(configdir).
#

proc dsk_read_config {} {
    global tkdesk env

    if ![info exists tkdesk(configdir)] {

	set notice 0
	set version 1.0a1
	if ![file exists $env(HOME)/.tkdesk] {
	    set tkdesk(first_time_user) 1
	    exec mkdir $env(HOME)/.tkdesk
	    exec mkdir $env(HOME)/.tkdesk/.trash
	    catch {eval exec cp [glob $tkdesk(library)/configs/*] \
		    $env(HOME)/.tkdesk}
	    set fd [open $env(HOME)/.tkdesk/_version w]
	    puts $fd $tkdesk(version)
	    close $fd
	} else {
	    if ![file readable $env(HOME)/.tkdesk/_version] {
		set notice 1
	    } else {
		set fd [open $env(HOME)/.tkdesk/_version r]
		set version [gets $fd]
		close $fd
		if {$version != $tkdesk(version)} {
		    set notice 1
		}
	    }
	}

	if {$notice} {
	    cb_info "Apparently you've been running a previous version of TkDesk. You might like to have a look at menu entry \"Help/Changes\" to find out what has changed since version $version."
	    set fd [open $env(HOME)/.tkdesk/_version w]
	    puts $fd $tkdesk(version)
	    close $fd
	}

    	#
    	# Look for configuration directory in:
    	# 1. home dir, 2. current dir, 3. library dir
    	#
    
    	if [file isdirectory $env(HOME)/.tkdesk] {
            set tkdesk(configdir) $env(HOME)/.tkdesk
    	} elseif [file isdirectory [pwd]/.tkdesk] {
            set tkdesk(configdir) [pwd]/.tkdesk
    	} elseif [file isdirectory $tkdesk(library)/configs] {
            set tkdesk(configdir) $tkdesk(library)/configs
    	} else {
            exit 1
    	}

    } else {
	# else configdir has been set via command line
	dsk_debug "TkDesk: Reading configuration from $tkdesk(configdir)"
	if ![file isdirectory $env(HOME)/.tkdesk] {
	    set tkdesk(first_time_user) 1
	}
    }

    if ![file writable $tkdesk(configdir)] {
	cb_alert "Will not be able to auto-save configuration (configuration directory is not writable)."
    }

    dsk_debug "Setting tkdesk(configdir) to $tkdesk(configdir)"

    set tkdesk(configfiles) {AppBar ButtonBar Commands Directories FileTags \
	Local Popups Sounds System}

    read_System
    read_ButtonBar
    read_Preferences
    read_FileTags
    read_Commands
    read_Directories
    read_Popups
    read_AppBar
    read_Sounds
    read_Local
    read__history
    read__layout
    read__annotations
    read__bookmarks
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_edit_configs
# Args:		files		opt. list of config files to edit
# Returns: 	""
# Desc:		Calls the editor via dsk_edit on all or selected config files.
# Side-FX:	none
#

proc dsk_edit_configs {args} {
    global tkdesk

    set cdir [string trimright $tkdesk(configdir) "/"]
    set clist ""
    set files $args
    if {$files == ""} {
	foreach cfile $tkdesk(configfiles) {
	    lappend clist $cdir/$cfile
	}
    } else {
	foreach cfile $files {
	    lappend clist $cdir/$cfile
	}
    }

    dsk_Editor .de[dsk_Editor :: id] -files $clist
    #eval dsk_edit $clist
}

#
# -----------------------------------------------------------------------------
#
# Proc:		source_cfg
# Args:		file - path of cfg file to source
# Returns: 	""
# Desc:		Sources the configuration file $file.
# Side-FX:	none
#

proc source_cfg {file} {
    global tkdesk tkdesk_anno tk_strictMotif cb_tools env

    set err [catch "source $file" errmsg]
    if $err {
	dsk_lazy
	dsk_errbell
	cb_error "Error in file [file tail $file]: $errmsg"
    }
    return
}

#
# -----------------------------------------------------------------------------
#
# Proc:		read_System
# Args:		none
# Returns: 	""
# Desc:		Reads the "System" configuration file.
#		Sets colors, fonts and other system-wide parameters.
# Side-FX:	Exits TkDesk if it was not found.
#

# Defaults:
set tkdesk(color,directories) blue2
set tkdesk(color,executables) red
set tkdesk(color,symlinks) black
set tkdesk(color,symdirectories) blue2
set tkdesk(color,symexecutables) red
set tkdesk(font,directories) -*-helvetica-bold-r-*-*-12-*-*-*-*-*-*-*
set tkdesk(font,executables) -*-helvetica-bold-r-*-*-12-*-*-*-*-*-*-*
set tkdesk(font,symlinks) -*-helvetica-medium-o-*-*-12-*-*-*-*-*-*-*
set tkdesk(font,symdirectories) -*-helvetica-bold-o-*-*-12-*-*-*-*-*-*-*
set tkdesk(font,symexecutables) -*-helvetica-bold-o-*-*-12-*-*-*-*-*-*-*
set tkdesk(font,file_lbs) -*-helvetica-medium-r-*-*-12-*-*-*-*-*-*-*

set tkdesk(num_lbs) 3           ;# number of file-lists in browser
set tkdesk(path,images) "$tkdesk(library)/images"
set tkdesk(path,sounds) "$tkdesk(library)/sounds"
set tkdesk(icon,filebrowser) $tkdesk(library)/images/bigfiling.xpm
set tkdesk(icon,filelist) $tkdesk(library)/images/filing.xpm
set tkdesk(icon,help) $tkdesk(library)/images/help.xpm
set tkdesk(icon,editor) $tkdesk(library)/images/pencil3.xpm
set tkdesk(editor,default_geometry) "80x25"
set tkdesk(cmd,netscape) "netscape -ncols 64"
set tkdesk(cmd,df) "df"
set tkdesk(focus_follows_mouse) 1
set tkdesk(editor,word_wrap) 0
set tkdesk(editor,real_tabs) 1
set tkdesk(editor,tab_width) 8
set tkdesk(editor,do_backups) 1
set tkdesk(editor,cursor) black
set tkdesk(desk_items,wm_managed) 0

set tkdesk(title,browser) "%p"
set tkdesk(title,list) "%d"
set tkdesk(title,icon) "%d"

proc read_System {} {
    global tkdesk tk_strictMotif

    dsk_debug "Reading $tkdesk(configdir)/System"

    if ![file readable $tkdesk(configdir)/System] {
        catch {puts stderr "Couldn't read $tkdesk(configdir)/System! Exiting..."}
        exit 1
    } else {
        source_cfg $tkdesk(configdir)/System
    }
    
    #
    # Set colors
    #

    if [info exists tkdesk(color,basic)] {
	tk_setPalette $tkdesk(color,basic)
    }
    option add *Entry.background $tkdesk(color,entry)
    option add *Text.background $tkdesk(color,text)
    
    if {$tkdesk(color,entry) != "white"} {
	option add *Entry.selectBackground white
	option add *Entry.selectForeground black
    } else {
	option add *Entry.selectForeground white
	option add *Entry.selectBackground black
    }
    
    if {$tkdesk(color,text) != "white"} {
	option add *Text.selectBackground white
	option add *Text.selectForeground black
    } else {
	option add *Text.selectForeground white
	option add *Text.selectBackground black
    }

    if ![info exists tkdesk(editor,background)] {
	set tkdesk(editor,background) $tkdesk(color,text)
    }

    if {$tkdesk(color,filelb_background) != "white"} {
	dsk_Listbox :: selcolor white
    } else {
	dsk_Listbox :: selcolor black
    }
    
    if {[winfo depth .] > 1} {
	option add *Listbox.selectBackground white
    }
    
    #
    # Set fonts
    #
    
    option add *Label.font $tkdesk(font,labels)
    option add *Entry.font $tkdesk(font,entries)
    option add *Text.font $tkdesk(font,entries)
    option add *Button.font $tkdesk(font,buttons)
    option add *Menubutton.font $tkdesk(font,menubuttons)
    option add *Menu.font $tkdesk(font,menus)
    
    dsk_FileListbox :: font $tkdesk(font,file_lbs)
    
    dsk_FileListbox :: tag config dir \
    	$tkdesk(color,directories) $tkdesk(font,directories)
    dsk_FileListbox :: tag config exec \
	$tkdesk(color,executables) $tkdesk(font,executables)
    dsk_FileListbox :: tag config sym \
    	$tkdesk(color,symlinks) $tkdesk(font,symlinks)
    dsk_FileListbox :: tag config symdir \
    	$tkdesk(color,symdirectories) $tkdesk(font,symdirectories)
    dsk_FileListbox :: tag config symexec \
    	$tkdesk(color,symexecutables) $tkdesk(font,symexecutables)
    
    cb_help setfont $tkdesk(font,mono)
    #if [info exists tkdesk(color,basic)] {
    #	 cb_help textbg $tkdesk(color,basic)
    #}

    #
    # Set shell commands
    #

    dsk_DiskUsage :: cmds $tkdesk(cmd,du) $tkdesk(cmd,sort)

    #
    # Others:
    #

    option add *Menubutton.padX 3
    option add *Menubutton.padY 3
    option add *Scrollbar.width 13
    option add *Scrollbar.borderWidth 2
    option add *Menu.tearOff $tkdesk(tearoff-menus)

    if $tkdesk(focus_follows_mouse) {
	tk_focusFollowsMouse
    }

    return
}

#
# -----------------------------------------------------------------------------
#
# Proc:		read_ButtonBar
# Args:		none
# Returns: 	""
# Desc:		Reads the "ButtonBar" config file.
# Side-FX:	sets the tkdesk(directories) list
#

proc read_ButtonBar {} {
    global tkdesk

    #
    # Read the button bar list
    #

    set tkdesk(small_button_bar) {}
    if [file readable $tkdesk(configdir)/ButtonBar] {
    	dsk_debug "Reading $tkdesk(configdir)/ButtonBar"
        source_cfg $tkdesk(configdir)/ButtonBar
    }

    return
}

#
# -----------------------------------------------------------------------------
#
# Proc:		read_FileTags
# Args:		none
# Returns: 	""
# Desc:		Reads the "FileTags" config file.
# Side-FX:	Creates tags for the file listboxes.
#

proc read_FileTags {} {
    global tkdesk

    #
    # Read file tags
    #
    
    if [file readable $tkdesk(configdir)/FileTags] {
        dsk_debug "Reading $tkdesk(configdir)/FileTags"
        source_cfg $tkdesk(configdir)/FileTags

	if [info exists tkdesk(file_tags,directories)] {
	    foreach tag $tkdesk(file_tags,directories) {
		set font ""; set licon ""; set dicon ""; 
		ot_maplist $tag pats col font licon dicon
		if {$pats == "!default"} {
		    dsk_FileListbox :: tag config "dir" $col $font $licon
		} elseif {$pats == "!symlink"} {
		    dsk_FileListbox :: tag config "symdir" $col $font $licon
		} elseif {$pats == "!opened"} {
		    dsk_FileListbox :: pathimage $licon
		}
		foreach pat $pats {
		    dsk_FileListbox :: tag create "dir $pat" $col $font $licon
		}
	    }
	}

	if [info exists tkdesk(file_tags,executables)] {
	    foreach tag $tkdesk(file_tags,executables) {
		set font ""; set licon ""; set dicon ""; 
		ot_maplist $tag pats col font licon dicon
		if {$pats == "!default"} {
		    dsk_FileListbox :: tag config "exec" $col $font $licon
		} elseif {$pats == "!symlink"} {
		    dsk_FileListbox :: tag config "symexec" $col $font $licon
		}
		foreach pat $pats {
		    dsk_FileListbox :: tag create "exec $pat" $col $font $licon
		}
	    }
	}

	# this must be the last so that the other lists get precedence
	if [info exists tkdesk(file_tags)] {
	    foreach tag $tkdesk(file_tags) {
		set font ""; set licon ""; set dicon ""; 
		ot_maplist $tag pats col font licon dicon
		if {$pats == "!default"} {
		    dsk_FileListbox :: font $font
		    dsk_FileListbox :: color $col
		    dsk_FileListbox :: defimage $licon
		} elseif {$pats == "!symlink"} {
		    dsk_FileListbox :: tag config "sym" $col $font $licon
		}
		foreach pat $pats {
		    dsk_FileListbox :: tag create $pat $col $font $licon
		}
	    }
	}
    }

    return
}

#
# -----------------------------------------------------------------------------
#
# Proc:		read_Preferences
# Args:		none
# Returns: 	""
# Desc:		Reads the "Preferences" config file.
# Side-FX:	
#

# Defaults:
set tkdesk(add_icons) 0		;# add icons to file lists?
set tkdesk(show_all_files) 0	;# show files starting with "." ?
set tkdesk(folders_on_top) 1	;# put folders always on top ?
set tkdesk(append_type_char) 0	;# append "/" to folders etc. ?
set tkdesk(single_click) 0	;# Let dirs be opened by a single click?
set tkdesk(strip_home) 1	;# use $HOME as root dir if under ~ ?
set tkdesk(at_pointer) 1	;# Place dialogs at mouse pointer?
set tkdesk(overwrite_always) 0	;# Overwrite existing files /wo asking?
set tkdesk(really_delete) 0	;# Delete REALLY by default?
set tkdesk(quick_dragndrop) 0   ;# don't ask when dropping files?
set tkdesk(sort_history) 1      ;# sort history menus?
set tkdesk(use_sound) 1         ;# Use sound if available? (see file "Sounds")
set tkdesk(in_browser) 0        ;# open new file windows always as browsers?
set tkdesk(default_sort) name   ;# default sort style
set tkdesk(ask_on_delete) 1     ;# ask even if not really deleting

set tkdesk(confvars) {add_icons show_all_files folders_on_top \
	append_type_char single_click strip_home at_pointer overwrite_always \
	really_delete quick_dragndrop sort_history use_sound default_sort \
        ask_on_delete in_browser netscape_help}

set tkdesk(autosave,history) 1
set tkdesk(autosave,layout) 1
set tkdesk(autosave,annotations) 1
set tkdesk(autosave,bookmarks) 1
set tkdesk(autosave,options) 1

set cb_tools(balloon_help) 1	;# display neat/annoying help popups?
set tkdesk(netscape_help) 0	;# display help thru netscape?

proc read_Preferences {} {
    global tkdesk

    #
    # Read preferences
    #

    if [file readable $tkdesk(configdir)/_options] {
	set pfile "_options"
    } elseif [file readable $tkdesk(configdir)/Preferences] {
	set pfile "Preferences"
    } else {
	set pfile "no-prefs" ;# fake file name for next test
    }
    
    if [file readable $tkdesk(configdir)/$pfile] {
    	dsk_debug "Reading $tkdesk(configdir)/$pfile"
	set nlb $tkdesk(num_lbs)  ;# for backward compatibilty
        source_cfg $tkdesk(configdir)/$pfile
	set tkdesk(num_lbs) $nlb
    }
    dsk_FileListbox :: showall $tkdesk(show_all_files)
    dsk_FileListbox :: topfolders $tkdesk(folders_on_top)
    dsk_FileListbox :: typechar $tkdesk(append_type_char)
    dsk_FileListbox :: addicons $tkdesk(add_icons)
    dsk_FileListbox :: sort $tkdesk(default_sort)

    # always copy, move etc. all selected files by default:
    set tkdesk(all_files) 1

    # we don't want free selection!
    set tkdesk(free_selection) 0
    
    return
}

#
# -----------------------------------------------------------------------------
#
# Proc:		read_Commands
# Args:		none
# Returns: 	""
# Desc:		Reads the "Commands" config file.
# Side-FX:	sets the tkdesk(commands) list
#

proc read_Commands {} {
    global tkdesk

    #
    # Read directories
    #
    
    if [file readable $tkdesk(configdir)/Commands] {
    	dsk_debug "Reading $tkdesk(configdir)/Commands"
        source_cfg $tkdesk(configdir)/Commands
    }

    return
}

#
# -----------------------------------------------------------------------------
#
# Proc:		read_Directories
# Args:		none
# Returns: 	""
# Desc:		Reads the "Directories" config file.
# Side-FX:	sets the tkdesk(directories) list
#

proc read_Directories {} {
    global tkdesk

    #
    # Read directories
    #
    
    if [file readable $tkdesk(configdir)/Directories] {
    	dsk_debug "Reading $tkdesk(configdir)/Directories"
        source_cfg $tkdesk(configdir)/Directories
    }

    return
}

#
# -----------------------------------------------------------------------------
#
# Proc:		read_Popups
# Args:		none
# Returns: 	""
# Desc:		Reads the "Popups" config file.
# Side-FX:	sets the tkdesk(popup,...) lists
#

proc read_Popups {} {
    global tkdesk

    #
    # Read popups
    #
    
    if [file readable $tkdesk(configdir)/Popups] {
    	dsk_debug "Reading $tkdesk(configdir)/Popups"
        source_cfg $tkdesk(configdir)/Popups
    }

    return
}

#
# -----------------------------------------------------------------------------
#
# Proc:		read_AppBar
# Args:		none
# Returns: 	""
# Desc:		Reads the "AppBar" config file.
# Side-FX:	sets the tkdesk(appbar) list
#

# some defaults:
set tkdesk(appbar,wm_managed) 0
set tkdesk(appbar,font,time) -*-courier-medium-r-*-*-10-*-*-*-*-*-*-*
set tkdesk(appbar,font,weekday) -*-helvetica-medium-r-*-*-10-*-*-*-*-*-*-*
set tkdesk(appbar,font,day) -*-times-bold-r-*-*-24-*-*-*-*-*-*-*
set tkdesk(appbar,font,month) -*-helvetica-medium-o-*-*-12-*-*-*-*-*-*-*
set tkdesk(appbar,12hour) 0
set tkdesk(appbar,load,delay) 15
set tkdesk(appbar,mail,delay) 30
set tkdesk(appbar,mail,nomail) mailbox_empty.xpm
set tkdesk(appbar,mail,oldmail) mailbox_old.xpm
set tkdesk(appbar,mail,newmail) mailbox_full.xpm
set tkdesk(appbar,mail,newbg) slategrey
set tkdesk(appbar,trash,empty) trashcan.xpm
set tkdesk(appbar,trash,full) trashcan_full.xpm

proc read_AppBar {} {
    global tkdesk

    #
    # Read application bar
    #
    
    if [file readable $tkdesk(configdir)/AppBar] {
    	dsk_debug "Reading $tkdesk(configdir)/AppBar"
        source_cfg $tkdesk(configdir)/AppBar
    }

    return
}

# -----------------------------------------------------------------------------
#
# Proc:		read_Sounds
# Args:		none
# Returns: 	""
# Desc:		Reads the "Sound" config file.
# Side-FX:	sets the tkdesk(sound,...) variables
#

proc read_Sounds {} {
    global tkdesk

    #
    # Read the sounds file
    #
    
    if [file readable $tkdesk(configdir)/Sounds] {
    	dsk_debug "Reading $tkdesk(configdir)/Sounds"
        source_cfg $tkdesk(configdir)/Sounds
    }

    return
}

# -----------------------------------------------------------------------------
#
# Proc:		read_Local
# Args:		none
# Returns: 	""
# Desc:		Reads the "Local" config file.
# Side-FX:	depends on user skills...
#

proc read_Local {} {
    global tkdesk

    #
    # Read the local config file
    #
    
    if [file readable $tkdesk(configdir)/Local] {
    	dsk_debug "Reading $tkdesk(configdir)/Local"
        source_cfg $tkdesk(configdir)/Local
    }

    return
}

#
# -----------------------------------------------------------------------------
#
# Proc:		read__history
# Args:		none
# Returns: 	""
# Desc:		Reads the _history file if it exists.
# Side-FX:	Sets the global path history.
#

proc read__history {} {
    global tkdesk

    if [file readable $tkdesk(configdir)/_history] {
    	dsk_debug "Reading $tkdesk(configdir)/_history"
	set fd [open $tkdesk(configdir)/_history]

	set tkdesk(history_list) ""
	while 1 {
	    set h [gets $fd]
	    if {$h == ""} {
		break
	    } else {
		lappend tkdesk(history_list) [list $h]
	    }
	}
	close $fd
    }

    return
}

#
# -----------------------------------------------------------------------------
#
# Proc:		read__layout
# Args:		none
# Returns: 	""
# Desc:		Reads the _layout file if it exists.
# Side-FX:	none
#

proc read__layout {} {
    global tkdesk env

    if [file readable $tkdesk(configdir)/_layout] {
    	dsk_debug "Reading $tkdesk(configdir)/_layout"
	set fd [open $tkdesk(configdir)/_layout]

	set tkdesk(layout) ""
	while 1 {
	    set l [gets $fd]
	    if {$l == ""} {
		break
	    } else {
		lappend tkdesk(layout) $l
	    }
	}
	close $fd
	if {$tkdesk(layout) == ""} {
	    catch {puts stderr "TkDesk: _layout corrupted, skipping"}
	    unset tkdesk(layout)
	}
    } else {

	# start in new user's home directory
	set tkdesk(initdir) $env(HOME)
    }

    return
}

#
# -----------------------------------------------------------------------------
#
# Proc:		read__annotations
# Args:		none
# Returns: 	""
# Desc:		Reads the _annotations file if it exists.
# Side-FX:	Sets the file annotations.
#

proc read__annotations {} {
    global tkdesk tkdesk_anno

    if [file readable $tkdesk(configdir)/_annotations] {
    	dsk_debug "Reading $tkdesk(configdir)/_annotations"
	source_cfg $tkdesk(configdir)/_annotations
    }

    return
}

# -----------------------------------------------------------------------------
#
# Proc:		read__bookmarks
# Args:		none
# Returns: 	""
# Desc:		Reads the _bookmarks file if it exists.
# Side-FX:	Sets the bookmarks list.
#

proc read__bookmarks {} {
    global tkdesk

    set tkdesk(bookmarks) {}
    if [file readable $tkdesk(configdir)/_bookmarks] {
    	dsk_debug "Reading $tkdesk(configdir)/_bookmarks"
	source_cfg $tkdesk(configdir)/_bookmarks
    }

    return
}

#
# =============================================================================
#

# -----------------------------------------------------------------------------
#
# Proc:		dsk_save_config
# Args:		none
# Returns: 	""
# Desc:		Saves the saveable configuration parameters to disk.
# Side-FX:	none
#

set dsk_save_config(busy) 0

proc dsk_save_config {{all 0}} {
    global tkdesk dsk_save_config

    if $dsk_save_config(busy) return
    set dsk_save_config(busy) 1

    if {$tkdesk(autosave,history) || $all} save__history
    if {$tkdesk(autosave,layout) || $all} save__layout
    if {$tkdesk(autosave,annotations) || $all} save__annotations
    if {$tkdesk(autosave,bookmarks) || $all} save__bookmarks
    if {$tkdesk(autosave,options) || $all} save__options
    dsk_status "Ready."

    set dsk_save_config(busy) 0
    return
}

#
# -----------------------------------------------------------------------------
#
# Proc:		save__history
# Args:		none
# Returns: 	""
# Desc:		Saves the global path history to _history.
# Side-FX:	none
#

proc save__history {} {
    global tkdesk

    set err [catch "set fd \[open $tkdesk(configdir)/_history w\]"]
    if !$err {
	dsk_status "Saving Path History ..."
	set hist [history get]
	for {set i [expr [llength $hist] - 1]} {$i > -1} {incr i -1} {
	    puts $fd [lindex $hist $i]
	}
	dsk_status "Saving Command History ..."
	puts $fd "# Command history:"
	set hist [cmd_history get]
	for {set i [expr [llength $hist] - 1]} {$i > -1} {incr i -1} {
	    puts $fd [lindex $hist $i]
	}
	dsk_status "Saving Periodic Command History ..."
	puts $fd "# Periodic command history:"
	set hist [pcmd_history get]
	for {set i [expr [llength $hist] - 1]} {$i > -1} {incr i -1} {
	    puts $fd [lindex $hist $i]
	}
	dsk_status "Saving HyperSearch History ..."
	puts $fd "# HyperSearch history:"
	set hist [hsearch_history get]
	for {set i [expr [llength $hist] - 1]} {$i > -1} {incr i -1} {
	    puts $fd [lindex $hist $i]
	}
	dsk_status "Saving Files History ..."
	puts $fd "# Files history:"
	set hist [file_history get]
	for {set i [expr [llength $hist] - 1]} {$i > -1} {incr i -1} {
	    puts $fd [lindex $hist $i]
	}
	dsk_status "Saving Executables History ..."
	puts $fd "# Executables history:"
	set hist [exec_history get]
	for {set i [expr [llength $hist] - 1]} {$i > -1} {incr i -1} {
	    puts $fd [lindex $hist $i]
	}
	close $fd
    } else {
	dsk_debug "Couldn't open $tkdesk(configdir)/_history for writing."
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		save__layout
# Args:		none
# Returns: 	""
# Desc:		Tries to save the window layout of TkDesk to _layout.
# Side-FX:	
#

proc save__layout {} {
    global tkdesk

    set err [catch "set fd \[open $tkdesk(configdir)/_layout w\]"]
    if !$err {
	dsk_status "Saving Layout ..."
	foreach class {dsk_FileViewer dsk_FileList dsk_DeskItem} {
	    set obj_list [itcl_info objects -class $class]
	    foreach obj $obj_list {
		if ![winfo exists $obj] continue
		switch $class {
		    dsk_FileViewer -
		    dsk_FileList {
			set state [wm state $obj]
			if {$state == "normal" || $state == "iconic"} {
			    set geom [wm geometry $obj]
			    if {[string first "+-" $geom] > -1} {
				# shift the window into visible area
				set geom [split $geom x+]
				set w [lindex $geom 0]
				set h [lindex $geom 1]
				set x [lindex $geom 2]
				set y [lindex $geom 3]
				set sw [winfo screenwidth $obj]
				set sh [winfo screenheight $obj]
				while {$x < 0} {incr x $sw}
				while {$y < 0} {incr y $sh}
				set geom "${w}x${h}+${x}+${y}"
			    }
			    puts -nonewline $fd "$class \{[$obj curdir]\} "
			    puts -nonewline $fd "$geom "
			    if {$class == "dsk_FileViewer"} {
				puts -nonewline $fd "$state "
				puts $fd "[$obj cget num_lbs]"
			    } else {
				puts $fd "$state"
			    }
			}
		    }
		    dsk_DeskItem {
			puts -nonewline $fd "$class \{[$obj cget -file]\} "
			set geom [wm geometry $obj]
			if {[string first "+-" $geom] > -1} {
			    # shift the window into visible area
			    set geom [split $geom x+]
			    set w [lindex $geom 0]
			    set h [lindex $geom 1]
			    set x [lindex $geom 2]
			    set y [lindex $geom 3]
			    set sw [winfo screenwidth $obj]
			    set sh [winfo screenheight $obj]
			    while {$x < 0} {incr x $sw}
			    while {$y < 0} {incr y $sh}
			    set geom "${w}x${h}+${x}+${y}"
			}
			puts $fd $geom
		    }
		}
	    }
	}
	foreach toplevel {dsk_ask_exec dsk_ask_dir dsk_jobs dsk_copy \
		dsk_delete dsk_appbar dsk_find_annotation dsk_bgexec \
		dsk_find_files} {
	    set geom ""
	    if [winfo exists .$toplevel] {
		set geom [wm geometry .$toplevel]
		if {[string first "+-" $geom] > -1} {
		    # shift the window into visible area
		    set geom [split $geom x+]
		    set w [lindex $geom 0]
		    set h [lindex $geom 1]
		    set x [lindex $geom 2]
		    set y [lindex $geom 3]
		    set sw [winfo screenwidth .$toplevel]
		    set sh [winfo screenheight .$toplevel]
		    while {$x < 0} {incr x $sw}
		    while {$y < 0} {incr y $sh}
		    set geom "${w}x${h}+${x}+${y}"
		}
	    } else {
		if [info exists tkdesk(geometry,$toplevel)] {
		    set geom $tkdesk(geometry,$toplevel)
		}
	    }
	    if {$geom != ""} {
		puts $fd "Toplevel $toplevel [winfo exists .$toplevel] $geom"
	    }
	}
	close $fd
    } else {
	dsk_debug "Couldn't open $tkdesk(configdir)/_layout for writing."
    }

}

#
# -----------------------------------------------------------------------------
#
# Proc:		save__annotations
# Args:		none
# Returns: 	""
# Desc:		Saves the file annotations to _annotations.
# Side-FX:	none
#

proc save__annotations {} {
    global tkdesk tkdesk_anno

    if ![info exists tkdesk_anno] return
    set err [catch "set fd \[open $tkdesk(configdir)/_annotations w\]"]
    if !$err {
	dsk_status "Saving File Annotations ..."
	foreach name [array names tkdesk_anno] {
	    puts $fd "set tkdesk_anno($name) \\"
	    puts $fd \{$tkdesk_anno($name)\}
	}
	close $fd
    } else {
	dsk_debug "Couldn't open $tkdesk(configdir)/_annotations for writing."
    }
}

# -----------------------------------------------------------------------------
#
# Proc:		save__bookmarks
# Args:		none
# Returns: 	""
# Desc:		Saves the bookmark list to _bookmarks.
# Side-FX:	none
#

proc save__bookmarks {} {
    global tkdesk

    set err [catch "set fd \[open $tkdesk(configdir)/_bookmarks w\]"]
    if !$err {
	dsk_status "Saving Bookmarks ..."
	puts $fd "set tkdesk(bookmarks) {"
	foreach bm $tkdesk(bookmarks) {
	    puts $fd "\{$bm\}"
	}
	puts $fd "}"
	close $fd
    } else {
	dsk_debug "Couldn't open $tkdesk(configdir)/_annotations for writing."
    }
}

# -----------------------------------------------------------------------------
#
# Proc:		save__options
# Args:		none
# Returns: 	""
# Desc:		Saves the settings of the "Options" menu to _options.
# Side-FX:	none
#

proc save__options {} {
    global tkdesk cb_tools

    set err [catch "set fd \[open $tkdesk(configdir)/_options w\]"]
    if !$err {
	dsk_status "Saving Options ..."
	foreach var $tkdesk(confvars) {
	    puts $fd "set tkdesk($var) $tkdesk($var)"
	}
	puts $fd "set tkdesk(autosave,history) $tkdesk(autosave,history)"
	puts $fd "set tkdesk(autosave,layout) $tkdesk(autosave,layout)"
	puts $fd "set tkdesk(autosave,annotations) $tkdesk(autosave,annotations)"
	puts $fd "set tkdesk(autosave,bookmarks) $tkdesk(autosave,bookmarks)"
	puts $fd "set tkdesk(autosave,options) $tkdesk(autosave,options)"
	puts $fd "set cb_tools(balloon_help) $cb_tools(balloon_help)"
	close $fd
    } else {
	dsk_debug "Couldn't open $tkdesk(configdir)/_options for writing."
    }
}

#
# =============================================================================
#

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_restore_layout
# Args:		none
# Returns: 	""
# Desc:		Tries to restore the layout of the last session.
# Side-FX:	Creates several windows, depending on the file _layout.
#

set tkdesk(have_window) 0

proc dsk_restore_layout {} {
    global tkdesk

    if [info exists tkdesk(layout)] {
	dsk_debug "Restoring layout..."
    	dsk_debug "tkdesk(ayout): $tkdesk(layout)"
    	foreach l $tkdesk(layout) {
	    set class [lindex $l 0]
	    switch $class {
	    	dsk_FileViewer {
		    set tkdesk(have_window) 1
		    dsk_progress "Creating a file browser..."
		    set win .fv[dsk_FileViewer :: id]
		    set nlb [lindex $l 4]
		    if {$nlb == ""} {
			set nlb $tkdesk(num_lbs)
		    }
		    if [info exists tkdesk(user,startdir)] {
			set dir $tkdesk(user,startdir)
			unset tkdesk(user,startdir)
		    } else {
			set dir [lindex $l 1]
		    }
		    dsk_FileViewer $win -dir $dir \
			    -num_lbs $nlb -dontmap 1
		    #wm withdraw $win
		    wm geometry $win [lindex $l 2]
		    if {[lindex $l 3] == "iconic" || $tkdesk(iconic)} {
			update
			wm iconify $win
		    } else {
			wm deiconify $win
			tkwait visibility $win
			catch "lower $win .dsk_welcome"
			update
		    }
		}
	    	dsk_FileList {
		    set tkdesk(have_window) 1
		    dsk_progress "Creating a file list..."
		    set win .dfl[dsk_FileList :: id]
		    dsk_FileList $win -dir [lindex $l 1] -dontmap 1
		    #wm withdraw $win
		    wm geometry $win [lindex $l 2]
		    if {[lindex $l 3] == "iconic" || $tkdesk(iconic)} {
		    	update
			wm iconify $win
		    } else {
			wm deiconify $win
			tkwait visibility $win
			catch "lower $win .dsk_welcome"
			update
		    }
		}
	    	dsk_DeskItem {
		    dsk_progress "Creating a desk item..."
		    set win .di[dsk_DeskItem :: id]
		    set itemname [lindex $l 1]
		    if [file isdirectory $itemname] {
			set tkdesk(have_window) 1
		    }
		    dsk_DeskItem $win -file $itemname -dontmap 1
		    #wm withdraw $win
		    wm geometry $win [lindex $l 2]
		    wm deiconify $win
		    tkwait visibility $win
		    catch "lower $win .dsk_welcome"
		    update
		}
	    	Toplevel {
		    ot_maplist $l type func posted geom
		    if {$func == "dsk_appbar"} {
			set tkdesk(have_window) 1
		    }
		    set tkdesk(geometry,$func) $geom
		    if $posted {
			eval $func
		    }
	    	}
	    }
    	}
    }

    return
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_reread_config
# Args:		none
# Returns: 	""
# Desc:		Rereads the config files.
# Side-FX:	Does a dsk_save_config.
#

proc dsk_reread_config {{cfgfile ""}} {
    global tkdesk

    if {$cfgfile != ""} {
	dsk_busy
	dsk_status "Rereading $cfgfile..."
	
	switch $cfgfile {
	    AppBar {
		if [winfo exists .dsk_appbar] {
		    dsk_appbar close
		    set open_appbar 1
		}
		cb_image !reset
		read_AppBar
		if [info exists open_appbar] {
		    dsk_appbar
		}
	    }
	    ButtonBar {
		cb_image !reset
		read_ButtonBar
		foreach br [itcl_info objects -class dsk_FileViewer] {
		    $br _button_bar
		}
		foreach fl [itcl_info objects -class dsk_FileList] {
		    $fl _button_bar
		}
	    }
	    Commands {
		read_Commands
		dsk_rebuild_cmdmenus
	    }
	    Directories {
		read_Directories
		dsk_rebuild_dirmenus
	    }
	    FileTags {
		#dsk_reread_config		
		dsk_FileListbox :: tag reset
		cb_image !reset
		read_FileTags
		foreach fl [itcl_info objects -class dsk_FileListbox] {
		    if [winfo exists $fl] {
			$fl refresh
		    }
		}
		foreach di [itcl_info objects -class dsk_DeskItem] {
		    if [winfo exists $di] {
			$di refresh
		    }
		}
	    }
	    Local {
		read_Local
	    }
	    Popups {
		read_Popups
	    }
	    Preferences {
		read_Preferences
	    }
	    Sounds {
		read_Sounds
	    }
	    System {
		read_System
	    }
	    default {
		dsk_reread_config		
	    }
	}
	
	dsk_status "Ready."
	dsk_lazy
	return
	
    } else {
	foreach cfg $tkdesk(configfiles) {
	    dsk_reread_config $cfg
	}
	return
    }

    # OBSOLETE - this is no longer executed
    #
    # Reread all config files:
    #

    foreach obj [itcl_info objects -class dsk_Editor] {
	if {[$obj close_win] == "cancel"} {
	    return
	}
    }

    dsk_busy

    dsk_save_config
    dsk_status "Rereading configuration..."
    dsk_read_config

    dsk_debug "Deleting old windows..."
    foreach class {dsk_FileViewer dsk_FileList} {
	foreach obj [itcl_info objects -class $class] {
	    $obj delete
	}
	$class :: id reset
    }
    dsk_Editor :: id reset
    foreach topname {dsk_ask_exec dsk_ask_dir dsk_jobs dsk_copy \
		dsk_delete dsk_appbar dsk_find_annotation} {
	catch "destroy .$topname"
    }

    dsk_restore_layout

    dsk_lazy
    return
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_persave_config
# Args:		none
# Returns: 	""
# Desc:		Saves the configuration (i.e. history and layout) periodically.
# Side-FX:	none
#

proc dsk_persave_config {} {
    global tkdesk

    if {[itcl_info objects -class dsk_FileViewer] != ""} {
    	dsk_save_config
    	after [expr $tkdesk(update,config) * 60000] dsk_persave_config
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		_add_cmd_to_menu
# Args:		menu - name of menu widget
#               cmd - list of commands (initially from the Commands cfgfile)
# Returns: 	""
# Desc:		Adds new menu entries to the given menu to invoke commands.
# Side-FX:	none
#

set tkdesk(_cmdmenu,cnt) 0
proc _add_cmd_to_menu {menu cmd} {
    global tkdesk

    if {[llength $cmd] == 2} {
	set command [string_replace [lindex $cmd 1] \" \\\"]
	$menu add command -label [lindex $cmd 0] \
		-command "cd \[dsk_active dir\] ;\
		eval \[_expand_pc \"$command\"\]; cd ~" 
		#-font $tkdesk(font,file_lbs)
    } elseif {[llength $cmd] == 1} {
	$menu add separator
    } else {
	incr tkdesk(_cmdmenu,cnt)
	set m ${menu}.c$tkdesk(_cmdmenu,cnt)
	$menu add cascade -label [lindex $cmd 0] -menu $m 
		#-font $tkdesk(font,file_lbs)

	catch "destroy $m"
	menu $m
	set cmd [lreplace $cmd 0 0]
	foreach c $cmd {
	    _add_cmd_to_menu $m $c
	}
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_rebuild_cmdmenus
# Args:		none
# Returns: 	""
# Desc:		Rebuilds the Commands menus of all Viewers and Lists.
# Side-FX:	none
#

proc dsk_rebuild_cmdmenus {} {
    global tkdesk

    set tkdesk(_cmdmenu,cnt) 0
    if [info exists tkdesk(commands)] {
	foreach class "dsk_FileViewer dsk_FileList" {
	    foreach t [itcl_info objects -class $class] {
		if {$class == "dsk_FileViewer"} {
		    set m $t.fMenu.mbCmds.menu
		} else {
		    set m $t.fMenu.mbOthers.menu.cmd
		}
		set fe [expr [$m cget -tearoff] ? 5 : 4]
		catch "$m delete $fe last"
		foreach cmd $tkdesk(commands) {
		    if {[llength $cmd] > 1} {
			_add_cmd_to_menu $m $cmd
		    } else {
			$m add separator
		    }
		}
	    }
	}
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		_add_dir_to_menu
# Args:		menu - name of menu widget
#               mdir - list of directories
# Returns: 	""
# Desc:		Adds directory menu entries to the given menu.
# Side-FX:	none
#

set tkdesk(_dirmenu,cnt) 0
proc _add_dir_to_menu {this menu mdir} {
    global tkdesk

    if {[llength $mdir] == 1} {
	if {[string index $mdir 0] != "*"} {
	    $menu add command -label $mdir \
		    -command "$this config -dir $mdir" \
		    -font $tkdesk(font,file_lbs)
	} else {
	    set mdir [string range $mdir 1 end]
	    incr tkdesk(_dirmenu,cnt)
	    set m ${menu}.cas$tkdesk(_dirmenu,cnt)
	    catch {destroy $m}
	    menu $m -postcommand "dsk_casdirs $mdir $m 1"
	    $menu add cascade -label "$mdir (*)" -menu $m \
		    -font $tkdesk(font,file_lbs) \
	}
    } else {
	incr tkdesk(_dirmenu,cnt)
	set m ${menu}.d$tkdesk(_dirmenu,cnt)
	$menu add cascade -label [lindex $mdir 0] -menu $m \
		-font $tkdesk(font,file_lbs)
	catch "destroy $m"
	menu $m
	bind $m <ButtonRelease-1> "
	set tkdesk(menu,control) 0
	[bind Menu <ButtonRelease-1>]"
	bind $m <Control-ButtonRelease-1> "
	set tkdesk(menu,control) 1
	[bind Menu <ButtonRelease-1>]"

	foreach d $mdir {
	    if {$d == "-"} {
		$m add separator
	    } else {
		_add_dir_to_menu $this $m $d
	    }
	}
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_rebuild_dirmenus
# Args:		none
# Returns: 	""
# Desc:		Rebuilds the Directories menus of all Viewers and Lists.
# Side-FX:	none
#

proc dsk_rebuild_dirmenus {} {
    global tkdesk

    set tkdesk(_dirmenu,cnt) 0
    if [info exists tkdesk(directories)] {
	foreach class "dsk_FileViewer dsk_FileList" {
	    foreach t [itcl_info objects -class $class] {
		set m $t.fMenu.mbDirs.menu
		set fe [expr [$m cget -tearoff] ? 9 : 8]
		catch "$m delete $fe last"
		foreach mdir $tkdesk(directories) {
		    if {$mdir == "-"} {
			$m add separator
		    } else {
			_add_dir_to_menu $t $m $mdir
		    }
		}
	    }
	}
    }
}

