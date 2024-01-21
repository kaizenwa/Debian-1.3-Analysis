# =============================================================================
#
# File:		find.tcl
# Project:	TkDesk
#
# Started:	12.12.94
# Changed:	29.03.96
# Author:	cb
#
# Description:	Implements procs for searching files and annotations.
#               Also does the bookmarks stuff.
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
# =============================================================================

# ----------------------------------------------------------------------------
# dsk_find_files:
# TkDesk's "Find Files..." dialog.
#

if ![info exists tkdesk(geometry,dsk_find_files)] {
    set tkdesk(geometry,dsk_find_files) ""
}
set dsk_find(paths) ""
set dsk_find(name) ""
set dsk_find(string) ""
set dsk_find(strtype) "Strings:"
set dsk_find(size) ""
set dsk_find(days) ""
set dsk_find(type) "All Types"
set dsk_find(time) "Last Modified:"
set dsk_find(samefs) 0
set dsk_find(follow) 0

proc dsk_find_files {} {
    global tkdesk dsk_find

    set t .dsk_find_files
    if [winfo exists $t] {
	cb_raise $t
	return
    }

    toplevel $t
    wm withdraw $t
    frame $t.f -bd 1 -relief raised
    pack $t.f -fill x

    # ---- Paths:
    frame $t.f1
    pack $t.f1 -in $t.f -fill x -expand yes -padx $tkdesk(pad)

    label $t.lsp -text "Path(s):" -width 11 -anchor w
    entry $t.esp -bd 2 -relief sunken -width 20 -textvar dsk_find(paths)
    $t.esp delete 0 end
    $t.esp insert end [string trimright [dsk_active dir] /]
    bind $t.esp <Return> "$t.bSearch.button flash ; $t.bSearch.button invoke"
    cb_bindForCompletion $t.esp <Control-Tab>
    cb_balloonHelp $t.esp "The search will start at this directory. You can enter more than one directory by separating them with spaces. If this field remains blank, search will start at the current directory of the last active file browser resp. viewer."
    
    frame $t.fs1 -width 8
    menubutton $t.mbDir -bd 2 -relief raised \
		-bitmap @$tkdesk(library)/cb_tools/bitmaps/slash.xbm \
		-menu $t.mbDir.menu
    cb_balloonHelp $t.mbDir "This menu contains the custom directories from the Directories menu."
    menu [set m $t.mbDir.menu] \
	    -postcommand "_dsk_dmenu $t.mbDir.menu $t.esp"
    # add dummy entry to work around bug in pre Tk 4.0p2:
    $m add command -label "dummy"

    menubutton $t.mbHist -bd 2 -relief raised \
		-bitmap @$tkdesk(library)/cb_tools/bitmaps/combo.xbm \
		-menu $t.mbHist.menu
    cb_balloonHelp $t.mbHist "This menu contains the directories you have last visited."
    menu $t.mbHist.menu -postcommand "_dsk_hmenu $t.mbHist.menu $t.esp"
    # add dummy entry to work around bug in pre Tk 4.0p2:
    $t.mbHist.menu add command -label "dummy"

    pack $t.lsp $t.esp $t.fs1 $t.mbDir $t.mbHist -in $t.f1 -side left \
	    -pady $tkdesk(pad)
    pack config $t.esp -fill x -ipady 2 -expand yes
    pack config $t.mbDir $t.mbHist -ipadx 2 -ipady 2

    # ---- Name, Time, Type:
    frame $t.f2
    pack $t.f2 -in $t.f -fill x -expand yes -padx $tkdesk(pad)

    label $t.ln -text "File-Mask:" -width 11 -anchor w
    entry $t.en -bd 2 -relief sunken -width 14 -textvar dsk_find(name)
    bind $t.en <Return> "$t.bSearch.button flash ; $t.bSearch.button invoke"
    cb_balloonHelp $t.en "Enter a shell-like glob pattern here (e.g. *.c) to search for files with a certain name."
    frame $t.fs2 -width 8
    tk_optionMenu $t.mt dsk_find(type) "All Types" "Regular Files" \
	    "Directories" "Symbolic Links" "Sockets" "Named Pipes"
    $t.mt config -width 12 -anchor w
    cb_balloonHelp $t.mt "The type of files to look for."
    frame $t.fs4 -width 8
    tk_optionMenu $t.mtm  dsk_find(time) "Last Accessed:" "Last Modified:"
    $t.mtm config -width 13 -anchor w
    cb_balloonHelp $t.mtm "This setting relates to the entry field right to this menu button. The value entered there can either relate to when the file was last accessed or last modified."
    entry $t.etm -bd 2 -relief sunken -width 5 -textvar dsk_find(days)
    cb_balloonHelp $t.etm "10 will match files which are 10 days old, +10 will match all files older than 10 days, and -10 all newer than 10 days. Leave this field blank if this is not important for your search."
    bind $t.etm <Return> "$t.bSearch.button flash ; $t.bSearch.button invoke"
    pack $t.ln $t.en $t.fs2 $t.mt $t.fs4 $t.mtm $t.etm \
	    -in $t.f2 -side left -pady $tkdesk(pad)
    pack config $t.en -fill x -ipady 2 -expand yes

    # ---- Size, String:
    frame $t.f3
    pack $t.f3 -in $t.f -fill x -expand yes -padx $tkdesk(pad)

    tk_optionMenu $t.mst dsk_find(strtype) "Strings:" "RegExp:" "ExtReg:"
    $t.mst config -width 7 -anchor w
    cb_balloonHelp $t.mst "Whether to look for a set of strings (separated by ||), for a regular expression, or for an extended regular expression (see the grep manual-page for more)."
    #label $t.lst -text "String:"
    entry $t.est -bd 2 -relief sunken -width 34 -textvar dsk_find(string)
    bind $t.est <Return> "$t.bSearch.button flash ; $t.bSearch.button invoke"
    cb_balloonHelp $t.est "Enter a regular expression in this field if you're looking for files containing a certain string."
    frame $t.fs3 -width 8
    label $t.ls -text "File-Size (kB):"
    entry $t.es -bd 2 -relief sunken -width 4 -textvar dsk_find(size)
    bind $t.es <Return> "$t.bSearch.button flash ; $t.bSearch.button invoke"
    cb_balloonHelp $t.es "If the size of files is important, this either gives the exact number of kilobytes, or, if preceded with a + resp. -, files will be matched which are bigger resp. smaller than this number. E.g. +100 will match all files which are bigger than 100 kB."
    pack $t.mst $t.est $t.fs3 $t.ls $t.es \
	    -in $t.f3 -side left -pady $tkdesk(pad)
    pack config $t.est $t.es $t.etm -fill x -ipady 2 -expand yes

    # ---- Buttons:
    frame $t.f4
    pack $t.f4 -in $t.f -fill x -expand yes -padx $tkdesk(pad)

    cb_button $t.bSearch -text " Search " -default 1 \
	    -command dsk_do_find_files
    cb_balloonHelp $t.bSearch.button "Starts the search. The search will be carried out in the background."
    button $t.bClose -text "  Close  " -command \
	    "set tkdesk(geometry,dsk_find_files) \[wm geometry $t\] ;\
	    destroy $t"
    cb_balloonHelp $t.bClose "Closes this dialog."
    frame $t.fs5 -width 8
    checkbutton $t.cbs -text "Same Filesystem " -variable dsk_find(samefs)
    cb_balloonHelp $t.cbs "If selected, only the filesystem of the start directory/ies will be searched."
    checkbutton $t.cbf -text "Follow SymLinks" -variable dsk_find(follow)
    cb_balloonHelp $t.cbf "If you are using GNU find, you can follow symbolic links to directories when performing a search."
    
    pack $t.bSearch $t.bClose $t.fs5 $t.cbs $t.cbf -in $t.f4 -side left \
	    -padx $tkdesk(pad) -pady $tkdesk(pad)
    pack config $t.bClose -ipady 1

    # ---- dsk_Listbox:
    frame $t.flb -bd 1 -relief raised
    pack $t.flb -fill both -expand yes
    frame $t.f5
    pack $t.f5 -in $t.flb -fill both -expand yes -pady $tkdesk(pad)
    
    label $t.llb -text "Matching files:" -anchor w
    pack $t.llb -in $t.f5 -anchor w -padx $tkdesk(pad) 
    
    pack [_dsk_find_lb $t.dlb] -in $t.f5 -fill both -expand yes


    wm title $t "Find Files"
    wm minsize $t 10 2
    dsk_place_window $t dsk_find_files 10x5 1
    #if {$tkdesk(geometry,dsk_find_files) == ""} {
    #	 wm geometry $t 10x5	
    #} else {
    #	 wm geometry $t $tkdesk(geometry,dsk_find_files)
    #}
    wm protocol $t WM_DELETE_WINDOW "$t.bClose invoke"
    wm deiconify $t
}

# ----------------------------------------------------------------------------
# dsk_do_find_files:
# Executes "find" with the parameters set thhrough the dialog.
#
proc dsk_do_find_files {} {
    global tkdesk dsk_find

    set cmd "find "
    
    if {$dsk_find(paths) != ""} {
	foreach path $dsk_find(paths) {
	    if {$path != "/"} {
		append cmd " [cb_tilde [string trimright $path /] expand]"
	    } else {
		append cmd " $path"
	    }
	}
    } else {
	append cmd " [cb_tilde [string trimright \
		[dsk_active dir] /] expand]"
    }

    switch $dsk_find(type) {
	"Regular Files" {
	    append cmd " -type f"
	}
	"Directories" {
	    append cmd " -type d"
	}
	"Symbolic Links" {
	    append cmd " -type l"
	}
	"Sockets" {
	    append cmd " -type s"
	}
	"Named Pipes" {
	    append cmd " -type p"
	}
    }
    
    if {$dsk_find(name) != ""} {
	append cmd " -name $dsk_find(name)"
    }
    if {$dsk_find(size) != ""} {
	set rc [string index $dsk_find(size) 0]
	if {$rc != "+" && $rc != "-"} {
	    set rc ""
	}
	set num [string trimleft $dsk_find(size) "+-"]
	# 2 * $num, because -size argument is in blocks (512 bytes)
	append cmd " -size $rc[expr 2 * $num]"
    }
    if {$dsk_find(days) != ""} {
	if {$dsk_find(time) == "Last Accessed:"} {
	    append cmd " -atime $dsk_find(days)"
	} else {
	    append cmd " -mtime $dsk_find(days)"
	}
    }
    if $dsk_find(samefs) {
	if [regexp -nocase "irix" [exec uname]] {
	    Append cmd "-local"
	} else {
	    append cmd " -xdev"
	}
    }
    if $dsk_find(follow) {
	append cmd " -follow"
    }
    
    if {$dsk_find(string) != ""} {
	set str $dsk_find(string)
	switch $dsk_find(strtype) {
	    "Strings:" {
		set grep "fgrep"
		set str [string_replace $dsk_find(string) "||" "\n"]
	    }
	    "RegExp:" {
		set grep "grep"
	    }
	    "ExtReg:" {
		set grep "egrep -e"
	    }
	}
	append cmd " -exec $grep -l \"$str\" \\\{\\\} \\\;"
    } else {
	append cmd " -print"
    }

    set cmd [string_replace $cmd \[ \\\[]
    set cmd [string_replace $cmd \] \\\]]
    append cmd " 2>/dev/null"
    if $tkdesk(debug) {
	catch {puts stderr "$cmd"}
    }

    set t .dsk_find_files
    $t.dlb config -list {}
    $t.bSearch.button config -state disabled
    $t.llb config -text "Looking for matches..."
    set matches [dsk_bgexec $cmd "Searching matches..."]
    catch {$t.bSearch.button config -state normal} ;# button may not exist
    if {$matches != "break"} {
	dsk_find_files ;# make sure dialog is visible
	set ml [split $matches \n]
	set ll [llength $ml]
	if {$ll == 1} {
	    $t.llb config -text "Found 1 matching file:"
	} elseif {$ll == 0} {
	    $t.llb config -text "No matches found."
	} else {
	    $t.llb config -text "Found $ll matching files:"
	}
	set lbl ""
	foreach file $ml {
	    if [file exists $file] {
		lappend lbl [file dirname $file]/[subst \
			[lindex [dsk_ls -l -o $file] 0]]
	    }
	}
	$t.dlb config -list $lbl
    } else {
	catch {$t.llb config -text "Search abandoned."}
    }
}


#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_find_annotation
# Args:		none
# Returns: 	""
# Desc:		Creates a window for searching an annotation.
# Side-FX:	none
#

if ![info exists tkdesk(geometry,dsk_find_annotation)] {
    set tkdesk(geometry,dsk_find_annotation) ""
}

set dsk_anno(case_sensitive) 0

proc dsk_find_annotation {} {
    global tkdesk tkdesk_anno

    set t .dsk_find_annotation
    if [winfo exists $t] {
	cb_raise $t
	return
    }

    toplevel $t
    wm withdraw $t
    frame $t.fe -bd 1 -relief raised
    pack $t.fe -fill x

    frame $t.f1
    pack $t.f1 -in $t.fe -fill x \
	    -padx $tkdesk(pad) -pady $tkdesk(pad)
    
    frame $t.fh1
    pack $t.fh1 -in $t.f1 -fill x
    label $t.le -text "Annotation to seach for (regexp):" -anchor w
    pack $t.le -in $t.fh1 -side left
    
    checkbutton $t.cbCase -text "Case sensitive" -relief flat \
	    -variable dsk_anno(case_sensitive)
    pack $t.cbCase -in $t.fh1 -side right

    entry $t.eAnno -bd 2 -relief sunken
    pack $t.eAnno -in $t.f1 -fill x -ipady 2 \
	    -padx $tkdesk(pad) -pady $tkdesk(pad)
    bind $t.eAnno <Return> "$t.bSearch.button flash ; $t.bSearch.button invoke"

    frame $t.fb
    pack $t.fb -in $t.f1 -fill x
    
    cb_button $t.bSearch -text " Search " -default 1 -command dsk_anno_find
    pack $t.bSearch -in $t.fb -side left -padx $tkdesk(pad) -pady $tkdesk(pad)
    button $t.bBrowse -text " Browse " -command "dsk_anno_browse"
    button $t.bClose -text "  Close  " -command \
	    "set tkdesk(geometry,dsk_find_annotation) \[wm geometry $t\] ;\
	    destroy $t"
    pack $t.bBrowse $t.bClose -in $t.fb -side left \
	    -padx $tkdesk(pad) -pady $tkdesk(pad) -ipady 1

    frame $t.flb -bd 1 -relief raised
    pack $t.flb -fill both -expand yes
    frame $t.f2
    pack $t.f2 -in $t.flb -fill both -expand yes -pady $tkdesk(pad)
    
    label $t.llb -text "Matching files:" -anchor w
    pack $t.llb -in $t.f2 -anchor w -padx $tkdesk(pad) 
    
    pack [_dsk_find_lb $t.dlb] -in $t.f2 -fill both -expand yes


    bind $t <Any-Enter> "focus $t.eAnno"

    wm title $t "Find Annotation"
    wm minsize $t 10 1
    dsk_place_window $t dsk_find_annotation 24x5 1
    wm protocol $t WM_DELETE_WINDOW "$t.bClose invoke"
    wm deiconify $t
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_anno_find
# Args:		none
# Returns: 	""
# Desc:		Searches in all annotated files for the annotation in the
#               entry widget.
# Side-FX:	none
#

proc dsk_anno_find {} {
    global tkdesk_anno dsk_anno

    if ![info exists tkdesk_anno] {
	dsk_bell
	cb_info "No file has been annotated yet."
	return
    }

    dsk_busy
    set t .dsk_find_annotation
    set re [$t.eAnno get]
    if {$re != ""} {
	set mfiles ""
	foreach afile [array names tkdesk_anno] {
	    if ![file exists $afile] {
		unset tkdesk_anno($afile)
		continue
	    }
	    if !$dsk_anno(case_sensitive) {
		if [regexp -nocase $re $tkdesk_anno($afile)] {
		    lappend mfiles $afile
		}
	    } else {
		if [regexp $re $tkdesk_anno($afile)] {
		    lappend mfiles $afile
		}
	    }
	}
	#$t.lbFiles.lbox delete 0 end
	#eval $t.lbFiles.lbox insert end $mfiles
	if {$mfiles != ""} {
	    set lbl ""
	    foreach file $mfiles {
		lappend lbl [file dirname $file]/[subst \
			[lindex [dsk_ls -l -o $file] 0]]
	    }
	    $t.dlb config -list $lbl
	} else {
	    dsk_bell
	    cb_info "No match."
	}
    }
    dsk_lazy
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_anno_browse
# Args:		none
# Returns: 	""
# Desc:		Fills a text window with all file annotations.
# Side-FX:	none
#

proc dsk_anno_browse {} {
    global tkdesk_anno

    if ![info exists tkdesk_anno] {
	dsk_bell
	cb_info "No file has been annotated yet."
	return
    }

    set afiles ""
    foreach afile [array names tkdesk_anno] {
	if ![file exists $afile] {
	    unset tkdesk_anno($afile)
	    continue
	}
	append afiles "$afile:\n"
	append afiles "$tkdesk_anno($afile)\n\n"
    }

    if {$afiles != ""} {
	dsk_editor string $afiles
    } else {
	dsk_bell
	cb_info "No file has been annotated yet."
    }
}

# ----------------------------------------------------------------------------
# dsk_find_lb name:
# Creates a dsk_Listbox for the find dialogs with name $name.
#
proc _dsk_find_lb {name} {
    global tkdesk

    catch {$name delete}
    
    dsk_Listbox $name -width 10 -height 2 -font $tkdesk(font,file_lbs) \
	    -bg $tkdesk(color,filelb_background)
    $name-frame config -relief flat
    $name.text config -tabs {465 right 480 left 575 left  635 left 695 \
	    left 775 left 795 left}
    
    bind $name.text <Any-Double-1> "_dsk_find_lb_open $name %s"
    bind $name.text <3> "[bind $name.text <1>];\
	    _dsk_find_lb_popup $name %X %Y"

    blt_drag&drop source $name.text config \
	    -button 2 -packagecmd "find_dd_pkgcmd $name" \
	    -rejectfg red -selftarget 0 -send {file text} \
	    -tokenanchor nw \
	    -tokencursor "@$tkdesk(library)/images/hand.xbm \
	    $tkdesk(library)/images/hand.mask.xbm \
	    black wheat"

    blt_drag&drop source $name.text handler \
	    file dd_send_file text dd_send_text

    bind $name.text <Shift-Button-2> "[bind Text <Button-2>]"
    bind $name.text <Shift-B2-Motion> "[bind Text <B2-Motion>]"
    bind $name.text <ButtonPress-2> "
        $name _sel_for_dd @%x,%y
        $name _dd_start %X %Y
        [bind $name.text <ButtonPress-2>]
    "
    
    bind $name.text <Control-ButtonPress-2> \
	    "[bind $name.text <ButtonPress-2>]"
    
    bind $name.text <Control-B2-Motion> \
	    "set tkdesk(file_lb,control) 1 ;\
	    [bind $name.text <B2-Motion>] ;\
	    _dsk_find_dragcmd"
    
    bind $name.text <Control-ButtonRelease-2> \
	    "set tkdesk(file_lb,control) 1 ;\
	    [bind $name.text <ButtonRelease-2>]
    dsk_desktop_drop %X %Y \[_dsk_find_get_sel $name\]
    focus -force .
    "

    bind $name.text <ButtonRelease-2> "
    set tkdesk(file_lb,control) 0
    [bind $name.text <ButtonRelease-2>]
    dsk_desktop_drop %X %Y \[_dsk_find_get_sel $name\]
    focus -force .
    "

    bind $name.text <B2-Motion> \
	    "set tkdesk(file_lb,control) 0;\
	    [bind $name.text <B2-Motion>] ;\
	    _dsk_find_dragcmd"

    bind $name.text <B2-KeyRelease-Control_L> \
	    "set tkdesk(file_lb,control) 0; \
	    _dsk_find_dragcmd"
    
    bind $name.text <B2-KeyPress-Control_L> \
	    "set tkdesk(file_lb,control) 1; \
	    _dsk_find_dragcmd"
    
    return $name
}

proc _dsk_find_dragcmd {} {
    global tkdesk

    set token $tkdesk(dd_token_window)
    if $tkdesk(quick_dragndrop) {
	if $tkdesk(file_lb,control) {
	    $token.label config -text "Copying:"		
	} else {
	    $token.label config -text "Moving:"
	}
	update idletasks
    }
}
    
# binding for double click:
proc _dsk_find_lb_open {dlb shift} {
    global tkdesk
    
    set tkdesk(file_lb,control) [expr $shift & 4]
    set sel [$dlb select get]
    if {$sel != ""} {
	set tmpfile [lindex [lindex [$dlb get] $sel] 0]
	if {$tmpfile != ""} {
	    dsk_open $tkdesk(active_viewer) $tmpfile
	}
    }
}

# binding for right click:
proc _dsk_find_lb_popup {dlb x y} {
    
    set sel [$dlb select get]
    if {$sel != ""} {
	set tmpfile [lindex [lindex [$dlb get] $sel] 0]
	if {$tmpfile != ""} {
	    dsk_popup $dlb $tmpfile $x $y dir
	}
    }
}

# binding for right click:
proc _dsk_find_get_sel {dlb} {

    set files ""
    set sel [$dlb select get]
    if {$sel != ""} {
	foreach s $sel {
	    set tmpfile [lindex [lindex [$dlb get] $s] 0]
	    if {$tmpfile != ""} {
		lappend files $tmpfile
	    }
	}
    }
    return $files
}

# package cmd for find's d&d:
proc find_dd_pkgcmd {dlb token} {
    global tkdesk

    set tkdesk(dd_token_window) $token
    set fl [$dlb select get]
    set flist ""
    set list [$dlb get]
    foreach f $fl {
	lappend flist [lindex [lindex $list $f] 0]
    }

    set nfiles 0
    set ndirs 0
    foreach file $flist {
        if [file isdirectory $file] {
    	incr ndirs
        } else {
    	incr nfiles
        }
    }

    catch "destroy $token.label"	
    catch "destroy $token.lFiles"
    catch "destroy $token.lDirs"
    if $tkdesk(quick_dragndrop) {
        label $token.label -text "Moving:" \
    	    -font -*-helvetica-medium-r-*-*-10-*-*-*-*-*-*-*
        pack $token.label
    }
    if {$ndirs > 0} {
        if {$ndirs == 1} {
        	label $token.lDirs -text " 1 Directory "
        } else {
        	label $token.lDirs -text " $ndirs Directories "
        }
        pack $token.lDirs -anchor w
    }
    if {$nfiles > 0} {
        if {$nfiles == 1} {
        	label $token.lFiles -text " 1 File "
        } else {
        	label $token.lFiles -text " $nfiles Files "
        }
        pack $token.lFiles -anchor w
    }

    catch "wm deiconify $token"
    #update
    return $flist
}

# ===========================================================================
# ---------------------------------------------------------------------------
# bookmark cmd ?args ...?
# Manages the bookmarks menu and the bookmarks list.
#
proc dsk_bookmark {cmd args} {
    global tkdesk env

    switch $cmd {
	menu {
	    set m [lindex $args 0]
	    catch {$m delete 0 last}
	    $m add command -label "Add Bookmark" -underline 0 \
		    -command "dsk_bookmark add"
	    if {[llength $tkdesk(bookmarks)] > 0} {
		$m add separator
	    }

	    set bms [lsort $tkdesk(bookmarks)]
	    foreach bm $bms {
		$m add command -label "$bm" -font $tkdesk(font,file_lbs) \
			-command "dsk_open \$tkdesk(active_viewer) \"$bm\""
	    }
	    
	    $m add separator
	    $m add cascade -label "Remove Bookmark" -menu $m.rb
	    catch {destroy $m.rb}
	    menu [set m $m.rb]

	    foreach bm $bms {
		$m add command -label "$bm" -font $tkdesk(font,file_lbs) \
			-command "dsk_bookmark remove \"$bm\""
	    }
	}
	add {
	    if {$args == ""} {
		set files [_make_fnames_safe]
	    } else {
		set files $args
	    }
	    if {$files == ""} {
		cb_info "Please select one or more files first."
		return
	    }
	    foreach file $files {
		set file [subst $file]
		if {[string first $env(HOME) $file] == 0} {
		    set file [string_replace $file $env(HOME) ~]
		}
		lappend tkdesk(bookmarks) $file
	    }
	}
	remove {
	    set bm [lindex $args 0]
	    set nr [lsearch $tkdesk(bookmarks) $bm]
	    if {$nr > -1} {
		set tkdesk(bookmarks) \
			[lreplace $tkdesk(bookmarks) $nr $nr]
	    }
	}
    }
}



