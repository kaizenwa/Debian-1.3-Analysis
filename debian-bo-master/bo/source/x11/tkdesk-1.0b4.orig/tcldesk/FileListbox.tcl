# =============================================================================
#
# File:		FileListbox.tcl
# Project:	TkDesk
#
# Started:	09.10.94
# Changed:	09.10.94
# Author:	cb
#
# Description:	Implements a class for a configurable file listbox.
#		Based on dsk_Listbox. Needs the BLT-library for drag&drop.
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

#
# =============================================================================
#
# Class:	dsk_FileListbox
# Desc:		Configurable listbox widget for the displaying of file lists.
#		Consists of a menubutton on top and a dsk_Listbox.
#
# Methods:	config <options>	(see Publics below)
#		refresh			rereads the filelist
#		clear			empties the filelist
#
# Procs:	font <font>		set listbox font
#		showall <bool>		show all files ?
#		topfolders <bool>	put folders on top ?
#		typechar <bool>		append a type character ?
#		tag create <pattern> <color> ?font?
#					create a tag for files that match <p.>
#		tag config <tagname> <color> ?font?
#					configure the given tag
#
# Publics:	dir/directory		directory to display
#		mask			file mask (glob style)
#		width			width of listbox
#		height			height of listbox
#		pad			padding of listbox
#		invert <bool>		invert file lists ?
#		sort <"name"|"size"|"date"|"ext"|"not">    you know what
#		notrivialdirs <bool>	show . and .. ?
#		addsize <bool>		add size to displayed files ?
#		adddate <bool>		add date to displayed files ?
#		long <bool>		long listing ?
#

itcl_class dsk_FileListbox {

    constructor {args} {
	global [set this] tkdesk
	#
	# Create a frame with this object's name
	# (later accessible as $this-frame):
	#
        set class [$this info class]
        ::rename $this $this-tmp-
        ::frame $this -class $class
        ::rename $this $this-frame
        ::rename $this-tmp- $this

	# set config from common vars
	set showall $_showall
	set topfolders $_topfolders
	set typechar $_typechar
	set add_icons $_addicons
	set sort $_sort

	frame $this.fMb
	pack $this.fMb -fill x

	menubutton $this.mb -text "" -bd 1 -relief raised \
		-menu $this.mb.menu -indicatoron 0
	cb_balloonHelp $this.mb "This is a menu button!"

	menu $this.mb.menu
	$this.mb.menu add command -label "Refresh " \
		-command "$this refresh; $this _selstatus"
	$this.mb.menu add command -label "Set Mask... " \
		-command "$this _ask_mask"
	$this.mb.menu add command -label "No Mask " \
		-command "$this config -mask {*}"
	$this.mb.menu add separator
	$this.mb.menu add command -label "Disk Usage " \
		-command "dsk_du \[$this curdir\]"
	$this.mb.menu add command -label "Free Space " \
		-command "dsk_periodic \[list $tkdesk(cmd,df) \[$this cget -directory\]\] 60"
	$this.mb.menu add command -label "Command... " \
		-command "$this command"
	$this.mb.menu add separator
	$this.mb.menu add cascade -label "Sort by ... " -menu $this.mb.menu.smenu
	$this.mb.menu add checkbutton -label " Show all files " \
		-variable [set this](showall) \
		-command "$this config -showall \[set [set this](showall)\]"
	::set [set this](showall) $showall
	$this.mb.menu add checkbutton -label " Invert Listing " \
		-variable [set this](invert) \
		-command "$this config -invert \[set [set this](invert)\]"
	::set [set this](invert) $invert
	$this.mb.menu add separator
	$this.mb.menu add command -label "Open List Window" \
		-command "dsk_FileList .dfl\[dsk_FileList :: id\] \
			-directory \[$this info public directory -value\]"
	
	menu $this.mb.menu.smenu
	$this.mb.menu.smenu add radiobutton -label " Name " \
		-variable [set this](sort) -value name \
		-command "$this config -sort \[set [set this](sort)\]"
	$this.mb.menu.smenu add radiobutton -label " Size " \
		-variable [set this](sort) -value size \
		-command "$this config -sort \[set [set this](sort)\]"
	$this.mb.menu.smenu add radiobutton -label " Date " \
		-variable [set this](sort) -value date \
		-command "$this config -sort \[set [set this](sort)\]"
	$this.mb.menu.smenu add radiobutton -label " Extension " \
		-variable [set this](sort) -value ext \
		-command "$this config -sort \[set [set this](sort)\]"
	$this.mb.menu.smenu add radiobutton -label " Don't sort " \
		-variable [set this](sort) -value not \
		-command "$this config -sort \[set [set this](sort)\]"
	::set [set this](sort) $sort

	pack $this.mb -in $this.fMb -fill x -side left -expand no

	label $this.lFiles -text " 1234 Items" -anchor e \
		-font $tkdesk(font,labels2) -bd 3
	pack $this.lFiles -in $this.fMb -side left -padx $pad

	dsk_Listbox $this.dlb -width $width -height $height -font $lbfont \
		-bg $tkdesk(color,filelb_background) -fg $lbcolor
	$this.dlb.text config -tabs {190 right 205 left 300 left \
		360 left 420 left 500 left 520 left}
	
	if {[winfo depth .] != 1} {
	    foreach t $taglist {
	    	$this.dlb tag config $t \
			-foreground $tags($t,color) -font $tags($t,font)
	    }
	} else {
	    foreach t $taglist {
	    	$this.dlb tag config $t -font $tags($t,font)
	    }
	}

	$this.dlb tag config path -borderwidth 1 -relief raised
	# $this.dlb tag config path -underline 1
	$this.dlb tag lower path
	pack $this.dlb -fill both -expand yes

	bind $this.dlb.text <Any-Enter> {
	    if $tkdesk(focus_follows_mouse) {
		focus [winfo toplevel %W]
	    }
	}

	bind $this.dlb.text <Double-1> "
	    set tkdesk(file_lb,control) 0
	    set tmpsc \$tkdesk(single_click)
	    set tkdesk(single_click) 0
	    set tkdesk(config_nostat) 1
	    [winfo toplevel $this] _dblclick $this \[$this.dlb select get\]
	    set tkdesk(config_nostat) 0
	    catch {set tkdesk(single_click) \$tmpsc}
	    catch {unset tmpsc}
	    break
	"
	bind $this.dlb.text <ButtonRelease-1> \
	    "+if \$tkdesk(single_click) \{
	     set tkdesk(file_lb,control) 0
	     [winfo toplevel $this] _dblclick $this \[$this.dlb select get\]
	    \}
	"
	bind $this.dlb.text <Control-ButtonRelease-1> \
	    "+if \$tkdesk(single_click) \{
	     set tkdesk(file_lb,control) 1
	     [winfo toplevel $this] _dblclick $this \[$this.dlb select get\]
	    \}
	"
	bind $this.dlb.text <Shift-ButtonRelease-1> "$this _selstatus"
	bind $this.dlb.text <Control-Double-1> "
	    set tkdesk(file_lb,control) 1
	    set tmpsc \$tkdesk(single_click)
	    set tkdesk(single_click) 0
	    set tkdesk(config_nostat) 1
	    [winfo toplevel $this] _dblclick $this \[$this.dlb select get\]
	    set tkdesk(config_nostat) 0
	    catch {set tkdesk(single_click) \$tmpsc}
	    catch {unset tmpsc}
	    break
	"

	bind $this.dlb.text <Control-Shift-1> "
	    [bind $this.dlb.text <1>]
	    $this _selmask
	"

	bind $this.dlb.text <3> "
	    [bind $this.dlb.text <1>]
	    update idletasks
	    $this _selstatus
	    [winfo toplevel $this] _popup $this \[$this.dlb select get\] %X %Y"
	bind $this.dlb.text <Control-3> {# nothing}

	#bind $this.dlb.text <Any-ButtonRelease> "+$this _selstatus"
	bind $this.dlb.text <ButtonRelease-1> "+$this _selstatus"

	if {[winfo depth .] != 1} {
	    set cc wheat
	} else {
	    set cc white
	}

	blt_drag&drop source $this.dlb.text config \
		-button 2 -packagecmd "$this _dd_pkgcmd" -rejectfg red \
		-selftarget 1 -send {file text} \
		-tokenanchor nw \
		-tokencursor "@$tkdesk(library)/images/hand.xbm \
			$tkdesk(library)/images/hand.mask.xbm \
			black $cc"

	blt_drag&drop source $this.mb config \
		-button 2 -packagecmd "$this _dd_pkgcmd_mb" -rejectfg red \
		-selftarget 1 -send {file text} \
		-tokenanchor nw \
		-tokencursor "@$tkdesk(library)/images/hand.xbm \
			$tkdesk(library)/images/hand.mask.xbm \
			black $cc"

	blt_drag&drop source $this.dlb.text handler \
		file dd_send_file text dd_send_text

	blt_drag&drop source $this.mb handler \
		file dd_send_file text dd_send_text

	blt_drag&drop target $this.dlb.text handler \
		file "$this _dd_drophandler"

	blt_drag&drop target $this.mb handler \
		file "$this _dd_drophandler $this.mb"

	#bind $this.dlb.text <Shift-Button-2> "[bind Text <Button-2>]"
	#bind $this.dlb.text <Shift-B2-Motion> "[bind Text <B2-Motion>]"

	bind $this.dlb.text <ButtonPress-2> "
	    $this.dlb _sel_for_dd @%x,%y
	    $this.dlb _dd_start %X %Y
	    $this _selstatus
	    [bind $this.dlb.text <ButtonPress-2>]
	    focus %W
	"
	bind $this.mb <ButtonPress-2> "
	    [bind $this.mb <ButtonPress-2>]
	    focus $this.dlb.text
	"

	bind $this.dlb.text <Control-ButtonPress-2> \
		"[bind $this.dlb.text <ButtonPress-2>]"
		
	bind $this.dlb.text <Control-B2-Motion> \
		"set tkdesk(file_lb,control) 1 ;\
		[bind $this.dlb.text <B2-Motion>] ;\
		$this _dd_dragcmd"
		
	bind $this.dlb.text <Control-ButtonRelease-2> \
		"set tkdesk(file_lb,control) 1 ;\
		[bind $this.dlb.text <ButtonRelease-2>] ;\
		dsk_desktop_drop %X %Y ;\
		focus -force ."

	bind $this.dlb.text <ButtonRelease-2> \
		"set tkdesk(file_lb,control) 0 ;\
		[bind $this.dlb.text <ButtonRelease-2>] ;\
		dsk_desktop_drop %X %Y ;\
		focus -force ."

	bind $this.mb <Control-ButtonRelease-2> \
		"set tkdesk(file_lb,control) 1 ;\
		[bind $this.mb <ButtonRelease-2>] ;\
		dsk_desktop_drop %X %Y $this ;\
		focus -force ."

	bind $this.mb <ButtonRelease-2> \
		"set tkdesk(file_lb,control) 0 ;\
		[bind $this.mb <ButtonRelease-2>] ;\
		dsk_desktop_drop %X %Y $this ;\
		focus -force ."

	bind $this.dlb.text <B2-Motion> \
		"set tkdesk(file_lb,control) 0;\
		[bind $this.dlb.text <B2-Motion>] ;\
		$this _dd_dragcmd"

	bind $this.mb <Control-B2-Motion> \
		"set tkdesk(file_lb,control) 1 ;\
		[bind $this.mb <B2-Motion>] ;\
		$this _dd_dragcmd"
		
	bind $this.mb <B2-Motion> \
		"set tkdesk(file_lb,control) 0;\
		[bind $this.mb <B2-Motion>] ;\
		$this _dd_dragcmd"

	bind $this.dlb.text <B2-KeyRelease-Control_L> \
		"set tkdesk(file_lb,control) 0; \
		$this _dd_dragcmd"
		
	bind $this.dlb.text <B2-KeyPress-Control_L> \
		"set tkdesk(file_lb,control) 1; \
		$this _dd_dragcmd"
		
	$this _build_ls_cmd
	eval config $args
    }

    destructor {
	global [set this]
	
	$this.dlb delete
        #after 10 rename $this-frame {}		;# delete this name
        catch {destroy $this}		;# destroy associated window
	::unset [set this]
    }

    #
    # ----- Methods and Procs -------------------------------------------------
    #

    method config {config} {
    }

    method cget {var} {
	return [set [string trimleft $var -]]
    }

    method curdir {} {
	return $directory
    }

    method refresh {} {
	if {$directory == ""} {
	    return
	}

	set ptag [$this.dlb tag ranges path]
	if {$ptag != ""} {
	    set pname [$this.dlb get $ptag]
	}
	set ypos [lindex [cb_old_sb_get $this.dlb.sb] 2]
	$this config -directory $directory
	$this.dlb _yview $ypos
	if {$ptag != ""} {
	    $this.dlb tag add path [lsearch [$this.dlb get] $pname]
	}
    }

    method clear {} {
	$this.mb config -text "" -state disabled
	catch "pack unpack $this.mb"
	$this.lFiles config -text ""
	$this.dlb config -list {}
	set directory ""
    }

    method tagpath {{dir ""}} {
	global tkdesk

	set index [$this.dlb tag ranges path]
	if {$index != ""} {
	    $this.dlb tag remove path $index
	    if $add_icons {
		#incr index
		$this.dlb.text.l$index config \
			-image [dsk_image $tags(dir,image)]
	    }
	}
	if {$dir == ""} {
	    return
	}

	set fname [file tail [string trimright $dir "/"]]
	if $typechar {
	    set fname $fname/
	}
	set list [$this.dlb get]
	set index [lsearch $list $fname]
	if {$index < 0} {
	    set index [lsearch -glob $list "$fname *"]
	    if {$index < 0} {
		#puts "tagpath: couldn't find $dir"
		return
	    }
	}
	$this.dlb tag add path $index
	#incr index
	if $add_icons {
	    $this.dlb.text.l$index config \
		    -image [dsk_image $path_image]
	}
	# $this.dlb.text yview -pickplace $index
	$this.dlb select clear
    }

    method imginsert {tname lines} {
	global tkdesk
	
	if {$lines == ""} return
	
	if {$tname != ""} {
	    set image $tags($tname,image)
	} else {
	    set image $default_image
	}

	foreach l $lines {
	    label $this.dlb.text.l$l -image [dsk_image $image] -bd 0
	    bind $this.dlb.text.l$l <1> "
	        [winfo toplevel $this] _selecting $this
	        focus [winfo toplevel $this]
	        $this.dlb select clear
	        $this.dlb select $l
	        $this.dlb _sel_first $l
	    "
	    bind $this.dlb.text.l$l <Shift-1> "
	        [winfo toplevel $this] _selecting $this
	        $this.dlb _sel_toggle $l
	        $this.dlb _sel_first $l
	    "
	    bind $this.dlb.text.l$l <ButtonRelease-1> \
		    [bind $this.dlb.text <ButtonRelease-1>]
	    bind $this.dlb.text.l$l <Double-1> \
		    [bind $this.dlb.text <Double-1>]
	    bind $this.dlb.text.l$l <ButtonPress-2> "
	        $this.dlb _sel_for_dd $l
	        $this.dlb _dd_start %X %Y
	        $this _selstatus
	        drag&drop drag $this.dlb.text %X %Y
	        focus $this.dlb.text
	    "
	    bind $this.dlb.text.l$l <Control-ButtonPress-2> \
		    "[bind $this.dlb.text.l$l <ButtonPress-2>]"
	    bind $this.dlb.text.l$l <Control-B2-Motion> \
		"set tkdesk(file_lb,control) 1 ;\
		drag&drop drag $this.dlb.text %X %Y ;\
		$this _dd_dragcmd"
	    bind $this.dlb.text.l$l <B2-Motion> \
		"set tkdesk(file_lb,control) 0 ;\
		drag&drop drag $this.dlb.text %X %Y ;\
		$this _dd_dragcmd"
	    bind $this.dlb.text.l$l <Control-ButtonRelease-2> \
		"set tkdesk(file_lb,control) 1 ;\
		drag&drop drop $this.dlb.text %X %Y ;\
		dsk_desktop_drop %X %Y ;\
		focus -force ."
	    bind $this.dlb.text.l$l <ButtonRelease-2> \
		"set tkdesk(file_lb,control) 0 ;\
		drag&drop drop $this.dlb.text %X %Y ;\
		dsk_desktop_drop %X %Y ;\
		focus -force ."
	    bind $this.dlb.text.l$l <3> "
	        [bind $this.dlb.text.l$l <1>]
	        update idletasks
	        $this _selstatus
	        [winfo toplevel $this] _popup $this \
			\[$this.dlb select get\] %X %Y
	    "
	    bind $this.dlb.text.l$l <Control-3> {# nothing}

	    $this.dlb.text window create [expr $l + 1].0 \
		    -window $this.dlb.text.l$l -padx 2
	}
    }

    method set_mask {m} {
	set mask $m
    }

    method _ask_mask {} {
	global m

	set m $_last_mask
	cb_readString "Set mask for this file-list to:" m "Set mask"
	if {$m != ""} {
	    set _last_mask $m
	    $this config -mask $m
	}
    }

    method command {{cmd ""}} {
	global tkdesk_filelb_cmd dsk_exec

	if {$cmd == ""} {
	    cb_readString "Enter Command (use %A for selected files):" \
		    tkdesk_filelb_cmd "Execute here"
	    set cmd $tkdesk_filelb_cmd
	}
	if {$cmd != ""} {
	    update
	    dsk_busy
	    set dsk_exec(dir) $directory
	    set dsk_exec(shell) 1
	    set cmd [_expand_pc $cmd]
	    set err [catch {eval $cmd} errmsg]
	    dsk_lazy
	    if $err {
		cb_error $errmsg
	    }
	}
    }

    method _build_ls_cmd {} {
	# assembles the appropriate options for dsk_ls

	set options {-l -t}	;# this is always needed

	if {$showall} {
	    lappend options -a
	}
	if $invert {
	    lappend options -i
	}
	if $notrivialdirs {
	    lappend options -p
	}
	if {$topfolders} {
	    lappend options -f
	}
	switch $sort {
	    name	{lappend options -s name}
	    size	{lappend options -s size}
	    date	{lappend options -s date}
	    ext		{lappend options -s ext}
	    not		{lappend options -s not}
	}

	set ls_cmd "dsk_ls $options"
    }

    method _selstatus {} {
	global tkdesk

	if {$viewer != ""} {
	    $viewer selstatus
	}
    }

    method _dd_pkgcmd {token} {
	global tkdesk

	set tkdesk(dd_token_window) $token
	set flist [[winfo toplevel $this] select get]

	catch "destroy $token.label"	
	catch "destroy $token.lFiles"
	catch "destroy $token.lDirs"
	if $tkdesk(quick_dragndrop) {
	    label $token.label -text "Moving:" \
		    -font -*-helvetica-medium-r-*-*-10-*-*-*-*-*-*-*
	    pack $token.label
	}

	set ll [llength $flist]
	if {$ll == 1} {
	    label $token.lFiles -text " 1 Item "
	} else {
	    label $token.lFiles -text " $ll Items "
	}
	pack $token.lFiles -anchor w

	catch "wm deiconify $token"
	#focus $token
	return $flist
    }

    method _dd_pkgcmd_mb {token} {
	global tkdesk

	set tkdesk(dd_token_window) $token
	if {$directory != "/"} {
	    set flist [string trimright $directory /]
	} else {
	    set flist $directory
	}

	catch "destroy $token.label"	
	catch "destroy $token.lFiles"
	catch "destroy $token.lDirs"
	if $tkdesk(quick_dragndrop) {
	    label $token.label -text "Moving:" \
		    -font -*-helvetica-medium-r-*-*-10-*-*-*-*-*-*-*
	    pack $token.label
	}

	label $token.lFiles -text " 1 Item "
	pack $token.lFiles -anchor w

	catch "wm deiconify $token"
	#focus $token
	return $flist
    }

    method _dd_dragcmd {} {
	global tkdesk

	set err [catch {set token $tkdesk(dd_token_window)}]
	if $err return
	if $tkdesk(quick_dragndrop) {
	    if $tkdesk(file_lb,control) {
		$token.label config -text "Copying:"		
	    } else {
		$token.label config -text "Moving:"
	    }
	    update idletasks
	}
    }

    method _dd_drophandler {{win ""}} {
	global DragDrop tkdesk

	if ![$this.dlb _dd_end [winfo pointerx .] [winfo pointery .] 6] {
	    return
	}
	
	if {$win == ""} {
	    set win $this.dlb.text
	}

	if {$win == "$this.dlb.text"} {
	    set xy [blt_drag&drop location]
	    set x  [expr [lindex $xy 0]-[winfo rootx $this.dlb.text]]
	    set y  [expr [lindex $xy 1]-[winfo rooty $this.dlb.text]]

	    catch "wm withdraw $tkdesk(dd_token_window)"
	    update

	    set f [$this.dlb.text get "@$x,$y linestart" \
		    "@$x,$y + 1 lines linestart"]
	    set file [string trimright [lindex [split $f \t] 0]]
	    #puts $file
	    if $tkdesk(append_type_char) {
		set file [dsk_striptc $file]
	    }
	    set dest $directory

	    if {$file != ""} {
		#puts $dest$file
		if [file isdirectory $dest$file] {
		    set dest $dest$file/
		}
	    }
	} else {
	    # drop on menubutton above text widget
	    set dest $directory
	}

	if ![file writable $dest] {
	    dsk_errbell
	    if {$dest != ""} {
	    	cb_error "You don't have write permission for this directory!"
	    } else {
		cb_error "This listbox is not a valid target (since it's empty)."
	    }
	    return
	}

	#dsk_debug "Rec.: $DragDrop(file)"
	#dsk_debug "dest: $dest"
	if {[string first "$tkdesk(configdir)/.trash/" $dest] == -1} {
	    dsk_ddcopy $DragDrop(file) $dest
	} else {
	    if !$tkdesk(quick_dragndrop) {
		dsk_delete $DragDrop(file)
	    } else {
		if {!$tkdesk(file_lb,control) && !$tkdesk(really_delete)} {
		    dsk_ddcopy $DragDrop(file) $dest
		} else {
		    if {[cb_yesno "Really deleting! Are you sure that this is what you want?"] == 0} {
			dsk_sound dsk_really_deleting
			dsk_bgexec "$tkdesk(cmd,rm) $DragDrop(file)" \
				"Deleting [llength $DragDrop(file)] File(s)..."
			dsk_refresh "$DragDrop(file) $dest"
		    }
		}		    
	    }
	}
    }

    method _dragscroll {mode} {
	if {$mode == "press"} {
	    eval [bind Text <Button-2>]
	} else {
	    eval [bind Text <B2-Motion>]
	}
    }

    method _mb_label {} {
	global tkdesk

	set dn [file tail [string trimright $directory /] ]
	set l $dn/$mask

	set sl [string length $l]
	set d [expr $tkdesk(file_lb,minwidth) - $sl]
	if {$d < 6} {
	    # leave space for "..."
	    set dlc [expr [string length $dn] - 4]
	    set dlc [expr $dlc - $sl + $tkdesk(file_lb,minwidth) - 6]
	    set l "[string range $dn 0 $dlc].../$mask"
	}
	return $l
    }

    method _selmask {} {
	$this.dlb select name *[file extension [lindex [lindex [$this.dlb get] [$this.dlb select get]] 0]]
	$this _selstatus
    }

    proc font {fnt} {
	foreach obj [itcl_info objects -class dsk_FileListbox] {
	    $obj.dlb.text config -font $fnt
	}
	set lbfont $fnt
    }

    proc color {col} {
	foreach obj [itcl_info objects -class dsk_FileListbox] {
	    $obj.dlb.text config -foreground $col
	}
	set lbcolor $col
    }

    proc defimage {img} {
	set default_image $img
    }

    proc pathimage {img} {
	set path_image $img
    }

    proc showall {val} {
	set _showall $val
	foreach object [itcl_info objects -class dsk_FileListbox] {
	    $object config -dont_refresh 1
	    $object config -showall $val
	    $object config -dont_refresh 0
	}
    }

    proc topfolders {val} {
	set _topfolders $val
	foreach object [itcl_info objects -class dsk_FileListbox] {
	    $object config -topfolders $val
	}
    }

    proc typechar {val} {
	set _typechar $val
	foreach object [itcl_info objects -class dsk_FileListbox] {
	    $object config -typechar $val
	}
    }

    proc addicons {val} {
	set _addicons $val
	foreach object [itcl_info objects -class dsk_FileListbox] {
	    $object config -add_icons $val
	}
    }

    proc sort {val} {
	set _sort $val
	foreach object [itcl_info objects -class dsk_FileListbox] {
	    #$object config -dont_refresh 1
	    $object config -sort $val
	    #$object config -dont_refresh 0
	}
    }

    proc tag {cmd args} {
	# syntax: tag create <pattern> <color> ?font?   returns tag name
	#         tag config <tagname> <color> ?font?
	#         tag reset

	switch -glob -- $cmd {
	    create	{
		set tname filetag[incr numfiletags]
		set pat [lindex $args 0]
		if {[llength $pat] > 1} {
		    if {[lindex $pat 0] == "dir"} {
			set tags($tname,match) "[lindex $pat 1]/"
		    } else {
			set tags($tname,match) "[lindex $pat 1]\\*"
		    }
		} else {
		    set tags($tname,match) "${pat}_"
		}
		if {[winfo depth .] != 1} {
		    set tags($tname,color) [lindex $args 1]
		} else {
		    set tags($tname,color) "black"
		}
		if {[llength $args] > 2} {
		    set f  [lindex $args 2]
		    if {$f != ""} {
			set tags($tname,font) $f
		    } else {
			set tags($tname,font) $lbfont
		    }
		} else {
		    set tags($tname,font) $lbfont
		}
		if {[llength $args] > 3} {
		    set f  [lindex $args 3]
		    if {$f != ""} {
			set tags($tname,image) [lindex $args 3]
		    } else {
			set tags($tname,image) $default_image
		    }
		} elseif ![info exists tags($tname,image)] {
		    set tags($tname,image) $default_image
		}
		#eval lappend taglist $tname
		# Comment the previous line and uncomment the following two
		# to let file_tags overrule type tags.
		set di [lsearch $taglist dir]
		set taglist [linsert $taglist $di $tname]
		foreach this \
			[itcl_info objects -class dsk_FileListbox] {
		    $this.dlb tag config $tname \
			    -foreground $tags($tname,color) \
			    -font $tags($tname,font)
		}
		return $tname
	    }
	    config* {
		set tname [lindex $args 0]
		if {[winfo depth .] != 1} {
		    set tags($tname,color) [lindex $args 1]
		} else {
		    set tags($tname,color) "black"
		}
		if {[llength $args] > 2} {
		    set f  [lindex $args 2]
		    if {$f != ""} {
			set tags($tname,font) $f
		    } else {
			set tags($tname,font) $lbfont
		    }
		} else {
		    set tags($tname,font) $lbfont
		}
		if {[llength $args] > 3} {
		    set tags($tname,image) [lindex $args 3]
		} elseif ![info exists tags($tname,image)] {
		    set tags($tname,image) $default_image
		}
		foreach this \
			[itcl_info objects -class dsk_FileListbox] {
		    $this.dlb tag config $tname \
			    -foreground $tags($tname,color) \
			    -font $tags($tname,font)
		}
	    }
	    reset {
		set numfiletags 0
		set taglist "dir exec sym symdir symexec"
	    }
	    default {
		error "unknown tag cmd: $cmd"
	    }
	    get {
		set ti {}
		foreach t $taglist {
		    lappend ti [list $t $tags($t,match) \
			    $tags($t,color) $tags($t,font) $tags($t,image)]
		}
		return $ti
	    }
	}
    }

    proc print_ready {val} {
	set print_ready $val

    }

    #
    # ----- Variables ---------------------------------------------------------
    #

    #
    #  Options for dsk_ls:
    #

    protected ls_cmd "dsk_ls -l -t -p"

    public invert {0} {
	$this _build_ls_cmd
	$this refresh
    }

    public sort {name} {
	global [set this]
	
	$this _build_ls_cmd
	if !$dont_refresh {
	    $this refresh
	}
	::set [set this](sort) $sort
    }

    public notrivialdirs {0} {
	$this _build_ls_cmd
	$this refresh
    }

    public showall {0} {
	global [set this]

	$this _build_ls_cmd
	if !$dont_refresh {
	    $this refresh
	}
	::set [set this](showall) $showall
    }

    public topfolders {0} {
	$this _build_ls_cmd
	if {$toplevel != ""} {
	    $this refresh
	}
    }

    public typechar {0}	{ #  this is not really for dsk_ls
	$this _build_ls_cmd
	if {$toplevel != ""} {
	    $this refresh
	}
    }

    public add_icons {1} {
	if {$toplevel != ""} {
	    $this refresh
	}
    }

    public dont_refresh 0

    common _showall 0
    common _topfolders 0
    common _typechar 0
    common _addicons 0
    common _sort name

    #
    #  Other
    #

    public toplevel "" {	# a hack for the file list windows
	global [set this]

	if {$toplevel != ""} {
	    $this.mb.menu delete 15	;# delete separator
	    $this.mb.menu delete 16	;# delete " Open Window "

	    # $this.mb.menu add checkbutton -label " Show all files " \
	    #	-variable [set this](showall) \
	    #	-command "$this config -showall \[set [set this](showall)\]"
	    # set [set this](showall) $showall
	    $this.mb.menu add checkbutton -label " Folders on top " \
		-variable [set this](topfolders) \
	      -command "$this config -topfolders \[set [set this](topfolders)\]"
	    ::set [set this](topfolders) $topfolders
	    #$this.mb.menu add checkbutton -label " Append type char " \
	    #	 -variable [set this](typechar) \
	    #	 -command "$this config -typechar \[set [set this](typechar)\]"
	    #::set [set this](typechar) $typechar
	    $this.mb.menu add checkbutton -label " Add icons " \
		-variable [set this](add_icons) \
	        -command "$this config -add_icons \[set [set this](add_icons)\]"
	    ::set [set this](add_icons) $add_icons

	    $this.mb.menu add separator
#	    $this.mb.menu add command -label "Open Window " \
#		-command "dsk_FileList .dfl\[dsk_FileList :: id\] \
#			-directory \[$this info public directory -value\]"
	    $this.mb.menu add command -label "Open Browser " \
		-command "dsk_FileViewer .fv\[dsk_FileViewer :: id\] \
			-directory \[$this info public directory -value\]"
	}
    }

    public dir {} {
	$this config -directory $dir
    }

    public directory {} {
	global tkdesk
	
	if {$directory == ""} return
	dsk_busy

	if [info exists tkdesk(action_on_open)] {
	    set newd [string trimright $directory /]
	    set dmatch 0
	    foreach d $tkdesk(action_on_open) {
		set dp [lindex $d 0]  ;# list of patterns
		set da [lindex $d 1]  ;# Tcl script to execute
		foreach p $dp {
		    set p [string trimright [cb_tilde $p expand] /]
		    if [string match $p $newd] {
			#cd [dsk_active dir]
			set err [catch {eval [_expand_pc $da]} errmsg]
			#cd ~
			if $err {
			    dsk_errbell
			    cb_error $errmsg
			}
			set dmatch 1
			break
		    }
		}
		if $dmatch break
	    }
	}

	if {$viewer != ""} {	
	    $viewer status "Reading files in $directory ..."
	}

	# resolve ~
	if {[string index $directory 0] == "~"} {
	    set directory [glob $directory]
	}

	# read the filelist and modification time
	#dsk_debug "Reading dir: $directory (with $ls_cmd)"
	set flist [eval $ls_cmd [list $directory]]
	set mtime [file mtime $directory]

	#
	# Preprocessing of filelist:
	#   This sets the list "list", which equals $flist without the type
	#   character, if $typechar is false. It also applies the currently
	#   selected file mask $mask. Additionally it sets the array mt for
	#   tags that matched. The index name for an entry in mt is the tag's
	#   name, and the value is list of numbers that represent indices
	#   in the file list that matched this tag.
	#

	dsk_debug -nonewline "Processing file list ($directory) ... "
	set list ""
	catch {unset mt} 	;# array for matching tags

	dsk_ppflist
	
	dsk_debug "Done."

	if {$viewer != "" && $print_ready} {	
	    $viewer status "Ready."
	}

    	dsk_lazy
    }

    public mask {*} {
	$this config -directory $directory
	if {$toplevel == ""} {	
	    [winfo toplevel $this] status "Ready."
	}
    }

    public width {10} {
	$this.dlb config -width $width
    }

    public height {10} {
	$this.dlb config -height $height
    }

    public pad {4} {
	$this.dlb config -pad $pad
    }

    public viewer {} {
	bind $this.dlb.text <1> "
		$viewer _selecting $this
		[bind $this.dlb.text <1>]
		bind $this.dlb.text <Any-ButtonRelease> \"$this _selstatus\""
	bind $this.dlb.text <Shift-1> "
		$viewer _selecting $this
		[bind $this.dlb.text <Shift-1>]
		bind $this.dlb.text <Any-ButtonRelease> \"$this _selstatus\""
	bind $this.dlb.text <Control-1> "
		$viewer _selecting $this
		[bind $this.dlb.text <Control-1>]
		bind $this.dlb.text <Any-ButtonRelease> \"$this _selstatus\""
	bind $this.dlb.text <2> "$viewer _selecting $this
		[bind $this.dlb.text <2>]"
	bind $this.dlb.text <3> "$viewer _selecting $this
		[bind $this.dlb.text <3>]"
    }

    protected _last_mask {*}
    protected _last_size_stat ""
    protected mtime 0
    
    common default_image ficons16/file.xpm
    common path_image ficons16/diropen.xpm
    common lbfont {-*-courier-medium-r-*-*-*-120-*-*-*-*-*-*}
    common lbcolor black
    common print_ready 1

    common taglist {}
    lappend taglist dir exec sym symdir symexec

    common numfiletags 0
    common tags

    set tags(dir,match) {*/}
    set tags(exec,match) {*\*}
    set tags(sym,match) {*@}
    set tags(symdir,match) {*-}
    set tags(symexec,match) {*+}

    set tags(dir,color) black
    set tags(exec,color) black
    set tags(sym,color) black
    set tags(symdir,color) black
    set tags(symexec,color) black

    set tags(dir,font) {-*-courier-medium-r-*-*-*-120-*-*-*-*-*-*}
    set tags(exec,font) {-*-courier-medium-r-*-*-*-120-*-*-*-*-*-*}
    set tags(sym,font) {-*-courier-medium-r-*-*-*-120-*-*-*-*-*-*}
    set tags(symdir,font) {-*-courier-medium-r-*-*-*-120-*-*-*-*-*-*}
    set tags(symexec,font) {-*-courier-medium-r-*-*-*-120-*-*-*-*-*-*}
    
    set tags(dir,image) ficons16/dir.xpm
    set tags(exec,image) ficons16/exec.xpm
    set tags(sym,image) ficons16/sym.xpm
    set tags(symdir,image) ficons16/symdir.xpm 
    set tags(symexec,image) ficons16/symexec.xpm 
}

