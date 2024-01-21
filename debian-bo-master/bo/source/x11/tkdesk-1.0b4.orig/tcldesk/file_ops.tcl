# =============================================================================
#
# File:		file_ops.tcl
# Project:	TkDesk
#
# Started:	22.10.94
# Changed:	22.10.94
# Author:	cb
#
# Description:	Implements classes and procs for file operations like
#		copy, move, delete, file info and disk usage (and others).
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
#	proc dsk_bgexec {cmd label}
#	itcl_class dsk_DiskUsage
#	proc dsk_du {dir}
#	itcl_class dsk_FileInfo
#	proc dsk_fileinfo {}
#	proc dsk_create {type}
#	proc dsk_select {cmd}
#	proc dsk_copy {{files ""} {dest ""}}
#	proc dsk_delete {{files ""}}
#
# =============================================================================

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_bgexec
# Args:		cmd 	command to execute
#		label	label for "stand by" window
# Returns: 	Result of cmd of "break".
# Desc:		Executes dsk_bgexec in the background and waits for the
#		command to be finished. Also displays a "stand by" window,
#		which contains a button to interrupt the execution.
# Side-FX:	none
#

global dsk_bgexec
set dsk_bgexec(cnt) 0
set tkdesk(bgexec,working) 0
if ![info exists tkdesk(geometry,dsk_bgexec)] {
    set tkdesk(geometry,dsk_bgexec) ""
}

proc dsk_bgexec {cmd label} {
    global dsk_bgexec tkdesk

    set pad 8

    # ---- first create the "stand by" window

    set cnt [incr dsk_bgexec(cnt)]
    set t ".dsk_bgexec$cnt"
    catch "destroy $t"
    toplevel $t
    wm withdraw $t

    frame $t.fl -bd 1 -relief raised
    pack $t.fl -fill x

    label $t.label -text "$label" \
		-font -*-helvetica-bold-r-*-*-14-*-*-*-*-*-*-*
    pack $t.label -in $t.fl -side left -padx $pad -pady $pad

    frame $t.fb -bd 1 -relief raised
    pack $t.fb -fill x

    button $t.bBreak -text "  Stop  " \
	    -command "set dsk_bgexec(out$cnt) break ;\
	    set dsk_bgexec(stat$cnt) {} ;\
	    destroy .dsk_bgexec$cnt"
    pack $t.bBreak -in $t.fb -side top -padx $pad -pady $pad -ipady 2

    #grab $t
    wm title $t "Background #$cnt"
    wm protocol $t WM_DELETE_WINDOW {# ignore}
    if {$tkdesk(geometry,dsk_bgexec) == ""} {
	cb_centerToplevel $t
    } else {
	set g [split $tkdesk(geometry,dsk_bgexec) +x]
	set vw [winfo vrootwidth .]
	set vh [winfo vrootheight .]
	set x [lindex $g 2]
	while {$x < 0} {incr x $vw}
	while {$x > $vw} {incr x -$vw}
	set y [lindex $g 3]
	while {$y < 0} {incr y $vh}
	while {$y > $vh} {incr y -$vh}
	wm geometry $t +$x+$y
	wm deiconify $t
    }
    update

    # ---- now execute cmd

    catch "unset dsk_bgexec(cmd$cnt)"
    catch "unset dsk_bgexec(err$cnt)"
    set owd [pwd]
    catch "cd [dsk_active dir]"
    set tkdesk(bgexec,working) 1
    #eval blt_bgexec -errorvar dsk_bgexec(err$cnt) dsk_bgexec(out$cnt) $cmd
    eval blt_bgexec dsk_bgexec(stat$cnt) \
	    -output dsk_bgexec(out$cnt) \
	    -error dsk_bgexec(err$cnt) $cmd &
    tkwait variable dsk_bgexec(stat$cnt)
    
    set tkdesk(bgexec,working) 0
    catch {set tkdesk(geometry,dsk_bgexec) [wm geometry $t]}
    catch "destroy $t"
    if [catch "cd $owd"] {
	# maybe $owd has just been deleted
	cd $tkdesk(startdir)
    }
    incr dsk_bgexec(cnt) -1

    if ![info exists dsk_bgexec(err$cnt)] {
	return $dsk_bgexec(out$cnt)
    } elseif {$dsk_bgexec(err$cnt) != ""} {
	dsk_errbell
	cb_error "$dsk_bgexec(err$cnt)"
    	return "error"
    } else {
	return $dsk_bgexec(out$cnt)
    }
}

#
# =============================================================================
#
# Class:	dsk_DiskUsage
# Desc:		Creates a window for displaying the disk usage
#		of a directory.
#
# Methods:	
# Procs:	
# Publics:	directory	name of directory
#

itcl_class dsk_DiskUsage {

    constructor {args} {
	global tkdesk

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

	frame $this.fl -bd 1 -relief raised
	pack $this.fl -fill x

	label $this.lDir -text ""
	pack $this.lDir -in $this.fl -side left \
		-padx $tkdesk(pad) -pady $tkdesk(pad)

	cb_listbox $this.flb -vscroll 1 -hscroll 1 -lborder 1 -uborder 1 \
		-width 5 -height 1 -font $tkdesk(font,mono) \
		-selectmode single
	$this.flb config -bd 1 -relief raised
	pack $this.flb -fill both -expand yes

	bind $this.flb.lbox <Double-1> {
	    dsk_open_dir [lindex [%W get [%W nearest %y]] 1]
	}

	frame $this.fb -bd 1 -relief raised
	pack $this.fb -fill x

	button $this.bClose -text " Close " -command "$this delete"
	button $this.bRefresh -text " Refresh " -command "$this refresh"
	pack $this.bClose $this.bRefresh -in $this.fb -side left \
		-padx $tkdesk(pad) -pady $tkdesk(pad) -ipady 2

	wm minsize $this 40 16
	#wm geometry $this 40x15
	wm title $this "Disk Usage"
	wm protocol $this WM_DELETE_WINDOW "$this delete"

	eval config $args
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

    method refresh {} {
	$this config -directory $directory
    }

    proc id {} {
	set i $id
	incr id
	return $i
    }

    proc cmds {du sort} {
	global tkdesk

	set cmd(du) $du
	set cmd(sort) $sort
    }

    #
    # ----- Variables ---------------------------------------------------------
    #

    public dir "/" {
	$this config -directory $dir
    }

    public directory "/" {
	#dsk_busy
	dsk_status "Determining disk usage..."

	if {$directory != "/"} {
	    set directory [string trimright $directory "/"]
	}
	$this.lDir config -text "Disk Usage of $directory:"

	# automatically read linked directories
	set err [catch "set d \[file readlink $directory\]"]
	if !$err {
	    set directory $d
	}

	#set du_list [eval exec "$cmd(du) $directory | $cmd(sort)"]
	set du_list [dsk_bgexec "$cmd(du) $directory | $cmd(sort)" \
			"Determining disk usage..."]
	if {$du_list != "break"} {
	    set du_list [cb_lines2list $du_list]
	    $this.flb.lbox delete 0 end
	    foreach d $du_list {
	    	$this.flb.lbox insert end [string trimright [format "%6s %s" \
	    	    [lindex $d 0] [cb_tilde [lindex $d 1] collapse]] "\n"]
	    }
	    $this.flb.lbox config -width 40 \
		    -height [cb_min 15 [llength $du_list]]
	    dsk_place_window $this diskusage ""
	    wm deiconify $this
	} else {
	    after 200 $this delete
	}
	dsk_status "Ready."
	#dsk_lazy
    }

    common cmd
    set cmd(du) "du"
    set cmd(sort) "sort -rn"

    common id 0
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_du
# Args:		dir	name of directory
# Returns: 	""
# Desc:		Creates an object of class dsk_DiskUsage on $dir.
# Side-FX:	
#

proc dsk_du {dir} {
    if ![file isdirectory $dir] {
	set dir [file dirname $dir]
    }

    if {$dir != ""} {
    	dsk_DiskUsage .du[dsk_DiskUsage :: id] -directory $dir
    }
}

#
# =============================================================================
#
# Class:	dsk_FileInfo
# Desc:		Implements a class for file information windows.
#
# Methods:	
# Procs:	
# Publics:
#

itcl_class dsk_FileInfo {

    constructor {args} {
	global tkdesk

	#
	# Create a toplevel with this object's name
	#
        set class [$this info class]
        ::rename $this $this-tmp-
        ::toplevel $this -class $class
	wm withdraw $this
        ::rename $this $this-top
        ::rename $this-tmp- $this

	frame $this.fl -bd 1 -relief raised
	pack $this.fl -fill x

	label $this.label -text "$file" -font $tkdesk(font,labels3)
	pack $this.label -in $this.fl -side top \
		-padx $tkdesk(pad) -pady $tkdesk(pad)

	frame $this.fi -bd 1 -relief raised
	pack $this.fi -fill x

#	frame $this.fmsg
#	pack $this.fmsg -in $this.fi -side bottom -fill x

#	message $this.mMagic -text "" \
#		-width 270 -font $tkdesk(font,labels2)
#	pack $this.mMagic -in $this.fmsg -side left -anchor w \
#		-pady $tkdesk(pad) ;# -padx $tkdesk(pad)

	frame $this.f1
	pack $this.f1 -in $this.fi -side left -fill x -anchor n \
		-padx $tkdesk(pad) -pady $tkdesk(pad)

	label $this.lPath -text "Path....:" -font $tkdesk(font,mono)
	label $this.lSize -text "Size....:" -font $tkdesk(font,mono)
	label $this.lLink -text "Links...:" -font $tkdesk(font,mono)
	label $this.lMod  -text "Modified:" -font $tkdesk(font,mono)
	label $this.lOwn  -text "Owner...:" -font $tkdesk(font,mono)
	label $this.lGrp  -text "Group...:" -font $tkdesk(font,mono)
	label $this.lMode -text "Mode....:" -font $tkdesk(font,mono)
	label $this.lType -text "Type....:" -font $tkdesk(font,mono)

	pack $this.lPath $this.lSize $this.lMod $this.lOwn \
		$this.lGrp $this.lMode $this.lLink $this.lType \
		-in $this.f1 -side top

	frame $this.f2
	pack $this.f2 -in $this.fi -side left -fill x \
		-padx $tkdesk(pad) -pady $tkdesk(pad)

	label $this.lrPath -text "" -font $tkdesk(font,mono)
	label $this.lrSize -text "" -font $tkdesk(font,mono)
	label $this.lrLink -text "" -font $tkdesk(font,mono) -height 1
	button $this.bMod -text "" -font $tkdesk(font,mono) \
		-command "$this touch" \
		-padx 1 -pady 1 -highlightthickness 0
	button $this.bOwn -text "" -font $tkdesk(font,mono) \
		-command "$this chown" \
		-padx 1 -pady 1 -highlightthickness 0
	button $this.bGrp -text "" -font $tkdesk(font,mono) \
		-command "$this chgrp" \
		-padx 1 -pady 1 -highlightthickness 0
	message $this.mMagic -text "" -anchor w \
		-width 200 -font $tkdesk(font,labels2) 

	frame $this.fmod
	for {set i 1} {$i < 10} {incr i} {
	    button $this.bm($i) -width 2 -font $tkdesk(font,mono) \
		-command "$this chmod $i" \
		-padx 1 -pady 1 -highlightthickness 0
	    pack $this.bm($i) -in $this.fmod -side left
	}

	pack $this.lrPath $this.lrSize $this.bMod $this.bOwn \
		$this.bGrp $this.fmod $this.lrLink $this.mMagic \
		-in $this.f2 -side top -anchor w

	frame $this.fa -bd 1 -relief raised
	pack $this.fa -fill both -expand yes

	frame $this.fa1
	pack $this.fa1 -in $this.fa -fill both -expand yes -pady $tkdesk(pad)

	label $this.lComment -text "Annotation:" -anchor w
	pack $this.lComment -in $this.fa1 -fill x -expand no -anchor w \
		-padx $tkdesk(pad)
	
	cb_text $this.ft -vscroll 1 -lborder 1 -pad $tkdesk(pad) \
		-width 36 -height 6 \
		-selectbackground black -selectforeground white \
		-wrap word
	pack $this.ft -in $this.fa1 -fill both -expand yes

	frame $this.fb -bd 1 -relief raised
	pack $this.fb -fill x

	button $this.bClose -text " Close " -command "$this delete"
	button $this.bChmod -text " Change Mode " -state disabled \
		-command "$this chmod set"

	pack $this.bClose $this.bChmod -in $this.fb -side left \
		-padx $tkdesk(pad) -pady $tkdesk(pad) -ipady 1

	bind $this <Any-Enter> "focus $this.ft.text"

	wm title $this "File Information"
	wm protocol $this WM_DELETE_WINDOW "$this delete"

	eval config $args
    }

    destructor {
	global tkdesk_anno
	
	set anno [string trimright [$this.ft.text get 1.0 end] \n]
	if {$anno != ""} {
	    set tkdesk_anno($file) $anno
	} elseif [info exists tkdesk_anno($file)] {
	    unset tkdesk_anno($file)
	}
	#after 10 rename $this-top {}
        catch {destroy $this}		;# destroy associated window
    }

    #
    # ----- Methods and Procs -------------------------------------------------
    #

    method config {config} {
    }

    method touch {} {
	if {![file owned $file] && [dsk_logname] != "root"} {
	    dsk_errbell
	    cb_error "Sorry, you're not the owner of [file tail $file]!"
	    return
	}

	if {[cb_okcancel "Touch [file tail $file]?"] == 0} {
	    set err [catch {exec touch $file} errmsg]
	    if $err {
		cb_error $errmsg
	    } else {
		$this config -file $file
	    }
	}
    }

    method chown {} {
	global o

	if {![file owned $file] && [dsk_logname] != "root"} {
	    dsk_errbell
	    cb_error "Sorry, you're not the owner of [file tail $file]!"
	    return
	}

	set o $owner
	cb_readString "Change owner of [file tail $file] to:" o "Change Owner"
	if {$o != ""} {
	    set err [catch {exec chown $o $file} errmsg]
	    if $err {
		cb_error $errmsg
	    } else {
		$this config -file $file
	    }
	}
    }

    method chgrp {} {
	global g

	if {![file owned $file] && [dsk_logname] != "root"} {
	    dsk_errbell
	    cb_error "Sorry, you're not the owner of [file tail $file]!"
	    return
	}

	set g $group
	cb_readString "Change group of [file tail $file] to:" g "Change Group"
	if {$g != ""} {
	    set err [catch {exec chgrp $g $file} errmsg]
	    if $err {
		cb_error $errmsg
	    } else {
		$this config -file $file
	    }
	}
    }

    method chmod {num} {
	# num is: user r/w/x: 1/2/3, group r/w/x: 4/5/6, world r/w/x: 7/8/9,
	# 	  or "set"
	# fmode contains the current mode string

	if {![file owned $file] && [dsk_logname] != "root"} {
	    dsk_errbell
	    cb_error "Sorry, you're not the owner of [file tail $file]!"
	    return
	}

	$this.bChmod config -state normal

	for {set i 0} {$i < 10} {incr i} {
	    set m($i) [string index $fmode $i]
	}

	switch $num {
	    1	-
	    4	-
	    7   {
		if {$m($num) == "r"} {
		    set m($num) "-"
		} else {
		    set m($num) "r"
		}
		}
	    2	-
	    5	-
	    8   {
		if {$m($num) == "w"} {
		    set m($num) "-"
		} else {
		    set m($num) "w"
		}
		}
	    3	-
	    6	{
		if {$m($num) == "x"} {
		    set m($num) "s"
		} elseif {$m($num) == "s"} {
		    set m($num) "S"
		} elseif {$m($num) == "S"} {
		    set m($num) "-"
		} else {
		    set m($num)  "x"
		}
		}
	    9	{
		if {$m($num) == "x"} {
		    set m($num) "t"
		} elseif {$m($num) == "t"} {
		    set m($num) "T"
		} elseif {$m($num) == "T"} {
		    set m($num) "-"
		} else {
		    set m($num)  "x"
		}
		}
	}

	if {$num != "set"} {
	    $this.bm($num) config -text $m($num)
	    set fmode ""
	    for {set i 0} {$i < 10} {incr i} {
	    	append fmode $m($i)
	    }
	} else {
	    set s 0 ; set o 0 ; set g 0 ; set w 0
	    if {$m(1) == "r"} {incr o 4}
	    if {$m(2) == "w"} {incr o 2}
	    if {$m(3) == "x"} {
		incr o 1
	    } else {
		if {$m(3) != "-"} {
		    incr s 4
		    if {$m(3) == "s"} {incr o 1}
		}
	    }
	    if {$m(4) == "r"} {incr g 4}
	    if {$m(5) == "w"} {incr g 2}
	    if {$m(6) == "x"} {
		incr g 1
	    } else {
		if {$m(6) != "-"} {
		    incr s 2
		    if {$m(6) == "s"} {incr g 1}
		}
	    }
	    if {$m(7) == "r"} {incr w 4}
	    if {$m(8) == "w"} {incr w 2}
	    if {$m(9) == "x"} {
		incr w 1
	    } else {
		if {$m(9) != "-"} {
		    incr s 1
		    if {$m(9) == "t"} {incr w 1}
		}
	    }

	    set amode ""
	    append amode $s $o $g $w
	    exec chmod $amode $file
	    dsk_refresh $file
	    catch {$this.bChmod config -state disabled}
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

    public file "" {
	global tkdesk tkdesk_anno

	if ![file exists $file] {return}
	dsk_debug "file: $file"	

	dsk_busy
	$this.label config -text "[file tail $file]"
	set p [file dirname $file]
	if {[string first $tkdesk(configdir)/.trash $p] == 0} {
	    set p [string range $p \
		    [string length $tkdesk(configdir)/.trash/] 1000]
	    if {$p == ""} {
		set p "Trash"
	    }
	}
	$this.lrPath config -text [cb_tilde $p collapse]
	set lsl ""
	set lsl [lindex [dsk_ls -l -o $file] 0]
	regsub -all {\\t} $lsl "\t" lsl
	set lsl [split $lsl "\t"]
	dsk_debug "$file: $lsl"
	$this.lrSize config -text "[lindex $lsl 1] Bytes"
	$this.lrLink config -text [string trimleft [lindex $lsl 6] " "]
	$this.bMod config -text "[lindex $lsl 2]"
	set owner [lindex $lsl 3]
	$this.bOwn config -text "$owner"
	set group [lindex $lsl 4]
	$this.bGrp config -text "$group"

	set fmode [lindex $lsl 5]
	for {set i 1} {$i < 10} {incr i} {
	    $this.bm($i) config -text [string index $fmode $i]
	}

	set m [exec file $file]
	$this.mMagic config -text [string trimleft [string range $m \
		[expr [string first ":" $m] + 1] 10000] " "]

	if [info exists tkdesk_anno($file)] {
	    $this.ft.text delete 1.0 end
	    $this.ft.text insert end $tkdesk_anno($file)
	}

	if {[file isdirectory $file] && ![winfo exists $this.bDU]} {
	    button $this.bDU -text " Disk Usage " -command "dsk_du \"$file\""
	    pack $this.bDU -in $this.fb -side left \
			-padx $tkdesk(pad) -pady $tkdesk(pad) -ipady 2
	}

	wm iconname $this "Info: [file tail $file]"
	dsk_place_window $this fileinfo ""
	wm deiconify $this
	dsk_lazy
    }

    protected owner
    protected group
    protected fmode

    common id 0
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_fileinfo
# Args:		none!
# Returns: 	""
# Desc:		Creates an object of class dsk_FileInfo on each selected file
#		in the calling file viewer.
# Side-FX:	
#

proc dsk_fileinfo {args} {
    global tkdesk

    set files $args
    if {$files == ""} {
    	set files [_make_fnames_safe]
    }

    if {$files == ""} {
	dsk_bell
	cb_info "Please select one or more files first."
	return
    }

    foreach file $files {
	set file [subst $file]
    	set file [dsk_striptc $file]
	if ![file exists $file] {
	    #dsk_errbell
	    cb_info "[file tail $file]\nis a broken symbolic link."
	    continue
	}

    	if {$file != ""} {
    	    dsk_FileInfo .fi[dsk_FileInfo :: id] -file $file
    	}
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_create
# Args:		type		"file" or "directory"
# Returns: 	""
# Desc:		Creates a new file or directory in the current directory.
# Side-FX:	none
#

proc dsk_create {type} {
    global tkdesk f

    set f ""
    if {$type == "file"} {
	cb_readString "Name of new file:" f "New File"
    } else {
	cb_readString "Name of new directory:" f "New Directory"
    }

    if {$f != ""} {
	catch "set f \[cb_tilde $f expand\]"

	if {$type == "file"} {
	    set cmd touch
	} else {
	    set cmd mkdir
	}

	if {[string index $f 0] != "/"} {
	    set f [string trimright [dsk_active dir] "/"]/$f
	    set err [catch {exec $tkdesk(cmd,$cmd) "$f"} errmsg]
	} else {
	    set err [catch {exec $tkdesk(cmd,$cmd) "$f"} errmsg]
	}
	if $err {
	    dsk_errbell
	    cb_error "Couldn't create $f! ($errmsg)"
	    return
	}
	dsk_refresh [file dirname $f]
	dsk_select file $f
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_select
# Args:		cmd		command argument (X or clear or file)
# Returns: 	""
# Desc:		Copies the selected files of calling viewer to the X sel. or
#		clears the selection.
# Side-FX:	none
#

proc dsk_select {cmd args} {
    global tkdesk

    switch -glob -- $cmd {
	file {
	    set d [file dirname $args]
	    set t [file tail $args]
	    foreach fl [itcl_info objects -class dsk_FileListbox] {
		if {$d == [string trimright \
			[$fl info public directory -value] "/"]} {
		    $fl.dlb select name $t
		}
	    }
	}
	X {
	    if [winfo exists $tkdesk(active_viewer)] {
		$tkdesk(active_viewer) select X
	    }
	}
   	clear {
	    if [winfo exists $tkdesk(active_viewer)] {
		$tkdesk(active_viewer) select clear
	    }
	}
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_copy
# Args:		files		(opt.) list of files to copy, move etc.
#		dest		(opt.) destination directory
# Returns: 	""
# Desc:		Creates a window for copying, moving and linking the selected
#		files or $files (opt. to $dest).
# Side-FX:	Rereads the concerned file listboxes.
#

if ![info exists tkdesk(geometry,dsk_copy)] {
    set tkdesk(geometry,dsk_copy) ""
}

global dsk_copy
set dsk_copy(num_cas_dirs) 0	;# number of cascaded dir menus
set dsk_copy(flist) ""		;# initial file list for dsk_copy
set dsk_copy(fcnt) 0		;# counter: that many files have been processed
set dsk_copy(fmax) 0		;# number of files to process - 1
set dsk_copy(ldest) ""		;# last destination

proc dsk_copy {{files ""} {dest ""}} {
    global tkdesk dsk_copy

    if {$files == ""} {
    	set files [dsk_active sel]
    }

    set dsk_copy(fcnt) 0
    set dsk_copy(fmax) [expr [llength $files] - 1]
    set dsk_copy(flist) $files
    set dsk_copy(all) $tkdesk(all_files)

    set t .dsk_copy
    if [winfo exists $t] {
	destroy $t
    }

    toplevel $t

    # ---- Source Files

    frame $t.ff -bd 1 -relief raised
    pack $t.ff -fill both -expand yes

    frame $t.fff
    pack $t.fff -in $t.ff -fill both -expand yes \
		-padx $tkdesk(pad) -pady $tkdesk(pad)

    entry $t.eFile -width 40 -bd 2 -relief sunken
    pack $t.eFile -in $t.fff -side bottom -fill x -expand yes -ipady 2
    $t.eFile insert end [lindex $dsk_copy(flist) 0]
    $t.eFile icursor end
    $t.eFile xview end
    bind $t.eFile <Tab> "focus $t.eDest"
    bind $t.eFile <Return> {set dummy x ; unset dummy}
    bind $t.eFile <3> {dsk_popup %W [%W get] %X %Y}
    cb_bindForCompletion $t.eFile <Control-Tab>

    blt_drag&drop target $t.eFile handler text "dd_handle_text $t.eFile 1"

    label $t.lFile
    if {$files == ""} {
	$t.lFile config -text "File (no files selected):"
    } else {
	$t.lFile config \
      -text "File ([expr $dsk_copy(fcnt) + 1] of [expr $dsk_copy(fmax) + 1]):"
    }
    pack $t.lFile -in $t.fff -side left

    if {$dsk_copy(fmax) > 0} {
    	checkbutton $t.cbAll -text "all selected files" \
		-padx $tkdesk(pad) -relief flat -variable dsk_copy(all)
    	pack $t.cbAll -in $t.fff -side right
    }

    # ---- Destination

    frame $t.fd -bd 1 -relief raised
    pack $t.fd -fill both -expand yes

    frame $t.fdf
    pack $t.fdf -in $t.fd -fill both -expand yes \
		-padx $tkdesk(pad) -pady $tkdesk(pad)

    entry $t.eDest -width 40 -bd 2 -relief sunken
    pack $t.eDest -in $t.fdf -side bottom -fill x -expand yes -ipady 2
    if {$dest == ""} {
    	$t.eDest insert end $dsk_copy(ldest)
    } else {
    	$t.eDest insert end $dest
    }
    $t.eDest icursor end
    $t.eDest xview end
    bind $t.eDest <Tab> "focus $t.eFile"
    bind $t.eDest <Return> "
    set tmpret \[cb_dialog ${t}AD {Copy, Move or Link?} \
	    {Select the desired action:} {} 0 \
	    {Cancel} {Copy} {Move} {Link} {SymLink}\]
    switch \$tmpret {
	1	{dsk_copy_action copy}
	2	{dsk_copy_action move}
	3	{dsk_copy_action link}
	4	{dsk_copy_action symlink}
    }
    unset tmpret"
    cb_bindForCompletion $t.eDest <Control-Tab>

    blt_drag&drop target $t.eDest handler text "dd_handle_text $t.eDest 1"

    label $t.lDest -text "Destination:"
    pack $t.lDest -in $t.fdf -side left

    frame $t.fdlm
    pack $t.fdlm -in $t.fdf -side right

    menubutton $t.mbDir -bd 2 -relief raised \
		-bitmap @$tkdesk(library)/cb_tools/bitmaps/slash.xbm \
		-menu $t.mbDir.menu
    pack $t.mbDir -in $t.fdlm -side left -ipadx 2 -ipady 2

    menu [set m $t.mbDir.menu] \
	    -postcommand "_dsk_dmenu $t.mbDir.menu $t.eDest"
	# add dummy entry to work around bug in pre Tk 4.0p2:
	$m add command -label "dummy"

    menubutton $t.mbHist -bd 2 -relief raised \
		-bitmap @$tkdesk(library)/cb_tools/bitmaps/combo.xbm \
		-menu $t.mbHist.menu
    pack $t.mbHist -in $t.fdlm -side left -ipadx 2 -ipady 2

    menu $t.mbHist.menu -postcommand "_dsk_hmenu $t.mbHist.menu $t.eDest"
	# add dummy entry to work around bug in pre Tk 4.0p2:
	$t.mbHist.menu add command -label "dummy"

    # ---- Buttons

    frame $t.fb -bd 1 -relief raised
    pack $t.fb -fill x

    button $t.bCopy -text " Copy " -command "dsk_copy_action copy"
    button $t.bMove -text " Move " -command "dsk_copy_action move"
    button $t.bLink -text " Link " -command "dsk_copy_action link"
    button $t.bSymLink -text " SymLink " -command "dsk_copy_action symlink"

    button $t.bCancel -text " Cancel " -command  {
		set dsk_copy(ldest) [.dsk_copy.eDest get]
		set tkdesk(geometry,dsk_copy) [wm geometry .dsk_copy]
		destroy .dsk_copy }

    pack $t.bCancel -in $t.fb -side right \
		-padx $tkdesk(pad) -pady $tkdesk(pad) -ipady 1
		
    if {$dsk_copy(fmax) > 0} {
	button $t.bSkip -text " Skip " -command "dsk_copy_action skip"
	pack $t.bSkip -in $t.fb -side right \
		-padx $tkdesk(pad) -pady $tkdesk(pad) -ipady 1
    } else {
	set f [lindex $dsk_copy(flist) 0]
	if {[string match "*.tar.gz" $f] || \
		[string match "*.tgz" $f] || \
		[string match "*.taz" $f] || \
		[string match "*.tar.Z" $f] || \
		[string match "*.tar" $f]} {
	    button $t.bUntar -text " Untar " -command "dsk_copy_action untar"
	    pack $t.bUntar -in $t.fb -side right \
		    -padx $tkdesk(pad) -pady $tkdesk(pad) -ipady 1
	}
    }

    pack $t.bCopy $t.bMove $t.bLink $t.bSymLink \
		-in $t.fb -side left \
		-padx $tkdesk(pad) -pady $tkdesk(pad) -ipady 1

    #bind $t <Any-Enter> "+focus $t.eFile"
    #focus $t.eFile

    wm minsize $t 486 162
    wm title $t "Copy, Move, Link Files"
    wm protocol $t WM_DELETE_WINDOW {.dsk_copy.bCancel invoke}
    
    dsk_place_window $t dsk_copy 486x162
    focus -force $t.eFile
}


proc dsk_copy_action {cmd} {
    global dsk_copy tkdesk tkdesk_anno

    dsk_debug "dsk_copy_action $cmd"

    set t .dsk_copy
    #cd [$tkdesk(active_viewer) curdir]

    set tlist ""
    if $dsk_copy(all) {
    	set tlist [lrange $dsk_copy(flist) $dsk_copy(fcnt) $dsk_copy(fmax)]
    }
    if {($tlist == "" || !$dsk_copy(all)) && [winfo exists $t.eFile]} {
	set tlist [$t.eFile get]
	set err [catch "set tlist \[glob $tlist\]"]
	if $err {
	    set tlist [$t.eFile get]
	}
    }
    if {$tlist == ""} {
	dsk_bell
	cb_info "Nothing to $cmd!"
	return
    }
    set anum [llength $tlist]
    dsk_debug " anum = $anum"

    set alist ""
    foreach f $tlist {
	catch {set f [cb_tilde $f expand]}
	if {[string index $f 0] != "/"} {
	    set f [dsk_active dir]$f
	}
	set err [catch {set fl [glob $f]}]
	if !$err {
	    foreach fe $fl {
		lappend alist $fe
	    }
	} else {
	    dsk_errbell
	    cb_error "$f: no match!"
	}
    }
    if {$alist == ""} {
	return
    }

    set dest [$t.eDest get]
    if {$dest == "" && $cmd != "skip"} {
	cb_info "Please fill in the destination first!"
	return
    }
    catch "set dest \[cb_tilde $dest expand\]"
    if {[string index $dest 0] != "/"} {
	set dest [dsk_active dir]$dest
    }

    dsk_debug " alist = \{$alist\}"
    if [dsk_on_rofs $dest] {
	dsk_errbell
	cb_error "The target filesystem is mounted read-only."
	return
    }
    
    # check permissions:
    if {![file writable $dest] && [file exists $dest]} {
	cb_error "[file tail $dest]:\nnot writable."
	return
    }
    foreach file $alist {
	if {$cmd == "move"} {
	    if ![dsk_check_perm $file] {
		cb_error "[file tail $file]:\nYou don't have permission to move this item."
		return
	    }
	} elseif {$cmd == "copy"} {
	    if ![file readable $file] {
		cb_error "[file tail $file]:\nnot readable."
		return
	    }
	}
    }

    # check for existing files in destination dir:
    if {[file exists $dest] && !$tkdesk(overwrite_always) && $cmd != "skip"} {
	if ![file isdirectory $dest] {
	    if {[cb_dialog $t-ED "File exists" "$dest already exists!" \
			question 0 "Overwrite" "  Skip  "] == 1} {
	   	set cmd skip
	    }
	} elseif {[llength $alist] == 1} {
	    if {$cmd == "move"} {
		if [dsk_on_rofs $alist] {
		    # all files in $alist live on the same file system
		    dsk_errbell
		    cb_error "The source filesystem is mounted read-only."
		    return
		}
	    }
	    set dfile [string trimright $dest /]/[file tail $alist]
	    if [file exists $dfile] {
	    	if {[cb_dialog $t-ED "File exists" "$dfile already exists!" \
			question 0 "Overwrite" "  Skip  "] == 1} {
	    	    set cmd skip
	    	}
	    }
	} else {
	    # "all selected files" has been selected
	    set tlist $alist
	    set dir [string trimright $dest /]
	    set rofs_checked 0
	    foreach file $tlist {
		if {$cmd == "move" && !$rofs_checked} {
		    if [dsk_on_rofs $file] {
			# all files in $alist live on the same file system
			dsk_errbell
			cb_error "The source filesystem is mounted read-only."
			return
		    }
		    set rofs_checked 1
		}
		set dfile $dir/[file tail $file]
		if [file exists $dfile] {
		    set ret [cb_dialog $t-ED "File exists" \
				"$dfile already exists!" question 0 \
				"Overwrite" "Overwrite all" \
				"  Skip  " " Cancel "]
		    switch $ret {
			1 {
			    break
			}
			2 {
			    set i [lsearch $alist $file]
			    if {$i > -1} {
				set alist [lreplace $alist $i $i]
			    } else {
				cb_error "?? Couldn't find $file !?"
			    }
			}
			3 {
			    catch {$t.bCancel invoke}
			    return
			}
		    }
		}
	    }
	    if {$alist == ""} {
		catch {$t.bCancel invoke}
		return
	    }
	    set anum [llength $alist]
	}
    }

    if $dsk_copy(all) {
	catch {$t.bCancel invoke}
    }
    if {$anum > 1} {set fll files} {set fll file}
    dsk_debug " Files: $anum"
    switch $cmd {
	skip {
	    if $dsk_copy(all) return
	    incr dsk_copy(fcnt)
	    $t.lFile config -text "File ([expr $dsk_copy(fcnt) + 1] of [expr $dsk_copy(fmax) + 1]):"
	    if {$dsk_copy(fcnt) > $dsk_copy(fmax)} {
		catch {$t.bCancel invoke}
		return
	    }
	    $t.eFile delete 0 end
	    $t.eFile insert end [lindex $dsk_copy(flist) $dsk_copy(fcnt)]
	    $t.eFile icursor end
	    $t.eFile xview end
	    return
	}
	copy {
	    #dsk_busy
	    dsk_sound dsk_copying
	    set out [dsk_bgexec "$tkdesk(cmd,cp) $alist \"$dest\"" \
			"Copying $anum $fll..."]
	    #dsk_lazy
	}
	move {
	    #dsk_busy
	    dsk_sound dsk_moving
	    if ![file isdirectory $alist] {
	    	set out [dsk_bgexec "$tkdesk(cmd,mv) $alist \"$dest\"" \
			"Moving $anum $fll..."]
	    } else {
		# try to hide different file systems from user:
		file stat [lindex $alist 0] s1
		set s2(dev) 0
		catch "file stat $dest s2"
		if !$s2(dev) {
		    catch {file stat [file dirname $dest] s2}
		}
		if {$s1(dev) == $s2(dev) || !$s2(dev) \
		    || ([llength $alist] == 1 \
		    && ![file isdirectory [lindex $alist 0]])} {
	    	    set out [dsk_bgexec "$tkdesk(cmd,mv) $alist \"$dest\"" \
				"Moving $anum $fll..."]
		} else {
	    	    set out [dsk_bgexec "$tkdesk(cmd,cp) $alist $dest" \
				"Move: Copying $anum $fll..."]
		    if {$out != "error" && $out != "break"} {
	    	    	set out [dsk_bgexec "$tkdesk(cmd,rm) $alist" \
				"Move: Deleting $anum $fll..."]
		    }
		}
	    }
	    #dsk_lazy
	}
	link {
	    #dsk_busy
	    dsk_sound dsk_linking
	    set out [dsk_bgexec "$tkdesk(cmd,ln) $alist \"$dest\"" \
			"Linking $anum $fll..."]
	    #dsk_lazy
	}
	symlink {
	    #dsk_busy
	    dsk_sound dsk_symlinking
	    set out [dsk_bgexec "$tkdesk(cmd,symln) $alist \"$dest\"" \
			"Linking $anum $fll symbolically..."]
	    #dsk_lazy
	}
	untar {
	    #dsk_busy
	    set f $alist ;# this can only be just one file
	    if {[string match "*.tar.gz" $f] || \
		    [string match "*.tgz" $f] || \
		    [string match "*.taz" $f] || \
		    [string match "*.tar.Z" $f]} {
		dsk_sound dsk_untaring
		set out [dsk_bgexec "gzip -cd $f | tar xf - -C \"$dest\"" \
			"Untaring [file tail $f]..."]
	    } elseif {[string match "*.tar" $f]} {
		dsk_sound dsk_untaring
		set out [dsk_bgexec "tar xf $f -C \"$dest\"" \
			"Untaring [file tail $f]..."]
	    } else {
		dsk_errbell
		cb_error "This file doesn't look like a tar file."
	    }
	    #dsk_lazy
	}
	default {
	    cb_error "dsk_copy_action: unknown cmd ($cmd)"
	    return
	}
    }

    if {$out != "error"} {
	# copy annotations and adjust desk items:
	set dest [string trimright $dest "/"]
	foreach f $alist {
	    if [info exists tkdesk_anno($f)] {
		if [file isdirectory $dest] {
		    set tkdesk_anno($dest/[file tail $f]) $tkdesk_anno($f)
		} else {
		    set tkdesk_anno($dest) $tkdesk_anno($f)
		}
		if {$cmd == "move"} {
		    unset tkdesk_anno($f)
		}
	    }
	    if [file isdirectory $dest] {
		dsk_DeskItem :: move $f $dest/[file tail $f]
	    } else {
		dsk_DeskItem :: move $f $dest
	    }
	}
	if {$cmd == "move"} {
	    dsk_refresh "$alist $dest"
	} else {
	    dsk_refresh "$dest"
	}
	if !$dsk_copy(all) {
	    incr dsk_copy(fcnt)
	    if {$dsk_copy(fcnt) > $dsk_copy(fmax)} {
		catch {$t.bCancel invoke}
		return
	    }
	    $t.eFile delete 0 end
	    $t.eFile insert end [lindex $dsk_copy(flist) $dsk_copy(fcnt)]
	    $t.eFile icursor end
	    $t.eFile xview end
	}	    
    }
}


proc _dsk_hmenu {menu entry} {
    global tkdesk

    catch "$menu delete 0 last"
    if $tkdesk(sort_history) {
	set l [lsort [history get]]
    } else {
	set l [history get]
    }
    foreach dir $l {
	$menu add command -label $dir \
		-command "$entry delete 0 end;\
		$entry insert end $dir" \
		-font $tkdesk(font,entries)
    }
}

proc _dsk_dmenu {menu entry} {
    global tkdesk _dsk_dmenu

    catch "$menu delete 0 last"
    set _dsk_dmenu(num_cas_dirs) 0
    if [info exists tkdesk(directories)] {
	foreach mdir $tkdesk(directories) {
	    if {$mdir == "-"} {
		$menu add separator
	    } else {
		_dsk_add_dir_to_menu $menu $mdir $entry
	    }
	}
    }
}

proc _dsk_add_dir_to_menu {menu mdir entry} {
    global tkdesk _dsk_dmenu

    #set mdir [string trimleft $mdir *]
    if {[llength $mdir] == 1} {
	#$menu add command -label $mdir -command "
	#$entry delete 0 end
	#$entry insert end $mdir" \
	#	-font $tkdesk(font,file_lbs)
	if {[string index $mdir 0] != "*"} {
	    $menu add command -label $mdir \
		    -command "$entry delete 0 end;\
		    $entry insert end $mdir" \
		    -font $tkdesk(font,file_lbs)
	} else {
	    set mdir [string range $mdir 1 end]
	    incr tkdesk(_dirmenu,cnt)
	    set m ${menu}.cas$tkdesk(_dirmenu,cnt)
	    catch {destroy $m}
	    menu $m -postcommand "\
		    dsk_casdirs $mdir $m 1 \{$entry delete 0 end;\
		    $entry insert end %d\}"
	    $menu add cascade -label "$mdir (*)" \
		    -font $tkdesk(font,file_lbs) \
		    -menu $m
	}
    } else {
	incr _dsk_dmenu(num_cas_dirs)
	set m ${menu}.$_dsk_dmenu(num_cas_dirs)
	$menu add cascade -label [lindex $mdir 0] -menu $m \
		-font $tkdesk(font,file_lbs)
	catch {destroy $m}
	menu $m

	foreach d $mdir {
	    if {$d == "-"} {
		$m add separator
	    } else {
		_dsk_add_dir_to_menu $m $d $entry
	    }
	}
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_ddcopy
# Args:		files - list of files
#               dest - destination path
# Returns: 	""
# Desc:		Is called when files have been dropped onto a file listbox.
#               If quick_dragndrop is set, the files are copied or moved
#               (in case the Control-Key was pressed) without asking.
#               Else dsk_copy gets called with the arguments of dsk_ddcopy.
# Side-FX:	none
#

proc dsk_ddcopy {files dest} {
    global tkdesk

    set anum [llength $files]
    if {$anum == 1} {
	set fll "File"
    } else {
	set fll "Files"
    }

    if {[string first "$tkdesk(configdir)/.shelf/" $dest] > -1} {
	set out [dsk_bgexec "$tkdesk(cmd,symln) $files $dest" \
		"Linking $anum $fll symbolically..."]
	dsk_refresh $dest
	return
    }	

    if !$tkdesk(quick_dragndrop) {
	dsk_copy $files $dest
	return
    } else {
	if {[llength $files] == 1} {
	    if {[file dirname $files] == [string trimright $dest /] || \
		$files == [string trimright $dest /]} {
		# avoid copying of files to themselves 
		return
	    }
	}
	
	set dir [string trimright $dest /]
	set rofs_checked 0
	foreach file $files {
	    if {!$tkdesk(file_lb,control) && !$rofs_checked} {
		if [dsk_on_rofs $file] {
		    # all files in $alist live on the same file system
		    dsk_errbell
		    cb_error "The source filesystem is mounted read-only."
		    return
		}
		set rofs_checked 1
	    }
	    set dfile $dir/[file tail $file]
	    if [file exists $dfile] {
		set ret [cb_dialog .ddcopy-ED "File exists" \
			"$dfile already exists!" question 0 \
			"Overwrite" "Overwrite all" "  Skip  "]
		if {$ret == 1} {
		    break
		} elseif {$ret == 2} {
		    set i [lsearch $files $file]
		    if {$i > -1} {
			set files [lreplace $files $i $i]
		    } else {
			cb_error "?? Couldn't find $file !?"
		    }
		}
	    }
	}
	if {$files == ""} {
	    return
	}

	if $tkdesk(file_lb,control) {
	    # copy files
	    dsk_sound dsk_copying
	    set out [dsk_bgexec "$tkdesk(cmd,cp) $files $dest" \
		    "Copying $anum $fll..."]
	} else {
	    # move files
	    # try to hide different file systems from user:
	    dsk_sound dsk_moving
	    file stat [lindex $files 0] s1
	    set s2(dev) 0
	    catch "file stat $dest s2"
	    if {$s1(dev) == $s2(dev) || !$s2(dev) \
		|| ([llength $files] == 1 \
		&& ![file isdirectory [lindex $files 0]]) } {
		set out [dsk_bgexec "$tkdesk(cmd,mv) $files $dest" \
			"Moving $anum $fll..."]
	    } else {
		set out [dsk_bgexec "$tkdesk(cmd,cp) $files $dest" \
			"Move: Copying $anum $fll..."]
		if {$out != "error" && $out != "break"} {
		    set out [dsk_bgexec "$tkdesk(cmd,rm) $files" \
			    "Move: Deleting $anum $fll..."]
		}
	    }
	}
	if {$out != "error"} {
	    if !$tkdesk(file_lb,control) {
		dsk_refresh "$files $dest"
	    } else {
		dsk_refresh "$dest"
	    }
	}
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_delete
# Args:		files		(opt.) list of files
# Returns: 	""
# Desc:		Deletes the selected files or $files.
# Side-FX:	none
#

if ![info exists tkdesk(geometry,dsk_delete)] {
    set tkdesk(geometry,dsk_delete) ""
}

global dsk_delete
set dsk_delete(flist) ""	;# initial file list for dsk_copy
set dsk_delete(fcnt) 0		;# counter: that many files have been processed
set dsk_delete(fmax) 0		;# number of files to process - 1
set dsk_delete(really) 0	;# REALLY delete?

proc dsk_delete {{files ""}} {
    global tkdesk dsk_delete

    if {$files == ""} {
    	set files [dsk_active sel]
    }

    set dsk_delete(fcnt) 0
    set dsk_delete(fmax) [expr [llength $files] - 1]
    set dsk_delete(flist) $files
    set dsk_delete(all) $tkdesk(all_files)
    set dsk_delete(really) $tkdesk(really_delete)

    if {[string first "$tkdesk(configdir)/.trash" [lindex $files 0]] > -1} {
	set dsk_delete(really) 1
    }

    if !$tkdesk(ask_on_delete) {
	if {!$dsk_delete(really) && $dsk_delete(flist) != ""} {
	    dsk_delete_action delete
	    return
	}
    }

    set t .dsk_delete
    if [winfo exists $t] {
	destroy $t
    }

    toplevel $t

    # ---- File Box

    frame $t.ff -bd 1 -relief raised
    pack $t.ff -fill both -expand yes

    frame $t.fff
    pack $t.fff -in $t.ff -fill both -expand yes \
		-padx $tkdesk(pad) -pady $tkdesk(pad)

    entry $t.eFile -width 40 -bd 2 -relief sunken
    pack $t.eFile -in $t.fff -side bottom -fill x -expand yes -ipady 2
    $t.eFile insert end [lindex $dsk_delete(flist) 0]
    $t.eFile icursor end
    $t.eFile xview end
    bind $t.eFile <Return> "$t.bDelete flash; $t.bDelete invoke"
    bind $t.eFile <3> {dsk_popup %W [%W get] %X %Y}
    cb_bindForCompletion $t.eFile <Control-Tab>

    blt_drag&drop target $t.eFile handler text "dd_handle_text $t.eFile 1"

    label $t.lFile
    if {$files == ""} {
	$t.lFile config -text "File (no files selected):"
    } else {
	$t.lFile config \
  -text "File ([expr $dsk_delete(fcnt) + 1] of [expr $dsk_delete(fmax) + 1]):"
    }
    pack $t.lFile -in $t.fff -side left

    if {$dsk_delete(fmax) > 0} {
    	checkbutton $t.cbAll -text "all selected files" \
		-padx $tkdesk(pad) -relief flat -variable dsk_delete(all)
    	pack $t.cbAll -in $t.fff -side right
    }

    # ---- Buttons

    frame $t.fb -bd 1 -relief raised
    pack $t.fb -fill x

    button $t.bDelete -text "  Delete  " -command "dsk_delete_action delete"
    checkbutton $t.cbReally -text "REALLY delete" \
		-padx $tkdesk(pad) -relief flat -variable dsk_delete(really)
    button $t.bSkip -text "  Skip  " -command "dsk_delete_action skip"
    if {$dsk_delete(fmax) < 1} {
	$t.bSkip config -state disabled
    }
    button $t.bCancel -text " Cancel " -command  {
		set tkdesk(geometry,dsk_delete) [wm geometry .dsk_delete]
		destroy .dsk_delete }

    pack $t.bCancel $t.bSkip \
		-in $t.fb -side right \
		-padx $tkdesk(pad) -pady $tkdesk(pad) -ipady 1

    pack $t.bDelete $t.cbReally \
		-in $t.fb -side left \
		-padx $tkdesk(pad) -pady $tkdesk(pad) -ipady 1

    #bind $t <Any-Enter> "+focus $t.eFile"
    
    wm minsize $t 326 100
    wm title $t "Delete Files"
    wm protocol $t WM_DELETE_WINDOW {.dsk_delete.bCancel invoke}

    dsk_place_window $t dsk_delete 470x98
    focus -force $t.eFile
}

proc dsk_delete_action {cmd} {
    global dsk_delete tkdesk

    set t .dsk_delete
    #cd [$tkdesk(active_viewer) curdir]

    set alist ""
    if $dsk_delete(all) {
    	set alist [lrange $dsk_delete(flist) \
		$dsk_delete(fcnt) $dsk_delete(fmax)]
    }
    if {($alist == "" || !$dsk_delete(all)) && [winfo exists $t.eFile]} {
	set alist [$t.eFile get]
	set err [catch "set alist \[glob $alist\]"]
	if $err {
	    set alist [$t.eFile get]
	}
    }
    if {$alist == ""} {
	dsk_bell
	cb_info "Nothing to $cmd!"
	return
    }

    set tmplist ""
    foreach f $alist {
	catch {set f [cb_tilde $f expand]}
	if {[string index $f 0] != "/"} {
	    set f [dsk_active dir]$f
	}
	set err [catch {set fl [glob $f]}]
	if !$err {
	    foreach fe $fl {
		lappend tmplist $fe
	    }
	} else {
	    dsk_errbell
	    cb_error "$f: no match!"
	}
    }
    if {$tmplist == ""} {
	return
    }
    set alist $tmplist
    set anum [llength $alist]

    set dest $tkdesk(configdir)/.trash

    if $dsk_delete(all) {
	catch {$t.bCancel invoke}
    }

    if {$anum > 1} {set fll files} {set fll file}
    switch $cmd {
	skip {
	    if $dsk_delete(all) return
	    incr dsk_delete(fcnt)
	    $t.lFile config -text "File ([expr $dsk_delete(fcnt) + 1] of [expr $dsk_delete(fmax) + 1]):"
	    if {$dsk_delete(fcnt) > $dsk_delete(fmax)} {
		catch {$t.bCancel invoke}
		return
	    }
	    $t.eFile delete 0 end
	    $t.eFile insert end [lindex $dsk_delete(flist) $dsk_delete(fcnt)]
	    $t.eFile icursor end
	    $t.eFile xview end
	    return
	}
	delete {
	    #dsk_busy
	    foreach file $alist {
		if ![dsk_check_perm $file] {
		    dsk_errbell
		    set rc [cb_dialog $t-ED "Permission denied" \
		    "[file tail $file]:\nYou don't have permission to delete this item." \
			    error 0 " OK " "Cancel"]
		    if {$rc == 1} {
			return
		    } else {
			set i [lsearch $alist $file]
			set alist [lreplace $alist $i $i]
		    }
		}
	    }
	    if {$alist == ""} {
		return
	    }
	    if $dsk_delete(really) {
		dsk_sound dsk_really_deleting
	    	set out [dsk_bgexec "$tkdesk(cmd,rm) $alist" \
			"Deleting $anum $fll..."]
	    } else {
		
		set dir [string trimright $dest /]
		set rofs_checked 0
		foreach file $alist {
		    if !$rofs_checked {
			if [dsk_on_rofs $file] {
			    # all files in $alist live on the same file system
			    dsk_errbell
			    cb_error "The filesystem is mounted read-only."
			    return
			}
			set rofs_checked 1
		    }
		    set dfile $dir/[file tail $file]
		    if [file exists $dfile] {
			set ret [cb_dialog $t-ED "File exists" \
				"There already is a file \"[file tail $file]\" in the trash can!" question 0 \
				"Overwrite" "Overwrite all" "  Skip  "]
			if {$ret == 1} {
			    break
			} elseif {$ret == 2} {
			    set i [lsearch $alist $file]
			    if {$i > -1} {
				set alist [lreplace $alist $i $i]
			    } else {
				cb_error "?? Couldn't find $file !?"
			    }
			}
		    }
		}
		if {$alist == ""} {
		    return
		}
		set anum [llength $alist]

		# try to hide different file systems from user:
		file stat $dest s2
		set one 1
		foreach file $alist {
		    set s1(dev) $s2(dev)
		    catch {file stat $file s1}
		    set t file
		    catch {set t [file type $file]}
		    if {$s1(dev) != $s2(dev) && $t != "file"} {
			set one 0
			break
		    }
		}

		if $one {
	    	    set out [dsk_bgexec "$tkdesk(cmd,mv) $alist $dest" \
				"Moving $anum $fll to the trash can..."]
		} else {
	    	    set out [dsk_bgexec "$tkdesk(cmd,cp) $alist $dest" \
				"Copying $anum $fll to the trash can..."]
		    if {$out != "error" && $out != "break"} {
	    	    	set out [dsk_bgexec "$tkdesk(cmd,rm) $alist" \
				"Deleting $anum $fll..."]
		    }
		}

	    }
	    #dsk_lazy
	}
	default {
	    cb_error "dsk_delete_action: unknown cmd ($cmd)"
	    return
	}
    }

    if {$out != "error"} {
	dsk_refresh "$alist $dest"
	foreach f $alist {
	    dsk_DeskItem :: remove $f
	}
	if ![winfo exists $t] return
	if !$dsk_delete(all) {
	    incr dsk_delete(fcnt)
	    if {$dsk_delete(fcnt) > $dsk_delete(fmax)} {
		catch {$t.bCancel invoke}
		return
	    }
	    $t.eFile delete 0 end
	    $t.eFile insert end [lindex $dsk_delete(flist) $dsk_delete(fcnt)]
	    $t.eFile icursor end
	    $t.eFile xview end
	}	    
    }
}

# -----------------------------------------------------------------------------
# dsk_on_rofs file:
# Checks if $file lives on a read-only filesystem. Returns 1 if it does.
#

proc dsk_on_rofs {file} {
    set dfo ""
    catch {set dfo [split [exec df $file] \n]}
    if {$dfo == ""} {return 0}
    set fs [lindex [lindex $dfo 1] 0]

    set fd ""
    catch {set fd [open /etc/mtab]}
    if {$fd == ""} {return 0}
    set mtab [split [read -nonewline $fd] \n]
    close $fd
    
    foreach mfs $mtab {
	if {$fs == [lindex $mfs 0]} {
	    if [regexp {^ro$|,ro$|^ro,|,ro,} [lindex $mfs 3]] {
		return 1
	    } else {
		return 0
	    }
	}
    }
    return 0
}

# -----------------------------------------------------------------------------
# dsk_check_perm file:
# Checks if $file (which can also be a directory) is deletable etc.
# by the current user.
#
proc dsk_check_perm {file} {
    set perm 1
    if ![file writable [file dirname $file]] {
	set perm 0
    }
    if [file isdirectory $file] {
	if ![file writable $file] {
	    if [file readable $file] {
		# if dir is empty it may be deleted
		set lso [exec ls $file]
		if {$lso != ""} {
		    set perm 0
		}
	    } else {
		set perm 0
	    }
	}
    }
    #else {
    #	 if ![file writable $file] {
    #	     set perm 0
    #	 }
    #}

    return $perm
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_casdirs
# Args:		dir - directory
#               m - menu's widget name
# Returns: 	""
# Desc:		Builds a cascaded menu for directory $dir.
# Side-FX:	none
#

set dsk_casdirs(menunum) 0
set dsk_casdirs(cmax) [expr [winfo screenheight .]/21]

proc dsk_casdirs {dir m {reset 0} {cmd ""}} {
    global tkdesk dsk_casdirs tk_version

    set err [catch {set fm [winfo containing [winfo pointerx .] \
	    [winfo pointery .]]}]
    if !$err {
	if {[string first "tearoff" $fm] > -1} {
	    # we're inside a torn-off menu
	    if ![info exists dsk_casdirs($fm:fixed)] {
		# adjust -menu and -postcommand options
		set li [$fm index last]
		for {set mi 0} {$mi <= $li} {incr mi} {
		    if {[$fm type $mi] == "cascade"} {
			set mm [$fm entrycget $mi -menu]
			if {[$fm index active] == $mi} {
			    set m $mm
			}
			# the submenus already exist
			set pc [$mm cget -postcommand]
			set pc [lreplace $pc 2 2 $mm]
			$mm config -postcommand $pc
		    }
		}
		wm protocol $fm WM_DELETE_WINDOW \
			"unset dsk_casdirs($fm:fixed); destroy $fm"
		set dsk_casdirs($fm:fixed) 1
	    }
	}
    }
    
    #catch "destroy $m"
    bind $m <ButtonRelease-1> "
      set tkdesk(menu,control) 0
      [bind Menu <ButtonRelease-1>]"
    bind $m <Control-ButtonRelease-1> "
      set tkdesk(menu,control) 1
      [bind Menu <ButtonRelease-1>]"
    bind $m <ButtonRelease-3> [bind $m <ButtonRelease-1>]
    bind $m <Control-ButtonRelease-3> [bind $m <Control-ButtonRelease-1>]

    if {$cmd == ""} {
	set mcmd "$tkdesk(active_viewer) config -dir [list $dir]"
    } else {
	set mcmd [string_replace $cmd "%d" [list $dir]]
    }
    
    $m delete 0 last
    if ![file readable $dir] {
	$m config -disabledforeground black
	$m add command -label (unreadable) \
		-font $tkdesk(font,file_lbs) \
		-state disabled
	update idletasks
	return
    }
	
    $m add command -label [cb_tilde $dir collapse] \
	    -font $tkdesk(font,file_lbs) \
	    -command $mcmd
    if {$reset} {
	for {set i 0} {$i < $dsk_casdirs(menunum)} {incr i} {
	    catch "destroy ${m}_$i"
	}
	set dsk_casdirs(menunum) 0
	set reset 0
    }
    
    set err [catch {set dirs [dsk_ls -d -p [lindex [glob $dir] 0]]}]
    if !$err {
	set c 0
	foreach d $dirs {
	    set d [string trimright $d]
	    set nm ${m}.$dsk_casdirs(menunum)
	    if ![winfo exists $nm] {
		menu $nm -postcommand "dsk_casdirs [list $d] $nm $reset \{$cmd\}"
	    } else {
		$nm config -postcommand "dsk_casdirs [list $d] $nm $reset \{$cmd\}"
		#$nm delete 0 last
	    }
	    $m add cascade -label [file tail $d] -menu $nm \
		    -font $tkdesk(font,file_lbs)
	    incr dsk_casdirs(menunum)
	    incr c
	    if {$c >= $dsk_casdirs(cmax)} {
		$m config -disabledforeground black
		$m add command -label "..." -state disabled
		break
	    }
	}
    }
    update idletasks
}

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_rename
# Args:		file - name of file to rename
# Returns: 	""
# Desc:		Opens an entry dialog to rename the given file.
# Side-FX:	none
#

proc dsk_rename {args} {
    global tmpnewname tkdesk tkdesk_anno

    set files $args
    if {$files == ""} {
    	set files [_make_fnames_safe]
    }

    if {$files == ""} {
	dsk_bell
	cb_info "Please select one or more files first."
	return
    }

    foreach file $files {
	set file [subst $file]
	if ![dsk_check_perm $file] {
	    set rc [cb_dialog .trename "Permission denied" \
		    "[file tail $file]:\nYou don't have permission to rename this item." \
		    error 0 " OK " "Cancel"]
	    if {$rc == 1} {
		return
	    } else {
		continue
	    }
	}
	if [file exists $file] {
	    set tmpnewname [file tail [dsk_striptc $file]]
	    set path [file dirname [dsk_striptc $file]]
	    cb_readString "Rename file to:" tmpnewname "Rename File"
	    if {$tmpnewname != "" && \
		    [list $file] != [list $path/$tmpnewname]} {
		if [file exists [list $path/$tmpnewname]] {
		    if {[cb_okcancel "File \"$tmpnewname\" already exists. Overwrite it?"] == 1} {
			unset tmpnewname
			continue
		    }
		}
		eval exec $tkdesk(cmd,mv) [list $file] [list $path/$tmpnewname]
		dsk_refresh $path/
		dsk_DeskItem :: move [list $file] [list $path/$tmpnewname]
		# move annotations:
		if [info exists tkdesk_anno($file)] {
		    set tkdesk_anno($path/$tmpnewname) $tkdesk_anno($file)
		    unset tkdesk_anno($file)
		}
	    }
	    unset tmpnewname
	} else {
	    dsk_errbell
	    cb_error "File does not exist."
	}
    }
}

# ----------------------------------------------------------------------------
# dsk_empty_trash:
# Empties the trash can.
#
proc dsk_empty_trash {} {
    global tkdesk

    set alist [dsk_ls -p -a $tkdesk(configdir)/.trash]
    set anum [llength $alist]
    if {$anum == 0} {
	cb_info "The trash can is empty."
    } else {
	if {[cb_okcancel "Empty trash can?\nThis will delete ALL files in the trash can!"] == 0} {
	    dsk_sound dsk_really_deleting
	    if {$anum == 1} {set fll "File"} {set fll "Files"}
	    set tlist ""
	    foreach f $alist {
		lappend tlist [string trimright \
			$tkdesk(configdir)/.trash/$f " "]
	    }
	    dsk_bgexec "$tkdesk(cmd,rm) $tlist" "Deleting $anum $fll..."
	    dsk_refresh $tkdesk(configdir)/.trash
	}
    }
}
