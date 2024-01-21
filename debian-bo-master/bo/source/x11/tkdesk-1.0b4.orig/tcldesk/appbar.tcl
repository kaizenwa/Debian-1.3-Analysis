# =============================================================================
#
# File:		appbar.tcl
# Project:	TkDesk
#
# Started:	13.11.94
# Changed:	13.11.94
# Author:	cb
#
# Description:	Implements an application bar. This features popup menus
#		and drag and drop targets.
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
#    proc dsk_appbar {args}
#    proc _appbar_create {}
#    proc _appbar_add_to_menu {menu cmd}
#    proc _appbar_unpost_parents {menu}
#    proc _appbar_dd_action {cmd}
#    proc _appbar_show_menu {butnum rootx rooty}
#    proc _appbar_close {}
#    proc _appbar_raise {}
#    proc _appbar_layout {orient}
#    proc _appbar_move {}
#    proc _appbar_date {}
#
# =============================================================================

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_appbar
# Args:		layout <horizontal|vertical>	change the layout
#		move		move the application bar around
# Returns: 	""
# Desc:		Meta proc for all things that concern the appbar.
# Side-FX:	none
#

proc dsk_appbar {args} {
    global tkdesk

    dsk_progress "Creating the application bar..."
    if ![info exists tkdesk(appbar)] {
	dsk_errbell
	cb_error "Couldn't read config file AppBar. Sorry, no application bar available."
	return
    }

    if {$args == ""} {
	_appbar_create
    } else {
	set cmd [lindex $args 0]
	set opts [lrange $args 1 [llength $args]]
	switch $cmd {
	    layout	{eval _appbar_layout $opts}
	    move	{eval _appbar_move}
	    raise	{eval _appbar_raise}
	    close	{eval _appbar_close}
	}
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		_appbar_create
# Args:		none
# Returns: 	""
# Desc:		Builds and displays the application bar.
# Side-FX:	none
#

if ![info exists tkdesk(geometry,dsk_appbar)] {
    set tkdesk(geometry,dsk_appbar) ""
}

proc _appbar_create {} {
    global tkdesk dsk_appbar cb_tools

    set t .dsk_appbar
    if [winfo exists $t] {
	#cb_raise $t
	_appbar_close
	return
    }

    dsk_busy 

    toplevel $t
    wm withdraw $t

    set side top
    set fside left
    set dsk_appbar(layout) vertical
    if {$tkdesk(geometry,dsk_appbar) != ""} {
	set glist [split $tkdesk(geometry,dsk_appbar) x+]
	if {[lindex $glist 0] > [lindex $glist 1]} {
	    set side left
	    set fside top
	    set dsk_appbar(layout) horizontal
	}
    }
    if ![info exists tkdesk(appbar,max)] {set tkdesk(appbar,max) 100}
    set dsk_appbar(tmp_bh) $cb_tools(balloon_help)
    
    # Setup Quick Menus
    menu [set m .dsk_appbar.qmConfig]
    foreach cf $tkdesk(configfiles) {
	$m add command -label $cf \
		-command "dsk_edit_configs $cf"
    }
    menu [set m .dsk_appbar.qmAppbar]
    $m add command -label {Edit AppBar} -command {dsk_edit_configs AppBar}
    $m add command -label {Reload AppBar} -command {dsk_reread_config AppBar}
    $m add separator
    $m add command -label {Vertical} -command {dsk_appbar layout vertical}
    $m add command -label {Horizontal } -command {dsk_appbar layout horizontal}
    $m add command -label {Raise} -command {dsk_appbar raise}
    $m add command -label {Hide} -command {dsk_appbar close}

    set count 0
    set fcount 0
    foreach but $tkdesk(appbar) {
	if {[expr $count % $tkdesk(appbar,max)] == 0} {
	    incr fcount	    
	    frame $t.f$fcount -bg $tkdesk(color,icon_background)
	    pack $t.f$fcount -side $fside -fill both
	    #if {$count == 0} {
	    #	 #frame $t.fHandle -height 16 -bd 2 -relief raised \
	    #	 #	 -cursor top_left_arrow
	    #	 #pack $t.fHandle -in $t.f$fcount -side $side -fill x
	    #	 label $t.lHandle -font fixed -text "" \
	    #		 -bd 2 -relief raised -cursor top_left_arrow
	    #	 pack $t.lHandle -in $t.f$fcount -side $side -fill x
	    #}
	}

	if {[llength $but] > 1} {
	    set bitmap [lindex $but 0]
	    set dsk_appbar(bgcolor) [. cget -background]
	    set dsk_appbar(fgcolor) black
	    if {[llength $bitmap] > 1} {
		if {[lindex $bitmap 1] != ""} {
		    set dsk_appbar(fgcolor) [lindex $bitmap 1]
		}
		if {[llength $bitmap] > 2} {
		    if {[lindex $bitmap 2] != ""} {
			set dsk_appbar(bgcolor) [lindex $bitmap 2]
		    }
		}
		set bitmap [lindex $bitmap 0]
	    }

	    set appmenu [lindex $but 1]
	    menu [set m $t.m$count] -disabledforeground blue2
	    bind $m <ButtonRelease-3> {
		if {[winfo containing %X %Y] != $dsk_appbar(lastbut)} {
		    if {[llength [info args tkMenuInvoke]] == 2} {
			tkMenuInvoke %W 1
		    } else {
			tkMenuInvoke %W
		    }
		}
		break
	    }
	    
	    set dsk_appbar(num_cas) 0
	    set dsk_appbar(defaction) ""
	    set dsk_appbar(deflabel) ""
	    set dsk_appbar(ddaction) ""
	    if [$m cget -tearoff] {
		set inr 1
	    } else {
		set inr 0
	    }
	    foreach me $appmenu {
		if {[llength $me] == 1} {
		    if {$me == "-"} {
			$m add separator
		    } elseif {$me == "history:dirs"} {
			$m add cascade -label "Directories" -menu $m.mhd
			menu $m.mhd -postcommand \
				"history buildmenu $m.mhd open; update"
			# add dummy entry to work around bug in pre Tk 4.0p2:
			$m.mhd add command -label "dummy"
			history changed
			bind $m.mhd <Visibility> "_appbar_mcheck $m $inr %W"
		    } elseif {$me == "history:files"} {
			$m add cascade -label "Files" -menu $m.mhf
			menu $m.mhf -postcommand \
				"file_history buildmenu $m.mhf; update"
			# add dummy entry to work around bug in pre Tk 4.0p2:
			$m.mhf add command -label "dummy"
			file_history changed
			bind $m.mhf <ButtonRelease-3> "
			      set tkdesk(file_lb,control) 0
			      [bind Menu <ButtonRelease-3>]"
			bind $m.mhf <Control-ButtonRelease-3> "
			      set tkdesk(file_lb,control) 1
			      [bind Menu <ButtonRelease-3>]"
			bind $m.mhf <Visibility> "_appbar_mcheck $m $inr %W"
		    } elseif {$me == "history:execs"} {
			$m add cascade -label "Commands" -menu $m.mhe
			menu $m.mhe -postcommand \
				"exec_history buildmenu $m.mhe; update"
			# add dummy entry to work around bug in pre Tk 4.0p2:
			$m.mhe add command -label "dummy"
			exec_history changed
			bind $m.mhe <ButtonRelease-3> "
			      set tkdesk(file_lb,control) 0
			      [bind Menu <ButtonRelease-3>]"
			bind $m.mhe <Control-ButtonRelease-3> "
			      set tkdesk(file_lb,control) 1
			      [bind Menu <ButtonRelease-3>]"
			bind $m.mhe <Visibility> "_appbar_mcheck $m $inr %W"
		    } elseif {$me == "bookmarks"} {
			$m add cascade -label "Bookmarks" -menu $m.book
			menu $m.book -postcommand "dsk_bookmark menu $m.book"
			# add dummy entry to work around bug in pre Tk 4.0p2:
			$m.book add command -label "dummy"
			bind $m.book <ButtonRelease-3> "
			   set tkdesk(file_lb,control) 0
			   [bind Menu <ButtonRelease-3>]"
			bind $m.book <Control-ButtonRelease-3> "
			   set tkdesk(file_lb,control) 1
			   [bind Menu <ButtonRelease-3>]"
			bind $m.book <Visibility> "_appbar_mcheck $m $inr %W"
		    } elseif {$me == "config"} {
			$m add cascade -label "Configuration" -menu $m.cfg
			menu [set tm $m.cfg]
			#menu $m.cfg
			#$m.cfg add cascade -label "Edit Config Files" \
			#	 -menu $m.cfg.edmenu
			#$m.cfg add cascade -label "Reread Config Files" \
			#	 -menu $m.cfg.rdmenu
			#
			#menu [set tm $m.cfg.edmenu]
			$tm add command -label "All" \
				-command "dsk_edit_configs"
			$tm add separator
			foreach cf $tkdesk(configfiles) {
			    $tm add command -label $cf \
				    -command "dsk_edit_configs $cf"
			}

			#menu [set tm $m.cfg.rdmenu]
			#$tm add command -label "All" \
			#	 -command "dsk_reread_config"
			#$tm add separator
			#foreach cf $tkdesk(configfiles) {
			#    $tm add command -label $cf \
			#	     -command "dsk_reread_config $cf"
			#}
		    } elseif {$me == "buffers"} {
			$m add cascade -label "Buffers" -menu $m.bufs
			menu [set tm $m.bufs] -postcommand \
				"dsk_Editor :: bufferMenu $tm"
	            } else {
			$m add command -label [subst [lindex $me 0]] \
				-state disabled
		    }
		} else {
		    _appbar_add_to_menu $m $me
		}
		incr inr
	    }

	    if {[string first "special:" $bitmap] == 0} {
		switch $bitmap {
		    "special:date" {
			set mw [_appbar_date $t.f$fcount $side]
			_appbar_bind_date $count
			set dsk_appbar(date,countval) $count
		    }
		    "special:load" {
			if {[dsk_auto_execok uptime] || \
				$tkdesk(systype) == "Linux"} {
			    set mw [_appbar_load $t.f$fcount $side]
			    _appbar_bind_load $count
			    set dsk_appbar(load,countval) $count
			} else {
			    dsk_errbell
			    cb_error "Disabling load display as I don't know how to get the load on your system.  If you do, please send an email to Christian.Bolik@Mainz.netsurf.de.  Thank you."
			    #incr count -1
			}
		    }
		    "special:mail" {
			_appbar_mail $t.b$count
			pack $t.b$count -in $t.f$fcount -side $side \
				-fill both -ipadx 2 -ipady 2
			bind $t.b$count <3> \
				"_appbar_show_menu $count %X %Y"
			bind $t.m$count <B3-Motion> \
				"_appbar_motion $count %X %Y"
			cb_balloonHelp $t.b$count  $dsk_appbar(deflabel)
			bindtags $t.b$count "appbar $t.b$count Button all"

			if {$dsk_appbar(ddaction) != ""} {
			    blt_drag&drop target $t.b$count handler file \
			    "_appbar_dd_action \"$dsk_appbar(ddaction)\""
			}
		    }
		    "special:trash" {
			_appbar_trash $t.b$count
			pack $t.b$count -in $t.f$fcount -side $side \
				-fill both -ipadx 2 -ipady 2
			bind $t.b$count <3> \
				"_appbar_show_menu $count %X %Y"
			bind $t.m$count <B3-Motion> \
				"_appbar_motion $count %X %Y"
			cb_balloonHelp $t.b$count  $dsk_appbar(deflabel)
			bindtags $t.b$count "appbar $t.b$count Button all"

			if {$dsk_appbar(ddaction) != ""} {
			    blt_drag&drop target $t.b$count handler file \
			    "_appbar_dd_action \"$dsk_appbar(ddaction)\""
			}
		    }
		}

	    } else {
		button $t.b$count -image [dsk_image $bitmap \
			-background $dsk_appbar(bgcolor) \
			-foreground $dsk_appbar(fgcolor)] \
			-activebackground $dsk_appbar(bgcolor) \
			-activeforeground $dsk_appbar(fgcolor) \
			-cursor top_left_arrow \
			-command $dsk_appbar(defaction) \
			-padx 0 -pady 0 -highlightthickness 0
		pack $t.b$count -in $t.f$fcount -side $side -fill both \
			-ipadx 2 -ipady 2
		bind $t.b$count <3> "_appbar_show_menu $count %X %Y"
		bind $t.m$count <B3-Motion> "_appbar_motion $count %X %Y"
		cb_balloonHelp $t.b$count  $dsk_appbar(deflabel)
		#_appbar_bind_global $t.b$count
		bindtags $t.b$count "appbar $t.b$count Button all"

		if {$dsk_appbar(ddaction) != ""} {
		    blt_drag&drop target $t.b$count handler file \
			    "_appbar_dd_action \"$dsk_appbar(ddaction)\""
		}
	    }

	} else {
	    set special [lindex $but 0]
	    switch [lindex $special 0] {
		date {
		    _appbar_date $t.f$fcount $side
		    # the date occupies 2 buttons:
		    incr count
		}
	    }
	}

	incr count
    }


    wm title $t "TkDesk Application Bar"
    wm overrideredirect $t [expr !$tkdesk(appbar,wm_managed)]
    wm deiconify $t
    _appbar_bind_global
    
    if {$tkdesk(geometry,dsk_appbar) == ""} {
	wm geometry $t +0+0
    } else {
	set glist [split $tkdesk(geometry,dsk_appbar) x+]
	wm geometry $t +[lindex $glist 2]+[lindex $glist 3]
    }

    dsk_lazy
}

proc _appbar_mcheck {appmenu nr menu} {
    if {[$appmenu index active] != $nr && \
	[string first "tearoff" $menu] == -1} {
	$menu unpost
    }
}

proc _appbar_add_to_menu {menu cmd} {
	global tkdesk dsk_appbar

	if {[llength $cmd] == 2} {
	    set label [lindex $cmd 0]
	    set command [string_replace [lindex $cmd 1] \" \\\"]
	    if {$label != "dd" && $label != "DD"} {
	        $menu add command -label $label \
			-command "$menu unpost ;\
			_appbar_unpost_parents $menu ;\
			cd \[dsk_active dir\] ;\
			eval \[_expand_pc [list $command]\]; cd ~"
	        if {$dsk_appbar(defaction) == ""} {
		    set dsk_appbar(defaction) \
			    "cd \[dsk_active dir\] ;\
			    eval \[_expand_pc [list $command]\]; cd ~"
		    set dsk_appbar(deflabel) $label
	    	}
	    } else {
		set dsk_appbar(ddaction) $command
	    }
	} elseif {[llength $cmd] == 1} {
	    $menu add separator
	} else {
	    set m ${menu}.mc$dsk_appbar(num_cas)
	    incr dsk_appbar(num_cas)
	    $menu add cascade -label [lindex $cmd 0] -menu $m

	    menu $m
	    set cmd [lreplace $cmd 0 0]
	    foreach c $cmd {
	    	_appbar_add_to_menu $m $c
	    }
	}
}

proc _appbar_unpost_parents {menu} {

    set p [winfo parent $menu]
    while {$p != ""} {
	if {[winfo class $p] == "Menu"} {
	    catch "$p unpost"
	}
	set p [winfo parent $p]
    }
}

proc _appbar_dd_action {cmd} {
    global DragDrop tkdesk

    catch "wm withdraw $tkdesk(dd_token_window)"
    update
    
    if {[string first %A $cmd] > -1} {
	set cmd [string_replace $cmd %A $DragDrop(file)]
    } else {
	set cmd [_expand_pc $cmd]
    }
    cd [dsk_active dir]
    eval $cmd
    cd ~
}

proc _appbar_show_menu {butnum rootx rooty {win ""}} {
    global dsk_appbar

    set t .dsk_appbar

    if {$win == ""} {
	set win $t.b$butnum
    }

    set geom [split [wm geometry $t] x+]
    set tw [lindex $geom 0]
    set th [lindex $geom 1]
#    set bx [lindex $geom 2]
#    set by [lindex $geom 3]
    set bgeom [split [winfo geometry $win] x+]
    set bw [lindex $bgeom 0]
    set bh [lindex $bgeom 1]
    set bx [winfo rootx $win]
    set by [winfo rooty $win]
    set sw [winfo screenwidth $t]
    set sh [winfo screenheight $t]
    set mw [winfo reqwidth $t.m$butnum]
    set mh [winfo reqheight $t.m$butnum]

    if {$tw > $th} {
	# horizontal layout
	set x [winfo rootx $win]
	if {$by > ($sh >> 1)} {
	    set y [expr $by - $mh]
	} else {
	    set y [expr $by + $bh]
	}
    } else {
	# vertical layout
	set y [winfo rooty $win]
	if {$bx > ($sw >> 1)} {
	    set x [expr $bx - $mw]
	} else {
	    set x [expr $bx + $bw]
	}
    }

    #cb_MenuPopupAdd $t.b$butnum 3 $t.m$butnum {} {} 1 $x $y 1
    update
    set dsk_appbar(lastbut) $win
    tk_popup $t.m$butnum $x $y
}

set dsk_appbar(motion) 0
proc _appbar_motion {lastnum x y {win ""}} {
    global dsk_appbar

    if $dsk_appbar(motion) return
    set dsk_appbar(motion) 1
    set t .dsk_appbar
    set new [winfo containing $x $y]
    
    if {$new != $dsk_appbar(lastbut)} {
	if [string match $t.b* $new] {
	    $t.m$lastnum unpost
	    scan $new "$t.b%d" num
	    _appbar_show_menu $num $x $y
	} else {
	    catch {set num $dsk_appbar(num,$new)}
	    if [info exists num] {
		$t.m$lastnum unpost
		_appbar_show_menu $num $x $y $win
	    }
	}
    }
    set dsk_appbar(motion) 0
}


#
# -----------------------------------------------------------------------------
#
# Proc:		_appbar_close
# Args:		none
# Returns: 	""
# Desc:		Removes the application bar.
# Side-FX:	none
#

proc _appbar_close {} {
    global tkdesk

    if {[dsk_active viewer] != 0} {
	if [winfo exists .dsk_appbar] {
	    set tkdesk(geometry,dsk_appbar) [wm geometry .dsk_appbar]
	    destroy .dsk_appbar
	}
    } else {
	cb_info "The application bar cannot be closed because there is no file browser window on screen."
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		_appbar_raise
# Args:		none
# Returns: 	""
# Desc:		Raises the application bar.
# Side-FX:	none
#

proc _appbar_raise {} {
    global tkdesk

    if [winfo exists .dsk_appbar] {
    	raise .dsk_appbar
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		_appbar_layout
# Args:		orient		orientation: horizontal or vertical
# Returns: 	""
# Desc:		Repacks the buttons of the appbar accordingly to $orient.
# Side-FX:	none
#

proc _appbar_layout {orient} {
    global dsk_appbar

    if ![winfo exists .dsk_appbar] return

    if {$orient == "horizontal"} {
	set side left
	set fside top
	set dsk_appbar(layout) horizontal
    } else {
	set side top
	set fside left
	set dsk_appbar(layout) vertical
    }

    foreach obj [winfo children .dsk_appbar] {
	if {[winfo class $obj] == "Button" || \
		[winfo class $obj] == "AppDate" || \
		[winfo class $obj] == "AppLoad"} {
	    if {[winfo class $obj] == "AppDate"} {
		_appbar_date "" $side
		catch {_appbar_bind_date $dsk_appbar(date,countval)}
	    } elseif {[winfo class $obj] == "AppLoad"} {
		_appbar_load "" $side
		catch {_appbar_bind_load $dsk_appbar(load,countval)}
	    }
	    pack config $obj -side $side
	} elseif {[winfo class $obj] == "Frame"} {
	    pack config $obj -side $fside
	}
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		_appbar_move
# Args:		none
# Returns: 	""
# Desc:		Displays a hand cursor to move the appbar around.
# Side-FX:	none
#

proc _appbar_move {} {
    global dsk_appbar tkdesk cb_tools

	if {[winfo depth .] != 1} {
	    set cc wheat
	} else {
	    set cc white
	}

    catch "unset dsk_appbar(released)"
	catch {destroy .dsk_appbar._Busy}
    foreach but [winfo children .dsk_appbar] {
	if {[winfo class $but] != "Button"} continue
	
	$but config -cursor "@$tkdesk(library)/images/hand.xbm \
			$tkdesk(library)/images/hand.mask.xbm \
			black $cc"
	bind appmove <B1-Motion> {
		wm geometry .dsk_appbar +[expr %X - $dsk_appbar(mx)]+[expr %Y - $dsk_appbar(my)]; break}
	bind appmove <ButtonRelease-1> {set dsk_appbar(released) 1; break}
	bind appmove <ButtonPress-1> {
	    set dsk_appbar(mx) [expr %X - [winfo rootx .dsk_appbar]]
	    set dsk_appbar(my) [expr %Y - [winfo rooty .dsk_appbar]]
	    break
	}
	bindtags $but "appmove"
    }

    set cbbh $cb_tools(balloon_help)
    set cb_tools(balloon_help) 0
    set gl [split [wm geometry .dsk_appbar] x+]
    ot_warp_pointer [expr [lindex $gl 2] + 16] [expr [lindex $gl 3] + 16]
    grab -global .dsk_appbar
    tkwait variable dsk_appbar(released)
    grab release .dsk_appbar
    set cb_tools(balloon_help) $cbbh

    catch "unset dsk_appbar(released)"
    foreach but [winfo children .dsk_appbar] {
	if {[winfo class $but] != "Button"} continue
	
	$but config -cursor top_left_arrow
	bindtags $but "appbar $but Button all"
    }
}

#
# -----------------------------------------------------------------------------
#
# Proc:		_appbar_bind_special
# Args:		widgets count args
# Returns: 	""
# Desc:		Binds the widget of a "special" appbar button.
# Side-FX:	none
#

proc _appbar_bind_special {widgets count mw} {
    global dsk_appbar

    foreach w $widgets {
	bind $w	<ButtonPress-1> "$mw config -relief sunken"
	bind $w	<ButtonRelease-1> \
		"$dsk_appbar(defaction); $mw config -relief raised"
	bind $w	<3> "_appbar_show_menu $count %X %Y $mw"
	bindtags $w "appbar $w [winfo class $w] all"
    }
    foreach w $widgets {
	cb_balloonHelp $w $dsk_appbar(deflabel)
	if {$dsk_appbar(ddaction) != ""} {
	    blt_drag&drop target $w handler file \
		    "_appbar_dd_action \"$dsk_appbar(ddaction)\""
	}
    }
}

# ----------------------------------------------------------------------------
# _appbar_bind_global:
# Adds bindings to the widget which apply to all widgets in the appbar
# (most of which are accessed by pressing Meta and mousebutton simultaneously.
#
proc _appbar_bind_global {} {

    bind appbar <Alt-Button-1> {
	set dsk_appbar(tmp_bh) $cb_tools(balloon_help)
	set cb_tools(balloon_help) 0
	set dsk_appbar(mx) [expr %X - [winfo rootx .dsk_appbar]]
	set dsk_appbar(my) [expr %Y - [winfo rooty .dsk_appbar]]
	%W config -cursor "@$tkdesk(library)/images/hand.xbm \
		$tkdesk(library)/images/hand.mask.xbm \
		black wheat"
	raise .dsk_appbar
	break
    }
    bind appbar <Meta-Button-1> [bind appbar <Alt-Button-1>]
    bind appbar <Alt-B1-Motion> {
	wm geometry .dsk_appbar +[expr %X - $dsk_appbar(mx)]+[expr %Y - $dsk_appbar(my)]
	break
    }
    bind appbar <Meta-B1-Motion> [bind appbar <Alt-B1-Motion>]
    bind appbar <ButtonRelease-1> {
	if [info exists dsk_appbar(mx)] {
	    unset dsk_appbar(mx)
	    unset dsk_appbar(my)
	    set cb_tools(balloon_help) $dsk_appbar(tmp_bh)
	    %W config -cursor top_left_arrow
	    break
	}
    }

    # Quick Menus:
    bind appbar <Alt-Button-2> {
	tk_popup .dsk_appbar.qmConfig [expr %X + 1] [expr %Y + 1]
	break
    }
    bind appbar <Meta-Button-2> [bind appbar <Alt-Button-2>]
    bind appbar <Alt-Button-3> {
	tk_popup .dsk_appbar.qmAppbar [expr %X + 1] [expr %Y + 1]
	break
    }
    bind appbar <Meta-Button-3> [bind appbar <Alt-Button-3>]

    # work around bug (?) in Tk 4.0/4.1:
    bind appbar <Button-1> {
	if {%s == 8} {
	    set dsk_appbar(tmp_bh) $cb_tools(balloon_help)
	    set cb_tools(balloon_help) 0
	    set dsk_appbar(mx) [expr %X - [winfo rootx .dsk_appbar]]
	    set dsk_appbar(my) [expr %Y - [winfo rooty .dsk_appbar]]
	    %W config -cursor "@$tkdesk(library)/images/hand.xbm \
		    $tkdesk(library)/images/hand.mask.xbm \
		    black wheat"
	    raise .dsk_appbar
	    break
	}
    }
    bind appbar <Button-2> {
	if {%s == 8} {
	    tk_popup .dsk_appbar.qmConfig [expr %X + 1] [expr %Y + 1]
	    break
	}
    }
    bind appbar <Button-3> {
	if {%s == 8} {
	    tk_popup .dsk_appbar.qmAppbar [expr %X + 1] [expr %Y + 1]
	    break
	}
    }
}

# -----------------------------------------------------------------------------
# _appbar_get_button_width:
# Returns the width of the first button in the appbar - 4. This should
# correspond to the width of the associated pixmap.
#
proc _appbar_get_button_width {} {
    global tkdesk

    set w 32
    for {set i 0} {$i < [llength $tkdesk(appbar)]} {incr i} {
	if [winfo exists .dsk_appbar.b$i] {
	    #update idletasks
	    set w [expr [winfo reqwidth .dsk_appbar.b$i] - 4]
	    break
	}
    }
    return $w
}

#
# -----------------------------------------------------------------------------
#
# Proc:		_appbar_date
# Args:		none
# Returns: 	""
# Desc:		Displays the time and date in the application bar.
# Side-FX:	none
#

proc _appbar_date {frame side} {
    global tkdesk dsk_appbar

    if {$frame != ""} {
	set dsk_appbar(date,frame) $frame
    } else {
	set frame $dsk_appbar(date,frame)
    }
    
    set ft .dsk_appbar.fDate
    if ![winfo exists $ft] {
	frame $ft -class "AppDate"
	pack $ft -fill both -expand yes -side $side -in $frame
    }
    set f .dsk_appbar.fDate.f
    catch {destroy $f}
    frame $f -bd 2 -relief raised
    pack $f -fill both -expand yes -in $ft

    label $f.lTime -font $tkdesk(appbar,font,time) \
	    -pady 0 -bg black -fg green -cursor top_left_arrow \
	    -bd 2 -relief raised
    label $f.lWeekday -font $tkdesk(appbar,font,weekday) \
	    -pady 0 -cursor top_left_arrow
    label $f.lDay -font $tkdesk(appbar,font,day) \
	    -pady 0 -cursor top_left_arrow
    label $f.lMonth -font $tkdesk(appbar,font,month) \
	    -pady 0 -cursor top_left_arrow

    set cw [_appbar_get_button_width]
    set cw4 [expr $cw + 4]
    set cw42 [expr $cw4 / 2]
    
    if {$dsk_appbar(layout) == "vertical"} {
	canvas $f.cDate -bd 0 -relief flat -width $cw4 -height 76 \
		-cursor top_left_arrow -highlightthickness 0

	$f.cDate create window $cw42 2 -window $f.lTime -anchor n
	$f.cDate create window $cw42 20 -window $f.lWeekday -anchor n
	$f.cDate create window $cw42 34 -window $f.lDay -anchor n
	$f.cDate create window $cw42 58 -window $f.lMonth -anchor n

	if {$cw >= 48} {
	    # nasty hack...
	    $f.lTime config -width 6
	}
    } else {
	canvas $f.cDate -bd 0 -relief flat -width 76 -height $cw4 \
		-cursor top_left_arrow -highlightthickness 0

	$f.cDate create window 18 2 -window $f.lTime -anchor n
	$f.cDate create window 18 [expr $cw42 - 1] -window $f.lMonth -anchor n
	$f.cDate create window 58 4 -window $f.lDay -anchor n -height 20
	$f.cDate create window 58 [expr $cw42 + 6] \
		-window $f.lWeekday -anchor n -height 10
    }
    raise $f.lTime
    raise $f.lWeekday
    raise $f.lDay
    raise $f.lMonth

    pack $f.cDate -in $f -fill none -expand yes

    _appbar_get_date
    return $f
}

proc _appbar_bind_date {count} {
    global dsk_appbar

    set f .dsk_appbar.fDate.f
    _appbar_bind_special \
	    [list $f.lTime $f.lWeekday $f.lDay $f.lMonth $f.cDate] \
	    $count $f
}

set _ab_date(wday,0) "Sun"; set _ab_date(wday,1) "Mon"; 
set _ab_date(wday,2) "Tue"; set _ab_date(wday,3) "Wed"; 
set _ab_date(wday,4) "Thu"; set _ab_date(wday,5) "Fri"; 
set _ab_date(wday,6) "Sat";

set _ab_date(mon,0) "Jan"; set _ab_date(mon,1) "Feb"; 
set _ab_date(mon,2) "Mar"; set _ab_date(mon,3) "Apr"; 
set _ab_date(mon,4) "May"; set _ab_date(mon,5) "Jun"; 
set _ab_date(mon,6) "Jul"; set _ab_date(mon,7) "Aug"; 
set _ab_date(mon,8) "Sep"; set _ab_date(mon,9) "Oct"; 
set _ab_date(mon,10) "Nov"; set _ab_date(mon,11) "Dec"; 

proc _appbar_get_date {} {
    global dsk_appbar tkdesk _ab_date

    set f .dsk_appbar.fDate.f
    if ![winfo exists $f.lTime] {
	return
    }

    array set time [dsk_localtime]

    if !$tkdesk(appbar,12hour) {
	$f.lTime config -text "$time(hour):$time(min)"
    } else {
	if {$time(hour) != 12} {
	    $f.lTime config -text \
		    [format "%02d:%02d" [expr $time(hour) % 12] $time(min)]
	} else {
	    $f.lTime config -text "$time(hour):$time(min)"
	}
    }
    if {$time(wday) == 0} {
	# it's a Sunday
	set col red
    } else {
	set col black
    }
    $f.lWeekday config -text $_ab_date(wday,$time(wday)) -fg $col
    $f.lDay config -text $time(mday)
    $f.lMonth config -text $_ab_date(mon,$time(mon))

    if [info exists dsk_appbar(date,afterid)] {
	catch {after cancel $dsk_appbar(date,afterid)}
    }
    set dsk_appbar(date,afterid) [after 60000 _appbar_get_date]
}

#
# -----------------------------------------------------------------------------
#
# Proc:		_appbar_load
# Args:		none
# Returns: 	""
# Desc:		Displays the system load in the appbar.
# Side-FX:	none
#

proc _appbar_load {frame side} {
    global tkdesk dsk_appbar

    if {$frame != ""} {
	set dsk_appbar(load,frame) $frame
    } else {
	set frame $dsk_appbar(load,frame)
    }
    
    set ft .dsk_appbar.fLoad
    if ![winfo exists $ft] {
	frame $ft -class "AppLoad"
	pack $ft -fill both -expand yes -side $side -in $frame
    }
    set f .dsk_appbar.fLoad.f
    catch {destroy $f}
    frame $f -bd 2 -relief raised
    pack $f -fill both -expand yes -in $ft

    set dsk_appbar(load,size) [_appbar_get_button_width]
    set cw $dsk_appbar(load,size)
    
    canvas $f.cLoad -bd 0 -relief flat \
	    -width [expr $cw + 4] -height [expr $cw + 4] \
	    -cursor top_left_arrow -highlightthickness 0

    $f.cLoad create rectangle 1 1 [expr $cw + 1] [expr $cw + 1] \
	    -fill white -outline black

    set dsk_appbar(load,scale) 1.0
    set dsk_appbar(load,lastx) 0
    set dsk_appbar(load,shrinkcnt) 0
    set dsk_appbar(load,uptime_ok) 0
   
    pack $f.cLoad -in $f
    _appbar_get_load

    return $f
}

proc _appbar_bind_load {count} {
    global dsk_appbar

    set f .dsk_appbar.fLoad.f
    _appbar_bind_special [list $f.cLoad] $count $f
}

proc _appbar_get_load {} {
    global dsk_appbar tkdesk

    set f .dsk_appbar.fLoad.f
    if ![winfo exists $f.cLoad] {
	return
    }

    if [file readable /proc/loadavg] {
	set fd [open /proc/loadavg]
	set uptime [gets $fd]
	close $fd
	set load1 [lindex $uptime 0]
    } else {
	set err [catch {set uptime [exec uptime]} errmsg]
	if $err {
	    dsk_errbell
	    cb_alert "Executing uptime gave an error. Disabling load display."
	    return
	}
	set li [llength $uptime]
	set load1 [string trimright [lindex $uptime [incr li -3]] ,\;]
	regsub "," $load1 "." load1
	if !$dsk_appbar(load,uptime_ok) {
	    if ![regexp {[a-zA-Z:]} $load1] {
		set dsk_appbar(load,uptime_ok) 1
	    } else {
		cb_alert "Unknown uptime output. Please mail the output of the uptime command and your system type to Christian.Bolik@mainz.netsurf.de. Disabling load display."
		return
	    }
	}
    }

    set cw $dsk_appbar(load,size)
    set cw1 [expr $dsk_appbar(load,size) + 1]
    set oldscale $dsk_appbar(load,scale)
    if {$load1 >= $dsk_appbar(load,scale)} {
	set dsk_appbar(load,shrinkcnt) 0
	set dsk_appbar(load,scale) [expr ceil($load1)] 
    } else {
	if {$load1 > $dsk_appbar(load,scale) - 1.0} {
	    set dsk_appbar(load,shrinkcnt) 0
	} else {
	    incr dsk_appbar(load,shrinkcnt)
	    if {$dsk_appbar(load,shrinkcnt) >= $cw && $oldscale > 1.0} {
		set dsk_appbar(load,shrinkcnt) 0
		set dsk_appbar(load,scale) [expr $oldscale - 1.0]
	    }
	}
    }
    if {$oldscale != $dsk_appbar(load,scale)} {
	set scale $dsk_appbar(load,scale)
	catch {$f.cLoad delete scale}
	$f.cLoad scale load 1 $cw1 1 [expr $oldscale/$scale]
	if {$scale > 1.0} {
	    for {set y [expr $cw1 - round($cw/$scale)]} {$y > 1} { \
		    set y [expr $y - round($cw/$scale)]} {
		$f.cLoad create line 2 $y $cw1 $y -fill red -tags scale
	    }
	}
    }
    
    set y [expr $cw1 - round($load1 / $dsk_appbar(load,scale) * $cw.)]
    if {$dsk_appbar(load,lastx) < $cw} {
	set x [incr dsk_appbar(load,lastx)]
    } else {
	set x $cw
	set d [$f.cLoad find closest 1 $cw]
	if {[$f.cLoad gettags $d] == "load"} {
	    $f.cLoad delete $d
	}
	$f.cLoad move load -1 0
    }
    $f.cLoad create line $x $cw1 $x $y -tags load
    catch {$f.cLoad raise scale load}

    if [info exists dsk_appbar(load,afterid)] {
	catch {after cancel $dsk_appbar(load,afterid)}
    }
    set dsk_appbar(load,afterid) \
	    [after [expr $tkdesk(appbar,load,delay) * 1000] _appbar_get_load]
}

#
# -----------------------------------------------------------------------------
#
# Proc:		_appbar_mail
# Args:		none
# Returns: 	""
# Desc:		The appbar's xbiff replacement.
# Side-FX:	none
#

proc _appbar_mail {but} {
    global tkdesk dsk_appbar env

    # sort out location of mail folder:
    if [info exists env(MAIL)] {
	set dsk_appbar(mail,folder) $env(MAIL)
    } else {
	foreach dir {/usr/mail /var/mail /usr/spool/mail /var/spool/mail} {
	    if [file isdirectory $dir] {
		set dsk_appbar(mail,folder) \
			$dir/[exec $tkdesk(cmd,whoami)]
		break
	    }
	}
    }
    if ![info exists dsk_appbar(mail,folder)] {
	cb_alert "Couldn't locate your incoming mail folder."
	set dsk_appbar(mail,folder) ""
    }
    set dsk_appbar(mail,laststat) ""

    # set images to use:
    foreach img {nomail oldmail newmail} {
	set bitmap $tkdesk(appbar,mail,$img)
	set dsk_appbar(mail,img,$img) [dsk_image $bitmap]
    }

    # create button:
    set dsk_appbar(mail,button) $but
    button $but -image $dsk_appbar(mail,img,nomail) \
	    -activebackground $dsk_appbar(bgcolor) \
	    -activeforeground $dsk_appbar(fgcolor) \
	    -cursor top_left_arrow \
	    -command $dsk_appbar(defaction) \
	    -padx 0 -pady 0 -highlightthickness 0
    
    _appbar_check_mail
}

proc _appbar_check_mail {} {
    global dsk_appbar tkdesk

    set b $dsk_appbar(mail,button)
    if ![winfo exists $b] return
    
    set f $dsk_appbar(mail,folder)
    if ![file exists $f] {
	set ns nomail
    } else {
	file stat $f stat
	if {$stat(size) == 0} {
	    set ns nomail
	} else {
	    if {$stat(mtime) > $stat(atime)} {
		set ns newmail
	    } else {
		set ns oldmail
	    }
	}
    }

    if {$ns != $dsk_appbar(mail,laststat)} {
	set dsk_appbar(mail,laststat) $ns
	$b config -image $dsk_appbar(mail,img,$ns)
	if {$ns == "newmail"} {
	    if {$tkdesk(appbar,mail,newbg) != ""} {
		$b config -bg $tkdesk(appbar,mail,newbg)
	    }
	    dsk_sound dsk_new_mail beep
	} else {
	    if [info exists tkdesk(color,basic)] {
		$b config -bg $tkdesk(color,basic)
	    }
	}
    }
    
    if [info exists dsk_appbar(mail,afterid)] {
	catch {after cancel $dsk_appbar(mail,afterid)}
    }
    set dsk_appbar(mail,afterid) \
	    [after [expr $tkdesk(appbar,mail,delay) * 1000] _appbar_check_mail]
}

# ----------------------------------------------------------------------------
# _appbar_trash but:
# Creates a trash button that's displays the current fill state of the
# trash can.
#

set dsk_appbar(trash,button) ""

proc _appbar_trash {but} {
    global tkdesk dsk_appbar
    
    # set images to use:
    foreach img {empty full} {
	set bitmap $tkdesk(appbar,trash,$img)
	set dsk_appbar(trash,img,$img) [dsk_image $bitmap]
    }

    # create button:
    set dsk_appbar(trash,button) $but
    button $but -activebackground $dsk_appbar(bgcolor) \
	    -activeforeground $dsk_appbar(fgcolor) \
	    -cursor top_left_arrow \
	    -command $dsk_appbar(defaction) \
	    -padx 0 -pady 0 -highlightthickness 0

    if {[llength [dsk_ls -a $tkdesk(configdir)/.trash]] > 2} {
	$but config -image $dsk_appbar(trash,img,full)
    } else {
	$but config -image $dsk_appbar(trash,img,empty)
    }
}

proc _appbar_trash_refresh {{state unknown}} {
    global tkdesk dsk_appbar

    if ![winfo exists $dsk_appbar(trash,button)] return
    set but $dsk_appbar(trash,button)

    switch $state {
	"empty" {
	    $but config -image $dsk_appbar(trash,img,empty)
	}
	"full" {
	    $but config -image $dsk_appbar(trash,img,full)
	}
	unknown {
	    if {[llength [dsk_ls -a $tkdesk(configdir)/.trash]] > 2} {
		$but config -image $dsk_appbar(trash,img,full)
	    } else {
		$but config -image $dsk_appbar(trash,img,empty)
	    }
	}
    }
}
