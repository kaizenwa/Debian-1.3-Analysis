# =============================================================================
#
# File:		dsk_help.tcl
# Project:	TkDesk
#
# Started:	22.10.94
# Changed:	21.12.94
#
# Description:	Contains the on-line help and "About"-dialog.
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
#	proc dsk_about {}
#	proc dsk_help_window {}
#
# =============================================================================

#
# -----------------------------------------------------------------------------
#
# Proc:		dsk_about
# Args:		none
# Returns: 	""
# Desc:		Shows the "About..." dialog box.
# Side-FX:	none
#

proc dsk_about {} {
    global tkdesk

    set t .dsk_about
    if [winfo exists $t] {
	cb_raise $t
	#$t.la config -image dsk_blank
	#update idletasks
	#cb_aboutAnim $t.la $tkdesk(library)/about/tkdesk 10 0 blue3
	dsk_sound dsk_about
	return
    }

    toplevel $t
    wm withdraw $t

    frame $t.f -bd 3 -relief raised
    pack $t.f -ipadx 4 -ipady 4
    
    #image create bitmap dsk_blank \
	#    -data [exec gzip -cd $tkdesk(library)/about/blank.xbm.gz \
	#    2>/dev/null]
    image create bitmap welcome_bm -data $tkdesk(welcome_bm) -foreground blue3
    label $t.la -image welcome_bm
    pack $t.la -in $t.f -padx $tkdesk(pad) -pady [expr $tkdesk(pad) + 2]

    message $t.m -aspect 1000 -justify center \
	    -text "A File Manager for\nUnix and the X Window System\n\nVersion $tkdesk(version), dated $tkdesk(date)\n\nWritten by Christian Bolik\n($tkdesk(authormail))\n\nPlease don't hesitate to send me suggestions\nfor improvements and future enhancements!\n"
    pack $t.m -in $t.f -padx $tkdesk(pad) -pady $tkdesk(pad) -ipady 1

    frame $t.fButtons
    pack $t.fButtons -in $t.f
    
    button $t.b -text "OK" -width 6 -command "destroy $t"
    pack $t.b -in $t.fButtons \
	    -padx $tkdesk(pad) -pady $tkdesk(pad) -side left

    button $t.bLic -text " License... " -command "dsk_help license"
    pack $t.bLic -in $t.fButtons \
	    -padx $tkdesk(pad) -pady $tkdesk(pad) -side left

    wm title $t "About TkDesk"
    wm iconname $t "About TkDesk"
    wm resizable $t 0 0
    cb_centerToplevel $t
    wm deiconify $t

    # start the animation:
    #update idletasks
    #cb_aboutAnim $t.la $tkdesk(library)/about/tkdesk 10 0 blue3
    dsk_sound dsk_about
}

# ----------------------------------------------------------------------------
# dsk_help:
# Displays the TkDesk User's Guide etc. If there is a running Netscape TkDesk
# will use this for display, otherwise TkDesk uses its own help viewer.
#

proc dsk_help {what} {
    global tkdesk

    update idletasks
    dsk_busy
    set cbhm ""
    switch $what {
	"guide" {
	    set nfile $tkdesk(library)/doc/guide.html
	    set tfile $tkdesk(library)/doc/Guide
	    set cbhm howto
	}
	"changes" {
	    set nfile $tkdesk(library)/doc/CHANGES
	    set tfile $tkdesk(library)/doc/CHANGES
	}
	"license" {
	    set nfile $tkdesk(library)/doc/License
	    set tfile $tkdesk(library)/doc/License
	}
	"quick" {
	    set nfile $tkdesk(library)/doc/QuickStart
	    set tfile $tkdesk(library)/doc/QuickStart
	}
	"faq" {
	    set nfile $tkdesk(library)/doc/guide-8.html
	    set tfile $tkdesk(library)/doc/Guide\#Frequently
	    set cbhm howto
	}
    }
    
    if $tkdesk(netscape_help) {
	dsk_netscape file $nfile window
    } else {
	if {$cbhm != ""} {
	    dsk_cbhelp $tfile $cbhm
	} else {
	    dsk_cbhelp $tfile
	}
    }
    dsk_lazy
}
