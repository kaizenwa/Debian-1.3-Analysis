##
## This is the initialization file for the Tkined network editor.
##
## Copyright (c) 1993, 1994, 1995
##
## J. Schoenwaelder
## TU Braunschweig, Germany
## Institute for Operating Systems and Computer Networks
##
## Permission to use, copy, modify, and distribute this
## software and its documentation for any purpose and without
## fee is hereby granted, provided that this copyright
## notice appears in all copies.  The University of Braunschweig
## makes no representations about the suitability of this
## software for any purpose.  It is provided "as is" without
## express or implied warranty.
##

# option add Tkined*exportSelection true startupFile

##
## I dont like the fat font tk uses per default. So here is good
## old fixed font.
##

option add Tkined*Text.font        fixed startupFile

##
## Redefine the new tk 4.0 highlight rectangles drawn around the 
## outside of the widget when it has the input focus.
##

#option add Tkined*highlightThickness 0 startupFile
#option add Tkined*Entry.highlightThickness 1 startupFile
#option add Tkined*Button.highlightThickness 1 startupFile
#option add Tkined*Radiobutton.highlightThickness 1 startupFile
#option add Tkined*Checkbutton.highlightThickness 1 startupFile
#option add Tkined*Scale.highlightThickness 1 startupFile
#option add Tkined*Scrollbar.highlightThickness 1 startupFile

##
## Redefine the new tk 4.0 paddings to get things small.
##

#option add Tkined*padX 1 startupFile
#option add Tkined*padY 1 startupFile

##
## Redefine the new tk 4.0 activeBorderWidth used inside menus.
##

#option add Tkined*activeBorderWidth 1 startupFile

##
## Make scrollbars smaller than the default.
##

#option add Tkined*Scrollbar.width 12 startupFile

##
## Options specific to the BLT extension.
##

option add Tkined*plotBorderWidth 1 startupFile
option add Tkined*plotRelief sunken startupFile
option add Tkined*legendBorderWidth 0 startupFile
option add Tkined*lineElemScale 0.5 startupFile
option add Tkined*lineElemLinewidth 1 startupFile
option add Tkined*lineElemBackground white startupFile
option add Tkined*lineElemActiveBackground black startupFile
# option add Tkined*halo 4 startupFile
option add Tkined*xTitle "" startupFile
option add Tkined*yTitle "" startupFile
option add Tkined*xFont fixed startupFile
option add Tkined*yFont fixed startupFile
option add Tkined*elemForeground black
option add Tkined*elemBackground white

##
## Read the site specific initialization file.
##

if [file exists $tkined_lib/site/init.tcl] {
    source $tkined_lib/site/init.tcl
}

##
##
##

proc STREAM:Stripchart { id values } {
    static strip
    if {![info exists strip($id)]} {
	set strip($id) [STRIPCHART create]
	$strip($id) canvas [$id canvas]
    }
    $strip($id) values [lindex $values 0]
    return $values
}

proc STREAM:Barchart { id values } {
    static bar
    if {![info exists bar($id)]} {
	set bar($id) [BARCHART create]
	$bar($id) canvas [$id canvas]
    }
    $bar($id) values $values
    return $values
}

proc STREAM:Echo { id values } {
    puts stdout "$values"
    return $values
}
