# A Simple Multi-User Text Editor that Uses Turntaking.

# =============================================================
# Do some standard groupkit setups...
# -------------------------------------------------------------
# Initialize conference
gk_initConf $argv


# =============================================================
# Procedures To Add Items to the GroupKit menu bar 
# -------------------------------------------------------------

# Add menus and items to the GroupKit menu bar
proc menuAddItems {bar} {
    global gk_menu

    # Add Items to the existing "File" menu
    $bar itemcommand 0 insert 1 command -label New -command Clear -underline 0
    $bar itemcommand 0 insert 2 command -label Open -command Open -underline 0
    $bar itemcommand 0 insert 3 command -label Save -command Save -underline 0
    $bar itemcommand 0 insert 4 separator

    # Create and add Items to a "Selection" menu
    $bar add select 1 \
         -text "Selection"
    menuAddItem $bar 1 "Copy" "Copy" 0
    menuAddItem $bar 1 "Paste" {Paste $textausschnitt} 0
    $bar itemcommand 1 add separator
    menuAddItem $bar 1 "Cut" "Cut" 1
    menuAddItem $bar 1 "Delete" "Delete" 0

    # Create and add Items to a "Configuration" menu
    $bar add config 2 \
        -text "Configuration"
    menuAddItem $bar 2 "Set Colours" "Colour" 4
    menuAddItem $bar 2 "Set Font" "Fontset" 4
}

# A convenience routine that will add menu items to a menu
proc menuAddItem {bar menu label command underline} {
    $bar itemcommand $menu add command \
       -label $label \
       -command $command \
       -underline $underline
}


# =============================================================
# Procedures To Create a Text Widget and Add Bindings to it
# -------------------------------------------------------------

# Create a new textwidget with scrollbar
proc createTextWidget {font fontsize bg_color fg_color} {
    scrollbar .scroll -orient vertical
    text .text \
	-yscrollcommand {.scroll set} \
	-width 50 -height 10  \
	-font "$font$fontsize-*" \
	-background $fg_color \
	-foreground $bg_color
    bindTextWidget 
    .scroll configure -command {.text yview}
    pack .scroll -side right -fill y
}

proc bindTextWidget {} {
    global access
    bindtags .text {.text Text}
    
    bind .text <Return> {
	if {$access == "true"} {
	    syncCursor
	    gk_toAll tinsert "\n"
	}
	break
    }
    bind .text <Any-KeyPress> {
	if {$access == "true"} {
	    syncCursor
	    gk_toAll tinsert "%A" 
	}
	break
    }
    bind .text <Delete> {
	if {$access == "true"} {
	    syncCursor
	    gk_toAll .text delete insert-1char insert 
	}
	break
    }
    bind .text <BackSpace> {
	if {$access == "true"} {
	    syncCursor
	    gk_toAll .text delete insert-1char insert 
	}
	break
    }
    bind .text <Left>  {
	if {$access == "true"} {
	    syncCursor
	    gk_toAll bewegen .text -1 0 
	} 
	break
    }
    bind .text <Right> {
	if {$access == "true"} {
	    syncCursor
	    gk_toAll bewegen .text 1 0 
	}
	break
    }
    bind .text <Down> {
	if {$access == "true"} {
	    syncCursor
	    gk_toAll bewegen .text 0 1 
	}
	break
    }
    bind .text <Up> {
	if {$access == "true"} {
	    syncCursor
	    gk_toAll bewegen .text 0 -1 
	}
	break
    }
}



# =============================================================
# Procedures To Create and Configure a Status Label
# -------------------------------------------------------------

proc createStatusLabel {writer requestor access} {
    label .status \
	-text "Writer: $writer   Next: $requestor Access: $access"
}

proc changeStatusLabel {writer requestor access} {
    .status configure \
	-text "Writer: $writer   Next: $requestor Access: $access"
}



# =============================================================
# Procedures To Create and Configure a Turntaking Button
# -------------------------------------------------------------

# Create a button to enable the user to call for the writing-access
proc createTurnTakingButton {} {
    global access want_to_write number name
    button .turn_button \
	-text "Request Turn" \
	-command {
	    checkwriter 
	    if { ( $access == "false" ) && ( $want_to_write == 0 )} {
		gk_toAll Schreibrecht $number $name 
	    }
	}

    # Giving the writing-access to the user opening a conference
    if {[gk_amOriginator ] == 1 } {
	set access true
	.turn_button configure \
	    -text "Give turn to next user" \
	    -command abgeben
    } else {
	set access false
    } 
}

# Procedure for changing the writing-access button's display

proc setTurnTakingButton {} {
    global access number name
    if {$access == "true"} {
	.turn_button configure \
	    -text "Give turn to next user" -command abgeben
    } else { 
	.turn_button configure \
	    -text  "Request Turn" \
	    -command {
		checkwriter
		if { ( $access == "false" ) && ( $want_to_write == 0 )} { 
		    gk_toAll Schreibrecht $number $name }
	    }
    }
}



# =============================================================
# Procedures For Clearing Text, Opening and Saving Files
# -------------------------------------------------------------

# Procedure for clearing the current textwidget
proc Clear {} {
	global access
	if {$access == "true"} {
		toplevel .new
		frame .new.fr
		label .new.fr.l1 \
		    -text " Are you sure ? " \
		    -borderwidth 2 -relief ridge -padx 20 -pady 20
		button .new.fr.yes \
		    -text " Yes " \
		    -borderwidth 3  \
		    -command {gk_toAll .text delete 0.1 end; destroy .new}
		button .new.fr.no \
		    -text "  No  "\
		    -borderwidth 3 \
		    -command {destroy .new}
		pack .new.fr.l1 -side top -padx 20 -pady 20 
		pack .new.fr.yes -side left 
		pack .new.fr.no -side right
		pack .new.fr -fill both -padx 100 -pady 70
	}
}

# Procedure for loading existing files into the textwidget
proc Open {} {
	global access
	if {$access == "true"} {
  		set fln [FSBox]
  		gk_toAll .text delete 0.1 end
  		set fnnummer [open $fln r]  
  		while {[eof $fnnummer] < 1} {
    			set txtzeile [read $fnnummer]
    			gk_toAll .text insert insert $txtzeile
    		}
  		close $fnnummer
  	}
}

# Procedure for saving the current textwidget
proc Save {{w .tpl}} {
	set fid 1
	while {$fid == 1} {
		set dateiname [FSBox]
		set fid [file isdirectory $dateiname]
		set dateinamel [string length $dateiname]
		if {$dateinamel == 0} {set fid 0}
	}
	if {$dateinamel > 0} {
		set fn [eval open $dateiname w+]
		set s [eval .text get 0.1 end]
		puts $fn $s
		close $fn
	}
}



# =============================================================
# Procedures For Editing Text
# -------------------------------------------------------------

# Insert Text
proc tinsert {was} {
	.text insert insert "$was"
}

# COPY
proc Copy {} {
	global access textausschnitt
	if {$access == "true"} {set fehler [catch {set tas [selection get]}]
		if {$fehler == 0} {set textausschnitt [selection get]
		}
	}
}

# PASTE
proc Paste {textausschnitt} {
	global access
	syncCursor
	if {$access == "true"} {
	    gk_toAll .text insert insert $textausschnitt
	}
}

# DELETE
proc Delete {} {
    global access
    if {$access == "true"} {
	set pos [.text tag nextrange sel 1.0 end]
	set fehler [string length $pos]
	if {$fehler > 0} {
	    gk_toAll .text delete [lindex $pos 0] [lindex $pos 1]
	}
    }
}

# CUT = COPY + DELETE
proc Cut {} {
	global textausschnitt
	set fehler [catch {set tas [selection get]}]
	if {$fehler == 0} {set textausschnitt [selection get]}
	set pos [.text tag nextrange sel 1.0 end]
	set fehler [string length $pos]
	if {$fehler > 0} {gk_toAll .text delete [lindex $pos 0] [lindex $pos 1]}
}


# ================================================================
# WRITING-ACCESS PROCEDURES
#-----------------------------------------------------------------

# The following procedure checks, if the holder of the writing-access
# still exists, and in case of a negativ answer forwards it to the next user
proc checkwriter {} {
    global writer_nummer name number access want_to_write want_to_write_name writer
    if {$writer_nummer != 0} {
	set a [gk_findUser $writer_nummer]
	if {$a ==  ""} { 
	    if { $want_to_write == 0 } { 
		gk_toAll set writer_nummer $number
		gk_toAll set writer $name
		set access true 
		gk_toAll Schreibrecht 0 nobody
		setTurnTakingButton 
	    } else { 
		gk_toUserNum $want_to_write set access true
		gk_toAll set writer_nummer $want_to_write
		gk_toAll set writer $want_to_write_name
		gk_toAll Schreibrecht $number $name
		gk_toAll setTurnTakingButton 
	    }
	}
    }
}

# Procedure for giving the writing-access to the next user while quitting the conference
proc updateEntrant entrantUserNum {
    global want_to_write want_to_write_name writer writer_nummer

    gk_toUserNum $entrantUserNum set access false
    gk_toUserNum $entrantUserNum set writer $writer
    gk_toUserNum $entrantUserNum set writer_nummer $writer_nummer
    gk_toUserNum $entrantUserNum Schreibrecht $want_to_write $want_to_write_name
    gk_toUserNum $entrantUserNum setTurnTakingButton
    gk_toUserNum $entrantUserNum .text delete 1.0 end
    set abfresh [.text get 1.0 end]
    gk_toUserNum $entrantUserNum .text insert 1.0 $abfresh
    unset abfresh
    syncCursor
}
 
# Procedure for giving the writing-access to the next user
proc abgeben {} {
    global want_to_write access number writer want_to_write_name
    set a [gk_findUser $want_to_write]
    if {$a ==  ""} { gk_toAll Schreibrecht 0 nobody }
    if {$want_to_write != 0} {
	set access false
	gk_toUserNum $want_to_write set access true
	gk_toAll set writer $want_to_write_name
	gk_toAll set writer_nummer $want_to_write
	gk_toAll Schreibrecht 0 nobody
	gk_toAll setTurnTakingButton 
    }
}

# Procedure for requesting the writing-access
proc Schreibrecht {num na} {
    global access want_to_write want_to_write_name number writer writer_nummer
    set want_to_write $num
    set want_to_write_name $na
    changeStatusLabel $writer $want_to_write_name $access    
}    

 
# ================================================================
# THE SYNCRONE CURSOR AND THE CURSOR KEYS
#-----------------------------------------------------------------

proc syncCursor {} {
    set aktpos [.text index insert]
    gk_toOthers .text mark set insert $aktpos
}

proc bewegen {textfenster x y} {
    set akt_pos [$textfenster index insert]
    scan $akt_pos "%d.%d" zeile spalte
    set zeile [expr $zeile+$y]
    set spalte [expr $spalte+$x]
    set mogl [catch {set s [eval $textfenster get $zeile.0 $zeile.end]}]
    if { !$mogl && ($y != 0) && ($spalte > [string length $s])}  {
	set spalte [string length $s]
    }
    if {$spalte < 0} {
	set zeile [expr $zeile-1]
	set s [eval $textfenster get $zeile.0 $zeile.end]
	set spalte [string length $s]
    }
    $textfenster mark set insert $zeile.$spalte
    $textfenster yview -pickplace [expr $zeile-1]
}

# ================================================================
# THE COLOUR PROBLEM
#----------------------------------------------------------------

proc convert d {
    set tabelle 0123456789abcdef
    set h1 [expr $d%16]
    set d1 [expr $d/16]
    set h2 [expr $d1%16]
    set d2 [expr $d1/16]
    set h3 [expr $d2%16]
    set d3 [expr $d2/16]
    set h4 [expr $d3%16]
    set e1 [string index $tabelle $h1]
    set e2 [string index $tabelle $h2]
    set e3 [string index $tabelle $h3]
    set e4 [string index $tabelle $h4]
    set e5 $e4$e3$e2$e1
    return $e5
}

proc setcol fred {
    set er .ftpl.frim
    set f1 [$er.s1 get]
    set f2 [$er.s2 get]
    set f3 [$er.s3 get]
    set f4 [$er.s4 get]
    set f5 [$er.s5 get]
    set f6 [$er.s6 get]
    set mul [expr 65535/1000]
    set f1 [expr $f1*$mul]
    set f2 [expr $f2*$mul]
    set f3 [expr $f3*$mul]
    set f4 [expr $f4*$mul]
    set f5 [expr $f5*$mul]
    set f6 [expr $f6*$mul]
    set w1 [convert $f1]
    set w2 [convert $f2]
    set w3 [convert $f3]
    set w4 [convert $f4]
    set w5 [convert $f5]
    set w6 [convert $f6]
    set bfarbe $w1$w2$w3
    set ffarbe $w4$w5$w6
    .text configure -background #$bfarbe -foreground #$ffarbe
}

proc Colour {} {
    global altfg altbg
    catch {destroy .ftpl}
    toplevel .ftpl
    frame .ftpl.frim
    pack .ftpl.frim
    wm title  .ftpl "Farbenwahl"
    set dummy [.text configure -fg]
    set altfg [lindex $dummy 4]
    set dummy [.text configure -bg]
    set altbg [lindex $dummy 4]
    scale .ftpl.frim.s1 \
	-orient vertical \
	-label "Rot" \
	-length 400 \
	-from 0 \
	-to 1000 \
	-width 10 \
	-command "setcol" \
	-tickinterval 50
    scale .ftpl.frim.s2 \
	-orient vertical \
	-label "Gruen" \
	-length 400 \
	-from 0 \
	-to 1000 \
	-width 10 \
	-command "setcol" \
	-tickinterval 50
    scale .ftpl.frim.s3 \
	-orient vertical \
	-label "Blau" \
	-length 400 \
	-from 0 \
	-to 1000 \
	-width 10 \
	-command "setcol" \
	-tickinterval 50
    scale .ftpl.frim.s4 \
	-orient horizontal \
	-label "Rot - Vordergrund" \
	-length 400 \
	-from 0 \
	-to 1000 \
	-width 10 \
	-command "setcol" \
	-tickinterval 100
    scale .ftpl.frim.s5 \
	-orient horizontal \
	-label "Grn - Vordergrund" \
	-length 400 \
	-from 0 \
	-to 1000 \
	-width 10 \
	-command "setcol" \
	-tickinterval 100
    scale .ftpl.frim.s6 \
	-orient horizontal \
	-label "Blau - Vordergrund" \
	-length 400 \
	-from 0 \
	-to 1000 \
	-width 10 \
	-command "setcol" \
	-tickinterval 100
    button .ftpl.frim.fok \
	-text "OK" \
	-command {destroy .ftpl}
    button .ftpl.frim.fca \
	-text "Cancel" \
	-command {
	    global altfg 
	    altbg.text configure -bg $altbg -fg $altfg
	    destroy .ftpl
	}
    pack .ftpl.frim.s1 .ftpl.frim.s2 .ftpl.frim.s3 -side left
    pack .ftpl.frim.s4 .ftpl.frim.s5 .ftpl.frim.s6 -side top
    pack .ftpl.frim.fok .ftpl.frim.fca -side left
}



#================================================================
# Procedures for changing the fonts
#-----------------------------------------------------------------
proc Fontset {} {
    global font fontsize neufont groesse
    toplevel .font
    label .font.l -text "Current Font: $font"
    frame .font.f
    listbox .font.fontlist -relief sunken -width 24 -height 7
    .font.fontlist insert end Courier Helvetica "New Century Schoolbook" Times Lucida Lucidabright Lucidatypewriter 
    listbox .font.fontsize -relief sunken -width 4 -height 6
    .font.fontsize insert end  8 10 12 14 18 24 
    
    label .font.f.bsp -text "Example Text"  -relief ridge -font "$font$fontsize-*"
    frame .font.f2	
    button .font.f2.nok \
	-text "Abort" \
	-command {destroy .font}
    button .font.f2.ok \
	-text "OK" \
	-command { 
	    if { [ catch {.text config -font "$neufont$groesse-*"} ] > 0 } {
		catch {destroy .fehl}
		toplevel .fehl
		wm geometry .fehl +300+250
		#                	wm geometry .fehl 300x100
		wm title .fehl "ERROR !!!"
		label .fehl.meld -text "The font $neufont $groesse does not exist" -font "*-times-bold-r-normal--*-120-*"
		button .fehl.ok -text "OK" -relief raised -command "destroy .fehl" 
		pack .fehl.meld -side top -fill x
		pack .fehl.ok -side bottom -fill x -expand yes 
	    } else {
		set font $neufont
		set fontsize $groesse
		destroy .font 
	    }
  	}
    
    pack .font.l
    pack .font.f.bsp -fill x -padx 5 -pady 5
    pack .font.f2.ok .font.f2.nok -fill x -side bottom -pady 5 -ipadx 2
    pack .font.fontlist .font.fontsize .font.f .font.f2 -side left -padx 5 -pady 5
    bind .font.fontlist <Double-Button-1> { 
	set i [.font.fontlist curselection]
	switch $i {
	    0 {set neufont *-Courier-medium-r-*-}
	    1 {set neufont *-Helvetica-medium-r-*-}
	    2 {set neufont "*-New Century LucidaSchoolbook-medium-r-*-"}
	    3 {set neufont *-Times-medium-r-*-}
	    4 {set neufont *-Lucida-medium-r-*-}
	    5 {set neufont *-Lucidabright-medium-r-*-}
	    6 {set neufont *-Lucidatypewriter-medium-r-*-}
	} 
	if { [catch {.font.f.bsp config -font "$neufont$groesse-*"}] > 0 } {
	    catch {destroy .fehl}
	    toplevel .fehl
	    wm geometry .fehl +300+250
	    #                	wm geometry .fehl 300x100
	    wm title .fehl "ERROR !!!"
	    label .fehl.meld -text "The font $neufont $groesse does not exist" -font "*-times-bold-r-normal--*-120-*"
	    button .fehl.ok -text "OK" -relief raised -command "destroy .fehl" 
	    pack .fehl.meld -side top -fill x
	    pack .fehl.ok -side bottom -fill x -expand yes 
	}
    }
    bind .font.fontsize <Double-Button-1> {
	set index2 [.font.fontsize curselection] 
	switch $index2 {
	    0 {set groesse 8}
	    1 {set groesse 10}
	    2 {set groesse 12}
	    3 {set groesse 14}
	    4 {set groesse 18}
	    5 {set groesse 24}
	}
	if { [catch {.font.f.bsp config -font "$neufont$groesse-*"}] > 0 } {
	    catch {destroy .fehl}
	    toplevel .fehl
	    wm geometry .fehl +300+250
	    #                	wm geometry .fehl 300x100
	    wm title .fehl "ERROR !!!"
	    label .fehl.meld -text "This font does not exist" -font "*-times-bold-r-normal--*-180-*"
	    button .fehl.ok -text "OK" -relief raised -command "destroy .fehl" 
	    pack .fehl.meld -side top -fill x
	    pack .fehl.ok -side bottom -fill x -expand yes 
	}        
    } 
}


# ================================================================
# Globals: Help description
# -------------------------------------------------------------
set ed(title)  \
     "Text Editor"

set ed(description) {
{normalbold} {A Simple Multi-User Text Editor that Uses Turntaking.

}
{normal} {This multi-user-editor works on the basis of a\
 textwidget. Although rather restricted, the editor includes several\
 useful functions.

1. Changes made by each user are displayed on all terminals involved\
 in the conference. 

2. In addition to simple write and delete functions the users\
 can employ commands such as "save" for saving documents into specific files\
 anytime during and after conferences and \"load\" for loading previously saved\
 files into the textwidget in order to work on them. 

3. Functions like "cut", "copy", and "paste" are available to the user. 

4. Cursor keys are functional and the user can use the mouse to move the cursor,\
 for users can only write and delete chars at cursor position. 

5. The writing-access is first given to the user opening the conference and\
 can later be requested by other members via pressing a button. If a user is\
 granted the right to work on the document, all other members of a conference\
 are blocked until the writer hands over the writing-access to the user\
 who first demanded it.

This was a student project, in a class run by Michael Tauber in\
 the Computer Science Department at the University\
 of Paderbor, Germany. The students are:
   -Christine Barckow
   -Thomas Nagel
   -Bengt Mueck
   -Stefan Ludewig

The submitted code was cleaned up and modified somewhat by Saul Greenberg.}
}

# ================================================================
# Globals:  Colors and Fonts
#----------------------------------------------------------------

set altfg white
set altbg red

# Default Fonts
set font "*-Times-medium-r-*-"
set fontsize 14
set colhint white
set colvor black
set groesse $fontsize
set neufont $font

#================================================================
# Globals for writing access
#----------------------------------------------------------------

# Identify the user name and start the conference
set number [users local.usernum]
set name [users local.username]

# Define global variables for the Writing-access-problem

set access true
set want_to_write 0
set want_to_write_name nobody 
set writer $name
set writer_nummer $number

# ================================================================
# Main
# -------------------------------------------------------------

# Set some window attributes
wm title . "Text Editor"
wm minsize . 50 6

# Create the Groupkit menubar and modify it
gk_defaultMenu .topmenu
pack .topmenu -side top -fill x 

.topmenu itemcommand 2 add command -label $ed(title) \
                          -command "gk_topicWindow .helpWindow \
                                    -title \"$ed(title)\" \
                                    -text {$ed(description)}"
menuAddItems .topmenu
gk_bind updateEntrant "updateEntrant %U"

#----------------------------------------------------
# CREATING THE WINDOW 
#----------------------------------------------------

createTextWidget $font $fontsize $colhint $colvor
.text config -font "$font$fontsize-*" -background $colhint -foreground $colvor

createStatusLabel $writer $want_to_write_name $access
createTurnTakingButton
pack .text
pack .status
pack .turn_button


