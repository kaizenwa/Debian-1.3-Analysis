#
# overview widget
#

# create the overview
proc overview {w args} {
  # pull out the -mainText option
  set argnum 0
  set mainText ""
  set found 0
  foreach thisarg $args {
    if {$thisarg == "-mainText"} {
        set mainText [lindex $args [expr $argnum + 1]]
        set found 1
        break
    }
    incr argnum
  }
  if {$found == 1} {
      set args [lreplace $args $argnum [expr $argnum + 1]]
  }

  eval text $w $args

  overview_setupFisheye $w

  if {$found == 1} {
      overview_attachToMainText $w $mainText
      overview_copyTo $w $mainText
  }

  return $w
}

# set up the fisheye lens
proc overview_setupFisheye {w} {
    gk_newenv info$w
 
    # Font and Default Fisheye configuration
    info$w import mag_factor \
        { {4 3} {6 3} {8 2} {10 1} {12 1} }

    info$w default_fontsize 2
    info$w others_size 10
    info$w others_range 1

    info$w global_focus 1
    info$w others {}

    info$w numb_fonts [overview_createFonts $w]
    set self [users local.usernum]
    overview_createFontTags $w user${self}font

    $w configure -font [overview_fontName [info$w default_fontsize]]

    gk_bind userDeleted "overview_deleteOther $w %U"

    bindtags $w [list $w . all]
}

# reset to defaults
proc overview_reset {w} {
    info$w default_fontsize 2
    info$w others_size 10
    info$w others_range 1

    info$w global_focus 1

    info$w delete others
    info$w others {}
    $w delete 1.0 end-1c
}


# delete a user that just left
proc overview_deleteOther {w usernum} {
    # First remove the information about the other person from the others data structure
    info$w delete others.$usernum

    # Now remove the tags representing the other person
    set usertag user$usernum
    $w tag remove ${usertag}color 1.0 end
    overview_removeFontTags $w ${usertag}font
}


# remove the overview from main text
proc overview_removeFromMainText {w} {
    catch {
        set mainText [info$w mainText]
        notifier$mainText delete [info$w insertBinding]
        notifier$mainText delete [info$w selectBinding]
    }
}

# attach overview to main text
proc overview_attachToMainText {w mainText} {
    catch {
        info$w insertBinding [notifier$mainText bind insertText \
            "overview_insertText $w %U %F %L %N %T"]
        info$w selectBinding [notifier$mainText bind selectText \
            "overview_selectText $w %U %F %L %I"]
        info$w mainText $mainText
    }
}

# copy text to the overview
proc overview_copyTo { wTo wFrom } {
    $wTo insert 1.0 [$wFrom get 1.0 end-1c]
}


# insert text
proc overview_insertText {w usernum selStart selEnd newSelEnd theText} {
    if {$selStart != $selEnd} {
        $w delete $selStart $selEnd
    }
    # need to make sure the new next is tagged correctly when at start of line...
    $w insert $selStart $theText

    set color [gk_getUserAttrib $usernum color]
    overview_setFocus $w $usernum [overview_getLineNumber $selStart] false

#    overview_showSelection $w $usernum $color $selStart $newSelEnd
    overview_showSelection $w $usernum $color $newSelEnd
    update idletasks
}

# select text
proc overview_selectText {w usernum selStart selEnd insertionPoint} {
    if {$insertionPoint == ""} {set insertionPoint $selStart}

    set color [gk_getUserAttrib $usernum color]
    overview_setFocus $w $usernum [overview_getLineNumber $insertionPoint] false
    if {$selStart == ""} {
        overview_showSelection $w $usernum $color $insertionPoint 
    } else {
        overview_showSelection $w $usernum $color $selStart $selEnd
    }
    update idletasks
}

# set the focus for a given user
proc overview_setFocus {w usernum line_number force_update} {
    # If the user's fisheye is already centered on this line, do nothing
    if {$line_number == [info$w others.$usernum.line] && $force_update == "false"} {
	return
    }
    set usertag user$usernum
    if {$usernum != [users local.usernum]} {
        if {[lsearch [$w tag names] ${usertag}font*] == -1 } {
            overview_createFontTags $w ${usertag}font
        }
    }

    # Remove all the font tags for this user
    overview_removeFontTags $w ${usertag}font

    # set the fisheye lens focus
    overview_drawFisheye $w $usernum $line_number

    # and make sure we can see it!
    if {$usernum == [users local.usernum]} {
        $w see $line_number.0
    }

    # remember what line the focus is on
    info$w others.$usernum.line $line_number

    update idletasks
}

# actually draw the fisheye for a user at the given line
proc overview_drawFisheye {w usernum line_number} {
    set offset 0
    foreach mag [info$w keys mag_factor] {
	set range [info$w mag_factor.$mag]
	# Zero range can be ignored
	if {$range == 0} {continue}
	set size $mag
        set thistag user${usernum}font${size}

        # Do the center lines
        if {$offset == 0} {
	    set from [expr $line_number - $range + 1]
	    set to [expr $line_number + $range - 1]
            overview_setLineTags $w $thistag $from $to $size 
	    incr offset $range
        } else {
            #Do the lines before
	    set from [expr $line_number - $offset - $range + 1]
	    set to [expr $line_number - $offset] 
            overview_setLineTags $w $thistag $from $to $size 

	    #Do the lines after 
	    set from [expr $line_number + $offset ]
	    set to [expr $line_number + $offset + $range - 1 ]
            overview_setLineTags $w $thistag $from $to $size 

	    incr offset $range
        }
    }
}

# show the selection at the given line
proc overview_showSelection {w usernum color selStart {selEnd ""} } {
    set colortag user${usernum}color
    if {[lsearch [$w tag names] $colortag] == -1 } {
        $w tag configure $colortag -background $color
        info$w others.$usernum.color $color
    }
    $w tag remove $colortag 1.0 end
    if {$selEnd == ""} {
        $w tag add $colortag $selStart 
    } else {
        $w tag add $colortag $selStart $selEnd
    }
}


# 
# tag utilities
#

#Create font descriptions for Tags
proc overview_createFonts {w} {
    foreach mag [info$w keys mag_factor] {
        info$w fontDescription.$mag [overview_fontName $mag]
    }
    return [llength [info$w mag_factor]]
}

#Create font tags
proc overview_createFontTags {w thistag} {
    foreach mag [info$w keys mag_factor] {
       $w tag configure $thistag$mag \
	   -font [info$w fontDescription.$mag]
    }
}

# Remove all the local font tags from the text
proc overview_removeFontTags {w tagToRemove} {
    foreach thistag [$w tag names] {
        if { [string first $tagToRemove $thistag] != -1} {
           $w tag remove $thistag 1.0 end
        }
    }
}

# Attach the tag to the line numbers starting at "from" and going to "to",
proc overview_setLineTags {w tag from to size} {
   # Make sure that the line numbers are contained in the text widget
   set last_line [overview_getLineNumber [$w index end]]
   if {$from <= 0 && $to <= 0} {return}

   if {$from <= 0} {set from 1}
   if {$to > $last_line} {set to $last_line}

   # Add the tag to the range
   for {set line $from} {$line <= $to} {incr line} {
       set current_size [overview_lineFontSize $w $line]
       if  {$current_size < $size} {
	   set oldTag [overview_lineFontTag $w $line $current_size]
           $w tag add $tag "$line.0 linestart" "$line.0 lineend + 1 chars"
           if {$oldTag  != ""} { 
                $w tag raise $tag $oldTag
	   #    $w tag remove $oldTag "$line.0" "$line.0 lineend + 1 chars"
	   }
       }
   }
}

# Find the font size of the current line
proc overview_lineFontSize {w line} {
    set fontsize [info$w default_fontsize]
    foreach tag [$w tag names $line.0] {
	set idx [string first font $tag]
        if { $idx != -1} {
            set thisfontsize [string range $tag [expr $idx+4] end]
            if {$thisfontsize == "others"} {set thisfontsize [info$w others_size]}
            if {$thisfontsize > $fontsize} {set fontsize $thisfontsize}
        }
    }
    return $fontsize
}

# Find the specified font tag of the current line
proc overview_lineFontTag {w line size} {
    set tagname "" 
    foreach tag [$w tag names $line.0] {
	set idx [string first font$size $tag]
        if { $idx != -1} {
		set tagname $tag
        }
    }
    return $tagname
}


# Given an index, return the line number
proc overview_getLineNumber {idx} {
    return [string range $idx 0 [expr [string first "." $idx] - 1 ]]
}

# Given a font size, construct a font name from it
proc overview_fontName {size} {
    return [concat -*-lucida-medium-r-*-*-$size-*-*-*-*-*-*-*]
}





