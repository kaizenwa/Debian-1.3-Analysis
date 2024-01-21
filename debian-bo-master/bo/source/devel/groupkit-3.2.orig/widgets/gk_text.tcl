proc gktext {w args} {
  eval text $w -exportselection false $args
  bindtags $w [list . all $w gkTextTag]
  bind gkTextTag <Enter> "gkTextBind $w Enter"
  bind gkTextTag <FocusIn> "gkTextBind $w FocusIn"
  return $w
}


# text.tcl --
#
# This file defines the default bindings for Tk text widgets.
#
# @(#) text.tcl 1.26 95/03/07 22:11:48
#
# Copyright (c) 1992-1994 The Regents of the University of California.
# Copyright (c) 1994-1995 Sun Microsystems, Inc.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#

#-------------------------------------------------------------------------
# Elements of gkPriv that are used in this file:
#
# afterId -		If non-null, it means that auto-scanning is underway
#			and it gives the "after" id for the next auto-scan
#			command to be executed.
# char -		Character position on the line;  kept in order
#			to allow moving up or down past short lines while
#			still remembering the desired position.
# mouseMoved -		Non-zero means the mouse has moved a significant
#			amount since the button went down (so, for example,
#			start dragging out a selection).
# prevPos -		Used when moving up or down lines via the keyboard.
#			Keeps track of the previous insert position, so
#			we can distinguish a series of ups and downs, all
#			in a row, from a new up or down.
# selectMode -		The style of selection currently underway:
#			char, word, or line.
# x, y -		Last known mouse coordinates for scanning
#			and auto-scanning.
#-------------------------------------------------------------------------


# gkTextBind --
# This procedure below invoked the first time the mouse enters a text
# widget or a text widget receives the input focus.  It creates all of
# the class bindings for texts.
#
# Arguments:
# event -	Indicates which event caused the procedure to be invoked
#		(Enter or FocusIn).  It is used so that we can carry out
#		the functions of that event in addition to setting up
#		bindings.

proc gkTextBind {w event} {
    global gkPriv tk_strictMotif

    bind gkTextTag <Enter> {break}
    bind gkTextTag <FocusIn> {break}

    # Standard Motif bindings:

    bind gkTextTag <1> {
	gkTextButton1 %W %x %y
	%W tag remove sel 0.0 end
    }
    bind gkTextTag <B1-Motion> {
	set gkPriv(x) %x
	set gkPriv(y) %y
	gkTextSelectTo %W %x %y
    }
    bind gkTextTag <Double-1> {
	set gkPriv(selectMode) word
	gkTextSelectTo %W %x %y
	catch {%W mark set insert sel.first}
    }
    bind gkTextTag <Triple-1> {
	set gkPriv(selectMode) line
	gkTextSelectTo %W %x %y
	catch {%W mark set insert sel.first}
    }
    bind gkTextTag <Shift-1> {
	gkTextResetAnchor %W @%x,%y
	set gkPriv(selectMode) char
	gkTextSelectTo %W %x %y
    }
    bind gkTextTag <Double-Shift-1>	{
	set gkPriv(selectMode) word
	gkTextSelectTo %W %x %y
    }
    bind gkTextTag <Triple-Shift-1>	{
	set gkPriv(selectMode) line
	gkTextSelectTo %W %x %y
    }
    bind gkTextTag <B1-Leave> {
	set gkPriv(x) %x
	set gkPriv(y) %y
	gkTextAutoScan %W
    }
    bind gkTextTag <B1-Enter> {
	tkCancelRepeat
    }
    bind gkTextTag <ButtonRelease-1> {
	tkCancelRepeat
    }
    bind gkTextTag <Control-1> {
	%W mark set insert @%x,%y
    }
    bind gkTextTag <Left> {
	gkTextSetCursor %W [%W index {insert - 1c}]
    }
    bind gkTextTag <Right> {
	gkTextSetCursor %W [%W index {insert + 1c}]
    }
    bind gkTextTag <Up> {
	gkTextSetCursor %W [gkTextUpDownLine %W -1]
    }
    bind gkTextTag <Down> {
	gkTextSetCursor %W [gkTextUpDownLine %W 1]
    }
    bind gkTextTag <Shift-Left> {
	gkTextKeySelect %W [%W index {insert - 1c}]
    }
    bind gkTextTag <Shift-Right> {
	gkTextKeySelect %W [%W index {insert + 1c}]
    }
    bind gkTextTag <Shift-Up> {
	gkTextKeySelect %W [gkTextUpDownLine %W -1]
    }
    bind gkTextTag <Shift-Down> {
	gkTextKeySelect %W [gkTextUpDownLine %W 1]
    }
    bind gkTextTag <Control-Left> {
	gkTextSetCursor %W [%W index {insert - 1c wordstart}]
    }
    bind gkTextTag <Control-Right> {
	gkTextSetCursor %W [%W index {insert wordend}]
    }
    bind gkTextTag <Control-Up> {
	gkTextSetCursor %W [gkTextPrevPara %W insert]
    }
    bind gkTextTag <Control-Down> {
	gkTextSetCursor %W [gkTextNextPara %W insert]
    }
    bind gkTextTag <Shift-Control-Left> {
	gkTextKeySelect %W [%W index {insert - 1c wordstart}]
    }
    bind gkTextTag <Shift-Control-Right> {
	gkTextKeySelect %W [%W index {insert wordend}]
    }
    bind gkTextTag <Shift-Control-Up> {
	gkTextKeySelect %W [gkTextPrevPara %W insert]
    }
    bind gkTextTag <Shift-Control-Down> {
	gkTextKeySelect %W [gkTextNextPara %W insert]
    }
    bind gkTextTag <Prior> {
	gkTextSetCursor %W [gkTextScrollPages %W -1]
    }
    bind gkTextTag <Shift-Prior> {
	gkTextKeySelect %W [gkTextScrollPages %W -1]
    }
    bind gkTextTag <Next> {
	gkTextSetCursor %W [gkTextScrollPages %W 1]
    }
    bind gkTextTag <Shift-Next> {
	gkTextKeySelect %W [gkTextScrollPages %W 1]
    }
    bind gkTextTag <Control-Prior> {
	%W xview scroll -1 page
    }
    bind gkTextTag <Control-Next> {
	%W xview scroll 1 page
    }

    bind gkTextTag <Home> {
	gkTextSetCursor %W [%W index {insert linestart}]
    }
    bind gkTextTag <Shift-Home> {
	gkTextKeySelect %W [%W index {insert linestart}]
    }
    bind gkTextTag <End> {
	gkTextSetCursor %W {insert lineend}
    }
    bind gkTextTag <Shift-End> {
	gkTextKeySelect %W {insert lineend}
    }
    bind gkTextTag <Control-Home> {
	gkTextSetCursor %W 1.0
    }
    bind gkTextTag <Control-Shift-Home> {
	gkTextKeySelect %W 1.0
    }
    bind gkTextTag <Control-End> {
	gkTextSetCursor %W {end - 1 char}
    }
    bind gkTextTag <Control-Shift-End> {
	gkTextKeySelect %W {end - 1 char}
    }

    bind gkTextTag <Tab> {
	gkTextInsert %W \t
	focus %W
	break
    }
    bind gkTextTag <Shift-Tab> {
	# Needed only to keep <Tab> binding from triggering;  doesn't
	# have to actually do anything.
    }
    bind gkTextTag <Control-Tab> {
	focus [tk_focusNext %W]
    }
    bind gkTextTag <Control-Shift-Tab> {
	focus [tk_focusPrev %W]
    }
    bind gkTextTag <Control-i> {
	gkTextInsert %W \t
    }
    bind gkTextTag <Return> {
	gkTextInsert %W \n
    }
    bind gkTextTag <Delete> {
	if {[%W tag nextrange sel 1.0 end] != ""} {
            gk_serialize gkTextDoDelete %W [users local.usernum] \
                sel_[users local.usernum].first sel_[users local.usernum].last
	} else {
            gk_serialize gkTextDoDelete %W [users local.usernum] \
                insert_[users local.usernum]
	}
    }
    bind gkTextTag <BackSpace> {
	if {[%W tag nextrange sel 1.0 end] != ""} {
            gk_serialize gkTextDoDelete %W [users local.usernum] \
               sel_[users local.usernum].first sel_[users local.usernum].last
	} elseif [%W compare insert != 1.0] {
            gk_serialize gkTextDoDelete %W [users local.usernum] \
                insert_[users local.usernum]-1c
	}
    }

    bind gkTextTag <Control-space> {
	%W mark set anchor insert
    }
    bind gkTextTag <Select> {
	%W mark set anchor insert
    }
    bind gkTextTag <Control-Shift-space> {
	set gkPriv(selectMode) char
	gkTextKeyExtend %W insert
    }
    bind gkTextTag <Shift-Select> {
	set gkPriv(selectMode) char
	gkTextKeyExtend %W insert
    }
    bind gkTextTag <Control-slash> {
	%W tag add sel 1.0 end
    }
    bind gkTextTag <Control-backslash> {
	%W tag remove sel 1.0 end
    }
    gkTextClipboardKeysyms $w F16 F20 F18
    bind gkTextTag <Insert> {
	catch {gkTextInsert %W [selection get -displayof %W]}
    }
    bind gkTextTag <KeyPress> {
	gkTextInsert %W %A
    }

    # Ignore all Alt, Meta, and Control keypresses unless explicitly bound.
    # Otherwise, if a widget binding for one of these is defined, the
    # <KeyPress> class binding will also fire and insert the character,
    # which is wrong.  Ditto for <Escape>.

    bind gkTextTag <Alt-KeyPress> {# nothing }
    bind gkTextTag <Meta-KeyPress> {# nothing}
    bind gkTextTag <Control-KeyPress> {# nothing}
    bind gkTextTag <Escape> {# nothing}

    # Additional emacs-like bindings:

    if !$tk_strictMotif {
	bind gkTextTag <Control-a> {
	    gkTextSetCursor %W [%W index {insert linestart}]
	}
	bind gkTextTag <Control-b> {
	    gkTextSetCursor %W [%W index insert-1c]
	}
	bind gkTextTag <Control-d> "
            gk_serialize gkTextDoDelete $w [users local.usernum] insert_[users local.usernum]
	"
	bind gkTextTag <Control-e> {
	    gkTextSetCursor %W [%W index {insert lineend}]
	}
	bind gkTextTag <Control-f> {
	    gkTextSetCursor %W [%W index insert+1c]
	}
	bind gkTextTag <Control-k> { 
           gkTextDeleteLine %W 
	}
	bind gkTextTag <Control-n> {
	    gkTextSetCursor %W [gkTextUpDownLine %W 1]
	}
	bind gkTextTag <Control-o> {
	    %W insert insert \n
	    %W mark set insert insert-1c
	}
	bind gkTextTag <Control-p> {
	    gkTextSetCursor %W [gkTextUpDownLine %W -1]
	}
	bind gkTextTag <Control-t> {
	    gkTextTranspose %W
	}
	bind gkTextTag <Meta-b> {
	    gkTextSetCursor %W {insert - 1c wordstart}
	}
	bind gkTextTag <Meta-d> {
	    %W delete insert {insert wordend}
	}
	bind gkTextTag <Meta-f> {
	    gkTextSetCursor %W {insert wordend}
	}
	bind gkTextTag <Meta-less> {
	    gkTextSetCursor %W 1.0
	}
	bind gkTextTag <Meta-greater> {
	    gkTextSetCursor %W end-1c
	}
	bind gkTextTag <Meta-BackSpace> {
	    %W delete {insert -1c wordstart} insert
	}
	gkTextClipboardKeysyms $w Meta-w Control-w Control-y

	# A few additional bindings of my own.
    
	bind gkTextTag <Control-h> {
	    if [%W compare insert != 1.0] {
                gkTextDoDelete %W [users local.usernum] \
                   insert_[users local.usernum]-1c
	    }
	}
	bind gkTextTag <Control-v> {
	    catch {
		%W insert insert [selection get -displayof %W]
		%W see insert
	    }
	}
	bind gkTextTag <2> {
	    %W scan mark %x %y
	    set gkPriv(x) %x
	    set gkPriv(y) %y
	    set gkPriv(mouseMoved) 0
	}
	bind gkTextTag <B2-Motion> {
	    set names [array names gkPriv]
	    if { [lsearch $names x] < 0  || [lsearch $names y] < 0 } {return}

	    if {(%x != $gkPriv(x)) || (%y != $gkPriv(y))} {
		set gkPriv(mouseMoved) 1
	    }
	    if $gkPriv(mouseMoved) {
		%W scan dragto %x %y
	    }
	}

	bind gkTextTag <ButtonRelease-2> {
	    if { [lsearch [array names gkPriv] mouseMoved] < 0 } { return }

	    if !$gkPriv(mouseMoved) {
		catch {
		    %W insert insert [selection get -displayof %W]
		    gkTextSeeInsert %W
		}
	    }
	}
    }

#    rename gkTextBind {}
    set gkPriv(prevPos) {}

}

# gkTextClipboardKeysyms --
# This procedure is invoked to identify the keys that correspond to
# the "copy", "cut", and "paste" functions for the clipboard.
#
# Arguments:
# copy -	Name of the key (keysym name plus modifiers, if any,
#		such as "Meta-y") used for the copy operation.
# cut -		Name of the key used for the cut operation.
# paste -	Name of the key used for the paste operation.

proc gkTextClipboardKeysyms {w copy cut paste} {
    bind gkTextTag <$copy> {
	if {[selection own -displayof %W] == "%W" \
		&& [%W tag nextrange sel 0.0 end] != "" } {
	    clipboard clear -displayof %W
	    clipboard append -displayof %W [selection get -displayof %W]
	}
    }
    bind gkTextTag <$cut> {
	if {[selection own -displayof %W] == "%W" \
		&& [%W tag nextrange sel 0.0 end] != ""}  {
	    clipboard clear -displayof %W
	    clipboard append -displayof %W [selection get -displayof %W]
	    %W delete sel.first sel.last
	}
    }
    bind gkTextTag <$paste> {
	catch {
	    %W insert insert [selection get -displayof %W \
		    -selection CLIPBOARD]
	}
    }
}

# gkTextButton1 --
# This procedure is invoked to handle button-1 presses in text
# widgets.  It moves the insertion cursor, sets the selection anchor,
# and claims the input focus.
#
# Arguments:
# w -		The text window in which the button was pressed.
# x -		The x-coordinate of the button press.
# y -		The x-coordinate of the button press.

proc gkTextButton1 {w x y} {
    global gkPriv

    set gkPriv(selectMode) char
    set gkPriv(mouseMoved) 0
    set gkPriv(pressX) $x
    gk_serialize gkTextSetMark $w [users local.usernum] [$w index @$x,$y]
    if {[$w cget -state] == "normal"} {focus $w}
}

proc gkTextSetMark {w usernum posn} {
  $w mark set insert_$usernum $posn
  if {$usernum==[users local.usernum]} {
    $w mark set insert $posn
    $w mark set anchor insert
  } else {
    gkTextUpdateMark $w $usernum
  }
}

# gkTextSelectTo --
# This procedure is invoked to extend the selection, typically when
# dragging it with the mouse.  Depending on the selection mode (character,
# word, line) it selects in different-sized units.  This procedure
# ignores mouse motions initially until the mouse has moved from
# one character to another or until there have been multiple clicks.
#
# Arguments:
# w -		The text window in which the button was pressed.
# x -		Mouse x position.
# y - 		Mouse y position.

proc gkTextSelectTo {w x y} {
    global gkPriv

    set cur [$w index @$x,$y]
    if [catch {$w index anchor}] {
	$w mark set anchor $cur
    }
    set anchor [$w index anchor]
    if {[$w compare $cur != $anchor] || (abs($gkPriv(pressX) - $x) >= 3)} {
	set gkPriv(mouseMoved) 1
    }
    switch $gkPriv(selectMode) {
	char {
	    if [$w compare $cur < anchor] {
		set first $cur
		set last anchor
	    } else {
		set first anchor
		set last [$w index "$cur + 1c"]
	    }
	}
	word {
	    if [$w compare $cur < anchor] {
		set first [$w index "$cur wordstart"]
		set last [$w index "anchor - 1c wordend"]
	    } else {
		set first [$w index "anchor wordstart"]
		set last [$w index "$cur wordend"]
	    }
	}
	line {
	    if [$w compare $cur < anchor] {
		set first [$w index "$cur linestart"]
		set last [$w index "anchor - 1c lineend + 1c"]
	    } else {
		set first [$w index "anchor linestart"]
		set last [$w index "$cur lineend + 1c"]
	    }
	}
    }
    if {$gkPriv(mouseMoved) || ($gkPriv(selectMode) != "char")} {
	gk_serialize gkTextDoSelect $w [users local.usernum] \
             [$w index $first] [$w index $last]
    }
}

proc gkTextDoSelect {w usernum first last} {
  $w tag remove sel_$usernum 0.0 $first
  $w tag add sel_$usernum $first $last
  $w tag remove sel_$usernum $last end
  if {$usernum==[users local.usernum]} {
      $w tag remove sel 0.0 $first
      $w tag add sel $first $last
      $w tag remove sel $last end
      update idletasks
  } else {
    $w tag configure sel_$usernum -background [gk_getUserAttrib $usernum color]
  }
}

# gkTextKeyExtend --
# This procedure handles extending the selection from the keyboard,
# where the point to extend to is really the boundary between two
# characters rather than a particular character.
#
# Arguments:
# w -		The text window.
# index -	The point to which the selection is to be extended.

proc gkTextKeyExtend {w index} {
    global gkPriv

    set cur [$w index $index]
    if [catch {$w index anchor}] {
	$w mark set anchor $cur
    }
    set anchor [$w index anchor]
    if [$w compare $cur < anchor] {
	set first $cur
	set last anchor
    } else {
	set first anchor
	set last $cur
    }
    $w tag remove sel 0.0 $first
    $w tag add sel $first $last
    $w tag remove sel $last end
}

# gkTextAutoScan --
# This procedure is invoked when the mouse leaves a text window
# with button 1 down.  It scrolls the window up, down, left, or right,
# depending on where the mouse is (this information was saved in
# gkPriv(x) and gkPriv(y)), and reschedules itself as an "after"
# command so that the window continues to scroll until the mouse
# moves back into the window or the mouse button is released.
#
# Arguments:
# w -		The text window.

proc gkTextAutoScan {w} {
    global gkPriv
    if {$gkPriv(y) >= [winfo height $w]} {
	$w yview scroll 2 units
    } elseif {$gkPriv(y) < 0} {
	$w yview scroll -2 units
    } elseif {$gkPriv(x) >= [winfo width $w]} {
	$w xview scroll 2 units
    } elseif {$gkPriv(x) < 0} {
	$w xview scroll -2 units
    } else {
	return
    }
    gkTextSelectTo $w $gkPriv(x) $gkPriv(y)
    set gkPriv(afterId) [after 50 gkTextAutoScan $w]
}

# gkTextSetCursor
# Move the insertion cursor to a given position in a text.  Also
# clears the selection, if there is one in the text, and makes sure
# that the insertion cursor is visible.  Also, don't let the insertion
# cursor appear on the dummy last line of the text.
#
# Arguments:
# w -		The text window.
# pos -		The desired new position for the cursor in the window.

proc gkTextSetCursor {w pos} {
    global gkPriv

    if [$w compare $pos == end] {
	set pos {end - 1 chars}
    }
    gk_serialize gkTextDoSetCursor $w [users local.usernum] $pos
}

proc gkTextDoSetCursor {w usernum pos} {
    gkTextSetMark $w $usernum $pos
    if {$usernum==[users local.usernum]} {
      $w see insert
    }
#    $w mark set insert_$usernum $pos
#    $w tag remove sel_$usernum 1.0 end
}

# gkTextKeySelect
# This procedure is invoked when stroking out selections using the
# keyboard.  It moves the cursor to a new position, then extends
# the selection to that position.
#
# Arguments:
# w -		The text window.
# new -		A new position for the insertion cursor (the cursor hasn't
#		actually been moved to this position yet).

proc gkTextKeySelect {w new} {
    global gkPriv

    if {[$w tag nextrange sel 1.0 end] == ""} {
	if [$w compare $new < insert] {
	    $w tag add sel $new insert
	} else {
	    $w tag add sel insert $new
	}
	$w mark set anchor insert
    } else {
	if [$w compare $new < anchor] {
	    set first $new
	    set last anchor
	} else {
	    set first anchor
	    set last $new
	}
	$w tag remove sel 1.0 $first
	$w tag add sel $first $last
	$w tag remove sel $last end
    }
    $w mark set insert $new
    $w see insert
    update idletasks
}

# gkTextResetAnchor --
# Set the selection anchor to whichever end is farthest from the
# index argument.  One special trick: if the selection has two or
# fewer characters, just leave the anchor where it is.  In this
# case it doesn't matter which point gets chosen for the anchor,
# and for the things like Shift-Left and Shift-Right this produces
# better behavior when the cursor moves back and forth across the
# anchor.
#
# Arguments:
# w -		The text widget.
# index -	Position at which mouse button was pressed, which determines
#		which end of selection should be used as anchor point.

proc gkTextResetAnchor {w index} {
    global gkPriv

    if {[$w tag ranges sel] == ""} {
	$w mark set anchor $index
	return
    }
    set a [$w index $index]
    set b [$w index sel.first]
    set c [$w index sel.last]
    if [$w compare $a < $b] {
	$w mark set anchor sel.last
	return
    }
    if [$w compare $a > $c] {
	$w mark set anchor sel.first
	return
    }
    scan $a "%d.%d" lineA chA
    scan $b "%d.%d" lineB chB
    scan $c "%d.%d" lineC chC
    if {$lineB < $lineC+2} {
	set total [string length [$w get $b $c]]
	if {$total <= 2} {
	    return
	}
	if {[string length [$w get $b $a]] < ($total/2)} {
	    $w mark set anchor sel.last
	} else {
	    $w mark set anchor sel.first
	}
	return
    }
    if {($lineA-$lineB) < ($lineC-$lineA)} {
	$w mark set anchor sel.last
    } else {
	$w mark set anchor sel.first
    }
}

# gkTextInsert --
# Insert a string into a text at the point of the insertion cursor.
# If there is a selection in the text, and it covers the point of the
# insertion cursor, then delete the selection before inserting.
#
# Arguments:
# w -		The text window in which to insert the string
# s -		The string to insert (usually just a single character)

proc gkTextInsert {w s} {
    if {$s == ""} {
	return
    }
    gk_serialize gkTextDoInsert $w [users local.usernum] $s
}

proc gkTextDoInsert {w usernum s} {
    catch {
	if {[$w compare sel_$usernum.first <= insert_$usernum]
		&& [$w compare sel_$usernum.last >= insert_$usernum]} {
	    $w delete sel_$usernum.first sel_$usernum.last
	}
    }
    $w insert insert_$usernum $s
    if {$usernum==[users local.usernum]} {
      $w see insert
    }
}

proc gkTextDeleteLine {w} {
  set usernum [users local.usernum]
  if [$w compare insert == {insert lineend}] {
    gk_serialize gkTextDoDelete $w $usernum insert_$usernum
  } else {
    gk_serialize gkTextDoDelete $w $usernum insert_$usernum \
      [list insert_$usernum lineend]
  }
}

proc gkTextDoDelete {w usernum posn {posn2 ""}} {
  if {$posn2!=""} {
    $w delete $posn $posn2
  } else {
    $w delete $posn
  }
  gkTextUpdateMark $w $usernum
  if {$usernum==[users local.usernum]} {
    $w see insert
  }
}

proc gkTextUpdateMark {w usernum} {
  if {$usernum!=[users local.usernum]} {
    $w tag delete insert_$usernum
    $w tag add insert_$usernum insert_$usernum
    $w tag configure insert_$usernum -background [gk_getUserAttrib $usernum color]
  }
}

# gkTextUpDownLine --
# Returns the index of the character one line above or below the
# insertion cursor.  There are two tricky things here.  First,
# we want to maintain the original column across repeated operations,
# even though some lines that will get passed through don't have
# enough characters to cover the original column.  Second, don't
# try to scroll past the beginning or end of the text.
#
# Arguments:
# w -		The text window in which the cursor is to move.
# n -		The number of lines to move: -1 for up one line,
#		+1 for down one line.

proc gkTextUpDownLine {w n} {
    global gkPriv

    set i [$w index insert]
    scan $i "%d.%d" line char
    if {[string compare $gkPriv(prevPos) $i] != 0} {
	set gkPriv(char) $char
    }
    set new [$w index [expr $line + $n].$gkPriv(char)]
    if {[$w compare $new == end] || [$w compare $new == "insert linestart"]} {
	set new $i
    }
    set gkPriv(prevPos) $new
    return $new
}

# gkTextPrevPara --
# Returns the index of the beginning of the paragraph just before a given
# position in the text (the beginning of a paragraph is the first non-blank
# character after a blank line).
#
# Arguments:
# w -		The text window in which the cursor is to move.
# pos -		Position at which to start search.

proc gkTextPrevPara {w pos} {
    set pos [$w index "$pos linestart"]
    while 1 {
	if {(([$w get "$pos - 1 line"] == "\n") && ([$w get $pos] != "\n"))
		|| ($pos == "1.0")} {
	    if [regexp -indices {^[ 	]+(.)} [$w get $pos "$pos lineend"] \
		    dummy index] {
		set pos [$w index "$pos + [lindex $index 0] chars"]
	    }
	    if {[$w compare $pos != insert] || ($pos == "1.0")} {
		return $pos
	    }
	}
	set pos [$w index "$pos - 1 line"]
    }
}

# gkTextNextPara --
# Returns the index of the beginning of the paragraph just after a given
# position in the text (the beginning of a paragraph is the first non-blank
# character after a blank line).
#
# Arguments:
# w -		The text window in which the cursor is to move.
# start -	Position at which to start search.

proc gkTextNextPara {w start} {
    set pos [$w index "$start linestart + 1 line"]
    while {[$w get $pos] != "\n"} {
	if [$w compare $pos == end] {
	    return [$w index "end - 1c"]
	}
	set pos [$w index "$pos + 1 line"]
    }
    while {[$w get $pos] == "\n"} {
	set pos [$w index "$pos + 1 line"]
	if [$w compare $pos == end] {
	    return [$w index "end - 1c"]
	}
    }
    if [regexp -indices {^[ 	]+(.)} [$w get $pos "$pos lineend"] \
	    dummy index] {
	return [$w index "$pos + [lindex $index 0] chars"]
    }
    return $pos
}

# gkTextScrollPages --
# This is a utility procedure used in bindings for moving up and down
# pages and possibly extending the selection along the way.  It scrolls
# the view in the widget by the number of pages, and it returns the
# index of the character that is at the same position in the new view
# as the insertion cursor used to be in the old view.
#
# Arguments:
# w -		The text window in which the cursor is to move.
# count -	Number of pages forward to scroll;  may be negative
#		to scroll backwards.

proc gkTextScrollPages {w count} {
    set bbox [$w bbox insert]
    $w yview scroll $count pages
    if {$bbox == ""} {
	return [$w index @[expr [winfo height $w]/2],0]
    }
    set x [expr [lindex $bbox 0] + [lindex $bbox 2]/2]
    set y [expr [lindex $bbox 1] + [lindex $bbox 3]/2]
    return [$w index @$x,$y]
}

# gkTextTranspose --
# This procedure implements the "transpose" function for text widgets.
# It tranposes the characters on either side of the insertion cursor,
# unless the cursor is at the end of the line.  In this case it
# transposes the two characters to the left of the cursor.  In either
# case, the cursor ends up to the right of the transposed characters.
#
# Arguments:
# w -		Text window in which to transpose.

proc gkTextTranspose w {
    set pos insert
    if [$w compare $pos != "$pos lineend"] {
	set pos [$w index "$pos + 1 char"]
    }
    set new [$w get "$pos - 1 char"][$w get  "$pos - 2 char"]
    if [$w compare "$pos - 1 char" == 1.0] {
	return
    }
    $w delete "$pos - 2 char" $pos
    $w insert insert $new
    $w see insert
}
