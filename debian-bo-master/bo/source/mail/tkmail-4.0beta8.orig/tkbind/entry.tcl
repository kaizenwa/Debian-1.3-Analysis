# entry.tcl --
#
# This file defines the default bindings for Tk entry widgets and is a
# highly extended version of the original distributed with Tk4.0. It
# is intended as a drop in replacement for the original. If you do not
# have the permission or desire to do that, then you can run the following
# lines in your applicaiton to achieve the efficitively same result
#
# foreach key [bind Entry] { bind Entry $key {} }
# source entry.tcl
#
# This is beta software so beware. It will work only with Tk4.0.
#
# ORIGINAL COPYRIGHT INFORMATION
#
# Copyright (c) 1992-1994 The Regents of the University of California.
# Copyright (c) 1994-1995 Sun Microsystems, Inc.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# ADDITIONAL COPYRIGHT INFORMATION
#
#  Copyright 1995 by Paul Raines (raines@slac.stanford.edu)
#
#  Permission to use, copy, modify, and distribute this software and
#  its documentation for any purpose and without fee is hereby
#  granted, provided that the above copyright notice appear in all
#  copies.  The University of Pennsylvania, Stanford University, and
#  Stanford Linear Accelerator Center makes no representations
#  about the suitability of this software for any purpose.  It is
#  provided "as is" without express or implied warranty.

#-------------------------------------------------------------------------
# Elements of tkPriv that are used in this file:
#
# afterId -		If non-null, it means that auto-scanning is underway
#			and it gives the "after" id for the next auto-scan
#			command to be executed.
# mouseMoved -		Non-zero means the mouse has moved a significant
#			amount since the button went down (so, for example,
#			start dragging out a selection).
# pressX -		X-coordinate at which the mouse button was pressed.
# selectMode -		The style of selection currently underway:
#			char, word, or line.
# x, y -		Last known mouse coordinates for scanning
#			and auto-scanning.
#-------------------------------------------------------------------------
global tkPriv tkEntry tk_strictMotif

# tkEntryDefVar --
# Set the element 'elem' in the tkEntry array to 'def' only if 
# it does not already exist. Useful to allow developer to override
# defaults before this file is sourced

proc tkEntryDefVar {elem def} {
    global tkEntry
    if {![info exists tkEntry($elem)]} {
	set tkEntry($elem) $def
    }
}

tkEntryDefVar cancelhooks {}

if [string length [glob -nocomplain ~/.tkEntryrc]] {
  if {[file readable [glob -nocomplain ~/.tkEntryrc]]} {
    source [glob -nocomplain ~/.tkEntryrc]
  }
}

# tkEntrySetup --
# Set up private variables for the specified Entry widget 'w'.

proc tkEntrySetup w {
  global tkEntry tkBind

  if [info exists tkEntry($w,killLast)] return

  set tkEntry($w,prevCmd) Setup
  set tkEntry($w,markActive) 0
  set tkEntry($w,markRing) 0
  set tkEntry($w,ovwrt) 0
  set tkEntry($w,killLast) 0.0

  set tkBind($w,arg) {}
  tkBindDefVar "$w,mesgvar" tkBind($w,mesg)
  tkBindDefVar "$w,mesg" {}

  set tkBind($w,bindtags) {}
  tkBindDefVar "$w,prebindtags" {}
  tkBindDefVar "$w,postbindtags" {}

  if $tkBind(bindUndo) { tkEntryUndoSetup $w }

  if {[info proc tkEntryWidgetHook]!=""} {
    tkEntryWidgetHook $w
  }
}

# tkEntryDestroy --
# Free memory of private variables for the specified Entry widget 'w'.

proc tkEntryDestroy w {
  global tkEntry tkBind

  if {![info exists tkEntry($w,prevCmd)]} return

  unset tkEntry($w,prevCmd)
  unset tkEntry($w,markActive)
  unset tkEntry($w,markRing)
  unset tkEntry($w,ovwrt)
  unset tkEntry($w,killLast)

  unset tkBind($w,bindtags)
  unset tkBind($w,prebindtags)
  unset tkBind($w,postbindtags)

  unset tkBind($w,arg)
  unset tkBind($w,mesgvar)
  unset tkBind($w,mesg)

  tkEntryUndoFree $w
}

# tkEntryKeyCancel --
# Cancels everything

proc tkEntryKeyCancel w {
  global tkEntry tkBind

  $w selection clear
  if {[lsearch [bindtags $w] BindState] > -1 || \
	    [string length $tkBind($w,arg)] || \
	$tkEntry($w,markActive)} {
    tkBindCancelStateKey $w
    set tkEntry($w,markActive) 0
    set tkBind($w,arg) {}
  } else {
    # nothing to clear, so call exceptions
    foreach proc $tkEntry(cancelhooks) {
      $proc $w
    }
  }
  set tkEntry($w,prevCmd) KeyCancel
}

# tkEntryButtonInsert --
# Do the mouse button insertion in XTerm fashion, looking to the
# clipboard if no PRIMARY selection is found.
#
# Arguments:
# w -		The Entry window in which to insert.
#

proc tkEntryButtonInsert {w x} {
  global tkEntry tkBind

  if $tkBind(insertAtClick) {
    $w icursor @$x
    $w selection from insert
  }
  set ndx [$w index insert]
  set cmd "selection get -displayof $w"
  set res [catch "$w insert insert \[$cmd\]"]
  if {$res} {
    append cmd " -selection CLIPBOARD"
    set res [catch "$w insert insert \[$cmd\]"]
  }
  if {!$res} {
    tkEntryUndoPush $w {} $ndx insert
    tkEntrySeeInsert $w
    set tkEntry($w,markActive) 0
    set tkBind($w,arg) {}
    set tkEntry($w,prevCmd) ButtonInsert
  }
}


# tkEntryButton1 --
# This procedure is invoked to handle button-1 presses in entry
# widgets.  It moves the insertion cursor, sets the selection anchor,
# and claims the input focus.
#
# Arguments:
# w -		The entry window in which the button was pressed.
# x -		The x-coordinate of the button press.

proc tkEntryButton1 {w x} {
  global tkPriv tkEntry

  tkEntryButton3 $w $x
  $w icursor @$x
  set tkEntry($w,prevCmd) Button1
}

# tkEntryButton3 --
# This procedure is invoked to handle button-3 presses in entry
# widgets.  Sets the selection anchor, and claims the input focus.
#
# Arguments:
# w -		The entry window in which the button was pressed.
# x -		The x-coordinate of the button press.

proc tkEntryButton3 {w x} {
  global tkPriv tkEntry tkBind

  set tkPriv(selectMode) char
  set tkPriv(mouseMoved) 0
  set tkPriv(pressX) $x
  $w selection from @$x
  if {[lindex [$w configure -state] 4] == "normal"} {focus $w}
  set tkEntry($w,markActive) 0
  set tkBind($w,arg) {}
  set tkEntry($w,prevCmd) Button3
  tkBindSetMesg $w {}
}

# tkEntryMouseSelect --
# This procedure is invoked when dragging out a selection with
# the mouse.  Depending on the selection mode (character, word,
# line) it selects in different-sized units.  This procedure
# ignores mouse motions initially until the mouse has moved from
# one character to another or until there have been multiple clicks.
#
# Arguments:
# w -		The entry window in which the button was pressed.
# x -		The x-coordinate of the mouse.

proc tkEntryMouseSelect {w x} {
  global tkPriv tkEntry tkBind

  set cur [$w index @$x]
  set anchor [$w index anchor]
  if {($cur != $anchor) || (abs($tkPriv(pressX) - $x) >= 3)} {
    set tkPriv(mouseMoved) 1
  }
  switch $tkPriv(selectMode) {
    char {
      if $tkPriv(mouseMoved) {
	if {$cur < [$w index anchor]} {
	  $w selection to $cur
	} else {
	  $w selection to [expr $cur+1]
	}
      }
    }
    word {
      if {$cur < [$w index anchor]} {
	set first [tkEntryWordIndex $w $cur -1]
	set last [tkEntryWordIndex $w [expr $anchor-1] 1]
      } else {
	set first [tkEntryWordIndex $w $anchor -1]
	set last [tkEntryWordIndex $w $cur 1]
      }
      $w selection range $first $last
    }
    line {
      $w selection range 0 end
    }
  }
  update idletasks

  set tkBind($w,arg) {}
  set tkEntry($w,prevCmd) MouseSelect
  set tkEntry($w,mesg) {}
}

proc tkEntryWordIndex {w i n} {
  global tkBind

  set ndx [$w index $i]
  set str [$w get]
  set strlen [string length $str]
  if {$n > 0} {
    for {} {$n > 0} {incr n -1} {
      while {[regexp $tkBind(notWord) [string index $str $ndx]] &&
	     $ndx < $strlen} {
	incr ndx
      } 
      while {![regexp $tkBind(notWord) [string index $str $ndx]] &&
	     $ndx < $strlen} {
	incr ndx
      } 
    }
  } else {
    for {set i 0} {$i > $n } {incr i -1} {
      incr ndx -1
      while {[regexp $tkBind(notWord) [string index $str $ndx]] &&
	     $ndx > 0} {
	incr ndx -1
      } 
      while {![regexp $tkBind(notWord) [string index $str $ndx]] &&
	     $ndx > 0} {
	incr ndx -1
      } 
    }
    if {[regexp $tkBind(notWord) [string index $str $ndx]]} {incr ndx}
  }

  return $ndx
}


# tkEntryAutoScan --
# This procedure is invoked when the mouse leaves an entry window
# with button 1 down.  It scrolls the window left or right,
# depending on where the mouse is, and reschedules itself as an
# "after" command so that the window continues to scroll until the
# mouse moves back into the window or the mouse button is released.
#
# Arguments:
# w -		The entry window.

proc tkEntryAutoScan {w} {
    global tkPriv
    set x $tkPriv(x)
    if {$x >= [winfo width $w]} {
	$w xview scroll 2 units
	tkEntryMouseSelect $w $x
    } elseif {$x < 0} {
	$w xview scroll -2 units
	tkEntryMouseSelect $w $x
    }
    set tkPriv(afterId) [after 50 tkEntryAutoScan $w]
}

# tkEntryKeySelect --
# This procedure is invoked when stroking out selections using the
# keyboard.  It moves the cursor to a new position, then extends
# the selection to that position.
#
# Arguments:
# w -		The entry window.
# new -		A new position for the insertion cursor (the cursor hasn't
#		actually been moved to this position yet).

proc tkEntryKeySelect {w new} {
  global tkEntry tkBind

  if ![$w selection present] {
    $w selection from insert
    $w selection to $new
  } else {
    $w selection adjust $new
  }
  $w icursor $new
  tkEntrySeeInsert $w

  set tkBind($w,arg) {}
  set tkEntry($w,prevCmd) KeySelect
  tkBindSetMesg $w {}
}

# tkEntryInsertChar --
# Insert a string into a Entry at the point of the insertion cursor.
# If there is a selection in the Entry, and it covers the point of the
# insertion cursor, then delete the selection before inserting.
#
# Arguments:
# w -		The Entry window in which to insert the string
# s -		The string to insert (usually just a single character)

proc tkEntryInsertChar {w s} {
  global tkEntry tkBind

  if {$s == "" || ([$w cget -state] == "disabled")} return
  
  set txt {}
  set cutbuf {}
  for {set n [tkBindDefArg $w +]} {$n > 0} {incr n -1} { 
    append txt $s
  }

  set start [$w index insert]
  if {$tkBind(delSel) && [$w selection present] &&
      ([$w index sel.first] <= $start) && ([$w index sel.last] >= $start)} {
    set cutbuf [tkEntryGet $w sel.first sel.last]
    $w delete sel.first sel.last
  } elseif $tkEntry($w,ovwrt) {
    set ndx [expr $start+[string length $txt]]
    set cutbuf [tkEntryGet $w $start $ndx]
    $w delete insert $ndx
  }
  $w insert insert $txt
  tkEntrySeeInsert $w

  if {[info exists tkEntry($w,undoCnt)]} {
    if {$tkEntry($w,ovwrt) || $tkEntry($w,prevCmd) != "InsertChar" ||
	$tkEntry($w,undoLast) != $start || [string length $tkEntry($w,undoCut)]} {
      tkEntryUndoPush $w $cutbuf $start insert
    } else {
      set tkEntry($w,undoLast) [$w index insert]
    }
  }

  $w selection clear
  set tkEntry($w,markActive) 0
  set tkEntry($w,prevCmd) InsertChar
  tkBindSetMesg $w {}
}

# tkEntryInsert --
# Insert a string into an entry at the point of the insertion cursor.
# If there is a selection in the entry, and it covers the point of the
# insertion cursor, then delete the selection before inserting.
#
# Arguments:
# w -		The entry window in which to insert the string
# s -		The string to insert (usually just a single character)

proc tkEntryInsert {w ndx s} {
  global tkEntry tkBind

  if {$s == "" || ([$w cget -state] == "disabled")} return

  tkEntryUndoPush $w {} $ndx [expr $ndx+[string length $s]]
  $w insert $ndx $s

  set tkEntry($w,markActive) 0
  set tkBind($w,arg) {}
  set tkEntry($w,prevCmd) Insert

}

# tkEntryDelete --
# Deletes characters between indices 'ndx1' and 'ndx2'.
#
# Arguments:
# w -		The entry window in which to delete
# ndx1,ndx2 -	Entry indices surrounding text to delete
# dsel		If true, delete selection instead if it exists
# cut -		If true, add deletion to kill ring

proc tkEntryDelete {w ndx1 {ndx2 {}} {dsel 0} {cut 0}} {
  global tkEntry tkBind

  if {[$w cget -state] == "disabled"} return
  set ndx1 [$w index $ndx1]
  if {![string length $ndx2]} { 
    set ndx2 $ndx1 
  } else {
    set ndx2 [$w index $ndx2]
  }

  if {$dsel && [$w selection present]} {
    set start [$w index sel.first]
    set cutbuf [tkEntryGet $w sel.first sel.last]
    $w delete sel.first sel.last
  } else {
    if {$ndx1 < $ndx2} {
      set start $ndx1
      set cutbuf [tkEntryGet $w $ndx1 $ndx2]
      $w delete $ndx1 $ndx2
    } else {
      set start $ndx2
      set cutbuf [tkEntryGet $w $ndx2 $ndx1]
      $w delete $ndx2 $ndx1
    }
  }
  tkEntrySeeInsert $w

  tkEntryUndoPush $w $cutbuf $start $start
  if $cut { 
    if {$tkEntry($w,prevCmd) == "Delete" && $tkEntry($w,killLast) == $start} {
      set where 1
    } else { set where 0 }
    tkEntryPushTagBuffer $cutbuf $where
    set tkEntry($w,killLast) $start
    clipboard clear -displayof $w
    clipboard append -displayof $w $cutbuf
  }
  set tkEntry($w,markActive) 0
  set tkBind($w,arg) {}
  set tkEntry($w,prevCmd) Delete
  tkBindSetMesg $w {}
}

# tkEntryEatSpace --
# Deletes whitespace characters around insert mark
#
# Arguments:
# w -		The entry window in which to eat space

proc tkEntryEatSpace { w } {
  global tkEntry 

  if {[$w cget -state] == "disabled"} return
  set start [$w index insert]
  set ndx $start
  while {[string match "\[ \t\]" [string index [$w get] $ndx]]} {
    incr ndx
  }
  while {[string match "\[ \t\]" [string index [$w get] [expr $start-1]]]} {
    incr start -1
  }
  if {$start < $ndx} {
    tkEntryDelete $w $start $ndx 0 0
  }
}


# tkEntryBackspace --
# Backspace over the character just before the insertion cursor.
# If backspacing would move the cursor off the left edge of the
# window, reposition the cursor at about the middle of the window.
#
# Arguments:
# w -		The entry window in which to backspace.

proc tkEntryBackspace w {
    if [$w selection present] {
	$w delete sel.first sel.last
    } else {
	set x [expr {[$w index insert] - 1}]
	if {$x >= 0} {$w delete $x}
	if {[$w index @0] >= [$w index insert]} {
	    set range [$w xview]
	    set left [lindex $range 0]
	    set right [lindex $range 1]
	    $w xview moveto [expr $left - ($right - $left)/2.0]
	}
    }
}

# tkEntrySeeInsert --
# Make sure that the insertion cursor is visible in the entry window.
# If not, adjust the view so that it is.
#
# Arguments:
# w -		The entry window.

proc tkEntrySeeInsert w {
    set c [$w index insert]
    set left [$w index @0]
    if {$left > $c} {
	$w xview $c
	return
    }
    set x [winfo width $w]
    while {([$w index @$x] <= $c) && ($left < $c)} {
	incr left
	$w xview $left
    }
}

# tkEntrySetCursor -
# Move the insertion cursor to a given position in an entry.  Also
# clears the selection, if there is one in the entry, and makes sure
# that the insertion cursor is visible.
#
# Arguments:
# w -		The entry window.
# pos -		The desired new position for the cursor in the window.

proc tkEntrySetCursor {w pos} {
  global tkEntry tkBind

  if $tkEntry($w,markActive) {
    return [tkEntryKeySelect $w $pos]
  }

  $w icursor $pos
  $w selection clear
  tkEntrySeeInsert $w

  set tkBind($w,arg) {}
  set tkEntry($w,prevCmd) SetCursor
  tkBindSetMesg $w {}
}

# tkEntryTranspose -
# This procedure implements the "transpose" function for entry widgets.
# It tranposes the characters on either side of the insertion cursor,
# unless the cursor is at the end of the line.  In this case it
# transposes the two characters to the left of the cursor.  In either
# case, the cursor ends up to the right of the transposed characters.
#
# Arguments:
# w -		The entry window.

proc tkEntryTranspose w {
  global tkEntry tkBind

  set i [$w index insert]
  if {$i < [$w index end]} {
    incr i
  }
  set first [expr $i-2]
  if {$first < 0} {
    return
  }
  set new [string index [$w get] [expr $i-1]][string index [$w get] $first]
  set cutbuf [string index $new 1][string index $new 0]
  $w delete $first $i
  $w insert insert $new
  tkEntryUndoPush $w $cutbuf $first $i
  tkEntrySeeInsert $w

  set tkEntry($w,markActive) 0
  set tkBind($w,arg) {}
  set tkEntry($w,prevCmd) Transpose
  tkBindSetMesg $w {}
}

######################################################################
# KILL BUFFER manipulation routines
######################################################################

proc tkEntryGet { w start stop } {
  set start [$w index $start]
  set stop [$w index $stop]
  if {$stop < [$w index end]} {
    incr stop -1
  }
  return [string range [$w get] $start $stop]
}

proc tkEntryPushTagBuffer { thiscut {where 0} } {
  tkTextPushTagBuffer [list $thiscut {}] $where
}

# tkTextYank --
# Paste contents from kill buffer stack in to text widget
#
# Arguments:
# w -		The text window in which to yank
# n -		Depth in to kill buffer stack

proc tkEntryYank { w {n 1}} {
  global tkEntry tkBind
    
  if {[$w cget -state] == "disabled"} return

  set n [tkBindDefArg $w $n]
  set ndx [$w index insert]
  $w insert insert [tkTextGetBufferText $n]
  $w selection from $ndx
  tkEntryUndoPush $w {} $ndx [$w index insert]
  tkEntrySeeInsert $w

  $w selection clear
  set tkEntry($w,markActive) 0
  set tkEntry($w,prevCmd) Yank
  tkBindSetMesg $w {}
}

# tkEntryYank --
# Replace previous yank with next contents of kill buffer stack
#
# Arguments:
# w -		The text window in which to yank
# n -		Depth in to kill buffer stack

proc tkEntryYankPop { w {n 1}} {
  global tkEntry tkBind

  if {$tkEntry($w,prevCmd) != "Yank"} {
    eval $tkBind(bell); return
  }
  set n [tkBindDefArg $w $n]

  $w delete anchor insert
  
  set ndx [$w index insert]
  $w insert insert [tkTextGetBufferText [incr n]]
  $w selection from $ndx

  if {[info exists tkEntry($w,undoCnt)]} {
    set tkEntry($w,undoFirst) $ndx
    set tkEntry($w,undoLast) [$w index insert]
  }

  set tkEntry($w,prevCmd) Yank
  tkBindSetMesg $w {}
}

# tkEntryCopy --
# Place currently marked region onto kill buffer stack
#
# Arguments:
# w -		The text window in which to copy

proc tkEntryCopy { w } {
  global tkEntry tkBind

  clipboard clear -displayof $w
  if {[$w selection present]} {
    clipboard append -displayof $w [selection get -displayof $w]
    tkEntryPushTagBuffer [tkEntryGet $w sel.first sel.last] 0
  } else {
    if {[$w index anchor] < [$w index insert]} {
      clipboard append -displayof $w [tkEntryGet $w anchor insert]
      tkTextPushTagBuffer [tkEntryGet $w anchor insert] 0
    } else {
      clipboard append -displayof $w [tkEntryGet $w insert anchor]
      tkTextPushTagBuffer [tkEntryGet $w insert anchor] 0
    }
    set ndx [$w index insert]
    $w icursor anchor
    tkEntrySeeInsert $w
    after 200 $w icursor $ndx
  }

  $w selection clear
  set tkEntry($w,markActive) 0
  set tkBind($w,arg) {}
  set tkEntry($w,prevCmd) Copy
  tkBindSetMesg $w {}
}

# tkEntryCut --
# Cut currently marked region onto kill buffer stack
#
# Arguments:
# w -		The text window in which to cut

proc tkEntryCut { w } {
  tkEntryDelete $w anchor insert 1 1 
}


######################################################################
# UNDO ROUTINES
######################################################################

# tkEntryUndoSetup --
# Initialize globals for handling undo ring in given widget
#
# Arguments:
# w -		The entry window 

proc tkEntryUndoSetup { w } {
  global tkEntry
  set tkEntry($w,undoCnt) 0
  set tkEntry($w,undoPtr) 0
  set tkEntry($w,undoCut) {}
  set tkEntry($w,undoFirst) -1
  set tkEntry($w,undoLast) -1
  set tkEntry($w,undoList) {}
  set tkEntry($w,undoSafe) 1
}

# tkEntryUndoFree --
# Free globals for handling undo ring in given widget
#
# Arguments:
# w -		The entry window 

proc tkEntryUndoFree { w } {
  global tkEntry

  if {![info exists tkEntry($w,undoCnt)]} return
  unset tkEntry($w,undoCnt)
  unset tkEntry($w,undoPtr)
  unset tkEntry($w,undoCut)
  unset tkEntry($w,undoFirst)
  unset tkEntry($w,undoLast)
  unset tkEntry($w,undoList)
  unset tkEntry($w,undoSafe)
}

# tkEntryUndo --
# Undo the last modification to the given entry widget and
# reset entry state globals
#
# Arguments:
# w -		The entry window 

proc tkEntryUndo { w } {
  global tkEntry tkBind

  if {![info exists tkEntry($w,undoCnt)]} return
  $w selection clear
  tkBindSetMesg $w {Undo.}
  if {$tkEntry($w,prevCmd) != "Undo"} {
    set tkEntry($w,undoPtr) $tkEntry($w,undoCnt)
  }
  set tkEntry($w,undoSafe) 0
  tkEntryUndoPop $w
  set tkEntry($w,undoSafe) 1
  set tkEntry($w,markActive) 0
  set tkBind($w,arg) {}
  set tkEntry($w,prevCmd) Undo
}

# tkEntryUndoPush --
# Push information on undo ring on how to undo a modification
#
# Arguments:
# w -		The entry window 
# cutbuf -	Cut buffer of entry that used to be between the
#		  entry indices first and last
# first, last - The start and ending entry indices of affected
#		  region of entry widget

proc tkEntryUndoPush { w cutbuf first last } {
  global tkEntry tkBind

  if {![info exists tkEntry($w,undoCnt)]} return

  set first [$w index $first]
  set last [$w index $last]

  if {$first == $last} {
    if {![string length $cutbuf]} {
      error "Warning! Empty undo-push"
      return
    }
  }

  lappend tkEntry($w,undoList) [list $tkEntry($w,undoCut) \
				    $tkEntry($w,undoFirst) $tkEntry($w,undoLast)]
  set tkEntry($w,undoCut) $cutbuf
  set tkEntry($w,undoFirst) $first
  set tkEntry($w,undoLast) $last

  incr tkEntry($w,undoCnt)
  if {$tkEntry($w,undoCnt) > $tkBind(undoMax) && $tkEntry($w,undoSafe)} {
    set num [expr $tkEntry($w,undoCnt)-$tkBind(undoMax)]
    set tkEntry($w,undoList) [lreplace $tkEntry($w,undoList) 1 $num]
    set tkEntry($w,undoCnt) $tkBind(undoMax)
  }
}

# tkEntryUndoPop --
# Undo the last modification to the given entry widget
#
# Arguments:
# w -		The entry window 

proc tkEntryUndoPop { w } {
  global tkEntry tkBind

  if {![info exists tkEntry($w,undoCnt)]} {
    eval $tkBind(bell)
    error "Undo tracking is not turned on for this widget!"
  }

  if {$tkEntry($w,undoPtr) < 1} {
    eval $tkBind(bell)
    tkBindSetMesg $w {No more to undo.}
    return
  }

  if {$tkEntry($w,undoPtr) == $tkEntry($w,undoCnt)} {
    set undoCut $tkEntry($w,undoCut)
    set undoFirst $tkEntry($w,undoFirst)
    set undoLast $tkEntry($w,undoLast)
  } else {
    lassign [lindex $tkEntry($w,undoList) $tkEntry($w,undoPtr)] undoCut undoFirst undoLast
  }    

  set loop 0
  set retval {}
  if { $undoFirst == "grp"} {
    set grp [lindex $undoCut 0]
    if { $undoLast == -2.0 } {
      set loop 1
    } else {
      $w icursor [lindex $undoCut 1]
      set retval $grp
    }
  } else {
    if {$undoFirst < $undoLast} {
      set cutbuf [tkEntryGet $w $undoFirst $undoLast]
    } else {
      set cutbuf {}
    }

    $w icursor $undoFirst
    $w delete insert $undoLast
    $w insert insert $undoCut

    lappend tkEntry($w,undoList) [list $tkEntry($w,undoCut) \
				      $tkEntry($w,undoFirst) $tkEntry($w,undoLast)]
    set tkEntry($w,undoCut) $cutbuf
    set tkEntry($w,undoFirst) $undoFirst
    set tkEntry($w,undoLast) [$w index insert]
    incr tkEntry($w,undoCnt)
  }

  incr tkEntry($w,undoPtr) -1
  if $loop {
    tkEntryUndoBeginGroup $w $grp -1.0
    while { $tkEntry($w,undoPtr) > 0 && 
	    [tkEntryUndoPop $w] != $grp} { }
    tkEntryUndoBeginGroup $w $grp -2.0
  }
  tkEntrySeeInsert $w

  return $retval
}

# tkEntryUndoBeginGroup --
# Signals to the undo ring that each subsequent undo push until an
# end group marker is found should be considered part of "one"
# operation when popped later
#
# Arguments:
# w -		The entry window 
# grp -		A arbritrary but unique identifier for the group
#		which must match the one eventually given to 
#		tkEntryUndoEndGroup
# signal -	-1.0 means begin group, -2.0 means end group

proc tkEntryUndoBeginGroup { w grp {signal -1.0}} {
  global tkEntry tkBind

  if {![info exists tkEntry($w,undoCnt)]} return
  lappend tkEntry($w,undoList) [list $tkEntry($w,undoCut) \
				    $tkEntry($w,undoFirst) $tkEntry($w,undoLast)]
  set tkEntry($w,undoCut) [list $grp [$w index insert]]
  set tkEntry($w,undoFirst) grp
  set tkEntry($w,undoLast) $signal

  incr tkEntry($w,undoCnt)
}

# tkEntryUndoEndGroup --
# Signals to the undo ring that a grouping is to end
#
# Arguments:
# w -		The entry window 
# grp -		A arbritrary but unique identifier for the group
#		which must match the one that was given to 
#		tkEntryUndoBeginGroup to start the group

proc tkEntryUndoEndGroup { w grp } {
  tkEntryUndoBeginGroup $w $grp -2.0
}

######################################################################

# tkEntryNumKey --
# Check if currenlty building a number argument and if so, append to
# the argument. Otherwise, insert the number in the entry.
#
# Arguments:
# w -		The entry window in which to yank
# a -		The ascii character of key (decimal number)

proc tkEntryNumKey { w a } {
  global tkEntry tkBind

  if {![string length $tkBind($w,arg)]} {
    tkEntryInsertChar $w $a
  } else {
    tkBindArgKey $w $a
  }
} 

# tkEntryPlaceChar --
# Returns the index of the character that is 'n' characters
# away from the current index
#
# Arguments:
# w -		The entry window in which the cursor is to move.
# n -		The number of chars to move: -1 for left one char,
#		+1 for right one char.

proc tkEntryPlaceChar {w n {ndx insert}} {
  global tkEntry

  set n [tkBindDefArg $w $n]
  if {$n > -1} { set n "+$n"}
  return [expr [$w index $ndx]+$n]
}

# tkEntryPlaceWord --
# Returns the index of the character that is 'n' words
# away from the current index
#
# Arguments:
# w -		The entry window in which the cursor is to move.
# n -		The number of words to move: -1 for left one word,
#		+1 for right one word.

proc tkEntryPlaceWord {w n {ndx insert}} {
  global tkEntry

  set n [tkBindDefArg $w $n]
  return [tkEntryWordIndex $w $ndx $n]
}

######################################################################
# EMACS MARK manipulation routines
######################################################################

# tkEntrySetMark --
# Set the emacs mark to the given Entry index on 0 argument,
# else pop off the given mark in the mark ring
#
# Arguments:
# w -		Entry window in which to set mark.
# ndx -		Entry index to place mark
# n - 		Index of mark to pop off, if non-zero

proc tkEntrySetMark {w {ndx insert} {n 0}} {
  global tkEntry tkBind

  set n [tkBindDefArg $w $n]
  if {$n != 0} {
    if {![llength $tkEntry($w,markRing)]} { eval $tkBind(bell); return }
    set ndx [lindex $tkEntry($w,markRing) end]
    set tkEntry($w,markRing) [concat $ndx [lreplace $tkEntry($w,markRing) end end]]
  } else {
    lappend tkEntry($w,markRing) [$w index $ndx]
    if {[llength $tkEntry($w,markRing)] > $tkBind(killMax)} {
      set tkEntry($w,markRing) [lreplace $tkEntry($w,markRing) 0 0]
    }
    set tkEntry($w,markActive) 1
  }
  $w selection from $ndx

  $w selection clear
  set tkBind($w,arg) {}
  set tkEntry($w,prevCmd) SetMark
  tkBindSetMesg $w {Mark set.}
}

# tkEntryExchangeMark --
# Exchange index positon of insert cursor and emacs mark
#
# Arguments:
# w -		Entry window in which to exchange mark.

proc tkEntryExchangeMark { w } {
  global tkEntry tkBind

  set tmp [$w index insert]
  $w icursor anchor
  tkEntrySetMark $w $tmp

  $w selection to insert
  tkEntrySeeInsert $w
  update idletasks

  set tkEntry($w,markActive) 1
  set tkBind($w,arg) {}
  set tkEntry($w,prevCmd) SetMark
}

# tkEntrySelectAll --
# Select whole Entry buffer
#
# Arguments:
# w -		The Entry window.

proc tkEntrySelectAll w {
  global tkEntry tkBind

  $w selection range 0 end
  $w selection from 0
  $w icursor end
  tkSeeInsert $w

  set tkBind($w,arg) {}
  set tkEntry($w,prevCmd) SelectAll
  tkBindSetMesg $w {}
}

proc tkEntryEvalSel w {
  global tkEntry tkBind

  if [$w selection present] {
    set txt [uplevel #0 "eval \[tkEntryGet $w sel.first sel.last\]"]
  } else {
    if {[$w index anchor] < [$w index insert]} {
      set txt [uplevel #0 "eval \[tkEntryGet $w anchor insert\]"]
    } else {
      set txt [uplevel #0 "eval \[tkEntryGet $w insert anchor\]"]
    }
  }
  regsub -all \n $txt "^J" mtxt
  tkBindSetMesg $w "Eval Result: $mtxt"

  set tkEntry($w,markActive) 0
  set tkBind($w,arg) {}
  set tkEntry($w,prevCmd) EvalSel
  return $txt
}

######################################################################
proc tkEntrySetupPreExist {wlist} {
  foreach w $wlist {
    if {[winfo class $w] == "Entry"} {
      tkEntrySetup $w
    }
    tkEntrySetupPreExist [winfo children $w]
  }
}
tkEntrySetupPreExist .

if {![string length [info proc tkEntryOrig]]} {
  rename entry tkEntryOrig
  proc entry {w args} {
    eval "tkEntryOrig $w $args"
    tkEntrySetup $w
    return $w
  }
}

# Setup/Cleanup bindings

bind Entry <Destroy> {tkEntryDestroy %W}

if ![tkBindSource entry/$tkBind(model).tcl . 1] {
  bell
  puts stderr "WARNING: can't find $tkBind(model) entry bindings. Using base."
  tkBindSource entry/base.tcl
}

# Run user's hook
if {[info proc tkEntryInitHook]!=""} {
  tkEntryInitHook
}
