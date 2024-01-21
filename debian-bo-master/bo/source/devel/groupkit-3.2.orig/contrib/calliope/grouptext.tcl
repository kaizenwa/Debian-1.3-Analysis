#
# grouptext.tcl, a shared text widget
# written by Alex Mitchell, with help by Mark Roseman
# Copyright 1996 Alex Mitchell and the University of Toronto
#
# originally based on
# @(#) text.tcl 1.26 95/03/07 22:11:48
#
# which is
#
# Copyright (c) 1992-1994 The Regents of the University of California.
# Copyright (c) 1994-1995 Sun Microsystems, Inc.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#

# create the grouptext widget
proc grouptext {w args} {
  # pull out the -share option 
  set argnum 0
  set share ""
  set found 0
  foreach thisarg $args {
    if {$thisarg == "-share"} {
        set share [lindex $args [expr $argnum + 1]]
        set found 1
        break
    }
    incr argnum
  }
  if {$found == 1} {
      set args [lreplace $args $argnum [expr $argnum + 1]]
  } else {
      puts "Need to specify sharing type..."
      return
  }

  # create the text widget
  eval text $w -exportselection true $args

  # create environment to store useful info, and clear versions and history list
  gk_newenv info$w
  info$w historyList ""
  grouptextResetVersions $w

  # create a notifier to report changes
  gk_notifier notifier$w

  # determine the desired sharing type, and set up widget accordingly
  if {[grouptextInitializeSharing $w $share] == "false"} { 
      # clean up and return
      grouptextDestroy $w
      return
  }

  # set bindings
  bindtags $w [list $w . all]
  bind $w <Enter> "grouptextBindings $w Enter"
  bind $w <FocusIn> "grouptextBindings $w FocusIn"

  # set ownership to visible
  info$w showOwnership true

  # stop selection from displaying as raised...
  $w tag configure sel -relief flat

  # watch for users leaving 
  gk_bind userDeleted "grouptextDeleteUser $w %U"

  return $w
}

# initialize sharing
proc grouptextInitializeSharing {w share} {
  switch -glob -- $share {
      "serialize" {
          # serialized concurrency control, everyone has write access
          grouptextSetAccess $w true
      }
      "floorControl" {
          # floor control: entire text locking concurrency control 
          # initially noone has the floor...
          grouptextSetAccess $w false
          info$w floorUser -1
      }  
      "locking*" {
          # locking: lock on a given region, non-optimistic
          # initially we don't have a lock
          grouptextSetAccess $w false
      }
      "none" {
          # no sharing - actions are only seen locally
          # obviously give us write access
          grouptextSetAccess $w true
      }
      * { 
          # unknown sharing type...
          puts "Expected serialize, floorControl, locking, or none, got $share" 
          return "false"
      }
  }

  # remember the sharing type
  info$w share $share

  return "true"
}

# cleanly destroy the grouptext widget
proc grouptextDestroy {w} {
    # remove the related information
    catch {info$w destroy}

    # remove the notifier
    catch {notifier$w destroy}

    # and finally remove the text widget
    catch {destroy $w}
}
  
# reset the grouptext widget
proc grouptextReset {w} {  
  # clear the text
  $w delete 1.0 end-1c

  # clear all tags and marks
  foreach thistag [$w tag names] {
      .t tag delete $thistag
  }
  foreach thismark [$w mark names] {
      .t mark unset $thismark
  }

  # set ownership info to visible
  info$w showOwnership true
  
  # clear the history list and version numbers
  info$w historyList ""
  grouptextResetVersions $w

  grouptextInitializeSharing $w [info$w share]
}

# reset the version numbers
proc grouptextResetVersions {w} {
  # reset version information
  foreach i [concat [users keys remote] [list [users local.usernum]]] {
      info$w versions.$i 0
  }
}

# send selection and locking info to a given (usually newly joined) user
proc grouptextSendUserInfo {w number} {
    foreach usernum [concat [users keys remote] [list [users local.usernum]]] {
      if {$usernum != $number} {
        catch {
          # get selection info
          set insertionPos [$w index insert_$usernum]
          if {[$w tag nextrange sel_$usernum 1.0 end] != ""} {
              set selStart [$w index sel_$usernum.first]
              set selEnd [$w index sel_$usernum.last]
          } else {
              set selStart ""
              set selEnd ""
          }
          set anchor ""
    
          # send selection info
          gk_toUserNum $number grouptextDoSetSelection $w $usernum $insertionPos \
              $selStart $selEnd $anchor [info$w versions]

          # get lock info
          if {[$w tag nextrange lock_$usernum 1.0 end] != ""} {
              # a tagged lock region...
              set lockStart [$w index lock_$usernum.first] 
              set lockEnd [$w index lock_$usernum.last] 
              gk_toUserNum $number grouptextSetLock $w $usernum $lockStart $lockEnd
          } else {
              # no tag, but perhaps a mark?
              if ![catch {set lockPos [$w index lock_$usernum]}] {
                  gk_toUserNum $number grouptextSetLock $w $usernum $lockPos $lockPos
              }   
          }
        }
      }
    }
}
                
# a user left, so delete all information about the user
proc grouptextDeleteUser {w usernum} {
    info$w delete versions.$usernum 
    if {[$w tag nextrange sel_$usernum 1.0 end] != ""} {
        $w tag delete sel_$usernum
    }
    if {[$w tag nextrange insert_$usernum 1.0 end] != ""} {
        $w tag delete insert_$usernum
    }
    $w mark unset insert_$usernum

    # determine the sharing type
    switch -glob -- [info$w share] {
        "serialize" {
            # nothing special
        }
        "floorControl" {
            # if this user had the floor, its now free...
            if {[info$w floorUser] == $usernum} {
                info$w floorUser -1
                notifier$w notify floorChange [list [list U -1]]
            }
        }
        "locking*" {
            # delete version entry for this user
            info$w delete versions.$usernum

            # and remove the lock region, if it exists
            if {[$w tag nextrange lock_$usernum 1.0 end] != ""} {
                $w tag delete lock_$usernum
            }
            $w mark unset lock_$usernum
        }
        "none" {
            # nothing special
        }
    }
}


# grouptext bindings --
# The procedure below is invoked the first time the mouse enters a text
# widget or a text widget receives the input focus.  It creates all of
# the class bindings for shared text widgets.
#
# Arguments:
# event -	Indicates which event caused the procedure to be invoked
#		(Enter or FocusIn).  It is used so that we can carry out
#		the functions of that event in addition to setting up
#		bindings.
proc grouptextBindings {w event} {
    global gkPriv tk_strictMotif

    bind $w <Enter> {break}
    bind $w <FocusIn> "grouptextReclaimSelection $w"

    # Standard Motif bindings:
    bind $w <Shift-3> "showHistory $w"

    bind $w <1> {
	grouptextButton1 %W %x %y
    }
    bind $w <B1-Motion> {
	set gkPriv(x) %x
	set gkPriv(y) %y
	grouptextSelectTo %W %x %y
    }
    bind $w <Double-1> {
        grouptextButton1Double %W %x %y
    }
    bind $w <Triple-1> {
        grouptextButton1Triple %W %x %y
    }
    bind $w <Shift-1> {
	grouptextResetAnchor %W @%x,%y
	set gkPriv(selectMode) char
	grouptextSelectTo %W %x %y
    }
    bind $w <Double-Shift-1>	{
        grouptextButton1Double %W %x %y
    }
    bind $w <Triple-Shift-1>	{
        grouptextButton1Triple %W %x %y
    }
    bind $w <B1-Leave> {
	set gkPriv(x) %x
	set gkPriv(y) %y
	grouptextAutoScan %W
    }
    bind $w <B1-Enter> {
	tkCancelRepeat
    }
    bind $w <ButtonRelease-1> {
	tkCancelRepeat
    }
    bind $w <Left> {
	grouptextSetCursor %W [%W index {insert - 1c}]
    }
    bind $w <Right> {
	grouptextSetCursor %W [%W index {insert + 1c}]
    }
    bind $w <Up> {
	grouptextSetCursor %W [grouptextUpDownLine %W -1]
    }
    bind $w <Down> {
	grouptextSetCursor %W [grouptextUpDownLine %W 1]
    }
    bind $w <Shift-Left> {
	grouptextKeySelect %W [%W index {insert - 1c}]
    }
    bind $w <Shift-Right> {
	grouptextKeySelect %W [%W index {insert + 1c}]
    }
    bind $w <Shift-Up> {
	grouptextKeySelect %W [grouptextUpDownLine %W -1]
    }
    bind $w <Shift-Down> {
	grouptextKeySelect %W [grouptextUpDownLine %W 1]
    }
    bind $w <Control-Left> {
	grouptextSetCursor %W [%W index {insert - 1c wordstart}]
    }
    bind $w <Control-Right> {
	grouptextSetCursor %W [%W index {insert wordend}]
    }
    bind $w <Control-Up> {
	grouptextSetCursor %W [grouptextPrevPara %W insert]
    }
    bind $w <Control-Down> {
	grouptextSetCursor %W [grouptextNextPara %W insert]
    }
    bind $w <Shift-Control-Left> {
	grouptextKeySelect %W [%W index {insert - 1c wordstart}]
    }
    bind $w <Shift-Control-Right> {
	grouptextKeySelect %W [%W index {insert wordend}]
    }
    bind $w <Shift-Control-Up> {
	grouptextKeySelect %W [grouptextPrevPara %W insert]
    }
    bind $w <Shift-Control-Down> {
	grouptextKeySelect %W [grouptextNextPara %W insert]
    }
    bind $w <Prior> {
	grouptextSetCursor %W [grouptextScrollPages %W -1]
    }
    bind $w <Shift-Prior> {
	grouptextKeySelect %W [grouptextScrollPages %W -1]
    }
    bind $w <Next> {
	grouptextSetCursor %W [grouptextScrollPages %W 1]
    }
    bind $w <Shift-Next> {
	grouptextKeySelect %W [grouptextScrollPages %W 1]
    }
    bind $w <Control-Prior> {
	%W xview scroll -1 page
    }
    bind $w <Control-Next> {
	%W xview scroll 1 page
    }

    bind $w <Home> {
	grouptextSetCursor %W [%W index {insert linestart}]
    }
    bind $w <Shift-Home> {
	grouptextKeySelect %W [%W index {insert linestart}]
    }
    bind $w <End> {
	grouptextSetCursor %W {insert lineend}
    }
    bind $w <Shift-End> {
	grouptextKeySelect %W {insert lineend}
    }
    bind $w <Control-Home> {
	grouptextSetCursor %W 1.0
    }
    bind $w <Control-Shift-Home> {
	grouptextKeySelect %W 1.0
    }
    bind $w <Control-End> {
	grouptextSetCursor %W {end - 1 char}
    }
    bind $w <Control-Shift-End> {
	grouptextKeySelect %W {end - 1 char}
    }

    bind $w <Tab> {
	grouptextKeyInsert %W \t
	focus %W
	break
    }
    bind $w <Shift-Tab> {
	# Needed only to keep <Tab> binding from triggering;  doesn't
	# have to actually do anything.
    }
    bind $w <Control-Tab> {
	focus [tk_focusNext %W]
    }
    bind $w <Control-Shift-Tab> {
	focus [tk_focusPrev %W]
    }
    bind $w <Control-i> {
	grouptextKeyInsert %W \t
    }
    bind $w <Return> {
	grouptextKeyInsert %W \n
    }
    bind $w <Delete> {
        grouptextDelete %W del
    }
    bind $w <BackSpace> {
        grouptextDelete %W bs
    }

    grouptextClipboardKeysyms $w Alt-c Alt-x Alt-v

    bind $w <Insert> "grouptextPaste $w"
    bind $w <KeyPress> {
	grouptextKeyInsert %W %A
    }

    # Ignore all Alt, Meta, and Control keypresses unless explicitly bound.
    # Otherwise, if a widget binding for one of these is defined, the
    # <KeyPress> class binding will also fire and insert the character,
    # which is wrong.  Ditto for <Escape>.

    bind $w <Alt-KeyPress> {# nothing }
    bind $w <Meta-KeyPress> {# nothing}
    bind $w <Control-KeyPress> {# nothing}
    bind $w <Escape> {# nothing}

    set gkPriv(prevPos) {}
}

# grouptextClipboardKeysyms --
# This procedure is invoked to identify the keys that correspond to
# the "copy", "cut", and "paste" functions for the clipboard.
#
# Arguments:
# copy -	Name of the key (keysym name plus modifiers, if any,
#		such as "Meta-y") used for the copy operation.
# cut -		Name of the key used for the cut operation.
# paste -	Name of the key used for the paste operation.

proc grouptextClipboardKeysyms {w copy cut paste} {
    bind $w <$copy> "grouptextCopy $w"
    bind $w <$cut> "grouptextCut $w"
    bind $w <$paste> "grouptextPaste $w"
}


proc grouptextCopy {w} {
    if {[selection own -displayof $w] == "$w"} {
        clipboard clear -displayof $w
        catch {
            clipboard append -displayof $w [selection get -displayof $w]
        }
    }
}

proc grouptextCut {w} {
    if {[selection own -displayof $w] == "$w"} {
        clipboard clear -displayof $w
        catch {
            clipboard append -displayof $w [selection get -displayof $w]
            if {[$w tag nextrange sel 1.0 end] != ""} {
                grouptextDelete $w 
            }
        }
    }
}

proc grouptextPaste {w} {
    catch {
        grouptextKeyInsert $w [selection get -displayof $w \
            -selection CLIPBOARD]
    }
}

proc grouptextReclaimSelection {w} {
    if {[selection own -displayof $w] != "$w"} {
        catch {
            set contents [selection get -displayof $w]
            if {$contents != ""} {
                clipboard clear -displayof $w
                clipboard append -displayof $w $contents
            }
        }
        set self [users local.usernum]
        if {[$w tag nextrange sel_$self 1.0 end] != ""} {
            $w tag add sel sel_$self.first sel_$self.last
        }
    }
}

#
# mouse events
#

# grouptextButton1 --
# This procedure is invoked to handle button-1 presses in text
# widgets.  It moves the insertion cursor, sets the selection anchor,
# and claims the input focus.
#
# Arguments:
# w -		The text window in which the button was pressed.
# x -		The x-coordinate of the button press.
# y -		The x-coordinate of the button press.
proc grouptextButton1 {w x y} {
    global gkPriv

    set gkPriv(selectMode) char
    set gkPriv(mouseMoved) 0
    set gkPriv(pressX) $x
    grouptextSetSelection $w [$w index @$x,$y]
    if {[$w cget -state] == "normal"} {focus $w}
}

# button 1 double click
# with or without modification (ie. shift key) select a WORD
proc grouptextButton1Double {w x y} {
    global gkPriv

    set gkPriv(selectMode) word
    grouptextSelectTo $w $x $y
}

# button 1 triple click, select a line
proc grouptextButton1Triple {w x y} {
    global gkPriv

    set gkPriv(selectMode) line
    grouptextSelectTo $w $x $y
}


# 
# selection
#

# grouptextSelectTo --
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
proc grouptextSelectTo {w x y} {
    global gkPriv

    # figure out what to select, based on selection mode and mouse location

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

    # now, if there is a selection change, transmit to users
    if {$gkPriv(mouseMoved) || ($gkPriv(selectMode) != "char")} {
        set selStart [$w index $first]
        set selEnd [$w index $last]
        set selAnchor [$w index anchor]
        grouptextSetSelection $w [$w index $cur] \
             $selStart $selEnd $selAnchor 
    }
}

# grouptextAutoScan --
# This procedure is invoked when the mouse leaves a text window
# with button 1 down.  It scrolls the window up, down, left, or right,
# depending on where the mouse is (this information was saved in
# gkPriv(x) and gkPriv(y)), and reschedules itself as an "after"
# command so that the window continues to scroll until the mouse
# moves back into the window or the mouse button is released.
#
# Bug: for some reason it won't stop until a mouse click is received...
# Arguments:
# w -		The text window.
proc grouptextAutoScan {w} {
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
    grouptextSelectTo $w $gkPriv(x) $gkPriv(y)
    set gkPriv(afterId) [after 50 grouptextAutoScan $w]
}

# grouptextSetCursor
# Move the insertion cursor to a given position in a text.  Also
# clears the selection, if there is one in the text, and makes sure
# that the insertion cursor is visible.  Also, don't let the insertion
# cursor appear on the dummy last line of the text.
#
# Arguments:
# w -		The text window.
# pos -		The desired new position for the cursor in the window.
proc grouptextSetCursor {w pos} {
    global gkPriv

    if [$w compare $pos == end] {
	set pos {end - 1 chars}
    }
    grouptextSetSelection $w $pos
    update idletasks
}

# grouptextKeySelect
# This procedure is invoked when stroking out selections using the
# keyboard.  It moves the cursor to a new position, then extends
# the selection to that position.
#
# Arguments:
# w -		The text window.
# new -		A new position for the insertion cursor (the cursor hasn't
#		actually been moved to this position yet).
proc grouptextKeySelect {w new} {
    global gkPriv

    if {[$w tag nextrange sel 1.0 end] == ""} {
	if [$w compare $new < insert] {
	    set first $new
            set last [$w index insert]
	} else {
            set first [$w index insert]
            set last $new
	}
        set anchor [$w index insert]
    } else {
	if [$w compare $new < anchor] {
	    set first $new
	    set last [$w index anchor]
	} else {
	    set first [$w index anchor]
	    set last $new
	}
        set anchor [$w index anchor]
    }

    grouptextSetSelection $w $new $first $last $anchor
}

# grouptextResetAnchor --
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
proc grouptextResetAnchor {w index} {
    global gkPriv

    if {[$w tag ranges sel] == ""} {
# try removing this to let user shift-select from insertion point...
#	$w mark set anchor $index
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

# handle keypresses - mask out non-printing characters...
proc grouptextKeyInsert {w s} {
    if {$s == ""} {
        return
    }
    grouptextInsert $w $s
}


# ...shared text procedures begin here...


#
# selection
#

# request that a selection change be made.  Depending on the sharing
# type being used, we may have to first request a lock...
proc grouptextSetSelection {w insertion {first ""} {last ""} \
        {anchor ""}} {

    set self [users local.usernum]

    # determine the sharing type in use
    switch -glob -- [info$w share] {
        "serialize" {
            # for serialized sharing, serialize the selection request
            gk_serialize grouptextDoSetSelection $w $self \
                $insertion $first $last $anchor [info$w versions]
        }
        "floorControl" {
            # for floor control, if we have the floor, select locally and
            # then send to everyone else (gk_toAll might do this...)
            if {[info$w access] == "true"} {
                grouptextDoSetSelection $w $self $insertion \
                    $first $last $anchor [info$w versions]
                gk_toOthers grouptextDoSetSelection $w $self $insertion \
                    $first $last $anchor [info$w versions]
            }
        }
        "locking*" {
            # first show the local selection, for feedback
#            grouptextDoSetSelection $w $self $insertion $first $last $anchor \
#                [info$w versions]

            # now determine the lock region
            if {$anchor==""} {
                set lockAnchor $insertion
            } else {
                set lockAnchor $anchor
            }
            switch -glob -- [info$w share] {
                "lockingSelection" {
                    # lock the selection
                    if {$first == ""} {
                        set lockFirst $insertion
                        set lockLast $insertion
                    } else {
                        set lockFirst $first
                        set lockLast $last
                    }
                }
                "lockingWord" {
                    # grab the word rather than than selection
                    if [$w compare $insertion < $lockAnchor] {
                        set lockFirst [$w index "$insertion wordstart"]
                        set lockLast [$w index "$lockAnchor - 1c wordend"]
                    } else {
                        set lockFirst [$w index "$lockAnchor wordstart"]
                        set lockLast [$w index "$insertion wordend"]
                    }
                }
                "lockingLine" {
                    # grab the line rather than selection
                    if [$w compare $insertion < $lockAnchor] {
                        set lockFirst [$w index "$insertion linestart"]
                        set lockLast [$w index "$lockAnchor - 1c lineend + 1c"]
                    } else {
                        set lockFirst [$w index "$lockAnchor linestart"]
                        set lockLast [$w index "$insertion lineend + 1c"]
                    }
                }
                "lockingParagraph" {
                    # grab the paragraph rather than selection
                    set lockFirst [grouptextPrevPara $w $insertion]
                    set lockLast [grouptextNextPara $w $insertion]
                }
            }    

            # sanity check: make sure selection isn't bigger than lock
            if {$first != "" && [$w compare $first < $lockFirst]} {
                set lockFirst $first
            }
            if {$last != "" && [$w compare $last > $lockLast]} {
                set lockLast $last
            }

            # find existing lock region...
            if ![catch {set lockMark [$w index lock_$self]}] {
                set oldLockFirst $lockMark
                set oldLockLast $lockMark
            } else {
                set lockRegion [$w tag nextrange lock_$self 1.0 end]
                set oldLockFirst [lindex $lockRegion 0]
                set oldLockLast [lindex $lockRegion 1]
            }

            # is the new region different?
            if {$lockFirst != $oldLockFirst || $lockLast != $oldLockLast} {
                # remove the local user's lock if it exists
                if {[info$w access] == "true"} {
                    $w tag delete lock_$self
                    $w mark unset lock_$self
                    info$w access false
                }
 
                # region is different, so send out a lock request to the network
                gk_serialize grouptextDoLockRequest $w $self $insertion \
                    $first $last $anchor $lockFirst $lockLast [info$w versions]
            } else {
                # no change to lock region, so just change selection...
                gk_serialize grouptextDoSetSelection $w $self $insertion $first $last $anchor \
                    [info$w versions]
            }
        }
        "none" {
            grouptextDoSetSelection $w $self $insertion $first $last $anchor [info$w versions]
        }
    }
}

# this combines the insertion and selection point, and may
# currently contain some redundancy (ie. the selection may be moved
# twice, once to match the insertion point, and then again to 
# match the selection).  This will be fixed...
proc grouptextDoSetSelection {w usernum insertion first last anchor {versions ""}} {
  # translate the incoming selection range
  if {[string first locking [info$w share]] != -1} {
    if {$versions != ""} {
      set trans [grouptextPretranslations $w $usernum $versions \
          $first $last $insertion $anchor]
      set first [lindex $trans 0]
      set last [lindex $trans 1]
      set insertion [lindex $trans 2]
      set anchor [lindex $trans 3]
    }
  }

  # set the insertion point
  $w mark set insert_$usernum $insertion

  if {$usernum==[users local.usernum]} {
    # remove the selection
    $w tag remove sel 0.0 end
    $w tag remove sel_$usernum 0.0 end
    $w mark set insert $insertion
    $w mark set anchor insert
    $w see insert
  } else {
    # remove selection, and show insertion
    if {$first == ""} {
      grouptextUpdateMark $w $usernum "true"
    } else {
      grouptextUpdateMark $w $usernum "false"
    }
  }

  # set the selection if there is one
  if {$first != ""} {
      $w tag remove sel_$usernum 0.0 $first
      $w tag add sel_$usernum $first $last
      $w tag remove sel_$usernum $last end

      if {$usernum==[users local.usernum]} {
          # set the window manager selection
          $w tag remove sel 0.0 $first
          $w tag add sel $first $last
          $w tag remove sel $last end
          $w mark set anchor $anchor

          # make sure insertion is within selection
          if [$w compare insert < $first] {
              $w mark set insert $first
          }
          if [$w compare insert > $last] {
              $w mark set insert $last
          }
      } 

      # show the remote selection
      $w tag configure sel_$usernum -background \
          [gk_getUserAttrib $usernum color] -borderwidth 2 \
          -bgstipple @[userprefs scriptpath]/stipple.xbm
  }

  update idletasks

  # announce the selection change
  notifier$w notify selectText [list [list W $w] [list U $usernum] \
      [list F $first] [list L $last] [list I $insertion] \
      [list A $anchor]]

  # and execute any operation that was waiting for the lock request to be processed...
  if {$usernum == [users local.usernum]} {
      set pendingOperation [info$w pendingOperation]
      if {$pendingOperation != ""} {
          info$w pendingOperation ""
          eval $pendingOperation
      }
  }
}

# update display of remote user's insertion point
# since the text widget won't show marks, we need to
# use a tag on the adjacent character...
proc grouptextUpdateMark {w usernum show} {
  if {$usernum!=[users local.usernum]} {
    $w tag delete sel_$usernum 0.0 end
    $w tag delete insert_$usernum
    $w tag add insert_$usernum insert_$usernum
    if {$show == "true"} {
      $w tag configure insert_$usernum -background \
          [gk_getUserAttrib $usernum color] -borderwidth 2 \
          -bgstipple @[userprefs scriptpath]/stipple.xbm
    }
  }
}


# 
# insert
#

# grouptextInsert --
# Insert a string into a text at the point of the insertion cursor.
# If there is a selection in the text, and it covers the point of the
# insertion cursor, then delete the selection before inserting.
#
# Arguments:
# w -		The text window in which to insert the string
# s -		The string to insert (usually just a single character)
proc grouptextInsert {w s} {
    set self [users local.usernum]
    switch -glob -- [info$w share] {
        "serialize" {
            # for serialized sharing, serialize the insertion
            gk_serialize grouptextDoInsert $w $self $s
        }
        "floorControl" {
            # for floor control, if we have the floor, insert
            # locally, and then tell the others
            if {[info$w access] == "true"} {
                grouptextDoInsert $w $self $s
                gk_toOthers grouptextDoInsert $w $self $s
            }
        }
        "locking*" {
            # for locking, if we have a lock on the region, 
            # insert locally, and then tell others...
            if {[info$w access] == "true"} {
                grouptextDoInsert $w $self $s
                gk_serializeToOthers grouptextDoInsert $w $self $s
            }
        }
        "none" {
            grouptextDoInsert $w $self $s
        }
    }
}

# actually insert text for the specified user
proc grouptextDoInsert {w usernum s} {
    # handle selection/insertion point differences
    set selStart [$w index insert_$usernum]
    set selEnd [$w index insert_$usernum]
    catch {
	if {[$w compare sel_$usernum.first <= insert_$usernum]
		&& [$w compare sel_$usernum.last >= insert_$usernum]} {
            # remember the old selection range (for history list)
            set selStart [$w index sel_$usernum.first]
            set selEnd [$w index sel_$usernum.last]

            # delete the selection
	    $w delete sel_$usernum.first sel_$usernum.last
	}
    }

    # insert the text at the insertion point
    $w insert insert_$usernum $s

    # update the user's insertion and lock if necessary
    if {$usernum!=[users local.usernum]} {
      grouptextUpdateMark $w $usernum true
    }
    set newSelEnd [$w index insert_$usernum]
    grouptextSetLock $w $usernum $newSelEnd $newSelEnd

    # scroll if necessary to keep local user's insertion visible
    if {$usernum==[users local.usernum]} {
      $w see insert
    }

    # update the history list...
    grouptextUpdateHistoryList $w $usernum $s $selStart $selEnd $newSelEnd 

    # insert ownership information
    if {[info$w share] != "none"} {
        grouptextAddOwnership $w $usernum $selStart $newSelEnd
    }
}


#
# delete
#

# delete the text at the selection; need to handle special
# cases of backspace and delete keys...
proc grouptextDelete {w {modifier ""}} {
    global gkPriv

    # figure out the current selection and insertion point
    set self [users local.usernum]
    set selRegion [$w tag nextrange sel_$self 1.0 end]
    set selStart [lindex $selRegion 0]
    set selEnd [lindex $selRegion 1]
    set insertion ""
    catch {set insertion [$w index insert_$self]}

    # handle backspace/delete keys, but only if we have no selection
    # need to select the region and THEN delete the contents
    if {$selStart == ""} {
        switch -exact $modifier {
            "del" {
                # delete key: want to delete the NEXT character
                info$w pendingOperation "grouptextDelete $w"
                grouptextSetSelection $w $insertion $insertion $insertion+1c $insertion+1c
            }
            "bs" {
                # backspace key: want to delete PREVIOUS character
                info$w pendingOperation "grouptextDelete $w"
                grouptextSetSelection $w $insertion $insertion-1c $insertion $insertion-1c 
            }
        }
        return
    }
 
    # now delete the selected text by inserting an empty string
    grouptextInsert $w ""
}


#
# manage the version and history list...
#

# add an entry to the history list
proc grouptextUpdateHistoryList {w usernum s selStart selEnd newSelEnd} {
    # build the entry
    set thisEntry [list $usernum $s $selStart $selEnd $newSelEnd]

    # add to the history list
    info$w historyList [concat [info$w historyList] [list $thisEntry]]

    # increment the version number
    info$w versions.$usernum [expr [info$w versions.$usernum] + 1]

    # tell people an action happened
    notifier$w notify insertText [list [list W $w] [list U $usernum] [list F $selStart] \
        [list L $selEnd] [list N $newSelEnd] [list T "$s"]]
}

# display the list
proc showHistory {w} {
    puts [info$w historyList]
}

# display version numbers
proc showVersions {w} {
    foreach i [concat [users keys remote] [list [users local.usernum]]] {
        puts [info$w versions.$i]
    }
}


#
# utility routines specific to floor control sharing
#

# try to grab the floor; if we already have it, don't bother
proc grouptextGrabFloor {w} {
set floorUser [info$w floorUser]
    if { [info$w access] == "true" } { return }
    gk_serialize grouptextDoGrabFloor $w [users local.usernum]
}

# attempt to get the floor for the given user
proc grouptextDoGrabFloor {w usernum} {
    if { [info$w floorUser] == -1 } {
        # floor is free, so give it to user
        info$w floorUser $usernum
        if { $usernum == [users local.usernum] } {
            # and its me, so give me write access!
            grouptextSetAccess $w true
            notifier$w notify floorChange [list [list U $usernum]]
        }
    }
}

# release the floor (only if we have it, of course)
proc grouptextReleaseFloor {w} {
    if {[info$w access] == "true"} {
        gk_serialize grouptextDoReleaseFloor $w [users local.usernum]
    }
}

# release the floor for the given user
proc grouptextDoReleaseFloor {w usernum} {
    if { [info$w floorUser] == $usernum } {
        # this user has floor, so release
        info$w floorUser -1
        if { $usernum == [users local.usernum] } {
            # and its me, so give up my write access!
            grouptextSetAccess $w false

            notifier$w notify floorChange [list [list U -1]]
        }
    }
}

# who has the floor?
proc grouptextWhoHasFloor {w} {
    return [info$w floorUser]
}


#
# Locking concurrency control:
#

#
# Concurrent access with guaranteed consistency is a 
# strict requirement for a collaborative editor.  It is 
# essential that the text remain the same on all users 
# screens.  The text need not always be exactly the same 
# at a given instant, but, depending on the rate at 
# which the system notifies users, there must be a 
# guarantee of convergence of the contents of the 
# document when the system enters a rest state.  
# This consistency can be maintained by either 
# serializing operations to ensure that changes to the 
# document contents are executed in the same order, or 
# by using a locking mechanism to prevent the execution 
# of conflicting actions.
# 
# GroupKit provides support for serialized concurrency 
# control.  Operations are simply serialized through the 
# central server (in GroupKit this is actually one of 
# the conferences in the session), and then executed on 
# each copy of the document.  This ensures consistency, 
# but can lead to delays, especially if the network 
# connection between the server application and the 
# other applications is slow.  The alternative is to use 
# locking, which allows local insert operations to be 
# performed before being sent to the rest of the copies 
# of the application.
# 
# Calliope implements concurrency control by means of a 
# combination of a central communications server and 
# replicated locks, using a mechanism similar to that 
# used in ShrEdit and SASSE.  The data model is 
# replicated, with a copy of the text document on each 
# userUs instance of the Calliope application.  Using an 
# open protocol approach, a mixture of different 
# locking levels can be used within the same active 
# session.  The local system determines the level of 
# locking to be used locally, and interprets incoming 
# requests.
# 
# There are three basic operations that can be performed 
# on text in Calliope: select, insert and lock.  Select 
# changes a userUs selection to cover a specified range 
# of text.  Insert replaces the text in a userUs current 
# selection with a specified text string.  Lock requests 
# write access for a user on a specified range of text.  
# Select and lock do not alter the document contents.  
# It is important that these operations be applied 
# consistently across all copies of the document.
# 
# When a lock request is made, the request is serialized 
# through the central server.  This ensures that all 
# lock requests are seen in the same order on all copies 
# of the conference.  When a request is received, the 
# local user checks to see if the lock range is free.  
# If it is, the lock is granted.  Users may only alter 
# text which they have locked.
# 
# When an insert is performed, it is executed first 
# locally, and then sent to all other copies of the 
# conference through the serialization server.  
# Performing the insert locally and then transmitting it 
# to the network maximizes local responsiveness.  When 
# the operation is performed, an operation vector is 
# incremented.  This vector keeps track of the number of 
# changes performed by each user on the local copy of 
# the document.  Since insert operations are only 
# permitted on regions on which the local user has a 
# lock, there is no possibility of conflicting 
# operations.  
# 
# The operation vector is used to translate incoming 
# selection and lock requests.  Since local insert 
# operations could have been performed which the remote 
# user does not know about, the selection or lock range 
# must be transformed to take into consideration these 
# local changes.  Similarly, lock and selection ranges 
# stored in the local document must be translated when 
# changes are made to the document.
#

# a user has requested a lock...
proc grouptextDoLockRequest {w usernum insertion first last anchor lockFirst lockLast versions} {
    # pre translations
    set trans [grouptextPretranslations $w $usernum $versions \
        $lockFirst $lockLast]
    set lockFirst [lindex $trans 0]
    set lockLast [lindex $trans 1]

    # remove the current lock, and make the selection
    $w tag delete lock_$usernum 1.0 end
    $w mark unset lock_$usernum

    # check if we can get the lock
    if {[grouptextCheckLock $w $usernum $lockFirst $lockLast] == "false"} {
        # user has been given the lock...
        grouptextSetLock $w $usernum $lockFirst $lockLast
        if {$usernum == [users local.usernum]} {
            grouptextSetAccess $w true
        }
    } else {
        if {$usernum == [users local.usernum]} {
            grouptextSetAccess $w false
        }
    }

    # and set the selection
    grouptextDoSetSelection $w $usernum $insertion $first $last $anchor $versions
}

# set a lock for the given user on the specified range...
proc grouptextSetLock {w usernum lockFirst lockLast} {
    if {[$w compare $lockFirst == $lockLast]} {
        # lock region is between chars, so use a mark
        $w mark set lock_$usernum $lockFirst
    } else {
        # lock is on a range, so use a tag, and show it
        $w tag add lock_$usernum $lockFirst $lockLast
        $w tag configure lock_$usernum -relief ridge -borderwidth 2
    }
}

# set local user's access
proc grouptextSetAccess {w access} {
    set nowriteBitmap [userprefs scriptpath]/nowrite_cursor.xbm
    set nowriteMask [userprefs scriptpath]/nowrite_cursor_mask.xbm

    info$w access $access
    if {$access == "true"} {
        $w configure -cursor xterm
    } else {
        $w configure -cursor "@$nowriteBitmap \
            $nowriteMask black white"
    }
}

# translate the incoming selection, insertion point and anchor
# should use a keyed list instead...?
proc grouptextPretranslations {w usernum versions \
        first last {insertion ""} {anchor ""}} {
    # compare local and remote version numbers
    set differenceFound false
    foreach i [concat [users keys remote] [list [users local.usernum]]] {
        # find difference in version numbers
        set userDiffs(total$i) [expr [info$w versions.$i] - \
            [keylget versions $i]]

        # remember if we found any        
        if {$userDiffs(total$i) !=0} {
            set differenceFound true
            set userDiffs(firstItem$i) -1
        }
    }
    
    # were there any differences? if not, no translation necessary...
    if {$differenceFound == "false"} {
        return [list $first $last $insertion $anchor]
    }
        
    # go back in history list and find all changes that the remote
    # user is unaware of for each user
    set earliestItemNeeded -1
    set historyIndex [expr [llength [info$w historyList]] - 1]
    set finished false
    while {$historyIndex != -1 && $finished != "true"} {
        set historyEntry [lindex [info$w historyList] $historyIndex] 
        set i [lindex $historyEntry 0]

        # are we looking for entries for this user, and have
        # we found all the ones we need?
        if {$userDiffs(total$i) > 0} {
            # remember this history item
            set userDiffs(firstItem$i) $historyIndex
            set userDiffs(total$i) [expr $userDiffs(total$i) -1]
            set earliestItemNeeded $historyIndex
        }

        # are we finished? ie. found all needed differences
        set finished true
        foreach i [concat [users keys remote] [list [users local.usernum]]] {
            if {$userDiffs(total$i) > 0} {
                set finished false
                break
            }
        }
        
        set historyIndex [expr $historyIndex - 1]
    }

    # now move forward through the history list and apply all the
    # necessary translations to bring the remote user up to date
    set historyIndex $earliestItemNeeded
    set historyEnd [llength [info$w historyList]]
    while {$historyIndex < $historyEnd} {
        set historyEntry [lindex [info$w historyList] $historyIndex]
        set i [lindex $historyEntry 0]
      
        # was the remote user unaware of this change?
        if {$historyIndex >= $userDiffs(firstItem$i)} {
            if {$first != ""} {
                # translate the selection range, if it exists
                set trans [grouptextSelectionTranslations \
                    $historyEntry $first $last] 
                set first [lindex $trans 0]
                set last [lindex $trans 1]
            }
            if {$insertion != ""} {
                # translate the insertion point, if any
                set trans [grouptextSelectionTranslations \
                    $historyEntry $insertion] 
                set insertion [lindex $trans 0]
            }
            if {$anchor != ""} {
                # translate the anchor, if any
                set trans [grouptextSelectionTranslations \
                    $historyEntry $anchor] 
                set anchor [lindex $trans 0]
            }
        }
        incr historyIndex
    }

    # return the translated selection and insertion point
    return [list $first $last $insertion $anchor]
}

# actually translate the selection information...
proc grouptextSelectionTranslations {historyEntry first {last ""}} {
    # extract history entry contents
    set selStart [lindex $historyEntry 2]
    set oldSelEnd [lindex $historyEntry 3]
    set newSelEnd [lindex $historyEntry 4]

    # adjust selection start
    set theSelectionPos upstream
    if {[grouptextCompareIndex $first $selStart] == "after"} {
        # selection start is not upstream
        if {[grouptextCompareIndex $first $oldSelEnd] == "before"} { 
            # selection start is within the region, so make it match end
            set first $newSelEnd
            set theSelectionPos within
        } else {
            # selection start is completely downstream, so just shift
            set first [grouptextShiftDownstreamSelection $first $oldSelEnd $newSelEnd]
            set theSelectionPos downstream
        }
    }

    # adjust selection end
    if {$last != ""} {
      if {[grouptextCompareIndex $last $selStart] == "after"} { 
        # selection end is not upstream
        if {[grouptextCompareIndex $last $oldSelEnd] != "after"} { 
            # selection end is within the region
            if {$theSelectionPos != "within"} {
                # for upstream overlap, match selStart
                set theSelectionPos overlapUpstream
                set last $selStart
            } else {
                # for completely within, match newSelEnd
                set last $newSelEnd
            }
        } else {
            # selection end is completely downstream, so just shift
            set last [grouptextShiftDownstreamSelection $last $oldSelEnd $newSelEnd]
            if {$theSelectionPos != "downstream"} {
                if {$theSelectionPos == "within"} {
                    set theSelectionPos overlapDownstream
                } else {
                    set theSelectionPos surronds
                }
            }
        }
      }
    }
    return [list $first $last]
}

# compare 2 text indices when we no longer have the correct 
# version of the tk text widget available; complicated since tk stores indices 
# as line.char, not just a char offset...
proc grouptextCompareIndex {a b} {
    # split indices into line and character
    scan $a "%d.%d" aLine aChar
    scan $b "%d.%d" bLine bChar

    # first compare lines, then if that's inconclusive compare char positions
    if {$aLine == $bLine} {
        # on the same line, so compare character positions
        if {$aChar == $bChar} {
            return equal
        } else {
            if {$aChar < $bChar} {
                return before
            } else {
                return after
            }
        }
    } else {
        if {$aLine < $bLine} {
            return before
        } else {
            return after
        }
    }
}  

# shift a downstream selection; again, need to deal with line.char indices...
proc grouptextShiftDownstreamSelection {shiftPt oldSelEnd newSelEnd} {
    # find the change...
    scan $oldSelEnd "%d.%d" oldSelEndLine oldSelEndChar
    scan $newSelEnd "%d.%d" newSelEndLine newSelEndChar
    set lineDiff [expr $newSelEndLine - $oldSelEndLine] 
    set charDiff [expr $newSelEndChar - $oldSelEndChar] 

    # split up the incoming point
    scan $shiftPt "%d.%d" shiftPtLine shiftPtChar

    # now shift
    if {$oldSelEndLine == $shiftPtLine} {
        # on the same line, so need to shift by character as well as line
        set shiftPtChar [expr $shiftPtChar + $charDiff]
    }
    # and then shift by line
    set shiftPtLine [expr $shiftPtLine + $lineDiff]

    return "$shiftPtLine.$shiftPtChar"
}

# check if we can give user the lock... returns true if region is locked,
# false if region is free.
proc grouptextCheckLock {w usernum first last} {
    # see if the lock region is free
    set locked false
    foreach i [concat [users keys remote] [list [users local.usernum]]] {
        set lockRegion [$w tag nextrange lock_$i 1.0 end]
        if ![catch {set lockMark [$w index lock_$i]}] {
            set lockRegion [concat $lockRegion [list $lockMark $lockMark]]
        } else {
        }
        for {set j 0} {$j < [llength $lockRegion]} {
            incr j
            incr j
        } {
            if {$usernum != $i && $lockRegion != ""} {
                set otherFirst [lindex $lockRegion $j]
                set otherLast [lindex $lockRegion [expr $j+1]]
                if {[grouptextTestOverlap $w $first $last \
                  $otherFirst $otherLast] == "true"} {
                    set locked true
                }
            }
        }
    }
    return $locked
}

# check for overlapping selections...
proc grouptextTestOverlap {w first last otherFirst otherLast} {
    if {[$w compare $otherFirst <= $last] 
            && [$w compare $otherLast >= $first]} {
        return true
    } else {
        return false
    }
}

#
# text ownership information
#

# tag a range of text as written by the specified user
proc grouptextAddOwnership {w usernum selStart selEnd} {
    # add the tag
    set theColour [gk_getUserAttrib $usernum color]
    foreach thistag [$w tag names $selStart] {
        if { [ string first author_ $thistag ] != -1 } {
            $w tag remove $thistag $selStart $selEnd
        }
    }
    $w tag add author_$theColour $selStart $selEnd
    $w tag raise author_$theColour

    # add colour if appropriate
    if {[info$w showOwnership] == "true"} {
        $w tag configure author_$theColour -foreground \
            $theColour
    }

    # set callback for author info
    if {$usernum == [users local.usernum]} {
        set creator [users local.username]
    } else {
        set creator [users remote.$usernum.username]
    }
    $w tag bind author_$theColour <Shift-2> "grouptextOwnershipPopup $w.popup \"$creator\" %X %Y"
    $w tag bind author_$theColour <Shift-B2-ButtonRelease> "destroy $w.popup"

    update idletasks
}

# popup to display user information
proc grouptextOwnershipPopup {w creator x y} {
    menu $w -tearoff 0
    $w add command -label "Written by $creator"
    $w post $x $y
}

# are we displaying ownership information?
proc grouptextOwnershipShowing {w} {
    return [info$w showOwnership]
}

# show ownership information
proc grouptextShowOwnership {w} {
    if {[info$w showOwnership] == "true"} { return }
    info$w showOwnership true
    foreach thisTag [$w tag names] {
        switch -glob -- $thisTag {
            "author_*" {
                scan $thisTag "author_%s" theColour
                $w tag configure $thisTag -foreground \
                    $theColour
            }
        }
    }
}

# hide ownership information
proc grouptextHideOwnership {w} {
    if {[info$w showOwnership] == "false"} { return }
    info$w showOwnership false
    foreach thisTag [$w tag names] {
        switch -glob -- $thisTag {
            "author_*" {
                $w tag configure $thisTag -foreground ""
            }
        }
    }
}

# make selected text public
proc grouptextRemoveOwnership {w} {
    gk_serialize grouptextDoRemoveOwnership $w [users local.usernum]
}

proc grouptextDoRemoveOwnership {w usernum} {
    if {[$w tag nextrange sel_$usernum 1.0 end] != ""} {
        foreach thisTag [$w tag names] {
            switch -glob -- $thisTag {
                "author_*" {
                    $w tag remove $thisTag sel_$usernum.first sel_$usernum.last
                }
            }
        }
    }
}


#
# text utility procedures...
#

# grouptextUpDownLine --
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

proc grouptextUpDownLine {w n} {
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

# grouptextPrevPara --
# Returns the index of the beginning of the paragraph just before a given
# position in the text (the beginning of a paragraph is the first non-blank
# character after a blank line).
#
# Arguments:
# w -		The text window in which the cursor is to move.
# pos -		Position at which to start search.

proc grouptextPrevPara {w pos} {
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

# grouptextNextPara --
# Returns the index of the beginning of the paragraph just after a given
# position in the text (the beginning of a paragraph is the first non-blank
# character after a blank line).
#
# Arguments:
# w -		The text window in which the cursor is to move.
# start -	Position at which to start search.

proc grouptextNextPara {w start} {
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

# grouptextScrollPages --
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

proc grouptextScrollPages {w count} {
    set bbox [$w bbox insert]
    $w yview scroll $count pages
    if {$bbox == ""} {
	return [$w index @[expr [winfo height $w]/2],0]
    }
    set x [expr [lindex $bbox 0] + [lindex $bbox 2]/2]
    set y [expr [lindex $bbox 1] + [lindex $bbox 3]/2]
    return [$w index @$x,$y]
}

# searching
proc grouptextSearch {w string} {
    if {$string == ""} {
	return
    }

    if {$string != [info$w search.string]} {
        info$w search.string $string
        info$w search.cur 1.0
    }

    grouptextDoSearch $w
}

proc grouptextDoSearch {w} {
    set string [info$w search.string]
    set start [info$w search.cur]

    $w tag remove search 0.0 end
    set cur [$w search -count length $string $start end]
    if {$cur == ""} {
        info$w search.cur 1.0
        return
    }
    set curEnd [$w index "$cur + $length char"]
    $w tag add search $cur $curEnd
   
    $w tag configure search -background red
    $w see $cur
    info$w search.cur $curEnd
}

