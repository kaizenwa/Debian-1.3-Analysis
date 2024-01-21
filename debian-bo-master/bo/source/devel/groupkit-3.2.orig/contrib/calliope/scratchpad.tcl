#
# scratchpad: an unstructured text space
#

# create the scratchpad
proc scratchpad {w args} {
    global numTextItems

    eval canvas $w $args

    gk_newenv info$w
    info$w myLock -1
    set numTextItems 0

    # Set up bindings. The B3 callbacks will draw a line when the
    # mouse is moved with the left button down;B3 with shift callback erases lines
    bind $w <3> "startDraw $w %x %y"
    bind $w <B3-Motion> "draw $w %x %y"

    # add in text items
    bind $w <Shift-1> "addText $w %x %y"

    # Add a callback so new conference entrants can be updated
    gk_bind updateEntrant "sendScratchpad $w %U"

    # add a callback to delete users when they leave
    gk_bind userDeleted "scratchpadDeleteUser $w %U"

    return $w
}


# Send the drawing to the entering user by sending them each line in the canvas
proc sendScratchpad {w usernum} { 
    # send lines
    foreach line [$w find withtag line] {
	set x1 [lindex [$w coords $line] 0]
	set y1 [lindex [$w coords $line] 1]
	set x2 [lindex [$w coords $line] 2]
	set y2 [lindex [$w coords $line] 3]
        set theColour [$w itemcget $line -fill]
	gk_toUserNum $usernum  doDraw $w -1 $x1 $y1 $x2 $y2 $theColour
    }

    # send text
    foreach text [info$w keys textItems] {
        set x [lindex [$w coords $text] 0]
        set y [lindex [$w coords $text] 1]
        set content [$w itemcget $text -text]
        gk_toUserNum $usernum  doAddText $w -1 $x $y $content $text
        if {$usernum != "persist_server"} {
            set lockUser [info$w textItems.$text.lockUser]
            gk_toUserNum $usernum info$w textItems.$text.lockUser $lockUser
            if {$lockUser != -1} {
                gk_toUserNum $usernum textShowLock $w $lockUser $text
            }
        } else {
            gk_toUserNum $usernum info$w textItems.$text.lockUser -1
        }
        gk_toUserNum $usernum info$w textItems.$text.access false
    }
    gk_toUserNum $usernum set numTextItems $numTextItems
}    

# a user left, so delete
proc scratchpadDeleteUser {w usernum} {
    foreach text [info$w keys textItems] {
        set lockUser [info$w textItems.$text.lockUser]
        if {$lockUser == $usernum} {
            # this user just left, so remote the lock
            info$w textItems.$text.lockUser -1
            textRemoveLock $w $text
        }
    }
}


# Clear the contents of the canvas
proc clearCanvas {w} {
    gk_serialize doClearCanvas $w
}

proc doClearCanvas {w} {
   global numTextItems

   $w delete line
   $w delete text
   $w delete lock
   info$w destroy
   gk_newenv info$w
   info$w myLock -1
   set numTextItems 0
}

#
# drawing
#

# Draw a line in the canvases of all participants
proc startDraw {w x y} {
    global lastX lastY
    set lastX [$w canvasx $x]
    set lastY [$w canvasy $y]
}

# draw the line
proc draw {w thisX thisY} {
    global lastX lastY 

    # figure out the location
    set thisX [$w canvasx $thisX]
    set thisY [$w canvasy $thisY]
    set x1 $lastX
    set y1 $lastY
    set lastX $thisX
    set lastY $thisY

    # tell all to draw the segment
    gk_toAll doDraw $w [users local.usernum] $x1 $y1 $thisX $thisY
    gk_update
}

# actually draw the line
proc doDraw {w usernum lastX lastY thisX thisY {theColour ""}} {
   if {$theColour == ""} {
       set theColour [gk_getUserAttrib $usernum color]
   }

   # add the line to the canvas
   $w addtag line withtag [$w create line $lastX $lastY $thisX $thisY -width 2  -tag line -fill $theColour]
   update idletasks
}


#
# text
#

# enter text in the canvases of all participants
proc addText {w x y} {
    set x [$w canvasx $x]
    set y [$w canvasy $y]

    # tell all to add the text
    gk_serialize doAddText $w [users local.usernum] $x $y
    gk_update
}

# actually enter the text
proc doAddText {w usernum x y {content ""} {textItemId ""}} {
    global numTextItems

    # get the next unique textitem id
    if {$textItemId == ""} {
        set textItemId text$numTextItems
    }

    # create the textitem, and tag it as text and with its id
    set canvasId [$w create text  $x $y ]
    info$w textItem.$textItemId.canvasId $canvasId
    $w addtag text withtag $canvasId
    $w addtag $textItemId withtag $canvasId
    incr numTextItems

    # add bindings
    $w bind $textItemId <1> "textButton1 $w $textItemId %x %y"
    $w bind $textItemId <Alt-1> "textAltButton1 $w $textItemId %x %y"
    $w bind $textItemId <Left> "textSetInsert $w $textItemId left"
    $w bind $textItemId <Right> "textSetInsert $w $textItemId right"
    $w bind $textItemId <KeyPress> "textInsert $w $textItemId %A"
    $w bind $textItemId <Return> "textInsert $w $textItemId \\n"
    $w bind $textItemId <BackSpace> "textBs $w $textItemId"
    $w bind $textItemId <Delete> "textDel $w $textItemId"
    $w bind $textItemId <2> "textStartDrag $w $textItemId %x %y"
    $w bind $textItemId <B2-Motion> "textDrag $w $textItemId %x %y"
    
    # cut, copy and paste
    $w bind $textItemId <Alt-c> "textCopy $w $textItemId"
    $w bind $textItemId <Alt-x> "textCut $w $textItemId"
    $w bind $textItemId <Alt-v> "textPaste $w $textItemId"    
    $w bind $textItemId <Meta-x> "textCut $w $textItemId"
    $w bind $textItemId <Meta-c> "textCopy $w $textItemId"
    $w bind $textItemId <Meta-v> "textPaste $w $textItemId"    


    # add content if necessary
    if {$content != ""} {
        $w insert $textItemId insert $content
    }

    # set access initially to false
    info$w textItems.$textItemId.access false
    info$w textItems.$textItemId.lockUser -1

    # if its us, try to grab access...
    if {$usernum == [users local.usernum]} {
        $w focus $textItemId
        focus $w
        textGrabAccess $w $textItemId
    }

    update idletasks
}

# action for mouse button 1
proc textButton1 {w textItem x y} {
    set x [$w canvasx $x]
    set y [$w canvasy $y]

    # grab the focus
    $w focus $textItem
    focus $w
    
    # if we have access, then set the insertion point; otherwise try to grab access first
    set insertionPt [$w index $textItem @$x,$y]
    if {[info$w textItems.$textItem.access] == "true"} {
        gk_serialize textDoSetInsert $w $textItem $insertionPt
    } else {
        textGrabAccess $w $textItem
    }
}

# action for alt + button 1 
proc textAltButton1 {w textItem x y} {
    set x [$w canvasx $x]
    set y [$w canvasy $y]

    # if we have access release it
    if {[info$w textItems.$textItem.access] == "true"} {
        textReleaseAccess $w
    }
}

# try to grab access to a text item
proc textGrabAccess {w textItem} {
    # release any existing locks
    textReleaseAccess $w 

    # and request a new lock
    gk_serialize textDoGrabAccess [users local.usernum] $w $textItem
}

# actually try for access
proc textDoGrabAccess {usernum w textItem} {
    if {[info$w textItems.$textItem.lockUser] == -1} {
        # item is free, so grant access
        info$w textItems.$textItem.lockUser $usernum
        textShowLock $w $usernum $textItem
        if {$usernum == [users local.usernum]} {
            info$w textItems.$textItem.access true
            info$w myLock $textItem
        }
    } else { 
        set lockUser [info$w textItems.$textItem.lockUser]
    }
}

# release a textitem
proc textReleaseAccess {w} {
    set myLock [info$w myLock]
    if {$myLock != -1} {
        gk_serialize textDoReleaseAccess [users local.usernum] $w $myLock
    }
}
    
# actually release the item
proc textDoReleaseAccess {usernum w textItem} {
    # remove the lock
    textRemoveLock $w $textItem
    info$w textItems.$textItem.lockUser -1
    if {$usernum == [users local.usernum]} {
        # its us, so set our access to false
        info$w textItems.$textItem.access false
        info$w myLock -1
    }
}

# display the lock on a textitem
proc textShowLock {w usernum textItem} {
    set theBbox [$w bbox $textItem]
    $w create rectangle [lindex $theBbox 0] [lindex $theBbox 1] [lindex $theBbox 2]  [lindex $theBbox 3] -outline [gk_getUserAttrib $usernum color] -tags lock$textItem
    $w addtag lock withtag lock$textItem
}

# remove the lock display on a textitem
proc textRemoveLock {w textItem} {
    $w delete lock$textItem 
}

# set the insertion point in a textitem
proc textSetInsert {w textItem direction} {
    if {[info$w textItems.$textItem.access] == "false" } { return }
        
    figure out which way the insertion point moved
    set currentPosition [$w index $textItem insert]
    if {$direction == "left"} {
        set newPos [expr $currentPosition - 1]
    } else {
        set newPos [expr $currentPosition + 1]
    }

    # tell others about it
    gk_serialize textDoSetInsert $w $textItem $newPos
}

# actually set the insertion point
proc textDoSetInsert {w textItem insertionPt} {
    $w icursor $textItem $insertionPt
}

# copy text to clipboard
proc textCopy {w textItem} {
#    if {[selection own -displayof $w] == "$w"} {
        clipboard clear -displayof $w
        catch {
            clipboard append -displayof $w [$w itemcget $textItem -text]
        }
#    }
}

# copy to clipboard and delete textitem
proc textCut {w textItem} {
    clipboard clear -displayof $w
    catch {
        clipboard append -displayof $w [$w itemcget $textItem -text]
        if {[info$w textItems.$textItem.access] == "true" } {
            textRemoveLock $w $textItem
            $w delete $textItem
        }
    }
}

# paste to the current text item from clipboard
proc textPaste {w textItem} {
    catch {
        textInsert $w $textItem [selection get -displayof $w  -selection CLIPBOARD]
    }
}

# insert text at selection point
proc textInsert {w textItem string} {
    if {[info$w textItems.$textItem.access] == "false" } { return }
        
    if {$string == ""} {
	return
    }

    gk_serialize textDoInsert $w [users local.usernum] $textItem $string
}

# actually do the insert
proc textDoInsert {w usernum textItem string} {
    catch {$w dchars $textItem sel.first sel.last}
    $w insert $textItem insert $string
    textRemoveLock $w $textItem
    textShowLock $w $usernum $textItem
}

# backspace delete
proc textBs {w textItem} {
    if {[info$w textItems.$textItem.access] == "false" } { return }
        
    gk_serialize textDoBs $w [users local.usernum] $textItem
}

proc textDoBs {w usernum textItem} {
    set char [expr {[$w index $textItem insert] - 1}]
    if {$char >= 0} {$w dchar $textItem $char}
    textRemoveLock $w $textItem
    textShowLock $w $usernum $textItem
}

# delete key
proc textDel {w textItem} {
    if {[info$w textItems.$textItem.access] == "false" } { return }
        
    gk_serialize textDoDel $w [users local.usernum] $textItem
}

proc textDoDel {w usernum textItem} {
    $w dchars textItem insert
    textRemoveLock $w $textItem
    textShowLock $w $usernum $textItem
}

# drag a text item
proc textStartDrag {w textItem x y} {
    if {[info$w textItems.$textItem.access] == "false" } { return }
        
    global lastX lastY
    set lastX [$w canvasx $x]
    set lastY [$w canvasy $y]
}

proc textDrag {w textItem x y} {
    if {[info$w textItems.$textItem.access] == "false" } { return }
        
    global lastX lastY
    set x [$w canvasx $x]
    set y [$w canvasy $y]
    gk_toAll textDoDrag $w $textItem [expr $x-$lastX] [expr $y-$lastY]
    set lastX $x
    set lastY $y
    update idletasks
}

proc textDoDrag {w textItem x y} {
    $w move $textItem $x $y
    $w move lock$textItem $x $y
}

