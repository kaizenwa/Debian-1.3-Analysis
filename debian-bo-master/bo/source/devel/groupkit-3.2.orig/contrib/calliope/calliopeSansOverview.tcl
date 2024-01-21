############################################################## 
#
# Calliope: A Multi-User Shared Editor
#
# Copyright 1996 Alex Mitchell and the University of Toronto
#
# For details see http://www.dgp.toronto.edu/people/alex/thesis/calliope.html
#
############################################################## 

#
# file menu items
#

# open a calliope document
proc Open {} {
    global annotations annotationNumber globalFileName

    set fileName [FSBox]
    if {$fileName == ""} {return}
    set oldcursor [.t configure -cursor]
    .t configure -cursor watch
    update

    Clear
    catch {source $fileName}

    # clone to overview
#    gk_serialize overview_copyTo .overview.t .t

    wm title . $fileName
    set globalFileName $fileName
    .t configure -cursor [lindex $oldcursor 4]
    update
}

# import a text file
proc OpenAsText {} {
    global globalFileName

    set fileName [FSBox]
    if {$fileName == ""} {return}
    set oldcursor [.t configure -cursor]
    .t configure -cursor watch
    update

    Clear
    set fd [open $fileName r]
    set result [gets $fd temp]
    while {$result != -1} {
        gk_serialize .t insert end $temp\n
	set result [gets $fd temp]
    }
    close $fd

    # clone to overview
#    gk_serialize overview_copyTo .overview.t .t

    wm title . $fileName
    set globalFileName $fileName
    .t configure -cursor [lindex $oldcursor 4]
    update
}

# save a calliope document
proc Save {} {
    global globalFileName

    if {$globalFileName == ""} {
        set fileName [FSBox]
        if {$fileName == ""} {return}
    } else {
        set fileName $globalFileName
    }
    set oldcursor [.t configure -cursor]
    .t configure -cursor watch
    update

    set content [getConferenceContent]

    set fd [open $fileName w]
    foreach thisItem $content {
        puts $fd "gk_serialize $thisItem"
    }
    close $fd

    wm title . $fileName
    set globalFileName $fileName
    .t configure -cursor [lindex $oldcursor 4]
    update
}

# save, but force user to enter the name of the file
proc SaveAs {} {
    global globalFileName

    set fileName [FSBox]
    if {$fileName == ""} {return}
    set oldcursor [.t configure -cursor]
    .t configure -cursor watch
    update

    set content [getConferenceContent]

    set fd [open $fileName w]
    foreach thisItem $content {
        puts $fd "gk_serialize $thisItem"
    }
    close $fd

    wm title . $fileName
    set globalFileName $fileName
    .t configure -cursor [lindex $oldcursor 4]
    update
}

# save document as text
proc SaveAsText {} {
    global globalFileName

    set fileName [FSBox]
    if {$fileName == ""} {return}
    set oldcursor [.t configure -cursor]
    .t configure -cursor watch
    update

    set fd [open $fileName w]
    puts $fd [.t get 1.0 end-1c]
    close $fd

    wm title . $fileName
    set globalFileName $fileName
    .t configure -cursor [lindex $oldcursor 4]
    update
}

# clear the main document
proc Clear {} {
    gk_serialize doClear
}

proc doClear {} {
    global annotationNumber annotations globalFileName

    # remove the annotations, and reset the environment
    for {set i 0} {$i < $annotationNumber} {incr i} {
        set a annot$i
        set type [annotations $a.type]
        if {$type == "text"} {
          catch {
            grouptextDestroy .$a.t 
            destroy .$a
          }
        }
    }
    annotations destroy
    gk_newenv annotations
    set annotationNumber 0   

    # clear the main text widget
    grouptextReset .t 

    # reset local user's selection
    grouptextSetSelection .t 1.0

    # clear the overview
#    overview_reset .overview.t

    wm title . "Calliope"
    set globalFileName ""
}

# print the file to postscript printer
proc Print {} {
    set i [tk_dialog .print "Print" {Print file to mom?}  questhead 0 OK Cancel]
    if {$i == 0} {
        set theText [.t get 1.0 end-1c]
        exec enscript -2r -Pmom <<$theText
    }
}


#
# joining
#

# update a new user
proc updateNewUser {number} {
#    puts "Updating user $number"
#gk_toUserNum $number puts "About to get updated by [users local.usernum]"

    # freeze everyone here
    if {$number != "persist_server"} {
        freeze $number
        gk_serializeToOthers freeze $number
    }

    # get the contents of the text and annotations
    set content [getConferenceContent]

    # send to the new user
    foreach thisItem $content {
        gk_toUserNum $number eval $thisItem
    }

    # reset version numbers (for all)
    gk_serialize grouptextResetVersions .t
  
    # send new user all of our selection points 
    if {$number != "persist_server"} {
        grouptextSendUserInfo .t $number
    }

    # get new user to select in the main text window... 
    if {$number != "persist_server" } {
        gk_toUserNum $number grouptextSetSelection .t 1.0
    }

    # send the overview
#    gk_toUserNum $number overview_copyTo .overview.t .t

    # and now unfreeze
    if {$number != "persist_server" } {
        gk_serialize unfreeze
    }
}

# freeze the conference until the new user has been updated
proc freeze {number} {
    if [winfo exists .freezeWindow] {return}
    toplevel .freezeWindow
    grab .freezeWindow
    wm focusmodel .freezeWindow active
    wm withdraw .freezeWindow

    # Calculations for position of window
    set parentPos [string range [winfo geometry .] \
        [string first + [winfo geometry .]] end]
    set xPos [string range $parentPos 1 [expr [string last + $parentPos] - 1]]
    set yPos [string range $parentPos [expr [string last + $parentPos] + 1] end]
    set where "+[expr ([winfo width .] / 3) + $xPos]+[expr ([winfo height .] / 3) + $yPos]"

    # Draw window
    wm geometry .freezeWindow $where
    wm title .freezeWindow ""
    wm deiconify .freezeWindow
    if {$number == [users local.usernum]} {
        label .freezeWindow.l -text "Joining, please wait" -relief flat
    } else {
        label .freezeWindow.l -text "A new user is joining, please wait" -relief flat
    }
    pack .freezeWindow.l
    update
}

proc unfreeze {} {
    destroy .freezeWindow
    update
}


# extract the conference content and return as a list
# of tcl commands...
proc getConferenceContent {} {
    global annotationNumber annotations

    # get the main text document
    lappend confContent ".t insert 1.0 \{[.t get 1.0 end-1c]\}"

    # get annotations - need to account for deleted annotations...
    # deleted annotations are saved, but not linked to the text
    for {set i 0} {$i < $annotationNumber} {incr i} {
        set a annot$i
        set type [annotations $a.type]
        set content [annotations $a.content]
        set creator [annotations $a.creator]

        lappend confContent "annotations $a.type $type"
        lappend confContent "annotations $a.content $content"
        lappend confContent "annotations $a.creator \"$creator\""
        if {[.t tag nextrange $a 1.0 end] == ""} {
            # label annotation (or maybe an orphan - catch to make sure...)
            if ![catch {set insertionPt [.t index .t.$a]}] {
                lappend confContent "insertLabelAnnotation .t $a $insertionPt"
                lappend confContent "bind .t.$a <2> \"showAnnotation $a\""
            }
        } else {
            # range annotation
            set first [.t index $a.first]
            set last [.t index $a.last]
            lappend confContent "insertRangeAnnotation .t $a $first $last"
            lappend confContent ".t tag bind $a <2> \"showAnnotation $a\""
        }
      
        if {$type == "text"} {
            lappend confContent "createAnnotationWindow $a -1 \"$creator\""
            set content [.$a.t get 1.0 end-1c]
            lappend confContent ".$a.t insert 1.0 \{$content\}"
        }
    }
    lappend confContent "set annotationNumber $annotationNumber"
 
    # ownership information
    foreach thistag [.t tag names] {
        if { [string first author_ $thistag] != -1 } {
            # get colour of tag
            set theColour [string range $thistag 7 end]
            lappend confContent ".t tag configure $thistag -foreground $theColour"

            # get the callbacks
            set callback [.t tag bind $thistag <Shift-2>]
            set idx1 [string first "\"" $callback]
            set idx2 [string last "\"" $callback]
            set creator [string range $callback [expr $idx1+1] [expr $idx2-1]]
            lappend confContent ".t tag bind $thistag <Shift-2> \
                \"grouptextOwnershipPopup .t.popup \\\"$creator\\\" %X %Y\""
            lappend confContent ".t tag bind $thistag <Shift-B2-ButtonRelease> \
                \"destroy .t.popup\""

            # get the ranges
            set theRanges [.t tag ranges $thistag]
            if {$theRanges != ""} {
                for {set j 0} {$j < [llength $theRanges]} {
                    incr j
                    incr j
                } {
                    set first [lindex $theRanges $j]
                    set last [lindex $theRanges [expr $j+1]]
                    lappend confContent ".t tag add $thistag $first $last"
                }
            }
        }
    }

    return $confContent
}


#
# telepointers
#

# toggle telepointing on/off
proc toggleTelepointer {} {
    set self [users local.usernum]

    if {[telepointers users.$self.show] == 0} {
        telepointers users.$self.show 1
        gk_toOthers showTelepointer $self
    } else {
        telepointers users.$self.show 0
        gk_toOthers hideTelepointer $self
    }
}

# hide telepointer
proc hideTelepointer {who} {
    if {[telepointers users.$who.show] == 0} {return}

    # hide from display
    if {[telepointers users.$who.frame] != ""} {
        place forget [telepointers users.$who.frame]
    }
    telepointers users.$who.show 0
}

# show telepointer
proc showTelepointer {who} {
    if {[telepointers users.$who.show] != 0} {return}
    
    telepointers users.$who.show 1
}


#
# coloured text
#

proc toggleOwnership {} {
    if {[grouptextOwnershipShowing .t] == "true"} {
        grouptextHideOwnership .t
    } else {
        grouptextShowOwnership .t
    }
}

proc makePublic {} {
    grouptextRemoveOwnership .t
}

# utility procs

proc showWindow {w} {
    wm deiconify $w
    raise $w
}

#
# scratchpad
#

# create the scratchpad
proc createScratchpad {} {
    toplevel .scratchpad
    wm withdraw .scratchpad
    pack [frame .scratchpad.buttons] -side bottom
    pack [button .scratchpad.buttons.b1 -text "Close" -command "wm withdraw .scratchpad"] \
        -side right
    pack [button .scratchpad.buttons.b2 -text "Clear scratchpad" -command \
        "clearCanvas .scratchpad.c"] -side left
    pack [scratchpad .scratchpad.c -relief ridge -borderwidth 2 ] \
         -fill both -expand yes -side left

    # Attach the GroupKit telepointers to the canvas
    gk_specializeWidgetTreeTelepointer .scratchpad.c
}

proc ShowScratchpad {} {
    showWindow .scratchpad
}


#
# overview
#

# create the overview
proc createOverview {} {
    toplevel .overview
    wm withdraw .overview
    pack [frame .overview.buttons] -side bottom
    pack [button .overview.buttons.b1 -text "Close" -command \
        "wm withdraw .overview"] -side right
    pack [scrollbar .overview.scr -command ".overview.t yview"] \
        -fill y -side right
    pack [overview .overview.t -mainText .t -width 200 -height 100 \
        -background white -yscrollcommand ".overview.scr set" \
        -wrap none -font -*-lucida-medium-r-*-*-10-*-*-*-*-*-*-* ] \
        -expand y -fill both
}

proc showOverview {} {
    showWindow .overview
}


#
# interface
#

# create the main text window
proc createMainText {} {
    global annotationNumber privateTextNumber

    # interface
    pack [gk_scrollbar .scr -command ".t yview"] -side right -fill y
    pack [grouptext .t -share lockingSelection -width 80 -height 24 \
        -yscrollcommand ".scr set" -background white -wrap none] \
        -side left -fill both -expand yes

    # handle update entrant events
    gk_bind updateEntrant {updateNewUser %U}

    # test to see if we can handle user left events
#    gk_bind userDeleted {puts "User deleted..."}

    # annotations
    gk_newenv annotations
    set annotationNumber 0

    # telepointers
    gk_initializeTelepointers
    gk_specializeWidgetTreeTelepointer .t
}


# create the main menu
proc createMainMenu {} {
    pack [gk_defaultMenu .menu] -side top -fill x
}

# file menu
proc addFileMenu {} {
    .menu itemcommand 0 insert 1 command -label "Open" -command "Open" -accelerator "meta+o"
    bind . <Meta-o> "Open"
    .menu itemcommand 0 insert 2 command -label "Open text file" -command "OpenAsText"
    .menu itemcommand 0 insert 3 command -label "Save" -command "Save" -accelerator "meta+s"
    bind . <Meta-s> "Save"
    .menu itemcommand 0 insert 4 command -label "SaveAs" -command "SaveAs"
    .menu itemcommand 0 insert 5 command -label "Save as text" -command "SaveAsText"
    .menu itemcommand 0 insert 6 command -label "Clear" -command "Clear"
    .menu itemcommand 0 insert 7 command -label "Print" -command "Print" -accelerator "meta+p"
    bind . <Meta-p> "Print"
    .menu itemcommand 0 entryconfigure 8 -accelerator "meta+q"
    bind . <Meta-q> "_gk_properQuit"
}

# edit menu
proc addEditMenu {} {
    .menu add edit 1 -text "Edit"
    .menu itemcommand edit insert 1 command -label "Cut" -command "grouptextCut .t"  -accelerator "meta+x"
    bind . <Meta-x> "grouptextCut .t"
    .menu itemcommand edit insert 2 command -label "Copy" -command "grouptextCopy .t"  -accelerator "meta+c"
    bind . <Meta-c> "grouptextCopy .t"
    .menu itemcommand edit insert 3 command -label "Paste" -command "grouptextPaste .t"  -accelerator "meta+v"
    bind . <Meta-v> "grouptextPaste .t"
}

# tools menu
proc addToolsMenu {} {
    .menu add tools 2 -text "Tools"
    .menu itemcommand tools insert 1 command -label "Public Annotation" -command "PublicAnnotation"
    .menu itemcommand tools insert 2 command -label "external annotation" -command "WebAnnotation"
    .menu itemcommand tools insert 3 command -label "List annotations" -command "listAnnotations"
    .menu itemcommand tools insert 4 command -label "private text" -command "PrivateText .t"
    bind .t <3> "PrivateText .t"
    .menu itemcommand tools insert 5 command -label "scratchpad" -command "ShowScratchpad"
    menu .menu.tools.menu.sharing
    .menu itemcommand tools insert 6 cascade -label "Sharing" \
        -menu .menu.tools.menu.sharing
    .menu.tools.menu.sharing insert 1 command -label "selection" -command "selectionLocking"
    .menu.tools.menu.sharing insert 2 command -label "word" -command "selectionWord"
    .menu.tools.menu.sharing insert 3 command -label "line" -command "lineLocking"
    .menu.tools.menu.sharing insert 4 command -label "paragraph" -command "paragraphLocking"
}

proc selectionLocking {} {
     info.t share lockingSelection
}

proc selectionWord {} {
     info.t share lockingWord
}

proc lineLocking {} {
     info.t share lockingLine
}

proc paragraphLocking {} {
     info.t share lockingParagraph
}



# collaboration menu
proc addCollaborationMenu {} {
    .menu itemcommand 3 insert 5 command -label "Toggle telepointer" \
        -command "toggleTelepointer"
    .menu itemcommand 3 insert 6 command -label "Toggle coloured text" \
        -command "toggleOwnership"
    .menu itemcommand 3 insert 7 command -label "Make public" \
        -command "makePublic"
#    .menu itemcommand 3 insert 8 command -label "Overview" \
#        -command "showOverview"
}

# help menu
proc addHelpMenu {} { 
    set help_title "About Calliope"
    set help_text "http://www.dgp.toronto.edu/people/alex/thesis/calliope.html"
 
    .menu itemcommand 4 add command \
        -label "$help_title" \
        -command "openURL $help_text" 
}

proc createMessageWindow {} {
    pack [frame .message] -side bottom -fill x
    pack [label .message.l -height 1 -relief ridge -justify left] \
        -side left -fill x -expand yes 
}


#
# main program
#

# initialize conference with groupkit
gk_initConf $argv

# load in modules
source [userprefs scriptpath]/grouptext.tcl
source [userprefs scriptpath]/scratchpad.tcl
source [userprefs scriptpath]/overview.tcl
source [userprefs scriptpath]/annotations.tcl
source [userprefs scriptpath]/private.tcl
source [userprefs scriptpath]/conf.tcl

# show watch cursor
. configure -cursor watch
wm title . "Please wait..."

# set up interface for the main window
createMainMenu
createMainText

# add menu items
addFileMenu
addEditMenu
addToolsMenu
addCollaborationMenu
addHelpMenu

# create the scratchpad
createScratchpad

# create the overview window
#createOverview

# set filename to nothing for now
set globalFileName ""

wm iconbitmap . @[userprefs scriptpath]/calliope.xbm

# reset cursor
. configure -cursor {}
wm title . "Calliope"

# current bugs:
# 1. can't handle 3 or more users... (a timing problem in groupkit)
# 2. cut/paste in scratchpad is a bit strange
# 4. coloured lock indicators



