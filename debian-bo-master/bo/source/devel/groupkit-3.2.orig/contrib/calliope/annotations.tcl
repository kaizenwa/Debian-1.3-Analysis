#
# annotations
#

# add a public annotation...
proc PublicAnnotation {{content ""}} {
    gk_serialize DoPublicAnnotation .t [users local.usernum] $content
}

# actually add a public annotation
proc DoPublicAnnotation {w usernum {content ""}} {
    global annotations annotationNumber

    # get the unique identifier for the annotation
    set a annot$annotationNumber
    annotations $a.type text
    annotations $a.content ""
    if {$usernum == [users local.usernum]} {
        annotations $a.creator [users local.username]
    } else {
        annotations $a.creator [users remote.$usernum.username]
    }
    incr annotationNumber
    catch {.annotations.list insert end "${annotationNumber}. $content"}

    # is the annotation on a range or between 2 characters?
    if {[$w tag nextrange sel_$usernum 1.0 end] == ""} {
        # between: create the label in the main text window
        insertLabelAnnotation $w $a insert_$usernum
        bind $w.$a <2> "showAnnotation $a"
    } else {
        # range: create a tag to cover the text, and underline it...
        insertRangeAnnotation $w $a sel_$usernum.first sel_$usernum.last
        $w tag bind $a <2> "showAnnotation $a"
    }

    # now create the annotation window
    createAnnotationWindow $a $usernum [annotations $a.creator]
    if {$content != ""} {
        annotations $a.content $content
        .$a.t insert insert $content
    }

    # if we created it, grab the floor
    if {$usernum == [users local.usernum]} {
        #after 1 grouptextGrabFloor .$a.t
        grouptextGrabFloor .$a.t
    }
}

# create the public annotation window
proc createAnnotationWindow {a usernum creator} {
    toplevel .$a
    wm title .$a "Public Annotation"
    if {$usernum != [users local.usernum]} {
        wm withdraw .$a
    }
    pack [frame .$a.buttons] -side bottom
    pack [button .$a.buttons.b1 -text "Close" -command "wm withdraw .$a"]  -side right
    if {$usernum != [users local.usernum]} {
        createTurnTakingButton .$a.buttons.b2 .$a.t false
    } else {
        createTurnTakingButton .$a.buttons.b2 .$a.t true
    }
    pack [label .$a.buttons.l1 -text "Created by: $creator"  -relief ridge -borderwidth 2] -side left
    pack [scrollbar .$a.scr -command ".$a.t yview"] -side right -fill y
    pack [grouptext .$a.t -share floorControl -height 5 -width 20  -yscrollcommand ".$a.scr set" -wrap word -background white]  -side left -fill both -expand yes

    # add telepointers?
    gk_specializeWidgetTreeTelepointer .$a.t

    # set callback for floor change
    notifier.$a.t bind floorChange "FloorChange %U .$a.t .$a.buttons.b2"
}

# create a button to let user request the floor
proc createTurnTakingButton {b t access} {
    if {$access == "false"} {
        pack [button $b -bitmap @[userprefs scriptpath]/nowrite.xbm  -relief ridge -command "GrabFloor $t"] -side left 
    } else {
        pack [button $b -bitmap @[userprefs scriptpath]/write.xbm  -relief ridge -command "ReleaseFloor $t"] -side left
    }
}

# set the bitmap on button 
proc setTurnTakingButton {b t access} {
    if {$access == "false"} {
        $b configure -bitmap @[userprefs scriptpath]/nowrite.xbm  -command "GrabFloor $t" 
    } else {
        $b configure -bitmap @[userprefs scriptpath]/write.xbm  -command "ReleaseFloor $t"
    }
}
    
# callback when the floor changes hands, called by the text widget
proc FloorChange {user t b} {
    if {$user == [users local.usernum]} {
        # I just got the floor
        setTurnTakingButton $b $t true
    } else {
        # someone else has the floor
        setTurnTakingButton $b $t false
    }
}

# try to get floor
proc GrabFloor {t} {
    grouptextGrabFloor $t
}

# give up floor
proc ReleaseFloor {t} {
    grouptextReleaseFloor $t
} 


# insert a web annotation
proc WebAnnotation {} {
    toplevel .webEntry
    pack [label .webEntry.l -width 30 -text "Enter URL of annotation:"]
    pack [entry .webEntry.e -width 30 -relief sunken -textvariable url]
    bind .webEntry.e <Return> {
        gk_serialize DoWebAnnotation .t [users local.usernum] $url
        destroy .webEntry
    }
}
    
# actuallly insert a web annotation
proc DoWebAnnotation {w usernum url} {
    global annotations annotationNumber

    # get the unique identifier for the annotation
    set a annot$annotationNumber
    annotations $a.type url
    annotations $a.content $url
    if {$usernum == [users local.usernum]} {
        annotations $a.creator [users local.username]
    } else {
        annotations $a.creator [users remote.$usernum.username]
    }
    incr annotationNumber
    catch {.annotations.list insert end "${annotationNumber}. $content"}

    # is the annotation on a range or between 2 characters?
    if {[$w tag nextrange sel_$usernum 1.0 end] == ""} {
        # between: create the label in the main text window
        insertLabelAnnotation $w $a insert_$usernum
        bind $w.$a <2> "showAnnotation $a"
    } else {
        # range: create a tag to cover the text, and underline it...
        insertRangeAnnotation $w $a sel_$usernum.first sel_$usernum.last
        $w tag bind $a <2> "showAnnotation $a"
    }
}

# open a url using netscape
proc openURL {url} {
    if [catch { exec <@stdin >@stdout netscape -remote openURL($url) } ] {
        # netscape isn't running - try to fork a process to run it...
        puts "trying to start netscape..."
        if [ catch { exec <@stdin >@stdout netscape $url & } ] {
            # netscape failed to run...
            toplevel .oops
            pack [label .oops.l -text "Can't find netscape..."]
            pack [button .oops.b -text "OK" -command "destroy .oops"]
        } 
    }
}


# insert an annotation between 2 characters
proc insertLabelAnnotation {w a insertionPt} {
    label $w.$a -bitmap @[userprefs scriptpath]/text.xbm -background white
    $w window create $insertionPt -window $w.$a
    gk_specializeWidgetTelepointer $w.$a
}

# insert an annotation over a range of characters
proc insertRangeAnnotation {w a startRange endRange} {
    $w tag add $a $startRange $endRange
    $w tag configure $a -underline y
}

# show the given annotation
proc showAnnotation {a} {
    set type [annotations $a.type]
    set content [annotations $a.content]
    if {$type == "text"} {
        showWindow .$a
    } else {
        openURL $content
    }
}


# handle orphan annotations
proc listAnnotations {} {
    global annotationNumber annotations

    catch {destroy .annotations}
    toplevel .annotations
    pack [frame .annotations.buttons] -side bottom
    pack [button .annotations.buttons.b1 -text "Close" -command "destroy .annotations"]  -side right
    pack [button .annotations.buttons.b2 -text "Update" -command "updateAnnotationList"]  -expand yes -fill x -side top
    pack [listbox .annotations.list -yscrollcommand ".annotations.scroll set"]  -expand yes -fill both -side left
    pack [scrollbar .annotations.scroll -command ".annotations.list yview"]  -side right -fill y
  
    updateAnnotationList 
    bind .annotations.list <Double-1> {showAnnotation annot[.annotations.list curselection]}
}

proc updateAnnotationList {} { 
    global annotationNumber annotations

    # get the annotations and put in list
    .annotations.list delete 0 end
    for {set i 0} {$i < $annotationNumber} {incr i} {
        set type [annotations annot$i.type]
        if {$type == "text"} {
            set content [.annot$i.t get 1.0 end-1c]
        } else {
            set content [annotations annot$i.content]
        }
        .annotations.list insert end "[expr $i + 1]. $content"
    }
}





