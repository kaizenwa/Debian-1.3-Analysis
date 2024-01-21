#
# private text
#

# add a private text window...
proc PrivateText {w} {
    global privateTextNumber

    if {[info exists privateTextNumber] == 0} {
        set privateTextNumber 0
    }
    set pr private$privateTextNumber
    incr privateTextNumber

    toplevel .$pr
    wm title .$pr "private text"
    pack [frame .$pr.buttons] -side bottom
    pack [button .$pr.buttons.b1 -text "Cancel" -command "destroy .$pr"] \
        -side right
    pack [button .$pr.buttons.b2 -text "Insert" -command \
        "insertPrivateText $w .$pr"] -side left
    pack [button .$pr.buttons.b3 -text "Attach" -command \
        "attachPrivateText $w .$pr"] -side left
    pack [scrollbar .$pr.scr -command ".$pr.t yview"] -side right -fill y
    pack [grouptext .$pr.t -share none -height 5 -width 20 \
        -yscrollcommand ".$pr.scr set" -wrap word -background white] \
        -side left -fill both -expand yes

    set usernum [users local.usernum]
    if {[$w tag nextrange sel_$usernum 1.0 end] != ""} {
        .$pr.t insert 1.0 [$w get sel_$usernum.first sel_$usernum.last]
    }
}

# insert the text into the current selection
proc insertPrivateText {w theText} {
    grouptextInsert $w [$theText.t get 1.0 end-1c]
    destroy $theText
}

# attach the text as an annotation
proc attachPrivateText {w theText} {
    PublicAnnotation [$theText.t get 1.0 end-1c]
    destroy $theText
}

