# thread.tcl
#
#
# Gets all messages in a thread and displays them in a new FTOC
#
# Ignacio Martinez        <martinez@fundesco.es>
# Fundesco
# Madrid, April 1995
#

proc Thread_Scan { folder initmsgs } {
    global thread

    set scan_fmt "%(msg)%{message-id}%{in-reply-to}%{references}"
    set scan_cmd \
      [list exec scan -noheader -noclear -width 9999 -format $scan_fmt +$folder all]

    if [catch $scan_cmd out] {
        Exmh_Status "fail: $out" purple
        set thread {}
    }

    Exmh_Status "Indexing message references ..." blue
    set scanlines [split $out \n]
    while {[llength $scanlines] > 0} {
        set line [lindex $scanlines 0]
        regexp {^([0-9]+)} $line x msg
        set msgs($msg) {}
        while {[regexp {<([^>]*)>(.*)} $line x mid newline] == 1} {
            if ![info exists refs($mid)] {
                set refs($mid) {}
            }
            lappend refs($mid) $msg
            lappend msgs($msg) $mid
            set line $newline
        }
        set scanlines [lreplace $scanlines 0 0]
    }

    Exmh_Status "Computing cross-references ..." blue
    set thread $initmsgs
    while {[llength $initmsgs] > 0} {
        set msg [lindex $initmsgs 0]
        foreach id $msgs($msg) {
            foreach ref $refs($id) {
                if {[lsearch $thread $ref] < 0} {
                    lappend thread   $ref
                    lappend initmsgs $ref
                }
            }
        }
        set initmsgs [lreplace $initmsgs 0 0]
    }        

    set thread [lsort -integer $thread]
}

proc Thread_Display {} {
    global thread exwin exmh ftoc msg

# check if we are in limbo.
#   if {$ftoc(folder) != $exmh(folder)} {
#       Exmh_Status "Temporary display. Select a valid folder first" warn
#       return
#   }

    # Nothing selected so do nothing.
    # Should never happen if button is state (current|range) enabled
    if {[Ftoc_PickSize] < 1} {
      Exmh_Status "You must select at least one message first" warn
      return
    }

    set autocommit 0               ;# allow automatic commit changes? debatable
    set folder     $exmh(folder)   ;# the global (real) folder name
    set thread     {}              ;# list of hits
    set curmsg     {}              ;# the current selected message
    set show       noshow          ;# redisplay message?

    # single (current) or multiple (range) message selection?
    if $ftoc(pickone) {
        # current
        set curmsg $msg(id)
        # inquire display state for later redisplaying
        # this doesn't always work because msg(dpy) is not cleared by MsgClear
        # and clicking <3> clears message window but msg(dpy) remains set :-(
        if {$msg(dpy) == $curmsg} {
            set show show
        }
        lappend msgs $curmsg
    } else {
        # range
        foreach l $ftoc(lineset) {
            lappend msgs [Ftoc_MsgNumber $l]
        }
    }

    # Do changes first to synchronize with scan
    if {[Ftoc_Changes "Change folder" $autocommit] > 0} {
        return
    }

    # Check that selection hasn't changed after Ftoc_Commit
    set ok 1
    if {$curmsg != {}} {
        if {$msg(id) != $curmsg} {
            set ok 0
        }
    } else {
        foreach m $msgs {
            set L [Ftoc_FindMsg $m]
            if {$L == {}} {
                set ok 0; break
            }
        }
    }
    if {!$ok} {
        Exmh_Status "Selection has changed after committing. Reselect" warn
        return
    }

    # Scan whole folder for cross references
    Exmh_Status "Scanning $folder for references to $msgs ..." blue
    busy Thread_Scan $folder $msgs
    set hits  [llength $thread]
    if {$hits <= 0} {
        # error already signaled by Thread_Scan
        return
    }
    if {$hits == 1} {
        # just one hit means no thread
        Exmh_Status "No references to $msgs found" purple
        return
    }

    # get text associated to hit messages
    set lines {}
    foreach hit $thread {
        set L [Ftoc_FindMsg $hit]
        if {$L == {}} {
            # found in recent scan and not in cache. Rescan and give a chance
            Exmh_Status "New messages in folder ... rescanning" warn
            Scan_Folder $folder 0       ;# sets displayDirty for cache update
            set L [Ftoc_FindMsg $hit]
        }
        if {$L == {}} {
            Exmh_Status "Error when rescanning. Lines don't match" purple
            return
        }
        lappend lines [$exwin(ftext) get $L.0 $L.end]
    }

#   Ftoc_RangeUnHighlight              ;# reset FTOC marks
    Msg_CheckPoint                     ;# save current MH state

    Msg_Reset $hits $folder            ;# reset message state
    set ftoc(folder) {}                ;# the new 'pseudo-folder'
    set ftoc(displayValid) 0           ;# don't cache this one

    $exwin(ftext) configure -state normal
    $exwin(ftext) delete 0.0 end
    foreach line $lines {
        $exwin(ftext) insert end "$line\n"
    }
    $exwin(ftext) configure -state disabled

    Ftoc_ShowUnseen $folder            ;# show unseen messages

    if {$curmsg != {}} {
        # single message
        set msg(id) $curmsg            ;# reset current message
        set ftoc(curLine) \
           [Ftoc_FindMsg $curmsg]      ;# don't bother Ftoc_ClearCurrent
        Buttons_Current 1              ;# reset button state
        Msg_ShowCurrent $show          ;# redisplay if necessary
    } else {
        # range
        Buttons_Current 0              ;# reset button state
        Buttons_Range                  ;#
        Ftoc_PickMsgs $msgs 0          ;# restore original selection
    }
    Exmh_Status "$hits references found" blue
}
