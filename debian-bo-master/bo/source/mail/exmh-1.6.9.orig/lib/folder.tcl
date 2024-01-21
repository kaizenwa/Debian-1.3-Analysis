# 
# folder.tcl
#
# Folder operations, minus scan & inc.
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Folder_Init {} {
    global exmh argc argv mhProfile
    set exmh(target) {}		;# Name of target, for refile
    if {$argc > 0 && \
	[file isdirectory $mhProfile(path)/[lindex $argv 0]]} then {
	#scan named folder
	set exmh(folder) $argv
    } else {
	if [catch {exec folder -fast < /dev/null} f] {
	    set exmh(folder) {}
	} else {
	    set exmh(folder) $f
	}
    }
}

proc Folder_Summary { folder } {
    global mhProfile env
    if [catch {pwd} cwd] {
	Exmh_Status $cwd
	cd
	set cwd [pwd]
    }
    if [catch {cd $mhProfile(path)/$folder}] {
	catch {cd $cwd}
	return "${folder}+ does not exist"
    }
    set low 100000
    set high 0
    set num 0
    if {[catch {glob *} files] == 0} {
	foreach f $files {
	    if {[regexp {^[0-9]+$} $f]} {
		if {$f < $low} {
		    set low $f
		}
		if {$f > $high} {
		    set high $f
		}
		incr num
	    }
	}
    }
    catch {cd $cwd}
    if {$num <= 0} {
	return "${folder}+ has no messages"
    } else {
	return "${folder}+ $num msgs ($low-$high)"
    }
}

proc Folder_Change {f {msgShowProc Msg_ShowCurrent}} {
#    LogStart "Folder_Change $f"
    Exmh_Debug ****************
    Exmh_Debug Folder_Change $f [time [list  FolderChange $f $msgShowProc]]
}
proc FolderChange {f msgShowProc} {
    global exmh mhProfile ftoc
    if {[string compare [wm state .] normal] != 0} {
	if {$exmh(iconic)} {
	    # Ignore once if starting up with -iconic flag
	    set exmh(iconic) 0
	} else {
	    wm deiconify .
	}
    }
    if {[Ftoc_Changes "Change folder"] > 0} {
	# Need to reselect previous button here
	return
    }
    # Trim off leading mail path
    if [regsub ^$mhProfile(path)/ $f {} newf] {
	set f $newf
    }
    if {[string length $f] == 0} {
	return
    }
    if ![file isdirectory $mhProfile(path)/$f] {
	Exmh_Status "Folder $f doesn't exist" purple
	return
    }
    set oldFolder $exmh(folder)
    Exmh_Status "Changing to $f ..."
    if {$f != $exmh(folder)} {
	Exmh_Debug Exmh_CheckPoint [time Exmh_CheckPoint]
	global mhProfile
	set summary [Mh_Folder $f]	;# Set MH folder state
    } else {
	set summary {}
    }
    global folderHook
    if [info exists folderHook(leave,$oldFolder)] {
	$folderHook(leave,$oldFolder) $oldFolder leave
    }
    Label_Folder $f $summary
    Fdisp_HighlightCur $f
    Flist_Visited $f
    set exmh(folder) $f
    if {$ftoc(autoSort)} {
	if [Flist_NumUnseen $f] {
	    Ftoc_Sort
	}
    }
    Scan_Folder $f 1
    Exmh_Status $f
    # Either Msg_ShowCurrent or Msg_ShowUnseen
    eval $msgShowProc

    # Take any required folder-specific action (e.g., for drafts folder)
    if [info exists folderHook(enter,$f)] {
	$folderHook(enter,$f) $f enter
    }
    foreach cmd [info commands Hook_FolderChange*] {
	$cmd $f
    }
}

proc Folder_Unseen {} {
    Ftoc_NextFolder
#    Folder_Change [Flist_NextUnseen]
}

proc Folder_Target {f} {
    global exmh mhProfile

    if ![file isdirectory $mhProfile(path)/$f] {
	Exmh_Status "$mhProfile(path)/$f doesn't exist"
	return
    }
    if {$exmh(folder) == $f} {
	Exmh_Status "Target must be different than current" red
	return
    }
    Fdisp_HighlightTarget $f
    set exmh(target) $f
    Exmh_Status "$f is target for moves"
}
proc Folder_TargetMove { f {moveProc Ftoc_MoveMark} } {
    Folder_Target $f
    Msg_Move $moveProc
}

proc Folder_TargetCopy { f {copyProc Ftoc_CopyMark} } {
    Folder_Target $f
    Msg_Move $copyProc advance?
}

proc Folder_TargetClear {f} {
    global exmh

    Fdisp_HighlightTarget ""
    set exmh(target) ""
    Exmh_Status "No target set for moves"
}


proc Folder_Sort { args } {
    global exmh

    if {[Ftoc_Changes "Sort"] == 0} then {
	Background_Wait
	Exmh_Status "Sorting folder..." blue
	eval {Mh_Sort $exmh(folder)} $args
	Scan_FolderForce
	set id [Mh_Cur $exmh(folder)]
	if {$id != {}} {
	    Msg_Change $id
	} else {
	    Msg_ClearCurrent
	}
    }
}

proc Folder_Pack {} {
    global exmh

    if {[Ftoc_Changes "Pack"] == 0} then {
	Background_Wait
	Exmh_Status "Packing folder..." blue
	Mh_Pack $exmh(folder)
	Scan_FolderForce
	set id [Mh_Cur $exmh(folder)]
	if {$id != {}} {
	    Msg_Change $id
	} else {
	    Msg_ClearCurrent
	}
    }
}
proc Folder_Commit { {rmmCommit Mh_Rmm} {moveCommit Mh_Refile} {copyCommit Mh_Copy} } {
    busy FolderCommit $rmmCommit $moveCommit $copyCommit
    return 0
}
proc FolderCommit { rmmCommit moveCommit copyCommit } {
    global exmh exwin ftoc

    Msg_CheckPoint	;# Update sequence state
    Ftoc_Commit $rmmCommit $moveCommit $copyCommit
    Exmh_Debug Scan_CacheUpdate [time Scan_CacheUpdate]

    if $ftoc(autoPack) {
	Background_Wait	;# Let folder ops complete
        Folder_Pack	;# Before packing
    }
    Label_Folder $exmh(folder)
}
# Streamlined Commit called before Folder_Change
proc Folder_CommitType { type } {
    global ftoc
    Exmh_Debug Folder_CommitType $type
    if {[string compare $type "Change folder"] == 0} {
	Msg_CheckPoint
	busy Ftoc_Commit Mh_Rmm Mh_Refile Mh_Copy
	if $ftoc(autoPack) {
	    Background_Wait	;# Let folder ops complete
	    BgAction Pack Folder_Pack	;# Before packing
	}
    } else {
	Folder_Commit
    }
}

proc Folder_Purge { {folder {}} } {
    global exmh
    if {[string length $folder] == 0} {
	set folder $exmh(folder)
    }
    set uid 0
    while {[file exists [set fn /tmp/exmh.[pid].touch.$uid]]} {
	incr uid
    }
    exec touch $fn
    set now [file mtime $fn]
    exec rm $fn

    global mhProfile
    if ![info exists mhProfile(delprefix)] {
	set mhProfile(delprefix) #
    }
    if ![info exists mhProfile(purgeage)] {
	set mhProfile(purgeage) 7
    }
    set purgesecs [expr $mhProfile(purgeage) * 24 * 60 * 60]
    set n 0
    foreach f [glob -nocomplain $mhProfile(path)/$folder/$mhProfile(delprefix)*] {
	if {[file mtime $f] + $purgesecs < $now} {
	    Exmh_Debug Purge $f
	    exec rm $f
	    incr n
	}
    }
    if {$n > 0} {
	Exmh_Status "Folder_Purge $folder $n msgs purged"
    }
    return $n
}

proc Folder_PurgeAll {} {
    global flist
    set n 0
    foreach f $flist(allfolders) {
	incr n [Folder_Purge $f]
    }
    if {$n > 0} {
	Exmh_Status "Folder_PurgeAll $n msgs purged total"
    }
}

proc Folder_PurgeBg { {folderlist {}} } {
    global exmh mhProfile wish
    if {[string length $folderlist] == 0} {
	set folderlist $exmh(folder)
    }
    set uid 0
    while {[file exists [set fn [Env_Tmp]/exmh.[pid].purge.$uid]]} {
	incr uid
    }
    set out [open $fn w]
    puts $out "wm withdraw ."
    puts $out "source $exmh(library)/folder.tcl"
    puts $out [list set mhProfile(delprefix) $mhProfile(delprefix)]
    puts $out [list set mhProfile(purgeage) $mhProfile(purgeage)]
    puts $out [list set mhProfile(path) $mhProfile(path)]
    puts $out "proc Exmh_Status { s } \{catch \{send \"[winfo name .]\" \[list Exmh_Status \$s]\}\}"
    puts $out "proc Exmh_Debug { args } {}"
    foreach folder $folderlist {
	puts $out "Folder_Purge $folder"
    }
    puts $out "exec rm $fn"
    puts $out exit
    close $out
    exec $wish -f $fn &
}
proc Folder_PurgeAllBg {} {
    global flist
    Folder_PurgeBg $flist(allfolders)
}
