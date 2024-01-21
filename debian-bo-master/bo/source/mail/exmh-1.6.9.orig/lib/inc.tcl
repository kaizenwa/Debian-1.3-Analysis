# inc.tcl
#
# Incorporate new mail into folders.
# The routines here are prepared to be operating in a different
# interpreter than the main UI.  After they do the inc thing,
# they use BgRPC to invoke the UI-related routines in the
# correct interpreter.
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Inc_Init {} {
    global exmh inc
    if {$exmh(slocal) == {}} {
	set exmh(slocal) slocal
    }
    if [info exists exmh(incStyle)] {
	# Backward compatibility
	set inc(style) $exmh(incStyle)
    }
    if [info exists exmh(incOnStart)] {
	# Backward compatibility
	set inc(onStartup) $exmh(incOnStart)
    }
    Preferences_Add "Incorporate Mail" \
"Exmh knows several ways to incorporate mail from your system's spool file into the MH folder hierarchy." {
	{inc(style) incStyle	{CHOICE inbox presort multidrop presortmulti none} {Ways to Inc}
"inbox - basic MH inc from your spool file to inbox folder.
presort - slocal filtering directly into various folders.
multidrop - slocal filtering or POP delivery into various drop boxes,
as specified by ~/.xmhcheck, that are in turn inc'ed into folders.
presortmulti - presort + multidrop.
none - you have an external agent that handles inc for you, so
don't bother trying it from within exmh."}
	{inc(onStartup) incOnStartup OFF	  {Inc at startup}
"Run your flavor of inc when exmh starts up."}
	{inc(onMapping) incOnMapping OFF	  {Inc on mapping}
"Run your flavor of inc when exmh is opened
after being iconic."}
	{inc(presortFeedback) incSortFeedback ON	  {Presort Inc feedback}
"This option causes a message to be generated each time a
new message is sorted into a folder.  There isn't much info,
it's just sort of a heart-beat feature..."}
	{inc(xnsgetmail) xnsGetMail OFF	  {Run xnsgetmail -k}
"Run xnsgetmail -k before inc in order to fetch your
XNS mail messages into your UNIX spool file."}
    }
}
proc Inc_Startup {} {
    global inc
    if {! $inc(onStartup) || $inc(style) == "multidrop"} {
	Exmh_Status "Checking folders"
	Flist_FindUnseen
    }
    if {$inc(onStartup)} {
	set s [Sound_Off]
	Inc
	if {$s} { Sound_On }
    }
}
proc Inc_Mapped {} {
    global inc
    if {$inc(onMapping)} {
	Inc
    }
}

# Inc_Show is appropriate for the Inc button, but not for the
# background inc process.  It incs, changes to inbox, and shows
# the first unseen message in the folder.  (That may or may not
# be the first one just inc'ed.)

proc Inc_Show {} {
    global exmh

    Inc
    if { $exmh(folder) != "inbox" } {
	Folder_Change inbox
    }
    Msg_ShowUnseen
}

proc Inc {} {
    BgAction "Inc" IncInner	;# Execute in the background process.
}
proc IncInner {} {
    # There are three different styles of inc'ing messages...
    global inc
    if $inc(xnsgetmail) {
	Xns_GetMail
    }
    case $inc(style) {
	{default inbox}	{ busy Inc_Inbox }
	presort		{ busy Inc_Presort }
	multidrop	{ busy Inc_All }
	presortmulti	{ busy Inc_PresortMultidrop }
	none		{ return }
    }
}

proc Inc_PresortMultidrop {} {
    # Copy from drop boxes to folders.  Anything
    # that goes into MyIncTmp will get sorted later.
    global mhProfile
    if ![file isdirectory $mhProfile(path)/MyIncTmp] {
	exec mkdir $mhProfile(path)/MyIncTmp
    }
    # Inc, but don't fiddle with scan listings
    Inc_All 0

    # Now do the Inc Presort on MyIncTmp and update scan listings
    Inc_Presort
}

proc Inc_Inbox {} {
    # Inc from the spool file to the inbox folder
    global exmh mhProfile ftoc
    if [info exists mhProfile(inbox)] {
	set inbox [string trimleft $mhProfile(inbox) +]
    } else {
	set inbox inbox
    }
    Exmh_Status "Inc ..."
    if [catch {exec inc +$inbox -truncate -nochangecur -width $ftoc(scanWidth)} incout] {
	Exmh_Debug Inc_Inbox +${inbox}: $incout
	set incout {}
    }
    BgRPC Inc_InboxFinish $inbox $incout Flist_Done
}
proc Inc_InboxFinish { f incout {hook {}} } {
    global exmh
    set msgids {}
    Scan_Iterate $incout line {
	set id [Ftoc_MsgNumberRaw $line]
	if {$id != {}} {
	    lappend msgids $id
	}
    }
    Exmh_Debug Inc_InboxFinish $f $msgids hook=$hook
    if {[llength $msgids] == 0} {
	Exmh_Status "No new messages in $f"
	return
    }
    Flist_AddUnseen $f $msgids
    if {$hook != {}} {
	eval $hook
    }
    if {$exmh(folder) == $f} {
	Scan_Inc $f $incout
    } else {
	Exmh_Status "New messages in $f"
    }
}

proc Inc_Presort {} {
    # Transfer from the mail spool file directly into folders
    global exmh mhProfile env inc
    # Use inc to copy stuff into a temp directory
    if ![file isdirectory $mhProfile(path)/MyIncTmp] {
	exec mkdir $mhProfile(path)/MyIncTmp
    }
    if {[catch {exec inc +MyIncTmp -silent} err]} {
	# Some versions of inc exit non-zero when they should not.
	Exmh_Debug Inc_Presort +MyIncTmp: $err
    }
    if [catch {set env(USER)} user] {
	if [catch {set env(LOGNAME)} user] {
	    Exmh_Status "No USER or LOGNAME envar"
	    set user ""
	}
    }
    if {[catch {glob $mhProfile(path)/MyIncTmp/*} files] == 0} {
	Exmh_Status "presort inc ..."
	foreach file $files {
	    if {[string compare [file tail $file] "++"] == 0} {
		if [catch {exec rm $file} err] {
		    Exmh_Debug Inc_Presort: $err
		}
		continue
	    }
	    if {$inc(presortFeedback)} {
		if [catch {exec grep "^Subject:" $file | head -1} \
		    subject] {
		    set subject ""
		}
	    }
		
	    # now use slocal to (presumably) shift things to a folder
	    if [catch {exec $exmh(slocal) -user $user < $file} err] {
		# Move it out of MyIncTmp in case it really did
		# get filed somewhere.  Certain file system errors
		# (can't stat .) lead to this behavior
		Exmh_Status "$file - $err - check MyIncErrors"
		if ![file isdirectory $mhProfile(path)/MyIncErrors] {
		    exec mkdir $mhProfile(path)/MyIncErrors
		}
		Mh_Refile MyIncTmp [file tail $file] MyIncErrors 
	    } else {
		if [catch {exec rm $file} err] {
		    Exmh_Debug Inc_Presort: $err
		}
		if {$inc(presortFeedback)} {
		    if {[string length $subject] > 0} {
			regsub "^Subject: *" $subject "" subject
			Exmh_Status "[file tail $file]: $subject" red
		    } else {
			Exmh_Status [file tail $file] red
		    }
		}
	    }
	}
    }
    catch {exec rm -f $mhProfile(path)/MyIncTmp/$mhProfile(mh-sequences)}
    catch {exec rmdir $mhProfile(path)/MyIncTmp}

    Flist_FindUnseen		;# Needed to set new message state.
    # This after breaks a potential deadlock:
    # UI Inc button is pressed - registers outstanding Inc operation
    # PresortFinish notes new messages in the current folder and wants
    # to rescan the folder to pick up the changes.
    # The scan notes the outstanding operation and waits for it to complete.
    # Deadlock
    after 1 {BgRPC Inc_PresortFinish}
}
proc Inc_PresortFinish {} {
    global exmh ftoc
    Mh_Folder $exmh(folder)	;# prestort inc has changed this to MyIncTmp
    if {[Flist_NumUnseen $exmh(folder)] > 0} {
	Label_Folder $exmh(folder)
	Scan_Folder $exmh(folder) $ftoc(showNew)
    }
}

# The following are useful if you have lots of drop boxes
# because you use maildelivery to divert messages into dropboxes.
# This also works with POP hosts.

proc Inc_All {{updateScan 1}} {
    global exdrops exmh ftoc

    Exmh_Status "Checking dropboxes..." red
    MhSetMailDrops	;# Update, if needed

    set hits {}
    foreach record [array names exdrops] {
        set fields [ scan $exdrops($record) "%s %s %s" folder dropname popname]
        Exmh_Status "dropbox... $dropname" red
	if {[string first / $dropname] < 0} {
	    # Not a pathname, but a POP hostname
	    set host [lindex $dropname 0]
	    Exmh_Status "$popname @ $host"
	    if {$popname != ""} {
                set user $popname
		set cmd [list exec inc +$folder -host $host \
			-user $user -truncate -width $ftoc(scanWidth)]
	    } else {
		set cmd [list exec inc +$folder -host $host \
			-truncate -width $ftoc(scanWidth)]
	    }
	} else {
	    if { [file exists $dropname] && [file size $dropname] } {
		set cmd [list exec inc +$folder -file $dropname \
			-truncate -width $ftoc(scanWidth)]
	    } else {
		set cmd {}
	    }
	}
	if [llength $cmd] {
	    if {[catch $cmd incout] == 0} {
		lappend hits $folder
	        if $updateScan {
		    BgRPC Inc_InboxFinish $folder $incout
		}
	    } else {
		Exmh_Debug Inc_All +${folder}: $incout
	    }
	}
    }
    if {$updateScan && ([llength $hits] > 0)} {
	BgRPC Flist_Done
	Exmh_Status "New mail in: $hits" blue
    } else {
	Exmh_Status ""
    }
}
proc Inc_Pending {} {
    # Figure out which folders have stuff in their inbox
    global exdrops exwin
    set active {}
    foreach record [array names exdrops] {
        set fields [ scan $exdrops($record) "%s %s %s" folder dropfile popname]
	if {[file exists $dropfile] && [file size $dropfile]} {
	    lappend active $folder
	}
    }
    if [llength $active] {
	# Output the list of active folder
	Exmh_Status "Active: $active" blue
    } else {
	Exmh_Status "No active folders" red
    }
    Flist_FindUnseen
}
