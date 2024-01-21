#
# editor.tcl --
#	Editor interactions
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

# Message composing and editor interaction

proc Edit_Init {} {
    global mhProfile editor
    case $mhProfile(editor) {
	{*mxedit* sedit} {set prog $mhProfile(editor)}
	default {set prog sedit}
    }
    Preferences_Add "Editor Support" \
"Exmh has a built-in editor for composing messages, named \"sedit\".
It can also interface to other editors.  For non-TK editors like vi
or emacs, preface the editor command with \"exmh-async\" so the editor
runs as a detached process.  For TK-aware applications, suffix the
command with an & so your editor detaches into the background.  In
this case the first argument to the editor is the name of the exmh
interpreter, and the second argument is the draft file name.
If your editor knows how to post a message directly, preface
the command with \"give-away\".  The emacsclient and gnuclient
cases listed in the examples below are special cases of this.
If you want to pass a -geo argument to your program, you need to
wrap it up to prevent exmh-async from grabbing it.  See the last
example below.
Example commands include:
sedit
mxedit
emacsclient &
give-away emacsclient
gnuclient &
give-away gnuclient
exmh-async emacs
exmh-async emacsclient
exmh-async xterm -e vi
exmh-async xterm {-geom 80x44+0+0} -e vi" \
    [list \
	[list editor(prog) editCmd $prog {Editor command} \
"The editor command used to compose mail messages.
The filename will be appended to the command."] \
	[list editor(alternate) edit2Cmd {exmh-async emacs} {2nd Editor command} \
"This is an alternate editor command.  For example, if your default
editor is sedit, you might want to drop into emacs for a particular
message.  The filename will be appended to the command."] \
	[list editor(sedit!) seditOverride OFF {Sedit override} \
"Use this to override the regular editor command and
use the built-in editor instead.  This parameter is set
initially by the command line -sedit switch."] \
	[list editor(async_sedit) seditAfterAsync OFF {Use Sedit after Async} \
"If enabled, after editing with external editor, exmh will bring the message
up in sedit.  This is so you can initially create the message with your
favorite editor, and then pop into to sedit for richtext or attachments."] \
    [list editor(spell) spellCmd "exmh-async xterm -e ispell" {Spell command} \
"The spell command specifies a program used to spell-check mail messages. 
The filename will be appended to the command."] \
        [list editor(mhn) mhnCmd "mhn" {MHN command} \
"The mhn command specifies a program used to reformat
a message with the MH mhn program.
The filename will be appended to the command."] \
    ]
    set editor(sedit) sedit
}

proc Edit_Draft {} {
    global mhProfile exmh
    # Run the editor on the current draft
    if {$exmh(folder) == $mhProfile(draft-folder)} {
	Msg_CheckPoint	;# Update cur message
    }
    set draftID [Mh_Cur $mhProfile(draft-folder)]
    if {$draftID == {}} {
	Exmh_Status "No current message in Draft-Folder $mhProfile(draft-folder)" error
	return
    }
    EditWhatNow $draftID prog
}
proc Edit_DraftID { id } {
    # No mucking with context
    EditWhatNow $id prog
}

proc EditDraftFolderHook { f what } {
    # Called when we change to the drafts folder
    Buttons_DraftMode 1
}
proc EditDraftFolderCleanup { f what } {
    Buttons_DraftMode 0
}

# Throw up a dialog asking whether user wants to
# send, re-edit, save as draft, abort.
# As with the command line whatnowproc, this also starts up the
# editor on the draft.

proc EditWhatNow {draftID {edittype prog}} {
    global mhProfile editor

    if {([string compare $edittype prog] == 0) && $editor(sedit!)} {
	set edittype sedit	;# sedit override
    }
    Exmh_Debug EditWhatNow $draftID $edittype
    if ![regexp {^[0-9]+$} $draftID] {
	# Must be a message in a non-drafts folder
	set path $mhProfile(path)/$draftID
    } else {
	# Delete ".orig" file if it exists, Mhn_* thanks to Colm Buckley
	Mhn_DeleteOrig $draftID
	set path [Mh_Path $mhProfile(draft-folder) $draftID]
    }
    if [EditStart $path $edittype] {
	# Editor has run synchronously, so we go right into the dialog
	Edit_Dialog $draftID
    }
    global exmh
    if {[string compare $exmh(folder) $mhProfile(draft-folder)] == 0} {
	# It isn't safe to leave a current message active in the drafts
	# folder because a second edit can loose changes in the first.
	# User saves a draft.
	# User changes to draft folder.
	# The "Send" button uses the saved draft, and they make edits.
	# Another "Send" button hit resets the draft to their last save,
	# but they probably expected to start a new draft.
	Msg_ClearCurrent
    }
}

# The following can be invoked by remote editor interpreters
proc Edit_Dialog {draftID} {
    global exmh mhProfile editor
    Exmh_Debug Edit_Dialog $draftID
    if {$editor(async_sedit)} {
	Sedit_Start $mhProfile(path)/$mhProfile(draft-folder)/$draftID
    } else {
	EditShowDialog $draftID "What should I do with draft $draftID?"
    }
}
# For compatibility with old versions of mxedit
proc EditDialog {draftID} {
    Edit_Dialog $draftID
}
proc EditShowDialog {id text} {
    global exwin editor
    # Create the buttons and arrange them from left to right in the RH
    # frame. Embed the left button in an additional sunken frame to indicate
    # that it is the default button.

    if [Exwin_Toplevel .edit$id "What Now?" WhatNow nomenu] {
	set d .edit$id
	wm transient $d
	$d config -relief raised -borderwidth 2

	foreach but [Widget_GetButDef $d] {
	    Widget_AddButDef $d $but
	    Widget_ReEvalCmd $d.$but	;# expand $id variable now
	}
	catch {pack $d.abort -padx 15}
	catch {pack $d.send -ipady 5 -ipadx 5 -padx 5}

	foreach M [Widget_GetMenuBDef $d] {
	    set menu [Widget_AddMenuBDef $d $M {right padx 1}]
	    ButtonMenuInner $menu	;# This also expands variables
	}

	Widget_Message $d msg -text "$text\nReturn for Send\nControl-c for Kill" -aspect 300

	focus $d
	bind $d <Return> [list $d.send invoke]
	bind $d <Control-c> [list $d.abort invoke]

	tkwait visibility .edit$id
	Exmh_Focus
    } else {
	set d .edit$id
        catch {destroy $d.f}	;# Whom results
    }
}
proc EditDialogMsg {id msg} {
    global tk_version
    set d .edit$id
    catch {destroy $d.f}
    Widget_Frame $d f
    if {$tk_version >= 3.3} {
	pack $d.f -before [lindex [pack slaves $d] 0] -side bottom
    } else {
	pack before [lindex [pack info $d] 0] $d.f {bottom}
    }
    set lines [llength [split $msg \n]]
    Widget_Text $d.f [expr {$lines > 8 ? 8 : $lines}]
    $d.f.t insert 1.0 $msg
    $d.f.t config -state disabled -width 40
}
proc EditDialogDone {act id {hide hide}} {
    if [string match hide $hide] {
	global tk_version
	if {$tk_version >= 4.0} {
	    Exwin_Dismiss .edit$id nosize
	} else {
	    # Avoid Tk 3.6 bug that reuses window IDs too soon
	    after 10 [list Exwin_Dismiss .edit$id nosize]
	}
    }
    Edit_Done $act $id
}

proc EditStart { draft {type prog} } {
    # Start the editor, reusing an existing session if possible
    global editor exmh mhProfile

    Exmh_Debug EditStart $draft $type

    switch -glob -- $editor($type) {
	*mxedit* {
	    if ![info exists exmh(editInterp)] {
		set exmh(editInterp) "mxedit"
	    }
	    if ![info exists exmh(mxeditVersion)] {
		if ![catch {send $exmh(editInterp) {set mxVersion}} vers] {
		    set exmh(mxeditVersion) $vers
		}
	    }
	    set id "$exmh(editInterp) $draft"
	    if [info exists exmh(mxeditVersion)] {
		if {$exmh(mxeditVersion) >= 2.4} {
		    global env
		    if [regsub $env(HOME) $draft ~ newpath] {
			set id "$exmh(editInterp) $newpath"
		    }
		}
	    }
	    Exmh_Debug $id mxReset
	    if [catch {send $id mxReset}] {
		if [catch {send $exmh(editInterp) {set mxVersion}}] {
		    Exmh_Status "Starting mxedit..." warn
		    # Start the editor and tell it to make a callback
		    # that identifies the TCL interpreter in the editor
		    eval exec $editor($type) {-globalCmd [list mxSendInterpName [winfo name .] Edit_Ident] $draft &}
		} else {
		    Exmh_Status "Opening mxedit..." warn
		    catch {send $exmh(editInterp) [list mxOpen $draft]}
		}
	    } else {
		Exmh_Status "Reopening mxedit..." warn
		catch {send $id {wm deiconify .}}
	    }
	    return 0		;# Asynchronous edit
	}
	sedit {
	    Sedit_Start $draft
	    return 0		;# Asynchronous edit
	}
	exmh-async* {
	    global wish argv0
	    Exmh_Status "Starting ASYNC $editor($type) ..." warn
	    eval exec $wish -f ${argv0}-async \"[winfo name .]\" \
		[lrange $editor($type) 1 end] $draft &
	    return 0		;# Asynchronous edit
	}
	give-away* -
	gnuclient*& -
	emacsclient*& {
	    set cmd $editor($type)	;# Tcl 7.0 bug in regsub
	    regsub ^give-away $cmd {} cmd
            set cmd [string trimright $cmd "& \t"]
	    Exmh_Status "Starting $cmd ..." warn
            if [catch {eval exec $cmd $draft &} err] {
                Exmh_Status $err error
            }
	    return 0		;# Asynchronous edit

	}
	*& {
	    Exmh_Status "Starting TK $editor($type) ..." warn
            set cmd [string trimright $editor($type) "& \t"]
            if [catch {eval exec $cmd \"[winfo name .]\" $draft &} err] {
                Exmh_Status $err error
            }
	    return 0		;# Asynchronous edit

	}
	default {
	    Exmh_Status "Starting $editor($type) ..." warn
	    if [catch {eval exec $editor($type) $draft} err] {
		Exmh_Status $err error
	    }
	    return 1		;# Synchronous edit
	}
    }
}

# The following is invoked via "send" by mxedit when it
# starts up the first time in order to identify itself to us.

proc Edit_Ident { interpName } {
    global exmh
    set exmh(editInterp) $interpName
}

# The following is invoked by remote editor interpreters
proc EditDone {act msg} {
    Edit_Done $act $msg
}
proc Edit_Done {act {msg cur}} {
    # Commit or abort an edit session
    global mhProfile exmh env editor
    if {$msg == "cur"} {
	set msg [Mh_Cur $mhProfile(draft-folder)]
	if {$msg == {}} {
	    Exmh_Status "No current draft"
	    return
	}
    }
    Exmh_Debug action = $act msg = $msg
    if ![regexp {^[0-9]+$} $msg] {
	# Message not in the drafts folder
	set path $mhProfile(path)/$msg
    }
    case $act in {
	send	{
	    Aliases_CheckPoint
	    if [info exists path] {
		# Copy message to drafts folder
		set id [file tail [Mh_Path $mhProfile(draft-folder) new]]
		Exmh_Status "Copying $msg to draft $id"
		MhExec comp +[file dirname $msg] [file tail $msg] -nowhatnowproc
		set msg $id
	    }
	    set anno [Mh_AnnoEnviron $msg]
	    Exmh_Debug Edit_Done send: anno=$anno
	    if [catch {Mh_Send $msg} err2] {
		# move the orig message back to the draft if it exists
		Mhn_RenameOrig $msg
		Exmh_Status $err2 error
		Send_Error $err2 $msg
		return
	    }
	    if {$exmh(folder) == $mhProfile(draft-folder)} {
		# Clean up scan listing
		if [catch {Msg_RemoveById $msg} err] {
		    Exmh_Debug Msg_RemoveById $msg $err
		}
	    }
	    Exmh_Status "Draft $msg sent" normal
	    # Delete "orig" message if it exists
	    Mhn_DeleteOrig $msg
	    Mh_AtLinkCleanup
	    if {$anno} {
		if {[string compare $exmh($msg,folder) $exmh(folder)] == 0} {
		    set ix [Ftoc_FindMsg $exmh($msg,mhmessages)]
		    Exmh_Debug Edit_Done ix=$ix
		    if {$ix != {}} {
			Ftoc_RescanLine $ix dash
		    }
		}
	    }
	    Mh_AnnoCleanup $msg
	}
	reedit	{
	    Exmh_Status " "
            # Rename the orig message back to the draft
	    Mhn_RenameOrig $msg
	    EditWhatNow $msg prog
	}
	sedit	{
	    Exmh_Status " "
	    EditWhatNow $msg sedit
	}
	alternate {
	    Exmh_Status " "
	    EditWhatNow $msg alternate
	}
        spell {
            Exmh_Status " "
            EditWhatNow $msg spell
        }
        mhn {
	    # If the orig file exists, move it back to the draft
	    Mhn_RenameOrig $msg
	    if [info exists path] {
		set env(mhdraft) $path
	    } else {
		set env(mhdraft) [Mh_Path $mhProfile(draft-folder) $msg]
	    }
	    if [catch {exec $editor(mhn) $env(mhdraft)} err] {
		EditDialogMsg $msg "MHN failed: $err"
	    } else {
		EditDialogMsg $msg "MHN returned a-o.k."
	    }
        }
	abort	{
	    if ![info exists path]  {
		catch {Mh_Rmm $mhProfile(draft-folder) $msg}
		Mhn_DeleteOrig $msg
		if {$exmh(folder) == $mhProfile(draft-folder)} {
		    # Clean up scan listing
		    if [catch {Msg_RemoveById $msg} err] {
			Exmh_Debug Msg_RemoveById $msg $err
		    }
		}
		Exmh_Status "Draft $msg aborted" error
		Mh_AtLinkCleanup
		Mh_AnnoCleanup $msg
	    }
	}
	dismiss	{
	    Exmh_Status "Draft $msg dismissed" normal
	    Mh_AtLinkCleanup
	    Mhn_RenameOrig $msg
	    Mh_AnnoCleanup $msg
	}
	whom	{
	    Mh_AnnoEnviron $msg
	    if [info exists path] {
		catch {Mh_Whom $path} result
	    } else {
		catch {Mh_Whom $msg} result
	    }
	    EditDialogMsg $msg $result
	}
	default	{
	    Exmh_Error "Unknown action in Edit_Done"
	}
    }
}

