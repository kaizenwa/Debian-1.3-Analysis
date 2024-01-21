# labels.tcl
#
# Labels used for various things:
#	Main - the exmh version, or the name of a subfolder
#	Folder - the name of the current folder
#	Message - the id of the current message
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

# Main label line - for version number or subfolder name
proc Label_MainSetup { frame } {
    global label
    set label(main) [Widget_Label $frame label {left fill expand}]
}
proc Label_Main { {text {} } } {
    global exmh label
    if {$text == {}} {
	$label(main) configure -text $exmh(version)
    } else {
	$label(main) configure -text $text
    }
}

# Folder label for folder name
proc Label_FolderSetup { frame } {
    global label
    set label(folder) [Widget_Label $frame label {left fill expand}]
    catch {$label(folder) configure -width 35}
}
proc Label_Folder { folder {text {}} } {
    global label
    if {[string length $text] == 0} {
	set text [Folder_Summary $folder]
    }
    if [regsub {has[ 	]*no *messages} $text {is empty} newtext] {
	set text $newtext
    } else {
	set len [string length $text]
	if {$len > 35} {
	    set i [expr $len-35]
	    set text "[string range $text $i end]"
	}
    }
    $label(folder) configure -text $text
}
proc Label_FolderClear { folder } {
    global label
    $label(folder) configure -text $folder+
}
# Message label for message id
proc Label_MessageSetup { frame } {
    global label
    set label(message) [Widget_Label $frame label {left fill}]
    $label(message) configure -width 20 -anchor w
}
proc Label_Message { text } {
    global label
    set len [string length $text]
    if {$len > 20} {
	set i [expr $len-17]
	set text "...[string range $text $i end]"
    }
    $label(message) configure -text $text
}
