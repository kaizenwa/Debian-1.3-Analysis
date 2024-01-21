# =============================================================================
#
# File:		cb_bindings.tcl
# Project:	cb_tools
# Started:	?
# Changed:	10.07.95
# Author:       Christian Bolik (zzhibol@rrzn-user.uni-hannover.de)
#
# Description:	This file contains additional bindings for several 
#		widget classes.
#
# -----------------------------------------------------------------------------
#
# Sections:
#	==== Entry-Bindings
#
# =============================================================================

if ![info exists cb_tools(keys_bound)] {
set cb_tools(keys_bound) 1

# Make some bindings available even if tk_strictMotif is true:
#if $tk_strictMotif {}
# Buttons:
bind Button <Return> [bind Button <space>]
bind Checkbutton <Return> [bind Button <space>]
bind Radiobutton <Return> [bind Button <space>]

# Text:
bind Text <2> {
    %W scan mark %x %y
    set tkPriv(x) %x
    set tkPriv(y) %y
    set tkPriv(mouseMoved) 0
}
bind Text <B2-Motion> {
    if {(%x != $tkPriv(x)) || (%y != $tkPriv(y))} {
	set tkPriv(mouseMoved) 1
    }
    if $tkPriv(mouseMoved) {
	%W scan dragto %x %y
    }
}
bind Text <ButtonRelease-2> {
    if {!$tkPriv(mouseMoved)} {
	catch {
	    cb_Text_change_callback %W insert [selection get -displayof %W]
	}
	if {[info proc tkTextPaste] == ""} {
	    catch {
		%W insert @%x,%y [selection get -displayof %W]
	    }
	} else {
	    tkTextPaste %W %x %y
	}
	%W see insert
    }
}
bind Text <Control-k> {
    if [%W compare insert == {insert lineend}] {
	%W delete insert
    } else {
	%W delete insert {insert lineend}
    }
}

# Entry:
bind Entry <2> {
    %W scan mark %x
    set tkPriv(x) %x
    set tkPriv(y) %y
    set tkPriv(mouseMoved) 0
}
bind Entry <B2-Motion> {
    if {abs(%x-$tkPriv(x)) > 2} {
	set tkPriv(mouseMoved) 1
    }
    %W scan dragto %x
}
bind Entry <ButtonRelease-2> {
    if {!$tkPriv(mouseMoved)} {
	if {[info proc tkEntryPaste] == ""} {
	    catch {
		%W insert @%x [selection get -displayof %W]
	    }
	} else {
	    tkEntryPaste %W %x
	}
    }
}
bind Entry <Control-k> {
    %W delete insert end
}


# ==== Entry-Bindings =========================================================

#bind Entry <FocusIn> {
#    %W selection range 0 end
#}
#bind Entry <FocusOut> {
#    %W selection clear
#}
catch {bind Entry <braceleft> {%W insert insert \{}}
catch {bind Entry <braceright> {%W insert insert \}}}
catch {bind Entry <bracketleft> {%W insert insert \[}}
catch {bind Entry <bracketright> {%W insert insert \]}}
catch {bind Entry <backslash> {%W insert insert \\}}
catch {bind Entry <at> {%W insert insert @}}
catch {bind Entry <mu> {%W insert insert µ}}
catch {bind Entry <bar> {%W insert insert |}}
catch {bind Entry <asciitilde> {%W insert insert ~}}
catch {bind Entry <dead_tilde> {%W insert insert ~}}
catch {bind Entry <grave> {%W insert insert `}}
catch {bind Entry <dead_grave> {%W insert insert `}}
catch {bind Entry <dead_acute> {%W insert insert ´}}
catch {bind Entry <dead_circumflex> {%W insert insert ^}}

# ==== Text-Bindings ==========================================================

catch {bind Text <braceleft> {cb_Text_change_callback %W insert \{; \
	%W insert insert \{}}
catch {bind Text <braceright> {cb_Text_change_callback %W insert \}; \
	%W insert insert \}}}
catch {bind Text <bracketleft> {cb_Text_change_callback %W insert \[; \
	%W insert insert \[}}
catch {bind Text <bracketright> {cb_Text_change_callback %W insert \]; \
	%W insert insert \]}}
catch {bind Text <backslash> {cb_Text_change_callback %W insert \\; \
	%W insert insert \\}}
catch {bind Text <at> {cb_Text_change_callback %W insert @; \
	%W insert insert @}}
catch {bind Text <mu> {cb_Text_change_callback %W insert µ; \
	%W insert insert µ}}
catch {bind Text <bar> {cb_Text_change_callback %W insert |; \
	%W insert insert |}}
catch {bind Text <asciitilde> {cb_Text_change_callback %W insert ~; \
	%W insert insert ~}}
catch {bind Text <dead_tilde> {cb_Text_change_callback %W insert ~; \
	%W insert insert ~}}
catch {bind Text <grave> {cb_Text_change_callback %W insert `; \
	%W insert insert `}}
catch {bind Text <dead_grave> {cb_Text_change_callback %W insert `; \
	%W insert insert `}}
catch {bind Text <dead_acute> {cb_Text_change_callback %W insert ´; \
	%W insert insert ´}}
catch {bind Text <dead_circumflex> {cb_Text_change_callback %W insert ^; \
	%W insert insert ^}}

bind Text <Control-Left> {
    %W mark set insert "insert - 1 chars"
    %W mark set insert "insert wordstart"
    %W yview -pickplace insert
    cb_Text_extend_sel %W    
}
bind Text <Control-Right> {
    %W mark set insert "insert wordend"
    %W yview -pickplace insert
    cb_Text_extend_sel %W    
}
bind Text <Alt-d> {
    cb_Text_change_callback %W delete word
    set tmpsel {insert "insert wordend"}
    set cb_Text(cutbuffer,%W) [eval %W get $tmpsel]
    eval %W delete $tmpsel
    catch "selection clear %W"
}
bind Text <Control-space> {
    selection clear %W
    if [info exists cb_Text(selstart)] {
	if [%W compare cb_selstart == insert] {
	    %W mark unset cb_selstart
	    unset cb_Text(selstart)
	} else {
	    %W mark unset cb_selstart
	    %W mark set cb_selstart insert
	    set cb_Text(selstart) 1
	}
    } else {
	%W mark set cb_selstart insert
	set cb_Text(selstart) 1
    }
}
bind Text <Left> {+cb_Text_extend_sel %W}
bind Text <Right> {+cb_Text_extend_sel %W}
bind Text <Up> {+cb_Text_extend_sel %W}
bind Text <Down> {+cb_Text_extend_sel %W}
bind Text <Prior> {+cb_Text_extend_sel %W}
bind Text <Next> {+cb_Text_extend_sel %W}
bind Text <Home> {+cb_Text_extend_sel %W}
bind Text <End> {+cb_Text_extend_sel %W}
bind Text <Control-Home> {+cb_Text_extend_sel %W}
bind Text <Control-End> {+cb_Text_extend_sel %W}
bind Text <Button-1> {+if [info exists cb_Text(selstart)] {
    unset cb_Text(selstart)
    catch {%W mark unset cb_selstart}
}   }

bind Text <Control-k> {
    catch {
	clipboard clear -displayof %W
	clipboard append -displayof %W [%W get insert {insert lineend}]
    }
    if [%W compare insert == {insert lineend}] {
	cb_Text_change_callback %W delete char
	%W delete insert
    } else {
	cb_Text_change_callback %W delete lineend
	%W delete insert {insert lineend}
    }
}

bind Text <Key> "cb_Text_change_callback %W insert %A; [bind Text <Key>]"
bind Text <Control-d> "cb_Text_change_callback %W delete char; \
	[bind Text <Control-d>]"
bind Text <Control-h> "cb_Text_change_callback %W delete backchar; \
	[bind Text <Control-h>]"
bind Text <Control-i> "cb_Text_change_callback %W insert <tab>; \
	[bind Text <Control-i>]"
bind Text <Control-o> "cb_Text_change_callback %W; [bind Text <Control-o>]"
bind Text <Control-t> "cb_Text_change_callback %W; [bind Text <Control-t>]"
bind Text <Control-w> "cb_Text_change_callback %W; [bind Text <Control-w>]"
bind Text <BackSpace> "cb_Text_change_callback %W delete backchar; \
	[bind Text <BackSpace>]"
bind Text <Delete> "cb_Text_change_callback %W delete char; \
	[bind Text <Delete>]"
bind Text <F18> "cb_Text_change_callback %W; [bind Text <F18>]"
bind Text <F20> "cb_Text_change_callback %W; [bind Text <F20>]"
bind Text <Return> "cb_Text_change_callback %W insert return; \
	[bind Text <Return>]"
bind Text <Tab> "cb_Text_change_callback %W insert <tab>; \
	[bind Text <Tab>]"
bind Text <mu> "cb_Text_change_callback %W insert µ; [bind Text <mu>]"
bind Text <Meta-BackSpace> "cb_Text_change_callback %W delete backchar; \
	[bind Text <Meta-BackSpace>]"
bind Text <Meta-Delete> "cb_Text_change_callback %W delete char; \
	[bind Text <Meta-Delete>]"

#
# ==== Procs ==================================================================
#

#
# -----------------------------------------------------------------------------
#
# Proc:		cb_pointToType
# Args:		
# Returns: 	""
# Desc:		Adds bindings to toplevels, entries and texts to support
#		point-to-type focus model.
# Side-FX:	none
#

proc cb_pointToType {} {

    bind Toplevel <Any-Enter> "
	[bind Toplevel <Any-Enter>]
	focus %W
	"
    bind Text <Any-Enter> "
	[bind Text <Any-Enter>]
	focus %W
	"
    bind Entry <Any-Enter> "
	[bind Entry <Any-Enter>]
	focus %W
	"
    bind Toplevel <Any-Leave> "
	[bind Toplevel <Any-Leave>]
	if {\[focus\] == \"%W\"} {
	    focus none
	}
	"
    bind Text <Any-Leave> "
	[bind Text <Any-Leave>]
	if {\[focus\] == \"%W\"} {
	    focus none
	}
	"
    bind Entry <Any-Leave> "
	[bind Entry <Any-Leave>]
	if {\[focus\] == \"%W\"} {
	    focus none
	}
	"
}

#
# -----------------------------------------------------------------------------
#
# Proc:		cb_bindForCompletion
# Args:		w	entry widget name
#		ev	(opt.) event that triggers the completion (in <...>)
# Returns: 	""
# Desc:		This adds a binding to the entry widget $w that tries
#		to complete its contents to a filename.
# Side-FX:	none
#

proc cb_bindForCompletion {w {ev "<Tab>"}} {
    global cb_tools

    bind $w $ev {
	set tmp [%W get]*
	if {[set tmpspc [string first " " $tmp]] > -1} {
	    set tmp [string range $tmp [expr $tmpspc + 1] end]
	}
	set tmplist ""
	catch "set tmplist \[glob $tmp\]"
	if {[llength $tmplist] == 1} {
	    if [file isdirectory $tmplist] {
		set tmplist $tmplist/
	    }
	    %W delete [expr $tmpspc + 1] end
	    %W insert end $tmplist
	    %W xview end
	} else {
	    if [info exists cb_tools(bell_cmd)] {
		eval $cb_tools(bell_cmd)
	    } else {
		catch blt_bell
	    }
	}
	catch "unset tmp tmplist tmpspc"
	break
    }
}

# -----------------------------------------------------------------------------
#
# Proc:    cb_Text_extend_sel
# Desc:    Extends the sel tag on the given window.
# In:      w: text widget
# Out:     ""
# Side FX: none
#

proc cb_Text_extend_sel {w} {
    global cb_Text

    if [info exists cb_Text(selstart)] {
	$w tag remove sel 1.0 end
	if [$w compare cb_selstart < insert] {
	    $w tag add sel cb_selstart insert
	} else {
	    $w tag add sel insert cb_selstart
	}
    }
}

# -----------------------------------------------------------------------------
#
# Proc:    cb_Text_change_callback
# Desc:    Invokes a callback when the text widget's contents has been changed.
# In:      w: text widget's name
# Out:     ""
# Side FX: none
#

proc cb_Text_change_callback {w {type unknown} {data x}} {
    global cb_Text

    if [info exists cb_Text(selstart)] {
	unset cb_Text(selstart)
	catch {$w mark unset cb_selstart}
    }
    if {$data != ""} {
	if [info exists cb_Text(change_callback,$w)] {
	    eval $cb_Text(change_callback,$w) $type [list $data]
	}
    }
}

}