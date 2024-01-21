#
# the name of the program to run should be a preference.  currently it
# is hardcoded to be my program.  yours will be called as `net.pl ja',
# where `ja' is the string for expansion.  it should output something
# of the form:
# Jack In A Box <jack@box.org>
# Big Jabberwocky <big@jab.com>
# this code doesn't really care when the result of the
# expansion is, it just replaces the original text with whatever you
# pick from the list.  if only one line is output from the match, then
# the substitution takes place without the list being presented.
# i've only used it for the case described above,
# although i guess in principal it could be used for mailing lists.
#
# probably the most trivial use of it would be to use `ali', and get
# your mh aliases expanded.  that's a bit painful at the moment because
# you have the type in the full name to get it expanded.
# set dbparse "ali"

# net.pl parses a BBDB database
set dbparse "/beta/davided/lib/net.pl"

proc ExpansionDialog { win text } {
    global dbparse

    set p [open "|$dbparse $text" r]
    set data [read $p]
    close $p
    if {![string compare $data ""]} {
	return
    }
    
    # is there just one line ?
    set l [expr [string length $data] - 1]
    if {[string first "\n" $data] == $l} {
	$win delete bow eow
	$win insert bow [string range $data 0 [expr $l - 1]]
	return
    }
    
    if [Exwin_Toplevel $win.exp "Name Expansion" Expansion] {
	$win.exp.but.quit config -command [list ExpansionDismiss $win]
	wm protocol $win.exp WM_DELETE_WINDOW [list ExpansionDismiss $win]
	Widget_Label $win.exp.but label {left fill} -text \
	    "Name Expansions"
	frame $win.exp.f
	listbox $win.exp.f.l -relief {sunken} \
	    -yscrollcommand "$win.exp.f.s set" -geometry "40x10"
	scrollbar $win.exp.f.s -relief {sunken} \
	    -command "$win.exp.f.l yview"

	bind $win.exp.f.l <Double-Button-1> [list ExpansionSelect $win]
	pack $win.exp.f $win.exp.f.s -side left -fill y
	pack $win.exp.f $win.exp.f.l -side top -expand 1 -fill both
    }
    
    $win.exp.f.l delete 0 end

    foreach line [split $data \n] {
	$win.exp.f.l insert end $line
    }
}

proc ExpansionSelect { win } {
    $win delete bow eow
    $win insert bow [$win.exp.f.l get [$win.exp.f.l curselection]]
    Exwin_Dismiss $win.exp
    focus $win
}

proc ExpansionDismiss { win } {
    Exwin_Dismiss $win.exp
    # this doesn't seem to work.  focus is not put back to the sedit
    # window.
    focus $win
}

