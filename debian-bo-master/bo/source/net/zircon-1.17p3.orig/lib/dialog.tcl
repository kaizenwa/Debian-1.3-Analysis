#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/dialog.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
proc doButtons {w param lv pars} {
    if {[llength $pars] > 0} {
	set arg [lindex $pars 0]
	frame $w.bot.0 -borderwidth 1 -relief raised
	pack $w.bot.0 -side left -expand 1 -fill x -padx 5 -padx 5
	if ![string match {} [set cmd [lindex $arg 1]]] {append cmd " $param"}
	if ![string match {} $lv] {
	   bind $lv <Return> "$cmd ; killWindow $w ; notIdle {} ; break"
	   bind $lv <Tab> "focus $w.top.v0.value ; notIdle {} ; break"
	}
	button $w.bot.0.button -text [lindex $arg 0] -width 7 \
		-command "$cmd ; killWindow $w ; notIdle {}"
	pack $w.bot.0.button -expand 1 -fill x -padx 5 -pady 5
	bind $w <Return> "$cmd ; killWindow %W ; notIdle {} ; break"
	set i 1
	foreach arg [lrange $pars 1 end] {
	    if {[set cmd [lindex $arg 1]] != {}} { append cmd " $param" }
	    button $w.bot.$i -text [lindex $arg 0] -width 7 \
	      -command "$cmd ; killWindow $w ; notIdle {}"
	    pack $w.bot.$i -side left -expand 1 -fill x -padx 5 -pady 5
	    incr i
	}
    }
    if [string match {} $lv] { set sf $w } { set sf $w.top.v0.value }
    bind $w <Any-Enter> "focus $sf ; notIdle {} ; break"
    focus $sf
}
#
proc doEntries {w entries arg} {
    set param {}
    set vb 0
    set lv {}
    foreach entry $entries {
	set f [frame $w.top.v$vb]
	set lv $f.value
	set lb $f.label
	if [string compare [llength $entry] 3] {
	    label $lb -text [lindex $entry 0]
	} {
	    menubutton $lb -text [lindex $entry 0] \
	      -menu $lb.menu
	    menu $lb.menu -tearoff 0
	    foreach x [lindex $entry 2] {
		$lb.menu add command -label $x -command "entrySet $lv $x"
	    }
	}
	emacsEntry $lv
	if ![string match {} [set init [lindex $entry 1]]] {
	    $lv insert end $init
	}
	append param " \[$lv get\]"
	pack $lb -side left -padx 5 -pady 5
	pack $lv -side left -expand 1 -fill x -padx 10 -pady 10
	pack $f -fill x -padx 5 -pady 5
	incr vb
	bind $lv <Return> "notIdle {} ; focus $w.top.v$vb.value ; break "
	bind $lv <Tab> "notIdle {} ; focus $w.top.v$vb.value ; break"
    }
    doButtons $w $param $lv $arg
}
#
proc mkDialog {kind w title msgText entries args} {
    if [string match {} $args] {
	global ztrans
	set args [list [list $ztrans(dismiss) {}]]
    }
    if ![string match {} $kind] {
	global current
	set net $current(net)
	if {[lsearch [$net noConfirm] $kind] >= 0} {
	    set param {}
	    foreach entry $entries { append param "{[lindex $entry 1]}" }
	    if {[llength $args] > 0 && \
	      [set cmd [lindex [lindex $args 0] 1]] != {}} {
		append cmd " $param"
	    }
	    eval $cmd
	    return
	}
	if {[lsearch [$net toInfo] $kind] >= 0} {
	    $net inform $msgText
	    return
	}
    }
    set just [expr {[lsearch \
      {DCC WHO STATS INFO LINKS WHOIS} $kind] >= 0 ? "left" : "center"}]
    if [string match {} $w] {set w .@[newName dlg]} {killWindow $w}
    toplevel $w -class Zircon
    wm title $w "$title"
    pack [frame $w.top -relief raised] -fill both -expand 1
    pack [frame $w.bot -relief raised] -fill x
    if ![string compare left $just] {
	scrollbar $w.top.vscroller -command "$w.top.message yview"
	text $w.top.message -yscrollcommand "$w.top.vscroller set"
	bindtags $w.top.message ROText
	$w.top.message insert insert $msgText
	set ln [lindex [split [$w.top.message index end] .] 0]
	$w.top.message conf -height $ln
	pack $w.top.message -side left -expand 1 -fill both
	pack $w.top.vscroller -side left -fill y
    } {
	message $w.top.message -justify $just -text $msgText -aspect 800
	pack $w.top.message -expand 1 -fill both
    }
    doEntries $w $entries $args
    catch {tkwait visibility $w ; grab $w}
    return $w
}
#
proc mkEntryBox {w title msgText entries args} {
    if [string match {} $args] {
	global ztrans
	set args [list [list $ztrans(dismiss) {}]]
    }
    if [string match {} $w] { set w .@[newName dlg] } {killWindow $w}
    toplevel $w -class Zircon
    wm title $w $title
    wm protocol $w WM_DELETE_WINDOW {}
    pack [frame $w.top -relief raised] -fill both -expand 1
    pack [frame $w.bot -relief raised] -fill x
    pack [message $w.top.message -text $msgText -aspect 800] -expand 1 \
      -fill both
    doEntries $w $entries $args
    return $w
}
#
proc mkRadioBox {w title msgText flags dflag entries args} {
    global RFlags
    killWindow $w
    toplevel $w -class Zircon
    wm title $w "$title"
    wm protocol $w WM_DELETE_WINDOW {}
    frame $w.top -relief raised
    frame $w.bot -relief raised
    pack $w.top -fill both -expand 1
    pack $w.bot -fill x
    message $w.top.message -text $msgText -aspect 800
    pack $w.top.message -expand 1 -fill both

    set param {}
    set vb 0
    set lv {}
    foreach entry $entries {
	set f [frame $w.top.v$vb]
	set lv $f.value
	label $f.label -text [lindex $entry 0]
	emacsEntry $lv
	if {[set init [lindex $entry 1]] != {}} { $lv insert end $init }
	append param " \[$lv get\]"
	pack $f.label -side left -padx 5 -pady 5
	pack $lv -side left -expand 1 -fill x  -padx 10 -pady 10
	pack $f -fill x -padx 5 -pady 5
	incr vb
	bind $lv <Return> "notIdle {} ; focus $w.top.v$vb.value ; break"
	bind $lv <Tab> "notIdle {} ; focus $w.top.v$vb.value ; break"
    }
    set f [frame $w.top.flg]
    foreach entry $flags {
	radiobutton $f.rb$entry -text $entry -value $entry -variable RFlags($w)
	pack $f.rb$entry -side left
    }
    set RFlags($w) $dflag
    pack $w.top.flg -fill x -expand 1 -padx 5 -pady 5
    append param " RFlags $w"
    doButtons $w $param $lv $args
    return $w
}
#
proc mkInfoBox {kind w title msgText args} {
    if [string match {} $args] {
	global ztrans
	set args [list [list $ztrans(dismiss) {}]]
    }
    if ![string match {} $kind] {
	global current
	set net $current(net)
	if {[lsearch [$net noConfirm] $kind] >= 0} {
	    if {[llength $args] > 0} {eval [lindex [lindex $args 0] 1] }
	    return [[$net info] text]
	}
	if {[lsearch [$net toInfo] $kind] >= 0} {
	    $net inform $msgText
	    return [[$net info] text]
	}
    }
    set just [expr {[lsearch \
      {DCC WHO STATS INFO LINKS WHOIS} $kind] >= 0 ? {left} : {center}}]

    killWindow $w
    toplevel $w -class Zircon -borderwidth 2
    wm title $w $title
    wm protocol $w WM_DELETE_WINDOW {}
    frame $w.top -borderwidth 0
    bind $w.top <Destroy> break
    frame $w.bot -borderwidth 0
    if ![string compare $just left] {
        frame $w.top.hs -borderwidth 0
	frame $w.top.vs -borderwidth 0
	scrollbar $w.top.vs.vscroller -command "$w.top.message yview"
	scrollbar $w.top.hs.hscroller -command "$w.top.message xview" \
	  -orient horizontal
	text $w.top.message -width 80 -height 10 -wrap none \
	  -xscrollcommand "hsSet $w.top" \
	  -yscrollcommand "bsSet $w.top.vs.vscroller"
	bindtags $w.top.message ROText
	if ![string match {} $msgText] {
	    $w.top.message insert insert $msgText
	    set ln [lindex [split [$w.top.message index end] .] 0]
	    set ln [expr $ln > 24 ? 24 : $ln]
	    $w.top.message conf -height $ln
	}
	pack $w.top.hs.hscroller -side left -fill x -expand 1
	frame $w.top.hs.pd -borderwidth 0
	pack $w.top.message -side left -expand 1 -fill both -in $w.top.vs
	pack $w.top.vs -fill both -expand 1
    } {
	message $w.top.message -justify $just -text $msgText -aspect 800
	pack $w.top.message -expand 1 -fill both
    }
    pack $w.bot -fill x -side bottom
    pack $w.top -fill both -expand 1 -side top
    doButtons $w {} {} $args
    return $w.top.message
}
#
proc hsSet {sb f l} {
    if {$f == 0 && $l == 1} {
	catch {pack forget $sb.hs}
    } {
	catch "pack $sb.hs -fill x"
	$sb.hs.hscroller set $f $l
	if ![catch {pack info $sb.vs.vscroller}] {
	    catch "pack $sb.hs.pd -side right -padx 10"
	}
    }
}
#
proc setFile {w y cmd init} {
    set x [$w nearest $y]
    set fn [$w get $x]
    checkFile $w $fn $cmd $init
}
#
proc checkFile {w fn cmd init} {
    if [file isdirectory $fn] {
	global FBFilter
	cd $fn
	set win [winfo toplevel $w]
	entrySet $win.mid.fn.entry [pwd]/$init
	$w delete 1 end
	if ![catch {set fls [glob *]}] {
	    foreach fl [lsort $fls] {
		if [file isdirectory $fl] { append fl / } \
		elseif ![regexp -- $FBFilter($win) $fl] { continue }
		$w insert end $fl
	    }
	}
    } {
	eval $cmd $fn
	killFWindow [winfo toplevel $w]
	notIdle {}
    }
}
#
proc mkFileBox {w filter title msgText init args} {
    if [string match {} $w] {set w .@[newName flb]} {killFWindow $w}
    global FBFilter
    set FBFilter($w) $filter
    toplevel $w -class Zircon
    wm title $w "$title"
    wm protocol $w WM_DELETE_WINDOW "catch {unset FBFilter($w)}"
    wm resizable $w 1 1

    frame $w.top -relief raised
    frame $w.mid -borderwidth 0
    frame $w.bot -relief raised
    pack $w.top $w.mid -fill both -expand 1
    pack $w.bot -fill x -side bottom
    if ![string match {} $msgText] {
	message $w.top.message -text $msgText -aspect 800
	pack $w.top.message -expand 1 -fill both
    }
    makeLB $w.mid.flist -setgrid 1
    pack $w.mid.flist -expand 1 -fill both
    set arg [lindex $args 0]
    set cmd [lindex $arg 1]
    labelEntry 0 $w.mid.filter {-text Filter} {.*} "setFilter %W {$init} ; break"
    labelEntry 0 $w.mid.fn {-text Filename} $init \
      "checkFile $w.mid.flist.l \[%W get\] {$cmd} {$init} ; killFWindow $w ; break"
    pack $w.mid.filter $w.mid.fn -expand 1 -fill x
    $w.mid.flist.l insert end ../
    if ![catch {set fls [glob *]}] {
	foreach fl [lsort $fls] {
	    if [file isdirectory $fl] { append fl / }
	    $w.mid.flist.l insert end $fl
	}
    }
    bind $w.mid.flist.l <1> "
	set x \[%W nearest %y\]
	set f \[%W get \$x\]
	entrySet $w.mid.fn.entry \
	 \[expr {\[file isdirectory \$f\] ? {} : \$f }\]
	%W selection anchor \$x
	break
    "
    bind $w.mid.flist.l <Double-1> "setFile %W %y {$cmd} {$init} ; break"
    if ![string match {} $args] {
	frame $w.bot.0 -relief raised -border 1
	pack $w.bot.0 -side left -expand 1 -fill x -padx 5 -pady 5
	set cmd [lindex $arg 1]
	if {$cmd != {}} { append cmd " \[$w.mid.fn.entry get\]" }
	button $w.bot.0.button -text [lindex $arg 0] \
		-command "$cmd ; killFWindow $w ; notIdle {}"
	pack $w.bot.0.button -expand 1 -fill x -padx 5 -pady 5
	bind $w <Return> "$cmd ; killFWindow %W ; notIdle {} ; break"
	focus $w
	set i 1
	foreach arg [lrange $args 1 end] {
	    set cmd [lindex $arg 1]
	    if ![string match {} $cmd] {
		append cmd " \[$w.mid.fn.entry get\]"
	    }
	    button $w.bot.$i -text [lindex $arg 0] \
	      -command "$cmd ; killFWindow $w ; notIdle {}"
	    pack $w.bot.$i -side left -expand 1 -fill x
	    incr i
	}
    }
    bind $w <Any-Enter> {focus %W ; notIdle {} ; break}
    return $w
}
#
proc setFilter {win init} {
   global FBFilter
   set w [winfo toplevel $win]
   set FBFilter($w) [$win get]
   checkFile $w.mid.flist.l {} {} $init
}
#
proc killFWindow {w} {
    global FBFilter
    catch {unset FBFilter($w)}
    killWindow $w
}
