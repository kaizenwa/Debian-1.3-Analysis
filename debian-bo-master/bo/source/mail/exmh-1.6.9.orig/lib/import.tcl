# import.tcl
#
# Import folders from other mail tools.
#
# Copyright (c) 1994 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Import_Init {} {
}

proc Import_Dialog {} {
    global _import
    set w .import
    if [Exwin_Toplevel $w "Import Tool" Import nobuttons] {
	set f [Widget_Frame $w name Rim {top expand fill} -bd 10]
	set _import(win,msg) [Widget_Message $f msg -width 250]
	set f [Widget_Frame $f rim Rim {top expand fill} -bd 2 -relief raised]
	Widget_Label $f label {left} -text "Mail Directory:"
	set _import(win,entry) [Widget_Entry $f name {right fillx expand} -relief sunken]

	Widget_BindEntryCmd $_import(win,entry) <Return> [list ImportWhat $w]

	set f [Widget_Frame $w but Rim {top fill expand} -bd 10]
	Widget_AddBut $f cancel "Cancel" "destroy $w" left
	set _import(win,okbut) [Widget_AddBut $f ok "OK" [list ImportWhat $w] right]
    }
    catch {destroy .import.folders}
    $_import(win,msg) config -text \
"Import ucb-mailbox files into MH format.
Enter mail directory and click OK."
    focus $_import(win,entry)
}
proc ImportWhat { w } {
    global _import mhProfile
    set dir [string trim [$_import(win,entry) get]]
    if {[string length $dir] == 0} {
	Exmh_Status "Not a directory: $dir"
	$_import(win,msg) config -text \
"Please enter a mail directory and click OK."
	return
    }
    set dir [glob -nocomplain $dir]
    if ![file isdirectory $dir] {
	Exmh_Status "Not a directory: $dir"
	$_import(win,msg) config -text \
"Not a directory: $dir
Enter valid mail directory and click OK."
    } elseif {[string compare $dir $mhProfile(path)] == 0} {
	$_import(win,msg) config -text \
"$dir clashes with default MH setting.
You must quit exmh and change the Path:
setting in your ~/.mh_profile file to a new directory."
    } else {
	$_import(win,msg) config -text \
"(Un)Select folders to import and click Import.  You'll have to remove the old mail folders manually later."
	set folders {}
	set maxl 0
	global _importlist
	catch {unset _importlist}
	foreach f [glob -nocomplain $dir/*] {
	    if [file isfile $f] {
		set tail [file tail $f]
		lappend folders $tail
		if {[string length $tail] > $maxl} {
		    set maxl [string length $tail]
		}
	    }
	}
	set col 3
	set nframes [expr [llength $folders] / $col]
	for {set i 0} {$i <= $nframes} {incr i} {
	    set f [Widget_Frame $w but$i]
	    for {set j 0} {$j < $col} {incr j} {
		set next [lindex $folders 0]
		if {[string length $next] == 0} {
		    break
		}
		set folders [lreplace $folders 0 0]
		set b [Widget_CheckBut $f j$j $next _importlist($next) {right expand fill}]
		$b config -onvalue $dir/$next -offvalue {} -width $maxl -anchor w
		set _importlist($next) $dir/$next
	    }
	}

	$_import(win,okbut) config -text Import -command ImportIt
	set f [winfo parent $_import(win,okbut)]
	Widget_AddBut $f clear "Unselect All" [list ImportUnselectAll $f.clear]
    }
}
proc ImportUnselectAll {button} {
    global _importlist
    foreach f [array names _importlist] {
	set _importlist($f) {}
    }
    $button config -text "Select All" -command [list ImportSelectAll $button]
}
proc ImportSelectAll {button} {
    global _importlist _import
    set dir [glob -nocomplain [$_import(win,entry) get]]
    foreach f [array names _importlist] {
	set _importlist($f) $dir/$f
    }
    $button config -text "Unselect All" -command [list ImportUnselectAll $button]
}

proc ImportIt {} {
    global _importlist tk_version
    set t [Help Import "Log of Import Actions"]
    $t config -state normal -height 20
    foreach name [lsort [array names _importlist]] {
	set f $_importlist($name)
	if {[string length $f] != 0} {
	    if [catch {glob $f} file] {
		Exmh_Status $file
		continue
	    }
	    Exmh_Status "inc +$name -file $file -notruncate"
	    $t insert end "inc +$name -file $file -notruncate"
	    update idletasks
	    catch {
		MhExec inc +$name -file $file -notruncate < /dev/null
	    } result
	    $t insert end $result
	    if {$tk_version >= 4.0} {
		$t see end
	    } else {
		$t yview -pickplace end
	    }
	    update idletasks
	}
    }
    $t config -state disabled
    Flist_Refresh
    destroy .import
}

