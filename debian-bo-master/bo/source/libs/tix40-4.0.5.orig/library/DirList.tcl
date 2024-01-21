# DirList.tcl --
#
#	Implements the tixDirList widget.
#
# 	   - overrides the -browsecmd and -command options of the
#	     HList subwidget
#
# Copyright (c) 1996, Expert Interface Technologies
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#

tixWidgetClass tixDirList {
    -classname TixDirList
    -superclass tixScrolledHList
    -method {
	chdir
    }
    -flag {
	 -browsecmd -command -dircmd -disablecallback 
	 -root -rootname -showhidden -value
    }
    -configspec {
	{-browsecmd browseCmd BrowseCmd {}}
	{-command command Command {}}
	{-dircmd dirCmd DirCmd {}}
	{-disablecallback disableCallback DisableCallback 0 tixVerifyBoolean}
	{-root root Root {}}
	{-rootname rootName RootName {}}
	{-showhidden showHidden ShowHidden 0 tixVerifyBoolean}
	{-value value Value {}}
    }
    -default {
	{.scrollbar			auto}
	{*borderWidth			1}
	{*hlist.background		#c3c3c3}
	{*hlist.indent			7}
	{*hlist.relief			sunken}
	{*hlist.height			10}
	{*hlist.width			20}
	{*hlist.padX			2}
	{*hlist.padY			0}
	{*hlist.wideSelection		0}
	{*hlist.drawBranch		0}
	{*hlist.highlightBackground	#d9d9d9}
	{*hlist.itemType		imagetext}
	{*hlist.takeFocus		1}
    }
}

proc tixDirList::InitWidgetRec {w} {
    upvar #0 $w data

    tixChainMethod $w InitWidgetRec

    if {$data(-value) == {}} {
	global env
	if {[info exists env(PWD)]} {
	    tixDirList::SetValue $w [tixFileIntName $env(PWD)]
	} else {
	    tixDirList::SetValue $w [tixFileIntName [pwd]]
	}
    } else {
	tixDirList::SetValue $w [tixFileIntName $data(-value)]
    }

    if {$data(-root) == {}} {
	set data(-root) [tixRootDir]
    }
}

proc tixDirList::ConstructWidget {w} {
    upvar #0 $w data

    tixChainMethod $w ConstructWidget
    tixDoWhenMapped $w "tixDirList::LoadDir $w"

    $data(w:hlist) config \
	-separator [tixDirSep] \
	-selectmode "single"
}

proc tixDirList::SetBindings {w} {
    upvar #0 $w data

    tixChainMethod $w SetBindings

    $data(w:hlist) config \
	-browsecmd "tixDirList::Browse $w" \
	-command "tixDirList::Command $w"
}

proc tixDirList::AddDir {w dir} {
    upvar #0 $w data
    uplevel #0 set TRANSPARENT_GIF_COLOR [$data(w:hlist) cget -bg]

    set path {}
    foreach name [tixFileSplit $dir] {
	set path [tixSubFolder $path $name]
	if {![$data(w:hlist) info exists $path]} {
	    $data(w:hlist) add $path -text [tixFileDisplayName $path] \
		-image [tix getimage openfold]
	}
    }
}

proc tixDirList::ListSubDirs {w dir} {
    upvar #0 $w data

    if {$data(-dircmd) != {}} {
	tixDirList::UserListSubDirs $w $dir
	return
    }

    uplevel #0 set TRANSPARENT_GIF_COLOR [$data(w:hlist) cget -bg]
    $data(w:hlist) entryconfig $dir \
	-image [tix getimage act_fold]

    tixBusy $w on $data(w:hlist)

    foreach name [tixListDir $dir 1 0 0 $data(-showhidden)] {
	set subdir [tixSubFolder $dir $name]
	$data(w:hlist) add $subdir -text [tixFileDisplayName $subdir] \
	    -image [tix getimage folder]
    }

    tixWidgetDoWhenIdle tixBusy $w off $data(w:hlist)
}

proc tixDirList::UserListSubDirs {w aDir} {
#
#	BROKEN
#
#    upvar #0 $w data
#    tixBusy $w on $data(w:hlist)
#    uplevel #0 set TRANSPARENT_GIF_COLOR [$data(w:hlist) cget -bg]

#    set olddir {}
#    foreach fname [lsort [tixEvalCmdBinding $w $data(-dircmd) {} \
#	$data(-value) $data(-showhidden)]] {

#	set fname [file tail $fname]
#	if {$data(-value) == "/"} {
#	    set dir /$fname
#	} else {
#	    set dir $data(-value)/$fname
#	}

#	if {$dir == $olddir} {
#	    continue
#	}

#	if {![$data(w:hlist) info exists $dir]} {
#	    $data(w:hlist) add $dir -text $fname \
#		-image [tix getimage folder]
#	}
#	set olddir $dir
#    }
#    tixWidgetDoWhenIdle tixBusy $w off $data(w:hlist)
}


# 
#
#
proc tixDirList::LoadDir {w} {
    if {![winfo exists $w]} {
	return
    }
    if {![winfo ismapped [winfo toplevel $w]]} {
	tixDoWhenMapped [winfo toplevel $w] "tixDirList::LoadDir $w"
	return
    }

    upvar #0 $w data

    if [$data(w:hlist) info exists $data(-root)] {
	$data(w:hlist) delete offsprings $data(-root)
    } else {
	$data(w:hlist) delete all
    }

    tixDirList::AddDir $w $data(i-value)

    uplevel #0 set TRANSPARENT_GIF_COLOR [$data(w:hlist) cget -bg]
    $data(w:hlist) entryconfig $data(-root) \
	-image [tix getimage openfold]

    tixDirList::ListSubDirs $w $data(i-value)
    $data(w:hlist) anchor set $data(i-value)
    $data(w:hlist) select clear
    $data(w:hlist) select set $data(i-value)

    # Make sure it is visible
    # ToDo: also make sure children are visible!
    $data(w:hlist) see $data(i-value)
}

proc tixDirList::ChangeDir {w value} {
    upvar #0 $w data

    tixDirList::SetValue $w $value
    tixDirList::LoadDir $w

    if {$data(-command) != {} && !$data(-disablecallback)} {
	set bind(specs) ""
	tixEvalCmdBinding $w $data(-command) bind $data(-value)
    }
}

proc tixDirList::Command {w args} {
    upvar #0 $w data

    set value [tixEvent flag V]
    tixDirList::ChangeDir $w $value
}

proc tixDirList::Browse {w args} {
    upvar #0 $w data

    uplevel #0 set TRANSPARENT_GIF_COLOR [$data(w:hlist) cget -bg]
    set value [tixEvent flag V]

    if {$data(i-value) != {} && $data(i-value) != $value} {
	if {[$data(w:hlist) info children $data(i-value)] == {}} {
	    $data(w:hlist) entryconfig  $data(i-value)\
		-image [tix getimage folder]
	} else {
	    $data(w:hlist) entryconfig  $data(i-value)\
		-image [tix getimage openfold]
	}

	tixDirList::SetValue $w $value

	$data(w:hlist) entryconfig  $data(i-value)\
	    -image [tix getimage act_fold]
    }


    if {$data(-browsecmd) != {}} {
	set bind(specs) ""
	tixEvalCmdBinding $w $data(-browsecmd) bind $data(-value)
    }
}

proc tixDirList::SetValue {w intName} {
    upvar #0 $w data

    set data(i-value) $intName
    set data(-value)  [tixNativeName $intName]
}

#----------------------------------------------------------------------
# Config options
#----------------------------------------------------------------------
proc tixDirList::config-value {w value} {
    upvar #0 $w data

    set intName [tixFileIntName $value]
    if {[$data(w:hlist) info exists $intName]} {
	$data(w:hlist) anchor set $intName
	$data(w:hlist) select clear
	$data(w:hlist) select set $intName
	$data(w:hlist) see $intName
	tixDirList::SetValue $w $intName
	
	if {$data(-command) != {} && !$data(-disablecallback)} {
	    set bind(specs) ""
	    tixEvalCmdBinding $w $data(-command) bind $data(-value)
	}
	return
    }

    tixWidgetDoWhenIdle tixDirList::ChangeDir $w $intName
}

proc tixDirList::config-root {w value} {
    upvar #0 $w data

    $data(w:hlist) delete all
}

proc tixDirList::config-showhidden {w value} {
    upvar #0 $w data

    tixWidgetDoWhenIdle tixDirList::LoadDir $w
}

#----------------------------------------------------------------------
# Public methods
#----------------------------------------------------------------------
proc tixDirList::chdir {w value} {
    upvar #0 $w data

    tixDirList::ChangeDir $w [tixFile trimslash [tixFile tildesubst $value]]
}
