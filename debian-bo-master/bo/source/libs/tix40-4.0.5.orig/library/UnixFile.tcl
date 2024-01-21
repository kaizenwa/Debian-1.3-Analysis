# UnixFile.tcl --
#
#	Unix file access portibility routines.
#
# Copyright (c) 1996, Expert Interface Technologies
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#

proc InitFileCmpt:Unix {} {

# Directory separator
#
proc tixDirSep {} {
    return "/"
}

# nativeName:	native filename used in this OS, comes from the user or
#		application programmer
# defParent:	if the filename is not an absolute path, treat it as a
#		subfolder of $defParent
proc tixFileIntName {nativeName {defParent {}}} {
    if {![tixIsAbsPath $nativeName]} {
	if {$defParent != {}} {
	    set path [tixSubFolder $defParent $nativeName]
	} else {
	    set path $nativeName
	}
    } else {
	set path $nativeName
    }

    set intName {}
    set path [tixFile trimslash [tixFile tildesubst $path]]
    foreach name [tixFileSplit $path] {
	set intName [tixSubFolder $intName $name]
    }

    return $intName
}

proc tixNativeName {name {mustBeAbs {}}} {
    return $name
}

proc tixFileDisplayName {intName} {
    if {$intName == "/"} {
	return "/"
    } else {
	return [file tail $intName]
    }
}


proc tixFileSplit {intName} {

    set l ""
    foreach n [split $intName /] {
	if {$n == {}} {
	    continue
	}
	if {$n == "."} {
	    continue
	}

	lappend l $n
    }
    

    while 1 {
	set idx [lsearch $l ".."]
	if {$idx == -1} {
	    break;
	}
	set l [lreplace $l [expr $idx -1] $idx]
    }


    if {[string index $intName 0] == "/"} {
	return [concat "/" $l]
    } else {
	return $l
    }
}

proc tixSubFolder {parent sub} {
    if {$parent == {}} {
	return $sub
    }
    if {$parent == "/"} {
	return /$sub
    } else {
	return $parent/$sub
    }
}

# dir:		Make a listing of this directory
# showSubDir:	Want to list the subdirectories?
# showFile:	Want to list the non-directory files in this directory?
# showPrevDir:	Want to list ".." as well?
# showHidden:	Want to list the hidden files?
#
# return value:	a list of files and/or subdirectories
#
proc tixListDir {dir showSubDir showFile showPrevDir showHidden {pattern {}}} { 

    set appPWD [pwd]

    if [catch {cd $dir} err] {
	# The user has entered an invalid directory
	# %% todo: prompt error, go back to last succeed directory
	cd $appPWD
	return {}
    }

    if {$pattern == {}} {
	if $showHidden {
	    set pattern "* .*"
	} else {
	    set pattern *
	}
    } elseif {$pattern == "*"} {
	if $showHidden {
	    set pattern "* .*"
	}
    }

    set list {}
    foreach pat $pattern {
	if [catch {set names [lsort [glob -nocomplain $pat]]} err] {
	    # Cannot read directory
	    # %% todo: show directory permission denied
	    continue
	}

	catch {
	    # We are catch'ing, just in case the "file" command
	    # returns unexpected errors
	    #
	    foreach fname $names {
		if {![string compare . $fname]} {
		    continue
		}
		if [file isdirectory $fname] {
		    if {![string compare ".." $fname] && !$showPrevDir} {
			continue
		    }
		    if $showSubDir {
			lappend list [file tail $fname]
		    }
		} else {
		    if $showFile {
			lappend list [file tail $fname]
		    }
		}
	    }
	}
    }

    cd $appPWD

    if {[llength $pattern] > 1} {
	set list1 {}
	set oldfile {}
	foreach name [lsort $list] {
	    if {$name == $oldfile} {
		continue
	    }
	    lappend list1 $name
	    set oldfile $name
	}
	return $list1
    } else {
	return $list
    }
}

# returns the "root directory" of this operating system
#
proc tixRootDir {} {
    return "/"
}

proc tixIsAbsPath {nativeName} {
    set c [string index $nativeName 0]
    if {$c == "~" || $c == "/"} {
	return 1
    } else {
	return 0
    }
}

proc tixVerifyFile {file} {
    return [tixFileIntName $file]
}

proc tixFilePattern {args} {
    if {[lsearch $args allFiles] != -1} {
	return *
    }
    return *
}
}





