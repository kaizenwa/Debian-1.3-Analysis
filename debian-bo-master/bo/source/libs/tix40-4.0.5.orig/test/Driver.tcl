# This is the "Test Driver" program that sources in each test script. It
# must be invoked by the test/Test.tcl program (in Unix) or by a properly
# configured wish.exe program (in Wondows).
#

set oldglobals {}
set oldglobals [info globals]

if {![info exists tix]} {
    if ![info exists tcl_platform(platform)] {
	puts "ERROR: this version of wish doesn't support dynamic loading"
	exit -1
    }

    # This must have been executed by a plain wish expecting to
    # dynamic load Tix.

    puts "trying to dynalically load Tix"

    if {$tcl_platform(platform) == "unix"} {
	load ../unix-tk4.1/libtix[info sharedlibextension] Tix
    } else {
	load ..\\win\\libtix[info sharedlibextension] Tix
    }
    puts succeeded
}

proc Drive:Test {name f} {
    global oldglobals


    foreach w [winfo children .] {
	destroy $w
    }
	
    foreach g [info globals] {
	if {[lsearch $oldglobals $g] == -1} {
#	    uplevel #0 unset $g
	}
    }

    puts ------------------------------------------------------------
    puts "Loading script $name"

    update
    uplevel #0 source $f
}

proc Drive:GetFiles {} {
    set data {}
    set fd [open files {RDONLY}]
	while {![eof $fd]} {
	    append data [gets $fd]\n
		puts $data
		update
	}
    close $fd
	regsub -all [format %s \n] $data " " files
	return $files
}

proc Drive:Main {} {
    global argv env

    set argvtarget [lindex $argv 0]
    set argvfiles  [lrange $argv 1 end]

    if {$argvtarget == {}} {
	set argvtarget windows
    }

    set env(WAITTIME) 1000

    set genDirs {
	general xpm hlist
    }

    set tk40(dir)    [concat $genDirs]
    set tk41(dir)    [concat $genDirs]
    set itcl(dir)    [concat $genDirs itcl]
    set load(dir)    load
    set windows(dir) general

    set errCount 0

    upvar 0 $argvtarget target

    set PWD [pwd]
    if {$argvfiles == {}} {
	foreach dir $target(dir) {
	    puts "Entering directory $dir ..."
	    cd $dir
	    
	    foreach f [Drive:GetFiles] {
		Drive:Test $dir/$f $f
	    }
	    puts "Leaving directory $dir ..."
	    cd $PWD
	}
    } else {
	foreach f $argvfiles {
	    if [file isdir $f] {
		set dir $f
		puts "Entering directory $dir ..."
		cd $dir
		set files [exec cat files]
		regsub -all [format %s \n] $files " " files
		
		foreach f $files {
		    Drive:Test $dir $f $f
		}
		puts "Leaving directory $dir ..."
		cd $PWD
	    } else {
		set dir [file dirname $f]
		if {$dir != {}} {
		    puts "Entering directory $dir ..."
		    cd $dir
		    set f [file tail $f]
		}

		Drive:Test $f $f

		puts "Leaving directory $dir ..."
		cd $PWD
	    }
	}
    }
}

wm title . "Test-driving Tix"
Drive:Main

if {[tix platform] == "windows"} {
    tkwait visibility .
}

# We are done if we safely come to here
#
exit 0
