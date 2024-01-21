###############################################################################
#
# hfsutils - tools for reading and writing Macintosh HFS volumes
# Copyright (C) 1996, 1997 Robert Leslie
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
###############################################################################

proc getvol {var} {
    global curvol
    upvar $var vol

    if {! [info exists curvol]} {
	error "no volume is current"
    }

    set vol $curvol
}

proc hinfo {} {
    getvol vol

    if {[$vol islocked]} {
	set locked " (locked)"
    } else {
	set locked ""
    }

    puts "Volume name is \"[$vol vname]\"$locked"
    puts "Volume was created on [ctime [$vol crdate]]"
    puts "Volume was last modified on [ctime [$vol mddate]]"
    puts "Volume has [lindex [$vol size] 1] bytes free"
}

proc hmount {path {partno 1}} {
    global mounts curpath curvol

    set vol [hfs mount $path $partno]

    if {[info exists mounts($path)]} {
	humount $path
    }

    set curpath $path
    set curvol $vol
    set mounts($path) $vol

    hinfo
}

proc humount {{path {}}} {
    global mounts curpath curvol

    if {[string length $path] == 0} {
	if {! [info exists curpath]} {
	    error "no volume is current"
	}

	set path $curpath
    } elseif {! [info exists mounts($path)]} {
	error "$path not mounted"
    }

    set vol $mounts($path)
    unset mounts($path)

    if {[string compare $vol $curvol] == 0} {
	unset curpath
	unset curvol
    }

    $vol umount
}

proc hvol {name} {
    global mounts curpath curvol

    if {[info exists mounts($name)]} {
	set curpath $name
	set curvol $mounts($name)
	return
    }

    error "unknown volume"
}

proc hpwd {} {
    getvol vol

    set cwd [$vol cwd]
    set path ""

    while {$cwd != 1} {
	set info [$vol dirinfo $cwd]

	if {[info exists path]} {
	    set path "[lindex $info 0]:$path"
	} else {
	    set path [lindex $info 0]
	}

	set cwd [lindex $info 1]
    }

    return $path
}

proc hcd {{path ""}} {
    getvol vol

    set globbed [$vol glob [list $path]]
    if {[llength $globbed] != 1} {
	error "$path: ambiguous path"
    }
    set path [lindex $globbed 0]

    if {[string length $path] == 0} {
	set path "[$vol vname]:"
    }

    $vol cd $path
}

proc assoc {list var} {
    upvar $var array

    foreach elt $list {
	if {[info exists key]} {
	    set array($key) $elt
	    unset key
	} else {
	    set key $elt
	}
    }
}

proc timestr {secs} {
    set ctime [ctime $secs]

    return "[string range $ctime 4 15][string range $ctime 19 23]"
}

proc ternary {test true false} {
    if {[uplevel expr $test]} {
	return $true
    } else {
	return $false
    }
}

proc hdir {{path ":"}} {
    getvol vol

    set globbed [$vol glob [list $path]]
    if {[llength $globbed] != 1} {
	error "$path: ambiguous path"
    }
    set path [lindex $globbed 0]

    foreach ent [$vol dir $path] {
	assoc $ent item

	if {$item(kind) == "directory"} {
	    puts [format "d%s %9lu item%s               %s %s"  \
		    [ternary {[lsearch $item(flags) "invis"] >= 0} "i" " "]  \
		    $item(size)  \
		    [ternary {$item(size) == 1} " " "s"]  \
		    [timestr $item(mddate)]  \
		    $item(name)]
	} else {
	    puts [format "%s%s %4s/%4s %9lu %9lu %s %s"  \
		    [ternary {[lsearch $item(flags) "locked"] >= 0} "F" "f"]  \
		    [ternary {[lsearch $item(flags) "invis"] >= 0} "i" " "]  \
		    $item(type)  \
		    $item(creator)  \
		    $item(rsize)  \
		    $item(dsize)  \
		    [timestr $item(mddate)]  \
		    $item(name)]
	}
    }
}

proc hstat {{path ":"}} {
    getvol vol

    set globbed [$vol glob [list $path]]
    if {[llength $globbed] != 1} {
	error "$path: ambiguous path"
    }
    set path [lindex $globbed 0]

    assoc [$vol stat $path] item

    foreach elt [lsort [array names item]] {
	if {[regexp {date$} $elt]} {
	    set value [ctime $item($elt)]
	} else {
	    set value $item($elt)
	}

	puts [format "%-10s %s" "$elt:" $value]
    }
}

proc hmkdir {args} {
    getvol vol

    foreach arg [$vol glob $args] {
	$vol mkdir $arg
    }
}

proc hrmdir {args} {
    getvol vol

    foreach arg [$vol glob $args] {
	$vol rmdir $arg
    }
}

proc hcreate {path {type "TEXT"} {creator "UNIX"}} {
    getvol vol

    $vol create $path $type $creator
}

proc htouch {args} {
    getvol vol

    foreach arg [$vol glob $args] {
	if [catch {$vol touch $arg}] {
	    hcreate $arg
	}
    }
}

proc hdel {args} {
    getvol vol

    foreach arg [$vol glob $args] {
	$vol delete $arg
    }
}

proc hrename {src dst} {
    getvol vol

    set globbed [$vol glob [list $src]]
    if {[llength $globbed] != 1} {
	error "$src: ambiguous path"
    }
    set src [lindex $globbed 0]

    $vol rename $src $dst
}

proc hcat {path} {
    getvol vol

    set globbed [$vol glob [list $path]]
    if {[llength $globbed] != 1} {
	error "$path: ambiguous path"
    }
    set path [lindex $globbed 0]

    set file [$vol open $path]

    while {1} {
	set buf [$file read 512]
	if {[string length $buf] == 0} {
	    $file close
	    break
	}

	regsub -all "\r" $buf "\n" buf

	puts -nonewline $buf
    }
}

proc hcopyout {path {dest "."} {mode ""}} {
    getvol vol

    set globbed [$vol glob [list $path]]
    if {[llength $globbed] != 1} {
	error "$path: ambiguous path"
    }
    set path [lindex $globbed 0]

    if {[string length $mode] == 0} {
	assoc [$vol stat $path] item

	if {$item(kind) == "directory"} {
	    error "can't copy whole directories"
	} elseif {[regexp {^TEXT|ttro$} $item(type)]} {
	    set mode text
	} else {
	    set mode macb
	}
    }

    $vol copyout $mode $path $dest
}

proc hcopyin {path {dest ":"} {mode ""}} {
    getvol vol

    set globbed [$vol glob [list $path]]
    if {[llength $globbed] != 1} {
	error "$path: ambiguous path"
    }
    set path [lindex $globbed 0]

    if {[string length $mode] == 0} {
	if {[regexp {\.bin$} $path]} {
	    set mode macb
	} elseif {[regexp {\.hqx$} $path]} {
	    set mode binh
	} elseif {[regexp {\.(txt|c|h)$} $path]} {
	    set mode text
	} elseif {[regexp {\.(sit|sea|cpt|tar|gz|Z|gif|jpg)$} $path]} {
	    set mode raw
	} elseif {[catch {exec file -L $path} type] == 0 &&  \
		[regexp {text} $type]} {
	    set mode text
	} else {
	    set mode raw
	}
    }

    $vol copyin $mode $path $dest
}

proc hformat {path {vname "Untitled"}} {
    global mounts

    if {[info exists mounts($path)]} {
	humount $path
    }

    set partno 0
    hfs format $path $partno $vname

    hmount $path $partno
}

###############################################################################

proc license {} {
    puts -nonewline "\n[hfs license]"
}

proc version {} {
    puts "[hfs version] - [hfs copyright]"
}

if {[string compare [lindex $argv 0] "--license"] == 0} {
    license
    exit
}

version

if {[string compare [lindex $argv 0] "--version"] == 0} {
    puts "`$argv0 --license' for licensing information."
    exit
}

puts "This is free software but comes with ABSOLUTELY NO WARRANTY."
puts "Type `license' for details.\n"

###############################################################################

proc echo {args} {
    puts [join $args " "]
}

proc quit {} {
    exit
}

###############################################################################

if {$argc > 0} {
    eval hmount $argv
}

while {1} {
    puts -nonewline "hfs> "
    flush stdout

    if {[gets stdin line] == -1} {
	exit
    }

    while {! [info complete $line]} {
	if {[gets stdin more] == -1} {
	    break
	} else {
	    set line "$line$more"
	}
    }

    if {[string length [info procs "h[lindex $line 0]"]] > 0} {
	set result [catch {eval h$line} msg]
    } else {
	set result [catch {eval $line} msg]
    }

    if {[string length $msg] > 0} {
	if {$result == 1} {
	    puts "Error: $msg"
	} else {
	    puts $msg
	}
    }
}
