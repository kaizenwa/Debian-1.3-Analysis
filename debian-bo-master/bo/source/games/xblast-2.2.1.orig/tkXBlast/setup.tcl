#
# tcl script to install tkXBlast
#

#
# step 1 get wish 
#
proc testWish {cmd} {
    global env
    set wishCmd {}
    #
    foreach p [split $env(PATH) ":"] {
	set wishCmd "$p/${cmd}"
	if [file exists $wishCmd] {
	    if [file executable $wishCmd] {
		return $wishCmd
	    }
	}
    }
    #
    return $wishCmd
}

proc versionWish {cmd} {
    set fp [open ".test.tcl" w]
    puts $fp "puts \$tk_version"
    puts $fp "destroy ."
    close $fp
    #
    set fp [open "|$cmd -f .test.tcl" r]
    set version [gets $fp]
    close $fp
    #
    exec rm ".test.tcl"
    #
    return $version
}

#
# main programm
#

#
# parse args
#

set libPath [lindex $argv 0]
set binPath [lindex $argv 1]
set tkLibPath [lindex $argv 2]

set wishCmd [testWish wish]
puts "Enter the path for \"wish\" on your system (default is \"$wishCmd\"):" 

gets file0 newWishCmd
if {$newWishCmd != {}} {
    set wishCmd $newWishCmd
} else {
    if {$wishCmd == {}} {
	puts "Error: path for \"wish\" not defined."
	puts "Aborting programm"
	exit 1
    }
}

set version [versionWish $wishCmd]
puts "tk version is $version"

if {$version < 4.0} {
    puts "You need at least tk 4.0 to run tkXBlast"
    set wishCmd [testWish wish4.0]
    puts "Enter the path for \"wish4.0\" on your system\
 (default is \"$wishCmd\"):" 

    gets file0 newWishCmd
    if {$newWishCmd != {}} {
	set wishCmd $newWishCmd
    } else {
	if {$wishCmd == {}} {
	    puts "Error: path for \"wish\" not defined."
	    puts "Aborting programm"
	    exit 1
	}
    }
    
    set version [versionWish $wishCmd]
    puts "tk version is $version"

    if {$version < 4.0} {
	puts "You need at least tk 4.0 to run tkXBlast"
	exit 1<
    }
}


puts "Creating tkXBlast script"

set fin [open "./tkXBlast.tcl" "r"]
set fout [open "./tkXBlast" "w"]

gets $fin line
puts $fout "\#!$wishCmd -f"

while {-1 != [gets $fin line]} {
    puts $fout $line
    if {"$line" == "\#XBLASTCOMMAND"} {
	puts $fout "set xblastCommand \"$binPath/xblast\""
	gets $fin line
    } elseif {"$line" == "\#XRDBCOMMAND"} {
	puts $fout "set xrdbCommand \"$binPath/xrdb\""
	gets $fin line
    } elseif {"$line" == "\#RGBFILE"} {
	puts $fout "set rgbFile \"$libPath/rgb.txt\""
	gets $fin line
    } elseif {"$line" == "\#BITMAPPATH"} {
	puts $fout "set bitmapPath \"$tkLibPath\""
	gets $fin line
    } 
}

puts "Done"




