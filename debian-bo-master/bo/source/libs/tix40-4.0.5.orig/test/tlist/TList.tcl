# This tests the TList widget.
#
#
# Assumptions:
#	None
#

proc test {cmd {result {}} {ret {}}} {
    if [catch {set ret [uplevel 1 $cmd]} err] {
	set done 0
	foreach r $result {
	    if [regexp $r $err] {
		puts "error message OK: $err"
		set done 1
		break
	    }
	}
	if {!$done} {
	    error $err
	}
    } else {
	puts "execution OK: $cmd"
    }
    return $ret
}

#
# Test the creation
#
test {tixTList} {args}
test {tixTList .t -ff} {unknown}
test {tixTList .t -width} {missing}

if {[info command .t] != {}} {
    error "widget not destroyed when creation failed"
}

set t [tixTList .t]
test {$t} {args}

#
# Test the "insert" command
#
test {$t insert} {args}
test {$t insert 0 -foo} {missing}
test {$t insert 0 -foo bar} {unknown}
test {$t insert 0 -itemtype foo} {unknown}
test {$t insert 0 -itemtype text -image foo} {unknown}
test {$t insert 0 -itemtype text -text Hello} 

pack $t
