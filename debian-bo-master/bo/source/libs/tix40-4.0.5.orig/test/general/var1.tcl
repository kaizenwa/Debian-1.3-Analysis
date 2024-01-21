proc About {} {
    return "Testing -variable option with Tix widgets"
}

proc Test {} {
    global foo bar arr

    set classes {tixControl}
    set value 1234

    foreach class $classes {
	set w [$class .foo]

	puts "testing config -variable with initialized value"
	#
	set bar $value
	$w config -variable bar
	update idletasks
	Assert {[$w cget -value] == $value}


	puts "testing config -variable with uninitialized value"
	#
	destroy $w
	set w [$class .foo]
	$w config -variable bar
	Assert {[$w cget -value] == $bar}

	puts "testing config -variable"
	#
	set foo 111
	$w config -variable foo
	update idletasks
	Assert {[$w cget -value] == $foo}

	puts "testing config -value"
	#
	$w config -value 123
	Assert {[$w cget -value] == 123}
	Assert {[set [$w cget -variable]] == 123}

	puts "testing config -variable on array variable"
	#
	set arr(12) 1234
	$w config -variable arr(12)
	Assert {[$w cget -value] == $arr(12)}

	puts "testing config -value on array variable"
	#
	$w config -value 12
	Assert {[$w cget -value] == 12}
	Assert {[set [$w cget -variable]] == 12}
    }
}


## MAIN_BEGIN

source ../library/TestLib.tcl
Test
Done

## MAIN_END
