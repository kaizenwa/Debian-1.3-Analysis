source test/zero.tcl

puts "Creating files ..."

set file1 "Foo"
set file2 "Bar"

htouch $file1
htouch $file2

puts "Fragmenting files ..."

source test/block.tcl

for {set i 0} {$i < 500} {incr i} {
    puts -nonewline "$i "
    flush stdout

    set file [$curvol open $file1]

    $file fork data
    $file seek 0 end
    $file write $block

    $file fork rsrc
    $file seek 0 end
    $file write $block

    $file close

    set file [$curvol open $file2]

    $file fork data
    $file seek 0 end
    $file write $block

    $file fork rsrc
    $file seek 0 end
    $file write $block

    $file close
}

puts ""
