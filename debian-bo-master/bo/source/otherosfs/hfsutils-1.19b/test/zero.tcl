set path ../disk.test
set size 1440

catch {humount $path}

puts "Zeroing $path (${size}K) ..."
catch {exec dd if=/dev/zero of=$path bs=1k count=$size} msg
puts $msg

puts "Formatting $path ..."
hformat $path
