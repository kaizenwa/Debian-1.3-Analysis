if {$argv == {}} {
    set files [lsort [glob *.tcl]]
} else {
    set files $argv
}

puts "Checking ungarded string comparisons"
catch {
    set lines ""
    set lines [eval exec egrep -n [list {if.*[!=]=.*[^x]\$}] $files]
}

foreach line [split $lines \n] {
    if [regexp {[x]\$} $line] {
	continue
    }
    puts $line
}

puts ""
puts ----------------------------------------------------------------------
puts "Checking improperly guarded string comparisons"
catch {
    set lines ""
    set lines [eval exec egrep -n [list {if.*[!=]=.*[x]\$}] $files]
}

foreach line [split $lines \n] {
    if ![regexp {[\"]x[^\"]*[\"][ ]*[!=]=[ ]*[\"]x[^\"]*[\"]} $line stuff] {
	puts $line
	continue
    }
}

puts ""
puts ----------------------------------------------------------------------
puts "Checking unverified boolean options (missing tixVerifyBoolean)"

set boolOpts {
    -disablecallback
    -dropdown
    -editable
    -fancy
    -history
    -prunehistory
    -disablecallback
    -showhidden
    -takefocus
    -allowzero
    -radio
    -dynamicgeometry
    -ignoreinvoke
}

puts "checking the following options: \{$boolOpts\}"

set rexp ""
set prefix ""

foreach opt $boolOpts {
    append rexp $prefix\(\{$opt\[\ \].*\[^n\]\})
    set prefix "|"
}

catch {
    puts [eval exec egrep -n [list $rexp] $files]
}
