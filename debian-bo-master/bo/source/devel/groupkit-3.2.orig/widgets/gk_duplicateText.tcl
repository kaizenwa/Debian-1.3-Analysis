proc gk_duplicateInsertKey {parent child ascii keysym} {
    switch -regexp $keysym {
        ^F1|^F2|^F3|^F4|^F5|^F6|^F7|^F8|^F9|^F0|^Control|^Meta|^Alt|^Shift {
	    # Do Nothing
#	    puts "WARNING: Key symbol, $keysym, ignored"
	}
	Multi|Caps_Lock|Tab|Down|Left|Up|Right|Help|Escape|Num_Lock|\\\?\\\? {
	    # Do Nothing
#	    puts "WARNING: Key symbol, $keysym, ignored"
	}
	default {
	    if { [set range [$parent tag nextrange sel 1.0 end]] != ""} {
		gk_duplicateDoDelete $parent $child
		gk_toAll $child insert [lindex $range 0] "$ascii"
	    } else {
		gk_toAll $child insert [$parent index insert] "$ascii"
	    }
	}
    }
}

proc gk_duplicateToPrevious {parent child command} {
    set x [lindex [split [$parent index insert] .] 1]
    set y [lindex [split [$parent index insert] .] 0] 
    if { $y == 1 && $x == 0 } {
	return
    } elseif { $x > 0 } {        
	set x [expr $x - 1]
        set index $y.$x
    } else {	
	set y [expr $y - 1]
        set index [$child index "$y.0 lineend"]
    }
    set command [eval subst \"$command\"]
    gk_toAll eval $command
}

proc gk_duplicateToCurrent {parent child command {index {}}} {
    if {$index == {} } { set index [$parent index insert] }
    set command [eval subst \"$command\"]
    gk_toAll eval $command
}

proc gk_duplicateGetSelection {parent child} {
    catch {gk_duplicateToCurrent $parent $child {$child insert \$index \
	[selection get -displayof $parent]}}
}

proc gk_duplicateDoDelete {parent child {command {}}} {
    if { [set range [$parent tag nextrange sel 1.0 end]] != ""} {
	gk_toAll eval $child delete $range
    } elseif {$command != {} } {
	gk_toAll $command $parent $child {$child delete \$index}
    }
}

proc gk_duplicateDoControlk {parent child} {
    set index [$parent index insert]
    if { [$parent index "$index lineend"] == [$parent index "$index linestart"] } {
	gk_duplicateToPrevious $parent $child {$child delete \$index}
    }  else {
	gk_duplicateToCurrent $parent $child {$child delete \$index  \\\"\$index lineend\\\"} 
    }
}

proc gk_duplicateDoControlt {parent child} {
    set index [$parent index insert]
    set x [lindex [split $index .] 1]
    set y [lindex [split $index .] 0]

    if {$x == 0} {
	set char [$parent get $index]
        gk_toAll $child delete $y.$x
        set y [expr $y -1]
        gk_toAll $child insert "$y.0 lineend" $char
    } elseif { $index == [$parent index "$y.0 lineend"]} {
	set char [$parent get $y.[expr $x - 2]]
        gk_toAll $child delete $y.[expr $x - 2]
	gk_toAll $child insert $y.[expr $x -1] $char
    } else {
	set char [$parent get $y.[expr $x - 1]]
        gk_toAll $child delete $y.[expr $x - 1]
	gk_toAll $child insert $index $char
   }
}

proc gk_duplicateSelection {parent child command} {
    if { [set range [$parent tag nextrange sel 1.0 end]] != ""} {
	gk_duplicateDoDelete $parent $child
	eval $command [lindex $range 0]
    } else {
	eval $command
    }
}

proc gk_duplicateCopy {parent child} {
    if { [$parent tag nextrange sel 0.0 end] == "" } { return }
    clipboard clear -displayof $child
    clipboard append -displayof $child [$parent get sel.first sel.last]
}

proc gk_duplicateCut {parent child} {
    if { [$parent tag nextrange sel 0.0 end] == "" } { return }
    set first [$parent index sel.first]; set last [$parent index sel.last];
    clipboard clear -displayof $child
    clipboard append -displayof $child [$parent get $first $last]
    $child delete $first $last
}

proc gk_duplicatePaste {parent child} {
    catch {$child insert [$parent index insert] \
	    [selection get -displayof $child -selection CLIPBOARD]}
}


proc gk_duplicateText {parent child} {
    # Ignore key sequences not specified
    bind $parent <Control-KeyPress> { # nothing }
    bind $parent <Alt-KeyPress> { # nothing }
    bind $parent <Meta-KeyPress> { # nothing}
    bind $parent <Escape> { # nothing}


    bind $parent <Key>  "+ gk_duplicateInsertKey $parent $child %A %K"
    
    # Return or Newline
    bind $parent <Return> "+ gk_duplicateSelection $parent $child \
	   {gk_duplicateToCurrent $parent $child {$child insert \$index {\n}}}"
    bind $parent <Control-o> "+ gk_duplicateToCurrent $parent $child \
	    {$child insert \$index {\n}}"

    # Tabbing
    bind $parent <Tab> "+ gk_duplicateToCurrent $parent $child \
	    {$child insert \$index {\t}}"
    bind $parent <Control-i> "+ gk_duplicateSelection $parent $child \
	   {gk_duplicateToCurrent $parent $child {$child insert \$index {\t}}}"
 
    # Insert key
    bind $parent <Insert> "+ gk_duplicateGetSelection $parent $child"
    bind $parent <Button-2> "+ gk_duplicateGetSelection $parent $child"
    bind $parent <Control-v> "+ gk_duplicateGetSelection $parent $child"
    
    # Backspacing
    bind $parent <BackSpace> "+ gk_duplicateDoDelete $parent $child \
	    gk_duplicateToPrevious"
    bind $parent <Control-h> \
	    "+ gk_duplicateToPrevious $parent $child {$child delete \$index}"
    bind $parent <Meta-BackSpace> "+ gk_duplicateToPrevious $parent $child \
	    {$child delete \\\"\$index wordstart\\\" \$index+1c}"

    # Deleting
    bind $parent <Delete> "+ gk_duplicateDoDelete $parent $child \
	    gk_duplicateToCurrent"
    bind $parent <Control-d> "+ gk_duplicateToCurrent $parent $child \
	    {$child delete \$index}"
    bind $parent <Meta-d> "+ gk_duplicateToCurrent $parent $child \
	    {$child delete \$index \\\"\$index wordend\\\"}"

    # Toggle characters
    bind $parent <Control-t> "+ gk_duplicateDoControlt $parent $child"
    
    # Cut, Copy and Paste
    bind $parent <Control-k> "+ gk_duplicateDoControlk $parent $child"
    bind $parent <Meta-w> "+ gk_duplicateCut $parent $child"
    bind $parent <F16> "+ gk_duplicateCut $parent $child"
    bind $parent <Control-w> "+ gk_duplicateCut $parent $child"
    bind $parent <F20> "+ gk_duplicateCut $parent $child"
    bind $parent <Control-y> "+ gk_duplicatePaste $parent $child"
    bind $parent <F18> "+ gk_duplicatePaste $parent $child"
}

