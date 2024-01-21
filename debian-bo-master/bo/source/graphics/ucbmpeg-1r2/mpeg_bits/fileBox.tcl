proc fileBox { w ttle function afterFunction } {
    toplevel $w
    wm title $w $title
    frame $w.f1
    frame $w.f2
    frame $w.f2.e1 -textvariable fiName -relief sunk
    $w.f2.e1 delete 0 end
    button $w.f2.b1 -text "Cancel" -command {destroy $w }
    button $w.f2.b2 -text "OK" -command {
	uplevel \#0 "handleEntry [string trim $fiName] $w $function $afterFunction"
    }
    
    scrollbar $w.f1.scroll -command "$w.f1.list yview"
    listbox $w.f1.list -yscroll "$w.f1.scroll set"
    tk_listboxSingleSelect $w.f1.list
    label $w.f1.l1 -text [pwd] -relief raised
    set fileList [glob -nocomplain .* *]
    for { set i 0 } { $i < [llength $fileList]} { incr i } {
	if {[isDirectory [lindex $fileList $i]]} {
	    set fileList [lreplace $fileList $i $i "[lindex $fileList $i]/"]
	}
    }
    eval "$w.f1.list insert end [lsort -ascii $fileList]"
    bind $w.f2.e1 <KeyPress-Return> { 
	uplevel \#0 "handleEntry [string trim $openFileName] $w $function $afterFunction"
    }
    bind $w.f1.list <ButtonPress-1> { 
	$w.f1.list select from [$w.f1.list nearest %y]
	$w.f2.e1 delete 0 end
	$w.f2.e1 insert 0 [$w.f1.list get [$w.f1.list nearest %y]]
    }
    bind $w.f1.list <B1-Motion> {
	$w.f1.list select from [$w.f1.list nearest %y]
	$w.f2.e1 delete 0 end
	$w.f2.e1 insert 0 [$w.f1.list get [$w.f1.list nearest %y]]
    }
    bind $w.f1.list <ButtonPress-3> {
	$w.f1.list select clear
	$w.f2.e1 delete 0 end
    }
    bind $w.f1.list <Double-ButtonPress-1> { 
	uplevel \#0 "handleEntry [$w.f1.list get [lindex [$w.f1.list curselection] 0] ] $w $function $afterFunction"}
    pack append $w.f1 $w.f1.l1 {top fillx} \
	$w.f1.scroll {right filly} $w.f1.list {left expand fill}
    pack append $w.f2 $w.f2.e1 {left fillx expand} \
	$w.f2.b2 {left} $w.f2.b1 {left}
    pack append $w $w.f2 {top fillx} $w.f1 {bottom expand fill}
    grab $w
    focus $w.f2.e1
    wm minsize $w  200 200
}

proc handleEntry { fileName function afterFunction } {
    if {[string length $fileName] == 0} {
	return
    }
    if 