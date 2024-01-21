## very preliminary front end to the very preliminary gk_text 
## groupware text edit widget
##  by mark roseman

gk_initConf $argv

pack [gk_defaultMenu .menu] -side top -fill x
pack [gk_scrollbar .scr -command ".t yview"] -side right -fill y
pack [gktext .t -width 50 -yscrollcommand ".scr set"] -side left -fill both -expand yes


.menu itemcommand 0 insert 1 command -label "Open" -command "Open"
.menu itemcommand 0 insert 2 command -label "Save" -command "Save"
.menu itemcommand 0 insert 3 command -label "Clear" -command "Clear"

proc Open {} {
    set fileName [FSBox]
    if {$fileName == ""} {return}
    Clear
    set fd [open $fileName r]
    set result [gets $fd temp]
    while {$result != -1} {
        gkTextInsert .t $temp\n
	set result [gets $fd temp]
    }
    close $fd
}

proc Save {} {
    set fileName [FSBox]
    if {$fileName == ""} {return}
    set fd [open $fileName a]
    puts $fd [.t get 1.0 end-1c]
    close $fd

}

proc Clear {} {
    gk_serialize doClear
}

proc doClear {} {
    .t delete 1.0 end-1c
}
