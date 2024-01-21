#!/usr/dist/pkgs/tk,v4.0/5.x-sparc/bin/wish
## exmh helper
##  a sideline to exmh that shows a display of which folders have unseen
##  messages
##  by Mark Roseman <roseman@cpsc.ucalgary.ca>

## create the display

wm title . "Exmh Helper"
pack [frame .buttons] -side left
pack [button .buttons.comp -font 6x10 -text Compose -command send_compose] \
	-side top -fill x
pack [button .buttons.updt -font 6x10 -text Update -command do_update] \
	-side top -fill x
pack [text .unseen -width 20 -height 5 -font 6x10 -state disabled] \
	-side left -expand yes -fill both
bind .unseen <1> "try_goto %x %y"

## send a message to exmh to open a compose window

proc send_compose {} { send exmh Msg_Compose }

## update the display of unseen messages

proc do_update {} { 
    .unseen config -state normal
    .unseen delete 1.0 end
    set output ""
    catch {set output [eval exec grep unseen [glob ~/Mail/*/.mh_sequences]]}
    set lines [split $output \n]
    foreach i $lines {
        set folder [string range $i [expr [string first "/Mail/" $i]+6] end]
        set folder [string range $folder 0 [expr [string first "/" $folder]-1]]
	set seq [string range $i [string last ":" $i] end]
	.unseen insert end $folder$seq\n
    }
    .unseen config -state disabled
}

## call the do_update function every minute

proc periodic_update {} {
    do_update
    after 60000 periodic_update
}

## button clicked in list of unseen messages; if clicked on a folder,
## tell exmh to switch to that folder

proc try_goto {x y} {
  set index [.unseen index @$x,$y]
  set line [lindex [split $index .] 0]
  set txt [.unseen get "$index linestart" "$index lineend"]
  if {$txt!=""} {
      set folder [lindex $txt 0]
      set folder [string range $folder 0 [expr [string length $folder]-2]]
      send exmh Folder_Change $folder
  }
}

## start the whole thing off

periodic_update


