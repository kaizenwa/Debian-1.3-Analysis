#!/usr/local/bin/wishx -f
#
# Tk control panel routine
# Copyright (c) 1996,1997  Takashi Iwai
#

#----------------
# initialize global variables
#
proc InitGlobal {} {

    global Stat

    # time table
    set Stat(MaxSecs) 0
    set Stat(LastSec) 0
    set Stat(TotalTimeStr) "/ 0:00"

    # message lines
    set Stat(CurMsgs) 0
    set Stat(MaxMsgs) 500

    # current status
    set Stat(Playing) 0
    set Stat(Paused) 0
    set Stat(Blinking) 0

    # MIDI file list
    set Stat(CurIdx) -1
    set Stat(MaxFiles) 0
    set Stat(FileList) {}
    set Stat(ShuffleList) {}
    set Stat(CurFile) "--------"
    set Stat(NextFile) "--------"

    global Config
    # playing mode
    set Config(Tracing) 0
    set Config(RepeatPlay) 0
    set Config(ShufflePlay) 0
    set Config(AutoStart) 0
    set Config(AutoExit) 0
    set Config(ConfirmExit) 1

    # display configuration
    set Config(Disp:file) 1
    set Config(Disp:time) 1
    set Config(Disp:text) 0
    set Config(Disp:volume) 1
    set Config(Disp:button) 1
    set Config(Disp:trace) 0

    set Config(Mode:chorus) 4
    set Config(Mode:reverb) 4

    set Config(CurFileMode) 0

    # current volume
    set Config(CurVol) 100
    set Config(CurBass) 5
    set Config(CurTreble) 9

    wm title . "TkAWEMidi"
    wm iconname . "TkAWEMidi"
    #wm iconbitmap . @$bitmap_path/tkawe.xbm
}


#----------------
# read a message from stdin
#
proc HandleInput {} {
    global Stat Config

    set mlist [gets stdin]
    set msg [lindex $mlist 0]

    if {$msg == "TIME"} {
	# set total time
	set csecs [expr [lindex $mlist 1] / 100]
	set Stat(TotalTimeStr) [sec2time $csecs]
	set Stat(MaxSecs) $csecs
	set tics [expr $csecs / 8]
	set tics [expr (($tics + 4) / 5) * 5]
	.body.time.scale configure -tickinterval $tics -to $csecs
	SetTime 0

    } elseif {$msg == "MVOL"} {
	# set master volume
	SetVolume [lindex $mlist 1]

    } elseif {$msg == "FILE"} {
	# set playing file
	set Stat(CurFile) [join [lrange $mlist 1 end]]
	wm title . "TkAWEMidi: $Stat(CurFile)"
	wm iconname . "TkAWEMidi: $Stat(CurFile)"
	.body.curfile.title configure -text $Stat(CurFile)
	if {$Config(CurFileMode) == 0} {
	    .body.curfile.time configure -text "00:00"
	} else {
	    .body.curfile.time configure -text "-$Stat(TotalTimeStr)"
	}
	AppendMsg "------"

    } elseif {$msg == "LIST"} {
	# set file list
	.body.file.list delete 0 end
	set Stat(MaxFiles) [lindex $mlist 1]
	set Stat(FileList) {}
	for {set i 0} {$i < $Stat(MaxFiles)} {incr i} {
	    set file [gets stdin]
	    .body.file.list insert end $file
	    lappend Stat(FileList) $file
	}
	# MakeShuffleList

	set Stat(CurIdx) -1
	SelectNumber 

    } elseif {$msg == "PREV"} {
	# previous file
	set Stat(CurIdx) [expr $Stat(CurIdx) - 1]
	if {$Stat(CurIdx) < 0} {set Stat(CurIdx) 0}
	SelectNumber 

    } elseif {$msg == "NEXT" || $msg == "TEND"} {
	# next file
	incr Stat(CurIdx)
	if {$Stat(CurIdx) >= $Stat(MaxFiles)} {
	    if {$Config(RepeatPlay)} {
		set Stat(CurIdx) 0
	    } elseif {$Config(AutoExit)} {
		QuitCmd
	    } else {
		StopCmd
	    }
	}
	SelectNumber

    } elseif {$msg == "CMSG"} {
	# put message
	set type [lindex $mlist 1]
	set str [gets stdin]
	AppendMsg $str

    } elseif {$msg == "CERR"} {
	error [format "%s: %s" $Stat(NextFile) [gets stdin]]
	WriteMsg "NEXT"

    } elseif {$msg == "QUIT"} {
	# quit
	exit
    } elseif {$msg == "RSTA"} {
	# restart file
	SelectNumber
    } elseif {$msg == "CHRS"} {
	set Config(Mode:chorus) [lindex $mlist 1]
	SetChorus
    } elseif {$msg == "EVRB"} {
	set Config(Mode:reverb) [lindex $mlist 1]
	SetReverb
    }
}


#----------------
# make shuffled list
#
proc MakeShuffleList {} {
    global Stat
    set tmplist {}
    for {set i 0} {$i < $Stat(MaxFiles)} {incr i} {
	lappend tmplist $i
    }
    set Stat(ShuffleList) {}
    set len $Stat(MaxFiles)
    while {$len > 0} {
	set pos [my-random $len]
	lappend Stat(ShuffleList) [lindex $tmplist $pos]
	set tmplist [lreplace $tmplist $pos $pos]
	set len [expr $len - 1]
    }
}

#
# append a string to message buffer
#
proc AppendMsg {str} {
    global Stat

    incr Stat(CurMsgs)
    if {$Stat(CurMsgs) >= $Stat(MaxMsgs)} { ClearMsg }
    .body.text.buf insert end $str\n
    .body.text.buf yview -pickplace end
}

#
# clear message buffer
#
proc ClearMsg {} {
    global Stat
    .body.text.buf delete 0.0 end
    .body.text.buf yview 0
    set Stat(CurMsgs) 0
}


#----------------
# select the file in listbox and load it
#
proc SelectNumber {} {
    global Stat Config
    .body.file.list select clear 0 end
    set idx -1
    if {$Stat(CurIdx) >= 0 && $Stat(CurIdx) < [llength $Stat(FileList)]} {
	if {$Config(ShufflePlay)} {
	    if {$Stat(ShuffleList) == {}} {
		MakeShuffleList
	    }
	    set idx [lindex $Stat(ShuffleList) $Stat(CurIdx)]
	} else {
	    set idx $Stat(CurIdx)
	}
	set thefile [lindex $Stat(FileList) $idx]
	set Stat(NextFile) $thefile
    }
    if {$idx >= 0 && $thefile != "-" && ![file exists $thefile]} {
	warning "Can't open file \"$thefile\"."
	set idx -1
    }

    if {$idx >= 0} {
	.body.file.list select set $idx
	LoadCmd $idx
	set Stat(Playing) 1
    } else {
	SetTime 0
	.body.curfile.title configure -text "--------"
	.body.curfile.time configure -text "00:00"
	set Stat(Playing) 0
	set Stat(Paused) 0
    }
    DispButtonPlay
}


#
# update current time
#
proc SetTime {val} {
    global Stat Config
    if {$Stat(CurIdx) == -1} {
	return
    }
    set Stat(LastSec) $val
    if {$Config(CurFileMode) == 0} {
	set curt [sec2time $val]
	.body.curfile.time configure -text "$curt"
    } else {
	set curt [sec2time [expr $val - $Stat(MaxSecs)]]
	.body.curfile.time configure -text "$curt"
    }
    set curt [sec2time $val]
    .body.time.label configure\
	    -text "$curt / $Stat(TotalTimeStr)"
    .body.time.scale set $val
    DispButtonPlay
}


#
# colorize buttons with each state
#
proc DispButtonPlay {} {
    global Stat
    if {$Stat(Playing)} {
	if {$Stat(Blinking)} {
	    set color green
	    set Stat(Blinking) 0
	} else {
	    set color red
	    set Stat(Blinking) 1
	}
	.body.button.play configure -fg $color -activeforeground $color
    } else {
	.body.button.play configure -fg black -activeforeground black
    }

    if {$Stat(Playing) && $Stat(Paused)} {
	.body.button.pause configure -fg red -activeforeground red
    } else {
	.body.button.pause configure -fg black -activeforeground black
    }
}    

#
# update current volume
#
proc SetVolume {val} {
    global Config
    set Config(CurVol) $val
    #.body.volume.label configure -text [format "Volume: %d%%" $val]
    .body.volume.scale set $val
}


#----------------
# write message
# messages are: PREV, NEXT, ZAPP, QUIT, FWRD, BACK, RSET, STOP
#	LOAD\n<filename>, JUMP <time>
#	VOLM <%>, VBAS <val>, VTRB <val>
#

proc WriteMsg {str} {
    puts stdout $str
    flush stdout
}


#----------------
# callback commands
#

#
# jump to specified time
#
proc JumpCmd {val} {
    global Stat
    if {$val != $Stat(LastSec)} {
	WriteMsg [format "JUMP %d" [expr $val * 100]]
    }
}

#
# change volume amplitude
#
proc VolumeCmd {val {force 0}} {
    global Config
    if {$val < 0} {set val 0}
    if {$val > 200} {set val 200}
    if {$force != 0 || $val != $Config(CurVol)} {
	WriteMsg [format "VOLM %d" $val]
    }
}
#
proc SetChorus {} {
    global Config
    WriteMsg [format "CHRS %d" $Config(Mode:chorus)]
}

proc SetReverb {} {
    global Config
    WriteMsg [format "EVRB %d" $Config(Mode:reverb)]
}
    

#
# load the specified file
#
proc LoadCmd {idx} {
    global Stat Config
    WriteMsg "LOAD"
    WriteMsg [lindex $Stat(FileList) $idx]
    AppendMsg ""
    VolumeCmd $Config(CurVol) 1
    BassCmd $Config(CurBass)
    TrebleCmd $Config(CurBass)
}

#
# play the first file
#
proc PlayCmd {} {
    global Stat
    MakeShuffleList
    if {$Stat(Playing) == 0} {
	WriteMsg "NEXT"
    }
}

#
# pause music
#
proc PauseCmd {} {
    global Stat
    if {$Stat(Playing)} {
	if {$Stat(Paused)} {
	    set Stat(Paused) 0
	} else {
	    set Stat(Paused) 1
	}
	DispButtonPlay
	WriteMsg "STOP"
    }
}

#
# stop playing
#
proc StopCmd {} {
    global Stat Config
    if {$Stat(Playing)} {
	WriteMsg "QUIT"
	set Stat(CurIdx) -1
	SelectNumber
	if {$Config(Tracing)} {
	    TraceReset
	}
	wm title . "TkAWEMidi"
	wm iconname . "TkAWEMidi"
    }
}

#
# quit TkAWEMidi
#
proc QuitCmd {} {
    global Config Stat
    if {$Config(AutoExit) || !$Config(ConfirmExit)} {
	WriteMsg "ZAPP"
	return
    }
    set oldpause $Stat(Paused)
    if {!$oldpause} {PauseCmd}
    if {[question "Really Quit TkAWEMidi?" 0]} {
	WriteMsg "ZAPP"
	return
    }
    if {!$oldpause} {PauseCmd}
}

#
# play previous file
#
proc PrevCmd {} {
    global Stat
    if {$Stat(Playing)} {
	WriteMsg "PREV"
    }
}

#
# play next file
#
proc NextCmd {} {
    global Stat
    if {$Stat(Playing)} {
	WriteMsg "NEXT"
    }
}

#
# forward/backward 2 secs
#
proc ForwardCmd {} {
    global Stat
    if {$Stat(Playing)} {
	WriteMsg "FWRD"
    }
}
    
proc BackwardCmd {} {
    global Stat
    if {$Stat(Playing)} {
	WriteMsg "BACK"
    }
}


#
# volume up/down
#
proc VolUpCmd {} {
    global Stat Config
    if {$Stat(Playing)} {
	VolumeCmd [expr $Config(CurVol) + 5]
    }
}

proc VolDownCmd {} {
    global Stat Config
    if {$Stat(Playing)} {
	VolumeCmd [expr $Config(CurVol) - 5]
    }
}

proc BassCmd {val} {
    WriteMsg [format "VBAS %d" $val]
}

proc TrebleCmd {val} {
    WriteMsg [format "VTRB %d" $val]
}

#----------------
# display configured tables
#
proc DispTables {} {
    global Config
    set allitems {file time text volume button trace}

    foreach i $allitems {
	pack forget .body.$i
	if {$Config(Disp:$i)} {
	    pack .body.$i -side top -fill x
	} 
    }
}

#
# save configuration and playing mode
#
proc SaveConfig {} {
    global Config ConfigFile
    set fd [open $ConfigFile w]
    if {$fd != ""} {
	puts $fd "global Config"
	foreach i [array names Config] {
	    puts $fd "set Config($i) $Config($i)"
	}
	close $fd
    }
}

#
# load configuration file
#
proc LoadConfig {} {
    global ConfigFile Stat
    catch {source $ConfigFile}
}

#
# from command line
#
proc InitCmdLine {argc argv} {
    global Config
    set Config(Disp:trace) 0
    for {set i 0} {$i < $argc} {incr i} {
	if {[lindex $argv $i] == "-mode"} {
	    incr i
	    set mode [lindex $argv $i]
	    if {$mode == "trace"} {
		set Config(Tracing) 1
		set Config(Disp:trace) 1
	    } elseif {$mode == "shuffle"} {
		set Config(ShufflePlay) 1
	    } elseif {$mode == "normal"} {
		set Config(ShufflePlay) 0
	    } elseif {$mode == "autostart"} {
		set Config(AutoStart) 1
	    } elseif {$mode == "autoexit"} {
		set Config(AutoExit) 1
	    } elseif {$mode == "repeat"} {
		set Config(RepeatPlay) 1
	    }
	}
    }
}


#
# selection callback of the playing file from listbox
#
proc SelectList {lw pos} {
    global Config Stat
    set idx [$lw nearest $pos]
    if {$idx >= 0 && $idx < $Stat(MaxFiles)} {
	if {$Config(ShufflePlay)} {
	    set found [lsearch -exact $Stat(ShuffleList) $idx]
	    if {$found != -1} {
		set Stat(CurIdx) $found
	    }
	} else {
	    set Stat(CurIdx) $idx
	}
	set Stat(Playing) 1
	SelectNumber
    }
}
    

#
#
#
proc OpenFiles {} {
    global Stat
    set files [filebrowser .browser "" "*.mid*"]
    if {$files != ""} {
	set Stat(MaxFiles) [expr $Stat(MaxFiles) + [llength $files]]
	foreach i $files {
	    .body.file.list insert end $i
	    lappend Stat(FileList) $i
	}
	# MakeShuffleList
    }
}

#
#
#
proc CloseFiles {} {
    global Stat
    if {[question "Really Clear List?" 0]} {
	StopCmd
	.body.file.list delete 0 end
	set Stat(MaxFiles) 0
	set Stat(FileList) {}
	set Stat(SuffleList) {}
    }
}

proc ToggleCurFileMode {} {
    global Config Stat
    if {$Config(CurFileMode) == 0} {
	set Config(CurFileMode) 1
    } else {
	set Config(CurFileMode) 0
    }
    SetTime $Stat(LastSec)
}

#----------------
# create main window
#

proc CreateWindow {} {
    global Config Stat

    # menu bar
    frame .menu -relief raised -bd 1
    pack .menu -side top -expand 1 -fill x

    # File menu
    menubutton .menu.file -text "File" -menu .menu.file.m\
	    -underline 0
    menu .menu.file.m
    .menu.file.m add command -label "Open Files" -underline 0\
	    -command OpenFiles
    .menu.file.m add command -label "Clear List" -underline 0\
	    -command CloseFiles
    .menu.file.m add command -label "Save Config" -underline 0\
	    -command SaveConfig
    .menu.file.m add command -label "About" -underline 0\
	    -command {
	information "TkAWEMidi -- Tcl/Tk interface for AWE MIDI player\n  written by T.IWAI"
    }
    .menu.file.m add command -label "Quit" -underline 0\
	    -command QuitCmd

    # Mode menu
    menubutton .menu.mode -text "Mode" -menu .menu.mode.m\
	    -underline 0
    menu .menu.mode.m
    .menu.mode.m add check -label "Repeat" -underline 0\
	    -variable Config(RepeatPlay)
    .menu.mode.m add check -label "Shuffle" -underline 0\
	    -variable Config(ShufflePlay) -command {
	if {$Config(ShufflePlay)} {MakeShuffleList}
    }
    .menu.mode.m add check -label "Auto Start" -underline 5\
	    -variable Config(AutoStart)
    .menu.mode.m add check -label "Auto Exit" -underline 5\
	    -variable Config(AutoExit)
    .menu.mode.m add check -label "Confirm Quit" -underline 0\
	    -variable Config(ConfirmExit)

    # Display menu
    menubutton .menu.disp -text "Display" -menu .menu.disp.m\
	    -underline 0
    menu .menu.disp.m
    .menu.disp.m add check -label "File List" -underline 0\
	    -variable Config(Disp:file) -command "DispTables"
    .menu.disp.m add check -label "Time" -underline 0\
	    -variable Config(Disp:time) -command "DispTables"
    .menu.disp.m add check -label "Messages" -underline 0\
	    -variable Config(Disp:text) -command "DispTables"
    .menu.disp.m add check -label "Volume" -underline 0\
	    -variable Config(Disp:volume) -command "DispTables"
    .menu.disp.m add check -label "Buttons" -underline 0\
	    -variable Config(Disp:button) -command "DispTables"
    if {$Config(Tracing)} {
	.menu.disp.m add check -label "Trace" -underline 1\
		-variable Config(Disp:trace) -command "DispTables"
    }

    # chorus and reverb mode menu
    menubutton .menu.rc -text "Reverb/Chorus" -menu .menu.rc.m\
	    -underline 0
    menu .menu.rc.m
    .menu.rc.m add radio -label "Chorus 1"\
	    -variable Config(Mode:chorus) -value 0 -command "SetChorus"
    .menu.rc.m add radio -label "Chorus 2"\
	    -variable Config(Mode:chorus) -value 1 -command "SetChorus"
    .menu.rc.m add radio -label "Chorus 3"\
	    -variable Config(Mode:chorus) -value 2 -command "SetChorus"
    .menu.rc.m add radio -label "Chorus 4"\
	    -variable Config(Mode:chorus) -value 3 -command "SetChorus"
    .menu.rc.m add radio -label "Feedback"\
	    -variable Config(Mode:chorus) -value 4 -command "SetChorus"
    .menu.rc.m add radio -label "Flanger"\
	    -variable Config(Mode:chorus) -value 5 -command "SetChorus"
    .menu.rc.m add radio -label "Short Delay"\
	    -variable Config(Mode:chorus) -value 6 -command "SetChorus"
    .menu.rc.m add radio -label "Short Delay 2"\
	    -variable Config(Mode:chorus) -value 7 -command "SetChorus"
    .menu.rc.m add separator

    .menu.rc.m add radio -label "Room 1"\
	    -variable Config(Mode:reverb) -value 0 -command "SetReverb"
    .menu.rc.m add radio -label "Room 2"\
	    -variable Config(Mode:reverb) -value 1 -command "SetReverb"
    .menu.rc.m add radio -label "Room 3"\
	    -variable Config(Mode:reverb) -value 2 -command "SetReverb"
    .menu.rc.m add radio -label "Hall 1"\
	    -variable Config(Mode:reverb) -value 3 -command "SetReverb"
    .menu.rc.m add radio -label "Hall 2"\
	    -variable Config(Mode:reverb) -value 4 -command "SetReverb"
    .menu.rc.m add radio -label "Plate"\
	    -variable Config(Mode:reverb) -value 5 -command "SetReverb"
    .menu.rc.m add radio -label "Delay"\
	    -variable Config(Mode:reverb) -value 6 -command "SetReverb"
    .menu.rc.m add radio -label "Panning Delay"\
	    -variable Config(Mode:reverb) -value 7 -command "SetReverb"

    pack .menu.file .menu.mode .menu.disp .menu.rc -side left

    # display body
    frame .body -relief flat
    pack .body -side top -expand 1 -fill both

    # current playing file
    frame .body.curfile -relief flat
    label .body.curfile.title -text "--------" -relief flat -width 25
    button .body.curfile.time -text "00:00" -relief ridge\
	    -command "ToggleCurFileMode"
    pack .body.curfile.title -side left -expand 1
    pack .body.curfile.time -side right
    pack .body.curfile -side top -fill x -expand 1

    # playing files list
    frame .body.file -relief raised -bd 1
    scrollbar .body.file.bar -relief sunken\
	    -command ".body.file.list yview"
    pack .body.file.bar -side right -fill y
    listbox .body.file.list -width 36 -height 10 -relief sunken -bd 2\
	    -yscroll ".body.file.bar set"
    bind .body.file.list <Button-1> {SelectList %W %y}

    pack .body.file.list -side top -expand 1 -fill both

    # time label and scale
    frame .body.time -relief raised -bd 1
    label .body.time.label -text "0:00 / 0:00"
    pack .body.time.label -side top
    scale .body.time.scale -orient horizontal -length 280\
	    -from 0 -to 100 -tickinterval 10
    bind .body.time.scale <ButtonRelease-1> {JumpCmd [%W get]}
    pack .body.time.scale -side bottom -expand 1 -fill x

    # text browser
    frame .body.text -relief raised -bd 1
    scrollbar .body.text.bar -relief sunken\
	    -command ".body.text.buf yview"
    pack .body.text.bar -side right -fill y
    text .body.text.buf -width 36 -height 12 -relief sunken -bd 2\
	    -wrap word -yscroll ".body.text.bar set"
    bind .body.text.buf <Button-1> { }
    bind .body.text.buf <Any-Key> { }
    pack .body.text.buf -side top -expand 1 -fill both
    button .body.text.clear -text "Clear"\
	    -command ClearMsg
    pack .body.text.clear -side bottom

    # volume label and scale
    frame .body.volume -relief raised -bd 1
    #label .body.volume.label -text "Volume:"
    #pack .body.volume.label -side top
    scale .body.volume.scale -orient horizontal -length 280\
	    -from 0 -to 200 -tickinterval 25\
	    -showvalue true -label "Volume"\
	    -command VolumeCmd
    #bind .body.volume.scale <ButtonRelease-1> {VolumeCmd [%W get]}
    pack .body.volume.scale -side top -expand 1 -fill x
    frame .body.volume.mixer -relief flat -bd 0
    scale .body.volume.mixer.bass -orient horizontal -length 135\
	    -from 0 -to 11 -showvalue false -label "Bass"\
	    -variable Config(CurBass)\
	    -command BassCmd
    #bind .body.volume.mixer.bass <ButtonRelease-1> {BassCmd [%W get]}
    scale .body.volume.mixer.treble -orient horizontal -length 135\
	    -from 0 -to 11 -showvalue false -label "Treble"\
	    -variable Config(CurTreble)\
	    -command TrebleCmd
    #bind .body.volume.mixer.treble <ButtonRelease-1> {TrebleCmd [%W get]}
    pack .body.volume.mixer.bass .body.volume.mixer.treble -side left -expand 1 -expand 1
    pack .body.volume.mixer -side bottom -expand 1 -fill x

    # buttons
    global bitmap_path
    frame .body.button -relief raised -bd 1
    button .body.button.play -bitmap @$bitmap_path/play.xbm\
	    -command "PlayCmd"
    button .body.button.stop -bitmap @$bitmap_path/stop.xbm\
	    -command "StopCmd"
    button .body.button.prev -bitmap @$bitmap_path/prev.xbm\
	    -command "PrevCmd"
    button .body.button.back -bitmap @$bitmap_path/back.xbm\
	    -command "BackwardCmd"
    button .body.button.fwrd -bitmap @$bitmap_path/fwrd.xbm\
	    -command "ForwardCmd"
    button .body.button.next -bitmap @$bitmap_path/next.xbm\
	    -command "NextCmd"
    button .body.button.pause -bitmap @$bitmap_path/pause.xbm\
	    -command "PauseCmd"
    button .body.button.quit -bitmap @$bitmap_path/quit.xbm\
	    -command "QuitCmd"
    pack .body.button.play .body.button.pause\
	    .body.button.prev .body.button.back\
	    .body.button.stop\
	    .body.button.fwrd .body.button.next\
	    .body.button.quit\
	    -side left -ipadx 4 -pady 2 -fill x

    if {$Config(Tracing)} {
	# trace display
	TraceCreate
    }
    TraceUpdate

    # pack all items
    DispTables

    focus .
    tk_menuBar .menu .menu.file .menu.mode .menu.disp
    bind . <Key-Right> "ForwardCmd"
    bind . <Key-n> "NextCmd"
    bind . <Key-Left> "BackwardCmd"
    bind . <Key-p> "PrevCmd"
    bind . <Key-Down> "VolDownCmd"
    bind . <Key-v> "VolDownCmd"
    bind . <Key-Up> "VolUpCmd"
    bind . <Key-V> "VolUpCmd"
    bind . <Key-s> "PauseCmd"
    bind . <Key-space> "PauseCmd"
    bind . <Return> "PlayCmd"
    bind . <Key-c> "StopCmd"
    bind . <Key-q> "QuitCmd"

    SetVolume $Config(CurVol)
}
