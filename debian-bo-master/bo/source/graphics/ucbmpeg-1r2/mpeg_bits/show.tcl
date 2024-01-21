# mpeg_bits

# libDir is defined ultimately by BITS_DIR, set in the Makefile
proc Init_Dirs {} {
  global bitmapDir
  global stripedFile
  global FileArray
  global auto_path
  global libDir

  set bitmapDir $libDir/bitmaps
  set stripedFile "$bitmapDir/striped.bit"
  set auto_path [linsert $auto_path 0 $libDir ]
  source $libDir/helpModule

  set FileArray(.filHelp) [format "%s/filHelp" $libDir]
  set FileArray(.ediHelp) [format "%s/ediHelp" $libDir]
  set FileArray(.ovrHelp) [format "%s/ovrHelp" $libDir]
  set FileArray(.sesHelp) [format "%s/sesHelp" $libDir]
  }
global bitmapDir
global stripedFile
global FileArray
global auto_path
global libDir

# global variables
set OptimalBitRate 0
set xstart 0
set ystart 0
set selList {}
set FramesPerSec 0
set GaugeSize 80
set NumFrames 0
set SizeFrame 0
set TypeFrame 0
set FrameCurrentlyDisplayed 0
set startFrame              0
set editList               {}
set editRegionList          {}
set dirtyWork               0
set BoxArray(0,0,0)         0
set DisplayWidth            0
set DisplayHeight           0
set QscaleChange ""
set ColorIncrement 100
set ColorList { ffffff e0e0ff cfcfff c0c0ff aeaeff 9a9aff 8f8ff8 8383f8 \
		    7777f8 6b6bef 5f5fef 5353ef 4949ef 3f3fe8 3535e8 2b2bdf \
		    2121d8 1717c8 }
set bitsInBF ""
set ScrollRectScalingFactor 0
set ScrollRectSizeList {}
set MAX_BARSIZE 100
set AVG_BLOCKSIZE 900
set lastFrame 0
set FrameTypeArray(0) 0
set videoLoaded 1
set indx 0
set sFrame 0
set fFrame 0
set qScale 0
set noHelp 0
set defaultShade ""
set LeftOffset 0
set TopOffset 0
set itemBeingDrawn 0
# IBS = infowin blocksize, size of macroblocks in infowin
set IBS 24
# DBS = display blocksize, size of an actual macroblock in the display
set DBS 16
#####################
# ACTION PROCEDURES #
#####################


#
# misc
#

proc isint { strg } {
    if { [ string match {[0-9]*} $strg ] } {
	return 1
    }
    if { [string match {-[0-9]*}  $strg ]}  {
	return 1
    }
    return 0
}
	 


proc tail {aList} \
{
    set length [llength $aList]
    set index [expr $length-1]
    set entry [lindex $aList $index]

    return $entry
}


proc SetLoop {} \
{
    global loopVar

    ToggleLoop $loopVar
}


proc SetRealTime {} \
{
    global realTimeVar

    ToggleRealTime $realTimeVar
}


#
# drawing of controls
#

proc ScaleHandler {value} \
{
    SetFPS $value
}


proc PercentScaleHandler {value} \
{
    SetPercent $value
}

proc setOffsets { l t } {
    global LeftOffset
    global TopOffset
    set LeftOffset $l
    set TopOffset $t
}

proc MakeBoxes { cols rows } {
    global BoxArray
    global DisplayWidth
    global DisplayHeight
    global ScrollRectScalingFactor
    global MAX_BARSIZE
    global AVG_BLOCKSIZE
    global IBS

    set DisplayWidth $cols
    set DisplayHeight $rows
    set ScrollRectScalingFactor [expr $cols*$rows*$AVG_BLOCKSIZE]

    .infowin.mvFrame.mvCanvas delete box
    for { set col 0 } { $col < $cols } { incr col } {
	for { set row 0 } { $row < $rows } { incr row } {
	    set x [expr $col * $IBS + 1]
	    set y [expr $row * $IBS + 1]
	    set BoxArray($col,$row,0) [.infowin.mvFrame.mvCanvas create rectangle $x $y [expr $x + $IBS] [expr $y + $IBS] -tag box] 
	}
    }
}

proc DrawBox {atCanvas x y colorIndex size } {
    global ColorList
    set color [lindex $ColorList $colorIndex]
    $atCanvas create rectangle $x $y [expr $x + $size] [expr $y + $size] \
	-tag box -width 2 -outline black -fill \#$color
}

proc ModBox {x y size } {
    global BoxArray
    global ColorList	
    global ColorIncrement
    if { $size } {
	set color [lindex $ColorList [expr $size/$ColorIncrement]]
	.infowin.mvFrame.mvCanvas itemconfigure $BoxArray($x,$y,0) -fill \#$color
    } else {
	.infowin.mvFrame.mvCanvas itemconfigure $BoxArray($x,$y,0) -fill ""
    }
    set BoxArray($x,$y,1) $size
}

proc DrawText {atCanvas x y str} {
    $atCanvas create text $x $y -text $str
}

proc PlotBitsSec { bitsSec } {
    global GaugeSize
    
    set atCanvas .infowin.bitGauge.gauge 
    set firstTime 0
    
    if { [string length [$atCanvas gettags gaugeItem]] } {
	$atCanvas move gaugeItem -25 0
	$atCanvas move gaugeLine -25 0
    } else {
	set firstTime 1
    }
  
    if { $bitsSec > $GaugeSize } {
	set bitsSec $GaugeSize
    }
    $atCanvas create rectangle 298 [expr ($GaugeSize + 12) -$bitsSec] 302 [expr ($GaugeSize + 8)-$bitsSec] -tags gaugeItem -fill black
    # remove rectangles that are offscreen
    if { [llength [set itemList [$atCanvas find withtag gaugeItem]]] > 14 } {
	$atCanvas delete [lindex $itemList 0]
    }
    
    # create or modify line.  Delete coords to the line that are offscreen
    if {$firstTime} {
	$atCanvas create line 300 [expr ($GaugeSize + 10)-$bitsSec] 300 [expr ($GaugeSize + 10)-$bitsSec] -tags gaugeLine
    } else {
	set coordList [$atCanvas coords gaugeLine]
	lappend coordList 300 [expr ($GaugeSize + 10)-$bitsSec]
	if { [llength $coordList] > 30 } {
	    set coordList [lreplace $coordList 0 1]
	} 
	eval "$atCanvas coords gaugeLine $coordList"
    }    
}

proc ScrollRect {size frametype} {
    global ScrollRectScalingFactor
    global ScrollRectSizeList
    global MAX_BARSIZE
    
    set atCanvas .infowin.nextframe.frameBar.barCanvas
    # record and adjust size	
	
    lappend ScrollRectSizeList $size
    set size $size*$MAX_BARSIZE
    set size [expr $size/$ScrollRectScalingFactor]
    if {$size > $MAX_BARSIZE } {
	set size $MAX_BARSIZE
    }
    
    if { [string length [ $atCanvas gettags bitRect ]] } {
	$atCanvas move bitRect -25 0
    }
    set boxCol 0
    if { $frametype == 1 } {
	set boxCol pink
    } else {
	if {$frametype == 2 } { 
	    set boxCol tan
	} else {
	    set boxCol SeaGreen
	}
    }
    $atCanvas create rectangle 290 [expr 110-$size] 310 110 -tags bitRect -fill $boxCol
    if { [llength [set itemList [$atCanvas find withtag bitRect]]] > 16 } {
	$atCanvas delete [lindex $itemList 0]
    }
    if { [llength $ScrollRectSizeList] > 16 } {
	set ScrollRectSizeList [lreplace $ScrollRectSizeList 0 0]
    } 
}

proc DestroyObject {atCanvas obj} {
  $atCanvas delete $obj
}

proc RestoreControls {} {
    setControls normal
    .infowin.menuframe.file configure -state normal
    .infowin.menuframe.edit configure -state normal
}
  
proc DrawControlPanel {} {

  global labelForeColor
  global labelBackColor
  global loopVar
  global realTimeVar
  global bitmapDir
  
  option add *Button.font -Adobe-Helvetica-Bold-R-Normal--12-*
  option add *Label.font -Adobe-Helvetica-Bold-R-Normal--12-*
  option add *Checkbutton.font -Adobe-Helvetica-Bold-R-Normal--12-*
  option add *Scale.font -Adobe-Helvetica-Bold-R-Normal--12-*
  
#  option add *Toplevel.background gray
#  option add *Label.foreground white
#  option add *Label.background black
#  option add *Canvas.background black
#  option add *Button.activeBackground white
#  option add *Button.activeForeground black
#  option add *Checkbutton.activeBackground white
#  option add *Checkbutton.activeForeground black
  
  toplevel .p
  wm title .p "Control Panel"
  
  button .p.rewindButton -bitmap @$bitmapDir/rewind -command {
      setControls disabled
      Rewind
      setControls normal
  }
  .p.rewindButton configure -foreground black -background gray81
  
  button .p.stopButton -bitmap @$bitmapDir/stop -command {
      RestoreControls
      Stop
  }
  .p.stopButton configure -foreground black -background gray81
  
  button .p.playPauseButton -bitmap @$bitmapDir/play -command  {
      .p.rewindButton configure -state disabled
      .p.nextFrame configure -state disabled
      .p.playPauseButton configure -state disabled
      .infowin.menuframe.file configure -state disabled
      .infowin.menuframe.edit configure -state disabled
      PlayPause 
  }
  .p.playPauseButton configure -foreground black -background gray81
  
  button .p.nextFrame -bitmap @$bitmapDir/stepfwd -command {
      setControls disabled
      NextFrame 
      setControls normal
  }
  
  .p.nextFrame configure -foreground black -background gray81
  
  pack .p.rewindButton .p.stopButton .p.playPauseButton \
    .p.nextFrame \
    -ipadx 2m -ipady 2m -padx 2m -pady 2m -side left -fill both
    

}

proc setControls { state } {
    .p.rewindButton configure -state $state
    .p.stopButton configure -state $state
    .p.playPauseButton configure -state $state
    .p.nextFrame configure -state $state
}

proc noVideoDisable { } {
    setControls disabled
    .infowin.menuframe.edit configure -state disabled
}

proc videoControlsEnable { } {
    setControls normal
    .infowin.menuframe.edit configure -state normal
}

proc openFile { } {
    if {[winfo exists .open ]} {
	raise .open
	return
    }
    toplevel .open
    wm title .open "Open Video"
    frame .open.f1
    frame .open.f2
    entry .open.f2.e1 -textvariable openFileName -relief sunk
    .open.f2.e1 delete 0 end
    button .open.f2.b1 -text "Cancel" -command { 
	grab release .open
	destroy .open 
    }
    button .open.f2.b2 -text "OK" -command { 
	uplevel \#0 "loadVideo [string trim $openFileName]" 
    }
    scrollbar .open.f1.scroll -command ".open.f1.list yview"
    listbox .open.f1.list -yscroll ".open.f1.scroll set"
    tk_listboxSingleSelect .open.f1.list
    label .open.f1.l1 -text [pwd] -relief raised
    set fileList [glob -nocomplain .* *]
    for { set i 0 } { $i < [llength $fileList]} { incr i } {
	if {[isDirectory [lindex $fileList $i]]} {
	    set fileList [lreplace $fileList $i $i "[lindex $fileList $i]/"]
	}
    }
    eval ".open.f1.list insert end [lsort -ascii $fileList]"
    bind .open.f2.e1 <KeyPress-Return> { 
	uplevel \#0 "loadVideo [string trim $openFileName]"
    }
    bind .open.f1.list <ButtonPress-1> { 
	.open.f1.list select from [.open.f1.list nearest %y]
	.open.f2.e1 delete 0 end
	.open.f2.e1 insert 0 [.open.f1.list get [.open.f1.list nearest %y]]
    }
    bind .open.f1.list <B1-Motion> {
	.open.f1.list select from [.open.f1.list nearest %y]
	.open.f2.e1 delete 0 end
	.open.f2.e1 insert 0 [.open.f1.list get [.open.f1.list nearest %y]]
    }
    bind .open.f1.list <ButtonPress-3> {
	.open.f1.list select clear
	.open.f2.e1 delete 0 end
    }
    bind .open.f1.list <Double-ButtonPress-1> { 
	uplevel \#0 "loadVideo [.open.f1.list get [lindex [.open.f1.list curselection] 0] ]"}
    pack append .open.f1 .open.f1.l1 {top fillx} \
	.open.f1.scroll {right filly} .open.f1.list {left expand fill}
    pack append .open.f2 .open.f2.e1 {left fillx expand} \
	.open.f2.b2 {left} .open.f2.b1 {left}
    pack append .open .open.f2 {top fillx} .open.f1 {bottom expand fill}
    grab set .open
    focus .open
    wm minsize .open  200 200
}

proc loadVideo { filename } {
    global videoLoaded
    if {[string length $filename] == 0} {
	return 
    }
    if { [isDirectory $filename] } {
	cd $filename
	.open.f1.l1 configure -text [pwd]
	.open.f1.list delete 0 end
	set fileList [glob -nocomplain .* *]
	for { set i 0 } { $i < [llength $fileList]} { incr i } {
	    if {[isDirectory [lindex $fileList $i]]} {
		set fileList [lreplace $fileList $i $i "[lindex $fileList $i]/"]
	    }
	}
	eval ".open.f1.list insert end [lsort -ascii $fileList]"
	.open.f2.e1 delete 0 [string length $filename]
	return
    }
    if { ([file exists $filename] && [file readable $filename]) } { 
	# if there's something wrong with the file, SetTitle will set
	# videoLoaded to 0, so we know to leave the dialog up.
	set videoLoaded 1
	uplevel \#0 "SetTitle $filename"
	if {$videoLoaded} {
	    videoControlsEnable
	    grab release .open
	    destroy .open
	} else {
	    noVideoDisable
	    # make sure you're still grabbing - you may have lost grab
	    # c/o a dialog.
	    grab set .open
	}
	return
    }
    tk_dialog .access_trouble "Access Problems" "\"$filename\" is not readable or cannot be found" "" 0 "OK"
    # get the grab back.
    grab set .open
    return
}

proc isDirectory { fileName } {
	llength [glob -nocomplain $fileName/]
}

proc promptSave { funct } {
    set goAhead [tk_dialog .prSave "Save First?" "You have made edits that are not saved to any param file.  Do you want to save first?" "" 0 "Yes" "No" "Cancel"]
    if { $goAhead == 1 } {
	$funct
	return
    } elseif { $goAhead == 2 } {
	return
    } else {
	saveFileAs $funct
    }
}

proc openEditList {} {
    if {[winfo exists .openEd]} {
	raise .openEd
	return 
    }
    toplevel .openEd
    wm title .openEd "Open Edit List"
    frame .openEd.f1
    frame .openEd.f2
    entry .openEd.f2.e1 -textvariable openFileName -relief sunk
    .openEd.f2.e1 delete 0 end
    button .openEd.f2.b1 -text "Cancel" -command { 
	grab release .openEd
	destroy .openEd 
    }
    button .openEd.f2.b2 -text "OK" -command { 
	uplevel \#0 "startLoadEditList [string trim $openFileName]" 
    }
    scrollbar .openEd.f1.scroll -command ".openEd.f1.list yview"
    listbox .openEd.f1.list -yscroll ".openEd.f1.scroll set"
    tk_listboxSingleSelect .openEd.f1.list
    label .openEd.f1.l1 -text [pwd] -relief raised
    set fileList [glob -nocomplain .* *]
    for { set i 0 } { $i < [llength $fileList]} { incr i } {
	if {[isDirectory [lindex $fileList $i]]} {
	    set fileList [lreplace $fileList $i $i "[lindex $fileList $i]/"]
	}
    }
    eval ".openEd.f1.list insert end [lsort -ascii $fileList]"
    bind .openEd.f2.e1 <KeyPress-Return> { 
	uplevel \#0 "startLoadEditList [string trim $openFileName]"
    }
    bind .openEd.f1.list <ButtonPress-1> { 
	.openEd.f1.list select from [.openEd.f1.list nearest %y]
	.openEd.f2.e1 delete 0 end
	.openEd.f2.e1 insert 0 [.openEd.f1.list get [.openEd.f1.list nearest %y]]
    }
    bind .openEd.f1.list <B1-Motion> {
	.openEd.f1.list select from [.openEd.f1.list nearest %y]
	.openEd.f2.e1 delete 0 end
	.openEd.f2.e1 insert 0 [.openEd.f1.list get [.openEd.f1.list nearest %y]]
    }
    bind .openEd.f1.list <ButtonPress-3> {
	.openEd.f1.list select clear
	.openEd.f2.e1 delete 0 end
    }
    bind .openEd.f1.list <Double-ButtonPress-1> { 
	uplevel \#0 "startLoadEditList [.openEd.f1.list get [lindex [.openEd.f1.list curselection] 0] ]"}
    pack append .openEd.f1 .openEd.f1.l1 {top fillx} \
	.openEd.f1.scroll {right filly} .openEd.f1.list {left expand fill}
    pack append .openEd.f2 .openEd.f2.e1 {left fillx expand} \
	.openEd.f2.b2 {left} .openEd.f2.b1 {left}
    pack append .openEd .openEd.f2 {top fillx} .openEd.f1 {bottom expand fill}
    grab set .openEd
    focus .openEd
    wm minsize .openEd  200 200
}

proc startLoadEditList { filename } {

    if {[string length $filename] == 0} {
	return 
    }
    if { [isDirectory $filename] } {
	cd $filename
	.openEd.f1.l1 configure -text [pwd]
	.openEd.f1.list delete 0 end
	set fileList [glob -nocomplain .* *]
	for { set i 0 } { $i < [llength $fileList]} { incr i } {
	    if {[isDirectory [lindex $fileList $i]]} {
		set fileList [lreplace $fileList $i $i "[lindex $fileList $i]/"]
	    }
	}
	eval ".openEd.f1.list insert end [lsort -ascii $fileList]"
	.openEd.f2.e1 delete 0 [string length $filename]
	return
    }
    if {[loadEditList $filename]} {
	grab release .openEd
	destroy .openEd
    } else {
	# there was some error.  Make sure you still have the grab.
	grab set .openEd
    }
    return
}


proc loadEditList { filename } {
    global lastFrame
    global DisplayWidth
    global DisplayHeight
    global editList
    global editRegionList
    global dirtyWork
    
    if { ([file exists $filename] && [file readable $filename]) } {
	set specFile [open $filename r]
	gets $specFile test
	# error if this is not an mpeg_bits generated file
	if {[string compare $test "\/* mpeg_bits generated file */"]} {
	    tk_dialog .bad_edit_list "Bad Edit List File" "\"$filename\" is not an mpeg_bits-generated file." "" 0 "OK"
	    return 0 
	}
	
	#chuck the version line
	gets $specFile versionLineJunk
	#chuck the start comment line
	gets $specFile commentLineJunk

	#get and check the display size
	if {[gets $specFile sizeLine] == -1} {
	    tk_dialog .corrupted_file "Edit List File Corrupted" "\"$filename\" does not have expected format: cannot load edit list." "" 0 "OK"
	    return 0 
	}
	scan $sizeLine " WaH %d %d" width height
	if {($width != $DisplayWidth)||($height != $DisplayHeight)} {
	    tk_dialog .bad_dimensions "Dimension Mismatch" "\"$filename\" contains edits for a $width by $height clip: the current video is $DisplayWidth by $DisplayHeight." "" 0 "OK"
	    return 0
	}

	# get the last frame
	if {[gets $specFile lastFrameLine] == -1} {
	    tk_dialog .corrupted_file "Edit List File Corrupted" "\"$filename\" does not have expected format: cannot load edit list." "" 0 "OK"
	    return 0
	}	
	scan $lastFrameLine " LaF %d" tmp
	if { $tmp > $lastFrame } {
	    set lastFrame $tmp
	}

	#get the edit list
	if {[gets $specFile editListLine] == -1} {
	    tk_dialog .corrupted_file "Edit List File Corrupted" "\"$filename\" does not have expected format: cannot load edit list." "" 0 "OK"
	    return 0
	}	
	set tmp [string range $editListLine 5 end]
	#check it
	foreach el $tmp {
	    if {[llength $el] != 3} {
		tk_dialog .corrupted_file "Edit List File Corrupted" "\"$filename\" does not have expected format: cannot load edit list." "" 0 "OK"
		return 0
	    }
	}
	set editList [concat $editList $tmp]
	
	#get the edit region list
	if {[gets $specFile editRegionListLine] == -1} {
	    tk_dialog .corrupted_file "Edit List File Corrupted" "\"$filename\" does not have expected format: cannot load edit list." "" 0 "OK"
	    return 0
	}
	set tmp [string range $editRegionListLine 5 end]
	#check it
	foreach sublist $tmp {
	    foreach blck $sublist {
		if {[llength $blck] != 4} {
		    tk_dialog .corrupted_file "Edit List File Corrupted" "\"$filename\" does not have expected format: cannot load edit list." "" 0 "OK"
		    return 0
		}
	    }
	}
	set editRegionList [concat $editRegionList $tmp]
	set dirtyWork 1
	return 1
    }
    tk_dialog .access_trouble "Access Problems" "\"$filename\" is not readable or cannot be found" "" 0 "OK"
    return 0
}

proc saveFileAs { funct } {
    global afterFunction

    if { [ winfo exists .save ]} {
	raise .save 
	return 
    }

    set afterFunction $funct
    toplevel .save
    wm title .save "Save Editing Session"
    frame .save.f1
    frame .save.f2
    label .save.l1 -text "Enter the name of the specifics file you"
    label .save.l2 -text "wish to generate from this editing session"
    entry .save.f2.e1 -textvariable fileName -relief sunk
    button .save.f2.b1 -text "Cancel" -command { 
	destroy .save
    }
    button .save.f2.b2 -text "OK" -command { 
	if {[writeFile [string trim $fileName]]} {
	    destroy .save
	    if {[string length $afterFunction]} {
		$afterFunction
	    }
	}
    }
    scrollbar .save.f1.scroll -command ".save.f1.list yview"
    listbox .save.f1.list -yscroll ".save.f1.scroll set"
    tk_listboxSingleSelect .save.f1.list
    label .save.f1.l1 -text [pwd] -relief raised
    set fileList [glob -nocomplain .* *]
    for { set i 0 } { $i < [llength $fileList]} { incr i } {
	if {[isDirectory [lindex $fileList $i]]} {
	    set fileList [lreplace $fileList $i $i "[lindex $fileList $i]/"]
	}
    }
    eval ".save.f1.list insert end [lsort -ascii $fileList]"
    bind .save.f2.e1 <KeyPress-Return> { 
	if {[writeFile [string trim $fileName]] } {
	    destroy .save
	    if {[string length $afterFunction]} {
		$afterFunction
	    }
	} 
    }
    bind .save.f1.list <ButtonPress-1> { 
	.save.f1.list select from [.save.f1.list nearest %y]
	.save.f2.e1 delete 0 end
	.save.f2.e1 insert 0 [.save.f1.list get [.save.f1.list nearest %y]]
    }
    bind .save.f1.list <B1-Motion> {
	.save.f1.list select from [.save.f1.list nearest %y]
	.save.f2.e1 delete 0 end
	.save.f2.e1 insert 0 [.save.f1.list get [.save.f1.list nearest %y]]
    }
    bind .save.f1.list <ButtonPress-3> {
	.save.f1.list select clear
	.save.f2.e1 delete 0 end
    }
    bind .save.f1.list <Double-ButtonPress-1> { 
	if {[writeFile [string trim [.save.f1.list get [lindex [.save.f1.list curselection] 0]]]] } {
	    destroy .save
	    if {[string length $afterFunction]} {
		$afterFunction
	    }
	} 
    }
    pack append .save.f1 .save.f1.l1 {top fillx} .save.f1.scroll {right filly} .save.f1.list {left expand fill}
    pack append .save.f2  .save.f2.e1 {left fillx expand} .save.f2.b2 {left} .save.f2.b1 {left}
    pack append .save .save.l1 {top fillx expand} .save.l2 {top fillx expand} .save.f2 {top fillx} .save.f1 {bottom expand fill}
    grab set .save
    focus .save
    wm minsize .save  200 200
}

proc changeSaveDirectory { fileName } {
    cd $fileName
    .save.f1.l1 configure -text [pwd]
    .save.f1.list delete 0 end
    set fileList [glob -nocomplain .* *]
    for { set i 0 } { $i < [llength $fileList]} { incr i } {
	if {[isDirectory [lindex $fileList $i]]} {
	    set fileList [lreplace $fileList $i $i "[lindex $fileList $i]/"]
	}
    }
    eval ".save.f1.list insert end $fileList"
    .save.f2.e1 delete 0 end
}

proc writeFile {fileName} {
    global editList 
    global editRegionList
    global dirtyWork
    global lastFrame
    global DisplayWidth
    global DisplayHeight
    global FrameTypeArray
    
    #if it's a directory, cd.
    if { [isDirectory $fileName] } {
	changeSaveDirectory $fileName
	return 0
    }
    
    #check to see that a valid name has been given
    if { [string length $fileName] == 0 } {
	tk_dialog .no_name_given "No file name given" "Please enter the name of a file to save to." "" 0 "OK"
	# make sure you still have the grab
	grab set .save
	return 0
    }
    
    if { [file exists $fileName] } {
	set overwrite [tk_dialog .overwrite "Overwrite?" "\"$fileName\" already exists.  Overwrite?" "" 0 "No" "Yes"]
	if { $overwrite == 0 } {
	    #make sure you still have the grab
	    grab set .save
	    return 0
	}
    }
    
    set newFile [open $fileName w+]
    
    # puts list of edits and associated regions in a comment at the top of the
    # file so they can be read back in during later sessions
    puts $newFile "/* mpeg_bits generated file */"
    puts $newFile "version 2"
    puts $newFile "/*"
    puts $newFile " WaH $DisplayWidth $DisplayHeight"
    puts $newFile " LaF $lastFrame"
    puts $newFile " EdL $editList"
    puts $newFile " ERL $editRegionList"
    puts $newFile "*/"

    set numMBs [expr $DisplayWidth * $DisplayHeight]
    
    #get copies of the edit lists
    set tmpEditList $editList
    set tmpEditRegionList $editRegionList
    
    # intitialize the arrays
    for {set i 0} { $i <= $lastFrame } { incr i } {
	set frameArray($i) 0
    }
    for {set i 0} { $i <= $numMBs } { incr i } {
	set MBArray($i) {}
    }
    
    #fill in arrays and process as we go
    for {set i 0} { $i <= $lastFrame } { incr i } {
	
	#if this is the start of an edit, put it in the array
	for { set j 0 } { $j < [llength $tmpEditList] } { incr j} {
	    # get the jth edit
	    set jthEdit [lindex $tmpEditList $j]
	    # if i is the first frame in this edit, note that these frames
	    # contain an edit, and store the edit info in the assocaited 
	    # macroblocks
	    
	    if { [lindex $jthEdit 0] == $i} {
		
		for { set k $i } { $k <= [lindex $jthEdit 1] } {incr k } {
		    set frameArray($k) 1
		}
		
		set jthEdit [linsert $jthEdit 0 $j]
		
		set regionList [lindex $tmpEditRegionList $j]
		for { set a 0 } { $a < [llength $regionList] } { incr a } {
		    set athRegion [lindex $regionList $a] 
		    for {set b [lindex $athRegion 0]} {$b <= [lindex $athRegion 2]} {incr b} {
			for {set c [lindex $athRegion 1]} {$c <= [lindex $athRegion 3]} {incr c} {
			    set MBIndex [expr ($c*$DisplayWidth) + $b]
			    set MBArray($MBIndex) [ lsort [ linsert $MBArray($MBIndex) 0 $jthEdit]]
			}
		    }
		}
	    }
	}
	
	# Now produce the new params for this frame if any edits apply to it.
	# Remove any edits that you're done with as you go.
	if { $frameArray($i) } {
	    puts $newFile "frame $i - -1"
	    set previousQ 0
	    set newQ 0
	    
	    for {set j 0} {$j < $numMBs} {incr j} {
 		set jthList $MBArray($j)
 		set listLength [llength $MBArray($j)]
				
		# figure out what the qscale for the MB should be
		if {$listLength} {
		    set newQ [lindex [lindex $MBArray($j) 0] 3]
		} else {
		    set newQ 0
		}
		
		# if it's changed since the last MB, output a command to
		# change the qscale, and record the new previous Q.
		if { $newQ != $previousQ } {
		    set Qchange [expr $newQ - $previousQ]
		    if {$Qchange > 0} {
			set Qchange "+$Qchange"
		    }
		    puts $newFile "block $j $Qchange"
		    set previousQ $newQ
		}

		# for all of the edits associated with a given;
		# macroblock, is this is the last frame the edit
		# applies to, remove it.
		for {set a [expr $listLength-1]} {$a >= 0} {set a [expr $a-1]} {
		    if { $i == [lindex [lindex $MBArray($j) $a] 2] } {
			set MBArray($j) [lreplace $MBArray($j) $a $a]
		    }
		}
	    }
	}
    }

    close $newFile
    
    set dirtyWork 0
    return 1
}

proc quitNow {} {
    destroy .
}


proc updateLastFrame {} {
    global lastFrame
    global FrameCurrentlyDisplayed
    if { $FrameCurrentlyDisplayed > $lastFrame } {
	set lastFrame $FrameCurrentlyDisplayed
    }
}

proc notePictureType { ptype } {
    global FrameTypeArray
    global TypeFrame
    global FrameCurrentlyDisplayed

    if { $ptype == 1 } {
	set TypeFrame "Frame Type: I"
	set FrameTypeArray($FrameCurrentlyDisplayed) 1
	return
    }
    if { $ptype == 2 } {
	set TypeFrame "Frame Type: P"
	set FrameTypeArray($FrameCurrentlyDisplayed) 2
	return
    }
    if { $ptype == 3 } {
	set TypeFrame "Frame Type: B"
	set FrameTypeArray($FrameCurrentlyDisplayed) 3
	return
    }
}    

proc changeWorkDir { title } {
    set endIndex [string last / $title]
    if { $endIndex != -1 } {
	set title [string range $title 0 $endIndex]
	cd $title
    }
}

proc startSelect { } {
    global startFrame
    global FrameCurrentlyDisplayed
    
    if {[winfo exists .startSel]} {
	raise .startSel
	return
    }

    toplevel .startSel
    wm title .startSel "Frame Selection"
    label .startSel.l1 -text "You are now in frame selection mode."
    set startFrame $FrameCurrentlyDisplayed
    label .startSel.l15 -text [ format "You may select a range of frames to edit, starting with frame %d" $startFrame ] 
    label .startSel.l2 -text "Select \"Mark Last Frame\" once you have advanced"
    label .startSel.l3 -text "to the last frame you want included in this edit."
    frame .startSel.f1
    button .startSel.f1.b1 -text "Mark Last Frame" -command {
	if { $FrameCurrentlyDisplayed < $startFrame } {
	    tk_dialog .badLF "Bad frame range" "Last frame in selected range must be greater than or equal to first frame, which is $startFrame." "" 0 "OK"
	} else {
	    destroy .startSel
	    reEncode  
	}
    }
    button .startSel.f1.b2 -text "Cancel" -command { 
	destroy .startSel 
	deselectBlocks
    }
    pack .startSel.f1.b1 .startSel.f1.b2 -side left -expand 1 -fill x
    pack .startSel.l1 .startSel.l15 .startSel.l2 .startSel.l3 .startSel.f1 -expand 1 -fill x
}

proc reEncode { } {
    global startFrame
    global FrameCurrentlyDisplayed
    global QscaleChange 
   
    if {[winfo exists .reenc]} {
	raise .reenc 
	return 
    }

    if {$QscaleChange==0} {
	set QscaleChange ""
    }
    
    toplevel .reenc
    wm title .reenc "Re-encode Frames"
    label .reenc.l1 -text [format "Set Q-Scale adjustment for selected regions of frames %d - %d" $startFrame $FrameCurrentlyDisplayed]
    frame .reenc.f1
    frame .reenc.f4

    grab .reenc
    
    pack .reenc.l1 -expand 1 -pady 5 -fill x
    label .reenc.f1.l1 -text "Q-Scale Adjustment:" 
    entry .reenc.f1.e1 -textvariable QscaleChange -relief sunk -bd 2
    pack .reenc.f1 -fill x -expand 1 -pady 5
    pack .reenc.f1.l1 -side left
    pack .reenc.f1.e1 -fill x -expand 1 -side left

    button .reenc.f4.b1 -text "Cancel" -command {
	deselectBlocks	
	grab release .reenc
	destroy .reenc	
    }
    button .reenc.f4.b2 -text "Confirm Edit" -command {
	if { [isint $QscaleChange ]} {
	    confirmEdit
	    grab release .reenc
	    destroy .reenc
	} else {
	    tk_dialog .badnumber "Invalid adjustment" "\"$QscaleChange\" is an invalid q-scale adjustment: please input a positive or negative integer." "" 0 "OK"
	}
    }
    pack .reenc.f4 -fill x -expand 1 -pady 5
    pack .reenc.f4.b2 -expand 1 -side left
    pack .reenc.f4.b1 -expand 1 -side left
    
    bind .reenc.f1.e1 <KeyPress-Return> { 
	set QscaleChange [string trim $QscaleChange] 
	if { [isint $QscaleChange ]} {
	    confirmEdit
	    grab release .reenc
	    destroy .reenc
	} else {
	    tk_dialog .badnumber "Invalid adjustment" "\"$QscaleChange\" is an invalid q-scale adjustment: please input a positive or negative integer." "" 0 "OK"
	}
    }

    focus .reenc.f1.e1    
}

proc confirmEdit {} {
    global QscaleChange
    global dirtyWork
    global editList
    global editRegionList
    global selList
    global startFrame
    global FrameCurrentlyDisplayed

    set dirtyWork 1
    if { [string length [string trim [.reenc.f1.e1 get]]] == 0 } { 
	set QscaleChange 0
    }    
    lappend editList "$startFrame $FrameCurrentlyDisplayed $QscaleChange"
    lappend editRegionList $selList
    deselectBlocks
}

proc viewEditList {} {
    global editList    
    global editRegionList

    if {[winfo exists .edli]} {
	raise .edli
	return 
    }

    toplevel .edli
    wm title .edli "Edit List"
    grab .edli
    frame .edli.f1
    frame .edli.f2
    scrollbar .edli.f1.scroll -command ".edli.f1.list yview"
    listbox .edli.f1.list -yscroll ".edli.f1.scroll set"
    tk_listboxSingleSelect .edli.f1.list
    label .edli.f1.l1 -text "Confirmed Edits" -relief raised
    fillEditListBox 
    pack append .edli.f1 .edli.f1.l1 {top fillx} .edli.f1.scroll {right filly} .edli.f1.list {left expand fill}

    frame .edli.f2.
    button .edli.f2.b1 -text "Delete" -command { deleteEdit }
    button .edli.f2.b2 -text "Adjust" -command { adjustEdit }
    button .edli.f2.b3 -text "View Macroblocks" -command { viewEdit }
    button .edli.f2.b4 -text "Raise Priority" -command {raisePriority }
    button .edli.f2.b5 -text "Close" -command {
	grab release .edli
	destroy .edli
    }
    
    bind .edli.f1.list <ButtonPress-3> {
	.edli.f1.list select clear
    }

    pack append .edli.f2 .edli.f2.b1 { left fillx expand } .edli.f2.b2 { left fillx expand} .edli.f2.b3 { left fillx expand} .edli.f2.b4 { left fillx expand} .edli.f2.b5 { left fillx expand} 
    pack append .edli .edli.f1 { top fillx filly expand } .edli.f2 { top fillx }
    wm minsize .edli  200 200
}

proc fillEditListBox {} {
    global editList
    .edli.f1.list delete 0 end
    for { set i 0 } { $i < [llength $editList]} { incr i } {
	set listItem [lindex $editList $i]
	set entry [eval "format \"frames %d-%d  Adjustment: %s\" $listItem"]
	.edli.f1.list insert end $entry
    }
}

proc deleteEdit {} {
    global editList
    global editRegionList
    global dirtyWork

    if { [llength [.edli.f1.list curselection]] == 0 } {
	return
    }
    set indx [ lindex [.edli.f1.list curselection] 0]
    .edli.f1.list delete $indx
    set dirtyWork 1
    set editList [lreplace $editList $indx $indx]
    set editRegionList [lreplace $editRegionList $indx $indx] 
    .edli.f1.list select clear
}


proc adjustEdit {} {
    global editList
    global editRegionList
    global dirtyWork
    global indx
    global sFrame
    global fFrame
    global qScale

    if {[winfo exists .adjust]} {
	raise .adjust 
	return 
    }

    if { [llength [.edli.f1.list curselection]] == 0 } {
	return
    }
    
    set indx [ lindex [.edli.f1.list curselection] 0]
    set editToAdjust [ lindex $editList $indx ]
    set sFrame [lindex $editToAdjust 0]
    set fFrame [lindex $editToAdjust 1]
    set qScale [lindex $editToAdjust 2]
    
    toplevel .adjust
    wm title .adjust "Adjust Edit"
    label .adjust.l1 -text "Adjust a previously defined edit."
    pack .adjust.l1 -expand 1 -pady 5 -fill x
    frame .adjust.f1
    pack .adjust.f1 -fill x -expand 1 -pady 5
    frame .adjust.f2
    pack .adjust.f2 -fill x -expand 1 -pady 5
    frame .adjust.f3
    pack .adjust.f3 -fill x -expand 1 -pady 5
    frame .adjust.f4
    pack .adjust.f4 -fill x -expand 1 -pady 5

    grab .adjust

    label .adjust.f1.l1 -text "Start frame:"
    entry .adjust.f1.e1 -textvariable sFrame
    pack .adjust.f1.l1 -side left
    pack .adjust.f1.e1 -fill x -expand 1 -side left
    label .adjust.f2.l1 -text "Final frame:"
    entry .adjust.f2.e1 -textvariable fFrame
    pack .adjust.f2.l1 -side left
    pack .adjust.f2.e1 -fill x -expand 1 -side left
    label .adjust.f3.l1 -text "Q-scale adjustment:"
    entry .adjust.f3.e1 -textvariable qScale
    pack .adjust.f3.l1 -side left
    pack .adjust.f3.e1 -fill x -expand 1 -side left
 
    button .adjust.f4.b1 -text "Cancel" -command {
	grab release .adjust
	destroy .adjust
    }
    button .adjust.f4.b2 -text "Confirm Adjustment" -command {
	if { !([isint $sFrame] && [isint $fFrame] && [isint $qScale]) } {
	    tk_dialog .badEntries "Invalid adjustments" "Entries must all be integers." "" 0 "OK"
	} elseif { !(($sFrame >= 0) && ($fFrame >= 0)) } {
	    tk_dialog .badFrameNum "Bad frame numbers" "Frame numbers must be positive." "" 0 "OK"
	} elseif { $sFrame > $fFrame } {
	    tk_dialog .badFrameRange "Bad frame range" "Start frame number must be less than final frame number" "" 0 "OK"
	} else {
	    set editList [lreplace $editList $indx $indx "$sFrame $fFrame $qScale"]
	    grab release .adjust
	    destroy .adjust
	    fillEditListBox
	}
    }
    
    pack .adjust.f4.b2 -expand 1 -side left
    pack .adjust.f4.b1 -expand 1 -side left
    
    bind .adjust.f1.e1 <KeyPress-Return> {
	set sFrame [string trim $sFrame]
	focus .adjust.f2.e1
    }
    bind .adjust.f1.e1 <Tab> {
	set sFrame [string trim $sFrame]
	focus .adjust.f2.e1
    }
    bind .adjust.f2.e1 <KeyPress-Return> {
	set sFrame [string trim $sFrame]
	focus .adjust.f3.e1
    }
    bind .adjust.f2.e1 <Tab> {
	set sFrame [string trim $sFrame]
	focus .adjust.f3.e1
    }
    bind .adjust.f3.e1 <KeyPress-Return> {
	set sFrame [string trim $sFrame]
	focus .adjust.f1.e1
    }
    bind .adjust.f3.e1 <Tab> {
	set sFrame [string trim $sFrame]
	focus .adjust.f1.e1
    }
}


proc raisePriority { } {
    global editRegionList
    global editList
    global dirtyWork

    if { [llength [.edli.f1.list curselection]] == 0 } {
	return
    }
    set indx [ lindex [.edli.f1.list curselection] 0]
    
    # we can't raise the priority if it's already top priority
    if {!$indx } {
	return 
    }

    set dirtyWork 1
    set tmp [lreplace $editList $indx $indx]
    set editList [linsert $tmp [expr $indx-1] [lindex $editList $indx]]
    set tmp [lreplace $editRegionList $indx $indx]
    set editRegionList [linsert $tmp [expr $indx-1] [lindex $editRegionList $indx]]
    fillEditListBox
    .edli.f1.list select from [expr $indx-1]
}

proc viewEdit {} {
    global editRegionList
    
    if { [llength [.edli.f1.list curselection]] == 0 } {
	return
    }
    set indx [ lindex [.edli.f1.list curselection] 0]
    set regionList [ lindex $editRegionList $indx ]
    displaySelEdit $regionList
}
    
proc displaySelEdit { regionList } {
    global DisplayWidth
    global DisplayHeight
    global stripedFile

    if {[winfo exists .diEd]} {
	raise .diEd
	return
    }

    toplevel .diEd
    grab .diEd
    wm title .diEd "Macroblocks Selected for Modified Q-scale"
    canvas .diEd.can -height [expr $DisplayHeight*15] -width [expr $DisplayWidth*15]
    button .diEd.b -text "OK" -command {
	grab release .diEd
	destroy .diEd
    }
    
    for { set col 0 } { $col < $DisplayWidth } { incr col } {
	for { set row 0 } { $row < $DisplayHeight } { incr row } {
	    set x [expr $col * 15]
	    set y [expr $row * 15]
	    .diEd.can create rectangle $x $y [expr $x + 15] [expr $y + 15] \
		-fill white 
	}
    }
    for { set i 0 } { $i < [llength $regionList] } { incr i } {
	set coor [lindex $regionList $i]
	.diEd.can addtag chosen enclosed [expr 15*[lindex $coor 0]-1] \
	    [expr 15*[lindex $coor 1]-1] [expr 15*[lindex $coor 2]+16] \
	    [expr 15*[lindex $coor 3]+16]
    }
    .diEd.can  itemconfigure chosen -stipple @$stripedFile
    pack .diEd.can -side top -padx 4m -pady 4m -expand 1 
    pack .diEd.b -side top -expand 1
}




proc startBox {xcoor ycoor} {
    global xstart
    global ystart
    set xstart $xcoor
    set ystart $ycoor
    .infowin.mvFrame.mvCanvas create rectangle $xcoor $ycoor $xcoor $ycoor
}

proc drawRect { curItem xcoor ycoor} {
    global xstart
    global ystart 
    .infowin.mvFrame.mvCanvas coords $curItem $xcoor $ycoor $xstart $ystart
}

proc highlightBoxes { itemNo } {
    global selList
    global stripedFile
    global IBS

    set coors [ .infowin.mvFrame.mvCanvas coords $itemNo]
    set x1 [expr [lindex $coors 0] - ($IBS - 1)]
    set y1 [expr [lindex $coors 1] - ($IBS - 1)]
    set x2 [expr [lindex $coors 2] + ($IBS - 1)]
    set y2 [expr [lindex $coors 3] + ($IBS - 1)]
    .infowin.mvFrame.mvCanvas addtag chosen enclosed $x1 $y1 $x2 $y2 
    .infowin.mvFrame.mvCanvas itemconfigure chosen -stipple @$stripedFile
    .infowin.mvFrame.mvCanvas delete $itemNo
    if { $x1 < 0 } { 
	set x1 0
    }
    if { $y1 < 0 } {
	set y1 0
    }
    set x1 [ expr ($x1+($IBS-1))/$IBS ]
    set y1 [ expr ($y1+($IBS-1))/$IBS ]
    set x2 [ expr ($x2-($IBS-1))/$IBS ]
    set y2 [ expr ($y2-($IBS-1))/$IBS ]
    
    lappend selList "$x1 $y1 $x2 $y2"
}

proc deselectBlocks {} {
    global selList
    set selList {}
    .infowin.mvFrame.mvCanvas itemconfigure chosen -stipple ""
    .infowin.mvFrame.mvCanvas dtag chosen
}

proc setUpGauge {} {
    global FramesPerSec
    global OptimalBitRate
  
    .infowin.bitGauge.legend.canv delete infoText
    .infowin.bitGauge.legend.canv create text 165 30 -text [ format "Total bits used over the past second,\nat %d frames/sec\n(not displayed until after %d frames)" $FramesPerSec $FramesPerSec] -tags infoText
    .infowin.bitGauge.legend.canv create text 165 70 -text [format "Target bitrate: %d bits/sec" $OptimalBitRate] -tags infoText
}

proc getRect { x y } {
    set can .infowin.nextframe.frameBar.barCanvas
    set chosenRect [$can find overlapping $x $y $x $y]
    if {[llength $chosenRect] == 0} {
	#< if you selected nothing, return
	return -1
    }
    for { set i 0 } { $i < [llength $chosenRect]} {incr i} {
	# look thru list of things you selected for a scroll rect
	set taglist [$can gettags [lindex $chosenRect $i]]
	if {[lsearch $taglist "bitRect"] != -1 } {
	    # once you find one, return it's index in the list of
	    # displayed rectangles
	    set rectList [$can find withtag "bitRect"]
	    return [lsearch $rectList [lindex $chosenRect $i]]
	}
    }
    return -1
}


proc DrawInfoWin {} {
    global GaugeSize
    global NumFrames
    global TypeFrame
    global SizeFrame
    global FileArray

    global bitsInBF
    global DisplayWidth
    global DisplayHeight
    global ScrollRectSizeList
    global defaultShade
    global itemBeingDrawn
    global IBS

    option add *Button.font -Adobe-Helvetica-Bold-R-Normal--12-*
    option add *Label.font -Adobe-Helvetica-Bold-R-Normal--12-*
    
    toplevel .infowin
    wm title .infowin "mpeg_bits (version 1.0)"
    set defaultShade [lindex [.infowin configure -background] 4]
    
    #######################
    # Menu Stuff          #
    #######################
    set mf .infowin.menuframe
    frame $mf -relief raised -bd 2
    pack $mf -side top -fill x -expand 1
    
    menubutton $mf.file -text "File" -menu $mf.file.fmenu 
    menubutton $mf.edit -text "Edit" -menu $mf.edit.emenu 
    menubutton $mf.help -text "Help" -menu $mf.help.hmenu 
    pack $mf.file -side left 
    pack $mf.edit -side left
    label $mf.filler
    $mf.filler configure -background [ lindex [$mf.file configure -background] 4]
    pack $mf.filler -side left -expand 1 -fill x
    pack $mf.help -side right -fill x 
    
    menu $mf.file.fmenu -postcommand {
	if {!$videoLoaded} {
	    .infowin.menuframe.file.fmenu entryconfigure 1 -state disabled
	} else {
	    .infowin.menuframe.file.fmenu entryconfigure 1 -state normal
	} 
    }
    
    $mf.file.fmenu add command -label "Open Video" -command { 
	if { $dirtyWork } {
	    promptSave openFile
	} else {
	    openFile
	}
    }
    $mf.file.fmenu add command -label "Open Edit List" -command { 
	openEditList 
    }
    $mf.file.fmenu add command -label "Save" -command { 
	saveFileAs "" 
    }
    $mf.file.fmenu add command -label "Quit" -command { 
	if { $dirtyWork } {
	    promptSave quitNow
	} else {
	    quitNow
	}    
    }
    
    menu $mf.edit.emenu
    $mf.edit.emenu add command -label "Mark First Frame" -command { startSelect }
    $mf.edit.emenu add command -label "View Edit List" -command { viewEditList }
    
    menu $mf.help.hmenu
    $mf.help.hmenu add command -label "Overview" -command {
	getHelp .ovrHelp
    }
    $mf.help.hmenu add command -label "Sample Session" -command {
	getHelp .sesHelp
    }
    $mf.help.hmenu add command -label "The File Menu" -command {
	getHelp .filHelp
    }
    $mf.help.hmenu add command -label "The Edit Menu" -command {
	getHelp .ediHelp
    }
	    
    #############################
    # Bits in block/frame label #
    #############################

    label $mf.bfLabel -textvariable bitsInBF
    pack $mf.bfLabel -side right -padx 5m

    ########################
    # Motion Vectors stuff #
    ########################
    frame .infowin.mvFrame -bd 5
    pack .infowin.mvFrame -side top -fill both -ipadx 5m -ipady 5m
    
    canvas .infowin.mvFrame.mvCanvas -width 200 -height 100
    pack .infowin.mvFrame.mvCanvas -expand 1 
    
    bind .infowin.mvFrame.mvCanvas <ButtonPress-1> { 
	set itemBeingDrawn [startBox %x %y] 
    }
    bind .infowin.mvFrame.mvCanvas <B1-Motion> { 
	drawRect $itemBeingDrawn %x %y 
    }
    bind .infowin.mvFrame.mvCanvas <ButtonRelease-1> { 
	highlightBoxes $itemBeingDrawn 
    }
    bind .infowin.mvFrame.mvCanvas <ButtonPress-3> { 
	deselectBlocks 
    }
    bind .infowin.mvFrame.mvCanvas <ButtonPress-2> { 
	set xindex [expr %x/$IBS]
	set yindex [expr %y/$IBS]
	set bitsInBF "Bits in macroblock ($xindex,$yindex): $BoxArray($xindex,$yindex,1)"
    }
    bind .infowin.mvFrame.mvCanvas <B2-Motion> { 
	set xindex [expr %x/$IBS]
	set yindex [expr %y/$IBS]
	if { $xindex < 0 } {
	    set bitsInBF ""
	} elseif { $xindex >= $DisplayWidth } {
	    set bitsInBF ""
	} elseif { $yindex < 0 } {
	    set bitsInBF ""
	} elseif { $yindex >= $DisplayHeight } {
	    set bitsInBF ""
	} else {
	    set bitsInBF "Bits in macroblock ($xindex,$yindex): $BoxArray($xindex,$yindex,1)"
	}
    }
    bind .infowin.mvFrame.mvCanvas <B2-ButtonRelease> {
	set bitsInBF ""
    }

    # Legend stuff
    frame .infowin.mvLegend -bd 5
    pack .infowin.mvLegend -side top -fill both -ipadx 1m -ipady 1m
    
    canvas .infowin.mvLegend.legendCanvas -width 400 -height 60
    pack .infowin.mvLegend.legendCanvas -expand 1
    
    set xpos 25
    DrawBox .infowin.mvLegend.legendCanvas $xpos 20  0  18
    DrawText .infowin.mvLegend.legendCanvas $xpos 49 "0"
    set xpos [expr $xpos + 18]
    DrawBox .infowin.mvLegend.legendCanvas $xpos 20 1 18
    set xpos [expr $xpos + 18]
    DrawBox .infowin.mvLegend.legendCanvas $xpos 20 2 18
    set xpos [expr $xpos + 18]
    DrawBox .infowin.mvLegend.legendCanvas $xpos 20 3  18
    DrawText .infowin.mvLegend.legendCanvas $xpos 49 "300"
    set xpos [expr $xpos + 18]
    DrawBox .infowin.mvLegend.legendCanvas $xpos 20 4 18
    set xpos [expr $xpos + 18]
    DrawBox .infowin.mvLegend.legendCanvas $xpos 20 5 18
    set xpos [expr $xpos + 18]
    DrawBox .infowin.mvLegend.legendCanvas $xpos 20 6  18
    DrawText .infowin.mvLegend.legendCanvas $xpos 49 "600"
    set xpos [expr $xpos + 18]
    DrawBox .infowin.mvLegend.legendCanvas $xpos 20 7 18
    set xpos [expr $xpos + 18]
    DrawBox .infowin.mvLegend.legendCanvas $xpos 20 8 18
    set xpos [expr $xpos + 18]
    DrawText .infowin.mvLegend.legendCanvas $xpos 10 "bits/macroblock"
    DrawBox .infowin.mvLegend.legendCanvas $xpos 20 9  18
    DrawText .infowin.mvLegend.legendCanvas $xpos 49 "900"
    set xpos [expr $xpos + 18]
    DrawBox .infowin.mvLegend.legendCanvas $xpos 20 10 18
    set xpos [expr $xpos + 18]
    DrawBox .infowin.mvLegend.legendCanvas $xpos 20 11 18
    set xpos [expr $xpos + 18]
    DrawBox .infowin.mvLegend.legendCanvas $xpos 20 12  18
    DrawText .infowin.mvLegend.legendCanvas $xpos 49 "1200"
    set xpos [expr $xpos + 18]
    DrawBox .infowin.mvLegend.legendCanvas $xpos 20 13 18
    set xpos [expr $xpos + 18]
    DrawBox .infowin.mvLegend.legendCanvas $xpos 20 14 18
    set xpos [expr $xpos + 18]
    DrawBox .infowin.mvLegend.legendCanvas $xpos 20 15 18
    DrawText .infowin.mvLegend.legendCanvas $xpos 49 "1500"
    set xpos [expr $xpos + 18]
    DrawBox .infowin.mvLegend.legendCanvas $xpos 20 16 18
    set xpos [expr $xpos + 18]
    DrawBox .infowin.mvLegend.legendCanvas $xpos 20 17 18
    set xpos [expr $xpos + 18]
    DrawText .infowin.mvLegend.legendCanvas $xpos 49 "1800+"
    unset xpos
    
    # The infoWindow
    frame .infowin.nextframe
    pack .infowin.nextframe -side top -fill both
    
    set frm .infowin.nextframe.iwin
    frame $frm -relief ridge -bd 5 -width 220 -height 120
    pack $frm -side left -fill both -expand 1 -ipadx 1m -ipady 1m
    
    set NumFrames "Current Frame: 0"
    label $frm.l1 -textvariable NumFrames
    pack $frm.l1 -side top -expand 1

    set TypeFrame "Frame Type:"
    label $frm.l2 -textvariable TypeFrame
    pack $frm.l2 -side top -expand 1
    
    set SizeFrame "Frame Size: 0"
    label $frm.l3 -textvariable SizeFrame
    pack $frm.l3 -side top -expand 1

    # put up interesting info

    # The Frame Bar
    set frm .infowin.nextframe.frameBar
    frame $frm -relief ridge -bd 5
    pack $frm -side right -ipadx 1m -ipady 1m
    
    frame $frm.legend 
    pack $frm.legend -side left 
    
    canvas $frm.legend.canv -width 80 -height 120
    pack $frm.legend.canv -expand 1 
    
    $frm.legend.canv create text 40 40 -text "Relative\nFrame\nSizes"

    $frm.legend.canv create rectangle 5 86 25 110 -fill pink
    $frm.legend.canv create rectangle 30 86 50 110 -fill tan
    $frm.legend.canv create rectangle 55 86 75 110 -fill SeaGreen 
    
    $frm.legend.canv create text 15 98 -text "I"
    $frm.legend.canv create text 40 98 -text "P"
    $frm.legend.canv create text 65 98 -text "B"
    
    canvas $frm.barCanvas -width 314 -height 120 -relief sunk 
    pack $frm.barCanvas -side right
   
    bind $frm.barCanvas <ButtonPress-2> {
	set selectedRect [getRect %x %y]
	if {$selectedRect == -1 } {
	    set bitsInBF ""
	} else {
	    set barsize [lindex $ScrollRectSizeList $selectedRect]
	    set bitsInBF "Bits in frame: $barsize"
	}
    }
    
    bind $frm.barCanvas <B2-Motion> {
	set selectedRect [getRect %x %y]
	if {$selectedRect == -1} {
	    set bitsInBF ""
	} else {
	    set barsize [lindex $ScrollRectSizeList $selectedRect]
	    set bitsInBF "Bits in frame: $barsize"
	}
    }
	    
    bind $frm.barCanvas <B2-ButtonRelease> {
	set bitsInBF ""
    }


	
    $frm.barCanvas create polygon 295 120 300 110 305 120 300 120 -fill black
    $frm.barCanvas create polygon 295 0 300 10 305 0 300 0 -fill black
    $frm.barCanvas create line 300 10 300 110 -fill red 
    
    # The bitrate gauge
    set frm .infowin.bitGauge
    frame $frm -relief ridge -bd 5
    pack $frm -side top -fill both -ipadx 1m -ipadx 1m
    
    frame $frm.legend
    pack $frm.legend -side left
    
    canvas $frm.legend.canv -height [expr $GaugeSize + 20] -width 300
    pack $frm.legend.canv
    
    # sample lines
    $frm.legend.canv create line 10 20 35 20
    $frm.legend.canv create rectangle 8 18 12 22 -fill black
    $frm.legend.canv create rectangle 33 18 37 22 -fill black
    # The text is written in setUpGauge after we've parsed
    # off the frames/sec
    
    $frm.legend.canv create line 10 70 35 70 -fill red

    canvas $frm.gauge -width 314 -height [expr $GaugeSize + 20] -relief sunk 
    pack $frm.gauge -side right
    
    $frm.gauge create polygon 295 [expr $GaugeSize + 20] 300 [expr $GaugeSize + 10] 305 [expr $GaugeSize + 20] 295 [expr $GaugeSize + 20] -fill black
    $frm.gauge create polygon 295 0 300 10 305 0 295 0 -fill black
    $frm.gauge create line 300 10 300 [expr $GaugeSize + 10] -fill red
    $frm.gauge create line 0 [expr $GaugeSize/2 + 10] 314 [expr $GaugeSize/2 + 10] -fill red
}  

proc AnnounceError { msg } {
    tk_dialog .error_msg "Error in MPEG stream" "$msg  File cannot be loaded as an MPEG stream." "" 0 "OK"
}

proc ClearState {} {
    global OptimalBitRate
    global selList
    global FrameCurrentlyDisplayed
    global editList
    global editRegionList
    global dirtyWork
    global QscaleChange
    global bitsInBF
    global ScrollRectSizeList
    global lastFrame
    global videoLoaded
    set OptimalBitRate 0
    set selList {}
    set FrameCurrentlyDisplayed 0
    set editList               {}
    set editRegionList          {}
    set dirtyWork               0
    set QscaleChange ""
    set bitsInBF ""
    set ScrollRectSizeList {}
    set lastFrame 0
    set videoLoaded 1
    .infowin.nextframe.frameBar.barCanvas delete bitRect
    .infowin.mvFrame.mvCanvas delete box
    .infowin.bitGauge.gauge delete gaugeItem gaugeLine     
    .infowin.bitGauge.legend.canv delete infoText
}

proc ClearGauges {} {
    global ScrollRectSizeList
    .infowin.nextframe.frameBar.barCanvas delete bitRect
    .infowin.bitGauge.gauge delete gaugeItem gaugeLine     
    set ScrollRectSizeList {}
}

proc adjustCoord { oldCoord offset} {
    global IBS
    global DBS
    set x [expr $oldCoord - $offset]
    set x [expr (($oldCoord - $offset)*$IBS)/$DBS]
    return $x
}

proc inDisplay { x y } {
    global DBS
    global LeftOffset 
    global TopOffset
    global DisplayWidth
    global DisplayHeight
    
    if { $x < $LeftOffset } {
	return 0
    } 
    if { $y < $TopOffset } {
	return 0
    }
    if { $x > [expr $LeftOffset + ($DisplayWidth*$DBS)] } {
	return 0
    }
    if { $y > [expr $TopOffset + ($DisplayHeight*$DBS)] } {
	return 0
    }
    return 1;
}


proc BindDisplayWindow {} {
    global itemBeingDrawn
    global LeftOffset
    global TopOffset
    
    bind . <ButtonPress-1> { 
	set xloc %x
	set yloc %y
	if { [inDisplay $xloc $yloc] } {
	    set xloc [adjustCoord $xloc $LeftOffset]
	    set yloc [adjustCoord $yloc $TopOffset]
	    set itemBeingDrawn [startBox $xloc $yloc]
	} else {
	    set itemBeingDrawn -1
	}
    }
    bind . <B1-Motion> {
	if { $itemBeingDrawn != -1 } {
	    set xloc [adjustCoord %x $LeftOffset]
	    set yloc [adjustCoord %y $TopOffset]
	    drawRect $itemBeingDrawn $xloc $yloc 
	}
    }
    bind . <ButtonRelease-1> { 
	if { $itemBeingDrawn != -1 } {
	    highlightBoxes $itemBeingDrawn 
	}
    }
    bind . <ButtonPress-3> { 
	deselectBlocks 
    }
}
    
proc Init_Win {} {
  global defaultShade

  . configure -background gray
  PercentScaleHandler 100
  DrawControlPanel
  DrawInfoWin 
  BindDisplayWindow
  . configure -background $defaultShade
}


