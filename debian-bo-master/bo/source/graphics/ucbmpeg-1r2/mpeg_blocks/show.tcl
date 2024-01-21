# mpeg_blocks


###################
# COLOR CONSTANTS #
###################

#set activeBackColor blue
#set activeForeColor yellow
#set inactiveBackColor black
#set inactiveForeColor white
#set labelBackColor white
#set labelForeColor blue

# toggle selection colors

set selectForeColor white
set selectBackColor brown

###############################################
# Some variables interacte with mpeg_analyzer #
###############################################

# MPEG
set Mpeg(fps) 30	;# Frame speed
set Mpeg(width) 0	;# Frame width
set Mpeg(height) 0	;# Frame height
set Mpeg(frame) 0	;# Total number of frames
set Mpeg(avgByteFrame) 0	;# Average frame byte size
set Mpeg(avgBitFrame) 0		;# Average frame bit size
set Mpeg(decode) 0	;# Total decode time
set Mpeg(avgDecode) 0	;# Average frame decode time

# Frame
set Frame(num) 0 	;# Total number of macroblocks in frame
set Frame(mb_width) 0	;# Macroblock width
set Frame(mb_height) 0	;# Macroblock height
set Frame(current) 0	;# Current frame number
set Frame(type) ""	;# Frame type
set Frame(byte) 0	;# Frame byte size (total byte size of mb)
set Frame(bit) 0	;# Frame bit size (total bit size of mb)
set Frame(decode) 0	;# Frame decode time
set Frame(fpel) full    ;# forward full/half pixel used in this frame
set Frame(bpel) full    ;# backward full/half pixel used in this frame

# I Frame
set IFrame(num) 0	;# Total number of I frame
set IFrame(byte) 0	;# Byte size
set IFrame(bit) 0	;# Bit size

# P Frame
set PFrame(num) 0	;# Total number of P frame
set PFrame(byte) 0	;# Byte size
set PFrame(bit) 0	;# Bit size

# B Frame
set BFrame(num) 0	;# Total number of B frame
set BFrame(byte) 0	;# Byte size
set BFrame(bit) 0	;# Bit size

# I Block 
set IBlock(num) 0	;# Total number of I block
set IBlock(byte) 0	;# Avg byte size
set IBlock(bit) 0 	;# Avg bit size
set IBlock(color) "red" ;# I Block color (for interface purpose)

# P Block 
set PBlock(num) 0	;# Total number of P block
set PBlock(byte) 0	;# Avg byte size
set PBlock(bit) 0	;# Avg bit size
set PBlock(color) "blue"	;# P Block color (for interface purpose)


# B Block 
set BBlock(num) 0	;# Total number of B block
set BBlock(byte) 0   	;# Avg byte size
set BBlock(bit) 0	;# Avg bit size
set BBlock(color) "green" 	;# B Block color (for interface purpose)


# Bi Block 
set BiBlock(num) 0	;# Total number of Bi block
set BiBlock(byte) 0 	;# Avg byte size 
set BiBlock(bit) 0	;# Avg bit size
set IBlock(color) "yellow" 	;# Bi Block color (for interface purpose)

# Skipped Block
set SkipBlock(num) 0	;# Total number of Skipped block
set IBlock(color) "gray"

# Motion Vector
set FMV(0,0) ""
set BMV(0,0) ""

#####################
# ACTION PROCEDURES #
#####################

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

proc DrawBox {atCanvas x y color {tagname "noname"} } {
  $atCanvas create rectangle $x $y [expr $x + 24] [expr $y + 24] \
    -tag "$tagname box" -width 2 -outline black -fill $color 
}

proc DrawDot {atCanvas x y color {tagname "noname"} } {
  $atCanvas create oval [expr $x-2] [expr $y-2] \
    [expr $x+2] [expr $y+2] -tag $tagname -width 1 \
    -fill $color
}

proc ChangeColor {atCanvas tagname color} {
  $atCanvas itemconfigure $tagname -fill $color
}

proc DrawText {atCanvas x y str {tagname "text"} } {
  $atCanvas create text $x $y -tag $tagname -text $str
}

proc DestroyObject {atCanvas obj} {

  $atCanvas delete $obj
}
  
proc DrawControlPanel {} {

  global labelForeColor
  global labelBackColor
  global bitmapDir
  
  option add *Button.font -Adobe-Helvetica-Bold-R-Normal--12-*
  option add *Label.font -Adobe-Helvetica-Bold-R-Normal--12-*
  option add *Checkbutton.font -Adobe-Helvetica-Bold-R-Normal--12-*
  option add *Scale.font -Adobe-Helvetica-Bold-R-Normal--12-*
  
  option add *Toplevel.background gray
  option add *Label.foreground white
  option add *Label.background black
  option add *Canvas.background black
  option add *Button.activeBackground white
  option add *Button.activeForeground black
  option add *Checkbutton.activeBackground white
  option add *Checkbutton.activeForeground black
  
  toplevel .p
  wm title .p "Control Panel"
  
  button .p.quitButton -text "Quit" -relief raised -command \
  {
    destroy .
  }
  .p.quitButton configure -foreground black -background gray81
  
#  button .p.rewindButton -bitmap @$bitmapDir/rewind -command Rewind
#  .p.rewindButton configure -foreground black -background gray81
  
  button .p.stopButton -bitmap @$bitmapDir/stop -command Stop
  .p.stopButton configure -foreground black -background gray81
  
  button .p.playPauseButton -bitmap @$bitmapDir/play \
    -command PlayPause
  .p.playPauseButton configure -foreground black -background gray81
  
  button .p.nextFrame -bitmap @$bitmapDir/stepfwd \
    -command NextFrame
  .p.nextFrame configure -foreground black -background gray81
  
  pack .p.stopButton .p.playPauseButton \
    .p.nextFrame .p.quitButton \
    -ipadx 2m -ipady 2m -padx 2m -pady 2m -side left -fill both
}

proc DrawInfoWin {} {

  global labelForeColor
  global labelBackColor
  global realTimeVar

  global Mpeg
  global Frame
  global IFrame
  global PFrame
  global BFrame
  global IBlock
  global PBlock
  global BBlock
  global BiBlock
  global SkipBlock
  global FMV
  global BMV

  option add *Toplevel.background gray
  option add *Button.font -Adobe-Helvetica-Bold-R-Normal--12-*
  option add *Label.font -Adobe-Helvetica-Bold-R-Normal--12-*
  option add *Label.foreground black
  option add *Label.background gray
  option add *Frame.background gray
  option add *Canvas.background gray
  option add *Button.activeBackground white
  option add *Button.activeForeground black
  option add *Checkbutton.activeBackground white
  option add *Checkbutton.activeForeground black
  option add *Radiobutton.Background gray

  toplevel .infowin
  wm title .infowin "mpeg_blocks"

  ########################
  # Motion Vectors stuff #
  ########################
  frame .infowin.mvFrame -relief sunken -bd 5
  pack .infowin.mvFrame -side top -fill both -ipadx 5m -ipady 5m
  
  canvas .infowin.mvFrame.mvCanvas -width 200 -height 150
  pack .infowin.mvFrame.mvCanvas -expand 1 


  ########################################
  # Vector and Full/Half pixel indicator #
  ########################################
  canvas .infowin.mvFrame.vectorCanvas -height 30
  pack .infowin.mvFrame.vectorCanvas -side top -fill both

  DrawDot .infowin.mvFrame.vectorCanvas 10 10 black pel
  DrawText .infowin.mvFrame.vectorCanvas 48 10 \
    "Full Pixel" pel
  DrawDot .infowin.mvFrame.vectorCanvas 10 20 orange pel
  DrawText .infowin.mvFrame.vectorCanvas 50 20 \
    "Half Pixel" pel

  set fmv ""
  set bmv ""
  label .infowin.mvFrame.fmv -textvariable fmv
  label .infowin.mvFrame.bmv -textvariable bmv
  pack .infowin.mvFrame.fmv .infowin.mvFrame.bmv -side left -fill both \
    -expand 1

  bind .infowin.mvFrame.mvCanvas <Motion> { 
    DestroyObject .infowin.mvFrame.vectorCanvas vector
    set i [expr %y / 24]
    set j [expr %x / 24]

    if {$i == $Frame(mb_height)} {
      incr i -1
    }

    if {$j == $Frame(mb_width)} {
      incr j -1
    }
     
    set fmv "$FMV($i,$j)"
    set bmv "$BMV($i,$j)"
  }


  ##############
  # Info stuff #
  ##############
  frame .infowin.statFrame -relief ridge -bd 5
  pack .infowin.statFrame -side top -fill both

  # Header label
  label .infowin.mbLabel -text "Information" -relief raised
  pack .infowin.mbLabel -in .infowin.statFrame -side top -fill both

  # Legend stuff
  canvas .infowin.statFrame.pict -width 60 -height 290
  pack .infowin.statFrame.pict -side left -fill y

  # I block 
  DrawBox .infowin.statFrame.pict 20 70 red legend

  # P block 
  DrawBox .infowin.statFrame.pict 20 115 blue legend

  # B block 
  DrawBox .infowin.statFrame.pict 20 160 green legend

  # Bi block 
  DrawBox .infowin.statFrame.pict 20 205 yellow legend

  # Skip
  DrawBox .infowin.statFrame.pict 20 250 gray legend


  # Frame size ?x?
  frame .infowin.l1 
  pack .infowin.l1 -side top -in .infowin.statFrame -fill both
  label .infowin.frameSizeLabel1 -text "Size:"
  label .infowin.frameSizeLabel2 -text "x"
  label .infowin.widthValue -textvariable Mpeg(width)
  label .infowin.heightValue -textvariable Mpeg(height)
  pack .infowin.frameSizeLabel1 .infowin.widthValue \
    .infowin.frameSizeLabel2 .infowin.heightValue \
    -in .infowin.l1 -side left

  # Current frame number
  frame .infowin.l2
  pack .infowin.l2 -side top -in .infowin.statFrame -fill both
  label .infowin.currFrameLabel -text "Current frame:"
  label .infowin.currFrameValue -textvariable Frame(current) 
  pack .infowin.currFrameLabel .infowin.currFrameValue \
    -in .infowin.l2 -side left 

  # Current frame type
  frame .infowin.l3
  pack .infowin.l3 -side top -in .infowin.statFrame -fill both
  label .infowin.frameTypeLabel -text "Frame type:"
  label .infowin.frameTypeValue -textvariable Frame(type)
  pack .infowin.frameTypeLabel .infowin.frameTypeValue \
    -in .infowin.l3 -side left

  # I Block 
  set l 3
  set l [expr $l + 1]
  frame .infowin.l$l 
  pack .infowin.l$l -side top -in .infowin.statFrame -fill both
  label .infowin.numOfIMBLabel -text "I Macroblocks"
  label .infowin.numOfIMBValue -textvariable IBlock(num)
  pack .infowin.numOfIMBValue .infowin.numOfIMBLabel  -in .infowin.l$l \
    -side left 

  set l [expr $l + 1]
  frame .infowin.l$l
  pack .infowin.l$l -side top -in .infowin.statFrame -fill both
  label .infowin.avgSizeOfIMBLabel1 -text "Avg Size:"
  label .infowin.avgSizeOfIMBLabel2 -text "bytes"
  label .infowin.avgSizeOfIMBLabel3 -text "bits"
  label .infowin.avgSizeOfIMBValue1 -textvariable IBlock(byte)
  label .infowin.avgSizeOfIMBValue2 -textvariable IBlock(bit) 
  pack .infowin.avgSizeOfIMBLabel1 .infowin.avgSizeOfIMBValue1 \
    .infowin.avgSizeOfIMBLabel2 .infowin.avgSizeOfIMBValue2 \
    .infowin.avgSizeOfIMBLabel3 -in .infowin.l$l -side left 

  # P Block 
  set l [expr $l + 1]
  frame .infowin.l$l
  pack .infowin.l$l -side top -in .infowin.statFrame -fill both
  label .infowin.numOfPMBLabel -text "P Macroblocks"
  label .infowin.numOfPMBValue -textvariable PBlock(num) 
  pack .infowin.numOfPMBValue .infowin.numOfPMBLabel -in .infowin.l$l \
    -side left 

  set l [expr $l + 1]
  frame .infowin.l$l
  pack .infowin.l$l -side top -in .infowin.statFrame -fill both
  label .infowin.avgSizeOfPMBLabel1 -text "Avg Size:"
  label .infowin.avgSizeOfPMBLabel2 -text "bytes"
  label .infowin.avgSizeOfPMBLabel3 -text "bits"
  label .infowin.avgSizeOfPMBValue1 -textvariable PBlock(byte) 
  label .infowin.avgSizeOfPMBValue2 -textvariable PBlock(bit)
  pack .infowin.avgSizeOfPMBLabel1 .infowin.avgSizeOfPMBValue1 \
    .infowin.avgSizeOfPMBLabel2 .infowin.avgSizeOfPMBValue2 \
    .infowin.avgSizeOfPMBLabel3 -in .infowin.l$l -side left 

  # B Block 
  set l [expr $l + 1]
  frame .infowin.l$l
  pack .infowin.l$l -side top -in .infowin.statFrame -fill both
  label .infowin.numOfBMBLabel -text "B Macroblocks"
  label .infowin.numOfBMBValue -textvariable BBlock(num)
  pack .infowin.numOfBMBValue .infowin.numOfBMBLabel -in .infowin.l$l \
    -side left 

  set l [expr $l + 1]
  frame .infowin.l$l
  pack .infowin.l$l -side top -in .infowin.statFrame -fill both
  label .infowin.avgSizeOfBMBLabel1 -text "Avg Size:"
  label .infowin.avgSizeOfBMBLabel2 -text "bytes"
  label .infowin.avgSizeOfBMBLabel3 -text "bits"
  label .infowin.avgSizeOfBMBValue1 -textvariable BBlock(byte) 
  label .infowin.avgSizeOfBMBValue2 -textvariable BBlock(bit)
  pack .infowin.avgSizeOfBMBLabel1 .infowin.avgSizeOfBMBValue1 \
    .infowin.avgSizeOfBMBLabel2 .infowin.avgSizeOfBMBValue2 \
    .infowin.avgSizeOfBMBLabel3 -in .infowin.l$l -side left

  # Bi Block 
  set l [expr $l + 1]
  frame .infowin.l$l
  pack .infowin.l$l -side top -in .infowin.statFrame -fill both
  label .infowin.numOfBiMBLabel -text "Bi Macroblocks:"
  label .infowin.numOfBiMBValue -textvariable BiBlock(num)
  pack .infowin.numOfBiMBValue .infowin.numOfBiMBLabel -in .infowin.l$l \
    -side left 

  set l [expr $l + 1]
  frame .infowin.l$l
  pack .infowin.l$l -side top -in .infowin.statFrame -fill both
  label .infowin.avgSizeOfBiMBLabel1 -text "Avg Size:"
  label .infowin.avgSizeOfBiMBLabel2 -text "bytes"
  label .infowin.avgSizeOfBiMBLabel3 -text "bits"
  label .infowin.avgSizeOfBiMBValue1 -textvariable BiBlock(byte) 
  label .infowin.avgSizeOfBiMBValue2 -textvariable BiBlock(bit)
  pack .infowin.avgSizeOfBiMBLabel1 .infowin.avgSizeOfBiMBValue1 \
    .infowin.avgSizeOfBiMBLabel2 .infowin.avgSizeOfBiMBValue2 \
    .infowin.avgSizeOfBiMBLabel3 -in .infowin.l$l -side left 

  # Skipped Block 
  set l [expr $l + 1]
  frame .infowin.l$l
  pack .infowin.l$l -side top -in .infowin.statFrame -fill both
  label .infowin.numOfSkipMBLabel -text "Skipped Macroblocks:"
  label .infowin.numOfSkipMBValue -textvariable SkipBlock(num)
  pack .infowin.numOfSkipMBValue .infowin.numOfSkipMBLabel -in .infowin.l$l \
    -side left 

  set l [expr $l + 1]
  frame .infowin.l$l
  pack .infowin.l$l -side top -in .infowin.statFrame -fill both
  label .infowin.avgSizeOfSkipMBLabel1 -text "Avg Size:"
  label .infowin.avgSizeOfSkipMBLabel2 -text "bytes"
  label .infowin.avgSizeOfSkipMBLabel3 -text "bits"
  label .infowin.avgSizeOfSkipMBValue1 -text "0"
  label .infowin.avgSizeOfSkipMBValue2 -text "0"
  pack .infowin.avgSizeOfSkipMBLabel1 .infowin.avgSizeOfSkipMBValue1 \
    .infowin.avgSizeOfSkipMBLabel2 .infowin.avgSizeOfSkipMBValue2 \
    .infowin.avgSizeOfSkipMBLabel3 -in .infowin.l$l -side left 

  unset l
}

  
. configure -background gray
ScaleHandler $Mpeg(fps)
PercentScaleHandler 100
DrawControlPanel
DrawInfoWin 

