#!/usr/bin/X11/wish -f

#
# Search for *CONFIG* and edit variables to your needs
#

# Program "tkXBlast 2.2.1"
# (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
# January 26th, 1997
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public Licences as by published
# by the Free Software Foundation; either version 2; or (at your option)
# any later version
#
# This program is distributed in the hope that it will entertaining,
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILTY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
# Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.
# 675 Mass Ave, Cambridge, MA 02139, USA.

#
# *CONFIG*: filename of the xblast binary
#
#XBLASTCOMMAND
set xblastCommand "../xblast"
#
# *CONFIG*: path for xrdb command
#
#XRDBCOMMAND
set xrdbCommand "/usr/X11R6/bin/xrdb"    
#
# *CONFIG* path for X11 rgb.txt and for personal rgb file
#
#RGBFILE
set rgbFile "/usr/lib/X11/rgb.txt"
#
# path for image files
#
#BITMAPPATH
set bitmapPath "./"

# 
# User configurable variables
#

#
# *CONFIG: user resource file interpreted by xrdb at login
#
set resourceFile "$env(HOME)/.Xresources"
#
# *CONFIG*: font names 
#
set fontLarge  "-*-helvetica-bold-r-*-*-24-*-*-*-*-*-iso8859-*"
set fontMedium "-*-helvetica-bold-r-*-*-14-*-*-*-*-*-iso8859-*"
set fontSmall  "-*-helvetica-bold-r-*-*-12-*-*-*-*-*-iso8859-*"
set fontTypewriter "-*-courier-bold-r-*-*-12-*-*-*-*-*-iso8859-*"

#
# no further user configurable variables
#

#
# private (converted and tested) rgb data file
#
set myRgbFile "$env(HOME)/.xblast-rgb"
#
# name of the savefile for your xblast settings
#
set saveFile "$env(HOME)/.xblast"              
#
# file for xblast statistics
#
set statFile "$env(HOME)/.xblast_stat"
#
# private resource files
#
set playerColorFile "$env(HOME)/.xblast-player_colors"
set playerResourceFile "$env(HOME)/.xblast-player_resource"


#
# global variables
#

#
# for statistics
#
global levelList
set levelList {}
global playerList
set playerList {{Draw Game} {Out Of Time}}
global levelVictories
global levelGames

#
# for player colors
#
global playerColorItemList
set playerColorItemList {helmet face body handsFeet armsLegs backpack}
global playerColors1
global playerColors2
global playerColors3
global playerColors4
global playerColors5
global playerColors6
set playerColors1 {Turquoise LightSalmon White Gray75 Gray25 RoyalBlue}
set playerColors2 {MidnightBlue LightSalmon NavyBlue Red RoyalBlue Gold }
set playerColors3 {Red LightSalmon Red ForestGreen IndianRed DarkSeaGreen }
set playerColors4 {Yellow LightSalmon SpringGreen OrangeRed LightYellow RoyalBlue }
set playerColors5 {DeepPink LightSalmon Orchid SpringGreen RoyalBlue DeepPink }
set playerColors6 {OliveDrab LightSalmon DarkOliveGreen OrangeRed DarkKhaki Firebrick }

#############################
#                           #
# init and generic commands #
#                           #
#############################

#
# procedure initWish
#
# get tk version number, visual mode etc
#
proc initWish {} {
    #
    global colorMode
    #
    global frameColor
    global frameColorLight
    global textColorLight
    global textColor
    #
    global argv
    #
    # try to detect color mode
    #
    if {[lindex $argv 0]!="-mono"} {
	case [winfo screenvisual .] {
	    {grayscale staticgray} {
		set colorMode 0
	    } 
	    default {
		set colorMode 1
	    }
	}
    } else {
	set colorMode 0
    }
    #
    # set palette
    #
    if {$colorMode} {
	set frameColor(.launch)         rgb:ff/7f/7f
	set frameColor(.levels)         rgb:ff/ff/7f
	set frameColor(.statistics)     rgb:7f/ff/7f
	set frameColor(.playerColor)    rgb:7f/ff/ff
	set frameColor(.playerResource) rgb:7f/7f/ff
	set frameColor(.xblastColor)    rgb:ff/7f/ff
	#
	set frameColorLight(.launch)         rgb:ff/bf/bf
	set frameColorLight(.levels)         rgb:ff/ff/bf
	set frameColorLight(.statistics)     rgb:bf/ff/bf
	set frameColorLight(.playerColor)    rgb:bf/ff/ff
	set frameColorLight(.playerResource) rgb:bf/bf/ff
	set frameColorLight(.xblastColor)    rgb:ff/bf/ff
	#
	set textColorLight Black
	set textColor Black
    } else {
	foreach i {.launch .levels .statistics .playerColor .playerResource \
		.xblastColor} {
	    set frameColor($i) Black
	    set frameColorLight($i) White
	}
	set textColorLight Black
	set textColor White
	#tk_setPalette White
    }
    #
    # set default player colors
    #
    global playerColorItemList
    global playerColors1
    global playerColors2
    #
    if {$colorMode} {
	for {set i 0} {$i < 6} {incr i} {
	    set var1 [lindex $playerColorItemList $i]Color1
	    set var2 [lindex $playerColorItemList $i]Color2
	    global $var1
	    global $var2
	    set $var1 [lindex $playerColors1 $i]
	    set $var2 [lindex $playerColors2 $i]
	}
    } else {
	foreach i $playerColorItemList {
	    set ${i}Color1 White
	    set ${i}Color2 White
	}
    }
    
}


#
# procedure initXBlast
#
# get xblast version
#
proc initXBlast {} {
    global xblastCommand
    #
    global xblastVersion
    global canFork
    global hasTclList
    global hasTeamMode
    global hasDspSound
    #
    set fp [open "|$xblastCommand -s"]
    gets $fp line
    #
    while {-1 != [gets $fp dummy]} {
    }
    catch "close $fp" xblastLog
    puts $xblastLog
    #
    set xblastVersion [lindex $line 1]
    scan $xblastVersion "%d.%d.%d" majorVersion minorVersion patchVersion
    #
    if {$majorVersion != 2} {
	error "XBlast $xblastVersion not supported by tkXBlast.\n"
    }
    #
    set hasTclList 0
    set canFork 0
    if {$minorVersion >= 1} {
	set canFork 1
	if { ($minorVersion > 1) || ($patchVersion > 4) } {
	    set hasTclList 1
	} 
    } 
    set hasTeamMode 0
    if {$minorVersion >= 2} {
	set hasTeamMode 1
    }
    #
    set hasDspSound 0
    if {"Sound" == [lindex $line 2]} {
	set hasDspSound 1
    } 
}

# first the exec routine
#
proc execXBlast {playerString optionString levelString} {
    global xblastCommand
    #
    set xblast "|$xblastCommand $playerString $optionString $levelString"
    #puts "DEBUG: $xblast"
    #
    # open xblast via pipe 
    #
    set fp [open $xblast "r"]
    #
    # read lines from output of xblast
    #
    while {-1 != [gets $fp line]} {
	#
	# check first element
	#
	case [lindex $line 0] {
	    {LevelStat} {
	set levelName [lindex $line 1]
		levelWinner $levelName [lindex $line 2] 
		foreach i [lindex $line 3] {
		    levelPlayer $levelName $i
		}
		levelPlayer $levelName {Draw Game}
		levelPlayer $levelName {Out Of Time}
	    }
	    {GameStat} {
	    }
            default {
                # echo other lines
		puts $line
	    }
	}
    }
    #
    # close pipe to xblast
    #
    catch "close $fp" xblastLog
    #
    puts -nonewline $xblastLog
}

proc createButtonRow {W} {
    global fontMedium
    global colorMode
    global frameColor
    global textColor 
    global frameColorLight
    global textColorLight
    #
    frame $W 
    button $W.launch -text "  Launch  " \
	    -font $fontMedium \
	    -command "showFrame .launch" \
	    -foreground $textColor \
	    -background $frameColor(.launch) \
	    -activebackground $frameColorLight(.launch)\
	    -activeforeground $textColorLight
    button $W.levels -text "  Levels  " \
	    -font $fontMedium \
	    -command "showFrame .levels" \
	    -foreground $textColor \
	    -background $frameColor(.levels) \
	    -activebackground $frameColorLight(.levels) \
	    -activeforeground $textColorLight
    button $W.statistics -text "  Statistics  " \
	    -font $fontMedium \
	    -command "showFrame .statistics" \
	    -foreground $textColor \
	    -background $frameColor(.statistics) \
	    -activebackground $frameColorLight(.statistics)\
	    -activeforeground $textColorLight
    if {$colorMode} {
	button $W.playerColor -text "  Player Colors  " \
		-font $fontMedium \
		-command "showFrame .playerColor" \
		-foreground $textColor \
		-background $frameColor(.playerColor)\
		-activebackground $frameColorLight(.playerColor)\
		-activeforeground $textColorLight
    }
    button $W.playerResource -text "  Player Resources  " \
	    -font $fontMedium \
	    -command "showFrame .playerResource"\
	    -foreground $textColor \
	    -background $frameColor(.playerResource)\
	    -activebackground $frameColorLight(.playerResource)\
	    -activeforeground $textColorLight
#    if {$colorMode} {
#	button $W.xblastColor -text "  XBlast Colors  " \
#		-font $fontMedium \
#		-command "showFrame .xblastColor" \
#		-foreground $textColor \
#		-background $frameColor(.xblastColor)\
#		-activebackground $frameColorLight(.xblastColor)\
#		-activeforeground $textColorLight
#    }

    foreach w [winfo children $W] {
	pack $w -side left -fill y -expand yes -padx 8 -pady 4
    }
}



proc createGenericButtons {W colorID defaultButton textList commandList} {
    global fontMedium
    global frameColor
    global frameColorLight
    global textColor
    global textColorLight
    #
    #
    # surounding frame
    #
    frame $W -borderwidth 2 -relief sunken 
    #
    # compare length
    #
    if {[llength $textList] != [llength $commandList]} {
	error "Button and command list differ in length"
    }
    set l [llength $textList] 
    for {set i 0} {$i<$l} {incr i} {
	if {$i == $defaultButton} {
	    frame $W.b${i} -borderwidth 2 -relief sunken \
		    -background $frameColor($colorID)
	    button $W.b${i}.b \
		    -borderwidth 4 \
		    -text "  [lindex $textList $i]  " \
		    -command [lindex $commandList $i] \
		    -font $fontMedium -padx 8 -pady 4 \
		    -foreground $textColor \
		    -background $frameColor($colorID) \
		    -activeforeground $textColorLight \
		    -activebackground $frameColorLight($colorID) \
		    -highlightbackground $frameColor($colorID) 
	    pack $W.b${i}.b -padx 4 -pady 2 -fill both 
	} else {
	    button $W.b${i} \
		    -text "  [lindex $textList $i]  " \
		    -command [lindex $commandList $i] \
		    -font $fontMedium -padx 8 -pady 4 \
		    -foreground $textColor \
		    -background $frameColor($colorID) \
		    -activeforeground $textColorLight \
		    -activebackground $frameColorLight($colorID) 
	}
	pack $W.b${i} -side left -expand yes \
		-padx 8 -pady 4
    }
}


proc makeTitle {W colorID text} {
    global fontMedium
    global frameColor
    global textColor
    #
    label $W -text $text \
	    -font $fontMedium \
	    -foreground $textColor \
	    -background $frameColor($colorID) \
	    -borderwidth 1 -relief sunken \
	    -pady 2 
    pack $W -side top -fill x -pady 1 -padx 2
}


proc showFrame {W} {
    global currentFrame
    global fontLarge
    #
    if {$currentFrame == $W} {
	return
    }
    if {$currentFrame != {}} {
	pack forget $currentFrame
	.button$currentFrame config -relief raised
    }
    if {![winfo exists $W]} {
	switch $W {
	    {.launch} {
		createLaunchFrame $W
	    }
	    {.levels} {
		createLevelFrame $W
	    }
	    {.statistics} {
		createStatisticsFrame $W
	    }
	    {.playerColor} {
		createPlayerColorFrame $W
	    }
	    {.playerResource} {
		createPlayerResourceFrame $W
	    }
	    {.xblastColor} {
		createXBlastColorFrame $W
		#label $W -text "Empty" -font $fontLarge
	    }
	}
    }
    case $W {
	    {.launch} {
		bind . <Return> startXBlast
	    }
	    {.levels} {
		bind . <Return> startXBlast
	    }
	    {.statistics} {
		bind . <Return> showStat
	    }
	    {.playerColor} {
		bind . <Return> applyPlayerColors
	    }
	    {.playerResource} {
		bind . <Return> applyPlayerResource
	    }
	    default {
		bind . <Return> {}
	    }
    }
    pack $W -side top -fill both -expand yes
    .button$W config -relief sunken
    set currentFrame $W
}

proc insertResourceLine {newLine} {
    global resourceFile 
    #	
    set resourceComment "! Resources inserted by tkXBlast"
    set cFlag 1
    #
    set fin [open $resourceFile r]
    set fout [open "$resourceFile#" w]
    #
    while {-1 != [gets $fin line]} {
	if {$line != $newLine} {
	    puts $fout $line
	}
	if {$line == $resourceComment} {
	    set cFlag 0
	    puts $fout $newLine
	}
    }
    close $fin
    #
    # insert resource line
    #
    if {$cFlag} {
	puts $fout $resourceComment
	puts $fout $newLine
    }
    close $fout
    #
    # move files
    #
    exec mv $resourceFile ${resourceFile}~
    exec mv "$resourceFile#" $resourceFile
}

#####################
#                   #
# Database commands #
#                   #
#####################

proc levelWinner {level name} {
    #puts "DEBUG: levelWinner \{$level\} \{$name\}"
    #
    global levelList
    global playerList
    global levelVictories
    global levelGames
    #
    # first check if level already exists in list
    #
    if {[lsearch -exact $levelList $level] == -1} {
	#puts "DEBUG: appending level \"$level\""
	#
	# otherwise append it and sort list
	#
	lappend levelList $level
	set levelList [lsort -ascii $levelList]
	#
	# create player entries for this level
	#
	foreach p $playerList {
	    set levelVictories($level,$p) 0
	    set levelGames($level,$p) 0
	}
    }
    #
    # now check if player already exists in list
    # 
    if {[lsearch -exact $playerList $name] == -1} {
	#puts "DEBUG: appending name \"$name\""
	#
	# otherwise append it and sort list
	#
	lappend playerList $name
	#
	# create level victory entries for this player
	#
	foreach l $levelList  {
	    set levelVictories($l,$name) 0
	    set levelGames($l,$name) 0
	}
    }
    #
    # now increment number of victories for this player
    # 
    incr levelVictories($level,$name)  
}


proc levelPlayer {level name} {
    #puts "DEBUG: levelPlayer \{$level\} \{$name\}"
    #
    global levelList
    global playerList
    global levelVictories
    global levelGames
    #
    # first check if level already exists in list
    #
    if {[lsearch -exact $levelList $level] == -1} {
	#puts "DEBUG: appending level \"$level\""
	#
	# otherwise append it and sort list
	#
	lappend levelList $level
	set levelList [lsort -ascii $levelList]
	#
	# create player entries for this level
	#
	foreach p $playerList {
	    set levelVictories($level,$p) 0
	    set levelGames($level,$p) 0
	}
    }
    #
    # now check if player already exists in list
    # 
    if {[lsearch -exact $playerList $name] == -1} {
	#puts "DEBUG: appending name \"$name\""
	#
	# otherwise append it and sort list
	#
	lappend playerList $name
	#
	# create level victory entries for this player
	#
	foreach l $levelList  {
	    set levelVictories($l,$name) 0
	    set levelGames($l,$name) 0
	}
    }
    #
    # now increment number of victories for this player
    # 
    incr levelGames($level,$name)  
}


proc writeStatFile {} {
    #
    global levelList
    global playerList
    global levelVictories
    global levelGames
    #
    # open statistic file
    #
    global env
    set fp [open "$env(HOME)/.xblast_stat" "w"]
    #
    # write header
    #
    puts $fp "XBlastStat"
    #
    # write level and player numbers
    #
    puts $fp "[llength $levelList] [llength $playerList]"
    #
    # write levelNames
    #
    foreach l $levelList {
	puts $fp $l
    }
    #
    # write player names
    #
    foreach p $playerList {
	puts $fp $p
    }
    #
    # now number of victories
    #
    foreach l $levelList {
	set line {}
	foreach p $playerList {
	    lappend line $levelVictories($l,$p) 
	}
	puts $fp $line
    }
    #
    # now number of games
    #
    foreach l $levelList {
	set line {}
	foreach p $playerList {
	    lappend line $levelGames($l,$p) 
	}
	puts $fp $line
    }
    #
    # that's all folks
    #
    close $fp
}

proc readStatFile {} {
    #
    global levelList
    global playerList
    global levelVictories
    global levelGames
    #
    # open statistic file
    #
    global env
    if {[file exists "$env(HOME)/.xblast_stat"]} {
	set fp [open "$env(HOME)/.xblast_stat" "r"]
	#
	# read header
	#
	gets $fp line
	#
	# read level and player numbers
	#
	gets $fp line 
	set numLevel [lindex $line 0]
	set numPlayer [lindex $line 1]
	#
	# read levelNames
	#
	set levelList {}
	for {set l 0} {$l < $numLevel} {incr l} {
	    gets $fp line
	    lappend levelList $line
	}
	#
	# read playerNames
	#
	set playerList {}
	for {set l 0} {$l < $numPlayer} {incr l} {
	    gets $fp line
	    lappend playerList $line
	}
	#
	# now number of victories
	#
	foreach l $levelList {
	    gets $fp line
	    for {set i 0} {$i < $numPlayer} {incr i} {
		set levelVictories($l,[lindex $playerList $i]) \
			[lindex $line $i]
	    }
	}
	#
	# now number of games
	#
	foreach l $levelList {
	    gets $fp line
	    for {set i 0} {$i < $numPlayer} {incr i} {
		set levelGames($l,[lindex $playerList $i]) \
			[lindex $line $i]
	    }
	}
	#
	# that's all folks
	#
	close $fp
    }
}

proc statSort {list} {
    set result {}

    while {[llength $list] !=0 } {
	#
	# set starting value
	#
	set best [lindex $list 0]
	set bindex 0
	set index 0
	#
	# search for highest elemnt in list
	#
	foreach i $list {
	    if {[lindex $i 1] > [lindex $best 1]} {
		set best $i
		set bindex $index
	    }
	    incr index 
	}
	#
	# append best to result
	#
	lappend result $best
	#
	# delete it in old list
	#
	set list [lreplace $list $bindex $bindex]
    }
    return $result

}


proc createLevelStat {level} {
    global playerList
    global levelList
    global levelVictories
    global levelGames
    #
    # create unsorted list 
    #
    set list {}
    if {$level != "Total"} {
	foreach i $playerList {
	    if {$levelGames($level,$i) != 0} {
		lappend list [list $i \
			[expr 100.0*$levelVictories($level,$i) \
			/ $levelGames($level,$i)] \
			$levelVictories($level,$i) $levelGames($level,$i) ]
	    }
	}
    } else {
	foreach i $playerList {
	    set totalGames 0
	    set totalWins  0
	    foreach j $levelList {
		incr totalGames $levelGames($j,$i)
		incr totalWins  $levelVictories($j,$i)
	    }
	    if {$totalGames != 0} {
		lappend list [list $i \
			[expr 100.0 * $totalWins / $totalGames] \
			$totalWins $totalGames ]
	    } else {
		lappend list [list $i 0.0\
			$totalWins $totalGames ]
	    }
	}
    }
    #
    # split list for sorting
    #
    set list1 [lrange $list 0 1]
    set list2 [lreplace $list 0 1]
    #
    # sort list2 
    #
    return [concat $list1 [statSort $list2] ]
}

proc printLevelStat {W level} {
    if {$level == "Total"} {
	$W.title config -text "Statistics for All Levels"
    } else {
	$W.title config -text "Statistics for Level $level"
    }
    #
    # get stat list for level
    #
    $W.text.box delete 0.0 end
    $W.text.box insert end\
	    [format "%-40s %5s  %5s/%5s\n" Level Ratio Wins Games]
    $W.text.box insert end \
	    "-----------------------------------------------------------\n"
    foreach i [createLevelStat $level] {
	$W.text.box insert end \
		[format "%-40s%5.1f%%  %5d/%5d\n" \
		[lindex $i 0] \
		[lindex $i 1] \
		[lindex $i 2] \
		[lindex $i 3] ]
	if {[lindex $i 0]== "Out Of Time"} {
	    $W.text.box insert end \
                  "-----------------------------------------------------------\n"
	}
    }
    $W.text.box insert end \
	    "-----------------------------------------------------------\n"
}



proc createPlayerStat {player} {
    global levelList
    global levelVictories
    global levelGames
    #
    # create unsorted list 
    #
    set list {}
    foreach l $levelList {
	if {$levelGames($l,$player) != 0} {
	    lappend list [list $l \
		    [expr 100.0*$levelVictories($l,$player) \
		    / $levelGames($l,$player)] \
		    $levelVictories($l,$player) $levelGames($l,$player) ]
	}
    }
    #
    # return sorted list
    #
    return [statSort $list] 
}

proc printPlayerStat {W player} {
    case $player {
	{"Draw Game"} {
	    $W.title config -text "Statistics for Draw Games"
	}
	{"Out Of Time"} {
	    $W.title config -text "Statistics for Out Of Time Games"
	}
	default {
	    $W.title config -text "Statistics for Player $player"
	}
    }
    #
    # get stat list for level
    #
    $W.text.box delete 0.0 end
    #
    set sWin  0
    set sGame 0
    $W.text.box insert end\
	    [format "%-40s %5s  %5s/%5s\n" Level Ratio Wins Games]
    $W.text.box insert end \
	    "-----------------------------------------------------------\n"
    foreach i [createPlayerStat $player] {
	$W.text.box insert end\
		[format "%-40s%5.1f%%  %5d/%5d\n" \
		[lindex $i 0] \
		[lindex $i 1] \
		[lindex $i 2] \
		[lindex $i 3] ]
	incr sWin [lindex $i 2]
	incr sGame [lindex $i 3]
    }
    if {$sGame != 0} {
	set sRate [expr 100.0 * $sWin / $sGame]
    } else {
	set sRate 0.0
    }
    $W.text.box insert end \
	    "-----------------------------------------------------------\n"
    $W.text.box insert end \
	    [format "%-40s%5.1f%%  %5d/%5d\n" "Total:" $sRate $sWin $sGame]
}


proc mergePlayer {target source} {
    global playerList
    global levelList
    global levelGames
    global levelVictories
    #
    # check if both players exist
    #
    if {-1 == [lsearch $playerList $target]} {
	puts "Player $target not in database"
	return
    }
    if {-1 == [set lpos [lsearch $playerList $source]]} {
	puts "Player $source not in database"
	return
    }
    #
    # set new values
    #
    foreach l $levelList {
	incr levelGames($l,$target) $levelGames($l,$source)
	unset levelGames($l,$source)
	incr levelVictories($l,$target) $levelVictories($l,$source)
	unset levelVictories($l,$source)
    }
    #
    # remove source player from list
    #
    set playerList [lreplace $playerList $lpos $lpos]
    #
    return
}


proc deletePlayer {player} {
    global playerList
    global levelList
    global levelGames
    global levelVictories
    #
    # check if both players exist
    #
    if {-1 == [set lpos [lsearch $playerList $player]]} {
	puts "Player $player not in database"
	return
    } 
    #
    # unset all variables
    #
    foreach l $levelList {
	unset levelGames($l,$player)
	unset levelVictories($l,$player)
    }
    #
    # remove player from list
    #
    set playerList [lreplace $playerList $lpos $lpos]
    #
    return
}

proc updateLevelList {W} {
    global levelList
    #
    readStatFile
    #
    $W.list.box delete 0 end
    $W.list.box insert 0 Total
    foreach l $levelList {
	$W.list.box insert end $l
    }
}

proc updatePlayerList {W} {
    global playerList
    #
    readStatFile
    #
    $W.list.box delete 0 end
    #$W.list.box insert 0 Total
    foreach l $playerList {
	$W.list.box insert end $l
    }
}

proc createListbox {W mode} {
    global fontMedium
    global fontSmall
    #
    global colorMode
    #
    frame $W -relief raised -borderwidth 2
    makeTitle $W.title .statistics "Select $mode"
    #
    # the actual listbox
    #
    frame $W.list
    scrollbar $W.list.scroll \
	    -orient vertical \
	    -command "$W.list.box yview" \
	    -width 12
    listbox $W.list.box \
	    -yscrollcommand "$W.list.scroll set"
    if {$colorMode} {
	$W.list.box config -background Gray95
    }
    if {$mode=="Level"} {
	$W.list.box config -font $fontSmall 
	$W.list.box config -height 24 -width 34 -selectmode single 
    } else {
	$W.list.box config -font $fontMedium
	$W.list.box config -height 20 -width 18 -selectmode single 
    }
    pack $W.list.box -side left -fill both -expand yes
    pack $W.list.scroll -side left -fill y
    #
    pack $W.list -side top -fill both -expand yes
    #
}

proc createTextbox {W} {
    global fontTypewriter
    global fontMedium
    #
    global colorMode
    #
    frame $W -relief raised -borderwidth 2
    makeTitle $W.title .statistics "Statistics Output" 
    #
    # the actual listbox
    #
    frame $W.text
    scrollbar $W.text.scroll \
	    -orient vertical \
	    -command "$W.text.box yview" \
	    -width 12
    text $W.text.box \
	    -width 60 \
	    -height 24 \
	    -font $fontTypewriter \
	    -yscrollcommand "$W.text.scroll set" 
    if {$colorMode} {
	$W.text.box config -background Gray95
    }
    pack $W.text.box -side left -fill both -expand yes
    pack $W.text.scroll -side left -fill y
    #
    pack $W.text -side top -fill both -expand yes
    #
}

proc showPlayerStat {} {
    set playerW .statistics.out.player
    set statW .statistics.out.stat
    #
    # get level name
    #
    set listIndex [$playerW.list.box curselection]
    if {$listIndex != {}} {
	printPlayerStat $statW [$playerW.list.box get $listIndex]
    }
}

proc showLevelStat {} {
    set levelW .statistics.out.level
    set statW .statistics.out.stat
    #
    # get level name
    #
    set listIndex [$levelW.list.box curselection]
    if {$listIndex != {}} {
	printLevelStat $statW [$levelW.list.box get $listIndex]
    }
}

proc showStat {} {
    set levelW .statistics.out.level
    set playerW .statistics.out.player
    #
    if { {} != [$levelW.list.box curselection]} {
	showLevelStat
    } elseif { {} != [$playerW.list.box curselection]} {
	showPlayerStat
    }
}

proc updateStatLists {} {
    updateLevelList .statistics.out.level
    updatePlayerList .statistics.out.player
    printLevelStat .statistics.out.stat Total
}

proc createStatisticsFrame {W} {
    global frameColorLight
    #
    frame $W 
    #
    frame $W.out -relief sunken -borderwidth 2 \
	    -background $frameColorLight(.statistics)
    createListbox $W.out.level Level
    createListbox $W.out.player Player
    createTextbox $W.out.stat  
    pack $W.out.level $W.out.player $W.out.stat \
	    -side left -fill both -expand yes \
	    -padx 8 -pady 8
    #
    updateStatLists
    #
    createGenericButtons $W.button  .statistics 0 \
	    { {Show Statistics} {Update} {Exit} } \
	    { {showStat} {updateStatLists} {destroy .} }
    bind . <Escape> {destroy .}
    #
    pack $W.out -side top -fill both -expand yes
    pack $W.button -side top -fill x
}

####################
#                  #
# the launch frame #
#                  #
####################

proc startXBlast {} {
    global playerWidget
    global xblastCommand
    #
    global numPlayer
    global teamMode
    global numLives
    global numVictories
    global randomPlayer
    global randomLevel
    global blackWhite
    global overRide
    global soundOption
    global forkMode
    global gameStat
    global frameRate
    #
    # first set player string
    #
    set P1 "\"[$playerWidget(1).name get]@[$playerWidget(1).disp get]\""
    set P2 "\"[$playerWidget(2).name get]@[$playerWidget(2).disp get]\""
    set P3 "\"[$playerWidget(3).name get]@[$playerWidget(3).disp get]\""
    set P4 "\"[$playerWidget(4).name get]@[$playerWidget(4).disp get]\""
    set P5 "\"[$playerWidget(5).name get]@[$playerWidget(5).disp get]\""
    set P6 "\"[$playerWidget(6).name get]@[$playerWidget(6).disp get]\""
    switch $numPlayer {
	{2} {
	    set playerString "$P1 $P2"
	}
	{3} {
	    set playerString "$P1 $P2 $P3"
	}
	{4} {
	    set playerString "$P1 $P2 $P3 $P4"
	}
	{5} {
	    set playerString "$P1 $P2 $P3 $P4 $P5"
	}
	{6} {
	    set playerString "$P1 $P2 $P3 $P4 $P5 $P6"
	}
    }
    set optionString "$teamMode -L $numLives -v $numVictories $randomPlayer\
	    $randomLevel $blackWhite $overRide -f $frameRate $soundOption \
	    $forkMode $gameStat" 
    wm withdraw .
    #    catch "exec $xblastCommand $playerString $optionString" output
    readStatFile
    execXBlast $playerString $optionString [getLevelString]
    writeStatFile
    # puts $output
    wm deiconify .
}

#
# now something more sophisticated
#
proc setNumPlayer {N} {
    global playerWidget
    for {set i 1} {$i <= $N} {incr i} {
	enablePlayerWidget $playerWidget($i)
    }
    for {} {$i <= 6} {incr i} {
	disablePlayerWidget $playerWidget($i)
    }
} 


#
# now something more sophisticated
#
proc setTeamMode {flag} {
    global numPlayer
    global numPlayerWidget
    if {$flag == "-team"} {
	case $numPlayer {
	    {2 3} {
		set numPlayer 4
		setNumPlayer $numPlayer
	    }
	    {5} {
		set numPlayer 6
		setNumPlayer $numPlayer
	    }
	}
	foreach i {2 3 5} {
	    ${numPlayerWidget}.b${i} config -state disabled
	}
    } else {
	foreach i {2 3 5} {
	    ${numPlayerWidget}.b${i} config -state normal
	}
    }
} 




#
# save setup
#
proc saveSetup {} {
    global saveFile
    global playerWidget
    global numPlayer
    global teamMode 
    global numLives
    global numVictories
    global randomPlayer
    global randomLevel
    global blackWhite
    global overRide
    global soundOption
    global forkMode
    global gameStat
    global frameRate
    #
    set fp [open $saveFile "w"]
    puts $fp "xblast.numberOfPlayers: $numPlayer"
    for {set i 1} {$i <= $numPlayer} {incr i} {
	set name [$playerWidget($i).name get]
	if {$name != ""} {
	    puts $fp "xblast.player${i}: $name"
	}
	set disp [$playerWidget($i).disp get]
	if {$disp != ""} {
	    puts $fp "xblast.display${i}: $disp"
	}
    }
    global hasTeamMode 
    if {$hasTeamMode} {
	if {$teamMode != "-team"} {
	    puts $fp "xblast.teamMode: None"
	} else {
	    puts $fp "xblast.teamMode: Team"
	}
    }

    puts $fp "xblast.numberOfLives: $numLives"
    puts $fp "xblast.numberOfVictories: $numVictories"
    if {$randomPlayer=="-rp"} {
	puts $fp "xblast.randomPlayerPosition: True"
    } else {
	puts $fp "xblast.randomPlayerPosition: False"
    }
    if {$randomLevel=="-rl"} {
	puts $fp "xblast.randomLevelOrder: True"
    } else {
	puts $fp "xblast.randomLevelOrder: False"
    }
    if {$blackWhite=="-bw"} {
	puts $fp "xblast.allowColorMode: False"
    } else {
	puts $fp "xblast.allowColorMode: True"
    }
    if {$overRide=="-wm"} {
	puts $fp "xblast.forceOverride: True"
    } else {
	puts $fp "xblast.forceOverride: False"
    }
    global hasDspSound
    if {$hasDspSound} {
	if {$soundOption == "-stereo"} {
	    puts $fp "xblast.soundServer: stereo"
	} elseif {$soundOption == "-mono"} {
	    puts $fp "xblast.soundServer: mono"
	} else {
	    puts $fp "xblast.soundServer: none"
	}
    } else {
	if {$soundOption=="-q"} {
	    puts $fp "xblast.bellSound: False"
	} else {
	    puts $fp "xblast.bellSound: True"
	}
    }
    global canFork
    if {$canFork} {
	if {$forkMode=="+F"} {
	    puts $fp "xblast.fork: True"
	} else {
	    puts $fp "xblast.fork: False"
	}
    }
    if {$gameStat=="-P"} {
	puts $fp "xblast.printStat: True"
    } else {
	puts $fp "xblast.printStat: False"
    }
    puts $fp "xblast.frameRate: $frameRate"
    #
    close $fp
}

proc loadSetup {} {
    global saveFile
    #
    global playerWidget
    global numPlayer
    global teamMode
    global numLives
    global numVictories
    global randomPlayer
    global randomLevel
    global blackWhite
    global overRide
    global soundOption
    global forkMode
    global gameStat
    global frameRate
    #
    set fp [open $saveFile "r"]
    #
    while {-1!=[gets $fp line]} {
	#
	# check resource line
	#
	set tmp [lindex $line 0] 
	set resource [string range $tmp 0 [expr [string first : $tmp]-1]]
	set value [string range $line [string first [lindex $line 1] $line] end]
	case $resource {
	    {xblast.numberOfPlayers} {
		# number of players
		scan $value "%d" numPlayer
		setNumPlayer $numPlayer
	    }
	    {xblast.player1} {
		# name of player 1
		$playerWidget(1).name delete 0 end
		$playerWidget(1).name insert 0 $value
	    }
	    {xblast.player2} {
		# name of player 2
		$playerWidget(2).name delete 0 end
		$playerWidget(2).name insert 0 $value
	    }
	    {xblast.player3} {
		# name of player 3
		$playerWidget(3).name delete 0 end
		$playerWidget(3).name insert 0 $value
	    }
	    {xblast.player4} {
		# name of player 4
		$playerWidget(4).name delete 0 end
		$playerWidget(4).name insert 0 $value
	    }
	    {xblast.player5} {
		# name of player 5
		$playerWidget(5).name delete 0 end
		$playerWidget(5).name insert 0 $value
	    }
	    {xblast.player6} {
		# name of player 6
		$playerWidget(6).name delete 0 end
		$playerWidget(6).name insert 0 $value
	    }
	    {xblast.display1} {
		# name of player 1
		$playerWidget(1).disp delete 0 end
		$playerWidget(1).disp insert 0 $value
	    }
	    {xblast.display2} {
		# name of player 2
		$playerWidget(2).disp delete 0 end
		$playerWidget(2).disp insert 0 $value
	    }
	    {xblast.display3} {
		# name of player 3
		$playerWidget(3).disp delete 0 end
		$playerWidget(3).disp insert 0 $value
	    }
	    {xblast.display4} {
		# name of player 4
		$playerWidget(4).disp delete 0 end
		$playerWidget(4).disp insert 0 $value
	    }
	    {xblast.display5} {
		# name of player 5
		$playerWidget(5).disp delete 0 end
		$playerWidget(5).disp insert 0 $value
	    }
	    {xblast.display6} {
		# name of player 6
		$playerWidget(6).disp delete 0 end
		$playerWidget(6).disp insert 0 $value
	    }
	    {xblast.teamMode} {
		global hasTeamMode 
		if {$hasTeamMode} {
		    if {$value == "Team"} {
			set teamMode "-team"
			setTeamMode $teamMode
		    } else {
			set teamMode "-single"
			setTeamMode $teamMode
		    }
		} else {
		    puts "Unsupported resource \"$resource\""
		    set teamMode {}
		}
	    }
	    {xblast.numberOfLives} {
		# num lives
		scan $value "%d" numLives
	    }
	    {xblast.numberOfVictories} {
		# num victories
		scan $value "%d" numVictories
		
	    }
	    {xblast.randomPlayerPosition} {
		# random playyer pos
		if {$value} {
		    set randomPlayer "-rp"
		} else {
		    set randomPlayer "+rp"
		}
	    }
	    {xblast.randomLevelOrder} {
		# random level order
		if {$value} {
		    set randomLevel "-rl"
		} else {
		    set randomLevel "+rl"
		}
	    }
	    {xblast.allowColorMode} {
		# black and white mode
		if {$value} {
		    set blackWhite "+bw"
		} else {
		    set blackWhite "-bw"
		}
	    }
	    {xblast.forceOverride} {
		# override WM
		if {$value} {
		    set overRide "-wm"
		} else {
		    set overRide "+wm"
		}
	    }
	    {xblast.bellSound} {
		# toggle bell sound
		global hasDspSound
		if {! $hasDspSound} {
		    if {$value} {
			set soundOption "+q"
		    } else {
			set soundOption "-q"
		    }
		}
	    }
	    {xblast.soundServer} {
		global hasDspSound
		if {$hasDspSound} {
		    switch $value {
			{stereo} { set soundOption "-stereo" }
			{mono}   { set soundOption "-mono" }
			{none}   { set soundOption "-nosound" }
		    }
		} else {
		    puts "Unsupported resource \"$resource\""
		}
	    }
	    {xblast.fork} {
		global canFork
		if {$canFork} {
		    # toggle forkMode
		    if {$value} {
			set forkMode "+F"
		    } else {
			set forkMode "+F"
			set forkMode "-F"
		    }
		} else {
		    puts "Unsupported resource \"$resource\""
		    set forkMode {}
		}
	    }
	    {xblast.printStat} {
		# toggle game stats
		if {$value} {
		    set gameStat "-P"
		} else {
		    set gameStat "+P"
		}
	    }
	    {xblast.frameRate} {
		# frame rate
		scan $value "%d" frameRate
	    }
	    default {
		#
		# default
		#
		puts "Unknown resource \"$resource\""
	    }
	}
    }
    #
    close $fp
}

#
# first low level routines
#
proc createPlayerWidget {W player} {
    #
    global colorMode
    #
    global bitmapPath
    global fontLarge 
    global fontMedium 
    global fontSmall 
    #
    # set global variables
    #
    global widgetState 
    set widgetState($W) enabled
    global playerID
    set playerID($W) $player

    #
    # first make a surrounding frame
    #
    frame $W -borderwidth 2 -relief raised
    #
    # player n label
    #
    makeTitle $W.player .launch "Player $player"
    #
    # player image
    #
    frame $W.image -borderwidth 2 -relief sunken 
    #
    # create images
    #
    # use bitmaps
    image create bitmap winnerImage${player} \
		-file "${bitmapPath}/winner_${player}.xbm" 
    image create bitmap looserImage${player} \
	    -file "${bitmapPath}/player_${player}_4.xbm" 
    label $W.image.bits -image winnerImage${player} -bg White
    #
    pack $W.image.bits -fill both -expand yes
    #
    # player name entry
    #
    entry $W.name -bg white -font $fontMedium -relief sunken -width 16
    #
    # at sign
    #
    label $W.at -text "@" -font $fontLarge
    #
    # player disp entry
    #
    entry $W.disp -bg white -font $fontSmall -relief sunken
    # 
    # pack all
    # 
    pack $W.image -side top -expand yes -fill both \
	    -padx 4 -pady 2 
    pack $W.name $W.at $W.disp -side top -fill x  \
	    -padx 4 -pady 2 
}

proc disablePlayerWidget {W} {
    #
    global bitmapPath
    global playerID
    global widgetState
    #
    # check if necessary
    #
    if {$widgetState($W)=="enabled"} {
	#
	# set global variable
	#
	set widgetState($W) disabled
	#
	# set entries disabled
	#
	$W.name config -state disabled -fg white
	$W.disp config -state disabled -fg white
	#
	# set players to looser bitmap
	#
	$W.image.bits config \
		-image looserImage$playerID($W) 
    }
}
    
proc enablePlayerWidget {W} {
    #
    global bitmapPath
    global playerID
    global widgetState
    #
    # check if necessary
    #
    if {$widgetState($W)=="disabled"} {
	#
	# set global variable
	#
	set widgetState($W) enabled
	#
	# set entries disabled
	#
	$W.name config -state normal -fg black
	$W.disp config -state normal -fg black
	#
	# set players to looser bitmap
	#
	$W.image.bits config \
		-image winnerImage$playerID($W) 
    }
}
    

proc createSetupWidget {W} {
    #
    global frameColorLight
    #
    global playerWidget
    set playerWidget(1) ${W}.pa.p1
    set playerWidget(2) ${W}.pa.p2
    set playerWidget(3) ${W}.pa.p3
    set playerWidget(4) ${W}.pb.p4
    set playerWidget(5) ${W}.pb.p5
    set playerWidget(6) ${W}.pb.p6
    
    frame ${W} -background $frameColorLight(.launch)
    frame ${W}.pa -background $frameColorLight(.launch)
    frame ${W}.pb -background $frameColorLight(.launch)
    foreach i {1 2 3 4 5 6} {
	createPlayerWidget $playerWidget($i) ${i}
    }
    pack $playerWidget(1) $playerWidget(2) $playerWidget(3) \
	    -padx 4 -pady 4 \
	    -side left -fill both -expand yes 
    pack $playerWidget(4) $playerWidget(5) $playerWidget(6) \
	    -padx 4 -pady 4 \
	    -side right -fill both -expand yes
    pack ${W}.pa ${W}.pb -fill both -expand yes -side top
}

proc createOptionsWidget {W} {
    global fontMedium 
    global fontSmall 
    #
    # first outer frame
    #
    frame $W -borderwidth 2 -relief raised
    #
    # title
    #
    makeTitle $W.title .launch "Game Options"
    #
    # options
    #
    frame $W.o
    #
    frame $W.o.l 
    frame $W.o.r
    #
    # numplayer box
    #
    global numPlayer
    global numPlayerWidget
    set numPlayerWidget $W.o.r.numPlayer
    label $W.o.l.numPlayer -text "Number of Players" -font $fontMedium
    frame $W.o.r.numPlayer -relief sunken
    foreach i {2 3 4 5 6} {
	radiobutton $W.o.r.numPlayer.b${i} \
		-variable numPlayer \
		-text "${i} " \
		-value ${i}\
		-command {setNumPlayer $numPlayer} \
		-font $fontMedium
	pack $W.o.r.numPlayer.b${i} -side left -fill y -expand yes -pady 2 
    }
    setNumPlayer $numPlayer
    #
    # team mode box
    #
    global hasTeamMode
    global teamMode
    if {$hasTeamMode} {
	label $W.o.l.teamMode -text "Team Mode" -font $fontMedium
	frame $W.o.r.teamMode -relief sunken
	radiobutton $W.o.r.teamMode.b1 \
	    -variable teamMode \
	    -text "yes " \
	    -value "-team" \
	    -command "setTeamMode -team" \
	    -font $fontMedium
	radiobutton $W.o.r.teamMode.b0 \
	    -variable teamMode \
	    -text "no " \
	    -value "-single" \
	    -command "setTeamMode -single" \
	    -font $fontMedium
	pack $W.o.r.teamMode.b1 $W.o.r.teamMode.b0\
	    -side left -fill y -expand yes -pady 2 
    }
    #
    # numlives box
    #
    global numLives
    label $W.o.l.numLives -text "Number of Lives" -font $fontMedium
    frame $W.o.r.numLives -relief sunken
    foreach i {1 2 3} {
	radiobutton $W.o.r.numLives.b${i} \
		-variable numLives \
		-text "${i} " \
		-value ${i}\
		-font $fontMedium
	pack $W.o.r.numLives.b${i} -side left -fill y -expand yes -pady 2 
    }
    #
    # numVictories box
    #
    global numVictories
    label $W.o.l.numVictories -text "Number of Victories" -font $fontMedium
    scale $W.o.r.numVictories \
	    -from 1 -to 9 \
	    -width 12 \
	    -orient horizontal \
	    -showvalue yes \
	    -variable numVictories
    #
    # random player box
    #
    global randomPlayer
    label $W.o.l.randomPlayer -text "Random player position" -font $fontMedium
    frame $W.o.r.randomPlayer -relief sunken
    radiobutton $W.o.r.randomPlayer.b1 \
	    -variable randomPlayer \
	    -text "yes " \
	    -value "-rp" \
	    -font $fontMedium
    radiobutton $W.o.r.randomPlayer.b0 \
	    -variable randomPlayer \
	    -text "no " \
	    -value "+rp" \
	    -font $fontMedium
    pack $W.o.r.randomPlayer.b1 $W.o.r.randomPlayer.b0 \
	    -side left -fill y -expand yes -pady 2 

    #
    # random player box
    #
    global randomLevel
    label $W.o.l.randomLevel -text "Random level order" -font $fontMedium
    frame $W.o.r.randomLevel -relief sunken
    radiobutton $W.o.r.randomLevel.b1 \
	    -variable randomLevel \
	    -text "yes " \
	    -value "-rl" \
	    -font $fontMedium
    radiobutton $W.o.r.randomLevel.b0 \
	    -variable randomLevel \
	    -text "no " \
	    -value "+rl" \
	    -font $fontMedium
    pack $W.o.r.randomLevel.b1 $W.o.r.randomLevel.b0 \
	    -side left -fill y -expand yes -pady 2 

    #
    # force block and white box
    #
    global blackWhite
    label $W.o.l.blackWhite -text "Allow color mode" -font $fontMedium
    frame $W.o.r.blackWhite -relief sunken
    radiobutton $W.o.r.blackWhite.b1 \
	    -variable blackWhite \
	    -text "yes " \
	    -value "+bw" \
	    -font $fontMedium
    radiobutton $W.o.r.blackWhite.b0 \
	    -variable blackWhite \
	    -text "no " \
	    -value "-bw" \
	    -font $fontMedium
    pack $W.o.r.blackWhite.b1 $W.o.r.blackWhite.b0 \
	    -side left -fill y -expand yes -pady 2 
    #
    # override wm box
    #
    global overRide
    label $W.o.l.overRide -text "Override Window Manager" -font $fontMedium
    frame $W.o.r.overRide -relief sunken
    radiobutton $W.o.r.overRide.b1 \
	    -variable overRide \
	    -text "yes " \
	    -value "-wm" \
	    -font $fontMedium
    radiobutton $W.o.r.overRide.b0 \
	    -variable overRide \
	    -text "no " \
	    -value "+wm" \
	    -font $fontMedium
    pack $W.o.r.overRide.b1 $W.o.r.overRide.b0 \
	    -side left -fill y -expand yes -pady 2 
    

    #
    # frameRate box
    #
    global frameRate
    label $W.o.l.frameRate -text "Frame rate" -font $fontMedium
    scale $W.o.r.frameRate \
	    -from 12 -to 36 \
	    -width 12 \
	    -orient horizontal \
	    -showvalue yes \
	    -variable frameRate
    
    #
    # bell sound box
    #
    global soundOption
    global hasDspSound
    if {$hasDspSound} {
	label $W.o.l.soundOption -text "DSP sound" -font $fontMedium
	frame $W.o.r.soundOption -relief sunken
	radiobutton $W.o.r.soundOption.b0 \
	    -variable soundOption \
	    -text "stereo " \
	    -value "-stereo" \
	    -font $fontMedium
	radiobutton $W.o.r.soundOption.b1 \
	    -variable soundOption \
	    -text "mono " \
	    -value "-mono" \
	    -font $fontMedium
	radiobutton $W.o.r.soundOption.b2 \
	    -variable soundOption \
	    -text "none " \
	    -value "-nosound" \
	    -font $fontMedium
	pack $W.o.r.soundOption.b0 $W.o.r.soundOption.b1 $W.o.r.soundOption.b2 \
	    -side left -fill y -expand yes -pady 2 
    } else {
	label $W.o.l.soundOption -text "Bell sound" -font $fontMedium
	frame $W.o.r.soundOption -relief sunken
	radiobutton $W.o.r.soundOption.b1 \
	    -variable soundOption \
	    -text "yes " \
	    -value "+q" \
	    -font $fontMedium
	radiobutton $W.o.r.soundOption.b0 \
	    -variable soundOption \
	    -text "no " \
	    -value "-q" \
	    -font $fontMedium
	pack $W.o.r.soundOption.b1 $W.o.r.soundOption.b0 \
	    -side left -fill y -expand yes -pady 2 
    }
	
    #
    # forkMode box 
    #
    global forkMode
    global canFork
    if {$canFork} {
	label $W.o.l.forkMode -text "Fork XBlast" -font $fontMedium
	frame $W.o.r.forkMode -relief sunken
	radiobutton $W.o.r.forkMode.b1 \
		-variable forkMode \
		-text "yes " \
		-value "+F" \
		-font $fontMedium
	radiobutton $W.o.r.forkMode.b0 \
		-variable forkMode \
		-text "no " \
		-value "-F" \
		-font $fontMedium
	pack $W.o.r.forkMode.b1 $W.o.r.forkMode.b0 \
		-side left -fill y -expand yes -pady 2 
    }

    #
    # game stat box
    #
    global gameStat
    label $W.o.l.gameStat -text "Game Statistics" -font $fontMedium
    frame $W.o.r.gameStat -relief sunken
    radiobutton $W.o.r.gameStat.b1 \
	    -variable gameStat \
	    -text "on " \
	    -value "-P" \
	    -font $fontMedium
    radiobutton $W.o.r.gameStat.b0 \
	    -variable gameStat \
	    -text "off " \
	    -value "+P" \
	    -font $fontMedium
    pack $W.o.r.gameStat.b1 $W.o.r.gameStat.b0 \
	    -side left -fill y -expand yes -pady 2 
    

    #
    # pack left/right
    #
    foreach w [winfo children $W.o.l] {
	pack $w -side top -fill x -expand yes 
    }
    foreach w [winfo children $W.o.r] {
	pack $w -side top -fill x -expand yes 
    }
    #
    # pack all
    #
    pack $W.o.l $W.o.r -side left -fill both -expand yes -padx 8
    #
    pack $W.o -fill both -expand yes
}



proc createLaunchFrame {W} {
    global frameColorLight
    #
    frame $W
    #
    # create setup widget
    # 
    frame $W.setup -background $frameColorLight(.launch) \
	    -borderwidth 2 -relief sunken
    createSetupWidget $W.setup.p
    createOptionsWidget $W.setup.opt
    #
    # create button widget 
    #
    createGenericButtons $W.buttons  .launch 0 \
	    { {Start XBlast} {Load Setup} {Save Setup} {Exit} } \
	    { startXBlast loadSetup saveSetup {destroy .} } 
    bind . <Escape> {destroy .}
    #
    # pack it
    #
    pack $W.setup.p  -side left -fill both -expand yes -padx 4 -pady 4
    pack $W.setup.opt -side left -fill both -expand yes -padx 8 -pady 8
    pack $W.setup -fill both -expand yes
    pack $W.buttons -fill both -expand no
    
}



###########################
#                         #
# the player color frame  #
#                         # 
###########################

proc writePlayerColors {file} {
    global playerColorFile
    global singlePlayer
    #
    set resourceList {helmetColor faceColor bodyColor handsFeetColor \
	    armsLegsColor backpackColor}
    set imageList {playerHelmet playerFace playerBody playerHandsfeet \
	    playerArmslegs playerBackpack}
    #
    if {$file == {}} {
	set file $playerColorFile
    }
    set fp [open $file w]
    #
    # save single player
    #
    for {set i 0} {$i<6} {incr i} {
	set resource [lindex $resourceList $i]
	set image [lindex $imageList $i]$singlePlayer
	puts $fp "xblast.singlePlayer.${resource}: [$image cget -foreground]"
    }
    #
    # save right player
    #
    for {set i 0} {$i<6} {incr i} {
	set resource [lindex $resourceList $i]
	set image [lindex $imageList $i]1
	puts $fp "xblast.rightPlayer.${resource}: [$image cget -foreground]"
    }
    #
    # save single player
    #
    for {set i 0} {$i<6} {incr i} {
	set resource [lindex $resourceList $i]
	set image [lindex $imageList $i]2
	puts $fp "xblast.leftPlayer.${resource}: [$image cget -foreground]"
    }
    close $fp
}

proc readPlayerColors {W file} {
    global singlePlayer
    global playerColorFile
    #
    if {$file == {} } {
	set file $playerColorFile
    }
    set fp [open $file r] 
    #
    while {-1!=[gets $fp line]} {
	#
	# check resource line
	#
	set resource [lindex $line 0]
	set value [lindex $line 1]
	regsub -all \[.:\] $resource " " resource
	#
	if {[string match \[Xx\]\[Bb\]last [lindex $resource 0]] \
		&& ([llength $resource]==3)} {
	    switch [lindex $resource 1] {
		{singlePlayer} { set player $singlePlayer }
		{rightPlayer}  { set player 1 }
		{leftPlayer}   { set player 2 }
		default        { set player {} }
	    }
	    switch [lindex $resource 2] {
		{helmetColor}    { set item helmet }
		{faceColor}      { set item face }
		{bodyColor}      { set item body }
		{handsFeetColor} { set item handsFeet }
		{armsLegsColor}  { set item armsLegs }
		{backpackColor}  { set item backpack }
		default          { set item {} }
	    }
	    if {($player!={}) && ($item!={})} {
		set var ${item}Color${player}
		global $var
		set $var $value
	    }
	}
    }
    close $fp
}

proc applyPlayerColors {} {
    global xrdbCommand
    #
    # save to temp file and call xrdb
    #
    set tmpFile ".player_colors"
    #
    writePlayerColors $tmpFile
    exec $xrdbCommand -merge $tmpFile
    exec rm $tmpFile
}

proc queryPlayerColors {} {
    global xrdbCommand
    #
    readPlayerColors .playerColor.option.color "|$xrdbCommand -query"
}

proc linkPlayerColors {} {
    global playerColorFile 
    global env
    #
    if {0 == [string first $env(HOME) $playerColorFile]} {
	set fileName [string range $playerColorFile  \
		[expr 1+[string length $env(HOME)]] end ]
    } else {
	set fileName $playerColorFile
    }
    insertResourceLine "#include \"$fileName\""
}


proc loadPlayerColors {} {
    global playerColorFile
    readPlayerColors .playerColor.option.color $playerColorFile
}

proc savePlayerColors {} {
    global playerColorFile
    writePlayerColors  $playerColorFile
}

proc createColorSelector {W colorID numEntries} {
    global fontSmall
    global fontMedium
    global frameColor
    global frameColorLight
    global rgbFile
    global myRgbFile
    #
    global startPos$W
    global colorArray
    #
    # build widget
    #
    frame $W -relief raised -borderwidth 2
    #
    # title
    #
    makeTitle $W.title $colorID "Select Color"
    #
    # test if my rgb file exits
    #
    if {![file exists $myRgbFile]} {
	puts "Creating rgb database"
	set fp   [open $rgbFile r]
	set fout [open $myRgbFile w]
	while {-1 != [gets $fp line]} {
	    if {4 == [llength $line]} {
		scan $line "%d %d %d %s" dummy dummy dummy colorName
		if {(![string match *\[Gg\]rey* $colorName ]) && \
			(![string match \[Gg\]ray*\[12346789\] $colorName]) } {
		    if [catch "label ${W}.colorTest -bg \"$colorName\"" text] {
			puts $text
		    } else {
			destroy ${W}.colorTest 
			puts $fout $colorName
		    }
		}
	    }
	}
	close $fp
	close $fout
    }

    #
    # parse my rgb file
    #
    set numColor 0
    set fp [open $myRgbFile r] 
    while {-1 != [gets $fp line]} {
	set colorArray($numColor) $line
	incr numColor
    }

    if {$numColor < $numEntries} {
	set numEntries $numColor
    }
    #
    # color entries
    #
    frame $W.color 
    for {set i 0} {$i < $numEntries} {incr i} {
	frame ${W}.color.c${i} 
	label ${W}.color.c${i}.c -bg $colorArray($i) -width 8 
	button ${W}.color.c${i}.n -text $colorArray($i) \
		-command "transferColor $colorArray($i)" \
		-width 16 \
		-font $fontSmall
	pack ${W}.color.c${i}.c -side left -fill y -expand no -padx 8 -pady 4
	pack ${W}.color.c${i}.n -side left -fill both -expand yes
	pack ${W}.color.c${i} -side top -fill both -expand yes 
    }
    #
    # control buttons
    #
    frame $W.control -relief sunken -borderwidth 2
    button $W.control.pgup -text "  <<  " -font $fontMedium \
	    -command "setColor $W [expr 1 - $numEntries] $numEntries $numColor" \
	    -background $frameColor($colorID) \
	    -activebackground $frameColorLight($colorID)
    button $W.control.up -text "   <   " -font $fontMedium \
	    -command "setColor $W -1 $numEntries $numColor" \
	    -background $frameColor($colorID) \
		    -activebackground $frameColorLight($colorID) 
    button $W.control.pgdown -text "  >>  " -font $fontMedium \
	    -command "setColor $W [expr $numEntries - 1] $numEntries $numColor"\
	    -background $frameColor($colorID) \
	    -activebackground $frameColorLight($colorID)
    button $W.control.down -text "   >   " -font $fontMedium \
	    -command "setColor $W 1 $numEntries $numColor"\
	    -background $frameColor($colorID) \
	    -activebackground $frameColorLight($colorID)
    pack $W.control.pgup $W.control.up $W.control.down $W.control.pgdown \
	    -side left -fill y -expand yes 
    #
    # pack frames
    #
    pack $W.color -side top -fill both -expand yes \
	    -padx 2 -pady 2 
    pack $W.control -side top -fill x -expand no \
	    -padx 2 -pady 2 
    #
    set startPos$W 0
}

proc setColor {W offset numEntries numColor} {
    global startPos$W
    global colorArray
    #
    set startPos [expr [set startPos$W] + $offset]
    if {$startPos < 0} {
	set startPos 0
    }
    if {[expr $startPos + $numEntries] > $numColor} {
	set startPos [expr $numColor - $numEntries]
    }
    set j $startPos
    for {set i 0} {$i < $numEntries} {incr i} {
	${W}.color.c${i}.n config -text $colorArray($j) \
		-command "transferColor $colorArray($j)"
	${W}.color.c${i}.c config -background $colorArray($j)
	incr j
    }
    #
    set startPos$W $startPos
}

proc transferColor {colorName} {
    set target [focus]
    #
    if {$target != {} } {
	if {[winfo class $target] == "Entry"} {
	    $target delete 0 end
	    $target insert 0 $colorName
	    eval [bind $target <Return>]
	}
    }
}


proc configImageColor {image name1 name2 ops} {
    global $name1
    $image config -foreground [set $name1]
}

proc configEntryColor {entry name1 name2 ops} {
    global $name1
    $entry delete 0 end
    $entry insert 0 [set $name1]
}

proc createBigImage {W id} {
    global bitmapPath
    global colorMode
    #
    # check if color or bw mode
    #
    frame $W -background White \
	    -relief sunken -borderwidth 2 
    if {! $colorMode} {
	label $W.c \
		-background white \
		-foreground black \
		-bitmap @${bitmapPath}/big_${id}.xbm

    } else {
	#
	# create images needed
	#
	if {-1 == [lsearch -exact [image names] playerFrame${id}]} {
	    image create bitmap playerFrame${id} \
		    -file "${bitmapPath}/P_frame.xbm" \
		    -maskfile "${bitmapPath}/P_frame.xbm" 
	    #
	    global faceColor$id
	    image create bitmap playerFace${id} \
		    -foreground [set faceColor$id] \
		    -file "${bitmapPath}/P_face.xbm" \
		    -maskfile "${bitmapPath}/P_face.xbm" 
	    trace variable faceColor$id w \
		    "configImageColor playerFace${id}" 
	    #
	    global helmetColor$id 
	    image create bitmap playerHelmet${id} \
		    -foreground [set helmetColor$id] \
		    -file "${bitmapPath}/P_helmet.xbm" \
		    -maskfile "${bitmapPath}/P_helmet.xbm" 
	    trace variable helmetColor$id  w \
		    "configImageColor playerHelmet${id}" 
	    #
	    global bodyColor$id 
	    image create bitmap playerBody${id} \
		    -foreground [set bodyColor$id] \
		    -file "${bitmapPath}/P_body.xbm" \
		    -maskfile "${bitmapPath}/P_body.xbm" 
	    trace variable bodyColor$id  w \
		    "configImageColor playerBody${id}" 
	    #
	    global handsFeetColor$id 
	    image create bitmap playerHandsfeet${id} \
		    -foreground [set handsFeetColor$id] \
		    -file "${bitmapPath}/P_handsfeet.xbm" \
		    -maskfile "${bitmapPath}/P_handsfeet.xbm" 
	    trace variable handsFeetColor$id  w \
		    "configImageColor playerHandsfeet${id}" 
	    #
	    global armsLegsColor$id 
	    image create bitmap playerArmslegs${id} \
		    -foreground [set armsLegsColor$id] \
		    -file "${bitmapPath}/P_armslegs.xbm" \
		    -maskfile "${bitmapPath}/P_armslegs.xbm" 
	    trace variable armsLegsColor$id  w \
		    "configImageColor playerArmslegs${id}" 
	    #
	    global backpackColor$id 
	    image create bitmap playerBackpack${id} \
		    -foreground [set backpackColor$id] \
		    -file "${bitmapPath}/P_backpack.xbm" \
		    -maskfile "${bitmapPath}/P_backpack.xbm" 
	    trace variable backpackColor$id  w \
		    "configImageColor playerBackpack${id}" 
	}
	#
	# create canvas
	#
	canvas $W.c -height 144 -width 112 \
		-background White \
		-highlightbackground White 
	#
	# place images
	#
	$W.c create image 4 0 -anchor nw -image playerFrame${id}
	$W.c create image 4 0 -anchor nw -image playerFace${id}
	$W.c create image 4 0 -anchor nw -image playerHelmet${id}
	$W.c create image 4 0 -anchor nw -image playerBody${id}
	$W.c create image 4 0 -anchor nw -image playerHandsfeet${id}
	$W.c create image 4 0 -anchor nw -image playerArmslegs${id}
	$W.c create image 4 0 -anchor nw -image playerBackpack${id}
    }
    pack $W.c -side left -expand yes -fill none
}



#
# 
# 
proc setPlayerColors {id colorSet} {
    #
    global playerColorItemList
    global playerColors1
    global playerColors2
    global playerColors3
    global playerColors4
    global playerColors5
    global playerColors6
    #
    for {set i 0} {$i<6} {incr i} {
	set item [lindex $playerColorItemList $i]
	global ${item}Color${id}
	set ${item}Color${id} [lindex [set playerColors$colorSet] $i]
    }
}



proc createPlayerColor {W id} {
    global fontMedium
    global fontSmall
    global frameColor
    global frameColorLight
    #
    # some lists
    global playerColorItemList
    set imageList {playerHelmet playerFace playerBody playerHandsfeet \
	    playerArmslegs playerBackpack}
    set labelList {Helmet Face Body {Hands & Feet} {Arms & Legs} Backpack}
    #
    frame $W -borderwidth 2 -relief raised
    #
    makeTitle $W.title .playerColor "Player"
    #
    frame $W.player
    createBigImage $W.player.canvas $id
    #
    frame $W.player.entries 
    for {set i 0} {$i<6} {incr i} {
	set item [lindex $playerColorItemList $i]
	set image [lindex $imageList $i]${id}
	set label [lindex $labelList $i]
	set entry $W.player.entries.$item
	#
	frame $entry 
	label $entry.l -text "$label Color: " \
		-anchor w -font $fontSmall
	entry $entry.e \
		-background White -width 24 \
		-font $fontSmall 
	#
	set var ${item}Color$id
	global $var
	#
	$entry.e insert 0 [set $var]
	bind $entry.e <Return> \
		"global $var\n set $var \[$entry.e get\]"
	bind $entry.e <FocusOut> \
		"global $var\n set $var \[$entry.e get\]"
	trace variable $var w \
		"configEntryColor $entry.e"
	#
	pack $entry.l -side left -fill both -expand yes 
	pack $entry.e -side left -fill y
	pack $entry -side top -fill x -expand yes
    }
    #
    # buttons
    #
    frame $W.button -relief sunken -borderwidth 2
    radiobutton $W.button.single \
	    -text "  Single Player  " \
	    -value "$id" \
	    -variable singlePlayer \
	    -command "setSinglePlayer [winfo parent $W]" \
	    -font $fontSmall
    foreach i {1 2 3 4 5 6} {
	button $W.button.b${i} \
		-text "${i}" \
		-font $fontSmall \
		-command "setPlayerColors $id $i" \
		-background $frameColor(.playerColor) \
		-activebackground $frameColorLight(.playerColor) \
		-padx 8
    }
    foreach w [winfo children $W.button] {
	pack $w -side left -fill y -expand yes 
    }
    # set colors accordingly
    # $W.button.b${id} invoke
    
    
    pack $W.player.canvas -side left -fill both -expand yes \
	    -padx 4 -pady 4
    pack $W.player.entries -side left -fill both -expand yes -padx 2 -pady 2
    #
    pack $W.player -side top -fill both -expand yes
    pack $W.button -side top -fill x 
}

proc setSinglePlayer {W} {
    global singlePlayer
    #
    if {$singlePlayer == 1} {
	$W.player1.title config -text "Single/Right Player"
	$W.player2.title config -text "Left Player"
    } else {
	$W.player1.title config -text "Right Player"
	$W.player2.title config -text "Single/Left Player"
    }
}

proc createPlayerColorFrame {W} {
    global singlePlayer
    global frameColorLight
    #
    frame $W 
    frame $W.option -relief sunken -borderwidth 2 \
	-background $frameColorLight(.playerColor)
    #
    createColorSelector $W.option.select $W 14
    #
    frame $W.option.color  -background $frameColorLight(.playerColor)
    createPlayerColor $W.option.color.player1 1
    createPlayerColor $W.option.color.player2 2
    setSinglePlayer $W.option.color
    #
    pack $W.option.color.player1 $W.option.color.player2 \
	-fill both -expand yes -padx 4 -pady 4
    #
    pack $W.option.color  -side left -fill both -expand yes \
	-padx 4 -pady 4
    pack $W.option.select -side left -fill both -expand yes \
	-padx 8 -pady 8
    #
    createGenericButtons $W.button  .playerColor 0 \
	{ {Apply} {Query} {Link to Defaults} {Load} {Save} {Exit} } \
	{ applyPlayerColors queryPlayerColors linkPlayerColors \
	      loadPlayerColors savePlayerColors  {destroy .} } 
    bind . <Escape> {destroy .}
    #
    pack $W.option -side top -fill both -expand yes
    pack $W.button -side top -fill x 
}

#############################
#                           # 
# the player resource frame #
#                           #
#############################

proc writePlayerResource {W fileName} {
    global playerResourceFile 
    global singlePlayer
    #
    set resourceList { {} .welcomeMsg .gloatMsg .loseLifeMsg .,winLevelMsg \
	    .winGameMsg .abortMsg .abortCancelMsg }
    set varList {playerName welcomeMsg gloatMsg loseLifeMsg winLevelMsg \
	    winGameMsg abortMsg abortCancelMsg }
    set keyList {upKey downKey leftKey rightKey stopKey bombKey specialKey \
	    abortKey abortCancelKey }
    #
    set fp [open $fileName w]
    #
    # save single Player
    #
    for {set i 0} {$i<8} {incr i} {
	set resource [lindex $resourceList $i]
	set var [lindex $varList $i]$singlePlayer
	global $var
	if {[set $var] != {}} {
	    puts $fp "xblast.singlePlayer${resource}: [set $var]"
	}
    }
    foreach item $keyList {
	set val1 [.playerResource.player$singlePlayer.keys.$item.e1 cget -text]
	set val2 [.playerResource.player$singlePlayer.keys.$item.e2 cget -text]
	if { ($val1 != {}) || ($val2 != {})} {
	    puts $fp "xblast.singlePlayer.${item}: $val1 $val2"
	}
    }
    #
    # save right Player
    #
    for {set i 0} {$i<8} {incr i} {
	set resource [lindex $resourceList $i]
	set var [lindex $varList $i]1
	global $var
	if {[set $var] != {}} {
	    puts $fp "xblast.rightPlayer${resource}: [set $var]"
	}
    }
    foreach item $keyList {
	set val1 [.playerResource.player1.keys.$item.e1 cget -text]
	set val2 [.playerResource.player1.keys.$item.e2 cget -text]
	if { ($val1 != {}) || ($val2 != {})} {
	    puts $fp "xblast.rightPlayer.${item}: $val1 $val2"
	}
    }
    #
    # save left Player
    #
    for {set i 0} {$i<8} {incr i} {
	set resource [lindex $resourceList $i]
	set var [lindex $varList $i]2
	global $var
	if {[set $var] != {}} {
	    puts $fp "xblast.leftPlayer${resource}: [set $var]"
	}
    }
    foreach item $keyList {
	set val1 [.playerResource.player2.keys.$item.e1 cget -text]
	set val2 [.playerResource.player2.keys.$item.e2 cget -text]
	if { ($val1 != {}) || ($val2 != {})} {
	    puts $fp "xblast.leftPlayer.${item}: $val1 $val2"
	}
    }
    #
    close $fp
}

proc readPlayerResource {W file} {
    global singlePlayer
    #
    set fp [open $file r] 
    #
    while {-1!=[gets $fp line]} {
	#
	# check resource line
	#
	set resource [lindex $line 0]
	set value [lrange $line 1 end]
	#puts "V=$value"
	set vlength [llength $value]
	if { $vlength >= 1} {
	    set val1 [lindex $value 0] 
	    if { $vlength >= 2} {
		set val2 [lindex $value 1]
	    } else {
		set val2 {}
	    }
	} else {
	    set val1 {}
	    set val2 {}
	}
	regsub -all \[.:\] $resource " " resource
	if {[string match \[Xx\]\[Bb\]last [lindex $resource 0]]} {
	    switch [lindex $resource 1] {
		{singlePlayer} { set player $singlePlayer }
		{rightPlayer}  { set player 1 }
		{leftPlayer}   { set player 2 }
		default        { set player {} }
	    }
	    if {[llength $resource]==2} {
		if {$player!={}} {
		    global playerName$player
		    set playerName$player $value
		}
	    } else {
		set res [lindex $resource 2] 
		case $res {
		    {welcomeMsg gloatMsg loseLifeMsg loseLevelMsg winGameMsg\
			    winLevelMsg abortMsg abortCancelMsg}    { 
			# messages are easy
			if {$player != {}} {
			    global $res$player
			    set $res$player $value
			}	
		    }
		    {upKey leftKey rightKey downKey stopKey bombKey \
			    specialKey abortKey abortCancelKey} {
			# keys are only a little trickier
			if {$player != {} } {
			    .playerResource.player$player.keys.$res.e1 \
				    config -text $val1
			    .playerResource.player$player.keys.$res.e2 \
				    config -text $val2
			}
		    }
		}
	    }
	}
    }
    close $fp
}

proc savePlayerResource {} {
    global playerResourceFile
    #
    writePlayerResource .playerResource $playerResourceFile
}

proc applyPlayerResource {} {
    global xrdbCommand
    #
    # save to temp file and call xrdb
    #
    set tmpFile ".player_resource"
    #
    writePlayerResource .playerResource $tmpFile
    exec $xrdbCommand -merge $tmpFile
    exec rm $tmpFile
}

proc queryPlayerResource {} {
    global xrdbCommand
    #
    readPlayerResource .playerResource "|$xrdbCommand -query"
}

proc loadPlayerResource {} {
    global playerResourceFile
    readPlayerResource .playerResource $playerResourceFile
}

proc linkPlayerResource {} {
    global playerResourceFile 
    global env
    #
    if {0 == [string first $env(HOME) $playerResourceFile]} {
	set fileName [string range $playerResourceFile  \
		[expr 1+[string length $env(HOME)]] end ]
    } else {
	set fileName $playerResourceFile
    }
    insertResourceLine "#include \"$fileName\""
}

proc createPlayerMsg {W id} {
    global fontSmall
    #
    set itemList {welcome gloat loseLife loseLevel winLevel winGame \
	    abort abortCancel}
    set labelList {Welcome Gloat {Lose Life} {Lose Level} {Win Level} \
	    {Win Game} {Abort Request} {Abort Cancel} } 
    #
    frame $W -relief raised -borderwidth 2
    makeTitle $W.title .playerResource "Messages"
    #
    for {set i 0} {$i < 8} {incr i} {
	set item [lindex $itemList $i]
	set label [lindex $labelList $i]
	#
	frame $W.${item}
	label $W.${item}.l \
		-text "$label Message: " \
		-font $fontSmall -anchor w
	entry $W.${item}.e -background White -width 32 \
		-font $fontSmall \
		-textvariable ${item}Msg${id}
	pack $W.$item.l -side left -fill both -expand yes 
	pack $W.$item.e -side left -fill y
	pack $W.$item -side top -fill x -expand yes -padx 8
    }
}

proc buttonReceiveKey {W K} {
    $W config -text $K
    #
    focus .
}

proc buttonGetFocus {W} {
    global storeBind
    #
    set storeBind [bind . <Return>]
    bind . <Return> {}
    $W config -text {}
    bind $W <Key> "buttonReceiveKey $W %K"
}
proc buttonLooseFocus {W} {
    global storeBind
    #
    bind . <Return> $storeBind
    bind $W <Key> {}
}

proc createPlayerKeys {W id} {
    global fontSmall
    #
    set itemList {upKey leftKey rightKey downKey stopKey bombKey specialKey \
	    abortKey abortCancelKey}
    set labelList { {Go up} {Go left} {Go right} {Go down} {Stop} \
	    {Drop bomb} {Special action} {Request abort} {Cancel abort} }
    #
    frame $W -relief raised -borderwidth 2
    makeTitle $W.title .playerResource "Key Bindings"
    #
    for {set i 0} {$i < 9} {incr i} {
	set item [lindex $itemList $i]
	set label [lindex $labelList $i]
	#
	frame $W.${item}
	label $W.${item}.l \
		-text "$label : " \
		-font $fontSmall -anchor w
	foreach e {e1 e2} {
	    button $W.${item}.${e} \
		    -font $fontSmall \
		    -background White \
		    -width 10 \
		    -relief sunken \
		    -pady 0 \
		    -command "focus $W.$item.${e}"
	    bind $W.${item}.${e} <FocusIn> "buttonGetFocus %W"
	    bind $W.${item}.${e} <FocusOut> "buttonLooseFocus %W"
	}
	#
	pack $W.$item.l -side left -fill both -expand yes 
	pack $W.$item.e1 $W.$item.e2 -side left -fill y -padx 4
	pack $W.$item -side top -fill x -expand yes -padx 8
    }
}

proc createPlayerResource {W id} {
    global fontMedium
    global fontSmall
    global bitmapPath
    global frameColorLight
    #
    frame $W -background $frameColorLight(.playerResource)
    #
    # central player widget 
    #
    frame $W.player \
	    -relief raised -borderwidth 2 
    #
    makeTitle $W.player.title .playerResource "Player" 
    #
    createBigImage $W.player.canvas $id
    #
    label $W.player.l -text "Name:" \
	    -font $fontMedium
    entry $W.player.e -width 16 \
	    -background White \
	    -relief sunken -borderwidth 2 \
	    -font $fontMedium \
	    -textvariable playerName${id}
    #
    pack $W.player.canvas -side top -fill both -expand yes \
	    -pady 4 -padx 4 
    pack $W.player.l $W.player.e \
	    -side top -fill x \
	    -padx 16 -pady 4
    #
    # set msg window(s)
    #  
    createPlayerMsg $W.msg $id
    #
    # set keys window(s)
    #  
    createPlayerKeys $W.keys $id
    #
    pack $W.keys  $W.player $W.msg \
	    -side left -fill both -expand yes \
	    -padx 4 -pady 4

} 

proc setSinglePlayer2 {W} {
    global singlePlayer
    #
    if {$singlePlayer == 1} {
	$W.player1.player.title config -text "Single/Right Player"
	$W.player2.player.title config -text "Left Player"
    } else {
	$W.player1.player.title config -text "Right Player"
	$W.player2.player.title config -text "Single/Left Player"
    }
}

proc createPlayerResourceFrame {W} {
    global singlePlayer
    global frameColorLight
    #
    frame $W -relief sunken -borderwidth 2 \
	    -background $frameColorLight(.playerResource)
    
    #
    createPlayerResource $W.player1 1
    createPlayerResource $W.player2 2
    setSinglePlayer2 $W
    #
    createGenericButtons $W.button  .playerResource 0 \
	    { {Apply} {Query} {Link to Defaults} {Load} \
	    {Save} {Exit} } \
	    { applyPlayerResource queryPlayerResource linkPlayerResource \
	    loadPlayerResource savePlayerResource {destroy .} }
    bind . <Escape> {destroy .}
    #
    pack $W.player1 $W.player2 \
	    -side top -fill both -expand yes \
	    -padx 4 -pady 4
    pack $W.button -side top -fill x
}

###################
#                 #
# the level frame #
#                 #
###################


proc createLevelList {} {
    global xblastCommand
    global hasTclList
    #
    #
    set result {}
    #
    if {$hasTclList} {
	set fp [open "|$xblastCommand -stcl" r]
	#
	# ignore header
	#
	gets $fp line
	gets $fp line
	#
	# get level data
	#
	while {-1 != [gets $fp line]} {
	    lappend result $line
	}
	#
    } else {
	set fp [open "|$xblastCommand -s" r]
	#
	# ignore first two lines
	#
	gets $fp line 
	gets $fp line 
	#
	# get format
	#
	gets $fp line
	#
	set nameLeft [string first Level $line]
	set nameRight [expr [string first Author $line] - 1]
	#
	while {-1 != [gets $fp line]} {
	    set levelID [lindex $line 0]
	    set levelName [string trim [string range \
		    $line $nameLeft $nameRight] " "]
	    lappend result [list $levelID $levelName]
	}
    }
    catch "close $fp" xblastLog
    puts -nonewline $xblastLog
    #
    return $result
}

proc createLevelButtons {W} {
    global fontSmall
    global fontMedium
    global frameColorLight
    #
    # get level list
    #
    set levelList [createLevelList]
    set length [llength $levelList]
    #
    set mod [expr $length % 4]
    set div [expr $length / 4]
    #
    frame $W -relief sunken -borderwidth 2 -background $frameColorLight(.levels)
    #
    for {set i 1} {$i <= 4} {incr i} {
	frame $W.l${i} -relief raised -borderwidth 2 
	makeTitle $W.l${i}.title .levels {}
	pack $W.l${i} -side left -fill both -expand yes -padx 8 -pady 8
    }
    #
    set frame 0
    set nextFrameChange 0
    #
    set l 0
    foreach level $levelList {
	set levelID [lindex $level 0]
	if {$l == $nextFrameChange} {
	    if {$frame != 0} {
		$W.l${frame}.title config \
			-text "Level $startLevel - [expr $levelID-1]"
	    }
	    set startLevel $levelID
	    if {[incr frame] <= $mod} {
		incr nextFrameChange
	    }
	    incr nextFrameChange $div
	}
	global levelSelected
	set levelSelected($levelID) " $levelID"
	checkbutton $W.l${frame}.b${l} \
		-text " [lindex $level 1] " \
		-variable levelSelected($levelID) \
		-onvalue " $levelID" \
		-offvalue {} \
		-font $fontSmall \
		-anchor w
	pack $W.l${frame}.b${l} -side top -fill x  
	incr l
    } 
    $W.l${frame}.title config \
	    -text "Level $startLevel - [expr $levelID-1]"
}

proc getLevelString {} {
    global levelSelected
    #
    if {! [info exists levelSelected]} {
       return {}
    }
    #
    set result "-U"
    set all True
    #
    foreach l [array names levelSelected] {
	if {$levelSelected($l) != {}} {
	    set result "${result}$levelSelected($l)"
	} else {
	    set all False
	}
    }
    if {$all} {
	set result {}
    }
    #
    return $result
}

proc levelSelect {action} {
    global levelSelected
    #
    switch $action {
	{all} {
	    foreach l [array names levelSelected] {
		set levelSelected($l) " $l"
	    }
	}
	{none} {
	    foreach l [array names levelSelected] {
		set levelSelected($l) {}
	    }
	}
	{toggle} {
	    foreach l [array names levelSelected] {
		if {$levelSelected($l) == {} } {
		    set levelSelected($l) " $l"
		} else {
		    set levelSelected($l) {}
		}
	    }
	}
    }
}

proc createLevelFrame {W} {
    #
    frame $W
    #
    createLevelButtons $W.level 
    createGenericButtons $W.button  .levels 0 \
	    { {Start XBlast} {All} {None} {Toggle} {Exit} } \
	    { startXBlast {levelSelect all} {levelSelect none} \
	    {levelSelect toggle} {destroy .} }
    bind . <Escape> {destroy .}
    pack $W.level -side top -fill both -expand yes
    pack $W.button -side top -fill x 
}

########################
#                      #
# xblast colors frame  #
#                      #
########################

proc traceXBlastFont {W name1 name2 ops} {
    global xblastFont
    #
    $W.f.$name2.e delete 0 end
    $W.f.$name2.e insert 0 $xblastFont($name2)
    #
    catch "$W.demo.$name2 config -font $xblastFont($name2)" log
    puts $log
}

proc createFontSelector {W} {
    global fontMedium
    global fontSmall
    global xblastFont
    #
    frame $W -relief raised -borderwidth 2
    makeTitle $W.title .xblastColor "Fonts"
    #
    canvas $W.demo -relief sunken -borderwidth 2 \
	    -width 192 -height 112 \
	    -background White
    #
    pack $W.demo -side left -fill y -padx 4 -pady 2 
    #
    trace variable xblastFont w "traceXBlastFont $W"
    frame $W.f
    foreach F { \
	    {large "Large" 28  -*-helvetica-bold-r-*-*-24-*-*-*-*-*-iso8859-*}\
	    {med  "Medium" 60  -*-helvetica-bold-r-*-*-18-*-*-*-*-*-iso8859-*}\
	    {small "Small" 92 -*-helvetica-bold-r-*-*-14-*-*-*-*-*-iso8859-*}\
	} {
        set path [lindex $F 0]
        set text [lindex $F 1]
        set height [lindex $F 2]
	set value [lindex $F 3]
	#
	label $W.demo.$path -text $text -anchor center -background White
        $W.demo create window 96 $height -window $W.demo.$path
        #
	frame $W.f.$path 
	#label $W.f.$path.l -text "$text:" \
	#	-font $fontMedium \
	#	-anchor w
	entry $W.f.$path.e -width 48 \
		-font $fontSmall \
		-background White
	#
	bind $W.f.$path.e <Return> "set xblastFont($path) \[$W.f.$path.e get\]"
	bind $W.f.$path.e <FocusOut> "set xblastFont($path) \[$W.f.$path.e get\]"
	set xblastFont($path) $value
	#
	#pack $W.f.$path.l -side left -fill x -expand yes 
	pack $W.f.$path.e -side left  
	pack $W.f.$path -side top -fill both -expand yes -padx 8 -pady 2
    }
    pack $W.f -side left -fill both -expand yes
}

proc createTextCanvas {W} {
    global bitmapPath
    #
    # create bitmap images first
    #
    image create bitmap background \
	    -file "${bitmapPath}/bg.xbm"
    image create bitmap lightText1 \
	    -file "${bitmapPath}/light_text_1.xbm" \
	    -maskfile "${bitmapPath}/light_text_1.xbm"
    image create bitmap lightText2 \
	    -file "${bitmapPath}/light_text_2.xbm" \
	    -maskfile "${bitmapPath}/light_text_2.xbm"
    image create bitmap darkText1 \
	    -file "${bitmapPath}/dark_text_1.xbm" \
	    -maskfile "${bitmapPath}/dark_text_1.xbm"
    image create bitmap darkText2 \
	    -file "${bitmapPath}/dark_text_2.xbm" \
	    -maskfile "${bitmapPath}/dark_text_2.xbm"
    #
    # now the canvas
    #
    canvas $W -background White \
	    -relief sunken -borderwidth 2 \
	    -width 192 -height 144 
    
    #
    $W create image 4 4 -anchor nw -image background
    foreach i {lightText1 lightText2 darkText1 darkText2} {
	$W create image 100 76 -anchor center -image $i
    }
    #
    # set default colors
    #
    background config -foreground SpringGreen -background Cyan
    lightText1 config -foreground Yellow
    lightText2 config -foreground Gold
    darkText1 config -foreground Black
    darkText2 config -foreground MidnightBlue
}

proc createTextColor {W} {
    global fontSmall
    #
    frame $W -relief raised -borderwidth 2
    #
    makeTitle $W.title .xblastColor "Text and Background Colors"
    #
    createTextCanvas $W.canvas 
    pack $W.canvas -padx 4 -pady 2 -fill y -side left
    #
    frame $W.l -background red
    frame $W.e
    foreach T { \
		    {lightText1 "Light Color 1:"} \
		    {lightText2 "Light Color 2:"} \
		    {darkText1  "Dark Color 1:" } \
		    {darkText2  "Dark Color 2:" } \
		    {background1  "Background 1:" } \
		    {background2  "Background 2:" } \
		} {
        set path [lindex $T 0]
        set text [lindex $T 1]
	#
	label $W.l.$path -anchor w -font $fontSmall \
	    -text $text
        pack $W.l.$path -side top -fill both -expand yes
        #
        entry $W.e.$path -background white -width 32 -font $fontSmall 
        pack $W.e.$path -side top -expand yes -padx 8
        #
    }
    pack $W.l -side left -fill both -expand yes
    pack $W.e -side left -fill y
}

proc createXBlastColorFrame {W} {
    global frameColorLight
    #
    frame $W
    #
    frame $W.resource -relief sunken -borderwidth 2 \
	    -background $frameColorLight($W)
    #
    frame $W.resource.misc \
	    -background $frameColorLight($W) 
    createFontSelector $W.resource.misc.font
    createTextColor $W.resource.misc.text
    pack $W.resource.misc.font -side top -fill both -expand yes -padx 4 -pady 4
    pack $W.resource.misc.text -side top -fill both -expand yes -padx 4 -pady 4
    #
    createColorSelector $W.resource.color $W 8
    #
    pack $W.resource.misc -side left -fill both -expand yes -padx 4 -pady 4
    pack $W.resource.color -side left -fill both -expand yes -padx 8 -pady 8
    #
    createGenericButtons $W.button $W 0 \
	    { {Apply} {Query} {Link to Defaults} {Load} {Save} {Exit} } \
	{ {} {} {} {} {} {destroy .} }
    bind . <Escape> {destroy .}
    #
    pack $W.resource -side top -fill both -expand yes
    pack $W.button -side top -fill x
}


####################
#                  #
# the main program #
#                  #
####################

#
# initialize 
#
initXBlast
initWish 

#
# wm options
#
wm title . "tkXBlast 2.2.1 - Copyright \251 1996 by Oliver Vogel"
wm iconname . "tkXBlast"
wm iconbitmap . @$bitmapPath/winner_1.xbm 
wm iconmask . @$bitmapPath/winner_mask.xbm 
#
# global Config vars
#
global numPlayer
set numPlayer 2
#
global teamMode
if {$hasTeamMode} {
    set teamMode -single
} else {
    set teamMode {}
}
#
global numLives
set numLives 3
#
global numVictories
set numVictories 5
#
global randomPlayer
set randomPlayer "+rp"
#
global randomLevel
set randomLevel "+rl"
#
global blackWhite
set blackWhite "+bw"
#
global overRide
set overRide "+wm"
#
global soundOption
if {$hasDspSound} {
    set soundOption "-stereo"
} else {
    set soundOption "+q"
}
#
global forkMode
if {$canFork} {
    set forkMode "+F"
} else {
    set forkMode {}
}
#
global gameStat
set gameStat "+P"
#
global frameRate
set frameRate 20

#
global currentFrame
set currentFrame {}
#
global singlePlayer
set singlePlayer 1
#


#
# create title
#
createButtonRow .button

pack .button -side top -fill x 

showFrame .launch

#update idletasks
#wm geometry . [wm geometry .]
wm resizable . false false

pack forget .title

