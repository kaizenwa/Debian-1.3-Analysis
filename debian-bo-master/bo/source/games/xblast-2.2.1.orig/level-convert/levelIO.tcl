#
# XBlast Level Editor V0.1
#
# (c) 1996 by Oliver Vogel
#
# file levelIO.tcl - load and saving level data
#
#

#
# procedure mergeLevel 
#
proc mergeLevel {inFile outFile} {
    set fin [open $inFile r]
    set fout [open $outFile w]
    
    # print header
    gets $fin line
    puts -nonewline $fout "set levelHeader \{ "
    while {! [string match "*BMLevelData*" $line]} {
	gets $fin line
	puts -nonewline $fout "\"$line\" "
    } 
    puts $fout "\}"
    puts $fout "set levelID [lindex $line 2]"
    set header $line
    puts -nonewline $fout "set levelData "

    # get rest of file
    while {-1!=[gets $fin line]} {
	if {(-1 != [string first "GM_" $line]) \
	    ||(-1 != [string first "IF_" $line])} {
	    regsub -all " \[|\]" $line "|" newLine
	    set line $newLine
	    regsub -all "\[|\] " $line "|" newLine
	    set line $newLine
	    regsub -all "," $line " " newLine
	    set line $newLine
	} else {
	    regsub -all "," $line " " newLine
	    set line $newLine
	    regsub -all ";" $line " " newLine
	}
	puts -nonewline $fout $line
    }
    puts $fout ""
    #
    close $fin
    close $fout
    #
    return $header
}

#
# procedure setLevelVars 
#
# sets all global level varaiables
#
proc setLevelVars {ld} {
    #
    # level data variables
    #
    global levelName
    global authorName
    global resourceName
    global gameMode
    global scrambleDraw
    global scrambleDel
    global shrinkFunc
    global initFunc
    global gameFunc
    global extraFunc
    global keyFunc
    global playerPos
    global initBombs
    global initRange
    global bombDir
    global initFlags
    global initHealth
    global fuseTime
    global defaultBomb
    global buttonBomb
    global evilBomb
    global extraProb
    global blockData 
    global shadowMode
    global extraDist
    global bombClick
    global wallClick
    global playerClick
    global levelTip
    global blockMap
    #
    # check length of data
    #
    if {30 != [llength $ld]} {
	puts "Error: wrong level data format"
	return 1
    }
   
    # level name
    set levelName [lindex $ld 0]
    # author name
    set authorName [lindex $ld 1]
    # resourceName
    set resourceName [lindex $ld 2]
    # game mode flags as list
    regsub -all "\[|\]" [lindex $ld 3] " " gameMode
    # scramele blocks
    set scrambleDraw [lindex $ld 4]
    set scrambleDel [lindex $ld 5]
    # shrink and other function
    set shrinkFunc [lindex $ld 6]
    set initFunc   [lindex $ld 7]
    set gameFunc   [lindex $ld 8]
    set extraFunc  [lindex $ld 9]
    set keyFunc    [lindex $ld 10]
    # position array
    set j 1
    foreach i [lindex $ld 11] {
	set playerPos($j) $i
	incr j
    }
    if {$j != 7} {
	puts "Warning: only [expr $j -1] player positions are given"
	while {$j <= 6} {
	    set playerPos($j) "-1 -1"
	    incr j
	}
    }
    # initial equipment
    set initBombs [lindex $ld 12]
    set initRange [lindex $ld 13]
    # initial bomb dir
    set bombDir [lindex $ld 14]
    # initial status
    regsub -all "\[|\]" [lindex $ld 15] " " initFlags
    set initHealth [lindex $ld 16]
    # bomb attributes
    set fuseTime [lindex $ld 17]
    set defaultBomb [lindex $ld 18]
    set buttonBomb [lindex $ld 19]
    set evilBomb [lindex $ld 20]
    # extra probs
    set j 0 
    foreach i [lindex $ld 21] {
	set extraProb($j) $i
	incr j
    }
    # block data
    set j 0 
    foreach i [lindex $ld 22] {
	set blockData($j) $i
	incr j
    }
    for {} {$j <= 10} {incr j} {
	set blockData($j) {BLScoreFloor RoyalBlue RoyalBlue RoyalBlue}
    }
    # shadow mode
    set shadowMode [lindex $ld 23]
    # extra distribution
    set extraDist [lindex $ld 24]
    # bomb click behavior
    set bombClick [lindex $ld 25]
    set wallClick [lindex $ld 26]
    set playerClick [lindex $ld 27]
    set levelTip [lindex $ld 28]
    # set block map
    set x 0
    foreach i [lindex $ld 29] {
	set y 0
	foreach j $i {
	    set blockMap($x,$y) $j
	    incr y
	}
	incr x
    }
    return 0
}



#
# procedure loadLevel
#
# load a level from file in XBlast 2.0 format
#
proc loadLevel {levelFile} {
    global levelID
    global levelHeader
    #
    # check if level file exists
    #
    if {![file exists $levelFile]} {
	puts "Error: level file \"$levelFile\" does not exist."
	return 1
    }
    #
    # merge level in to set instruction
    #
    mergeLevel $levelFile .merge.dat
    source .merge.dat 
    # exec rm .merge.dat
    #
    # set level variables for list
    #
    setLevelVars $levelData
    #
    return 1
}


#
# procedure saveLevel
#
# save current level to file in 2.0 format
#
proc saveLevel {levelFile} {
    #
    global levelID
    global levelName
    global authorName
    global resourceName
    global gameMode
    global scrambleDraw
    global scrambleDel
    global shrinkFunc
    global initFunc
    global gameFunc
    global extraFunc
    global keyFunc
    global playerPos
    global initBombs
    global initRange
    global bombDir
    global initFlags
    global initHealth
    global fuseTime
    global defaultBomb
    global buttonBomb
    global evilBomb
    global extraProb
    global blockData 
    global shadowMode
    global extraDist
    global bombClick
    global wallClick
    global playerClick
    global levelTip
    global blockMap
    #
    # open output file
    #
    set fp [open $levelFile w]
    #
    # write header
    #
    puts "/* XBlast 2.0 level */"
    #
    # write data
    #
    puts "static BMLevelData $levelID ="
    puts "\{"
    puts "  \"$levelName\","
    puts "  \"$authorName\","
    puts "  \"$resourceName\","
    # game mode
    puts -nonewline "  "
    set l [llength $gameMode] 
    set j 1
    foreach i $gameMode {
	puts -nonewline $i
	if {$j != $l} {
	    puts -nonewline " | "
	} else {
	    puts ","
	}
	incr j
    }
    # scrambles
    puts "  $scrambleDraw,"
    puts "  $scrambleDel,"
    # functions
    puts "  $shrinkFunc,"
    puts "  $initFunc,"
    puts "  $gameFunc,"
    puts "  $extraFunc,"
    puts "  $keyFunc,"
    # player positions
    puts "  \{"
    for {set i 1} {$i <= 6} {incr i} {
	puts "    \{ [format "%2s, %2s" [lindex $playerPos($i) 0] \
		[lindex $playerPos($i) 1]] \},"
    }
    puts "  \},"
    # initial equipment
    puts "  $initBombs, $initRange, $bombDir, $initFlags, $initHealth,"
    # bomb behavior
    puts "  $fuseTime, $defaultBomb, $buttonBomb, $evilBomb,"
    # extra probs
    puts -nonewline "  \{ "
    for {set i 0} {$i < 4} {incr i} {
	puts -nonewline "$extraProb($i), "
    }
    puts "$extraProb(4) \},"
    # block data
    puts "  \{"
    for {set i 0} {$i < 11} {incr i} {
	if {1 == [llength $blockData($i)] } {
	    puts "    $blockData($i),"
	} else {
	    puts "    \{ \
		    [lindex $blockData($i) 0],\
		    \"[lindex $blockData($i) 1]\",\
		    \"[lindex $blockData($i) 2]\",\
		    \"[lindex $blockData($i) 3]\" \},"
	}
    }
    puts "  \},"
    # further modes
    puts "  $shadowMode, $extraDist,"
    puts "  $bombClick, $wallClick, $playerClick,"
    # level hint
    puts "  \"$levelTip\","
    # block map
    puts "  \{"
    for {set x 0} {$x < 15} {incr x} {
	puts -nonewline "    \{ "
	for {set y 0} {$y < 13} {incr y} {
	    puts -nonewline "$blockMap($x,$y),"
	}
	puts " \},"
    }
    puts "  \},"
    # the end
    puts "\};"
}



#
# procedure saveLevel
#
# save current level to file in 2.0 format
#
proc saveLevel218 {levelFile} {
    #
    global levelID
    global levelHeader
    global levelName
    global authorName
    global resourceName
    global gameMode
    global scrambleDraw
    global scrambleDel
    global shrinkFunc
    global initFunc
    global gameFunc
    global extraFunc
    global keyFunc
    global playerPos
    global initBombs
    global initRange
    global bombDir
    global initFlags
    global initHealth
    global fuseTime
    global defaultBomb
    global buttonBomb
    global evilBomb
    global extraProb
    global blockData 
    global shadowMode
    global extraDist
    global bombClick
    global wallClick
    global playerClick
    global levelTip
    global blockMap
    #
    # open output file
    #
    set fp [open $levelFile w]
    #set fp file1
    #
    # write header
    #
    puts $fp "/* XBlast 2.1.8 level */"
    #
    foreach l $levelHeader {
	puts $fp $l
    }
    #
    # write data
    #
    #puts $fp "static BMLevelData $levelID ="
    puts $fp "\{"
    #
    # first the BMLevel struct 
    #
    puts $fp "  /* BMLevel */"
    puts $fp "  \{"
    puts $fp "    \"$levelName\","
    puts $fp "    \"$authorName\","
    puts $fp "    \"$resourceName\","
    puts $fp "    \"$levelTip\","
    # game mode
    set l [llength $gameMode] 
    set j 1
    puts -nonewline $fp "    "
    foreach i $gameMode {
	puts -nonewline $fp $i
	if {$j != $l} {
	    puts -nonewline $fp " | "
	} else {
	    puts $fp ","
	}
	incr j
    }
    puts $fp "    (void *) &$levelID," 
    puts $fp "    NULL," 
    puts $fp "  \},"
    #
    # now the BMShrink struct
    #
    puts $fp "  /* BMShrinkData */"
    puts $fp "  \{"
    puts $fp "    $shrinkFunc,"
    if {1 == [llength $scrambleDraw]} {
	puts $fp "    $scrambleDraw,"
    } else {
	puts -nonewline $fp "    \{ "
	foreach l $scrambleDraw {
	puts -nonewline $fp "$l, "
	}
	puts $fp "\},"
    }
    if {1 == [llength $scrambleDel]} {
	puts $fp "    $scrambleDel,"
    } else {
	puts -nonewline $fp "    \{ "
	foreach l $scrambleDel {
	puts -nonewline $fp "$l, "
	}
	puts $fp "\},"
    }
    puts $fp "  \},"
    #
    # the BMFuncData struct
    #
    puts $fp "  /* BMFuncData */"
    puts $fp "  \{"
    puts $fp "    $initFunc,"
    puts $fp "    $gameFunc,"
    puts $fp "    $extraFunc,"
    puts $fp "    $keyFunc,"
    puts $fp "  \},"
    #
    # the BMPlayerData struct 
    #
    puts $fp "  /* BMPlayerData */"
    puts $fp "  \{"
    puts $fp "    $initRange, $initBombs,"
    # player positions
    puts $fp "    \{"
    for {set i 1} {$i <= 6} {incr i} {
	puts $fp "      \{ [format "%2s, %2s" [lindex $playerPos($i) 0] \
		[lindex $playerPos($i) 1]] \},"
    }
    puts $fp "    \},"
    puts $fp "    PM_Same,"
    puts -nonewline $fp "    $initHealth, "
    set l [llength $initFlags]
    set j 1
    foreach i $initFlags {
	puts -nonewline $fp $i
	if {$j != $l} {
	    puts -nonewline $fp " | "
	} else {
	    puts $fp ","
	}
	incr j
    }
    puts $fp "  \},"
    #
    # the bomb data struct 
    #
    puts $fp "  /* BMBombData */"
    puts $fp "  \{"
    case $bombClick {
	{BC_None}          { puts -nonewline $fp "    bomb_click_none,"}
	{BC_Rebound}       { puts -nonewline $fp "    bomb_click_rebound,"}
	{BC_Contact}       { puts -nonewline $fp "    bomb_click_contact,"}
	{BC_Clockwise}     { puts -nonewline $fp "    bomb_click_clockwise," }
	{BC_Anticlockwise} { puts -nonewline $fp "    bomb_click_anticlockwise,"}
	{BC_Random}        { puts -nonewline $fp "    bomb_click_randomdir,"}
	{BC_Snooker}       { puts -nonewline $fp "    bomb_click_snooker,"}
    }
    case $wallClick {
	{WC_None}          { puts -nonewline $fp " bomb_click_none," }
	{WC_Rebound}       { puts -nonewline $fp " bomb_click_rebound," }
	{WC_Contact}       { puts -nonewline $fp " bomb_click_contact," }
	{WC_Clockwise}     { puts -nonewline $fp " bomb_click_clockwise," }
	{WC_Anticlockwise} { puts -nonewline $fp " bomb_click_anticlockwise," }
	{WC_Random}        { puts -nonewline $fp " bomb_click_randomdir," }
    }
    case $playerClick {
	{PC_StunStop}     { puts $fp " bomb_click_none," }
	{PC_StunThruInit} { puts $fp " bomb_click_initial," }
	{PC_StunThru}     { puts $fp " bomb_click_thru," }
	{PC_Contact}      { puts $fp " bomb_click_contact," }
	{PC_Rebound}      { puts $fp " bomb_click_rebound," }
    }
    puts $fp "    $bombDir, $fuseTime,"
    puts $fp "    $defaultBomb, $buttonBomb, $evilBomb,"
    puts $fp "  \},"
    #
    # the BMGraphicsData struct
    #
    puts $fp "  /* BMGraphicsData */"
    puts $fp "  \{"
    puts $fp "    \{"
    for {set i 0} {$i < 11} {incr i} {
	if {1 == [llength $blockData($i)] } {
	    puts $fp "      $blockData($i),"
	} else {
	    puts $fp "      \{\
		    [lindex $blockData($i) 0],\
		    \"[lindex $blockData($i) 1]\",\
		    \"[lindex $blockData($i) 2]\",\
		    \"[lindex $blockData($i) 3]\" \},"
	}
    }
    puts $fp "    \},"
    puts $fp "  \},"
    #
    # the BMMapData struct
    #
    puts $fp "  /* BMMapData */"
    puts $fp "  \{"
    puts $fp "    $shadowMode, $extraDist,"
    # extra probs
    puts -nonewline $fp "    \{ "
    for {set i 0} {$i < 4} {incr i} {
	puts -nonewline $fp "$extraProb($i), "
    }
    puts $fp "$extraProb(4) \},"
    # block map
    puts $fp "    \{"
    for {set x 0} {$x < 15} {incr x} {
	puts -nonewline $fp "      \{ "
	for {set y 0} {$y < 13} {incr y} {
	    puts -nonewline $fp "$blockMap($x,$y),"
	}
	puts $fp " \},"
    }
    puts $fp "    \},"
    puts $fp "  \},"

    #
    # the end
    #
    puts $fp "\};"
    #
    close $fp
}






