#!/usr/local/bin/tclsh 

# This file generates a bunch of options for the Tix Display Items styles
#
#
#
if 0 {
set record TixImageTextStyle
set defName DEF_IMAGETEXTSTYLE
} else {
set record TixTextStyle
set defName DEF_TEXTSTYLE
}

if {$argv == "-define"} {
foreach name {NORMAL} {
    set x {}
    append x "#define $defName\_$name\_FG_COLOR	BLACK"
    append x \n
    append x "#define $defName\_$name\_FG_MONO	BLACK"
    append x \n
    append x "#define $defName\_$name\_BG_COLOR	NORMAL_BG"
    append x \n
    append x "#define $defName\_$name\_BG_MONO	WHITE"
    append x \n

    puts $x\n
}

foreach name {ACTIVE SELECTED DISABLED} {
    set x {}
    append x "#define $defName\_$name\_FG_COLOR	BLACK"
    append x \n
    append x "#define $defName\_$name\_FG_MONO	WHITE"
    append x \n
    append x "#define $defName\_$name\_BG_COLOR	NORMAL_BG"
    append x \n
    append x "#define $defName\_$name\_BG_MONO	BLACK"
    append x \n

    puts $x\n
}
}

set prefix(BG,NORMAL)   {-b 	    b 	      B}
set prefix(BG,ACTIVE)   {-activeb   activeB   ActiveB}
set prefix(BG,SELECTED) {-selectb   selectB   SelectB}
set prefix(BG,DISABLED) {-disabledb disabledB DisabledB}

set prefix(FG,NORMAL)   {-f 	    f 	      F}
set prefix(FG,ACTIVE)   {-activef   activeF   ActiveF}
set prefix(FG,SELECTED) {-selectf   selectF   SelectF}
set prefix(FG,DISABLED) {-disabledf disabledF DisabledF}

set grounds(BG) bg
set grounds(FG) fg

set suffix(BG) ackground
set suffix(FG) oreground

if {$argv != "-define"} {
foreach name {NORMAL ACTIVE SELECTED DISABLED} {
    foreach ground {BG FG} {
	foreach mode {COLOR MONO} {
	    set x {}
	    append x \t

	    append x "{"
	    append x TK_CONFIG_COLOR,
	    append x \"[lindex $prefix($ground,$name) 0]$suffix($ground)\",
	    append x \"[lindex $prefix($ground,$name) 1]$suffix($ground)\",
	    append x \"[lindex $prefix($ground,$name) 2]$suffix($ground)\",
	    append x \n\t

	    append x $defName\_$name\_$ground\_$mode,

	    append x \n\t
	    append x Tk_Offset($record, colors\[TIX_DITEM_$name\].$grounds($ground)),
	    append x \n\t

	    append x TK_CONFIG_$mode\_ONLY
	    append x "}"
	    append x ,

	    puts $x
	}
    }
}}

