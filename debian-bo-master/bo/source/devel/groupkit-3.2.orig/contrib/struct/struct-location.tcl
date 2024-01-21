# Structure Radar
# ============================
# Last modified June 28 by Carl Gutwin

# A location awareness widget that uses the structure of the text
# to show location

# Help Description to be added to the help menu
#----------------------------------------------
set help_title "About Structure Radar"

set help_text  {
    {normal} {PROTOTYPE - USE AT YOUR OWN RISK.
This conference demonstrates a structure radar view. The structure of Macbeth is used to generate a table of contents, in which portraits of the conference participants are placed in the correct location. To use your own picture, put <username>.gif into the directory where this conference sits.}
}

gk_initConf $argv

gk_defaultMenu .menu
.menu itemcommand 2 add command \
     -label "$help_title" \
     -command "gk_topicWindow .helpWindow \
          -title \"$help_title\" \
          -text \"$help_text\""

pack .menu -side top -fill x

# environment

gk_newenv -bind -share positions
gk_bind newUserArrived "createOther %U"

set dir "[userprefs scriptpath]/"

proc createPosition {} {
    global dir me

    positions set $me.section 1

    # image setup
    
    set path $dir[users get local.userid].gif

    if {[file exists $path]} {
	image create photo face$me -file $path
    } else {
	set file "generic.gif"
	image create photo face$me -file $dir$file
    }

    label .l.z.face$me -image face$me
    .l.z window create 1.0 -window .l.z.face$me
}


proc createOther {user} {
    global dir
	set file [users remote.$user.userid].gif
	set path $dir$file

	if {[file exists $path]} {
	    image create photo face$user -file $path
	} else {
	    set file "generic.gif"
	    image create photo face$user -file $dir$file
	}

	label .l.z.face$user -image face$user
	.l.z window create 1.0 -window .l.z.face$user
}

positions bind envReceived {
    foreach user [positions keys] {

	set file [users remote.$user.userid].gif
	set path $dir$file

	if {[file exists $path]} {
	    image create photo face$user -file $path
	} else {
	    set file "generic.gif"
	    image create photo face$user -file $dir$file
	}

	label .l.z.face$user -image face$user
	.l.z window create [positions get $user.section].0 \
		-window .l.z.face$user
    }
}

positions bind changeEnvInfo {
    set pieces [split %K .]
    set id [lindex $pieces 0]
    moveFace $id
}



# Globals

set indices {0 8 58 80 173 360 438 535 586 704 791 896 1097 1175 1351 1425 1472 1660 1709 1784 1993 2103 2408 2493 2540 2627 2666 2739 2762 2818 2871 10000}
set current 1
set totalLines 2927

set me [users get local.usernum]

# Window Setup

scrollbar .s -command "scroll yview"
pack .s -side right -fill y

text .t -bg ivory -wrap word -yscrollcommand ".s set" -relief raised \
	-width 50 \
	-font "-*-lucidatypewriter-medium-r-*-*-*-140-*-*-*-*-*-*"
pack .t -fill both -padx 10 -pady 10 -expand yes

toplevel .l
wm title .l "Outline"
wm geometry .l 212x516
wm geometry . 679x392

text .l.z -font "-*-lucidatypewriter-bold-r-*-*-*-140-*-*-*-*-*-*" \
	-bg ivory
pack .l.z -fill both -expand yes

# Text setup

set file "macbeth.heads"
set path $dir$file
set blah  [exec cat $path]

set file "macbeth.txt"
set path $dir$file
set s [exec cat $path]
.t insert end $s
.l.z insert end $blah

# Bindings

bind .t <1> "setCursorPosition %x %y"

# Procedures

proc setCursorPosition {x y} {
    global indices

    set here [.t index @$x,$y]
    set alist [split $here .]
    set line [lindex $alist 0]
    
    checkPosition $line
}

proc scroll {which cmd amount {unit ""}} {
    global totalLines

    if {$unit == ""} {
	.t $which $cmd $amount
    } else {
	.t $which $cmd $amount $unit
    }
    checkPosition [expr [lindex [.t yview] 0] * $totalLines]
}

proc checkPosition {line} {
    global me indices

    # find out what section I'm in

    set count 0
    while {$line > [lindex $indices $count]} {
	incr count
    }

    # if I've moved sections, change the environment

    if {$count != [positions get $me.section]} {
	positions set $me.section $count
    }
}

proc moveFace {user} {

    set section [positions get $user.section]

    .l.z delete .l.z.face$user
    label .l.z.face$user -image face$user
    .l.z window create $section.0 -window .l.z.face$user
}

createPosition


