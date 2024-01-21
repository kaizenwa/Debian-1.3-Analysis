# Pong
# ============================
# Last modified June 28 by Carl Gutwin

# Pong. What more do you need to know?

# Help Description to be added to the help menu
#----------------------------------------------
set help_title "About Pong"

set help_text  {
    {normal} {The first and second entrants to this conference can play Pong, the greatest video game ever built.}
}

gk_initConf $argv

wm title . "Pong"
wm geometry . 640x480

gk_defaultMenu .menu
.menu itemcommand 2 add command \
     -label "$help_title" \
     -command "gk_topicWindow .helpWindow \
          -title \"$help_title\" \
          -text \"$help_text\""

pack .menu -side top -fill x

set canvasWidth 630
set canvasHeight 470

set leftY 10
set leftX 30
set rightY 10
set rightX [expr $canvasWidth - 30]

set ballWidth 30
set ballX [expr $canvasWidth / 2]
set ballY [expr $canvasHeight / 2]

set speed 10
set updown 0
set englishEffect 0.4
set lastMoves {0 0 0}

set leftScore 0
set rightScore 0

canvas .c -width $canvasWidth -height $canvasHeight \
	-bg black
pack .c

.c create rectangle 10 10 30 100 -fill orange -tags left
.c create rectangle $rightX 10 [expr $canvasWidth - 10] 100 \
	-fill red -tags right
.c create rectangle $ballX $ballY [expr $ballX + 30] [expr $ballY + 30] \
	-fill yellow -tags ball

if [gk_amOriginator] {
    # me left
    set me left
    after 1000 moveBall
} else {
    # me right
    set me right
}

bind .c <Motion> "movePaddle %y"


#______________Procs_______________

proc movePaddle y {
    global me lastMoves leftY rightY
    
    set y [.c canvasy $y]

    if {$me == "left"} {
	set dY [expr $y - $leftY]
	gk_toAll .c move $me 0 $dY
	gk_toAll set leftY $y
    } else {
	set dY [expr $y - $rightY]
	gk_toAll .c move $me 0 $dY
	gk_toAll set rightY $y
    }
    set lastMoves [linsert $lastMoves 0 $dY] 
    set lastMoves [lreplace $lastMoves 3 3]
}

proc dampPaddle {} {
    global leftY rightY lastMoves me

    if {$me == "left"} {
    set lastMoves [linsert $lastMoves 0 0] 
    set lastMoves [lreplace $lastMoves 3 3]
    } else {
    set lastMoves [linsert $lastMoves 0 0] 
    set lastMoves [lreplace $lastMoves 3 3]
    }
}

proc pointScored {who} {
    global ballX ballY canvasWidth canvasHeight leftScore rightScore \
	    ballWidth updown

    set ballX [expr $canvasWidth / 2]
    set ballY [expr $canvasHeight / 2]

    set updown [expr $updown / 2]

    if {$who == "left"} {incr leftScore} else {incr rightScore}
    puts "Left: $leftScore; Right: $rightScore"

    if [gk_amOriginator] {
	#restart the ball
	after 1000 "gk_toAll .c coords ball $ballX $ballY \
		[expr $ballX + $ballWidth] \
	    [expr $ballY + $ballWidth]; moveBall"
    }
}

proc moveBall {} {
    global ballX ballY leftY leftX rightY rightX speed canvasWidth canvasHeight \
	    me ballWidth lastMoves englishEffect updown

    # is the ball close to the paddle

    if {($ballX <= $leftX) || ($ballX >= [expr $rightX - $ballWidth])} {
	
	# is the ball on the paddle
	
	if {$speed < 0} {
	    # moving left, check left paddle
	    
	    if {($ballY <= [expr $leftY + 90]) && ($ballY >= $leftY)} {
		## ball hits left paddle, so bounce

		set speed [expr $speed * -1]

		# calculate english (+ve or -ve depending on direction of paddle)

		set paddleSpeed [expr ([lindex $lastMoves 0] + \
			[lindex $lastMoves 1] + \
			[lindex $lastMoves 2]) / 3]
		set english [expr $paddleSpeed * $englishEffect]
		set updown [expr $updown + $english]
	    } else {
		# ball passes paddle, left loses
		return [pointScored right]
		#set speed [expr $speed * -1]
	    }
	} else {  
	    # moving right, check right paddle

	    if {($ballY <= [expr $rightY + 90]) && ($ballY >= $rightY)} {
		## ball hits right paddle, so bounce

		set speed [expr $speed * -1]

		# calculate english (+ve or -ve depending on direction of paddle)

		set paddleSpeed [expr ([lindex $lastMoves 0] + \
			[lindex $lastMoves 1] + \
			[lindex $lastMoves 2]) / 3]
		set english [expr $paddleSpeed * $englishEffect]
		set updown [expr $updown + $english]
	    } else {
		# ball passes paddle, right loses
		return [pointScored left]
		#set speed [expr $speed * -1]
	    }
	}
    } else {
	# the ball is not near the paddles, so check if it hits the 
	# floor/ceiling and bounce off

	if {[expr $ballY + $updown] <= 0} {set updown [expr $updown * -1]}
	if {([expr $ballY + $updown] >= [expr $canvasHeight - $ballWidth]) && \
		($updown > 0)} {set updown [expr $updown * -1]}
    }


    gk_toAll .c move ball $speed $updown
    set ballX [expr $ballX + $speed]
    set ballY [expr $ballY + $updown]

    gk_toAll dampPaddle
    gk_toAll update idletasks
    after 10 moveBall
}
