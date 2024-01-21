#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/initial.tcl,v $
# $Date: 1996/07/01 13:13:33 $
# $Revision: 1.17.1.4 $
#
# ----------------------------------------------------------------------
#   AUTHOR:  Lindsay Marshall <lindsay.marshall@newcastle.ac.uk>
# ----------------------------------------------------------------------
# Copyright 1995 The University of Newcastle upon Tyne (see COPYRIGHT)
# ======================================================================
#
#
proc away {args} { global current ; $current(net) configure +aways [join $args] }
#
proc nick {args} { global current ; $current(net) configure +nicks [join $args] }
#
proc ircname {args} { global current ; $current(net) configure +ircnames [join $args] }
#
proc action {args} { global current ; $current(net) configure +actions [join $args] }
#
proc leave {args} { global current ; $current(net) configure +leaves [join $args] }
#
proc signoff {args} { global current ; $current(net) configure +signoffs [join $args] }
#
proc zbind {chan sequence action} {
    if [string match {} $chan] {
	global current
	$current(net) configure +bindings [list $sequence $action]
    } {
	[Channel :: make $chan] configure +bindings [list $sequence $action]
    }
}
#
proc srvName {srv} {
    if [regexp {(.*):(.*)} $srv m hst prt] {
	if [string compare nil [set sid [Server :: make $hst]]] {
	    if [string compare [$sid port] $prt] {
		set sid [Server [newName srv] -host $hst -port $prt]
	    }
	} {
	    set sid [Server $hst -port $prt]
	}
    } {
	set sid [Server :: make $srv]
    }
    return $sid
}
#
proc setCE {var val evar} {
    global env
    if ![string match {} $val] {
	uplevel set $var "{$val}"
	return 1
    } \
    elseif {[info exists env($evar)]} {
	uplevel set $var "{$env($evar)}"
	return 1
    }
    return 0
}
#
proc envCheck {arg evar op dflt} {
    global env current
    set lst [$current(net) $op]
    if [setCE v $arg $evar] {
	if {[set x [lsearch $lst $v]] > 0} { listdel lst $x }
	$current(net) configure -$op [linsert $lst 0 $v]
    } \
    elseif {[string match {} $lst]} { $current(net) configure -$op $dflt }
}
#
proc layout {net window geom} {
    uplevel #0 set zlayout($net,$window) $geom
}
#
proc srcit {file what} {
    if [file exists $file] {
	if [catch {uplevel #0 source $file} msg] {
	    puts stderr "**** Error in the $what file - $msg"
	    exit 1
	}
	return 1
    }
    return 0
}
#
proc Initialise {} {
    global user argv zircon host defaults
    if [file exists $zircon(lib)/zircon.ad] {
	option readfile $zircon(lib)/zircon.ad startupFile
    }
    option add *Checkbutton*relief flat widgetDefault
    option add *Checkbutton*borderwidth 0 widgetDefault
    option add *CheckButton*padX 5 widgetDefault
    option add *CheckButton*padY 4 widgetDefault
    option add *CheckButton*highlightThickness 0 widgetDefault
    option add *Button*padX 5 widgetDefault
    option add *Button*padY 4 widgetDefault
    option add *Button*highlightThickness 0 widgetDefault
    option add *Menubutton*padX 5 widgetDefault
    option add *Menubutton*pady 4 widgetDefault
    option add *Menubutton*relief raised widgetDefault
    option add *Menubutton*width 10 widgetDefault
    option add *Frame*borderWidth 2 widgetDefault
    option add *Scrollbar*relief raised widgetDefault
    option add *Listbox*relief raised widgetDefault
    option add *Entry*relief raised widgetDefault
    option add *Entry*highlighThickness 1 widgetDefault
    option add *Text*setGrid 1 widgetDefault
    option add *Text*wrap word widgetDefault
    option add *Text*relief raised widgetDefault
    option add *Text*exportSelection 1 widgetDefault
    option add *Canvas*relief raised widgetDefault
    
    foreach x [bind Text] { bind ROText $x [bind Text $x] }
    foreach x {<KeyPress-F20> <KeyPress-F18> <Tab> <Control-i> <Return>
      <Delete> <BackSpace> <Insert> <KeyPress> <Control-d> <Control-k>
      <Control-o> <Control-t> <Meta-BackSpace> <Meta-Delete>
      <Control-h> <ButtonRelease-2>} {
	bind ROText $x {}
    }
    set host [thisHost]
    set user [expr {[info exists env(USER)] ? $env(USER) : [exec whoami]}]
    array set zircon "
	tmp		/tmp
	busymsg		{I am busy and am not accepting calls at the moment.}
	multion		0
	images		0
	bellcmd		bell
	envnick		IRCNICK
	envname		IRCNAME
	envserver	IRCSERVER
	envport		IRCPORT
	prefdir		[glob ~]/.zircon
	reconnect	0
	register	[file exist ~/.zirconreg]
	command		0
	raw		0
	style		original
	language	english
	look		standard
	autos		{join open close menu draw jump quiet}
	ircIImode	0
	action		Shift-Return
	beep		BEEP
	sepColor	red
	ping		0
	ircop 0
	ignore		{Notices Public Invites Wallops Notes CTCP Others}
	nosplit		0
	nicksize	9
	wavplayer	{}
	wavpath		{}
	async		0
    "
    if ![string compare 7.5 [info tclversion]] { set zircon(async) 1 }
    array set defaults [array get zircon]
    array set zircon {
	nameCount 0
	idle	0
	C	0
	j	0
	o	0
	z	0
	i	{}
	N	{}
	S	{}
	p	{}
    }
# Process args
    set opts {}
    foreach arg $argv {
	if [string match {-*} $arg] {
	    foreach bit [split [string range $arg 1 end]] {
		switch $bit {
		C - j - o - z { set zircon($bit) 1 }
		d - r - i - N - S - p { lappend opts $bit}
		default { puts stderr "Unknown option -$bit" }
		}
	    }
	} {
	    set opt [lindex $opts 0]
	    set opts [lrange $opts 1 end]
	    switch $opt {
	    i - N - S - r - p { set zircon($opt) $arg }
	    default { }
	    }
	}
    }
}
#
proc InitGlobals {} {
    global env zircon defaults user current verboseCTCP \
      friendsStyle friendsOn host monitorIn monitorOut \
      noRefresh killPath noConfirm toInfo noPopup popInfo \
      listPattern topicPattern showFriends
    set net $current(net)
    getOption showFriends 0
    getOption friendsStyle window
    getOption smiley ":-\)"
    getOption scowl ":-\("
    getOption wink ";-\)"
    getOption listPattern {.*}
    getOption topicPattern {.*}
    getOption minMembers 3
    foreach arg {noPopup popInfo invisible wallops srvmsg showPrivate
      topicOnly DEBUG monitorIn monitorOut verboseCTCP} {
	getOption $arg 0
    }
    foreach arg {noRefresh friendsOn killPath showLocal showPublic} {
	getOption $arg 1
    }
    foreach arg {noConfirm toInfo ignores} {
	getOption $arg {}
    }
# control panel
    global namesTxt namesChan monitorTime notifyInterval testTime \
      trust helpService
    set notify($net) {}
    set namesTxt($net) {}
    set namesChan($net) {}
    getOption monitorTime 60
    set monitorTime [expr $monitorTime * 1000]
    set notifyInterval 30000
    set testTime $notifyInterval
# Messages
    getOption helpService {}
    array set trust {
	eval	{}
	draw	.+
    }
    global defChan defChat defMsg defNotice
    set defChan [Channel *default*]
    set defChat [Chat *default* -height 10]
    set defMsg [Message *default* -height 10]
    set defNotice [Notice *default* -height 10]
#
# Array variables
#
    makeArray TFg TBg TAF TAB TSplit Heal Split MkOp \
      OnCode Host AChat Offer Send Get Shost 
#
# Source the system and then the user's rc file if they exist. The -z
# flag turns off reading rc files. First source the English language
# in case of errors.
#
    srcit $zircon(lib)/lang/english.tcl "the system Zircon english message"
    catch {set zircon(prefdir) $env(ZIRCONPREFDIR)}
    if [info exists zircon(d)] { set zircon(prefdir) $zircon(d) }
    if !$zircon(z) {
	if [info exists zircon(r)] {
	    srcit $zircon(r) "your $zircon(r)"
	} {
	    set sp $zircon(prefdir)
	    if [srcit $zircon(lib)/rc {the system rc}] {
	        foreach sv [$net servers] { $sv configure -sys 1 }
	        foreach sv [$net services] { $sv configure -sys 1 }
	        foreach sv [$net channels] { $sv configure -sys 1 }
		$defChan configure -sys 0
	    }
	    set zircon(prefdir) $sp
	    if ![srcit $zircon(prefdir)/preferences \
		"your $zircon(prefdir)/preferences"] {
		if [srcit ~/.zirconrc {your .zirconrc}] { upgradeRC }
	    }
	}
    }
    global DEBUG hostIPaddress
    catch {set hostIPaddress $env(HOSTIPADDR)}
    if ![srcit $zircon(prefdir)/look/$zircon(look).tcl \
      "your Zircon $zircon(look) look"] {
	srcit $zircon(lib)/look/$zircon(look).tcl \
	  "the system Zircon $zircon(look)"
    }
    if [string compare english $zircon(language)] {
	srcit $zircon(lib)/lang/$zircon(language).tcl \
	  "system Zircon $zircon(language) message"
    }
    srcit $zircon(prefdir)/lang/$zircon(language).tcl \
      "your Zircon $zircon(language) message"
    srcit $zircon(prefdir)/layout \
      "your Zircon layout"
    envCheck $zircon(N) $zircon(envnick) nicks $user
    envCheck $zircon(i) $zircon(envname) ircnames $user@$host
    if {$monitorIn || $monitorOut} { set DEBUG 1 }
#    setCE v $zircon(p) $zircon(envport)
    set srv nil
    if ![string match {} $zircon(S)] { set srv [srvName $zircon(S)] } \
    elseif {[info exists env($zircon(envserver))]} {
	foreach v [split $env($zircon(envserver))] {lappend sid [srvName $v]}
	set srv [lindex $sid 0]
    } \
    elseif {![string compare nil [$net hostid]]} {
	if ![string compare nil [set srv [Server :: find default]]] {
	    set zircon(C) 2
	}
    }
    if [string compare nil $srv] { $net configure -hostid $srv }
#
# Re-Initialise things in case they were set in the rc file....
#
    array set zircon {
	version		1.17
	patchlevel	3
	windows		0
    }
#
#
#
    global confChange closeTime
    set closeTime [expr [$defChan closetime] * 1000]
    set zircon(ping) [expr $zircon(ping) * 1000]
    set confChange 0
#
# Create the User object for me!! It gets ref'd just below
#
    set nk [lindex [$net nicks] 0]
    foreach x [$net users] {
	Friend [$x name] -notify [$x notify] -id [$x id]
	$x delete
    }
#
# Flag channels that are created in the rc file. This makes sure they
# dont get thrown away when the channel is closed
#
    foreach x [$net channels] { $x configure -keep 1 }
    foreach x [$net messages] { $x configure -keep 1 }
    global showLocal showPublic showPrivate topicOnly minMembers nickname \
      wallops srvmsg invisible
    $net configure -showLocal $showLocal \
      -showPublic $showPublic  -showPrivate $showPrivate \
      -topicOnly $topicOnly -ping $zircon(ping) \
      -minMembers $minMembers \
      -friendsStyle $friendsStyle -helpService $helpService \
      -friendsOn $friendsOn -monitorTime $monitorTime \
      -verboseCTCP $verboseCTCP -noRefresh $noRefresh \
      -killPath $killPath -showFriends $showFriends \
      -noConfirm $noConfirm -toInfo $toInfo \
      -noPopup $noPopup -popInfo $popInfo -topicPattern $topicPattern \
      -listPattern $listPattern -wallops $wallops -srvmsg $srvmsg \
      -invisible $invisible -nicksize $zircon(nicksize) \
      -notifyInterval $notifyInterval -testTime $testTime \
      -closeTime $closeTime
}
#
class Channel {
    name	{}
    lname	{}
    nocase	0
    open	0
    close	0
    history	50
    draw	1
    jump	1
    quiet	0
    hpos	end
    actions	0
    logfile	{}
    logfd	{}
    closetime	0
    crypt	{}
    buttons	1
    closecount	0
    window	{}
    patterns	{}
    messages	{}
    menu	0
    join	0
    ops		{}
    msg		{}
    bindings	{}
    icon	{}
    topics	{}
    keep	0
    monitor	0
    key		{}
    limit	{}
    text	{}
    foreground	{}
    background	{}
    font	{}
    geometry	{}
    height	24
    width	80
    boldfont	{}
    sys		0
    actionmode	0
    p		0
    m		0
    s		0
    i		0
    t		0
    n		0
    net		{}
    cinfo	{}
}
#
# Configuration panel stuff
#
array set cVars {
    IRC		{nicks ircnames}
    Channels	{}
    People	{ignores showFriends friendsOn}
    Info	{showLocal showPublic showPrivate topicOnly minMembers \
		 noConfirm toInfo popInfo verboseCTCP helpService \
		 noPopup noRefresh killPath listPattern topicPattern \
		 notifyInterval }
    Others	{aways actions invisible wallops srvmsg leaves signoffs}
}
#
array set confData {
    channel	{{{Auto Join} join} {{Pop Up} open} \
		 {{Pop Down} close} {{On Menu} menu} {Draw draw} \
		 {Jump jump} {Quiet quiet} {{Nocase} nocase}}
    single	{showLocal showPublic showPrivate \
		 topicOnly minMembers popInfo verboseCTCP helpService \
		 invisible wallops srvmsg noRefresh noPopup friendsOn
		 killPath listPattern topicPattern showFriends notifyInterval}
    msg		{Join Kick Kill Leave Mode Quit Topic}
    info	{Ctcp Signoff Who Whois Whowas Error Ison Info}
    nconf	{Quit Leave Kill SaveConf}
}
