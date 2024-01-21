#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/Channel.tcl,v $
# $Date: 1996/06/24 14:36:48 $
# $Revision: 1.17.1.5 $
#
global defChan
set defChan {}
if [catch {foreach x {clear leave names action mode channel} {
    if [file exists $zircon(lib)/gifs/${x}.gif] {
	set image1($x) [image create photo -file $zircon(lib)/gifs/$x.gif]
	set image2($x) [image create photo -file $zircon(lib)/gifs/${x}2.gif]
	if [file exists $zircon(lib)/gifs/${x}3.gif] {
	    set image3($x) [image create photo -file $zircon(lib)/gifs/${x}3.gif]
	}
    }
}}] {
    catch {unset image1 image2}
}
#
proc addImage {w name prnt} {
    global image1 image2 image3 zircon
    if {$zircon(images) && [info exists image1($name)]} {
	$w configure -image [set i1 $image1($name)] \
	  -width [image width $i1] -height [image height $i1] \
	  -borderwidth 0 -relief flat -highlightthickness 0 -padx 0 -pady 0
	bind $w <Enter> "$w configure -image $image2($name)"
	bind $w <Leave> "$w configure -image $i1"
	if [info exists image3($name)] {
	    bind $w <ButtonPress-1> "
		$w configure -image $image3($name)
		bind $w <Leave> {}
	    "
	    bind $w <ButtonRelease-1> "
		$w configure -image $image1($name)
		bind $w <Leave> {$w configure -image $i1}
	    "
	}
	pack $w -in $prnt -side left -expand 1
    } {
	pack $w -in $prnt -side left -expand 1 -fill x
    }
}
#
proc nil {op args} {
    switch $op {
    extendTime -
    window { return {} }
    nocase -
    active -
    operator -
    isa { return 0 }
    opText -
    addText {global current ; eval [$current(net) info] addText $args}
    default { error "***** \007\007 nil called : $op $args" }
    }	
}
#
proc Channel {name args} {
    if ![string compare :: $name] {
	return [eval Channel_[lindex $args 0] [lrange $args 1 end] ]
    }
    if ![string compare nil [set id [Channel :: find $name]]] {
	set id [makeChannel $name]
    }
    if ![string match {} $args] { eval $id configure $args }
    return $id
}
#
proc cleanup {name} {
    set pattern "\[ ,\n\r\]"
    regsub -all "$pattern" $name {} name
    return $name
}
#
proc oneLiner {win chid} {
    notIdle $win
    set x [split [$win get]]
    if {![string match {} [set who [lindex $x 0]]] &&
      ![string match {} [set msg [join [lrange $x 1 end]]]]} {
	[$chid net] PRIVMSG $who $msg
	$chid addText {} ">>$who : $msg"
    }
    $win delete 0 end
}
#
proc palIns {ch chan} { [$chan window].cmdLine.commandLine insert insert $ch }
#
proc channel_makePalette {this} {
    set w .@palette
    if [winfo exists $w] {popup $w ; return} 
    global Ft
    killWindow $w
    toplevel $w -class Zircon
    wm title $w "[$this name] Palette"
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    for {set i 2} {$i < 16} {incr i} {
	frame $w.f$i -borderwidth 0
	for {set j 0} {$j < 16} {incr j} {
	    set c [format "%c" [expr {$i * 16 + $j}]]
	    button $w.f$i.l$j -text $c -relief raised -width 1 \
	      -command "palIns {$c} $this" -font $Ft($this)
	    pack $w.f$i.l$j -side left
	}
	pack $w.f$i -side top
    }
    button $w.ok -text Done -command "destroy $w"
    pack $w.ok -fill x
}
#
proc setClose {this val} {
    if {[regexp {^[0-9]+$} $val]} { $this configure -closetime $val }
}
#
proc channel_set {this flag} {
    switch $flag {
    close {
	    if [$this close] {
		global ztrans
		if ![string compare [set v [$this closetime]] 0] { set v {} }
		mkEntryBox .@cl$this {Close Time} \
		  "Enter the close time for [$this name]:" \
		  "{Time $v}" "$ztrans(ok) {setClose $this}" \
		  "$ztrans(cancel) {}"
	        tkwait window .@cl$this
	    }
	}
    nocase {
	
	}
    }
}
#
proc setCrypt {this key} {
    $this configure -crypt $key
    if [$this isa Message] {
	[User :: find [$this name]] configure -crypt $key
    }
}
#
proc clearCrypt {this args} { setCrypt $this {} }
#
proc channel_getCrypt {this} {
    global ztrans
    mkEntryBox .@cr$this {Encryption Key} \
      "Enter the encryption key for [$this name]:" \
      "{$ztrans(key) [$this crypt]}" "$ztrans(ok) {setCrypt $this}" \
      "$ztrans(clear) {clearCrypt $this}" "$ztrans(cancel) {}"
    tkwait window .@cr$this
}
#
proc channel_drawSet {this} {
    global ztrans
    set mn [$this window].channel.menu
    $mn entryconfigure [$mn index $ztrans(draw)] \
      -state [expr {[$this draw] ? {normal} : {disabled}}]
}
#
proc makeChannel {chan} {
    set this [objName Channel]
    initObj $this Channel
    global defChan current
    proc $this {args} "eval channel_call $this \$args"
    set lchan [string tolower $chan]
    set net $current(net)
    upvar #0 $this cdata CTO$net CTO
    if ![string match {} $defChan] {
	array set cdata [uplevel #0 array get $defChan]
    } 
    set cdata(name) $chan
    set cdata(lname) $lchan
    set cdata(keep) 0
    set cdata(net) $net
    $net register channels $this
    set CTO($lchan) $this
    return $this
}
#
proc channel_show {this args} {
    if ![$this active] {
	global zircon
	switch $zircon(style) {
	hicaffiene -
	diet -
	original {$this original $args}
	}
    }
    if [$this isa Channel] { [$this net] qSend MODE :[$this name] }
}
#
proc channel_operator {this} {return [$this isOp [[$this net] myid]]}
#
proc channel_speaker {this} {return [$this isSpeaker [[$this net] myid]]}
#
proc channel_call {this op args} {
    upvar #0 $this cdata
    switch $op {
    isa { global OType ; return [expr {$OType($this) == [lindex $args 0]}] }
    closetime { return [expr $cdata(closetime) / 1000]}
    window {
	    if [string match {} $cdata(window)] { return {} }
	    return [$cdata(window) name]
	}
    active { return [expr ![string match {} $cdata(window)]] }
    }
    if [info exists cdata($op)] { return $cdata($op) }
    return [eval channel_$op $this $args]
}
#
proc channel_quiesce {this} {
}
#
proc channel_setTopic {this string} {
    set w [$this window].topic.entry
    set state [$w cget -state]
    $w configure -state normal
    $w delete 1.0 end
    insertText $this $w $string {}
    if ![string compare $state disabled] {
	$w configure -state disabled -relief raised
    } {
	$w configure -relief sunken
    }
}
#
proc channel_changeMode {this mode} {
    [$this net] MODE [$this name] [expr {[$this $mode] ? {+} : {-}}]${mode}
}
#
proc channel_optText {this name string} {
    if [$this wantMessage $name] { $this addText @$name $string }
}
#
proc channel_addText {this tag text} {
    $this doPopUp
    $this doAddText $tag $text
}
#
proc tagConf {txt tag fg bg ft} {
    catch {$txt tag configure $tag -foreground $fg}
    catch {$txt tag configure $tag -background $bg}
    if ![string match {} $ft] {catch {$txt tag configure $tag -font $ft}}
}
#
proc confTag {txt tag fg bg font bold} {
    if ![string match {} $tag] { tagConf $txt $tag $fg $bg $font }
    tagConf $txt @b@$tag $fg $bg $bold
    tagConf $txt @v@$tag $bg $fg {}
    tagConf $txt @u@$tag $fg $bg {}
    $txt tag configure @u@$tag -underline 1
    tagConf $txt @a@$tag $bg $fg {}
    $txt tag configure @a@$tag -relief raised -borderwidth 2
}
#
proc channel_makeTag {this usr} {
    global Fg Bg BF
    if [[$this net] me $usr] {
	set tagInfo [$this tagInfo [set tag @me]]
    } {
	set tagInfo [$this tagInfo [[set tag $usr] lname]]
    }
    set name [$this text]
    confTag $name $tag [lindex $tagInfo 0] [lindex $tagInfo 1] \
       [lindex $tagInfo 2] [expr {$BF($this) == {} ? $font : $BF($this)}]
}
#
proc channel_delTag {this usr} {
    global TFn TFa TFg TBg TAF TAB TBl
    if [[$this net] me $usr] { set tag @me } { set tag [$usr lname] }
    foreach x {TFn TFa TFg TBg TAF TAB TBl} { catch {unset ${x}($this,$tag)} }
}
#
proc channel_tagInfo {this tag} {
    if [string match {} $tag] { return {} } {
	global TFn TFg TBg
	set indx "$this,$tag"
	if ![info exists TFn($indx)] { setTags $this $tag }
	return [list $TFg($indx) $TBg($indx) $TFn($indx)]
    }
}
#
proc channel_display {this string} {$this send $string}
#
proc channel_send {this string args} {
    if ![string match {} $string] {
	global Secure
	set lc [$this lname]
	set rchan [expr \
	  {[info exists Secure($lc)] ? $Secure($lc) : [$this name]}]
	[$this net] PRIVMSG $rchan [encrypt $string [$this crypt]]
	if [string match {} $args] {
	    $this addText @me "> $string"
	} {
	    $this doAddText @me "> $string"
	}
	$this configure -hpos end
    }
}
#
proc channel_isOp {this usr} {
    upvar #0 $this cdata
    if ![info exists cdata(Op,$usr)] { return 0 }
    return $cdata(Op,$usr)
}
#
proc channel_isSpeaker {this usr} {
    upvar #0 $this cdata
    if ![info exists cdata(Spk,$usr)] { return 0 }
    return $cdata(Spk,$usr)
}
#
proc channel_killUser {this usr} {
    set w [$this window]
    catch {destroy $w.cFrm.uFrm.userBtn.$usr
	if {[set x [indexHack $w.users.menu [$usr name] 3]] >=0} {
	    $w.users.menu delete $x
	}
	$this delTag $usr
	$usr leave $this
    }
}
#
proc channel_doNames {this param} {
    set win [$this window].cFrm.uFrm.userBtn
    set kids [winfo children $win]
    regsub -all {[\\\{\}\"]} $param {\\&} param
    foreach n $param {
	set op 0
	set sp 0
	while {[string match {[@+]*} $n]} {
	    if [string match @* $n] { set op 1 } { set sp 1}
	    set n [string range $n 1 end] ;
	}
	set usr [User :: make $n]
	if ![string compare $usr [set myid [[$this net] myid]]] {
	    upvar #0 $this cdata
	    set cdata(Op,$myid) $op
	    set cdata(Spk,$myid) $sp
	    if $op { $this markOp $usr } elseif {$sp} { $this markV $usr }
	} {
	    $this addUser $usr $op $sp
	}
	listkill kids $win.$usr
    }
    foreach n $kids {
	if [string compare disabled [$n cget -state]] {
	    $this killUser [winfo name $n]
	}
    }
}
#
proc channel_isJoined {this usr} {
    return [winfo exists [$this window].cFrm.uFrm.userBtn.$usr]
}
#
proc channel_sendJoin {this args} {
    if ![string match {} [lindex $args 0]] { $this configure -key [lindex $args 0] }
    [$this net] qSend JOIN [$this name] :[$this key]
}
#
proc channel_replace {this usr1 usr2} {
    if [$this active] {
	set op [$this isOp $usr1]
	set sp [$this isSpeaker $usr1]
	$this killUser $usr1
	$this addUser $usr2 $op $sp
    }
}
#
proc channel_doJoin {this usr nm prefix} {
    $this show
    if ![$this isJoined $usr] {
	$this optText JOIN \
	  "*** [$usr name] ($nm) has joined channel [$this name]"
	$this addUser $usr 0 0
	$this setOps $prefix $usr
    } {
	set w [$this window]
	if {[set x [indexHack $w.users.menu [$usr name] 3]] >=0} {
	    $w.users.menu entryconfigure $x -state normal
	}
	$w.cFrm.uFrm.userBtn.$usr conf -state normal
	$usr heal
	[$this net] qSend NAMES :[$this name]
    }
}
#
proc channel_inactive {this} {
    if {[$this close] && [$this active]} {
	upvar #0 $this cdata
	if {[incr cdata(closecount) -[[$this net] testTime]] <= 0} {
	    $this popdown
	    set cdata(closecount) $cdata(closetime)
	}
    }
}
#
proc channel_extendTime {this} {
    upvar #0 $this cdata
    set cdata(closecount) $cdata(closetime)
}
#
proc setOption {this sub opt val} {
    if ![string compare [$this lname] *default*] {
	set id *cFrm*${sub}[capitalise $opt]
    } {
	set id *$this*${sub}$opt
    }
    option add $id $val
}
#
proc channel_control {this args} { return [[$this net] control] }
#
proc channel_configure {this args} {
    upvar #0 $this cdata
    while {![string match {} $args]} {
	set val [lindex $args 1]
	set name [lindex $args 0]
	set opt [string range $name 1 end]
	switch -glob -- $name {
	-foreground -
	-background -
	-font {
		set cdata($opt) $val
		setOption $this {} $opt $val
	    }
	-geometry {
		set cdata($opt) $val
		setOption $this {cFrm.} $opt $val
	    }
	-height -
	-width  {
		global OType
		set cdata($opt) $val
		switch $OType($this) {
		Channel	{ set type text }
		Message	{ set type msg }
		Notice	{ set type note }
		Chat	{ set type chat }
		Info	{ set type info }
		}
		setOption $this $type. $opt $val
	    }
	-boldfont {
		global OType
		set cdata($opt) $val
		switch $OType($this) {
		Channel	{ set type text }
		Message	{ set type msg }
		Notice	{ set type note }
		Chat	{ set type chat }
		Info	{ set type info }
		}
		setOption $this $type. boldFont $val
	    }
	-topic {
		if ![string match {} $val] {
		    [$this net] TOPIC [$this name] $val
		}
	    }
	-closetime {
		set ct [expr {$val * 1000}]
		if {$ct > 0 && $ct < [[$this net] testTime]} {
		    [$this net] configure -testTime $ct
		}
		set cdata(closetime) $ct
		set cdata(closecount) $ct
	    }
	-buttons {
		set cdata(buttons) $val
		if {[winfo exists [$this window].users.menu]} {
		    [$this window].users.menu entryconfigure 0 -label \
		      [expr {[$this buttons] ? {No Buttons} : {Buttons}}]
		}
	    }
	-name {
		upvar #0 CTO$cdata(net) CTO
		catch {unset CTO($cdata(lname))}
		set cdata(name) $val
		set cdata(lname) [string tolower $val]
		set CTO($cdata(lname)) $this
	    }
	-window {
		if ![string match {} $cdata(window)] {
		    $cdata(window) delete
		    catch {destroy .@zd$this}
		}
		if {![string match {} [set cdata(window) $val]] &&
		  ![string match {} $cdata(geometry)]} {
		    $val configure -geometry $cdata(geometry)
		}
	    }
	+*  { lappend cdata($opt) $val }
	-*  { set cdata($opt) $val }
	}
	set args [lrange $args 2 end]
    }
}
#
proc channel_log {this val} {
    upvar #0 $this cdata
    if ![string match {} $cdata(logfd)] {
	puts $cdata(logfd) $val
	$this doLog Flush
    }
    $this extendTime
}
#
proc channel_popup {this} {
    popup [$this window]
    $this extendTime
    handleOn POPUP [list [$this lname]]
    return $this
}
#
proc channel_popdown {this} {
    wm iconify [$this window]
    if [winfo exists .@zd$this] { wm iconify .@zd$this }
    handleOn POPDOWN [list [$this lname]]
    return $this
}
#
proc cmkw {path} {
    if {![winfo exists $path] && [catch {frame $path} msg]} {
	mkDialog ERROR .@option {Option Host Error} \
	  "Error in your X resources - $msg" {}
	return 1
    }
    return 0
}
#
proc channel_tagWindow {this} {
   global OType
   set w [$this lname]
   set type [string tolower $OType($this)]
   if ![winfo exists .$type.$w] {
	if [cmkw [set path .$type]] { return . }
	set r $w
	while {[regexp {([^.]*)\.(.*)} $r match f r]} {	
	    append path ".$f"
	    if [cmkw $path] { return .$type }
	}
	if [cmkw "$path.$r"] { return .$type }
   }
   return .$type.$w
}
#
proc channel_original {this opts} {
    global Name OType ztrans
    set rchan [$this name]
    set chan [$this lname]
    set realChan [$this isa Channel]
    set wndw [Window .$this]
    set w [$wndw name]
    set Name($w) $this
    set net [$this net]
    $this configure -window $wndw
    switch $OType($this) {
    Channel {
	    deMonitor $net $this 0
	    set lp {IRC Channel} ; set wname $rchan
	    $this configure -text $w.cFrm.text
        }
    Notice {
	    set lp {IRC Notice from} ; set wname $rchan
	    $this configure -text $w.cFrm.note
        }
    Chat {
	    set lp {DCC Chat with} ; set wname "Chat $rchan"
	    $this configure -text $w.cFrm.chat
         }
    Message {
	    set lp {IRC Conversation with} ; set wname $rchan
	    $this configure -text $w.cFrm.msg
	}
    }
    $wndw configure -title "$lp $rchan" 
    $wndw setIcon $this $wname

    set oFrm $w

    if $realChan {
	frame $w.topic -borderwidth 0
	menubutton $w.topic.label -text $ztrans(topic) -relief raised
	$w.topic.label conf -menu $w.topic.label.menu
	set om [menu $w.topic.label.menu -tearoff 0]
	$om add command -label $ztrans(refresh) \
	  -command "$net TOPIC [$this name]"
	$om add command -label $ztrans(new) -command "getTopic \[channel $w\]"
	$om add separator
	foreach nn [$this topics] {
	    $om add command -label "[prune $nn 15]" \
	      -command "$this configure -topic {${nn}}"
	}
	emacsTEntry $w.topic.entry
	pack $w.topic.label -side left
	pack $w.topic.entry -side left -expand 1 -fill x
	bind $w.topic.entry <Return> {sendTopic %W ; notIdle %W}
    }

    frame $w.cmds -borderwidth 0
    frame $w.cmds.cmds0 -borderwidth 0
    set om [makeMB $w.mode Mode]
    addImage $w.mode mode $w.cmds.cmds0
    $om add checkbutton -label {Pop Up} -variable ${this}(open) \
	-command "$this set open"
    $om add checkbutton -label {Pop Down} -variable ${this}(close) \
	-command "$this set close"
    $om add checkbutton -label $ztrans(draw) -variable ${this}(draw) \
      -command "$this drawSet"
    if [$this isa Chat] { $om entryconfigure last -state disabled }
    $om add checkbutton -label $ztrans(jump) -variable ${this}(jump) \
	-command "$this set jump"
    $om add checkbutton -label $ztrans(quiet) -variable ${this}(quiet) \
	-command "$this set quiet"
    $om add checkbutton -label $ztrans(actions) -variable ${this}(actions) \
      -command "$this flipActions"
    if [$this isa Chat] { $om entryconfigure last -state disabled }
    if $realChan {
	$om add checkbutton -label $ztrans(nocase) -variable ${this}(nocase) \
	-command "$this set nocase"
	upvar #0 $this cdata
	$om add command -command "$this setBan" -label $ztrans(ban)
	foreach vl "{p $ztrans(private)} {m $ztrans(moderated)} \
	  {s $ztrans(secret)} {i {Invite Only}} {t $ztrans(topic)} \
	  {n {No Msg}}" {
	    set v [lindex $vl 0]
	    set cdata($v) 0
	    $om add checkbutton -command "$this changeMode $v" \
	      -variable ${this}($v) -state disabled -label [lindex $vl 1]
	}	
	$om add command -command "$this setLimit" -label $ztrans(limit)
	$om add command -command "$this setKey" -label $ztrans(key)

	set om [makeMB $w.channel Channel]
	addImage $w.channel channel $w.cmds.cmds0
	$om add command -command "$net qSend WHO {:$rchan}" -label $ztrans(who)
	$om add command -command "channelInvite $net {$chan}" -label $ztrans(invite)
	$om add command -command "channelNotice $net {$chan}" -label $ztrans(notice)
	$om add cascade -label $ztrans(messages) -menu $om.msgs
	set omm [menu $om.msgs]
	catch {$omm configure  -tearoffcommand "retitle {Messages for $chan}"}
	$omm add command -label $ztrans(new) -command "$this newMsg"
	$omm add separator
	foreach x [$this messages] {
	    $omm.menu add command -label [prune $x 15] \
	      -command "$this send {$x}"
	}
	$om add command -command "$this makeZDraw" -label $ztrans(draw)
	if ![$this draw] { $om entryconfigure 3 -state disabled }
	addCTCPMenu $net $om $this
	addSoundMenu $net $om $this
    } {
	set usr [User :: find $rchan]
	set om [makeMB $w.channel User]
	addImage $w.channel user $w.cmds.cmds0
	$om add command -command "$net WHOIS \[$usr name\]" -label $ztrans(whois)
	$om add command -label $ztrans(invite) -state disabled
	$om add command  -command "channelNotice $net \[$usr name\]" \
	  -label $ztrans(notice)
	$om add command -command "$this makeZDraw" -label $ztrans(draw)
	if {![$this draw] || [$this isa Chat]} {
	    $om entryconfigure last -state disabled
	}
	addCTCPMenu $net $om $this
	addDCCMenu $om $this
	addSoundMenu $net $om $this
	addIgnoreMenu $om $usr
    }
    $om add cascade -label $ztrans(log) -menu $om.log
    menu $om.log -tearoff 0
    foreach cmd {Close Open Flush Empty} {
	$om.log add command -label [trans $cmd] -command "$this doLog $cmd" \
	  -state disabled
    }
    if ![logOpen $this a 0 [$this logfile]] {
	$om.log entryconfigure 1 -state normal
    } 
    $om add command -label $ztrans(crypt) -command "$this getCrypt"
    $om add command -label $ztrans(history) -command "$this sizeHistory"
    $om add command -label $ztrans(exec) -command "doExec $this"
    $om add cascade -label $ztrans(script) -state disabled
    $om add cascade -label $ztrans(plugin) -state disabled
    if [$this isa Chat] { $om entryconfigure last -state disabled }
    set om [makeMB $w.action Action]
    catch {$w.action.menu configure -tearoffcommand "retitle {Actions for $chan}"}
    addImage $w.action action $w.cmds.cmds0
    if ![$this isa Chat] {
	$om add command -label $ztrans(new) -command "$this getAction"
	$om add separator

	foreach act [$net actions] {
	    $om add command -label [prune $act 15] \
	      -command "$this action {$act}"
	}
    } {
	$w.action configure -state disabled
    }
    $this buildUsersMenu [makeMB $w.users Names]
    addImage $w.users names $w.cmds.cmds0
    buttonmenu $w.quit -command "$this leave" -text $ztrans(leave) -width 10
    bind $w.quit <Shift-1> {tkButtonDown %W}
    bind $w.quit <Shift-ButtonRelease-1> "
	set sc \[%W cget -command\]
	%W configure -command \"$net quit\"
	tkButtonUp %W
	%W configure -command \$sc
    "
    set qm [menu $w.quit.menu]
    if $realChan {
	bind $w.quit <Control-1> {tkButtonDown %W}
	bind $w.quit <Control-ButtonRelease-1> "
	    tkButtonUp %W
	    $this configure -monitor 1
	"
	$qm add command -label $ztrans(leave) -command "$this doLeave {}"
	$qm add command -label $ztrans(monitor) \
	  -command "$this configure -monitor 1 ; $this doLeave {}"
    } {
	$qm add command -label $ztrans(leave) -command "$this leave"
    }
    $qm add cascade -label $ztrans(quit) -menu $qm.menu
    menu $qm.menu -tearoff 0
    set ctl [$this control]
    $qm.menu add command -label $ztrans(new) -command "getQuit $ctl"
    $qm.menu add separator
    foreach x [$net signoffs] { $qm.menu add command -label [prune $x 15] \
	  -command "$net doQuit {$x}" }
    if $realChan {	
	$qm add command -label $ztrans(new) -state disabled
	$qm add separator
	foreach x [$net leaves] {
	    $om add command -label [prune $x 15] -command "$this doLeave {$x}"
	}
    }
    addImage $w.quit leave $w.cmds.cmds0

    button $w.clear -command "$this clear 0" -text $ztrans(clear) -width 10
    addImage $w.clear clear $w.cmds.cmds0
    bind $w.clear <Shift-1> {tkButtonDown %W}
    bind $w.clear <Shift-ButtonRelease-1> "
	set sc \[lindex \[%W configure -command \] 4 \]
	%W configure -command {$this clear 1}
	tkButtonUp %W
	%W configure -command \$sc
    "
    pack $w.cmds.cmds0 -side left -expand 1 -fill x
    frame $w.cFrm -borderwidth 0
    frame $w.cFrm.textFrm -borderwidth 0
    $this buildUsers $w
    set ot [$this text]
    scrollbar $w.cFrm.vscroller -command "doScroll $ot"
    text $ot -yscrollcommand "setScroll $ot $w.cFrm.vscroller"
    rebind $ot $net
    set tgw [$this tagWindow]
    foreach tag {foreground background font selectForegound
      selectBackground width height} { 
	if ![string match {} [set v [option get $tgw $tag [capitalise $tag]]]] {
	    $ot configure -[string tolower $tag] $v
	}
    }
    global BF Fg Bg Ft Bl
    set BF($this) [getOValue $ot font boldFont Font]
    set Ft($this) [getOValue $ot font font Font]
    set Fg($this) [getOValue $ot foreground foreground Foreground]
    set Bg($this) [getOValue $ot background background Background]
    set Bl($this) [option get $tgw bell Bell]
    $ot conf -selectforeground $Bg($this) -selectbackground $Fg($this)
    pack $w.cFrm.vscroller -side right -fill y -in $w.cFrm.textFrm
    pack $ot -side left -expand 1 -fill both -in $w.cFrm.textFrm
    set om [frame $w.cmdLine -borderwidth 0]
    scrollbar $om.cscroller -orient horizontal -command "$om.commandLine xview"
    emacsEntry $om.commandLine -xscrollcommand "$om.cscroller set"
    pack $om.commandLine $om.cscroller -expand 1 -fill x
    $this addUser [$net myid] 0 0
    if $realChan {
	confTag $w.topic.entry {} $Fg($this) $Bg($this) $Ft($this) $BF($this)
	pack $w.topic -fill x
    }
    pack $w.cmds -side top -fill x
    pack $w.cmdLine -side bottom -fill x
    pack $w.cFrm.textFrm -expand 1 -fill both
    pack $w.cFrm -expand 1 -fill both
    set occ $w.cmdLine.commandLine
    doBindings $occ $this $chan
    if [string match {} $opts] { focus $occ }
    bind $ot <Enter> "focus $occ ; notIdle %W"
    bind $ot <Configure> {%W see end ; notIdle %W}
    bind $w.cFrm <Enter> "focus $occ ; notIdle %W"
    bind $w.cFrm <Destroy> "$this doLeave {}"
    bind $w.cFrm <Visibility> {
	set win [winfo toplevel %W]
	global Icon IconBM
	if [info exists Icon($win)] {wm iconname $win $Icon($win)}
	if [info exists IconBM($win)] {
	    wm iconbitmap $win [lindex $IconBM($win) 0]
	}
	notIdle %W
    }
    wm protocol $w WM_DELETE_WINDOW "$this leave"
    tkwait visibility $w

    if {[$this open] && [$this join]} {	$wndw iconify }
}
#
proc bsSet {scr f l} {
    if {$f == 0.0 && $l == 1.0} {
	catch "pack forget $scr"
    } {
	catch "pack $scr -side left -fill y"
	$scr set $f $l
    }
}
#
proc channel_newMsg {this} {
    global ztrans
    mkEntryBox .@m$this $ztrans(message) "Enter your new message:" \
      "{$ztrans(message) {}}" "$ztrans(ok) {$this send}" \
      "$ztrans(keep) {$this keepMsg}" "$ztrans(cancel) {}"
}
#
proc channel_keepMsg {this str} {
    if ![string match {} $str] {
	$this send $str
	[$this window].channel.menu.msgs add command \
	  -label "[prune $str 15]" -command "$this send {$str}"
	[$this net] configure +messages $str
	uplevel #0 set confChange 1
    }
}
#
proc channel_buildUsers {this w} {
    set f [frame $w.cFrm.uFrm -borderwidth 0]
    scrollbar $f.vscroller -command "$f.userBtn yview" 
    text $f.userBtn -yscrollcommand " bsSet $f.vscroller" -width 10 \
      -relief flat -borderwidth 0 -highlightthickness 0 -padx 0 -pady 0
    bindtags $f.userBtn {null}
    pack $f.userBtn -side left -fill y
    if [$this isa Channel] { pack $f -side right -fill y }
}
#
proc channel_buildUsersMenu {this w} {
    upvar #0 $this cdata
    set myid [[$this net] myid]
    set cdata(Op,$myid) 0
    set cdata(Spk,$myid) 0
    $w add command -label [expr {[$this buttons] ? {No Buttons} : {Buttons}}] \
      -command "$this toggleUsers"
    $w add separator
}
#
proc channel_toggleUsers {this} {
    global ztrans
    set w [$this window]
    if [winfo ismapped $w.cFrm.uFrm] {
	pack forget $w.cFrm.uFrm
	$w.users.menu entryconfigure 1 -label $ztrans(buttons)
    } {
	pack $w.cFrm.uFrm -side right -fill y -before $w.cFrm.textFrm
	$w.users.menu entryconfigure 1 -label {No Buttons}
    }
}
#
proc channel_delete {this} { mcnDelete $this CTO[$this net] channels}
#
proc mcnDelete {this nvar reg} {
    global Name $nvar $this MKOp OType
    upvar #0 $this cdata
    set net $cdata(net)
    if ![string match {} $cdata(logfd)] { close $cdata(logfd) }
    if ![string match {} [set win [$this window]]] {
	foreach x [winfo children $win.cFrm.uFrm.userBtn] {
	    [winfo name $x] leave $this
	}
    }
    $this configure -window {}
    if {![$this keep] && ![$this monitor]} {
	set chan [$this lname]
	unset ${nvar}($chan) OType($this)
	catch {unset Name($win)}
	unset $this
	rename $this {}
	$net deregister $reg $this
    } \
    elseif [$this monitor] { channelMonitor [$this net] [$this name] }
    global TFn TFa TFg TBg TAF TAB TBl Bl Bg Fg BF Ft
    foreach x {Bl Bg Fg BF Ft} { catch {unset ${x}($this) } }
    foreach x {TFn TFa TFg TBg TAF TAB TBl} {
	foreach y [array names $x $this,*] { unset ${x}($y) }
    }
    catch {unset MKOp($this)}
}
#
proc setHistory {this val} {
    if [regexp {^[0-9]+$} $val] { $this configure -history $val }
}
#
proc channel_sizeHistory {this} {
    global ztrans
    mkEntryBox .@sh$this {History Size} \
      "Enter the history size for [$this name]:" \
      "{$ztrans(history) [$this history]}" "$ztrans(ok) {setHistory $this}" \
      "$ztrans(cancel) {}"
    tkwait window .@sh$this
}
#
proc pickOut {net win pos} {
    switch -glob -- [set txt [$win get "$pos linestart" "$pos lineend"]] {
    {[=->]*} { return [string range $txt 2 end] }
    {!>*} { return [string range $txt 3 end] }
    {\**} {
	    set idx [expr 3 + [string length [$net nickname]]]
	    return [string range $txt $idx end]
	}
    }
    return $txt
}
#
proc channel_getPrev {this} {
    upvar #0 $this cdata
    set rng [[$this text] tag range @me]
    if [string match {} [set foo [lindex $rng $cdata(hpos)]]] { return {} }
    if ![string compare $cdata(hpos) end] {
	set cdata(hpos) [expr [llength $rng] - 1]
    }
    if {[incr cdata(hpos) -2] < 0}  {set cdata(hpos) end }
    return [pickOut [$this net] [$this text] $foo]
}
#
proc channel_getNext {this} {
    upvar #0 $this cdata
    set rng [[$this text] tag range @me]
    if [string match {} [set foo [lindex $rng $cdata(hpos)]]] { return {} }
    if ![string compare $cdata(hpos) end] {
	set cdata(hpos) [expr [llength $rng] - 1]
    }
    if {[incr cdata(hpos) 2] >= [llength $rng]}  { set cdata(hpos) 1 }
    return [pickOut [$this net] [$this text] $foo]
}
#
proc channel_flipActions {this} {
    global zircon
    set win [$this window].cmdLine.commandLine
    set ret [bind $win <Return>]
    set sret [bind $win <$zircon(action)>]
    bind $win <Return> $sret
    bind $win <$zircon(action)> $ret
    upvar #0 $this cdata
    set cdata(actionmode) [expr !$cdata(actionmode)]
}
#
# Leaving channels :
#	doLeave sends the PART message
#	leaveChannel does the are you sure dialog
#
proc channel_doLeave {this msg} {
    set net [$this net]
    if {[$this isa Channel] && [$net active]} {
	set w [$this window]
	$w.quit configure -state disabled -text Leaving
	$w.cmdLine.commandLine configure -state disabled
	$net PART [$this name] $msg
    } {
	$this delete
    }
}
#
proc channel_leave {this} {
    global ztrans
    set chan [$this name]
    mkDialog LEAVE .@$this "Leaving $chan" "Really leave channel $chan?" \
      "{$ztrans(message) {}}" "$ztrans(ok) {$this doLeave}" \
      "$ztrans(cancel) {}"
}
#
proc channel_doAddText {this tag text} {
    $this log $text
    set name [$this text]
    $name configure -state normal
    insertText $this $name $text $tag
    $name insert end "\n"
    $name configure -state disabled
    if [$this jump] { $name see end }
}
#
proc channel_doPopUp {this} {
    if [string match {} [set win [$this window]]] return
    if {[$this open] && ![winfo ismapped $win]} {
	if [[$this net] noPopup] {
	    global Icon IconBM
	    wm iconname $win "*$Icon($win)*"
	    if {[info exists IconBM($win)] && \
		![string match {} [set icn [lindex $IconBM($win) 1]]]} {
		wm iconbitmap $win $icn
	    }
	} {
	    $this popup
	}
    }
}
#
proc channel_keepAction {this value} {
    $this action $value
    [$this window].action.menu add command -label "[prune $value 15]" \
      -command "$this action {$value}"
    [$this net] configure +actions $value
    uplevel #0 set confChange 1
}
#
proc channel_getAction {this} {
    global ztrans
    mkEntryBox .@${this}action $ztrans(action) "Enter your action:" \
      "{$ztrans(action) {}}"\
      "$ztrans(ok) {$this action}" "$ztrans(keep) {$this keepAction}" \
      "$ztrans(cancel) {}"
}
#
# Send string to channel as an ACTION and echo to channel window
#
proc channel_action {this string} {
    if ![string match {} $string] {
	[$this net] PRIVMSG [$this name] "\001ACTION $string\001"
	$this addText @me "* [[$this net] nickname] $string"
	$this configure -hpos end
    }
}
#
# Currently this next proc doesnt work!! It's a tk problem *not* a
# zircon problem
#
proc channel_clear {this hist} {
    set t [$this text]
    if $hist {
	$t configure -state normal
	$t delete 1.0 end
	$t configure -state disabled
	$this configure -hpos end
    } {
	$t xview moveto 0
	$t yview moveto 1.0
	return
	set posn [$t yview]
	regexp {(.*)\..*} [$t index end] m ln
	$t configure -state normal
	while {$st > 0} { $t insert end "\n" ; incr st -1 }
	$t configure -state disabled
	$t see end
    }
}
#
proc channel_insert {this text} {
    if ![string match {} $text] {
	set op send
	if [$this actionmode] { set op action }
	set ent [$this window].cmdLine.commandLine
	while {[regexp "(\[^\n\]*)\n(.*)" $text d line text]} {
	    if [string match {} $line] continue
	    tkEntryInsert $ent $line
	    $this $op [$ent get]
	    $ent delete 0 end
	}
	if ![string match {} $text] {
	    $ent insert insert $text
	    tkEntrySeeInsert $ent
	}
	$this configure -hpos end
    }
}
#
proc channel_insertSelect {this} {
    if ![catch {selection get} bf] { $this insert $bf }
}
#
proc channel_markOp {this usr} {
    uplevel #0 set ${this}(Op,$usr) 1
    if [[$this net] me $usr] { $this opItems normal }
    set w [$this window]
    markButton $w.cFrm.uFrm.userBtn.$usr operator
    markEntry $w.users.menu [indexHack $w.users.menu [$usr name] 3] operator
}
#
proc channel_unmarkOp {this usr} {
    uplevel #0 set ${this}(Op,$usr) 0
    set par [expr  {[$this isSpeaker $usr] ? "speaker" : {}}]
    if [[$this net] me $usr] { $this opItems disabled }
    set w [$this window]
    markButton $w.cFrm.uFrm.userBtn.$usr $par
    markEntry $w.users.menu [indexHack $w.users.menu [$usr name] 3] $par
}
#
proc channel_markV {this usr} {
    uplevel #0 set ${this}(Spk,$usr) 1
    if ![$this isOp $usr] {
	set w [$this window]
	markButton $w.cFrm.uFrm.userBtn.$usr speaker
	markEntry $w.users.menu [indexHack $w.users.menu [$usr name] 3] speaker
    }
}
#
proc channel_unmarkV {this usr} {
    uplevel #0 set ${this}(Spk,$usr) 0
    if ![$this isOp $usr] {
	set w [$this window]
	markButton $w.cFrm.uFrm.userBtn.$usr {}
	markEntry $w.users.menu [indexHack $w.users.menu [$usr name] 3] {}
    }
}
#
proc channel_flag {this state} {
    if ![$this active] return
    set win [$this window]
    foreach w {mode channel action} { catch "$win.$w conf -state $state" }
    if [winfo exists $win.topic] {
	$win.topic.label conf -state disabled
	$win.topic.entry conf -state disabled
    }
    foreach w [winfo children $win.cFrm.uFrm.userBtn] {
	$w conf -state $state
    }
    set l [$win.users.menu index last]
    while {$l > 2} {
	$win.users.menu entryconfigure $l -state $state
	incr l -1
    }
    if ![string compare $state normal] {
	global Split
	foreach w [array names Split] {
	    foreach n $Split($w) {
		if [$this isJoined $n] {
		    $win.cFrm.uFrm.userBtn.$n conf -state disabled
		}
	    }
	}
    } {
	foreach w [winfo children $win.cFrm.uFrm.userBtn] {
	    set p [winfo name $w]
	    $this unmarkOp $p
	    $this unmarkV $p
	}
    }
}
#
proc channel_nickChange {this usr nnk} {
    if [string match {} [set w [$this window]]] return
    set net [$this net]
    $this optText NICK "*** [$usr name] is now known as $nnk"
    if [$net me $usr] {
	$w.cFrm.uFrm.userBtn.$usr configure -text $nnk
	$w.users.menu entryconfigure 3 -label $nnk
    } {
	if [$this isJoined $usr] {
	    set win $w.cFrm.uFrm.userBtn.$usr
	    set fg [$win cget -foreground]
	    set bg [$win cget -background]
	    set ft [$win cget -font]
	    destroy $win
	    wsortIns $this $w.cFrm.uFrm.userBtn $nnk $usr
	    $w.cFrm.uFrm.userBtn.$usr configure -fg $fg -bg $bg -font $ft
	}
	if {[set x [indexHack $w.users.menu [$usr name] 4]] >=0} {
	    $w.users.menu delete $x
	    sortIns $this $nnk $usr $w.users.menu
	}
    }
}
#
proc sc {a b} { return [string compare $a $b] }
#
proc scl {a b} { return [string compare $a [string tolower $b]] }
#
proc sortIns {chan nk usr w} {
    set x 4
    set last [$w index last]
    if [$chan nocase] {
	set cnk [string tolower $nk]
	set fn sc
    } {
	set cnk $nk
	set fn scl
    }
    makeUserMenu $chan $w.$usr $usr
    while {$x <= $last} {
	if {[$fn $cnk [$w entrycget $x -label]] < 0} {
	    $w insert $x cascade -label $nk -menu $w.$usr
	    return $x
	}
	incr x
    }
    $w add cascade -label $nk -menu $w.$usr
    return last
}
#
proc wsortIns {chan win un usr} {
    set wnk $win.$usr
    menubutton $wnk -text $un -width 9 \
      -highlightthickness 0 -borderwidth 2 -pady 1 -padx 2
    $wnk configure -menu [makeUserMenu $chan $wnk.menu $usr]
    if [$chan nocase] {
	set un [string tolower $un]
	set fn lname
    } {
	set fn name
    }
    set y 1
    while {![catch {set x [$win window cget 1.$y -window]}]} {
	if ![string match {} $x] {
	    if {[string compare $un [[winfo name $x] $fn]] < 0} {
		$win window create [$win index $x] -window $wnk -padx 0
		return
	    }
	}
	incr y
    }
    $win window create end -window $wnk -padx 0
}
#
proc channel_addUser {this usr op sp} {
    upvar #0 $this cdata
    set cdata(Op,$usr) $op
    set cdata(Spk,$usr) $sp
    set net [$this net]
    set w [$this window]
    set un [$usr name]
    set jn 1
    if [winfo exists [set win $w.cFrm.uFrm.userBtn]] {
	set wnk $win.$usr
	if ![winfo exists $wnk] {
	    wsortIns $this $win $un $usr
	} {
	    set jn 0
	}
	$wnk configure -state normal
	if $op { markButton $wnk operator } \
	elseif $sp { markButton $wnk speaker } { markButton $wnk {} }
    }
    set w $w.users.menu
    if [$net me $usr] {
	if ![winfo exists $w.$usr] {
	    $w insert 3 cascade -label $un -menu $w.$usr
	    makeUserMenu $this $w.$usr $usr
	}
	set x 3
    } \
    elseif {[set x [indexHack $w $un 4]] < 0} {
	set x [sortIns $this $un $usr $w]
    } {
	$w entryconfigure $x -state normal
    }
    if $op { markEntry $w $x operator } \
    elseif {$sp} { markEntry $w $x speaker } { markEntry $w $x {} }
    $this makeTag $usr
    if $jn { $usr join $this }
}
#
proc channel_ircOp {this state} {
    if ![string match {} [set w [$this window]]] {
	if [winfo exists $w.cFrm.uFrm.userBtn] {
	    foreach name [winfo children $w.cFrm.uFrm.userBtn] {
		setState $name.menu ircop $state
	    }
	}
	foreach name [winfo children $w.users.menu] {
	    setState $name ircop $state
	}
    }
}
#
proc channel_userMode {this usr mode} {
    switch -exact -- $mode {
    o { set val [$this isOp $usr] }
    v { set val [$this isSpeaker $usr] }
    }
    [$this net] MODE [$this name] [expr {$val ? {+} : {-}}]$mode [$usr name]
}
#
proc channel_setOps {this prefix usr} {
    global MkOp
    foreach n [$this ops] {
	if [regexp -nocase $n $prefix] {
	    lappend MkOp($this) $usr
	    $usr ref
	    break
	}
    }
}
#
proc channel_mode {this vals} {
    upvar #0 $this cdata
    set nxt {}
    set flag {}
    foreach par $vals {
	if ![string match {} $nxt] {
	    set flag [string index $nxt 0]
	    set m [string index $nxt 1]
	    set nxt [string range $nxt 2 end]
	    switch -exact -- $m {
	    o   {
		    $this [expr {[string match + $flag] ? \
		      "markOp" : "unmarkOp"}] [User :: make $par]
		}
	    v   {
		    $this [expr {[string match + $flag] ? \
		      "markV" : "unmarkV"}] [User :: make $par]
		}
	    k   {
		    set cdata(key) $par
		    $this showInfo key "Key: $par"
		}
	    l	{
		    set cdata(limit) $par
		    $this showInfo limit "Limit: $par"
		}
	    }
	    handleOn MODE [list [$this name] $flag$m $par]
	} {
	    set nxt {}
	    foreach m [split $par {}] {
		switch -glob -- $m {
		[+-] { set flag $m }
		[kl] {
			if [string match + $flag] {
			    append nxt $flag$m
			} {
			    if [string compare k $m] {set vr limit} {set vr key}
			    set cdata($vr) {}
			    $this showInfo $vr {}
			    handleOn MODE [list [$this name] -$m]
			}
		    }
		[ovb] { append nxt $flag$m }
		[psinm] {
			set cdata($m) [string match + $flag]
			handleOn MODE [list [$this name] $flag$m]
		    }
		t   {
			set x [string match + $flag]
			set cdata(t) $x
			if ![$this operator] {
			    set st [expr {$x ? {disabled} : {normal}}]
			    if ![string match {} [set w [$this window]]] {
				$w.topic.label conf -state $st
				$w.topic.entry conf -state $st
				if [string compare $st disabled] {
				    $w.topic.entry configure -relief sunken
				} {
				    $w.topic.entry configure -relief raised
				}
			    }
			}
			handleOn MODE [list [$this name] ${flag}t]
		    }
		}
	    }
	}
    }
}
#
proc channel_opItems {this state} {
    global ztrans
    set win [$this window]
    foreach name [winfo children $win.cFrm.uFrm.userBtn] {
	setState $name.menu chanop $state
    }
    foreach name [winfo children $win.users.menu] {
	setState $name chanop $state
    }
    set mn $win.mode.menu
    if [$this isa Channel] {
	set vl [$mn index $ztrans(ban)]
	incr vl
	set last [$mn index last]
	while {$vl <= $last} {
	    $mn entryconfigure $vl -state $state
	    incr vl
	}
	upvar #0 $this cdata
	if {![string compare $state normal] || $cdata(t)} {
	    $win.topic.entry conf -state $state
	    $win.topic.label conf -state $state
	}
    }
}
#
proc Channel_make {name args} {
    global current
    upvar #0 CTO$current(net) CTO
    set ln [string tolower $name]
    set id [expr {[info exists CTO($ln)] ? $CTO($ln) : [Channel $name]}]
    if ![string match {} $args] {eval $id configure $args}
    return $id
}
#
proc Channel_find {name} {
    global current
    upvar #0 CTO$current(net) CTO
    set ln [string tolower $name]
    return [expr {[info exists CTO($ln)] ? $CTO($ln) : {nil}}]
}
#
# proc to determine if a message is wanted
#
proc channel_wantMessage {this msg} {
    set cm [$this msg]
    return [expr {[lsearch $cm "!$msg"] >= 0 || [lsearch $cm $msg] < 0}]
}
#
proc valCmp {this op ln} {
    global defChan
    set v [$this $op]
    if ![string compare $this $defChan] {
	if {![info exists zDefs] || [string compare $v $zDefs($op)]} {
	    append ln " -$op {$v}"
	}
    }\
    elseif {[string compare $v [$defChan $op]]} {
	append ln " -$op {$v}"
    }
    return $ln
}
#
array set rcSave {
    all		{open close jump quiet history buttons close closetime}
    Channel	{draw menu nocase join actionmode}
    Message	{draw}
    Notice	{}
    Chat	{}
}
#
array set rcOpt {
    all		{foreground background font geometry height width
		  boldfont messages}
    Channel	{msg ops patterns topics logfile icon key}
    Message	{logfile icon}
    Notice	{logfile icon}
    Chat	{logfile icon}
}
#
proc mncSave {this def} {
    global OType rcSave rcOpt
    regsub -all {[][\\{\"}]} [$this name] {\\&} n
    set ln "[set typ $OType($this)] $n"
    foreach x [concat $rcSave(all) $rcSave($typ)] {
	set ln [valCmp $this $x $ln]
    }
    upvar #0 $this cdata
    foreach xv [concat $rcOpt(all) $rcOpt($typ)] {
	if ![string match {} $cdata($xv)] { set ln [valCmp $this $xv $ln] }
    }
    return $ln
}
#
proc channel_save {this desc} {
    global defChan
    puts $desc [mncSave $this $defChan]
    foreach b [$this bindings] {
	puts $desc "zbind [$this name] [lindex $b 0] {[lindex $b 1]}"
    }
}
#
proc defSave {desc def args} {
    global OType rcSave rcOpt zDefs Channel
    array set zDefs $Channel
    foreach x $args {
	global $x
	array set zDefs [set $x]
    }
    set typ $OType($def)
    foreach x [concat $rcSave(all) $rcSave($typ) $rcOpt(all) $rcOpt($typ)] {
	if [string compare $zDefs($x) [$def $x]] {
	    $def save $desc
	    return
	}
    }
    unset zDefs
}
#
proc Channel_save {desc net} {
    global defChan
    defSave $desc $defChan
    foreach ch [$net channels] {
	if {[string compare $ch $defChan] && ![$ch sys] && [$ch keep]} {
	    $ch save $desc
	}
    }
}
#
proc Channel_pack {net} { foreach ch [$net channels] { $ch pack new }}
#
proc channel_pack {this where} {
    if ![$this isa Channel] return
    global $where$this $this
    array set $where$this [array get $this]
}
#
proc channel_unpack {this where} {
    upvar #0 $where$this new
    foreach prop {name open close jump join draw quiet menu msg \
      history closetime key icon logfile nocase keep sys} {
	$this configure -$prop $new($prop)
    }
    unset new
}
#
proc Channel_cleanup {where} {catch {uplevel #0 unset [info globals newcha*]}}
#
proc indexHack {w nk start} {
#
# Do non-pattern based index matching....
#
    set x [$w index end]
    set i $start
    while {$i <= $x} {
	if ![string compare [$w entrycget $i -label] $nk] { return $i }
	incr i
    }
    return -1

}
#
proc makeCinfo {w} {
    if ![winfo exists $w.cinfo] {
	frame $w.cinfo -relief raised
	foreach x {limit key create url topic} {
	    label $w.cinfo.$x -relief flat
	    pack $w.cinfo.$x -side left
	}
	pack $w.cinfo -after $w.topic -fill x -expand 1
    }
}

#
proc channel_showInfo {this lb txt} {
    upvar #0 $this cdata
    makeCinfo [set w [$this window]]
    if [string match {} $txt] {
	$w.cinfo.$lb configure -text {} -relief flat
	listkill cdata(cinfo) $lb
	if [string match {} $cdata(cinfo)] { destroy $w.cinfo }
    } {
	$w.cinfo.$lb configure -text $txt -relief raised
	lappend cdata(cinfo) $lb
    }
}
