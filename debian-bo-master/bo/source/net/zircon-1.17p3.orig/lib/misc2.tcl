#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/misc2.tcl,v $
# $Date: 1996/06/20 19:56:05 $
# $Revision: 1.17.1.2 $
#
#
proc net_doMisc2 {this chid win} {
    set line [$win get]
    $win delete 0 end
    if [regexp {^(/[a-zA-Z]+)( (.*))?$} $line dummy pr r1 rest] {
	set pr [string toupper $pr]
	if ![string match {} [info procs $pr]] {
	    $chid addText {} "!> $line"
	    $chid configure -hpos end
	    $pr $this $chid $rest
	    return
	} 
    }
    $chid send $line
}
#
proc /ADMIN {net chid arg} { $net nsend ADMIN [lindex $arg 0]}
#
proc /ALIAS {net chid arg} { sillyPerson $net }
#
proc /ASSIGN {net chid arg} { sillyPerson $net }
#
proc /AWAY {net chid arg} { $net AWAY $arg }
#
proc /BIND {net chid arg} { sillyPerson $net }
#
proc /BYE {net chid arg} { $net doQuit [lindex $arg 0]}
#
proc /CD {net chid arg} { sillyPerson $net }
#
proc /CHANNEL {net chid arg} { channelJoin $net [lindex $arg 0] }
#
proc /CLEAR {net chid arg} { sillyPerson $net }
#
proc /COMMENT {net chid arg} { }
#
proc /CONNECT {net chid arg} {
    $net CONNECT [lindex $arg 0] [lindex $arg 1] [lindex $arg 3]
}
#
proc /CTCP {net chid arg} {
    set nk [lindex $arg 0]
    if ![string compare $nk *] { set nk [$chid name] }
    switch [set cmd [string toupper [lindex $arg 1]]] {
    PING { $net CTCP PING $nk [ZPING] }
    CLIENTINFO {$net CTCP $CMD $nk [lindex $arg 2]}
    ERRMSG -
    SOUND -
    ACTION -
    ECHO {$net CTCP $cmd $nk $arg }
    default {
	    $net CTCP $cmd $nk {}
	}
    }
}
#
proc /DATE {net chid arg} { $net TIME $arg }
#
proc /DCC {net chid arg} { sillyPerson $net }
#
proc /DEOP {net chid arg} { $net deIRCOp }
#
proc /DESCRIBE {net chid arg} {
    set chan [lindex $arg 0]
    if {[string compare nil [set ch [Channel :: find $chan]]] ||
	[string cpmpare nil [set ch [Message :: find $chan]]]} {
	$ch action [lrange $arg 1 end]
    } {
	$net PRIVMSG $chan "\001ACTION [lrange $arg 1 end]\001"
    }
}
#
proc /DIE {net chid arg} { sillyPerson $net }
#
proc /DIGRAPH {net chid arg} { sillyPerson $net }
#
proc /DMSG {net chid arg} { sillyPerson $net }
#
proc /DQUERY {net chid arg} { sillyPerson $net }
#
proc /ECHO {net chid arg} { $net inform $arg }
#
proc /ENCRYPT {net chid arg} {
    set nk [lindex $arg 0]
    if [string match {[#&]} $nk] {
	set chid [Channel :: make $nk]
    } {
	set chid [Message :: make $nk]
    }
    $chid configure -key [lindex $arg 1]
}
#
proc /EVAL {net chid arg} { sillyPerson $net }
#
proc /EXEC {net chid arg} { runCmd $chid $arg }
#
proc /EXIT {net chid arg} { $net doQuit [lindex $arg 0]}
#
proc /FLUSH {net chid arg} { sillyPerson $net }
#
proc /FOREACH {net chid arg} { sillyPerson $net }
#
proc /HELP {net chid arg} { sillyPerson $net }
#
proc /HISTORY {net chid arg} { sillyPerson $net }
#
proc /HOOK {net chid arg} { sillyPerson $net }
#
proc /IF {net chid arg} { sillyPerson $net }
#
proc /IGNORE {net chid arg} { sillyPerson $net }
#
proc /INFO {net chid arg} { $net INFO $arg }
#
proc /INPUT {net chid arg} { sillyPerson $net }
#
proc /INVITE {net chid arg} {
    set nk [lindex $arg 0]
    foreach x [lrange $arg 1 end] { $net INVITE $nk $x }
}

#
proc /JOIN {net chid arg} { channelJoin $net $arg }
#
proc /KICK {net chid arg} { $net KICK [lindex $arg 0] [lrange $arg 1 end] }
#
proc /KILL {net chid arg} {$net KILL [lindex $arg 0] [lrange $arg 1 end]}
#
proc /LASTLOG {net chid arg} { sillyPerson $net }
#
proc /LEAVE {net chid arg} { /PART $chid $arg }
#
proc /LINKS {net chid arg} { $net LINKS [lindex $arg 0] [lindex $arg 1] }
#
proc /LIST {net chid arg} { sillyPerson $net }
#
proc /LOAD {net chid arg} {  sillyPerson $net }
#
proc /LUSERS {net chid arg} {$net send LUSERS [lindex $arg 0] [lindex $arg 1]}
#
proc /ME {net chid arg} { $chid action $arg }
#
proc /MLOAD {net chid arg} { sillyPerson $net }
#
proc /MODE {net chid arg} { eval $net send MODE $arg }
#
proc /MOTD {net chid arg} { $net send MOTD $arg }
#
proc /MSG {net chid arg} {
    $net PRIVMSG [lindex $arg 0] [join [lrange $arg 1 end]]
}
#
proc /NAMES {net chid arg} { $net NAMES $arg }
#
proc /NICK {net chid arg} { $net NICK $arg }
#
proc /NOTE {net chid arg} { sillyPerson $net }
#
proc /NOTICE {net chid arg} {
    $net NOTICE [lindex $arg 0] [join [lrange $arg 1 end]]
}
#
proc /NOTIFY {net chid arg} { sillyPerson $net }
#
proc /ON {net chid arg} { sillyPerson $net }
#
proc /OPER {net chid arg} {$net OPER [lindex $arg 0] [lindex $arg 1]}
#
proc /PARSEKEY {net chid arg} { sillyPerson $net }
#
proc /PART {net chid arg} {
    if [string match {} $arg] { $chid leave } {
	if [string compare nil [set id [find $arg]]] { $id leave }
    }
}
#
proc /PING {net chid arg} { foreach nk $arg { doCtcp $net PING $nk } }
#
proc /QUERY {net chid arg} {
    set usr [User :: make $arg]
    set msg [Message :: make [$usr name]]
    $msg show
    if ![$msg isJoined $usr] { $msg addUser $usr 0 0 }
}
#
proc /QUIT {net chid arg} { $net doQuit $arg }
#
proc /QUOTE {net chid arg} {
    regsub -all {[][$\\"{}]} $arg {\\&} cmd
    eval $net send $cmd
}
#
proc /REDIRECT {net chid arg} { sillyPerson $net }
#
proc /REHASH {net chid arg} { $net q1Send REHASH }
#
proc /RESTART {net chid arg} { $net q1Send RESTART }
#
proc /SAVE {net chid arg} { sillyPerson $net }
#
proc /SAY {net chid arg} { $chid send $arg }
#
proc /SEND {net chid arg} { $chid send $arg }
#
proc /SENDLINE {net chid arg} { sillyPerson $net }
#
proc /SERVER {net chid arg} {
    set sv [Server :: make [lindex $arg 0]]
    if {[set port [lindex $arg 1]] != {}} { $sv configure -port $port }
    $net changeServer $sv
}
#
proc /SET {net chid arg} { sillyPerson $net }
#
proc /SIGNOFF {net chid arg} { $net doQuit $arg }
#
proc /SLEEP {net chid arg} { sillyPerson $net }
#
proc /SQUIT {net chid arg} { $net send SQUIT $arg }
#
proc /STATS {net chid arg} { $net STATS [lindex $arg 0] [lindex $arg 1] }
#
proc /SUMMON {net chid arg} { $net send SUMMON $arg }
#
proc /TIME {net chid arg} { $net send TIME $arg }
#
proc /TIMER {net chid arg} { sillyPerson $net }
#
proc /TOPIC {net chid arg} { $chid configure -topic [lindex $arg 0] }
#
proc /TRACE {net chid arg} { $net nsend TRACE $arg }
#
proc /TYPE {net chid arg} { sillyPerson $net }
#
proc /USERHOST {net chid arg} {
    foreach x $arg {
	if [string compare -cmd $x] {$net USERHOST $x} {
	    sillyPerson $net
	    break
	}
    }
}
#
proc /USERS {net chid arg} { $net send USERS $arg }
#
proc /VERSION {net chid arg} { $net send VERSION $arg }
#
proc /WAIT {net chid arg} { sillyPerson $net }
#
proc /WALLOPS {net chid arg} { sillyPerson $net }
#
proc /WHICH {net chid arg} { sillyPerson $net }
#
proc /WHILE {net chid arg} { sillyPerson $net }
#
proc /WHO {net chid arg} { eval $net send WHO $arg }
#
proc /WHOIS {net chid arg} { eval $net send WHOIS $arg }
#
proc /WHOWAS {net chid arg} { eval $net send WHOWAS $arg }
#
proc /WINDOW {net chid arg} { sillyPerson $net}
#
proc /XECHO {net chid arg} { sillyPerson $net}
#
proc /XTYPE {net chid arg} { sillyPerson $net}
#
proc sillyPerson {net} {
    bell
    tkwait window [mkDialog {} Silly {Don't be silly! This is Zircon!!} {}]
}
#
proc register {net menu} {
    $menu delete 0 end
    $menu add command -label {Join mailing list} -command "doZList $net"
    $menu add command -label {Leave mailing list} -command "doZLeave $net"
    if [file exist ~/.zirconreg] {
	$menu add command -label {No update messages} \
	  -command "doDeregister $net"
    } {
	$menu add command -label {Get update messages} \
	  -command "doRegister $net"
    }
}
#
proc doRegister {net} {
    global zircon
    if ![file exists ~/.zirconreg] { 
	doBotcom $net REGISTER "$zircon(version) PL $zircon(patchlevel)"
	catch {close [open ~/.zirconreg w]}
    }
}
#
proc doDeregister {net} {
    if [file exists ~/.zirconreg] { 
	doBotcom $net DEREGISTER {}
	exec rm -f [glob ~/.zirconreg]
    }
}
#
proc doZList {net} {
    global ztrans
    mkEntryBox .@lj {Join mailing List} "Enter your email address:" \
      {{{Email address} {}}} "$ztrans(ok) {doBotcom $net ZLIST}" \
      "$ztrans(cancel) {}"
}
#
proc doZLeave {net} {doBotcom $net ZLEAVE {}}
#
proc doBotcom {net type par} { $net PRIVMSG ZirconBot "!$type $par" }
