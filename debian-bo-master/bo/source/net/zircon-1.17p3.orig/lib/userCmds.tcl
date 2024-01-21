#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/userCmds.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
#
#	Users cmds
#
#
proc userInvite {net chan} {
    global ztrans
    mkEntryBox .@inv$net $ztrans(invite) {Enter user and channel:} \
      "{$ztrans(user) {}} {$ztrans(channel) $chan}" \
      "$ztrans(ok) {$net INVITE}" "$ztrans(cancel) {}"
}
#
proc doMsg {net nk} {if ![string match {} $nk] { Message :: make $nk }}
#
proc userMessage {net} {
    global ztrans
    mkEntryBox .@msg$net $ztrans(message) {Enter user name:} \
      "{$ztrans(user) {}}" "$ztrans(ok) {doMsg $net}" "$ztrans(cancel) {}"
}
#
proc userFinger {net} {
    global ztrans
    mkEntryBox .@fng$net $ztrans(finger) {Enter user name:} \
      "{$ztrans(user) {}}" "$ztrans(ok) {finger $net}" "$ztrans(cancel) {}"
}
#
proc userNotice {net} {
    global ztrans
    mkEntryBox .@not$net $ztrans(notice) {Enter user name and notice text:} \
      "{$ztrans(user) {}} {$ztrans(notice) {}}" \
      "$ztrans(ok) {$net NOTICE}" "$ztrans(cancel) {}"
}
#
proc userWhois {net} {
    global ztrans
    mkEntryBox .@wis$net $ztrans(whois) {Enter user name and server:} \
      "{$ztrans(user) {}} {Where {}}" "$ztrans(ok) {$net WHOIS}" "$ztrans(cancel) {}"
}
#
proc userWhowas {net} {
    global ztrans
    mkEntryBox .@was$net $ztrans(whowas) {Enter user name and count:} \
      "{$ztrans(user) {}} {Count {}}" "\
      $ztrans(ok) {$net WHOWAS}" "$ztrans(cancel) {}"
}
#
proc doUMode {net nk mode} {
    if {![string match {} $nk] && [string match {[+-]*} mode]} {$net qSend MODE $nk :$mode }
}
#
proc userMode {net} {
    global ztrans
    mkEntryBox .@umd$net {User Mode} {Enter user and mode:} \
      "{$ztrans(user) [$net nickname]} {$ztrans(mode) {}}" \
      "$ztrans(ok) {doUMode $net}" "$ztrans(cancel) {}"
}
#
proc userCmd {net cmd args} {
    switch -exact -- $cmd {
    Mode { userMode $net}
    Finger {userFinger $net}
    Invite { userInvite $net {} }
    Message {userMessage $net}
    Notice {userNotice $net}
    Kill {userKill $net}
    Whois {userWhois $net}
    Whowas {userWhowas $net}
    default {
	    global ztrans
	    mkEntryBox .@$cmd$net [trans $cmd] {Enter user pattern:} \
	      "{$ztrans(user) {}}" \
	      "$ztrans(ok) {$net send [string toupper $cmd]}" \
	      "$ztrans(cancel) {}"
	}
    }
}
