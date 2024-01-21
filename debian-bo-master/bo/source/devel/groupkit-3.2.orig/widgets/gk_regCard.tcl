
proc _gk_reg_card_setattr {w attr val} {
    if {$val!=""} {$w.card configure $attr $val}
}

proc gk_regMakeCard {usernum} {
    set w .card$usernum
    if {![winfo exists $w]} {
	toplevel $w
        wm title $w [regclients remote.$usernum.name]
	pack [gk_card $w.card -command "destroy $w"]
	_gk_reg_card_setattr $w -name [regclients remote.$usernum.name]
	_gk_reg_card_setattr $w -title [regclients remote.$usernum.title]
	_gk_reg_card_setattr $w -dept [regclients remote.$usernum.dept]
	_gk_reg_card_setattr $w -company [regclients remote.$usernum.company]
	_gk_reg_card_setattr $w -phone [regclients remote.$usernum.phone]
	_gk_reg_card_setattr $w -fax [regclients remote.$usernum.fax]
	_gk_reg_card_setattr $w -email [regclients remote.$usernum.email]
	_gk_reg_card_setattr $w -www [regclients remote.$usernum.www]
	_gk_reg_card_setattr $w -office [regclients remote.$usernum.office]
	if {[member portrait$usernum [image names]]} {
	    $w.card configure -image portrait$usernum
	} else {
	    if {$usernum==[gk_rc_getMyID]} {
		if {[file exists ~/.gkpict.gif]} {
		    image create photo portrait$usernum -file ~/.gkpict.gif
		    $w.card configure -image portrait$usernum
		}
	    } else {
		gk_sendToRC $usernum _gk_reg_requestPortrait [gk_rc_getMyID]
	    }
	}
    }
}

proc _gk_reg_requestPortrait usernum {
    if {[file exists ~/.gkpict.gif]} {
	set portrait [exec uuencode [glob ~/.gkpict.gif] portrait.gif]
	gk_sendToRC $usernum _gk_reg_receivePortrait [gk_rc_getMyID] $portrait
    }
}

proc _gk_reg_receivePortrait {usernum portrait} {
    if {[winfo exists .card$usernum]} {
	cd /tmp
	set fd [open gkport$usernum w]
	puts $fd $portrait
	close $fd
	exec uudecode gkport$usernum
	image create photo portrait$usernum -file portrait.gif
	.card$usernum.card configure -image portrait$usernum
	exec /bin/rm gkport$usernum portrait.gif
    }
}