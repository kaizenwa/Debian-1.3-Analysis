
proc _gk_conf_card_setattr {w attr val} {
    if {$val!=""} {$w.card configure $attr $val}
}

proc gk_confMakeCard {usernum} {
    set w .card$usernum
    if {![winfo exists $w]} {
	toplevel $w
        wm title $w [gk_getUserAttrib $usernum username]
	pack [gk_card $w.card -command "destroy $w"]
	_gk_conf_card_setattr $w -name [gk_getUserAttrib $usernum username]
	_gk_conf_card_setattr $w -title [gk_getUserAttrib $usernum personal.title]
	_gk_conf_card_setattr $w -dept [gk_getUserAttrib $usernum personal.dept]
	_gk_conf_card_setattr $w -company [gk_getUserAttrib $usernum personal.company]
	_gk_conf_card_setattr $w -phone [gk_getUserAttrib $usernum personal.phone]
	_gk_conf_card_setattr $w -fax [gk_getUserAttrib $usernum personal.fax]
	_gk_conf_card_setattr $w -email [gk_getUserAttrib $usernum personal.email]
	_gk_conf_card_setattr $w -www [gk_getUserAttrib $usernum personal.www]
	_gk_conf_card_setattr $w -office [gk_getUserAttrib $usernum personal.office]
	if {[member portrait$usernum [image names]]} {
	    $w.card configure -image portrait$usernum
	} else {
	    if {$usernum==[users local.usernum]} {
		if {[file exists ~/.gkpict.gif]} {
		    image create photo portrait$usernum -file ~/.gkpict.gif
		    $w.card configure -image portrait$usernum
		}
	    } else {
		gk_toUserNum $usernum _gk_conf_requestPortrait [users local.usernum]
	    }
	}
    }
}

proc _gk_conf_requestPortrait usernum {
    if {[file exists ~/.gkpict.gif]} {
	set portrait [exec uuencode [glob ~/.gkpict.gif] portrait.gif]
	gk_toUserNum $usernum _gk_conf_receivePortrait [users local.usernum] $portrait
    }
}

proc _gk_conf_receivePortrait {usernum portrait} {
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