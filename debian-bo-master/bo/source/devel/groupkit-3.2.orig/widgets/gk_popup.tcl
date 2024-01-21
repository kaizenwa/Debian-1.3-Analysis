proc gk_popup {w user local which} {

    if {$user==$local} {
	set msg "This is you"
    } else {
	set msg "[users remote.$user.username]"
    }

    switch -exact $which {
	canvas {
	    $w bind tag$user <1> "_gk_popupMenu $w.popup {$msg} %X %Y"
	    $w bind tag$user <B1-ButtonRelease> "destroy $w.popup"
	}
	text { 
	    $w tag bind tag$user <1> "_gk_popupMenu $w.popup {$msg} %X %Y"
	    $w tag bind tag$user <B1-ButtonRelease> "destroy $w.popup"
	}
	default {
	    bind $w <1> "_gk_popupMenu $w.popup {$msg} %X %Y"
	    bind $w <B1-ButtonRelease> "destroy $w.popup"
	}
    }
}

proc _gk_popupMenu {w msg x y} {
    menu $w
    $w add command -label $msg
    $w post $x $y
}