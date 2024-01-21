#!/usr/local/bin/wish -f

# ------------- Code for popup menus -------------
proc tixCreatePopupMenu {w text mkmenu_proc} {
   toplevel $w -cursor arrow
   wm overrideredirect $w 1
   wm withdraw $w
   menubutton $w.mb -text $text -menu $w.mb.m -anchor w
   pack $w.mb -expand yes -fill both

   $mkmenu_proc $w.mb.m

   bind $w.mb <1>                  "pupUnpost $w"
   bind $w.mb <3>                  "pupUnpost $w"
   bind $w.mb.m <Unmap>            "+pupUnmap $w"
   bind $w.mb.m <ButtonRelease-3> {tk_invokeMenu %W}
}

proc pupBindParents {w args} {
    foreach p $args {
	bind $p <3>         "pupMap $p $w %x %y"
	bind $p <Control-1> "pupMap $p $w %x %y"
    }
}

proc pupMap {parent w x y} {
    global tk_priv

    set width  [winfo reqwidth  $w]
    set height [winfo reqheight $w]
    wm geometry $w [format "%dx%d" [winfo reqwidth $w.mb.m] $height]    
    set width  [winfo reqwidth  $w]

    set wx [expr [winfo rootx $parent] + $x - $width  / 2]
    set wy [expr [winfo rooty $parent] + $y - $height / 2]

    wm geometry $w [format "+%d+%d" $wx $wy]
    wm deiconify $w
    raise $w

    update idletasks
    grab -global $w
    set tk_priv(inMenuButton) $w.mb 
    tk_mbButtonDown $w.mb
}

proc pupUnmap {w} {
    grab release $w.mb
    wm withdraw $w
}

proc pupUnpost {w} {
    grab release $w.mb
    tk_mbUnpost
    wm withdraw $w
}

if 0 {
# ---------- demostration ---------------------

proc MkMenu_Proc {w} {
    menu $w
    $w add command -label "New       " -command {puts new}
    $w add command -label "Open ...  " -command {puts open}
    $w add cascade -label "More      " -menu $w.more

    menu $w.more
    $w.more add command -label "New       " -command {puts new}
    $w.more add command -label "Open      " -command {puts open}
}

tixCreatePopupMenu .a Choices MkMenu_Proc
pupBindParents .a .

}
