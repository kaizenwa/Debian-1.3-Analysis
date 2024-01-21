proc gk_make_about_box {} {
    if {([winfo depth .]<4)||([userprefs fancy_about]=="no")} {
	gk_about .about
    } else {
	_gk_do_fancy_about
    }
}

proc _gk_do_fancy_about {} {
    toplevel .about
    wm title .about "About GroupKit"
    pack [frame .about.buttons] -side bottom -fill x -pady 3
    pack [button .about.buttons.dismiss -text Dismiss \
	    -command _gk_about_dismiss]  -side right -padx 3
    pack [button .about.buttons.vers32 -text Version \
	    -command _gk_about_version] -side left -padx 3
    pack [button .about.buttons.credits -text Credits \
	    -command _gk_about_credits] -side left -padx 3
    pack [button .about.buttons.contact -text Contact \
	    -command _gk_about_contact] -side left -padx 3
    pack [canvas .about.canvas -width 430 -height 265] -side top
    _gk_about_version
}

proc _gk_about_dismiss {} {
    catch {image delete vers32}
    catch {image delete credits}
    catch {image delete contact}
    destroy .about
}

proc _gk_about_show which {  global gk_library
    if {[lsearch [image names] $which]==-1} {
	image create photo $which -file $gk_library/library/images/${which}.gif
    } 
    .about.canvas delete image
    .about.canvas create image 0 0 -anchor nw -image $which -tags image
#    if {$which=="vers32"} {
#	.about.canvas create text 300 223 -text "Beta 4" -tags image
#    }
    .about.buttons.vers32 config -state normal
    .about.buttons.contact config -state normal
    .about.buttons.credits config -state normal
    .about.buttons.$which config -state disabled
}

proc _gk_about_version {} {_gk_about_show vers32}
proc _gk_about_credits {} {_gk_about_show credits}
proc _gk_about_contact {} {_gk_about_show contact}
