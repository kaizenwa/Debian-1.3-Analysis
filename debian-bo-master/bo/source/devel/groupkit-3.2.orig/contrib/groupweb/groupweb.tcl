gk_initConf $argv
pack [gk_defaultMenu .menu] -side top -fill x

pack [entry .e] -side top -fill x
bind .e <Return> {switchto [.e get]}

pack [gk_scrollbar .s -command ".t yview"] -side right -fill y
pack [text .t -yscrollcommand ".s set"] -side left -fill both -expand yes

gk_initializeTelepointers
gk_specializeWidgetTreeTelepointer .t

source [userprefs scriptpath]/html_library.tcl
source [userprefs scriptpath]/url.tcl
source [userprefs scriptpath]/http_get.tcl

HMinit_win .t

proc HMlink_callback {win href} {
    global url
    set newurl [URL_canonicalise $url $href]
    switchto $newurl
}

proc HMset_image {win handle src} {
    global url counter
    set src [URL_canonicalise $url $src]
    set fn /tmp/webbrowse$counter.img
    incr counter
    http_get $src file $fn "finishImage $counter $handle $fn"
}

proc finishImage {counter handle fn status} {
    if {$status=="complete"} {
	if [winfo exists $handle] {
	    catch {image create photo img$counter -file $fn}
	    catch {HMgot_image $handle img$counter} 
	    catch {gk_specializeWidgetTelepointer $handle}
	    catch {exec /bin/rm -f $fn}
	}
    }
}    

proc switchto {theurl} {
    global notes
    gk_serialize clearNote
    gk_toAll go_url $theurl
    if [info exists notes($theurl)] {
	gk_serialize setNote $notes($theurl)
    }
}

proc go_url {theurl} {
    global url
    set url $theurl
    .e delete 0 end
    .e insert 0 $theurl
    HMreset_win .t
    if {[catch {http_get $url variable main_page "renderPage"} errmsg]==1} {
	.t insert 1.00 "Could not fetch page: $errmsg"
    }
}

proc renderPage {status} {
    if {$status=="complete"} {
	global main_page
	HMparse_html $main_page {HMrender .t}
    }
}

set counter 0


set start_url http://www.cpsc.ucalgary.ca/projects/grouplab/groupkit/
toplevel .notes
pack [gktext .notes.txt -width 50 -height 20]

gk_specializeWidgetTreeTelepointer .notes

proc clearNote {} {
    global notes url
    set notes($url) [.notes.txt get 1.0 end-1c]
    .notes.txt delete 1.0 end
}

proc setNote {txt} {
    .notes.txt insert 1.0 $txt
}

proc HMtag_title {win param text} {
    upvar $text data
    wm title [winfo toplevel $win] $data
    wm title .notes "Notes on $data"
    set data ""
}

if [gk_amOriginator] {
    after 1000 go_url $start_url
}

gk_bind updateEntrant "doUpdateEntrant %U"
proc doUpdateEntrant usernum {
    global url
    gk_toUserNum $usernum go_url $url
}


