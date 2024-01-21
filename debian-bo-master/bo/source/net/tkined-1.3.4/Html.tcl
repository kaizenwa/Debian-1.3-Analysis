##
## Html.tcl
##
## This file contains all the code that implements the HTML view.
##
## Copyright (c) 1993, 1994, 1995
##
## J. Schoenwaelder
## TU Braunschweig, Germany
## Institute for Operating Systems and Computer Networks
##
## Permission to use, copy, modify, and distribute this
## software and its documentation for any purpose and without
## fee is hereby granted, provided that this copyright
## notice appears in all copies.  The University of Braunschweig
## makes no representations about the suitability of this
## software for any purpose.  It is provided "as is" without
## express or implied warranty.
##

##
## The following set of procedures handle log objects.
##

proc HTML::canvas { log } {

    # The offset used to position log windows automatically.
    static offset
    if {![info exists offset]} {
	set offset 80
    } else {
	incr offset 10
	if {$offset > 180} {set offset 80}
    }

    set c [$log canvas]
    set w [winfo parent $c]
    toplevel $w.$log

    # set up the menu bar
    frame $w.$log.menu -borderwidth 1 -relief raised

    # set up the file menu
    menubutton $w.$log.menu.file -text "File" -menu $w.$log.menu.file.m
    menu $w.$log.menu.file.m
    $w.$log.menu.file.m add command -label "Clear" \
	-accelerator "  ^C" \
	-command "$log clear"
    bind $w.$log <Control-c> "$w.$log.menu.file.m invoke Clear"
    $w.$log.menu.file.m add command -label "Open..." \
	-accelerator "  ^O" \
	-command "LOG::load $log"
    bind $w.$log <Control-o> "$w.$log.menu.file.m invoke Open..."
    $w.$log.menu.file.m add command -label "Save As..." \
	-accelerator "  ^S" \
	-command "LOG::save $log"
    bind $w.$log <Control-s> "$w.$log.menu.file.m invoke {Save As...}"
    $w.$log.menu.file.m add sep
    $w.$log.menu.file.m add command -label "Print..." \
	-accelerator "  ^P" \
	-command "LOG::print $log"
    bind $w.$log <Control-p> "$w.$log.menu.file.m invoke Print..."
    $w.$log.menu.file.m add command -label "Email..." \
	-accelerator "  ^E" \
	-command "LOG::email $log"
    bind $w.$log <Control-e> "$w.$log.menu.file.m invoke Email..."
    $w.$log.menu.file.m add sep
    $w.$log.menu.file.m add command -label "New View" \
	-accelerator "  ^V" \
	-command "set l \[LOG create\]; \$l canvas $c; \$l name \[$log name\]"
    bind $w.$log <Control-v> "$w.$log.menu.file.m invoke {New View}"
    $w.$log.menu.file.m add command -label "Close View" \
	-accelerator "  ^K" \
	-command "$log delete"
    bind $w.$log <Control-k> "$w.$log.menu.file.m invoke {Close View}"

    # set up the option menu
    menubutton $w.$log.menu.opts -text "Options" -menu $w.$log.menu.opts.m
    menu $w.$log.menu.opts.m
    $w.$log.menu.opts.m add checkbutton -label "Freeze" \
	-offvalue 0 -onvalue 1 -variable freeze$log \
	-accelerator "  ^Z"
    bind $w.$log <Control-z> "$w.$log.menu.opts.m invoke Freeze"

    pack $w.$log.menu -side top -fill x
    pack $w.$log.menu.file -side left
    pack $w.$log.menu.opts -side left

    scrollbar $w.$log.scrollbar -command "$w.$log.text yview" -relief sunken
    text $w.$log.text -height 24 -width 80 -setgrid true -wrap word \
	-relief sunken -borderwidth 2 \
	-yscrollcommand "$w.$log.scrollbar set" 
    pack $w.$log.scrollbar -side right -fill y
    pack $w.$log.text -side left -padx 2 -pady 2 -fill both -expand yes

    $log items $w.$log

    # This special purpose binding makes it possible to send
    # complete lines back to the interpreter that created this
    # window. This allows us to use a log window as a simple
    # command frontend.

    bind $w.$log.text <Shift-Return> "LOG::process $log"

    # Position the log window on the screen.

    wm withdraw $w.$log
    update idletasks
    set top [winfo toplevel $w]

    set rx [expr {[winfo rootx $top]}]
    set ry [expr {[winfo rooty $top]}]

    set cx [expr $rx+[winfo width $top]/4]
    set cy [expr $ry+[winfo height $top]/4]

    set x  [expr $cx+$offset]
    set y  [expr $cy+$offset]

    if {$x < 0} { set x 0 }
    if {$y < 0} { set y 0 }

    wm geometry $w.$log +$x+$y
    wm deiconify $w.$log

    update idletasks
}

proc LOG::process { log } {

    set w [$log items].text
    $w delete insert
#    set line [$w index "insert - 1 line"]
    set line insert
    set txt [$w get [$w index "$line linestart"] [$w index "$line lineend"]]
    if {$txt == ""} return

    [$log interpreter] send \
	eval ined append $log \"\[ catch \{ $txt \} err \; set err \]\\n\"

    $w insert insert "\n"
}

##
## Implementation of the HTML object. Note, this will be the successor
## of the LOG object. For now, we use the LOG methods as the implementation
## is not finished yet.
##

proc HTML::name { html } {
    wm title [$html items] [$html name]
    wm iconname [$html items] [$html name]
}

proc HTML::icon { html bitmap } {
    wm iconbitmap [$html items] $bitmap
}

proc HTML::<TITLE> {html t} {
    catch {$html name $t} err
    puts stderr "HTML::<TITLE> $html $t -> $err"
}

proc HTML::<H1> {html t} {
    [$html items].text insert end "\n\n$t\n\n" <H1>
}

proc HTML::<H2> {html t} {
    [$html items].text insert end "\n\n$t\n\n" <H2>
}

proc HTML::<H3> {html t} {
    [$html items].text insert end "\n$t\n" <H3>
}

proc HTML::<H4> {html t} {
    [$html items].text insert end "\n$t\n" <H4>
}

proc HTML::<H5> {html t} {
    [$html items].text insert end "\n$t\n" <H5>
}

proc HTML::<H6> {html t} {
    [$html items].text insert end "\n$t\n" <H6>
}

proc HTML::<PRE> {html t} {
    [$html items].text insert end "\n\n$t\n\n" <PRE>
}

proc HTML::<LI> {html t} {
puts stderr HTML::<LI> 
    [$html items].text insert end "\no $t"
}

proc HTML::html { html args } {
    global freeze$html
    set w [$html items]

    foreach elem $args {
	set tag [lindex $elem 0]
	set txt [lindex $elem 1]
	if {[info commands HTML::<$tag>] != ""} {
	    HTML::<$tag> $html $txt
	    continue
	}
	switch $tag {
	    "" { $w.text insert end $txt NOTAG
		continue
	    }
	    BR {
		$w.text insert end "\n"
		continue
	    }
	    P {
		$w.text insert end "\n\n"
		continue
	    }
	}
	$w.text insert end $txt <$tag>
    }
    
    $w.text configure -font -*-times-medium-r-normal--*-120-*

    $w.text tag configure <U> -underline on
    $w.text tag configure <B>    -font -*-times-bold-r-normal--*-120-* 
    $w.text tag configure <IT>   -font -*-times-medium-i-normal--*-120-*
    $w.text tag configure <CODE> -font -*-courier-medium-r-normal--*-120-*
    $w.text tag configure <PRE>  -font -*-courier-medium-r-normal--*-120-*
    $w.text tag configure <LISTING> -font -*-courier-medium-r-normal--*-100-*
    $w.text tag configure <CENTER> -justify center

    $w.text tag configure <H1> -font -*-times-bold-r-normal--*-180-* 
    $w.text tag configure <H2> -font -*-times-bold-r-normal--*-140-* 
    $w.text tag configure <H3> -font -*-times-bold-r-normal--*-140-* 
    $w.text tag configure <H4> -font -*-times-bold-r-normal--*-120-* 
    $w.text tag configure <H5> -font -*-times-bold-r-normal--*-120-* 
    $w.text tag configure <H6> -font -*-times-bold-r-normal--*-100-* 

    $w.text tag configure <UL> -lmargin1 20 -lmargin2 20

# tkined.font1:  -*-courier-medium-r-normal--*-100-* Courier:C 10
# tkined.font2:  -*-courier-medium-r-normal--*-120-* Courier:C 12
# tkined.font3:  -*-courier-medium-r-normal--*-140-* Courier:C 14
# tkined.font4:  -*-courier-medium-r-normal--*-180-* Courier:C 18
# tkined.font5:  -*-helvetica-medium-r-normal--*-100-* Helvetica:H 10
# tkined.font6:  -*-helvetica-medium-r-normal--*-120-* Helvetica:H 12
# tkined.font7:  -*-helvetica-medium-r-normal--*-140-* Helvetica:H 14
# tkined.font8:  -*-helvetica-medium-r-normal--*-180-* Helvetica:H 18
# tkined.font9:  -*-times-medium-r-normal--*-100-* Times-Roman:T 10
# tkined.font10: -*-times-medium-r-normal--*-120-* Times-Roman:T 12
# tkined.font11: -*-times-medium-r-normal--*-140-* Times-Roman:T 14
# tkined.font12: -*-times-medium-r-normal--*-180-* Times-Roman:T 18

    if {! [set freeze$html]} {
	$w.text yview -pickplace end
    }
}

proc HTML::bind { html cmd text } {

    set w [$html items].text

if {1} {
    if {[winfo cells .] > 2} {
        set bold "-foreground red"
        set normal "-foreground {}"
    } else {
        set bold "-foreground white -background black"
        set normal "-foreground {} -background {}"
    }

    set start [$w index insert]
    $w insert $start "<$text>"
    set end [$w index insert]
    set tag "tag$start$end"
    $w tag add $tag $start $end
    $w yview -pickplace end

#    $w tag configure $tag -borderwidth 1 -relief groove
    $w tag configure $tag -underline 1
    $w tag bind $tag <Any-Enter> "$w tag configure $tag $bold"
    $w tag bind $tag <Any-Leave> "$w tag configure $tag $normal"
    $w tag bind $tag <Button-3> "[$html interpreter] send $cmd"
    $w tag bind $tag <Button-1> "[$html interpreter] send $cmd"
} else {

    static idx
    if {![info exists idx]} {set idx 0}
    incr idx
    button $w.$idx -text $text -font [$w cget -font] \
	-command "[$html interpreter] send $cmd" \
	-padx 0 -pady 0 -relief groove
    $w window create end -window $w.$idx
}
}

proc HTML::unbind { html } {
    set w [$html items].text
    foreach tag [$w tag names] {
	catch {$w tag delete $tag}
    }
}

proc HTML::clear { html } {
    [$html items].text delete 0.0 end
}

proc HTML::delete { html } {
    destroy [$html items]}

proc HTML::freeze { html } {
    if {[[$html items].menu.freeze cget -text] == "freeze"} {
	[$html items].menu.freeze configure -text melt
    } else {
	[$html items].menu.freeze configure -text freeze
    }
}

proc HTML::save { html } {

    set fname [Dialog::fileselect [$html items] "Write to file:"]
    if {$fname==""} return

    set mode "w"
    if {[file exists $fname]} {
	set result [Dialog::confirm [$html items] \
		    "File $fname already exists!" [list replace append cancel]]
	switch [lindex $result 0] {
	    "cancel" {
		return
	    }
	    "replace" {
		set mode "w"
	    }
	    "append" {
		set mode "a"
	    }
	}
    }

    if {[catch {open $fname $mode} file]} {
	Dialog::acknowledge [$html items] "Unable to open $fname."
	return
    }

    puts $file [[$html items].text get 1.0 end]
    close $file

    $html attribute filename $fname
}

proc HTML::load { html } {
    set dir [file dirname [$html attribute filename]]
    set fname [Dialog::fileselect [$html items] "Read from file:" $dir]
    if {$fname == ""} return

    if {[catch {open $fname r} file]} {
	Dialog::acknowledge [$html items] "Unable to read from $fname"
	return
    }

    $html attribute filename $fname

    set txt ""
    while {![eof $file]} {
	append txt "[gets $file]\n"
    }
    $html clear
    $html append $txt

    [$html items].text yview -pickplace end
    [$html items].text yview 1.0
    close $file
}

proc HTML::print { html } {

    set fname "/tmp/tkined[pid].html"
    catch {exec /bin/rm -f $fname}

    if {[file exists $fname] && ![file writable $fname]} {
	Dialog::acknowledge [$html items] "Can not write temporary file $fname."
	return
    }

    if {[catch {open $fname w} file]} {
	Dialog::acknowledge [$html items] "Can not open $fname: $file"
	return
    }

    if {[catch {puts $file [[$html items].text get 1.0 end]} err]} {
	Dialog::acknowledge [$html items] "Failed to write $fname: $err"
    }

    catch {close $file}

    Editor::print [$html editor] [$html items] $fname

    catch {exec /bin/rm -f $fname}
}

proc HTML::email { html } {
    global env

    set result [Dialog::request [$html items] \
		"Please enter the email address:" \
		[list [list To: [$html address]] \
		      [list Subject: [$html name]] ] \
		[list send cancel] ]
    if {[lindex $result 0] == "cancel"} return

    set to [lindex $result 1]
    $html address $to
    set subject [lindex $result 2]

    if {[catch {split $env(PATH) :} path]} {
	set path "/usr/bin /bin /usr/ucb /usr/local/bin"
    }

    set mprog ""
    foreach mailer "Mail mail" {
	foreach dir $path {
	    set fname $dir/$mailer
	    if {[file executable $fname] && [file isfile $fname]} {
		set mprog $fname
		break
	    }
	}
	if {$mprog != ""} break
    }

    if {$mprog == ""} {
	Dialog::acknowledge [$html items] "Sorry, can not find mail program."
	return
    }

    if {[catch {open "|$mprog -s \"$subject\" $to" w} file]} {
        Dialog::acknowledge [$html items] "Unable to write to $mprog $to"
        return
    }

    puts $file [[$html items].text get 1.0 end]
    close $file
}

