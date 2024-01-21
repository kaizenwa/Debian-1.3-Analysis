##
## Objects.tcl
##
## This file contains the callbacks that are called whenever and object
## changes its internal state. All calls to procs defined in this file
## should be made by one of the methods defined in methods.c. This file
## implements the View according to the Model-View-Controller paradigm.
##
## Every object can stores its main canvas item in its items attribute.
## All additional items used to create a label etc. are accessed using 
## tags. We use the following tag names (assuming $item has the canvas 
## as stored in the item attribute of an object):
##
##    mark$item   - a tag for all items marking $item as selected
##    label$item  - a tag for the text item containing a label
##    clip$item   - a tag for a box used to clip
##
## More speedups are possible by removing some indirection, but this
## will make the code less readable. So if we need more speed, we 
## should create a real icon canvas object that makes all this tk 
## code superfluous
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
## These procs are of general interest and are called by the
## type specific callbacks to save some code.
##

proc ::delete { id } {
    set c [$id canvas]
    if {$c == ""} {
	set c .[$id editor].canvas
    }
    foreach item [$id items] {
	$c delete $item mark$item label$item clip$item
    }
    $id items ""
}

proc ::size { id type } {
    return [[$id canvas] bbox [$id items]]
}

proc ::move { id dx dy } {
    set c [$id canvas]
    set item [$id items]
    $c move $item $dx $dy
    $c move mark$item $dx $dy
    $c move label$item $dx $dy
    $c move clip$item $dx $dy
}

proc ::select { id type } {
    set c [$id canvas]
    set item [$id items]
    $c addtag selected withtag $item
    if {[$c type $item] == "line" && $type != "GROUP"} {
	tkined_mark_points $c $item
    } else {
	tkined_mark_box $c $item
    }
}

proc ::unselect { id } {
    set c [$id canvas]
    set item [$id items]
    $c dtag $item selected
    $c delete mark$item
}

proc ::color { id type color } {
    set c [$id canvas]
    set item [$id items]
    switch [$c type $item] {
	bitmap { $c itemconfigure $item -foreground $color }
	line   { $c itemconfigure $item -fill $color }
	text   { $c itemconfigure $item -fill $color }
	stripchart { $c itemconfigure $item -outline $color }
	barchart   { $c itemconfigure $item -outline $color }
    }
}

proc ::font { id type font} {
    set c [$id canvas]
    set item [$id items]
    if {[$c type $item] == "text"} {
	$c itemconfigure $item -font $font
    }
    catch {$c itemconfigure label$item -font $font}
}

proc ::raise { id } {
    [$id canvas] raise "id $id"
}

proc ::lower { id } {
    [$id canvas] lower "id $id"
}

proc ::icon { id bitmap } {
    set c [$id canvas]
    set item [$id items]
    if {[$c type $item] == "bitmap"} {
	$c itemconfigure $item -bitmap $bitmap
    }
}

proc ::clearlabel { id } {
    set item [$id items]
    [$id canvas] delete label$item clip$item
}

proc ::label { id type text } {

    set c [$id canvas]
    set item [$id items]

    if {$item == ""} return

    # search for an existing label

    set label [$c find withtag label$item]
    set clip  [$c find withtag clip$item]

    if {$label != ""} {
        $c itemconfigure $label -text $text
        if {$clip != ""} {
            eval $c coords $clip [$c bbox $label]
        }
	return
    }

    # create a new label with and a box behind it

    set color [$c cget -background]

    if {$type == "NETWORK"} {
	set xy [$id labelxy]
	set x [lindex $xy 0]
	set y [lindex $xy 1]
    } else {
	set bb [$c bbox $item]
	set x1 [lindex $bb 0]
	set x2 [lindex $bb 2]
	set y [expr {[lindex $bb 3]+1}]
	set x [expr {$x1+(($x2-$x1)/2)}]
    }

    set label [$c create text $x $y -anchor n -text $text -font fixed \
	       -justify center -tags [list label$item "id $id"] ]
    set tags [list clip$item "id $id"]
    set clip [eval $c create rectangle [$c bbox $label] \
	      -tags {$tags} -fill $color -outline $color -width 0 ]
    $c lower $clip $label
    $id font [$id font]
}

proc ::postscript { id } {

    global tkined_ps_map
    set tkined_ps_map(fixed) [list Courier 10]

    set c [$id canvas]
    set item [$id items]

    set bb [$c bbox $items $label$item $clip$item]

    set width  [expr {[lindex $bb 2] -[lindex $bb 0]}]
    set height [expr {[lindex $bb 3] -[lindex $bb 1]}]
    set x [lindex $bb 0]
    set y [lindex $bb 1]

    $c postscript -fontmap tkined_ps_map \
	    -height $height -width $width -x $x -y $y
}

##
## The following set of procedures handle node objects.
##

proc NODE::canvas { node } {
    $node items [[$node canvas] create bitmap \
		  [lindex [$node move] 0] [lindex [$node move] 1] \
		      -background [[$node canvas] cget -background] \
		      -bitmap machine -tags [list NODE "id $node"] ]
}

proc NODE::delete { node } {
    ::delete $node
}

proc NODE::size { node } {
    ::size $node NODE
}

proc NODE::move { node dx dy } {
    ::move $node $dx $dy
}

proc NODE::select { node } {
    ::select $node NODE
}

proc NODE::unselect { node } {
    ::unselect $node
}

proc NODE::color { node color } {
    ::color $node NODE $color
}

proc NODE::font { node font } {
    ::font $node NODE $font
}

proc NODE::raise { node } {
    ::raise $node
}

proc NODE::lower { node } {
    ::lower $node
}

proc NODE::icon { node bitmap } {
    ::icon $node $bitmap
}

proc NODE::clearlabel { node } {
    ::clearlabel $node
}

proc NODE::label { node text } {
    ::label $node NODE $text
}


##
## The following set of procedures handle group objects.
##

proc GROUP::canvas { group } {
    $group collapse
}

proc GROUP::delete { group } {
    ::delete $group
}

proc GROUP::size { group } {
    ::size $group GROUP
}

proc GROUP::move { group dx dy } {
    ::move $group $dx $dy
}

proc GROUP::select { group } {
    ::select $group GROUP
}

proc GROUP::unselect { group } {
    ::unselect $group
}

proc GROUP::color { group color } {
    ::color $group GROUP $color
}

proc GROUP::font { group font } {
    ::font $group GROUP $font
}

proc GROUP::raise { group } {
    ::raise $group
}

proc GROUP::lower { group } {
    ::lower $group
}

proc GROUP::icon { group bitmap } {
    ::icon $group $bitmap
}

proc GROUP::clearlabel { group } {
    ::clearlabel $group
}

proc GROUP::label { group text} {
    ::label $group GROUP $text
}

proc GROUP::collapse { group } {

    ::delete $group
    foreach id [$group member] {
	::delete $id
    }

    $group items [[$group canvas] create bitmap \
		    [lindex [$group move] 0] [lindex [$group move] 1] \
		      -background [[$group canvas] cget -background] \
		      -bitmap group -tags [list GROUP "id $group"] ]
}

proc GROUP::expand { group } {

    set c [$group canvas]
    ::delete $group

    set bb [$group move]
    set x1 [expr {[lindex $bb 0]-30}]
    set y1 [expr {[lindex $bb 1]-30}]
    set x2 [expr {[lindex $bb 0]+30}]
    set y2 [expr {[lindex $bb 1]+30}]
    
    set tags [list GROUP "id $group"]
    $group items [eval $c create line $x1 $y1 $x1 $y2 $x2 $y2 $x2 $y1 $x1 $y1 \
		    -width 2 -fill Black -stipple gray50 -joinstyle miter \
			-tags {$tags}]

    if {[$group member] != ""} {
	GROUP::resize $group
    }
}

proc GROUP::resize { group } {

    set c [$group canvas]

    set memberitems ""
    foreach id [$group member] {
	set item [$id items]
        lappend memberitems $item label$item
    }
    if {$memberitems == ""} return

    set bb [eval $c bbox [join $memberitems]]
    set x1 [expr {[lindex $bb 0]-3}]
    set y1 [expr {[lindex $bb 1]-3}]
    set x2 [expr {[lindex $bb 2]+3}]
    set y2 [expr {[lindex $bb 3]+3}]

    $c coords "id $group" $x1 $y1 $x1 $y2 $x2 $y2 $x2 $y1 $x1 $y1
}


##
## The following set of procedures handle network objects.
##

proc NETWORK::canvas { network } {
    set c [$network canvas]
    set points [join [$network points]]
    set tags [list NETWORK "id $network"]
    set item [eval $c create line $points -width 3 -fill black -tags {$tags} ]
    eval $c move $item [$network move]
    $network items $item
}

proc NETWORK::delete { network } {
    ::delete $network
}

proc NETWORK::size { network } {
    ::size $network NETWORK
}

proc NETWORK::move { network dx dy } {
    ::move $network $dx $dy
}

proc NETWORK::select { network } {
    ::select $network NETWORK
}

proc NETWORK::unselect { network } {
    ::unselect $network
}

proc NETWORK::color { network color } {
    ::color $network NETWORK $color
}

proc NETWORK::icon { network width } {
    [$network canvas] itemconfigure [$network items] -width $width
}

proc NETWORK::clearlabel { network } {
    ::clearlabel $network
}

proc NETWORK::label { network text } {
    ::label $network NETWORK $text
}

proc NETWORK::font { network font } {
    ::font $network NETWORK $font
}

proc NETWORK::raise { network } {
    ::raise $network
}

proc NETWORK::lower { network } {
    ::lower $network
}


##
## The following set of procedures handle link objects.
##

proc LINK::canvas {link} {
    set c [$link canvas]
    set points [join [$link points]]
    set len [llength $points]
    if {$len%2 != 0} { 
	incr len -2
	set points [join [lrange $points 1 len]]
	inr len
    }
    set xya [[$link src] move]
    set xyb [[$link dst] move]
    set tags [list LINK "id $link"]
    $link items [eval $c create line $xya $points $xyb \
		 -fill black -tags {$tags}]
}

proc LINK::delete { link } {
    ::delete $link
}

proc LINK::size { link } {
    ::size $link LINK
}

proc LINK::move { link dx dy } {
    ::move $link $dx $dy
}

proc LINK::select { link } {
    ::select $link LINK
}

proc LINK::unselect { link } {
    ::unselect $link
}

proc LINK::color { link color } {
    ::color $link LINK $color
}

proc LINK::raise { link } {
    [$link canvas] raise [$link items]
}

proc LINK::lower { link } {
    [$link canvas] lower [$link items]
}


##
## The following set of procedures handle text objects.
##

proc TEXT::canvas { text } {
    set c [$text canvas]
    set w [winfo parent $c]
    set x [lindex [$text move] 0]
    set y [lindex [$text move] 1]
    set txt [$text text]
    regsub -all "\\\\n" $txt "\n" txt
    $text items [$c create text $x $y -anchor nw -text $txt -font fixed \
		 -tags [list TEXT "id $text"]  ]
}

proc TEXT::delete { text } {
    ::delete $text
}

proc TEXT::size { text } {
    ::size $text TEXT
}

proc TEXT::move { text dx dy } {
    ::move $text $dx $dy
}

proc TEXT::select { text } {
    ::select $text TEXT
}

proc TEXT::unselect { text } {
    ::unselect $text
}

proc TEXT::color { text color } {
    ::color $text TEXT $color
}

proc TEXT::font { text font } {
    ::font $text TEXT $font
}

proc TEXT::text { text } {
    set c [$text canvas]
    regsub -all "\\\\n" [$text text] "\n" txt
    set item [$text items]
    $c itemconfigure $item -text $txt
}

proc TEXT::raise { text } {
    [$text canvas] raise [$text items]
}

proc TEXT::lower { text } {
    [$text canvas] lower [$text items]
}


##
## The following set of procedures handle image objects.
##

proc IMAGE::canvas { image } {

    set c [$image canvas]
    set fname [$image name]

    # expand URL ftp syntax

    if {[string match "ftp://*" $fname] || [string match "file://*" $fname]} {
        set idx [string first "://" $fname]
        incr idx 3
        set fname [string range $fname $idx end]
        set idx [string first "/" $fname]
        set server [string range $fname 0 [expr {$idx-1}]]
        set file [string range $fname $idx end]
        set fname "/tmp/$server:[file tail $file]"

        if {[catch {tkined_ftp $server $file $fname} err]} {
            Dialog::acknowledge $w.canvas "Can not retrieve file $fname." $err
            return ""
        }
    }

    set x [lindex [$image move] 0]
    set y [lindex [$image move] 1]
    set tags [list IMAGE "id $image"]
if {0} {
    if {[catch {$c create bitmap $x $y -bitmap @$fname -tags $tags} item]} {
	Dialog::acknowledge $c "Image file not readable!"
	$image canvas ""
	return
    }
} else {
    if {[catch {image create photo -file $fname} img]} {
	if {[catch {image create bitmap -file $fname -maskfile $fname} img]} {
	    Dialog::acknowledge $c "Image file not readable!" "" $img
	    $image canvas ""
	    return
	}
    }
    set item [$c create image $x $y -image $img -tags $tags]
}
    $image items $item
    $image lower
}

proc IMAGE::delete { image } {
    ::delete $image
}

proc IMAGE::size { image } {
    ::size $image IMAGE
}

proc IMAGE::move { image dx dy } {
    ::move $image $dx $dy
}

proc IMAGE::select { image } {
    ::select $image IMAGE
}

proc IMAGE::unselect { image } {
    ::unselect $image
}

proc IMAGE::color { image color } {
    ::color $image IMAGE $color
}

proc IMAGE::lower { image } {
    [$image canvas] lower [$image items]
}


##
## The following set of procedures handle MENU objects.
##

proc MENU::canvas { menu } {

    set interpreter [$menu interpreter]
    set c [$menu canvas]
    set w [winfo parent $c]
    set name $w.tools.[$menu id]

    set name $w.menu.[$menu id]
    menubutton $name -text [$menu name] -menu $name.m
    menu $name.m
    foreach cmd [$menu items] {
	if {$cmd==""} {
	    $name.m add separator
	} else {
	    tkined_makemenu $name.m $cmd newname newcmd
	    $newname add command -label $newcmd \
		-command [list MENU::send $menu $newcmd]
	}
    }

    pack $name -side left

    $interpreter items "$menu [$interpreter items]"
}

proc MENU::send { menu cmd } {

    set editor [$menu editor]
    set interpreter [$menu interpreter]
    set l ""
    foreach id [$editor selection] {
	lappend l [$id retrieve]
    }
    $interpreter send $cmd $l
}

proc MENU::delete { menu } {

    set interpreter [$menu interpreter]
    set c [$menu canvas]
    set w [winfo parent $c]

    destroy $w.menu.[$menu id]

    # and remove the menu from the interpreter
    set new ""
    foreach t [$interpreter items] {
	if {$t != $menu} { 
	    lappend new $t
	}
    }
    $interpreter items $new
}



##
## The following set of procedures handle interpreter objects.
## The delete method is a dummy that is needed in cases where
## the creation of a new interpreter fails (e.g. we can't open
## another pipe).
##

proc INTERPRETER::queue { interpreter qlen } {
    set w [[$interpreter editor] toplevel]
    if {$qlen > 0} { set state disabled } { set state normal }

    foreach menu [$interpreter items] {
	set name $w.menu.$menu
	$name configure -state $state
	set last [$name.m index last]
	if {$last == "none"} continue
	for {set idx 0} {$idx <= $last} {incr idx} {
	    catch {$name.m entryconfigure $idx -state $state}
	}
    }
}

proc INTERPRETER::delete { interpreter } {
}


##
## The following set of procedures handle log objects.
##

proc LOG::canvas { log } {

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
	-accelerator "  Alt+C" \
	-command "$log clear"
    bind $w.$log <Alt-c>  "$w.$log.menu.file.m invoke Clear"
    bind $w.$log <Alt-C>  "$w.$log.menu.file.m invoke Clear"
    bind $w.$log <Meta-c> "$w.$log.menu.file.m invoke Clear"
    bind $w.$log <Meta-C> "$w.$log.menu.file.m invoke Clear"
    $w.$log.menu.file.m add command -label "Open..." \
	-accelerator "  Alt+O" \
	-command "LOG::load $log"
    bind $w.$log <Alt-o>  "$w.$log.menu.file.m invoke Open..."
    bind $w.$log <Alt-O>  "$w.$log.menu.file.m invoke Open..."
    bind $w.$log <Meta-o> "$w.$log.menu.file.m invoke Open..."
    bind $w.$log <Meta-O> "$w.$log.menu.file.m invoke Open..."
    $w.$log.menu.file.m add command -label "Save As..." \
	-accelerator "  Alt+S" \
	-command "LOG::save $log"
    bind $w.$log <Alt-s>  "$w.$log.menu.file.m invoke {Save As...}"
    bind $w.$log <Alt-S>  "$w.$log.menu.file.m invoke {Save As...}"
    bind $w.$log <Meta-s> "$w.$log.menu.file.m invoke {Save As...}"
    bind $w.$log <Meta-S> "$w.$log.menu.file.m invoke {Save As...}"
    $w.$log.menu.file.m add sep
    $w.$log.menu.file.m add command -label "Print..." \
	-accelerator "  Alt+P" \
	-command "LOG::print $log"
    bind $w.$log <Alt-p>  "$w.$log.menu.file.m invoke Print..."
    bind $w.$log <Alt-P>  "$w.$log.menu.file.m invoke Print..."
    bind $w.$log <Meta-p> "$w.$log.menu.file.m invoke Print..."
    bind $w.$log <Meta-P> "$w.$log.menu.file.m invoke Print..."
    $w.$log.menu.file.m add command -label "Email..." \
	-accelerator "  Alt+E" \
	-command "LOG::email $log"
    bind $w.$log <Alt-e>  "$w.$log.menu.file.m invoke Email..."
    bind $w.$log <Alt-E>  "$w.$log.menu.file.m invoke Email..."
    bind $w.$log <Meta-e> "$w.$log.menu.file.m invoke Email..."
    bind $w.$log <Meta-E> "$w.$log.menu.file.m invoke Email..."
    $w.$log.menu.file.m add sep
    $w.$log.menu.file.m add command -label "New View" \
	-accelerator "  Alt+N" \
	-command "set l \[LOG create\]; \$l canvas $c; \$l name \[$log name\]"
    bind $w.$log <Alt-n>  "$w.$log.menu.file.m invoke {New View}"
    bind $w.$log <Alt-N>  "$w.$log.menu.file.m invoke {New View}"
    bind $w.$log <Meta-n> "$w.$log.menu.file.m invoke {New View}"
    bind $w.$log <Meta-N> "$w.$log.menu.file.m invoke {New View}"
    $w.$log.menu.file.m add command -label "Close View" \
	-accelerator "  Alt+W" \
	-command "$log delete"
    bind $w.$log <Alt-w>  "$w.$log.menu.file.m invoke {Close View}"
    bind $w.$log <Alt-W>  "$w.$log.menu.file.m invoke {Close View}"
    bind $w.$log <Meta-w> "$w.$log.menu.file.m invoke {Close View}"
    bind $w.$log <Meta-W> "$w.$log.menu.file.m invoke {Close View}"

    # set up the option menu
    menubutton $w.$log.menu.opts -text "Options" -menu $w.$log.menu.opts.m
    menu $w.$log.menu.opts.m
    $w.$log.menu.opts.m add checkbutton -label "Freeze" \
	-offvalue 0 -onvalue 1 -variable freeze$log \
	-accelerator "  ^Z"
    bind $w.$log <Control-z> "$w.$log.menu.opts.m invoke Freeze"
    $w.$log.menu.opts.m add checkbutton -label "Wrap" \
	-offvalue none -onvalue word -variable wrap$log \
	-accelerator "  ^W" -command "LOG::wrap $log"
    bind $w.$log <Control-w> "$w.$log.menu.opts.m invoke Wrap"

    pack $w.$log.menu -side top -fill x
    pack $w.$log.menu.file -side left
    pack $w.$log.menu.opts -side left

    scrollbar $w.$log.scrollbar -command "$w.$log.text yview" -relief sunken
    text $w.$log.text -height 24 -width 80 -setgrid true -wrap none \
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

proc LOG::name { log } {
    wm title [$log items] [$log name]
    wm iconname [$log items] [$log name]
}

proc LOG::icon { log bitmap } {
    wm iconbitmap [$log items] $bitmap
}

proc LOG::append { log text } {
    global freeze$log
    set w [$log items]
    $w.text insert end $text
    if {! [set freeze$log]} {
	$w.text yview -pickplace end
    }
}

proc LOG::bind { log cmd text } {

    set w [$log items].text

if {1} {
    set bold "-foreground red"
    set normal "-foreground {}"

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
    $w tag bind $tag <Button-3> "[$log interpreter] send $cmd"
    $w tag bind $tag <Button-1> "[$log interpreter] send $cmd"
} else {

    static idx
    if {![info exists idx]} {set idx 0}
    incr idx
    button $w.$idx -text "<$text>" -font [$w cget -font] \
	-command "[$log interpreter] send $cmd" \
	-padx 0 -pady 0 -relief groove
    $w window create end -window $w.$idx
}
}

proc LOG::unbind { log } {
    set w [$log items].text
    foreach tag [$w tag names] {
	catch {$w tag delete $tag}
    }
}

proc LOG::clear { log } {
    [$log items].text delete 0.0 end
}

proc LOG::delete { log } {
    destroy [$log items]
}

proc LOG::freeze { log } {
    if {[[$log items].menu.freeze cget -text] == "freeze"} {
	[$log items].menu.freeze configure -text melt
    } else {
	[$log items].menu.freeze configure -text freeze
    }
}

proc LOG::wrap { log } {
    global wrap$log
    [$log items].text configure -wrap [set wrap$log]
}

proc LOG::save { log } {

    set fname [Dialog::fileselect [$log items] "Write to file:"]
    if {$fname==""} return

    set mode "w"
    if {[file exists $fname]} {
	set result [Dialog::confirm [$log items] \
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
	Dialog::acknowledge [$log items] "Unable to open $fname."
	return
    }

    puts $file [[$log items].text get 1.0 end]
    close $file

    $log attribute filename $fname
}

proc LOG::load { log } {
    set dir [file dirname [$log attribute filename]]
    set fname [Dialog::fileselect [$log items] "Read from file:" $dir]
    if {$fname == ""} return

    if {[catch {open $fname r} file]} {
	Dialog::acknowledge [$log items] "Unable to read from $fname"
	return
    }

    $log attribute filename $fname

    set txt ""
    while {![eof $file]} {
	append txt "[gets $file]\n"
    }
    $log clear
    $log append $txt

    [$log items].text yview -pickplace end
    [$log items].text yview 1.0
    close $file
}

proc LOG::print { log } {

    set fname "/tmp/tkined[pid].log"
    catch {exec /bin/rm -f $fname}

    if {[file exists $fname] && ![file writable $fname]} {
	Dialog::acknowledge [$log items] "Can not write temporary file $fname."
	return
    }

    if {[catch {open $fname w} file]} {
	Dialog::acknowledge [$log items] "Can not open $fname: $file"
	return
    }

    if {[catch {puts $file [[$log items].text get 1.0 end]} err]} {
	Dialog::acknowledge [$log items] "Failed to write $fname: $err"
    }

    catch {close $file}

    Editor::print [$log editor] [$log items] $fname

    catch {exec /bin/rm -f $fname}
}

proc LOG::email { log } {
    global env

    set result [Dialog::request [$log items] \
		"Please enter the email address:" \
		[list [list To: [$log address]] \
		      [list Subject: [$log name]] ] \
		[list send cancel] ]
    if {[lindex $result 0] == "cancel"} return

    set to [lindex $result 1]
    $log address $to
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
	Dialog::acknowledge [$log items] "Sorry, can not find mail program."
	return
    }

    if {[catch {open "|$mprog -s \"$subject\" $to" w} file]} {
        Dialog::acknowledge [$log items] "Unable to write to $mprog $to"
        return
    }

    puts $file [[$log items].text get 1.0 end]
    close $file
}

##
## Implementation of references to other tkined maps.
##

proc REFERENCE::canvas { ref } {
    set c [$ref canvas]
    $ref items [$c create bitmap \
		[lindex [$ref move] 0] [lindex [$ref move] 1] \
		      -background [[$ref canvas] cget -background] \
		      -bitmap reference -tags [list REFERENCE "id $ref"] ]
}

proc REFERENCE::open { reference {editor ""} } {
    if {$editor == ""} {
	set editor [$reference editor]
    }
    foreach w [winfo children .] { $w configure -cursor watch }
    update idletasks
    set result [Command::Open $editor [$reference address]]
    foreach w [winfo children .] { $w configure -cursor top_left_arrow }
    return $result
}

proc REFERENCE::load { reference } {
    set editor [EDITOR]
    if {[REFERENCE::open $reference $editor] == ""} {
	$editor delete
    }
}

proc REFERENCE::delete { reference } {
    ::delete $reference
}

proc REFERENCE::size { reference } {
    ::size $reference REFERENCE
}

proc REFERENCE::move { reference dx dy } {
    ::move $reference $dx $dy
}

proc REFERENCE::select { reference } {
    ::select $reference REFERENCE
}

proc REFERENCE::unselect { reference } {
    ::unselect $reference
}

proc REFERENCE::color { reference color } {
    ::color $reference REFERENCE $color
}

proc REFERENCE::font { reference font } {
    ::font $reference REFERENCE $font
}

proc REFERENCE::raise { reference } {
    ::raise $reference
}

proc REFERENCE::lower { reference } {
    ::lower $reference
}

proc REFERENCE::icon { reference bitmap } {
    ::icon $reference $bitmap
}

proc REFERENCE::clearlabel { reference } {
    ::clearlabel $reference
}

proc REFERENCE::label { reference text } {
    ::label $reference REFERENCE $text
}


##
##
##

proc STRIPCHART::canvas { stripchart } {
    set c [$stripchart canvas]
    set item [$c create stripchart -31 -21 31 21 -outline black \
	    -background [$c cget -background] \
	    -tags [list STRIPCHART "id $stripchart"]]
    eval $c move $item [$stripchart move]
    $stripchart items $item
}

proc STRIPCHART::delete { stripchart } {
    ::delete $stripchart
}

proc STRIPCHART::size { stripchart } {
    ::size $stripchart STRIPCHART
}

proc STRIPCHART::move { stripchart dx dy } {
    ::move $stripchart $dx $dy
}

proc STRIPCHART::select { stripchart } {
    ::select $stripchart STRIPCHART
}

proc STRIPCHART::unselect { stripchart } {
    ::unselect $stripchart
}

proc STRIPCHART::color { stripchart color } {
    ::color $stripchart STRIPCHART $color
}

proc STRIPCHART::font { stripchart font } {
    ::font $stripchart STRIPCHART $font
}

proc STRIPCHART::raise { stripchart } {
    ::raise $stripchart
}

proc STRIPCHART::lower { stripchart } {
    ::lower $stripchart
}

proc STRIPCHART::clearlabel { stripchart } {
    ::clearlabel $stripchart
}

proc STRIPCHART::label { stripchart text } {
    ::label $stripchart STRIPCHART $text
}

proc STRIPCHART::resize { stripchart x1 y1 x2 y2 } {
    set c [$stripchart canvas]
    set item [$stripchart items]
    $c coords $item $x1 $y1 $x2 $y2
}

proc STRIPCHART::clear { stripchart } {
    set c [$stripchart canvas]
    set item [$stripchart items]
    set bb [$c bbox $item] 
    set lower [lindex $bb 0]
    set upper [lindex $bb 2]
    set values 0
    for {set x $lower} {$x < $upper} {incr x} {
	lappend values 0
    }
    $c itemconfigure $item -values $values
}

proc STRIPCHART::values { stripchart args } {
    set c [$stripchart canvas]
    set item [$stripchart items]
    if {$args == ""} {
	return [$c itemcget $item -values]
    } else {
	$c itemconfigure $item -values [join $args]
    }
}

proc STRIPCHART::scale { stripchart num } {
    set c [$stripchart canvas]
    set item [$stripchart items]
    $c itemconfigure $item -scalevalue $num
}

proc STRIPCHART::jump { stripchart num } {
    set c [$stripchart canvas]
    set item [$stripchart items]
    $c itemconfigure $item -jump $num
}


##
##
##

proc BARCHART::canvas { barchart } {
    set c [$barchart canvas]
    set item [$c create barchart -31 -21 31 21 \
	    -autocolor [expr [winfo cells .] > 2] \
	    -background [$c cget -background] \
	    -tags [list BARCHART "id $barchart"]]
    eval $c move $item [$barchart move]
    $barchart items $item
}

proc BARCHART::delete { barchart } {
    ::delete $barchart
}

proc BARCHART::size { barchart } {
    ::size $barchart BARCHART
}

proc BARCHART::move { barchart dx dy } {
    ::move $barchart $dx $dy
}

proc BARCHART::select { barchart } {
    ::select $barchart BARCHART
}

proc BARCHART::unselect { barchart } {
    ::unselect $barchart
}

proc BARCHART::color { barchart color } {
    ::color $barchart BARCHART $color
}

proc BARCHART::font { barchart font } {
    ::font $barchart BARCHART $font
}

proc BARCHART::raise { barchart } {
    ::raise $barchart
}

proc BARCHART::lower { barchart } {
    ::lower $barchart
}

proc BARCHART::clearlabel { barchart } {
    ::clearlabel $barchart
}

proc BARCHART::label { barchart text } {
    ::label $barchart BARCHART $text
}

proc BARCHART::resize { barchart x1 y1 x2 y2 } {
    set c [$barchart canvas]
    set item [$barchart items]
    $c coords $item $x1 $y1 $x2 $y2
}

proc BARCHART::clear { barchart } {
    $barchart values 0
}

proc BARCHART::values { barchart args } {
    set c [$barchart canvas]
    set item [$barchart items]
    if {$args == ""} {
	return [$c itemcget $item -values]
    } else {
	$c itemconfigure $item -values [join $args]
    }
}

proc BARCHART::scale { barchart num } {
    set c [$barchart canvas]
    set item [$barchart items]
    $c itemconfigure $item -scalevalue $num
}

## -------------------------------------------------------------------------

proc GRAPH::canvas { graph } {
    set c [$graph canvas]
    $c element create $graph
}

proc GRAPH::delete { graph } {
    set c [$graph canvas]
    $c element delete $graph
}

proc GRAPH::select { graph } {
    set c [$graph canvas]
    $c element configure $graph -symbol circle
    $c element configure $graph -scale 0.5
    $c element configure $graph -linewidth 2
}

proc GRAPH::unselect { graph } {
    set c [$graph canvas]
    $c element configure $graph -symbol line
    $c element configure $graph -linewidth 0
}

proc GRAPH::color { graph color } {
    set c [$graph canvas]
    $c element configure $graph -foreground $color
}

proc GRAPH::icon { graph dashes } {
    [$graph canvas] element configure $graph -dashes $dashes
}

proc GRAPH::clearlabel { graph } {
## XXX got memory errors
##    [$graph canvas] element configure $graph -label ""
}

proc GRAPH::label { graph text } {
## XXX got memory errors
##    [$graph canvas] element configure $graph -label $text
}

proc GRAPH::clear { graph } {
    set c [$graph canvas]
    $c element delete $graph
    $c element create $graph
}

proc GRAPH::postscript { graph } {

    global tkined_ps_map
    set tkined_ps_map(fixed) [list Courier 10]

    set c [$graph canvas]

    $c postscript -fontmap tkined_ps_map	    
}

##
##
##

proc DATA::canvas { data } {
    set s [STRIPCHART create]
    $s canvas [$data canvas]
    $data items $s
}

proc DATA::values { data args } {
    set values [join $args]
    foreach proc [info commands STREAM:*] {
	eval [list $proc $data $values]
    }
}
