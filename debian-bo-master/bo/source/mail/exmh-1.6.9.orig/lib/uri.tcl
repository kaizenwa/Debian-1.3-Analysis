# uri.tcl
#
# A little support for Uniform Resource Identifiers.
#
# martin hamilton <martin@mrrl.lut.ac.uk>
# &
# John Robert LoVerso <loverso@osf.org>
# Toivo Pedaste <toivo@ucs.uwa.edu.au>
# Fred Douglis <douglis@research.att.com>

proc URI_Init {} {
    global env uri

    if [info exists uri(init)] {
	return
    }

    set uri(init) 1

    Preferences_Add "WWW" \
      "These options control how exmh deals with Uniform Resource
Identifiers, e.g. World-Wide Web URLs.  You can arrange for URLs
embedded in messages to be turned into hyperlinks, and there is an
option to decipher the experimental X-URL (or X-URI) header.  This may be used
by the sender of a message to include contact information, such as
the address of their World-Wide Web homepage.  You can add this
header to your messages by editing compcomps, replcomps and so on." {
	{uri(scanForXURIs) uriScanForXURIs	ON {Scan for X-URL: headers}
"This tells exmh whether to look for X-URL (or X_URI) headers in messages when
you read them."}
	{uri(scanForURIs) uriScanForURIs	OFF {Scan for URLs in messages}
"This tells exmh whether to look for URL in the bodies of messages.
If you turn it on, any URLs it finds will be turned into buttons
which you can click on to launch a viewer application.  NB - this can
slow down message displaying somewhat."}
	{uri(scanLimit) uriScanLimit	1000 {Max lines to scan for URL}
"This limits the number of lines scanned for embedded URLs,
which can run slowly on large messages.  Set to a number of lines,
or to the keyword \"end\" to scan the whole message."}
	{uri(scanSoftLimit) uriScanSoftLimit	1000 {Stop button max lines}
"If the number of lines to scan is more than this soft limit, then
a stop button is displayed so you can terminate URL scanning before
it completes.."}
	{uri(viewer)	uriViewer {CHOICE Mosaic netscape webedit surfit other} {URL Viewer}
"The Mosaic and netscape options attempt to connect to a running
instance of these programs.  webedit and surfit are Tcl/Tk web browsers.
The other option lets you define your
own script to display the URL.  You need to include the $xuri
variable reference in your command to run."}
	{uri(viewHtml)	mimeShowHtml OFF {Show text/html immediately}
"If you want to automatically start the URL browser when you
get a text/html MIME part, choose this.  Otherwise you must
ask for the browser to be started for each text/html part."}
	{uri(mosaicApp) uriMosaicApp	{Mosaic} {Mosaic program name}
"This is the name of the binary program that corresponds to the
Mosaic viewer option.  For example, some sites use \"xmosaic\"."}
	{uri(netscapeFlags) uriNetscapeFlags	{} {Netscape flags}
"These are additional flags passed to netscape when it starts up.
The -install flag makes it use a private colormap."}
	{uri(viewerApp) uriViewerApp	{Mosaic $xuri} {Other URL Viewer}
"This is the command line used if you select the \"other\" browser option.
The variable \$xuri gets replaced with the URL."}
	{uri(logOnEnter) uriLogOnEnter	ON	{Show selected URL}
"With 'Show selected URL' enabled exmh will display
the coresponding URL if you move with the mouse on an
activated (looks like a button) X-Face or URL in the
message text.
NOTE: When you change the option you have to rescan your
      current message or read another one to de/active
      the option."}
    }
    if {$uri(viewer) == "xmosaic"} {
	set uri(viewer) Mosaic
    }
}

proc URI_StartViewer {xuri} {
    global uri

    regsub -nocase "URL:" $xuri {} xuri
    string trimright $xuri "."
    Exmh_Status "$uri(viewer) $xuri"

    if [regexp {^mailto:(.*)$} $xuri x address] {
	Sedit_Mailto $xuri
	return
    }
    if [catch {
	switch -- $uri(viewer) {
	    Mosaic	{ Mosaic_Load $xuri}
	    netscape {
		if [ catch { eval {exec netscape} $uri(netscapeFlags) {-remote openURL($xuri)} >& /dev/null } tmp ] {
		    if [catch {
		        eval {exec netscape} $uri(netscapeFlags) { $xuri & }
		        Exmh_Status "Starting netscape"
                    } err] {
			Exmh_Status "netscape: $err"
		    }
		}
	    }
	    surfit -
	    webedit {
		set interps [winfo interps]
		set ix [lsearch -glob $interps $uri(viewer)*]
		if {$ix >= 0} {
		    set interp [lindex $interps $ix]
		} else {
		    Exmh_Status "$uri(viewer) is not running"
		    return
		}
		if [catch {send -async $interp [list Url_DisplayNew $xuri]} err] { 
		    Exmh_Status $err
		} else {
		    Exmh_Status "Viewing URL with $uri(viewer)"
		}
	    }
	    other {eval exec $uri(viewerApp) &}
	}
    } err] {
	Exmh_Status $err
    }
}

proc URI_OpenSelection {} {
    if [catch {selection get} xuri] {
        return
    }
    URI_StartViewer $xuri
}

proc Hook_MsgShowParseUri {msgPath hmm} {
    global uri exwin mimeHdr

    foreach hdr {x-uri x-url} {
	if {[info exists mimeHdr(0=1,hdr,$hdr)] && $uri(scanForXURIs)} {
	    set temp_uri [MsgParseFrom $mimeHdr(0=1,hdr,$hdr) noaddr]
	}
    }
    if [info exists temp_uri] {
        regsub -all "\[ \t\n\]" $temp_uri {} temp_uri
	set but [Faces_Button [list URI_StartViewer $temp_uri]]
	global exmh
	$but config -bitmap @$exmh(library)/url.bitmap
        if $uri(logOnEnter) {
	    regsub -all % $temp_uri %% temp_uri
	    bind $but <Enter> [list Exmh_Status "X-URL:\t$temp_uri"]
	    bind $but <Leave> [list Exmh_Status "\t$temp_uri"]
	}
    } else {
	Uri_ClearCurrent
    }

    if !$uri(scanForURIs) {
        return
    }
    URI_ScanMsg $exwin(mtext) $uri(scanLimit)
}
proc Uri_ClearCurrent {} {
    Faces_ClearButton
}

proc Hook_MsgClipParseUri {msgPath t} {
    global uri exwin

    if !$uri(scanForURIs) {
        return
    }
    URI_ScanMsg $t $uri(scanLimit)
}

proc URI_ActiveText { w start end URI} {
    global uri
    # quote percents in URLs because they appear in binding commands
    regsub -all % $URI %% URI
    set id [TextButtonRange $w $start $end [list URI_StartViewer $URI]]
    if $uri(logOnEnter) {
	$w tag bind $id <Any-Enter> [list +Exmh_Status "X-URL:\t$URI"]
	$w tag bind $id <Any-Leave> [list +Exmh_Status "\t$URI"]
    }
    update idletasks
    return $id
}

proc URI_ScanMsg { {w {}} {limit end} } {
    global uri exwin
    if {$w == {}} {
	set w $exwin(mtext)
    }
    set x [lindex [$w config -cursor] 4]
    $w config -cursor watch

    set grab 0
    set uri(stop) 0
    scan [$w index end] %d lnum
    set limit [string trim $limit]
    if {$limit != "end"} {
	if {$limit > $lnum} {
	    set limit $lnum.0
	} else {
	    set limit $limit.0
	}
    }
    if {$lnum > $uri(scanSoftLimit) && ($uri(scanSoftLimit) < $limit)} {
	set g $w.ustop
	if [winfo exists $g] {
	     destroy $g
	}
	frame $g -bd 4 -relief raised
	set f [frame $g.pad -bd 20]
	set msg [Widget_Message $f msg -text "$lnum Lines to scan" -aspect 1000]
	Widget_AddBut $f stop STOP {set uri(stop) 1}  {top padx 2 pady 2}
	bind $f.stop <Any-Key> {set uri(stop) 1 ; Exmh_Status Stop warn}
	pack $f
	Widget_PlaceDialog $w $g
	tkwait visibility $f.stop
	focus $f.stop
	catch {grab $f.stop}
	set grab 1
    }
    Exmh_Debug "URI_ScanMsg $limit"
    set multiline 0
    set hit 0
#    set protocol (ftp|http|https|gopher|nntp|telnet|wais|file|prospero|finger|urn|mailto|news|solo|x500)
    set protocol {[A-Za-z_]+[-A-Za-z0-9_]*}

    for {set i 0} {[$w compare $i.0 < $limit]} {if {! $hit} {incr i}} {
	if {! $hit} {
	    set begin 0
	    set text [$w get $i.0 "$i.0 lineend"]
	} else {
	    # Look for more on the same line
	    set text [string range $text $begin end]
	}
	set hit 0

	if {$grab && $i && (($i % 20) == 0)} {
	    $msg config -text "Scanned $i of $lnum"
	    update
	}
	if {$uri(stop)} {
	    break
	}
	######
	# In this loop $i is the current line,
	# $text is the remaining part of the line
	# $begin is the offset of $text within the line
	#

        #######
        # match URIs continued from the previous line (begin is zero)
        if $multiline {
            set right [string first ">" $text]
            if {$right != -1} {
		Exmh_Debug Regexp0 right=$right begin=$begin
                set last $i.$right
                regsub -all "\n" [$w get $mstart $last] {} temp_uri

		URI_ActiveText $w $mstart $last $temp_uri
 
                set begin $right
		set text [string range $text $right end]
	        set multiline 0
		set hit 1
            }
	    # note: we will continue to look until a close is found
	    continue
        }

	# Each regexp clause must set:
	# hit to 1 if it matched
	# start to the index within the text line to begin highlight
	# end to end index within the text line to end highlight
	# temp_uri to the value of the URL.

	if {[regexp -indices "<$protocol://\[^>)\]+>" $text indices] == 1} {

	    # check for URIs like <protocol: > present
	    Exmh_Debug Regexp1 $indices
	    set start [expr [lindex $indices 0] + 1]
	    set end [expr [lindex $indices 1] -1]
	    set hit 1

	} elseif {[regexp -indices -nocase {<a href=([^>]+)>(.*)(</a>)?} \
		$text indices i1 i2] == 1} {

	    # match real HTML links
	    Exmh_Debug Regexp2 $indices $i1 $i2
	    set temp_uri [string trim [string range $text [lindex $i1 0] [lindex $i1 1]] {"}]
	    set text [string range $text [lindex $indices 1] end]

	    $w configure -state normal

	    $w delete $i.[expr $begin + [lindex $i2 1] + 1] \
		    $i.[expr $begin + [lindex $indices 1] + 1]
	    $w delete $i.[expr $begin + [lindex $indices 0]] \
		    $i.[expr $begin + [lindex $i2 0]]

	    $w configure -state disabled

	    set start [expr $begin + [lindex $indices 0]]
	    set end [expr $begin + [lindex $indices 0] + [lindex $i2 1] - [lindex $i2 0] + 1]
	    URI_ActiveText $w $i.$start $i.$end $temp_uri

            set begin $end
	    set hit 1

	    # Continue because we have set up begin and text properly
	    continue

        } elseif {[regexp -indices -nocase "<(urn|url|uri)\[: \]\[^>\]+>" $text indices] == 1} {
	    # match URIs wholly contained on one line
	    Exmh_Debug Regexp3 $indices
            set start [expr [lindex $indices 0] + 1]
            set end [expr [lindex $indices 1] - 1]
	    set hit 1

        } elseif {[regexp -indices "$protocol://\[^ \n\t\]+\[^ \n\t,\.\)>\'\"\]" \
		$text indices] == 1} {
	    # check for unencapsulated URIs by protocol if no < > present
	    Exmh_Debug Regexp4 $indices
            set start [lindex $indices 0]
            set end [lindex $indices 1]
	    set hit 1

	} elseif {[regexp -indices -nocase \
     "(urn|mailto|news|solo|x500):\[^ \n\t\)\]*\[^ \n\r\)\.\]" \
               $text indices] == 1} {
	    Exmh_Debug Regexp5 $indices
            set start [lindex $indices 0]
            set end [lindex $indices 1]
	    set hit 1

        } elseif {[regexp -indices -nocase "<(urn|url|uri)\[: \]" $text indices] == 1} {
	    # match the start of a URI which is broken over more than one line
	    # must include <URN or <URL
	    Exmh_Debug Regexp6 $indices
            set mstart $i.[expr [lindex $indices 0] + $begin + 1]
            set multiline 1
        }
        if {$hit} {
	    # Found a URL - handle the offset between $text and the text widget line
            set temp_uri [string range $text $start $end]
	    URI_ActiveText $w $i.[expr $begin+$start] $i.[expr $begin+$end] $temp_uri
	    set begin [expr $begin + $end]
	    set text [string range $text $end end]
	}
     }
    if {$grab} {
	catch {grab release $g.stop}
	Exmh_Focus
	destroy $g
    }

     $w config -cursor $x
 }
