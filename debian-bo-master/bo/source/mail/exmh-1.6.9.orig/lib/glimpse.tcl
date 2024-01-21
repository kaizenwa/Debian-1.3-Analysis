# $Header: /home/bwelch/src/exmh/lib/RCS/glimpse.tcl,v 1.11 1996/08/21 01:09:05 bwelch Exp $
#
# link glimpse to exmh to provide full text searching within and
# across folders faster but less thoroughly than pick
#
# Requirements: glimpse V2.0
#		tk3.6,  exmh-1.5.3 (or exmh-1.6alpha)
#		does not work with with tk4.0
#
# by Tom Phelps (phelps@cs.berkeley.edu)
# ** Note: Tom Phelps does no longer maintain the code		**
# ** Bugs: to ach@rosat.mpe-garching.mpg.de (Achim Bohnet)	**
# **       or exmh-workers@parc.xerox.com (EXMH mailing list)	**
#	
# 29-Dec-94, 2pm-7pm  &  30-Dec-94, 2:30pm-4pm
# 20-Feb	don't recursively index subfolders (Achim Bohnet)
# 22-Feb 	Preferences support and Glimpse_Index could be run in background (ach)
# 23-Feb	subfolder search mode added (ach)
#
# features:
#    glimpse indexing of folders
#    glimpse searching of folders, with hypertext links from match list to matches
#    selected glimpse options available from pulldown menu
#

#    Before searching files, you need to index them with the glimpseindex
#    button.  Click all to iterate over all folders (both searching and indexing).
#    From a list of matches, click on the blue identifier to jump directly
#    to that message and highlight for the first match of the pattern.
#    Read the glimpse(1) manual page for more information.

# Glimpse_Init has been moved to extrasInit.tcl

proc Glimpse_Startup {} {
# tmp: no need for $exwin when 'text' is replaced by 'Widget_Text'
    global glimpse exwin tk_version

    if ![info exists glimpse(init)] {
	Exmh_Status "Glimpse not initialized" error
	return
    }
    # set default variable values
    set glimpse(search) ""
    set glimpse(andscope) ""
    set glimpse(giall) 0

    if {$glimpse(maxErrors) == "none"} {
	set glimpse(maxerr) " "
    } else {
	set glimpse(maxerr) "-$glimpse(maxErrors)"
    }
    if $glimpse(caseSensitive) {
	set glimpse(caseSen) ""
    } else {
	set glimpse(caseSen) "-i"
    }
    if $glimpse(wholeWord) {
	set glimpse(word) "-w"
    } else {
	set glimpse(word) ""
    }
    set glimpse(searchrng) $glimpse(searchRange)
    set glimpse(maxhits)   $glimpse(maxHits)


    # Current prefs are set exit it glimpse window exits
    set w .glimpse
    if ![Exwin_Toplevel $w "Full Text Searching with Glimpse" Glimpse] {
	# Interface already initialized
	return
    }

    # build the gui
    wm minsize $w 200 200
    wm iconname $w "exmh Glimpse"

    # info, glimpseindex, dismiss
    set f $w.but
    pack $f.quit -side right -padx 2 -fill y	;# Tweak to match
    Widget_AddBut $f gui "UNindex" Glimpse_Unindex	{right padx 2 filly}
    Widget_AddBut $f gi "index" Glimpse_Index 		{right padx 2 filly}
    Widget_CheckBut $f all "All" glimpse(giall)		{right padx 2 filly}
    Widget_Label $f info {top fillx} -textvariable glimpse(info) -anchor w

    set glimpse(giall) 1

    # glimpse
    set f [Widget_Frame $w g Menubar {top fillx}]
    Widget_AddBut $f glimpse "Search" Glimpse_Search	{left padx 2 filly}
    set glimpse(searchButton) $f.glimpse
    $f.glimpse config -width 6
    set m [Widget_AddMenuB $f opts "Opts..." 		{left padx 2 filly}]
    Widget_CheckMenuItem $m "case sensitive" {} glimpse(caseSen) \
		     -onvalue "" -offvalue "-i"
    set m2 [Widget_CascadeMenuItem $m "max hits per folder" {} m2]
    foreach i {10 50 100 200 500 1000 2000 10000} {
	Widget_RadioMenuItem $m2 $i {} glimpse(maxhits)
    }
    set m2 [Widget_CascadeMenuItem $m "max errors in match" {} m3]
    Widget_RadioMenuItem $m2 none {} glimpse(maxerr) -value " "
    foreach i {1 2 3 4 5 6 7 8} {
	Widget_RadioMenuItem $m2 $i {} glimpse(maxerr) -value "-$i"
    }
# best match doesn't work very well (weird output)
    Widget_RadioMenuItem $m2 best {} glimpse(maxerr) -value "-B"

    Widget_CheckMenuItem $m "match whole word" {} glimpse(word) \
			    -onvalue "-w" -offvalue ""
# AND scope buggy?
    Widget_CheckMenuItem $m "AND scope whole file" {} glimpse(andscope) \
			    -onvalue "-W" -offvalue ""
    Widget_Entry $f e {left fillx expand padx 2}  -textvariable glimpse(search)
    Widget_BindEntryCmd $f.e <Key-Return> "$f.glimpse invoke"
    if {$glimpse(searchrng) != "all-in-one"} {
	Widget_RadioBut $f all "all" glimpse(searchrng)
	Widget_RadioBut $f sub "subtree" glimpse(searchrng)
	Widget_RadioBut $f cur "current" glimpse(searchrng)
    }

    # results list
    Widget_Frame $w results
    set t [Widget_Text $w.results 20 \
	    -relief raised -borderwidth 2]
    global tk_version
    # Set up tag for hyper link
    if {[tk colormodel .] == "color"} {
	# Colors as in Mosaic: blue3 and ?violetred3?
        Preferences_Resource glimpse(anchorColor) anchorColor blue
        Preferences_Resource glimpse(visitedAnchorColor) visitedAnchorColor "violet red"
	set glimpse(hyper) [list -underline 1 -foreground $glimpse(anchorColor)]
	set glimpse(hyperUsed) [list -foreground $glimpse(visitedAnchorColor)]
    } else {
	set fg [option get $t foreground Foreground]
	set bg [option get $t background Background]
	set glimpse(hyper) [list -foreground $bg -background $fg]
	set glimpse(hyperUsed) $glimpse(hyper)
    }
    if {$tk_version >= 4.0} {
	append glimpse(hyper) " -lmargin2 1i"	;# wrap indent
	append glimpse(hyperUsed) " -lmargin2 1i"	;# wrap indent
	$t tag configure indent -lmargin2 10m -lmargin1 5m
    }
    eval {$t tag configure hyper} $glimpse(hyper)
    eval {$t tag configure hyperUsed} $glimpse(hyperUsed)
    $t tag bind hyper <ButtonRelease-1> {
	Glimpse_Hyper [%W get "@%x,%y linestart" "@%x,%y lineend"]
	Glimpse_HyperUsed %W @%x,%y
    }
    $t tag bind hyperUsed <ButtonRelease-1> {
	Glimpse_Hyper [%W get "@%x,%y linestart" "@%x,%y lineend"]
    }
    $t tag bind hyper <Enter> {set glimpse(cursor) [lindex [%W config -cursor] 4] ; %W config -cursor tcross}
    $t tag bind hyperUsed <Enter> {set glimpse(cursor) [lindex [%W config -cursor] 4] ; %W config -cursor tcross}
    $t tag bind hyper <Leave> {%W config -cursor $glimpse(cursor)}
    $t tag bind hyperUsed <Leave> {%W config -cursor $glimpse(cursor)}

    bind $t <Destroy> {catch unset glimpse(results)}
    set glimpse(results) $t
}



proc Glimpse_Search {} {
    global glimpse mhProfile flist exmh env tk_version

    if [regexp -- "^\[ 	\]*\$" $glimpse(search)] {
	set glimpse(info) "Empty search string specified"
	if {$tk_version >= 4.0} { bell } else { catch {blt_bell} }
	return
    }
    set glimpse(stop) 0
   $glimpse(searchButton) config -text Stop -command Glimpse_Stop
   after 1 Glimpse_SearchInner
}
proc Glimpse_SearchInner {} {
    global glimpse mhProfile flist exmh env tk_version
    set t $glimpse(results)
    set zname1 {\((.+)\)$}
    set zname2 {"?([^"]+)"? <[^>]+>$}
    set zdate {([0-9]+.(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec).[0-9]+)}
    set me $env(USER)

    $t configure -state normal
    $t delete 1.0 end
    $t mark set insert 1.0
    $t configure -state disabled
    if {$glimpse(searchrng) == "all-in-one" } {
	set folders "."
    } elseif {$glimpse(searchrng) == "current"} {
	set folders $exmh(folder)
    } else {
	# all or subtree (checked inside loop)
	set folders $flist(allfolders)
    }

    set opts "-y -L $glimpse(maxhits) $glimpse(caseSen) $glimpse(maxerr) $glimpse(word) $glimpse(andscope)"
    set cnt 0
    foreach f $folders {
	if {$glimpse(searchrng) == "subtree"} {
	    if ![regexp -- $exmh(folder).* $f] { continue }
	}
	set glimpse(info) "Searching $f..."
	# Allow button hits
	update
	if {$glimpse(stop)} {
	    break
	}
	set dir $mhProfile(path)/$f
	set idir $mhProfile(path)/.glimpse/$f
	catch {unset gtmp}

	if {![file readable $idir/.glimpse_index]} {
	    GlimpseLog $t "$f: (no glimpse index)\n"
	    continue
	}

	set lastnum -1
	Exmh_Debug $glimpse(path)/glimpse $opts -H $idir $glimpse(search)
	catch {
	    eval exec $glimpse(path)/glimpse $opts -H $idir {$glimpse(search)}
	} result
	$t configure -state normal
	Exmh_Debug $result

	# collect all matches to MH messages
	# (two pass process in order to sort by message number)
	set mhresult {}
	foreach r [split $result "\n"] {
	    if [regexp -- {/[^ ]+/([0-9]+): (.*)} $r all num context] {
		if {[lsearch -exact $mhresult $num]==-1} {lappend mhresult $num}
		# this is a local array, so goes away after this proc is done
		lappend gtmp($num) $context
	    }
	}

	foreach r [lsort -integer $mhresult] {
	    set num $r; set context $gtmp($num)

	    if {$num!=$lastnum} {
		incr cnt
		set lastnum $num
		set file $dir/$num
		$t insert end "$f/$num"
		$t insert end " "
		$t tag add hyper "insert linestart" "insert lineend -1c"

		## grab some header fields
		if [file readable $file] {
		    set from ""; set to ""; set subject ""; set date ""

		    set fid [open $file]
		    while {[gets $fid line]!=-1} {
			if {$line==""} break
			if {[regexp -nocase "^(from|to|subject|date): *(.*)" $line all field match]} {
			    set [string tolower $field] $match
			}
		    }

		    ## post processing of fields

		    # From => To if me; chop e-mail
		    set pfx ""
		    if {[string match "*$me*" $from]} {set from $to; set pfx "to:"}
		    # blind carbon copy?
		    if {$from==""&&$to==""&&[gets $fid line]!=-1 && [string match "*Blind-Carbon-Copy*" $line]} {
			gets $fid; # gobble blank line
			while {[gets $fid line]!=-1 && $line!=""} {
			    if {[regexp {^To: (.*)} $line all match]} {
				set from $match
				break
			    }
			}
		    }
		    if {[regexp $zname1 $from all match]} {set from $match} \
		    elseif {[regexp $zname2 $from all match]} {set from $match}
		    set from "$pfx$from"

		    # Date => day-mon-year
		    if {[regexp -nocase $zdate $date all match]} {set date $match}

		    # Subject (no change)

		    set start [$t index insert]
		    $t insert end "$from / $date / $subject\n"
		    if {$tk_version >= 4.0} {
			$t tag add indent $start insert
		    }
		    catch {close $fid}
		}
	    }
	    foreach c $context {
		set start [$t index insert]
		$t insert end "$c\n"
		if {$tk_version >= 4.0} {
		    $t tag add indent $start insert
		}
	    }
	}
	$t configure -state disabled
	if {$tk_version >= 4.0} {
	    $t see end
	} else {
	    $t yview -pickplace end
	}
    }
    $glimpse(searchButton) config -text Search -command Glimpse_Search

    switch -- $cnt {
	0	{set match "no matches"}
	1	{set match "1 match"}
	default {set match "$cnt matches"}
    }
    set glimpse(info) "Searching completed, $match"
    $t yview 1.0
}
proc Glimpse_Stop {} {
    global glimpse
    set glimpse(stop) 1
}

proc Glimpse_HyperUsed {t idx} {
    set range [$t tag nextrange hyper "$idx linestart" "$idx lineend"]
    if {"x$range" == "x"} {
	return
    }
    eval $t tag remove hyper $range
    eval $t tag add hyperUsed $range
}

proc Glimpse_Hyper {hyper} {
    global glimpse exmh

    if {![regexp {([^ ]+)/([0-9]+)} $hyper all folder msg]} return

    # show message
    if {[string compare $folder $exmh(folder)] != 0} {
	Folder_Change $folder [list Msg_Change $msg]
    } else {
	Msg_Change $msg
    }

    # show (first) search string within message
    # (save old values of choice, entry?)
    regsub -all -- {;|,} $glimpse(search) "|" search

    Find_Setup
    global glimpse find
    set find(choice) "Msg"
    # find(entry) should use a textvariable
    $find(entry) delete 0 end; $find(entry) insert 0 $search
    Find_It
}

proc Glimpse_Index { } {
    global glimpse mhProfile flist exmh tk_version

    #Glimpse toplevel never created
    if [info exists glimpse(results)] {
	set t $glimpse(results)
	$t configure -state normal
	$t delete 1.0 end
	$t mark set insert 1.0
	$t configure -state disabled
    }
    set folders [expr $glimpse(giall)?"$flist(allfolders)":"$exmh(folder)"]

    # only update if out of date
    set i 0
    if ![file isdirectory $mhProfile(path)/.glimpse] {
	if [catch {exec mkdir $mhProfile(path)/.glimpse} err] {
	    set glimpse(info) $err
	    return
	}
    }
    # Put glimpse indexes into an alternate directory structure
    # so modifications to the glimpse files do not bother exmh
    foreach f $folders {
	set dir $mhProfile(path)/$f
	set idir $mhProfile(path)/.glimpse/$f
	if ![file exists $dir] {
	    # Folder gone
	    catch {exec rm -rf $idir}
	    continue
	}
	set gf $idir/.glimpse_filenames
	if [catch {MakeDir $idir} err] {
	    continue
	} elseif {![file writable $idir]} {
	    set rmsg "$f: $idir not writable"
	    continue
	}
	if ![file exists $gf] {
	    set index full
	    set glimpse(info) "Indexing $f..."
	} elseif {[file mtime $dir]>[file mtime $gf]} {
	    #set index update
	    # Incremental seems broken (infinite loop) and
	    # does not handle deleted files properly, so
	    set index full
	    set glimpse(info) "Updating index for $f..."
	} else {
	    # index still current
	    set glimpse(info) "Index for '$f' still current"
	    update idletasks
	    continue
	}
	update idletasks
	# exclude subdirectories (want no-recursive switch), but...
	# collect subdirectories of current directory
	set subdirs {}
	foreach sub $folders {
	    if [string match $f/* $sub] {lappend subdirs $sub}
	}

	# don't index deleted files (.*) or subdirectories
	set fid [open "$idir/.glimpse_exclude" "w"]
	puts $fid "*/.*"
	puts $fid "*/$mhProfile(delprefix)*"
	puts $fid "*/*~"
	foreach sub $subdirs { puts $fid "*/[file tail $sub]" }
	close $fid

	Exmh_Status "Indexing $f..."
	update idletask
	set r [Glimpse_Batch $i\
		    [list $glimpse(path)/glimpseindex -H $idir $dir]]
	incr i
	if ![file exists $idir/.glimpse_index] {
	    set rmsg "$f index failed"
	    continue
	}
	if ![file exists $idir/.glimpse_filenames] {
	    set rmsg "$f has no files"
	    continue
	}
	set rl [split $r "\n"]
	if {[set ix [lsearch -regexp $rl "^Size"]]!=-1} {
	    regexp -- {.*= ([0-9]+) B.* = ([0-9]+)$} \
		[lindex $rl $ix] all bytes files
	    set isize [expr \
		[file size $idir/.glimpse_index] + \
		[file size $idir/.glimpse_filenames]]
	    if {$bytes > 0} {
		set overhead [expr 100.0 * $isize / $bytes]
	    } else {
		set overhead 0
	    }
	    set rmsg [format \
		"%-20s %5d files, %8d bytes, %7d index bytes, %%%.1f" \
		$f $files $bytes $isize $overhead]
	} else {
	    set rmsg $r
	}

	# status message (check if glimpse window was already created)
	if [info exists glimpse(results)] {
	    GlimpseLog $t $rmsg\n
	}
    }

    set glimpse(info) "Indexing completed"
    if [info exists t] {
	if {$tk_version >= 4.0} {
	    $t see 1.0
	} else {
	    $t yview 1.0
	}
    }
}
proc MakeDir { path } {
    if [file isdirectory $path] {
	return
    } else {
	if ![file isdirectory [file dirname $path]] {
	    MakeDir [file dirname $path]
	}
	exec mkdir $path
    }
}

proc Glimpse_Batch { i cmd } {
    global glimpse wish exmh

    if !$exmh(bgAsync) {
        # For UNSECURE X-servers
	catch {eval exec $cmd} out
	Glimpse_BatchDone "$out"
	set glimpse(done) $out
	update idletask
    } else {
        # For working 'send' (SECURE X-servers)
	set path [Env_Tmp]/glimpse.[pid].$i
	if [catch {open $path w} out] {
	Exmh_Status "$path $out"
	return
	}
	puts $out "wm withdraw ."
	set myname [winfo name .]
	puts $out "catch \{exec $cmd\} out"
	puts $out "catch \{send [list $myname] \[list Glimpse_BatchDone \$out\]\}"
	puts $out "exec rm $path"
	puts $out exit
	close $out
	set glimpse(done) {}
	exec $wish -f $path &
	tkwait var glimpse(done)
    }
    return $glimpse(done)
}

proc Glimpse_BatchDone { out } {
    global glimpse
    Exmh_Status "Glimpse_BatchDone $out"
    set glimpse(done) $out
}


proc Glimpse_Unindex {} {
    global glimpse mhProfile flist exmh tk_version

    set t $glimpse(results)
    $t configure -state normal
    $t delete 1.0 end
    $t mark set insert 1.0
    $t configure -state disabled
    set folders [expr $glimpse(giall)?"$flist(allfolders)":"$exmh(folder)"]

    foreach f $folders {
	set glimpse(info) "Unindex $f..."
	update idletasks
	set rmsg [Glimpse_Delete $f]
	GlimpseLog $t $rmsg\n
    }

    set glimpse(info) "Unindexing completed"
    if {$tk_version >= 4.0} {
	$t see 1.0
    } else {
	$t yview 1.0
    }
}
proc Glimpse_Delete { f } {
    global glimpse mhProfile
    set idir $mhProfile(path)/.glimpse/$f
    set gf $idir/.glimpse_filenames
    set rmsg "$f: "

    if {![file writable $idir]} {
	append rmsg "$idir not writable"
    } elseif {![file exists $gf]} {
	append rmsg "(no glimpse index files)"
    } else {
	if {[catch {eval exec rm [glob "$idir/.glimpse_*"]} info]} {
	    append rmsg "$info"
	} else {
	    append rmsg "glimpse index files deleted"
	    catch {exec rmdir $idir}
	}
    }
    return $rmsg
}
proc GlimpseLog {t string} {
    global tk_version
    $t config -state normal
    $t insert end $string
    if {$tk_version >= 4.0} {
	$t see end
    } else {
	$t yview -pickplace end
    }
    $t config -state disabled
    update idletasks
}
