# MkSample.tcl --
#
#	This file implements the "Sample" page in the widget demo
#
#	This file has not been properly documented. It is NOT intended
#	to be used as an introductory demo program about Tix
#	programming. For such demos, please see the files in the
#	demos/samples directory or go to the "Samples" page in the
#	"widget demo"
#
#
# Copyright (c) 1996, Expert Interface Technologies
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#
#

set tix_demo_running 1

# types: "d" directory "f" file
# conditions: "i" image "c" command "v" variable
set root {
    {d "File Selectors"		file	}
    {d "Hierachical ListBox"	hlist	}
    {d "Tabular ListBox"	tlist	{c tixTList}}
    {d "Grid Widget"		grid	{c tixGrid}}
    {d "Manager Widgets"	manager	}
    {d "Scrolled Widgets"	scroll	}
    {d "Miscellaneous Widgets"	misc	}
    {d "Image Types"		image	}
}

#
#    These are only alpha codes ...
#    {"Drag and Drop"		DragDrop.tcl}
#

set image {
    {d "Compound Image"		cmpimg	}
    {d "XPM Image"		xpm	{i pixmap}}
}

set cmpimg {
    {f "In Buttons"		CmpImg.tcl	}
    {f "In NoteBook"		CmpImg2.tcl	}
    {f "Icons"			CmpImg3.tcl	}
}

set xpm {
    {f "In Button"		Xpm.tcl		{i pixmap}}
    {f "In Menu"		Xpm1.tcl	{i pixmap}}
}

set file {
    {f DirList				DirList.tcl	}
    {f DirTree				DirTree.tcl	}
    {f ExFileSelectDialog		EFileDlg.tcl	}
    {f FileSelectDialog			FileDlg.tcl	}
    {f FileEntry			FileEnt.tcl	}
}

set hlist {
    {f HList			HList1.tcl	}
    {f CheckList		ChkList.tcl	{c tixCheckList}}
    {f "ScrolledHList (1)"	SHList.tcl	}
    {f "ScrolledHList (2)"	SHList2.tcl	}
    {f Tree			Tree.tcl	}
    {f "Tree (Dynamic)"		DynTree.tcl	{v win}}
}

set tlist {
    {f "ScrolledTList (1)"	STList1.tcl	{c tixTList}}
    {f "ScrolledTList (2)"	STList2.tcl	{c tixTList}}
    {f "TList File Viewer"	STList3.tcl	{c tixTList}}
}

set grid {
    {f "Simple Grid"		SGrid0.tcl	{c tixGrid}}
    {f "ScrolledGrid"		SGrid1.tcl	{c tixGrid}}
}

set scroll {
    {f ScrolledListBox		SListBox.tcl	}
    {f ScrolledText		SText.tcl	}
    {f ScrolledWindow		SWindow.tcl	}
}

set manager {
    {f ListNoteBook		ListNBK.tcl	}
    {f NoteBook			NoteBook.tcl	}
    {f PanedWindow		PanedWin.tcl	}
}

set misc {
    {f Balloon			Balloon.tcl	}
    {f ButtonBox		BtnBox.tcl	}
    {f ComboBox			ComboBox.tcl	}
    {f Control			Control.tcl	}
    {f LabelEntry		LabEntry.tcl	}
    {f LabelFrame		LabFrame.tcl	}
    {f Meter			Meter.tcl	{c tixMeter}}
    {f OptionMenu		OptMenu.tcl	}
    {f PopupMenu		PopMenu.tcl	}
    {f Select			Select.tcl	}
    {f StdButtonBox		StdBBox.tcl	}
}

proc MkSample {nb page} {
    global tixOption

    #----------------------------------------------------------------------
    set w [$nb subwidget $page]

    set pane [tixPanedWindow $w.pane -orient horizontal]
    pack $pane -expand yes -fill both

    set f1 [$pane add 1 -expand 1]
    set f2 [$pane add 2 -expand 3]
    $f1 config -relief flat
    $f2 config -relief flat

    # Left pane: the Tree:
    #
    set lab [label $f1.lab  -text "Select a sample program:" -anchor w]
    set tree [tixTree $f1.slb \
	-options {
	    hlist.selectMode single
	    hlist.width  40
	}]
    $tree config \
	-command   "Sample:Action $w $tree run" \
	-browsecmd "Sample:Action $w $tree browse"

    pack $lab -side top -fill x -padx 5 -pady 5
    pack $tree -side top -fill both -expand yes -padx 5

    # Right pane: the Text
    #
    set labe [tixLabelEntry $f2.lab -label "Source:" -options {
	label.anchor w
    }]

    $labe subwidget entry config -state disabled

    set stext [tixScrolledText $f2.stext]
    set f3 [frame $f2.f3]

    set run  [button $f3.run  -text "Run ..."  \
	-command "Sample:Action $w $tree run"]
    set view [button $f3.view -text "View Source in Browser ..." \
	-command "Sample:Action $w $tree view"]

    pack $run $view -side left -fill y -pady 2

    pack $labe -side top -fill x -padx 7 -pady 2
    pack $f3 -side bottom -fill x -padx 7
    pack $stext -side top -fill both -expand yes -padx 7

    #
    # Set up the text subwidget

    set text [$stext subwidget text]
    bind $text <1> "focus %W"
    bind $text <Up>    "%W yview scroll -1 unit"
    bind $text <Down>  "%W yview scroll 1 unit"
    bind $text <Left>  "%W xview scroll -1 unit"
    bind $text <Right> "%W xview scroll 1 unit"
    bind $text <Tab>   {focus [tk_focusNext %W]; break}

    bindtags $text "$text Text [winfo toplevel $text] all"

    $text config -bg [$tree subwidget hlist cget -bg] \
	-state disabled -font $tixOption(fixed_font) -wrap none

    $run  config -state disabled
    $view config -state disabled

    global demo
    set demo(w:run)  $run
    set demo(w:view) $view
    set demo(w:tree) $tree
    set demo(w:lab1) $labe
    set demo(w:stext) $stext

    set hlist [$tree subwidget hlist]
    $hlist config -separator "." -width 30 -drawbranch 0 \
	-wideselect false

    set style [tixDisplayStyle imagetext -refwindow $hlist \
	-fg #202060 -padx 4]

    uplevel #0 set TRANSPARENT_GIF_COLOR [$hlist cget -bg]

    set file   [tix getimage textfile]
    set folder [tix getimage openfold]

    AddSamples $tree $hlist root "" $style $file $folder

    global env sampleHlist
    uplevel #0 set sampleHlist $hlist 
    if [info exists env(TIX_DO_ALL_DEMOS)] {
	# This is undocumented feature. Don't do it.
	#
	after 200 DoAll $hlist
    }
}


proc DoAll {hlist {path ""}} {
    catch {
	set theSample [$hlist info data $path]
	if {$theSample != {}} {
	    set title [lindex $theSample 0]
	    set prog  [lindex $theSample 1]

	    RunProg $title $prog
	   update
	}
    }

    foreach p [$hlist info children $path] {
	DoAll $hlist $p
    }    
}

proc AddSamples {t h name ent style fileIm folderIm} {
    global $name win

    if {[tix platform] == "windows"} {
	set win 1
    }

    foreach line [set $name] {
	set type [lindex $line 0]
	set text [lindex $line 1]
	set dest [lindex $line 2]
	set cond [lindex $line 3]

	case [lindex $cond 0] {
	    c {
		set cmd [lindex $cond 1]
		if {[info command $cmd] != $cmd} {
		    if ![auto_load $cmd] {
			continue
		    }
		}
	    }
	    i {
		if {[lsearch [image types] [lindex $cond 1]] == -1} {
		    continue
		}
	    }
	    v {
		set doit 1
		foreach var [lrange $cond 1 end] {
		    if [uplevel #0 info exists [list $var]] {
			set doit 0
			break
		    }
		}
		if !$doit {
		    continue
		}
	    }
	}

	if {$type == "d"} {
	    set e [$h addchild $ent -itemtype imagetext -style $style \
		-image $folderIm -text $text]
	    
	    AddSamples $t $h $dest $e $style $fileIm $folderIm
	    $t setmode $e close
	    $t close $e
	} else {
	    set e [$h addchild $ent -itemtype imagetext \
		-image $fileIm -text $text -data [list $text $dest]]
	}
    }
}

set sample_filename {}

proc Sample:Action {w slb action args} {
    global samples demo_dir demo

    set hlist [$slb subwidget hlist]
    set ent [$hlist info anchor]

    if {$ent == ""} {
	$demo(w:run)  config -state disabled
	$demo(w:view) config -state disabled
	return
    }
    if {[$hlist info data $ent] == {}} {
	# This is just a comment
	$demo(w:run)  config -state disabled
	$demo(w:view) config -state disabled
	return
    } else {
	$demo(w:run)  config -state normal
	$demo(w:view) config -state normal
    }

    set theSample [$hlist info data $ent]
    set title [lindex $theSample 0]
    set prog  [lindex $theSample 1]

    set samples_dir [tixNSubFolder $demo_dir samples]

    case $action {
	"run" {
	    RunProg $title $prog
	}
	"view" {
	    LoadFile [list [tixNSubFolder $samples_dir $prog]]
	}
	"browse" {
	    # Bring up a short description of the sample program
	    # in the scrolled text about

	    set text [$demo(w:stext) subwidget text]
	    uplevel #0 set sample_filename [list [tixNSubFolder $samples_dir $prog]]
	    tixWidgetDoWhenIdle ReadFileWhenIdle $text

	    $demo(w:lab1) subwidget entry config -state normal
	    $demo(w:lab1) subwidget entry delete 0 end
	    $demo(w:lab1) subwidget entry insert end [tixNSubFolder $samples_dir $prog]
	    $demo(w:lab1) subwidget entry xview end
	    $demo(w:lab1) subwidget entry config -state disabled
	}
    }
}

proc RunProg {title prog} {
    global samples demo_dir demo

    set samples_dir [tixNSubFolder $demo_dir samples]
    set w .[lindex [split $prog .] 0]
    set w [string tolower $w]

    if [winfo exists $w] {
	wm deiconify $w
	raise $w
	return
    }

    uplevel #0 source [list [tixNSubFolder $samples_dir $prog]]

    toplevel $w 
    wm title $w $title
    RunSample $w
}


proc LoadFile {filename} {
    global tixOption

    set tmp $filename
    regsub -all . $filename _ tmp
    set w [string tolower .$tmp]

    if [winfo exists $w] {
	wm deiconify $w
	raise $w
	return
    }

    toplevel $w 
    wm title $w "Source View: $filename"

    button $w.b -text Close -command "destroy $w"
    set t [tixScrolledText $w.text]
    tixForm $w.b    -left 0 -bottom -0 -padx 4 -pady 4
    tixForm $w.text -left 0 -right -0 -top 0 -bottom $w.b

    $t subwidget text config -highlightcolor [$t cget -bg] -bd 2 \
	-bg [$t cget -bg] -font $tixOption(fixed_font) 
    if {$filename == {}} {
	return
    }

    set text [$w.text subwidget text]
    $text config -wrap none

    ReadFile $text $filename
}

proc ReadFileWhenIdle {text} {
    global sample_filename
    
    ReadFile $text $sample_filename
}

proc ReadFile {text filename} {
    set oldState [$text cget -state]
    $text config -state normal
    $text delete 0.0 end

	set fd [open $filename {RDONLY}]
	$text delete 1.0 end
    
	while {![eof $fd]} {
	    $text insert end [gets $fd]\n
	}
	close $fd

    $text see 1.0
    $text config -state $oldState
}
