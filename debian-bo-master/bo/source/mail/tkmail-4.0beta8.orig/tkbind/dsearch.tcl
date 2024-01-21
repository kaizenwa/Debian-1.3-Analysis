# DSearch Package for tkTextEnhanced --
#
#
#  Copyright 1995 by Paul Raines (raines@slac.stanford.edu)
#
#  Permission to use, copy, modify, and distribute this software and
#  its documentation for any purpose and without fee is hereby
#  granted, provided that the above copyright notice appear in all
#  copies.  The University of Pennsylvania, Stanford University, and
#  Stanford Linear Accelerator Center makes no representations
#  about the suitability of this software for any purpose.  It is
#  provided "as is" without express or implied warranty.


global tkText tkBind

if {![info exists tkBind(dsearch,bind)] || $tkBind(dsearch,bind)} {

  bind Text <Control-s> {
    tkTextDSearchStart %W -forwards -exact
  }

  bind Text <Control-r> {
    tkTextDSearchStart %W -backwards -exact
  }
}

proc tkTextDSearchStart {w dir mode} {
  global tkText tkBind

  set tkText($w,markActive) 0
  set tkText($w,arg) {}
  $w tag remove sel 0.0 end

  if {![info exists tkText($w,dsearchMode)]} {
    lappend tkText($w,destroyHooks) tkTextDSearchClean
  }

  set tkText($w,dsearchDir) $dir
  set tkText($w,dsearchMode) $mode
  if $tkBind(noCase) { 
    set tkText($w,dsearchCase) "-nocase" 
  } else { set tkText($w,dsearchCase) {} }
  set tkText($w,dsearchHome) [$w index insert]
  set tkText($w,dsearchLast) {}
  tkBindSetMesg $w {}

  if {![info exists tkText($w,dsearchDlg)] ||
      ![winfo exists $tkText($w,dsearchDlg)]} {
    for {set cnt 0} {[winfo exists .dsearch$cnt]} {incr cnt} {}
    set dlg .dsearch$cnt
    toplevel $dlg -class TextSearch
    set tkText($w,dsearchDlg) $dlg

    wm title $dlg "Text Search Dialog"
    wm minsize $dlg 220 80
    wm protocol $dlg WM_DELETE_WINDOW "tkTextDSearchStop $w"

    frame $dlg.str
    label $dlg.str.lbl -text "Search for:" -width 15 -anchor e
    entry $dlg.str.ent -relief sunken
    pack $dlg.str.lbl -side left -pady 10 
    pack $dlg.str.ent -side left -expand true -fill x -padx 10 -pady 10
    bind $dlg.str.ent <Return> "$dlg.bb.search invoke; focus %W"

    frame $dlg.repl
    label $dlg.repl.lbl -text "Replace with:" -width 15 -anchor e
    entry $dlg.repl.ent -relief sunken
    pack $dlg.repl.lbl -side left -pady 10
    pack $dlg.repl.ent -side left -expand true -fill x -padx 10 -pady 10
    bind $dlg.repl.ent <Return> "$dlg.bb.replace invoke; focus %W"

    frame $dlg.mod
    checkbutton $dlg.mod.case -text "Case insensitive" \
	-variable tkText($w,dsearchCase) -width 20 -relief flat \
	-offvalue {} -onvalue {-nocase}
    checkbutton $dlg.mod.regexp -text "Regular expression" \
	-variable tkText($w,dsearchMode) -width 20 -relief flat \
	-offvalue {-exact} -onvalue {-regexp}
    checkbutton $dlg.mod.back -text "Backward" \
	-variable tkText($w,dsearchDir) -width 20 -relief flat \
	-offvalue {-forwards} -onvalue {-backwards}
    pack $dlg.mod.case $dlg.mod.regexp $dlg.mod.back \
	-side left -pady 10 -padx 10 -fill x -expand true

    frame $dlg.bb
    button $dlg.bb.search -text "Search" -width 10 \
	-command "tkTextDSearchNext $w \[$dlg.str.ent get\] insert"
    button $dlg.bb.replace -text "Replace" -width 10 \
	-command "tkTextDSearchReplace $w"
    button $dlg.bb.repl_all -text "Replace All" -width 10 \
	-command "while {\[tkTextDSearchReplace $w\]} { }"
    button $dlg.bb.home -text "Home" -width 10 \
	-command "tkTextDSearchHome $w"
    button $dlg.bb.dismiss -text "Dismiss" -width 10 \
	-command "tkTextDSearchStop $w"

    pack $dlg.bb.search $dlg.bb.replace $dlg.bb.repl_all \
	$dlg.bb.home $dlg.bb.dismiss -side left -padx 10 -pady 10
    pack $dlg.str $dlg.repl $dlg.mod $dlg.bb -side top -expand true -fill x
  } else {
    set dlg $tkText($w,dsearchDlg)
    wm deiconify $dlg
  }

  $dlg.repl.ent configure -state [$w cget -state]
  $dlg.bb.replace configure -state [$w cget -state]
  $dlg.bb.repl_all configure -state [$w cget -state]

  raise $dlg 
  focus $dlg.str.ent
  return $dlg
}

proc tkTextDSearchStop {w {setmark 1}} {
  global tkText tkBind

  wm withdraw $tkText($w,dsearchDlg)
  if {[winfo exists $w]} {
    if $setmark { 
      tkTextSetMark $w $tkText($w,dsearchHome) 
      set tkText($w,markActive) 0
    }

    focus $w
  }
}

proc tkTextDSearchClean w {
  global tkText tkBind

  if {![info exists tkText($w,dsearchDlg)]} return
  if {[winfo exists $tkText($w,dsearchDlg)]} { 
      catch "destroy $tkText($w,dsearchDlg)"
  }
  foreach elem [list dsearchDir dsearchMode dsearchCase dsearchHome \
		    dsearchDlg dsearchLast] {
    unset tkText($w,$elem)
  }
}

proc tkTextDSearchAgain {w} {
  global tkText
  if {![info exists tkText($w,dsearchDlg)]} {
    error "No previous search for this widget"
  }
  return [tkTextDSearchNext $w [$tkText($w,dsearchDlg).str.ent get] insert]
}

proc tkTextDSearchNext {w str start} {
  global tkText tkBind

  set tkText($w,markActive) 0
  set tkText($w,arg) {}
  set tkText($w,prevCmd) DSearch
  catch "$w tag remove sel 0.0 end"

  set start [$w index $start]
  if {$tkText($w,dsearchDir) == "-forwards"} {
    set stop end } else { set stop 1.0 }

  set ndx [$w search $tkText($w,dsearchDir) $tkText($w,dsearchMode) \
	   $tkText($w,dsearchCase) -count ccnt -- $str $start $stop]
  if [string length $ndx] {
    if {$tkText($w,dsearchDir) == "-forwards"} {
      $w mark set insert [$w index "$ndx + $ccnt c" ]
    } else {
      $w mark set insert $ndx
    }    
    $w tag add sel $ndx "$ndx + $ccnt c"
    $w see insert
    set tkText($w,dsearchLast) [list $ndx [$w index sel.last]]
    return 1
  } else {
    eval $tkBind(bell)
    return 0
  }
}

proc tkTextDSearchHome w {
  global tkText tkBind

  set tkText($w,markActive) 0
  set tkText($w,arg) {}
  set tkText($w,prevCmd) DSearchHome
  $w tag remove sel 0.0 end

  $w mark set insert $tkText($w,dsearchHome)
  $w see insert
}

proc tkTextDSearchReplace w {
  global tkText tkBind

  set dlg $tkText($w,dsearchDlg)
  if {![catch "$w index sel.first"]} {
    set cur [list [$w index sel.first] [$w index sel.last]]
    if {$tkText($w,dsearchLast) == $cur} {
      tkTextReplace $w sel.first sel.last [$dlg.repl.ent get]
    }
  }
  return [tkTextDSearchNext $w [$dlg.str.ent get] insert]
}
