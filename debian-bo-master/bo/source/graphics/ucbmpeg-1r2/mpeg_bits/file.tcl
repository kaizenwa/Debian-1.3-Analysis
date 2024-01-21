###PROCEDURES WHICH DEAL WITH THE FILE MENU---START######
proc loadQueryStart {w} {

  global var continueCommand

  #continueCommand is used if we have to pop up a sequence of windows, eg. \
      if need to prompt user for saving a query, then for loading it \
      my structure would forget about the second command.  therefore, set \
      continueCommand to be the procedure to be invoked and if it has a \
      command in it, fs will pick it up after ok is clicked on.
  
  set continueCommand "loadQueryStart $w"
  set yes_proc "saveQueryStart $w"
  set no_proc "fs {loadQueryEnd $w} {Load Query:}"
  set cancel_proc "return"

  #Can specify procedures for each button in a dialog box.  \
      Don't pop a dialog up unless query has changed 

  if {$var({$w}changed) == "true"} {
    dialogBox question.ppm "Save changes before opening\n a different canvas?" \
	"Save ?" yes_no_cancel $yes_proc $no_proc $cancel_proc
  } \
  else \
  {
    fs "loadQueryEnd $w" "Load Query:"
  }
}
#-----------------------------------------------------------
proc loadQueryEnd {w selected} {
  #Called from the file selector box with a file, but there is no 
  #guarantee that the file exists

  global fileName var

  #filter selected and such

    if {[file isfile $selected] != 0} {
      set fd [readQuery $w $selected]
      if {$fd != -1} {

	#would get a -1 if it was not a vod file

        close $fd
        set fileName($w) $selected
	wm title $w "VODS Query[string trim $w .query] - $selected"
        set var({$w}changed) false
      } \
      else \
      {
        dialogBox "Not A Berkeley VOD Query!" ok "" "" ""
      }  
    } \
    else \
    {
       dialogBox "File does not exist!" ok "" "" ""
    }
}
#-----------------------------------------------------------
proc saveQueryStart {w} {

  global fileName continueCommand

  set continueCommand ""
  set okCommand "saveQueryEnd $w"
  set num [string trim $w .query]


  if {$fileName($w) == ""} {
    fs "$okCommand" "Save Query $num:"
    #use fileselector box, if no name exists for this query yet. \
	second argument to savequeryend provided from fileselector
  } \
  else \
  {
    saveQueryEnd $w $fileName($w)
  }
}
#-----------------------------------------------------------
proc saveQueryEnd {w selected} {

  global fileName continueCommand var

  #filter selected for spaces and crap
  #overWrite $selected? if okay, continue, else return
  
  set fd [writeQuery $w $selected]
  close $fd
  set fileName($w) $selected
  wm title $w "VODS Query[string trim $w .query] - $selected"
  set var({$w}changed) false
  eval $continueCommand
  set continueCommand ""
}
#-----------------------------------------------------------
proc saveAsQueryStart {w} {

  fs "saveQueryEnd $w" "Save As File:"
}
#-----------------------------------------------------------
proc writeQuery {w name} {

  global NatLangList

  set fd [open $name w]
  puts $fd [composeQuery $w]
  return $fd
}
#-----------------------------------------------------------
proc readQuery {w name} {
    
  global NatLangList qstate ver var
  
  #clear this query entirely

  set fd [open $name r]
  gets $fd data
  set extras [lindex $data 0]
  set verify [lindex $extras 0]
  if {$verify == "Vodasaurus $ver"} {
    set var({$w}granularity) [lindex $extras 1]
    changeGranularity $w $var({$w}granularity)
    set var({$w}mpeg_one) [lindex $extras 2]
    set var({$w}mpeg_two) [lindex $extras 3]
    set var({$w}jpeg) [lindex $extras 4]
    set fileData [lindex $data 1]
    putDataInWindow $w $fileData
  } \
      else \
  {
    close $fd
    set fd -1
  }       
  return $fd
}
#-----------------------------------------------------------
###PROCEDURES WHICH DEAL WITH THE FILE MENU---END######

proc killWindow {w} {

  global winList NatLangList
  
  set num [string trim $w .query]
  set index [lsearch $winList $num]
  set winList [lreplace $winList $index $index]
  destroy $w
  unset NatLangList($w)
  if {[llength $winList] == 0} {destroy .}
}
  
  
proc quitQuery {w} {

  global var winList continueCommand

  set yes_proc "saveQueryStart $w"
  set no_proc "killWindow $w"
  set cancel_proc "return"
  set num [string trim $w .query]
  set where [lsearch $winList $num]
  set continueCommand "killWindow $w"
  
  if {$var({$w}changed) == "true"} {
    dialogBox question.ppm "Save changes before quitting?" "Save ?" yes_no_cancel "$yes_proc" $no_proc $cancel_proc
    update
  } \
      else \
      {
	killWindow $w
      }
}

proc exitProgram {} {

  global var winList continueCommand

  if {[llength $winList] != 0} {
    set index [lindex $winList 0]
    set w .query$index
    
    if {$var({$w}changed) == "true"} {
      set yes_proc "saveQueryStart $w; set continueCommand \"killWindow $w;exitProgram\""
      #Why such an ugly hack. Well if tcl had a way to control windows in a linear fashion \
	  then I'd be able to say put this window up and wait in procedures associated with \
	  that window.  But, no.  I have to put a window up, return from the proc that \
	  created the window and then deal with life.  So, in SaveQueryEnd, I reset \
	  continueCommand, which destroys the ability to keep going through the window list \
	  therefore, I must reset it here when the yes is activated from the dialog box \
	  ecch!
      
      set no_proc "killWindow $w;exitProgram"
      set cancel_proc "return"      
      set continueCommand "killWindow $w;exitProgram"
      dialogBox question.ppm "Save changes before quitting?"\
	  "Save Changes to Query $index ?" yes_no_cancel\
	  "$yes_proc" $no_proc $cancel_proc
    } \
	else \
	{
	  killWindow $w;exitProgram
	}
  }
}

  

proc notYet {} {
  dialogBox exclamation.ppm \
      {Feature not implemented yet} {Preferences} ok {} {} {}
}

proc clearQueryDialog {w} {

  set yes_proc "clearQuery $w"
  set no_proc "return"
  set cancel_proc "return"
  
  dialogBox question.ppm "Clear current query settings?"\
      "Clear ?" yes_no_cancel $yes_proc $no_proc $cancel_proc

}

proc clearQuery {w} {

  global NatLangList qstate

  set data $NatLangList($w)
  set length [llength $data]
  $w.query.list delete 0  end 
  for {set i 0} {$i < $length} {incr i} {
      set temp [lindex $data $i]
      set menu [lindex $temp $qstate(menu)]
      set menu_index [lindex $temp $qstate(menu_index)]
      set attrib_eng [lindex $temp $qstate(attrib_eng)]
      set all_value [lindex $temp $qstate(all_value)]
      set theMenu "$w.$menu"
      $theMenu entryconfigure $menu_index -label "$attrib_eng = $all_value"
  }
  set NatLangList($w) ""
}


proc duplicateQuery {w} {

  global NatLangList 

  set num [windowNumber]
  newQuery $num
  set data $NatLangList($w)
  set w ".query$num"  
  putDataInWindow $w $data
}

proc putDataInWindow {w data} {

  global qstate NatLangList
  
  set length [llength $data]
  for {set i 0} {$i < $length} {incr i} {
    set temp [lindex $data $i]
    set menu [lindex $temp $qstate(menu)]
    set menu_index [lindex $temp $qstate(menu_index)]
    set attrib_eng [lindex $temp $qstate(attrib_eng)]
    set attrib_op [lindex $temp $qstate(attrib_op)]
    set attrib_val [lindex $temp $qstate(attrib_val)]
    set attrib_op_eng [lindex $temp $qstate(attrib_op_eng)]
    set all_value [lindex $temp $qstate(all_value)]
    set theMenu "$w.$menu"
    #pathname for reconfigured menu
    
    #just hard-coded this, fix it
    $w.query.list insert end "[string trim $attrib_eng "  "] $attrib_op_eng $attrib_val"
    set NatLangList($w) [linsert $NatLangList($w) \
				    [llength $NatLangList($w)] $temp]
    
    $theMenu entryconfigure $menu_index -label "$attrib_eng $attrib_op $attrib_val"
    #menu
  }
}
