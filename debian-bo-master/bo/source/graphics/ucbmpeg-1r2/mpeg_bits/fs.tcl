##LEFT TO DO IS KEY BY KEY SELECT AND USE ARROWS
##still a bit messy, clean up in spare time
#---------------------------------------------
set bitmapDirectory /n/picasso/users/dvberger/vodasaurus/bitmaps

proc fillBoxes {directory mask} {

  global fileselect lastDir

  ## Fill the listboxes with files and with directories \
      using /bin/ls

  $fileselect(file_list) delete 0 end
  $fileselect(dir_list) delete 0 end

  cd $directory
  set lastDir [pwd]
  $fileselect(dir_label) configure -text $lastDir
 
  foreach i [exec /bin/ls -aF $lastDir] {
    if {[string compare $i "."] != 0 && \
	    [string compare $i ".."] != 0 && \
	    [string match */ $i] == 0} then \
	{ 
	  if {[string match $mask $i] == 1} {
	    $fileselect(file_list) insert end $i}
	} else \
	{
	 $fileselect(dir_list) insert end $i
	}
  }
}
#---------------------------------------------
proc fs {
    {cmd fileselect.default.cmd} 
    {purpose "File:"} 
    {w .fileSelectWindow} 
    {errorHandler fileselect.default.errorHandler}
} {
    
    global fileselect lastDir bitmapDirectory
    set lastDir ""
    
    set fileselect(entry) $w.fframe.entry
    set fileselect(file_list) $w.fframe.list
    set fileselect(file_yscroll) $w.fframe.yscroll
    #    set fileselect(filter) $w.fframe.filter
    #    set fileselect(buttonFilter) $w.fframe.buttonFilter
    
    set fileselect(dir_label) $w.dframe.label1
    set fileselect(dir_list) $w.dframe.list
    set fileselect(dir_yscroll) $w.dframe.yscroll
    #    set fileselect(drive) $w.fframe.filter
    #    set fileselect(filter) $w.fframe.buttonFilter
    
    set fileselect(ok) $w.bframe.ok
    set fileselect(cancel) $w.bframe.cancel
    
    
    catch {destroy $w}
    toplevel $w
    wm title $w $purpose
    
    ##FRAME FOR FILES
    
    frame $w.fframe -borderwidth 10
    label $w.fframe.label -anchor w -text "File Name:"
    label $w.fframe.spacer -text "       "
    entry $w.fframe.entry -relief sunken 
    
    scrollbar $w.fframe.yscroll -relief sunken \
	-command "$w.fframe.list yview"
    listbox $w.fframe.list -relief sunken \
	-yscroll "$w.fframe.yscroll set"
    
    frame $w.fframe.fframe
    label $w.fframe.fframe.label -text "List Files of Type:"
    label $w.fframe.fframe.filter -text\
	"Berkeley Vods Queries (*.vod)" -relief sunken
    
    menubutton $w.fframe.fframe.button -menu \
	$w.fframe.fframe.button.m -bitmap \
	@$bitmapDirectory/arrow.xbm -relief raised 
    
    menu $w.fframe.fframe.button.m
    $w.fframe.fframe.button.m add command -label \
	"Berkeley Vods Queries (*.vod)" -command \
	"fillBoxes . *.vod; $w.fframe.fframe.filter configure -text \
        \"Berkeley Vods Queries (*.vod)\""
    $w.fframe.fframe.button.m add command -label \
	"All Files (*.*)" -command \
	"fillBoxes . *.*; $w.fframe.fframe.filter configure -text \
        \"All Files (*.*)\""
    
    ##DIRECTORY
    frame $w.dframe -borderwidth 10 
    label $w.dframe.label -anchor w -text "Directories:"
    label $w.dframe.label1 -anchor w -text [pwd] 
    
    scrollbar $w.dframe.yscroll -relief sunken \
	-command "$w.dframe.list yview"
    listbox $w.dframe.list -relief sunken \
	-yscroll "$w.dframe.yscroll set" 
    
    set lastDir [pwd]
    fillBoxes "[pwd]" "*.vod"
    
    frame $w.dframe.dframe
    label $w.dframe.dframe.label -text "Drives:"
    label $w.dframe.dframe.filter -text \
	"c:/" -relief sunken
    
    menubutton $w.dframe.dframe.button -menu \
	$w.dframe.dframe.button.m -bitmap \
	@$bitmapDirectory/arrow.xbm -relief raised 
    
    menu $w.dframe.dframe.button.m
    $w.dframe.dframe.button.m add command -label \
	"c:/" -command \
	"$w.dframe.dframe.filter configure -text \
        \"c:/ \""
    $w.dframe.dframe.button.m add command -label \
	"d:/" -command \
	"$w.dframe.dframe.filter configure -text \
        \"d:/ \""
    
    ##BUTTONS
    frame $w.bframe -borderwidth 10 
    button $w.bframe.ok -text "OK" -relief raised -padx 10 -pady 10 \
	-command "fileselect.ok.cmd $w {$cmd} $errorHandler" \
	-width 4 
    label $w.bframe.label -text ""
    button $w.bframe.cancel -text Cancel -relief raised -padx 10 -pady 10\
	-command "fileselect.cancel.cmd $w" \
	-width 4 
    
    ## BINDINGS
    bind $fileselect(entry) <Return> {eval $fileselect(ok) invoke}
    bind $fileselect(entry) <Control-c> {eval $fileselect(cancel) invoke}
    
    bind $w <Control-c> {eval $fileselect(cancel) invoke}
    bind $w <Return> {eval $fileselect(ok) invoke}
    
    tk_listboxSingleSelect $fileselect(file_list)
    tk_listboxSingleSelect $fileselect(dir_list)
    
    ##FOR FILE BOX
    
    bind $fileselect(file_list) <Button-1> {
	# puts stderr "button 1 release"
	%W select from [%W nearest %y]
	$fileselect(entry) delete 0 end
	$fileselect(entry) insert 0 [%W get [%W nearest %y]]
    }
    
    bind $fileselect(file_list) <Double-ButtonPress-1> {
	# puts stderr "double button 1"
	%W select from [%W nearest %y]
	$fileselect(entry) delete 0 end
	$fileselect(entry) insert 0 [%W get [%W nearest %y]]
	$fileselect(ok) invoke
    }
    
    bind $fileselect(file_list) <Return> {
	%W select from [%W nearest %y]
	$fileselect(entry) delete 0 end
	$fileselect(entry) insert 0 [%W get [%W nearest %y]]
	$fileselect(ok) invoke
    }
    
    ##FOR DIRECTORY BOX
    
    bind $fileselect(dir_list) <Double-ButtonPress-1> {
	# puts stderr "double button 1"
	%W select from [%W nearest %y]
	fillBoxes "[pwd]/[%W get [%W nearest %y]]" "*"
    }
    
    bind $fileselect(dir_list) <Return> {
	%W select from [%W nearest %y]
	$fileselect(entry) delete 0 end
	$fileselect(entry) insert 0 [%W get [%W nearest %y]]
	$fileselect(ok) invoke
    }
    
    # set kbd focus to entry widget
    
    focus $fileselect(entry)
    
    #PACKING THE WINDOW
    pack $w.fframe $w.dframe -fill both -expand yes -side left
    pack $w.bframe -side left -fill both
    
    pack $w.fframe.label -anchor w 
    pack $w.fframe.entry $w.fframe.spacer -fill x
    pack $w.fframe.fframe -side bottom -anchor w
    pack $w.fframe.fframe.label -anchor w
    pack $w.fframe.fframe.button $w.fframe.fframe.filter -side left -anchor w
    pack $w.fframe.list -side left -fill both -expand yes
    pack $w.fframe.yscroll -side left -fill y
    
    pack $w.dframe.label -anchor w 
    pack $w.dframe.label1 -anchor w
    pack $w.dframe.dframe -side bottom -anchor w
    pack $w.dframe.dframe.label -anchor w
    pack $w.dframe.dframe.button $w.dframe.dframe.filter -side left -anchor w
    pack $w.dframe.list -side left -fill both -expand yes
    pack $w.dframe.yscroll -side right -fill y 
    
    pack $w.bframe.ok $w.bframe.label $w.bframe.cancel 
}

#---------------------------------------------
proc fileselect.default.cmd {f} {
  return $f
#  puts stderr "selected file $f"
  ##add action to file here
}
#---------------------------------------------
proc fileselect.default.errorHandler {errorMessage} {
    puts stdout "error: $errorMessage"
    catch { cd ~ }
}
#---------------------------------------------
proc fileselect.cancel.cmd {w} {
    # puts stderr "Cancel"
    destroy $w
}
#--------------------------------------------------
proc fileselect.ok.cmd {w cmd errorHandler} {

  global fileselect 

  set mask "*"
  set selected [$fileselect(entry) get]
  #some nasty file names may cause "file isdirectory" to return an error
  set sts [catch {set isDir [file isdirectory $selected]} errorMessage ]

  #if sts is 1 then there was an error in the filename
  if {$sts != 0} then {
    $errorHandler $errorMessage
    destroy $w
    return
  }

  # clean the text entry and prepare the list
  $fileselect(entry) delete 0 end
  $fileselect(file_list) delete 0 end

  if {$isDir == 1} {
    fillBoxes $selected $mask      
    return
  } \
      else \
      {
	set dirName [file dirname $selected]
	set tail [file tail $selected]

	#put in error is use ? [] \ + etc

	if {[file isdirectory $dirName] == 1} {
	  if {[regexp {\*} $tail] == 1} {
	    fillBoxes $dirName $tail
	  } \
	      else \
	      {
		destroy $w
		if {$selected != ""} {
		  eval $cmd {$selected}}
	      }
	} \
	    else \
	    {
	      #        puts invalid_directory
	    } 
	#the error for invalid directory goes here      
      }
}
#---------------------------------------------
proc setName {selected} {

global fileName

  set fileName $selected
}
#---------------------------------------------
