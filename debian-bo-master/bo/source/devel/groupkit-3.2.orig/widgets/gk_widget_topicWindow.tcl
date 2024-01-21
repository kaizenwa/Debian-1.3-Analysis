#
#  gk_topicWindow - create and manipulate help information
# 
#       See the manual pages for more information.
#


proc gk_topicWindow {w args} {
    upvar #0 $w gk_topic
 
   # Creates a new widget of class "gkTopicWindow".  It relies on the
   # groupkit class builder to create the class and the widget so for
   # more information on this, guess where you should look :-).

    if { [winfo exist $w] } { 
	 eval gkTopicWindow_ConstructWidget $w
         eval $w config $args
    } else {
  	  eval gkInt_CreateWidget $w gkTopicWindow GkTopicWindow $args
	  return $w
   }
}

##################################
# Routines for the class builder
##################################

proc gkTopicWindow_CreateClassRec {} {
    global gkTopicWindow

    # Default values for the gkTopicWindow class. Note that it inherits
    # all of the options and commands that a Tk text window  and it 
    # also defines a few others.

    set gkTopicWindow(inherit) { text }
    set gkTopicWindow(options) {-large -largebold -largebolditalic \
		-largeitalic -normal -normalbold -normalbolditalic \
 	        -normalitalic -text -title }
    set gkTopicWindow(methods) { button scrollbar text title window }

    set gkTopicWindow(-large)            {-large largeFont LargeFont \
	   "-*-*century*-medium-r-normal--*-180*" }
    set gkTopicWindow(-largebold)        {-largebold largeBoldFont LargeBoldFont \
            "-*-*century*-bold-r-normal--*-180*" }
    set gkTopicWindow(-largebolditalic)  {-largebolditalic largeBoldItalicFont \
	     LargeBoldItalicFont "-*-*century*-bold-i-normal--*-180*" }
    set gkTopicWindow(-largeitalic)      {-largeitalic largeItalicFont \
	     LargeItalicFont "-*-*century*-medium-i-normal--*-180*" }

    set gkTopicWindow(-normal)           {-normal normalFont NormalFont \
	     "-*-*century*-medium-r-normal--*-140*"}
    set gkTopicWindow(-normalbold)       {-normalbold normalBoldFont normalBoldFont \
	     "-*-*century*-bold-r-normal--*-140*"}
    set gkTopicWindow(-normalbolditalic) {-normalbolditalic normalBoldItalicFont\
             NormalBoldItalicFont "-*-*century*-bold-i-normal--*-140*"}
    set gkTopicWindow(-normalitalic)      {-normalitalic normalItalicFont \
	     NormalItalicFont  "-*-*century*-medium-i-normal--*-140*"}

    set gkTopicWindow(-text)              {-text text Text {} }
    set gkTopicWindow(-title)             {-title title Title {} }
}

proc gkTopicWindow_InitWidgetRec {w class className args} {
    upvar #0 $w gk_topic

    # The pathName of the window  
    set gk_topic(window) $w.gk_topic_window

    # Default Values for the TEXT Widget
    set gk_topic(-borderwidth)		2
    set gk_topic(-font)			$gk_topic(-normal)
    set gk_topic(-height)		10
    set gk_topic(-width) 		40
    set gk_topic(-relief)		ridge
    set gk_topic(-setgrid)		true
    set gk_topic(-state)		normal
    set gk_topic(-wrap)			word
    set gk_topic(-yscrollcommand) 	"$gk_topic(window).sb set"
}

proc gkTopicWindow_ConstructWidget {w} {
    upvar #0 $w gk_topic

    # If the window exists, the display it otherwise create it.
    if { ![winfo exist $gk_topic(window)] } {
       # create the window
       toplevel $gk_topic(window)
       wm title  $gk_topic(window) $gk_topic(-title)
       wm protocol $gk_topic(window) \
	   WM_DELETE_WINDOW "wm withdraw $gk_topic(window)"
       wm withdraw $gk_topic(window)

       # Create all of the subwidgets
       set gk_topic(button) [button "$gk_topic(window).ok"]
       set gk_topic(scrollbar) [scrollbar "$gk_topic(window).sb"]
       set gk_topic(text) [text "$gk_topic(window).body"]
       set gk_topic(title) [label "$gk_topic(window).title"]

       # Set up the font display styles for the array conatining the fonts
       foreach item [array names gk_topic] {
           if { ([string match -large* $item]) || ([string match -normal* $item])} {
	      $gk_topic(text) tag configure [string range $item 1 end] \
	             -font $gk_topic($item)
           }
       }
    }
    # input the text with tags
    _gk_showTopicWindow $w $gk_topic(-title) $gk_topic(-text)
}

proc gkTopicWindow_Config {w option args} {
    upvar #0 $w gk_topic

    # Here is where the manipulation of the options and there values
    # is controlled.  Note that some of it is done in the gkClass
    # builder.

    set args [lindex $args 0]
    switch -regexp [string range $option 1 end] {
        large*|normal* { 
 	    $gk_topic(text) config -state normal
            $gk_topic(text) tag configure [string range $option 1 end]  \
		 -font $args
        }
        title { 
	    wm title $gk_topic(window) $args
	    $gk_topic(title) config -text $args
        }
        text  { 
 	    $gk_topic(text) config -state normal
	    $gk_topic(text) delete 0.0 end
	    _gk_showTopicWindow $w $gk_topic(-title) $args
        }
        default { $gk_topic(text) config $option $args }
    }
}

proc gkTopicWindow_Methods {w command args} {
    upvar #0 $w gk_topic

     # Here is where the commands for the gkparticipants widget 
     # is controlled.  Note that some of it is done in the gkClass
     # builder and that all of these just return window paths.

    set args [lindex $args 0]
    switch -exact $command {
        button	  { return $gk_topic(button) }
	scrollbar { return $gk_topic(scrollbar) }
	title 	  { return $gk_topic(title) }
	text      { return $gk_topic(text) }
	window    { return $gk_topic(window) }
	default   { eval $gk_topic(text) $command $args }
    }
}

##########################
#  Internal Routines
##########################

proc _gk_showTopicWindow {w title text} {
    upvar #0 $w gk_topic

    # Build the Help Title
    #----------------------
    $gk_topic(title) config \
	-text $title \
	-font $gk_topic(-largebold)

    # Build the widget/styles for the Text
    #--------------------------------
    # Now construct the body of the help

    $gk_topic(text) config \
	-width $gk_topic(-width) \
	-height $gk_topic(-height) \
	-setgrid $gk_topic(-setgrid) \
	-wrap $gk_topic(-wrap) \
	-relief $gk_topic(-relief) \
	-bd $gk_topic(-borderwidth) \
	-state $gk_topic(-state) \
	-yscrollcommand $gk_topic(-yscrollcommand) \
	-font $gk_topic(-normal)

    $gk_topic(scrollbar) config \
	-relief flat \
	-command "$gk_topic(window).body yview"
    
    # Put the stylized help text into the widget
    #-------------------------------------------
    set done  [llength $text]
    set idx 0
    while {$idx < $done} { 
	set tags [lindex $text $idx]
	set phrase [lindex $text [incr idx]]
	_gk_topicInsertWithTags $gk_topic(text)  $phrase $tags
	incr idx
    }

    # We don't want the text widget editable
    $gk_topic(text) configure -state disabled 


    # Put in a button to dismiss the window
    #--------------------------------------
    $gk_topic(button) config \
	-text "Okay" \
	-command "destroy $gk_topic(window)"

    # Put it all together
    #-------------------
    pack $gk_topic(title) -side top
    pack $gk_topic(button) -side bottom 
    pack $gk_topic(scrollbar) -side right -fill y
    pack $gk_topic(text) -expand yes -fill both 
    wm deiconify $gk_topic(window)
}

# The procedure below inserts text into a given text widget and
# applies one or more tags to that text.  The arguments are:
#
# w		Window in which to insert
# text		Text to insert (it's inserted at the "insert" mark)
# args		One or more tags to apply to text.  If this is empty
#		then all tags are removed from the text.
# This routine came from the Tk Demos.
proc _gk_topicInsertWithTags {w text args} {
    
    set start [$w index insert]
    $w insert insert "$text"

    foreach tag [$w tag names $start] { 
	$w tag remove $tag $start insert
    }

    foreach i $args {
	$w tag add $i $start insert
    }
}
