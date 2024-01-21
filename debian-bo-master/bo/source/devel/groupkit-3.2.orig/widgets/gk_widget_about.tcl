#
#  gk_about - create and manipulate about box widgets
# 
#       See the manual pages for more information.
#

proc gk_about {w args} {
    # Constructs a new widget of the gkAbout class.  It uses the
    # groupkit class Builder to construct the class and the widget.

    eval gkInt_CreateWidget $w gkAbout GkAbout $args
    return $w
}

##################################
# Routines for the class builder
##################################
proc gkAbout_CreateClassRec {} {
   global gkAbout
   global gk_library

   # Defines the default values for the class "gkAbout".  It inherits all
   # of the options and commands from the Tk "frame" widget as well as
   # some additional ones.  

   set gkAbout(inherit)  {frame}
   set gkAbout(options)  {-bitmap -bitmapbg -bitmapfg -font -info -infowidth \
			  -title  -version -versionwidth}
   set gkAbout(methods)  {bitmap button info title version }

   set gkAbout(-bitmap)   [list -bitmap bitmap Bitmap \
                            @$gk_library/library/bitmaps/groupkit_logo.xbm]
			  
   set gkAbout(-bitmapbg) {-bitmapbg bitmapBg BitmapBg {}}
   set gkAbout(-bitmapfg) {-bitmapbg bitmapBg BitmapBg Blue}
   set gkAbout(-font)      {-font font Font \
			      "-Adobe-times-bold-r-normal--*-240*"}

   set gkAbout(-info)      {-info info Info \
	 "By: \n- Mark Roseman\n- Saul Greenberg\n- and many others!\n\n\
For information, contact\n- Saul Greenberg\n   saul@cpsc.ucalgary.ca"}
   set gkAbout(-infowidth) {-infowidth infoWidth InfoWidth 300}

   set gkAbout(-title)     {-title title Title "GroupKit"}
   set gkAbout(-version)   {-version version Version "3.1\tApril, 1995"}
   set gkAbout(-versionwidth) {-versionwidth versionWidth VersionWidth 250}
}

proc gkAbout_InitWidgetRec {w class className args} {
#global gk_library
   upvar #0 $w gkAbout

   # Some intial values for the widget
   # Widget window
   set gkAbout(window)    $w.about

   # Dismiss button text
   set gkAbout(ok_text) Ok
}

proc gkAbout_ConstructWidget {w} {
#    global gk_library 
    upvar #0 $w gkAbout

    # Construction of the widget.  

    # If the window exists then make it visible
    if {[winfo exists $gkAbout(window)] == 1} {
	wm deiconify $gkAbout(window)
	return
    }

    # Since the window doesn't exist then create it.  Note the change
    # in the window delete protocol.  This will hopefully speed things up.
    toplevel $gkAbout(window) 
    wm title $gkAbout(window) $gkAbout(-title)
    wm protocol $gkAbout(window) \
	    WM_DELETE_WINDOW "wm withdraw $gkAbout(window)"

    # Now Create all the bits and pieces in the window    
    set gkAbout(titleframe) [frame "$gkAbout(window).titleframe" \
           -relief ridge]
    set gkAbout(infoframe) [frame "$gkAbout(window).infoframe" ]

    set gkAbout(bitmap) [label "$gkAbout(infoframe).bitmap" \
	-bitmap $gkAbout(-bitmap) \
	-fg $gkAbout(-bitmapfg)]

    set gkAbout(title) [label "$gkAbout(titleframe).title" \
	-text $gkAbout(-title) \
	-font $gkAbout(-font) \
	-relief ridge]

    set gkAbout(version) [message "$gkAbout(titleframe).version" \
	-text "Version $gkAbout(-version)" \
	-width $gkAbout(-versionwidth)]

    set gkAbout(info) [message "$gkAbout(infoframe).info" \
	-text $gkAbout(-info) \
	-width $gkAbout(-infowidth)]

    set gkAbout(button) [button "$gkAbout(window).ok"  \
	-text $gkAbout(ok_text) \
	-command "destroy $gkAbout(window); destroy $w"]
    
    # Now pack all the bits and pieces
    pack $gkAbout(titleframe) -side top -expand true -fill x
    pack $gkAbout(infoframe) -side top
    pack $gkAbout(title) -side top 
    pack $gkAbout(version) -side top 
    pack $gkAbout(bitmap) -side left 
    pack $gkAbout(info) -side top 
    pack $gkAbout(button) -side bottom -fill x 
}

proc gkAbout_Config {w option args} {
    upvar #0 $w gkAbout

    # Here is where the manipulation of the options and there values
    # is controlled.  Note that some of it is done in the gkClass
    # builder.

    set args [lindex $args 0]
    switch -exact [string range $option 1 end] {
       bitmap       { eval $gkAbout(bitmap) config $option $args }
       bitmapbg     { $gkAbout(bitmap) config -bg $args }
       bitmapfg     { $gkAbout(bitmap) config -fg $args }
       font         { $gkAbout(title) config -font $args }
       info         { $gkAbout(info) config -text $args }
       infowidth    { $gkAbout(info) config -width $args }
       title        { $gkAbout(title) config -text $args }
       version      { $gkAbout(version) config -text "Version $args" }
       versionwidth { $gkAbout(version) config -width $args }
       default      { $gkAbout(rootCmd) config $option $args }
     }
}

proc gkAbout_Methods {w command args} {
    upvar #0 $w gkAbout

    # Here is where the commands for the gkAbout widget is controlled.
    # Note that some of it is done in the gkClass builder.

    set args [lindex $args 0]
    switch -regexp $command {
       bitmap|button|info|title|version { return $gkAbout($command) }
       default { $gkAbout(rootCmd) $command $args }
    }
}

