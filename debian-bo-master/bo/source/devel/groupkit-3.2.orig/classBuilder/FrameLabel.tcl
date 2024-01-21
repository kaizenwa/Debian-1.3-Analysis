##############################################################################
# frameLabel is an example of a composite widet.  It is composed of two
# widgets a fancyLabel and a frame.  Note that fancyLabel is one of the
# classes constructed with gkClassBuilder.  It inherits all of the options
# from fancyLabel as well as frame.  It has a number of new options which
# are used to access the frame.  These new options are as follows:
#	-framebd	border width of the frame
#       -framebg	background color of the frame
#       -frameheight    height of the frame
#       -framerelief	relief of the frame
#       -framewidth     width of the frame
#
# To create a frameLabel just do the following:
#       frameLabel .fl
#       pack .fl
#
# Note that this new widget .fl can be used in the same manner as the
# standard tcl widgets are.  For example try ".fl config" and see what
# happens.
#
# Also since the fancyLabel can flash then the frameLabel will also have
# this ability if the procedure frameLabel_flash is defined.  As you can
# see this has been done.
##############################################################################

proc frameLabel {w args} {
        # This procedure constructs a new widget of the class "frameLabel"
        # This is the only place where any of the gkClassBuilder procedures
        # get called.

	eval gkInt_CreateWidget $w frameLabel frameLabelClass $args
	return $w
}

proc frameLabel_CreateClassRec {} {
	global frameLabel

	# This procedure defines the default values for class of "frameLabel"
        # widgets. Note that it inherits all of the commands/options from
        # the fancyLabel and frame widgets.  It also has a number of new
        # options defined in "frameLabel(options)".  All of the new options
	# are used with the frame subwidget of this class.
       
	set frameLabel(inherit) {fancyLabel frame}
        set frameLabel(options) {-framebd -framebg -frameheight -framerelief\
	    -framewidth}
        set frameLabel(-framebd) {-framebd frameBd FrameBd 3}
        set frameLabel(-framebg) {-framebg frameBg FrameBg gray50}
        set frameLabel(-frameheight) {-frameheight frameHeight FrameHeight 30}
        set frameLabel(-framerelief) {-framerelief frameRelief FrameRelief ridge}
        set frameLabel(-framewidth) {-framewidth frameWidth FrameWidth 30}
}

proc frameLabel_ConstructWidget {w} {
	upvar #0 $w data

        # This procedure constructs a new widget of the type "frameLabel"
        # Since all of the values for the widget are stored in an array
        # by the same name as the widget it is neccesary to use the "upvar"
        # command in order to access these values.  Also the subwidgets
        # are given variable names so that other routines can access them.

        $w config -borderwidth $data(-framebd) \
		-bg $data(-framebg) -relief $data(-framerelief) \
		-width $data(-framewidth) -height $data(-frameheight)
	set data(label1) [fancyLabel $w.l1]
        pack $data(label1) -fill x -expand t -padx 10 -pady 10 -anchor center
}


proc frameLabel_Config {w option args} { 
	upvar #0 $w data

        # This procedure is used to change the options of a widget.
        # All of the options starting with "-frame" are associated
        # with the frame widget and all of the others are used with
        # the fancyLabel widget.  

        if { [string match *frame* $option] } {
           set option [string range $option 6 end]
	   $data(rootCmd) config -$option $args
        } else {
           $data(label1) config $option $args
        }
}

proc frameLabel_Methods {w command args} {
        upvar #0 $w data

        # This procedure is necessary in order to give the fancyLabel
        # widget the proper functionality.  ie. enables it to flash.
	
	$data(label1) $command $args
}
