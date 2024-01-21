#
#  gk_viewport - attach scrollbars to a window
# 
#       See the manual pages for more information.
#


proc gk_viewport {w widget args} {
        upvar #0 $w gkViewport

        # Creates an instance of the viewport class.  First the widget
        # is evaluated then the new widget is created with the specified
        # scrollbars.

	if {[llength $widget] == 1} {
	    # do not need to evaluate child since all that was given is
	    # the name of the child.
	    set gkViewport(child) $widget
	} else {
	    # evaluate the child widget, ie. create it and process
	    # all the options
	    set gkViewport(child) [eval $widget]
	}

        eval gkInt_CreateWidget $w gkViewport GkViewport $args
        return $w
}

##################################
# Routines for the class builder
##################################

proc gkViewport_CreateClassRec {} {
        global gkViewport

        # Definitions on the Class Record.  One option and three commands.
        # Also all of the commands and options are inherited from the
        # tk widget "frame".

        set gkViewport(inherit) {frame}
        set gkViewport(options) {-multiuser -scroll}
        set gkViewport(methods) {xcommand ycommand xscrollbar yscrollbar repack}
	set gkViewport(-multiuser) {-multiuser multiUser MultiUser yes}
        set gkViewport(-scroll) {-scroll scroll Scroll {right bottom}}
}

proc gkViewport_InitWidgetRec {w class className args} {
        upvar #0 $w gkViewport

        # The default values for a number of internal variables.

        # Left, Right, Top and Bottom  scrollbar 1/0 ie. yes/no
        set gkViewport(left)		0	
        set gkViewport(right)		1       
        set gkViewport(top)		0       
        set gkViewport(bottom)		1       

	# Horizontal and Vertical scrollbars yes/no
	set gkViewport(varh)		0       
	set gkViewport(varv)		0       

	# Horizontal and Vertical needs adjusting yes/no
        set gkViewport(von) 		1	
        set gkViewport(hon)		1	

	# xscrollbar and yscrollbar command specified yes/no
	set gkViewport(dox) 		1       
        set gkViewport(doy) 		1       
}

proc gkViewport_ConstructWidget {w args} {   
	upvar #0 $w gkViewport

	# Construct the gkViewport widget
        #   1. Create the vertical, horizontal and corner piece for the 
        #      scrolling area.
        #   2. Create the vertical scrollbar, if the child widget has a value
        #      for the yscrollcommand option then don't use default values
        #   3. Create the horizontal scrollbar, if the child widget has a value
        #      for the xscrollcommand option then don't use default values

	set gkViewport(up) 	[frame $w.up]
	set gkViewport(across)	[frame $w.across]
	set gkViewport(corner)	[frame $w.corner]

        # ignore scrolling if child doesn't have scrolling abilities
        if {[winfo exists $gkViewport(child) ]} {
	   if { [catch "$gkViewport(child) configure -xscrollcommand"] } {
		 set gkViewport(dox) 0
	   }
	   if { [catch "$gkViewport(child) configure -yscrollcommand"] } {
		 set gkViewport(doy) 0
	   }
        } else {
	    if {[catch "$gkViewport(child) configure -xscrollcommand"]} {
		set gkViewport(dox) 0
	    }
	    if {[catch "$gkViewport(child) configure -yscrollcommand"]} {
		set gkViewport(doy) 0
	    }
	}
       
        # NOTE: Both the scrollbars command the the childs (x/y) commands are
        #       set simultaneously.  This should be implemented better so
        #	that whether or not the scrollbar command is set depends on
        #	the value of the scrollbar's command.

	set gkViewport(horiz) [gk_scrollbar $w.horiz -orient horizontal]
  	if { $gkViewport(dox) } {
	    if {[lindex [$gkViewport(child) configure -xscrollcommand] 4] == ""} {
                $gkViewport(horiz) configure -command "$gkViewport(child) xview"
		$gkViewport(child) configure \
		   -xscrollcommand "_gkViewport_hscroll $w"
	    }
	}   

	set gkViewport(vert) [gk_scrollbar $w.vert]
  	if { $gkViewport(doy) } {
	    if {[lindex [$gkViewport(child) configure -yscrollcommand] 4] == ""} {
                $gkViewport(vert) configure -command "$gkViewport(child) yview"
	        $gkViewport(child) configure \
		    -yscrollcommand "_gkViewport_vscroll $w"
            }
        }

        _gkViewport_setscroll $w $args
}

proc gkViewport_Config {w option args} {
	upvar #0 $w gkViewport

        # The option is set to the value specified in the args list.
        # If the option is "-scroll" then the arguments are evaluated and
	# the gkViewport is repacked.   If the option is -multiuser then
        # the arguments are passed on to the group scrollbars. Otherwise 
        # the option and arguments are applied to the child widget.

        set args [lindex $args 0]

	if { [string match -scroll $option] } { 
           set gkViewport(right) 0 ; set gkViewport(left) 0 ; 
           set gkViewport(top) 0 ;  set gkViewport(bottom) 0 

           foreach arg $args { 
	      switch -exact $arg {
                 bottom {set gkViewport(bottom) 1 }
                 left   {set gkViewport(left) 1 }
		 right  {set gkViewport(right) 1 }
                 top    {set gkViewport(top) 1 }
		 default {error "Invalid value for scroll option: $arg"}
              }
            }
            _gkViewport_repack $w 
        } elseif [string match -multiuser $option] {
	    $w.horiz configure -multiuser $args
	    $w.vert configure -multiuser $args
	    _gkViewport_repack $w
	} else {
	   $gkViewport(child) config $option $args
        }
}          

proc gkViewport_Methods {w command args } {
	upvar #0 $w gkViewport
	
	# Execute the specified command with the given arguments.  If the
	# command is not one of repack, xscrollbar, or yscrollbar then
	# the command is executed on the child widget.  If it is one of the
	# three then the appropriate procedure call is made.

        switch -exact $command {
            repack     { _gkViewport_repack $w }
            xcommand   { return "_gkViewport_hscroll $w" }
            ycommand   { return "_gkViewport_vscroll $w" }
	    xscrollbar { return $gkViewport(horiz) }
	    yscrollbar { return $gkViewport(vert) }
            default    { $gkViewport(child) $command $args }
        }
}

##########################
#  Internal Routines
##########################

proc  _gkViewport_setscroll {w args} { 

	# Evaluates all of the internal variables used to determine which
	# scrollbars exist.  

	set args [lindex $args 0]
        if {$args == ""} {
           # Give precedence to the right and bottom scrollbars
  	   set gkViewport(left) 0 ; set gkViewport(right) 1
           set gkViewport(top) 0; set gkViewport(bottom) 1
        } 

        # Evaluate whether or not there are vertical and horizontal
        # scrollbars.
	set gkViewport(varh) 0; set gkViewport(varv) 0
	foreach scrollbar $args {
            switch -glob $scrollbar {
		var* {regsub "var(.*)" $scrollbar {\1} scroller
		    switch $scroller {
			left -
			right  {set gkViewport(varv) 1}
			top -
			bottom {set gkViewport(varh) 1}
		    }
		}
		*    {set scroller $scrollbar}
	    }
	    set $scroller 1
	}
	if {[winfo exists $w.up]} {
	    _gkViewport_repack $w 
	}
}

proc  _gkViewport_repack {w} {
        upvar #0 $w gkViewport

	# Repack the viewport widget.  Making sure that the scrollbars,
        # corner and child widget are all positioned appropriately.   The
	# internal variables are used extensively to do this.
	
        # Give precendence to right and bottom scrollbars
	if {$gkViewport(left) && $gkViewport(right)} {
	    set gkViewport(left) 0; set gkViewport(right) 1 
	}
	if {$gkViewport(top) && $gkViewport(bottom)} {
	    set gkViewport(top) 0; set gkViewport(bottom) 1
	}  

	# Packing of the vertical scrollbar
	pack forget $w.vert $gkViewport(child)
	if {$gkViewport(right) && (!$gkViewport(varv) || \
	         ($gkViewport(varv) && $gkViewport(von)))} {
	    pack $w.vert -in $w.up -side right -fill y
	}

	pack $gkViewport(child) -in $w.up -side right -fill both -expand true
	if {$gkViewport(left) && (!$gkViewport(varv) ||
		   ($gkViewport(varv) && $gkViewport(von)))} {
	    pack $w.vert -in $w.up -side left -fill y
	}

	# Packing of the horizontal and the corner scrollbars
	pack forget $w.horiz $w.corner
	set gkViewport(hon) 0
	if {$gkViewport(top) || $gkViewport(bottom)} {
	    if {$gkViewport(left) || $gkViewport(right)} {
		update
		regsub {.*x([0-9]+).*} [winfo reqwidth $w.vert] {\1} pad
		set pad [expr ($pad + 1) / 2]
	    }
	    if {$gkViewport(left)} {
		pack $w.corner -in $w.across -side left -ipadx $pad
	    }
	    pack $w.horiz -in $w.across -side left -fill x -expand true
	    set gkViewport(hon) 1
	    if {$gkViewport(right)} {
		pack $w.corner -in $w.across -side right -ipadx $pad
	    }
	}

	pack forget $w.across $w.up
	if {$gkViewport(bottom) && (!$gkViewport(varh) || \
			($gkViewport(varh) && $gkViewport(hon)))} {
	    pack $w.across -side bottom -fill x
	} 
	pack $w.up -side bottom -fill both -expand true
	if {$gkViewport(top) && (!$gkViewport(varh) || \
			($gkViewport(varh) && $gkViewport(hon)))} {
	    pack $w.across -fill x
	}
	raise $gkViewport(child)
	update
}

proc _gkViewport_hscroll {w max size args} {
        upvar #0 $w gkViewport

 	# Default method of controlling the horizontal scrolling

	if {$gkViewport(varh) && $max <= $size && $gkViewport(hon) } {
		set gkViewport(hon) 0; _gkViewport_repack $w}
	if {$gkViewport(varh) && $max > $size && !$gkViewport(hon)}  {
		set gkViewport(hon) 1; _gkViewport_repack $w}
        eval $gkViewport(horiz) set $max $size $args
}

proc _gkViewport_vscroll {w max size args} {
        upvar #0 $w gkViewport

 	# Default method of controlling the vertical scrolling

	if {$gkViewport(varv) && $max <= $size && $gkViewport(von) } {
		set gkViewport(von) 0; _gkViewport_repack $w}
	if {$gkViewport(varv) && $max > $size && !$gkViewport(von)}  {
		set gkViewport(von) 1; _gkViewport_repack $w}
        eval $gkViewport(vert) set $max $size $args
}

