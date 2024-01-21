#-----------------------------------------------------------------------------
# TclInit.tcl -- Extended Tcl initialization.
#-----------------------------------------------------------------------------
# $Id: TclInit.tcl,v 5.0 1995/07/25 06:00:03 markd Rel $
# ========================================================================
# >>>>>>>>>>>>>>>> INCLUDES MODIFICATIONS FOR [incr Tcl] <<<<<<<<<<<<<<<<<
#
#  AUTHOR:  Michael J. McLennan       Phone: (610)712-2842
#           AT&T Bell Laboratories   E-mail: michael.mclennan@att.com
#     RCS:  $Id: init.tcl,v 1.5 1995/09/14 16:22:25 mmc Exp $
# ========================================================================
#             Copyright (c) 1993-1995  AT&T Bell Laboratories
# ------------------------------------------------------------------------
# This software is copyrighted by the Regents of the University of
# California, Sun Microsystems, Inc., and other parties.  The following
# terms apply to all files associated with the software unless explicitly
# disclaimed in individual files.
# See the file "license.terms" from the Tcl distribution for information on
# usage and redistribution of this file, and for a DISCLAIMER OF ALL
# WARRANTIES.

set unknown_handler_order {}

# ------------------------------------------------------------------------
#  USAGE: unknown ?command arg arg...?
#
#  Invoked automatically whenever an unknown command is encountered.
#  Works through a list of "unknown handlers" that have been registered
#  to deal with unknown commands.  Extensions can integrate their own
#  handlers into the "unknown" facility via "unknown_handle".
#
#  If a handler exists that recognizes the command, then it will
#  take care of the command action and return a valid result or a
#  Tcl error.  Otherwise, it should return "-code continue" (=2)
#  and responsibility for the command is passed to the next handler.
# ------------------------------------------------------------------------
proc unknown {args} {
    global unknown_handler_order unknown_handlers errorInfo errorCode

    foreach handler $unknown_handler_order {
        set status [catch {uplevel $unknown_handlers($handler) $args} result]

        if {$status == 1} {
            #
            # Strip the last five lines off the error stack (they're
            # from the "uplevel" command).
            #
            set new [split $errorInfo \n]
            set new [join [lrange $new 0 [expr [llength $new] - 6]] \n]
            return -code $status -errorcode $errorCode \
                -errorinfo $new $result

        } elseif {$status != 4} {
            return -code $status $result
        }
    }

    set name [lindex $args 0]
    return -code error "invalid command name \"$name\""
}

# ------------------------------------------------------------------------
#  USAGE: unknown_handler name command ?arg arg...?
#
#  Registers another handler to be used in conjunction with the
#  unknown command.  Each time an unknown command is encountered, the
#  list of handlers is consulted to determine how to handle the command.
#  If a handler recognizes the command, then it should load, define,
#  or otherwise handle the command and return.  Otherwise, it should
#  return "-code continue", which tells the "unknown" facility to
#  continue with another handler.
#
#  Handlers registered first are consulted last.  This way, extensions
#  that are loaded later have higher priority for handling special cases.
#
#  Usually "command" is the name of a handling procedure.  But extra
#  arguments can be specified when the handler is registered, and
#  these will be kept as arguments when the handler is invoked.  The
#  actual unknown command and its arguments are appended after these.
# ------------------------------------------------------------------------
proc unknown_handler {name args} {
    global unknown_handler_order unknown_handlers

    set unknown_handlers($name) $args
    set unknown_handler_order [linsert $unknown_handler_order 0 $name]
}

#
# TclX unknown command trap handler.
#
proc tclx_unknown args {
    if [auto_load [lindex $args 0]] {
        set code [catch {uplevel 1 $args} msg]
        if {$code ==  1} {
            # Strip the last five lines off the error stack (they're
            # from the "uplevel" command).
            global errorInfo errorCode
            set new [split $errorInfo \n]
            set new [join [lrange $new 0 [expr [llength $new] - 6]] \n]
            return -code error -errorcode $errorCode \
                    -errorinfo $new $msg
        } else {
            return -code $code $msg
        }
    }
    if {([info proc tclx_unknown2] == "") && ![auto_load tclx_unknown2]} {
        error "can't find tclx_unknown2 on auto_path"
    }
    set code [catch {uplevel 1 tclx_unknown2 [list $args]} msg]
    if {$code ==  1} {
        # Strip the last five lines off the error stack (they're
        # from the "uplevel" command).
        global errorInfo errorCode
        set new [split $errorInfo \n]
        set new [join [lrange $new 0 [expr [llength $new] - 6]] \n]
        return -code error -errorcode $errorCode \
                -errorinfo $new $msg
    } else {
        return -code $code $msg
    }
}

unknown_handler "tclx" tclx_unknown

set auto_index(buildpackageindex) {source $tclx_library/buildidx.tcl}

# ITcl namespaces need mkindex parser namespace defined now.

if {[info commands @scope] != ""} {
    auto_load auto_mkindex
}

# == Put any code you want all Tcl programs to include here. ==

if !$tcl_interactive return

# == Interactive Tcl session initialization ==

if ![info exists tcl_prompt1] {
    set tcl_prompt1 {global argv0; puts -nonewline stdout [file tail $argv0]>}
}
if ![info exists tcl_prompt2] {
    set tcl_prompt2 {puts -nonewline stdout =>}
}
