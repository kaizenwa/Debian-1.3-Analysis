#
# tcllib.tcl --
#
# Various command dealing with auto-load libraries.  Some of this code is
# taken directly from the UCB Tcl library/init.tcl file.
#------------------------------------------------------------------------------
# Copyright 1992-1995 Karl Lehenbauer and Mark Diekhans.
#
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted, provided
# that the above copyright notice appear in all copies.  Karl Lehenbauer and
# Mark Diekhans make no representations about the suitability of this
# software for any purpose.  It is provided "as is" without express or
# implied warranty.
#------------------------------------------------------------------------------
# Copyright (c) 1991-1994 The Regents of the University of California.
# All rights reserved.
#
# Permission is hereby granted, without written agreement and without
# license or royalty fees, to use, copy, modify, and distribute this
# software and its documentation for any purpose, provided that the
# above copyright notice and the following two paragraphs appear in
# all copies of this software.
#
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
# OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
# CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
# ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
# PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
#------------------------------------------------------------------------------
# $Id: tcllib.tcl,v 5.0 1995/07/25 06:00:09 markd Rel $
#------------------------------------------------------------------------------
#

#@package: TclX-libraries searchpath auto_load_file

#------------------------------------------------------------------------------
# searchpath:
# Search a path list for a file. (catch is for bad ~user)
#
proc searchpath {pathlist file} {
    foreach dir $pathlist {
        if {$dir == ""} {set dir .}
        if {[catch {file exists $dir/$file} result] == 0 && $result}  {
            return $dir/$file
        }
    }
    return {}
}

#------------------------------------------------------------------------------
# auto_load_file:
# Search auto_path for a file and source it.
#
proc auto_load_file {name} {
    global auto_path errorCode
    if {[string first / $name] >= 0} {
        return  [uplevel 1 source $name]
    }
    set where [searchpath $auto_path $name]
    if [lempty $where] {
        error "couldn't find $name in any directory in auto_path"
    }
    uplevel 1 source $where
}

#@package: TclX-lib-list auto_packages auto_commands

#------------------------------------------------------------------------------
# auto_packages:
# List all of the loadable packages.  If -files is specified, the file paths
# of the packages is also returned.

proc auto_packages {{option {}}} {
    global auto_pkg_index

    auto_load  ;# Make sure all indexes are loaded.
    if ![info exists auto_pkg_index] {
        return {}
    }
    
    set packList [array names auto_pkg_index] 
    if [lempty $option] {
        return $packList
    }

    if {$option != "-files"} {
        error "Unknow option \"$option\", expected \"-files\""
    }
    set locList {}
    foreach pack $packList {
        lappend locList [list $pack [lindex $auto_pkg_index($pack) 0]]
    }
    return $locList
}

#------------------------------------------------------------------------------
# auto_commands:
# List all of the loadable commands.  If -loaders is specified, the commands
# that will be involked to load the commands is also returned.

proc auto_commands {{option {}}} {
    global auto_index

    auto_load  ;# Make sure all indexes are loaded.
    if ![info exists auto_index] {
        return {}
    }
    
    set cmdList [array names auto_index] 
    if [lempty $option] {
        return $cmdList
    }

    if {$option != "-loaders"} {
        error "Unknow option \"$option\", expected \"-loaders\""
    }
    set loadList {}
    foreach cmd $cmdList {
        lappend loadList [list $cmd $auto_index($cmd)]
    }
    return $loadList
}

#@package: TclX-ucblib auto_reset auto_mkindex

#------------------------------------------------------------------------------
# auto_reset:
# Destroy all cached information for auto-loading and auto-execution,
# so that the information gets recomputed the next time it's needed.
# Also delete any procedures that are listed in the auto-load index
# except those related to auto-loading.
# *** MODIFIED FOR TclX ***

proc auto_reset {} {
    global auto_execs auto_index auto_oldpath
    foreach p [info procs] {
	if {[info exists auto_index($p)] && ($p != "unknown")
		&& ![string match auto_* $p]} {
	    rename $p {}
	}
    }
    catch {unset auto_execs}
    catch {unset auto_index}
    catch {unset auto_oldpath}
    # *** TclX ***
    global auto_pkg_index tclx_library
    catch {unset auto_pkg_index}
    set auto_index(buildpackageindex) {source $tclx_library/buildidx.tcl}
    return
}

if {[info commands @scope] == ""} {

# No ITcl namespaces

#------------------------------------------------------------------------------
# auto_mkindex:
# Regenerate a tclIndex file from Tcl source files.  Takes two arguments:
# the name of the directory in which the tclIndex file is to be placed,
# and a glob pattern to use in that directory to locate all of the relevant
# files.

proc auto_mkindex {dir files} {
    global errorCode errorInfo
    set oldDir [pwd]
    cd $dir
    set dir [pwd]
    append index "# Tcl autoload index file, version 2.0\n"
    append index "# This file is generated by the \"auto_mkindex\" command\n"
    append index "# and sourced to set up indexing information for one or\n"
    append index "# more commands.  Typically each line is a command that\n"
    append index "# sets an element in the auto_index array, where the\n"
    append index "# element name is the name of a command and the value is\n"
    append index "# a script that loads the command.\n\n"
    foreach file [glob $files] {
	set f ""
	set error [catch {
	    set f [open $file]
	    while {[gets $f line] >= 0} {
		if [regexp {^proc[ 	]+([^ 	]*)} $line match procName] {
		    append index "set [list auto_index($procName)]"
		    append index " \"source \$dir/$file\"\n"
		}
	    }
	    close $f
	} msg]
	if $error {
	    set code $errorCode
	    set info $errorInfo
	    catch [close $f]
	    cd $oldDir
	    error $msg $info $code
	}
    }
    set f [open tclIndex w]
    puts $f $index nonewline
    close $f
    cd $oldDir
}

} else {

# Code for ITcl namespaces

# auto_mkindex:
# Regenerate a tclIndex file from Tcl source files.  Takes as argument
# the name of the directory in which the tclIndex file is to be placed,
# floowed by any number of glob patterns to use in that directory to
# locate all of the relevant files.

proc auto_mkindex {dir args} {
    global errorCode errorInfo
    set oldDir [pwd]
    cd $dir
    set dir [pwd]

    global ::tcl::mkindex-parser::index
    set index ""

    append index "# Tcl autoload index file, version 2.0 for \[incr Tcl\]\n"
    append index "# This file is generated by the \"auto_mkindex\" command\n"
    append index "# and sourced to set up indexing information for one or\n"
    append index "# more commands.  Typically each line is a command that\n"
    append index "# sets an element in the auto_index array, where the\n"
    append index "# element name is the name of a command and the value is\n"
    append index "# a script that loads the command.\n\n"

    foreach file [eval glob $args] {
        if {[catch {tcl::mkindex-parser::mkindex $file} msg] != 0} {
            set code $errorCode
            set info $errorInfo
            cd $oldDir
            error $msg $info $code
        }
    }

    set fid [open tclIndex w]
    puts $fid $index nonewline
    close $fid
    cd $oldDir
}

#
# Create a namespace that can be used to parse Tcl source files to
# generate a tclIndex file for autoloading.  This namespace contains
# commands for things that need index entries.  Each time a command
# is executed, it writes an entry out to the index file.
#
# An isolated namespace is set up to filter incoming commands.
# Those that aren't registered in the "mkindex-parser" namespace
# are simply ignored.
#
namespace ::tcl::mkindex-parser {
    ::public variable index ""          ;# maintains index as it is built
    ::protected variable scriptFile ""  ;# name of file being processed
    ::protected variable scopeStack ""  ;# stack of namespace scopes

    # --------------------------------------------------------------------
    # USAGE:  mkindex <fileName>
    #
    # Scans Tcl code from the specified <fileName> and adds entries
    # into the global "index" variable for elements that are recognized
    # by the autoloader.
    # --------------------------------------------------------------------
    ::public ::proc mkindex {file} {
        global scriptFile
        set scriptFile $file

        set fid [open $file]
        set contents [read $fid]
        close $fid

        #
        # There is one problem with sourcing files into the isolated
        # namespace:  references like "$x" will fail since code is not
        # really being executed and variables do not really exist.
        # Be careful to escape all naked "$" before evaluating.
        #
        regsub -all {([^\$])\$([^\$])} $contents {\1\\$\2} contents
        ::namespace ::tcl::mkindex-parser::isolated $contents
    }

    #
    # Set up an isolated namespace to filter incoming commands.
    # Put the "enforce_cmd" proc in the parent namespace, so commands
    # within it work with some sanity.
    #
    ::proc enforce_cmd {name} {
        global commands
        if {[info exists commands($name)]} {
            return $commands($name)
        }
        return "mkindex_ignore"
    }
    ::namespace isolated -local -enforced yes

    #
    # Set up a command registry.  All commands that require an
    # tclIndex entry, and all possible namespace paths for these
    # commands, should be registered in this array.
    #
    # The "source" command should work as usual, so that files
    # can be read into the namespace.
    #
    protected variable commands

    set commands(source) ::source
    set commands(::source) ::source

    #
    # Most commands get mapped to "mkindex_ignore" and are ignored.
    #
    ::proc mkindex_ignore {args} {}

    #
    # HANDLE:  namespace name ... {commands...}
    #
    ::proc namespace {name args} {
        global scopeStack
        set scopeStack [linsert $scopeStack 0 $name]

        set cmds [lindex $args end]
        ::namespace isolated $cmds

        set scopeStack [lrange $scopeStack 1 end]
    }
    set commands(namespace) namespace
    set commands(::namespace) namespace

    #
    # USAGE:  mkindex_path <name>
    #
    # Returns the complete namespace path for the specified <name>.
    # If <name> starts with "::", then it is returned directly.
    # Otherwise, elements from the current namespace stack are added
    # until the complete path starts with "::", or until all elements
    # have been added.
    #
    ::protected ::proc mkindex_path {name} {
        global scopeStack
        if {[string match ::* $name]} {
            return $name
        }
        foreach ns $scopeStack {
            set name "$ns::$name"
            if {[string match ::* $name]} {
                return $name
            }
        }
        return "::$name"
    }

    #
    # HANDLE:  ensemble name body
    #
    ::proc ensemble {name body} {
        global index scriptFile
        set name [mkindex_path $name]
        append index "set [list auto_index($name)]"
        append index " \"source \$dir/$scriptFile\"\n"
    }
    set commands(ensemble) ensemble
    set commands(::ensemble) ensemble

    #
    # HANDLE:  public ...
    #          protected ...
    #          private ...
    #
    ::proc plevel {args} {
        if {[llength $args] == 1} {
            ::namespace isolated [lindex $args 0]
        } else {
            ::namespace isolated $args
        }
    }
    set commands(public) plevel
    set commands(::public) plevel
    set commands(protected) plevel
    set commands(::protected) plevel
    set commands(private) plevel
    set commands(::private) plevel

    #
    # HANDLE:  proc name arglist body
    #
    ::proc proc {name arglist body} {
        global index scriptFile
        set name [mkindex_path $name]
        append index "set [list auto_index($name)]"
        append index " \"source \$dir/$scriptFile\"\n"
    }
    set commands(proc) proc
    set commands(::proc) proc
}
}
