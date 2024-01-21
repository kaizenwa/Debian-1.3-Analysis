# Init.tcl --
#
#	Initializes the Tix library
#
# Copyright (c) 1996, Expert Interface Technologies
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#



# STEP 1: Version checking
#
#
if [catch {
    if {[tixScriptVersion] != $tix_version} {
        puts -nonewline stderr "error: "
	puts -nonewline stderr "Tix library version ([tixScriptVersion]) "
	puts -nonewline stderr "does not match binary version ($tix_version)\n"
	error 2
    }
    if {[tixScriptPatchLevel] != $tix_patchLevel} {
        puts -nonewline stderr "error: "
	puts -nonewline stderr "Tix library patch-level "
	puts -nonewline stderr "([tixScriptPatchLevel]) "
	puts -nonewline stderr "does not match binary patch-level "
	puts -nonewline stderr "($tix_patchLevel)\n"
	error 1
    }
} err ]  {
    puts -nonewline stderr "Please check your TIX_LIBRARY "
    puts -nonewline stderr "environment variable and your Tix installation.\n"
}

# STEP 1.5: Initialize file compatibility modules
#
#

global tixPriv

if ![info exists tixPriv(isWindows)] {
    InitFileCmpt:Unix
} else {
    InitFileCmpt:Win
}

    
# STEP 2: Initialize the Tix application context
#
#

tixAppContext tix

# STEP 3: Initialize the bindings for widgets that are implemented in C
#
#
tixHListBind
if {[info command tixTList] != ""} {
    tixTListBind
}

# STEP 4: Some ITCL compatibility stuff
#
#

if {[info command "@scope"] != {}} {
    rename update __update

    proc update {args} {
	@scope :: eval __update $args
    }

    rename tkwait __tkwait

    proc tkwait {args} {
	@scope :: eval __tkwait $args
    }
}
