#
# This is so you can test the distribution without installing it.
#
if [file exists ../library] {
    set blt_library ../library
}

lappend auto_path $blt_library

#
# Try to import the blt namespace into the global scope.  If it
# fails, we'll assume BLT was loaded into the global scope.
#
catch { 
    import add blt 
}

#
# Convenient "quit" key
#
bind all <Control-KeyPress-c> { exit 0 } 
focus .

#
# Replace Tk widgets with tiling widgets
#

set tileCmds { button checkbutton radiobutton frame label scrollbar toplevel }

if { [info commands "namespace"] == "namespace" } {
    namespace ::tk 
    namespace blt {
	foreach cmd $tileCmds  {
	    rename ::$cmd ::tk::$cmd
	    rename ::blt::tile$cmd ::$cmd
	}
    }
} else {
    foreach cmd $tileCmds  {
	rename $cmd ""
	rename tile$cmd $cmd
    }
}
