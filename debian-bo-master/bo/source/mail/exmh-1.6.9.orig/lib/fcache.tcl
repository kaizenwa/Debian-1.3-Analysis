#
# fcache.tcl
#
# Folder cache display - a smaller folder display of frequently visted folders.
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Fcache_Init {} {
    global fcache exmh subFolders
    global mhProfile flist
    set fcache(folders) {}

    if {[info exists exmh(newuser)] && [info exists subFolders]} {
	set N [llength [array names subFolders]]
	if {$N <= 2} {
	    set fcache(lines) 0
	} elseif {$N < 10} {
	    set fcache(lines) 1
	} else {
	    set fcache(lines) 2
	}
    }

    Preferences_Add "Folder Cache" \
"Exmh can maintain a cache of buttons for recently used folders. Set the cache size to 0 (zero) to disable this feature.  The cache appears as a second display below the main display of folder labels.
The cache is useful if you have lots of folders or a heavily nested folder structure.  If you only have a few lines of folder labels, the cache probably just wastes space." {
	{fcache(lines) fcacheLines 1 {Num cached folder lines}
"Exmh can maintain a cache of buttons for recently used folders.
Set the cache size to 0 (zero) to disable this feature.  The
cache appears as a second display below the main display of
folder labels.  The cache is useful if you have lots of folders
or a heavily nested folder structure.  If you only have a few
lines of folder labels, the cache probably just wastes space." }
	{fcache(sticky) fcacheSticky inbox {Permanently cached folders}
"Set this to the list of folders you always want in the cache." }
	{fcache(stickyOff) fcacheStickyOff OFF {No permanently cached folders}
"The only way to have no permanently cached folders is to set this option."}
        {fcache(nicknames) fcacheNickNames ON {Use nicknames in the display}
"Use nicknames in the folder cache display. Very convenient if your folder
structure is deeply nested." }
    }
    trace variable fcache(lines) w FcacheFixupLines
    trace variable fcache(sticky) w FcacheFixupSticky

    # Init the cache and handle various error cases.

    if {$fcache(stickyOff)} {
	# Pref_Add will give us "inbox" if the user trys to set
	# the stickly list to zero.  Need this hack.
	set fcache(sticky) {}
    }

    if [catch {source $mhProfile(path)/.exmhfcache} msg] {
	set fcache(folders) $fcache(sticky)
	set fcache(LRU) $fcache(sticky)
    }
    set fcache(enabled) [expr $fcache(lines) > 0]
    set fcache(lastLines) $fcache(lines)

    if ![info exists fcache(LRU)] {
	set fcache(LRU) $fcache(folders)
    }
    if {[llength $fcache(folders)] != [llength $fcache(LRU)]} {
	set fcache(LRU) $fcache(folders)
    }
    foreach folder $flist(allfolders) {
       Fcache_FolderName $folder
    }
    FcacheFixupLines nodisplay
}
proc Fcache_FolderName { folder } {
   global folderNickName nickNameFolders fcache

   if {! $fcache(nicknames)} {
      return $folder
   } elseif [info exists folderNickName($folder)] {
      return $folderNickName($folder)
   }

   set nickname {}
   foreach part [Misc_Reverse [split $folder "/"]] {
      if {"$nickname" == {}} {
	 set nickname "$part"
      } else {
	 set nickname "$part/$nickname"
      }
      if [info exists nickNameFolders($nickname)] {
	 # there is a potential clash
	 set folders $nickNameFolders($nickname)
	 if {[lsearch $folders $folder] >= 0} {
	    # we have already dealt with this folder before
	    if {[llength $folders] == 1} {
	       # no name clash
	       set folderNickName($folder) $nickname
	       return $nickname
	    }
	 } else {
	    # we're new here
	    lappend nickNameFolders($nickname) $folder
	    foreach clashingFolder $folders {
	       catch {unset folderNickName($clashingFolder)}
	       Fcache_FolderName $clashingFolder
	    }
	    return [Fcache_FolderName $folder]
	 }
      } else {
	 set nickNameFolders($nickname) [list $folder]
	 set folderNickName($folder) $nickname
	 return $nickname
      }
   }
   set folderNickName($folder) $folder
   return $folder
}
proc Fcache_CreateWindow {} {
    global fdisp fcache
    # Create the canvas for cache display
    set fdisp(cache) [canvas $fdisp(parent).cache -bd 2 -relief raised]

    set h [expr {$fcache(lines) * ($fdisp(itemHeight) + $fdisp(ygap)) + $fdisp(ygap)}]
    $fdisp(cache) configure -height $h

    pack append $fdisp(parent) $fdisp(cache) {bottom expand fill}
    bind $fdisp(cache) <Configure> {Fcache_Display 1}
}
proc FcacheFixupLines { args } {
    global exwin fcache
    set fcache(enabled) [expr {$fcache(lines) > 0}]
    if !$fcache(enabled) {
	if {$fcache(lastLines) > 0} {
	    global fdisp
	    catch {
		destroy $fdisp(cache)
		unset fdisp(cache)
	    }
	}
    } else {
	global fdisp
	set nodisplay [expr {[string compare $args nodisplay] == 0}]
	if {$fcache(lastLines) == 0 || ![info exists fdisp(cache)]} {
	    if {!$nodisplay} {
		Fcache_CreateWindow
	    }
	} elseif {$fcache(lastLines) != $fcache(lines)} {
	    set h [expr {$fcache(lines) * ($fdisp(itemHeight) + $fdisp(ygap)) + $fdisp(ygap)}]
	    $fdisp(cache) configure -height $h
	}
	if {!$nodisplay} {
	    if [FcacheLRU] {
		after 1 {Fcache_Display 1}
	    }
	}
    }
    set fcache(lastLines) $fcache(lines)
}
proc FcacheFixupSticky { args } {
    global exwin fcache
    set fcache(enabled) [expr {$fcache(lines) > 0}]
    if !$fcache(enabled) {
	return
    } elseif [llength $fcache(sticky)] {
	set fcache(stickyOff) 0
	foreach f $fcache(sticky) {
	    Fcache_Folder $f
	}
    } else {
	set fcache(stickyOff) 1
    }
}
proc Fcache_CheckPoint {} {
    global exmh fcache mhProfile
    if [catch {open $mhProfile(path)/.exmhfcache w} out] {
	return
    }
    puts $out [list set fcache(folders) $fcache(folders)]
    puts $out [list set fcache(LRU) $fcache(LRU)]
    close $out
}

#### Cache of recently used folder labels

proc Fcache_Folder { folder } {
    # Add a folder to the set of cached ones
    global fcache exmh fdisp
    if {$folder == {} || !$fcache(enabled)} {
	return
    }
    if {$folder == $exmh(folder)} {
	set fdisp(cur,cache) $folder
    }
    if {$folder == $exmh(target)} {
	set fdisp(tar,cache) $folder
    }
    set ix [lsearch $fcache(LRU) $folder]
    if {$ix < 0} {
	Exmh_Debug Fcache_Folder $folder
	if {$fcache(folders) == {}} {
	    set fcache(folders) $folder
	    set fcache(LRU) $folder
	} else {
	    lappend fcache(folders) $folder
	    lappend fcache(LRU) $folder
	    FcacheLRU
	}
	Fcache_Display
    } else {
	set fcache(LRU) [lreplace $fcache(LRU) $ix $ix]
	lappend fcache(LRU) $folder
    }
}
proc FcacheLRU {} {
    global fcache fdisp
    set changed 0
    while {[Fdisp_Lines $fdisp(cache) $fcache(folders)] > $fcache(lines)} {
	set hit 0
	foreach f $fcache(LRU) {
	    if {[lsearch $fcache(sticky) $f] < 0} {
		set ix [lsearch $fcache(LRU) $f]
		set fcache(LRU) [lreplace $fcache(LRU) $ix $ix]
		set ix [lsearch $fcache(folders) $f]
		set fcache(folders) [lreplace $fcache(folders) $ix $ix]
		set changed 1 ; set hit 1
		break
	    }
	}
	if {! $hit} {
	    # No room to accomodate all sticky folders
	    Exmh_Status "Too many sticky folders for cache"
	    break
	}
    }
    return $changed
}

proc Fcache_Display { {force 0} } {
    # Layout the cache of folder buttons
    global fcache
    if {$fcache(enabled)} {
	if {($fcache(folders) != {})} {
	    Fdisp_Layout cache $fcache(folders) {} $force
	    Fdisp_HighlightCanvas cache
	}
    }
}
proc Fcache_FolderDiscard { folder } {
    # Remove a folder to the set of cached ones
    global fcache exmh fdisp
    if {$folder == {} || !$fcache(enabled)} {
	return
    }
    if {$folder == $fdisp(cur,cache)} {
	set fdisp(cur,cache) {}
    }
    if {$folder == $fdisp(tar,cache)} {
	set fdisp(tar,cache) {}
    }
    set ix [lsearch $fcache(LRU) $folder]
    if {$ix >= 0} {
	Exmh_Debug Fcache_FolderDisard $folder
	set fcache(LRU) [lreplace $fcache(LRU) $ix $ix]
	set ix [lsearch $fcache(folders) $folder]
	set fcache(folders) [lreplace $fcache(folders) $ix $ix]
	Fcache_Display
    }
}
