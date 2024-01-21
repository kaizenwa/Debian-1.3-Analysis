proc mfv:file-to-var { filename vname } {
  global mfp
  upvar $vname var

  if [catch "open $filename r" fid] {
    set mfp(last-error) $fid
    return 1
  }
  set var [read $fid]
  close $fid
  return 0
}

proc mfv:file-to-text { filename tw ndx {prefix ""} } {
  global mfp

  if [catch "open $filename r" fid] {
    set mfp(last-error) $fid
    return 1
  }

  if {[$tw compare end <= 2.0]} {
    tkTextUndoSetup $tw
    set cmd "$tw insert"
  } else {
    set cmd "tkTextInsert $tw"
  }
  $tw mark set insert $ndx
  if [string length $prefix] {
    while {![eof $fid]} {
      eval "$cmd insert $prefix\[gets $fid\]\n"
    }
  } else {
    eval "$cmd insert \[read $fid\]"
  }
  close $fid
  return 0
}

proc mfv:var-to-file { vname filename {overwrite 0} } {
  global mfp
  upvar $vname var

  if $overwrite {set mode w} else {set mode a}
  if [catch "open $filename $mode" fid] {
    set mfp(last-error) $fid
    return 1
  }

  puts -nonewline $fid $var
  close $fid

  return 0
}

proc mfv:text-to-file { tw filename {overwrite 0} {start 1.0} {stop end}} {
  global mfp

  if ![string length $filename] {return 1}

  if $overwrite {set mode w} else {set mode a}
  if [catch "open $filename $mode" fid] {
    set mfp(last-error) $fid
    return 1
  }

  puts -nonewline $fid [$tw get $start $stop]
  close $fid

  return 0
}

proc mfv:var-to-folder { vname folder } {
  upvar $vname var
  set file [mfv_util tmpfile tkmail]
  if [mfv:var-to-file var $file] { return 1 }
  return [mfv:move-folder $file $folder 1]
}

proc mfv:text-to-folder { tw folder {start 1.0} {stop end}} {
  set file [mfv_util tmpfile tkmail]
  if [mfv:text-to-file $tw $file 0 $start $stop] { return 1 }
  return [mfv:move-folder $file $folder 1]
}

proc mfv:move-folder {folder1 folder2 {forcerm 0}} {
  global mfp

  if $mfp(debug) { puts stderr "Appending $folder1 to $folder2" }

  if [catch "mfv_open -id tempmove $folder2" fid] {
    set mfp(last-error) $fid
    if $forcerm { exec rm -f $folder1 }
    return 1
  }

  set stat 0
  set corrupt 0

  if [catch "$fid append $folder1" res] {
    set mfp(last-error) "$res\n"
    set stat 1
    if {[string first "folderID" $res] > 0} {
      set corrupt 1
    }
  }

  if {$fid == "tempmove"} {
    mfv_close $fid
  } elseif !$corrupt {
    # update any viewers that have the record folder open
    foreach viewer $mfp(viewlist) {
      if {$fid == [keylget mfp($viewer) fid]} {
	if $corrupt {
	  mfv:reset-viewer $viewer
	} else {
	  mfv:reset-summary $viewer
	}
      }
    }
  }

  if {!$stat || $forcerm} {
    exec rm -f $folder1
  }
 
  return $stat
}


