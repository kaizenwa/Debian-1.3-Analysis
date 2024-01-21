
gk_initApplication

gk_newenv persinfo

gk_createServer persinfo 8888 ""


proc gk_persist_startRecording confnum {  global gk_confinfo
  dp_SetCheckCmd [rpcFile] gk_persist_record
  persinfo incoming.[rpcFile] $confnum
}

proc _gk_eval args { 
  foreach i $args {
    if {[lindex $i 0]=="gk_persist_startRecording"} {
      persinfo incoming.[rpcFile] [lindex $i 1]
      persinfo data.[lindex $i 1] ""
    } elseif {$i=="gk_persist_stopRecording"} {
      persinfo delete incoming.[rpcFile]
    } else {
      set conf [persinfo incoming.[rpcFile]]
      if {$conf!=""} {
	set buffer [persinfo data.$conf]
	lappend buffer $i
        persinfo data.$conf $buffer
      }
    }
  }
}

proc gk_persist_playBack {confnum} { 
  eval dp_RDO [rpcFile] _gk_eval [persinfo data.$confnum]
}


vwait xx
