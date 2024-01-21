
itcl_class ChatTool {
  inherit GKTool

  method makeWindow {} {
    pack [frame $win.buffer] -side top  
    pack [text $win.buffer.txt -width 40 -height 15 -yscroll "$win.buffer.scroll set" -state disabled] -side left
    pack [scrollbar $win.buffer.scroll -command "$win.buffer.txt yview"] -side left -fill y 
    pack [text $win.enter -width 42 -height 3] -side top
    bind $win.enter <Return> "$this sendText"
  }

  method sendText {} {
    gk_toAll $this addComment "[users local.username]: [$win.enter get 1.0 end]"
    $win.enter delete 1.0 end
  } 

  method addComment {text} {
    $win.buffer.txt config -state normal
    $win.buffer.txt insert end $text
    $win.buffer.txt yview -pickplace end
    $win.buffer.txt config -state disabled
  }
}

