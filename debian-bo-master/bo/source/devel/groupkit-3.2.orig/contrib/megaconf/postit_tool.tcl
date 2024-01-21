itcl_class PostitTool {
  inherit GKTool

  method makeWindow {} {
    pack [text $win.text -bg LightGoldenrod1 -width 20 -height 7 -wrap word]
    bind $win.text <Any-KeyPress> "$this broadcast"
  }
  
  method broadcast {} {
    gk_toOthers $this setvalue [$win.text get 1.0 end]
  }

  method setvalue {val} {
    $win.text delete 1.0 end
    $win.text insert end $val
  }

  method save {prefix} {
    eval $prefix setvalue [list [$win.text get 1.0 end]]
  }
}

