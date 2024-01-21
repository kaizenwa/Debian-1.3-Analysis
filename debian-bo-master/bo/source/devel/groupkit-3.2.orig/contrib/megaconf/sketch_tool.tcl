
itcl_class SketchTool {
  inherit GKTool
  
  method makeWindow {} {
    pack [canvas $win.c -bg white] 
    bind $win.c <1> "stuff dragx %x; stuff dragy %y"
    bind $win.c <B1-Motion> "$this draw %x %y"
    pack [frame $win.tray -bg grey]
    foreach i [list red blue green black eraser] {addPen $i}
    selectPen red
  }

  method addPen {which} {
    label $win.tray.$which -bitmap [bitmap $which] -fg [penColor $which] \
       -borderwidth 2 -relief sunken -bg grey
    bind $win.tray.$which <1> "$this selectPen $which"
    pack $win.tray.$which -side left -padx 5
  }

  method penColor {which} {
    if {$which=="eraser"} {return black} else {return $which}
  }

  method bitmap {which} {  global gk_library
    if {$which=="eraser"} {return @$gk_library/library/bitmaps/megaconf/eraser.xbm} else {return @$gk_library/library/bitmaps/megaconf/capped.xbm}
  }

  method cursor {which} {  global gk_library
    if {$which=="eraser"} {
      return [list @$gk_library/library/bitmaps/megaconf/erase_curs.xbm $gk_library/library/bitmaps/megaconf/erase_mask.xbm black white]
    } else {
      return [list @$gk_library/library/bitmaps/megaconf/uncapped.xbm $gk_library/library/bitmaps/megaconf/uncap_mask.xbm $which white]
    }
  }

  method selectPen {which} {
    if {$pen!=""} {
      $win.tray.$pen configure -fg [penColor $pen]
    }
    set pen $which
    $win.tray.$pen configure -fg grey
    $win.c configure -cursor [cursor $which]
  }

  method draw {x y} {
    gk_toAll $this dodraw [stuff dragx] [stuff dragy] $x $y $pen
    stuff dragx $x ; stuff dragy $y
  }

  method dodraw {x0 y0 x1 y1 color} {
    if {$color!="eraser"} {
      $win.c create line $x0 $y0 $x1 $y1 -fill $color
    } else {
      $win.c delete [$win.c find overlapping $x0 $y0 $x1 $y1]
    }
  }

  method save {prefix} {
    foreach line [$win.c find all] {
      eval $prefix dodraw [$win.c coords $line] [$win.c itemcget $line -fill]
    }
  }

  protected pen ""
}
