# BASE bindings for entries
global tkBind tkEntry tk_strictMotif

# Standard Motif bindings:

bind Entry <Button> {
  if {!(%b == 1 || %b == 3)} break
  tkEntryButton%b %W %x
  %W selection clear
}
bind Entry <B1-Motion> {
  set tkPriv(x) %x
  tkEntryMouseSelect %W %x
}
bind Entry <B3-Motion> {
  set tkPriv(x) %x
  tkEntryMouseSelect %W %x
}
bind Entry <Double-Button> {
  if {!(%b == 1 || %b == 3)} break
  set tkPriv(selectMode) word
  tkEntryMouseSelect %W %x
  if {%b == 1} { catch {%W icursor sel.first} }
}
bind Entry <Triple-Button> {
  if {!(%b == 1 || %b == 3)} break
  set tkPriv(selectMode) line
  tkEntryMouseSelect %W %x
  if {%b == 1} { %W icursor 0 }
}
bind Entry <Shift-Button> {
  if {!(%b == 1 || %b == 3)} break
  set tkPriv(selectMode) char
  %W selection adjust @%x
}
bind Entry <Double-Shift-Button> {
 if {!(%b == 1 || %b == 3)} break
  set tkPriv(selectMode) word
  tkEntryMouseSelect %W %x
}
bind Entry <Triple-Shift-Button> {
  if {!(%b == 1 || %b == 3)} break
  set tkPriv(selectMode) line
  tkEntryMouseSelect %W %x
}
bind Entry <B1-Leave> {
  set tkPriv(x) %x
  tkEntryAutoScan %W
}
bind Entry <B1-Enter> {
  tkCancelRepeat
}
bind Entry <B3-Leave> {
  set tkPriv(x) %x
  tkEntryAutoScan %W
}
bind Entry <B3-Enter> {
  tkCancelRepeat
}
bind Entry <ButtonRelease> {
  if {!(%b == 1 || %b == 3)} break
  tkCancelRepeat
  if {[selection own -displayof %W] == "%W" &&
      [%W selection present]} {
    clipboard clear -displayof %W
    clipboard append -displayof %W [selection get -displayof %W]
  }
}
bind Entry <Control-1> {
  %W icursor @%x
}

if !$tk_strictMotif {
  bind Entry <2> {
    %W scan mark %x
    set tkPriv(x) %x
    set tkPriv(y) %y
    set tkPriv(mouseMoved) 0
  }
  bind Entry <B2-Motion> {
    if {abs(%x-$tkPriv(x)) > 2} {
      set tkPriv(mouseMoved) 1
    }
    if $tkPriv(mouseMoved) {
      %W scan dragto %x
    }
  }
  bind Entry <ButtonRelease-2> {
    if !$tkPriv(mouseMoved) {tkEntryButtonInsert %W %x}
  }
}

# Standard key binding

bind Entry <Left> {
  tkEntrySetCursor %W [tkEntryPlaceChar %W -]
}
bind Entry <Right> {
  tkEntrySetCursor %W [tkEntryPlaceChar %W +]
}
bind Entry <Shift-Left> {
  tkEntryKeySelect %W [tkEntryPlaceChar %W -]
}
bind Entry <Shift-Right> {
  tkEntryKeySelect %W [tkEntryPlaceChar %W +]
}
bind Entry <Control-Left> {
  tkEntrySetCursor %W [tkEntryPlaceWord %W -]
}
bind Entry <Control-Right> {
  tkEntrySetCursor %W [tkEntryPlaceWord %W +]
}
bind Entry <Shift-Control-Left> {
  tkEntryKeySelect %W [tkEntryPlaceWord %W -]
}
bind Entry <Shift-Control-Right> {
  tkEntryKeySelect %W [tkEntryPlaceWord %W +]
}
bind Entry <Home> {
  tkEntrySetCursor %W 0
}
bind Entry <Shift-Home> {
  tkEntryKeySelect %W 0
}
bind Entry <End> {
  tkEntrySetCursor %W end
}
bind Entry <Shift-End> {
  tkEntryKeySelect %W end
}

bind Entry <Delete> {
  tkEntryDelete %W insert [tkEntryPlaceChar %W +] $tkBind(delSel) 0
}
bind Entry <BackSpace> {
  tkEntryDelete %W insert [tkEntryPlaceChar %W -] $tkBind(delSel) 0
}

bind Entry <Control-backslash> {tkEntryKeyCancel %W}

bind Entry <Control-i> {
  tkEntryInsertChar %W \t
}
bind Entry <Insert> {
  set tkEntry(%W,ovwrt) [expr !$tkEntry(%W,ovwrt)]
  if $tkEntry(%W,ovwrt) {
    set $tkBind(%W,mesgvar) "Entering overwrite mode."
  } else {
    set $tkBind(%W,mesgvar) "Leaving overwrite mode."
  }
}
bind Entry <KeyPress> {
  if [string length %A] {
    tkEntryInsertChar %W %A
  } else {
    tkBindNoBind %W %K %s
  }
}

# Ignore all Alt, Meta, and Control keypresses unless explicitly bound.
# Otherwise, if a widget binding for one of these is defined, the
# <KeyPress> class binding will also fire and insert the character,
# which is wrong.  Ditto for Escape, Return, and Tab.

bind Entry <Alt-KeyPress> {# nothing}
bind Entry <Meta-KeyPress> {# nothing}
bind Entry <Control-KeyPress> {# nothing}
bind Entry <Escape> {# nothing}
bind Entry <Return> {# nothing}
bind Entry <KP_Enter> {# nothing}
bind Entry <Tab> {# nothing}

