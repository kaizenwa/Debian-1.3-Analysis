# BASE bindings for the text widget
global tkBind tkText tk_strictMotif

# Mouse bindings

bind Text <Button> {
  if {!(%b == 1 || %b == 3)} break
  tkTextButton%b %W %x %y
  %W tag remove sel 0.0 end
}
bind Text <B1-Motion> {
  set tkPriv(x) %x
  set tkPriv(y) %y
  tkTextSelectTo %W %x %y
}
bind Text <B3-Motion> {
  set tkPriv(x) %x
  set tkPriv(y) %y
  tkTextSelectTo %W %x %y
}
bind Text <Double-Button> {
  if {!(%b == 1 || %b == 3)} break
  set tkPriv(selectMode) word
  tkTextSelectTo %W %x %y
  if {%b == 1} { catch {%W mark set insert sel.first} }
}
bind Text <Triple-Button> {
  if {!(%b == 1 || %b == 3)} break
  set tkPriv(selectMode) line
  tkTextSelectTo %W %x %y
  if {%b == 1} { catch {%W mark set insert sel.first} }
}
bind Text <Shift-Button> {
  if {!(%b == 1 || %b == 3)} break
  tkTextResetAnchor %W @%x,%y
  set tkPriv(selectMode) char
  tkTextSelectTo %W %x %y
}
bind Text <Double-Shift-Button>	{
  if {!(%b == 1 || %b == 3)} break
  set tkPriv(selectMode) word
  tkTextSelectTo %W %x %y
}
bind Text <Triple-Shift-Button> {
  if {!(%b == 1 || %b == 3)} break
  set tkPriv(selectMode) line
  tkTextSelectTo %W %x %y
}
bind Text <B1-Leave> {
  set tkPriv(x) %x
  set tkPriv(y) %y
  tkTextAutoScan %W
}
bind Text <B1-Enter> {
  tkCancelRepeat
}
bind Text <B3-Leave> {
  set tkPriv(x) %x
  set tkPriv(y) %y
  tkTextAutoScan %W
}
bind Text <B3-Enter> {
  tkCancelRepeat
}
bind Text <ButtonRelease> {
  if {!(%b == 1 || %b == 3)} break
  tkCancelRepeat
  if {[selection own -displayof %W] == "%W" &&
      [string length [%W tag nextrange sel 1.0 end]]} {
    clipboard clear -displayof %W
    clipboard append -displayof %W [selection get -displayof %W]
  }
}

if !$tk_strictMotif {
  bind Text <2> {
    %W scan mark %x %y
    set tkPriv(x) %x
    set tkPriv(y) %y
    set tkPriv(mouseMoved) 0
  }
  bind Text <B2-Motion> {
    if {(abs(%x - $tkPriv(x)) > 4) || (abs(%y - $tkPriv(y)) > 4)} {
      set tkPriv(mouseMoved) 1
    }
    if $tkPriv(mouseMoved) {
      %W scan dragto %x %y
    }
  }
  bind Text <ButtonRelease-2> {
    if !$tkPriv(mouseMoved) {tkTextButtonInsert %W %x %y}
  }
}

# Standard key bindings

bind Text <Left> {
  tkTextSetCursor %W [tkTextPlaceChar %W -]
}
bind Text <Right> {
  tkTextSetCursor %W [tkTextPlaceChar %W +]
}
bind Text <Up> {
  set pos [tkTextPlaceLine %W -]
  tkTextSetCursor %W $pos
  set tkText(%W,prevPos) $pos
}
bind Text <Down> {
  set pos [tkTextPlaceLine %W +]
  tkTextSetCursor %W $pos
  set tkText(%W,prevPos) $pos
}
bind Text <Shift-Left> {
  tkTextKeySelect %W [tkTextPlaceChar %W -]
}
bind Text <Shift-Right> {
  tkTextKeySelect %W [tkTextPlaceChar %W +]
}
bind Text <Shift-Up> {
  set pos [tkTextPlaceLine %W -]
  tkTextKeySelect %W $pos
  set tkText(%W,prevPos) $pos
}
bind Text <Shift-Down> {
  set pos [tkTextPlaceLine %W +]
  tkTextKeySelect %W $pos
  set tkText(%W,prevPos) $pos
}
bind Text <Control-Left> {
  tkTextSetCursor %W [tkTextPlaceWord %W -]
}
bind Text <Control-Right> {
  tkTextSetCursor %W [tkTextPlaceWord %W +]
}
bind Text <Control-Up> {
  tkTextSetCursor %W [tkTextPlacePara %W -]
}
bind Text <Control-Down> {
  tkTextSetCursor %W [tkTextPlacePara %W +]
}
bind Text <Shift-Control-Left> {
  tkTextKeySelect %W [tkTextPlaceWord %W -]
}
bind Text <Shift-Control-Right> {
  tkTextKeySelect %W [tkTextPlaceWord %W +]
}
bind Text <Shift-Control-Up> {
  tkTextKeySelect %W [tkTextPlacePara %W -]
}
bind Text <Shift-Control-Down> {
  tkTextKeySelect %W [tkTextPlacePara %W +]
}
bind Text <Prior> {
  tkTextSetCursor %W [tkTextScrollPages %W -]
}
bind Text <Shift-Prior> {
  tkTextKeySelect %W [tkTextScrollPages %W -]
}
bind Text <Next> {
  tkTextSetCursor %W [tkTextScrollPages %W +]
}
bind Text <Shift-Next> {
  tkTextKeySelect %W [tkTextScrollPages %W +]
}
bind Text <Control-Prior> {
  %W xview scroll [tkBindDefArg %W -] page
}
bind Text <Control-Next> {
  %W xview scroll [tkBindDefArg %W +] page
}

bind Text <Home> {
  tkTextSetCursor %W [tkTextPlaceHome %W +]
}
bind Text <Shift-Home> {
  tkTextKeySelect %W [tkTextPlaceHome %W +]
}
bind Text <End> {
  tkTextSetCursor %W [tkTextPlaceEnd %W +]
}
bind Text <Shift-End> {
  tkTextKeySelect %W [tkTextPlaceEnd %W +]
}
bind Text <Control-Home> {
  tkTextSetCursor %W 1.0
}
bind Text <Control-Shift-Home> {
  tkTextKeySelect %W 1.0
}
bind Text <Control-End> {
  tkTextSetCursor %W {end - 1 char}
}
bind Text <Control-Shift-End> {
  tkTextKeySelect %W {end - 1 char}
}

bind Text <Tab> {
  tkTextInsertChar %W \t
  focus %W
  break
}
bind Text <Shift-Tab> {
  # Needed only to keep <Tab> binding from triggering;  doesn't
  # have to actually do anything.
}
bind Text <Control-Tab> {
  focus [tk_focusNext %W]
}
bind Text <Control-Shift-Tab> {
  focus [tk_focusPrev %W]
}
bind Text <Control-i> {
  tkTextInsertChar %W \t
}
bind Text <Return> {
  tkTextInsertChar %W \n
  set tkText(%W,prevCmd) NewLine
}
bind Text <Delete> {
  tkTextDelete %W insert [tkTextPlaceChar %W +] $tkBind(delSel) 0
}
bind Text <BackSpace> {
  tkTextDelete %W insert [tkTextPlaceChar %W -] $tkBind(delSel) 0
}

bind Text <Control-backslash> {tkTextKeyCancel %W}

bind Text <Insert> {
  set tkText(%W,ovwrt) [expr !$tkText(%W,ovwrt)]
  if $tkText(%W,ovwrt) {
    set $tkBind(%W,mesgvar) "Entering overwrite mode."
  } else {
    set $tkBind(%W,mesgvar) "Leaving overwrite mode."
  }
}
bind Text <KeyPress> {
  if [string length %A] {
    tkTextInsertChar %W %A
  } else {
    tkBindNoBind %W %K %s
  }
}

bind Text <F10> { #pass thru to possible menu }

# Ignore all Alt, Meta, and Control keypresses unless explicitly bound.
# Otherwise, if a widget binding for one of these is defined, the
# <KeyPress> class binding will also fire and insert the character,
# which is wrong.  Ditto for <Escape>.

bind Text <Alt-KeyPress> {tkBindNoBind %W %K %s}
bind Text <Meta-KeyPress> {tkBindNoBind %W %K %s}
bind Text <Control-KeyPress> {tkBindNoBind %W %K %s}
bind Text <Escape> {tkBindNoBind %W %K %s}

# Motif Model 2 Edit bindings
bind Text <Control-z> {tkTextUndo %W}
bind Text <Control-c> { 
  if {[selection own -displayof %W] == "%W"} { tkTextCopy %W }
}
bind Text <Control-x> { 
  if {[selection own -displayof %W] == "%W"} {
    tkTextDelete %W sel.first sel.last 1 1 
  }
}
bind Text <Control-v> { tkTextYank %W }
bind Text <Control-slash> {tkTextMarkRegion %W}

# Sun keys
bind Text <F16> { 
  if {[selection own -displayof %W] == "%W"} { tkTextCopy %W }
}
bind Text <F20> { 
  if {[selection own -displayof %W] == "%W"} {
    tkTextDelete %W sel.first sel.last 1 1 
  }
}
bind Text <F18> { tkTextYank %W }
