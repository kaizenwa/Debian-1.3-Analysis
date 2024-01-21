# EMACS model bindings for entry widget
global tkBind tkEntry tk_strictMotif

# first get base bindings
tkBindSource entry/base.tcl

bind Entry <Control-a> {
  tkEntrySetCursor %W 0
}
bind Entry <Control-b> {
  tkEntrySetCursor %W [tkEntryPlaceChar %W -]
}
bind Entry <Control-d> {
  tkEntryDelete %W insert [tkEntryPlaceChar %W +] $tkBind(delSel) 0
}
bind Entry <Control-e> {
  tkEntrySetCursor %W end
}
bind Entry <Control-f> {
  tkEntrySetCursor %W [tkEntryPlaceChar %W +]
}
bind Entry <Control-g> {tkEntryKeyCancel %W}
bind Entry <Control-h> {
  tkEntryDelete %W insert [tkEntryPlaceChar %W -] $tkBind(delSel) 0
}
bind Entry <Control-k> {
  tkEntryDelete %W insert end 0 1
}
bind Entry <Control-t> {
  tkEntryTranspose %W
}
bind Entry <Control-w> { tkEntryCut %W }
bind Entry <Control-y> { tkEntryYank %W }
bind Entry <Control-underscore> {tkEntryUndo %W}
bind Entry <Control-slash> {tkEntryUndo %W}
bind Entry <Control-space> {tkEntrySetMark %W insert}
bind Entry <Select> {tkEntrySetMark %W insert}

bind Entry <F16> { tkEntryCopy %W }
bind Entry <F20> { tkEntryCut %W }
bind Entry <F18> { tkEntryYank %W }

# State key bindings

bind Entry <Control-q> {
  tkBindSetStateKey %W EntryCQ C-q
}
bind EntryCQ <KeyPress> {
  if {[lsearch $tkBind(modKeys) %K] > -1} break
  tkEntryInsertChar %W %A
  set $tkBind(%W,mesgvar) {}
}
bind EntryCQ <ButtonPress> { set $tkBind(%W,mesgvar) {} }

bind Entry <Control-x> {
  tkBindSetStateKey %W EntryCX C-x
}
bind EntryCX <KeyPress> {
  if {[lsearch $tkBind(modKeys) %K] > -1} break
  set $tkBind(%W,mesgvar) "C-x [tkBindGetMod %s]%K not bound."
  eval $tkBind(bell)
}
bind EntryCX <ButtonPress> {
  set $tkBind(%W,mesgvar) "C-x [tkBindGetMod %s]mouse-%b not bound."
  eval $tkBind(bell)
}
bind EntryCX <KeyPress-h> { tkEntrySelectAll %W }
bind EntryCX <KeyPress-u> { tkEntryUndo %W }
bind EntryCX <Control-g> { tkEntryKeyCancel %W }
bind EntryCX <Control-x> { tkEntryExchangeMark %W }
bind EntryCX <Control-e> { tkEntryEvalSel %W }

if $tkBind(useEsc) {
  bind Entry <Escape> {
    tkBindSetStateKey %W EntryEsc Esc-
  }
}
bind EntryEsc <KeyPress> {
  if {[lsearch $tkBind(modKeys) %K] > -1} break
  set $tkBind(%W,mesgvar) "ESC [tkBindGetMod %s]%K not bound."
  eval $tkBind(bell)
}
bind EntryEsc <ButtonPress> {
  set $tkBind(%W,mesgvar) "ESC [tkBindGetMod %s]mouse-%b not bound."
  eval $tkBind(bell)
}

bind EntryEsc <KeyPress-b> {
  tkEntrySetCursor %W [tkEntryPlaceWord %W -]
}
bind EntryEsc <KeyPress-d> {
  tkEntryDelete %W insert [tkEntryPlaceWord %W +] 0 1
}
bind EntryEsc <KeyPress-f> {
  tkEntrySetCursor %W [tkEntryPlaceWord %W +]
}
bind EntryEsc <KeyPress-w> { tkEntryCopy %W }
bind EntryEsc <KeyPress-y> {tkEntryYankPop %W}

bind EntryEsc <KeyPress-BackSpace> {
  tkEntryDelete %W insert [tkEntryPlaceWord %W -] 0 1
}
bind EntryEsc <KeyPress-Delete> {
  tkEntryDelete %W insert [tkEntryPlaceWord %W -] 0 1
}
bind EntryEsc <Control-g> {tkEntryKeyCancel %W}

# Special bindings to numeric keys for arguments
for {set n 0} {$n < 10} {incr n} {
  bind Entry <KeyPress-$n> {tkEntryNumKey %W %A}
  bind EntryEsc <KeyPress-$n> {tkBindArgKey %W %A}
}
bind Entry <KeyPress-minus> {tkEntryNumKey %W -}
bind EntryEsc <KeyPress-minus> {tkBindArgKey %W -}

# Meta key bindings
if {![catch "bind Entry <$tkBind(meta)-b>"]} {
  bind Entry <$tkBind(meta)-b> {
    tkEntrySetCursor %W [tkEntryPlaceWord %W -]
  }
  bind Entry <$tkBind(meta)-d> {
    tkEntryDelete %W insert [tkEntryPlaceWord %W +] 0 1
  }
  bind Entry <$tkBind(meta)-f> {
    tkEntrySetCursor %W [tkEntryPlaceWord %W +]
  }
  bind Entry <$tkBind(meta)-w> {tkEntryCopy %W}
  bind Entry <$tkBind(meta)-y> {tkEntryYankPop %W}
  bind Entry <$tkBind(meta)-BackSpace> {
    tkEntryDelete %W insert [tkEntryPlaceWord %W -] 0 1
  }
  bind Entry <$tkBind(meta)-Delete> {
    tkEntryDelete %W insert [tkEntryPlaceWord %W -] 0 1
  }

  # Special bindings to numeric keys for arguments
  for {set n 0} {$n < 10} {incr n} {
    bind Entry <$tkBind(meta)-KeyPress-$n> {tkBindArgKey %W %A}
  }
  bind Entry <$tkBind(meta)-minus> {tkBindArgKey %W -}

}


