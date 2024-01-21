# EMACS bindings for the text widget
global tkBind tkText tk_strictMotif

# first get base bindings
tkBindSource text/base.tcl

# define EMACS model bindings

bind Text <Control-a> {
  tkTextSetCursor %W [tkTextPlaceHome %W +]
}
bind Text <Control-b> {
  tkTextSetCursor %W [tkTextPlaceChar %W -]
}
bind Text <Control-d> {
  tkTextDelete %W insert [tkTextPlaceChar %W +] $tkBind(delSel) 0
}
bind Text <Control-e> {
  tkTextSetCursor %W [tkTextPlaceEnd %W +]
}
bind Text <Control-f> {
  tkTextSetCursor %W [tkTextPlaceChar %W +]
}
bind Text <Control-g> {tkTextKeyCancel %W}
bind Text <Control-h> {
  tkTextDelete %W insert [tkTextPlaceChar %W -] $tkBind(delSel) 0
}
bind Text <Control-k> {tkTextKillLine %W}
bind Text <Control-l> {tkTextCenterLine %W insert}
bind Text <Control-n> {
  set pos [tkTextPlaceLine %W +]
  tkTextSetCursor %W $pos
  set tkText(%W,prevPos) $pos
}
bind Text <Control-o> {
  tkTextInsertChar %W \n
  %W mark set insert insert-1c
}
bind Text <Control-p> {
  set pos [tkTextPlaceLine %W -]
  tkTextSetCursor %W $pos
  set tkText(%W,prevPos) $pos
}
bind Text <Control-t> {tkTextTranspose %W}
bind Text <Control-v> {
  tkTextSetCursor %W [tkTextScrollPages %W +]
}
bind Text <Control-w> { tkTextCut %W }
bind Text <Control-y> { tkTextYank %W }
bind Text <Control-z> { wm iconify [winfo toplevel %W] }
bind Text <Control-slash> {tkTextUndo %W}
bind Text <Control-underscore> {tkTextUndo %W}
bind Text <Control-space> {tkTextSetMark %W insert}
bind Text <Select> {tkTextSetMark %W insert}

bind Text <F16> { tkTextCopy %W }
bind Text <F20> { tkTextCut %W }
bind Text <F18> { tkTextYank %W }

# State key bindings

bind Text <Control-q> {
  tkBindSetStateKey %W TextCQ C-q
}
bind TextCQ <KeyPress> {
  if {[lsearch $tkBind(modKeys) %K] > -1} break
  tkTextInsertChar %W %A
  set $tkBind(%W,mesgvar) {}
}
bind TextCQ <ButtonPress> { set $tkBind(%W,mesgvar) {} }

bind Text <Control-x> {
  tkBindSetStateKey %W TextCX C-x
}
bind TextCX <KeyPress> {
  if {[lsearch $tkBind(modKeys) %K] > -1} break
  set $tkBind(%W,mesgvar) "C-x [tkBindGetMod %s]%K not bound."
  eval $tkBind(bell)
}
bind TextCX <ButtonPress> {
  set $tkBind(%W,mesgvar) "C-x [tkBindGetMod %s]mouse-%b not bound."
  eval $tkBind(bell)
}
bind TextCX <KeyPress-f> { tkTextSetFillCol %W }
bind TextCX <KeyPress-h> { tkTextMarkRegion %W }
bind TextCX <KeyPress-u> { tkTextUndo %W }
bind TextCX <Control-g> { tkTextKeyCancel %W }
bind TextCX <Control-o> { tkTextEatLines %W }
bind TextCX <Control-x> { tkTextExchangeMark %W }
bind TextCX <Control-e> { tkTextEvalSel %W }

if $tkBind(useEsc) {
  bind Text <Escape> {
    tkBindSetStateKey %W TextEsc Esc-
  }
}
bind TextEsc <KeyPress> {
  if {[lsearch $tkBind(modKeys) %K] > -1} break
  set $tkBind(%W,mesgvar) "ESC [tkBindGetMod %s]%K not bound."
  eval $tkBind(bell)
}
bind TextEsc <ButtonPress> {
  set $tkBind(%W,mesgvar) "ESC [tkBindGetMod %s]mouse-%b not bound."
  eval $tkBind(bell)
}

bind TextEsc <KeyPress-b> {
  tkTextSetCursor %W [tkTextPlaceWord %W -]
}
bind TextEsc <KeyPress-d> {
  tkTextDelete %W insert [tkTextPlaceWord %W +] 0 1
}
bind TextEsc <KeyPress-f> {
  tkTextSetCursor %W [tkTextPlaceWord %W +]
}
bind TextEsc <KeyPress-h> {tkTextMarkPara %W}
bind TextEsc <KeyPress-q> {tkTextFormatPara %W}
bind TextEsc <KeyPress-v> {
  tkTextSetCursor %W [tkTextScrollPages %W -]
}
bind TextEsc <KeyPress-w> { tkTextCopy %W }
bind TextEsc <KeyPress-y> {tkTextYankPop %W}
bind TextEsc <KeyPress-space> {tkTextEatSpace %W 1}
bind TextEsc <KeyPress-backslash> {tkTextEatSpace %W}
bind TextEsc <KeyPress-braceright> {
  tkTextSetCursor %W [tkTextPlacePara %W +]
}
bind TextEsc <KeyPress-braceleft> {
  tkTextSetCursor %W [tkTextPlacePara %W -]
}
bind TextEsc <KeyPress-less> {
  tkTextSetCursor %W 1.0
}
bind TextEsc <KeyPress-greater> {
  tkTextSetCursor %W end-1c
}
bind TextEsc <KeyPress-BackSpace> {
  tkTextDelete %W insert [tkTextPlaceWord %W -] 0 1
}
bind TextEsc <KeyPress-Delete> {
  tkTextDelete %W insert [tkTextPlaceWord %W -] 0 1
}
bind TextEsc <Control-g> {tkTextKeyCancel %W}

# Special bindings to numeric keys for arguments
for {set n 0} {$n < 10} {incr n} {
  bind Text <KeyPress-$n> {tkTextNumKey %W %A}
  bind TextEsc <KeyPress-$n> {tkBindArgKey %W %A}
}
bind Text <KeyPress-minus> {tkTextNumKey %W -}
bind TextEsc <KeyPress-minus> {tkBindArgKey %W -}

# Meta key bindings
if {![catch "bind Text <$tkBind(meta)-b>"]} {

  bind Text <$tkBind(meta)-b> {
    tkTextSetCursor %W [tkTextPlaceWord %W -]
  }
  bind Text <$tkBind(meta)-d> {
    tkTextDelete %W insert [tkTextPlaceWord %W +] 0 1
  }
  bind Text <$tkBind(meta)-f> {
    tkTextSetCursor %W [tkTextPlaceWord %W +]
  }
  bind Text <$tkBind(meta)-h> {tkTextMarkPara %W}
  bind Text <$tkBind(meta)-q> {tkTextFormatPara %W}
  bind Text <$tkBind(meta)-v> {
    tkTextSetCursor %W [tkTextScrollPages %W -]
  }
  bind Text <$tkBind(meta)-w> {tkTextCopy %W}
  bind Text <$tkBind(meta)-y> {tkTextYankPop %W}
  bind Text <$tkBind(meta)-space> {tkTextEatSpace %W 1}
  bind Text <$tkBind(meta)-backslash> {tkTextEatSpace %W}
  bind Text <$tkBind(meta)-braceright> {
    tkTextSetCursor %W [tkTextPlacePara %W +]
  }
  bind Text <$tkBind(meta)-braceleft> {
    tkTextSetCursor %W [tkTextPlacePara %W -]
  }
  bind Text <$tkBind(meta)-less> {
    tkTextSetCursor %W 1.0
  }
  bind Text <$tkBind(meta)-greater> {
    tkTextSetCursor %W end-1c
  }
  bind Text <$tkBind(meta)-BackSpace> {
    tkTextDelete %W insert [tkTextPlaceWord %W -] 0 1
  }
  bind Text <$tkBind(meta)-Delete> {
    tkTextDelete %W insert [tkTextPlaceWord %W -] 0 1
  }

  # Special bindings to numeric keys for arguments
  for {set n 0} {$n < 10} {incr n} {
    bind Text <$tkBind(meta)-KeyPress-$n> {tkBindArgKey %W %A}
  }
  bind Text <$tkBind(meta)-minus> {tkBindArgKey %W -}

}

