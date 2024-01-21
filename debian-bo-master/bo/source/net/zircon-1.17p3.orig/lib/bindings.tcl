#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/bindings.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
proc doBindings {entry chid chan} {
    doInfoBindings $entry $chid
    doChanBindings $entry $chid $chan
}
#
proc winsrt {win str} {notIdle $win ; tkEntryInsert $win $str}
#
proc ifSel {win cmd} {
    notIdle $win
    if {![catch {selection get} bf] && {} != $bf} {eval "$cmd {[cleanup $bf]}"}
}
#
proc notNull {win cmd} {
    if {[set x [$win get]] != {} ||
      (![catch {selection get} x] && $x != {})} { eval "$cmd {$x}" }
    $win delete 0 end
    notIdle $win
}
#
proc doChanBindings {entry chid chan} {
    global zircon
    set net [$chid net]
    bind $entry <Meta-b> {winsrt %W \002 ; break}
    bind $entry <Meta-o> {winsrt %W \017 ; break}
    bind $entry <Meta-v> {winsrt %W \026 ; break}
    bind $entry <Meta-u> {winsrt %W \037 ; break}
    bind $entry <Meta-s> {winsrt %W $smiley ; break}
    bind $entry <Shift-Meta-S> {winsrt %W $scowl}
    bind $entry <Control-Meta-s> {winsrt %W $wink}
    bind $entry <Control-c> "
	notIdle %W; $chid send \[toMorse \[%W get\]\] ; %W delete 0 end
    "
    bind $entry "<$zircon(action)>" "
	notIdle %W ; $chid action \[%W get\]; %W delete 0 end ; break
    "
    bind $entry <Control-p> "%W delete 0 end ; winsrt %W \[$chid getPrev\]"
    bind $entry <Up> [bind $entry <Control-p>]
    bind $entry <Control-n> "%W delete 0 end ; winsrt %W \[$chid getNext\]"
    bind $entry <Down> [bind $entry <Control-n>]
    bind $entry <Control-Return> "
	notIdle %W ; doNotice $net $chan \[%W get\] ; %W delete 0 end
    "
    foreach b [$net bindings] {
	bind $entry [lindex $b 0] "notIdle %W ; [lindex $b 1]"
    }
    foreach b [$chid bindings] {
	bind $entry [lindex $b 0] "notIdle %W ; [lindex $b 1]"
    }
}
#
proc doInfoBindings {entry chid} {
    global zircon
    set net [$chid net]
    bind $entry <Control-u> {notIdle %W ; %W delete 0 insert}
    bind Entry <Control-w> {
	notIdle %W
	set txt [%W get]
	set idx [%W index insert]
	%W delete [string wordstart $txt $idx] [string wordend $txt $idx]
    }
    bind $entry <Meta-n> "ifSel %W {handleURL $net}"
    bind $entry <Meta-j> "ifSel %W {channelJoin $net}"
    bind $entry <Meta-m> {ifSel %W {Message :: make}}
    bind $entry <Meta-f> "ifSel %W {finger $net}"
    bind $entry <Meta-q> "oneLiner %W $chid"
    bind $entry <Meta-w> "notNull %W {$net WHOIS} ; break"
    bind $entry <Shift-Meta-W> "notNull %W {$net send WHO}"
    bind $entry <Control-g> {winsrt %W \007}
    bind $entry <Escape> "notIdle %W ; $chid makePalette"
    bind $entry <Meta-Return> "notIdle %W ; $net doMisc2 $chid %W"
    bind $entry <Shift-Meta-Return> "
	notIdle %W ; $chid send \[strrev \[%W get\]\] ; %W delete 0 end
    "
    bind $entry <Control-Meta-Return> "
	notIdle %W ; $chid send \[rot13 \[%W get\]\] ; %W delete 0 end
    "
    bind $entry <ButtonRelease-2> "notIdle %W ; $chid insertSelect ; break"
    if !$zircon(ircIImode) {
	bind $entry <Return> "
	    notIdle %W ; $chid send \[%W get\] ; %W delete 0 end
	"
    } {
	bind $entry <Return> "notIdle %W ; $net doMisc2 $chid %W"
    }
}
