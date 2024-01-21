#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/Entry.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
#
# Make an entry with some emacs-like edit keys......
#
proc emacsInsertSelect {ent} {
    if {[normal $ent] && ![catch {selection get} bf] && $bf != {}} {
	$ent insert insert $bf
	tkEntrySeeInsert $ent
    }
}
#
proc emacsTInsertSelect {ent} {
    if {[normal $ent] && ![catch {selection get} bf] && $bf != {}} {
	$ent insert insert $bf	
	$ent see insert
    }
}
#
proc emacsEntry {name args} {
    eval entry $name -relief sunken $args
#   bind $name <ButtonRelease-2> {notIdle %W ; emacsInsertSelect %W ; break}
    bind $name <Delete> {notIdle %W ; tkEntryBackspace %W ; break}
    bind $name <Control-u> {notIdle %W ; %W delete 0 insert }
    bind $name <Meta-b> {notIdle %W ; tkEntryInsert %W \002 ; %W icursor insert}
    bind $name <Meta-o> {notIdle %W ; tkEntryInsert %W \017}
    bind $name <Meta-u> {notIdle %W ; tkEntryInsert %W \037}
    bind $name <Meta-v> {notIdle %W ; tkEntryInsert %W \026}
    return $name
}
#
proc emacsTEntry {name args} {
    eval text $name -relief sunken -height 1 -wrap none -setgrid 0 $args
    bind $name <ButtonRelease-2> {notIdle %W ; emacsTInsertSelect %W ; break}
    bind $name <Control-u> {notIdle %W ; %W delete 1.0 insert }
    bind $name <Delete> "[bind Text <BackSpace>] ; break"
    bind $name <Meta-b> {notIdle %W ; tkEntryInsert %W \002 ; %W icursor insert}
    bind $name <Meta-o> {notIdle %W ; %W insert insert \017}
    bind $name <Meta-u> {notIdle %W ; %W insert insert \037}
    bind $name <Meta-v> {notIdle %W ; %W insert insert \026}
    return $name
}
#
proc entrySet {win val} { ${win} delete 0 end ; ${win} insert end $val }
#
proc labelEntry {t name opts init code} {
    frame $name
    eval label $name.label $opts
    [expr {$t ? "emacsTEntry" : "emacsEntry"}] $name.entry -relief sunken
    $name.entry insert end $init
    pack $name.label -side left
    pack $name.entry -side left -expand 1 -fill x
    bind $name.entry <Return> "notIdle %W ; $code"
    bind $name.entry <BackSpace> [bind $name.entry <Delete>]
    bind $name.entry <Control-h> [bind $name.entry <Delete>]
}
