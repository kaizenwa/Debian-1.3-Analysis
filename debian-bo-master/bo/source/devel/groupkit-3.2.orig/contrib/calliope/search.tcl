# searching

proc Find {w} {
    set findWindow .find
    catch {removeFind $findWindow}
    toplevel $findWindow
    wm title $findWindow "Search in [wm title [winfo toplevel $w]]"
    pack [frame $findWindow.f1]
    pack [label $findWindow.f1.l -width 10 -text "Search for:"] -side left
    pack [entry $findWindow.f1.e -width 30 -relief sunken] -side left
    bind $findWindow.f1.e <Return> "DoFind $w $findWindow.f1.e"
    pack [frame $findWindow.f2] -side right
    pack [button $findWindow.f2.b2 -text "Cancel" -command "removeFind $findWindow"] -side right
    pack [button $findWindow.f2.b1 -text "Search" -command "DoFind $w $findWindow.f1.e"] \
        -side right
    focus $findWindow.f1.e
}

proc DoFind {w findWindow} {
    set findString [$findWindow get]
    grouptextSearch $w $findString
}

proc FindAgain {w} {
    grouptextDoSearch $w
}

proc removeFind {w} {
    destroy $w
}



