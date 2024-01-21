proc About {} {
    return "Testing ScrolledListBox"
}

proc Test {} {
    set w [tixScrolledListBox .l]
    pack $w

    foreach item {{1 1} 2 3 4 5 6} {
	$w subwidget listbox insert end $item
    }

    testevent $w ButtonPress -button 1 -x 30 -y 30


}


## MAIN_BEGIN

source ../library/TestLib.tcl
Test
Done

## MAIN_END
