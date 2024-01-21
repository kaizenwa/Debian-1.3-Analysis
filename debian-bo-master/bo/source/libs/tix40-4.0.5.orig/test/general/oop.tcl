proc About {} {
    return "Testing OOP features"
}

proc Test {} {
    test {tix} {arg}
    test {tixWidgetClass} {arg}
    test {tixClass} {arg}
    test {tixNoteBook} {arg}
    test {tixAppContext} {arg}
}


## MAIN_BEGIN

source ../library/TestLib.tcl
Test
Done

## MAIN_END
