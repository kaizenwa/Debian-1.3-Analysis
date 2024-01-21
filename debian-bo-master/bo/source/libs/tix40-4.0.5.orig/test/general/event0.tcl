proc About {} {
    return "Testing the event emulation routines in the test suite"
}

proc Test {} {
    global foo
    set foo 0

    # Clicking a button
    #
    button .b -command "set foo 1"
    pack .b; update

    Click .b

    Assert {$foo == 1}

if {[tix platform] == "unix"} {
    Wait 200

    # Selecting a listbox
    #
    tixHList .l
    pack .l; update
    .l add 1 -text Hi
    .l add 2 -text Hi
    .l add 3 -text Hi
    .l add 4 -text Hi
    .l add 5 -text Hi

    Click .l 5 5
    Assert {[.l info selection] == "1"}
}
    Wait 200
    # Drag and selecting a combobox
    #
    tixComboBox .c
    .c insert end 10
    .c insert end 10
    .c insert end 10
    .c insert end 10
    .c insert end 10
    pack .c; update

    HoldDown [.c subwidget arrow]
    Drag [.c subwidget listbox] 10 10
    Release [.c subwidget listbox] 10 10
    Release [.c subwidget arrow] -30 30

    Assert {[.c cget -value] == "10"}

if {[tix platform] == "unix"}  {
    Wait 200
    # Double-clicking
    #
    tixDirList .d
    pack .d

    update
    Double [.d subwidget hlist] 5 5
    Assert {[.d cget -value] == "/"}


    update
}

    Wait 200
    #
    # A more sophisticated test: the -listcmd option of ComboBox
    UserMessage Testing the -listcmd of ComboBox

    global counter
    set counter 0
    .c config -listcmd "ListCmd .c"
    
    Click [.c subwidget arrow]
    Wait 100
    Assert {$counter == 1}
    Click [.c subwidget arrow]
    Wait 100

    Click [.c subwidget arrow]
    Wait 100
    Click [.c subwidget arrow]
    Wait 100
    Assert {$counter == 2}


    Assert {[.c subwidget listbox get 0] == "0"}
    Assert {[.c subwidget listbox get 1] == "1"}
    Assert {[.c subwidget listbox get 2] == "2"}

}

proc ListCmd {w} {
    global counter

    incr counter

    $w subwidget listbox delete 0 end
    $w subwidget listbox insert end 0
    $w subwidget listbox insert end 1
    $w subwidget listbox insert end 2
}

## MAIN_BEGIN

source ../library/TestLib.tcl
Test
Done

## MAIN_END
