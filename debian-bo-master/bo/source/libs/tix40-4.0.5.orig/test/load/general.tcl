# This file tests the pixmap image reader
#

proc About {} {
    return "This file performs general test on Tix w/ Tk 4.1 dynamic loading"
}

proc Test {} {
    test {load ../../unix-tk4.1/libtix.so Tix}
    test {tixComboBox .c}
    test {pack .c}
}

## MAIN_BEGIN

source ../library/TestLib.tcl
Test
Done

## MAIN_END
