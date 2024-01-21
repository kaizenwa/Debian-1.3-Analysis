#! /bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

set targets  [lindex $argv 0]
set argvfiles [lrange $argv 1 end]

set env(WAITTIME) 1000

set genDirs {
    general xpm hlist
}

set env(TCL_LIBRARY) 	$env(TEST_TCL_LIBRARY)
set env(TK_LIBRARY) 	$env(TEST_TK_LIBRARY)
set env(ITCL_LIBRARY) 	$env(TEST_ITCL_LIBRARY)
set env(ITK_LIBRARY) 	$env(TEST_ITK_LIBRARY)

catch {
    unset env(TIX_DEBUG_INTERACTIVE)
}

set load(bin) ../../tk4.1/unix/wish
set tk40(bin) ../unix-tk4.0/tixwish
set tk41(bin) ../unix-tk4.1/tixwish
set itcl(bin) ../unix-itcl2.0/itixwish

set errCount 0


foreach t $targets {
    upvar #0 $t target

    puts "Testing the application programming interface of $target(bin)"
    eval exec $target(bin) APITest.tcl all >@ stdout 2>@ stderr

    puts "Testing target $t with executable $target(bin)"
    eval exec $target(bin) Driver.tcl $t $argvfiles >@ stdout 2>@ stderr
}


#----------------------------------------------------------------------
#
# Here is the reporting section
#
#----------------------------------------------------------------------
puts "$errCount errors discovered"
