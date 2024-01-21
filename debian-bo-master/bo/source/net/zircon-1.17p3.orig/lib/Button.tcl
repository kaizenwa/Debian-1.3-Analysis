#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/Button.tcl,v $
# $Date: 1996/06/04 08:36:45 $
# $Revision: 1.17.1.1 $
#
# ----------------------------------------------------------------------
#   AUTHOR:  Lindsay Marshall <lindsay.marshall@newcastle.ac.uk>
# ----------------------------------------------------------------------
# Copyright 1995 The University of Newcastle upon Tyne (see COPYRIGHT)
# ======================================================================
#
proc mbproc {name op arg} {
    if [string match cget $op] {
	switch -- [lindex $arg 0] {
	    {-menu} { return $name.menu }
	    {-indicatoron} { return 0 }
	}
    }
    return [eval mb$name $op $arg]
}
#
proc buttonmenu {name args} {
    eval button $name $args
    rename $name mb$name
    proc $name {op args} "return \[mbproc $name \$op \$args\]"
    bind $name <Destroy> {
	catch {rename %W {}}
	catch {rename mb%W {}}
    }
    bind $name <Enter> {tkMbEnter %W ; continue}
    bind $name <Leave> {tkMbLeave %W ; continue}
    bind $name <Button-2> [bind Menubutton <Button-1>]
    bind $name <ButtonRelease-2> [bind Menubutton <ButtonRelease-1>]
    return $name
} 
