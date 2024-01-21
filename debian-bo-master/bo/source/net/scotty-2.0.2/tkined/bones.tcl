#! /usr/local/bin/scotty -inf
##
## Command to examine bones objects from within tkined.
##
## Copyright (c) 1993, 1994
##
## J. Schoenwaelder
## TU Braunschweig, Germany
## Institute for Operating Systems and Computer Networks
##
## Permission to use, copy, modify, and distribute this
## software and its documentation for any purpose and without
## fee is hereby granted, provided that this copyright
## notice appears in all copies.  The University of Braunschweig
## makes no representations about the suitability of this
## software for any purpose.  It is provided "as is" without
## express or implied warranty.
##

if {[info commands msqlconnect] == ""} {
    ined acknowledge \
	"This scotty version is not able to talk to a msql server."
    exit
}

LoadDefaults bones

##
## The global variable msqlserver is used to identify the host 
## that provides the database service.
##

if {[info exists default(server)]} {
    set msqlserver $default(server)
}

##
## Display the underlying tables of the bones database.
##

proc "Table Definitions" { list } {
    global db
    set txt "SQL tables of bones database on server [msqlinfo $db host?]:\n"
    foreach table [msqlinfo $db tables] {
	append txt "\nCREATE TABLE $table (\n"
	foreach elem [msqlcol $db $table "name type length non_null prim_key"] {
	    set name     [lindex $elem 0]
	    set type     [lindex $elem 1]
	    set length   [lindex $elem 2]
	    if {[lindex $elem 3]} {
		set non_null " not null"
	    } else {
		set non_null ""
	    }
	    if {[lindex $elem 4]} {
		set prim_key " primary key"
	    } else {
		set prim_key ""
	    }
	    append txt [format "\t%-16s %s(%d)%s%s,\n" \
			$name $type $length $non_null $prim_key]
	}
	set txt [string trimright $txt "\n,"]
	append txt "\n)\n"
    }
    writeln $txt
}

##
## Edit a tuple in a table. Also used to insert new tuples.
##

proc EditTuple { table {tuple ""}} {
    global db

    set attributes ""
    set query ""
    foreach elem [msqlcol $db $table "name type length non_null prim_key"] {
        set name        [lindex $elem 0]
        set type($name) [lindex $elem 1]
	lappend attributes $name
	lappend labels "$name ($type($name))"
	lappend query $name
    }
    set query "insert into $table ([join $query ,]) "

    while 1 {
	set list ""
	set i 0
	foreach label $labels {
	    lappend list [list $label [lindex $tuple $i]]
	    incr i
	}
	set result [ined request "Insert a tuple into the table $table:" \
		    $list "insert cancel"]
	
	if {[lindex $result 0] != "insert"} {
	    return
	}
	set tuple [lrange $result 1 end]

	set i 0
	set quotedvalues ""
	foreach att $attributes {
	    if {$type($att) == "char"} {
		lappend quotedvalues "'[lindex $tuple $i]'" 
	    } else {
		lappend quotedvalues [lindex $tuple $i]
	    }
	    incr i
	}
	append query " values ([join $quotedvalues ,])"
	
	if {! [catch {msqlexec $db $query} msg]} {
	    return

	}
	ined acknowledge $msg
    }
}

##
## Delete a tuple from a table.
##

proc DeleteTuple { table tuple } {
    global db

    if {$tuple == ""} return

    foreach elem [msqlcol $db $table "name type"] {
        set name        [lindex $elem 0]
        set type($name) [lindex $elem 1]
	lappend attributes $name
    }

    set query "delete from $table where"
    set i 0
    foreach att $attributes {
	if {$i > 0} {
	    append query " and"
	}
	if {$type($att) == "char"} {
	    append query " $att = '[lindex $tuple $i]'"
	} else {
	    append query " $att = [lindex $tuple $i]"
	}
	incr i
    }

    if {[catch {msqlexec $db $query} msg]} {
	ined acknowledge $msg
    }
}

##
## Modify an already existing tuple in a table.
##

proc ModifyTable { table } {
    global db

    set attributes [msqlcol $db $table name]
    msqlsel $db "select [join $attributes ,] from $table \
	order by [lindex $attributes 0]"

    for {set row [msqlnext $db]} {$row != ""} {set row [msqlnext $db]} {
	lappend list $row
    }

    if {! [info exists list]} {
	set result [ined confirm "Table $table is empty." \
		    [list insert cancel]]
	if {[lindex $result 0] != "insert"} return
	EditTuple $table
	return
    }

    set result [ined list "Select a row of table $table:" \
		$list "edit insert delete cancel"]
    switch [lindex $result 0] {
	"" {
	    EditTuple $table [lindex $result 1]
	}
	edit {
	    EditTuple $table [lindex $result 1]
	}
	insert {
	    EditTuple $table
	}
	delete {
	    DeleteTuple $table [lindex $result 1]
	}
	default return
    }
}

##
##
##

proc "Modify Table" { list } {
    global db
    if {[catch {msqlinfo $db tables} tables]} {
	ined acknowledge "Failed to retrieve table names: $tables"
	return
    }

    set res [ined list "Select a relation:" $tables "select cancel"]
    if {[lindex $res 0] == "cancel"} {
	return
    }
    ModifyTable [lindex $res 1]
}

##
## Set the parameter that control bones server access.
##

proc "Set Parameter" { list } {

    global msqlserver

    if {![info exists msqlserver]} {
	set msqlserver ""
    }

    set result [ined request "Set bones SQL server:" \
		 [list [list "Server Host:" $msqlserver entry 20] ] \
		 [list "set values" cancel] ]

    if {[lindex $result 0] == "cancel"} return

    set msqlserver [lindex $result 1]
}

##
## Show the defaults as loaded from the tkined.defaults files.
##

proc "Show Defaults" {list} {
    ShowDefaults
}

##
## Display some help about this tool.
##

proc "Help Bones" {list} {
    ined browse "Help about Bones" {
	"Table Definitions" 
	"    Displays the SQL table definitions used by the bones database." 
	"" 
	"Modify Tuple" 
	"    Insert, edit or delete tuples from the bones database." 
	"" 
	"Set Parameter:" 
	"    This dialog allows you to set the server host running the" 
	"    msql database server." 
	"" 
	"Show Defaults:" 
	"    Show the defaults that may be defined in the tkined.defaults" 
	"    files. This script makes use of definitions in the form" 
	"" 
	"        bones.server: <string>" 
    }
}

##
## Delete the Tool and exit the interpreter.
##

proc "Delete Bones" {list} {
    global menus db
    catch {msqlclose $db}
    catch {
	foreach id $menus { ined delete $id }
    }
    exit
}

##
## Connect to the bones database.
##

if {![info exists msqlserver]} {
    "Set Parameter" dummy
}

if {[catch {
    set db [msqlconnect $msqlserver]
    msqluse $db bones
} msg]} {
    ined acknowledge "Failed to connect to bones database server: $msg"
    "Delete Bones" dummy
}

if {[catch {
    set tables [msqlinfo $db tables]
} msg]} {
    ined acknowledge "Failed to retrieve table names: $msg"
    "Delete Bones" dummy
}

##
## Create the menus by querying the server for defined classes.
##

set menus [ ined create MENU "Bones" \
    "Table Definitions" "Modify Table" "" \
    "Set Parameter" "Show Defaults" "" \
    "Help Bones" "Delete Bones" ]

