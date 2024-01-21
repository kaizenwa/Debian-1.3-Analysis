/* Copyright (c) 1993 by Sanjay Ghemawat */
#include "basic.h"
#include "cal_tcl.h"
#include "ical.h"
#include "item_tcl.h"

/*
 * Notice Creator
 *
 *	notice <name>
 */

int Cmd_CreateNotice(ClientData, Tcl_Interp* tcl, int argc, char* argv[]) {
    if (argc != 1) {
	TCL_Error(tcl, "invalid arguments to notice");
    }

    Item_Tcl* item = new Item_Tcl(tcl, new Notice, 0);
    TCL_Return(tcl, (char*) item->handle());
}

/*
 * Appointment Creator
 *
 *	appointment <name>	-- Returns <name>
 */

int Cmd_CreateAppt(ClientData, Tcl_Interp* tcl, int argc, char* argv[]) {
    if (argc != 1) {
	TCL_Error(tcl, "invalid arguments to appointment");
    }

    Item_Tcl* item = new Item_Tcl(tcl, new Appointment, 0);
    TCL_Return(tcl, (char*) item->handle());
}

/*
 * Calendar Creator
 *
 *	calendar <name> <filename>	-- Returns <name>
 */

int Cmd_CreateCalendar(ClientData, Tcl_Interp* tcl, int argc, char* argv[]) {
    if (argc != 3) {
	TCL_Error(tcl, "invalid arguments to calendar");
    }

    Calendar_Tcl* c = new Calendar_Tcl(tcl, argv[1], argv[2]);
    if (!c->error())
	TCL_Return(tcl, "");

    Tcl_SetResult(tcl, (char*)c->error_msg(), TCL_VOLATILE);
    delete c;
    return TCL_ERROR;
}
