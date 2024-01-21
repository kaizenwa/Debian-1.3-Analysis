/*************************************************************************
 *  TinyFugue - programmable mud client
 *  Copyright (C) 1993, 1994 Ken Keys
 *
 *  TinyFugue (aka "tf") is protected under the terms of the GNU
 *  General Public License.  See the file "COPYING" for details.
 ************************************************************************/
/* $Id: special.c,v 33000.2 1994/04/20 00:31:31 hawkeye Exp $ */


/**********************************************************************
 * Entry point for triggers, portals, watchdog, and quiet login.
 * Written by Ken Keys.
 **********************************************************************/

#include "config.h"
#include <ctype.h>
#include "port.h"
#include "tf.h"
#include "util.h"
#include "world.h"
#include "socket.h"
#include "macro.h"
#include "history.h"    /* is_suppressed() */
#include "commands.h"   /* handle_dc_command() */
#include "special.h"

extern int restrict;
extern Sock *xsock;

static int  FDECL(keep_quiet,(char *what));
static int  FDECL(handle_portal,(char *what));

static int keep_quiet(what)
    char *what;
{
    if (!xsock->numquiet) return FALSE;
    if (!cstrncmp(what, "Use the WHO command", 19) ||
      !cstrncmp(what, "### end of messages ###", 23)) {
        xsock->numquiet = 0;
    } else (xsock->numquiet)--;
    return TRUE;
}

void special_hook(aline)
    Aline *aline;
{
    if (borg || hilite || gag)
        if (find_and_run_matches(aline->str, 0, aline))
            background_hook(aline->str);

    if (keep_quiet(aline->str))    aline->attrs |= F_GAG;
    if (is_suppressed(aline->str)) aline->attrs |= F_GAG;
    if (handle_portal(aline->str)) aline->attrs |= F_GAG;
}

static int handle_portal(what)
    char *what;
{
    smallstr name, address, port;
    STATIC_BUFFER(buffer);
    World *world;

    if (!bamf) return(0);
    if (sscanf(what,
        "#### Please reconnect to %64[^ @]@%64s (%*64[^ )]) port %64s ####",
        name, address, port) != 3)
            return 0;
    if (restrict >= RESTRICT_WORLD) {
        tfputs("% bamfing is restricted.", tferr);
        return 0;
    }

    if (bamf == 1) {
        Sprintf(buffer, 0, "@%s", name);
        world = fworld();
        world = new_world(buffer->s, world->character, world->pass,
            address, port, world->mfile, "");
        world->flags |= WORLD_TEMP;
    } else if (!(world = find_world(name))) {
        world = new_world(name, "", "", address, port, "", "");
        world->flags |= WORLD_TEMP;
    }

    do_hook(H_BAMF, "%% Bamfing to %s", "%s", name);
    if (bamf != 2) handle_dc_command("");
    if (!opensock(world, TRUE, FALSE))
        tfputs("% Connection through portal failed.", tferr);
    return 1;
}
