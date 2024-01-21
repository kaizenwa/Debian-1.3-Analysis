/*
 * USERLIST
 * version 1.0.0
 *
 * by Ken Hollis for CFINGERD
 *
 * Released under the GPL for public/private use.
 */

#include "userlist.h"
#include "proto.h"

void main(int argc, char *argv[])
{
    initialize_userlist();

    if (argc > 1)
	handle_options(argc, argv);

    process_display();
}
