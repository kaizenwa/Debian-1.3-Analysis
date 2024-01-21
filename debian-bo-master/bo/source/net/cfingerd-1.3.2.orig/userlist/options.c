/*
 * USERLIST
 * Command line options
 */

#include "userlist.h"
#include "proto.h"

int display_type;

void give_help(void)
{
    printf("\nUSERLIST command line options\n\n");
    printf("A number of options selects the emulated OS display type for finger output.\n\n");
/*
    printf("\t-b\tGive BSD/OS-type finger output\n");
*/
    printf("\t-c\tGive standard CFINGERD (custom) output\n");
/*
    printf("\t-g\tGive GNU-FINGER display output\n");
    printf("\t-l\tGive standard LINUX finger output (as if there is any)\n");
    printf("\t-r\tGive standard STRICT RFC1288 compilant output\n");
    printf("\t-u\tGive Unix System V finger output\n\n");
*/
    printf("Userlist for CFINGERD version 1.2.1\n\n");

    fflush(stdout);
    exit(1);
}

void handle_options(int argc, char *argv[])
{
    if (!strncmp(argv[1], "-c", 2)) {
	display_type = DISPLAY_CFINGERD;
	return;
    }

    if (argc > 1)
	give_help();
}
