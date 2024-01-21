#include <stdlib.h>
#include "dld.h"
    
int x;

/*
 *  Dynamically link in "add1.o", which defines the function "add1".
 *  Invoke "add1" to increment the global variable "x" defined *here*.
 */
main (argc, argv)
int argc;
char *argv[];
{
    register void (*func) ();

    /* required initialization. */
    (void) dld_init (dld_find_executable(argv[0]));

    x = 1;
    printf ("global variable x = %d\n", x);

    dld_link ("add1.o");

    /* grap the entry point for function "add1" */
    func = (void (*) ()) dld_get_func ("add1");

    /* invoke "add1" */
    if (func) (*func) ();
    printf ("global variable x = %d\n", x);
    if (2 != x) dld_perror("value not modified");
    return 2==x ? 0 : 1;
}
