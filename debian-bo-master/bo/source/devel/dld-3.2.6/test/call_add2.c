#include "dld.h"
    
int x;

/*
 *  Dynamically link in "add2.o", which defines the function "add2".
 *  Invoke "add2" to increment the global variable "x" defined *here*.
 */
main (argc, argv)
int argc;
char *argv[];
{
    register void (*func) ();

    /* required initialization. */
    (void) dld_init (argv[0]);

    x = 1;
    printf ("global variable x = %d\n", x);

    dld_link ("add2.o");

    /* grap the entry point for function "add2" */
    func = (void (*) ()) dld_get_func ("add2");

    /* invoke "add2" */
    (*func) ();
    printf ("global variable x = %d\n", x);
    if (3 != x) dld_perror("value not modified");
    return 3==x ? 0 : 1;
}
