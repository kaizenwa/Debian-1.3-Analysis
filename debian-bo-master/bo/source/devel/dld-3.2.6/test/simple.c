#include "dld.h"
    
main (argc, argv)
int argc;
char *argv[];
{
    void (*func) ();
    
    (void) dld_init (argv[0]);

    printf ("Hello world from %s\n", argv[0]);
    func = (void (*) ()) dld_get_func ("printf");
    if (func) {
      (*func) ("Hello world from %s\n", argv[0]);
      return 0;
    }
    dld_perror("dld_get_func (\"printf\")\n");
    return 1;
}
