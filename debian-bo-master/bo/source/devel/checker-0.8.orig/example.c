#include <stdlib.h>
#include <stddef.h>

int
main ()
{
 char *zone = malloc (20);
 char *ptr = NULL;
 int i;
 char c;
 
 c = zone[1];		/* error: read an uninitialized char */
 c = zone[-2];		/* error: read before the zone */
 zone[25] = ' ';	/* error: write after the zone */
 *ptr = 2;		/* error: use a NULL pointer, must produce a core */
}
 