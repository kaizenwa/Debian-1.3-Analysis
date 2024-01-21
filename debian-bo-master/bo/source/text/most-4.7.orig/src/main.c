#include "config.h"

# include <stdio.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include "most.h"

int main(int argc, char *argv[])
{
   most_initialize_most ();
   return most (argc, argv);
}


