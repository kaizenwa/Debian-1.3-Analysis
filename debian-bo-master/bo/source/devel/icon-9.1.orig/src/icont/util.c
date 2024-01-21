/*
 *  util.c -- general utility functions.
 */

#include <ctype.h>
#include "../h/gsupport.h"
#include "tproto.h"
#include "tglobals.h"
#include "tree.h"


extern int optindex;

extern char *ofile;


/*
 * Information about Icon functions.
 */

/*
 * Number of arguments.
 */


/*
 * Names of Icon functions.
 */
char *ftable[] = {
#define FncDef(p,n) Lit(p),
#define FncDefV(p) Lit(p),
#include "../h/fdefs.h"
#undef FncDef
#undef FncDefV
   };

int ftbsize = sizeof(ftable)/sizeof(char *);

/*
 * tcalloc - allocate and zero m*n bytes
 */
pointer tcalloc(m,n)
unsigned int m, n;
   {
   pointer a;

#if SCCX_MX
   /* Avoids compiler warning */
   if ( (a = calloc(m,n)) == 0 )
#else
   if (!(a = calloc(m,n)))
#endif					/* SCCX_MX */

      quit("out of memory");
   return a;
   }

/*
 * trealloc - realloc a table making it half again larger and zero the
 *   new part of the table.
 */
pointer trealloc(table, tblfree, size, unit_size, min_units, tbl_name)
pointer table;      /* table to be realloc()ed */
pointer tblfree;    /* reference to table free pointer if there is one */
unsigned int *size; /* size of table */
int unit_size;      /* number of bytes in a unit of the table */
int min_units;      /* the minimum number of units that must be allocated. */
char *tbl_name;     /* name of the table */
   {
   word new_size;
   word num_bytes;
   word free_offset;
   word i;
   char *new_tbl;

   new_size = (*size * 3) / 2;
   if (new_size - *size < min_units)
      new_size = *size + min_units;
   num_bytes = new_size * unit_size;

#if IntBits == 16
    {
    word max_bytes = 64000;

    if (num_bytes > max_bytes) {
       new_size = max_bytes / unit_size;
       num_bytes = new_size * unit_size;
       if (new_size - *size < min_units)
          quitf("out of memory for %s", tbl_name);
       }
    }
#endif					/* IntBits == 16 */

   if (tblfree != NULL)
      free_offset = DiffPtrs(*(char **)tblfree,  (char *)table);

#if SCCX_MX
   /* Avoids compiler warning */
   if ( (new_tbl = (char *)realloc(table, (unsigned)num_bytes)) == 0 )
#else
   if (!(new_tbl = (char *)realloc(table, (unsigned)num_bytes)))
#endif					/* SCCX_MX */

      quitf("out of memory for %s", tbl_name);

   for (i = *size * unit_size; i < num_bytes; ++i)
      new_tbl[i] = 0;

   *size = new_size;
   if (tblfree != NULL)
      *(char **)tblfree = (char *)(new_tbl + free_offset);

   return (pointer)new_tbl;
   }


/*
 * round2 - round an integer up to the next power of 2.
 */
unsigned int round2(n)
unsigned int n;
   {
   unsigned int b = 1;
   while (b < n)
      b <<= 1;
   return b;
   }


