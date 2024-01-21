/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Exim gets and frees all its store through these functions, which in
principle could do clever things, but currently don't. They are called via
macros store_malloc and store_free, which add the tracing information that is
used when debugging is turned on.

For checking out very obscure store over-writing problems, there's an alternate
set of functions which get a large chunk of store and continually check that
freed store is not written to. I wrote them when chasing one particular bug and
it seems sensible to save the code for future use. */

/* #define CHECK_STORE */


#include "exim.h"


#ifndef CHECK_STORE

static int sequence = 0;   /* For tracing */


/*************************************************
*                  Get store                     *
*************************************************/

/* Running out of store is a total disaster for exim. Some malloc functions
do not run happily on very small sizes, nor do they document this fact. This
function is called via the macro store_malloc().

Arguments:
  size        amount of store wanted
  filename    source file from which called
  linenumber  line number in source file

Returns:      pointer to gotten store (panic on failure)
*/

void *
store_malloc_3(size_t size, char *filename, int linenumber)
{
void *yield;

if (size < 16) size = 16;
yield = malloc(size);

if (yield == NULL)
  log_write(0, LOG_MAIN|LOG_PANIC_DIE, "failed to get %d bytes of memory: "
    "called from line %d of %s", size, linenumber, filename);

if (debug_trace_memory)
  {
  debug_printf("%08x %04d %5d (%-14s %4d)\n", yield, sequence++, size,
    filename, linenumber);
  }

return yield;
}


/************************************************
*             Free store                        *
************************************************/

/* This function is called by the macro store_free()

Arguments:
  block       block of store to free
  filename    source file from which called
  linenumber  line number in source file

Returns:      nothing
*/

void
store_free_3(void *block, char *filename, int linenumber)
{
if (debug_trace_memory)
  {
  debug_printf("%08x %04d  free (%-14s %4d)\n", block, sequence++,
    filename, linenumber);
  }
free(block);
}



/************************************************
*************************************************
*          Alternate, debugging functions       *
*************************************************
************************************************/

#else

#define CSIZE 1000
static char *checks[CSIZE];
static char *files[CSIZE];
static char *freefiles[CSIZE];
static int lines[CSIZE];
static int freelines[CSIZE];
static int lengths[CSIZE];
static int checkptr = 0;

static char *my_store = NULL;
static char *last_given;


/*************************************************
*          Check freed store is not written to   *
*************************************************/

static void
store_check(char *s, char *filename, int linenumber)
{
int i;
for (i = 0; i < checkptr; i++)
  {
  int j;
  int len = lengths[i];
  unsigned char *p = checks[i];

  if (len >= 0) continue;

  for (j = 0; j < -len; j++)
    {
    if (p[j] != 0xaa)
      {
      printf("!!! Corruption !!!\n");
      printf("detected in %s from %s line %d\n", s, filename, linenumber);
      printf("block %d length %d file %s line %d\n",
        checks[i], -len, files[i], lines[i]);
      printf("free file %s line %d\n", freefiles[i], freelines[i]);
      for (j = 0; j < -len; j++) printf("%d ", p[j]);
      printf("\n");
      printf("most recent successful get was %s line %d\n",
        files[checkptr-1], lines[checkptr-1]);
      exit(1);
      }
    }
  }
}



/*************************************************
*             Get and check store                *
*************************************************/

void *
store_malloc_3(size_t size, char *filename, int linenumber)
{
void *yield;
store_check("get", filename, linenumber);

if (size < 16) size = 16;
size = (size + 7) & (-8);

if (my_store == NULL)
  {
  my_store = malloc(200 * 1024);
  if (my_store == NULL)
    {
    printf("Failed to get chunk\n");
    exit(1);
    }
  last_given = my_store + 200 * 1024;
  }

last_given -= size;
if (last_given < my_store)
  {
  printf(">>>> Not enough store\n");
  exit(1);
  }
yield = last_given;

if (checkptr < CSIZE)
  {
  checks[checkptr] = (char *)yield;
  files[checkptr] = filename;
  lines[checkptr] = linenumber;
  lengths[checkptr++] = size;
  }
else
  {
  printf(">>> Not enough slots\n");
  exit(1);
  }

return yield;
}


/************************************************
*             "Free" store                      *
************************************************/

/* Actually just marks it free in the list */

void
store_free_3(void *block, char *filename, int linenumber)
{
int i;

store_check("free", filename, linenumber);

for (i = 0; i < checkptr; i++)
  {
  if (checks[i] == (char *)block)
    {
    unsigned char *p = checks[i];
    int len = lengths[i];
    int j;
    for (j = 0; j < len; j++) p[j] = 0xaa;

    lengths[i] = - lengths[i];
    freefiles[i] = filename;
    freelines[i] = linenumber;
    break;
    }
  }

if (i >= checkptr)
  printf("Freeing block %d - not in gotten list\n", block);
}

#endif

/* End of store.c */
