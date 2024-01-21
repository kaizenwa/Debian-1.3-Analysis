/**
 * hsearch.c --- PD simple implementation of System V hsearch(3c) routine
 * It is by Arnold Robbins (arnold@skeeve.atl.ga.us) -- thanks!
**/

#include "stdio.h"
#include "localsys.h"
#include "hsearch.h"

static ELEMENT **Table = NULL;	/* pointer to dynamicly allocated table */
static int Num_elem = -1;	/* number of elements */

static int hashit();

/*
 * table of primes just below 2^n, n=2..31 for use in finding the right prime
 * number to be the table size.  this table may be generally useful...
 */

static unsigned int primetab[] = {
    /*
     * comment these out, so that table will always have a minimal size...
      3, 7, 13, 31, 61,
     */
    127, 251, 509, 1021, 2039, 4093, 8191, 16381, 32749, 65521, 131071,
    262139, 524287, 1048573, 2097143, 4194301, 8388593, 16777213, 33554393,
    67108859, 134217689, 268435399, 536870909, 1073741789, 2147483647
};

/* hcreate --- create a hash table at least how_many big */

int hcreate (how_many)
register unsigned int how_many;
{
    register int i, j;

    /*
     * find first prime number >= how_many, and use it for table size
     */

    if (Num_elem != -1)	/* already a table out there */
	hdestroy();	/* remove it */

    j = sizeof (primetab) / sizeof (primetab[0]);
    for (i = 0; i < j; i++)
	if (primetab[i] >= how_many)
	    break;

    if (i >= j)	/* how_many bigger than any prime we have, use it */
	Num_elem = how_many;
    else
	Num_elem = primetab[i];

    if ((Table = (ELEMENT **) calloc ((unsigned) Num_elem, sizeof (ELEMENT *))) == NULL)
	return (0);
    else
	return (1);
}

/* idestroy --- destroy a single element on a chain */

static void idestroy (elem)
ELEMENT *elem;
{
    if (elem != NULL)
    {
	idestroy (elem->next);
	free ((char *) elem);
    }
}

/* hdestroy --- nuke the existing hash table */

void hdestroy()
{
    register unsigned int i;

    if (Table != NULL)
    {
	/* free all the chains */
	for (i = 0; i < Num_elem; i++)
	    idestroy (Table[i]);

	/* now the table itself */
	free ((char *) Table);
	Num_elem = -1;
	Table = NULL;
    }
}

/* hsearch --- lookup or enter an item in the hash table */

ENTRY *hsearch (entry, action)
ENTRY entry;
ACTION action;
{
    ELEMENT e;
    ELEMENT *ep = NULL;
    ELEMENT *ep2 = NULL;
    int hindex;

    if (Table == NULL)
	return (NULL);

    hindex = hashit (entry.key);
    if (Table[hindex] == NULL)	/* nothing there */
    {
	if (action == FIND)
	    return (NULL);
	else
	{
	    /* add it to the table */
	    e.item = entry;
	    e.next = NULL;
	    if ((Table[hindex] = (ELEMENT *) calloc (1, sizeof (ELEMENT))) == NULL)
		return (NULL);
	    *Table[hindex] = e;
	    return (& Table[hindex]->item);
	}
    }
    else
    {
	/* something in bucket, see if already on chain */
	for (ep = Table[hindex]; ep != NULL; ep = ep->next)
	{
	    if (strcmp (ep->item.key, entry.key) == 0)
	    {
		if (action == ENTER) 
		    ep->item.data = entry.data;
		/* already there, just change data */
		/* or action was just find it */
		return (& ep->item);
	    }
		else
		    ep2 = ep;
	}
	/* at this point, item was not in table */
	/* ep2 points at last element on the list */
	if (action == ENTER)
	{
	    if ((ep2->next = (ELEMENT *) calloc (1, sizeof (ELEMENT))) == NULL)
		return (NULL);
	    ep2->next->item = entry;
	    ep2->next->next = NULL;
	    return (& ep2->next->item);
	}
	else
	    return (NULL);
    }
    /*NOTREACHED*/
}

/* hashit --- do the hashing algorithm */

/*
* algorithm is sum of string elements, plus string length
* mod table size.
*/

static int hashit (text)
register char *text;
{
    register long int sum = 0;
    register int i;

    for (i = 0; text[i] != '\0'; i++)
	    sum += text[i];
    sum += i;

    return (sum % Num_elem);
}

/*
 * Prints all elements of the table, in no particular order.
 */
void
hprint(f)
void (*f)();	/* (*f) is a function that will be called by hprint, is passed
		 * (hashIndex, ptrToKey, ptrToData), and should print these.
		 */
{
    ELEMENT *p;
    int i;
    for (i=0; i<Num_elem; i++) {
	for (p = Table[i]; p; p = p->next)
	    (*f)(i, p->item.key, p->item.data);
    }
}

/*
 * A function suitable for passing to hprint, if both key and data are text
 * suitable for passing to printf.  You can use this one or supply your
 * own.
 */
void
htext(indx, key, data)
int indx;
char *key;
char *data;
{
    printf("(%d)\t%s :\t%s\n", indx, key, data);
}

