#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>

void fatal(const char *msg, ...);
void *malloc_(size_t);



/* this is BFS.h, prototypes for generic Breadth-First-Search lib. */
struct BFS {
    size_t keylength;	/* number of bytes for the key of the data 2..4 */
    size_t datalength;	/* total number of user bytes */
    size_t ptrsize;	/* number of bytes for backptrs */
    size_t totallength;
    int hashbits;	/* has table has size 1 << hashbits */
    int level;
    unsigned char *hashtable;
    unsigned char *data;
    unsigned char *endhash;
    unsigned long ndata;
    unsigned long hashmask;	/* is equal to nr. of entries */
    unsigned long read;		/* data item to read */
    unsigned long write;	/* data item to write */
    unsigned long stop;		/* helps for level wrap */
};

void BFS_init(size_t keylength, size_t datalength, int hashbits,
	      unsigned long ndata, int backtrack);
long BFS_store(void *data);	/* store data, return entrynr */
int BFS_newlevel(void);		/* request new level */
int BFS_retrieve(void *data);
long BFS_getentry(long entrynr, void *data); /* returns predecessor */

