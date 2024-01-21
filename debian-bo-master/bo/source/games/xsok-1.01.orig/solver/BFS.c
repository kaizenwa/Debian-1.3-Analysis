#include <stdio.h>
#include "BFS.h"
#include "crc16.h"

#define STATISTICS

static struct BFS bfs;

#ifdef STATISTICS
static unsigned long searches, misses;
#endif

void BFS_init(size_t keylength, size_t datalength, int hashbits,
	      unsigned long ndata, int backtrack) {
    if (hashbits < 9 || hashbits > 24)
	fatal("only 9..24 bits hashsize allowed!");
    /* copy args */
    bfs.keylength = keylength;
    bfs.datalength = datalength;
    bfs.hashbits = hashbits;
    bfs.ndata = ndata;
    /* compute rest */
    bfs.ptrsize = (hashbits+7) >> 3;
    bfs.hashmask = (1UL << hashbits) - 1UL;
    bfs.totallength = bfs.datalength;
    if (backtrack)
	bfs.totallength += bfs.ptrsize;
    bfs.level = 0;
    if (ndata >= bfs.hashmask)
	fatal("BFS: cannot hold that much data\n");
    printf("BFS memory usage:\n%ld byte for hashtable\n%ld byte for data\n",
	   bfs.ptrsize * bfs.hashmask, bfs.ndata * bfs.totallength);
    bfs.hashtable = malloc_(bfs.ptrsize * bfs.hashmask + 2); /* 2 for trick */
    memset(bfs.hashtable, 0xff, bfs.ptrsize * bfs.hashmask); /* free hash */
    bfs.data = malloc_(bfs.ndata * bfs.totallength);
    bfs.endhash = bfs.hashtable + bfs.ptrsize * bfs.hashmask;
    bfs.read = 0;
    bfs.write = 0;
    bfs.stop = 0;
}

int BFS_retrieve(void *data) {
    if (bfs.read == bfs.stop)
	return 0;
    memcpy(data, bfs.data + bfs.totallength * bfs.read++, bfs.datalength);
    return 1;
}

int BFS_newlevel(void) {
    if (bfs.stop == bfs.write)
	return 0;	/* no items were stored in this level */
    /* if (bfs.read == bfs.stop), the old level has been read out completely */
    bfs.read = bfs.stop;
    bfs.stop = bfs.write;
#ifdef STATISTICS
    printf("%6ld items stored at level %3d (%6lu total), "
	   "(%ld searches, %ld misses)\n", bfs.stop - bfs.read,
	   bfs.level, bfs.write, searches, misses);
    searches = 0;
    misses = 0;
#endif
    ++bfs.level;
    return 1;
}

long BFS_store(void *data) {
    unsigned long hashcode;
    unsigned char *ptr;

#ifdef STATISTICS
    ++searches;
#endif
    CRC16_reset();
    hashcode = ((unsigned long)CRC16_add(data, bfs.keylength) << (bfs.hashbits-16))
	& bfs.hashmask;
    ptr = bfs.hashtable + bfs.ptrsize * hashcode;
    /* printf("searching code %x\n", hashcode); */
    /* search a free entry in the hashtable */
    for (;;) {
	unsigned long entrynr;
	if (ptr == bfs.endhash)	    /* wrap around */
	    ptr = bfs.hashtable;
	entrynr = ((unsigned long)ptr[0] | ((unsigned long)ptr[1] << 8)
	     | ((unsigned long)ptr[2] << 16)) & bfs.hashmask;
	if (entrynr == bfs.hashmask)
	    break;			/* found a free entry */
	if (!memcmp(bfs.data+bfs.totallength*entrynr, data, bfs.keylength)) {
	    /* merge the entries */
	    return (long)entrynr;
	}
#ifdef STATISTICS
	++misses;
#endif
	ptr += bfs.ptrsize;
    }

    /* there was no matching entry => create a new one */
    if (bfs.write == bfs.ndata)
	fatal("BFS: data table full\n");
    ptr[0] = (unsigned char)bfs.write;
    ptr[1] = (unsigned char)(bfs.write >> 8);
    if (bfs.hashbits > 16)
	ptr[2] = (unsigned char)(bfs.write >> 16);

    ptr = bfs.data+bfs.totallength*bfs.write;
    memcpy(ptr, data, bfs.datalength);
    if (bfs.totallength > bfs.datalength) {
	/* now store the backtrack-pointer */
	ptr += bfs.datalength;
	ptr[0] = (unsigned char)bfs.read;
	ptr[1] = (unsigned char)(bfs.read >> 8);
	if (bfs.hashbits > 16)
	    ptr[2] = (unsigned char)(bfs.read >> 16);
    }
    return (long)(bfs.write++);
}

long BFS_getentry(long entrynr, void *data) {
    unsigned char *ptr = bfs.data + bfs.totallength * entrynr;
    memcpy(data, ptr, bfs.datalength);
    if (bfs.totallength == bfs.datalength)
	return -1L;
    ptr += bfs.datalength;
    entrynr = ((long)ptr[0] | ((long)ptr[1] << 8)
	     | ((long)ptr[2] << 16)) & bfs.hashmask;
    return entrynr - 1L;
}
