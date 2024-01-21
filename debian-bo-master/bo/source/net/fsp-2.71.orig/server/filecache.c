/* FILECACHE.C
 * Routines to perform file-caching with VMS-FSP V2.6.5jt
 *
 * 05-MAR-93 First Version <S.A.Pechler@bdk.tue.nl>
 * Merged into standard distribution on Mar 10 by jtraub@cs.cmu.edu
 */

#include "tweak.h"
#include "server_def.h"
#include "s_extern.h"

/* CLEAR_CACHE
 * clears all entries of the file_cache to zero, 
 * points cache_pointer to last empty cache-entry
 */
void clear_cache PROTO2(FPCACHE *, fpcache, FPCACHE **, cache_p)
{
  register int i;
    
#ifdef DEBUG
  printf("clear_cache: delete %d entries\n",FSP_FILE_CACHE);
#endif
  for (i=0;i<=FSP_FILE_CACHE;i++) {
    fpcache[i].fp=0;
    fpcache[i].filename[0]=0;
    fpcache[i].inet_num=0;
    fpcache[i].port_num=0;
  }
  *cache_p= &fpcache[FSP_FILE_CACHE];
}

/* FIND_CACHE
 * searches for an entry in the file-cache.
 * returns pointer to entry if found, 0 otherwise.
 */
FPCACHE *find_cache PROTO4(FPCACHE *, fpcache, unsigned short, port_num,
			   unsigned long, inet_num, char *, fname)
{
  register int i=FSP_FILE_CACHE;
#ifdef DEBUG
  printf("find_cache: searching for %s:\n",fname);
#endif
  while (i>=0) { /* search for file in cache */
    if (port_num==fpcache[i].port_num && inet_num==fpcache[i].inet_num &&
	!strcmp(fname,fpcache[i].filename))
      /* file found in cache, return cache-pointer */
      return((FPCACHE *)&fpcache[i]);
    i--;
  }
  return((FPCACHE *)0);
}

/* DELETE_CACHE
 * deletes current entry from the filecache (file will be closed).
 */
void delete_cache PROTO1(FPCACHE *, cache_p)
{
#ifdef DEBUG
  if (dbug) printf("delete_cache: deleting ptr:%d\n",cache_p);
#endif
  if (cache_p->fp) {
    fclose(cache_p->fp); /* close file */
    cache_p->fp=0;       /* mark entry as being emtpy */
    cache_p->port_num = 0;
    cache_p->inet_num = 0;
    cache_p->filename[0]=0;
  }
}

/* ADD_CACHE
 * Adds an entry to the filecache. Returns pointer to next empty
 * entry.
 */
FPCACHE *add_cache PROTO6(FPCACHE *, fpcache, FPCACHE *, cache_p, 
			  unsigned short,  port_num, unsigned long, inet_num,
			  char *, fname, FILE *, fp)
{
  cache_p->fp= fp;                 /* add filepointer */
  strcpy(cache_p->filename,fname); /* add filename */
  cache_p->inet_num=inet_num;      /* add IP address */
  cache_p->port_num=port_num;      /* add portnumber */
  
  /* return pointer to next cache-entry */
  if (cache_p<=fpcache) return((FPCACHE *)&fpcache[FSP_FILE_CACHE]);
  else return(cache_p-1);
}
