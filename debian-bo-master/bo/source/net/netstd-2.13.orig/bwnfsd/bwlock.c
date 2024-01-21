/*
VERSION  2.3a
MODIFIED 1/31/92

	BWNFSD is an RPC daemon used in conjunction with BWNFS ( a client
NFS for DOS based PCs. BWNFSD provides Authentication, Print Spooling,
DOS 3.1 Locking, DOS 3.1 Sharing, GID name mapping, UID name mapping
services for BWNFS and associated applications on the PC. BWNFSD is being
used also by Macintosh NFS clients.

	The BWNFSD code is originally copyright Beame & Whiteside Software
Ltd. and now is released to the Public Domain. The intent is that all server
vendors included a version of BWNFSD with their operating system. BWNFSD can
run simultantiously with PCNFSD, but provides many more features.

	Please send modifications to:

		Beame & Whiteside Software Ltd.
		P.O. Box 8130
		Dundas, Ontario
		Canada L9H 5E7
		+1 (416) 765-0822

	Please modify by including "ifdefs" for your particular operating
system, with appropriate modifications to the "makefile".

BWLOCK.C provides the locking and sharing service for BWNFS. This should be
used if rpc.lockd does not include the extensions for DOS 3.1 locking or
sharing, or if your system has the regular distribution of the rpc.lockd as
the extensions most likely will not work.

One might want to make operating system calls to lock and unlock the files
so that locking occurs not only against other BWNFS clients, but native
clients as well.

Modified 02/14/93, Fred N. van Kempen, <waltje@uwalt.nl.mugnet.org>,
to use #define macros for pathnames and such.  Much cleaner...

*/
/*
  HASH_LOCATION is a number between 0 and 31 inclusive. It should be set to
  the position in the file handle 32 bytes structure which changes the most
  between different files.
*/
#define HASH_LOCATION 17

#include <stdio.h>
#include <signal.h>
#ifdef SYSV32
#   include <string.h>
#   define  memcmp(a,b,c) bcmp((b),(a),(c))
#   define  memcpy(a,b,c) bcopy((b),(a),(c))
#else
#   include <memory.h>
#   include <strings.h>
#endif

#define LOCK_DMP	"/tmp/bwlocks.dmp"


#define LOCK 	'L'
#define SHARE   'S'
		      /*              1st Open             */
                      /* ALL     DR     DW     DRW   COMP  */
                      /* I O B  I O B  I O B  I O B  I O B */
char shares[15][15] = { {1,1,1, 0,0,0, 1,1,1, 0,0,0, 0,0,0}, /* I */
			{1,1,1, 1,1,1, 0,0,0, 0,0,0, 0,0,0}, /* O  ALL */
			{1,1,1, 0,0,0, 0,0,0, 0,0,0, 0,0,0}, /* B */
			{0,1,0, 0,0,0, 0,1,0, 0,0,0, 0,0,0}, /* I */
     			{0,1,0, 0,1,0, 0,0,0, 0,0,0, 0,0,0}, /* O  DR */
			{0,1,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0}, /* B */
			{1,0,0, 0,0,0, 1,0,0, 0,0,0, 0,0,0}, /* I */
			{1,0,0, 1,0,0, 0,0,0, 0,0,0, 0,0,0}, /* O DW 2nd Open */
			{1,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0}, /* B */
			{0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0}, /* I */
			{0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0}, /* O  DRW */
			{0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0}, /* B */
			{0,0,0, 0,0,0, 0,0,0, 0,0,0, 1,1,1}, /* I */
			{0,0,0, 0,0,0, 0,0,0, 0,0,0, 1,1,1}, /* O  COMP */
			{0,0,0, 0,0,0, 0,0,0, 0,0,0, 1,1,1}};/* B */

struct LK
	{
		char type;
		struct LK *nxt;
		struct LK *prv;
		struct LK *nxt_fh;
		struct LK *prv_fh;
		struct FH *fh;
		struct PC *pc;
		union {
			 struct {
			    char mode;
			    char access;
			 } s;
			 struct {
			    unsigned long offset;
			    unsigned long length;
			 } l;
		      } lock;
	};

struct FH
	{
		char fh[32];
		struct LK *lock;
		struct FH *nxt;
		struct FH *prv;
	};

struct PC
	{
		char  *owner;
		struct PC *nxt;
		struct PC *prv;
		struct LK *lock;
		unsigned long	last_cookie;
		int	last_result;
	};

struct PC *pcs[256];
struct FH *fhs[256];
int       debugmode;


char *my_strdup(p)
	char *p;
{
	char *p1;
	if ( (p1 = (char *) malloc(strlen(p)+1)) == NULL) return(NULL);
	return(strcpy(p1,p));
}


void display_locks()
{
	int i,j;
	struct PC *pc1;
	struct LK *lk1;
	struct FH *fh1;
	FILE *lock_file;

#ifdef	SIGUSR1
  (void) signal(SIGUSR1,display_locks);
#endif
   if ( (lock_file = fopen(LOCK_DMP,"w")) == NULL) return;
   for ( i = 0 ; i < 256;i++)
   {
      pc1 = pcs[i];
      while ( pc1 != NULL)
      {
	 j = 0;
	 (void) fprintf( lock_file,"%3d %s -> ", i, pc1->owner );
	 lk1 = pc1->lock;
	 while ( lk1 != NULL)
	 {
	    (void) fprintf( lock_file, "%c/%3u/",
	                    lk1->type, (lk1->fh)->fh[HASH_LOCATION] & 0xff);
	    if ( lk1->type == SHARE)
	    {
	       (void) fprintf( lock_file, "%1d -> ", lk1->lock.s.mode );
	       j+=11;
	    }
	    else
	    {
               (void) fprintf( lock_file, "%3ld+%3ld -> ",
                               lk1->lock.l.offset, lk1->lock.l.length );
	       j+=17;
	    }
	    if ( j > 70)
	    {
	       (void) fprintf( lock_file, "\n                ");
	       j=0;
	    }
	    lk1=lk1->nxt;
	 }
	 (void) fprintf( lock_file, "NULL\n" );
	 pc1=pc1->nxt;
      }
   }
   (void) fprintf( lock_file, "File Handles\n");
   for ( i = 0 ; i < 256;i++)
   {
      fh1 = fhs[i];
      while ( fh1 != NULL)
      {
	 (void) fprintf( lock_file, "%3d ", i );
	 for (j=0;j<32;j++)
	   (void) fprintf( lock_file, "%02X", fh1->fh[j] & 0xff );
	 (void) fprintf( lock_file, " -> " );
	 j   = 0;
	 lk1 = fh1->lock;
	 while ( lk1 != NULL)
	 {
	    (void) fprintf( lock_file, "%c/%3u/",lk1->type,*(lk1->pc)->owner & 0xff);
	    if ( lk1->type == SHARE)
	    {
	       (void) fprintf( lock_file, "%1d -> ", lk1->lock.s.mode );
	       j+=11;
	    }
	    else
	    {
               (void) fprintf( lock_file, "%3ld+%3ld -> ",
                               lk1->lock.l.offset, lk1->lock.l.length );
	       j+=17;
	    }
	    if ( j > 70)
	    {
	       (void) fprintf( lock_file, "\n                ");
	       j=0;
	    }
	    lk1=lk1->nxt_fh;
	 }
	 (void) fprintf( lock_file, "NULL\n" );
	 fh1=fh1->nxt;
      }
   }
   fclose(lock_file);
}


void init_locks()
{
	int	i;

        if (debugmode)
          (void) fprintf( stdout, "bwnfds: [init_locks] called\n" );
	for ( i = 0 ; i < 256;i++)
	   pcs[i] = NULL;
	for ( i = 0 ; i < 256;i++)
	   fhs[i] = NULL;
#ifdef	SIGUSR1
  (void) signal(SIGUSR1,display_locks);
#endif
}

#define TRUE  1
#define FALSE 0

static	int	pc_ptr,fh_ptr;

struct PC *locate_by_owner(owner)
char *owner;
{
	struct PC *p;

        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [locate_by_owner] called for %s\n",
                          owner );
	p = pcs[pc_ptr];
	while ( p != NULL)
	{
	   if (debugmode)
	     (void) fprintf (stdout, "bwnfsd: [locate_by_owner] owner is %s\n",
	                    p->owner );
	   if ( strcmp(p->owner,owner) == 0)
	      return(p);
	   else
	      p = p->nxt;
	}
	return(p);
}

struct FH *locate_by_handle(fh)
char *fh;
{
	struct FH *p;

        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [locate_by_handle] called\n" );
	p = fhs[fh_ptr];
	while ( p != NULL)
	   if ( memcmp(p->fh,fh,32) == 0)
	      return(p);
	   else
	      p = p->nxt;
	return(p);
}


int process_lock_request(request,owner,fh,access,mode,offset,length,cookie)
int	request;
char	*fh,*owner;
int	access,mode;
unsigned long	length,offset;
unsigned long   cookie;
{
	struct PC *pc1;
	struct LK *lk1;
	struct LK *lk2;
	struct FH *fh1;
	int	  j;

        if (debugmode)
        {
          (void) fprintf( stdout, "bwnfsd: [process_lock_request] called with\n" );
          (void) fprintf( stdout, "bwnfsd: request     = %d\n", request );
          (void) fprintf( stdout, "bwnfsd: owner       = %s\n", owner );
          (void) fprintf( stdout, "bwnfsd: file_handle = " );
          for(j=0;j<32;j++)
            (void) fprintf( stdout, "%02X", fh[j] & 0xff );
	  fprintf(stdout,"\n");
          (void) fprintf( stdout, "bwnfsd: access      = %d\n", access );
          (void) fprintf( stdout, "bwnfsd: mode        = %d\n", mode );
          (void) fprintf( stdout, "bwnfsd: offset      = %ld\n", offset );
          (void) fprintf( stdout, "bwnfsd: length      = %ld\n", length );
          (void) fprintf( stdout, "bwnfsd: cookie      = %ld\n", cookie );
        }
	{
		int len;
		if ( owner == NULL)
			len = 0;
		else
			len = strlen(owner);
		if ( len ==0)
			pc_ptr =0;
		else if ( len == 1)
			pc_ptr = *(owner+len-1) & 0xff;
		else
			pc_ptr = (*(owner+len-1) ^ *(owner+len-2)) & 0xff;
	}

	if ( request != 23)
	{
	   	if ( (pc1 = locate_by_owner(owner)) == NULL)
	   	{
      			if ( (pc1 = (struct PC *) malloc(sizeof(struct PC))) == NULL)
	 			return(FALSE);
	      		pc1->owner  = my_strdup(owner);
      			pc1->lock   = NULL;
      			pc1->prv    = NULL;
	      		pc1->nxt    = pcs[pc_ptr];
			pc1->last_cookie = cookie-1;
      			pcs[pc_ptr] = pc1;
	      		if ( (pc1->nxt) != NULL)
				(pc1->nxt)->prv = pc1;
	   	}
		if ( pc1->last_cookie == cookie ) return(pc1->last_result);
	        pc1->last_cookie = cookie;
	}
	switch(request)
	{
	case 20: /* share */
	   fh_ptr = fh[HASH_LOCATION] & 0xff;

	   if ( (fh1 = locate_by_handle(fh)) == NULL)
	   {
	      if ( (fh1 = (struct FH *) malloc(sizeof(struct FH))) == NULL)
	      {
		 pc1->last_result = FALSE;
		 return(FALSE);
	      }
	      (void) memcpy(fh1->fh,fh,32);
	      fh1->lock   = NULL;
	      fh1->prv    = NULL;
	      fh1->nxt    = fhs[fh_ptr];
	      fhs[fh_ptr] = fh1;
	      if ( (fh1->nxt) != NULL)
		(fh1->nxt)->prv = fh1;
	   }
	   lk1 = fh1->lock;
	   while ( lk1 != NULL)
	   {
		if ( lk1->type == SHARE)
		{
		   if ( !shares[mode*3+access-1][lk1->lock.s.mode*3+
			lk1->lock.s.access-1])
			{
			   pc1->last_result = FALSE;
			   return(FALSE);
			}
                }
		lk1=lk1->nxt_fh;
	   }
	   if ( (lk1 = (struct LK *) malloc(sizeof(struct LK))) == NULL)
	   {
	      pc1->last_result = FALSE;
	      return(FALSE);
	   }
	   lk1->fh            = fh1;
	   lk1->pc            = pc1;
	   lk1->type          = SHARE;
	   lk1->lock.s.mode   = mode;
	   lk1->lock.s.access = access;
	   lk1->prv           = NULL;
	   lk1->prv_fh        = NULL;
	   if ( ( lk1->nxt = pc1->lock) != NULL)
	      (lk1->nxt)->prv = lk1;
	   pc1->lock = lk1;
	   if ( ( lk1->nxt_fh = fh1->lock) != NULL)
	      (lk1->nxt_fh)->prv_fh = lk1;
	   fh1->lock = lk1;
	   pc1->last_result = TRUE;
	   return(TRUE);
	case 21: /* unshare */
	   fh_ptr = fh[HASH_LOCATION] & 0xff;
	   if ( (fh1 = locate_by_handle(fh)) == NULL)
	   {
		pc1->last_result = TRUE;
	        return(TRUE);
	   }
	   lk1=fh1->lock;
	   while ( lk1 != NULL)
	   {
	      if ( (lk1->pc == pc1) && ( lk1->type == SHARE) &&
		   ( mode == lk1->lock.s.mode) && ( access == lk1->lock.s.access))
	      {
	         if ( (lk1->prv_fh) == NULL)
	         {
		    if ((fh1->lock = lk1->nxt_fh) == NULL)
		    {
	               if ( fh1->prv == NULL)
	   	       {
	                  if ( (fhs[fh_ptr] = fh1->nxt) != NULL)
		             (fh1->nxt)->prv = NULL;
  		       }
	    	       else
	   	       {
	      	          if (((fh1->prv)->nxt = fh1->nxt) != NULL)
	                     (fh1->nxt)->prv = fh1->prv;
	               }
		       free(fh1);

		    }
		    else
		       (fh1->lock)->prv_fh = NULL;
	         }
	         else
	         {
	            if ( ((lk1->prv_fh)->nxt_fh = lk1->nxt_fh) != NULL)
	               (lk1->nxt_fh)->prv_fh = lk1->prv_fh;
	         }
	         if ( (lk1->prv) == NULL)
	         {
		    if ((pc1->lock = lk1->nxt) == NULL)
		    {
	               if ( pc1->prv == NULL)
	   	       {
	                  if ( (pcs[pc_ptr] = pc1->nxt) != NULL)
		             (pc1->nxt)->prv = NULL;
  		       }
	    	       else
	   	       {
	      	          if ( ((pc1->prv)->nxt = pc1->nxt) != NULL)
	                     (pc1->nxt)->prv = pc1->prv;
	               }
		       free(pc1->owner);
		       free(pc1);
		       pc1 = NULL;
		    }
		    else
		       (pc1->lock)->prv = NULL;
	         }
	         else
	         {
	            if (((lk1->prv)->nxt = lk1->nxt) != NULL)
	               (lk1->nxt)->prv = lk1->prv;
	         }
		 free(lk1);
		 if ( pc1 != NULL) pc1->last_result = TRUE;
		 return(TRUE);
	      }
	      lk1 = lk1->nxt_fh;
	   }
           if ( pc1 != NULL) pc1->last_result = TRUE;
	   return(TRUE);
	case 22: /* lock */
	   fh_ptr = fh[HASH_LOCATION] & 0xff;
	   if ( (fh1 = locate_by_handle(fh)) == NULL)
	   {
	      if ( (fh1 = (struct FH *) malloc(sizeof(struct FH))) == NULL)
	      {
		 pc1->last_result = FALSE;
		 return(FALSE);
	      }
	      (void) memcpy(fh1->fh,fh,32);
	      fh1->lock   = NULL;
	      fh1->prv    = NULL;
	      fh1->nxt    = fhs[fh_ptr];
	      fhs[fh_ptr] = fh1;
	      if ( (fh1->nxt) != NULL)
		(fh1->nxt)->prv = fh1;
	   }
	   lk1 = fh1->lock;
	   while ( lk1 != NULL)
	   {
		if ( lk1->type == LOCK)
		   if ( ((lk1->lock.l.offset >= offset) &&
			 (offset+length > lk1->lock.l.offset)) ||
			((lk1->lock.l.offset < offset) &&
			 (lk1->lock.l.offset + lk1->lock.l.length > offset)))
		      {
			   pc1->last_result = FALSE;
			   return(FALSE);
		      }
		lk1=lk1->nxt_fh;
	   }
	   if ( (lk1 = (struct LK *) malloc(sizeof(struct LK))) == NULL)
	   {
	      pc1->last_result = FALSE;
	      return(FALSE);
           }
	   lk1->fh            = fh1;
	   lk1->pc            = pc1;
	   lk1->type          = LOCK;
	   lk1->lock.l.offset = offset;
	   lk1->lock.l.length = length;
	   lk1->prv           = NULL;
	   lk1->prv_fh        = NULL;
	   if ( ( lk1->nxt = pc1->lock) != NULL)
	      (lk1->nxt)->prv = lk1;
	   pc1->lock = lk1;
	   if ( ( lk1->nxt_fh = fh1->lock) != NULL)
	      (lk1->nxt_fh)->prv_fh = lk1;
	   fh1->lock = lk1;
	   pc1->last_result = TRUE;
	   return(TRUE);
	case 23: /* remove all */
           if (  (pc1 = locate_by_owner(owner)) == NULL)
	      return(TRUE);
	   if ( pc1->prv == NULL)
	   {
	      if ( (pcs[pc_ptr] = pc1->nxt) != NULL)
		 (pc1->nxt)->prv = NULL;
	   }
	   else
	   {
	      if ( ((pc1->prv)->nxt = pc1->nxt) != NULL)
	         (pc1->nxt)->prv = pc1->prv;
	   }
	   lk1 = pc1->lock;
	   while ( lk1 != NULL)
	   {
	      if ( (lk1->prv_fh) == NULL)
	      {
	  	 fh1 = lk1->fh;
		 if ((fh1->lock = lk1->nxt_fh) == NULL)
		 {
	            if ( fh1->prv == NULL)
	   	    {
		       fh_ptr = fh1->fh[HASH_LOCATION] & 0xff;
	               if ( (fhs[fh_ptr] = fh1->nxt) != NULL)
		          (fh1->nxt)->prv = NULL;
  		    }
	    	    else
	   	    {
	      	       if (((fh1->prv)->nxt = fh1->nxt) != NULL)
	                  (fh1->nxt)->prv = fh1->prv;
	            }
		    free(fh1);

		 }
		 else
		    (fh1->lock)->prv_fh = NULL;
	      }
	      else
	      {
	         if ( ((lk1->prv_fh)->nxt_fh = lk1->nxt_fh) != NULL)
	            (lk1->nxt_fh)->prv_fh = lk1->prv_fh;
	      }
	      lk2 = lk1->nxt;
	      free(lk1);
	      lk1=lk2;
	   }
	   free(pc1->owner);
	   free(pc1);
	   return(TRUE);
	case 24: /* unlock */
	   fh_ptr = fh[HASH_LOCATION] & 0xff;
	   if ( (fh1 = locate_by_handle(fh)) == NULL)
	   {
	      pc1->last_result = TRUE;
	      return(TRUE);
	   }
	   lk1=fh1->lock;
	   while ( lk1 != NULL)
	   {
	      if ( (lk1->pc == pc1) && (lk1->type == LOCK) &&
	      ( offset == lk1->lock.l.offset) && (length == lk1->lock.l.length))
	      {
	         if ( (lk1->prv_fh) == NULL)
	         {
		    if ((fh1->lock = lk1->nxt_fh) == NULL)
		    {
	               if ( fh1->prv == NULL)
	   	       {
	                  if ( (fhs[fh_ptr] = fh1->nxt) != NULL)
		             (fh1->nxt)->prv = NULL;
  		       }
	    	       else
	   	       {
	      	          if ( ((fh1->prv)->nxt = fh1->nxt) != NULL)
	                     (fh1->nxt)->prv = fh1->prv;
	               }
		       free(fh1);

		    }
		    else
		       (fh1->lock)->prv_fh = NULL;
	         }
	         else
	         {
	            if ( ((lk1->prv_fh)->nxt_fh = lk1->nxt_fh) != NULL)
	               (lk1->nxt_fh)->prv_fh = lk1->prv_fh;
	         }
	         if ( (lk1->prv) == NULL)
	         {
		    if ((pc1->lock = lk1->nxt) == NULL)
		    {
	               if ( pc1->prv == NULL)
	   	       {
	                  if ( (pcs[pc_ptr] = pc1->nxt) != NULL)
		             (pc1->nxt)->prv = NULL;
  		       }
	    	       else
	   	       {
	      	          if ( ((pc1->prv)->nxt = pc1->nxt) != NULL)
	                     (pc1->nxt)->prv = pc1->prv;
	               }
  		       free(pc1->owner);
		       free(pc1);
		       pc1 = NULL;
		    }
		    else
		       (pc1->lock)->prv = NULL;
	         }
	         else
	         {
	            if ( ((lk1->prv)->nxt = lk1->nxt) != NULL)
	               (lk1->nxt)->prv = lk1->prv;
	         }
		 free(lk1);
		 if ( pc1 != NULL) pc1->last_result = TRUE;
		 return(TRUE);
	      }
	      lk1 = lk1->nxt_fh;
	   }
	   if ( pc1 != NULL) pc1->last_result = TRUE;
	   return(TRUE);
	}
	if ( pc1 != NULL) pc1->last_result = FALSE;
	return(FALSE);
}
