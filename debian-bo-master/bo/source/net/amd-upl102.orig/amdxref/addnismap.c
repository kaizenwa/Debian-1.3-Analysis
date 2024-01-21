/*
This software is provided as-is, with no warranties for suitability of
use or support implied.  This package makes no guarantees that it will
perform in any manner.  The authors and Texas A&M University accept no
liability for any damages incurred while using this software.

This software may be copied, redistributed, and updated in any
fashion as long as this comment header is not removed or altered.

Douglas Lee Schales
Doug.Schales@sc.tamu.edu
Texas A&M University
Supercomputer Center

01/26/1993
*/
#include <sys/types.h>
#include <rpc/rpc.h>
#include <rpcsvc/ypclnt.h>
#include <rpcsvc/yp_prot.h>

static int _loadnismap(int, char *, int, char *, int, char *);

static char *nisbufptr = 0;
static int nisbufsize = 0;
static int nisbufndx = 0;
static int nisstat = 0;

int
addnismap(char *mapname, char *dom, int usekey)
{
     char *domain = dom;
     struct ypall_callback callback;
     int status;

     if(!domain)
	  yp_get_default_domain(&domain);

     callback.foreach = _loadnismap;
     callback.data = (char *)&usekey;
     nisstat = 0;
     yp_all(domain, mapname, &callback);

     if(!nisstat){
	  if(nisbufndx == nisbufsize){
	       nisbufsize *= 2;
	       nisbufptr = (char *)realloc(nisbufptr, nisbufsize);
	  }
	  nisbufptr[nisbufndx++] = 0;
	  addmembuf(nisbufptr, mapname);
     }
     else if(nisbufptr)
	  free(nisbufptr);

     nisbufptr = 0;
     nisbufsize = 0;
     nisbufndx = 0;
     return nisstat;
}

static int
_loadnismap(int stat, char *key, int keylen,
	       char *value, int vallen,
	       char *data)
{
     int i;
     int usekey = *((int *)data);

     switch(stat){
     case YP_TRUE:
	  if(!nisbufsize){
	       nisbufsize = 64;
	       nisbufptr = (char *)malloc(nisbufsize);
	  }
	  if(usekey){
	       for(i=0;i<keylen;i++){
		    if(nisbufndx == nisbufsize){
			 nisbufsize *= 2;
			 nisbufptr = (char *)realloc(nisbufptr, nisbufsize);
		    }
		    nisbufptr[nisbufndx++] = key[i];
	       }
	       if(nisbufndx == nisbufsize){
		    nisbufsize *= 2;
		    nisbufptr = (char *)realloc(nisbufptr, nisbufsize);
	       }
	       nisbufptr[nisbufndx++] = ' ';
	  }
	  for(i=0;i<vallen;i++){
	       if(nisbufndx == nisbufsize){
		    nisbufsize *= 2;
		    nisbufptr = (char *)realloc(nisbufptr, nisbufsize);
	       }
	       nisbufptr[nisbufndx++] = value[i];
	  }
	  if(nisbufndx == nisbufsize){
	       nisbufsize *= 2;
	       nisbufptr = (char *)realloc(nisbufptr, nisbufsize);
	  }
	  nisbufptr[nisbufndx++] = '\n';

	  return 0;
	  break;
     case YP_NOMORE:
	  nisstat = 0;
	  return 1;
     default:
	  nisstat = stat;
	  return 1;
     }
}
