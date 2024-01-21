/*
 * File:	spool.c
 *
 * Author:	Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 *
 * History:	 
 * 
 *    1 Sep 95   Framstag	initial version
 *   10 Oct 95   Framstag	open bug fixed
 *    1 Nov 95   Framstag	added pgp signature (dummy)
 *    5 Nov 95   Framstag	added NeXT support
 *   15 Nov 95   Framstag	fixed memory leak (oops :-) )
 *   24 Jan 96   Framstag	new (received) DATEFORMAT
 *   25 Jan 96   Framstag	added keep and deljunk config
 *    6 Feb 96   Framstag 	added ATTRIBUTE=EXE
 *   16 Mar 96   Framstag	recognize file size 0 correctly
 *    2 Apr 96   Framstag	FROM now in UTF-7
 *   12 Apr 96   Framstag	sign is now in filelist
 * 				sign, comment and fname are now dynamic
 * 				added pgp support
 *   20 Apr 96   Framstag	build sender list only for requested sender
 *				(wildcards are ok)
 *   24 Apr 96   Framstag	added scanoutspool function
 *   24 Jun 96   Framstag	fixed bug when FROM contains no real name
 *
 * Functions for operations on files in the sendfile spool directory.
 *
 * Copyright © 1995,1996 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */


#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <dirent.h>

#include "config.h"     /* various #defines */
#include "spool.h"	/* operations on files in the sendfile spool */
#include "utf7.h"       /* UTF-7 coding */
#include "message.h"    /* information, warning and error messages */
#include "string.h"     /* extended string functions */
#include "reply.h"	/* the 3 digit reply codes with text messages */


/*
 * scanspool - scan through spool directory, build list-structures and
 *             delete old files if necessary
 *
 * INPUT:   sender  - sender name (wildcards * and ? are allowed)
 *
 * RETURN:  start of sender list
 */
struct senderlist *scanspool(char *sender)
{ extern int client;		/* flag to determine client or server */
  extern char userspool[];	/* user spool directory */
  unsigned char *ucp;		/* simple unsigned character pointer */
  char
    *cp,			/* simple character pointer */
    *arg,			/* the argument(s) of the header line */
    line[MAXLEN],		/* config line */
    hline[MAXLEN],		/* header line */
    from[MAXLEN],		/* header from line */
    fname[MAXLEN],		/* header file line */
    comment[MAXLEN],		/* file comment (printable string) */
    tmp[MAXLEN],		/* temporary string */
    dummy[MAXLEN],		/* dummy string for utf2iso */
    msg[MAXLEN],		/* information/error message */
    sign[MAXLEN],		/* pgp signature (Latin-1) */
    date[DLEN+1],		/* file date */
    charset[DLEN+1],		/* character set name */
    rdate[DLEN+1],		/* receive date */
    file[FLEN+1];		/* spool file name */
  int
    i,
    id,				/* spool file id number */
    hfc,			/* header file corruption flag */
    keep,			/* keep spool files at least xx days */
    deljunk,			/* delete corrupted spool files after xx days */
    flags;			/* source, text, compress, tar and exe flag */
  unsigned long
    osize,			/* original file size */
    csize,			/* compressed file size */
    tsize;			/* true spool data file size */
  time_t
    ctime,			/* current time */
    rtime;			/* receiving time */
  FILE
    *hf,			/* header file */
    *inf;			/* config file */
  struct stat finfo;		/* information about a file */
#ifdef NEXT
  FILE *pp;			/* pipe */
#else
  struct dirent *dire;		/* directory entry */
  DIR *dp;			/* directory pointer */
#endif
  struct filelist *flp,		/* file list pointer */
                  *fln;		/* new file list element */
  struct senderlist *sls,	/* sender list start */
                    *sln,	/* sender list next pointer */
                    *slp;	/* sender list pointer */
  static struct senderlist *sll=NULL;	/* last sender list start */


  keep=0;
  deljunk=10;
  ctime=time(NULL);
  flp=NULL;

  /* is there already an old senderlist? */
  if (sll)

    /* delete old senderlist and free memory */
    for (slp=sll; slp!=NULL;)
    { for (flp=slp->flist; flp!=NULL; )
      { fln=flp->next;
	free(flp->sign);
	free(flp->fname);
	free(flp->comment);
	free(flp);
	flp=fln;
      }
      sln=slp->next;
      free(slp);
      slp=sln;
    }

  sls=sll=NULL;

  /* parse the config-file */
  if ((inf=fopen(CONFIG,"r")))
  { while ((fgets(line,MAXLEN-1,inf)))
    {
      /* prepare line to be parsed */
      if ((cp=strchr(line,'#'))) *cp=0;
      if ((cp=strchr(line,'\n'))) *cp=0;
      if ((cp=strchr(line,'='))) *cp=' ';
      str_trim(line);
      str_tolower(line);

      /* is there an option and an argument? */
      if ((cp=strchr(line,' ')))
      { *cp=0; cp++;
	if (streq(line,"keep"))
	{ keep=atoi(cp);
	  continue;
	}
	if (streq(line,"deljunk"))
	{ deljunk=atoi(cp);
	  continue;
	}
      }

    }
    fclose(inf);
  }

  /* mega stupid NeXT has broken readdir() */
#ifdef NEXT
  /* open spool dir */
  sprintf(tmp,"ls %s 2>/dev/null",userspool);
  if ((pp=popen(tmp,"r")) == NULL) return(NULL);

  /* scan through spool directory */
  while (fgets(tmp,MAXLEN-1,pp))
  {
    if ((cp=strrchr(tmp,'\n'))) *cp=0;

    /* look for header files */
    if ((cp=strchr(tmp,'.')) && streq(cp,".h"))
    {
      /* extract spool id */
      *cp=0;
      id=atoi(tmp);
#ifdef JEDPARSE
    }}
#endif

#else
  /* open spool dir */
  if ((dp=opendir(userspool)) == NULL) return(NULL);

  /* scan through spool directory */
  while ((dire=readdir(dp)))
  {
    /* look for header files */
    if ((cp=strchr(dire->d_name,'.')) && streq(cp,".h"))
    {
      /* extract spool id */
      *cp=0;
      id=atoi(dire->d_name);
#endif

      /* open header file */
      sprintf(file,"%s/%d.h",userspool,id);
      if ((hf=fopen(file,"r")) == NULL)
      {
	/* called from receive client? */
	if (client)
	{ sprintf(msg,"cannot open spool file %s",file);
	  message("",'E',msg);
	}

        continue;
      }

      /* initialisize header entries */
      flags=0;
      csize=0;
      tsize=0;
      osize=0;
      *from=0;
      *date=0;
      *fname=0;
      *charset=0;
      *comment=0;
      *sign=0;

      /* get time of receiving (local) */
      sprintf(file,"%s/%d.d",userspool,id);
      stat(file,&finfo);
      rtime=finfo.st_mtime;

      /* spool file expired? */
      if (keep>0 && (ctime-rtime)/DAYSEC>=keep)
      { fclose(hf);
	unlink(file);
	sprintf(file,"%s/%d.h",userspool,id);
	unlink(file);
	continue;
      }

      strftime(rdate,21,DATEFORMAT,localtime(&rtime));

      /* read header file */
      hfc=0;
      while ((fgets(hline,MAXLEN-1,hf)) && hfc==0)
      {
	/* prepare the header line */
        if ((cp=strchr(hline,'\n'))) *cp=0;
        cp=strchr(hline,'\t');

	/* corrupt header file line? */
	if (cp==NULL)
	{ hfc=1;
	  continue;
	}

        arg=cp+1;
        *cp=0;

        /* extract the header-name and the argument */

        if (streq(hline,"FROM"))
	{ if ((cp=strchr(arg,' ')))
	  { *cp=0;
	    sprintf(tmp,"%s (%s)",arg,cp+1);
	  } else
	    strcpy(tmp,arg);
	  utf2iso(0,from,dummy,dummy,tmp);
          continue;
        }

        if (streq(hline,"FILE"))
	{ strcpy(fname,arg);
          continue;
        }

        if (streq(hline,"DATE"))
	{ strncpy(date,arg,DLEN);
          continue;
        }

        if (streq(hline,"CHARSET"))
	{ strncpy(charset,arg,DLEN);
          continue;
        }

        if (streq(hline,"SIZE"))
	{ sscanf(arg,"%ld %ld",&csize,&osize);
	  tsize=finfo.st_size;
          continue;
        }

        if (streq(hline,"ATTR"))
	{ str_toupper(arg);
	  if (streq(arg,"TAR")) flags=flags|F_TAR;
	  if (streq(arg,"EXE")) flags=flags|F_EXE;
          continue;
	}

        if (streq(hline,"TYPE"))
	{ str_toupper(arg);
	  if (strstr(arg,"SOURCE"))	flags=flags|F_SOURCE;
          if (strstr(arg,"TEXT"))	flags=flags|F_TEXT;
          if (strstr(arg,"COMPRESSED")) flags=flags|F_COMPRESS;
          if (strstr(arg,"CRYPTED"))    flags=flags|F_CRYPT;
          continue;
        }

	/* comment and signature are only needed for receive client */
	if (client)
	{
	  if (streq(hline,"COMMENT"))
	  {
	    /* save comment in printable code */
	    utf2iso(0,comment,dummy,dummy,arg);
	    for (ucp=(unsigned char *)comment,i=0; *ucp; ucp++)
	      if (*ucp==9 || *ucp==10 || (*ucp>31 && *ucp<127) || *ucp>159)
	        comment[i++]=*ucp;
	    comment[i]=0;
	    continue;
	  }

	  if (streq(hline,"SIGN"))
	  { utf2iso(0,sign,dummy,dummy,arg);
	    continue;
	  }

	}

      }

      fclose(hf);

      /* junk file expired? */
      if ((*from==0 || *fname==0 || *date==0 || tsize<csize) && deljunk>0 &&
	  (ctime-rtime)/DAYSEC>=deljunk)
      { sprintf(file,"%s/%d.d",userspool,id);
	unlink(file);
	sprintf(file,"%s/%d.h",userspool,id);
	unlink(file);
	continue;
      }

      /* bad header file? */
      if (*from==0 || *fname==0) continue;

      /* is it a requested sender name? */
      if (*sender==0 || simplematch(from,sender,1))
      {
        /* create new file list element */
	if ((fln=(struct filelist *) malloc(sizeof(struct filelist))) == NULL)
	  if (client) message("",'F',"cannot allocate memory"); else reply(453);
	if ((fln->fname=(char *) malloc(strlen(fname)+1)) == NULL)
	  if (client) message("",'F',"cannot allocate memory"); else reply(453);
	if ((fln->comment=(char *) malloc(strlen(comment)+1)) == NULL)
	  if (client) message("",'F',"cannot allocate memory"); else reply(453);
	if ((fln->sign=(char *) malloc(strlen(sign)+1)) == NULL)
	  if (client) message("",'F',"cannot allocate memory"); else reply(453);

	/* fill in new file list element */
	fln->id=id;
	fln->next=NULL;
	fln->osize=osize;
	fln->csize=csize;
	fln->tsize=tsize;
	fln->flags=flags;
	strcpy(fln->date,date);
	strcpy(fln->sign,sign);
	strcpy(fln->fname,fname);
	strcpy(fln->rdate,rdate);
	strcpy(fln->charset,charset);
	strcpy(fln->comment,comment);

        /* first sender? */
        if (sls==NULL)
          sls=newsle(fln,from);
        else
	{
	  /* search for sender in senderlist */
          slp=sls;
          while (slp->next && !streq(slp->from,from)) slp=slp->next;

          /* sender not found in sender list? */
          if (!streq(slp->from,from))

            /* create new sender list element and append it */
            slp->next=newsle(fln,from);

          else
	  {
	    /* is it the smallest id? */
            if (id < slp->flist->id)
	    { fln->next=slp->flist;
              slp->flist=fln;
            } else
	    {
             /* insert into files list */
              for (flp=slp->flist; flp->next!=NULL; flp=flp->next)
	      { if (id < flp->next->id)
		{ fln->next=flp->next;
                  flp->next=fln;
                  break;
                }
              }

              /* last element? */
              if (flp->next==NULL) flp->next=fln;

            }
          }
        }
      }
    }
  }

#ifdef NEXT
  pclose(pp);
#else
  closedir(dp);
#endif
  sll=sls;
  return(sls);
}


/*
 * scanoutspool - scan through outgoing spool directory, build list-structures
 *		  and delete bad files if necessary
 *
 * INPUT:   sender  - sender name
 *
 * RETURN:  start of host list
 *
 * All header lines in the outgoing spool files are SAFT-conform (strings are
 * in UTF-7, etc).
 */
struct hostlist *scanoutspool(char *sender)
{ extern int client;		/* flag to determine client or server */
  char
    *cp,			/* simple character pointer */
    *arg,			/* the argument(s) of the header line */
    *outgoing,			/* outgoing spool directory */
    hline[MAXLEN],		/* header line */
    host[MAXLEN],		/* header recipient host line */
    to[MAXLEN],			/* header recipient user line */
    from[MAXLEN],		/* header from line */
    fname[MAXLEN],		/* header file line */
    tmp[MAXLEN],		/* temporary string */
    msg[MAXLEN],		/* information/error message */
    hfn[MAXLEN],		/* outgoing spool header file name */
    dfn[MAXLEN];		/* outgoing spool data file name */
  int hfc;			/* header file corruption flag */
  unsigned long size;		/* size of outgoing spool data file */
  time_t timetick;		/* unix time (in seconds) */
  FILE *hf;			/* header file */
  struct stat finfo;		/* information about a file */
#ifdef NEXT
  FILE *pp;			/* pipe */
#else
  struct dirent *dire;		/* directory entry */
  DIR *dp;			/* directory pointer */
#endif
  struct outfilelist
    *oflp,			/* outgoing file list pointer */
    *ofln;			/* new file list element */
  struct hostlist
    *hls,			/* host list start */
    *hln,			/* host list next pointer */
    *hlp;			/* host list pointer */
  static struct hostlist
    *hll=NULL;			/* last host list start */


  timetick=time(0);
  oflp=NULL;
  outgoing=SPOOL"/OUTGOING";

  /* is there already an old hostlist? */
  if (hll)

    /* delete old hostlist and free memory */
    for (hlp=hll; hlp!=NULL;)
    { for (oflp=hlp->flist; oflp!=NULL; )
      { ofln=oflp->next;
	free(oflp->to);
	free(oflp->oshfn);
	free(oflp->fname);
	free(oflp);
	oflp=ofln;
      }
      hln=hlp->next;
      free(hlp);
      hlp=hln;
    }

  hls=hll=NULL;

  /* mega stupid NeXT has broken readdir() */
#ifdef NEXT
  /* open spool dir */
  sprintf(tmp,"ls %s/%s_*.h 2>/dev/null",outgoing,sender);
  if ((pp=popen(tmp,"r")) == NULL) return(NULL);

  /* scan through spool directory */
  while (fgets(hfn,MAXLEN-1,pp))
  {
    if ((cp=strrchr(hfn,'\n'))) *cp=0;
#ifdef JEDPARSE
    }}
#endif

#else
  /* open spool dir */
  if (!(dp=opendir(outgoing))) return(NULL);

  /* scan through outgoing spool directory */
  while ((dire=readdir(dp)))
  {
    strcpy(hfn,dire->d_name);

    /* look for header files */
    sprintf(tmp,"%s*.h",sender);
    if (simplematch(strrchr(hfn,'/')+1,tmp,1)==0) continue;
#endif

    /* open header file */
    if ((hf=fopen(hfn,"r")) == NULL)
    {
      /* called from receive client? */
      if (client)
      { sprintf(msg,"cannot open outgoing spool file %s",hfn);
	message("",'E',msg);
      }

      continue;
    }

    /* initialisize header entries */
    size=0;
    *to=0;
    *host=0;
    *from=0;
    *fname=0;

    /* read header file */
    hfc=0;
    while (fgets(hline,MAXLEN-1,hf) && hfc==0)
    {
      /* prepare the header line */
      if ((cp=strchr(hline,'\n'))) *cp=0;
      cp=strchr(hline,'\t');

      /* corrupt header file line? */
      if (cp==NULL)
      { hfc=1;
	continue;
      }

      arg=cp+1;
      *cp=0;

      /* extract the header-name and the argument */

      /* check the from line */
      if (streq(hline,"FROM"))
      { if ((cp=strchr(arg,' '))) *cp=0;
	utf2iso(0,from,tmp,tmp,arg);

	/* wrong from line? */
	if (*sender && !streq(from,sender)) hfc=1;

	continue;
      }

      /* check the to line */
      if (streq(hline,"TO"))
      { if (!(cp=strchr(arg,'@')))
	  hfc=1;
	else
	{ strcpy(host,cp+1);
	  *cp=0;
	  utf2iso(0,to,tmp,tmp,arg);
	}
	continue;
      }

      if (streq(hline,"FILE"))
      { utf2iso(0,fname,tmp,tmp,arg);
	continue;
      }

      if (streq(hline,"SIZE"))
      { sscanf(arg,"%ld",&size);
	strcpy(dfn,hfn);
	dfn[strlen(dfn)-1]='d';

	/* wrong size? */
	if (stat(dfn,&finfo)<0 || finfo.st_size!=size) hfc=1;

	continue;
      }

    }

    fclose(hf);

    /* delete bad files */
    if (*from==0 || *fname==0 || *to==0 || *host==0 || hfc)
    { strcpy(dfn,hfn);
      dfn[strlen(dfn)-1]='d';
      unlink(dfn);
      unlink(hfn);
      continue;
    }

    /* create new outgoing file list element */
    if ((ofln=(struct outfilelist *)malloc(sizeof(struct outfilelist))) == NULL)
      if (client) message("",'F',"cannot allocate memory"); else reply(453);
    if ((ofln->fname=(char *)malloc(strlen(fname)+1)) == NULL)
      if (client) message("",'F',"cannot allocate memory"); else reply(453);
    if ((ofln->to=(char *)malloc(strlen(to)+1)) == NULL)
      if (client) message("",'F',"cannot allocate memory"); else reply(453);
    if ((ofln->oshfn=(char *)malloc(strlen(hfn)+1)) == NULL)
      if (client) message("",'F',"cannot allocate memory"); else reply(453);
    if ((ofln->from=(char *)malloc(strlen(from)+1)) == NULL)
      if (client) message("",'F',"cannot allocate memory"); else reply(453);

    /* fill in new outgoing file list element */
    ofln->next=NULL;
    strcpy(ofln->to,to);
    strcpy(ofln->from,from);
    strcpy(ofln->oshfn,hfn);
    strcpy(ofln->fname,fname);

    /* first host? */
    if (hls==NULL)
      hls=newhle(ofln,host);
    else
    {
      /* search for host in hostlist */
      hlp=hls;
      while (hlp->next && !streq(hlp->host,host)) hlp=hlp->next;

      /* host not found in host list? */
      if (!streq(hlp->host,host))

	/* create new host list element */
	hlp->next=newhle(ofln,host);

      else  /* append to outgoing file list */
      { for (oflp=hlp->flist; oflp->next!=NULL; oflp=oflp->next);
	oflp->next=ofln;
      }

    }
  }

#ifdef NEXT
  pclose(pp);
#else
  closedir(dp);
#endif
  hll=hls;
  return(hls);
}


/*
 * newsle - create new sender list element and fill it out
 *
 * INPUT:  flp  - first file list element
 *         from - sendername
 *
 * RETURN: start of sender list
 */
struct senderlist *newsle(struct filelist *flp, const char *from)
{ struct senderlist *sln;	/* new sender list element */
  extern int client;		/* flag to determine client or server */

  /* create new sender list element */
  if ((sln=(struct senderlist *)malloc(sizeof(struct senderlist))) == NULL)
    if (client) message("",'F',"cannot allocate memory"); else reply(453);

  /* fill it out */
  sln->next=NULL;
  sln->flist=flp;
  strcpy(sln->from,from);

  return(sln);
}


/*
 * newhle - create new host list element and fill it out
 *
 * INPUT:  oflp  - first outgoing file list element
 *         host - host name
 *
 * RETURN: start of sender list
 */
struct hostlist *newhle(struct outfilelist *oflp, const char *host)
{ struct hostlist *hln;	/* new host list element */
  extern int client;	/* flag to determine client or server */

  /* create new host list element */
  if ((hln=(struct hostlist *)malloc(sizeof(struct hostlist))) == NULL)
    if (client) message("",'F',"cannot allocate memory"); else reply(453);

  /* fill it out */
  hln->next=NULL;
  hln->flist=oflp;
  strcpy(hln->host,host);

  return(hln);
}


/*
 * delete_sf - delete a spool file
 *
 * INPUT:  flp   	- file list pointer
 *         verbose	- if set, print success message
 *
 * RETURN: 0 on success, -1 on fail
 */
int delete_sf(struct filelist *flp, int verbose)
{ char msg[2*MAXLEN],		/* information/error message */
       file[MAXLEN],		/* spool file */
       dummy[MAXLEN],		/* dummy string for utf2iso */
       fname[MAXLEN];		/* displayble file name */
  extern int client;		/* flag to determine client or server */
  extern char userspool[];	/* user spool directory */

  sprintf(file,"%s/%d.d",userspool,flp->id);
  unlink(file);
  sprintf(file,"%s/%d.h",userspool,flp->id);
  utf2iso(1,dummy,fname,dummy,flp->fname);
  if(unlink(file)<0)
  { if (client)
    { sprintf(msg,"cannot delete spoolfile #%d",flp->id);
      message("",'W',msg);
    }
    return(-1);
  } else
  { if (verbose)
    { sprintf(msg,"%s deleted",fname);
      message("",'I',msg);
    }
    return(0);
  }
}
