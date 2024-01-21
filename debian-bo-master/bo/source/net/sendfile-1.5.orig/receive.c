/*
 * File:	receive.c
 *
 * Author:	Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 *
 * Contrib.:	Christian Recktenwald (chris@yeti.faveve.uni-stuttgart.de)
 * 		Rainer Bawidamann (widi@sol.wohnheim.uni-ulm.de)
 * 		Beate Herrmann (beate@juhu.lake.de)
 *
 * History:	
 * 
 *   11 Aug 95   Framstag	initial version
 *   14 Aug 95   Framstag	corrected bug when receiving archives
 *   10 Sep 95   Framstag	extracted functions to spool.c
 *   31 Oct 95   Framstag	fixed security problems
 *    5 Nov 95   Framstag	added NeXT support
 *    7 Nov 95   Framstag	corrected bug when receiving to
 *                              a non-writable directory
 *    2 Dec 95   Framstag	added rename option
 *   15 Jan 96   Framstag	better error handling
 *    6 Feb 96   Framstag	added ATTRIBUTE=EXE
 *    7 Feb 96   Chris		better Convex-OS support
 *   21 Feb 96   widi		better Solaris-2 support
 *   22 Feb 96   Framstag	added bounce (forward) option
 *    6 Mar 96   Framstag	moved time stamp setting to get_date
 *   17 Mar 96   Framstag	some bug fixes
 *   27 Mar 96   Framstag	added quiet mode: no questions asked
 *   28 Mar 96   Framstag	extended search for support programs
 *    1 Apr 96   Framstag	corrected forking bug
 *    2 Apr 96   Framstag	added verbose bouncing
 *    4 Apr 96   Framstag	added check for dangerous file names in archives
 *    5 Apr 96   Framstag	correction: 1 KB is now 1024 bytes
 *    8 Apr 96   Framstag	added question for renaming better dangerous 
 *				file name checking
 *   12 Apr 96   Framstag	added pgp support
 *   13 Apr 96   Framstag	added -f from option
 *   13 Apr 96   Beate		added -s list option
 *   18 Apr 96   Framstag	delete empty file when pgp fails
 *   20 Apr 96   Framstag	added preserve option for pgp and tar
 *				added -k keep option
 *   22 Apr 96   Framstag	some code cleanup
 *   23 Apr 96   Framstag	moved fcopy to io.c
 *   24 Apr 96   Framstag	changed bouncing option syntax
 *    2 May 96   Framstag	better tar error checking
 *   12 May 96   Framstag	better checking of not complete files
 *   23 May 96   Framstag	added check for writeable tmp-files
 *   26 May 96   Framstag	fixed bug with overwriting links
 * 		                fixed bug with returning from fork
 *   24 Jun 96   Framstag	added -P to stdout option
 *   13 Sep 96   Framstag	added rename option to "."-file checking
 *
 * The receive client of the sendfile package.
 * Receives, lists and deletes files from the sendfile spool.
 *
 * Copyright © 1995,1996 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */


#ifdef ULTRIX
  #define _POSIX_SOURCE
  #define S_IFMT   0170000 /* type of file */
  #define S_IFDIR  0040000 /* directory */
#endif

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <pwd.h>
#include <time.h>
#include <utime.h>
#include <ctype.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include "config.h"     /* various #defines */
#include "message.h"    /* information, warning and error messages */
#include "utf7.h"       /* UTF-7 coding */
#include "io.h"		/* read/write routines */
#include "string.h"     /* extended string functions */
#include "spool.h"	/* operations on files in the sendfile spool */

#ifndef AIX
  #ifndef CONVEXOS
    FILE *popen(const char *, const char *);
  #endif
  int pclose(FILE *);
  #if defined(IRIX) || defined(LINUX)
    #include <getopt.h>
  #else
    int getopt(int, char * const *, const char *);
  #endif
#endif


/* global variables */
char *lll,		/* last list line */
     *prg,		/* name of the game */
     *pgpvm,		/* pgp verbose mode string */
     error[MAXLEN],	/* error log file */
     tartmp[MAXLEN],	/* tar temporary file */
     userspool[FLEN],	/* user spool directory */
     pgp_bin[MAXLEN],	/* the pgp binary */
     tar_bin[MAXLEN],	/* the tar binary */
     gzip_bin[MAXLEN],	/* the gzip binary */
     recode_bin[MAXLEN];/* the recode binary */
int keep=0,		/* flag for keeping files in spool */
    pipeout=0,		/* flag for receiving to stdout (via pipe) */
    quiet=0,		/* quiet mode: no questions asked */
    client=1,		/* flag to determine client or server */
    ren=0,		/* flag for renaming files */
    pgppass=0,		/* flag if the pgp password is set as an env variable */
    preserve=0;		/* preserve pgp and tar attributes */
mode_t cmask;		/* umask value */


/* clean termination routine */
void cleanexit();

/* delete tmp-file */
void cleanup();

/* print short help usage text */
int usage(void);

/* list all spool files */
int list(struct senderlist *, int, char *);

/* receive a spool file */
void receive_sf(struct filelist *);

/* check what to do when file with same name already exists */
int checkfile(int, char *, char *, char *, char *);

/* create detached pgp signature file */
int create_sigfile(const char *, const char *, const char *, char *);

/* translate NVT format file to Unix text format file */
void crlf2lf(FILE *, FILE *, const char *, const char *);

/* spawn a subprocess and direct output to a file */
int spawn(char **, const char *, mode_t);

/* shameless plug from GNU fileutils (getdate.c) :-) */
time_t get_date(char *, void *);


int main(int argc, char *argv[])
{ extern int optind;
  extern char *optarg;
  int
    i,			/* simple loop count */
    number,		/* flag for specifying a spool file number */
    listformat,		/* format of listing */
    del,		/* flag for deleting */
    all,		/* flag for receiving all */
    bounce,		/* flag for bouncing files */
    id,			/* spool id */
    found,		/* flag for found spool id */
    opt;		/* option to test for */
  char
    *cp,		/* a simple string pointer */
    *verbose,		/* verbose mode on bouncing files */
    from[FLEN],		/* from which sender */
    tmp[3*MAXLEN],	/* temporary string */
    dummy[MAXLEN],	/* dummy string for utf2iso */
    fname[MAXLEN],	/* displayble file name */
    bouncelist[MAXLEN],	/* list of spool files to bounce */
    pattern[MAXLEN];	/* file name pattern to search for */
  struct stat finfo;	/* information about a file */
  struct passwd *pwe;	/* password entry */
  struct filelist *flp;	/* file list pointer */
  struct senderlist
    *sls,		/* sender list start */
    *slp;		/* sender list pointer */
  FILE *inf;

  del=0;
  all=0;
  found=0;
  number=0;
  bounce=0;
  listformat=0;
  lll="";
  verbose="";
  *bouncelist=0;
  strcpy(from,"*");
  cmask=umask(0);
  umask(cmask);

  prg=argv[0];
  if ((cp=strrchr(prg,'/'))) prg=cp+1;

  if (getenv("PGPPASS"))
  { pgppass=1;
    pgpvm="+verbose=0";
  } else
  { pgppass=0;
    pgpvm="+verbose=1";
  }

  /* no arguments? */
  if (argc==1)
  { listformat=2;
    lll="Type \"receive -a\" to receive all files or \"receive -h\" "
        "for a short help.\n\n";
  } else
  {
    /* scan the command line */
    while ((opt=getopt(argc,argv,"h?lLsndarbVvqpkPf:")) > 0)
    { switch (opt)
      { case ':':
        case 'h':
        case '?': exit(usage());
        case 's': listformat=1; break;
        case 'l': listformat=2; break;
        case 'L': listformat=3; break;
        case 'n': number=1; break;
        case 'r': ren=1; break;
        case 'b': bounce=1; break;
        case 'd': del=1; break;
        case 'a': all=1; break;
        case 'P': pipeout=1; break;
        case 'k': keep=1; break;
        case 'q': quiet=1; break;
        case 'p': preserve=1; break;
        case 'v': verbose="-v"; break;
        case 'f': sprintf(from,"*%s*",optarg); break;
        case 'V': message(prg,'I',"version "VERSION" revision "REVISION);
	          exit(0);
      }
    }

  }

  /* too few arguments? */
  if ((argc-optind==0 && !all &&
       (ren || del || (!streq(from,"*") && !listformat))) ||
      (argc-optind==0 && bounce && all) ||
      (argc-optind<2 && bounce && !all))
    exit(usage());

  /* get the own user name */
  if ((pwe=getpwuid(getuid())) == NULL)
    message(prg,'F',"cannot determine own user name");

  /* does the spool directory exist? */
  sprintf(userspool,SPOOL"/%s",pwe->pw_name);
  if (stat(userspool,&finfo)<0 || (finfo.st_mode&S_IFMT)!=S_IFDIR)
  { sprintf(tmp,"spool directory %s does not exist",userspool);
    message(prg,'W',tmp);
    exit(1);
  }

  /* get tmpdir */
  if ((cp=getenv("TMPDIR")))
    strcpy(tmp,cp);
  else
    strcpy(tmp,"/tmp");
  if (stat(tmp,&finfo)<0 || !(finfo.st_mode&S_IRWXU))
    strcpy(tmp,pwe->pw_dir);

  /* set file names */
  sprintf(tartmp,"%s/.receive_%d.tar",tmp,(int)getpid());
  sprintf(error,"%s/.sendfile_%d.error",pwe->pw_dir,(int)getpid());

  /* check tmp files */
  unlink(tartmp);
  unlink(error);
  if (stat(tartmp,&finfo)==0)
  { sprintf(tmp,"tmp-file %s does already exist and cannot be deleted",tartmp);
    message(prg,'F',tmp);
  }
  if (stat(error,&finfo)==0)
  { sprintf(tmp,"tmp-file %s does already exist and cannot be deleted",error);
    message(prg,'F',tmp);
  }

  /* set log file read status (st_atime) for xhoppel */
  sprintf(tmp,"%s/log",userspool);
  inf=fopen(tmp,"r");
  if (inf)
  { fgets(tmp,1,inf);
    fclose(inf);
  }

  /* are there any files to receive? */
  sls=scanspool(from);
  if (sls==NULL)
  { message(prg,'W',"no files in spool directory");
    exit(1);
  }

  /* incompatible options? */
  if (bounce)
  { if (listformat||ren||del||preserve)
      message(prg,'W',"you cannot use any other option "
		      "when bouncing a file - ignored");
    listformat=ren=del=0;
    argc--;
  }

  if (!streq(from,"*") && number)
      message(prg,'W',"ignoring -f option when specifying a spool number");

  if (del&&keep)
      message(prg,'F',"you cannot delete and keep a file at the same time");

  /* support programs defaults */
  strcpy(pgp_bin,PGP);
  strcpy(tar_bin,TAR);
  strcpy(gzip_bin,GZIP);
  strcpy(recode_bin,RECODE);

  /* look for environment variables */
  if ((cp=getenv("SF_PGP")))	strcpy(pgp_bin,cp);
  if ((cp=getenv("SF_TAR")))	strcpy(tar_bin,cp);
  if ((cp=getenv("SF_GZIP")))	strcpy(gzip_bin,cp);
  if ((cp=getenv("SF_RECODE"))) strcpy(recode_bin,cp);

  /* do the support programs really exist? */
  if (access(pgp_bin,X_OK)<0)	 strcpy(pgp_bin,"pgp");
  if (access(tar_bin,X_OK)<0)	 strcpy(tar_bin,"tar");
  if (access(gzip_bin,X_OK)<0)	 strcpy(gzip_bin,"gzip");
  if (access(recode_bin,X_OK)<0) strcpy(recode_bin,"recode");

  /* enable simple interrupt handler */
  signal(SIGTERM,cleanexit);
  signal(SIGABRT,cleanexit);
  signal(SIGQUIT,cleanexit);
  signal(SIGHUP,cleanexit);
  signal(SIGINT,cleanexit);

  /* list files? */
  if (listformat)
  { if (list(sls,listformat,from)<0)
      message(prg,'W',"no files in spool directory");
    exit(0);
  }

  /* number specified? */
  if (number)
  {
    /* loop over all args */
    for (i=optind; i<argc; i++)
    { id=atoi(argv[i]);

      /* loop over sender list */
      for (slp=sls, found=0; slp!=NULL && found==0; slp=slp->next)
      {
	/* loop over files list */
        for (flp=slp->flist; flp!=NULL && found==0; flp=flp->next)
	{
          /* spool id found and spool file complete? */
	  if (flp->id==id && flp->csize==flp->tsize)
	  {
	    /* delete, bounce or receive spool file? */
	    if (del)
	      delete_sf(flp,1);
	    else if (bounce)
	      sprintf(bouncelist,"%s %d",bouncelist,id);
	    else
	      receive_sf(flp);

	    found=1;

	  }
	}
      }

      /* not found? */
      if (!found)
      { sprintf(tmp,"spool file #%d not found",id);
	message(prg,'W',tmp);
      }

    }

  }
  else  /* file name specified */
  {
    /* loop over all args */
    for (i=optind; i<argc || all; i++)
    { if (all)
        strcpy(pattern,"*");
      else
      	strcpy(pattern,argv[i]);

      /* loop over sender list */
      for (slp=sls; slp!=NULL; slp=slp->next)
      {
	/* loop over files list */
	for (flp=slp->flist; flp!=NULL; flp=flp->next)
	{
	  /* spool file incomplete? */
	  if (flp->csize!=flp->tsize) continue;

	  /* translate UTF-7 name to the displayable file name */
	  utf2iso(1,dummy,fname,dummy,flp->fname);

	  /* match? */
	  if (simplematch(fname,pattern,0)==1)
	  {
	    /* delete, bounce or receive spool file? */
	    if (del)
	      delete_sf(flp,1);
	    else if (bounce)
	      sprintf(bouncelist,"%s %d",bouncelist,flp->id);
	    else
	      receive_sf(flp);

	    found=1;

	  }
	}
      }

      /* not found? */
      if (!found && !all)
      { sprintf(tmp,"file %s not found",pattern);
	message(prg,'W',tmp);
      }

      all=0;

    }
  }

  /* files to bounce? */
  if (bounce && *bouncelist)
  { if (keep)
      sprintf(tmp,"sendfile -bk=y %s %s %s",verbose,bouncelist,argv[argc]);
    else
      sprintf(tmp,"sendfile -bk=n %s %s %s",verbose,bouncelist,argv[argc]);
    system(tmp);
  }

  cleanup();
  exit(0);
}


/*
 * list - list spool files with attributes
 *
 * INPUT:  sls		- sender list start
 *         format	- format of listing
 * 	   from		- select from user
 *
 * RETURN: 0 if files found, -1 if no files to list
 */
int list(struct senderlist *sls, int format, char *from)
{ int
    i,				/* simple loop counter */
    found,			/* flag for files found */
    fff;			/* first file flag */
  char
    *cp1,*cp2,	    		/* simple character pointer */
    sigfile[FLEN],		/* pgp signature file */
    cmd[MAXLEN],		/* command for pipe() */
    dummy[MAXLEN],		/* dummy string for utf2iso */
    line[MAXLEN],		/* line to read in */
    showtar[MAXLEN],		/* shell command to look inside tar archive */
    show_fname[MAXLEN];		/* file name to display */
  struct senderlist *slp;	/* sender list pointer */
  struct filelist *flp;		/* file list pointer */
  FILE
    *outf,			/* output file */
    *pp;			/* pipe input stream */

  found=0;

  /* loop over sender list */
  for (slp=sls; slp!=NULL; slp=slp->next)
  {
    /* match this sender? */
    if (simplematch(slp->from,from,1)==1)
    {
      /* loop over files list */
      for (flp=slp->flist,fff=1; flp!=NULL; flp=flp->next)
      {
	/* not complete? */
	if (flp->csize!=flp->tsize) continue;

	/* first file from this sender? */
	if (fff)
	{ fff=0;

	  /* print from header */
	  printf("\nFrom %s\n",slp->from);
	  for (i=1; i<80 && i<=strlen(slp->from)+5; i++) printf("-");
	  printf("\n");

	}

	found=1;

	/* print spool file informations */
	utf2iso(1,dummy,show_fname,dummy,flp->fname);
	printf("%3d) %s  %6d KB  %s",
	       flp->id,flp->rdate,(int)(flp->osize+1023)/1024,show_fname);

	if (format>1)
	{
	  /* tar archive or encrypted? */
	  if (flp->flags&F_TAR && flp->flags&F_CRYPT)
	    printf(" (archive,encrypted)");
	  else if (flp->flags&F_TAR)
	    printf(" (archive)");
	  else if (flp->flags&F_CRYPT)
	    printf(" (encrypted)");
	}

	printf("\n");

	/* check signature */
	if (*(flp->sign) && format>1)
	{
	  /* write signature file */
	  sprintf(sigfile,"%s/%d.d.sig",userspool,flp->id);
	  if (!(outf=fopen(sigfile,"w")))
	    message(prg,'E',"cannot open signature file");
	  else
	  { fprintf(outf,"%s",flp->sign);
	    fclose(outf);

	    /* check signature file with pgp */
	    sprintf(cmd,"%s +batchmode=on +language=en %s 2>/dev/null",
		    pgp_bin,sigfile);
	    if (!(pp=popen(cmd,"r")))
	      message(prg,'E',"cannot call pgp");

	    /* print result */
	    while (fgets(line,MAXLEN-1,pp))
	    { if ((cp1=strchr(line,'\n'))) *cp1=0;
	      if ((cp1=strrchr(line,'.'))) *cp1=0;
	      if (strstr(line,"Good signature from user") ||
		  strstr(line,"Bad signature from user"))
		printf("     (%s)\n",line);
	      if (strstr(line,"Key matching expected"))
	      { printf("     (No matching signature found in pgp key file)\n");
		break;
	      }
	    }

	    /* close pipe and delete signature file */
	    pclose(pp);
	    unlink(sigfile);

	  }
	}

	/* comment available? */
	cp1=flp->comment;
	if (*cp1 && format>1)
	{ while ((cp2=strchr(cp1,'\n')))
	  { *cp2=0;
	    printf("     \"%s\"\n",cp1);
	    cp1=cp2+1;
	  }
	  printf("     \"%s\"\n",cp1);
	}

	/* on verbose mode look inside tar */
	if (flp->flags&F_TAR && format==3 &&
	    !(flp->flags&F_CRYPT && (quiet || preserve) && !pgppass))
	{
	  /* encrypted, compressed or normal tar file? */
	  if (flp->flags&F_CRYPT)
	    sprintf(showtar,"%s %s -f < %s/%d.d | %s tvf -",
		      pgp_bin,pgpvm,userspool,flp->id,tar_bin);
	  else if (flp->flags&F_COMPRESS)
	    sprintf(showtar,"%s -d < %s/%d.d | %s tvf -",
		    gzip_bin,userspool,flp->id,tar_bin);
	  else
	    sprintf(showtar,"%s tvf %s/%d.d",tar_bin,userspool,flp->id);

	  /* sneak inside... */
	  if ((pp=popen(showtar,"r")) == NULL)
	    message(prg,'E',"contents of archive is not accessible");
	  else
	  { if (flp->flags&F_CRYPT && !pgppass && fgets(line,MAXLEN-1,pp))
	      printf("\n\n     %s",line);
	    while (fgets(line,MAXLEN-1,pp)) printf("     %s",line);
	    if (flp->flags&F_CRYPT && !pgppass) printf("\n");
	  }
	  pclose(pp);

	}
      }
    }
  }

  if (found)
  { printf("\n%s",lll);
    return(0);
  }
  else
    return(-1);

}


/*
 * receive_sf - receive a spool file
 *
 * INPUT:  flp     - file list element
 */
void receive_sf(struct filelist *flp)
{ int
    utf,			/* return code from utf2iso */
    terr,			/* tar error flag */
    tom;			/* tar overwrite message flag */
  char
    *cp,			/* simple character pointer */
    *sad[10],			/* spawn argument descriptor */
    answer[FLEN],		/* answer string */
    line[MAXLEN],		/* one line of text */
    tmp[2*MAXLEN],		/* temporary string */
    cmd[2*MAXLEN],		/* command string for system() */
    sfile[MAXLEN],		/* spool data file */
    danger[OVERSIZE],		/* dangerous file names */
    fname[MAXLEN],		/* file name to write */
    nname[MAXLEN],		/* file name in display format */
    sname[MAXLEN];		/* file name in shell handling format */
  static char
    overwrite='n';		/* overwrite mode */
  struct stat finfo;		/* information about a file */
  struct utimbuf utb;		/* binary time */
  time_t dummy;			/* dummy arg for localtime() */
  FILE
    *pp,			/* pipe input stream */
    *inf,			/* spool data file to read */
    *outf;			/* file to create */

  tom=0;
  terr=0;
  *danger=0;
  *answer=0;

  /* oops? */
  if (flp==NULL) return;

  /* determine overwrite mode */
  if (quiet) overwrite='Y';
  if (!strchr("YNS",overwrite)) overwrite='n';

  (void) localtime(&dummy);

  /* translate UTF-7 file name */
  utf=utf2iso(1,fname,nname,sname,flp->fname);
  fname[FLEN-1]=nname[FLEN-1]=sname[FLEN-1]=0;

  /* pipe to stdout? */
  if (pipeout)
  { 
    /* encrypted spool file? */
    if (flp->flags&F_CRYPT)
    { sprintf(cmd,"%s %s -f < %s/%d.d",pgp_bin,pgpvm,userspool,flp->id);
      if (system(cmd)!=0)
      { errno=0;
	sprintf(tmp,"cannot decrypt '%s' :",nname);
	message(prg,'E',tmp);
	return;
      }
    }
    /* compressed spool file? */
    else if (flp->flags&F_COMPRESS)
    { sprintf(cmd,"%s -d < %s/%d.d",gzip_bin,userspool,flp->id);
      if (system(cmd)!=0 && !flp->flags&F_TAR)
      { errno=0;
	sprintf(tmp,"cannot decompress '%s' :",nname);
	message(prg,'E',tmp);
	return;
      }
    }
    else /* copy spool file to stdout */
    { 
      /* copy file */
      sprintf(tmp,"%s/%d.d",userspool,flp->id);
      if (fcopy(tmp,"",0)<0)
      { errno=0;
	sprintf(tmp,"cannot read '%s'",nname);
        message(prg,'E',tmp);
	return;
      }

    }

    /* delete tar spool file if required */
    if (!keep) delete_sf(flp,0);

    return;
  }

  /* tar file to receive? */
  if (flp->flags&F_TAR)
  {
    /* save file in transfer shape? */
    if (preserve)
    {
      /* set new file name */
      fname[FLEN-12]=nname[FLEN-12]=sname[FLEN-12]=0;
      strcat(fname,".tar");
      strcat(nname,".tar");
      strcat(sname,".tar");
      if (flp->flags&F_COMPRESS)
      { strcat(fname,".gz");
	strcat(nname,".gz");
	strcat(sname,".gz");
      }
      if (flp->flags&F_CRYPT)
      { strcat(fname,".pgp");
	strcat(nname,".pgp");
	strcat(sname,".pgp");
      }

      /* if file with same name already exists check what to do */
      if (checkfile(utf,fname,nname,sname,&overwrite)) return;

      /* copy file */
      sprintf(tmp,"%s/%d.d",userspool,flp->id);
      if (fcopy(tmp,fname,0666&~cmask)<0)
      { sprintf(tmp,"cannot receive '%s'",nname);
	errno=0;
        message(prg,'E',tmp);
	return;
      }

      /* pgp signature to save? */
      create_sigfile(flp->sign,fname,nname,&overwrite);

      if (!keep) delete_sf(flp,0);
      sprintf(tmp,"'%s' received",nname);
      message(prg,'I',tmp);

      return;
    }

    /* encrypted tar spool file? */
    if (flp->flags&F_CRYPT)
    { 
      sprintf(cmd,"%s %s -f < %s/%d.d > %s",
	      pgp_bin,pgpvm,userspool,flp->id,tartmp);

      /* create temporary decrypted tar file */
      system(cmd);
      if (stat(tartmp,&finfo)<0 || finfo.st_size==0)
      { errno=0;
	sprintf(tmp,"cannot decrypt '%s' :",nname);
	message(prg,'E',tmp);
	return;
      }

      if (!pgppass) printf("\n\n");

    }

    /* test on dangerous files inside */

    /* compressed, encrypted or normal tar file? */
    if (flp->flags&F_COMPRESS)
      sprintf(cmd,"%s -d < %s/%d.d | %s tf -",
	      gzip_bin,userspool,flp->id,tar_bin);
    else if (flp->flags&F_CRYPT)
      sprintf(cmd,"%s tf %s",tar_bin,tartmp);
    else
      sprintf(cmd,"%s tf %s/%d.d",tar_bin,userspool,flp->id);

    /* open pipe to read tar file-info */
    if ((pp=popen(cmd,"r")) == NULL)
    { message(prg,'E',"cannot open spool file for reading");
      return;
    }

    /* loop over all files in tar archive */
    while (fgets(fname,MAXLEN-1,pp))
    {
      /* does the file already exist? */
      if ((cp=strchr(fname,'\n'))) *cp=0;
      if (overwrite!='Y' && stat(fname,&finfo)==0)
      { if (!tom) printf("Archive '%s' will overwrite:\n",nname);
        tom=1;
        printf("  %s\n",fname);
      }

      /* is it a dangerous file name? */
      if (strstr(fname,"../") || strstr(fname,"/.") || fname[0]=='/' ||
	  (fname[0]=='.' && fname[1]!='/'))
      { strcat(danger,"\n  ");
	strcat(danger,fname);
      }

    }

    pclose(pp);

    /* ask user for overwriting */
    if (tom && overwrite!='Y')
    { printf("Overwrite (yYnN)? ");
      gets(answer);
      overwrite=answer[0];
      if (toupper(overwrite)!='Y') return;
    }

    /* ask user for extracting dangerous files */
    if (*danger && overwrite!='Y')
    { printf("Archive contains files with dangerous names:%s\n",danger);
      printf("Continue with receiving (yYnN)? ");
      gets(tmp);
      overwrite=tmp[0];
      if (toupper(overwrite)!='Y') return;
    }

    /* receive from tar file */
    sprintf(tmp,"receiving from archive '%s' :",nname);
    message(prg,'I',tmp);

    /* compressed, encrypted or normal tar file? */
    if (flp->flags&F_COMPRESS)
      sprintf(cmd,"%s -d < %s/%d.d | %s xvf - 2>%s",
	      gzip_bin,userspool,flp->id,tar_bin,error);
    else if (flp->flags&F_CRYPT)
      sprintf(cmd,"%s xvf %s 2>%s",tar_bin,tartmp,error);
    else
      sprintf(cmd,"%s xvf %s/%d.d 2>%s",tar_bin,userspool,flp->id,error);

    /* receive tar archive and check for errors */
    terr=0;
    if (system(cmd)!=0) terr=1;
    if (stat(error,&finfo)==0 && finfo.st_size>10)
    { errno=0;
      inf=fopen(error,"r");
      while (fgets(line,MAXLEN-1,inf))
	if (strbeq(TAR": ",line))
	{ if (!terr)
	  { terr=1;
	    sprintf(tmp,"errors while receive '%s' :",nname);
	    message(prg,'E',tmp);
	  }
	  printf("%s",line);
	}
      fclose(inf);
    }

    /* was there an error with tar? */
    if (terr)
    { sprintf(tmp,"leaving '%s' in spool intact",nname);
      message(prg,'I',tmp);
    } else
    {
      /* delete tar spool file */
      if (!keep) delete_sf(flp,0);
    }

    cleanup();
    return;
  }

  /* receive non-tar file */

  /* save file in transfer shape? */
  if (preserve && flp->flags&F_CRYPT)
  { fname[FLEN-5]=nname[FLEN-5]=sname[FLEN-5]=0;
    strcat(fname,".pgp");
    strcat(nname,".pgp");
    strcat(sname,".pgp");
  }

  /* if file with same name already exists check what to do */
  if (checkfile(utf,fname,nname,sname,&overwrite)) return;

  /* leading . in file name? */
  if (!strchr("yYr",overwrite) && fname[0]=='.')
  {
    /* skipping? */
    if (overwrite=='S' || overwrite=='N') return;

    /* ask user what to do */
    printf("'%s' starts with a '.' which may be dangerous\n",nname);
    printf("Receive it anyway (yY), rename (r) or skip (sS) it? ");
    gets(answer);
    overwrite=answer[0];

    /* skipping this file? */
    if (toupper(overwrite)=='S') return;

    /* request for renaming? */
    if (overwrite=='r')
    { printf("Rename '%s' to? ",nname);
      if (gets(fname)==NULL || *fname=='\n' || *fname==0)
	strcpy(fname,nname);
      if ((cp=strrchr(fname,'\n'))) *cp=0;
      strcpy(nname,fname);
    }

  }

  /* safety fallback: try to delete an old file with the same name */
  unlink(fname);
  if (stat(fname,&finfo)==0) 
  { sprintf(tmp,"cannot create '%s' : file does already exist and is not deletable",fname);
    errno=0;
    message(prg,'E',tmp);
    return;
  }

  /* save file encrypted? */
  if (preserve && flp->flags&F_CRYPT)
  {
    /* copy file */
    sprintf(tmp,"%s/%d.d",userspool,flp->id);
    if (fcopy(tmp,fname,0666&~cmask)<0)
    { sprintf(tmp,"cannot receive '%s'",nname);
      errno=0;
      message(prg,'E',tmp);
      return;
    }

    if ((flp->flags&F_SOURCE || flp->flags&F_TEXT) && !quiet)
    { sprintf(tmp,"'%s' has a SOURCE or TEXT attribute, you have to decode it "
	          "after pgp-decrypting with:  recode %s:"CHARSET" '%s'",
	      nname,flp->charset,nname);
      message(prg,'W',tmp);
    }

    /* pgp signature to save? */
    create_sigfile(flp->sign,fname,nname,&overwrite);

    if (!keep) delete_sf(flp,0);
    sprintf(tmp,"'%s' received",nname);
    message(prg,'I',tmp);

    return;
  }

  /* spool file compressed or encrypted? */
  if (flp->flags&F_COMPRESS || flp->flags&F_CRYPT)
  {
    /* source or text format? */
    if ((flp->flags&F_SOURCE) || (flp->flags&F_TEXT))
    {
      /* open pipe to uncompress or decrypt spool file */
      if (flp->flags&F_COMPRESS)
	sprintf(cmd,"%s -d < %s/%d.d",gzip_bin,userspool,flp->id);
      if (flp->flags&F_CRYPT)
	sprintf(cmd,"%s %s -f < %s/%d.d",pgp_bin,pgpvm,userspool,flp->id);
      if ((pp=popen(cmd,"r")) == NULL)
      { message(prg,'E',"cannot open spool file for reading");
        return;
      }

      if (flp->flags&F_CRYPT && !pgppass) printf("\n\n");

      /* open output file */
      if ((outf=fopen(fname,"w")) == NULL)
      { sprintf(tmp,"cannot open '%s' for writing",nname);
        message(prg,'E',tmp);
	pclose(pp);
        return;
      }

      /* translate CR LF to LF and write to output file */
      crlf2lf(pp,outf,fname,nname);

      /* ready */
      pclose(pp);
      fclose(outf);

    } else  /* binary format */
    {
      sprintf(sfile,"%s/%d.d",userspool,flp->id);

      /* compressed spool file? */
      if (flp->flags&F_COMPRESS)
      {
	/* uncompress and receive binary file */
	sad[0]=gzip_bin;
	sad[1]="-dc";
	sad[2]=sfile;
	sad[3]=NULL;

	if (spawn(sad,fname,cmask)<0)
	{ errno=0;
	  sprintf(tmp,"cannot receive '%s'",nname);
	  message(prg,'E',tmp); 
	  return;
	}

      }

      /* encrypted spool file? */
      if (flp->flags&F_CRYPT)
      {
	sprintf(cmd,"%s %s -f < %s > '%s'",pgp_bin,pgpvm,sfile,fname);
	if (system(cmd)!=0)
	{ errno=0;
	  sprintf(tmp,"cannot receive '%s', pgp failed",nname);
	  message(prg,'E',tmp);
	  unlink(fname);
	  return;
	}
	if (!pgppass) printf("\n\n");

      }
    }

  } else   /* not compressed and not encrypted or keep encryption */
  {
    /* source or text format? */
    if (((flp->flags&F_SOURCE) || (flp->flags&F_TEXT)) && !flp->flags&F_CRYPT)
    {
      /* open input file */
      sprintf(sfile,"%s/%d.d",userspool,flp->id);
      if ((inf=fopen(sfile,"r")) == NULL)
      { message(prg,'E',"cannot open spool file for reading");
        return;
      }

      /* open output file */
      if ((outf=fopen(fname,"w")) == NULL)
      { sprintf(tmp,"cannot open '%s' for writing",nname);
        message(prg,'E',tmp);
        fclose(inf);
        return;
      }

      /* translate CR LF to LF and write to output file */
      crlf2lf(inf,outf,fname,nname);

      /* ready */
      fclose(inf);
      fclose(outf);

    } else   /* binary file */
    {
      /* copy file */
      sprintf(tmp,"%s/%d.d",userspool,flp->id);
      if (fcopy(tmp,fname,0666&~cmask)<0)
      { sprintf(tmp,"cannot receive '%s'",nname);
	errno=0;
        message(prg,'E',tmp);
	return;
      }
    }
  }

  /* pgp signature to save? */
/*
  if (preserve) create_sigfile(flp->sign,fname,nname,&overwrite);
*/
  /* delete spool file */
  if (!keep) delete_sf(flp,0);

  /* executable flag set? */
  if (flp->flags&F_EXE) chmod(fname,(S_IRWXU|S_IRWXG|S_IRWXO)&~cmask);

  sprintf(tmp,"'%s' received",nname);
  message(prg,'I',tmp);

  /* foreign character set in text file? */
  if ((flp->flags&F_TEXT) && !streq(flp->charset,CHARSET))
  {
    /* call GNU recode */
    sprintf(tmp,"%s:"CHARSET,flp->charset);
    sad[0]="recode";
    sad[1]=tmp;
    sad[2]=fname;
    sad[3]=NULL;
    if (spawn(sad,NULL,cmask)<0)
    { sprintf(tmp,"cannot translate character set in '%s'",nname);
      message(prg,'E',tmp);
    }

  }

  /* set the original date */
  if (*(flp->date))
  { if (!strstr(flp->date,"UTC")) strcat(flp->date," UTC");
    utb.actime=utb.modtime=get_date(flp->date,NULL);
    utime(fname,&utb);
  }

}


/*
 * crlf2lf - translate NVT format file to Unix text format file
 *
 * INPUT:  inf	  - file to read from
 *         outf   - file to write to
 *         fname  - file name to write
 */
void crlf2lf(FILE *inf, FILE *outf, const char *fname, const char *nname)
{ int c1,c2;		/* characters to read in */
  char tmp[MAXLEN];	/* temporary string */

  /* read first char */
  c1=fgetc(inf);

  /* loop until EOF */
  while ((c2=fgetc(inf)) != EOF)
  {
    /* crlf? */
    if (c1=='\r' && c2=='\n')
    {
      /* write lf */
      if(fputc(c2,outf)==EOF)
      { sprintf(tmp,"cannot write to %s",nname);
        message(prg,'E',tmp);
        return;
      }

      /* read next char */
      if ((c2=fgetc(inf)) == EOF) return;

    } else
    {
      /* write char */
      if(fputc(c1,outf)==EOF)
      { sprintf(tmp,"cannot write to %s",nname);
        message(prg,'E',tmp);
        return;
      }

    }
    c1=c2;
  }

  /* write last char */
  if(fputc(c1,outf)==EOF)
  { sprintf(tmp,"cannot write to %s",nname);
    message(prg,'E',tmp);
    return;
  }

}


/*
 * spawn  - spawn a subprocess and direct output to a file
 *
 * INPUT:  sad     - spawn argument descriptor
 *         output  - output file
 *         cmask   - protection mask
 *
 * RETURN: 0 on success, -1 on failure
 *
 */
int spawn(char **sad, const char *output, mode_t cmask)
{ int status,		/* fork status */
      fd;		/* output file descriptor */
  char tmp[MAXLEN];	/* temporary string */
  pid_t pid;		/* process id */

  /* spawn subprocess */
  pid=fork();

  /* failed? */
  if (pid<0)
  { message(prg,'E',"cannot fork subprocess");
    return(-1);
  }

  /* is this the subprocess? */
  if (pid==0)
  {
    /* redirect stdout? */
    if (output)
    {
      /* close stdout */
      close(1);

      /* open output file as stdout */
      fd=creat(output,0666&~cmask);
      if (fd!=1)
      { errno=0;
	message(prg,'E',"file descriptor mismatch");
	exit(1);
      }

    }

    /* execute program */
    execvp(sad[0],sad);

    /* oops - failed */
    cleanup();
    exit(2);
  }

  /* wait for termination of subprocess */
#ifdef NEXT
  wait(&status);
#else
  waitpid(pid,&status,0);
#endif

  /* error in subprocess? */
  if (status)
  { errno=0;
    sprintf(tmp,"%s failed",sad[0]);
    message(prg,'E',tmp);
    return(-1);
  }

  return(0);
}


/*
 * cleanexit  - clean termination routine
 */
void cleanexit()
{
  /* ignore all relevant signals */
  signal(SIGTERM,SIG_IGN);
  signal(SIGABRT,SIG_IGN);
  signal(SIGQUIT,SIG_IGN);
  signal(SIGHUP,SIG_IGN);
  signal(SIGINT,SIG_IGN);

  cleanup();

  exit(0);
}


/*
 * cleanup  - delete tmp files
 */
void cleanup()
{
#ifndef DEBUG
  unlink(tartmp);
  unlink(error);
#endif
}


/*
 * checkfile  - check file name before writing
 *
 * INPUT:  fname	- real file name
 * 	   nname	- normal file name to display
 * 	   sname	- file name for shell usage
 * 	   overwrite	- overwriting, renaming or skipping
 *
 * OUTPUT: fname	- new real file name
 * 	   nname	- new normal name to display
 * 	   sname	- file name for shell usage
 * 	   overwrite	- overwriting, renaming or skipping
 *
 * RETURN: 0 if renaming or overwriting
 *         1 if skipping
 */
int checkfile(int utf, char *fname, char *nname, char *sname, char *overwrite)
{ char
    *cp,			/* a simple string pointer */
    tmp[MAXLEN],		/* temporary string */
    answer[FLEN];		/* answer string */
  static char storeformat='c';	/* file name storing format */
  struct stat finfo;		/* information about a file */

  if (quiet) storeformat='C';

  /* rename files before receiving? */
  if (ren)
  {  printf("Rename '%s' to? ",nname);
    if (gets(fname)==NULL || *fname=='\n' || *fname==0) strcpy(fname,nname);
    if ((cp=strrchr(fname,'\n'))) *cp=0;
    strcpy(nname,fname);
  } else
  {
    /* need user request? */
    if (!strchr("CNS",storeformat))
    {
      /* found Unicode or 0x0? */
      if (utf==1)
      { printf("The next file name contains characters which are not allowed "
	       "in Unix.\nThese characters have been substituted with '_'.\n");
      }

      /* found control or meta characters? */
      if (utf&2)
        { printf("The next file name contains characters which may cause problems"
	       " with your shell or\nmay do strange things with your terminal."
	       "\nThese characters have been substituted with '_'.\n");
	if (utf&1)
	  printf("Non-valid characters for Unix file names have been substituted"
		 ", too.\n");

	/* let user choose file name format */
	/*if (strcmp(nname,sname)!=0) */
	printf("(c)omplete file name with control code characters is not "
	       "displayable\n");
	printf("(n)ormal file name: '%s'\n",nname);
	printf("(s)hell file name:  '%s'\n",sname);
	do
	{ printf("c or n may cause severe security problems! "
		 "Kids, don't try this at home!");
	  printf("Select one of c n s (or C N S for no more questions): ");
	  gets(tmp);
	  storeformat=tmp[0];
	} while (strchr("cnsCNS",storeformat)==NULL);

      }
    }

    /* set file name */
    switch (toupper(storeformat))
    { case 'N': strcpy(fname,nname); break;
      case 'S': strcpy(fname,sname);
                strcpy(nname,sname);
    }

  }

  /* does the file already exist? */
  while (stat(fname,&finfo)==0 && (*overwrite!='Y'))
  {
    /* skipping? */
    if (*overwrite=='S' || *overwrite=='N') return(1);

    /* ask user what to do */
    printf("'%s' already exists.\n"
           "Overwrite (yY), rename (r) or skip (sS) it? ",nname);
    gets(answer);
    *overwrite=answer[0];

    /* request for renaming? */
    if (*overwrite=='r')
    { printf("Rename '%s' to? ",nname);
      if (gets(fname)==NULL || *fname=='\n' || *fname==0)
	strcpy(fname,nname);
      if ((cp=strrchr(fname,'\n'))) *cp=0;
      strcpy(nname,fname);
      continue;
    }

    /* overwriting or leaving? */
    if (toupper(*overwrite)=='Y') break; else return(1);

  }

  return(0);
}


/*
 * create_sigfile  - create detached pgp signature file
 *
 * INPUT:  sign		- signature
 * 	   fname	- original file name
 * 	   overwrite	- overwriting, renaming or skipping
 *
 * RETURN: 0 if ok, -1 if failed
 */
int create_sigfile(const char *sign, const char *fname, const char *nname,
		   char *overwrite)
{ char
    *cp,			/* a simple string pointer */
    tmp[MAXLEN],		/* temporary string */
    sigfile[MAXLEN],		/* signature file */
    nsigfile[MAXLEN],		/* normal displayable signature file name */
    answer[FLEN];		/* answer string */
  struct stat finfo;		/* information about a file */

  FILE *outf;			/* file to create */

  /* no pgp signature to save? */
  if (!*sign) return(0);

  sprintf(sigfile,"%s.sig",fname);
  sprintf(nsigfile,"%s.sig",nname);

  /* signature file does already exist? */
  while (stat(sigfile,&finfo)==0 && (*overwrite!='Y'))
  {
    /* skipping? */
    if (*overwrite=='S' || *overwrite=='N') return(-1);

    /* ask user what to do */
    printf("'%s' already exists.\n"
           "Overwrite (yY), rename (r) or skip (sS) it? ",nsigfile);
    gets(answer);
    *overwrite=answer[0];

    /* request for renaming? */
    if (*overwrite=='r')
    { printf("Rename to? ");
      if (gets(nsigfile)==NULL || *nsigfile=='\n' || *nsigfile==0)
	strcpy(nsigfile,sigfile);
      if ((cp=strrchr(nsigfile,'\n'))) *cp=0;
      strcpy(sigfile,nsigfile);
      continue;
    }

    /* overwriting or leaving? */
    if (toupper(*overwrite)=='Y') break; else return(-1);

  }

  /* safety fallback: try to delete an old file with the same name */
  unlink(fname);
  if (stat(sigfile,&finfo)==0) 
  { sprintf(tmp,"cannot create '%s' : "
	        "file does already exist and is not deletable",sigfile);
    errno=0;
    message(prg,'E',tmp);
    return(-1);
  }

  if (!(outf=fopen(sigfile,"w")))
  { sprintf(tmp,"cannot create signature file '%s' ",nsigfile);
    message(prg,'E',tmp);
    return(-1);
  }

  fprintf(outf,"%s",sign);
  fclose(outf);
  sprintf(tmp,"signature file '%s' created",nsigfile);
  message(prg,'I',tmp);
  return(0);

}


/*
 * reply  - dummy function, only needed for linking
 */
void reply(int x) { }


/*
 * usage - print short help usage text
 */
int usage()
{ fprintf(stderr,"usage:  %s [-dqrpkP] [-f from] file-name [...]\n",prg);
  fprintf(stderr,"   or:  %s -n [-dqrpkP] file-number [...]\n",prg);
  fprintf(stderr,"   or:  %s [-slL] [-f from]\n",prg);
  fprintf(stderr,"   or:  %s -a [-dqrpkP] [-f from]\n",prg);
  fprintf(stderr,"   or:  %s -b [-k] [-f from] file-name [...] user[@host]\n",prg);
  fprintf(stderr,"   or:  %s -bn [-k] file-number [...] user[@host]\n",prg);
  fprintf(stderr,"   or:  %s -ba [-k] user[@host]\n",prg);
  fprintf(stderr,"options:  -l  list of files\n");
  fprintf(stderr,"          -L  full list of files\n");
  fprintf(stderr,"          -s  short list of files\n");
  fprintf(stderr,"          -n  specify a file number\n");
  fprintf(stderr,"          -a  specify all files\n");
  fprintf(stderr,"          -f  specify only files from this user\n");
  fprintf(stderr,"          -d  delete file (do not receive)\n");
  fprintf(stderr,"          -q  quiet mode: no questions asked\n");
  fprintf(stderr,"          -r  rename file when receiving\n");
  fprintf(stderr,"          -p  preserve tar or pgp file formats\n");
  fprintf(stderr,"          -k  keep file in spool after receiving\n");
  fprintf(stderr,"          -P  receive file to stdout (decrypt but don't untar)\n");
  fprintf(stderr,"          -b  bounce (forward) a file\n");
  fprintf(stderr,"examples:  receive -af frams  ! receive all files from user frams\n");
  fprintf(stderr,"           receive -n 1       ! receive file number 1\n");
  fprintf(stderr,"           receive blubb      ! receive file blubb\n");
  return(2);
}
