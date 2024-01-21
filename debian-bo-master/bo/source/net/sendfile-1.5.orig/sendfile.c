/*
 * File:        sendfile.c
 *
 * Author:      Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 *
 * Contrib.:	Rainer Bawidamann (widi@sol.wohnheim.uni-ulm.de)
 * 		Martin Buck (Martin-2.Buck@student.uni-ulm.de)
 *
 * History:     11 Aug 95   Framstag	initial version
 *              12 Aug 95   Framstag	elm alias support
 *              10 Sep 95   Framstag	added delete and resend function
 *               6 Feb 96   Framstag	added ATTR EXE
 *               7 Feb 96   Framstag	check for already compressed files
 *              20 Feb 96   Framstag	follow forward address if given
 *              21 Feb 96   widi        better Solaris-2 support
 *              22 Feb 96   Framstag    added bouncing (forwarding) of files
 *	        23 Feb 95   mbuck       bug fixes for getting $HOME
 *	        27 Feb 95   Framstag	added quiet options
 *	         8 Mar 95   Framstag	catch up signals in cleanup()
 *	        17 Mar 95   Framstag	set umask (for tmp-files)
 *	        23 Mar 95   Framstag	added percentage output
 *	        24 Mar 95   Framstag	$TMPDIR replaces $HOME for tmp-files
 *              28 Mar 96   Framstag    extended search for support programs
 *               2 Apr 96   Framstag    added forward address to COMMENT
 *               6 Apr 96   Framstag    changed transaction message format
 *					added overwrite option
 *              10 Apr 96   Framstag    better usage text
 *              12 Apr 96   Framstag    added pgp support
 *              16 Apr 96   Framstag    better pgp signature creation
 *         	17 Apr 96   Framstag	new error handling for open_connection
 *         	18 Apr 96   Framstag	verbose mode displays system() commands
 * 					allowed multiple IDs for pgp encryption
 *         	20 Apr 96   Framstag	added pgp IDEA encryption option
 * 		24 Apr 96   Framstag	changed bouncing option syntax
 * 		 8 May 96   Framstag	correct bug when bouncing
 * 		10 May 96   Framstag	allowed multiple forwards
 * 		14 May 96   Framstag	moved send_data() to io.c
 * 		21 May 96   Framstag	corrected bug when sending archive
 * 		23 May 96   Framstag	added check for writeable tmp-files
 * 					added -P option (read from stdin)
 *		13 Aug 96   Framstag    fixed wrong "(compressed)" output
 *               4 Sep 96   Heiko	some fixes for IRIX
 *              11 Sep 96   Heiko	corrected redirection comment bug
 *
 * The sendfile client of the sendfile package.
 * Sends one or more files to the sendfiled of the destination system.
 *
 * Copyright © 1995,1996 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <pwd.h>
#include <fcntl.h>
#include <signal.h>
#include <time.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>

#include "config.h"		/* various #defines */
#include "string.h"		/* extended string functions */
#include "net.h"		/* the network routines */
#include "io.h"			/* (socket) read/write */
#include "message.h"		/* information, warning and error messages */
#include "spool.h"		/* operations on files in the sendfile spool */
#include "utf7.h"		/* UTF-7 coding */
#include "destination.h"	/* check recipient and host */

#ifndef AIX
  #ifndef CONVEXOS
    FILE *popen(const char *, const char *);
  #endif
  int pclose(FILE *);
  #if defined(IRIX) || defined(LINUX)
    #include <getopt.h>
  #else
    int getopt(int, char * const *, const char *);
    extern char *optarg;
  #endif
#endif

#if !defined(HPUX)
  int gethostname(char *, int);
#endif

#ifdef NEXT
  int shutdown(int, int);
#endif

#ifdef SOLARIS2
  #ifdef _SVID_GETTOD
    int gettimeofday(struct timeval *);
  #else
    int gettimeofday(struct timeval *, void *);
  #endif
#endif

#if defined(AIX) || defined(ULTRIX)
  #include "bsd.h"
  int gettimeofday(struct timeval *, struct timezone *);
#endif

/* print short help usage text */
int usage();

/* clean termination routine */
void cleanexit();

/* delete tmp-file  and send LOG command to local server */
void cleanup();

/* encrypt a file with pgp */
void pgp_encrypt(int, const char *, char *);

/* create detached pgp signature file and send SIGN header command */
void pgp_sign(const char *, const char *, int);

/* create and open outgoing spool header file */
FILE *outspool(const char *, const char *, char *);

/* test if there is an forward address set */
int check_forward(int, char *, char *, char *);

/* start local spool daemon for outgoing files */
void start_spooldaemon(char *);

/* create temporary user outgoing log file */
void outlog(char *, char *, char *, char *);


/* global variables */
int pgppass=0,		/* flag if the pgp password is set as an env variable */
    verbose=0,		/* flag for verbose mode */
    client=1,		/* flag to determine client or server */
    quiet=0;		/* quiet mode */
char *prg,		/* name of the game */
     *pgpvm,		/* pgp verbose mode string */
     pw_name[FLEN],	/* own user name */
     outlogf[MAXLEN],	/* user temporary outgoing log file */
     userspool[MAXLEN],	/* user spool directory */
     tar_bin[MAXLEN],	/* the tar binary */
     gzip_bin[MAXLEN],	/* the gzip binary */
     pgp_bin[MAXLEN],	/* the pgp binary */
     stdintmp[MAXLEN],	/* name of stdin temporary file */
     tartmp[MAXLEN],	/* name of tar temporary file */
     gziptmp[MAXLEN],	/* name of gzipped temporary file */
     texttmp[MAXLEN],	/* name of text temporary file in NVT telnet format */
     pgptmp[MAXLEN];	/* name of pgp temporary file */


int main(int argc, char *argv[])
{ extern int
    optind;		/* number of scanned args with getopt */
  int
    n,          	/* simple loop count */
    pid,        	/* current proccess id */
    sockfd,     	/* socket file descriptor */
    opt,        	/* option to test for */
    fn,         	/* file number in argv */
    stdinf,		/* read file from stdin */
    ch,         	/* character to read in from file */
    del,        	/* flag for deleting previous sent files */
    tar,        	/* flag for sending files as tar archive */
    exe,        	/* flag for sending executable */
    text,       	/* flag for sending text file */
    source,     	/* flag for sending source code file */
    spool,  	   	/* flag for outgoing spooling */
    bounce,     	/* flag for bouncing files */
    compress,   	/* flag for sending compressed */
    pgpcrypt, 		/* pgp public key or IDEA encryption */
    hopcount,		/* count for forwarding addresses */
    overwrite,   	/* flag for overwriting already sent files */
    do_compress;	/* flag for really do compressing */
  unsigned long
    size,       	/* size of file to send */
    orgsize;    	/* original file size uncompressed */
  char
    *cp,		/* simple string pointer */
    *pop,		/* pgp option string pointer */
    *fnp,		/* file name pointer */
    *type,	 	/* type of file */
    file[FLEN],		/* name of file to send */
    sdfn[FLEN],		/* spool data file name */
    shfn[FLEN],		/* spool header file name */
    archive[FLEN],	/* name of archive file */
    recipient[FLEN], 	/* recipient at serverhost */
    bouncearg[DLEN],   	/* bouncing files argument */
    sizes[FLEN],	/* original and compressed file size */
    user[FLEN], 	/* local user name */
    tmpdir[FLEN], 	/* user tmpdir directory */
    date[DLEN], 	/* date of file */
    host[FLEN], 	/* name of serverhost */
    pgpopt[FLEN], 	/* options for pgp */
    pgprid[FLEN], 	/* pgp recipient id */
    pgpsign[FLEN], 	/* pgp signature option */
    localhost[FLEN], 	/* name of the local host */
    redirect[MAXLEN], 	/* redirected comment */
    cmd[MAXLEN], 	/* cmd string for system-call */
    line[MAXLEN], 	/* one line of text */
    tmp[MAXLEN],	/* temporary string */
    comment[MAXLEN],	/* file comment */
    outgoing[MAXLEN],	/* outgoing spool directory */
    oshfn[MAXLEN],	/* outgoing spool header file name */
    osdfn[MAXLEN],	/* outgoing spool data file name */
    filelist[OVERSIZE],	/* list of files for tar command */
    *cft[]=		/* file types which are not compressible */
    { "compress","zip","zoo","frozen","gif","jpg","jpeg","mpg","mpeg","" };
  FILE
    *shf,		/* spool header file */
    *pipe,		/* pipe input stream */
    *oshf,		/* outgoing spool header file */
    *inf,		/* input file */
    *outf;		/* output file */
  struct passwd *pwe;	/* password entry */
  struct stat finfo;	/* information about a file */
  char
    utf_name[LEN_UNI],	/* name in UTF-7 format */
    iso_name[LEN_ISO];	/* name in ISO Latin-1 format */
  struct hostlist
    *hls; 		/* host list start */
  struct outfilelist
    *oflp;		/* outgoing file list pointer */


  del=0;
  tar=0;
  exe=0;
  text=0;
  spool=0;
  quiet=0;
  source=0;
  bounce=0;
  sockfd=0;
  stdinf=0;
  verbose=0;
  hopcount=0;
  compress=1;
  pgpcrypt=0;
  overwrite=0;
  *date=0;
  *tmpdir=0;
  *comment=0;
  *archive=0;
  *redirect=0;
  *filelist=0;
  *pgprid=0;
  *pgpopt=0;
  *pgpsign=0;
  type="BINARY";
  oshf=NULL;
  pid=(int)getpid();

  prg=argv[0];
  if ((cp=strrchr(prg,'/'))) prg=cp+1;

  if (getenv("PGPPASS"))
  { pgppass=1;
    pgpvm="+verbose=0";
  } else
  { pgppass=0;
    pgpvm="+verbose=1";
  }

  /* scann the command line on options */
  while ((opt=getopt(argc, argv, "vVtsuqoQPdSh?a:c:p:b:")) > 0)
  { switch (opt)
    { case ':':
      case 'h':
      case '?': exit(usage());
      case 'd': del=1; break;
      case 't': text=1; break;
      case 'S': spool=1; break;
      case 'q': quiet=1; break;
      case 'Q': quiet=2; break;
      case 'P': stdinf=1; break;
      case 's': source=1; break;
      case 'v': verbose=1; break;
      case 'u': compress=0; break;
      case 'o': overwrite=1; break;
      case 'c': strcpy(comment,optarg); break;
      case 'a': tar=1; strcpy(archive,optarg); break;
      case 'b': bounce=1; strcpy(bouncearg,optarg); break;
      case 'p': sprintf(pgpopt,"%s\n%s",pgpopt,optarg); break;
      case 'V': message(prg,'I',"version "VERSION" revision "REVISION); exit(0);
    }
  }

  /* too few arguments? */
  if (argc-optind<2) exit(usage());

  /* incompatible options? */
  if (bounce)
  { if (!(streq(bouncearg,"k=y") || streq(bouncearg,"k=n")))
      message(prg,'F',"wrong bouncing argument");
    if (source||text||tar||del|stdinf)
      if (quiet<2) message(prg,'W',"you cannot use any other option "
 		                   "when bouncing a file - ignored");
    text=source=tar=compress=del=stdinf=0;
  }

  if (del)
  { if (source||text||tar||overwrite||stdinf||*comment||*pgpopt)
      if (quiet<2) message(prg,'W',"you cannot use any other option "
	                           "when deleting a file - ignored");
    text=source=tar=compress=stdinf=0;
  }

  if (stdinf)
  { if (tar)
      if (quiet<2) message(prg,'W',"you cannot send stdin as an archive file; "
	                           "-a option will be ignored");
    tar=0;
  }

  if (tar)
  { if (source)
      if (quiet<2) message(prg,'W',"option SOURCE is not allowed when "
	                           "sending in archive format - ignored");
    if (text)
      if (quiet<2) message(prg,'W',"option TEXT is not allowed when "
	                           "sending in archive format - ignored");
    text=source=0;
  }

  /* correct comment? */
  if (*comment && argc-optind>2 && !tar)
  { errno=0;
    message(prg,'F',"you can only comment a single file");
  }

  /* check pgp options */
  if(*pgpopt)
  { pop=pgpopt;

    /* conventional IDEA encryption? */
    if (streq(pop,"\nc"))
    { pgpcrypt='c';
      compress=0;
    }
    else /* check other pgp options */
    {
      while (*pop)
      { pop++;

	/* pgp encryption? */
	if (*pop=='e')
	{ pgpcrypt='e';
	  compress=0;
	  pop++;

	  /* is there a recipient id? */
	  if (*pop>'\n')
	  { if (*pop=='=') pop++;
	    strcpy(pgprid,pop);

	    /* cut off any more options */
	    if ((cp=strchr(pgprid,'\n')))
	    { *cp=0;
	      pop=strchr(pop,'\n');
	    } else
	      *pop=0;

	  }

	  continue;
	}

	/* pgp signature? */
	if (*pop=='s')
	{ strcpy(pgpsign," ");
	  pop++;

	  /* is there a signature id? */
	  if (*pop>'\n')
	  { if (*pop=='=') pop++;
	    sprintf(pgpsign,"-u '%s",pop);

	    /* cut off any more options */
	    if ((cp=strchr(pgpsign,'\n')))
	    { *cp=0;
	      pop=strchr(pop,'\n');
	    } else
	      *pop=0;

	    strcat(pgpsign,"'");
	  }

	  continue;
	}

	/* wrong pgp options */
	errno=0;
	sprintf(tmp,"wrong pgp option, see 'man %s'",prg);
	message(prg,'F',tmp);

      }
    }
  }

  /* protect all tmp-files */
  umask(~(S_IRUSR|S_IWUSR));

  /* support programs defaults */
  strcpy(tar_bin,TAR);
  strcpy(pgp_bin,PGP);
  strcpy(gzip_bin,GZIP);

  /* look for environment variables */
  if ((cp=getenv("SF_TAR")))	strcpy(tar_bin,cp);
  if ((cp=getenv("SF_PGP")))	strcpy(pgp_bin,cp);
  if ((cp=getenv("SF_GZIP")))	strcpy(gzip_bin,cp);

  /* do the support programs really exist? */
  if (access(tar_bin,X_OK)<0)	strcpy(tar_bin,"tar");
  if (access(pgp_bin,X_OK)<0)	strcpy(pgp_bin,"pgp");
  if (access(gzip_bin,X_OK)<0)	strcpy(gzip_bin,"gzip");

  /* get the local host name */
  if (gethostname(localhost,FLEN-1)<0) strcpy(localhost,"localhost");

  /* get own user name, recipient name and host */
  destination(argc,argv,user,recipient,host);

  /* get the own user name and tmpdir */
  if ((pwe=getpwuid(getuid())) == NULL)
    message(prg,'F',"cannot determine own user name");
  strcpy(pw_name,pwe->pw_name);
  if ((cp=getenv("TMPDIR")))
    strcpy(tmpdir,cp);
  else
    strcpy(tmpdir,"/tmp");
  if (stat(tmpdir,&finfo)<0 || !(finfo.st_mode&S_IRWXU))
    strcpy(tmpdir,pwe->pw_dir);

  /* set various file names */
  sprintf(userspool,SPOOL"/%s",pw_name);
  sprintf(outlogf,"%s/.outlog",userspool);
  sprintf(tartmp,"%s/.sendfile_%d.tar",tmpdir,pid);
  sprintf(gziptmp,"%s/.sendfile_%d.gzip",tmpdir,pid);
  sprintf(texttmp,"%s/.sendfile_%d.text",tmpdir,pid);
  sprintf(pgptmp,"%s/.sendfile_%d.pgp",tmpdir,pid);
  sprintf(stdintmp,"%s/.sendfile_%d",tmpdir,pid);

  /* check tmp files */
  unlink(tartmp);
  unlink(gziptmp);
  unlink(texttmp);
  unlink(pgptmp);
  unlink(stdintmp);
  if (stat(tartmp,&finfo)==0)
  { sprintf(tmp,"tmp-file %s does already exist and cannot be deleted",tartmp);
    message(prg,'F',tmp);
  }
  if (stat(gziptmp,&finfo)==0)
  { sprintf(tmp,"tmp-file %s does already exist and cannot be deleted",gziptmp);
    message(prg,'F',tmp);
  }
  if (stat(texttmp,&finfo)==0)
  { sprintf(tmp,"tmp-file %s does already exist and cannot be deleted",texttmp);
    message(prg,'F',tmp);
  }
  if (stat(pgptmp,&finfo)==0)
  { sprintf(tmp,"tmp-file %s does already exist and cannot be deleted",pgptmp);
    message(prg,'F',tmp);
  }
  if (stat(stdintmp,&finfo)==0)
  { sprintf(tmp,"tmp-file %s does already exist and cannot be deleted",stdintmp);
    message(prg,'F',tmp);
  }

  /* look for the compress command */
/*
  if (compress)
  { if (access(gzip_bin,X_OK)<0)
    { compress=0;
      if (quiet<2) message(prg,'W',"no gzip found - sending uncompressed");
    }
  }
*/

  /* enable simple interrupt handler */
  signal(SIGTERM,cleanexit);
  signal(SIGABRT,cleanexit);
  signal(SIGQUIT,cleanexit);
  signal(SIGHUP,cleanexit);
  signal(SIGINT,cleanexit);

  /* file as stdin data stream? */
  if (stdinf)
  {
    /* write stdin to tmp-file */
    if (!(outf=fopen(stdintmp,"w")))
    { sprintf(tmp,"cannot open tmp-file %s",stdintmp);
      message(prg,'F',tmp);
    }
    while ((ch=getchar())!=EOF) putc(ch,outf);
    fclose(outf);

  }

  /* determine the file type */
  if (text)   type="TEXT";
  if (source) type="SOURCE";

  if (!spool)
  {
    /* name the local host */
    if (streq(host,"127.0.0.1") || streq(host,"0")) strcpy(host,localhost);

    /* try to connect to the recipient's server */
    for (hopcount=1; hopcount<11; hopcount++)
    {
      /* tell where to send to */
      sprintf(tmp,"opening connection to %s@%s",recipient,host);
      if (quiet<2) message(prg,'I',tmp);

      /* initiate the connection to the server */
      sockfd=open_connection(host,SAFT);
      if (sockfd==-1) sprintf(tmp,"cannot create a network socket");
      if (sockfd==-2) sprintf(tmp,"cannot open connection to %s",host);
      if (sockfd==-3) sprintf(tmp,"%s is unknown (name server down?)",host);
      if (sockfd<0)
      { errno=0;
	message(prg,'F',tmp);
      }

      /* no remote server or protocol error? */
      sock_getline(sockfd,line);
      if (verbose && *line) printf("%s\n",line);
      if (!strbeq(line,"220 ") || !strstr(line,"SAFT"))
      { errno=0;
	sprintf(tmp,"No SAFT server on port %d at %s",SAFT,host);
	message(prg,'F',tmp);
      }

      /* send constant header lines */
      sprintf(tmp,"FROM %s",user);
      sendheader(sockfd,tmp);
      sprintf(tmp,"TO %s",recipient);
      sock_putline(sockfd,tmp);

      /* is there a forward set? */
      if (check_forward(sockfd,recipient,host,redirect)) continue;

      /* test if server can receive files */
      sock_putline(sockfd,"FILE test");
      if (check_forward(sockfd,recipient,host,redirect)) continue;

      /* connection is successfull */
      break;

    }

    if (hopcount>10)
    { errno=0;
      message(prg,'F',"maximum hopcount reached (forward loop?)");
    }

  }
  else /* outgoing spooling */
  {
    /* does the outgoing spool exist? */
    strcpy(outgoing,SPOOL"/OUTGOING");
    if (stat(outgoing,&finfo)<0 || !S_ISDIR(finfo.st_mode))
    { sprintf(tmp,"spool directory %s does not exist",outgoing);
      message(prg,'F',tmp);
    }

    /* and does it have the correct protection? */
    if (!((finfo.st_mode&S_ISVTX) && (finfo.st_mode&S_IRWXO)))
    { sprintf(tmp,"spool directory %s has wrong protection (must have 1777)",
	      outgoing);
      message(prg,'F',tmp);
    }

    /* name the local host */
    if (streq(host,"127.0.0.1") || streq(host,"0")) strcpy(host,localhost);

  }

  /* bouncing files? */
  if (bounce)
  {
    /* does the spool directory exist? */
    if (chdir(userspool)<0)
    { sprintf(tmp,"cannot access spool directory %s",userspool);
      message(prg,'F',tmp);
    }

    /* main loop over the spool file names */
    for (fn=optind; fn<argc-1; fn++)
    { sprintf(sdfn,"%s.d",argv[fn]);
      sprintf(shfn,"%s.h",argv[fn]);

      /* try to open spool header file */
      if (!(shf=fopen(shfn,"r")))
      { sprintf(tmp,"cannot open spool file #%s",argv[fn]);
	message(prg,'E',tmp);
        continue;
      }

      /* write to outgoing spool? */
      if (spool)
      {
	/* open outgoing spool header file */
	if ((oshf=outspool(pw_name,outgoing,oshfn))==NULL)
	  message(prg,'F',"cannot create outgoing spool file");

	/* TAB as whitespace is needed here, because scanspool insists on it */
	fprintf(oshf,"FROM\t%s\n",user);
	fprintf(oshf,"TO\t%s@%s\n",recipient,host);

      }

      *comment=0;
      compress=0;

      /* scan spool header file and forward it */
      while (fgets(line,MAXLEN-1,shf))
      { str_trim(line);

	/* corrupt header line? */
	if (!strchr(line,' ')) continue;

	/* store original from as comment */
	if (strbeq(line,"FROM"))
	{ strcpy(comment,strchr(line,' ')+1);
	  if ((cp=strchr(comment,' ')))
	  { *cp=0;
	    sprintf(tmp,"%s+ACA-(%s)",comment,cp+1);
	    strcpy(comment,tmp);
	  }
	  continue;
	}

	/* store size and file name for later processing */
	if (strbeq(line,"FILE")) utf2iso(0,tmp,file,tmp,&line[5]);
	if (strbeq(line,"TYPE") && strstr(line,"COMPRESSED")) compress=1;
	if (strbeq(line,"SIZE ")) 
	{ sscanf(line,"SIZE %ld",&size);
	  strcpy(sizes,strchr(line,' ')+1);
	}

	/* is there already a comment line? */
	if (strbeq(line,"COMMENT"))
	{ sprintf(line,"%s+AA0ACg-forward+ACA-from+ACA-%s",line,comment);
	  if (*redirect)
	  { sprintf(line,"%s+AA0ACg-%s",comment,redirect);
	  }
	  if (spool)
	    fprintf(oshf,"%s\n",line);
	  else
	  { sock_putline(sockfd,line);
	    getreply(sockfd);
	  }
	  *comment=0;
	} else
	  if (spool)
	    fprintf(oshf,"%s\n",line);
	  else
	    sendheader(sockfd,line);
      }

      /* send comment if not already done */
      if (*comment)
      { iso2utf(tmp,"forward from ");
	sprintf(line,"COMMENT %s%s",tmp,comment);
	if (*redirect)
	{ sprintf(tmp,"\r\n%s",redirect);
	  iso2utf(comment,tmp);
	  strcat(line,comment);
	}
	if (spool)
	  fprintf(oshf,"%s\n",line);
	else
	{ sock_putline(sockfd,line);
	  getreply(sockfd);
	}
      }

      fclose(shf);

      /* check the file size */
      if (stat(sdfn,&finfo)<0 || size!=finfo.st_size)
      { sprintf(tmp,"spool file #%s has wrong size count - ignored",argv[fn]);
	message(prg,'E',tmp);
	if (spool) fclose(oshf);
	continue;
      }

      /* copy spool file to outgoing */
      if (spool)
      { fclose(oshf);
	strcpy(osdfn,oshfn);
	osdfn[strlen(osdfn)-1]='d';
	if (fcopy(sdfn,osdfn,S_IRUSR|S_IWUSR)<0)
	{ unlink(oshfn);
	  unlink(osdfn);
	}
	else
	{
	  if (streq(bouncearg,"k=n"))
	  { unlink(shfn);
	    unlink(sdfn);
	  }
	  strcpy(tmp,oshfn);
	  oshfn[strlen(oshfn)-1]='h';
	  rename(tmp,oshfn);
	  sprintf(tmp,"'%s' spooled",file);
	  message(prg,'I',tmp);
	}

      } 
      else /* forward the spool data file */
      { 
	if (send_data(sockfd,size,sdfn,file,compress,quiet)==0)
        { 
	  /* log data */
	  outlog(recipient,host,file,sizes);
	  
	  /* if not keep file delete the spool files */
	  if (streq(bouncearg,"k=n"))
	  { unlink(shfn);
	    unlink(sdfn);
	  }

	}
      }
    }
  }

  /* sending tar-archive */
  else if (tar)
  {
    /* translate the archive name to UTF-7 */
    iso2utf(utf_name,archive);

    /* collect all files */
    for (n=optind; n<argc-1; n++)
    { strcat(filelist," ");
      strcat(filelist,argv[n]);
    }

    /* do the tar, man! :-) */
    if (verbose)
      sprintf(cmd,"trap \"rm -f %s 2>/dev/null\" 1 2 3;%s cvf %s %s",
	      tartmp,tar_bin,tartmp,filelist);
    else
    { if (!quiet) printf("making archive...\r");
      fflush(stdout);
      sprintf(cmd,"trap \"rm -f %s 2>/dev/null\" 1 2 3;%s cf %s %s",
	      tartmp,tar_bin,tartmp,filelist);
    }
    if (verbose) printf("%s\n",strchr(cmd,';')+1);
    if (system(cmd)) message(prg,'F',"cannot create archive file");

    /* get the file name and size */
    strcpy(file,tartmp);
    if (stat(file,&finfo)<0) message(prg,'F',"cannot access tmp file");
    orgsize=finfo.st_size;
    size=orgsize;

    /* compressed mode? */
    if (compress)
    {
      /* compress tar-archive with gzip */
      if (!quiet) printf("compressing...       \r");
      fflush(stdout);
      sprintf(cmd,"trap \"rm -f %s %s 2>/dev/null\" 1 2 3;%s < %s > %s",
	      tartmp,gziptmp,gzip_bin,tartmp,gziptmp);
      if (verbose) printf("call to shell: %s\n",strchr(cmd,';')+1);
      if (system(cmd)) message(prg,'F',"cannot compress archive file");

      strcpy(file,gziptmp);
    }

    /* pgp encryption? */
    if (pgpcrypt) pgp_encrypt(pgpcrypt,pgprid,file);

    /* pgp signature to add? */
    if (*pgpsign) pgp_sign(pgpsign,file,sockfd);

    /* get the file size */
    if (stat(file,&finfo)<0) message(prg,'F',"cannot access tmp file");
    size=finfo.st_size;
    sprintf(sizes,"%ld %ld",size,orgsize);

    /* write to outgoing spool? */
    if (spool)
    {
      /* open outgoing spool header file */
      if ((oshf=outspool(pw_name,outgoing,oshfn))==NULL)
	message(prg,'F',"cannot create outgoing spool file");

      /* TAB as whitespace is needed here, because scanspool insists on it */
      fprintf(oshf,"FROM\t%s\n",user);
      fprintf(oshf,"TO\t%s@%s\n",recipient,host);
      fprintf(oshf,"FILE\t%s\n",utf_name);
      if (compress)
	fprintf(oshf,"TYPE\tBINARY COMPRESSED\n");
      else if (pgpcrypt)
	fprintf(oshf,"TYPE\tBINARY CRYPTED\n");
      else
	fprintf(oshf,"TYPE\tBINARY\n");
      fprintf(oshf,"SIZE\t%s\n",sizes);
      fprintf(oshf,"ATTR\tTAR\n");
    }
    else /* send header lines */
    {
      sprintf(tmp,"FILE %s",utf_name);
      sendheader(sockfd,tmp);
      if (compress)
	sendheader(sockfd,"TYPE BINARY COMPRESSED");
      else if (pgpcrypt)
	sendheader(sockfd,"TYPE BINARY CRYPTED");
      else
	sendheader(sockfd,"TYPE BINARY");
      sprintf(tmp,"SIZE %s",sizes);
      sendheader(sockfd,tmp);
      sendheader(sockfd,"ATTR TAR");
    }

    /* comment to send? */
    if (*comment || *redirect)
    { *line=0;
      if (*comment) strcat(line,comment);
      if (*redirect) 
      { if (*line)
	  sprintf(line,"%s\r\n%s",line,redirect);
	else
	  strcat(line,redirect);
      }
      iso2utf(tmp,line);
      if (spool)
	fprintf(oshf,"COMMENT\t%s\n",tmp);
      else
      { sprintf(line,"COMMENT %s",tmp);
	sock_putline(sockfd,line);
	getreply(sockfd);
      }
    }

    /* send the file data */
    if (spool)
    { fclose(oshf);
      strcpy(osdfn,oshfn);
      osdfn[strlen(osdfn)-1]='d';
      if (fcopy(file,osdfn,S_IRUSR|S_IWUSR)<0)
      { unlink(oshfn);
	unlink(osdfn);
      }
      else
      { strcpy(tmp,oshfn);
	oshfn[strlen(oshfn)-1]='h';
	rename(tmp,oshfn);
	sprintf(tmp,"'%s' spooled",archive);
	message(prg,'I',tmp);
      }
    } else
      if (send_data(sockfd,size,file,archive,compress||pgpcrypt,quiet)==0)
	outlog(recipient,host,utf_name,sizes);

  } 
  else /* sending or deleting single files */
  {
    /* main loop over the file names */
    for (fn=optind; fn<argc-1; fn++)
    {
      /* file from stdin? */
      if (stdinf)
      {
	/* get real file name and transmission file name */
	strcpy(file,stdintmp);
	strcpy(iso_name,argv[fn]);
	stdinf=0;
      }
      else /* normal file */
      {
	/* get real file name */
	strcpy(file,argv[fn]);

	/* get the file name without path */
	fnp=strrchr(file,'/');
	if (!fnp) fnp=file; else fnp++;

	/* get transmission file name */
	strcpy(iso_name,fnp);

      }

      /* translate the file name into UTF-7 */
      iso2utf(utf_name,iso_name);

      /* write to outgoing spool? */
      if (spool)
      {
	/* delete file? */
	if ((del || overwrite) && (hls=scanoutspool(user)))
	{
	  /* search for file in outgoing spool */
	  for (oflp=hls->flist; oflp->next; oflp=oflp->next)
	  {
	    /* if found, then delete it */
	    if (streq(oflp->fname,utf_name))
 	    { unlink(oflp->oshfn);
	      oflp->oshfn[strlen(oflp->oshfn)-1]='d';
	      unlink(oflp->oshfn);
	      sprintf(tmp,"deleted %s from outgoing spool",iso_name);
	      if (del && quiet<2) message(prg,'I',tmp);
	    }

	  }
	}

	if (del) continue;
	
	/* open outgoing spool header file */
	if ((oshf=outspool(pw_name,outgoing,oshfn))==NULL)
	  message(prg,'F',"cannot create outgoing spool file");

	fprintf(oshf,"FROM\t%s\n",user);
	fprintf(oshf,"TO\t%s@%s\n",recipient,host);
	fprintf(oshf,"FILE\t%s\n",utf_name);
	if (del || overwrite) fprintf(oshf,"DEL\n");
	if (del) continue;
      }
      else /* send header lines */
      {
	/* send file name */
	sprintf(tmp,"FILE %s",utf_name);
	sendheader(sockfd,tmp);

	/* delete file? */
	if (del || overwrite) sendheader(sockfd,"DEL");
	if (del)
	{ sprintf(tmp,"file %s deleted",iso_name);
	  if (quiet<2) message(prg,'I',tmp);
	  continue;
	}

      }

      /* is the file readable? */
      if (stat(file,&finfo)<0)
      { sprintf(tmp,"cannot access file %s",file);
	message(prg,'E',tmp);
	if (spool)
	{ fclose(oshf);
	  unlink(oshfn);
	}
	continue;
      }

      /* is it a regular file? */
      if (!S_ISREG(finfo.st_mode))
      { sprintf(tmp,"%s is not a regular file, skipping",file);
	message(prg,'E',tmp);
	if (spool)
	{ fclose(oshf);
	  unlink(oshfn);
	}
	continue;
      }

      /* is it a executable? */
      if (finfo.st_mode&S_IXUSR) exe=1; else exe=0;

      /* get the original file size */
      strftime(date,20,"%Y-%m-%d %H:%M:%S",gmtime(&finfo.st_mtime));
#ifdef HPUX
      /* dirty hack around HPUX strftime bug */
      date[17]=0;
      strcat(date,"00");
#endif

      /* text or source mode? */
      if (text || source) {

	/* open and test file to send and open tmp-file */
	inf=fopen(file,"r");
	outf=fopen(texttmp,"w");
	if (!inf)
	{ sprintf(tmp,"cannot open file %s",file);
	  message(prg,'E',tmp);
	  if (spool)
	  { fclose(oshf);
	    unlink(oshfn);
	  }
	  continue;
	}
	if (!outf) message(prg,'F',"cannot open tmp-file");

	/* convert file to NVT format */
	do
	{ ch=fgetc(inf);
	  if (ch!=EOF)
	  { if (ch=='\n')
	      fprintf(outf,"\r\n");
	    else
	      fputc(ch,outf);
	  }
	} while (!feof(inf));

	fclose(inf);
	fclose(outf);
	strcpy(file,texttmp);
      }

      /* get the original file size */
      stat(file,&finfo);
      orgsize=finfo.st_size;

      do_compress=compress;

      /* check if file is compressible */
      if (compress)
      {
	/* determine file type */
	sprintf(cmd,"file '%s'",file);
	if ((pipe=popen(cmd,"r")) && fgets(line,MAXLEN-1,pipe))
	{ str_tolower(line);

	  /* loop over all non-compressible file type strings */
	  for (n=0;*cft[n];n++)

	    /* is this file a not compressible one? */
	    if (strstr(line,cft[n]))
	    { do_compress=0;
 	      break;
	    }

	}
	pclose(pipe);
      }

      /* compressed mode? */
      if (do_compress)
      {
	/* compress tmp-file with gzip */
	if (!quiet) printf("compressing...       \r");
	fflush(stdout);
	sprintf(cmd,"trap \"rm -f %s 2>/dev/null\" 1 2 3;%s < '%s' > %s",
		gziptmp,gzip_bin,file,gziptmp);
	if (verbose) printf("call to shell: %s\n",strchr(cmd,';')+1);
	if (system(cmd))
	{ sprintf(tmp,"cannot compress %s",file);
	  message(prg,'E',tmp);
	  if (spool)
	  { fclose(oshf);
	    unlink(oshfn);
	  }
	  continue;
	}
	strcpy(file,gziptmp);

      } else
	size=orgsize;

      /* pgp encryption? */
      if (pgpcrypt) pgp_encrypt(pgpcrypt,pgprid,file);

      /* pgp signature to add? */
      if (*pgpsign) pgp_sign(pgpsign,file,sockfd);

      /* get the file size */
      if (stat(file,&finfo)<0) message(prg,'F',"cannot access tmp file");
      size=finfo.st_size;
      sprintf(sizes,"%ld %ld",size,orgsize);

      /* write to outgoing spool? */
      if (spool)
      {
	if (do_compress)
	  fprintf(oshf,"TYPE\t%s COMPRESSED\n",type);
	else if (pgpcrypt)
	  fprintf(oshf,"TYPE\t%s CRYPTED\n",type);
	else
	  fprintf(oshf,"TYPE\t%s\n",type);
	fprintf(oshf,"SIZE\t%s\n",sizes);
	fprintf(oshf,"DATE\t%s\n",date);
	if (exe)
	  fprintf(oshf,"ATTR\tEXE\n");
	else
	  fprintf(oshf,"ATTR\tNONE\n");
      }
      else /* send the header information */
      {
	if (do_compress)
	  sprintf(tmp,"TYPE %s COMPRESSED",type);
	else if (pgpcrypt)
	  sprintf(tmp,"TYPE %s CRYPTED",type);
	else
	  sprintf(tmp,"TYPE %s",type);
	sendheader(sockfd,tmp);
	sprintf(tmp,"SIZE %s",sizes);
	sendheader(sockfd,tmp);
	sprintf(tmp,"DATE %s",date);
	sendheader(sockfd,tmp);
	if (exe)
	  sock_putline(sockfd,"ATTR EXE");
	else
	  sock_putline(sockfd,"ATTR NONE");
	getreply(sockfd);
      }

      /* comment to send? */
      if (*comment || *redirect)
      { *line=0;
	if (*comment) strcat(line,comment);
	if (*redirect) 
	{ if (*line)
	    sprintf(line,"%s\r\n%s",line,redirect);
	  else
	    strcat(line,redirect);
	}
	iso2utf(tmp,line);
	if (spool)
	  fprintf(oshf,"COMMENT\t%s\n",tmp);
	else
	{ sprintf(line,"COMMENT %s",tmp);
	  sock_putline(sockfd,line);
	  getreply(sockfd);
	}
      }

      /* send the file data */
      if (spool)
      { fclose(oshf);
	strcpy(osdfn,oshfn);
	osdfn[strlen(osdfn)-1]='d';
	if (fcopy(file,osdfn,S_IRUSR|S_IWUSR)<0)
	{ unlink(oshfn);
	  unlink(osdfn);
	}
	else
	{ strcpy(tmp,oshfn);
	  oshfn[strlen(oshfn)-1]='h';
	  rename(tmp,oshfn);
	  sprintf(tmp,"'%s' spooled",iso_name);
	  message(prg,'I',tmp);
	}
      } else
        if (send_data(sockfd,size,file,iso_name,do_compress||pgpcrypt,quiet)==0)
          outlog(recipient,host,utf_name,sizes);
    }
  }

  /* close the connection */
  if (!spool)
  { sock_putline(sockfd,"QUIT");
    getreply(sockfd);
    close(sockfd);
  } 
  else
    start_spooldaemon(localhost);
  
  /* delete tmp-files */
  cleanup();
  exit(0);
}


/*
 * whereis - search for program name in PATH
 *
 * INPUT:  name - program name
 *         prg  - empty string
 *
 * OUTPUT: prg  - full pathname to program
 *
 * This function is superfluous and not needed any more.
 */
void whereis(const char *name, char *prg)
{ char *pp1,		/* path pointer 1 */
       *pp2, 		/* path pointer 2 */
       path[OVERSIZE]; 	/* path name */

  /* begin of PATH string */
  pp1=strcpy(path,getenv("PATH"));

  /* search PATH for programm */
  do
  {
    /* cut path-part */
    pp2=strchr(pp1,':');
    if (pp2) *pp2=0;

    /* test if programm is there */
    sprintf(prg,"%s/%s",pp1,name);
    if (access(prg,X_OK)<0) *prg=0;

    /* go to next path-part */
    pp1=pp2+1;

  } while (pp2 && *prg==0);

}


/*
 * cleanexit  - clean termination routine
 *
 * A very simple interrupt handler
 */
void cleanexit()
{
  cleanup();
  exit(0);
}


/*
 * cleanup  - delete tmp files and send LOG command to local server
 */
void cleanup()
{ int 
    sockfd;     	/* socket file descriptor */
  char  
    line[MAXLEN], 	/* one line of text */
    reply[MAXLEN],	/* reply string from server */
    server[FLEN]; 	/* saft server */

  verbose=0;
  *reply=0;
  
  /* ignore all relevant signals */
  signal(SIGTERM,SIG_IGN);
  signal(SIGABRT,SIG_IGN);
  signal(SIGQUIT,SIG_IGN);
  signal(SIGHUP,SIG_IGN);
  signal(SIGINT,SIG_IGN);

#ifndef DEBUG
  unlink(stdintmp);
  unlink(texttmp);
  unlink(gziptmp);
  unlink(tartmp);
  unlink(pgptmp);

  /* inform local saft server about outgoing log file */
  if (access(outlogf,R_OK)==0 && 
      (sockfd=open_connection("127.0.0.1",SAFT))>=0)
  { 
    /* check local saft server */
    sock_getline(sockfd,reply);
    if (verbose && *reply) printf("%s\n",reply);
    if (strbeq(reply,"220 ") && strstr(reply,"SAFT"))
    { 
      /* send LOG command */
      sprintf(line,"LOG %s",pw_name);
      sock_putline(sockfd,line);
      sock_getline(sockfd,reply);
      if (verbose && *reply) printf("%s\n",reply);
      str_trim(reply);

      /* forward address to real saft server? */
      if (strbeq(reply,"510 "))
      { strcpy(server,strrchr(reply,' ')+1);

	/* close current connection */
	sock_putline(sockfd,"QUIT");
	sock_getline(sockfd,reply);
	if (verbose && *reply) printf("%s\n",reply);
	shutdown(sockfd,2);

	/* connect real saft server */
	if ((sockfd=open_connection(server,SAFT))>=0)
	{ sock_getline(sockfd,reply);
	  if (verbose && *reply) printf("%s\n",reply);
	  if (strbeq(reply,"220 ") && strstr(reply,"SAFT"))
	  { 
	    /* send LOG command */
	    sprintf(line,"LOG %s",pw_name);
	    sock_putline(sockfd,line);
	    sock_getline(sockfd,reply);
	    if (verbose && *reply) printf("%s\n",reply);
	    str_trim(reply);

	  }
	}
      }
    }

    /* delete user outgoing log file if local server has accepted it */
    if (strbeq(reply,"200 ")) unlink(outlogf);
    
    /* close the connection */
    sock_putline(sockfd,"QUIT");
    sock_getline(sockfd,reply);
    if (verbose && *reply) printf("%s\n",reply);
    shutdown(sockfd,2);

  }
    
#endif
  
}


/*
 * pgp_encrypt  - encrypt a file with pgp
 *
 * INPUT:  pgpcrypt	- public key or IDEA encryption
 *	   pgprid	- pgp recipient id
 *         file		- input file name
 *
 * OUTPUT: file		- output file name
 */
void pgp_encrypt(int pgpcrypt, const char *pgprid, char *file)
{ char
    cmd[OVERSIZE],	/* the command for system() */
    rmcmd[3*MAXLEN];	/* the rm command */
  struct stat finfo;	/* information about a file */

  /* determine which tmp-files to delete in system-trap */
  if (streq(file,tartmp) || streq(file,texttmp))
    sprintf(rmcmd,"trap \"rm -f %s %s 2>/dev/null\" 1 2 3",file,pgptmp);
  else
    sprintf(rmcmd,"trap \"rm -f %s 2>/dev/null\" 1 2 3",pgptmp);

  /* pgp needs user input? */
  if (pgpcrypt=='c' || streq(pgprid," "))
  {
    if (!quiet) message(prg,'I',"call to pgp...");
    sprintf(cmd,"%s;%s +armor=off -f%c < %s > %s",
	    rmcmd,pgp_bin,pgpcrypt,file,pgptmp);
    if (verbose) printf("call to shell: %s\n",strchr(cmd,';')+1);
    if (system(cmd) || stat(pgptmp,&finfo)<0 || finfo.st_size==0)
    { errno=0;
      message(prg,'F',"call to pgp failed");
    }
    printf("\n");
  }
  else /* pgp needs no user input */
  {
    sprintf(cmd,"%s;%s +armor=off -fe %s < %s > %s 2>/dev/null",
	    rmcmd,pgp_bin,pgprid,file,pgptmp);
    if (verbose) printf("call to shell: %s\n",strchr(cmd,';')+1);
    if (system(cmd) || stat(pgptmp,&finfo)<0 || finfo.st_size==0)
    { errno=0;
      message(prg,'F',"call to pgp failed (wrong pgp user id?)");
    }
  }

  strcpy(file,pgptmp);
}


/*
 * pgp_sign  - create detached pgp signature file and send SIGN header line
 *
 * INPUT:  pgpsign	- pgp user id
 *         infile	- input file name
 *         sockfd	- socket descriptor
 */
void pgp_sign(const char *pgpsign, const char *infile, int sockfd)
{ int
    check;		/* check if sig is ok */
  char
    *cp,		/* simple string pointer */
    tmp[MAXLEN],	/* temporary string */
    sign[MAXLEN], 	/* signature */
    line[MAXLEN], 	/* one line of text */
    cmd[3*MAXLEN];	/* the command for popen() */
  FILE *pipe;		/* input file descriptor */

  check=0;
  *sign=0;

  if (!quiet && !pgppass) message(prg,'I',"call to pgp...");

  sprintf(cmd,"%s %s -fsba %s < %s",pgp_bin,pgpvm,pgpsign,infile);
  if (verbose) printf("call to shell: %s\n",cmd);
  if (!(pipe=popen(cmd,"r"))) message(prg,'F',"call to pgp (signature) failed");

  /* read signature file in NVT format */
  while (fgets(line,MAXLEN-1,pipe))
  { if ((cp=strchr(line,'\n'))) *cp=0;
    if (streq(line,"-----BEGIN PGP MESSAGE-----")) check+=1;
    if (streq(line,"-----END PGP MESSAGE-----")) check+=2;
    strcat(sign,line);
    strcat(sign,"\r\n");
  }
  pclose(pipe);

  if (!pgppass) printf("\n");
  if (check!=3) message(prg,'F',"call to pgp (signature) failed");

  iso2utf(tmp,sign);
  sprintf(sign,"SIGN %s",tmp);
  sock_putline(sockfd,sign);
  getreply(sockfd);
}


/*
 * outspool  - create and open outgoing spool header file
 *
 * INPUT:  user		- own user name
 * 	   outgoing	- outgoing spool directory
 *
 * OUTPUT: oshf		- outgoing spool header file name
 *
 * RETURN: FILE pointer if ok, NULL if failed
 *
 * This function does not use a locking or a atomar test&create function
 * because this would not work with NFS. To avoid race conditions (remember:
 * a spool daemon is normally running, too!) a tmp-header file is created.
 */
FILE *outspool(const char *user, const char *outgoing, char *oshf)
{ struct stat finfo;	/* information about a file */
  struct timeval tv;
#if !defined(SOLARIS2) && !defined(IRIX)
  struct timezone tz;
#endif

  /* get time structure */
#if defined(SOLARIS2) || defined(IRIX)
   #ifdef _SVID_GETTOD
     gettimeofday(&tv);
   #else
     gettimeofday(&tv,NULL);
   #endif
#else
  gettimeofday(&tv,&tz);
#endif

  /* build (tmp) header file name */
  sprintf(oshf,"%s/%s_%d.t",outgoing,user,(int)tv.tv_usec);

  /* does the file already exist? */
  if (stat(oshf,&finfo)==0) return(NULL);

  return(fopen(oshf,"w"));
}


/*
 * check_forward  - test if there is an forward address set
 *
 * INPUT:  sockfd	- socket file descriptor
 *         recipient	- recipient user name
 *	   host		- host to connect
 * 	   redirect	- redirect comment
 *
 * OUTPUT: recipient	- new recipient user name
 *	   host		- new host to connect
 * 	   redirect	- redirect comment
 *
 * RETURN: 1 if a forward is set, 0 if not
 */
int check_forward(int sockfd, char *recipient, char *host, char *redirect)
{ char
    *at,		/* simple string pointer */
    *reply,		/* reply string from server */
    tmp[MAXLEN];	/* temporary string */

  /* get reply from server */
  reply=getreply(sockfd);
  str_trim(reply);

  /* forward address set? */
  if (strbeq(reply,"510 "))
  { at=strchr(reply,'@');

    /* is there a forward address? */
    if (at)
    { if (quiet<2) message(prg,'I',"forward address found");
      if (*redirect)
	sprintf(redirect,"%s\r\nredirected by %s@%s",redirect,recipient,host);
      else
	sprintf(redirect,"redirected by %s@%s",recipient,host);

      /* save new recipient and host name */
      *at=0;
      strcpy(recipient,strrchr(reply,' ')+1);
      strcpy(host,at+1);

      /* close current connection */
      sock_putline(sockfd,"QUIT");
      getreply(sockfd);
      shutdown(sockfd,2);
      return(1);

    }
  }

  if (!strbeq(reply,"200 "))
  { sprintf(tmp,"server error: %s",&reply[4]);
    errno=0;
    message(prg,'F',tmp);
  }

  return(0);
}


/*
 * reply  - dummy function, only needed for linking
 */
void reply(int x) { }


/*
 * start_spooldaemon  - start local spool daemon for outgoing files
 * 
 * INPUT: localhost  - name of the local host
 */
void start_spooldaemon(char *localhost)
{ int
    sockfd;     	/* socket file descriptor */
  char
    *host,	 	/* name of local SAFT server */
    *reply,		/* reply string from remote server */
    tmp[MAXLEN],	/* temporary string */
    line[MAXLEN]; 	/* one line of text */
    
  /* open connection to the own server */
  sockfd=open_connection(localhost,SAFT);
  if (sockfd==-1) sprintf(tmp,"cannot create a network socket "
			      "- cannot start local spool daemon");
  if (sockfd==-2) sprintf(tmp,"cannot open connection to %s "
			      "- cannot start local spool daemon",localhost);
  if (sockfd==-3) sprintf(tmp,"%s is unknown (name server down?) "
			      "- cannot start local spool daemon",localhost);
  if (sockfd<0)
  { errno=0;
    message(prg,'F',tmp);
  }

  /* no remote server or protocol error? */
  sock_getline(sockfd,line);
  if (verbose && *line) printf("%s\n",line);
  if (!strbeq(line,"220 ") || !strstr(line,"SAFT"))
  { errno=0;
    sprintf(tmp,"No SAFT server on port %d at %s "
	        "- cannot start local spool daemon",SAFT,localhost);
    message(prg,'F',tmp);
  }

  sock_putline(sockfd,"START SPOOLDAEMON");
  reply=getreply(sockfd);
  str_trim(reply);

  /* forward to local SAFT-server set? */
  if (strbeq(reply,"510 ") && (host=strrchr(reply,' ')))
  { host++;

    /* close current connection */
    sock_putline(sockfd,"QUIT");
    getreply(sockfd);
    shutdown(sockfd,2);
      
    /* open connection to the own SAFT server */
    sockfd=open_connection(host,SAFT);
    if (sockfd==-1) sprintf(tmp,"cannot create a network socket "
			        "- cannot start local spool daemon");
    if (sockfd==-2) sprintf(tmp,"cannot open connection to %s "
			        "- cannot start local spool daemon",host);
    if (sockfd==-3) sprintf(tmp,"%s is unknown (name server down?) "
			        "- cannot start local spool daemon",host);
    if (sockfd<0)
    { errno=0;
      message(prg,'F',tmp);
    }

    /* no remote server or protocol error? */
    sock_getline(sockfd,line);
    if (verbose && *line) printf("%s\n",line);
    if (!strbeq(line,"220 ") || !strstr(line,"SAFT"))
    { errno=0;
      sprintf(tmp,"No SAFT server on port %d at %s "
	          "- cannot start local spool daemon",SAFT,host);
      message(prg,'F',tmp);
    }

    sendheader(sockfd,"START SPOOLDAEMON");
    sendheader(sockfd,"QUIT");
    
  }
}


/* 
 * outlog  - create temporary user outgoing log file
 * 
 * INPUT: from	- own user name
 *        to	- recipient user name
 * 	  host	- recipient host name
 *        file	- file name
 *        sizes	- original and compressed file size
 */
void outlog(char *to, char *host, char *file, char *sizes)
{ char currentdate[FLEN];	/* current date */
  FILE *outf;			/* output file */
  time_t timetick;		/* unix time (in seconds) */

  if ((outf=fopen(outlogf,"a")))
  { 
    /* get current date */
    timetick=time(0);
    strftime(currentdate,20,"%Y-%m-%d %H:%M:%S",localtime(&timetick));
  
    /* write to log file */
    fprintf(outf,"FROM\t%s\nTO\t%s@%s\nDATE\t%s\nFILE\t%s\nSIZES\t%s\n\n",
	    pw_name,to,host,currentdate,file,sizes);
  
    fclose(outf);

  }
  return;
    
}


/*
 * usage - print short help usage text
 */
int usage()
{ fprintf(stderr,"usage: %s [-stdPuvoqQ] file [...] user[@host]\n",prg);
  fprintf(stderr,"   or: %s -a archive [-uvoqQ] file-or-directory [...] user[@host]\n",prg);
  fprintf(stderr,"options: -s   send file(s) in source mode\n");
  fprintf(stderr,"         -t   send file(s) in text mode\n");
  fprintf(stderr,"         -d   delete previous sent file(s)\n");
  fprintf(stderr,"         -P   read data stream from stdin (normaly this is a pipe)\n");
  fprintf(stderr,"         -u   send file(s) uncompressed\n");
  fprintf(stderr,"         -v   verbose mode\n");
  fprintf(stderr,"         -o   overwrite already sent file(s) with same name\n");
  fprintf(stderr,"         -q   quiet mode 1: no transfer messages\n");
  fprintf(stderr,"         -Q   quiet mode 2: no transfer, information and warning messages\n");
  fprintf(stderr,"         -a   send file(s) as one archive\n");
  fprintf(stderr,"         Default mode: send file(s) in binary mode and compressed.\n");
  fprintf(stderr,"         For a full description of all options, see 'man %s'.\n",prg);
  fprintf(stderr,"example: %s rabbit.gif beate@juhu.lake.de\n",prg);
  return(2);
}
