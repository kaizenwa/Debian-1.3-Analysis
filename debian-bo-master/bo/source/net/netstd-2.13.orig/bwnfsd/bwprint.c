/*
VERSION  2.3a
MODIFIED 02/10/92

	BWNFSD is an RPC daemon used in conjunction with BWNFS ( a client NFS
for DOS based PCs. BWNFSD provides Authentication, Print Spooling, 
DOS 3.1 Locking, DOS 3.1 Sharing, GID name mapping, UID name mapping services
for BWNFS and associated applications on the PC. BWNFSD is being used also
by Macintosh NFS clients.

	The BWNFSD code is originally copyright Beame & Whiteside Software Ltd.
and now is released to the Public Domain. The intent is that all server vendors
included a version of BWNFSD with their operating system. BWNFSD can run
simultantiously as PCNFSD, but provides many more features.

	Please send modifications to:

		Beame & Whiteside Software Ltd.
		P.O. Box 8130 
		Dundas, Ontario
		Canada L9H 5E7
		+1 (416) 765-0822

	Please modify by including "ifdefs" for your particular operating 
system, with appropriate modifications to the "makefile".

BWPRINT.C provides the print spool services for BWNFS. This is where most
modification needs to be done as it is coded mainly for BSD systems, not
System V.

*/
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef SCO
#include <signal.h>
#endif
#ifdef SYSV32
#ifndef SCO
#define sgi
#endif
#include <string.h>
#else
#include <strings.h>
#endif

int     debugmode;


void print_it(file,printer,jobname)
char	*file,*printer,*jobname;
{
	struct printies
	{
		char *name;
		void (*routine)();
	};
	
	void	default_print();

	static	struct printies	print_list[] = {
		"lp" , default_print,
/*
		"your printer" , your_printer_routine,
*/
		};

#define PRINT_COUNT sizeof(print_list)/sizeof(struct printies)

	int	i;

	struct stat buf;

        if (debugmode)
	{
          (void) fprintf( stdout, "bwnfsd: [print_it] called\n" );
          (void) fprintf( stdout, "bwnfsd: Filename = %s\nbwnfsd: Jobname = %s\nbwnfsd: Printer = %s\n",
                          file, jobname, printer );
	}
	if ( stat(file,&buf) == 0 )
	{
		setuid(buf.st_uid);
		setgid(buf.st_gid);
	}


	for ( i = 0 ; i < PRINT_COUNT; i++)
	  if ( strcmp(printer,print_list[i].name) == 0)
	  {
            if (debugmode)
              (void) fprintf( stdout, "bwnfsd: [print_it] using %s\n", printer );
	    (*print_list[i].routine)(file,printer,jobname);
  	    return;
	  }
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [print_it] using default_print\n" );
	default_print(file,printer,jobname);
}


void default_print(file,printer,jobname)
char	*file,*printer,*jobname;
{
	char    *argys[20];
	int	ac;
#ifdef sgi
	char zots[255], zoots[255];
#endif
#ifdef SCO
	int	pid;
#endif

	ac=0;
#ifndef sgi
#ifdef SCO
       	argys[ac++]="/usr/bin/lp";
	argys[ac++]="lp";
	argys[ac++]="-d";
	argys[ac++]=printer;
	argys[ac++]="-c";
	argys[ac++]="-s";
#else
#ifdef LINUX
       	argys[ac++]="/usr/bin/lp";
	argys[ac++]="lp";
	argys[ac++]="-d";
	argys[ac++]=printer;
	argys[ac++]="-c";
	argys[ac++]="-s";
#else
       	argys[ac++]="/usr/ucb/lpr";
	argys[ac++]="lpr";
	argys[ac++]="-P";
	argys[ac++]=printer;
	argys[ac++]="-r";
#endif	/* LINUX */
#endif
#else
#ifdef AIX
       	argys[ac++]="/bin/enq";
	argys[ac++]="enq";
	strcpy(zots,"-P");
#else
       	argys[ac++]="/usr/bin/lp";
	argys[ac++]="lp";
	strcpy(zots,"-d");
#endif
	strcat(zots,printer);
	argys[ac++]=zots;
	argys[ac++]="-c";
#ifndef AIX
	argys[ac++]="-s";
#endif
#endif
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [default_print] called\n" );

	if ( jobname != NULL)
	{
#ifndef sgi
#ifdef SCO
		argys[ac++]="-t";
#else
		argys[ac++]="-T";
#endif
		argys[ac++]=jobname;
		argys[ac++]=file;
		argys[ac++]=NULL;
#else
		strcpy(zoots,"-T");
		strcat(zoots,jobname);
		argys[ac++]=zoots;
		argys[ac++]=file;
		argys[ac++]=NULL;
#endif
	}
	else
	{
		argys[ac++]=file;
		argys[ac++]=NULL;
	}
#ifndef sgi
#ifdef SCO
	chmod( file, 0777 );
	signal( SIGCLD, SIG_IGN );
	if ((pid = fork()) == 0)
	{
	  if (debugmode)
	    (void) fprintf( stdout, "bwnfsd: [default_print] execv %s, %s\n",
	                    argys[0], argys[1] );
          execv(argys[0],&argys[1]);
          exit(1);
	}
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [default_print] calling waitpid %u\n", uid );
	waitpid(pid,NULL,0);
#else
        if (debugmode)
	  (void) fprintf( stdout, "bwnfsd: [default_print] execv %s, %s\n",
	                  argys[0], argys[1] );
       	execv(argys[0],&argys[1]);
#endif
/***
   * If we get here, it means that the print failed. We unlink the
   * printfile to prevent lots of queued files. This may not be appropriate
   * for your application or for debugging purposes.
   ***/
   if (debugmode)
     (void) fprintf( stdout, "bwnfsd: [default_print] print failed, unlinking print-file\n" );
#else
	chmod(file,0777);
	if (fork() == 0)
	{
          if (debugmode)
            (void) fprintf( stdout, "bwnfsd: [default_print] execv %s, %s\n",
                            argys[0], argys[1] );
          execv(argys[0],&argys[1]);
        }
	wait(NULL);

/* --------------------------------------------------------
   If you are encountering defunct (zombie) processes, then
   this might be a good place to start looking
   -------------------------------------------------------- */
#endif
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [default_print] unlinking %s\n", file );
	(void) unlink(file);

}


