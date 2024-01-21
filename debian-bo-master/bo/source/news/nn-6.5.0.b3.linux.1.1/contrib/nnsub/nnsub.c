/*
    nnsub [-v] [newsgroups....]

	exit status ERR_EXIT indicates a failure

	option -v for verbosity
	-v0	don't show output, no questions.
		exit status shows number of newsgroups requested but
		not subscribed (bogus).
		Default if running without terminal control.
	-v1	if interactive the program can ask questions and
		a report of subscriptions is written.
		Default if running with a terminal.

	groups to subscribe to can be supplied as program arguments
	or on input. Partial groupnames can be given by starting or
	ending the name with a dot, and the program will try to find
	the groupname (as in the nn init file). If running without
	terminal control or with verbosity 0 all the matches found
	on such a partial name will be subscribed to.
        With a terminal and verbosity unequal 0 all names are handled
	as partial names and the options are to subscribe to all, or
	to none or list the found matches and ask confirmation to
	subscribe. Farther a questionmark (?) as groupname will result
	in menu's with NLINES groupnames and choices can be made by entering
        a string of the indicating letters (or only a NEWLINE if nothing
        is selected). The capital R will resume the option of entering
        (partial) newsgroup names, a Q will quit that phase entirily and
        complete the program by updating the newsrc file.
        
	For testing purposes there are some higher verbosity levels
        (if symbol DEBUG is defined).
	-v2	report some file read or file search actions.
	-v3	also the progress while reading or searching is made visible
        -v4	yet more output about internal databases (debugging)
	-v5	as -v4, but don't delete temporary files
	These higher verbosity-levels also causes the .newsrc file to be
	update in directory ~/tmp en not the home directory itself.

	Define symbols:
	ACTIVE		pathname of file containing active newsgroups
	DEFRC		pathname of default .newsrc (see nn INSTALL)
	ERR_EXIT	value for error exit
	NCOLS		number of colums on a terminal screen (80)
	NEWSGROUPS	pathname of file with short descriptions
	NEWSRC		file .newsrc
	NLINES		number of lines in a screen menu (22)
	NNTP		hostname of nntp server
	OLDRC		filename to save old .newsrc
	PORT		port number for nntp, see file /etc/services
	SYSV		compile and run on System V UNIX
	TEMPRC		temporary template to write new .newsrc
	TIMEOUT		timeout value while waiting for server reply
	iShwfac		interval for writing a dot if debugging
	iSlen		length of string buffer
	iMeuGrpart	max space to be used to write groupname in menu

	For NNTP-servers where the files "active" and "newsgroups"
	are kept local, mostly in "/usr/lib/news" the symbols ACTIVE
	and NEWSGROUPS must be set to the pathnames of these files
	and the symbol NNTP must *NOT* be defined, PORT is not used.
	For NNTP-clients the symbol NNTP must be set to the name of
	the NNTP-server (as in the file nntp-server in nn's "cofig.h")
	and the symbol PORT must be correct, ACTIVE and NEWSGROUPS are
	not used.

	Strategy:
	1.Build an avl-tree (balanced binary tree) from all the
	  newsgroups listed in the active file (nntp-clients can obtain
	  these data via nntp with the command 'list active').
	2.If Verbosity > 0 store with each newsgroup the byte address
	  of the corresponding entry in the newsgroups file (nntp-clients
	  must first build this file with the nntp command 'list newsgroups').
	3.Then build a second avl-tree with pointers to the requested
	  newsgroups.
	4.Write a new .newsrc combining the data with the second avl-tree.


	Copyright: Rudi van Houten <Rudi@cc.ruu.nl>
		   Acadmisch Computer Centrum Utrecht
		   Budapestlaan 6  3584 CD  Utrecht
*/

				/* filename definitions */

#ifndef ACTIVE
#define ACTIVE "/usr/lib/news/active"	/* file "active" (non NNTP) */
#endif
#ifndef NEWSGROUPS
#define NEWSGROUPS "/usr/lib/news/newsgroups" /* idem "newsgroups" */
#endif

#ifndef DEFRC
#define DEFRC  "/usr/spool/news/.nn/db/.defaultnewsrc"
#endif

#ifndef NEWSRC
#define NEWSRC ".newsrc"
#endif
#ifndef OLDRC
#define OLDRC  ".newsrc.old"
#endif
#ifndef TEMPRC
#define TEMPRC "tmprcXXXXXX"
#endif
#ifndef PORT
#define PORT 119			/* port for NNTP "/etc/services" */
#endif

				/* some constants */
#ifndef iSlen
#define iSlen 256
#endif
#ifndef NLINES
#define NLINES 22
#endif
#ifndef NCOLS
#define NCOLS 80
#endif
#ifndef iMenuGrpart
#define iMenuGrpart 20		/* max. space for newsgroupname in menu */
#endif

#ifndef TIMEOUT
#define TIMEOUT 15
#endif
#define ERR_EXIT 9999

				/* and more constants and includes */

#define iVreport 1
#ifdef DEBUG
#define iVactions 2
#define iVprogress 3
#define iVdebug 4
#define iVleavetmp 5
#define iShwfac 100
#endif

#define iVerbdef iVreport

#ifdef NNTP
#define CHARTERS "/tmp/ngrpXXXXXX"
#else
#define CHARTERS NEWSGROUPS
#endif

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "avl.h"
#include "version.h"
#include "patchlevel.h"
#ifdef NNTP
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#endif
#ifdef DEBUG
#include <sys/file.h>
#endif
#ifdef SYSV
#define index strchr
#define bcopy(a,b,c) memcpy(b,a,c)
#include <unistd.h>
#endif

typedef struct
    {
        char *sName;
        long iTitle_pos;
    } s_Group;
typedef s_Group * ps_Group;

typedef struct s_grlink
    {
        s_Group * pGroup;
        struct s_grlink * pNext;
    } s_Grlink;

static P_AVL_NODE paGr_tree= NULL, paGr_requests= NULL;
static s_Grlink *pGr_list= NULL, *pGr_endlist= NULL;
static short iVerbosity, bIntractive;
static FILE *fGroups, *fCharters, *fNewgroups;
static char *sNewgrplfn, *sCharterlfn;
#ifdef NNTP
static int sock, sock_wr;
struct sockaddr_in sNntpServer;
struct hostent *psNntpHost, *gethostbyname();
#endif

static char * sRequest;
static short bStart_is_free, bEnd_is_free; /* needed for fuzzy compare */

extern char * Findsub();
extern char * mktemp();
extern char * getenv();
extern char * malloc();

int Grpcmp(g1,g2)
s_Group *g1, *g2;
{
    return (strcmp(g1->sName,g2->sName));
} /* Grpcmp */

Printcharter(pGr,iOffset)
s_Group *pGr;
int iOffset;
{
    char sLine[iSlen], sNewsgroup[iSlen], sDescription[iSlen];
    int iNglen, iDesclen;      /* length of groupname and description */
    int iNgwidth, iDescwidth;  /* space to use for printing */
    int iAvailable;            /* available space on screnline */

    iAvailable= NCOLS - iOffset;
    if (pGr->iTitle_pos < 0) { puts(pGr->sName); return; }
    if (fseek(fCharters,pGr->iTitle_pos,0) < 0)
        { perror(sCharterlfn); exit(ERR_EXIT); }
    fgets(sLine,iSlen,fCharters);
    sscanf(sLine,"%[^ \t]%*[ \t]%[^\n]",sNewsgroup,sDescription);
    iNglen= strlen(sNewsgroup); iDesclen= strlen(sDescription);
    if (iNglen + 1 + iDesclen > iAvailable)
    { /* have to truncate dscription */
	iNgwidth= iNglen + 1; /* one blank after group name */
	iDescwidth= iAvailable - iNgwidth;   /* rest of line */
    }
    else
    if (iNglen < iMenuGrpart)
    {
	if (iDesclen + iMenuGrpart <= iAvailable)
	{ /* use "iMenuGrpart" columns for groupname */
	    iNgwidth= iMenuGrpart;
	    iDescwidth= iDesclen;
	}
	else
	{ /* use less space for groupname
	     and right-justify description */
	    iNgwidth= iAvailable - iDesclen;
	    iDescwidth= iDesclen;
	}
    }
    else /* if (iNglen < iMenuGrpart) */
    {
	if (iNglen + 5 + iDesclen <= iAvailable)
	{ /* leave 5 spaces between name and description */
	    iNgwidth= iNglen + 5;
	    iDescwidth= iDesclen;
	}
	else
	{ /* leave less blanks after groupname and right-justify description */
	    iNgwidth= iAvailable - iDesclen;
	    iDescwidth= iDesclen;
	}
    }
    printf("%-*s%.*s\n",iNgwidth,sNewsgroup,iDescwidth,sDescription);

} /* Printcharter */

int Add_list_element(pGr)
s_Group *pGr;
{
    register s_Grlink *pLocal;
    pLocal= (s_Grlink*)malloc(sizeof(s_Grlink));
    pLocal->pGroup= pGr;
    pLocal->pNext= NULL;
    if (pGr_list == NULL)
        pGr_list= pGr_endlist= pLocal;
    else
        pGr_endlist= pGr_endlist->pNext= pLocal;
#   ifdef DEBUG
    if (iVerbosity > iVdebug) putchar('.');
#   endif
    return 0;
} /* Add_list_element */

int Fuzzycheck(pGr)
s_Group *pGr;
/*
    if a fuzzy compare of the string sRequest matches the
    name of the supplied group that group is added in the
    one-way-linked list for later consideration
*/
{
    register short bFound;
    register char *pcC;
    if (bStart_is_free)
        { if ((pcC= Findsub(pGr->sName,sRequest)) == NULL) return(0); }
    else pcC= pGr->sName;
    if (!bEnd_is_free) bFound= strcmp(sRequest,pcC) == 0;
    else bFound= strncmp(sRequest,pcC,strlen(sRequest)) == 0;
    if (bFound) Add_list_element(pGr);
    return(bFound);
} /* int Fuzzycheck(pGr) */

int Checkrequest(sName)
char * sName;
{
    register P_AVL_NODE pNode;
    register s_Grlink *pLink;
    s_Group s_Glocal, *pGr_dum;
    char cC;
    int iCount;
    for (iCount=0;iCount<strlen(sName);iCount++)
        if (isupper(sName[iCount])) sName[iCount]= tolower(sName[iCount]);
    bStart_is_free= sName[0] == '.';
    bEnd_is_free= (cC= sName[strlen(sName)-1]) == '.';
    s_Glocal.sName= sName;
    if (!bStart_is_free && !bEnd_is_free)
    {
        if (find_avlnode(&s_Glocal,paGr_requests,Grpcmp)) return(0);
        if (pNode= find_avlnode(&s_Glocal,paGr_tree,Grpcmp))
        {
            insert_avl(pNode->content,&paGr_requests,Grpcmp,&pGr_dum);
#           ifdef DEBUG
            if (iVerbosity > iVprogress)
                printf("..Added in request tree: %s\n",sName);
#           endif
            return(1);
        }
        else /* if (pNode= find_avlnode(&s_Glocal,paGr_tree,Grpcmp)) */
        if (iVerbosity && bIntractive)
            bStart_is_free= bEnd_is_free= 1;
        else /* if (iVerbosity && bIntractive) */
        {
            if (iVerbosity) printf("BOGUS newsgroup \"%s\"\n",sName);
            return(0);
        } /* if (iVerbosity && bIntractive) */
        /* if (pNode= find_avlnode(&s_Glocal,paGr_tree,Grpcmp)) */
    } /* if (!bStart_is_free && !bEnd_is_free) */
    sRequest= sName;
    unravel_avl(&paGr_tree,Fuzzycheck,0);
    if (pGr_list == NULL)
    {
        if (iVerbosity) printf("\"%s\": no matching newsgroups found\n",sName);
        return(0);
    }
    iCount= 0; /* and now count the found groups */
    for (pGr_endlist= pGr_list;
         pGr_endlist != NULL;
         pGr_endlist= pGr_endlist->pNext) iCount++;
    if (iVerbosity)
    {
#       ifdef DEBUG
	if (iVerbosity >= iVdebug) putchar('\n');
#       endif
        if (bIntractive)
            if (iCount == 1)
            {
		putchar('\n');
                Printcharter(pGr_list->pGroup,1);
                fputs("  OK? (y) ",stdout);
                cC= getchar();
		if (cC != '\n') while (getchar() != '\n'); else cC= 'y';
                if (cC != 'y' && cC != 'Y') { free(pGr_list); pGr_list= NULL; }
            }
            else /* if iCount == 1) */
            {
                printf("Partial name \"%s\": %d matching newsgroups found\n"
                      ,sName,iCount);
                fputs("Subscribe to: a)ll, c)onfirmation, n)one? ",stdout);
                cC= getchar(); if (cC != '\n') while (getchar() != '\n');
                switch (cC)
                {
                default:
                    break; /* default answer is 'a' */
                case 'n':
                case 'N':
                    pGr_endlist= pGr_list; pGr_list= NULL;
                    while (pGr_endlist)
                    {
                        pLink= pGr_endlist; pGr_endlist= pLink->pNext;
                        free(pLink);
                    }
                    return(0);
                case 'c':
                case 'C':
                    return(Menusubscribe());
                } /* switch(cC) */
            } /* if (iCount == 1) */
        /* if (bIntractive) */
    } /* if (iVerbosity) */
    if (pGr_list == NULL) return(0);
    iCount= 0; pGr_endlist= pGr_list; pGr_list= NULL;
    while (pGr_endlist != NULL)
    {
        if (find_avlnode(pGr_endlist->pGroup,paGr_requests,Grpcmp) == NULL)
        {
            insert_avl(pGr_endlist->pGroup,&paGr_requests,Grpcmp,&pGr_dum);
#           ifdef DEBUG
            if (iVerbosity > iVprogress)
                printf("..Added in request tree: %s\n"
                      ,pGr_endlist->pGroup->sName);
#           endif
            iCount++;
        }
        pLink= pGr_endlist; pGr_endlist= pLink->pNext;
        free(pLink);
    } /* while (pGr_endlist != NULL) */
    return(iCount);
} /* int Checkrequest(sName) */

int Menusubscribe()
/*
    returnvalue: number of subscribed groups
*/
{
    ps_Group sgMline[NLINES], pGr_dum;
    char sLine[iSlen], cTop;
    register s_Grlink * pLink;
    register int i;
    short bStop;
    int iReturnvalue;

    if (pGr_list == NULL)    /* first unravel the tree */
#   ifdef DEBUG
    {
        if (iVerbosity >= iVdebug) puts("Unravel tree to one-way linked list");
#       endif
        unravel_avl(&paGr_tree,Add_list_element,0);
#       ifdef DEBUG
        if (iVerbosity >= iVdebug) puts("\n=Done ===========");
    }
#   endif
    pGr_endlist= pGr_list; pGr_list= NULL;
    i= iReturnvalue= 0; bStop= 0;
    while (pGr_endlist != NULL && bStop == 0)
    {
	if (i == 0) puts("\n\n\n");
        pLink= pGr_endlist; printf("%c: ",i+'a');
	Printcharter(sgMline[i]= pLink->pGroup,4);
        pGr_endlist= pLink->pNext; free(pLink);
        if (i++, pGr_endlist == NULL || i == NLINES)
        {
            cTop= (char)((int)'a' + i - 1);
            printf("\n>> Select with [a..%c], or Quit:",cTop);
            if (gets(sLine) == NULL) bStop= 1;
            else
            for (i= 0; sLine[i] != '\0'; i++)
                if (index("QREBSX",sLine[i])) bStop= 1;
                else
                if (sLine[i] >= 'a' && sLine[i] <= cTop)
		{
                    insert_avl(sgMline[(int)sLine[i] - (int)'a']
                              ,&paGr_requests,Grpcmp,&pGr_dum);
                    if (pGr_dum == sgMline[(int)sLine[i] - (int)'a'])
                    {
                        iReturnvalue++;
#                       ifdef DEBUG
                        if (iVerbosity >= iVdebug)
		        {
			    puts("Added to request tree:");
                            Printcharter(pGr_dum,1);
                        }
#                       endif
                    }
		}
                /* ignore garbage */
            /* for (i= 0; sLine[i] != '\0'; i++) */
            i= 0;
         } /* if (pGr_endlist == NULL || ++i > NLINES) */
    } /* while (pGr_endlist != NULL && bStop == 0) */
#   ifdef DEBUG
    if (iVerbosity >= iVdebug && pGr_endlist != NULL)
        puts("Clean up the one-way linked list");
#   endif
    while (pGr_endlist != NULL)
    {
        pLink= pGr_endlist; pGr_endlist= pLink->pNext;
        free(pLink);
#       ifdef DEBUG
        if (iVerbosity >= iVdebug)
	{
	    putchar('.');
            if (pGr_endlist == NULL) puts("\n>> Cleaning done\n");
	}
#       endif
    } /* while (pGr_endlist != NULL) */
    return(iReturnvalue);
} /* int Menusubscribe() */

int Addsubscription(pGr)
s_Group *pGr;
{
    if (islower(pGr->sName[0]))
    {
        fprintf(fNewgroups,"%s:\n",pGr->sName);
        if (iVerbosity) { puts("New subscription:"); Printcharter(pGr,1); }
    }
    return(0);
} /* Addsubscription */

#ifdef NNTP
static FILE *fNntpOut;

static Nntpcommand(sCommand)
char *sCommand;
/*
     send a command to the nntp server ad check the reply
     in case of error give an exit, timeout exits on alarm.
*/
{
    char sLine[iSlen];
    int iReply;
#   ifdef DEBUG
    if (iVerbosity >= iVactions) printf("NNTP: %s\n",sCommand);
#   endif
    fprintf(fNntpOut,"%s\r\n",sCommand);
    if (fflush(fNntpOut) == EOF)
	{ perror(sCommand); exit(ERR_EXIT); }
    alarm(TIMEOUT);
    if (fgets(sLine,iSlen,fGroups) == NULL)
	{ fprintf(stderr,"NULL response on %s\n",sCommand); exit(ERR_EXIT); }
    alarm(0);
    if (sscanf(sLine,"%d",&iReply) != 1)
    {
	fprintf(stderr,"Illegal response on %s:\n%s",sCommand,sLine);
        exit(ERR_EXIT);
    }
    if (iReply < 200 || iReply >= 400)
    {
	fprintf(stderr,"Error response on %s\n%s",sCommand,sLine);
	exit(ERR_EXIT);
    }
#   ifdef DEBUG
    if (iVerbosity >= iVactions) fputs(sLine,stdout);
#   endif
} /* Nntpcommand */
#endif


main(argc,argv,envp)
int argc;
char *argv[], *envp[];
{
char sLine[iSlen];
register char *pcC;
register s_Group *pG1;
s_Group *pG2;
register P_AVL_NODE p_AVL;
int i, j, k, iGr, iGrfault;

if (bIntractive= isatty(0)) iVerbosity= iVerbdef; else iVerbosity= 0;

iGr= 0; /* counter for groupname arguments */
if (argc > 1)
for (i=1; i<argc; i++)
    if (argv[i][0] == '-')
        switch(argv[i][1])
        {
        default:
            fprintf(stderr,"Unknown option \"%s\" ignored\n",argv[i]);
            break;
        case 'V':
        case 'v':
            if (argv[i][2] == '\0') iVerbosity= iVerbdef;
            else iVerbosity= atoi(&argv[i][2]);
            break;
        } /* switch(argv[i][1]) */
    else /* if (argv[i][0] == '-') */
        iGr++; /* increment supplied groupname counter */
/* for (i=1; i<argc; i++) */

if (iVerbosity)
    printf("\nNetnews subscribe\nRelease %d.%d PL%d (Rudi van Houten 1991)\n\n"
	  ,VERSION,SUBNR,PATCHLEVEL);

#ifdef NNTP
	/* create socket */
if ((sock= socket(AF_INET,SOCK_STREAM,0)) < 0)
    { perror("-- opening stream socket"); exit(ERR_EXIT); }

	/* fill socket structure .....*/
sNntpServer.sin_family= AF_INET;
if ((psNntpHost= gethostbyname(NNTP)) == 0)
{
#define cMessage "While trying to connect to "
    char *sMessage;
    sMessage= malloc(strlen(NNTP)+strlen(cMessage));
    strcpy(sMessage,cMessage); strcat(sMessage,NNTP);
    perror(sMessage);
    exit(ERR_EXIT);
}
bcopy(psNntpHost->h_addr,&sNntpServer.sin_addr,psNntpHost->h_length);
sNntpServer.sin_port= htons(PORT);

	/*........and connect */
if (iVerbosity) printf("Connecting to nntp server %s\n",NNTP);
if (connect(sock,&sNntpServer,sizeof(sNntpServer)) < 0)
    { perror("connecting stream socket"); exit(ERR_EXIT); }
if ((fGroups= fdopen(sock,"r")) == NULL)
    { perror("assign socket to FILE fGroups"); exit(ERR_EXIT); }
alarm(TIMEOUT);
if (fgets(sLine,iSlen,fGroups) == NULL)
    { fputs("No reply from server\n",stderr); exit(ERR_EXIT); }
alarm(0);
if (strncmp(sLine,"200 ",4)) { fputs(sLine,stderr); exit(ERR_EXIT); }
#ifdef DEBUG
if (iVerbosity >= iVactions) puts(sLine);
#endif
sock_wr= dup(sock);
if ((fNntpOut= fdopen(sock_wr,"w")) == NULL)
    { perror("assign outputfile to socket"); exit(ERR_EXIT); }
Nntpcommand("LIST ACTIVE");
#else
if ((fGroups= fopen(ACTIVE,"r")) == NULL) { perror(ACTIVE); exit(ERR_EXIT); }
#ifdef DEBUG
if (iVerbosity >= iVactions)
    printf("Reading active newsgroups from \"%s\"\n",ACTIVE);
#endif
#endif

i= 0; pG1= NULL;
#ifdef NNTP
#ifdef DEBUG
if (iVerbosity >= iVdebug) puts("Get active groups from server");
#endif
while (alarm(TIMEOUT),fgets(sLine,iSlen,fGroups) != NULL)
#else
while (fgets(sLine,iSlen,fGroups) != NULL)
#endif
{
#   ifdef NNTP
    alarm(0);
#   endif
    k= strlen(sLine);
    for (j=0; j<k; j++)
        if (index(" \t\r\n",sLine[j])) { sLine[j]= '\0'; break; }
        else if (isupper(sLine[j])) sLine[j]= tolower(sLine[j]);
    if (strcmp(sLine,".") == 0) break;
    if (pG1 == NULL) pG1= (s_Group*)malloc(sizeof(s_Group));
    pG1->sName= strsave(sLine); pG1->iTitle_pos= -1;
    insert_avl(pG1,&paGr_tree,Grpcmp,&pG2);
    if (pG1 == pG2)
    {
        i++;
#       ifdef DEBUG
        if (iVerbosity >= iVprogress)
            { if (i % iShwfac == 0) putchar('.'); }
#       endif
        pG1= NULL;
    }
} /* while (...fgets(sLine,iSlen,fGroups) != NULL) */
if (iVerbosity) printf("\n %d active groups found\n",i);
#ifndef NNTP
fclose(fGroups);
sCharterlfn= CHARTERS;
#else
sCharterlfn= strsave(CHARTERS);
sCharterlfn= mktemp(sCharterlfn);
if ((fCharters= fopen(sCharterlfn,"w")) == NULL)
    { perror(sCharterlfn); exit(ERR_EXIT); }
Nntpcommand("LIST NEWSGROUPS");
#ifdef DEBUG
if (iVerbosity >= iVactions)
{
    puts(sLine);
    printf("Copying nntp <LIST NEWSGROUPS> to \"%s\"\n",sCharterlfn);
}
#endif
for (j=0;;j++)
{
#   ifdef NNTP
    alarm(TIMEOUT);
#   endif
    if (fgets(sLine,iSlen,fGroups) == NULL) break;
#   ifdef NNTP
    alarm(0);
#   endif
    i= strlen(sLine) - 1;
    while (index(" \t\r\n",sLine[i])) sLine[i--]= '\0';
    if (strcmp(sLine,".") == 0) break;
    fputs(sLine,fCharters); putc('\n',fCharters);
#   ifdef DEBUG
    if (iVerbosity >= iVprogress) if (j % iShwfac == 0) putchar('.');
#   endif
}
#ifdef DEBUG
if (iVerbosity >= iVprogress) putchar('\n');
#endif
fclose(fCharters);
Nntpcommand("QUIT");
if (iVerbosity) printf("Connection with %s closed\n",NNTP);
fclose(fGroups); fclose(fNntpOut);
#endif

/* now read newsgroups with Charter/Title per group */

if ((fCharters= fopen(sCharterlfn,"r")) == NULL)
{
    if (iVerbosity)
        printf("File %s not found, no descriptions can be given\n"
              ,sCharterlfn);
}
else /* if ((fCharters= fopen(sCharterlfn,"r")) == NULL) */
{
#   ifdef DEBUG
    if (iVerbosity >= iVactions)
        printf("Reading short descriptions from \"%s\"\n",sCharterlfn);
#   endif
    if (pG1 == NULL) pG1= (s_Group*)malloc(sizeof(s_Group));
    pG1->iTitle_pos= 0; i= 0;
    while (fgets(sLine,iSlen,fCharters) != NULL)
    {
        pG1->sName= pcC= &sLine[0];
	while (isgraph(*pcC)) 
	    { if (isupper(*pcC)) *pcC= tolower(*pcC); pcC++; };
        *pcC++= '\0';
	if (sLine[0] != '\0')
            if ((p_AVL= find_avlnode(pG1,paGr_tree,Grpcmp)) != NULL)
                ((s_Group*)(p_AVL->content))->iTitle_pos= pG1->iTitle_pos;
#       ifdef DEBUG
            else
            if (iVerbosity > iVprogress)
                printf("\nNot \"active\" group: %s\n\t%s",sLine,pcC);
#       endif
        pG1->iTitle_pos= ftell(fCharters);
#       ifdef DEBUG
        if (iVerbosity >= iVprogress)
            if (++i % iShwfac == 0) putchar('.');
#       endif
    } /* while (fgets(sLine,iSlen,fCharters) != NULL) */
    /* don't close file, read random when titles must be printed */
#   ifdef DEBUG
    if (iVerbosity >= iVprogress) putchar('\n');
#   endif
} /* if ((fCharter= fopen(sCharterlfn,"r")) == NULL) */

if (iGr) /* requests are supplied as program arguments */
{
#   ifdef DEBUG
    if (iVerbosity >= iVdebug) puts("Process arguments");
#   endif
    iGrfault= iGr;
    for (i=1;i<argc;i++)
    {
        if (argv[i][0] != '-')
            if (strcmp(argv[i],"?") == 0)
            {
                if (bIntractive && iVerbosity)
                    if (Menusubscribe()) iGrfault--;
            }
            else
            if (strcmp(argv[i],"."))
                if (Checkrequest(argv[i])) iGrfault--;
    }
}

if (iGr == 0) /* no groupnames supplied as arguments, read from stdin */
{
    if (bIntractive && iVerbosity) puts("\nEnter newsgroups to subscribe to:");
    for (;;)
    {
        if (bIntractive && iVerbosity) fputs(" ?  ",stdout);
        if (gets(sLine) == NULL) break;
	if (sLine[0] == '\0') break;
	if (strcmp(sLine,"Q") == 0) break;
        if (strcmp(sLine,".") == 0) break;
        if (strcmp(sLine,"?") == 0)
        {
            if (bIntractive && iVerbosity) if (Menusubscribe()) break;
        }
        else
        if (Checkrequest(sLine) == 0) iGr++;
    }
}

if (paGr_requests == NULL)
{
    if (iVerbosity) puts("No legal subscribe requests");
#   ifdef NNTP
#   ifdef DEBUG
    if (iVerbosity == iVdebug) printf("unlink temporary %s\n",sCharterlfn);
    if (iVerbosity < iVleavetmp)
#   endif
    unlink(sCharterlfn);
#   ifdef DEBUG
    else printf("Temporary newsgroup descriptions on %s\n",sCharterlfn);
#   endif
#   endif
    exit(0);
}

/*
    And now go looking into .newsrc
*/

if (chdir(getenv("HOME"))) { perror("dir to $HOME"); exit(ERR_EXIT); }
#ifdef DEBUG
if (iVerbosity > iVreport)
{
    if (access("tmp",F_OK))
	if (mkdir("tmp",0750)) { perror("mkdir tmp"); exit(ERR_EXIT); }
    if (chdir("tmp")) { perror("dir to ~/tmp"); exit(ERR_EXIT); }
}
if (iVerbosity >= iVdebug) printf(" cwd = %s\n",getcwd(sLine,iSlen));
#endif
sNewgrplfn= strsave(TEMPRC);
sNewgrplfn= mktemp(sNewgrplfn);

if ((fNewgroups= fopen(sNewgrplfn,"w")) == NULL)
    { perror("NEW newsrc"); exit(ERR_EXIT); }
#ifdef DEBUG
if (iVerbosity >= iVactions) printf("Opened file %s for writing\n"
                                   ,sNewgrplfn);
#endif

if ((fGroups= fopen(NEWSRC,"r")) == NULL)
{
    FILE *fDefnewsrc;
    if ((fDefnewsrc= fopen(DEFRC,"r")) != NULL)
    { /* add groups on .defaultnewsrc */
        s_Group sGr_local, *pGr_dum;
	register char *pCc;
	register P_AVL_NODE pNode;
#       ifdef DEBUG
	if (iVerbosity >= iVactions)
	    printf("Read default groups from %s\n",DEFRC);
#       endif
	sGr_local.sName= sLine; /* and hold it during the loop */
	while (fgets(sLine,iSlen,fDefnewsrc) != NULL)
	    if (strlen(sLine))
		if (pCc= index(sLine,':'))
	        {
		    *pCc= '\0';
		    if (find_avlnode(&sGr_local,paGr_requests,Grpcmp) == NULL)
			if (pNode= find_avlnode(&sGr_local,paGr_tree,Grpcmp))
			{
                            insert_avl(pNode->content,&paGr_requests
					,Grpcmp,&pGr_dum);
#                           ifdef DEBUG
		            if (iVerbosity >= iVdebug)
			        printf("Default group %s added\n",sLine);
#                           endif
			}
                }
	fclose(fDefnewsrc);
    }
#   ifdef DEBUG
    else
    if (iVerbosity >= iVactions)
	printf("Default newsrc (%s) non-existant or not readable\n",DEFRC);
#   endif
}
else /* if ((fGroups= fopen(NEWSRC,"r")) == NULL) */
{
    s_Group sG_local;
    register char cC;
#   ifdef DEBUG
    if (iVerbosity >= iVactions) printf("Opened file %s for reading\n",NEWSRC);
#   endif
    sG_local.sName= sLine; /* and hold it so during read of .newsrc */
    for (;;)
    {
        if (fgets(sLine,iSlen,fGroups) == NULL)
        {
#           ifdef DEBUG
            if (iVerbosity >= iVprogress) putchar('\n');
            if (iVerbosity >= iVdebug)
                printf("End of %s reached\n",NEWSRC);
#           endif
            break;
        }

            /* search for '!' or ':' or 'white space' */
        pcC= sLine; while(index("!: \t\n",*pcC) == NULL) pcC++;
        cC= *pcC; *pcC= '\0';
        if (p_AVL= find_avlnode(&sG_local,paGr_requests,Grpcmp))
        {
#           ifdef DEBUG
	    if (iVerbosity >= iVprogress) putchar('\n');
#           endif
            if (iVerbosity)
                switch(cC)
                {
                default:
                    puts("Strange line in \".newsrc\", run \"nntidy\"");
                    *pcC= cC; puts(sLine); exit(ERR_EXIT);
                case ':':
                    puts("Already subscribed:");
                    Printcharter(p_AVL->content,1);
                    break;
                case '!':
                    puts("Renew cancelled subscription");
                    Printcharter(p_AVL->content,1);
                    break;
                } /* switch(cC) */
            /* if (iVerbosity) */
            *pcC= ':';
	    pcC= ((s_Group*)(p_AVL->content))->sName;
            *pcC= toupper(*pcC); /* mark as already processed */
        }
        else /* if (p_AVL= find_avlnode(&sG_local,paGr_requests,Grpcmp)) */
	{
            *pcC= cC;
#           ifdef DEBUG
	    if (iVerbosity >= iVprogress) putchar('.');
#           endif
	} /* if (p_AVL= find_avlnode(&sG_local,paGr_requests,Grpcmp)) */
        fputs(sLine,fNewgroups);
    } /* for (;;) */
    fclose(fGroups);
} /* if ((fGroups= fopen(NEWSRC,"r")) == NULL) */

unravel_avl(&paGr_requests,Addsubscription,0);
fclose(fNewgroups);
if (fCharters != NULL) fclose(fCharters);
#ifdef DEBUG
if (iVerbosity >= iVdebug) printf("\nunlink %s\n",OLDRC);
#endif
unlink(OLDRC);
#ifdef DEBUG
if (iVerbosity >= iVdebug) printf("rename %s to %s\n",NEWSRC,OLDRC);
#endif
rename(NEWSRC,OLDRC);
#ifdef DEBUG
if (iVerbosity >= iVdebug) printf("rename %s to %s\n",sNewgrplfn,NEWSRC);
#endif
rename(sNewgrplfn,NEWSRC);
#ifdef NNTP
#ifdef DEBUG
if (iVerbosity == iVdebug) printf("unlink temporary %s\n",sCharterlfn);
if (iVerbosity < iVleavetmp)
#endif
    unlink(sCharterlfn);
#ifdef DEBUG
else printf("Temporary newsgroup descriptions on %s\n",sCharterlfn);
#endif
#endif

if (iVerbosity == 0) exit(iGrfault);
exit(0);

} /* main */
