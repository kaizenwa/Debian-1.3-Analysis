/*
VERSION  2.3
MODIFIED 11/11/91

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

Modified 02/14/93, Fred N. van Kempen, <waltje@uwalt.nl.mugnet.org>
to use #define macros for pathnames and such.  Much cleaner!
*/
#include <stdio.h>
#include <sys/types.h>
#ifdef AIX
#include <sys/select.h>
#endif
#include <sys/stat.h>
#include <rpc/rpc.h>
#include <sys/socket.h>
#include <sys/param.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <ctype.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <pwd.h>
#include <signal.h>
#include <grp.h>

#ifdef SYSV32
#   include <string.h>
#else
#   include <strings.h>
#endif

#ifdef SHADOW
#   include <shadow.h>
#endif

#ifdef sgi
extern struct group *getgrent();
#endif

extern char *crypt();

extern char *strchr();

#ifdef WINNFS
#   include <rpc/clnt.h>
#   include <tiuser.h>
#endif


#define LP_ALLOW	"/usr/spool/lp/admins/lp/printers/%s/users.allow"
#define LP_DENY		"/usr/spool/lp/admins/lp/printers/%s/users.deny"
#define LP_FILE		"%s/%s.%03d"
#ifdef SCO
#   define P_STATUS	"/usr/spool/lp/system/pstatus"
#   define C_STATUS	"/usr/spool/lp/system/cstatus"
#   define LP_FILE1	"%s/%s.%03dq"
#else
#   define PRINTCAP	"/etc/printcap"
#   define LP_FILE1	"%s/%s.%03dqueued"
#endif


#ifdef SCO
#define ALLOW 0
#define DENY  1
#define CLASS 2

struct {
		int access;
		int count;
		union {
		char **users;
		int  *printers;
		} n;
	} printer_access[100];
#endif

#define BW_NFS_PROGRAM 0x2f00dbadL
#define BW_NFS_VERSION 1L

#define SPOOL_INQUIRE  1L	/* Return fhandle of spool directory */
#define SPOOL_FILE     2L	/* Spool file */
#define AUTHORIZE      3L	/* Authorize and return UID and GIDs (max 16) */
#define GRP_NAME_TO_NUMB 4L	/* Convert group name to number */
#define GRP_TO_NUMBER 5L	/* Convert group number(s) to name(s) */
#define RETURN_HOST    6L	/* Convert IP to hostname */
#define UID_TO_NAME    7L	/* Convert UID(s) to name(s) */
#define SHARE	      20L	/* DOS 3.1 Share function */
#define UNSHARE       21L	/* DOS 3.1 UnShare function */
#define LOCK          22L	/* DOS 3.1 Lock request */
#define REMOVE        23L	/* Remove all locks/shares for PC */
#define UNLOCK        24L	/* DOS 3.1 Unlock request */

#define NUSRS  10		/* Maximum names to convert */

static  char 	dir[32],g_string[32];
static  char 	spool_dir[132], file[256], file1[256];
static  char	remote_host[16];
static  char 	*printers[500];
static	int  	num_printers;
static	char  	printer[12];
static  char	*jobname;
static  char  	file_name[9];
static  char    *hostname,*p;
static  int   	extension, lgids[NGRPS+1];
static  int	luids[NUSRS+1];
static  int	A_flag=0, s_flag=0, do_print;
static  u_long	my_ip;
static  u_long  incomming_ip;
int debugmode;

static  struct mm_ips		/* Valid hosts to authenticate for */
 	{
		char	type;
		u_long	ip;
		u_long	mask;
		u_long	redirect_ip;
	} **ips;

static struct mm_uids		/* Cache of uid to name translation */
	{
		int	mm_uid;
		char	*mm_username;
	} **uds;


static struct mm_gidss		/* Cache of uid and associated gids */
	{
		int mm_uid;
		int mm_gid;
		int mm_gid_count;
		int mm_gids[NGRPS];
	} **gds;


static int gids_count=0,uids_count=0;


static int	ips_count=0;

static struct	locking
	{
		unsigned long   cookie;         /* 4 bytes */
		char	        name[17];       /* 16 character name */
		char            fh[32];         /* 32 bytes */
		int	        mode;
		int	        access;
		unsigned long	offset;
		unsigned long	length;
		int	        stat;
		int	        sequence;
	} lock = { 0 };

#ifdef SCO

int  check_access_printer(pn,name)
int pn;
char *name;
{
	int i;
	char **p;

        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [check_access_printer] pn = %d, name = %s\n",
                          pn, name );

	if ( printer_access[pn].count == -1)
        {
          if (debugmode)
            (void) fprintf( stdout, "bwnfsd: [check_access_printer] deny all requests\n" );
	  return(0);                                   /* Deny all */
	}
	p = printer_access[pn].n.users;
	for ( i = 0 ; i < printer_access[pn].count ; i++)
	{
	   if ( strcmp(*p++,name) == 0)
           {
             if (debugmode)
               (void) fprintf( stdout, "bwnfsd: [check_access_printer] request allowed\n" );
	     return ( printer_access[pn].access == ALLOW);
	   }
	}
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [check_access_printer] request denied\n" );
	return( printer_access[pn].access == DENY);
}

int check_access(pn,name)
int pn;
char *name;
{
	int i;
	int *pi;

        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [check_access] pn = %d, name = %s\n", pn, name );

	if ( printer_access[pn].access != CLASS)
		return(check_access_printer(pn,name));
	pi = printer_access[pn].n.printers;
	for ( i = 0 ; i < printer_access[pn].count; i++)
		if ( check_access_printer(*pi++,name))
		{
		  if (debugmode)
		    (void) fprintf( stdout, "bwnfsd: [check_access] returns 1\n" );
		  return(1);
		}
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [check_access] returns 0\n" );
	return(0);
}
#endif


void free_child()
{
	int	pstatus;

        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [free_child] called\n" );

	(void) wait(&pstatus);
#ifdef SIGCHLD
	(void) signal(SIGCHLD,free_child);
#endif
#ifdef SIGCLD
	(void) signal(SIGCLD,free_child);
#endif
}

void strlwr(p)
	char *p;
{
	do
	{
	   if ( (*p >= 'A') && (*p <= 'Z'))
	      *p += 'a'-'A';
	} while (*(++p) != '\00');
}


void getmask(ip,mask)
	u_long *ip,*mask;
{
	if ( *ip == 0L)
	   *mask = 0L;
	else if ( *ip <= 0xffL)
	{
	   *ip <<= 24;
	   *mask = 0xff000000L;
	}
	else if ( *ip <= 0xffffL)
	{
	   *ip <<= 16;
	   *mask = 0xffff0000L;
	}
	else if ( *ip <= 0xffffffL)
	{
	   *ip <<= 8;
	   *mask = 0xffffff00L;
	}
	else
	   *mask = 0xffffffffL;
}


/*
   For -s parameter, returns IP addresses and the associated mask to determine
   if the IP address of the destination machine/network is matches an entry
   in the -s file.
*/
int convert_ip(s,ip,mask)
	char *s;
	u_long *ip,*mask;
{
	u_long ia,lip,lmask;
	struct hostent *he;
	struct netent *ne;

        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [convert_ip] s = %s\n", s );

	if ( (ia =inet_network(s)) != (~0L))
	{
		lip = ia;
		getmask(&lip,&lmask);
	}
	else if ( (ia =inet_addr(s)) != (~0L))
	{
		lip = ia;
		lmask = 0xffffffffL;
	}
	else if ( (ne = getnetbyname(s)) != NULL)
	{
		lip = ne->n_net;
		getmask(&lip,&lmask);
	}
	else if ( (he = gethostbyname(s)) != NULL)
	{
		lip = * (u_long *) he->h_addr;
		lmask = 0xffffffffL;
	}
	else
        {
           if (debugmode)
             (void) fprintf( stdout, "bwnfsd: [convert_ip] returns 0\n" );
	   return(0);
	}
	*ip = lip;
	if ( mask != NULL) *mask = lmask;
        {
          if (debugmode)
            (void) fprintf( stdout, "bwnfsd: [convert_ip] (1) ip = %ul, mask = %ul\n",
                            ip, mask );
  	  return(1);
  	}
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [convert_ip] (0) ip = %ul, mask = %ul\n",
                          ip, mask );
}

/*
	Read the -s file which lists rules for which hosts can be
        authenticated. Each line is converted to a IP address and mask. Any
        incoming address is ANDed with the mask and compared to IP address,
	if it matches, the action associated with the line is taken.

	Valid actions:
		"+" - Perform Validatation check
		"-" - Return Authorization error
		"=" - Tell client to attempt Authorization at new address
*/
void read_ips(file)
	char *file;
{
	FILE *f;
	char s1[255],s2[255],line[255],type;
	int	i;
	u_long	ip,mask,redirect_ip;

        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [read_ips] file = %s\n", file );

	if ( (f = fopen(file,"r")) == NULL)
	{
		(void) fprintf(stdout,"bwnfsd: [read_ips] \42%s\42 file not found.\n",file);
                (void) fflush( stdout );
		exit(1);
	}

	ips = (struct mm_ips **) malloc(1);
	while ( fgets(line,255,f) != NULL)
	{
	   if ( (type = line[0]) != '#')
	   {
		i = sscanf(&line[1],"%s %s",s1,s2);
                if (debugmode)
                  (void) fprintf(stdout, "bwnfsd: [read_ips] parsing <%s>\n", line[1] );

	        if ( ( i == EOF) || ( i == 0) || ( i > 2) || ( (type != '+') &&
                     ( type != '-') && ( type != '=')) || ( (type == '=') &&
                     ( i != 2) ) || ( (type != '=') && ( i != 1)) )
		{
		   (void) fprintf( stdout, "bwnfsd: [read_ips] syntax error, line ignored : %s",line);
		}
		else
		 for ( ;;)
                 {
		   if ( !convert_ip(s1,&ip,&mask))
		   {
		      (void) fprintf( stdout, "bwnfsd: [read_ips] \42%s\42 invalid address.\n",s1);
		      break;
		   }
		   if ( i == 2)
		      if ( !convert_ip(s2,&redirect_ip,(u_long *)NULL))
		      {
		         (void) fprintf(stdout,"bwnfsd: [read_ips] \42%s\42 invalid address.\n",s1);
		         break;
		      }
		   ips = (struct mm_ips **) realloc(ips,sizeof(ips)*(ips_count+1));
		   *(ips+ips_count) = (struct mm_ips *) malloc(sizeof(struct mm_ips));
		   (*(ips+ips_count))->type        = type;
		   (*(ips+ips_count))->ip          = ip;
		   (*(ips+ips_count))->mask        = mask;
		   (*(ips+ips_count))->redirect_ip = redirect_ip;
		   ips_count++;
		   break;
		}
	   }
	}
	fclose(f);
        if (debugmode)
          (void) fprintf(stdout,"bwnfsd: [read_ips] exiting with %u values\n", ips_count );
}


/*
	Add UID to USERNAME translation to cache
*/
void add_uid(uid,username)
	int	uid;
	char	*username;
{
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [add_uid]  uid <%d>, name <%s>\n",
                          uid, username );
	if ( uids_count++ == 0 )
	   uds = (struct mm_uids **) malloc(1);

	uds                                = (struct mm_uids **) realloc(uds,sizeof(uds)*uids_count);
        (*(uds+uids_count-1))              = (struct mm_uids *) malloc(sizeof(struct mm_uids));
	(*(uds+uids_count-1))->mm_uid      = uid;
	(*(uds+uids_count-1))->mm_username = (char *) malloc( strlen(username)+1);
	(void) strcpy((*(uds+uids_count-1))->mm_username,username);
}


/*
	Lookup USERNAME given a UID, also add translation to cache
*/
char *get_ui_name(uid)
	int	uid;
{
	struct	passwd *pww;
	int	i;

        if (debugmode)
          (void) fprintf(stdout, "bwnfsd: [get_ui_name] uid <%u>\n", uid );

	for ( i = 0 ; i < uids_count; i++)
	   if ( (*(uds+i))->mm_uid == uid) return ((*(uds+i))->mm_username);
	if ( (pww = getpwuid(uid)) == NULL)
        {
           if (debugmode)
             (void) fprintf( stdout, "bwnfsd: [get_ui_name] getpwuid returned NULL\n" );
           return(NULL);
        }
	add_uid(uid,pww->pw_name);
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [get_ui_name] returns %s\n",
                          pww->pw_name );
	return(pww->pw_name);
}


/*
	Given a UID, GID and USERNAME, create a list of GIDs associated
	with the UID, a maximum of NGRPS GIDS are used.
*/
void fill_gid(gd,uid,gid,username)
	struct mm_gidss *gd;
	int	uid,gid;
	char	*username;
{
	struct group *gr;
	struct group *getgrent();
	char	**p;
	int	flag;

	add_uid(uid,username);	
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [fill_gid] uid = %u, gid = %u, user = %s\n",
                          uid, gid, username );
	flag             = 0;
	gd->mm_uid       = uid;
	gd->mm_gid       = gid;
	gd->mm_gid_count = 0;
	(void) setgrent();
	while ( (gr = getgrent()) != NULL)
	{
		p = gr->gr_mem;
		while ( ((*p) != NULL) && (gd->mm_gid_count < NGRPS))
		{
		  if ( strcmp((*p),username) == 0)
		  {
		     gd->mm_gids[gd->mm_gid_count++] = gr->gr_gid;
		     if ( gr->gr_gid == gid) flag=1;
		     break;
		  }
		  p++;
		}
	}
	if  ( (flag == 0) && ( gd->mm_gid_count < NGRPS) )
	   gd->mm_gids[gd->mm_gid_count++] = gid;
	(void) endgrent();
}


/*
	Return a list of GIDs for a given UID. The cache of GIDS/UID is
	updated and used.
*/	
struct mm_gidss *get_gids(uid,gid,username)
	int	uid,gid;
	char	*username;
{
	int	i;

        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [mm_gidss] uid = %u, gid = %u, name = %s\n",
                          uid, gid, username );

	for ( i = 0 ; i < gids_count; i ++)
	   if( (*(gds+i))->mm_uid == uid)
	   {
             if (debugmode)
	        (void) fprintf( stdout, "bwnfsd: [mm_gidss] gids_count = %u\n",
	                     gids_count );
	     return(*(gds+i));
	   }

	if ( gids_count++ == 0 )
	   gds = (struct mm_gidss **) malloc(1);

	gds = (struct mm_gidss **) realloc(gds,sizeof(gds)*gids_count);
	(*(gds+gids_count-1)) = (struct mm_gidss *) malloc(sizeof(struct mm_gidss));
	(void) fill_gid((*(gds+gids_count-1)),uid,gid,username);
        if (debugmode)
        (void) fprintf( stdout, "bwnfsd: [mm_gidss] gids_count = %u\n",
	                gids_count );
	return(*(gds+gids_count-1));
}

	
static struct my_groups
	{
	int my_gid;
	char	*my_name;
	} **g;

static int group_count=0;

/*
	Return the text name of a GID, if none is found, then the GID
	is convert to a text number.
*/
char *get_gr_name(gid)
	int	gid;
{
	static	char gd[32];
	int	i;

        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [get_gr_name] for gid %u\n", gid );

	for ( i = 0 ; i < group_count; i++)
		if ( (*(g+i))->my_gid == gid)
		{
		  if (debugmode)
		    (void) fprintf( stdout, "bwnfds: [get_gr_name] returns %s\n",
		                    ((*(g+i))->my_name) );
		  return ((*(g+i))->my_name);
		}
	(void) sprintf(gd,"%u",gid);
        if (debugmode)
          (void) fprintf( stdout, "bwnfds: [get_gr_name] returns %s\n", gd );
        return(gd);
}


/*
	Given a group name, return the GID associated with it.
*/
int *get_gr_gid(name)
	char *name;
{
	int	i;

        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [get_gr_gid] for %s\n", name );
	for ( i = 0; i < group_count; i++)
		if ( strcmp((*(g+i))->my_name,name) == 0 )
                {
                   if (debugmode)
                     (void) fprintf( stdout, "bwnfsd: [get_gr_gid] returns %s\n",
                                     (*(g+i))->my_gid);
  		   return(&(*(g+i))->my_gid);
                }
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [get_gr_gid] returns NULL\n" );
	return (NULL);
}


/*
	Cache a list of all group names and their associated GIDS.
*/
void get_groups()
{
	struct group *gr;
	struct group *getgrent();

        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [get_groups] called\n" );	

	g = (struct my_groups **) malloc(1);
	(void) setgrent();
	while (( gr = getgrent()) != NULL)
	{
	  group_count++;
	  g = (struct my_groups **) realloc(g,sizeof(g)*group_count);
	  (*(g+group_count-1)) = (struct my_groups *) malloc(sizeof(struct my_groups));
	  (*(g+group_count-1))->my_name = (char *) malloc(strlen(gr->gr_name)+1);	

	  (void) strcpy((*(g+group_count-1))->my_name,gr->gr_name);
	  (*(g+group_count-1))->my_gid = gr->gr_gid;
	}
	(void) endgrent();
}


#ifdef SCO

void get_class_list(char *name,int pn)
{
	FILE *f;
	char line[128];
	char file_name[64];
	int i;

        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [get_class_list] name = %s, pn =%u\n",
                          name, pn );
	printer_access[pn].access     = CLASS;
	printer_access[pn].n.printers = NULL;
	printer_access[pn].count      = 0;
	sprintf(file_name,"/usr/spool/lp/admins/lp/classes/%s",name);
	
	if ( (f = fopen(file_name,"r")) == NULL)
        {
          if (debugmode)
            (void) fprintf( stdout, "bwnfsd: [get_class_list] open file %s returned NULL\n",
                            file_name );
	  return;
	}
	while ( fgets(line,128,f) != NULL)
	{
		*(line+strlen(line)-1) = '\0';
		for ( i = 0 ; i < pn; i++)
		   if ( strcmp(line,printers[i]) == 0)
		   {
			if ( printer_access[pn].count++ == 0)
				printer_access[pn].n.printers = malloc(sizeof(int));
			else
				printer_access[pn].n.printers = realloc(printer_access[pn].n.printers,printer_access[pn].count * sizeof(int));	
			*(printer_access[pn].n.printers+printer_access[pn].count-1) = i;
			break;
	     	   }
	}
	fclose(f);
}

void get_access(char *name,int pn)
{
	FILE *f;
	char line[128];
	char file_name[64];

        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [get_access] name = %s, pn =%u\n",
                          name, pn );
	printer_access[pn].access  = ALLOW;
	printer_access[pn].n.users = NULL;
	printer_access[pn].count   = 0;
	sprintf(file_name, LP_ALLOW,name);
	if ( (f = fopen(file_name,"r")) != NULL)
	{
                if (debugmode)
                  (void) fprintf( stdout, "bwnfsd: [get_access] reading %s\n",
                                  file_name );
		while ( fgets(line,128,f) != NULL)
		{
			*(line+strlen(line)-1) = '\0';
			if ( strcmp(line,"any") == 0)
			{
				printer_access[pn].n.users = NULL;
				printer_access[pn].count = 0;
				break;	
			}
			if ( printer_access[pn].count++ == 0)
				printer_access[pn].n.users = malloc(sizeof(char*));
			else
				printer_access[pn].n.users = realloc(printer_access[pn].n.users,printer_access[pn].count * sizeof(char*));	
			*(printer_access[pn].n.users+printer_access[pn].count-1) = strdup(line);
		}
		fclose(f);
		if ( printer_access[pn].count != 0)
		{
		  if (debugmode)
		    (void) fprintf( stdout, "bwnfsd: [get_access] access count = %u\n",
		           printer_access[pn].count );
		  return;
                }		
	}
	else
	{
          if (debugmode)
            (void) fprintf( stdout, "bwnfsd: [getaccess] unable to open %s\n",
                            file_name );
	}
	printer_access[pn].access = DENY;
	sprintf(file_name,LP_DENY,name);
	if ( (f = fopen(file_name,"r")) != NULL)
	{
		while ( fgets(line,128,f) != NULL)
		{
			*(line+strlen(line)-1) = '\0';
			if ( strcmp(line,"any") == 0)
			{
				printer_access[pn].n.users = NULL;
				printer_access[pn].count   = -1;
				break;	
			}
			if ( printer_access[pn].count++ == 0)
				printer_access[pn].n.users = malloc(sizeof(char*));
			else
				printer_access[pn].n.users = realloc(printer_access[pn].n.users,printer_access[pn].count * sizeof(char*));	
			*(printer_access[pn].n.users+printer_access[pn].count-1) = strdup(line);
		}
		fclose(f);
	}
	else
	{
          if (debugmode)
            (void) fprintf( stdout, "bwnfsd: [getaccess] unable to open %s\n",
                            file_name );
	}
	
}


void get_printer_list(status_file,type)
char *status_file;
int  type;
{
	FILE *f;
	char line[128],name[32];
	int  i;

        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [get_printer_list] file = %s, type = %u\n",
                          status_file, type );
	if ( (f = fopen(status_file,"r")) == NULL)
        {
          if (debugmode)
            (void) fprintf( stdout, "bwnfsd: [get_printer_list] unable to open %s\n",
                            status_file );
	  return;
	}
	while ( fgets(line,128,f) != NULL)
	   if ( strncmp(line,"=====",5) == 0)
	   {
		if ( fgets(name,32,f) == NULL) break;
		if ( fgets(line,128,f) == NULL) break;
                if (debugmode)
                  (void) fprintf( stdout, "bwnfsd: [get_printer_list] name = %s, line = %s\n",
                                  name, line );
		for ( i = 0; i < strlen(line); i++)
			if ( strncmp(&line[i],"accepting",9) == 0) break;
		if ( i == strlen(line)) continue;
		*(name+strlen(name)-1) = '\0';
		printers[num_printers] = strdup(name);
		if ( type == 0 )
			get_access(name,num_printers++);
		else
			get_class_list(name,num_printers++);
	   }
	fclose(f);
}
#endif


/*
	Cache a list of available printers, BSD code searchs the /etc/printcap
	file. Creating a dummy /etc/printcap can also work.
*/
void get_printers()
{
#ifndef SCO
  static char line[132];
  char *p,*p1;
  FILE *f;
  int  i;
#endif

  if (debugmode)
    (void) fprintf( stdout, "bwnfsd: [get_printers] called\n" );

  num_printers = 0;
#ifdef SCO
	get_printer_list(P_STATUS,0);
	get_printer_list(C_STATUS,1);
}
#else
  if ( (f = fopen(PRINTCAP, "r")) == NULL)
  {
    (void) fprintf( stdout, "bwnfsd: File %s was not found.\n" , PRINTCAP);
    (void) fprintf( stdout, "        No printers are available for use.\n" );
    return;
  }

  while ( fgets(line,132,f) != NULL)
  {
     p = &line[0];
     while ( (*p == ' ') || (*p == '\t') ) p++;
     while (( *p != '#') && ( *p != ':') )
       {
	   p1 = strchr(p,'|');
	   if ( p1 == NULL) p1 = strchr(p,':');
           if ( p1 != NULL)
           {
              *p1 = '\000';
              printers[num_printers] = (char *) malloc(strlen(p) + 1);
              (void) strcpy(printers[num_printers++],p);
              p = p1 +1;
           }
           else
              *p = '#';
        }

  }
  fclose(f);

  if (debugmode)
  {
    (void) fprintf( stdout, "bwnfsd: [get_printers] Following devices are available\n" );
    for ( i = 0; i < num_printers; i++)
      (void) fprintf( stdout, "bwnfsd:                 %s\n", printers[i] );
  }

}
#endif


static struct var_file {
        int      	status;
        int         	dir_size;
        char         	*dir_handle;
} f;

/*
	DOS 3.1 Sharing and Unsharing code. Uses the same format for rpc.lockd
	as BWNFSD, so some parameters are ignored. The cookie under rpc.lockd
	must be 4 bytes long under BWNFSD.
*/
xdr_share(xdrsp,l)
	XDR *xdrsp;
	struct locking *l;
{
   int	i;
   char g[32];
   char *p;

   if (debugmode > 1)
   {
     (void) fprintf( stdout, "bwnfsd: [xdr_share] called for %s, ofs = %ld, len = %ld\n",
                     l->name, l->offset, l->length );
     (void) fprintf( stdout, "bwnfsd:             mode = %u, access = %u, stat = %u, sequence = %u\n",
                     l->mode, l->access, l->stat, l->sequence );
   }
   if (!xdr_int(xdrsp, &l->cookie)) return(0); /* Get length of cookie, MUST be 4 */
   if ( l->cookie != 4)             return(0);
   if (!xdr_int(xdrsp, &l->cookie)) return(0); /* Get cookie as an unsigned long */

   p = l->name;
   if (!xdr_string(xdrsp,&p,17))    return(0);
   p = l->fh;
   if (!xdr_bytes(xdrsp,&p,&i,32))  return(0);
   p = g;
   if (!xdr_bytes(xdrsp,&p,&i,32))  return(0);
   if (!xdr_int(xdrsp, &l->mode))   return(0);
   if (!xdr_int(xdrsp, &l->access)) return(0);

   if (debugmode)
     (void) fprintf( stdout, "bwnfsd: [xdr_share] returns (1)\n" );

   return(1);
}


/*
	DOS 3.1 Share response.
*/
xdr_shareres(xdrsp,l)
	XDR *xdrsp;
	struct locking *l;
{
   int i;
   char *p;

   if (debugmode)
     (void) fprintf( stdout, "bwnfsd: [xdr_shareres] called by %s\n", l->name );
   i = 4;
   if (!xdr_int(xdrsp, &i))         return(0); /* Send length of cookie, MUST be 4 */
   if (!xdr_int(xdrsp, &l->cookie)) return(0); /* Send cookie as an unsigned long */
   if (!xdr_int(xdrsp,&l->stat))    return(0);
   l->sequence++;
   if (!xdr_int(xdrsp,&l->sequence)) return(0);

   if (debugmode)
     (void) fprintf( stdout, "bwnfsd: [xdr_shareres] returns (1)\n" );
   return(1);
}


/*
	DOS 3.1 locking request. Note that several parameters are ignored.
*/
xdr_lock(xdrsp,l)
	XDR *xdrsp;
	struct locking *l;
{
   int	i;
   char g[32];
   char *p;

   if (debugmode)
   {
     (void) fprintf( stdout, "bwnfsd: [xdr_lock] called by %s\n", l->name );
     if (debugmode > 1)
       (void) fprintf( stdout, "bwnfsd:            ofs = %ul, len = %ul\n",
                       l->offset, l->length );
   }
   if (!xdr_int(xdrsp, &l->cookie)) return(0);  /* Get length of cookie, MUST be 4 */
   if (!xdr_int(xdrsp, &l->cookie)) return(0);  /* Get cookie as an unsigned long */
   if (!xdr_int(xdrsp,&i))          return(0);  /* block */
   if (!xdr_int(xdrsp,&i))          return(0);  /* exclusive */
   p = l->name;
   if (!xdr_string(xdrsp,&p,17))    return(0);
   p = l->fh;
   if (!xdr_bytes(xdrsp,&p,&i,32))  return(0);
   p = g;
   if (!xdr_bytes(xdrsp,&p,&i,32))  return(0);   /* oh */
   if (!xdr_int(xdrsp,&i))          return(0);  /* svid */
   if (!xdr_int(xdrsp, &l->offset)) return(0);
   if (!xdr_int(xdrsp, &l->length)) return(0);
   if (debugmode > 1)
     (void) fprintf( stdout, "bwnfsd: [xdr_lock] returns (1)\n" );
   return(1);
}


/*
	DOS 3.1 unlock request. Note that several parameters are ignored.
*/
xdr_unlock(xdrsp,l)
	XDR *xdrsp;
	struct locking *l;
{
   int	i;
   char g[32];
   char *p;

   if (debugmode)
   {
     (void) fprintf( stdout, "bwnfsd: [xdr_unlock] called by %s\n", l->name );
     if (debugmode > 1)
       (void) fprintf( stdout, "bwnfsd:              ofs = %ul, len = %ul\n",
                       l->offset, l->length );
   }
   if (!xdr_int(xdrsp, &l->cookie)) return(0);  /* Get length of cookie, MUST be 4 */
   if (!xdr_int(xdrsp, &l->cookie)) return(0);  /* Get cookie as an unsigned long */

   p = l->name;
   if (!xdr_string(xdrsp,&p,17))    return(0);
   p = l->fh;
   if (!xdr_bytes(xdrsp,&p,&i,32))  return(0);
   p = g;
   if (!xdr_bytes(xdrsp,&p,&i,32))  return(0);   /* oh */
   if (!xdr_int(xdrsp,&i))          return(0);   /* svid */
   if (!xdr_int(xdrsp, &l->offset)) return(0);
   if (!xdr_int(xdrsp, &l->length)) return(0);
   if (debugmode > 1)
     (void) fprintf( stdout, "bwnfsd: [xdr_unlock] returns (1)\n" );
   return(1);
}



/*
	Send Locking response.
*/
xdr_res(xdrsp,l)
	XDR *xdrsp;
	struct locking *l;
{
   int i;
   char *p;

   if (debugmode > 1)
     (void) fprintf( stdout, "bwnfsd: [xdr_res] called\n" );
   i = 4;
   if (!xdr_int(xdrsp, &i))         return(0); /* Send length of cookie, MUST be 4 */
   if (!xdr_int(xdrsp, &l->cookie)) return(0); /* Send cookie as an unsigned long */
   if (!xdr_int(xdrsp,&l->stat))    return(0);
   if (debugmode)
     (void) fprintf( stdout, "bwnfsd: [xdr_res] returns (1)\n" );
   return(1);
}


/*
	Remove all DOS 3.1 locks and shares, not that the PC's name is
	a maximum of 16 chaarcters.
*/
xdr_get_lock_name(xdrsp,l)
	XDR *xdrsp;
	char *l;
{
   if (debugmode)
     (void) fprintf( stdout, "bwnfsd: [xdr_get_lock_name] called\n" );
   return(xdr_string(xdrsp,&l,17));
}



/*
	Send a redirect to a new IP during an authorization request.
*/
xdr_new_ip(xdrsp, ip)
        XDR *xdrsp;
        u_long *ip;
{
   int i;

   if (debugmode)
     (void) fprintf( stdout, "bwnfsd: [xdr_new_ip] called\n" );
   i = 3;
   if (!xdr_int(xdrsp, &i)) return(0);
   if (!xdr_int(xdrsp, ip)) return(0);
   if (debugmode)
     (void) fprintf( stdout, "bwnfsd: [xdr_new_ip] returns (1)\n" );
   return(1);
}


/*
	Get GROUP name.
*/
xdr_g_string(xdrsp, st)
        XDR *xdrsp;
        char *st;
{
  char *p;

  if (debugmode)
    (void) fprintf( stdout, "bwnfsd: [xdr_g_string] called\n" );
  p = st;
  if (!xdr_string(xdrsp,&p,32)) return(0);
  if (debugmode)
    (void) fprintf( stdout, "bwnfsd: [xdr_g_string] returns (1)\n" );
  return(1);
}


/*
	Send Status and GID
*/
xdr_send_gid(xdrsp, st)
        XDR *xdrsp;
        char *st;
{
  int   *i,j;

  if (debugmode)
    (void) fprintf( stdout, "bwnfsd: [xdr_send_gid] called with st = %s\n",
                    st );
  i = get_gr_gid(st);
  if ( i == NULL)
  {
     j = 2;	/* Error Status */
     if (!xdr_int(xdrsp,&j)) return(0);
  }
  else
  {
     j = 0;
     if ( !xdr_int(xdrsp,&j) ) return(0);
     if ( !xdr_int(xdrsp,i) )  return(0);
  }
  if (debugmode)
    (void) fprintf( stdout, "bwnfsd: [xdr_send_gid] returns (1)\n" );
  return(1);
}


/*
	Get list of UIDs to translate to text.
*/
xdr_nuids(xdrsp, luids)
        XDR *xdrsp;
        int	luids[NUSRS+1];
{
  int i;

  if (debugmode)
    (void) fprintf( stdout, "bwnfsd: [xdr_nuids] called\n" );
  if ( !xdr_int(xdrsp,&luids[0]) ) return(0);
  if ( ( luids[0] < 0) || ( luids[0] > 16) ) luids[0]=0;
  for ( i = 1; i <= luids[0]; i++)
     if ( !xdr_int(xdrsp,&luids[i])) return(0);
  if (debugmode)
    (void) fprintf( stdout, "bwnfsd: [xdr_nuids] returns (1)\n" );
  return(1);
}


/*
	Get list of GIDs to translate to text.
*/
xdr_ngids(xdrsp, lgids)
        XDR *xdrsp;
        int	lgids[NGRPS+1];
{
  int i;

  if (debugmode)
    (void) fprintf( stdout, "bwnfsd: [xdr_ngids] called\n" );

  if ( !xdr_int(xdrsp,&lgids[0]) ) return(0);
  if ( ( lgids[0] < 0) || ( lgids[0] > 16) ) lgids[0]=0;
  for ( i = 1; i <= lgids[0]; i++)
     if ( !xdr_int(xdrsp,&lgids[i])) return(0);
  if (debugmode)
    (void) fprintf( stdout, "bwnfsd: [xdr_ngids] returns (1)\n" );
  return(1);
}


/*
	Send Usernames translated from UIDS.
*/
xdr_send_usernames(xdrsp, luids)
        XDR *xdrsp;
        int	luids[NUSRS+1];
{
  int i,j;
  char *p;

  if (debugmode)
    (void) fprintf( stdout, "bwnfsd: [xdr_send_usernames] called\n" );
  for ( i = 1; i <= luids[0]; i++)
  {
    p = get_ui_name(luids[i]);
    if (debugmode)
      (void) fprintf( stdout, "bwnfsd: [xdr_send_usernames] p = %s\n", p );
    if ( p == NULL)
    {
	j = 0;
	if( !xdr_int(xdrsp,&j))return(0);
    }
    else
    {
       if ( !xdr_string(xdrsp,&p,32)) return(0);
    }
  }
  if (debugmode)
    (void) fprintf( stdout, "bwnfsd: [xdr_send_usernames] returns (1)\n" );
  return(1);
}


/*
	Send Group names translated from GIDS.
*/
xdr_send_names(xdrsp, lgids)
        XDR *xdrsp;
        int	lgids[NGRPS+1];
{
  int i,j;
  char *p;

  if (debugmode)
    (void) fprintf( stdout, "bwnfsd: [xdr_send_names] called\n" );
  for ( i = 1; i <= lgids[0]; i++)
  {
    p = get_gr_name(lgids[i]);
    if (debugmode)
      (void) fprintf( stdout, "bwnfsd: [xdr_send_names] p = %s\n", p );
    if ( p == NULL)
    {
	j = 0;
	if( !xdr_int(xdrsp,&j)) return(0);
    }
    else
    {
       if ( !xdr_string(xdrsp,&p,32)) return(0);
    }
  }
  if (debugmode)
    (void) fprintf( stdout, "bwnfsd: [xdr_send_names] returns (1)\n" );
  return(1);
}


/*
	Return fhandle of spool directory. Note a loop of XDR_INTS is
	used to allow this to compile on all systems.
*/
xdr_file(xdrsp, file_var)
        XDR *xdrsp;
        struct var_file *file_var;
{
  int   i;

  if (debugmode)
    (void) fprintf( stdout, "bwnfsd: [xdr_file] called\n" );
  if (!xdr_int(xdrsp,&file_var->status)) return(0);
  if (file_var->status !=0)
  {
    if (debugmode > 1)
      (void) fprintf( stdout, "bwnfsd: [xdr_file] status = %u, returns (1)\n",
                      file_var->status );
    return(1);
  }
  for (i = 0 ; i < 32; i+=4)
     if (!xdr_int(xdrsp, (&file_var->dir_handle[0]) + i)) return(0);
  if (debugmode)
    (void) fprintf( stdout, "bwnfsd: [xdr_file] returns (1)\n" );
  return(1);
}


/*
	Spool a file to the printer:

		Parameters:
			printer_name (fixed 11 bytes)
			file_name    (fixed 8 bytes) (Internet Address in Hex).
			extension     xdr_int ( filename is form C02A0405.xxx)
*/
xdr_print(xdrsp )
   XDR *xdrsp;
{
   struct stat buf;
   struct passwd *pw1;
   char  *p;
   int   i;

   if (debugmode)
     (void) fprintf( stdout, "bwnfsd: [xdr_print] called\n" );
   i = 11;
   p = printer;
   if (!xdr_bytes(xdrsp,&p,&i, 11) )
   {
     if (debugmode > 1)
       (void) fprintf( stdout, "bwnfsd: [xdr_print] printer = %s, file = %s, ext = %s\n",
                       printer, file_name, extension );
     return(0);
   }
   printer[11]=' ';
   *strchr(printer, ' ') = '\0';
   i = 8;
   p = file_name;
   if (!xdr_bytes(xdrsp,&p,&i, 8) )
   {
     if (debugmode > 1)
       (void) fprintf( stdout, "bwnfsd: [xdr_print] printer = %s, file = %s, ext = %s\n",
                       printer, file_name, extension );
     return(0);
   }
   file_name[8]='\0';
   if (!xdr_int(xdrsp,&extension))
   {
     if (debugmode > 1)
       (void) fprintf( stdout, "bwnfsd: [xdr_print] printer = %s, file = %s, ext = %s\n",
                       printer, file_name, extension );
     return(0);
   }
   strlwr(file_name);
   (void) sprintf(file,LP_FILE,spool_dir,file_name,extension);
#ifdef SCO
   (void) sprintf(file1,LP_FILE1,spool_dir,file_name,extension);
#else
   (void) sprintf(file1,LP_FILE1,spool_dir,file_name,extension);
#endif
   if (debugmode > 1)
     (void) fprintf( stdout, "bwnfsd: [xdr_print] f = %s, f1 = %s\n",
                     file, file1 );
   jobname  = NULL;
   do_print = -1;
   if ( stat(file,&buf) == 0 )
   {
      do_print = rename(file,file1);
      pw1 = getpwuid( (int) buf.st_uid);
      if (pw1 != NULL)
      {
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [xdr_print] jobname = %s\n",
                          pw1->pw_name );
	 jobname = pw1->pw_name;
      }
   }
   if (debugmode)
     (void) fprintf( stdout, "bwnfsd: [xdr_print] returns (1)\n" );
   return(1);
}


struct auth_struct
{
   u_long ip;
   int   type;
   char  device[13];
   char  login_text[64];
   char  *username;
   char  *password;
} auth_request;

/*
	Request Authorization:
		Parameters:
			IP_Address of server host (XDR_INT)
			Type of request (XDR_INT) ( 4 - disk, 3 - printer)
			Device to link  (XDR_BYTES max 12) (printer name)
			Username/Password (XDR_BYTES max 64)
*/
xdr_auth_request(xdrsp, auth)
  XDR     *xdrsp;
  struct  auth_struct *auth;
{
  int   i,j,x,y;
  char  *p;

  if (!xdr_int(xdrsp,&auth->ip))   return(0);
  if (!xdr_int(xdrsp,&auth->type)) return(0);
  i = 11;
  p = auth->device;
  if (!xdr_bytes(xdrsp,&p,&i, 12) ) return(0);
  auth->device[11]= ' ';
  p=strchr(auth->device, ' ');
  if ( p == NULL) return(0);
  *p='\00';
  i = 64;
  p = auth->login_text;
  if (!xdr_bytes(xdrsp,&p,&i,64) ) return(0);
  if ( i < 6) auth->type = -1;
  p = auth->login_text;
  for (x = -1, j = 0; j < i; j++)
  {
     y = *p;
     x = *p ^ x;
     *p++ = x;
     x = y;
  }
  auth->username = auth->login_text + 2;
  auth->password = auth->username + strlen(auth->username) + 1;
  if (debugmode)
  {
     (void) fprintf( stdout, "bwnfsd: [xdr_auth_request] called\n" );
     (void) fprintf( stdout, "bwnfsd: Link to %d:%d:%d:%d\n",
                     (auth->ip >> 24 ) & 0xff, (auth->ip >> 16)  & 0xff,
                     (auth->ip >>  8 ) & 0xff, (auth->ip )       & 0xff );
     (void) fprintf( stdout, "bwnfsd: Request type : %u\n", auth->type );
     (void) fprintf( stdout, "bwnfsd: Device name  : %s\n", auth->device );
     (void) fprintf( stdout, "bwnfsd: Username     : %s\n", auth->username );
     (void) fprintf( stdout, "bwnfsd: Password     : %s\n", auth->password );

  }
  if (debugmode)
     (void) fprintf( stdout, "bwnfsd: [xdr_auth_request] returns (1)\n" );
  return(1);
}


/*
	Send successfull authorization reply.
		Results:
                  UID (XDR_INT)
                  GID (XDR_INT)
                  GID count (XDR_INT)
                  GIDs (XDR_INT)	(GID count entries)
*/
xdr_pw(xdrsp,pwd)
  XDR   *xdrsp;
  struct passwd *pwd;
{
  int   i;
  struct mm_gidss *gg;


  if (debugmode)
  {
     (void) fprintf( stdout, "bwnfsd: [xdr_pw] called\n" );
  }
  i  = 0;
  gg = get_gids(pwd->pw_uid,pwd->pw_gid,pwd->pw_name);
  if (debugmode > 1)
  {
     (void) fprintf( stdout, "bwnfsd: uid = %u, gid = %u, gids = %u\n",
                     gg->mm_uid, gg->mm_gid, gg->mm_gids[0] );
  }
  if( !xdr_int(xdrsp,&i) ) return(0);
  i = pwd->pw_uid;
  if( !xdr_int(xdrsp,&i) ) return(0);
  i = pwd->pw_gid;
  if( !xdr_int(xdrsp,&i) ) return(0);
  if( !xdr_int(xdrsp,&gg->mm_gid_count) ) return(0);
  for ( i = 0 ; i < gg->mm_gid_count; i++)
     if ( !xdr_int(xdrsp,&gg->mm_gids[i]) ) return(0);

  if (debugmode > 1)
     (void) fprintf( stdout, "bwnfsd: [xdr_pw] returns (1)\n" );
  return(1);
}


#ifdef SCO
#define SecureWare 1
#include <sys/security.h>
#include <sys/audit.h>
#include <prot.h>
#endif


/*
	The main RPC routine.
*/
create_spool(rqstp, transp)
  struct svc_req *rqstp;
  SVCXPRT *transp;
{
  struct   passwd *pw;
#ifdef SHADOW
      struct   spwd *sp;
#endif
#ifdef SCO
	struct pr_passwd *prpw;
#endif
  int   i;
  u_long	r_ip,i_ip;

  if (debugmode)
     (void) fprintf( stdout, "bwnfsd: [create_spool] called\n" );
  switch(rqstp->rq_proc)
  {
  case NULLPROC:
     if (!svc_sendreply(transp, xdr_void, 0))
     {
        (void) fprintf( stdout, "bwnfsd: [create_spool] (1) Can't reply to RPC call\n");
     }
     return;
  case SPOOL_INQUIRE:
     if (!svc_sendreply(transp, xdr_file, &f))
     {
        (void) fprintf( stdout, "bwnfsd : [create_spool] (2) Can't reply to RPC call\n");
     }
     return;
  case SPOOL_FILE:
     if (!svc_getargs(transp,xdr_print, NULL))
     {
        svcerr_decode(transp);
	return;
     }
     if (!svc_sendreply(transp, xdr_void, 0))
     {
        (void) fprintf( stdout, "bwnfsd: [create_spool] (3) Can't reply to RPC call\n");
     }	
     if ( do_print == 0)
        if ( fork() == 0)
        {
	   print_it(file1,printer,jobname);
      	   exit(0);
        }
     return;
  case AUTHORIZE:
     if (!svc_getargs(transp,xdr_auth_request, &auth_request))
     {
        svcerr_decode(transp);
	return;
     }
     if (auth_request.type == -1)
     {
        i = 1;
        if (!svc_sendreply(transp, xdr_int, &i))
        {
           (void) fprintf( stdout, "bwnfsd: [create_spool] (4) Can't reply to RPC call\n");
        }
        return;
     }
     if ( A_flag == 0)
     {
        if( s_flag == 0)
	{
	  if ( ntohl(auth_request.ip) != my_ip)
	  {
              i = 1;
              if (!svc_sendreply(transp, xdr_int, &i))
              {
                 (void) fprintf( stdout, "bwnfsd: [create_spool] (5) Can't reply to RPC call\n");
              }
              return;
	  }
	}
	else
	{
	for (i = 0; i < ips_count; i++)
        {
	   if ( ( (*(ips+i))->mask & ntohl(auth_request.ip)) == (*(ips+i))->ip)
	   {
	      switch((*(ips+i))->type)
	      {
	      case '+' :
			 i=ips_count+1;
			 break;
	      case '-' :
                         i = 1;
                         if (!svc_sendreply(transp, xdr_int, &i))
                         {
                            (void) fprintf( stdout, "bwnfsd: [create_spool] (5.1) Can't reply to RPC call\n");
                         }
                         return;
	      case '=' :
			 i_ip = (*(ips+i))->redirect_ip;
			 if (!svc_sendreply(transp, xdr_new_ip, &i_ip))
        		 {
                	    (void) fprintf( stdout, "bwnfsd: [create_spool] (4.1) Can't reply to RPC call\n");
        		 }
        		 return;
  	      }
	   }
	}
        if ( i == ips_count)
	{
           i = 1;
           if (!svc_sendreply(transp, xdr_int, &i))
           {
              (void) fprintf( stdout, "bwnfsd: [create_spool] (5.2) Can't reply to RPC call\n");
           }
           return;
	}
	}
     }
#ifdef SCO

     if ( (prpw = getprpwnam(auth_request.username)) == NULL)
     {
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [create_spool] getprpwnam failed\n" );
auth_error:
        i = 1;
        if (!svc_sendreply(transp, xdr_int, &i))
        {
           (void) fprintf( stdout, "bwnfsd: [create_spool] (5.3) Can't reply to RPC call\n");
        }
        return;
     }
     if ( prpw->uflg.fg_lock && (prpw->ufld.fd_lock != 0)) goto auth_error;
     i = prpw->ufld.fd_max_tries;
     if ( i == 0 ) i = 5;
     if ( prpw->ufld.fd_nlogins >= (short) i) goto auth_error;
     prpw->ufld.fd_encrypt[13]='\0';    	
     if ( strcmp(crypt(auth_request.password,prpw->ufld.fd_encrypt),
            prpw->ufld.fd_encrypt) != 0 ) goto auth_error;

     if ( (pw = getpwnam(auth_request.username)) == NULL) goto auth_error;
#else
     if ( (pw = getpwnam(auth_request.username)) == NULL)
     {
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [create_spool] getpwnam failed\n" );
        i = 1;
        if (!svc_sendreply(transp, xdr_int, &i))
        {
           (void) fprintf( stdout, "bwnfsd: [create_spool] (5.3) Can't reply to RPC call\n");
        }
        return;
     }
#ifndef SHADOW
     if ( strcmp(crypt(auth_request.password,pw->pw_passwd),
            pw->pw_passwd) != 0 )
     {
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [create_spool] crypt request failed\n" );
        i = 1;
        if (!svc_sendreply(transp, xdr_int, &i))
        {
           (void) fprintf( stdout, "bwnfsd: [create_spool] (6) Can't reply to RPC call\n");
           (void) fflush( stdout );
           exit(1);
        }
        return;
     }
#else /* if SHADOW */    /* Use Shadow password facility */
     if ( (sp = getspnam(auth_request.username)) == NULL)
     {
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [create_spool] getspnam request failed\n" );
        i = 1;
        if (!svc_sendreply(transp, xdr_int, &i))
        {
           (void) fprintf( stdout, "bwnfsd: [create_spool] (5.1) Can't reply to RPC call\n");
           (void) fflush( stdout );
           exit(1);
        }
        return;
     }

     if ( strcmp(crypt(auth_request.password,sp->sp_pwdp),
            sp->sp_pwdp) != 0 )
     {
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [create_spool] crypt request failed\n" );
        i = 1;
        if (!svc_sendreply(transp, xdr_int, &i)) {
           (void) fprintf( stdout, "bwnfsd: [create_spool] (6) Can't reply to RPC call\n");
           (void) fflush( stdout );
           exit(1);
        }
        return;
     }
#endif
#endif
     if (auth_request.type == 3)
     {
           for ( i = 0; i < num_printers; i++)
	        if( strcmp(printers[i],auth_request.device) == 0)
        	   break;
           if ( i == num_printers)
           {
              if (debugmode)
                (void) fprintf( stdout, "bwnfsd: [create_spool] Printer %s not found\n",
                                auth_request.device );
              i = 2;
	      if (!svc_sendreply(transp, xdr_int, &i))
              {
                (void) fprintf( stdout, "bwnfsd: [create_spool] (7) Can't reply to RPC call\n");
              }
              return;
           }
#ifdef SCO
   	   if ( !check_access(i,auth_request.username))
	   {
              if (debugmode)
                (void) fprintf( stdout, "bwnfsd: [create_spool] Not authorized for %s\n",
                                auth_request.device );
              i = 1;
              if (!svc_sendreply(transp, xdr_int, &i)) {
                 (void) fprintf( stdout, "bwnfsd: [create_spool] (6) Can't reply to RPC call\n");
                 (void) fflush( stdout );
                 exit(1);
              }
              return;
	   }
#endif
      }
     if (!svc_sendreply(transp, xdr_pw, pw))
     {
        (void) fprintf( stdout, "bwnfsd: [create_spool] (8) Can't reply to RPC call\n");
     }
     return;
  case GRP_NAME_TO_NUMB:
     if (!svc_getargs(transp,xdr_g_string, g_string))
     {
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [create_spool] svc_getargs request failed\n" );
        svcerr_decode(transp);
	return;
     }
     if (!svc_sendreply(transp, xdr_send_gid, g_string))
     {
        (void) fprintf( stdout, "bwnfsd: [create_spool] (9) Can't reply to RPC call\n");
     }
     return;
  case GRP_TO_NUMBER:
     if (!svc_getargs(transp,xdr_ngids, lgids))
     {
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [create_spool] svc_getargs request failed\n" );
        svcerr_decode(transp);
	return;
     }
     if (!svc_sendreply(transp, xdr_send_names, lgids))
     {
        (void) fprintf( stdout, "bwnfsd: [create_spool] (10) Can't reply to RPC call\n");
     }
     return;
  case RETURN_HOST:
     if (!svc_getargs(transp,xdr_int, &r_ip))
     {
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [create_spool] svc_getargs request failed\n" );
        svcerr_decode(transp);
	return;
     }
     if ( r_ip == my_ip)
	p = hostname;
     else
     {
	struct hostent *h;
        incomming_ip = ntohl(r_ip);
	h            = gethostbyaddr((char *) &incomming_ip,4,AF_INET);
	if ( h == NULL)
        {
           (void) sprintf(remote_host,"%u.%u.%u.%u",(incomming_ip ) & 0xff,
		   (incomming_ip >> 8) & 0xff,(incomming_ip >> 16) & 0xff,
                   (incomming_ip >> 24) & 0xff);
	   p = remote_host;
        }
	else
	   p = h->h_name;
     }
     if (debugmode)
       (void) fprintf( stdout, "bwnfsd: [create_spool] remote = %s\n", p );

     if (!svc_sendreply(transp, xdr_g_string, p))
        (void) fprintf( stdout, "bwnfsd: [create_spool] (11) Can't reply to RPC call\n");
     return;
  case UID_TO_NAME:
     if (!svc_getargs(transp,xdr_nuids, luids))
     {
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [create_spool] svc_getargs failed\n", p );
        svcerr_decode(transp);
	return;
     }
     if (!svc_sendreply(transp, xdr_send_usernames, luids))
     {
        (void) fprintf( stdout, "bwnfsd: [create_spool] (12) Can't reply to RPC call\n");
     }
     return;
  case REMOVE:
     if (!svc_getargs(transp,xdr_get_lock_name,lock.name))
     {
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [create_spool] svc_getargs failed\n");
        svcerr_decode(transp);
	return;
     }

     process_lock_request((int) rqstp->rq_proc,lock.name,lock.fh,lock.access,
	 		  lock.mode,lock.offset,lock.length,lock.cookie);
     if (!svc_sendreply(transp,xdr_void,NULL))
     {
        (void) fprintf( stdout, "bwnfsd: [create_spool] (13) Can't reply to RPC call\n");
     }
     return;
  case SHARE:
  case UNSHARE:
     if (!svc_getargs(transp,xdr_share,&lock))
     {
        if (debugmode)
          (void) fprintf( stdout, "bwnfsd: [create_spool] svc_getargs failed\n");
        svcerr_decode(transp);
	return;
     }
     lock.stat = !process_lock_request((int) rqstp->rq_proc,lock.name,lock.fh,
                          lock.access,lock.mode,lock.offset,lock.length,lock.cookie);
     if (debugmode)
     {
	if ( lock.stat )
	  fprintf( stdout, "bwnfsd: [create_spool] lock status = false\n");
	else
	  fprintf( stdout, "bwnfsd: [create_spool] lock status = true\n");
     }
     if (!svc_sendreply(transp,xdr_shareres,&lock))
     {
        (void) fprintf( stdout, "bwnfsd: [create_spool] (14) Can't reply to RPC call\n");
     }
     return;
  case LOCK:
     lock.stat = svc_getargs(transp,xdr_lock,&lock);
     goto check_lock;
  case UNLOCK:
     lock.stat = svc_getargs(transp,xdr_unlock,&lock);
check_lock:
     if (!lock.stat)
     if (!svc_getargs(transp,xdr_lock,&lock))
     {
        svcerr_decode(transp);
	return;
     }
     lock.stat = !process_lock_request((int) rqstp->rq_proc,lock.name,lock.fh,
                          lock.access,lock.mode,lock.offset,lock.length,lock.cookie);
     if (debugmode)
     {
	if ( lock.stat )
	  fprintf( stdout, "bwnfsd: [create_spool/check_lock] lock status = false\n");
	else
	  fprintf( stdout, "bwnfsd: [create_spool/check_lock] lock status = true\n");
     }
     if (!svc_sendreply(transp,xdr_res,&lock))
     {
        (void) fprintf( stdout, "bwnfsd: [create_spool] (15) Can't reply to RPC call\n");
     }
     return;
  default:
     svcerr_noproc(transp);
     return;
  }
}



static struct fhstatus {
     int status;
     char dir[32];
} mount_return;

/*
	Response from rpc.mountd for mount request of SPOOL directory.
*/
xdr_fhstatus(xdrsp, fhstat)
        XDR *xdrsp;
    struct fhstatus *fhstat;
{
   int i;

   if (debugmode)
     (void) fprintf( stdout, "bwnfsd: [xdr_fhstatus] called\n" );
   if (!xdr_int(xdrsp, &fhstat->status)) return(0);
   if (fhstat->status == 0)
   {
      for (i = 0 ; i <32 ; i+=4)
         if (!xdr_int(xdrsp, &(fhstat->dir[0])+i)) return(0);
   }
   if (debugmode > 1)
     (void) fprintf( stdout, "bwnfsd: [xdr_fhstatus] returns (1)\n" );
   return(1);
}


static struct text_string {
  int length;
  char *dir;
} request_text;

/*
	Request to mount SPOOL directory.
*/
xdr_text(xdrsp, req)
        XDR *xdrsp;
    struct text_string *req;
{
   if (debugmode)
     (void) fprintf( stdout, "bwnfsd: [xdr_text] mount request for %s\n", req->dir );
   if (!xdr_bytes(xdrsp, &req->dir, &req->length,255)) return(0);
   if (debugmode > 1)
     (void) fprintf( stdout, "bwnfsd: [xdr_text] returns (1)\n" );
   return(1);
}


/*
	Use rpc.mountd to determine the fhandle of the spool directory,
	Can be replaced with extracted code from rpc.mountd.
*/
int get_mount(dir,directory)
        char *dir,*directory;
{
  struct hostent *hp;
  struct timeval pentry_timeout, total_timeout;
  struct sockaddr_in server_addr;
  int    sock = RPC_ANYSOCK;
  register CLIENT *client;
  enum clnt_stat clnt_stat;

   if (debugmode)
     (void) fprintf( stdout, "bwnfds: [get_mount] called\n" );
     if (debugmode > 1)
       (void) fprintf( stdout, "bwnfsd: [get_mount] dir = %s, directory = %s\n",
                       dir, directory );
  request_text.length = strlen(directory);
  request_text.dir    = directory;
  if ((hp = gethostbyname(hostname)) == NULL)
  {
     (void) fprintf( stdout, "bwnfsd: [get_mount] \42%s\42 is not in the /etc/hosts file.\n", hostname );
     (void) fprintf( stdout, "bwnfsd:             please entered it and re-run.\n" );
     (void) fflush( stdout );
     exit(-1);
   }
  pentry_timeout.tv_sec  = 3;
  pentry_timeout.tv_usec = 0;
  bcopy(hp->h_addr, (caddr_t) &server_addr.sin_addr,hp->h_length);
  server_addr.sin_family = AF_INET;
  server_addr.sin_port   = 0;
  if ((client = clntudp_create( &server_addr, 100005L, 1L,
                                pentry_timeout, &sock)) == NULL)
  {
     if (debugmode)
       (void) fprintf( stdout, "bwnfsd: [get_mount] clntupd_create failed\n" );
     clnt_pcreateerror("clntudp_create");
     (void) fflush( stdout );
     exit(-1);
  }
  if ( bindresvport(sock, (struct sockaddr_in *) NULL))
  {
    if (debugmode)
      fprintf( stdout, "bwnfsd: [get_mount] cannot allocate reserved port ... not critical.\n");
  }
  client->cl_auth       = authunix_create_default();
  total_timeout.tv_sec  = 20;
  total_timeout.tv_usec = 0;
  clnt_stat             = clnt_call( client, 1L, xdr_text, &request_text,
                                     xdr_fhstatus, &mount_return,
                                     total_timeout);
  if ( clnt_stat != RPC_SUCCESS)
  {
    if (debugmode)
      (void) fprintf( stdout, "bwnfsd: [get_mount] clnt_stat failed\n" );
    clnt_perror(client,"rpc");
    (void) fflush( stdout );
    exit(-1);
  }
  auth_destroy( client->cl_auth );
  clnt_destroy( client );
  if ( mount_return.status == 0L )
     bcopy(&mount_return.dir[0],dir,32);
  if (debugmode)
    (void) fprintf( stdout, "bwnfsd: [get_mount] returns mount status %u\n",
                    mount_return.status );
  return( mount_return.status );
}


/*
	Daemon should be killed with a simple KILL and not KILL -9 so
	that it may unmap itself
*/
void die()
{
#ifndef LINUX
  (void) fprintf( stdout, "bwnfsd: [die] Daemon is dying\n\n");
#endif
  if (debugmode)
    (void) fflush( stdout );
  (void) pmap_unset(BW_NFS_PROGRAM, BW_NFS_VERSION);
  exit(1);
}


main(argc,argv)
  int argc;
  char **argv;
{
  SVCXPRT *transp;
  char *p,*prog;
  int	i;
  struct hostent *hp;
#ifdef SCO
  setluid(0);
  set_auth_parameters(1,"/");
#endif

#ifndef LINUX
  (void) fprintf( stdout, "BWNFSD Authentication Daemon Vsn 2.3 starting\n\n" );
#endif
  prog=argv[0];
  debugmode = 0;
  argc--;argv++;
another:
  if ((argc > 0) && ( strcmp(*argv,"-A") == 0))
  {
     argc--; argv++;
     A_flag=1;
     goto another;
  }

  if ((argc > 0) && ( strcmp(*argv,"-d") == 0))
  {
    debugmode++;
    argc--; argv++;
    goto another;
  }
  if ((argc > 0) && ( strcmp(*argv,"-s") == 0))
  {
     argc--; argv++;
     if (argc < 2)
     {
        (void) fprintf( stdout, "usage : %s [-A] [-d] [-s file] spool_area_mount_point\n", prog );
        (void) fflush( stdout );
        exit(1);
     }
     (void) read_ips(*argv);
     s_flag=1;
     argc--; argv++;
     goto another;
  }
  if (argc != 1)
  {
     (void) fprintf(stdout,"usage : %s [-A] [-d] [-s file] spool_area_mount_point\n", prog );
     (void) fflush( stdout );
     exit(1);
  }

  init_locks();
  hostname = (char *) malloc(255);
  (void) gethostname(hostname,255);
  if ((hp = gethostbyname(hostname)) == NULL)
  {
     (void) fprintf( stdout, "bwnfsd: host \42%s\42 is not in the /etc/hosts file\n", hostname );
     (void) fprintf( stdout, "bwnfsd: Please add it to the file and restart %s\n", prog );
     (void) fflush( stdout );
     exit(-1);
   }
  bcopy(hp->h_addr, (caddr_t) &my_ip,hp->h_length);
  if ( get_mount(dir,*argv) !=0 )
  {
     (void) fprintf( stdout, "bwnfsd: Spool area mount point %s could not be mounted.\n",  *argv );
     (void) fprintf( stdout, "bwnfsd: Check that %s is in the /etc/exports file & world-writable\n", *argv );
     (void) fflush( stdout );
     exit(1);
  }
#ifndef DEBUG
  if ( fork() != 0 ) exit(0);
#ifdef SCO
  for ( i = 0; i < 10; i++);
     (void) close(i);
  if ( (  i = open("/dev/null",2) ) < 0) exit(1);
  dup2(i,0);
  dup2(i,1);
  dup2(i,2);
  close(i);
#else
  for ( i = 0; i < 10; i++);
     (void) close(i);
/*
	Disassociate the terminal from the process
*/
  (void) open("/",0);
  (void) dup2(0,1);
  (void) dup2(0,2);
#ifdef TIOCNOTTY
  i = open("/dev/tty",2);
  if ( i >= 0)
  {
     (void) ioctl(i, TIOCNOTTY, 0);
     (void) close(i);
  }
#endif
#endif
#endif

#ifdef LINUX
  (void) setpgrp();
#else
  (void) setpgrp(0,0);
#endif
  p = strcpy(spool_dir,*argv) + strlen(*argv);
  if ( *(p-1) == '/')
    *(--p)='\000';
  get_printers();
  get_groups();
  f.status     = 0;
  f.dir_size   = 32;
  f.dir_handle = dir;

  transp = svcudp_create(RPC_ANYSOCK);
  if ( transp == NULL)
  {
     (void) fprintf( stdout,"bwnfsd: [main] Unable to create an RPC server\n");
     (void) fflush( stdout );
     exit(1);
  }
  (void) pmap_unset(BW_NFS_PROGRAM, BW_NFS_VERSION);
  if (!svc_register(transp,BW_NFS_PROGRAM, BW_NFS_VERSION,
                    create_spool, IPPROTO_UDP))
  {
     (void) fprintf( stdout, "bwnfsd: [main] Can't register BW-NFS service\n");
     (void) fflush( stdout );
     exit(1);
  }
  (void) signal(SIGHUP,die);
  (void) signal(SIGINT,die);
  (void) signal(SIGQUIT,die);
#ifdef SIGABRT
  (void) signal(SIGABRT,die);
#endif
  (void) signal(SIGTERM,die);
#ifdef SIGCHLD
  (void) signal(SIGCHLD,free_child);
#endif
#ifdef SIGCLD
#ifdef SCO
  (void) signal(SIGCLD,SIG_IGN);
#else
  (void) signal(SIGCLD,free_child);
#endif
#endif
        svc_run();
        (void) fprintf( stdout,"bwnfsd: [main] svc_run returned!\n");
        (void) fflush( stdout );
        exit(1);
}

