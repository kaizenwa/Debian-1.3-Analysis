/* Some unix emulation procedures */
/* 1-NOV-1990 GJC@MITECH.COM */
/* 24-DEC-1992 modified for use with VMS-fsp, <S.A.Pechler@bdk.tue.nl> */
/* 05-MAR-1993 added some CPU-friendly fsp-patches */

#include <stdio.h>
#include <string.h>
#include <descrip.h>
#include <rms.h>
#include <ssdef.h>
#include <stat.h>

#include "pwd.h"

#define fexist(A) (!vms_access(A,0))

/* Ignore these functions on VMS.
 */
char *getlogin(void)
{
 return (char *)0;
}

struct passwd *getpwnam(char *dummy)
{
 return (struct passwd *)0;
}

struct passwd *getpwuid(uid_t dummy)
{ 
  return (struct passwd *)0;
}

/* strcasecmp */
int strcasecmp(char *name1, char *name2)
{
 return(strcmp(name1,name2));
}

/* vms_access
 * Just a simple patch for VMS-fsp, it checks for the existance of a
 * file, regardless of the 'dummy' parameter (was originally
 * protection-flag).
 */
int vms_access(file,dummy)
char *file;
int dummy;
{
 struct FAB fab;
 char filename[255];

 if (convfile(filename,file)) return 1;
 fab=cc$rms_fab;
 fab.fab$l_fna=filename;
 fab.fab$b_fns=strlen(filename);
 if (sys$open(&fab) != RMS$_NORMAL) return 1;
 else sys$close(&fab);
 return 0;
}

/* vms_stat
 * Just a simple patch, returns only filetype DIR or REG in struct stat. 
 */
int vms_stat(file,vstat)
char *file;
struct stat *vstat;
{
 struct FAB fab;
 struct NAM nam;
 char exp_str[NAM$C_MAXRSS];
 char res_str[NAM$C_MAXRSS];

 char filename[255];

 /* convert the filename to VMS-format */
 if (convfile(filename,file)) return 1;

 /* fill in nam-block */
 nam= cc$rms_nam;
 nam.nam$l_rsa= res_str;
 nam.nam$b_rss= NAM$C_MAXRSS;
 nam.nam$l_esa= exp_str;
 nam.nam$b_ess= NAM$C_MAXRSS;

 /* fill in fab-block */
 fab= cc$rms_fab;
 fab.fab$l_nam= &nam;
 fab.fab$l_fop= fab.fab$v_nam;
 fab.fab$l_fna= filename;
 fab.fab$b_fns= (unsigned char)strlen(filename);

 sys$parse(&fab); /* needed for completion nam-block */
 if (sys$search(&fab) != RMS$_NORMAL)
 {
  if (filename[strlen(filename)-1]==']')  /* kludge to check for dir-path */
  { vstat->st_mode=S_IFDIR;
    return 0;
  }
  else return 1;
 }

 /* if name ends on .DIR;1 then it's a dir (Will go wrong when you have
    files with a .DIR;1 extension). */
 if (strncmp(".DIR;1",nam.nam$l_type,6)) vstat->st_mode=S_IFREG;
 else vstat->st_mode=S_IFDIR;
 return 0;
}

/* isvariable
 * returns true if file is in variable record length format.
 */
int isvariable(sb)
struct stat sb;
{
return (int)(sb.st_fab_rfm==FAB$C_VAR || sb.st_fab_rfm==FAB$C_VFC);
}

#ifndef bcopy /* only for UCX systems */
bcopy(x,y,n)
     char *x,*y;
     long n;
{memmove(y,x,n);}	/* reverse order of arguments */
#endif

static char *getwd_tmp = NULL;

char *getwd(p)
     char *p;
{
 int c;
 char *root_dir,*l2;
 getcwd(p,512,0);	/* get current working directory in unix format*/

 root_dir = strstr ( p, "/000000" );
 if ( root_dir != NULL ) {
    /* trim root directory out of specification */
    if ( (strlen(root_dir) == 7) && 
	 (strpbrk(p+1,"/") == root_dir) ) *root_dir = '\0';
 }
 /* special kludge for "/" directory */
 if ( strcmp ( p, "/DEVICE_LIST_ROOT" ) == 0 ) strcpy  ( p, "/" );
 return(p);
}

/*
** HARDLINK emulation, just a filecopy on VMS.
*/
#ifndef link
long
link(source,target)
 char *source;
 char *target;

{
 FILE *ft, *fp;
 char buf[512];
 int bytes;

 if (fexist(target)) return(-1); /* target already exists */
#ifdef DEBUG
 printf("link: source: %s, target: %s\n",source,target);
#endif
 if(!(ft = fopen(source,"r","mbf=2","mbc=32"))) return(-1);
 if(!(fp = fopen(target,"w","mbf=2","mbc=32"))) { fclose(ft); return(-1); }
 /* copy source to target */
 while(bytes = fread(buf,1,sizeof(buf),ft)) fwrite(buf,1,bytes,fp);
 fclose(ft); fclose(fp);
 return(0);
}
#endif

rindex(p,c)
     char *p;
     int c;
{
 return(strrchr(p,c));
}

/*
 * redefinition of lstat
 */
int lstat(f,st)		/* fake a stat operation to return file type */
   char *f;
   stat_t *st;
{
    char *dirext, *name;
    int extlen;

    st->st_mode = S_IFREG;	/* default to normal file */
    name = strrchr ( f, '/' );	/* locate rightmost slash */
    if ( name == NULL ) name = f; else name++;

    dirext = strstr ( name, ".DIR" );
    if ( dirext != NULL ) {
	/* make it an exact match */
	extlen = strcspn(&dirext[1],".;");
        if ( (extlen == 0) || (extlen == 3) ) {
	    st->st_mode = S_IFDIR;
	    if ( strncmp ( name, "000000.", 7 ) == 0 ) return 0;
	    else return (stat ( f, st ));
	}
    }
    return 0;
}

do_vms_wildcard(pargc,pargv)
     int *pargc;
     char ***pargv;
{int j,vsize;
 int argc; char **argv;
 argc = *pargc;
 argv = *pargv;
 *pargc = 0;
 vsize = 3;
 *pargv = (char **) malloc(sizeof (char *) * vsize);
 for(j=0;j<argc;++j)
   vms_wild_putargs(argv[j],pargc,pargv,&vsize);
}

vms_wild_putargs(s,pargc,pargv,pvsize)
     char *s; int *pargc; char ***pargv; int *pvsize;
{if ( (!strchr(s,'*')) && (!strchr(s,'%')) )
   vms_wild_put_one(s,pargc,pargv,pvsize);
 else
   vms_wild_put_wild(s,pargc,pargv,pvsize);
}


vms_wild_put_one(s,pargc,pargv,pvsize)
     char *s; int *pargc; char ***pargv; int *pvsize;
{int nvsize,i;
 char ** nargv, *uname, *SHELL$TRANSLATE_VMS();
 if (*pargc == *pvsize)
   {nvsize = 2 * *pvsize;
    nargv = (char **) malloc(sizeof (char *) * nvsize);
    for(i=0;i < *pargc; ++i) nargv[i] = (*pargv)[i];
    free(*pargv);
    *pargv = nargv;
    *pvsize = nvsize;}
 if ( uname = SHELL$TRANSLATE_VMS ( s ) ) {
    /* printf("vms: '%s' -> unix: '%s'\n", s, uname ); */
    if ( strlen(s) >= strlen(uname) ) { strcpy(s,uname); free(uname); }
    else s = uname;  /* will lose s's old allocation */
 } 
 (*pargv)[(*pargc)++] = s;
}


set_dsc(x,buff,len)
 struct dsc$descriptor *x;
 char *buff;
 int len;
{
 (*x).dsc$w_length = len;
 (*x).dsc$a_pointer = buff;
 (*x).dsc$b_class = DSC$K_CLASS_S;
 (*x).dsc$b_dtype = DSC$K_DTYPE_T;
 return(x);
}

struct dsc$descriptor *
set_dsc_cst(x,buff)
 struct dsc$descriptor *x;
 char *buff;
{
 return(set_dsc(x,buff,strlen(buff)));
}


vms_wild_put_wild(s,pargc,pargv,pvsize)
 char *s; int *pargc; char ***pargv; int *pvsize;
{
 struct dsc$descriptor fnamed,foutd,rfnamed;
 char *ns,*p;
 int rval;
 long context;

 set_dsc_cst(&rfnamed,";");
 set_dsc_cst(&fnamed,s);
 set_dsc(&foutd,0,0);
 foutd.dsc$b_class = DSC$K_CLASS_D;
 context = 0;
 while(1)
  {rval = lib$find_file(&fnamed,&foutd,&context,0,&rfnamed,0,0);
   if (rval == RMS$_NMF) break;
   if (rval == RMS$_FNF) break;
   if (rval != RMS$_NORMAL) exit(rval);
   ns = (char *) malloc(foutd.dsc$w_length + 1);
   ns[foutd.dsc$w_length] = 0;
   memcpy(ns,foutd.dsc$a_pointer,foutd.dsc$w_length);
   /*if (p = strchr(ns,']')) ns = p+1;*/
   /* if (p = strchr(ns,';')) *p = 0; */
   vms_wild_put_one(ns,pargc,pargv,pvsize);
  }
 if (foutd.dsc$a_pointer) lib$sfree1_dd(&foutd);
 if (context)
 { rval = lib$find_file_end(&context);
   if (rval != SS$_NORMAL) exit(rval);
 }
}

