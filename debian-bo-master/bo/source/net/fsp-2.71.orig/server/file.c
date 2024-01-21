    /*********************************************************************\
    *  Copyright (c) 1991 by Wen-King Su (wen-king@vlsi.cs.caltech.edu)   *
    *                                                                     *
    *  You may copy or modify this file in any manner you wish, provided  *
    *  that this notice is always included, and that you hold the author  *
    *  harmless for any loss or damage resulting from the installation or *
    *  use of this software.                                              *
    \*********************************************************************/

#include "tweak.h"
#include "server_def.h"
#include "s_extern.h"

#ifdef VMS
#define MOREDOTS         /* remove this if you don't want more than 1 dot
                             in a filename */
/* Use UNIX emulation functions */
#define O_CREAT 0x0200
#define fopen vms_fopen
#define open vms_open
#define access vms_access
#define FSP_STAT vms_stat
#define link rename
int dchanged=0;   /* flag which shows if .fsp_content needs to be updated */
int isvar=0;      /* flag: file is in variable record format */
#else
#define FSP_STAT stat
#endif

FPCACHE *cache_p, fpcache[FSP_FILE_CACHE+1]; /* file cache */

extern char *readme_file;

/*****************************************************************************
* This is a modified server_file that hashes file names.		     *
*****************************************************************************/

#define NBSIZE (2*sizeof(UBUF))

#define fexist(A) (!access(A,0))
#define touch(A) close(open(A,O_CREAT,0777))
#define LF_PFX '&'
#define LP_PFX '\\'

static int server_check_pass PROTO2(char *, ownerfile, char *, password)
{
  FILE *fp;
  char fsp_passwd[80];
  
  if(dbug) printf("check_pass: opening file %s\n", ownerfile);
  fp = fopen(ownerfile, "r");
  if(fscanf(fp, "%s", fsp_passwd) != 1) {
    if(dbug) printf("check_pass: no password in ownerfile\n");
    fclose(fp);
    return 0;
  }
  fclose(fp);
  if(dbug)
    printf("Check_pass: read %s, check with %s\n", fsp_passwd,
	   password);
  return strcmp(fsp_passwd, password);
}

/*****************************************************************************
 * Routine to check a path string given by the client.
 * Will replace null string by ".".
 * In case of error, returns the error string.
 *****************************************************************************/

/*****************************************************************************
 *  The PPATH structure is filled in by the function check_path when given a
 *  path string.  The elements are filled in as such:
 * 
 *	fullp      pointer to a string containing the full path name
 *	f_ptr      pointer to begining of the last component of the path
 *       f_len      length of the last component of the path
 *	d_ptr      pointer to begining of the directory part of path
 *       d_len      length of the directory part of the path
 *
 *  fullp is a null-terminated full path string.
 *  f_ptr is always a null-terminated sub-string of fullp.
 *  p_ptr is generally not null-terminated.
 *****************************************************************************/

char *check_path PROTO3(char *, fullp, int, len, PPATH *, pp)
{
  char *s;
  int state;
  
  if(len < 1) return("Path must have non-zero length");
  if(fullp[len-1]) return("Path not null terminated");
  
  pp->d_ptr = "."; pp->d_len = 1;	/* initial dir part ---> root */
  pp->passwd = "\0";			/* default, no password */
  
  if(len == 1 && fullp[0] == 0)	{ /* null path --> root */
    pp->fullp = pp->f_ptr = ".";
    pp->f_len = 1;
    return(NULLP);
  }
  
  for(s = pp->fullp = pp->f_ptr = fullp, state = 0; *s; s++) {
    if(*s == '\n') {
      pp->passwd = s+1; *s = '\0';
      if(dbug) printf("check_path: found password field %s\n", s+1);
    }
    else if(*s <= ' ' || *s >= '~') return("Path contains illegal chars");
      
    switch(*s) {
      case LF_PFX:
      case LP_PFX:
      case '.':
        if(!state) return("Path can't begin with '.'");
#ifdef MOREDOTS
	else *s='$'; /* convert dots to '$' */
#endif
	break;
	  
      case '/': if(!state) return("Path can't contain '//'");
	pp->d_ptr = fullp;
	pp->d_len = s - fullp;
	pp->f_ptr = s+1;
	state = 0;
	break;
	  
      default:
	state = 1;
	break;
    }
  }
  
  pp->f_len = s - pp->f_ptr;
  return(NULLP);
}

/*****************************************************************************
 * Put the directory part of the path pp into dbuf.
 * return pointer to the null character.
 *****************************************************************************/

static char *copy_dir PROTO2(char *, dbuf, PPATH *, pp)
{
  char *p1,*p2;
  int cnt;
  
  for(p1 = dbuf, p2 = pp->d_ptr, cnt = pp->d_len; cnt--; *p1++ = *p2++);
  *p1 = 0; return(p1);
}

/*****************************************************************************
 * Reads directory and write directory listing file.
 *****************************************************************************/

static build_dir_file PROTO1(FILE *, fp)
{
  int nlen, skip, rem;
  DIR *dir_f;
#ifdef HAVE_STRUCT_DIRENT
  struct dirent *dp;
#else
  struct direct *dp;
#endif
  struct stat    sb;
  register char  *s;
  RDIRENT rdb;
  static char marker[] = "******************";
  char longname[UBUF_SPACE];
  FILE *lfp;
#ifdef VMS
  char *ext;         /* pointer to strip .dir extension */
#endif
  
  if(!(dir_f = opendir(".")))  { /* assume I have cd to path already */
    fprintf(stderr,"Can't open dir during initialization\n");
    exit(1);
  }
  
#ifdef VMS
  /* VMS doesn't display the current directory as a dot in the list 
   * so create a dummy dotfile (maybe this should be included in the
   * readdir() emulation function) */
  
  rem = UBUF_SPACE;
  nlen = 2;
  rdb.type = RDTYPE_DIR;
  rdb.size = htonl(1024);
  rdb.time = htonl(time((time_t *)0)); /* get current time for curr. dir */
  fwrite(&rdb,1,RDHSIZE,fp);
  fwrite(".",1,nlen,fp);
  
  rem -= (nlen + RDHSIZE);
  
  if((skip = (nlen + RDHSIZE) & 0x3)) {
    fwrite(&rdb,1,4-skip,fp);
    rem -= (4-skip);
  }
  
  /* now collect the files in the directory */
  for( 1 ; dp = readdir(dir_f); ) {
    ext = dp->d_name;
    while (*ext=tolower(*ext)) ext++; /* kludge to convert filename to
					 lowercase */
    s = dp->d_name;
    if (*(ext = s+strlen(s)-1) == '.') *ext=0; /* strip last dot */
#else
#ifdef HAVE_STRUCT_DIRENT
  for(rem = UBUF_SPACE; dp = (struct dirent *)readdir(dir_f); ) {
#else
  for(rem = UBUF_SPACE; dp = (struct direct *)readdir(dir_f); ) {
#endif
    if (dp->d_ino == 0) continue;
#endif
    s = dp->d_name;
	  
    if(s[0] == LF_PFX)  {
      s[0] = LP_PFX;
      if(!(lfp = fopen(s,"r"))) {
	unlink(s);
	*s = LF_PFX;
	unlink(s);
	continue;
      }
      fgets(longname,UBUF_SPACE,lfp);  fclose(lfp);
      s[0] = LF_PFX; s = longname; longname[UBUF_SPACE-1] = 0;
    }
	  
    if(s[0] == LP_PFX) continue;
	  
    /* hide dot files */
    if((s[0]=='.') && ((s[1]!=0) && (s[1] != '.' || s[2] != 0))) continue;
	  
    if(stat(dp->d_name,&sb)) continue;
	  
#ifdef VMS  /* strip .DIR extension */
    if (sb.st_mode & S_IFDIR) if (ext=strstr(s,".dir")) *ext=0;
# ifdef MOREDOTS /* replace '$' with dots */
    while (s=strstr(s,"$")) *s++='.'; s=dp->d_name;
# endif
#endif /* VMS */
	  
    nlen = strlen(s)+1;
	  
    if(rem < RDHSIZE + nlen) {
      rdb.type = RDTYPE_SKIP;
      if(rem <= RDHSIZE) { fwrite(marker,1,rem    ,fp); }
      else {
	fwrite(marker,1,RDHSIZE,fp);
	fwrite(s, 1,rem-RDHSIZE,fp);
      }
      rem = UBUF_SPACE;
    }
	  
    rdb.type = ((S_IFDIR & sb.st_mode)) ? RDTYPE_DIR : RDTYPE_FILE;
    BB_WRITE4(rdb.bb_size,sb.st_size );
    BB_WRITE4(rdb.bb_time,sb.st_mtime);
	  
    fwrite((char *) &rdb,1,RDHSIZE,fp);
    fwrite(s,1,nlen,fp);
    rem -= (nlen + RDHSIZE);
	  
    if((skip = (nlen + RDHSIZE) & 0x3)) {
      fwrite((char *) &rdb,1,4-skip,fp);
      rem -= (4-skip);
    }
	  
    if(!rem) rem = UBUF_SPACE;
  }
      
  rdb.type = RDTYPE_END;
  fwrite((char *) &rdb,1,RDHSIZE,fp);
    
  fflush(fp);
  closedir(dir_f);
}
  
/***************************************************************************/
  
#ifdef VMS
  /* the problem on VMS-systems is, that the directory-date doesn't change
   * when anything is changed in it, so we need a manual update of the
   * .FSP_CONTENT file after a upload,deletion,etc.
   */
static char *server_update_dir PROTO2(PPATH *, pp, unsigned long, inet_num)
{
  FILE *fp;  /* dummy file, for server_get_dir() */
  char pathname[NBSIZE],*oldpath, *errmsg = "\0";
    
  dchanged=1; /* mark that fsp_content has been changed */
  copy_dir(pathname,pp); /* strip filename from path. */
  oldpath=pp->fullp;pp->fullp=pathname; /* new path, backup old one */
  if(!(errmsg=server_get_dir(pp,inet_num,&fp)))
    fclose(fp); /* update .FSP_CONTENT */
  pp->fullp=oldpath;  /* restore old path */
  return(errmsg);
}
#endif /* VMS */
  
/***************************************************************************/

static char *server_get_dir_2 PROTO3(PPATH *, pp, FILE **, fp,
				     struct stat *, sd)
{
  struct stat sf;
  char name_p[NBSIZE], list_p[NBSIZE], tmp[UBUF_SPACE];
  unsigned int hash, i, mask, new_namefile;
  unsigned char *p;
  FILE *np;
#ifdef VMS
  char pathname[NBSIZE];  /* for conversion of pp->fullp */
#endif
    
  mask = dir_cache_limit << 8;
    
  for(hash = 0, p = (unsigned char *) pp->fullp; *p; p++) {
    hash = (hash << 1) ^ *p;
    if(hash & mask) hash ^= (mask+1);
  }
    
  for(i = 8; i--; ) {
    hash <<= 1;
    if(hash & mask) hash ^= (mask+1);
  }
    
  hash = (hash >> 8) & (mask-1);
    
#ifdef VMS   /* dir_cache_dir is already in VMS format */
  sprintf(name_p,"%s.N%x",dir_cache_dir,hash);
  sprintf(list_p,"%s.L%x",dir_cache_dir,hash);
#else
  sprintf(name_p,"%s/.N%x",dir_cache_dir,hash);
  sprintf(list_p,"%s/.L%x",dir_cache_dir,hash);
#endif
    
  if(!(np = fopen(name_p,"r"))) new_namefile = 1;
  else {
    fgets(tmp,UBUF_SPACE,np); fclose(np);
	
    if(strcmp(tmp,pp->fullp)) new_namefile = 1; 
else {
#ifdef VMS
      if(!FSP_STAT(list_p,&sf) && (!dchanged)) {
	/* VMS doesn't change the modification-date of a directory
	   when the contents has been changed */
#else
      if(!FSP_STAT(list_p,&sf) && (sf.st_mtime >= sd->st_mtime)) {
#endif
	*fp = fopen(list_p,"r");
	if(!*fp) return("Can't read directory listing");
	return((char *)0);
      }
      new_namefile = 0;
    }
  }
    
  if(new_namefile) {
    if(!(np = fopen(name_p,"w")))
      return("Can't write directory listing name file");
    fputs(pp->fullp,np); fclose(np);
  }
#ifdef VMS
  unlink(list_p); /* erase old list first */
#endif
    
  if(!(*fp = fopen(list_p,"w+"))) 
{
return("directory unreadable");
}
  if(chdir(pp->fullp)) return("Can't cd to directory");
  build_dir_file(*fp);
  if(chdir(home_dir) == -1) {
    perror("chdir2");
    exit(1);
  }
#ifdef VMS
  dchanged=0; /* .fsp_content now up to date */
#endif
  return(NULLP);
}

char *server_get_dir PROTO3(PPATH *, pp, unsigned long, inet_num, FILE **, fp)
{
  struct stat sd, sf;
  char   list_p[NBSIZE],owner_p[NBSIZE],private_p[NBSIZE];
  int    fsp_content_exists;
#ifdef VMS
  char dirname[NBSIZE];
    
  /* now the stupid dirname conversion for stat(), I have to look for a
   * unix-like stat() for VMS.
   * I can't make the conversion in stat(), because I don't know
   * when a path is a full-dir or a dir with a filename on the end.
   */
    
  if (*pp->fullp=='.') getcwd(dirname,512,0);
  else strcpy(dirname,pp->fullp);
  strcat(dirname,".dir.1"); /* for here, add .DIR.1 for directory */
  
  if(FSP_STAT(dirname,&sd)) return("Can't find directory");
    
  if(!(S_IFDIR & sd.st_mode)) return("Not a directory");
    
  sprintf(owner_p,"%s/.OWN%08X"  ,pp->fullp,inet_num);
#else
  if(stat(pp->fullp,&sd)) return("Can't find directory");
  if(!(S_IFDIR & sd.st_mode)) return("Not a directory");
  
  sprintf(owner_p,"%s/.OWN.%08X"  ,pp->fullp,inet_num);
#endif
  sprintf(list_p,"%s/.FSP_CONTENT",pp->fullp);
  sprintf(private_p,"%s/.FSP_PRIVATE", pp->fullp);
    
  if(!fexist(owner_p) && fexist(private_p))
    return("This directory is private");
    
  if(fsp_content_exists = !FSP_STAT(list_p,&sf)) {
#ifdef VMS
    if (!dchanged) { /* VMS doesn't change the modification-date of a
		      directory when the contents has been changed */
#else
    if(sf.st_mtime >= sd.st_mtime) {
#endif
      *fp = fopen(list_p,"r");
      if(!*fp) return("Can't read directory listing");
      return((char *)0);
    }
  }
    
  if(!fsp_content_exists && always_use_cache_dir)
    return(server_get_dir_2(pp,fp,&sd));
    
#ifdef VMS
  unlink(list_p); /* remove old .FSP_CONTENT first (otherwise new file) */
#endif
    
  if(!(*fp = fopen(list_p,"w+"))) {
    if(!fsp_content_exists && !always_use_cache_dir)
      return(server_get_dir_2(pp,fp,&sd));
    return("directory unreadable");
  }
  if(chdir(pp->fullp)) return("Can't cd to directory");
    
  build_dir_file(*fp);
  if(chdir(home_dir) == -1) {
    perror("chdir2");
    exit(1);
  }
#ifdef VMS
  dchanged=0;  /* .fsp_content now up to date */
#endif
  return(NULLP);
}

static int fold_path PROTO1(PPATH *, pp)
{
  unsigned char *p;
  unsigned long v1, v2;
    
  p = (unsigned char *) pp->f_ptr;
    
  for(v1 = 0; v2 = *p++; ) {
    if(v1 & 0x80000000) 
      v1 = (v1 << 1) ^ v2 ^ 1;
    else 
      v1 = (v1 << 1) ^ v2;
  }
    
  sprintf(pp->f_ptr,"%c%08X",LF_PFX,v1);
  pp->f_len = 9;
}

/**********************************************************************/
/* assume path is validated */
char *server_del_file PROTO2(PPATH *, pp, unsigned long, inet_num)
{
  int is_long;
  struct stat sb;
  char ok_del_p[NBSIZE], owner_p[NBSIZE];
    
  if(is_long = (pp->f_len > max_nlen)) fold_path(pp);
    
  if(FSP_STAT(pp->fullp,&sb)) return("unlink: file not accessible");
  if(!(S_ISREG(sb.st_mode))) return("unlink: not an ordinary file");
    
#ifdef VMS  /* more than 2 dots not allowed in filenames */
  sprintf(copy_dir( owner_p,pp),"/.OWN%08X"  ,inet_num);
#else
  sprintf(copy_dir( owner_p,pp),"/.OWN.%08X"  ,inet_num);
#endif
  strcpy(copy_dir(ok_del_p,pp),"/.FSP_OK_DEL"         );
    
  if(!fexist(owner_p) && !fexist(ok_del_p))
    return("no permission for removing this file");
    
  if(unlink(pp->fullp) == -1) return("unlink: cannot unlink");
    
  if(is_long) {
    *pp->f_ptr = LP_PFX;
    unlink(pp->fullp);
  }
    
#ifdef VMS
  server_update_dir(pp,inet_num);  /* update .FSP_CONTENT */
#endif
  return(NULLP);
}
  
/**********************************************************************/
  
char *server_del_dir PROTO2(PPATH *, pp, unsigned long, inet_num)
{
  struct stat sb;
  char list_p[NBSIZE], ok_del_p[NBSIZE], ok_add_p[NBSIZE], owner_p[NBSIZE];
  char private_p[NBSIZE], ok_mkdir_p[NBSIZE];
  int has_ok_del_p, has_ok_add_p, has_private_p, has_mkdir_p;
#ifdef VMS
  char dirname[NBSIZE];
    
  /* Again the kludge for stat() */
  strcpy(dirname,pp->fullp);
  strcat(dirname,".dir.1"); /* for here, add .DIR.1 for directory */
    
  if(FSP_STAT(dirname,&sb)) return("rmdir: directory not accessible");
  if(!(S_ISDIR(sb.st_mode))) return("rmdir: not an ordinary directory");
    
  /* more than 2 dots not allowed in filenames */
  sprintf( owner_p,"%s/.OWN%08X", pp->fullp,inet_num);
    
#else
  if(FSP_STAT(pp->fullp,&sb)) return("rmdir: directory not accessible");
  if(!(S_ISDIR(sb.st_mode))) return("rmdir: not an ordinary directory");
    
  sprintf( owner_p,"%s/.OWN.%08X", pp->fullp,inet_num);
#endif
    
  sprintf(list_p,"%s/.FSP_CONTENT" ,pp->fullp);
  sprintf(ok_del_p,"%s/.FSP_OK_DEL"  ,pp->fullp);
  sprintf(ok_add_p,"%s/.FSP_OK_ADD"  ,pp->fullp);
  sprintf(private_p,"%s/.FSP_PRIVATE" ,pp->fullp);
  sprintf(ok_mkdir_p,"%s/.FSP_OK_MKDIR",pp->fullp         );
    
    
  if(!fexist(owner_p)) return("no permission for removing this directory");
    
  unlink(owner_p); unlink(list_p);
  has_ok_del_p= !unlink(ok_del_p);
  has_ok_add_p = !unlink(ok_add_p);
  has_private_p = !unlink(private_p);
  has_mkdir_p = !unlink(ok_mkdir_p);
    
#ifdef VMS
  if(rmdir(dirname) != 0) {
#else
  if(rmdir(pp->fullp) != 0) {
#endif
    if(has_ok_del_p) touch(ok_del_p);
    if(has_ok_add_p) touch(ok_add_p);
    if(has_private_p) touch(private_p);
    if(has_mkdir_p) touch(ok_mkdir_p);
    if((int) owner_p) touch(owner_p);
    return("rmdir: cannot unlink");
  }
    
#ifdef VMS
  server_update_dir(pp,inet_num); /* update .FSP_CONTENT */
#endif
  return(NULLP);
}
  
/**********************************************************************/
  
char *server_make_dir PROTO2(PPATH *, pp, unsigned long, inet_num)
{
  char ok_add_p[NBSIZE], owner_p[NBSIZE], private_p[NBSIZE];
    
#ifdef VMS /* more than 2 dots not allowed in filenames */
  sprintf(copy_dir( owner_p,pp),"/.OWN%08X"  ,inet_num);
#else
  sprintf(copy_dir( owner_p,pp),"/.OWN.%08X"  ,inet_num);
#endif
  strcpy(copy_dir(ok_add_p,pp),"/.FSP_OK_MKDIR");
    
  if(!fexist(owner_p) && !fexist(ok_add_p))
    return("no permission for directory creation");
    
  /* make directory and place ownerfile in it */
    
#ifdef VMS
  sprintf(owner_p,  "%s/.OWN%08X",pp->fullp,inet_num);
  if(mkdir(pp->fullp,0) != 0) return("Can't create directory");
  server_update_dir(pp,inet_num); /* update .FSP_CONTENT */
#else
  sprintf(owner_p,"%s/.OWN.%08X",pp->fullp,inet_num);
  if(mkdir(pp->fullp,0777) != 0) return("Can't create directory");
#endif
  strcpy(copy_dir(private_p,pp),"/.FSP_PRIVATE");
  if (fexist(private_p)) {  /* subdir in a private dir is also private */
    sprintf(private_p,"%s/.FSP_PRIVATE",pp->fullp);
    touch(private_p);
  }
  touch(owner_p);
  return(NULLP);
}
  
/**********************************************************************/
  
char *server_get_file PROTO4(PPATH *, pp, FILE **, fp,
			     unsigned long, inet_num,
			     unsigned short, port_num)
{
  struct stat sb;
  char   private_p[NBSIZE], owner_p[NBSIZE];
  FPCACHE *cache_f;
#ifdef VMS
# ifdef MOREDOTS
  char   *dot;
# endif
#endif
    
  if(pp->f_len > max_nlen) fold_path(pp);
    
#ifdef MOREDOTS
  if(FSP_STAT(pp->fullp,&sb)) {
    /* file not found, try without $-conversion (for linked directories) */
    if (dot=strstr(pp->fullp,"$")) *dot++='.'; /* conv. $ back to dot */
    if (FSP_STAT(pp->fullp,&sb)) return("Can't find file");
  }
#else
  if (FSP_STAT(pp->fullp,&sb)) return("Can't find file");
#endif
    
  if(!(S_ISREG(sb.st_mode))) return("Not a file");
    
#ifdef VMS  /* more than one dot not allowed in filenames */
  isvar = isvariable(sb); /* return file format type*/
#endif
  if(!(cache_f = find_cache(fpcache, port_num, inet_num, pp->fullp))) {
    /* file not found in cache? */
    /* clear current cache-entry first */
    if(dbug) printf("get_file: going to delete cache_p: %d\n", cache_p);
    delete_cache(cache_p);
#ifdef VMS
    sprintf(copy_dir(  owner_p,pp),"/.OWN%08X", inet_num);
#else
    sprintf(copy_dir(  owner_p,pp),"/.OWN.%08X", inet_num);
#endif
    strcpy(copy_dir(private_p,pp),"/.FSP_PRIVATE"       );
	
    if(!fexist(owner_p) && fexist(private_p))
      return("no permission for reading this file");
	
    if(fexist(private_p)) {
      /* if(!pp->passwd[0]) return("You need an FSP_PASSWORD"); */
      if(server_check_pass(owner_p, pp->passwd))
	return("Invalid FSP_PASSWORD");
    }
	
    /* open new file */
    if(!(*fp = fopen(pp->fullp,"r"))) return("Can't open file");
	
    /* add it to the file-cache */
    cache_p = add_cache(fpcache,cache_p,port_num,inet_num,pp->fullp,*fp);
  } else *fp = cache_f->fp; /* get filepoint from cache */
    
  return(NULLP);
}
  
/**********************************************************************/
/* result and pp->fullp may overlap */
char *server_get_pro PROTO3(PPATH *, pp, char *, result,
			    unsigned long, inet_num)
{
  struct stat sb;
  FILE *readme;
  char buf[NBSIZE], proflags=0;
  char buf1[UBUF_SPACE];
  unsigned len, num;
#ifdef VMS
  char dirname[NBSIZE];
    
  /* And again the kludge with the dirname conversion for stat() */
  if (*pp->fullp=='.') getcwd(dirname,512,0);
  else strcpy(dirname,pp->fullp);
  strcat(dirname,".dir.1"); /* for here, add .DIR.1 for directory */
    
  if(FSP_STAT(dirname,&sb)) return("getpro: directory not accessible");
  if(!(S_ISDIR(sb.st_mode))) return("getpro: not an ordinary directory");
    
  sprintf(buf,"%s/.OWN%08X",pp->fullp,inet_num);
#else
  if(FSP_STAT(pp->fullp,&sb)) return("getpro: directory not accessible");
  if(!(S_ISDIR(sb.st_mode))) return("getpro: not an ordinary directory");
    
  sprintf(buf,"%s/.OWN.%08X",pp->fullp,inet_num);
#endif
    
  if(fexist(buf)) proflags |= DIR_OWNER;
  sprintf(buf,"%s/.FSP_OK_DEL"   ,pp->fullp);
  if(fexist(buf)) proflags |= DIR_DEL;
  sprintf(buf,"%s/.FSP_OK_ADD"   ,pp->fullp);
  if(fexist(buf)) proflags |= DIR_ADD;
  sprintf(buf,"%s/.FSP_PRIVATE"  ,pp->fullp);
  if(fexist(buf)) proflags |= DIR_PRIV;
  sprintf(buf,"%s/.FSP_OK_MKDIR" ,pp->fullp);
  if(fexist(buf)) proflags |= DIR_MKDIR;
    
  sprintf(buf,"%s/%s", pp->fullp, readme_file);
    
  sprintf(result, "\n");
    
  len = strlen(result);
  /* the README file is opened with fopen() -- probably not necessary */
  if((readme = fopen(buf,"r")) != 0) {
    proflags |= DIR_README;
    /* Note, this number might have to be hand tweaked later. */
    num = fread(buf1, sizeof(char), UBUF_SPACE-len, readme);
    buf1[num] = '\0';
    fclose(readme);
    strcat(result, buf1);
  }
  result[strlen(result)] = '\0';
  result[strlen(result)+1] = proflags;
  return(NULLP);
}
  
/**********************************************************************/

char *server_set_pro PROTO3(PPATH *, pp, char *, key, unsigned long, inet_num)
{
  struct stat sb;
  char buf[NBSIZE];
#ifdef VMS
  char dirname[NBSIZE];
    
  /* And again the kludge with the dirname conversion for stat() */
  if (*pp->fullp=='.') getcwd(dirname,512,0);
  else strcpy(dirname,pp->fullp);
  strcat(dirname,".dir.1"); /* for here, add .DIR.1 for directory */
    
  if(FSP_STAT(dirname,&sb)) return("getpro: directory not accessible");
  if(!(S_ISDIR(sb.st_mode))) return("getpro: not an ordinary directory");
    
  sprintf(buf,"%s/.OWN%08X",pp->fullp,inet_num);
#else
  if(FSP_STAT(pp->fullp,&sb)) return("getpro: directory not accessible");
  if(!(S_ISDIR(sb.st_mode))) return("getpro: not an ordinary directory");
    
  sprintf(buf,"%s/.OWN.%08X",pp->fullp,inet_num);
#endif
  if(!fexist(buf)) return("no permission for changing the protection-mode");
    
  switch(key[1]) {
    case 'c':
      sprintf(buf,"%s/.FSP_OK_ADD"  ,pp->fullp);
      break;
    case 'd':
      sprintf(buf,"%s/.FSP_OK_DEL"  ,pp->fullp);
      break;
    case 'p':
      sprintf(buf,"%s/.FSP_PRIVATE" ,pp->fullp);
      break;
    case 'm':
      sprintf(buf,"%s/.FSP_OK_MKDIR",pp->fullp);
      break;
    default:
      return("bad flag");
  }
    
  switch(key[0]) {
    case '+':
      touch(buf);
      break;
    case '-':
      unlink(buf);
      break;
    default:
      return("bad flag");
    }
    
  return(NULLP);
}
  
/**********************************************************************
 *  These two are used for file uploading.
 **********************************************************************/
  
char *server_up_load PROTO5(char *, data, int, len, unsigned long, pos,
			    unsigned long, inet_num, unsigned short, port_num)
{
  FILE *fp;
  char  tname[NBSIZE];
  FPCACHE *cache_f;

#ifdef VMS  /* dir_cache_dir is already in VMS format */
  if(always_use_cache_dir)
    sprintf(tname,"%sT%08X%04X",dir_cache_dir,inet_num,port_num);
  else
    sprintf(tname,"T%08X%04X", inet_num, port_num);
#else
  if(always_use_cache_dir)
    sprintf(tname,"%s/.T%08X%04X",dir_cache_dir,inet_num,port_num);
  else
    sprintf(tname, ".T%08X%04X", inet_num, port_num);
#endif
    
  if(!(cache_f = find_cache(fpcache, port_num, inet_num, tname))) {
    /* file not found in cache? */
    /* clear out current cache-entry first */
    delete_cache(cache_p);
    if (pos) {
      if(fp = fopen(tname, "r+")) fseek(fp, pos, 0);
    } else {
      unlink(tname);
      fp = fopen(tname,"w");
    }
	
    if(!fp) return("Cannot open temporary file");
	
    fwrite(data, 1, len, fp);
    /* add it to the file cache */
    cache_p = add_cache(fpcache,cache_p,port_num,inet_num,tname,fp);
  } else { /* file found in cache, using cache-fp */
    fp = cache_f->fp;
    if(pos!=ftell(fp)) fseek(fp, pos, 0);
    fwrite(data, 1, len, fp);
  }
  return(NULLP);
}
  
char *server_install PROTO3(PPATH *, pp, unsigned long, inet_num,
			    unsigned short, port_num)
{
  FILE *ft, *fp;
  char tname[NBSIZE],  owner_p[NBSIZE], save_p[NBSIZE], buf[512];
  char ok_del_p[NBSIZE], ok_add_p[NBSIZE];
  int bytes, is_long;
  FPCACHE *cache_f;
#ifdef VMS
  char filename[NBSIZE];
# ifdef MOREDOTS
  char *dot; /* pointer to a dot in a filename */
# endif
#endif
    
  if(is_long = (pp->f_len > max_nlen)) {
    strcpy(save_p,pp->f_ptr);
    fold_path(pp);
  }
    
#ifdef VMS
  sprintf(copy_dir(owner_p,pp),"/.OWN%08X",inet_num);
  sprintf(tname,"%sT%08X%04X",dir_cache_dir,inet_num,port_num);
#else
  sprintf(copy_dir(owner_p,pp),"/.OWN.%08X",inet_num);
  sprintf(tname,"%s/.T%08X%04X",dir_cache_dir,inet_num,port_num);
#endif
  /* if file still in cache, then close it & remove it from cache */
  if(cache_f = find_cache(fpcache,port_num, inet_num, tname))
    delete_cache(cache_f);
    
#ifdef MOREDOTS
  while (dot=strstr(pp->fullp,".")) *dot++='$'; /* replace dots with '$' */
#endif
  if (dbug)
    printf("server_install: owner_p: %s, tname: %s, pp->fullp: %s\n", owner_p,
	   tname, pp->fullp);
  if(!fexist(owner_p)) {
    strcpy(copy_dir(ok_add_p,pp),"/.FSP_OK_ADD");
    strcpy(copy_dir(ok_del_p,pp),"/.FSP_OK_DEL");
	
    if(!fexist(ok_add_p)) {
      unlink(tname);
      return("no permission for creating that file");
    }
	
    if(!fexist(ok_del_p) && fexist(pp->fullp)) {
      unlink(tname);
      return("no permission for replacing that file");
    }
  }
    
  unlink(pp->fullp);
  if(link(tname,pp->fullp) == 0) {
    unlink(tname);
    goto done;
  }
    
  /* link failed, so just copy the temp file */
  if(!(ft = fopen(tname,"r"))) {
    unlink(tname);
    return("Can't open temporary file");
  }
    
  if(!(fp = fopen(pp->fullp,"w"))) {
    unlink(tname);
    fclose(ft);
    return("Can't open file for output");
  }
  /* copy temporary file to actual fput file */
  while(bytes = fread(buf,1,sizeof(buf),ft)) fwrite(buf,1,bytes,fp);
    
  fclose(ft); fclose(fp); unlink(tname);
    
 done:
    
  if(is_long) {
    *pp->f_ptr = LP_PFX;
    if(!(fp = fopen(pp->fullp,"w"))) {
      *pp->f_ptr = LF_PFX;
      unlink(tname);
      return("Can't create long named file for output");
    }
    fputs(save_p,fp); fclose(fp);
  }
    
#ifdef VMS
  server_update_dir(pp,inet_num); /* update .FSP_CONTENT */
#endif
  return(NULLP);
}
  
/**********************************************************************/
/* assume path is validated */
char *server_secure_file PROTO3(PPATH *, pp, unsigned long, inet_num,
				unsigned short, port_num)
{
  int is_long;
  struct stat sb;
  char ok_del_p[NBSIZE], owner_p[NBSIZE], temp_p[NBSIZE];
  char private_p[NBSIZE];
    
  if(is_long = (pp->f_len > max_nlen)) fold_path(pp);
  if(FSP_STAT(pp->fullp,&sb)) return("grab: file not accessible");
  if(!(S_ISREG(sb.st_mode))) return("grab: not an ordinary file");
    
#ifdef VMS /* more than one dot not allowed */
  sprintf(copy_dir( owner_p,pp),"/.OWN%08X", inet_num);
#else
  sprintf(copy_dir( owner_p,pp),"/.OWN.%08X", inet_num);
#endif
  strcpy(copy_dir( ok_del_p,pp),"/.FSP_OK_DEL");
  strcpy(copy_dir(private_p,pp),"/.FSP_PRIVATE");
  sprintf(copy_dir(   temp_p,pp),"/.G%08X%04X", inet_num,port_num);
    
  if(!fexist(owner_p) && (!fexist(ok_del_p) || fexist(private_p)))
    return("grab: no permission for grabbing this file");
    
  if (fexist(private_p)) {
    /* if (!pp->passwd[0]) return("You need an FSP_PASSWORD"); */
    if (server_check_pass(owner_p,pp->passwd))
      return("Invalid FSP_PASSWORD");
  }
    
  unlink(temp_p);
  /* link on VMS emulated as a filecopy */
  if(link(pp->fullp,temp_p) == -1) return("grab: cannot make link");
  if(unlink(pp->fullp) == -1) {
    unlink(temp_p);
    return("grab: cannot unlink");
  }
    
  if(is_long) {
    *pp->f_ptr = LP_PFX;
    unlink(pp->fullp);
  }
    
  return(NULLP);
}
  
char *server_grab_file PROTO4(PPATH *, pp, FILE **, fp,
			      unsigned long, inet_num,
			      unsigned short, port_num)
{
  struct stat sb;
  char temp_p[NBSIZE];
    
  sprintf(copy_dir(temp_p,pp),"/.G%08X%04X",inet_num,port_num);
    
  if(FSP_STAT(temp_p,&sb)) return("grab: can't find file");
  if(!(S_ISREG(sb.st_mode))) return("grab: Not a file");
  if(!(*fp = fopen(temp_p,"r"))) return("grab: can't open file");
  return(NULLP);
}
  
char *server_grab_done PROTO3(PPATH *, pp, unsigned long, inet_num,
			      unsigned short, port_num)
{
  struct stat sb;
  char temp_p[NBSIZE];
    
  sprintf(copy_dir(temp_p,pp),"/.G%08X%04X",inet_num,port_num);
  if(FSP_STAT(temp_p,&sb)) return("grab: can't find temporary file");
  if(unlink(temp_p) == -1) return("grab: can't delete temporary file");
#ifdef VMS
  server_update_dir(pp,inet_num); /* update .FSP_CONTENT */
#endif
  return(NULLP);
}
  
/**********************************************************************/
  
/**********************************************************************/
  
void init_home_dir PROTO0((void))
{
#ifndef VMS
  if(*home_dir != '/') { 
    fprintf(stderr,"[%s] does not start with a /\n", home_dir);
    exit(1); 
  }
#endif
/* test and goto home dir */
  if(chdir(home_dir) == -1) {
    perror(home_dir);
    exit(1);
  }

/* test cache dir */
  if(always_use_cache_dir && (chdir(dir_cache_dir) == -1)) {
    perror(dir_cache_dir);
    exit(1);
  }

/* go back to home_dir */
  if(chdir(home_dir) == -1) {
    perror(home_dir);
    exit(1);
  }

  if(dbug) {
    fprintf(stderr,"home on %s\n",home_dir);
    if(always_use_cache_dir)
      fprintf(stderr,"cache on %s\n", dir_cache_dir);
    fflush(stderr);
  }
}
