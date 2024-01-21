 /*********************************************************************\
 *  Copyright (c) 1991 by Wen-King Su (wen-king@vlsi.cs.caltech.edu)   *
 *                                                                     *
 *  You may copy or modify this file in any manner you wish, provided  *
 *  that this notice is always included, and that you hold the author  *
 *  harmless for any loss or damage resulting from the installation or *
 *  use of this software.                                              *
 \*********************************************************************/

#include "tweak.h"
#include "client_def.h"
#include "c_extern.h"

#ifndef VMS
extern char *realloc(), *malloc(), *getenv();
#endif
extern long atol();
extern int errno;

static int env_dir_malloced = 0;
char *env_dir = "/";
char *env_passwd = "\0";
char *env_myport;
char *env_host;
char *env_port;
char *env_local_dir;
int env_timeout;
unsigned short client_buf_len;
unsigned short client_net_len;

char *util_abs_path PROTO1(char *, s2)
{
  char *path, *s, *d, *t;
  
  if(!env_dir) env_dir = "";
  if(!s2) s2 = "";
  
  if(*s2 == '/') {
    path = malloc(strlen(s2)+2+strlen(env_passwd)+1);
    sprintf(path,"/%s",s2);
  } else {
    path = malloc(strlen(env_dir)+strlen(s2)+3+strlen(env_passwd)+1);
    sprintf(path,"/%s/%s",env_dir,s2);
  }
  
  for(t = path; *t; ) {
    if(t[0] == '/') {
      while(t[1] == '/') for(d = t, s = t+1; *d++ = *s++; );
      if(t != path && t[1] == 0) { t[0] = 0; return(path); }
    }
    if(t[0] == '.' && t[1] == '.') {
      if(t-1 == path && t[2] ==  0 ) {
	*t = 0;
	return(path);
      }
      if(t-1 == path && t[2] == '/') {
	for(d = t, s = t + 3; *d++ = *s++; );
	continue;
      }
      if(t[-1] == '/' && (t[2] == '/' || t[2] ==  0)) {
	s = t + 2; /* point to either slash or nul */
	t -= 2;	/* guaranteed that t >= path here */
	while(t > path && t[0] != '/') t--;
	if(t != path || *s == '/') for(d = t; *d++ = *s++; );
	else {
	  t[1] = 0;
	  return(path);
	}
	continue;
      }
    }
    if(t[0] == '.') {
      if(t-1 == path && t[1] ==  0 ) {
	*t = 0;
	return(path);
      }
      if(t-1 == path && t[1] == '/') {
	for(d = t, s = t + 2; *d++ = *s++; );
	continue;
      }
      if(t[-1] == '/' && (t[1] == '/' || t[1] ==  0)) {
	s = t + 1; /* point to either slash or nul */
	for(d = t-1; *d++ = *s++; ); 
	t--;
	continue;
      }
    }
    t++;
  }
  return(path);
}

char *util_getwd PROTO1(char *, p)
{
  if(p) strcpy(p,env_dir);
  return(p);
}

static RDIRENT **get_dir_blk PROTO1(char *, path)
{
  RDIRENT **dp;
  char *p1, *p2, *fpath, buf[2*UBUF_SPACE];
  unsigned long pos;
  int cnt, k, len, rem, acc, at_eof, rlen;
  UBUF *ub;
  
  fpath = util_abs_path(path);
  
  for(pos = 0, at_eof = acc = cnt = 0; ; ) {
    while((acc < UBUF_SPACE) && !at_eof) {
      ub = client_interact(CC_GET_DIR,pos, strlen(fpath),
			   (unsigned char *)fpath+1, 2,
			   (unsigned char *)&client_net_len);
	  
      if(ub->cmd == CC_ERR) {
	fprintf(stderr,"%s: %s\n",path, ub->buf);
	free(fpath);
	errno = EACCES;
	return((RDIRENT **) 0);
      }

      rlen = BB_READ2(ub->bb_len);
      if(rlen < client_buf_len) at_eof = 1;
      for(p1 = ub->buf, p2 = buf + acc, k = rlen; k--; ) *p2++ = *p1++;
      acc += rlen;
      pos += rlen;
    }
      
    if(acc >= UBUF_SPACE) len = UBUF_SPACE;
    else len = acc;
      
    for(p2 = buf, rem = len, k = 0; ; k++) {
      if(rem < RDHSIZE) break;
      if(((RDIRENT *) p2)->type == RDTYPE_SKIP) break;
      if(((RDIRENT *) p2)->type == RDTYPE_END ) { k++; break; }
      p2 += RDHSIZE; rem -= (RDHSIZE+1);
      while(*p2++) rem--;
      while((p2 - buf) & 3) {
	p2++;
	rem--;
      }
    }
      
    p1 = malloc(p2-buf);
    if(cnt) dp = (RDIRENT **) realloc(dp,(cnt+k+1)*sizeof(RDIRENT *));
    else dp = (RDIRENT **)  malloc((cnt+k+1)*sizeof(RDIRENT *));
      
    if(!p1 || !dp) {
      free(fpath);
      fputs("directory reading out of memory\n",stderr);
      return((RDIRENT **) 0);
    }
      
    for(p2 = buf, rem = len; ; cnt++) {
      if(rem < RDHSIZE) break;
      if(((RDIRENT *) p2)->type == RDTYPE_SKIP) break;
      if(((RDIRENT *) p2)->type == RDTYPE_END) {
	dp[cnt] = 0;
	return(dp);
      }
      dp[cnt] = (RDIRENT *) p1;
	  
      for(k = RDHSIZE, rem -= (RDHSIZE+1); k--; *p1++ = *p2++);
      while(*p1++ = *p2++) rem--;
      while((p2 - buf) & 3) {
	p2++;
	p1++;
	rem--;
      }
    }
      
    if(acc < UBUF_SPACE) {
      dp[cnt] = 0;
      return(dp);
    }
    for(p1 = buf + UBUF_SPACE, p2 = buf, k = (acc -= UBUF_SPACE); k--;)
      *p2++ = *p1++;
  }
}

static int util_download_main PROTO5(char *, path, char *, fpath, FILE *, fp,
				     unsigned long, start_from, int, cmd)
{   
  unsigned long pos, started_from = start_from, downloaded;
  unsigned tmax, wrote, sent_time, rlen;
  UBUF *ub;
  time_t t = time(NULL);
  
  for(tmax = 1, pos = start_from, sent_time = 0; ;) {   
    ub = client_interact(cmd,pos,strlen(fpath),(unsigned char *)fpath+1, 2,
			 (unsigned char *)&client_net_len);
      
    if(client_trace && (udp_sent_time != sent_time)) {
      sent_time = udp_sent_time;
      if(client_buf_len == UBUF_SPACE) fprintf(stderr,"\r%luk  ",1+(pos>>10));
      else fprintf(stderr,"\r%lu   ", pos);
      fflush(stderr);
    }
      
    if(ub->cmd == CC_ERR) {    
      fprintf(stderr,"downloading %s: %s\n",path,ub->buf);
      return(-1); 
    }
      
    rlen = BB_READ2(ub->bb_len);
    wrote = fwrite(ub->buf,1,rlen,fp);
    pos  += wrote;
      
    if(rlen < client_buf_len || rlen != wrote) break;
  }
  
  t = time(NULL) - t;
  if (t == 0) t = 1;
  downloaded = pos - started_from;
  if(client_trace) {
    fprintf(stderr,"\r%luk : %s [%db/s] \n", 1+(pos>>10), path, downloaded/t);
    fflush(stderr);
  }
  
  return(0);
}

int util_download PROTO3(char *, path, FILE *, fp, unsigned long, start_from)
{   
  int code, len;
  char *fpath;
  
  fpath = util_abs_path(path);
  if(*env_passwd) {
    strcat(fpath, "\n");
    strcat(fpath, env_passwd);
    len = strlen(fpath);
  }
  code = util_download_main(path, fpath, fp, start_from, CC_GET_FILE);
  free(fpath);
  return(code);
}

int util_grab_file PROTO3(char *, path, FILE *, fp, unsigned long, start_from)
{   
  int code, len;
  char *fpath;
  UBUF *ub;
  
  fpath = util_abs_path(path);
  if(*env_passwd) {
    strcat(fpath, "\n");
    strcat(fpath, env_passwd);
    len = strlen(fpath);
  }
  code = util_download_main(path, fpath, fp, start_from, CC_GRAB_FILE);
  if(code) {
    free(fpath);
    return(code);
  }
  
  ub = client_interact(CC_GRAB_DONE, 0L, len, (unsigned char *)fpath+1, 0,
		       (unsigned char *)NULLP);    
  
  if(ub->cmd == CC_ERR) {
    fprintf(stderr,"Warning, unexpected grab error: %s\n",ub->buf);
  }
  
  free(fpath);
  return(code);
}

int util_upload PROTO2(char *, path, FILE *, fp)
{   
  unsigned long pos;
  unsigned bytes, first, tmax, sent_time;
  char *fpath, buf[UBUF_SPACE];
  UBUF *ub;
  time_t t = time(NULL);
  
  fpath = util_abs_path(path);
  
  for(tmax = 1, sent_time = 0, pos = 0, first = 1; ; first = 0) {   
    if((bytes = fread(buf,1,client_buf_len,fp)) || first) {
      ub = client_interact(CC_UP_LOAD,pos, bytes, (unsigned char *)buf, 0,
			   (unsigned char *)NULLP);
      if(client_trace && (udp_sent_time != sent_time)) {
	sent_time = udp_sent_time;
	if(client_buf_len == UBUF_SPACE)
	  fprintf(stderr,"\r%luk  ",1+(pos>>10));
	else fprintf(stderr,"\r%lu   ",   pos     );
	fflush(stderr);
      }
    } else
      ub = client_interact(CC_INSTALL,pos,strlen(fpath), 
			   (unsigned char *)fpath+1, 0,
			   (unsigned char *)NULLP);
    if(ub->cmd == CC_ERR) {    
      fprintf(stderr,"uploading %s: %s\n",path,ub->buf);
      free(fpath);
      return(1); 
    }   
      
    if(!bytes && !first) break;
    pos += bytes;
  }
  
  t = time(NULL) - t;
  if(t == 0) t = 1;
  if(client_trace) {
    fprintf(stderr,"\r%luk : %s [%db/s] \n", 1+(pos>>10), path, pos/t);
    fflush(stderr);
  }
  free(fpath);
  return(0);
}

static void util_get_env PROTO0((void))
{
  char *p;
  
  if(!(env_host = getenv("FSP_HOST"))) {
    fputs("No FSP_HOST specified.\n",stderr);
    exit(1);
  }
  if(!(env_port = getenv("FSP_PORT"))) {
    fputs("No FSP_PORT specified.\n",stderr);
    exit(1);
  }
  if(!(env_dir  = getenv("FSP_DIR"))) {
    fputs("No FSP_DIR specified.\n",stderr);
    exit(1);
  }
  if(!(env_myport = getenv("FSP_LOCALPORT"))) env_myport = "0";
  if(!(env_passwd = getenv("FSP_PASSWORD"))) env_passwd = "\0";
  client_trace  = !!getenv("FSP_TRACE");
  if(p = getenv("FSP_BUF_SIZE")) client_buf_len = atoi(p);
  else client_buf_len = UBUF_SPACE;
  
  if(client_buf_len > UBUF_SPACE) client_buf_len = UBUF_SPACE; 
  client_net_len = htons(client_buf_len);
  
  if(p = getenv("FSP_DELAY")) target_delay = atol(p);
  if(target_delay < MIN_DELAY) target_delay = MIN_DELAY;
  
  if(!(env_local_dir = getenv("FSP_LOCAL_DIR"))) env_local_dir=".";
  
  if(!(p = getenv("FSP_TIMEOUT"))) env_timeout = 4;
  else env_timeout = atol(p);
}

static void client_intr PROTO1(int, signum)
{
  switch(client_intr_state) {
    case 0: exit(2);
    case 1: client_intr_state = 2; break;
    case 2: exit(3);
  }
}

void env_client PROTO0((void))
{
  util_get_env();
  init_client(env_host,atoi(env_port),atoi(env_myport));
  signal(SIGINT,client_intr);
}

static DDLIST *ddroot = 0;

RDIR *util_opendir PROTO1(char *, path)
{
  char *fpath;
  RDIRENT **dep;
  DDLIST *ddp;
  RDIR *rdirp;
  
  fpath = util_abs_path(path);
  
  for(ddp = ddroot; ddp; ddp = ddp->next)
    if(!strcmp(ddp->path,fpath)) break;
  
  if(!ddp) {
    if(!(dep = get_dir_blk(fpath))) return((RDIR *) 0);
    ddp = (DDLIST *) malloc(sizeof(DDLIST));
    ddp->dep_root = dep;
    ddp->path = fpath;
    ddp->ref_cnt = 0;
    ddp->next = ddroot;
    ddroot = ddp;
  } else free(fpath);
  
  ddp->ref_cnt++;
  
  rdirp = (RDIR *) malloc(sizeof(RDIR));
  rdirp->ddp = ddp;
  rdirp->dep = ddp->dep_root;
  return(rdirp);
}

void util_closedir PROTO1(RDIR *, rdirp)
{
  rdirp->ddp->ref_cnt--;
  free(rdirp);
}

rdirent *util_readdir PROTO1(RDIR *, rdirp)
{
  static rdirent rde;
  RDIRENT **dep;
  
  dep = rdirp->dep;
  
  if(!*dep) return((rdirent *) 0);
  
  rde.d_fileno = 10;
  rde.d_reclen = 10;
  rde.d_namlen = strlen((*dep)->name);
  rde.d_name   = (*dep)->name;
  rdirp->dep   = dep+1;
  
  return(&rde);
}

static int util_split_path PROTO4(char *, path, char **, p1, char **, p2,
				  char **, p3)
{
  char *s;
  static char junk;
  
  *p1 = "/";
  if(*path == '/') {
    *p2 =  path;
    *p3 = path+1;
  } else {
    *p2 = &junk;
    *p3 = path;
  }
  
  for(s = *p3; *s; s++) {
    if(*s == '/') {
      *p1 = path;
      *p2 = s;
      *p3 = s+1;
    }
  }
  
  if (**p3 == '\0') *p3 = ".";
  return(1);
}

int util_stat PROTO2(char *, path, struct stat *, sbuf)
{
  RDIR *drp;
  RDIRENT **dep;
  char *fpath, *ppath, *p1, *pfile;
  
  fpath = util_abs_path(path);
  
  if(!strcmp(fpath,env_dir)) {
    ppath = fpath;
    pfile = ".";
  } else {
    util_split_path(fpath,&ppath,&p1,&pfile);
    *p1 = 0;
  }
  
  if(drp = util_opendir(ppath)) {
    for(dep = drp->dep; *dep; dep++) {
      if(!strcmp((*dep)->name,pfile)) {
	if((*dep)->type & RDTYPE_DIR) sbuf->st_mode = 0777 | S_IFDIR;
	else sbuf->st_mode = 0666 | S_IFREG;
	      
	if((*dep)->type & RDTYPE_DIR) sbuf->st_nlink  = 2;
	else sbuf->st_nlink  = 1;
	sbuf->st_uid = 0;
	sbuf->st_gid = 0;
	sbuf->st_size  = BB_READ4((*dep)->bb_size);
	sbuf->st_atime = sbuf->st_mtime = 
			 sbuf->st_ctime = BB_READ4((*dep)->bb_time);
	util_closedir(drp);
	free(fpath);
	return(0);
      }
    }
    util_closedir(drp);
  }
  
  free(fpath);
  errno = ENOENT;
  return(-1);
}

int util_cd PROTO1(char *, p)
{
  char *fpath;
  UBUF *ub;
  DDLIST   *ddp;
  
  fpath = util_abs_path(p);
  for(ddp = ddroot; ddp; ddp = ddp->next)
    if(!strcmp(ddp->path,fpath)) break;
  
  if(!ddp && strcmp(p,".") && strcmp(p,"..")) {
    ub = client_interact(CC_GET_DIR,0L, strlen(fpath),
			 (unsigned char *) fpath+1, 2,
			 (unsigned char *)&client_net_len);
    if(ub->cmd == CC_ERR) {
      free(fpath);
      fprintf(stderr,"%s: %s\n",p, ub->buf);
      errno = EACCES;
      return(-1);
    }
  }
  
  if(env_dir_malloced) free(env_dir);
  env_dir_malloced = 1;
  env_dir = fpath;
  return(0);
}

/* Perform a cd, but don't verify path.  Assume the path has been
 * pre-verified
 */
int util_cd2 PROTO1(char *, p)
{
  char *fpath;
  
  fpath = util_abs_path(p);
  
  if(env_dir_malloced) free(env_dir);
  env_dir_malloced = 1;
  env_dir = fpath;
  return(0);
}

#ifdef HAVE_ANSI_PROTO
void util_process_file(char *path, int mode, void (*process_file)(),
		       int (*process_start_dir)(), void (*process_end_dir)(),
		       int level)
#else
void util_process_file(path, mode, process_file, process_start_dir,
			process_end_dir, level)
     char *path;
     int mode, level;
     void (*process_file)(), (*process_end_dir)();
     int (*process_start_dir)();
#endif
{
  struct stat sbuf;
  RDIR *rdir;
  struct rdirent *rde;
  int pathlen;
  char *newname;
  u_long sum;

  if (util_stat(path, &sbuf) < 0) {
    perror(path);
    return;
  }
  
  if (S_ISREG(sbuf.st_mode)) {
    if(process_file) (*process_file)(path, &sbuf, mode, level);
  } else if (S_ISDIR(sbuf.st_mode)) {
    sum = mode;
    if (process_start_dir && (*process_start_dir)(path, &sbuf, &sum) < 0)
      fprintf(stderr, "skipping remote directory `%s'\n", path);
    else {
      if ((rdir = util_opendir(path))) {
	pathlen = strlen(path);
	while ((rde = util_readdir(rdir))) {
	  /* skip over "." and ".." */
	  if (rde->d_name[0] == '.' &&
	      (rde->d_name[1] == '\0'  ||
	       (rde->d_name[1] == '.' && rde->d_name[2] == '\0')))
	    continue;
	  newname = malloc(pathlen + rde->d_namlen + 2);

	  strcpy(newname, path);
	  if(newname[pathlen-1] != '/') newname[pathlen] = '/';
	  else pathlen--;
	  strcpy(newname + pathlen + 1, rde->d_name);
	  util_process_file(newname, mode, process_file, process_start_dir,
			    process_end_dir, level + 1);
	  free(newname);
	}
	util_closedir(rdir);
      }
      if(process_end_dir) (*process_end_dir)(path, mode, sum, level);
    }
  } else
    fprintf(stderr, "remote file `%s' is not a file or directory!\n",path);
/*  free(path); */
}
