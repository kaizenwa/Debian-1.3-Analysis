/*
 * SGI gather.c		- gather and report process info.
 *
 * Michael Hamilton (michael@actrix.gen.nz).
 * Copyright (c) 1995
 * 
 * Basically the same licencing conditions that apply to gr_monitor.
 *
 * ALL RIGHTS RESERVED 
 * 
 * Permission to use, copy, modify, and distribute this software for 
 * any purpose and without fee is hereby granted, provided that the above
 * copyright notice appear in all copies and that both the copyright notice
 * and this permission notice appear in supporting documentation, and that 
 * the name of Michael Hamilton not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission. 
 * 
 * THE MATERIAL EMBODIED ON THIS SOFTWARE IS PROVIDED TO YOU "AS-IS"
 * AND WITHOUT WARRANTY OF ANY KIND, EXPRESS, IMPLIED OR OTHERWISE,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY OR
 * FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL MICHAEL
 * HAMILTON BE LIABLE TO YOU OR ANYONE ELSE FOR ANY DIRECT,
 * SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY
 * KIND, OR ANY DAMAGES WHATSOEVER, INCLUDING WITHOUT LIMITATION,
 * LOSS OF PROFIT, LOSS OF USE, SAVINGS OR REVENUE, OR THE CLAIMS OF
 * THIRD PARTIES, WHETHER OR NOT SILICON GRAPHICS, INC.  HAS BEEN
 * ADVISED OF THE POSSIBILITY OF SUCH LOSS, HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE
 * POSSESSION, USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */


#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/signal.h>
#include <sys/fault.h>
#include <sys/syscall.h>
#include <sys/procfs.h>
#include <sys/dir.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <time.h>

#include <pwd.h>

#include <sys/time.h>
#include <sys/resource.h>
#include <ctype.h>

#define PROCFS "/proc/pinfo"

typedef struct {
  int pid ;
  int uid ;
  unsigned long cpu ;
  unsigned long memory ;
  unsigned long resident ;
  unsigned long total_time ;
} pinfo_t ;


char *getname(int uid) 
{
  /*
   * Getting names can be inefficient, so cache any names we get.
   */

  struct PWDNAME {
    int uid;
    char name[12];		/* May as well be byte aligned */
  } ;

  static struct PWDNAME *cache = NULL;
  static count = 0 ;

  int i;
  struct passwd *pw ;

  if (uid == 0) {		/* We know this one. */
    return "{ root }";
  }

  for (i = 0; i < count ; i++) { /* Search the cache */
    if (cache[i].uid == uid) {
      return cache[i].name;
    }
  }
  
  pw = getpwuid(uid);

  if (pw) {			/* Look up and cache a new name */
    ++count ;
    cache = (struct PWDNAME *) realloc((void *) cache,
				       sizeof(struct PWDNAME) * count);
    cache[count - 1].uid = uid;
    strcpy(cache[count - 1].name, pw->pw_name);
    
  }

  return pw == NULL ? "Unknown" : pw->pw_name;
}

void show_process(pinfo_t *info)
{

  double up ;
  double idle ;
  struct passwd *pw ;

  unsigned long cpu_time ;
				/* Scale to reasonable values.
				 * Is this really gr_gathers job, I think not.
				 */
  printf("% 5d\n%s\n %d %d %d %d\n",
	 info->pid,
	 getname(info->uid),
	 info->cpu,
	 info->resident / 512,
	 info->memory / 512,
	 info->total_time / 60
	 ) ;
  
}

pinfo_t *get_p_info(int *num) {

  static int max = 0;
  static pinfo_t *info = NULL;

  struct timeval tp;

  int count = 0;
  prpsinfo_t p;
  
  DIR *dirp;
  struct direct *dp;

  char name[MAXNAMLEN];

  gettimeofday(&tp);

  dirp = opendir(PROCFS);	/* Open the process file system info dir. */
  if (dirp == NULL) {
    perror(PROCFS);
  }
  
  while ((dp = readdir(dirp)) != NULL) {

				/* Only do files that are numbers */
    if (strtol(dp->d_name, NULL, 10)) {
      int fd, ret;
      
      sprintf(name, "%s/%s", PROCFS, dp->d_name);

      fd = open( name, O_RDONLY);
      if (fd == -1 && errno != ENOENT) {
	perror(name);
      }    
				/* Get info on this process.
				 * See "man proc" under IRIX 5.x.
				 */
      ret = ioctl(fd, PIOCPSINFO, &p);      

      if (ret == 0) {		/* Got some info. */
	count++;
	if (count > max) {	/* Expand the array to fit a new entry */
	  info =  (pinfo_t *) realloc(info,sizeof(pinfo_t) * count);
	  max = count;
	}
				/* Tack on the end. */
	info[count - 1].pid        = p.pr_pid;
	info[count - 1].uid        = p.pr_uid;
	info[count - 1].cpu        = p.pr_time.tv_sec / 10;
	info[count - 1].memory     = p.pr_size;
	info[count - 1].resident   = p.pr_rssize;
	info[count - 1].total_time = (tp.tv_sec - p.pr_start.tv_sec) / 60;
	/* timestruc_t */
      }
      close(fd);
    }
  }
  closedir(dirp);
  *num = count;
  return info;
}

void show_all_processes ()
{
  float location ;
  time_t now;
  pinfo_t *data;
  int count, i;

  char host[80] ;

  gethostname(host, 79);
  now = time(NULL);

  data = get_p_info(&count) ;

  printf("Process View: %s %s", host, ctime(&now));
  printf("%d %d CPU RSS Mem Time\n", count, 4) ;

  for ( i = 0 ; i < count ; i++) {
    show_process(&data[i]) ;
  }

}

void usage(void) 
{
  printf("Usage: gather [-sleep n]\n");
}

int main(int argc, char **argv)
{
  /* loop, collecting process info and sleeping */
  
  int i, seconds = 2;  

  for (i = 1; i < argc; ++i) {

    if (!strcmp("-sleep", argv[i])) {
      seconds = strtol(argv[++i],NULL,0);

    } else {
      usage();
    }
  }
				/* Switch off buffering on our output */
  setvbuf(stdout,NULL,_IONBF,0);
  for (;;) {
    show_all_processes() ;
    sleep(seconds) ;
  }
}



