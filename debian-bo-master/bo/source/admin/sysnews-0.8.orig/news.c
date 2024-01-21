/* news.c     - display system news
 *
 * (C) 1993,94,95   Charles (int@link.xs4all.nl)
 *
 * GPL
 */

/* $Source: /project/master/sysnews/news.c,v $
 * $Revision: 1.7 $
 * $Date: 1995/01/18 07:18:50 $
 */

static char rcsid[] = "$Id: news.c,v 1.7 1995/01/18 07:18:50 int Exp $";


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <dirent.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>
#include <utime.h>
#include <fcntl.h>
#include <pwd.h>
#include <termios.h>
#include <errno.h>
#include <limits.h>
#include "getopt.h"
#include "news.h"

#define VERSION 	"0.8"

char  *progname;
char  *newsdir = NEWSDIR;
char  *dformat = DATEFORMAT;
char **exclude_list = 0;
int    nr_exclude = 0;

/*
 * MAIN
 */
void main(int argc, char **argv)
{
  int	 expireover=0;
  int	 ch;

     progname = (char *)argv[0];

     while ((ch = getopt(argc, argv, "adD:e:f:snlpvx:?h")) != EOF)
       switch(ch)
	{
	      case 'a':            /* show all articles    */
		   f.all = 1;
		   break;
	      case 'd':            /* show date of article */
		   f.datestamp = 1;
		   break;
	      case 'D':            /* date format          */
		   dformat = optarg;
		   break;
	      case 'e':            /* expire articles      */
		   expireover = atoi(optarg);
		   f.expire = 1;
		   break;
	      case 'f':            /* use alternate newsdir */
		   newsdir = optarg;
		   break;
	      case 's':            /* show # of articles   */
		   f.items = 1;
		   break;
	      case 'n':            /* show names of articles */
		   f.names = 1;
		   break;
	      case 'l':            /* show one article per line */
		   f.oneperline = 1;
		   break;
	      case 'p':            /* pipe through $PAGER  */
		   f.pager = 1;
		   break;
	      case 'v':
		   f.verbose = 1;
		   break;
	      case 'x':
		   add_exclude(optarg);
		   break;
	      case '?':            /* hm. what could this be? */
	      case 'h':
	      default:
		   print_usage();
		   exit(1);
		   break;
	}

     if(f.expire)
      doexpire(expireover);
     else
      read_sysnews(argc, argv);

  exit(0);
}

void read_sysnews(int argc, char **argv)
{
  DIR	 *dfp;
  FILE	 *pfp = 0;
  struct dirent *dir;
  struct stat	st;
  struct passwd *pw;
  time_t news_time = 0;
  char	 *home, line[1024];
  char	 tmp[512];
  char	 *newsnames;
  int	 art_cnt = 0, avc;
  int	 cols = 80;
  int	 ret;

     home = getenv("HOME");
     if(!home)
       {
	  fprintf(stderr, "%s: error: you are homeless!\n", progname);
	  exit(1);
       }
     sprintf(line, "%s/%s", home, NEWSTIME);

     ret = stat(line, &st);
     if(ret < 0)
       {
	 if(errno != ENOENT)
	   exit(1);
	 if(!f.all && !f.items && !f.names && !argv[optind])
	   {
	     ret = creat(line, 0600);
	     close(ret);
	   }
       }
     else
	 news_time = st.st_mtime;

#ifdef TIOCGWINSZ
     if(!f.names && !f.oneperline)
      {
	struct winsize win;

	   if(ioctl(1, TIOCGWINSZ, &win) != -1)
	     cols = win.ws_col;
      }
#endif
     newsnames = malloc(cols);


    if(!f.all && !f.items && !f.names && !argv[optind])
      {
	ret = utime(line, 0);
	if(ret < 0)
	  {
	    fprintf(stderr, "%s: (%s) %s\n", progname, line, ERRMSG);
	  }
      }

     dfp = opendir(newsdir);
     if(!dfp)
       {
	 fprintf(stderr, "%s: opendir(%s): %s\n", progname, newsdir, ERRMSG);
	 exit(1);
       }


     while((dir=readdir(dfp)))
      {
	if(dir->d_name[0] == '.')  /* skip all names starting with '.' */
	  continue;
	if(argv[optind])
	  {
	    avc=optind;
	    for(avc=optind;avc<argc;avc++)
	     {
	       if(strstr(dir->d_name, argv[avc]))
	       break;
	     }
	     if(!argv[avc])
	     continue;
	  }
	sprintf(line, "%s/%s", newsdir, dir->d_name);
	ret = stat(line, &st);
	if(ret < 0)
	  {
	    fprintf(stderr, "%s: (%s) %s\n", progname, line, ERRMSG);
	    continue;
	  }
	if(news_time < st.st_mtime || f.all || argv[optind])
	 {
	   if(!f.names && !f.items)
	     {
		if(f.pager && (!pfp))
		  {
		    pfp = open_pager();
		    if(!pfp)
		      exit(1);
		  }

		pw = getpwuid(st.st_uid);
		fprintf(pfp ? pfp : stdout, "\n** %s (%s)  %s\n", dir->d_name,
		 pw ? pw->pw_name : "unknown", ctime(&st.st_mtime));
		fflush(pfp ? pfp : stdout);

		if(f.pager)
		  more(pfp, line);
		else
		  cat(line);
	     }
	   if(f.names)
	     {
	       if(f.oneperline)
		 {
		   if(f.datestamp)
		     {
		       strftime(tmp, 512, dformat, gmtime(&st.st_mtime));
		       printf("NEWS: %s %s\n", tmp, dir->d_name);
		     }
		   else
		     {
		       printf("NEWS: %s\n", dir->d_name);
		     }
		 }
	       else
		{
		  if((strlen(dir->d_name) +
		      strlen(newsnames) +
		      strlen(progname) + 1 + 2) > cols)
		   {
		      printf("NEWS: %s\n", newsnames);
		      strcpy(newsnames, "");
		   }
		  strcat(newsnames, dir->d_name);
		  strcat(newsnames, " ");
		}
	     }
	   art_cnt++;
	 }
      }

     if(f.names && art_cnt && newsnames[0])
       printf("NEWS: %s\n", newsnames);
     if(f.items)
       printf("NEWS: %d news article%s\n", art_cnt, (art_cnt == 1) ? "" : "s");
     closedir(dfp);
     if(pfp)
       pclose(pfp);
}

/**
*** print a file to stdout
**/
int cat(char *file)
{
  FILE *fp;

     fp = fopen(file, "r");
     if(!fp)
      {
	fprintf(stderr, "%s: (%s) %s\n", progname, file, ERRMSG);
	return ERR;
      }

     fcat(fileno(fp), fileno(stdout));
    fclose(fp);
 return OK;
}

/**
*** pipe a file through $PAGER
**/
int more(FILE *pfp, char *file)
{
  FILE *fp;

     fp = fopen(file, "r");
     if(!fp)
       {
	  fprintf(stderr, "%s: (%s) %s\n", progname, file, ERRMSG);
	  return ERR;
       }
     fcat(fileno(fp), fileno(pfp));
     fclose(fp);
 return OK;
}

/**
*** setup a pipe to $PAGER
**/
FILE *open_pager()
{
  FILE *pfp;
  char *pager;

     if(!(pager = getenv("PAGER")))
      pager = DEF_PAGER;

     pfp = popen(pager, "w");
     if(!pfp)
       {
	 fprintf(stderr, "%s: popen(%s) failed: %s\n", progname, pager, ERRMSG);
	 return 0;
       }
 return pfp;
}

int fcat(int fpin, int fpout)
{
  int  i;
  char buf[1024];

     while((i=read(fpin, buf, 1024)))
      {
	if(i == -1)
	  {
	     fprintf(stderr, "%s: %s\n", progname, ERRMSG);
	     return ERR;
	  }
	write(fpout, buf, i);
      }
  return OK;
}


/**
*** expire old news
**/
void doexpire(int expireover)
{
  DIR		*dfp;
  struct stat	st;
  struct dirent *dir;
  time_t	cur_t, exp_t;
  int		i, ret;

     dfp = opendir(newsdir);
     if(!dfp)
       {
	 fprintf(stderr, "Error opening newsdir (%s): %s\n", newsdir, ERRMSG);
	 exit(1);
       }

     chdir(newsdir);
     create_exclude_list();

#ifdef DEBUG
     for(i=0;i<nr_exclude;i++)
       printf("#%02d: \"%s\"\n", i, exclude_list[i]);
#endif

     time(&cur_t);
     exp_t = cur_t - (expireover * (60 * 60 * 24));

     while((dir=readdir(dfp)))
       {
	 if(dir->d_name[0] == '.')
	   continue;

	 for(i=0;i<nr_exclude;i++)
	   if(!strcmp(exclude_list[i], dir->d_name))
	     continue;

	 stat(dir->d_name, &st);
	 if(st.st_mtime < exp_t)
	  {
	    ret = unlink(dir->d_name);
	    if(ret < 0)
	     {
	       fprintf(stderr, "Error: remove (%s): %s\n", dir->d_name, ERRMSG);
	     }
	    else
	     {
	       if(f.verbose)
		 printf("Expiring article: %s\n", dir->d_name);
	     }
	  }
       }
     closedir(dfp);
}

void create_exclude_list(void)
{
  FILE	 *fp;
  char	 buf[BUFSIZ];

     fp = fopen(NOEXPFILE, "r");
     if(!fp)
       return;

      while(fgets(buf, BUFSIZ, fp))
	{
	   if(buf[0] == '#' || buf[0] == ';')   /* skip comments */
	     continue;
	   add_exclude(buf);
	}
      fclose(fp);
}

void add_exclude(char *str)
{
   char line[NAME_MAX];
   int	lineidx = 0;
   int	st = 0;

     while(1)
       {
	  switch(*str)
	    {
	       case ' ':
		    break;
	       case '\0':
	       case '\r':
	       case '\n':
	       case ',':
		    if(st)
		      {
			line[lineidx] = 0;
			lineidx = 0;
			st = 0;

			if(!nr_exclude)
			  exclude_list = (char **)malloc(sizeof(char *));
			else
			  exclude_list = (char **)realloc(exclude_list,
					       sizeof(char *) * (nr_exclude + 1));
			exclude_list[nr_exclude++] = strdup(line);
		      }
		    if(!*str)
		      return;
		    break;
	      default:
		   st = 1;
		   line[lineidx++] = *str;
		   if(lineidx >= NAME_MAX)
		     {
		       fprintf(stderr, "add_exclude: filename too long (%d)\n", lineidx);
		       exit(1);
		     }
		   break;
	    }
	  str++;
       }
}

void print_usage(void)
{
  fprintf(stderr, "news v%s - display system news -  (C) 1993,94,95 int@link.xs4all.nl\n", VERSION);
  fprintf(stderr, "Usage: %s [-flags] [[article1] [article2] .. ]\n\n", progname);
  fprintf(stderr, "        -a       display all news\n");
  fprintf(stderr, "        -d       add datestamp\n");
  fprintf(stderr, "        -D <fmt> date format (see strftime(3) for details)\n");
  fprintf(stderr, "        -f <dir> use alternate newsdir\n");
  fprintf(stderr, "        -n       display news article names only\n");
  fprintf(stderr, "        -l       one article name per line\n");
  fprintf(stderr, "        -p       pipe article through $PAGER\n");
  fprintf(stderr, "        -s       display number of news articles\n");
  fprintf(stderr, "\n");
  fprintf(stderr, "        -e #     expire news older than # days\n");
  fprintf(stderr, "        -x a,b   list of articles to exclude when expiring\n\n");
}

