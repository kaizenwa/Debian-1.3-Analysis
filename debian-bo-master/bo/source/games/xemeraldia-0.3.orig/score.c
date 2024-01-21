/*                                                              */
/* score.c --  for xemeraldia                                   */
/*                                                              */
/* This file is copied from "xtetris" Version 2.5, and modified */

#include "graphics.h"
#include "games.h"
#include <X11/Xlibint.h>
#include <sys/file.h>
#include <sys/types.h>
#include <time.h>


/* #define DEBUG */

#define HIGH_TABLE_SIZE 30      /* size of high score table */

#define YES  (1)
#define NO   (0)

#if defined(SYSV) && !defined(sgi)
#define HAVE_FLOCK	NO
#define HAVE_LOCKF	YES
#else
#define HAVE_FLOCK	YES
#define HAVE_LOCKF	NO
#endif


static struct score_table
{
  char    name[25];
  int     score;
  int     level;
  char    date[30];
}  high_scores[HIGH_TABLE_SIZE];


void  update_highscore_table ()
{
  int           i, j;
  long          when;
  char	        padname[25];

  strcpy (padname, "            ");
  strncpy (padname, name, strlen (name));

  if (! app_data.usescorefile) return;
  read_high_scores ();

  /* Check for previous best score */
  for (i = 0; i < HIGH_TABLE_SIZE; i++)
    {
      if (strcmp (padname, high_scores[i].name) == 0) break;
    }
  if (i < HIGH_TABLE_SIZE)
    {
      /* We have a previous best score. */
      if (high_scores[i].score >= sc)
	return;                       /* Same/worse score - no update */
      for (j = i; j > 0; j--)         /* Remove previous best */
	high_scores[j] = high_scores[j - 1];
    }

  /* Next line finds score greater than current one */
  for (i = 0; i < HIGH_TABLE_SIZE && sc >= high_scores[i].score; i++);
  i--;
  if (i >= 0)
    {
      for (j = 0; j < i; j++)
	high_scores[j] = high_scores[j + 1];
      strcpy(high_scores[i].name, name);
      high_scores[i].score = sc;
      high_scores[i].level = blocks / 20 + 1; 
      time (&when);
      strncpy (high_scores[i].date, ctime (&when), 24); 
      high_scores[i].date[24] = 0;
      write_high_scores ();
    }
}


void  read_high_scores()
{
  FILE  *fp;
  int    i;
  int  ret;
  
  if (! app_data.usescorefile)  return;

  if ((fp = fopen (app_data.scorefile, "r+")) == NULL)
    {
      write_high_scores ();
      if ((fp = fopen (app_data.scorefile, "r+")) == NULL)
	{
	  app_data.usescorefile = False;
	  fprintf (stderr, "%s: No High score file.  Use '-noscore' to avoid this message.\n",
		  programname );
	  return;
	}
    }

#if HAVE_FLOCK
  if (flock (fileno (fp), LOCK_SH) == -1)
    perror ("read_high_scores:can't shared lock");
#else if HAVE_LOCKF
  rewind (fp);
  if ((ret = lockf (fileno (fp), F_LOCK, 0L)) == -1)
    perror ("read_high_scores:can't lock");
#ifdef DEBUG
  fprintf (stderr, "%d\n", ret);
#endif
#endif
  for (i = 0; i < HIGH_TABLE_SIZE; i++)
    {
      struct score_table *score = &(high_scores[i]);
      if (4 != fscanf( fp, "%12[^,],%7d,%6d,%24[^,]\n",
		      score->name, 
		      &score->score, 
		      &score->level,
		      score->date)) 
	{
	  strcpy( score->name, "No name" );
	  strcpy( score->date, "No date" );
	  score->level = score->score = 0;
	}
    }
#if HAVE_FLOCK
  if (flock (fileno (fp), LOCK_UN) == -1)
    perror ("read_high_scores:can't unlock");
#else if HAVE_LOCKF
  rewind (fp);
  if ((ret = lockf (fileno (fp), F_ULOCK, 0L)) == -1)
    perror ("read_high_scores:can't unlock");
#ifdef DEBUG
  fprintf (stderr, "%d\n", ret);
#endif
#endif
  fclose(fp);
}


void  write_high_scores()
{
  FILE   *fp;
  int     i;
  int   ret;
  
  if ((fp = fopen (app_data.scorefile, "w")) == NULL)
    {
      fprintf(stderr, "%s: Couldn't open high score file %s\n",
	      programname, app_data.scorefile );
      return;
    }

#if HAVE_FLOCK
  if (flock (fileno (fp), LOCK_EX) == -1)
    perror ("write_high_scores:can't exclusive lock");
#else if HAVE_LOCKF
  rewind (fp);
  if ((ret = lockf (fileno (fp), F_LOCK, 0L)) == -1)
    perror ("write_high_scores:can't lock");
#ifdef DEBUG
  fprintf (stderr, "%d\n", ret);
#endif
#endif
  for (i = 0; i < HIGH_TABLE_SIZE; i++)
    fprintf (fp, "%-12s,%7d,%6d,%-24s\n",
	     high_scores[i].name, 
	     high_scores[i].score, 
	     high_scores[i].level,
	     high_scores[i].date);
#if HAVE_FLOCK
  if (flock (fileno (fp), LOCK_UN) == -1)
    perror ("write_high_scores:can't unlock");
#else if HAVE_LOCKF
  rewind (fp);
  if ((ret = lockf (fileno (fp), F_ULOCK, 0L)) == -1)
    perror ("write_high_scores:can't unlock");
#ifdef DEBUG
  fprintf (stderr, "%d\n", ret);
#endif
#endif
  fclose(fp);
}


void  PrintHighScores ()
{
  int     i;
  static  char    buf[81*HIGH_TABLE_SIZE+100];
  char * start;
  int len;
  
  if (! app_data.usescorefile) return;
  read_high_scores();
  sprintf( buf, "\
Name         Score   Level  Date\n\
----         -----   ------ ----\n" );
  len = strlen (buf);
  start = buf + len;
  for (i = HIGH_TABLE_SIZE-1; i >= 0; i--)
    {
      if (high_scores[i].score != 0)
	{
	  int chars;
	  sprintf (start, "%-12s %-7d %-6d %-24s\n",
		   high_scores[i].name, 
		   high_scores[i].score, 
		   high_scores[i].level,
		   high_scores[i].date);
	  chars = strlen (start);
	  start += chars;
	  len += chars;
	}
    }
  XtVaSetValues (score_text, XtNstring, (XtArgVal) buf,
		 XtNlength, (XtArgVal) len+1, NULL );
  XtPopupSpringLoaded (score_frame);
}
