#include "config.h"

#ifndef SLRNPULL_CODE
# include "slrnfeat.h"
#endif

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include <slang.h>
#include "jdmacros.h"

#ifndef SLRNPULL_CODE
# include "slrn.h"
# include "group.h"
# include "misc.h"
# include "server.h"
# include "hash.h"
# include "util.h"
#endif

#include "art.h"
#include "xover.h"
#include "score.h"

/* The score file will implement article scoring for slrn.  Basically the idea 
 * is that the user will provide a set of regular expressions that
 * will act on the header of an article and return an integer or
 * 'score'. If this score is less than zero, the article will be
 * killed.  If it is a positive number greater than some user defined
 * value, the article will be flagged as interesting.  If the score
 * is zero, the article is not killed and it is not flagged as
 * interesting either.
 */

int Slrn_Perform_Scoring = SLRN_XOVER_SCORING | SLRN_EXPENSIVE_SCORING;

/* These two structures are pseudo-score types containing no compiled
 * regular expressions.
 */
typedef struct PScore_Regexp_Type
{
#define MAX_KEYWORD_LEN 24
   char keyword[MAX_KEYWORD_LEN];      /* used only by generic type */
   unsigned int header_type;
#define SCORE_SUBJECT 1
#define SCORE_FROM 2
#define SCORE_XREF 3
#define SCORE_NEWSGROUP 4
#define SCORE_REFERENCES 5
#define SCORE_LINES 6
#define SCORE_MESSAGE_ID 7
#define SCORE_DATE 8
   /* generic requires extra server interaction */
#define SCORE_GENERIC 16
   unsigned int flags;
#define NOT_FLAG 1
#define USE_INTEGER 2
   union
     {
	unsigned char *regexp_str;
	int ival;
     }
   ireg;
   struct PScore_Regexp_Type *next;
}
PScore_Regexp_Type;

typedef struct PScore_Type
{
   int score;
   unsigned int flags;
#define RETURN_THIS_SCORE 1
#define SCORE_IS_OR_TYPE 2
   struct PScore_Regexp_Type *pregexp_list;
   struct PScore_Type *next;
}
PScore_Type;

typedef struct
{
   SLRegexp_Type regexp;
   unsigned char buf[512];	       /* for compiled pattern */
}
Our_SLRegexp_Type;

/* These two structures are compiled versions of the above. */
typedef struct Score_Regexp_Type
{
   unsigned int header_type;
   char *generic_keyword;	       /* pointer to space in pscore */
   int not_flag;
   int do_osearch;
   union
     {
	int ival;		       /* used by certain headers */
	Our_SLRegexp_Type re;
	SLsearch_Type se;
     }
   search;
   struct Score_Regexp_Type *next;
}
Score_Regexp_Type;

typedef struct Score_Type
{
   PScore_Type *pscore;		       /* points at structure this is derived from */
   struct Score_Regexp_Type regexp_list;
   struct Score_Type *next;
}
Score_Type;

#define MAX_GROUP_REGEXP_SIZE	256
#define MAX_GROUP_NAME_LEN	80

typedef struct Group_Score_Name_Type
{
   SLRegexp_Type group_regexp;
   char name[MAX_GROUP_NAME_LEN];      /* group name or pattern */
   unsigned char buf[MAX_GROUP_REGEXP_SIZE];/* for compiled pattern */
   struct Group_Score_Name_Type *next;
}
Group_Score_Name_Type;

typedef struct Group_Score_Type
{
   unsigned int gst_not_flag;
   Group_Score_Name_Type gsnt;
   PScore_Type *pst;
   struct Group_Score_Type *next;
}
Group_Score_Type;

static Group_Score_Type *Group_Score_Root;
static Group_Score_Type *Group_Score_Next;
static Score_Type *Score_Root, *Score_Tail;

int Slrn_Apply_Score = 1;

int slrn_score_header (Slrn_Header_Type *h, char *newsgroup)
{
   Score_Type *st;
   int score = 0;
   char *s;
   int ival = 0;
   
#ifndef SLRNPULL_CODE
#if SLRN_HAS_MSGID_CACHE
   s = slrn_is_msgid_cached (h->msgid, newsgroup, 1);
   if (s != NULL)
     {
	/* Kill it if this wasn't the newsgroup where we saw it */
	if (strcmp (s, newsgroup))
	  return Slrn_Kill_Score_Max;
     }
#endif
#endif				       /* NOT SLRNPULL_CODE */
   
   s = NULL;

   st = Score_Root;
   while (st != NULL)
     {
	unsigned int len;
	Score_Regexp_Type *srt = &st->regexp_list;
#ifndef SLRNPULL_CODE
	char buf[1024];
#endif
	int or_type = st->pscore->flags & SCORE_IS_OR_TYPE;
	
	while (srt != NULL)
	  {
	     switch (srt->header_type)
	       {
		case SCORE_LINES:
		  ival = h->lines;
		  goto integer_compare;/* ugly, ugly, ugly!! */
		  
		case SCORE_SUBJECT:
		  s = h->subject;
		  break;
		  
		case SCORE_FROM:
		  s = h->from;
		  break;
		  
		case SCORE_DATE:
		  s = h->date;
		  break;

		case SCORE_MESSAGE_ID:
		  s = h->msgid;
		  break;
		  
		case SCORE_XREF:
		  s = h->xref;
		  break;
		  
		case SCORE_REFERENCES:
		  s = h->refs;
		  break;
		  
		case SCORE_NEWSGROUP:
		  s = newsgroup;
		  break;
		  
		case SCORE_GENERIC:
#ifdef SLRNPULL_CODE
		  s = slrn_get_extra_xover_header (srt->generic_keyword);
#else
		  
		  /* Yuk.  This will be slow! */
		  if (Slrn_Score_After_XOver)
		    {
		       if (((Slrn_Perform_Scoring & SLRN_EXPENSIVE_SCORING) == 0)
			   || (-1 == Slrn_Server_Obj->sv_xhdr_command (srt->generic_keyword,
							h->number, buf, sizeof(buf))))
			 s = NULL;
		       else s = buf;
		    }
		  else
		    {
		       if (Slrn_Perform_Scoring & SLRN_EXPENSIVE_SCORING)
			 s = slrn_get_extra_xover_header (srt->generic_keyword);
		       else s = NULL;
		    }
#endif
		  break;
		  
		default:
		  s = NULL;	       /* not supposed to happen */
	       }
	     
	     if (s == NULL)
	       {
		  if (or_type) goto next_srt;
		  break;
	       }
	     
	     len = strlen (s);
	     
	     if (srt->do_osearch)
	       {
		  SLsearch_Type *se = &srt->search.se;
		  
		  if ((len < (unsigned int) se->key_len)
		      || (NULL == SLsearch ((unsigned char *) s,
					    (unsigned char *) s + len,
					    se)))
		    {
		       if (srt->not_flag == 0)
			 {
			    if (or_type) goto next_srt;
			    break;
			 }
		    }
		  else if (srt->not_flag)
		    {
		       if (or_type) goto next_srt;
		       break;
		    }
	       }
	     else
	       {
		  SLRegexp_Type *re;
		  
		  re = &srt->search.re.regexp;
		  if ((len < re->min_length)
		      || (NULL == SLang_regexp_match ((unsigned char *)s, len, re)))
		    {
		       if (srt->not_flag == 0)
			 {
			    if (or_type) goto next_srt;
			    break;
			 }
		    }
		  else if (srt->not_flag)
		    {
		       if (or_type) goto next_srt;
		       break;
		    }
	       }
	     /* Get here if above matched */
	     if (or_type) break;
	     srt = srt->next;
	     continue;
	     
	     /* This is ugly but I am worried about speed. --- we only get
	      * here for those headers that have integer values.
	      */
	     integer_compare:
	     if (ival < srt->search.ival)
	       {
		  if (srt->not_flag == 0)
		    {
		       if (or_type) goto next_srt;
		       break;
		    }
	       }
	     else if (srt->not_flag)
	       {
		  if (or_type) goto next_srt;
		  break;
	       }
	     
	     /* If we get here, the regular expression matched. */
	     if (or_type) break;
	     
	     next_srt:
	     srt = srt->next;
	  }
	
	if (((srt == NULL) && (or_type == 0))
	    || (or_type && (srt != NULL)))
	  {
	     int st_score = st->pscore->score;
	     
	     if (st->pscore->flags & RETURN_THIS_SCORE)
	       return st_score;
	     
	     if ((st_score == 9999)
		 || (st_score == -9999))
	       return st_score;
	     
	     if (st_score == 0)
	       return score;
	     
	     score += st_score;
	  }
	
	st = st->next;
     }
   return score;
}


static int chain_group_regexp (PScore_Type *pst, int *generic)
{
   PScore_Regexp_Type *psrt;
   Score_Regexp_Type *srt;
   Score_Type *st;
   SLRegexp_Type *re;
   
   while (pst != NULL)
     {
	st = (Score_Type *) slrn_malloc (sizeof (Score_Type), 1, 0);
	if (st == NULL)
	  return -1;
	
	if (Score_Root == NULL)
	  {
	     Score_Root = st;
	  }
	else Score_Tail->next = st;
	
	Score_Tail = st;
	
	st->pscore = pst;
	
	psrt = pst->pregexp_list;
	srt = &st->regexp_list;
	
	while (psrt != NULL)
	  {
	     unsigned int flags = psrt->flags;
	     
	     if (SCORE_GENERIC == (srt->header_type = psrt->header_type))
	       *generic += 1;
	     
	     srt->generic_keyword = psrt->keyword;
	     srt->not_flag = (0 != (flags & NOT_FLAG));
	     
	     if (flags & USE_INTEGER)
	       {
		  srt->search.ival = psrt->ireg.ival;
	       }
	     else
	       {
		  re = &srt->search.re.regexp;
		  re->pat = psrt->ireg.regexp_str;
		  re->buf = srt->search.re.buf;
		  re->buf_len = sizeof (srt->search.re.buf);
		  re->case_sensitive = 0;
		  
		  if (0 != SLang_regexp_compile (re))
		    {
		       return -1;
		    }
		  
		  /* If an ordinary search is ok, use it. */
		  if (re->osearch)
		    {
		       srt->do_osearch = 1;
		       SLsearch_init ((char *) psrt->ireg.regexp_str, 1, 0, &srt->search.se);
		    }
	       }
	     
	     psrt = psrt->next;
	     if (psrt == NULL) break;
	     
	     srt->next = (Score_Regexp_Type *) slrn_malloc (sizeof (Score_Regexp_Type),
							    1, 0);
	     if (NULL == (srt = srt->next))
	       return -1;
	  }
	pst = pst->next;
     }
   return 0;
}

static void free_group_chain (void)
{
   Score_Regexp_Type *srt;
   
   while (Score_Root != NULL)
     {
	Score_Type *next = Score_Root->next;
	srt = &Score_Root->regexp_list;
	srt = srt->next;	       /* first not malloced */
	while (srt != NULL)
	  {
	     Score_Regexp_Type *srt_next = srt->next;
	     SLFREE (srt);
	     srt = srt_next;
	  }
	SLFREE (Score_Root);
	Score_Root = next;
     }
   Score_Tail = NULL;
}

int slrn_open_score (char *group_name)
{
   Group_Score_Type *gsc = Group_Score_Root;
   unsigned int n;
   int generic;
   
   if ((Slrn_Perform_Scoring == 0) || (Group_Score_Root == NULL))
     {
	Slrn_Apply_Score = 0;
	return 0;
     }
   
   n = strlen (group_name);
   generic = 0;
   while (gsc != NULL)
     {
	Group_Score_Name_Type *gsnt;
	SLRegexp_Type *re;
	unsigned int gst_not_flag;
	
	gsnt = &gsc->gsnt;
	gst_not_flag = gsc->gst_not_flag;
	
	while (gsnt != NULL)
	  {
	     int match;
	     
	     re = &gsnt->group_regexp;
	
	     match = ((re->min_length <= n)
		      && (NULL != SLang_regexp_match ((unsigned char *) group_name, n, re)));
	     
	     /* Note: the following could be replaced by simply
	      *  match = (match ^ gst_not_flag);
	      * however, I may have more uses for this flag in the future.
	      */
	     match = ((match && (gst_not_flag == 0))
		      || ((match == 0) && gst_not_flag));
	       
	     if (match)
	       {
		  if (-1 == chain_group_regexp (gsc->pst, &generic))
		    {
		       free_group_chain ();
		       return -1;
		    }
		  break;
	       }
	     gsnt = gsnt->next;
	  }
	gsc = gsc->next;
     }
   
   if (Score_Root == NULL) return 0;
   
   Slrn_Apply_Score = 1;
   
   if ((generic == 0)
       || (0 == (Slrn_Perform_Scoring & SLRN_EXPENSIVE_SCORING)))
     return 1;

#ifndef SLRNPULL_CODE
   if ((generic >= 1) || !Slrn_Server_Obj->sv_has_xover || !Slrn_Server_Obj->sv_has_cmd ("XHDR"))
     {
	Slrn_Score_After_XOver = 0;
	slrn_open_suspend_xover ();
     }
#endif

   return 1;
}

void slrn_close_score (void)
{
   free_group_chain ();
#ifndef SLRNPULL_CODE
   slrn_close_suspend_xover ();
#endif
}



static int add_group_regexp (PScore_Type *pst,
			     unsigned char *str,
			     unsigned char *keyword, unsigned int type, int not_flag)
{
   unsigned int len;
   PScore_Regexp_Type *psrt;
   
   *str++ = 0;			       /* null terminate keyword by zeroing
					* out the colon.  This is by agreement
					* with the calling routine.
					*/
   
   if (*str == ' ') str++;	       /* space following colon not meaningful */

   len = (unsigned int) (slrn_trim_string ((char *) str) - (char *) str);
   if (0 == len) return -1;
   
   psrt = (PScore_Regexp_Type *) slrn_safe_malloc (sizeof (PScore_Regexp_Type));
   
   if (type != SCORE_LINES)
     psrt->ireg.regexp_str = (unsigned char *) slrn_safe_strmalloc ((char *)str);
   else
     {
	psrt->ireg.ival = atoi((char *)str);
	psrt->flags |= USE_INTEGER;
     }
   
   psrt->header_type = type;
   strncpy (psrt->keyword, (char *) keyword, MAX_KEYWORD_LEN);
   psrt->keyword[MAX_KEYWORD_LEN - 1] = 0;
   
   if (not_flag) psrt->flags |= NOT_FLAG;
   psrt->next = NULL;
   if (pst->pregexp_list == NULL)
     {
	pst->pregexp_list = psrt;
     }
   else
     {
	PScore_Regexp_Type *last = pst->pregexp_list;
	while (last->next != NULL) last = last->next;
	last->next = psrt;
     }
   return 0;
}


static int compile_group_names (char *group, Group_Score_Type *gst)
{
   char *comma;
   Group_Score_Name_Type *gsnt;
   unsigned int num_processed = 0;
   
   gsnt = &gst->gsnt;
   
   comma = group;
   while (comma != NULL)
     {
	SLRegexp_Type *re;
	
	group = slrn_skip_whitespace (group);
	
	comma = slrn_strchr ((char *) group, ',');
	if (comma != NULL) 
	  {
	     *comma++ = 0;
	  }
	
	(void) slrn_trim_string (group);
	if (*group == 0) continue;
	
	strncpy (gsnt->name, group, MAX_GROUP_NAME_LEN - 1);
	/* Note: because of the memset, this string is null terminated. */
   
	re = &gsnt->group_regexp;
	re->pat = (unsigned char *) slrn_fix_regexp ((char *)group);
	re->buf = gsnt->buf;
	re->buf_len = MAX_GROUP_REGEXP_SIZE;
	re->case_sensitive = 0;
	if (0 != SLang_regexp_compile (re))
	  {
	     return -1;
	  }
	
	if (comma != NULL)
	  {
	     Group_Score_Name_Type *gsnt1;
	     
	     gsnt1 = (Group_Score_Name_Type *) slrn_safe_malloc (sizeof (Group_Score_Name_Type));
	     
	     gsnt->next = gsnt1;
	     gsnt = gsnt1;
	     group = comma;
	  }
	num_processed++;
     }
   if (num_processed) return 0;
   return -1;
}

static PScore_Type *create_new_score (unsigned char *group, int new_group_flag, unsigned int gst_not_flag,
				      int score, unsigned int pscore_flags)
{
   PScore_Type *pst;
   
   if (new_group_flag)
     {
	Group_Score_Type *gst, *tail;
	
	tail = Group_Score_Next = Group_Score_Root;
	while (Group_Score_Next != NULL)
	  {
	     if ((Group_Score_Next->gsnt.next == NULL)
		 && (Group_Score_Next->gst_not_flag == gst_not_flag)
		 && !strcmp ((char *) group, Group_Score_Next->gsnt.name))
	       break;
	     tail = Group_Score_Next;
	     Group_Score_Next = Group_Score_Next->next;
	  }
	
	if (Group_Score_Next == NULL)
	  {
	     gst = (Group_Score_Type *) slrn_safe_malloc (sizeof (Group_Score_Type));
	     
	     if (-1 == compile_group_names ((char *)group, gst))
	       return NULL;
	     
	     gst->gst_not_flag = gst_not_flag;

	     if (Group_Score_Root == NULL)
	       {
		  Group_Score_Root = gst;
	       }
	     else tail->next = gst;
	     
	     Group_Score_Next = gst;
	  }
     }
   
   /* Now create the PseudoScore type and add it. */
   pst = (PScore_Type *) slrn_safe_malloc (sizeof (PScore_Type));

   pst->score = score;
   pst->flags = pscore_flags;
   
   if (Group_Score_Next->pst == NULL)
     {
	Group_Score_Next->pst = pst;
     }
   else
     {
	PScore_Type *last = Group_Score_Next->pst;
	while (last->next != NULL) last = last->next;
	last->next = pst;
     }
   return pst;
}


static void score_error (char *msg, char *line, unsigned int linenum, char *file)
{
   slrn_error ("Error processing %s\nLine %u:\n%s\n%s\n",
	       file, linenum, line, msg);
}



static void free_group_scores (void)
{
   while (Group_Score_Root != NULL)
     {
	Group_Score_Type *gnext = Group_Score_Root->next;
	PScore_Type *pst = Group_Score_Root->pst;
	Group_Score_Name_Type *gsnt;
	
	gsnt = Group_Score_Root->gsnt.next;
	while (gsnt != NULL)
	  {
	     Group_Score_Name_Type *next = gsnt->next;
	     SLFREE (gsnt);
	     gsnt = next;
	  }
	
	while (pst != NULL)
	  {
	     PScore_Type *pnext = pst->next;
	     PScore_Regexp_Type *r = pst->pregexp_list;
	     
	     while (r != NULL)
	       {
		  PScore_Regexp_Type *rnext = r->next;
		  
		  if ((r->flags & USE_INTEGER) == 0)
		    slrn_free ((char *) r->ireg.regexp_str);
		  SLFREE (r);
		  r = rnext;
	       }
	     SLFREE (pst);
	     pst = pnext;
	  }
	
	SLFREE (Group_Score_Root);
	Group_Score_Root = gnext;
     }
}

static int has_score_expired (unsigned char *s, unsigned long today)
{
   unsigned long mm, dd, yyyy;
   unsigned long score_time;
   
   s = (unsigned char *) slrn_skip_whitespace ((char *) s);
   if (*s == 0) return 0;
   
   if (((3 != sscanf ((char *) s, "%lu/%lu/%lu", &mm, &dd, &yyyy))
	&& (3 != sscanf ((char *) s, "%lu-%lu-%lu", &dd, &mm, &yyyy)))
       || (dd > 31)
       || (mm > 12)
       || (yyyy < 1900))
     return -1;
   
   score_time = (yyyy - 1900) * 10000 + (mm - 1) * 100 + dd;
   if (score_time > today) return 0;
   return 1;
}

static unsigned long get_today (void)
{
   unsigned long mm, yy, dd;
   time_t tloc;
   struct tm *tm_struct;
   
   time (&tloc);
   tm_struct = localtime (&tloc);
   yy = tm_struct->tm_year;
   mm = tm_struct->tm_mon;
   dd = tm_struct->tm_mday;
   
   return yy * 10000 + mm * 100 + dd;
}

int slrn_read_score_file (char *file)
{
   unsigned char group[256];
   char line[1024];
   FILE *fp;
   int start_new_group = 1, not_flag, gnt_not_flag = 0;
   unsigned int linenum = 0;
   int score = 0;
   PScore_Type *pst = NULL;
   unsigned long today;
   int start_new_score = 0, score_has_expired = 0;
   unsigned int pscore_flags = 0;
   
   today = get_today ();
   
   if (Group_Score_Root != NULL)
     {
	free_group_scores ();
     }
   
   fp = fopen (file, "r");
   
   if (fp == NULL)
     {
	Slrn_Apply_Score = 0;
	return 0;
     }
   
   group[0] = '*';
   group[1] = 0;
   
   while (fgets (line, sizeof (line) - 1, fp))
     {
	unsigned char *lp;
	
	linenum++;
	lp = (unsigned char *) slrn_skip_whitespace (line);
	if ((*lp == '#') || (*lp == '%') || (*lp <= ' ')) continue;
	
	if (*lp == '[')
	  {
	     unsigned char *g, *gmax, ch;
	     g = group;
	     gmax = g + sizeof (group);
	     
	     lp++;
	     lp = (unsigned char *) slrn_skip_whitespace ((char *)lp);
	     
	     gnt_not_flag = 0;
	     if (*lp == '~')
	       {
		  gnt_not_flag = 1;
		  lp = (unsigned char *) slrn_skip_whitespace ((char *)lp + 1);
	       }
	     
	     while (((ch = *lp++) != 0)
		    && (ch != ']') && (g < gmax))
	       *g++ = ch;
	     
	     if ((ch != ']') || (g == gmax))
	       {
		  score_error ("Syntax Error.", line, linenum, file);
		  goto error_return;
	       }
	     *g = 0;
	     start_new_group = 1;
	     score_has_expired = 0;
	     start_new_score = 0;
	     continue;
	  }
	
	if (!slrn_case_strncmp (lp, (unsigned char *)"Score:", 6))
	  {
	     unsigned char *lpp = lp + 6;
	     pscore_flags = 0;
	     if (*lpp == ':')
	       {
		  lpp++;
		  pscore_flags |= SCORE_IS_OR_TYPE;
	       }
	     lpp = (unsigned char *) slrn_skip_whitespace ((char *) lpp);
	     if (*lpp == '=')
	       {
		  pscore_flags |= RETURN_THIS_SCORE;
		  lpp++;
	       }
	     score = atoi ((char *)lpp);
	     start_new_score = 1;
	     continue;
	  }
	
	if (start_new_score)
	  {
	     if (!slrn_case_strncmp (lp, (unsigned char *) "Expires:", 8))
	       {
		  int ret;
		  ret = has_score_expired (lp + 8, today);
		  if (ret == -1)
		    {
		       score_error ("Expecting 'Expires: MM/DD/YYYY' or 'Expires: DD-MM-YYYY'",
				    line, linenum, file);
		       goto error_return;
		    }
		  if (ret)
		    {
		       slrn_message ("%s has expired score on line %d",
				     file, linenum);
		       start_new_score = 0;
		       score_has_expired = 1;
		    }
		  else score_has_expired = 0;
		  continue;
	       }
	     
	     
	     if (NULL == (pst = create_new_score (group, start_new_group, gnt_not_flag,
						  score, pscore_flags)))
	       {
		  score_error ("Bad group regular expression.", line, linenum, file);
		  goto error_return;
	       }
	     start_new_group = 0;
	     start_new_score = 0;
	     score_has_expired = 0;
	  }
	
	if (score_has_expired) continue;
	
	if (pst == NULL)
	  {
	     score_error ("Expecting Score keyword.", line, linenum, file);
	     goto error_return;
	  }
	
	if (*lp == '~')
	  {
	     not_flag = 1;
	     lp++;
	  }
	else not_flag = 0;
	
	
	/* Otherwise the line is a kill one */
	if (!slrn_case_strncmp (lp, (unsigned char *)"Subject:", 8))
	  add_group_regexp (pst, lp + 7, lp, SCORE_SUBJECT, not_flag);
	else if (!slrn_case_strncmp (lp, (unsigned char *)"From:", 5))
	  add_group_regexp (pst, lp + 4, lp, SCORE_FROM, not_flag);
	else if (!slrn_case_strncmp (lp, (unsigned char *)"Xref:", 5))
	  add_group_regexp (pst, lp + 4, lp, SCORE_XREF, not_flag);
	else if (!slrn_case_strncmp (lp, (unsigned char *)"Newsgroup:", 10))
	  add_group_regexp (pst, lp + 9, lp, SCORE_NEWSGROUP, not_flag);
	else if (!slrn_case_strncmp (lp, (unsigned char *)"References:", 11))
	  add_group_regexp (pst, lp + 10, lp, SCORE_REFERENCES, not_flag);
	else if (!slrn_case_strncmp (lp, (unsigned char *)"Lines:", 6))
	  add_group_regexp (pst, lp + 5, lp, SCORE_LINES, not_flag);
	else if (!slrn_case_strncmp (lp, (unsigned char *)"Date:", 5))
	  add_group_regexp (pst, lp + 5, lp, SCORE_DATE, not_flag);
	else if (!slrn_case_strncmp (lp, (unsigned char *)"Message-Id:", 11))
	  add_group_regexp (pst, lp + 10, lp, SCORE_MESSAGE_ID, not_flag);
	else
	  {
	     unsigned char *lpp = lp;
	     while (*lpp && (*lpp != ':')) lpp++;
	     if (*lpp != ':')
	       {
		  score_error ("Missing COLON.", line, linenum, file);
		  goto error_return;
	       }
	     
	     add_group_regexp (pst, lpp, lp, SCORE_GENERIC, not_flag);
	  }
     }
   
   fclose (fp);
   Slrn_Apply_Score = 1;
   return 1;
   
   error_return:
   fclose (fp);
   return -1;
}

