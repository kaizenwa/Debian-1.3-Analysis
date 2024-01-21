/******************************************************************************
** $Id: rewrite.c,v 2.20 1996/02/22 21:43:51 gerd Exp gerd $
**=============================================================================
** 
** This file is part of BibTool.
** It is distributed under the GNU General Public License.
** See the file COPYING for details.
** 
** (c) 1996 Gerd Neugebauer
** 
** Net: gerd@informatik.uni-koblenz.de
** 
******************************************************************************/

#include "bibtool.h"
#include "symbols.h"
#include "entry.h"
#include "error.h"
#include "macros.h"
#include "type.h"
#include "rsc.h"
#include "key.h"
#include "s_parse.h"
#include "sbuffer.h"
#include "rewrite.h"
#ifdef REGEX
#include "regex.h"
#endif

 typedef struct rULE
 { char			    *rr_field;
#ifdef REGEX
   struct re_pattern_buffer rr_pat_buff;
#endif
   char			    *rr_frame;
   struct rULE		    *rr_next;
 } SRule, *Rule;

#define RuleNULL	(Rule)0

#define RuleField(X)	((X)->rr_field)
#define RulePattern(X)	((X)->rr_pat_buff)
#define RuleFrame(X)	((X)->rr_frame)
#define NextRule(X)	((X)->rr_next)

/*****************************************************************************/
/* Internal Programs							     */
/*===========================================================================*/

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 int is_selected _ARG((Record rec));		   /* rewrite.c              */
 static Rule new_rule _ARG((char *field,char *pattern,char *frame,int casep));/* rewrite.c*/
 static char * check_regex _ARG((char *field,char *value,Rule rule,Record rec));/* rewrite.c*/
 static char * repl_regex _ARG((char *field,char *value,Rule rule,Record rec));/* rewrite.c*/
 static void add_rule _ARG((char *s,Rule *rp,Rule *rp_end,int casep));/* rewrite.c*/
 static void rewrite_1 _ARG((char *frame,StringBuffer *sb,char *match,Record rec));/* rewrite.c*/
 void add_check_rule _ARG((char *s));		   /* rewrite.c              */
 void add_extract _ARG((char *s));		   /* rewrite.c              */
 void add_field _ARG((char *spec));		   /* rewrite.c              */
 void add_rewrite_rule _ARG((char *s));		   /* rewrite.c              */
 void delete_field _ARG((char *token));		   /* rewrite.c              */
 void remove_field _ARG((char *field,Record rec)); /* rewrite.c              */
 void rewrite_record _ARG((Record rec));	   /* rewrite.c              */
 void save_regex _ARG((char *s));		   /* rewrite.c              */

/*****************************************************************************/
/* External Programs							     */
/*===========================================================================*/



/*****************************************************************************/
/***			  Field Add/Delete Section			   ***/
/*****************************************************************************/

 static Macro addlist = MacNull;
 static Macro dellist = MacNull;

/*-----------------------------------------------------------------------------
** Function:	add_field()
** Purpose:	Save a token and value for addition.
** Arguments:
**	spec	A string of the form
**		  token=value
** Returns:	nothing
**___________________________________________________			     */
void add_field(spec)				   /*			     */
  char *spec;					   /*			     */
{ register char *field, *value;			   /*			     */
						   /*			     */
  (void)sp_open(spec);				   /*			     */
  if ( (field = SParseSymbol(&spec)) == NULL )	   /*			     */
    return;					   /*			     */
  (void)SParseSkip(&spec);			   /*			     */
  if ( (value=SParseValue(&spec)) == NULL )	   /*			     */
    return;					   /*			     */
  (void)SParseEOS(&spec);			   /*			     */
						   /*			     */
  addlist = new_macro(field,value,addlist,0);	   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	delete_field()
** Purpose:	Save token for deletion.
** Arguments:
**	token	Name of the field to delete
** Returns:	nothing
**___________________________________________________			     */
void delete_field(token)			   /*			     */
  char *token;					   /*			     */
{ register char *field;				   /*			     */
						   /*			     */
  (void)sp_open(token);				   /*			     */
  if ( (field = SParseSymbol(&token)) == NULL )	   /*			     */
    return;					   /*			     */
  (void)SParseEOS(&token);			   /*			     */
						   /*			     */
  dellist = new_macro(field,(char*)0,dellist,0);   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	remove_field()
** Purpose:	Remove the given field from record.
** Arguments:
**	field
**	rec
** Returns:	nothing
**___________________________________________________			     */
void remove_field(field,rec)			   /*			     */
  register char *field;				   /*			     */
  Record	rec;				   /*			     */
{ register int	i;				   /*			     */
						   /*			     */
  for (i=0;i<RecordFree(rec);i+=2 )		   /*			     */
  { if ( field == RecordHeap(rec)[i] )		   /* compare symbols	     */
    { RecordHeap(rec)[i] = (char*)0; }		   /*			     */
  }						   /*			     */
						   /*			     */
  while ( RecordFree(rec) > 0 &&		   /* Adjust Heap Length     */
	  RecordHeap(rec)[RecordFree(rec)-2] == NULL )/*		     */
  { RecordFree(rec) -= 2;			   /*			     */
  }						   /*			     */
}						   /*------------------------*/


/*****************************************************************************/
/***				 Rule Section				   ***/
/*****************************************************************************/

#ifdef REGEX
 static struct re_registers reg;		   /*			     */
#endif

/*-----------------------------------------------------------------------------
** Function:	new_rule()
** Purpose:	Allocate a new Rule and fill some slots
** Arguments:
**	field
**	pattern
**	frame
**	casep
** Returns:	A pointer to the allocated structure or NULL upon failure.
**___________________________________________________			     */
static Rule new_rule(field,pattern,frame,casep)	   /*			     */
  char		*field;				   /*			     */
  char		*pattern;			   /*			     */
  char		*frame;				   /*			     */
  int		casep;				   /*			     */
{ register Rule new;				   /*			     */
						   /*			     */
  if ( (new=(Rule)malloc(sizeof(SRule))) == RuleNULL )/*		     */
  { OUT_OF_MEMORY("rewite rule"); } 		   /*			     */
						   /*			     */
  RuleField(new)   = field;			   /*			     */
  RuleFrame(new)   = frame;			   /*			     */
  NextRule(new)	   = RuleNULL;			   /*			     */
#ifdef REGEX
  if ( (RulePattern(new).buffer = (unsigned char *)malloc(16)) == NULL )/*   */
  { OUT_OF_MEMORY("pattern"); }	   		   /*			     */
  RulePattern(new).allocated = 16;		   /*			     */
  RulePattern(new).syntax    = RE_SYNTAX_EMACS;	   /*			     */
  RulePattern(new).translate = (casep?trans_lower:NULL);/*		     */
  RulePattern(new).fastmap   = NULL;		   /*			     */
						   /*			     */
  field = (char*)re_compile_pattern(pattern,	   /*			     */
				    strlen(pattern),/*			     */
				    &RulePattern(new) );/*		     */
						   /*			     */
  if ( field ) { Err(field); return NULL; }	   /*			     */
#endif
  return new;					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	add_rule()
** Purpose:	Generic addition of a rule to a list of rules.
** Arguments:
**	s
**	rp
**	rp_end
**	casep
** Returns:	nothing
**___________________________________________________			     */
static void add_rule(s,rp,rp_end,casep)		   /*			     */
  char		*s;				   /*			     */
  Rule		*rp;				   /*			     */
  Rule		*rp_end;			   /*			     */
  int		casep;				   /*			     */
{ char		*field;				   /*			     */
  char		*pattern;			   /*			     */
  char		*frame;				   /*			     */
  Rule		rule;				   /*			     */
  int		sp;				   /*                        */
  int		stackp;				   /* stack pointer for the  */
  static char **stack;				   /* local stack of fields  */
  static int    stacksize = 0;			   /*                        */

  if ( stacksize == 0 )
  { stacksize++;
    if ((stack=(char**)malloc(sizeof(char*)))==NULL)
    { OUT_OF_MEMORY("rule stack"); }
  }
  stackp = 0;
						   /*			     */
  DebugPrint2("Parsing ",s);	   		   /*			     */
  (void)sp_open(s);				   /*			     */
  (void)SParseSkip(&s);				   /*			     */
						   /*			     */
  while (*s && *s != '"')			   /*                        */
  { if ( (field=SParseSymbol(&s)) == NULL)	   /*                        */
    { return; }					   /*                        */
    DebugPrint2("field   = ",field);	   	   /*			     */
						   /*			     */
    (void)SParseSkip(&s);			   /*                        */
    if ( stackp > stacksize )			   /*                        */
    { stacksize += 4;				   /*                        */
      if ( (stack=(char**)realloc(stack,stacksize*sizeof(char*)))==NULL)/*   */
      { OUT_OF_MEMORY("rule stack"); }		   /*                        */
    }						   /*                        */
    stack[stackp++] = field;			   /*                        */
  }						   /*                        */
						   /*			     */
  if ( (pattern=SParseUnquotedString(&s)) == NULL )/*			     */
    return;					   /*			     */
						   /*			     */
  DebugPrint2("pattern = ",pattern);		   /*			     */
						   /*			     */
  (void)SParseSkip(&s);				   /*			     */
						   /*			     */
  if ( *s == '\0' )				   /*			     */
  { frame = (char*)NULL; }			   /*			     */
  else if ( (frame=SParseUnquotedString(&s)) == NULL )/*		     */
  { return; }					   /*			     */
  else						   /*			     */
  { (void)SParseEOS(&s); }			   /*			     */
    DebugPrint2("frame	 = ",frame);		   /*			     */
						   /*			     */
  if ( stackp == 0 )				   /* No field specified.    */
  { rule = new_rule(NULL,pattern,frame,casep);	   /*			     */
    if ( *rp == RuleNULL ) { *rp = *rp_end = rule; }/*			     */
    else { NextRule(*rp_end) = rule; *rp_end = rule;}/*			     */
    return;					   /*                        */
  }						   /*                        */

  for ( sp=0; sp<stackp; sp++ )			   /*                        */
  { rule = new_rule(stack[sp],pattern,frame,casep);/*			     */
    if ( *rp == RuleNULL ) { *rp = *rp_end = rule; }/*			     */
    else { NextRule(*rp_end) = rule; *rp_end = rule;}/*			     */
  }						   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	rewrite_1()
** Purpose:	
**		
**
** Arguments:
**	frame
**	sb
**	match
**	rec
** Returns:	nothing
**___________________________________________________			     */
static void rewrite_1(frame,sb,match,rec)	   /*			     */
  register char		*frame;			   /*			     */
  register StringBuffer *sb;			   /*			     */
  char			*match;			   /*			     */
  Record		rec;			   /*			     */
{						   /*			     */
  for ( ; *frame; frame++ )			   /*			     */
  { if ( *frame != '\\' )			   /* Transfer normal	     */
    { (void)sbputchar(*frame,sb); }		   /*	characters.	     */
    else					   /*			     */
    {						   /*			     */
      switch ( *++frame )			   /*			     */
      { case '1': case '2': case '3':		   /*			     */
	case '4': case '5': case '6':		   /*			     */
	case '7': case '8': case '9':		   /*			     */
#ifdef REGEX
	  { int i = *frame - '0';		   /* Look for register no   */
	    int e = reg.end[i];			   /* get end of match	     */
						   /*			     */
	    for ( i=reg.start[i]; i<e; ++i )	   /* transfer from start    */
	    { (void)sbputchar(match[i],sb); }	   /*	to end of match.     */
	  }					   /*			     */
#endif
	  break;				   /*			     */
	case '$':				   /*			     */
	  (void)sbputs(*RecordHeap(rec),sb);	   /*			     */
	  break;				   /*			     */
	case '@':				   /*			     */
	  (void)sbputs(EntryName(RecordToken(rec)),sb);/*		     */
	  break;				   /*			     */
	case 'n': (void)sbputchar('\n',sb); break; /*			     */
	case 't': (void)sbputchar('\t',sb); break; /*			     */
	default:				   /*			     */
	  if ( *frame ) (void)sbputchar(*frame,sb);/* Use '\\' as quote	     */
	  else --frame;				   /* or ignore at end of str*/
      }						   /*			     */
    }						   /*			     */
  }						   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	repl_regex()
** Purpose:	
**		
**
** Arguments:
**	field
**	value
**	rule
**	rec
** Returns:	
**___________________________________________________			     */
static char * repl_regex(field,value,rule,rec)	   /*			     */
  register char		*field;			   /*			     */
  register char		*value;			   /*			     */
  register Rule		rule;			   /*			     */
  Record		rec;			   /*			     */
{						   /*			     */
#ifndef REGEX
  return value;					   /*			     */
#else
  char			c;			   /*			     */
  int			len;			   /*			     */
  StringBuffer		*sp;			   /* intermediate pointer   */
  int			once_more;		   /*                        */
  int			limit;			   /* depth counter to break */
 						   /*  out of infinite loops */
  static StringBuffer	*s1 = 0L;		   /*			     */
  static StringBuffer	*s2 = 0L;		   /*			     */
						   /*			     */
  if ( rule == RuleNULL ) return value;		   /*			     */
						   /*			     */
  if ( s1 == NULL ) { s1 = sbopen(); s2 = sbopen(); }/*			     */
  else		    { sbrewind(s1);  sbrewind(s2);  }/*			     */
						   /*			     */
  (void)sbputs(value,s1);			   /*			     */
  value     = sbflush(s1);			   /*			     */
  len	    = strlen(value);			   /*			     */
  limit     = rsc_rewrite_limit;		   /*			     */
  once_more = TRUE;				   /*                        */
    					   	   /*			     */
  while ( once_more ) 				   /*			     */
  {						   /*			     */
    once_more = FALSE;				   /*                        */
    while ( rule != RuleNULL )			   /*                        */
    { if ( (   RuleField(rule) == NULL		   /*			     */
	    || RuleField(rule) == field )	   /*			     */
	  && re_search(&RulePattern(rule),	   /*			     */
		       value,len,0,len-1,&reg) >=0 /*			     */
	  )					   /*			     */
      {						   /*			     */
	if ( 0 > --limit )			   /*                        */
	{ WARNING("Rewrite limit exceeded.");	   /*                        */
	  break;				   /*                        */
	}					   /*                        */
	if ( RuleFrame(rule) == (char*)NULL )	   /*			     */
	{ return (char*)NULL; }			   /*			     */
						   /*			     */
	if ( reg.start[0] > 0 )			   /*			     */
	{ c = value[reg.start[0]];		   /* Push initial segment   */
	  value[reg.start[0]] = '\0';		   /*			     */
	  (void)sbputs(value,s2);		   /*			     */
	  value[reg.start[0]] = c;		   /*			     */
	}					   /*			     */
						   /*			     */
	rewrite_1(RuleFrame(rule),s2,value,rec);   /*			     */
						   /*			     */
	(void)sbputs(value+reg.end[0],s2);	   /* Transfer the end.	     */
						   /*			     */
	value = sbflush(s2);			   /* update the value	     */
	len   = strlen(value);			   /*  and its length	     */
	sp = s1; s1 = s2; s2 = sp;		   /* rotate the two string  */
	sbrewind(s2);				   /*  buffers and reset     */
      						   /*  the destination.      */
      	once_more = TRUE;			   /*                        */
      }						   /*                        */
      else					   /*                        */
      { rule = NextRule(rule);			   /*                        */
	limit = rsc_rewrite_limit;		   /*			     */
      }						   /*                        */
    }						   /*                        */
  }						   /*			     */
  return value;					   /* return the result.     */
#endif
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	check_regex()
** Purpose:	
**		
**
** Arguments:
**	field
**	value
**	rule
**	rec
** Returns:	
**___________________________________________________			     */
static char * check_regex(field,value,rule,rec)	   /*			     */
  register char		*field;			   /*			     */
  register char		*value;			   /*			     */
  register Rule		rule;			   /*			     */
  Record		rec;			   /*			     */
{						   /*			     */
#ifdef REGEX
  int			len;			   /*			     */
  static StringBuffer	*s2 = 0L;		   /*			     */
						   /*			     */
  if ( rule == RuleNULL ) return value;		   /*			     */
						   /*			     */
  if ( s2 == NULL ) { s2 = sbopen(); }		   /*			     */
  else		    { sbrewind(s2);  }		   /*			     */
						   /*			     */
  for ( len  =	strlen(value);			   /* Loop through all rules */
	rule != RuleNULL;			   /*			     */
	rule =	NextRule(rule) )		   /*			     */
  {						   /*			     */
    if ( (   RuleField(rule) == NULL		   /*			     */
	  || RuleField(rule) == field )		   /*			     */
	&& re_search(&RulePattern(rule),	   /*			     */
		    value,len,0,len-1,&reg) >=0	   /*			     */
       )					   /*			     */
    { if ( RuleFrame(rule) == (char*)0 )	   /*			     */
      { return (char*)NULL; }			   /*			     */
      rewrite_1(RuleFrame(rule),s2,value,rec);	   /*			     */
      value = sbflush(s2);			   /* update the value	     */
      return value;				   /*			     */
    }						   /*                        */
  }						   /*			     */
#endif
  return (char*)NULL;				   /* return the result.     */
}						   /*------------------------*/

/*---------------------------------------------------------------------------*/

 static Rule r_rule = RuleNULL;
 static Rule r_rule_end	= RuleNULL;

/*-----------------------------------------------------------------------------
** Function:	add_rewrite_rule()
** Purpose:	Save a rewrite rule for later use.
**		The main task is performed by add_rule
** Arguments:
**	s	rule to save.
** Returns:	nothing
**___________________________________________________			     */
void add_rewrite_rule(s)			   /*			     */
  register char *s;				   /*			     */
{						   /*			     */
  add_rule(s,&r_rule,&r_rule_end,rsc_case_rewrite);/*			     */
}						   /*------------------------*/

 static Rule c_rule = RuleNULL;
 static Rule c_rule_end = RuleNULL;

/*-----------------------------------------------------------------------------
** Function:	add_check_rule()
** Purpose:	Save a check rule for later use.
**		The main task is performed by add_rule
** Arguments:
**	s	rule to save.
** Returns:	nothing
**___________________________________________________			     */
void add_check_rule(s)				   /*			     */
  register char *s;				   /*			     */
{						   /*			     */
  add_rule(s,&c_rule,&c_rule_end,rsc_case_check);  /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	rewrite_record()
** Purpose:	Apply deletions, checks, additions, and rewriting steps
**		in that order.
** Arguments:
**	rec	Actual record to apply things to.
** Returns:	nothing
**___________________________________________________			     */
void rewrite_record(rec)			   /*			     */
  register Record rec;				   /*			     */
{ register int	  i;				   /*			     */
  register char	  **hp;				   /* heap pointer	     */
  register char	  *cp;				   /*			     */
  register Macro  mac;				   /*			     */
						   /*			     */
  for ( mac=dellist;				   /* Delete all items in    */
	mac;					   /*  the delete list	     */
	mac=NextMacro(mac) )			   /*			     */
  { remove_field(MacroName(mac),rec); }		   /*			     */
						   /*			     */
  if ( c_rule != RuleNULL )			   /*			     */
  {						   /*			     */
    for ( i=RecordFree(rec), hp=RecordHeap(rec);   /*			     */
	  i > 0;				   /*			     */
	  i-=2, hp +=2	)			   /*			     */
    {						   /*			     */
      if (   *hp				   /*			     */
	  && *(hp+1)				   /*			     */
	  && (char*)NULL !=			   /*			     */
	     (cp=check_regex(*hp,*(hp+1),c_rule,rec))/*			     */
	  )					   /*			     */
      { Err(cp); }				   /*			     */
    }						   /*			     */
  }						   /*			     */
						   /*			     */
  for ( mac=addlist;				   /* Add all items in the   */
	mac;					   /*  add list		     */
	mac=NextMacro(mac) )			   /*			     */
  { push_to_master_record(MacroName(mac),	   /*			     */
			  MacroValue(mac));	   /*			     */
  }						   /*			     */
						   /*			     */
  if ( r_rule != RuleNULL )			   /*			     */
  {						   /*			     */
    for ( i=RecordFree(rec), hp=RecordHeap(rec);   /*			     */
	 i>0;					   /*			     */
	 i-=2, hp+=2  )				   /*			     */
    {						   /*			     */
      if ( *hp && *(hp+1) )			   /*			     */
      {						   /*			     */
	cp = repl_regex(*hp,*(hp+1),r_rule,rec);   /*			     */
	if ( cp == (char*)NULL )		   /*			     */
	{ *hp = *(hp+1) = (char*)NULL; }	   /*			     */
	else if ( cp != *(hp+1) )		   /*			     */
	{ *(hp+1) = symbol(cp); }		   /*			     */
      }						   /*			     */
    }						   /*			     */
  }						   /*			     */
}						   /*------------------------*/

/*---------------------------------------------------------------------------*/


 static Rule x_rule = RuleNULL;
 static Rule x_rule_end = RuleNULL;

/*-----------------------------------------------------------------------------
** Function:	add_extract()
** Purpose:	Save an extraction rule for later use.
**		The main task is performed by add_rule
** Arguments:
**	s	rule to save.
** Returns:	nothing
**___________________________________________________			     */
void add_extract(s)				   /*			     */
  register char *s;				   /*			     */
{ add_rule(s,&x_rule,&x_rule_end,!rsc_case_select);/*			     */
  rsc_select = TRUE;				   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	save_regex()
** Purpose:	Save an extraction rule for later use.
**		Only the regular expression of the rule is given as argument.
**		The fields are taken from the resource select.fields.
** Arguments:
**	s	Regular expression to search for.
** Returns:	nothing
**___________________________________________________			     */
void save_regex(s)				   /*                        */
  register char *s;				   /*                        */
{ register char *t;				   /*			     */
 						   /*			     */
  if ( (t=malloc( (size_t)strlen(s)		   /*                        */
		 +(size_t)strlen(rsc_sel_fields)   /*                        */
		 +4 )) == NULL )   		   /*			     */
  { OUT_OF_MEMORY("string"); }	   		   /*			     */
 						   /*                        */
  (void)strcpy(t,rsc_sel_fields);		   /*			     */
  (void)strcat(t," \"");			   /*			     */
  (void)strcat(t,s);				   /*			     */
  (void)strcat(t,"\"");				   /*			     */
 						   /*                        */
  add_rule(t,&x_rule,&x_rule_end,!rsc_case_select);/*			     */
 						   /*                        */
  free(t);					   /*                        */
  rsc_select = TRUE;				   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	is_selected()
** Purpose:	Boolean function to decide whether a record should be
**		considered.
**		This function selects all records in no regular
**		expression support has been enabled.
** Arguments:
**	rec	Record to look at.
** Returns:	TRUE iff the record is seleced be a regexp or none is given.
**___________________________________________________			     */
int is_selected(rec)	   		   	   /*			     */
  Record rec;			   		   /*			     */
{						   /*			     */
#ifndef REGEX
  return TRUE;				   	   /* return the result.     */
#else
  int	 len, i;				   /*			     */
  char * value;					   /*                        */
  Rule   rule;					   /*                        */
 						   /*                        */
  if ( (rule=x_rule) == RuleNULL ) return TRUE;	   /* If no rule is given    */
 						   /*  select all records.   */
  for ( ;			   		   /* Loop through all rules */
	rule != RuleNULL;			   /*			     */
	rule =	NextRule(rule) )		   /*			     */
  {						   /*			     */
    if ( RuleField(rule) == NULL )		   /*			     */
    {						   /*                        */
      for ( i=2; i < RecordFree(rec); i+=2 )   	   /*                        */
      { if ( RecordHeap(rec)[i] )		   /*                        */
	{ value = RecordHeap(rec)[i+1];		   /*                        */
	  len   = strlen(value);		   /*                        */
          if ( re_search(&RulePattern(rule),	   /*			     */
			 value,len,0,len-1,&reg) >=0 )/*		     */
	  { return TRUE; }			   /*                        */
	}					   /*                        */
      }						   /*                        */
    }
    else if ( (value=get_field(rec,RuleField(rule)))/*                       */
	     != NULL )				   /*                        */
    { len = strlen(value);			   /*                        */
      if ( re_search(&RulePattern(rule),	   /*			     */
		     value,len,0,len-1,&reg) >=0 ) /*			     */
      { return TRUE; }				   /*                        */
    }						   /*			     */
  }						   /*                        */
  return FALSE;				   	   /* return the result.     */
#endif
}						   /*------------------------*/

