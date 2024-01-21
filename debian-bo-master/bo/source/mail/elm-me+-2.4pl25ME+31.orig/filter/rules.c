
static char rcsid[] ="@(#)$Id: rules.c,v 5.9 1994/03/11 20:40:24 syd Exp $";

/*******************************************************************************
 *  The Elm Mail System  -  $Revision: 5.9 $   $State: Exp $
 *
 * 			Copyright (c) 1988-1992 USENET Community Trust
 * 			Copyright (c) 1986,1987 Dave Taylor
 *****************************************************************************/

/** This file contains all the rule routines, including those that apply the
    specified rules and the routine to print the rules out.

**/

#include "defs.h"
#include <pwd.h>

#include <fcntl.h>

#include "elmlib.h"
#include "filter.h"
#include "s_filter.h"

extern char *date_n_user();

static struct regexp *last_regexp = NULL;

static int
matches(str, relation, cond)
char *str;
int relation;
struct condition_rec *cond;
{
	if (relation == RE) {
	  if (cond->regex == NULL) cond->regex = regcomp(cond->argument1);
	  if (regexec(cond->regex, str)) {
	    last_regexp = cond->regex;
	    return TRUE;
	  } else {
	    return FALSE;
	  }
	} else {
	  return contains(str, cond->argument1);
	}
}

void
regerror(s)
char *s;
{
	if (outfd != NULL)
	  fprintf(outfd,
	    catgets(elm_msg_cat,
		    FilterSet,FilterCantCompileRegexp,
		    "filter (%s): Error: can't compile regexp: \"%s\"\n"),
		 username, s);
	if (outfd != NULL) fclose(outfd);
	exit(1); 		
}

int
action_from_ruleset()
{
	/** Given the set of rules we've read in and the current to, from, 
	    and subject, try to match one.  Return the ACTION of the match
            or LEAVE if none found that apply.
	**/

	register int iindex = 0, not, relation, try_next_rule, x;
	struct condition_rec *cond;

	while (iindex < total_rules) {
	  cond = rules[iindex].condition;
	  try_next_rule = 0;

	  while (cond != NULL && ! try_next_rule) {
	    
	    not = (cond->relation < 0);
	    relation = abs(cond->relation);
	
	    switch (cond->matchwhat) {

	      case TO     : x = matches(to, relation, cond);		break;
	      case FROM   : x = matches(from, relation, cond); 		break;
	      case SENDER : x = matches(sender, relation, cond);	break;
	      case SUBJECT: x = matches(subject, relation, cond);	break;
	      case LINES  : x = compare(lines, relation, cond);		break;
		       
	      case CONTAINS: if (outfd != NULL) fprintf(outfd,
				catgets(elm_msg_cat,
				    FilterSet,FilterContainsNotImplemented,
       "filter (%s): Error: rules based on 'contains' are not implemented!\n"),
			    date_n_user());
			    if (outfd != NULL) fclose(outfd);
			    exit(0); 		

	      case ALWAYS: not = FALSE; x = TRUE;			break;
	    }

	    if ((not && x) || ((! not) && (! x))) /* this test failed (LISP?) */
	      try_next_rule++;
	    else
	      cond = cond->next;		  /* next condition, if any?  */
	  }

	  if (! try_next_rule) {
	    rule_choosen = iindex;
 	    return(rules[rule_choosen].action);
	  }
	  iindex++;
	}

	rule_choosen = -1;
	return(LEAVE);
}

#define get_the_time()	if (!gotten_time) { 		  \
			   thetime = time( (time_t *) 0);   \
			   timerec = localtime(&thetime); \
			   gotten_time++; 		  \
			}

static struct {
	int	id;
	char	*str;
} regmessage[] = {
	FilterWholeRegsub,	"<match>",
	FilterRegsubOne,	"<submatch-1>",
	FilterRegsubTwo,	"<submatch-2>",
	FilterRegsubThree,	"<submatch-3>",
	FilterRegsubFour,	"<submatch-4>",
	FilterRegsubFive,	"<submatch-5>",
	FilterRegsubSix,	"<submatch-6>",
	FilterRegsubSeven,	"<submatch-7>",
	FilterRegsubEight,	"<submatch-8>",
	FilterRegsubNine,	"<submatch-9>",
};

void expand_macros(word, buffer, line, display, size)
     char *word, *buffer;
     int  line, display;
     int size;
{
	/** expand the allowable macros in the word;
		%d	= day of the month  
		%D	= day of the week  
	        %h	= hour (0-23)	 
		%m	= month of the year
		%n      = sender of message
		%r	= return address of sender
	   	%s	= subject of message
	   	%S	= "Re: subject of message"  (only add Re: if not there)
		%t	= hour:minute 	
		%y	= year		  
		%&	= the whole string that matched last regexp
		%1-%9	= matched subexpressions in last regexp
	    or simply copies word into buffer. If "display" is set then
	    instead it puts "<day-of-month>" etc. etc. in the output.
	**/

#ifndef	_POSIX_SOURCE
	struct tm *localtime();
	time_t    time();
#endif
	struct tm *timerec;
	time_t	thetime;
	register int i, j=0, gotten_time = 0, reading_a_percent_sign = 0,
                     len, backslashed=0, regsub;
	
	buffer[0] = '\0';

	for (i = 0, len = strlen(word); i < len; i++) {
	  if (reading_a_percent_sign) {
	    reading_a_percent_sign = 0;
	    switch (word[i]) {

	      case 'n' : 
			 if (display)
			   strfcat(buffer,catgets(elm_msg_cat,
						 FilterSet,FilterSender,
						 "<sender>"),
				   size);
			 else {
			   strfcat(buffer, "\"", size);
			   strfcat(buffer, sender, size);
			   strfcat(buffer, "\"", size);
			 }
			 j = strlen(buffer);
			 break;

	      case 'r' : 
			 if (display)
	 		   strfcat(buffer,catgets(elm_msg_cat,
						  FilterSet,
						  FilterReturnAddress,
						  "<return-address>"),
				   size);
			 else
			   strfcat(buffer, from, sizeof buffer);
	                 j = strlen(buffer);
			 break;

	      case 's' : 
			 if (display)
	 		   strfcat(buffer,catgets(elm_msg_cat,
						FilterSet,FilterSubject,
						 "<subject>"),
				   size);
			 else {
			   strfcat(buffer, "\"", size);
			   strfcat(buffer, subject, size);
			   strfcat(buffer, "\"", size);
			 }
	                 j = strlen(buffer);
			 break;

	      case 'S' : 
			 if (display)
	 		   strfcat(buffer,catgets(elm_msg_cat,
						 FilterSet,FilterReSubject,
						 "<Re: subject>"),
				   size);
			 else {
			   strfcat(buffer, "\"", size);
			   if (! the_same(subject, "Re:")) 
			     strfcat(buffer, "Re: ", size);
			   strfcat(buffer, subject, size);
			   strfcat(buffer, "\"", size);
			 }
	                 j = strlen(buffer);
			 break;

	      case 'd' : get_the_time(); 
			 if (display)
			   strfcat(buffer,catgets(elm_msg_cat,
						 FilterSet,FilterDayOfMonth,
						  "<day-of-month>"),
				   size);
			 else
			   strfcat(buffer, itoa(timerec->tm_mday,FALSE),size);
	                 j = strlen(buffer);
			 break;

	      case 'D' : get_the_time(); 
			 if (display)
			   strfcat(buffer,catgets(elm_msg_cat,
						  FilterSet,FilterDayOfWeek,
						  "<day-of-week>"),
				   size);
			 else
			   strfcat(buffer, itoa(timerec->tm_wday,FALSE),
				   size);
	                 j = strlen(buffer);
			 break;

	      case 'm' : get_the_time(); 
			 if (display)
			   strfcat(buffer,catgets(elm_msg_cat,
						  FilterSet,FilterMonth,
						  "<month>"),
				   size);
			 else
			   sprintf(&buffer[j],"%2.2d",timerec->tm_mon+1);
	                 j = strlen(buffer);
			 break;

	      case 'y' : get_the_time(); 
			 if (display)
			   strfcat(buffer,catgets(elm_msg_cat,
						  FilterSet,FilterYear,
						  "<year>"),
				   size);
			 else
			   strfcat(buffer, itoa(timerec->tm_year,FALSE),
				   size);
	                 j = strlen(buffer);
			 break;

	      case 'h' : get_the_time(); 
			 if (display)
			   strfcat(buffer,catgets(elm_msg_cat,
						  FilterSet,FilterHour,
						  "<hour>"), size);
			 else
			   strfcat(buffer, itoa(timerec->tm_hour,FALSE),
				   size);
	                 j = strlen(buffer);
			 break;

	      case 't' : get_the_time(); 
			 if (display)
			   strfcat(buffer,catgets(elm_msg_cat,
						  FilterSet,FilterTime,
						  "<time>"),
				   size);
		         else {
			   strfcat(buffer, itoa(timerec->tm_hour,FALSE), size);
			   strfcat(buffer, ":", size);
			   strfcat(buffer, itoa(timerec->tm_min,TRUE), size);
			 }
	                 j = strlen(buffer);
			 break;

	      case '&': case '1': case '2': case '3': case '4':
	      case '5': case '6': case '7': case '8': case '9':

			 if (word[i] == '&') regsub = 0;
			 else regsub = word[i] - '0';

			 if (display) {
			   strfcpy(buffer,catgets(elm_msg_cat,
						  FilterSet,
						  regmessage[regsub].id,
						  regmessage[regsub].str),
				   size);
			   j = strlen(buffer);
			 } else {
			   if (last_regexp != NULL) {
			     char *sp = last_regexp->startp[regsub];
			     char *ep = last_regexp->endp[regsub];
			     if (sp != NULL && ep != NULL && ep > sp)
			       while (sp < ep && j < size-2) 
				 buffer[j++] = *sp++;
			     buffer[j] = '\0';
			   }
			 }
			 break;

	      default  : if (outfd != NULL) fprintf(outfd,
				catgets(elm_msg_cat,
					FilterSet,FilterErrorTranslatingMacro,
   "filter (%s): Error on line %d translating %%%c macro in word \"%s\"!\n"),
			         date_n_user(), line, word[i], word);
			 if (outfd != NULL) fclose(outfd);
			 exit(1);
	    }
	  }
	  else if (word[i] == '%') {
	    if (backslashed) {
	      if (j < size-2) {
		buffer[j++] = '%';
		buffer[j] = '\0';       
	      }
	      backslashed = 0;
            } else {
	         reading_a_percent_sign++;
	    }
	  } else if (word[i] == '\\') {
	    if (backslashed) {
	      if (j < size-2) {
		buffer[j++] = '\\';
		buffer[j] = '\0';			
	      }
	      backslashed = 0;
	    } else {
		 backslashed++;
	    }
	  } else {
	    if (j < size-2) {
	      buffer[j++] = word[i];
	      buffer[j] = '\0';
	    }
	    backslashed = 0;
	  }
	}
}

print_rules()
{
	/** print the rules out.  A double check, of course! **/

	register int i = -1;
	char     *whatname(), *actionname();
	struct   condition_rec *cond;

	if (outfd == NULL) return;	/* why are we here, then? */

	while (++i < total_rules) {
	  if (rules[i].condition->matchwhat == ALWAYS) {
	    fprintf(outfd,catgets(elm_msg_cat,FilterSet,FilterAlways,
			"\nRule %d:  ** always ** \n\t%s %s\n"), i+1,
		 actionname(rules[i].action), rules[i].argument2);
	    continue;
	  }

	  fprintf(outfd,catgets(elm_msg_cat,FilterSet,FilterRuleIf,
				"\nRule %d:  if ("), i+1);

	  cond = rules[i].condition;

	  while (cond != NULL) {
	    if (cond->relation < 0)
	      fprintf(outfd,catgets(elm_msg_cat,FilterSet,FilterNot,
				    "not %s %s %s%s%s"), 
		      whatname(cond->matchwhat),
		      relationname(- (cond->relation)),
		      quoteit(cond->matchwhat, -cond->relation),
		      cond->argument1,
		      quoteit(cond->matchwhat, -cond->relation));
	    else
	      fprintf(outfd, "%s %s %s%s%s",
		      whatname(cond->matchwhat),
		      relationname(cond->relation),
		      quoteit(cond->matchwhat, cond->relation),
		      cond->argument1,
		      quoteit(cond->matchwhat, cond->relation));

	    cond = cond->next;

	    if (cond != NULL)
	      fprintf(outfd,catgets(elm_msg_cat,
				    FilterSet,FilterAnd,
				    " and "));
	  }
	    
	  fprintf(outfd,catgets(elm_msg_cat,FilterSet,FilterThen,
				") then\n\t  %s %s\n"), 
		 actionname(rules[i].action), rules[i].argument2);
	}
	fprintf(outfd, "\n");
}

char *whatname(n)
int n;
{
	static char buffer[10];

	switch(n) {
	  case FROM   : return("from");
	  case TO     : return("to");
	  case SENDER : return("sender");
	  case SUBJECT: return("subject");
	  case LINES  : return ("lines");
	  case CONTAINS: return("contains");
	  default     : sprintf(buffer, "?%d?", n); return((char *)buffer);
	}
}

char *actionname(n)
int n;
{
	switch(n) {
	  case DELETE_MSG : return(catgets(elm_msg_cat,
					   FilterSet,FilterDelete,
					   "Delete"));
	  case SAVE       : return(catgets(elm_msg_cat,
					   FilterSet,FilterSave,
					   "Save"));
	  case SAVECC     : return(catgets(elm_msg_cat,
					   FilterSet,FilterCopyAndSave,
					   "Copy and Save"));
	  case FORWARD    : return(catgets(elm_msg_cat,
					   FilterSet,FilterForward,
					   "Forward"));
	  case FORWARDC   : return(catgets(elm_msg_cat,
					   FilterSet,FilterCopyAndForward,
					   "Copy and Forward"));
	  case LEAVE      : return(catgets(elm_msg_cat,
					   FilterSet,FilterLeave,
					   "Leave")); 
	  case EXEC       : return(catgets(elm_msg_cat,
					   FilterSet,FilterExecute,
					   "Execute"));
	  case EXECC       : return(catgets(elm_msg_cat,
					   FilterSet,FilterExecuteAndSave,
					   "Execute and Save"));
	  default         : return(catgets(elm_msg_cat,
					   FilterSet,FilterAction,
					   "?action?"));
	}
}

int
compare(line, relop, cond)
int line, relop;
struct condition_rec *cond;
{
	/** Given the actual number of lines in the message, the relop
	    relation, and the number of lines in the rule, as a string (!),
   	    return TRUE or FALSE according to which is correct.
	**/

	int rule_lines;

	rule_lines = atoi(cond->argument1);

	switch (relop) {
	  case LE: return(line <= rule_lines);
	  case LT: return(line <  rule_lines);
	  case GE: return(line >= rule_lines);
	  case GT: return(line >  rule_lines);
	  case NE: return(line != rule_lines);
	  case EQ: return(line == rule_lines);
	}
	return(-1);
}
