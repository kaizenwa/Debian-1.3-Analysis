#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	rgntoken.c (Region Token)
 * Purpose:	Parse tokens from a region descriptor file
 * Subroutine:	check_parens()		returns: int
 * Subroutine:	burst_line()		returns: int
 * Subroutine:	next_token()		returns: *char
 * Xlib calls:	none
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		9 August 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */
#include <ctype.h>		/* toupper, isalpha, etc. */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/region.h"	/* region parsing record (reg_param) */

#define TYPECNT 9
struct reg_types {
  int code;
  char name[16];
};
static struct reg_types types[TYPECNT] = {
  COP_Annuli, "ANNULUS",
  COP_Arrow, "ARROW",
  COP_Box, "BOX",
  COP_Circle, "CIRCLE",
  COP_Ellipse, "ELLIPSE",
  COP_PieSlice, "PIE",
  COP_Point, "POINT",
  COP_Polygon, "POLYGON",
  COP_Text, "TEXT"
};


/*  Subroutine:	check_parens
 *  Purpose:	Check nesting of parentheses.  Also, turn all token
 *		separators into ascii spaces
 *  Returns:	length of parsable line up to EOL or # (comment), else -1
 */
#ifdef ANSIC
int check_parens ( char *orig, char *line )
#else
int check_parens ( orig, line )
     char *orig;	/* original line */
     char *line;	/* workspace with line copy */
#endif
{
  int i;
  int error = 0;
  int paren = 0;
  char space = ' ';
  /* check nesting of parentheses and strip all parens, tabs, and commas */
  for( i=0; (line[i] != '\0') && (line[i] != '#'); i++ ) {
    if( line[i] == ')' ) {
      paren--;
      if( paren < 0 )
	error++;
      line[i] = space;
    } else if( line[i] == '(' ) {
      paren++;
      line[i] = space;
    } else if( line[i] == '\n' ) {
      if( paren > 0 )
	line[i] = space;
      else
	break;
    } else if( (line[i] == ',') || (line[i] == '\t') ) {
      line[i] = space;
    } else if( ((line[i] == ';') || (line[i] == '&') ||
		(line[i] == '|')) && (paren != 0) )
      /* also check region separators */
      error++;
  }
  /* make termination a null (removes comments and \n) */
  line[i] = '\0';
  if( error || (paren != 0) ) {
    (void)fprintf(stderr,"WARNING: unbalanced parens:\n%s\n",orig);
    return( -1 );
  } else {
    return( i );
  }
}

/*
 * Subroutine:	burst_line
 * Purpose:	Find each region descriptor and its identifying token
 * Returns:	Number of regions found
 */
int burst_line ( orig, line, len, records )
     char *orig;		/* original line */
     char *line;		/* workspace with line copy */
     int len;			/* length of meaningful part of line */
     struct reg_param *records;	/* region parse note pad */
{
  int i;
  int starting;		/* 1=fresh line, 2=fresh & must find region, 0=found */
  int count, no_error;
  int set_region_type();

  /* clear error flag, set count to 0, clear first record */
  no_error = 1;
  count = 0;
  bzero((char *)&records[0], sizeof(struct reg_param));
  /* initial state is looking for beginning of a region descriptor */
  starting = 1;
  for( i=0; (no_error) && (i<len); i++ ) {
    /* if looking for beginning of a region description */
    if( starting ) {
      if( line[i] == '-' ) {
	/* found an exclude indicator */
	starting = 2;
	records[count].exclude = 1;
      } else if( line[i] == '!' ) {
	/* found a 'not' indicator */
	records[count].not = 1;
	starting = 2;
      } else if( line[i] != ' ' ) {
	/* found first token */
	starting = 0;
	if( set_region_type(&records[count], &line[i]) == -1 ) {
	  (void)fprintf(stderr,"%s\n",orig);
	  return(-1);
	}
	count++;
	/* clear next record */
	if( count < REG_LIMIT )
	  bzero((char *)&records[count], sizeof(struct reg_param));
	else
	  no_error = 0;
      }
    }
    /* check for region separators to restart process */
    if( (line[i] == '&') || (line[i] == '|') ) {
      if( starting )
	no_error = 0;
      starting = 2;
      records[count].connector = line[i];
      /* terminate line of parameters for first record */
      line[i] = '\0';
    } else if( line[i] == ';' ) {
      if( starting > 1 )
	no_error = 0;
      starting = 1;
      /* terminate line of parameters for first record */
      line[i] = '\0';
    }
  }
  if( no_error && (starting <= 1) ) {
    return( count );
  } else {
    (void)fprintf(stderr,"WARNING: improper region separation:\n%s\n",orig);
    return( -1 );
  }
}

/*
 * Subroutine:	next_token
 * Purpose:	Advance the line position beyond the first cnt tokens
 * Note:	All token separators are ascii spaces (see check_parens)
 * Note:	line termination is a null ('\0') or new_line ('\n')
 * Returns:	NULL if insufficient or no more tokens, else ptr to line
 */
char *next_token ( line, cnt )
     char *line;
     int cnt;
{
  int i, j;

  i = 0;
  for( j = 0; j < cnt; j++ ) {
    /* check for premature end */
    if( (line[i] == '\0') || (line[i] == '\n') )
      return( NULL );
    /* advance to token */
    while( line[i] == ' ' ) i++;
    /* advance to end of token */
    while( (line[i] != ' ') && (line[i] != '\0') && (line[i] != '\n') ) i++;
  }
  /* advance to next token */
  while( line[i] == ' ' ) i++;
  if( line[i] == '\0' )
    /* report no more tokens */
    return( NULL );
  else
    /* report location of next token */
    return( &line[i] );
}

/*
 * Subroutine:	set_region_type
 * Purpose:	Check the name for a unique match with a known region type
 * Called by:	burst_line() in RegionParse.c
 */
int set_region_type ( record, line )
     struct reg_param *record;
     char *line;
{
  int i, j;
  int match = 0;
  for( i=0; line[i] != ' '; i++ ) {
    /* convert all lower case characters to uppercase */
    if( islower(line[i]) )
      line[i] = toupper(line[i]);
    /* check for line end overrun */
    if( line[i] == '\0' ) {
      (void)fprintf(stderr,"WARNING: missing region specification: %s\n",line);
      return( -1 );
    }
  }
  /* null terminate name token */
  line[i] = '\0';
  /* point record at parameter field */
  record->line = &line[i+1];
  for( i=0; i<TYPECNT; i++ ) {
    /* do match on all input chars (for unique abbreviation check) */
    for( j=0; (line[j] != '\0') && (line[j] == types[i].name[j]); j++ );
    if( (j > 0) && (line[j] == '\0') ) {
      record->line = &line[j+1];
      record->type = types[i].code;
      match++;
    }
  }
  /* possible outcomes: match == 1, match > 1, match < 1 */
  if( match == 1 ) {
    return( 0 );
  } else if( match > 1 ) {
    (void)fprintf(stderr,
		  "WARNING: non-unique region abbreviation: %s\n",line);
  } else {
    (void)fprintf(stderr,"WARNING: unknown region type: %s\n",line);
  }
  return( -1 );
}
