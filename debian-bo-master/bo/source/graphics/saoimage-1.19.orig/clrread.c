#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	clrread.c (Color Read)
 * Purpose:	Read and Write file and internally stored color tables
 * Subroutine:	parse_color_file()		returns: int
 * Xlib calls:	none
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  21 Nov 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */
#include <ctype.h>		/* toupper, isalpha, etc. */
#include "hfiles/colormap.h"	/* color table storage structs */
#include "hfiles/define.h"	/* define SZ_LINE, etc. */

/*
 * Subroutine:	parse_color_file
 * Purpose:	Parse values after color identifier
 */
int parse_color_file ( fp, ctable, max_entries )
     ColorTable *ctable;
     int max_entries;
     FILE *fp;
{
  int len, i;
  int line_num;
  char line[SZ_LINE];
  char cbuf[SZ_LINE];
  struct SubTable *table;
  static int parse_color_table(), advance_to_ascii();

  line_num = 0;
  /* advance to first non-comment line */
  len = advance_to_ascii(fp, line, cbuf, SZ_LINE, &line_num, 1);
  if( len == 0 )
    return( 0 );
  if( (len < 0) || (len != 11) ||
      (strncmp(cbuf, "PSEUDOCOLOR", 11) != 0) ) {
    (void)fprintf(stderr,
		  "ERROR: pseudocolor keyword not found at head of file\n");
    return( 0 );
  }
  /* advance to next non-comment line */
  len = advance_to_ascii(fp, line, cbuf, SZ_LINE, &line_num, 1);
  for( i=0; i<3; i++ ) {
    switch( len ) {
    case 0:
      return( 0 );
    case 3:
      if( strncmp(cbuf, "RED", 3) != 0 )
	table = NULL;
      else
	table = &ctable->red;
      break;
    case 4:
      if( strncmp(cbuf, "BLUE", 4) != 0 )
	table = NULL;
      else
	table = &ctable->blue;
      break;
    case 5:
      if( strncmp(cbuf, "GREEN", 5) != 0 )
	table = NULL;
      else
	table = &ctable->green;
      break;
    default:
      table = NULL;
    }
    if( table == NULL ) {
      (void)fprintf(stderr,
		    "ERROR: unrecognized keyword %s in line %d\n  %s\n",
		    cbuf, line_num, line);
      return( 0 );
    } else {
      if( (len = parse_color_table(fp, line, cbuf, len + 1, SZ_LINE,
				   &line_num, table, max_entries, i))
	 < 0 )
	return( 0 );
    }
  }
  return( 1 );
}

/*
 * Subroutine:	parse_color_table
 * Purpose:	Read color table entries for one color
 * Note:	Values may start with word "gamma" and its value
 */
static int parse_color_table ( fp, line, cbuf, i, len, line_num,
			       farb, max_entries, color_cnt )
     FILE *fp;
     char *line;		/* i/l: line as read from file */
     char *cbuf;		/* i/l: buffer for processing string */
     int i;			/* i/l: index in working buffer of token */
     int len;			/* i: length of character buffers */
     int *line_num;		/* i/o: line number in file */
     struct SubTable *farb;	/* i/o: structure to get color table info */
     int max_entries;		/* i: maximum number of entries in table */
     int color_cnt;		/* i: number of colors done so far */
{
  float level, intensity;
  char *level_token, *intensity_token;
  int status, entry;
  static int advance_to_ascii(), prep_alpha();
  int check_parens();
  char *next_token();

  status = 0;
  intensity_token = NULL;
  /* first check rest of this line */
  if( (cbuf[i] != '\0') && (cbuf[i] != '#') && (cbuf[i] != '\n') ) {
    /* strip of parens and other non-space spacers */
    if( check_parens(line, cbuf + i) < 0 )
      return( -1 );
    /* are there any tokens? */
    intensity_token = next_token(cbuf + i, 1);
  }
  /* else look for a later line */
  if( intensity_token == NULL ) {
    i = 0;
    status = advance_to_ascii(fp, line, cbuf, len, line_num, 1);
    if( status == 0 )
      return( -1 );
    /* prepare for scanf parsing, even though it starts with alpha */
    if( status > 0 ) {
      if( check_parens(line, cbuf) < 0 )
	return( -1 );
    }
    if( (intensity_token = next_token(cbuf + i, 1)) == NULL) {
      (void)fprintf(stderr, "ERROR: parse error at line %d or %d\n  %s\n",
		    *line_num - 1, *line_num, line);
      return( -1 );
    }
  }
  /* first look for gamma */
  /* don't do this prep if we know line starts with alpha */
  if( status <= 0 ) {
    /* this will be a check-paren'd spaced out line */
    while( cbuf[i] == ' ' ) i++;
    if( isalpha(cbuf[i]) )
      /* aha! */
      status = prep_alpha(cbuf + i, 10);
  }
  /* do we have alpha? */
  if( status > 0 ) {
    if( (status == 5) && (strncmp(cbuf+i, "GAMMA", 5) == 0) &&
        (sscanf(intensity_token, "%f", &intensity) == 1) ) {
      farb->gamma = intensity;
      if( intensity != 1.0 )
	farb->do_gamma = 1;
      level_token = next_token(intensity_token, 1);
    } else {
      (void)fprintf(stderr, "ERROR: parse error in line %d\n  %s\n",
		    *line_num, line);
      return( -1 );
    }
  } else
    level_token = cbuf + i;

  i = 0;
  while( i < max_entries ) {
    while( level_token != NULL ) {
      if( (intensity_token = next_token(level_token, 1)) == NULL ) {
	(void)fprintf(stderr,"ERROR: odd number of tokens\n");
	return( -1 );
      }
      if( sscanf(level_token, "%f %f", &level, &intensity) != 2 ) {
	(void)fprintf(stderr,"ERROR: could not parse floating values\n  %s\n",
		      line);
	return( -1 );
      }
      if( (i > 0) && (level < farb->cell_level[i-1]) ) {
	(void)fprintf
	  (stderr,"ERROR: levels not consecutive and assending (%.2f)\n  %s\n",
	   level, line);
	return( -1 );
      } else if( (intensity < 0.0) || (intensity > 1.0) ) {
	(void)fprintf(stderr,
		      "ERROR: intensity not between 0 and 1 (%.2f)\n  %s\n",
		      intensity, line);
	return( -1 );
      }
      farb->cell_level[i] = level;
      farb->intensity[i] = intensity;
      /* too many entries for table size? */
      if( ++i >= max_entries ) {
	(void)fprintf(stderr, "ERROR: too many entries defined\n");
	return( -1 );
      }
      level_token = next_token(intensity_token, 1);
    }
    /* advance to next non-comment line */
    status = advance_to_ascii(fp, line, cbuf, len, line_num, 0);
    /* check for termination conditions (text token or end of file) */
    if( status >= 0 ) {
      if( i < 2 )
	return( -1 );
      else {
	/* error if real parsing error, or not last color */
	if( (status == 0) && ((*line_num != 0) || (color_cnt < 2)) )
	  return( -1 );
	else {
	  /* this is the good return */
	  farb->vertex_cnt = i;
	  return( status );
	}
      }
    } else
      level_token = cbuf;
  }
  return( -1 );
}

/*
 * Subroutine:	advance_to_ascii
 * Purpose:	Get next processable line, and prepare a copy for processing
 * Returns:	Positive character count if string starts with a name
 *		Negative val if string starts with a number or paren
 *		0 if file ended, or first character was neither char nor num
 */
static int advance_to_ascii ( fp, line, scratch, len, line_num, dont_end )
     FILE *fp;		/* i: file descriptor */
     char *line;	/* i: buffer to get entire line */
     char *scratch;	/* i/o: buffer to get string ready for processing */
     int len;		/* i: length of buffers */
     int *line_num;	/* i/o: current line number */
     int dont_end;	/* flag that more is definitely expected */
{
  int i;
  static int find_token(), prep_alpha();
  int check_parens();

  /* advance to first non-comment line */
  do {
    if( fgets(line, len, fp) == NULL ) {
      if( dont_end )
	(void)fprintf(stderr, "ERROR: premature end of file at line %d\n",
		      *line_num);
      else
	*line_num = 0;
      return( 0 );
    }
    ++(*line_num);
    if( (i = find_token(line, len)) >= 0 ) {
      line[len-1] = '\0';
      (void)strcpy(scratch, &line[i]);
      if( isalpha(line[i]) )
	return( prep_alpha(scratch, len - i) );
      else if( (line[i] == '(') || (line[i] == '.') || isdigit(line[i]) ) {
	if( (i = check_parens(line, scratch)) < 0 )
	  return( 0 );
	else
	  return( -i );
      } else {
	(void)fprintf(stderr, "ERROR: could not parse line %d:\n  %s\n",
		      *line_num, line);
	return( 0 );
      }
    }
  } while( *line_num < 1000 );
  (void)fprintf(stderr, "ERROR: file has too many lines to be reasonable\n");
  return( 0 );
}

/*
 * Subroutine:	find_token
 * Purpose:	Point at first non-space character if it might be parsable
 * Returns:	index of character, or -1 if line ends or is comment
 */
static int find_token ( line, len )
     char *line;
     int len;
{ 
  int i = 0;
  /* advance to next first non-space character */
  while( (line[i] == ' ') || (line[i] == '\t') ) {
    if( ++i >= len )
      return( -1 );
  }
  if( (line[i] == '\0') || (line[i] == '#') || (line[i] == '\n') )
    return( -1 );
  else
    return( i );
}

/*
 * Subroutine:	prep_alpha
 * Purpose:	Convert all characters to upper case and null terminate word
 * Returns:	Number of characters in the word
 */
static int prep_alpha ( line, len )
     char *line;
{ 
  int i = 0;
  /* advance to next first non-space character */
  while( (i < len) && (isalpha(line[i])) ) {
    if( islower(line[i]) )
      line[i] = toupper(line[i]);
    i++;
  }
  if( line[i] != '\0' )
    line[i] = ' ';
  return( i );
}
