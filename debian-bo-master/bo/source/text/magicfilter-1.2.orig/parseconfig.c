/*
 * parseconfig.c
 * 
 * Routine library for parsing the configuration file
 *
 * Copyright © 1995 H. Peter Anvin
 */

#include "magicfilter.h"

/*
 * int getoffset(char *startpos, char **endpos)
 *
 * Returns the parsing offset (a decimal, octal or hex number, or "default").
 * Returns the number, or one of the negative numbers
 *    MAG_DEFAULT      ... is default
 *    MAG_COMMENT      ... comment line
 *    MAG_ERR          ... invalid offset
 *
 * *endpos is set to the first character after the offset, unless NULL
 */

int getoffset(char *p, char **endpos)
{
  int offset = 0;		/* So far */
  enum { st_start, st_zero, st_oct, st_dec, st_hex, st_done } state = st_start;

#if DEBUG > 3
  fprintf(stderr,"getoffset %s\n", p);
#endif

  while ( isspace(*p) ) p++;	/* Skip leading whitespace */

  while ( state != st_done )
    {
      switch( state )
	{
	case st_start:
	  switch ( *p )
	    {
	    case '\b':		/* Whitespace */
	    case '\t':
	    case '\n':
	    case '\v':
	    case '\f':
	    case '\r':
	    case ' ':
	      p++;
	      break;
	      
	    case '#':
	    case '\0':		/* Blank line counts as comment */
	      offset = MAG_COMMENT;
	      state = st_done;
	      break;

	    case '0':
	      state = st_zero;
	      p++;
	      break;

	    case '1':
	    case '2':
	    case '3':
	    case '4':
	    case '5':
	    case '6':
	    case '7':
	    case '8':
	    case '9':
	      offset = *p - '0';
	      state = st_dec;
	      p++;
	      break;
	      
	    case 'd':
	      if ( strncmp(p, "default", 7) )
		{
		  offset = MAG_ERR;
		}
	      else
		{
		  p += 7;
		  offset = MAG_DEFAULT;
		}
	      state = st_done;
	      break;

	    default:
	      offset = MAG_ERR;
	      state = st_done;
	      break;
	    }
	  break;

	case st_zero:
	  if ( *p == 'x' || *p == 'X' )
	    {
	      state = st_hex;
	      p++;
	      break;
	    }
	  /* else fall through */

	case st_oct:
	  if ( *p < '0' || *p > '7' )
	    {
	      state = st_done;
	    }
	  else
	    {
	      offset <<= 3;
	      offset += *(p++) - '0';
	    }
	  break;

	case st_dec:
	  if ( *p < '0' || *p > '9' )
	    {
	      state = st_done;
	    }
	  else
	    {
	      offset *= 10;
	      offset += *(p++) - '0';
	    }
	  break;

	case st_hex:
	  if ( *p >= '0' && *p <= '9' )
	    {
	      offset <<= 4;
	      offset += *(p++) - '0';
	    }
	  else if ( *p >= 'A' && *p <= 'F' )
	    {
	      offset <<= 4;
	      offset += *(p++) - 'A' + 10;
	    }
	  else if ( *p >= 'a' && *p <= 'f' )
	    {
	      offset <<= 4;
	      offset += *(p++) - 'a' + 10;
	    }
	  else
	    {
	      state = st_done;
	    }
	}
    }

  if ( endpos )
    *endpos = p;		/* Where did we end? */

  return offset;
}

/*
 * int getmagic(char *startpos, char **endpos, char *magic, char *mask)
 *
 * Parse a magic string, returning the number of bytes in the magic string.
 * Stores the endpoint in *endpos if requested.  If magic and/or mask
 * is NULL, do not store; used to count the number of characters in the
 * magic/mask string.
 *
 * Each character in mask is either 0 for \? (don't care) or (char)-1 for
 * any other (do care).
 *
 * If mask is set to the senitel value NO_WILD (a unique pointer value
 * pointing to a bogus static string) the \? will not be interpreted
 * as don't care; just as a regular escaped questionmark.
 *
 */
char NO_WILD[1] = "";		/* Used as senitel value */

int getmagic(char *p, char **endpos, char *magic, char *mask)
{
  int count = 0;		/* Count of bytes */
  int quote = 0;		/* Quoted? */
  int ch;
  int wildcards = 1;		/* Wildcards allowed by default */
  enum { st_def, st_backsl, st_oct, st_hex, st_done } state = st_def;

#if DEBUG > 3
  fprintf(stderr,"getmagic %s\n", p);
#endif

  if ( mask == NO_WILD )	/* If wildcards forbidden */
    {
      mask = NULL;		/* Don't store a mask */
      wildcards = 0;		/* Don't accept wildcards */
    }

  while ( isspace(*p) ) p++;	/* Skip leading whitespace */
  
  while ( state != st_done )
    {
      switch ( state )
	{
	case st_def:
	  switch ( *p )
	    {
	    case '\n':		/* Unconditional whitespace */
	    case '\r':
	    case '\f':
	    case '\0':
	      if ( quote )
		count = MAG_ERR;
	      state = st_done;
	      break;

	    case '\\':		/* Backslash */
	      p++;
	      state = st_backsl;
	      break;

	    case '\"':		/* Quote */
	      p++;
	      quote = !quote;
	      break;

	    case '\b':		/* Whitespace */
	    case '\t':
	    case '\v':
	    case ' ':
	      if ( !quote )
		{
		  state = st_done;
		  break;
		}
	      /* else fall though */

	    default:
	      if ( magic ) *(magic++) = *p;
	      if ( mask ) *(mask++) = (char)(-1);
	      p++; count++;
	      break;
	    }
	  break;
	  
	case st_backsl:		/* After backslash */
	  ch = -1;
	  switch ( *p )
	    {
	    case 'a':
	      ch = 7;		/* Bell */
	      break;
	    case 'b':
	      ch = '\b';	/* Backspace */
	      break;
	    case 'e':
	      ch = 27;		/* Escape */
	      break;
	    case 'f':
	      ch = '\f';	/* Form feed */
	      break;
	    case 'n':
	      ch = '\n';	/* Newline */
	      break;
	    case 'r':
	      ch = '\r';	/* Return */
	      break;
	    case 't':
	      ch = '\t';	/* Tab */
	      break;
	    case 'v':
	      ch = '\v';	/* Vtab */
	      break;
	    case 'x':
	    case 'X':
	      state = st_hex;	/* Hex prefix */
	      ch = 0;
	      break;
	    case '0':
	    case '1':
	    case '2':
	    case '3':
	    case '4':
	    case '5':
	    case '6':
	    case '7':
	      state = st_oct;	/* Octal prefix */
	      ch = *p - '0';
	      break;
	    case '?':
	      if ( wildcards )
		{
		  if ( magic ) *(magic++) = (char)0; /* Ignore byte */
		  if ( mask ) *(mask++) = (char)0;
		  count++;
		  state = st_def;
		  break;
		}
	      /* else fall through */
	    default:
	      ch = *p;		/* Preserve character (e.g. " or \) */
	      break;
	    }
	  
	  p++;
	  
	  if ( state == st_backsl )
	    {
	      if ( magic ) *(magic++) = ch;
	      if ( mask ) *(mask++) = (char)(-1);
	      count++;
	      state = st_def;
	    }
	  break;

	case st_oct:
	  if ( *p >= '0' && *p <='7' )
	    {
	      ch <<= 3;
	      ch += *(p++) - '0';
	    }
	  else
	    {
	      if ( magic ) *(magic++) = ch;
	      if ( mask ) *(mask++) = (char)(-1);
	      count++;
	      state = st_def;	/* Do not advance p */
	    }
	  break;

	case st_hex:
	  if ( *p >= '0' && *p <= '9' )
	    {
	      ch <<= 4;
	      ch += *(p++) - '0';
	    }
	  else if ( *p >= 'A' && *p <= 'F' )
	    {
	      ch <<= 4;
	      ch += *(p++) - 'A' + 10;
	    }
	  else if ( *p >= 'a' && *p <= 'f' )
	    {
	      ch <<= 4;
	      ch += *(p++) - 'a' + 10;
	    }
	  else
	    {
	      if ( magic ) *(magic++) = ch;
	      if ( mask ) *(mask++) = (char)(-1);
	      count++;
	      state = st_def;	/* Do not advance p */
	    }
	  break;
	}
    }

  if (endpos) *endpos = p;
  return count;
}

/*
 * enum actions getaction(char *pos, char **cmd)
 *
 * Reads the action proposed and returns:
 *
 * ACT_CAT       ... output file raw
 * ACT_DROP      ... silently ignore
 * ACT_REJECT    ... mail user and complain
 * ACT_FILTER    ... run through once, then done
 * ACT_PIPETHRU  ... run through command, then feed back through filter
 * ACT_ADDCR     ... \n -> \r\n, \f -> \r\f
 * ACT_PS        ... same as ADDCR, but add Ctrl-D at end
 * ACT_FFILTER   ... same as FILTER, but use a temp file
 * ACT_FPIPE     ... same as PIPETHRU, but use a temp file
 *
 * If the command takes an argument, point cmd to it if non-NULL
 *
 * IMPORTANT: The matching algorithm is a bit simplistic.  If any action
 * is a proper prefix of any other action, the longer action must be first
 * in the list.
 */

enum actions getaction(char *pos, char **cmd)
{
  enum actions act;
  static char *act_strs[] = { "cat", "ignore", "reject", "filter",
			     "pipe", "text", "postscript",
			     "ffilter", "fpipe", NULL };
  char **act_str;
  int i;

#if DEBUG > 3
  fprintf(stderr,"getaction %s\n", pos);
#endif
  
  while ( isspace(*pos) ) pos++; /* Ignore leading whitespace */
  
  if ( ! *pos )
    return ACT_ERR;		/* Syntax error */
    
  for ( act = ACT_CAT, act_str = act_strs ; *act_str ; act++, act_str++ )
    if ( strncmp(pos, *act_str, i = strlen(*act_str)) == 0 )
      {
	pos += i;
	break;
      }
  
  if ( ! *act_str )
    {
#if DEBUG > 3
      fprintf(stderr,"No match\n");
#endif
      return ACT_ERR;		/* Unknown action */
    }
  
  if ( *pos && !isspace(*pos) )
    {
#if DEBUG > 3
      fprintf(stderr,"Garbage at end: %s\n", pos);
#endif
      return ACT_ERR;		/* Garbage after action name */
    }

  if ( cmd )
    {
      while ( isspace(*pos) ) pos++; /* Skip leading whitespace again */
      *cmd = pos;
    }
  
  return act;
}

