/*
 * The original was spagetti. I have replaced Michael's code with some of
 * my own which is a thousand times more readable and can also handle '%',
 * which substitutes anything except a space. This should enable people
 * to position things better based on argument. I have also added '?', which
 * substitutes to any single character. And of course it still handles '*'.
 * this should be more efficient than the previous version too.
 *
 * Thus this whole file becomes:
 *
 * Written By Troy Rollo
 *
 * Copyright(c) 1992
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */

#if 0
static	char	rcsid[] = "@(#)$Id: reg.c,v 1.6 1994/07/02 02:32:13 mrg Exp $";
#endif

#include "irc.h"
#include "ircaux.h"
#include "output.h"

static	int	total_explicit;

/*
 * The following #define is here because we *know* its behaviour.
 * The behaviour of toupper tends to be undefined when it's given
 * a non lower case letter.
 * All the systems supported by IRCII should be ASCII
 */
#define	mkupper(c)	(((c) >= 'a' && (c) <= 'z') ? ((c) - 'a' + 'A') : c)


#if 1
/*  $Revision: 1.4 $
**
**  Do shell-style pattern matching for ?, \, [], and * characters.
**  Might not be robust in face of malformed patterns; e.g., "foo[a-"
**  could cause a segmentation violation.  It is 8bit clean.
**
**  Written by Rich $alz, mirror!rs, Wed Nov 26 19:03:17 EST 1986.
**  Rich $alz is now <rsalz@bbn.com>.
**  April, 1991:  Replaced mutually-recursive calls with in-line code
**  for the star character.
**
**  Special thanks to Lars Mathiesen <thorinn@diku.dk> for the ABORT code.
**  This can greatly speed up failing wildcard patterns.  For example:
**	pattern: -*-*-*-*-*-*-12-*-*-*-m-*-*-*
**	text 1:	 -adobe-courier-bold-o-normal--12-120-75-75-m-70-iso8859-1
**	text 2:	 -adobe-courier-bold-o-normal--12-120-75-75-X-70-iso8859-1
**  Text 1 matches with 51 calls, while text 2 fails with 54 calls.  Without
**  the ABORT, then it takes 22310 calls to fail.  Ugh.  The following
**  explanation is from Lars:
**  The precondition that must be fulfilled is that DoMatch will consume
**  at least one character in text.  This is true if *p is neither '*' nor
**  '\0'.)  The last return has ABORT instead of FALSE to avoid quadratic
**  behaviour in cases like pattern "*a*b*c*d" with text "abcxxxxx".  With
**  FALSE, each star-loop has to run to the end of the text; with ABORT
**  only the last one does.
**
**  Once the control of one instance of DoMatch enters the star-loop, that
**  instance will return either TRUE or ABORT, and any calling instance
**  will therefore return immediately after (without calling recursively
**  again).  In effect, only one star-loop is ever active.  It would be
**  possible to modify the code to maintain this context explicitly,
**  eliminating all recursive calls at the cost of some complication and
**  loss of clarity (and the ABORT stuff seems to be unclear enough by
**  itself).  I think it would be unwise to try to get this into a
**  released version unless you have a good test data base to try it out
**  on.
*/

#define TRUE			1
#define FALSE			0
#define ABORT			-1


    /* What character marks an inverted character class? */
#define NEGATE_CLASS		'^'
    /* Is "*" a common pattern? */
#define OPTIMIZE_JUST_STAR
    /* Do tar(1) matching rules, which ignore a trailing slash? */
#undef MATCH_TAR_PATTERN


/*
**  Match text and p, return TRUE, FALSE, or ABORT.
*/
static int
DoMatch(text, p)
    register char	*text;
    register char	*p;
{
    register int	matched;
#if 0
    register int	reverse;
    register int	last;
#endif
    
    for ( ; *p; text++, p++) {
	if (*text == '\0' && *p != '*')
	    return ABORT;
	switch (*p) {
	case '\\':
	    /* Literal match with following character. */
	    p++;
	    /* FALLTHROUGH */
	default:
	    if (mkupper(*text) != mkupper(*p))
		return FALSE;
	    total_explicit++;
	    continue;
	case '%':
	    p++;
	    while (*text && (*text != ' ' && *p != *text))
		text++;
 	    if ((matched = DoMatch(text, p)) != FALSE)
	        return matched;
	    return ABORT;
	case '?':
	    /* Match anything. */
	    continue;
	case '*':
	    while (*++p == '*')
		/* Consecutive stars act just like one. */
		continue;
	    if (*p == '\0')
		/* Trailing star matches everything. */
		return TRUE;
	    while (*text)
		if ((matched = DoMatch(text++, p)) != FALSE)
		    return matched;
	    return ABORT;
#if 0
	case '[':
	    reverse = p[1] == NEGATE_CLASS ? TRUE : FALSE;
	    if (reverse)
		/* Inverted character class. */
		p++;
	    for (last = 0400, matched = FALSE; *++p && *p != ']'; last = *p)
		/* This next line requires a good C compiler. */
		if (*p == '-' ? *text <= *++p && *text >= last : *text == *p)
		    matched = TRUE;
	    if (matched == reverse)
		return FALSE;
	    continue;
#endif
	}
    }

#ifdef	MATCH_TAR_PATTERN
    if (*text == '/')
	return TRUE;
#endif	/* MATCH_TAR_ATTERN */
    return *text == '\0';
}


/*
**  User-level routine.  Returns TRUE or FALSE.
*/
int match(char *p, char *text)
{
#ifdef	OPTIMIZE_JUST_STAR
    if (p[0] == '*' && p[1] == '\0')
	return TRUE;
#endif	/* OPTIMIZE_JUST_STAR */
    return DoMatch(text, p) == TRUE;
}


#ifdef	TEST
#include <stdio.h>

/* Yes, we use gets not fgets.  Sue me. */
extern char	*gets();


main()
{
    char	 p[80];
    char	 text[80];

    printf("Wildmat tester.  Enter pattern, then strings to test.\n");
    printf("A blank line gets prompts for a new pattern; a blank pattern\n");
    printf("exits the program.\n");

    for ( ; ; ) {
	printf("\nEnter pattern:  ");
	(void)fflush(stdout);
	if (gets(p) == NULL || p[0] == '\0')
	    break;
	for ( ; ; ) {
	    printf("Enter text:  ");
	    (void)fflush(stdout);
	    if (gets(text) == NULL)
		exit(0);
	    if (text[0] == '\0')
		/* Blank line; go back and get a new pattern. */
		break;
	    printf("      %s\n", wildmat(text, p) ? "YES" : "NO");
	}
    }

    exit(0);
    /* NOTREACHED */
}
#endif	/* TEST */
#endif

#if 0
#ifdef __STDC__
int match(char *pattern, char *string)
#else
int	match(pattern, string)
char	*pattern,
	*string;
#endif
{
	char	type;

#if 0
	if ((!pattern || !string) && !x_debug)
	{
		yell("match: pattern or string is NULL!");
		return 0;
	}
#endif

	while (*string && *pattern && *pattern != '*' && *pattern != '%')
	{
		if (*pattern == '\\' && *(pattern + 1))
		{
			if (!*++pattern || !(mkupper(*pattern) ==
					mkupper(*string)))
				return 0;
			else
				pattern++, string++, total_explicit++;
		}


		if (*pattern == '?')
			pattern++, string++;
		else if (mkupper(*pattern) == mkupper(*string))
			pattern++, string++, total_explicit++;
		else
			break;
	}
	if (*pattern == '*' || *pattern == '%')
	{
		type = (*pattern++);
		while (*string)
		{
			if (match(pattern, string))
				return 1;
			else if (type == '*' || *string != ' ')
				string++;
			else
				break;
		}
	}
	if (!*string && !*pattern)
		return 1;
	return 0;
}
#endif

/*
 * This version of wild_match returns 1 + the  count  of characters
 * explicitly matched if a match occurs. That way we can look for
 * the best match in a list
 */
/* \\[ and \\] handling done by Jeremy Nelson
 * EPIC will not use the new pattern matcher currently used by 
 * ircii because i am not convinced that it is 1) better * and 
 * 2) i think the \\[ \\] stuff is important.
 */
#ifdef __STDC__
int wild_match (char *pattern, char *str)
#else
int	wild_match(pattern, str)
char	*pattern,
	*str;
#endif
{
register	char *ptr;
register	char *ptr2 = pattern;
register	int nest = 0;
	char my_buff[2 * BIG_BUFFER_SIZE+1];
	char *arg;
	int best_total = 0;

	total_explicit = 0;

	/* Is there a \[ in the pattern to be expanded? */
	/* This stuff here just reduces the \[ \] set into a series of
	 * one-simpler patterns and then recurses */
	if ((ptr2 = strstr(pattern, "\\[")))
	{
		/* we will have to null this out, but not until weve used it */
		char *placeholder = ptr2;
		ptr = ptr2;

		/* yes. whats the character after it? (first time
		   through is a trivial case) */
		do
		{
			switch (ptr[1]) 
			{
					/* step over it and add to nest */
				case '[' :  ptr2 = ptr + 2 ;
					    nest++;
					    break;
					/* step over it and remove nest */
				case ']' :  ptr2 = ptr + 2;
					    nest--;
					    break;
			}
		}
		/* Repeat while there are more backslashes to look at and
		 * we have are still in nested \[ \] sets
		 */
		while ((nest) && (ptr = index(ptr2, '\\')));

		/* right now, we know ptr points to a \] or to null */
		/* remember that && short circuits and that ptr will 
		   not be set to null if (nest) is zero... */
		if (ptr)
		{
			/* null out and step over the original \[ */
			*placeholder = '\0';
			placeholder += 2;

			/* null out and step over the matching \] */
			*ptr = '\0';
			ptr +=2;

			/* grab words ("" sets or space words) one at a time
			 * and attempt to match all of them.  The best value
			 * matched is the one used.
			 */
			while ((arg = new_next_arg(placeholder, &placeholder)))
			{
				int tmpval;
				strcpy(my_buff, pattern);
				strcat(my_buff, arg);
				strcat(my_buff, ptr);

				/* the total_explicit we return is whichever
				 * pattern has the highest total_explicit */
				if ((tmpval = wild_match(my_buff, str)))
				{
					if (tmpval > best_total)
						best_total = tmpval;
				}
			}
			return best_total; /* end of expansion section */
		}
		/* Possibly an unmatched \[ \] set */
		else
		{
			total_explicit = 0;
			if (match(pattern, str))
				return total_explicit + 1;
			else
			{
#if 0
				yell("Unmatched \\[ !");
#endif
				return 0;
			}
		}
	}
	/* trivial case (no expansion) when weve expanded all the way out */
	else if (match(pattern, str))
		return total_explicit+1;
	else
		return 0;
}

