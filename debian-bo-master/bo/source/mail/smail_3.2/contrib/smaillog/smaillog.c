/* @(#) smaillog.c,v 1.7 1992/09/20 12:54:34 tron Exp */
/*
 *
 * Parse smail3 log files.
 *
 * Copyright 1990, Jeff Beadles.  All rights reserved.
 *
 * Permission is granted to freely copy this program as long as it is
 * not sold.  Source MUST be provided upon request.
 *
 * This message must be kept on all copies of the program.
 *
 *	jeff@onion.pdx.com
 *  or  uunet!onion.pdx.com!jeff
 *
 * History: 
 *	04.Apr.92
 *	Jan-Piet Mens - <jpm@Logix.DE>
 *	Added options -l to show size of message, and
 *	      option -d to output the date stamp additionally
 *
 *	/local/src-CVS/master/smail/contrib/smaillog/smaillog.c,v 1.7 1992/09/20 12:54:34 tron Exp
 */


#include <stdio.h>
#include <ctype.h>
#if defined(SYSV) || defined(POSIX)
#include <string.h>
#define index strchr
#define rindex strrchr
#else
#include <strings.h>
extern int strspn();
#endif

#ifndef TRUE		
# define TRUE		1
# define FALSE		0
#endif

void add_data();
struct qlist *lookup();
char *skipfields();

extern char *mspace();
long int malloc_space = 0L;	/* Space malloc had to allocate */

/*
 *	The qlist structure holds each unique "qid"'s entry.  It also
 *	points to the data structures.
 *	This is stored as a linked list with "qhead" pointing to the head
 *	of the list.
 */
struct qlist {
	char qid[32];
	char date[24];
	struct data *dp;
	struct qlist *next;
	};

/*
 *	The data structure is the data information for the qlist structure.
 *	There can be (and most likely are) multiple data structs for each
 *	qlist structure.
 *	tfl == "to from list"
 */

struct data {
	char *tfl;
	struct data *next;
	};

/*
 *	The structures look like this:
 *
 *
 *	(qhead)  qlist  -> data1 -> data2 -> data3
 *		  \|/
 *		 qlist  -> data1 -> data2
 *		  \|/
 *		 qlist  -> data1 -> data2 -> data3 -> data4 -> data5
 *		  \|/
 *		  NULL   (End of list )
 *
 */

static struct qlist *qhead;
extern char *fgets();

/* ARGSUSED */
int
main(argc,argv)
int argc;
char **argv;
{
	struct data  *dtmp;
	struct qlist *qtmp;

	char linebuf[4096], buffer[4096];
	char line_date[24];
	char *fg, *cp;
	int lines = 0;
	char to[4096], from[4096];
	char *source, *dest;
	char *ctmp;
	int c, date_opt = FALSE, length_opt = FALSE;
	extern int optind;
	long msgbytes, atol();

	qhead =(struct qlist *)NULL;

	while ((c = getopt(argc, argv, "dl")) != EOF)
		switch (c)
		{
			case 'd':
				date_opt = TRUE;
				break;
			case 'l':
				length_opt = TRUE;
				break;
			default:
				exit(fprintf(stderr, 
					"Usage: %s [-l] [-d] <input >output\n",
						*argv));
		}
/*
 * Give a terse usage message if called with arguements.
 */
	if ( argc - optind != 0) {
	   fprintf(stderr, "Error:  Usage:  %s [-l] [-d] <input >output\n",argv[0]);
	   exit(1);
	}
/*
 * Read each line from stdin
 */
	while ((fg=fgets(linebuf, sizeof(linebuf),stdin)) != (char *)NULL){
		lines++;

/*
 * Blast out the \n at the end... 
 */
		ctmp = rindex(fg, '\n');
		if (ctmp) {
			*ctmp = '\0';
		}

/* 
 * Skip "01/01/90 "  if date option not set. This is meant to run daily.
 */
		if (!date_opt)
			if ((fg=skipfields(fg, 1)) == (char *)NULL) {
				printf("Error:  Skip line %d: %s\n",
				       lines, linebuf);
				continue;
			}
/*
 * Save the date, to be put in the qlist structure later
 */

		(void)strncpy(line_date, fg, (date_opt) ? 17 : 8);

/* 
 * Skip past the date field now, as we have it.
 */

		if ((fg=skipfields(fg,(date_opt) ? 2 : 1)) == (char *)NULL) {
			printf("Error:  Skip line %d: %s\n", linebuf);
			continue;
		}
/*
 * Catches "open_spool:" and "pid ##: smail daemon started" and other stuff.
 * This just copies them verbatium to the output file.
 * (At the head of the list)
 */
		if ( *fg != '[' ) {
			printf("Warning: %s\n", fg);
			continue;
		}
/* 
 * Put qid in "buffer"  It looks like "[xxxxxx-xxxxxxx]".
 * This is the identifier that is used to index into the qlist struct.
 */
		cp=buffer;
		while(!isspace(*fg))
			*cp++ = *fg++;
		*cp = '\0';

/*
 * buffer now contains the qid, so look it up in the qlist.
 * Lookup will always return success.  It will malloc a new area if this
 * is a new record.
 */
		qtmp = lookup(buffer);
		(void)strcpy(qtmp->date, line_date);

/*
 * Concatenate continuation lines to one single line.
 */
		fg = linebuf;
		do {
			if ((c = getc(stdin)) == EOF)
				break;
			if (c != '|') {
				ungetc(c, stdin);
				break;
			}
			if (fgets(fg, sizeof(linebuf) - (fg - linebuf), stdin)
							== (char *)NULL)
				break;
			ctmp = rindex(fg, '\n');
			if (ctmp) {
				*ctmp = '\0';
			}
			fg += strlen(fg);
		} while (sizeof(linebuf) > (fg - linebuf));
		add_data(qtmp, linebuf);
	}
/*
 * Note:  We are now out of the fgets() loop, and have all of the information
 * loaded in the struct.  Time to print some stats, and process the info.
 */

	printf("Processing %d lines.  (Used %ld bytes.)\n",lines,malloc_space);

/*
 * Time to start stepping thru the list.  Start at the head, and process each
 * qlist structure and it's data elements.
 */
	qtmp = qhead;

	while(qtmp) {
		dtmp = qtmp->dp;
		*from = *to = '\0';
		while(dtmp) {

/* This marks a "from" line.  If so, then strip off the first 6 characters,
 * and set the variable "dest" to point to it.  It will automatically
 * append to the from line if needed.  It copies until it reaches the
 * end of line, or a whitespace.
 */
			if ((fg=skipfields(dtmp->tfl, 1)) == (char *)NULL) {
				dtmp = dtmp->next;
				continue;
			}
			if ( !strncmp(fg, "from: ", 6)) {
				dest = from + strlen(from);
				source = fg + 6;
				while (*source && !isspace(*source) )
					*dest++ = *source++;
				*dest++ = ' ';
				*dest = '\0';

				if (length_opt)
				{
					/*
					 * Look for "size: " to get to the
					 * bytes in the message. NOTE, the
					 * size specified here, *excludes* the
					 * headers that SMAIL adds.
					 */

					fg = source;
					while (fg = skipfields(fg, 1)) {
						if ( !strncmp(fg, "size: ", 6))
							break;
					}
					if (fg) {
						msgbytes = atol(fg + 6);
					}
				}
				dtmp = dtmp->next;
				continue;
			}
/* This is used to see if this is a "to" line.  If we find a "via" first,
 * this is a remote address, so append it to the "to" variable with a
 * terminating "!". The append the contents of the "to" line.
 */
			if ( !strncmp(fg, "via: ", 5)) {
				dest = to + strlen(to);
				source = fg + 5;
				while (*source && !isspace(*source) )
					*dest++ = *source++;
				if ((fg=skipfields(source, 1))
							== (char *)NULL) {
					*dest++ = ' ';
					*dest = '\0';
					dtmp = dtmp->next;
					continue;
				}
				*dest++ = '!';
				*dest = '\0';
			}
			if ( !strncmp(fg, "to: ", 4)) {
				dest = to + strlen(to);
				source = fg + 4;
				while (*source && !isspace(*source) )
					*dest++ = *source++;
				*dest++ = ' ';
				*dest = '\0';
				dtmp = dtmp->next;
				continue;
			}
/* A general error trap, that should not be reached, but you know how that
 * goes... :-)
 */
			printf("What is %s?\n", dtmp->tfl);
			dtmp = dtmp->next;
		}
/* At this point, we have traveled thru one entire message's data.
 * Now, print the date, and the to and from information.  Try to figure
 * out if this is to, or from someone on this machine, to do the "=>" "<="
 * right.  If all else fails though, punt and guess.
 */
		if ( *to && *from) {
			printf("%s: ", qtmp->date);
/*
 * kill trailing ',' and whitespace in the 'to' and 'from' fields
 */
			cp = to + strlen(to) - 1;
			while ((*cp) && (isspace(*cp) || (*cp == ',')))
				*cp-- = '\0';

			cp = from + strlen(from) - 1;
			while ((*cp) && (isspace(*cp) || (*cp == ',')))
				*cp-- = '\0';
/*
 * All of that for this to display it
 */

			if (length_opt)
				printf("%6ld %s => %s\n", msgbytes, from, to);
			else
				printf("%s => %s\n", from, to);
		}
/*
 * Time to get the next element and process it.
 */
		qtmp = qtmp->next;
	}


	exit(0);
}

/*
 * This routine will add the data (str) to the qlist member "q"
 */

void
add_data(q, str)
struct qlist *q;
char *str;
{
	struct data *dtmp;
	char *cp;

/*
 * Save the string
 */
	cp = mspace( (unsigned int)(strlen(str)));
	(void)strcpy(cp, str);

/*
 * If the data area is empty, then just load this into it.
 * and point the "next" entry to null.
 */
	if (q->dp == (struct data *)NULL) {
		q->dp = (struct data *)mspace(sizeof(struct data));
		dtmp = q->dp;
		dtmp->tfl = cp;
		dtmp->next = (struct data *)NULL;
		return;
	}
/*
 * otherwise, search and find the end of the data list for this qlist.
 * Then, add it to the end.
 */
	dtmp = q->dp;
	while(dtmp->next != (struct data *)NULL) {
		dtmp= dtmp->next;
	}
/*
 * Get space for the new element, and add it to the end of the list.
 * Then, move to the new element, and add the data to it.
 * After that, set the "next" flag to null, to show that this is now
 * the end of the list.
 */
	dtmp->next=(struct data *)mspace(sizeof(struct data));
	dtmp = dtmp->next;
	dtmp->tfl = cp;
	dtmp->next = (struct data *)NULL;
}


/*
 * This routine will search the qlist for an element with the "key" of
 * the qid this is called with.  If not found, then it will automatically
 * allocate a new member, and initialize it properly
 */
struct qlist *
lookup(str)
char *str;
{

	struct qlist *qtmp = qhead;


/*
 * Is this an empty list?  If so, then add to head.
 */
	if ( qhead == (struct qlist *)NULL) {
		qhead = (struct qlist *)mspace(sizeof(struct qlist));
		qtmp = qhead;
		(void)strcpy(qtmp->qid, str);
		qtmp->dp = (struct data *)NULL;
		qtmp->next = (struct qlist *) NULL;
		return (qtmp);
	}
	while (1) {
/*
 * Does this one match?  If so, then return it.
 */
		if (!strcmp(qtmp->qid, str)) {
			return(qtmp);
		}
/*
 * If at the end of the list, then add it.
 */
		if (qtmp->next == (struct qlist *)NULL) {
		    qtmp->next=(struct qlist *)mspace(sizeof(struct qlist));
		    qtmp = qtmp->next;
		    (void)strcpy(qtmp->qid, str);
		    qtmp->dp = (struct data *)NULL;
		    qtmp->next = (struct qlist *) NULL;
		    return (qtmp);
		}
		qtmp = qtmp->next;
	}
}


/*
 * This routine just skips past whitespace-delimited fields in a string.
 * IE:  If called as skipfields("a bb ccc dddd eeeee",2) it will return
 *  "ccc dddd eeeee"
 */
char *
skipfields(str,count)
char *str;
int count;

{
	if ( count < 1 )
		return( (char *)NULL);

	while (count--) {
		while((*str) && !(isspace(*str)))
			str++;
		if (!*str)
			return( (char *)NULL);
		while((*str) && (isspace(*str)))
			str++;
		if (!*str)
			return( (char *)NULL);
	}
	return(str);
}

/* This routine is a general "malloc" routine.  It does two additional
 * functions though.  One, is to keep track of how much space has been
 * malloc'ed, and the second is to automatically check for errors, and bail-
 * out if needed.  This prevents me from having to put the same code around
 * all of the malloc's.
 *
 * It also allocates "FUZZ" more bytes than were requested.  If you're 
 * having troubles with this, then change it to something >0.
 * (Don't make it less than 0 though, snicker :-)
 */

#ifndef FUZZ
#define FUZZ sizeof(short)
#endif /* !FUZZ */

char *
mspace(len)
unsigned int len;
{
	static char *mp;
	extern char *malloc();

	len += FUZZ;

	mp = malloc(len);

	if (mp) { 
	  malloc_space += len;
	  return(mp);
	} else {
	  fprintf(stderr, "Malloc:\tOut of space (Requested %d bytes)\n",len);
	  fprintf(stderr, "\tAlready requested %ld bytes\n", malloc_space);
	  exit(1);
	}
	/*NOTREACHED*/
}
