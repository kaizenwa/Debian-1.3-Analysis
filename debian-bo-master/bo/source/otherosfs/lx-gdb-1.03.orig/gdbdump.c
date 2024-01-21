/* GDBDUMP.C -- Generic HP100LX Database Dump Program
 *
 * Usage: gdbdump [-noqsw] file
 *    -n causes the first line of the output, which normally contains field
 *       names, to be omitted.
 *    -o causes note fields to be omitted.
 *    -q causes warning messages to be suppressed.
 *    -s causes special characters (those with character codes between 128
 *       and 254, inclusive) to be written unmodified to the output.  The
 *       default is to write them in \nnn notation.
 *    -w causes lines to be wrapped near 75 columns, with a trailing
 *       backslash on lines to be continued.  Lines will be broken at 70
 *       columns except that an escape sequence will not be broken (the
 *       break is delayed).
 *    -m causes multi-line string (note) fields to be written as multiple 
 *       lines, with line breaks in the output where line breaks are found 
 *       in the string field. Default (i.e. the good ol' behaviour) is to 
 *       output line breaks as \r\n sequences.
 *
 * Output:
 *    One line of field names (unless -n is specified), followed by one line
 *    for each record in the database.  Each line contains the fields of a
 *    record, separated by commas, in the order indicated by the first line.
 *    Only fields of the database which have values are output.
 *
 *    Fields are output as follows:
 *        Text, Category, or Note
 *            Text of the field is contained in double quote marks ("").
 *            Double quote marks within the data are escaped by a backslash
 *            (\), as are backslashes themselves.  Characters with ASCII
 *            codes outside the range 32-126 are represented as \nnn,
 *            where nnn is in octal, except that 10 and 13 are represented
 *            as \n and \r, respectively.
 *            Categories are represented as the concatenation of the
 *            category names, separated by semicolons (;).
 *        Number, Date, or Time
 *            Stored in standard ASCII representation.  Dates are in the
 *            form YYYYMMDD, for example 19930724.  Times are in the form
 *            HHMM, where HH ranges from 0-23.
 *        Option Button or Check Box
 *            Selected buttons are stored as 1; unselected buttons are
 *            stored as 0.
 *
 * Programming notes:
 *      There are lots of magic numbers in this code, which come from
 *      the document "Format of 100LX Databases" posted by Andrew J.
 *      Gryc of Hewlett-Packard to the comp.sys.palmtops newsgroup on
 *      18 June 1993.
 *      I do not use structures in this code to map into the database format,
 *      because they are not portable across architectures with different
 *      byte orders.
 *      This program reads the records in the database file sequentially,
 *      without looking at the index table.  This results in some speed
 *      degradation but allows operation on databases with incomplete
 *      indexes.
 *
 * Steve Roth's disclaimer:
 *      These programs are released into the public domain and I place no 
 *      restrictions on their use.  I make no warranties or guarantees for
 *      these programs and you use them at your own risk.  These programs are
 *      supplied by me personally and not by Hewlett-Packard Co., which
 *      incurs no obligations pertaining to them.  (Please note that my work
 *      with HP has no connection with palmtops or PC products, and only
 *      information and resources available to the general public were used
 *      in writing these programs.  Thanks to Andy Gryc for publishing the
 *      necessary information!)
 *
 * Arne Christensen's disclaimer:
 *	Read the above again except for the HP sentences :-)
 *
 * Revision History:
 *      1.03 - Added -m option.
 *      1.02 - (Maintenance taken over by arc@pine.dk)
 *             Fixed bitmask offsets of check boxes (credit Diomidis Spinellis)
 *      1.01 - Added -s flag and more comments.
 *      1.00 - Initial version.
 *
 * Written by:
 *      Steve Roth (stever@cup.hp.com)
 *
 * Currently being maintained by:
 *      Arne Christensen (arc@pine.dk)
 *
 * 11 February 1996
 */

#include <stdio.h>
#include <stdlib.h>

#if !defined(TRUE)
#define TRUE 1
#define FALSE 0
#endif

#define LINELEN 70			/* Wrap lines here if -w specified */
#define MAXFIELDS 100			/* Max fields in a database */

typedef unsigned char uchar;

FILE * hf;				/* Database file handle */
int    fNames;				/* Field names requested */
int    fNotes;				/* Notes requested */
int    fWarnings;			/* Warnings requested */
int    fSpecials;			/* Special characters requested */
int    fWrap;				/* Wrapped lines requested */
int    fMultiLine;			/* Notes on multiple lines requested */
long   lFirst;				/* Offset of first database record */
int    cFields = 0;			/* Number of fields in database */
uchar  fields[MAXFIELDS][28];		/* Field definitions */
int    cChars = 0;			/* # chars written on curr. line */

/* Exit the program with an error message. */
void die(char * msg)
	{
	fprintf(stderr, "%s\n", msg);
	exit(1);
	}

/* Exit the program with a usage message. */
void usage(void)
	{
	die("usage: gdbdump [-noqsw] file");
	}

/* Write out a string, wrapping lines appropriately.  If fBreak is FALSE,
 * do not wrap lines within this string; postpone the wrap until the entire
 * string has been written.
 */
void out(char * s, int fBreak)
	{
	if (!fWrap)			/* Are we wrapping lines at all? */
		{			
		fputs(s, stdout);
		return;
		}
	if (fBreak)			/* Can we wrap within this string? */
		{
		while (cChars + strlen(s) > LINELEN)
			{		/* Print to end of line, repeatedly */
			printf("%.*s\\\n", LINELEN - cChars, s);
			s += LINELEN - cChars;
			cChars = 0;
			}
		if (*s)			/* Any left over? */
			{
			fputs(s, stdout);
			cChars += strlen(s);
			}
		}
	else				/* Can't wrap within this string */
		{
		fputs(s, stdout);
		cChars += strlen(s);
		if (cChars > LINELEN)	/* Do we need to wrap after it? */
			{
			printf("\\\n");
			cChars = 0;
			}
		}
	}

/* Start the next line of output. */
void newline(void)
	{
	putchar('\n');
	cChars = 0;
	}

/* Print a string field, adding escape codes as needed. */
void PrintString(uchar * s)
	{
static	char charbuf[2] = { 0, 0 };
static	char intbuf[5] = { '\\', 0, 0, 0, 0 };

	out("\"", FALSE);		/* Open quotes */
	for (; *s; s++)
		switch (*s)		/* What type of char are we printing? */
			{		/* Escape CRs, LFs, \'s, quotes */
		case '\r': 
			if (fMultiLine && *(s+1) == '\n')
				{
				newline();
				s++; /* skip an extra char so this eats \r\n */
				}
			else
				out("\\r", FALSE);
			break;
		case '\n': out("\\n", FALSE); break;
		case '\\': out("\\\\", FALSE); break;
		case '"':  out("\\\"", FALSE); break;
		default:		/* Otherwise: */
			if (*s < 32 ||
			    (*s > 127 && !fSpecials) ||
			    *s == 255)
				{	/* Is it non-ASCII? */
				sprintf(intbuf, "\\%03o", *s);
				out(intbuf, FALSE);
				}
			else		/* No, it's ASCII, print it. */
				{
				charbuf[0] = *s;
				out(charbuf, FALSE);
				}
			}
	out("\"", FALSE);		/* Close quotes */
	}

/* Read the field definitions from the database */
void ReadFields(void)
	{
	uchar rechdr[6];		/* Record header buffer */
	short fFirst = TRUE;		/* First field? */
	
	for (;;)
		{
		if (6 != fread(rechdr, 1, 6, hf))
			break;		/* Read error */
		if (rechdr[0] == 31)
			break;		/* Hit index table at end of file */
		if (rechdr[0] != 6)
			{		/* Not a field def, skip it */
			fseek(hf, rechdr[3]*256+rechdr[2]-6, SEEK_CUR);
			continue;
			}

		/* This is a field definition.  Read it. */
		fread(fields[cFields], 1, rechdr[3]*256+rechdr[2]-6, hf);
		if (fields[cFields][4] & 0x80)
			/* Skip fields without data */
			continue;
		if (!fNotes && *fields[cFields] == 10)
			/* Skip notes if requested */
			continue;
		if (*fields[cFields] >= 16)
			{
			if (fWarnings)
				fprintf(stderr, "warning: application-defined "
						"field '%s' skipped\n",
					&fields[cFields][7]);
			continue;
			}
		if (fNames)		/* Are we printing field names? */
			{
			if (fFirst)	/* Is this the first one? */
				fFirst = FALSE;
			else		/* No, so put a comma first. */
				out(",", FALSE);
			PrintString (&fields[cFields][7]);
			}
		cFields++;
		}
	if (fNames)			/* Are we printing field names? */
		newline();
	}

/* Print out a note. */
void PrintNote(short note)
	{
	long    lCurrent = ftell(hf);	/* Save current position in file */
	uchar   rechdr[6];		/* Record header buffer */
	uchar * pch;
	
	fseek(hf, lFirst, SEEK_SET);	/* Go back to first record */
	for (;;)
		{			/* Search for note record */
		if (6 != fread(rechdr, 1, 6, hf))
			break;		/* Read error */
		if (rechdr[0] == 31)
			break;		/* Hit index table at end of file */
		if (rechdr[0] != 9 ||
		    rechdr[5]*256+rechdr[4] != note)
			{		/* Not a note or not our note, skip */
			fseek(hf, rechdr[3]*256+rechdr[2]-6, SEEK_CUR);
			continue;
			}
		/* read and print note */
		pch = (uchar *)malloc(rechdr[3]*256+rechdr[2]-5);
		fread(pch, 1, rechdr[3]*256+rechdr[2]-6, hf);
		pch[rechdr[3]*256+rechdr[2]-6] = '\0';
		PrintString(pch);
		free(pch);

		/* go back to where we were and return */
		fseek(hf, lCurrent, SEEK_SET);
		return;
		}

	/* Didn't find the note, so print a null string and a warning. */
	out("\"\"", FALSE);
	if (fWarnings)			/* Are we printing warnings? */
		fprintf(stderr, "warning: note record %d not found!\n", note);
	
	/* go back to where we were and return */
	fseek(hf, lCurrent, SEEK_SET);
	}

/* Print out a record */
void PrintRecord(uchar * rec)
	{
	int   field;			/* Field counter */
	short fFirst = TRUE;		/* First field? */
	short offset;			/* offset of string data */
	short word;			/* word containing data */
	char  buf[20];			/* date/time formatting buffer */

	for (field = 0; field < cFields; field++)
		{			/* Print each field */
		if (fFirst)		/* First field? */
			fFirst = FALSE;
		else			/* No, start with comma */
			out(",", FALSE);

		/* get the offset of field data */
		offset = fields[field][3]*256+fields[field][2];
		if (fields[field][4] & 0x20)
			offset = rec[offset+1]*256+rec[offset];
			
		switch (fields[field][0])
			{		/* What type of field? */
		case 0:
		case 1:			/* Bitmasks */
			word = rec[offset+1]*256+rec[offset];
			word &= fields[field][6]*256+fields[field][5];
			out(word ? "1" : "0", FALSE);
			break;
		case 2:
		case 3:
		case 4:
		case 5:
		case 6:
		case 13:
		case 15:		/* Strings of various types */
			PrintString(&rec[offset]);
			break;
		case 7:			/* Time */
			if (rec[offset] == 0 &&
			    rec[offset+1] == 0x80)
				break;	/* Empty time */
			word = rec[offset+1]*256+rec[offset];
			sprintf(buf, "%02d%02d", word / 60, word % 60);
			out(buf, TRUE);
			break;
		case 8:			/* Date */
			if (rec[offset] == 255 &&
			    rec[offset+1] == 255 &&
			    rec[offset+2] == 255)
				break;	/* Empty date */
			sprintf(buf, "%04d%02d%02d", rec[offset]+1900,
				rec[offset+1]+1, rec[offset+2]+1);
			out(buf, TRUE);
			break;
		case 9:			/* Radio button */
			if (rec[offset] == fields[field][5])
				out("1", FALSE);
			else
				out("0", FALSE);
			break;
		case 10:		/* Note */
			word = rec[offset+1]*256+rec[offset];
			if (word == -1)
				out("\"\"", FALSE);
			else
				PrintNote(word);
			break;
			}
		}
	newline();
	}

/* Read the data records */
void ReadData(void)
	{
static	int     fAppSpecFound = FALSE;
	uchar   rechdr[6];
	uchar * pch;
	
	for (;;)
		{			/* Search the records for data */
		if (6 != fread(rechdr, 1, 6, hf))
			break;		/* Read error */
		if (rechdr[0] == 31)
			break;		/* Hit index table at end */
		if (rechdr[0] > 13)
			{		/* User-defined record */
			if (fWarnings && !fAppSpecFound)
				fprintf(stderr, "warning: database contains "
						"application-specific records "
						"not handled by gdbdump\n");
			fAppSpecFound = TRUE;
			}
		if (rechdr[0] != 11 ||	/* Non-data record */
		    (rechdr[1] & 0x01))	/* Deleted record */
			{		/* Skip it */
			fseek(hf, rechdr[3]*256+rechdr[2]-6, SEEK_CUR);
			continue;
			}

		/* Read and print data record */
		pch = (uchar *)malloc(rechdr[3]*256+rechdr[2]-6);
		fread(pch, 1, rechdr[3]*256+rechdr[2]-6, hf);
		PrintRecord(pch);
		free(pch);
		}
	}

/* Dump the database */
void DoDump(void)
	{
	uchar sig[5];			/* File signature */
	uchar dbhdr[25];		/* Database header */
	
	sig[4] = '\0';
	if (4 != fread(sig, 1, 4, hf) || strcmp(sig, "hcD"))
		die("gdbdump: not a valid database");
	if (25 != fread(dbhdr, 1, 25, hf))
		die("gdbdump: read error");
	if (dbhdr[6] != 2 || dbhdr[7] != 1)
		die("gdbdump: wrong version of database format");
	if (dbhdr[8] == 'W' && fWarnings)
		fprintf(stderr, "warning: world time databases cannot "
				"be loaded with gdbload\n");
	if (dbhdr[8] == '2' && fWarnings)
		fprintf(stderr, "warning: appointment book databases cannot "
				"be loaded with gdbload\n");
	lFirst = ftell(hf);
	ReadFields();
	fseek(hf, lFirst, SEEK_SET);
	ReadData();
	}

/* Main program.  Parse arguments, open files, and get started. */
int main (int argc, char * argv[])
	{
					/* Switch defaults: */
	fNames = TRUE;			/*   Print field names */
	fNotes = TRUE;			/*   Print notes */
	fWarnings = TRUE;		/*   Print warnings */
	fSpecials = FALSE;		/*   Print special characters */
	fWrap = FALSE;			/*   Don't wrap lines */
	fMultiLine = FALSE;		/*   Don't put notes on multiple lines*/
	
	argc--;				/* Skip argv[0] */
	argv++;
	
	while (argc && **argv == '-')	/* Read option arguments */
		{
		(*argv)++;		/* Skip - */
		
		while (**argv)		/* Read option flags */
			{
			switch (**argv)
				{	/* Which flag? */
			case 'n': fNames    = FALSE; break;
			case 'o': fNotes    = FALSE; break;
			case 'q': fWarnings = FALSE; break;
			case 's': fSpecials = TRUE;  break;
			case 'w': fWrap     = TRUE;  break;
			case 'm': fMultiLine= TRUE;  break;
			default:
				usage();
				break;
				}
			(*argv)++;	/* Skip to next flag */
			}
		
		argc--;			/* Skip to next argument */
		argv++;
		}
	
	if (argc != 1)			/* Make sure one non-flag argument */
		usage();
	
	if (!(hf = fopen(*argv, "rb")) &&
	    !(hf = fopen(*argv, "r")))	/* Open the database file */
		{
		perror("gdbdump: can't open database");
		exit (1);
		}
	
	DoDump();			/* Dump the database */
	fclose (hf);			/* Close it */
	exit(0);
	}
