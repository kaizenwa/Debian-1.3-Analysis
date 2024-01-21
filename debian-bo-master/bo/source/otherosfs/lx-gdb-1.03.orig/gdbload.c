/* GDBLOAD.C -- Put data into an HP100LX database.
 *
 * usage: gdbload [-an] <database file> [<input file>]
 *
 * -a means add records to the database.  The default is to replace the
 *      records in the database.
 * -n means do not make a backup of the database before beginning.  The
 *      default is to back up the database.
 * <database file> is a 100LX database file (.GDB, .PDB, .NDB, or .WDB).
 *      Appointment book files (.ADB) are not supported (yet).
 * <input file> is a file containing the records to add to the database.
 *      If not specified, standard input is used.
 *      The first line of the file must contain a list of field names,
 *      separated by commas and quoted with "" marks if they contain
 *      commas.  Subsequent lines of the file represent records (one
 *      per line except that line breaks may occur within note fields),
 *      with the values of the fields in the record presented
 *      in the same order as the field names were on the first line.
 *      String fields must be quoted using "" if they contain commas
 *      or line breaks.
 *      Within strings, the usual C escape codes \r \n \\ \" \nnn and
 *      \xnn are understood.  Dates should be encoded as YYYYMMDD, for
 *      example 19930730.  Times should be encoded as HHMM, with HH in
 *      the range 00-23.  Radio buttons and check boxes are considered
 *      off if the field is empty or contains 0, on otherwise.
 *
 *      The database file written by gdbload will not be accepted
 *	immediately by the 100LX database engine. Rebuild the index by
 *	pressing F6 and selecting any "subset" (even the current one).
 *
 * Programming notes:
 *      There are lots of magic numbers in this code, which come from
 *      the document "Format of 100LX Databases" posted by Andrew J.
 *      Gryc of Hewlett-Packard to the comp.sys.palmtops newsgroup on
 *      18 June 1993.
 *      I do not use structures in this code to map into the database format,
 *      because they are not portable across architectures with different
 *      byte orders.
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
 *      1.03 - Now accepts line breaks in string (note) fields.
 *      1.02 - (Maintenance taken over by arc@pine.dk)
 *	       Added handling of national characters.
 *             Removed compiler dependency (credit Charles Stroom)
 *      1.01 - Fixed -n bug and added comments.
 *      1.00 - Initial version.
 *
 * Written by:
 *      Steve Roth (stever@cup.hp.com)
 *
 * Currently maintained by:
 *      Arne Christensen (arc@pine.dk)
 *
 * 24 July 1995
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* This define is needed for HP-UX systems, and probably not anywhere else
 * I would guess.  Comment it in if needed.
 */
/*#define stricmp   strcasecmp*/

#define MAXFIELDS 100			/* Max. fields in a database */

#if !defined(TRUE)
#define TRUE      1
#define FALSE     0
#endif

typedef unsigned char uchar;

int     fAdd;				/* Add to existing database? */
int     fBackup;			/* Backup existing database? */
int     cFields = 0;			/* # fields in database */
int     cInFields = 0;			/* # fields in input stream */
int     mapFields[MAXFIELDS];		/* mapping from input stream to db */
int     hiNote;				/* First available note record # */
int     hiData;				/* First available data record # */
int     relStart;			/* Offset of relative data in record */
int     cRecords;			/* Number of records in output */
int     lineNum = 0;			/* Line number in input stream */
char  * pszDatabase = NULL;		/* Database file name */
char  * pszInput = NULL;		/* Input file name */
char    szNew[BUFSIZ];			/* Database.NEW */
char    szOld[BUFSIZ];			/* Database.BAK */
char    szCategories[257];		/* Categories in database */
uchar   dbhdr[29];			/* Buffer for database header */
uchar   record[32769];			/* Buffer for a record */
uchar   fields[MAXFIELDS][28];		/* Field definitions */
uchar * lineFields[MAXFIELDS];		/* Fields in a line of the input */
FILE  * hfNew;				/* New database handle */
FILE  * hfOld;				/* Old database handle */
FILE  * hfIn;				/* Input stream handle */

/* This routine exits the program with an error message. */
die(char * message)
	{
	fprintf(stderr, "gdbload(%d): %s\n", lineNum, message);
	exit(1);
	}

/* This routine exits the program with a usage message. */
usage()
	{
	fprintf(stderr,
	        "usage: gdbload [-an] <database file> [<input file>]\n");
	exit(1);
	}

/* This routine parses the command line arguments. */
void parseArgs(int    argc,
	       char * argv[])
	{
					/* Switch defaults: */
	fAdd = FALSE;			/*   Replace the contents of the db */
	fBackup = TRUE;			/*   Back up the unchanged db */

	for (argc--, argv++; argc; argc--, argv++)
		{			/* For each argument */
		if (**argv == '-')	/* Is it an option? */
			for ((*argv)++; **argv; (*argv)++)
					/* For each character in it */	
				switch (**argv)
					{
				case 'a': fAdd    = TRUE;  break;
				case 'n': fBackup = FALSE; break;
				default:
					usage();
					break;
					}
		else if (!pszDatabase)	/* Is it the database name? */
			pszDatabase = *argv;
		else if (!pszInput)	/* Is it the input file name? */
			pszInput = *argv;
		else
			usage();	/* I don't know what it is! */
		}
	if (!pszDatabase)		/* Did we get a database name? */
		usage();
	}

/* Generate the file names we need. */
void makeFileNames()
	{
	char * pch;

	/* Work backwards through the database file name */
	pch = strchr(pszDatabase, '\0');
	while (pch > pszDatabase)
		switch (*(--pch))
			{
		case '.':
			/* Found a (.).  Replace extension with .new and .bak
			 * and return.
			 */
			*pch = '\0';
			strcpy(szNew, pszDatabase);
			strcpy(szOld, pszDatabase);
			strcat(szNew, ".new");
			strcat(szOld, ".bak");
			*pch = '.';
			pch = pszDatabase; /* breaks while loop */
			break;
		
		case '/':
		case '\\':
			/* Found a path separator.  Add .new or .bak
			 * extension and return.
			 */
			strcpy(szNew, pszDatabase);
			strcpy(szOld, pszDatabase);
			strcat(szNew, ".new");
			strcat(szOld, ".bak");
			pch = pszDatabase; /* breaks while loop */
			break;
			}
	
	if (!*szNew)			/* Did we find either one? */
		{
		strcpy(szNew, pszDatabase);
		strcpy(szOld, pszDatabase);
		strcat(szNew, ".new");
		strcat(szOld, ".bak");
		}
	}

/* This array of structures is used to build the record index table at the
 * end of the database file.  The 32 array elements correspond to the 32
 * record types in a database.  For each, the array contains the location
 * (file offset) and size of the first 32 records of that type, plus a
 * link to additional, identical structures in case there are more than
 * 32 records of that type.
 */
struct index_s
	{
	long             location[32];
	int              size[32];
	struct index_s * next;
	} Index[32];

/* This array contains the number of the highest record in use of each type,
 * plus 1.  (E.g. if the highest data record is #11, highIndex[11] == 12;
 * if there are no note records, highIndex[9] == 0.)
 */
int highIndex[32];

/* This routine adds an entry to the index table described above. */
void addIndexEntry(uchar * rechdr,
		   long    location)
	{
	struct index_s * indexptr;	/* Ptr to the index table entry */
	int              recnum;	/* Record number */
	int              i;

	indexptr = &Index[rechdr[0]];	/* Find the head of the index list */
	recnum = rechdr[4] + rechdr[5] * 256;
	if (recnum > highIndex[rechdr[0]] - 1)
		/* We have a new high record number for this type. */
		highIndex[rechdr[0]] = recnum + 1;

	/* Scan through the linked list of index table entries, looking
	 * for the one that should contain this record.  We may have to
	 * allocate and and new entries to the list.
	 */
	for (i = 0; i < recnum / 32; i++)
		if (indexptr->next)
			indexptr = indexptr->next;
		else
			{
			indexptr->next = (struct index_s *)
					 malloc(sizeof (struct index_s));
			memset((char *)indexptr->next, 0,
			       sizeof(struct index_s));
			indexptr = indexptr->next;
			}

	/* Store the location and size in the index table. */
	indexptr->location[recnum % 32] = location;
	indexptr->size[recnum % 32] = rechdr[2] + rechdr[3] * 256;
	}

/* This routine writes the index table into the new database. */
void writeIndexEntries(void)
	{
	struct index_s * indexptr;	/* Index pointer */
	uchar rechdr[6];		/* Record header buffer */
	uchar indexent[8];		/* Index entry */
	uchar intbuf[2];		/* Buffer for integer write */
	int   reclen;			/* Length of the index table */
	int   first;			/* First record # of each type */
	int   i;
	int   j;

	/* Count the number of records.  Note that, for the purposes of the
	 * database manager, we don't care exactly how many records are in
	 * the file; what we want is the sum of the highest record numbers
	 * of each type.
	 */
	cRecords = 0;
	for (i = 0; i < 32; i++)
		cRecords += highIndex[i];

	/* Generate and write the record header for the index table. */
	rechdr[0] = 31;
	rechdr[1] = 2;
	reclen = cRecords * 8 + 6;
	rechdr[2] = reclen % 0x100;
	rechdr[3] = reclen / 0x100;
	rechdr[4] = 0;
	rechdr[5] = 0;
	if (1 != fwrite(rechdr, 6, 1, hfNew))
		die("can't write index table header");

	/* Write the index table itself. */
	for (i = 0; i < 32; i++)	/* For each record type */
		{
		indexptr = &Index[i];
		for (j = 0; j < highIndex[i]; j++)
			{		/* For each record of that type */
			if (j % 32 == 0 && j > 0)
				indexptr = indexptr->next;

			/* Generate and write the index table entry. */
			indexent[0] = indexptr->size[j%32] % 0x100;
			indexent[1] = indexptr->size[j%32] / 0x100;
			indexent[2] = -1;
			indexent[3] = -1;
			if (indexptr->location[j%32])
				indexent[4] = 0;
			else		/* Record was deleted */
				indexent[4] = 0xc0;
			indexent[5] = indexptr->location[j%32] % 0x100;
			indexent[6] = indexptr->location[j%32] / 0x100 % 0x100;
			indexent[7] = indexptr->location[j%32] / 0x10000;
			if (1 != fwrite(indexent, 8, 1, hfNew))
				die("can't write index table entry");
			}
		}
	
	/* Now generate and write the type first table. */
	first = 0;
	for (i = 0; i < 32; i++)
		{
		intbuf[0] = first % 0x100;
		intbuf[1] = first / 0x100;
		if (1 != fwrite(intbuf, 2, 1, hfNew))
			die("can't write type first table entry");
		first += highIndex[i];
		}
	}

/* This routine copies a record from the old database to the new, unchanged. */
void CopyRecord(uchar * rechdr,
		uchar * record,
		int     reclen)
	{
	int recnum;

	if (rechdr[1] & 0x01)		/* Record was deleted */
		return;

	/* Save the record's information in our index table. */
	addIndexEntry(rechdr, ftell(hfNew));

	/* Write it out. */
	if (1 != fwrite(rechdr, 6, 1, hfNew) ||
	    (reclen != 0 && 1 != fwrite(record, reclen, 1, hfNew)))
		die("can't write record");

	/* Update the available data and note record numbers */
	recnum = rechdr[5] * 256 + rechdr[4];
	if (rechdr[0] == 9 && recnum > hiNote-1)
		hiNote = recnum+1;
	if (rechdr[0] == 11 && recnum > hiData-1)
		hiData = recnum+1;
	}

/* This routine copies a record from the old database to the new, but only
 * if -a was specified.
 */
void CopyIfAdd(uchar * rechdr,
	       uchar * record,
	       int     reclen)
	{
	if (fAdd)
		CopyRecord(rechdr, record, reclen);
	}

/* This routine saves the categories of the old database if -a was specified,
 * and ignores them otherwise.
 */
void Categories(uchar * rechdr,
		uchar * record,
		int     reclen)
	{
	if (fAdd)
		{
		szCategories[0] = ';';
		strcpy(szCategories+1, (char *)record);
		strcat(szCategories, ";");
		}
	}

/* This routine copies a field definition from the old database to the new,
 * and saves it in memory for later use also.
 */
void DoFieldDef(uchar * rechdr,
		uchar * record,
		int     reclen)
	{
	int ptr;			/* Data offset for field */

	/* Save the field def. */
	memcpy(fields[cFields], record, reclen);
	ptr = record[3] * 256 + record[2] + 2;
	if (!(record[4] & 0x80))	/* Field contains data? */
		{
		/* Save the beginning of relative data in relStart. */
		if (record[0] == 8 && !(record[4] & 0x20))
			/* Date fields take 3 bytes */
			ptr++;
		if ((record[0] == 0 || record[0] == 9) && !(record[4] & 0x20))	
			/* Byte booleans and radio buttons take 1 byte */
			ptr--;
		if (ptr > relStart)
			/* The absolute data for this field ends later than
			 * any other we have looked at.  Save this new end
			 * point.
			 */
			relStart = ptr;
		}
	cFields++;
	CopyRecord(rechdr, record, reclen);
	}

/* This routine writes an empty viewpoint table to the new database for
 * every viewpoint table found in the old.
 */
void ViewTable(uchar * rechdr,
	       uchar * record,
	       int     reclen)
	{
	rechdr[2] = 6+2;
	rechdr[3] = 0;
	record[0] = 0xff;
	record[1] = 0xff;
	addIndexEntry(rechdr, ftell(hfNew));
	if (1 != fwrite(rechdr, 6, 1, hfNew) ||
	    1 != fwrite(record, 2, 1, hfNew))
		die("can't write viewpoint table");
	}

/* This array contains the routines to be used to handle each type of record
 * in the database during a copy operation.
 */
typedef void (*RecordHandler)(uchar * rechdr,
		              uchar * record,
		              int     reclen);
RecordHandler RecordHandlers[32] =
	{
	CopyRecord, CopyRecord, CopyRecord, CopyRecord,
	CopyRecord, Categories, DoFieldDef, CopyRecord,
	CopyRecord, CopyIfAdd,  ViewTable,  CopyIfAdd,
	CopyRecord, CopyRecord, CopyRecord, CopyRecord,
	CopyRecord, CopyRecord, CopyRecord, CopyRecord,
	CopyRecord, CopyRecord, CopyRecord, CopyRecord,
	CopyRecord, CopyRecord, CopyRecord, CopyRecord,
	CopyRecord, CopyRecord, CopyRecord, CopyRecord
	};

/* This routine copies the appropriate records of the old database to the
 * new.
 */
void copyDatabase()
	{
	uchar rechdr[6];		/* Record header buffer */
	int   reclen;			/* Record length */

	if (!(hfOld = fopen(pszDatabase, "rb")) &&
	    !(hfOld = fopen(pszDatabase, "r")))
		die("can't read database");
	if (!(hfNew = fopen(szNew, "wb")) &&
	    !(hfNew = fopen(szNew, "w")))
		die("can't create .new file");

	/* Copy the database header from old to new.  Note that the database
	 * header in the new database will be overwritten once we know the
	 * number of records and such.  For now, it's just a placeholder.
	 * However, the magic ViewPtHash value is important to read, we would
	 * not know how to compute it otherwise.
	 */
	if (1 != fread(dbhdr, 29, 1, hfOld))
		die("can't read database header");
	addIndexEntry(dbhdr+4, 4);
	if (1 != fwrite(dbhdr, 29, 1, hfNew))
		die("can't write database header");

	/* Various checks for valid data. */
	if (memcmp(dbhdr, "hcD", 4))
		die("not a database file");
	if (dbhdr[11] != 1 || dbhdr[10] != 2)
		die ("wrong version of database format");
	if (dbhdr[12] == '2')
		die ("can't handle appointment book databases");
	if (dbhdr[12] == 'W')
		die ("can't handle world time databases");

	/* Scan through the old database, handling each record we find
	 * according to the handler table above.  Stop when we reach the
	 * index table or end of file.
	 */
	*szCategories = '\0';
	do
		{
		if (1 != fread(rechdr, 6, 1, hfOld))
			break;		/* Can't read, we're done */
		if (rechdr[0] == 31)
			break;		/* Index table, we're done */

		/* Read the record and process it. */
		reclen = rechdr[3] * 256 + rechdr[2] - 6;
		if (reclen != 0 && 1 != fread(record, reclen, 1, hfOld))
			die("read error in middle of record");
		(*RecordHandlers[rechdr[0]])(rechdr, record, reclen);
		} while (1);

	/* Did we find any fields? any categories? */
	if (cFields == 0)
		die("no FIELDDEF records found in database");
	if (!*szCategories)
		strcpy(szCategories, ";;");
	}

/* This routine reads a line of input and breaks it into fields. */
void readLine()
	{
	int     field = 0;		/* Field number being read */
	int     matchQuote = FALSE;	/* Inside quotation marks */
	int     fDone = FALSE;		/* Finished reading */
	uchar   ch;
	uchar * pch = record;

	/* Wipe out any old data. */
	memset(lineFields, 0, sizeof(lineFields));

	/* Forgive the goto...
	 * We want to skip blank and comment lines.
	 */
nextline:
	while ((ch = fgetc(hfIn)) == '\r' || ch == '\n')
		if (ch == '\n')
			lineNum++;
	if (ch == ';')
		{
		while ((ch = fgetc(hfIn)) != '\n' && !feof(hfIn))
			;
		lineNum++;
		goto nextline;
		}

	do				/* For all characters in input */
		{
		if (feof(hfIn))		/* End of file? */
			{
			lineFields[0] = NULL;
			return;
			}

		switch (ch)
			{
		default:		/* Normally, copy chars into buffer */
			*pch++ = ch;
			break;
			
		case '"':		/* Quote marks toggle the flag */
			matchQuote = ! matchQuote;
			break;
		
		case ',':		/* Commas skip to next field unless */
			if (matchQuote)	/* they are inside quotes */
				*pch++ = ch;
			else
				{
				*pch = '\0';
				lineFields[field++] = (uchar *)strdup(record);
				pch = record;
				}
			break;

		case '\\':		/* Backslashes handled specially */
			ch = fgetc(hfIn);
			switch (ch)	/* Look at next character */
				{
			case '\r':	/* Skip carriage returns */
				fgetc(hfIn);
			case '\n':	/* Line feeds are ignored */
				lineNum++;
				break;

			case 'n':	/* \n is a newline */
				*pch++ = '\n';
				break;
			
			case 'r':	/* \r is a carriage return */
				*pch++ = '\r';
				break;
			
			case 'x':	/* \xnn is a hexadecimal char */
				{
				uchar ch2;
				ch = fgetc(hfIn) - '0';
				ch2 = fgetc(hfIn) - '0';
				if (ch > 9)
					ch -= 'A' - '0' + 10;
				if (ch > 9)
					ch -= 'a' - 'A';
				if (ch2 > 9)
					ch2 -= 'A' - '0' + 10;
				if (ch2 > 9)
					ch2 -= 'a' - 'A';
				*pch++ = ch * 16 + ch2;
				break;
				}
			
			case '0': case '1': case '2': case '3':
			case '4': case '5': case '6': case '7':
					/* \nnn is an octal char */
				{
				uchar chstr_1, chstr_2;
				chstr_1 = fgetc(hfIn);
				chstr_2 = fgetc(hfIn);
				*pch++ = 64 * (ch - '0') +
					  8 * (chstr_1 - '0') +
					      (chstr_2 - '0');
				break;
				}
			
			default:	/* Anything else is copied to buffer */
				*pch++ = ch;
				}
			break;
		
		case '\r':		/* Skip carriage returns */
			fgetc(hfIn);
		case '\n':		/* Newlines end the record */
			lineNum++;
			if (matchQuote)
				{
				*pch++ = '\r';
				*pch++ = '\n';
				}
			else
				{
				*pch = '\0';
				lineFields[field++] = (uchar *)strdup(record);
				pch = record;
				fDone = TRUE;
				}
			break;
			}
		ch = fgetc(hfIn);
		} while (!fDone);
	ungetc(ch, hfIn);		/* Put back the last character */
	lineFields[field] = NULL;	/* Mark end of the record */
	}

/* This routine compares two field names and returns TRUE if they match. */
int matchField(uchar * name1,
	       uchar * name2)
	{
	char   namebuf1[21],
	       namebuf2[21],
	     * pch;
	
	/* Copy the field names into local buffers */
	strncpy(namebuf1, (char *)name1, 20);
	strncpy(namebuf2, (char *)name2, 20);
	namebuf1[20] = namebuf2[20] = '\0';

	/* Remove &s from the field names */
	while (pch = strchr(namebuf1, '&'))
		memmove(pch, pch+1, strlen(pch));
	while (pch = strchr(namebuf2, '&'))
		memmove(pch, pch+1, strlen(pch));

	/* Return a case-insensitive comparison of the result. */
	return (!stricmp(namebuf1, namebuf2));
	}

/* This routine reads the line of field names and compares it with the
 * field definitions in the database.
 */
void readFields()
	{
	int i;

	/* First open the input file. */
	if (!pszInput)
		hfIn = stdin;
	else
		if (!(hfIn = fopen(pszInput, "rt")) &&
		    !(hfIn = fopen(pszInput, "r")))
			die("can't open input file");
	memset(mapFields, -1, sizeof(mapFields));

	/* Read the line of field names */
	readLine();
	while (lineFields[cInFields])	/* For each field name listed */
		{
		for (i = 0; i < cFields; i++)
					/* For each field in the database */
			if (matchField(lineFields[cInFields], fields[i]+7))
				{	/* Do they match? */
				mapFields[cInFields] = i;
				break;
				}
		if (mapFields[cInFields] == -1)
			{	/* Didn't find a match for this field name */
			fprintf(stderr, "gdbload(%d): can't match field '%s'\n",
				lineNum, lineFields[cInFields]);
			exit(1);
			}
		if (fields[mapFields[cInFields]][4] & 0x80)
			{	/* Does this field contain data? */
			fprintf(stderr, "gdbload(%d): field '%s' has no data\n",
				lineNum, lineFields[cInFields]);
			exit(1);
			}
		if (fields[mapFields[cInFields]][0] == 16)
			{	/* Is this field app-specific? */
			fprintf(stderr, "gdbload(%d): field '%s' is not "
					"handled by gdbload\n", lineNum,
					lineFields[cInFields]);
			exit(1);
			}
		free(lineFields[cInFields++]);
		}
	if (cInFields == 0)	/* Did we find any fields? */
		die("no field mappings found");
	}

/* This routine adds a note to the database.  The record number of the
 * created note is returned.
 */
int addNote(uchar * line,
	    uchar * fielddef)
	{
	uchar rechdr[6];		/* Record header buffer */
	int   reclen = strlen((char *)line);
					/* Size of note */

	/* Fill in record header */
	rechdr[0] = 9;
	rechdr[1] = 2;
	rechdr[2] = (reclen+6) % 0x100;
	rechdr[3] = (reclen+6) / 0x100;
	rechdr[4] = hiNote % 0x100;
	rechdr[5] = hiNote / 0x100;
	hiNote++;

	/* Write index entry and record. */
	addIndexEntry(rechdr, ftell(hfNew));
	fwrite(rechdr, 6, 1, hfNew);
	fwrite(line, reclen, 1, hfNew);

	/* Return note record number. */
	return hiNote-1;
	}

/* This routine adds the categories in a data record to the list of
 * categories used in the database.
 */
void addCategories(uchar * line)
	{
static	char   buf[256];		/* Buffer for category name */
	char * end;

	if (!*line)			/* Any categories in this record? */
		return;
	do
		{			/* For each category in the record */
		end = strchr((char *)line, ';');
		if (end)
			*end = '\0';
		sprintf(buf, ";%s;", line);	/* Copy to buffer */
		if (!strstr(szCategories, buf))
					/* Is it already on the list? */
			if (strlen(szCategories) + strlen(buf) - 2 > 256)
					/* Would it overflow the list? */
				die("too many categories");
			else
				if (!strcmp(szCategories, ";;"))
					/* Is there anything on the list? */
					strcpy(szCategories, buf);
				else
					strcat(szCategories, buf+1);
		if (end)
			{	
			*end = ';';
			line = (uchar *)end+1;
			}
		} while (end && *line);
	}

/* This routine adds a field of a record to the record buffer. */
void DoField(uchar * line,
             uchar * fielddef,
             int     note,
             int   * endrec)
	{
	int offset = fielddef[2] + fielddef[3] * 256;

	switch (*fielddef)
		{
	case 6:				/* Category field */
		addCategories(line);	/*   Add to categ list */
		/* FALL-THROUGH */
	case 2:				/* String fields of various types */
	case 3:
	case 4:
	case 5:
	case 13:
	case 15:
		if (!*line)		/* Is there any text? */
			/* No, point to the null byte at the beginning of
			 * the relative data.
			 */
			record[offset] = relStart % 0x100,
			record[offset+1] = relStart / 0x100;
		else
			{
			/* Yes, so point to the end of the relative data
			 * area, and copy the string there.
			 */
			record[offset] = *endrec % 0x100;
			record[offset+1] = *endrec / 0x100;
			strcpy((char *)record+*endrec, (char *)line);
			*endrec += strlen((char *)line) + 1;
			}
		break;

	case 10:			/* Note field */
		if (!*line)		/* Is there any text? */
			/* No, so put a -1 for the note record number */
			record[offset] = -1,
			record[offset+1] = -1;
		else
			/* Yes, so put in the note record number */
			record[offset] = note % 0x100,
			record[offset+1] = note / 0x100;
		break;

	case 9:				/* Radio button */
		if (*line && strcmp((char *)line, "0"))
			/* If anything other than 0 is in the field, set
			 * the radio button value.
			 */
			record[offset] = fielddef[5];
		break;
	
	case 8:				/* Date field */
		{
		long date = atol((char *)line);
		int year = date / 10000;
		int month = date / 100 % 100;
		int day = date % 100;

		if (!*line)		/* Is there any text? */
			{
			/* No, put in a null date. */
			record[offset] = 
			record[offset+1] = 
			record[offset+2] = -1;
			break;
			}

		/* Check the date for validity. */
		if (year < 1900 || year > 2099 ||
		    month < 1 || month > 12 ||
		    day < 1 || day > 31)
			{
			fprintf(stderr, "gdbdump(%d): %s is not a valid date\n",
				lineNum, line);
			exit(1);
			}

		/* Put in the date. */
		record[offset] = year - 1900;
		record[offset+1] = month - 1;
		record[offset+2] = day - 1;
		break;
		}
	
	case 7:				/* Time */
		{
		int time = atoi((char *)line);
		int hour = time / 100;
		int minutes = time % 100;

		if (!*line)		/* Is there any text? */
			{
			/* No, so put in a null time */
			record[offset] = 0;
			record[offset+1] = 0x80;
			break;
			}

		/* Check the time for validity */
		if (hour < 0 || hour > 23 ||
		    minutes < 0 || minutes > 59)
			{
			fprintf(stderr, "gdbdump(%d): %s is not a valid time\n",
				lineNum, line);
			exit(1);
			}

		/* Put in the time */
		time = hour * 60 + minutes;
		record[offset] = time % 0x100;
		record[offset+1] = time / 0x100;
		break;
		}
	
	case 0:				/* Bit in a byte (checkbox) */
	case 1:				/* Bit in a word (checkbox) */
		{
		int word = record[offset] + record[offset+1] * 256;
		int mask = fielddef[5] + fielddef[6] * 256;

		/* Is there anything other than 0 in the text? */
		if (*line && strcmp((char *)line, "0"))
			/* Yes, set the bit. */
			word |= mask;
		else
			/* No, clear it. */
			word &= ~mask;
		record[offset] = word % 0x100;
		record[offset+1] = word / 0x100;
		break;
		}
		}
	}

/* This routine adds the input records to the database. */
void addRecords()
	{
	uchar rechdr[6];		/* Record header buffer */
	int   endrec;			/* Offset of first unused by in rec */
	int   note;			/* Record # of associated note */
	int   i;

	do
		{			/* For all records */
		readLine();
		if (!lineFields[0])	/* End of file? */
			return;

		/* Check for correct number of fields on line. */
		if (!lineFields[cInFields-1])
			die("not enough fields specified");
		if (cInFields < MAXFIELDS && lineFields[cFields])
			die("too many fields specified");

		/* Does the record have a note? */
		for (i = 0; i < cInFields; i++)
			if (fields[mapFields[i]][0] == 10 &&
			    lineFields[i][0])
				/* Yes.  Add it to database. */
				note = addNote(lineFields[i],
				               fields[mapFields[i]]);

		/* Now do the record */
		endrec = relStart + 1;
		memset(record, 0, relStart+1);
		for (i = 0; i < cInFields; i++)
			{
			DoField(lineFields[i], fields[mapFields[i]], note,
				&endrec);
			free(lineFields[i]);
			}

		/* Generate the data record header */
		rechdr[0] = 11;
		rechdr[1] = 2;
		rechdr[2] = (endrec + 6) % 0x100;
		rechdr[3] = (endrec + 6) / 0x100;
		rechdr[4] = hiData % 0x100;
		rechdr[5] = hiData / 0x100;
		hiData++;

		/* Write the index entry and the record. */
		addIndexEntry(rechdr, ftell(hfNew));
		fwrite(rechdr, 6, 1, hfNew);
		fwrite(record, endrec, 1, hfNew);
		} while(1);
	}

/* This routine writes out the database header, the categories record,
 * and the index table.
 */
void writeHeader()
	{
	uchar rechdr[6];		/* Record header buffer */
	int   reclen;			/* Record length */
	long  index;			/* Offset of index in file */

	/* Write out categories record. */
	rechdr[0] = 5;
	rechdr[1] = 2;
	reclen = strlen(szCategories) - 2 /* extra semicolons */
				      + 1 /* null terminator */
				      + 6 /* record header */ ;
	rechdr[2] = reclen % 0x100;
	rechdr[3] = reclen / 0x100;
	rechdr[4] = 0;
	rechdr[5] = 0;
	addIndexEntry(rechdr, ftell(hfNew));
	fwrite(rechdr, 6, 1, hfNew);
	fwrite(szCategories + 1, strlen (szCategories) - 2, 1, hfNew);
	fwrite(rechdr+5, 1, 1, hfNew);
	
	/* Update the database header with the location of the index table. */
	index = ftell(hfNew);
	dbhdr[18] = index % 0x100;
	dbhdr[19] = index / 0x100 % 0x100;
	dbhdr[20] = index / 0x10000 % 0x100;
	dbhdr[21] = index / 0x1000000;
	writeIndexEntries();

	/* Update the database header and write it out. */
	dbhdr[13] |= 0x02;		/* Database has been modified */
	dbhdr[16] = cRecords % 0x100;
	dbhdr[17] = cRecords / 0x100;
	fseek(hfNew, 0, SEEK_SET);
	fwrite(dbhdr, 29, 1, hfNew);
	}

/* Main program. */
main(int    argc,
     char * argv[])
	{
	parseArgs(argc, argv);
	makeFileNames();
	copyDatabase();
	readFields();
	addRecords();
	writeHeader();
	fclose(hfIn);
	fclose(hfOld);
	fclose(hfNew);
	unlink(szOld);
	rename(pszDatabase, szOld);
	rename(szNew, pszDatabase);
	if (!fBackup)
		unlink(szOld);
	exit(0);
	}
