/*
READEL.C
by Paul S. Hirose, 1992 Jan 4
Handles orbital element files and reads the elements.

This file is in the public domain.

400 - 499

library functions, typedefs, and #defines used in this file:

atof atoi cos EOF fclose FILE fopen free fseek ftell getc isdigit
isspace NULL printf strcmp strcpy strncpy
*/

#include "SEESAT.H"	/* global header */

#if ECOC
extern void free(), printf();
extern char *strcpy(), *strncpy();
extern long int fseek(), ftell();
extern double atof(), cos();
extern FILE *fopen();
#endif

/* Number of lines on your terminal.  Only affects the listing of satellite
names.  You will get LINES-1 rows of names, followed by "more>" on a separate
line.  The cursor will be positioned to the right of '>'. */
#define LINES 24

/* Number of chars to allow for satellite name in index. */
#define LNAME 22

#define BSIZE 50	/* # of index entries in a storage block */

#define NSTAT 0		/* normally 0.  Non-zero makes static functions
			and data extern, for testing */

#define MAGCOL 31	/* position (far left = 0) of magnitude column */


#if NSTAT
/* define & declare all functions in this file as extern */
#define STATIC
#define SC extern

#else
#define STATIC static
#define SC static
#endif

/*############################ LOCAL DATA ############################*/

/* RAAN and epoch.  Not modified by NOMINAL and ACTUAL commands. */
STATIC double node, epocho;

/* nominal & actual launch times */
STATIC double tnom, tact;

STATIC FILE *fp = NULL;		/* orbital element file */
STATIC long int fpos_eof;       /* saves last sat loaded file position */

/* Index entry.  Each satellite has one. */
struct indent {
	char name[LNAME+1];	/* satellite name */
	long int offset;	/* location of element set in file */
	int ahpr_flag:3;        /* above horizon or printed flag */
	int db_flag:2;          /* 1 if DB statement was specified */
	int filler:3;           /* filler to next byte boundary */
	unsigned int norad_nbr; /* Norad satellite number */
	long int ahpr_jd;       /* date and time of above horizon */
	double ahpr_mins;       /* or last printed values */
};

struct bloc {	/* space for BSIZE index entries */
	struct indent ents[BSIZE];
	struct bloc *next;	/* link to next block */
};

STATIC struct bloc *first;	/* ptr to first block */
STATIC struct bloc *curr_db_block;
STATIC struct indent *curr_db_index;
STATIC unsigned curr_db_count;
STATIC unsigned curr_db_total;

STATIC unsigned sets;		/* number of element sets in file */
STATIC unsigned maxnc = LNAME;	/* max no. of name chars to read */

/*######################## LOCAL FUNCTIONS ########################*/

SC void adjel();	/* adjust RAAN & epoch for launch time */
SC double angle();	/* convert string to angle in radians */
SC int csum();		/* checksums a line of data */
SC double elmod();	/* convert cmd line arg to double */
SC double epjd();	/* converts NORAD Epoch string to epoch */
SC int getel();		/* transfers elements from file to variables */
SC void nullsp();	/* replace spaces in buffer with nulls */
SC double sci();	/* returns value of scientific notation #'s */
SC char *sknull();	/* skip leading nulls */

/*############################## CODE ##############################*/


STATIC void
adjel()
/* adjust RAAN & epoch for difference between actual & nominal launch time */
{
	double deltat;

	deltat = tact - tnom;

	epoch = epocho + deltat;
	xnodeo = fmod2p(node + 4.37527e-3 * deltat);
	iflag = 1;
}


STATIC double
angle(string)
char *string;
/* Returns the radian value of the angle (degrees) in "string".
Leading nulls on "string" will be skipped, so it had better contain
at least one digit. */
{
	return atof(sknull(string)) * de2ra;
}


void
aop()		/* alter argument of the perigee */
{
	omegao = elmod() * de2ra;
}


void
b()		/* alter bstar */
{
	bstar = elmod();
}


STATIC int
csum(line)
char *line;
/* Does a checksum modulo 10 on the given line.  Digits = their
value, '-' = 1, all other chars = 0.  Returns 0 if ok. */
{
	char c;
	int i = 0;	/* checksum accumulator */
	int count = 69;		/* length of data line */

	/* accumulate checksum from all but the last char in the line,
	which is the desired checksum */

	while (--count) {
		if (isdigit(c = *line++))
			i += c - '0';
		else if (c == '-')
			++i;
		/* all other chars = 0 */
	}

	/* Convert accumulated # to mod 10, subtract desired sum */
	return i % 10 - (*line - '0');
}


void
ein()		/* alter eccentricity */
{
	eo = elmod();
}


STATIC double
elmod()
/* tasks common to nearly all commands that modify orbital elements */
{
	iflag = 1;	/* since elements will be altered */
	return atof(*tokp);
}


STATIC double
epjd(buf)
char *buf;
/* Given pointer to the orbital elements Epoch field, returns Julian Date
(unit = minutes).  Positions for Days tens and hundreds and Years tens digits
must contain either a digit or '\0'.  Format of Epoch field:  YYDDD.dddddddd
*/
{
	double day;
	int year;

	day = atof(sknull(buf + 2));

	buf[2] = '\0';			/* day 100s digit = null */
	year = atoi(sknull(buf));
	year += (year < 57) ? 1999 : 1899;

	/* Subtract .5 because julday() returns JD at 12h on given day */
	return (julday(year, 11, 31) + day - .5) * xmnpda;
}


void
epoc()
/* sets epoch of elements from command line */
{
	nullsp(*tokp);		/* turn any spaces to nulls */
	epoch = epocho = epjd(*tokp);
#if ENPRE	/* precession code enabled */
	/* initialize direction cosines to precess R.A./dec. */
	inpre();
#endif
}


STATIC int
getel()
/* Sets the global orbital element variables and name[] (satellite name)
using data from the currently open element file.  Initializes precession.
Sets global iflag to 1.  The position in the file must be at the satellite
name or in white space preceding the name.  Returns -1 if incorrect format in
file, 0 otherwise.  Prints warning if checksum error in file, but will still
attempt to use elements. */
{
#if 0
	double ao, a1, delo, del1;	/* currently not used */
#endif
	double temp;
	int check;
	char line1[70], line2[70];

	/* extract name of satellite */
	if ((check = getlin(line1, 70, fp)) == 0)	/* EOF */
		return -1;

	/* if the current file position is greater than the position */
	/* of the last loaded satellite, return a -1                 */
	if (ftell(fp) > fpos_eof)
		return -1;

	if  (showtle == 1)
		printf("%s\n", line1);
	strncpy(name, line1, maxnc);
	name[maxnc] = '\0';

	/* If line is long enough, extract magnitude and set mag flag. */

	if (check > MAGCOL) {
		nullsp(line1);
		abmag = atof(sknull(line1 + MAGCOL));
		mflag = '\001';
	} else {
		abmag = 0.;
		mflag = '\000';
	}

	/* Get first data line from file.  Verify that it's 69 chars long &
	begins with '1' */

	if (getlin(line1, 70, fp) != 69 || *line1 != '1')
		return -1;	/* incorrect format */
	check = csum(line1);	/* Line 1 checksum */

	/* get second data line */
	if (getlin(line2, 70, fp) != 69 || *line2 != '2')
		return -1;	/* incorrect format */
	check += csum(line2);	/* add Line 2 checksum */

	/* print both lines at console */
	if  (showtle == 1)
		printf("%s\n%s\n", line1, line2);

	if (check)
		printf("CAUTION:  FAILED CHECKSUM\n");

	/* replace spaces with nulls */
	nullsp(line1);
	nullsp(line2);

	norad_sat_nbr = atoi(line1 + 2);
	xmo = angle(line2 + 43);
	xnodeo = node = angle(line2 + 17);
	omegao = angle(line2 + 34);
	xincl = angle(line2 + 8);
	FTEST((400, 4, 3, &xmo, &xnodeo, &omegao, &xincl));

	/* put a decimal point in front of eccentricity, decode it */
	line2[25] = '.';
	eo = atof(line2 + 25);

	temp = twopi / (xmnpda * xmnpda);

	/* Make sure mean motion is null-terminated, since rev. no.
	may immediately follow. */
	line2[63] = '\0';
	xno = atof(sknull(line2 + 52)) * temp * xmnpda;

	if (line1[35])		/* field isn't blank */
		xndt2o = atof(sknull(line1 + 33)) * temp;
	else
		xndt2o = 0.;
	xndd6o = sci(line1 + 44) * temp / xmnpda;

	bstar = sci(line1 + 53);
	epoch = epocho = epjd(line1 + 18);
	ETEST((402, 5, 3, &eo, &xno, &xndt2o, &xndd6o, &bstar));
	FTEST((403, 1, 3, &epoch));

	toffs = 0.;	/* reset OFFSET */
	iflag = 1;

#if ENPRE	/* precession code enabled */
	/* initialize direction cosines to precess R.A./dec. */
	inpre();
#endif

#if 0
	/* Spacetrack Report #3 code for distinguishing between near earth or
	deep space satellites.  Provided for completeness but not used. */

	/* Compute period.  Object is deep space if <= 6.4 revs/day */
	a1 = POW(xke / xno, tothrd);
	temp = cos(xincl);
	temp = 1.5 * ck2 * (3. * temp * temp - 1.) /
	  POW(1. - eo * eo, 1.5);
	del1 = temp / (a1 * a1);
	ao = a1 * (1. - del1 * (.5 * tothrd + del1 *
	  (1. + 134. / 81. * del1)));
	delo = temp / (ao * ao);

	if (xno / (1. + delo) <= .0279253)
		return 1;	/* deep space */
	else
#endif
		return 0;	/* near earth */
}


#if LASERC == 0
int getlin(dest, size, filep)
int size;	/* must be at least 2 */
char *dest;
FILE *filep;
/* Copy a line from filep into dest[].  "Size" is length of dest[], and must
be at least 2.  Any leading white space is skipped.  String copied to dest[]
will be null terminated.
	 Exit occurs when:  1) size - 1 chars have been copied, or 2) '\n'
detected (the '\n' won't be sent to dest[]), or 3) EOF detected.  In case 1
or 2, the next read from the file is guaranteed to start at beginning of the
next line.  Returns # of chars copied to dest[] (final null not counted). */
{
	int c, i;

	i = size - 1;	/* i = # of chars desired */

	/* go past leading white space */
	while (isspace(c = getc(filep)));

	/* The NASA two line elements file has a - comment - as first line */
	/* Just skip all of this line */

	if  (c == '-') {
		while ( (c = getc(filep)) != '\n');
		while (isspace(c = getc(filep)));
	}

	DTEST((450, 2, &i, &c));

	/* Move chars to buffer till we we reach end of line, or
	have gotten # requested, or end of file. */

	do {	if (c == EOF)
			break;
		*dest++ = c;
	} while (--i && (c = getc(filep)) != '\n');

	*dest = '\0';

	DTEST((451, 2, &i, &c));
	STEST((452, dest));

	/* At this point, i is how many chars we've fallen short */

	if (!i)		/* requested # of chars were found */
		while ((c = getc(filep)) != '\n' && c != EOF)
			;	/* position at start of next line */

	DTEST((453, 2, &i, &c));
	return size - 1 - i;	/* how many copied */
}
#endif

#if LASERC
/* A bug in Laser C's ftell() forces us to open element file in binary mode,
which in turn necessitates this special version of getlin(). */

int getlin(dest, size, filep)
int size;	/* must be at least 2 */
char *dest;
FILE *filep;
/* Copy a line from filep into dest[].  "Size" is length of dest[], and must
be at least 2.  Any leading white space is skipped.  String copied to dest[]
will be null terminated.
	 Exit occurs when:  1) size - 1 chars have been copied, or 2) '\n'
detected (the '\n' won't be sent to dest[]), or 3) EOF detected.  In case 1
or 2, the next read from the file is guaranteed to start at beginning of the
next line.  Returns # of chars copied to dest[] (final null not counted). */
{
	int c, i;

	i = size - 1;	/* i = # of chars desired */

	/* go past leading white space */
	while (isspace(c = getc(filep)));

	/* Move chars to buffer till we we reach end of line, or
	have gotten # requested, or end of file. */

	do {	if (c == EOF  ||  c == 0x1A)
			break;		/* physical or logical end of file */
		*dest++ = c;
	} while (--i && (c = getc(filep)) != '\n');

	/* if last char written was a CR, kill it */
	if (*(dest - 1) == 0xD) {
		--dest;
		++i;
	}

	*dest = '\0';

	/* At this point, i is how many chars we've fallen short */

	if (!i)		/* requested # of chars were found */
		while ((c = getc(filep)) != '\n' && c != EOF);
				/* position at start of next line */

	return size - 1 - i;	/* how many copied */
}

#endif


void
hfree()
/* Follows the chain of index blocks, frees each one */
{
	struct bloc *blocp, *temp;

	for (blocp = first; blocp; blocp = temp) {
		temp = blocp->next;
		free((VOIDP) blocp);
	}
	first = NULL;
}


void
inc()		/* alter inclination */
{
	xincl = elmod() * de2ra;
}


void
indx()
/* List all satellites in the index generated by opn(). */
{
	unsigned total, u, line, column;
	struct bloc *blocp;
	struct indent *indexp;

	total = sets;	/* total no. of element sets in file */
	line = LINES - 1;
	column = 3;

	for (blocp = first;  blocp;  blocp = blocp->next) {
		indexp = blocp->ents;		/* 1st index entry in block */
		for (u = BSIZE;	u--; ) {
			if (line == 0) {	/* screen is full */
				printf("more");
				tok();		/* cmd line prompt */
				if (*tokp) {	/* cmd was typed */
					--tokp;
					return;
				} else		/* only a CR was typed */
					line = LINES - 1;
			}
			if (--column)
				printf("%-25s", indexp->name);
			else {		/* rightmost column */
				printf("%s\n", indexp->name);
				column = 3;
				--line;
			}
			++indexp;
			if (--total == 0)
				break;
	}	}
	if (column != 3)
		printf("\n");

	--tokp;		/* because this cmd takes no args */
}

void load()
/* Takes the next argument on the command line as the name of the satellite
to load from the currently open file. */
{
	struct bloc *blocp;	/* block pointer */
	struct indent *indexp;	/* index pointer */
	unsigned u, total;
	char *cp;

	cp = *tokp;		/* name of satellite */

	/* Sequential search for a matching name.  Outer loop checks all
	blocks, inner loop checks all entries in a block. */
	total = sets;		/* no. of sats in index */
	for (blocp = first; blocp != NULL; blocp = blocp->next) {
		indexp = blocp->ents;
		for (u = BSIZE; u && total; --u, --total) {
			if (strcmp(indexp->name, cp) == 0)
				goto done;	/* found matching name */
			++indexp;
	}	}
	done:
	if (total) {
	/* found matching name; go to its file position */
		fseek(fp, indexp->offset, 0);

		/* get elements, report status */
		if (getel() == 0)
			return;
		else
			printf("ABORTED LOAD:  INCORRECT FORMAT\n");
	} else
		printf("NO SUCH SATELLITE\n");
	LONGJMP(reset, 1);
}

void loadn()
/* Takes the next argument on the command line as the NORAD number of the */
/* satellite to load from the currently open file. */
{
	struct bloc *blocp;	/* block pointer */
	struct indent *indexp;	/* index pointer */
	unsigned u, total;
	unsigned int in_norad_nbr;

	in_norad_nbr = atoi(*tokp);  /* Norad number of satellite */

	/* Sequential search for a matching number.  Outer loop checks all
	blocks, inner loop checks all entries in a block. */
	total = sets;		/* no. of sats in index */
	for (blocp = first; blocp != NULL; blocp = blocp->next) {
		indexp = blocp->ents;
		for (u = BSIZE; u && total; --u, --total) {
			if (in_norad_nbr == indexp->norad_nbr)
				goto done;	/* found matching nbr */
			++indexp;
	}	}
	done:
	if (total) {
	/* found matching nbr; go to its file position */
		fseek(fp, indexp->offset, 0);

		/* get elements, report status */
		if (getel() == 0)
			return;
		else
			printf("ABORTED LOAD#: INCORRECT FORMAT\n");
	} else
		printf("NO SUCH NORAD SATELLITE NUMBER\n");
	LONGJMP(reset, 1);
}

void ma()		/* alter mean anomaly */
{
	xmo = elmod() * de2ra;
}


void
mm()		/* alter mean motion */
{
	xno = elmod() * twopi / xmnpda;		/* radians/min */
}


void
next()
/* load next sat in file */
{
	if (getel() == -1) {
		printf("END OF ELEMENTS\n");
		LONGJMP(reset, 1);
	}
	--tokp;		/* because this cmd takes no args */
}

int nextsat()
/* load next satellite in file, returns 1 if eof */
{
	if (getel() == -1) {
		return 1;
	}
	return 0;
}

void reset_tle_ptr()
{
	  fseek(fp,0L, 0);
}

STATIC void
nullsp(buf)
char *buf;
/* replaces each occurrence of ' ' in string "buf" with a null */
{
	char c;

	while (c = *buf) {
		if (c == ' ')
			*buf = '\0';
		++buf;
}	}


void
opn()
/* Take the next argument on the command line as the name of the orbital
element file to open.  Builds the index used by index() and load().  If an
elements file is already open, it will be closed and the associated index
deleted before opening the new file.  External "fp" is set to the returned
value from fopen(). */
{
	struct bloc *blocp, *last;
	struct indent *indexp;
	FILE *tfp;
	long int fpos;		/* position in file */
	unsigned int n50;
	char *cp, buffer[LNAME + 1];
	char linebuff[70];

	strcpy(max_yyddd,"00000.00000000");
	strcpy(min_yyddd,"99999.99999999");

	cp = *tokp;		/* satellite name */
#if LASERC
	if ((tfp = fopen(cp, "br")) == NULL) {
#else
	if ((tfp = fopen(cp, "r")) == NULL) {
#endif
		printf("CAN'T OPEN %s\n", cp);
		LONGJMP(reset, 1);
	}

	if (fp) {	/* close old file, delete its index */
		fclose(fp);
		hfree();		/* release index space */
	}

	fp = tfp;
	sets = 0;	/* counts # of element sets processed */
	n50 = 0;	/* storage remaining */
	fpos = 0L;	/* position in file */

	while (getlin(buffer, maxnc + 1, fp) != 0) {
	/* got a satellite name; add it to index */
		if (n50 == 0) {	/* need more storage */
			blocp = (struct bloc *) MALLOC(sizeof (struct bloc));
			if (blocp == NULL) {
				printf("MEMORY FULL: FIRST %u SATELLITES LOADED, ERROR AT:%s\n",
					sets,buffer);
				return;
			}
			if (first == NULL)	/* first block */
				first = blocp;
			else
				last->next = blocp;	/* link in */
			last = blocp;		/* new last block */
			blocp->next = NULL;	/* end of chain */
			indexp = blocp->ents;
			n50 = BSIZE;	/* reset storage counter */
		}
		/* storage is available; add the element set to the index */

		/* eliminate any trailing spaces on name */
		for (cp = buffer; *++cp; )
			;	/* find end of name string */
		while (isspace(*--cp))
			;	/* point to last non-white char */
		cp[1] = '\0';

	/*	strcpy(indexp->name, stoup(buffer));    */
		strcpy(indexp->name, buffer);
		indexp->offset = fpos;		/* location in file */

		indexp->db_flag = 0;

		getlin(linebuff, 70 , fp);	/* move past Line 1 */

		if  (strncmp(linebuff+18,max_yyddd,14) > 1) {
		    strncpy(max_yyddd,linebuff+18,14);
		    strcpy(max_satname,buffer);
		}
		if  (strncmp(linebuff+18,min_yyddd,14) < 1) {
		    strncpy(min_yyddd,linebuff+18,14);
		    strcpy(min_satname,buffer);
		}

		/* save the Norad satellite number */
		indexp->norad_nbr = atoi(linebuff+2);

		getlin(linebuff, 70 , fp);	/* move past Line 2 */
		++sets;			/* # of satellites found */
		--n50;		/* space remaining in index */
		++indexp;	/* point to next free place in index */
		fpos = ftell(fp); /* update file position */
		fpos_eof = fpos;  /* update last satellite file position */
	}
	fseek(fp, 0L, 0);	/* rewind */
	if  (reportlevel == 1) {
	    printf("complete; %u satellites found\n", sets);
	}

}

void summary()
{
	char epoch_str[15];
	long int work_li;

	if (strcmp(min_yyddd,"99999.99999999") != 0) {
	   work_li = (epjd(min_yyddd)) / 1440;
	   strcpy(epoch_str,jdstr(work_li));
	   epoch_str[4] = '\000';
	   epoch_str[8] = '\000';
	   if (epoch_str[10] == ' ')
	      epoch_str[10] = '\000';
	   else
	      epoch_str[11] = '\000';
	   printf("\nData summary for two line orbital elements file :\n");
	   printf(" Oldest epoch %s %s, %s for %s\n",
			epoch_str+5,
			epoch_str+9,
			epoch_str,
			min_satname);
	}

	if (strcmp(max_yyddd,"00000.00000000") != 0) {
	   work_li = (epjd(max_yyddd)) / 1440;
	   strcpy(epoch_str,jdstr(work_li));
	   epoch_str[4] = '\000';
	   epoch_str[8] = '\000';
	   if (epoch_str[10] == ' ')
	      epoch_str[10] = '\000';
	   else
	      epoch_str[11] = '\000';
	   printf(" Newest epoch %s %s, %s for %s\n",
			epoch_str+5,
			epoch_str+9,
			epoch_str,
			max_satname);
	}
	--tokp;
}

void raan()		/* alter Right Ascension of Ascending Node */
{
	xnodeo = node = elmod() * de2ra;
}

STATIC double sci(string)
char *string;
/* Converts quasi scientific notation to double.  Format:  SnSn where S is
either '-' or '\0' and n represents a sequence of 1 or more digits.  An
implied decimal point exists to the left of the left n.  Total length 27
chars max. */
{
	char buf[30], *bufptr;

	bufptr = buf;

	if (string[1] == '\0')
		return 0.;

	/* get significand */
	if (*string == '-')
		*bufptr++ = '-';
	*bufptr = '.';
	while (isdigit(*++bufptr = *++string))
		;	/* copy significand */
	*bufptr++ = 'E';

	/* get exponent */
	if (*string == '\0')	/* no sign on exponent */
		++string;

	strcpy(bufptr, string);		/* copy exponent */
	return atof(buf);
}


void
setact()
/* Set actual time of launch, adjust elements accordingly */
{
	tact = tokmin();
	adjel();
}


void
setlen()
/* Set the max number of chars to be read from the name line in file.
LNAME is size of the space  allocated for the name, in the index. */
{
	int i;

	i = atoi(*tokp);
	if (i > LNAME)
		maxnc = LNAME;
	else
		maxnc = i;
}


void
setnam()
/* manually set name of satellite */
{
	strncpy(name, *tokp, 22);
	name[22] = '\0';
}


void
setnd()
/* manually input first derivative of mean motion */
{
	xndt2o = elmod() * twopi / (xmnpda * xmnpda);
}


void
setndd()
/* manually input second derivative of mean motion */
{
	xndd6o = elmod() * twopi / (xmnpda * xmnpda * xmnpda);
}


void
setnom()
/* set nominal time of launch, adjust the elements accordingly */
{
	tnom = tokmin();
	adjel();
}


STATIC char *
sknull(cptr)
char *cptr;
/* Skip nulls.  Returns pointer to first non-null char in "cptr". */
{
	while (!*cptr)
		++cptr;

	return cptr;
}

int search_set_index(in_norad_nbr) /* Searches table for norad number and */
				   /* sets up the pointer curr_db_index   */
unsigned int in_norad_nbr;
{
	struct bloc *blocp;	/* block pointer */
	struct indent *indexp;	/* index pointer */
	unsigned u, total;

	/* Sequential search for a matching norad nbr. Outer loop checks all
	blocks, inner loop checks all entries in a block. */
	total = sets;		/* no. of sats in index */
	for (blocp = first; blocp != NULL; blocp = blocp->next) {
		indexp = blocp->ents;
		for (u = BSIZE; u && total; --u, --total) {
		    if (in_norad_nbr == indexp->norad_nbr) {
		       curr_db_index = indexp;
		       return 0;
		    }
		    ++indexp;
		}
	}

	printf("FAILED TO FIND SATELLITE #%5d\n",in_norad_nbr);
	LONGJMP(reset, 1);
	return 1;
}

int save_ah_values(in_norad_nbr,in_jd,in_mins)
unsigned int in_norad_nbr;
long int in_jd;
double in_mins;
{
	double temp1, temp2;

	if (in_norad_nbr != curr_db_index->norad_nbr) {
	   printf("FAILED TO SAVE ABOVE HORIZON VALUES, SATELLITE #%5d\n",
			in_norad_nbr);
	   LONGJMP(reset, 1);
	}

	if  (curr_db_index->ahpr_flag == 2) {
	    if  (   (in_jd > curr_db_index->ahpr_jd)
		 || (   (in_jd == curr_db_index->ahpr_jd)
		     && (in_mins > curr_db_index->ahpr_mins)
		    )
		) {
		    curr_db_index->ahpr_flag = 0;
		}
	    else {
		return 1;
	    }
	}

	/* If there are above horizon values already stored, check to */
	/* see if they are old i.e. more than 60 minutes old, and     */
	/* replace them if so. The 60 minutes is the default, can be  */
	/* changed by the ORBITMINS command                           */

	if  (curr_db_index->ahpr_flag == 1) {
	    temp1 = in_jd * xmnpda - 720. + in_mins;
	    temp2 = curr_db_index->ahpr_jd * xmnpda - 720.
			 + curr_db_index->ahpr_mins;
	    if  (temp1 > temp2 + def_orbit_mins) {
		 curr_db_index->ahpr_flag = 0;
	    }
	}

	if  (curr_db_index->ahpr_flag == 0) { /* no above horizon values exist */
	    curr_db_index->ahpr_jd = in_jd;
	    curr_db_index->ahpr_mins = in_mins;
	    curr_db_index->ahpr_flag = 1;
	}

	return 0;
}

int restore_ah_values(in_norad_nbr,in_jd_ptr,in_mins_ptr)
unsigned int in_norad_nbr;
long int *in_jd_ptr;
double *in_mins_ptr;
{
	if (in_norad_nbr != curr_db_index->norad_nbr) {
	   printf("WARNING:COULD NOT RESTORE VALUES FOR SATELLITE #%5d",
		  in_norad_nbr);
	   return 0;
	}

	if  (curr_db_index->ahpr_flag == 1) { /* above horizon values exist */
	    *in_jd_ptr = curr_db_index->ahpr_jd;
	    *in_mins_ptr = curr_db_index->ahpr_mins;
	    return 1;
	}
	return 0;
}

void save_pr_values(in_norad_nbr,in_jd,in_mins)
unsigned int in_norad_nbr;
long int in_jd;
double in_mins;
{
	if (in_norad_nbr != curr_db_index->norad_nbr) {
	    printf("FAILED TO SAVE LAST PRINT VALUES,NO SUCH SATELLITE #%5d\n",
			in_norad_nbr);
	    LONGJMP(reset, 1);
	    return;
	}

	curr_db_index->ahpr_jd = in_jd;
	curr_db_index->ahpr_mins = in_mins;
	curr_db_index->ahpr_flag = 2;
	return;
}

void reset_ahpr_flags()
/* Resets all ahpr_flags to zero */
{
	struct bloc *blocp;     /* block pointer */
	struct indent *indexp;  /* index pointer */
	unsigned u;

	/* Sequential search all entries.  Outer loop checks all
	blocks, inner loop checks all entries in a block. */

	for (blocp = first; blocp != NULL; blocp = blocp->next) {
		indexp = blocp->ents;
		for (u = BSIZE; u ; --u) {
		    indexp->ahpr_flag = 0;
		    ++indexp;
		}
	}
}

void dbs()
/* Takes the next argument on the command line as the name of the satellite
to set db flag to 1 */
{
	struct bloc *blocp;	/* block pointer */
	struct indent *indexp;	/* index pointer */
	unsigned u, total;
	char *cp;

	cp = *tokp;		/* name of satellite */

	/* Sequential search for a matching name.  Outer loop checks all
	blocks, inner loop checks all entries in a block. */
	total = sets;		/* no. of sats in index */
	for (blocp = first; blocp != NULL; blocp = blocp->next) {
		indexp = blocp->ents;
		for (u = BSIZE; u && total; --u, --total) {
			if (strcmp(indexp->name, cp) == 0)
				goto done;	/* found matching name */
			++indexp;
		}
	}

	done:

	if (total) {
	/* found matching name; go to its file position */
		indexp->db_flag = 1;
		return;
	}

	printf("DB ABORTED : NO SUCH SATELLITE\n");
	LONGJMP(reset, 1);

}

void dbsn()
/* Takes the next argument on the command line as the norad satellite number
to set the db flag on for */
{
	struct bloc *blocp;	/* block pointer */
	struct indent *indexp;	/* index pointer */
	unsigned u, total;
	unsigned int in_norad_nbr;

	in_norad_nbr = atoi(*tokp);  /* Norad nbr of satellite */

	/* Sequential search for a matching nbr.  Outer loop checks all
	blocks, inner loop checks all entries in a block. */
	total = sets;		/* no. of sats in index */
	for (blocp = first; blocp != NULL; blocp = blocp->next) {
		indexp = blocp->ents;
		for (u = BSIZE; u && total; --u, --total) {
			if (in_norad_nbr == indexp->norad_nbr)
				goto done;	/* found matching nbr */
			++indexp;
		}
	}

	done:

	if (total) {
	/* found matching nbr; set its flag */
		indexp->db_flag = 1;
		return;
	}

	printf("DBS# ABORTED : NO SUCH SATELLITE\n");
	LONGJMP(reset, 1);

}

void reset_db_ptr()
{
	curr_db_block = first;
	curr_db_index = curr_db_block->ents;
	--curr_db_index;
	curr_db_count = 0;
	curr_db_total = 0;
}

int next_db_sat()
/* Gets next satellite with db_flag set to 1 */
{

again:

	++curr_db_total;
	if  (curr_db_total > sets) {
	    return 1;
	}

	++curr_db_count;
	if (curr_db_count > BSIZE) {
	   if  (curr_db_block->next == NULL) {
	       return 1;
	   }
	   curr_db_block = curr_db_block->next;
	   curr_db_index = curr_db_block->ents;
	   curr_db_count = 1;
	}
	else {
	   ++curr_db_index;
	}

	if  (curr_db_index->db_flag == 1) {
	    goto done;
	}

	goto again;

  done:

	/* found a db flag set to 1 ; go to its file position */
	fseek(fp, curr_db_index->offset, 0);

	/* get elements, report status */
	if  (getel() == 0)
	    return 0;
	else
	    printf("RUNDBS ABORTED : INCORRECT TLE FORMAT\n");
	LONGJMP(reset, 1);
	return 1;
}

void reset_db_flags()
/* Resets all db_flags to zero */
{
	struct bloc *blocp;     /* block pointer */
	struct indent *indexp;  /* index pointer */
	unsigned u;

	/* Sequential search all entries.  Outer loop checks all
	blocks, inner loop checks all entries in a block. */

	for (blocp = first; blocp != NULL; blocp = blocp->next) {
		indexp = blocp->ents;
		for (u = BSIZE; u ; --u) {
		    indexp->db_flag = 0;
		    ++indexp;
		}
	}
}
