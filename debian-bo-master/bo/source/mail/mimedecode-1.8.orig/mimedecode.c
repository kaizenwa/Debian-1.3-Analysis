/*
 * This program comes with absolutely no guarantees and may
 * be used for whatever purpose,
 *
 * 	Frederik H. Andersen - 1996
 *	Dansk Data Elektronik A/S
 *
 *	fha@dde.dk
 *
 */



/*
 * This program performs the decoding of transfer encoded text type
 * mime messages. The message in its entirety is read from stdin.
 * The decoded message is written to stdout; hence, this program
 * behaves as a filter which may be placed wherever convenient.
 * I particular like it in front of my slocal command in my .forward
 * file.
 *
 * It is assumed that the message has reached its point of final
 * delivery and at that point 8-bit text types can be handled
 * natively. Hence, the need for transfer-encodings is not present
 * any more.
 *
 * Only some cases are handled:
 *      - encoded header fields are decoded from QP or B encoding.
 *        The charset is assumed to be iso-8859-1
 * 	- part or subparts of content-type text only are decoded
 *      - all other content-types are passed transparently
 *     
 */

#define USAGE "mimedecode [-h | -d <debug level>] < encoded_msg > decoded_msg"

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>

/* Some defines. Should have gone into a file by itself.
 */
#define MIMED_VERSION "mimedecode version 1.8"

#define FALSE 0
#define TRUE  1

#define lower(c)        ( isupper(c) ? tolower(c) : (c) )

struct mime_header {
	int content_type;
	int transfer_encoding;
	char boundary[80];
};

/* content types: */
#define UNDEF	0
#define TEXT	1
#define MULT    2
#define MESG    3

/* transfer encodings: */
#define QP	1
#define B64	2
#define BIT7	3
#define BIT8	4

#define UN 255

unsigned char b64_map[256] =
{
    UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, /* 0x00 - 0x0f, NUL - SI */
    UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, /* 0x10 - 0x1f, DLE - US */
    UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,62, UN,UN,UN,63, /* 0x20 - 0x2f, SP  - / */
    52,53,54,55, 56,57,58,59, 60,61,UN,UN, UN,0x3d,UN,UN, /* 0x30 - 0x3f, 0   - ? */
    UN, 0, 1, 2,  3, 4, 5, 6,  7, 8, 9,10, 11,12,13,14, /* 0x40 - 0x4f, @   - O */
    15,16,17,18, 19,20,21,22, 23,24,25,UN, UN,UN,UN,UN, /* 0x50 - 0x5f, P   - _ */
    UN,26,27,28, 29,30,31,32, 33,34,35,36, 37,38,39,40, /* 0x60 - 0x6f, `   - o */
    41,42,43,44, 45,46,47,48, 49,50,51,UN, UN,UN,UN,UN, /* 0x70 - 0x7f, p   - DEL */

    UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, /* 0x80 - 0x8f */
    UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, /* 0x90 - 0x9f */
    UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, /* 0xA0 - 0xAf */
    UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, /* 0xB0 - 0xBf */
    UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, /* 0xC0 - 0xCf */
    UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, /* 0xD0 - 0xDf */
    UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, /* 0xE0 - 0xEf */
    UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, /* 0xF0 - 0xFf */
};

extern void exit(int);
extern char *optarg;

extern int parse_message(char *); 
extern int parse_body(int, int, char *);
extern int parse_header(struct mime_header *);
extern char *decode_header_line(char *);
extern void print_state(int);
extern int casncmp(const char *, const char *, int);
extern int valid_charset(char *, int);
extern int decode_quoted_printable(FILE *, FILE *, char *);
extern int nextgetc(FILE *);
extern int decode_base64(FILE *, FILE *);
extern void write_cte(int, int);


int     debug = 0;
int     header_logging  = FALSE;


/*******************************************************************/
int     main(int argc, char *argv[])
/*******************************************************************/
{
     int   c;
     int   ret;

     while((c=getopt(argc, argv, "hd:")) >= 0) {
	switch (c) {
	case    'd':    /* Switch on debugging on stderr */
	    fprintf(stderr, "*** %s ***\n", MIMED_VERSION);
	    debug = atoi(optarg);
	    fprintf(stderr, "Processing with debug: %d\n", debug);
	    fprintf(stderr, "(This might produce voluminous output on stderr.)\n\n");
	    break;

	case	'h':	/* Switch on header logging */
	    header_logging = TRUE;
	    break;

	default:
	    fprintf(stderr,"%s \n", USAGE);
	    exit(1);
	}
     }


     if ((ret = parse_message(0)) != 0) {
	if (debug) fprintf(stderr,"parse of message failed: %d\n", ret);
	exit(1);
     } else
	if (debug) fprintf(stderr,"parse of message complete\n");

     exit(0);
}



/*******************************************************************/
int parse_message(boundary)
/*******************************************************************/
char	*boundary;
{
    int c;
    char linebuf[800];
    char *lbp = linebuf;
    struct mime_header mhead;
    int ret;

    if (debug) fprintf(stderr," - Entry parse_message - \n");

    /* parse header */
    if ((ret = parse_header(&mhead)) != 0) {
	if (debug)
	    fprintf(stderr,"parse of message header failed: %d\n", ret);

	return(1);
    }

    /* output the header-body separator: */
    putc('\n', stdout);

    if (mhead.content_type == MULT)
    {
	if (debug >=2 )
	{
	    fprintf(stderr,"message is multipart\n");
	    fprintf(stderr,"search header boundary line: %s\n", mhead.boundary);
	}

	/* search for the boundary line before parsing further */

	while ( (c=getc(stdin)) != EOF )
	{
            *lbp++ = c;

	    if ( c == '\n') 	/* end of line reached */
	    {
	        *lbp = '\0'; /* zero terminate the line buf */
		lbp = linebuf;

		fprintf(stdout,"%s",linebuf);

		if ((linebuf[0] != '-') || (linebuf[1] != '-'))
		    continue;

		if (!strncmp(linebuf+2,mhead.boundary,strlen(linebuf)-3))
		{
		    if (debug >=2)
		        fprintf(stderr,"header boundary line found\n");

		    break;
		}
	    }
	}

	/* When the message is of type multipart we have to handle each
	 * part as an individual message.
	 * We use the boundary to identify the beginning of each part
	 */
	parse_message(mhead.boundary);
    }
    else if(mhead.content_type == MESG)
    {
	/* When message is type message we have to handle the body as
	 * an individual message.
	 */
	parse_message((char *)0);
    }
    else
    {
	if (parse_body(mhead.content_type, mhead.transfer_encoding, boundary)) parse_message(boundary);
    }
    return(0);
}



/*******************************************************************/
int parse_body(type, encoding, boundary)
/*******************************************************************/
int  type, encoding;
char *boundary;
{
    int c;
    char linebuf[800];
    char *lbp = linebuf;
    int ret;

    if (debug) fprintf(stderr," -- Entry parse_body -- \n");
    if (debug >=2) fprintf(stderr," -- type: %d, enc: %d\n",type, encoding);

    if ((type == TEXT) && ((encoding == QP) || (encoding == B64)))
    {
        if (boundary)
        {
	    if (debug >= 2) fprintf(stderr," -- with boundary: %s\n",boundary);
	    if (encoding == QP)
	        ret = decode_quoted_printable(stdin, stdout, boundary);
	    else if (encoding == B64)
		ret = decode_base64(stdin, stdout);

	    if (ret) return(1);

            while ( (c=getc(stdin)) != EOF ) {
                *lbp++ = c;

                if ( c == '\n') 	/* end of line reached */
                {
	            *lbp ='\0';
		    lbp = linebuf;
		    fprintf(stdout,"%s",linebuf);

		    if ((linebuf[0] != '-') || (linebuf[1] != '-'))
		        continue;
		    else if (!strncmp(linebuf+2,boundary,strlen(boundary)-1))
		    {

		        if ((linebuf[2+strlen(boundary)] != '-') ||
			    (linebuf[3+strlen(boundary)] != '-'))
		        {
		            return(1);
		        }
		        else
		        {
		            if (debug >=2 )
		                fprintf(stderr," -- end boundary line found\n");
			    return(2);	/* end boundary found */
		        }
		    }
	        }
            }
        }
	else /* no boundary */
	{
	    if (encoding == QP)
	        decode_quoted_printable(stdin, stdout, 0);
	    else if (encoding == B64)
		decode_base64(stdin, stdout);
            return(0);
	}
    }
    else if (boundary)
    {
	if (debug >=2 ) fprintf(stderr," -- with boundary: %s\n",boundary);
        while ( (c=getc(stdin)) != EOF ) {
            *lbp++ = c;

            if ( c == '\n') 	/* end of line reached */
            {
	        *lbp ='\0';
		lbp = linebuf;
		fprintf(stdout,"%s",linebuf);

		if ((linebuf[0] != '-') || (linebuf[1] != '-'))
		    continue;
		else if (!strncmp(linebuf+2,boundary,strlen(boundary)-1))
		{

		    if ((linebuf[2+strlen(boundary)] != '-') ||
			(linebuf[3+strlen(boundary)] != '-'))
		    {
		        return(1);
		    }
		    else
		    {
		        if (debug >=2 )
		            fprintf(stderr," -- end boundary line found\n");
			return(2);	/* end boundary found */
		    }
		}
		continue;
	    }
        }
    }
    else 
    {
        while ( (c = getc(stdin)) != EOF )
	    putc(c, stdout);
    }
    return(0);
}



/*******************************************************************/
int parse_header(mhp)
/*******************************************************************/
struct mime_header *mhp;
{
	char linebuf[800];
	char *lbp = linebuf;
	char *nlbp;
	int c;
	char *cp;
	int ct_read;		/* Content-type field read */
	int cte_read;		/* Content-transfer-encoding read */
	int cte_written;	/* Content-transfer-encoding written */


        if (debug) fprintf(stderr," -- Entry parse_header -- \n");

	mhp->content_type = UNDEF;
	mhp->transfer_encoding = UNDEF;
	cte_read = FALSE;
	cte_written = FALSE;
	ct_read = FALSE;

	while ( (c=getc(stdin)) != EOF ) {
again:		*lbp++ = c;
		if ( c == '\n') 	/* end of line reached */
		{
		    *lbp = '\0'; /* zero terminate the line buf */
		    lbp = linebuf;

		    if (debug >= 4 )
			fprintf(stderr,"linebuf: %s\n", linebuf);

		    if (!casncmp(linebuf,"content-type:",13))
		    {
			ct_read = TRUE;

			/* we are only doing decoding of text types */
			if (strstr(linebuf, "text/") || strstr(linebuf, "Text/"))
			{
			    if (debug >=3 )
				fprintf(stderr,"Content IS text\n");

			    mhp->content_type = TEXT;
			}
			else if (  strstr(linebuf, "multipart/"))
			{
			    mhp->content_type = MULT;

			    /* search for the boundary parameter */
			    if ((cp = strchr(linebuf, ';')))
                            {
			        /* skip ; and remove white space */
			        cp++;
                                while (*cp == ' ' || *cp == '\t') cp++;
				if (!casncmp(cp,"boundary=",9))
				{
				    char *cp2 = mhp->boundary;

				    cp = strchr(cp, '=');
				    cp +=2;	/* skip =" */
				    while('"' != (*cp2++ = *cp++));
				    *--cp2 = '\0';
				}
			    }
			}
			else if (  strstr(linebuf, "message/"))
			{
			    mhp->content_type = MESG;
			}
		        nlbp = linebuf;
		    } else if (!casncmp(linebuf,"content-transfer-encoding:", 26))
		    {
			cte_read = TRUE;

			/* Remember the encoding in mhp! */
			if ((cp = strchr(linebuf, ':')))
                        {
			    /* skip : and remove white space */
			    cp++;
                            while (*cp == ' ' || *cp == '\t') cp++;

                            if (!casncmp(cp,"quoted-printable",16))
                            {
                                if (debug >= 3)
                                    fprintf(stderr,"Transfer-encoding IS QP\n");
                                mhp->transfer_encoding = QP;
                            }
			    else if (!casncmp(cp,"base64",6))
                            {
                                if (debug >= 3)
                                    fprintf(stderr,"Transfer-encoding IS B64\n");
                                mhp->transfer_encoding = B64;
                            }
			    else if (!casncmp(cp,"7bit",4))
                            {
                                if (debug >= 3)
                                    fprintf(stderr,"Transfer-encoding IS 7bit\n");
                                mhp->transfer_encoding = BIT7;
                            }
			    else if (!casncmp(cp,"8bit",4))
                            {
                                if (debug >= 3)
                                    fprintf(stderr,"Transfer-encoding IS 8bit\n");
                                mhp->transfer_encoding = BIT8;
                            }
                            else {
                                if (debug >= 3)
                                    fprintf(stderr,"Transfer-encoding IS UNKNOWN\n");
				mhp->transfer_encoding = UNDEF;
                            }
                        }
			nlbp = NULL;
	            }
		    else if ((mhp->content_type == MULT) 
			     && strstr(linebuf, "boundary="))
		    {
			char *cp2 = mhp->boundary;;

			cp = strchr(linebuf, '=');
			cp +=2;	/* skip =" */
			/* get what's between ".." */
			while('"' != (*cp2++ = *cp++));
			*--cp2 = '\0'; /* and null terminate */

		        nlbp = linebuf;
		    }
		    else
		        nlbp = decode_header_line(linebuf);

		    if (debug >=4)
			fprintf(stderr,"decoded line: %s\n", nlbp);

/* Generally, we will output _all_ lines as they are read (and
 * decoded).  The exception is the Content-transfer-encoding (cte)
 * header line:
 * We will only output cte when the Content-type has been read or
 * when the header/body boundary have been reached.
 */
		    if (nlbp) fprintf(stdout, "%s", nlbp);

		    /* Have we reached the header/body boundary? */
		    if ((c = getc(stdin)) == '\n')
		    {
			/* yes we have! */

			if (debug >= 2)
			    fprintf(stderr,"body reached\n");

			if ( cte_read && !cte_written)
			{
			    if ( !ct_read )
			    {
			        /* Content-type field is missing!! 
			         * Assume text/plain!
			         */
			        if ( debug >=3 )
			           fprintf(stderr,"Content-type field is missing!!\n");
                                fprintf(stdout,"X-Content-type: missing\n");

			        ct_read = TRUE;
			        mhp->content_type = TEXT;
			    }

			    write_cte(mhp->transfer_encoding, mhp->content_type);
                            cte_written = TRUE;
			}
			/* should we output the '\n' here? */
		        return(0);
		    }

		    /* 
		     * We have _not_ reached the header/body boundary
		     */
		    if ( ct_read && cte_read & !cte_written )
		    {
			write_cte(mhp->transfer_encoding, mhp->content_type);
                        cte_written = TRUE;
		    }

		    goto again;
	        }
	}
	return(1);
}



/*
 * General format of an encoded header field:
 *
 * header_field: <text>=?<charset>?<encoding>?<encoded_chars>?=<text>
 *
 * Disclaimer: We only handle charset of iso-8859-1
 *
 */

#define HUNT	0
#define START	1
#define CHARS	2
#define ENCD	3
#define Q_FIELD	4
#define DEC1	5
#define DEC2	6
#define B_FIELD	7
#define END	8


#define binhex(c) ( (c > '9') ? c-'A'+10 : c-'0')

char retbuf[800];

/*******************************************************************/
char *decode_header_line(buf)
/*******************************************************************/
char *buf;
{
    int state = HUNT;
    int c, c1, c2, b1, b2, tmp;
    char charset[80];
    char *charp = charset;
    char encoding[80];
    char *encp = encoding;
    char *retp = retbuf;


    while ( (c = *buf++) != '\0' ) {
	if ( header_logging == TRUE) print_state(state);

	if (state == HUNT)
	{
		if (c == ' ' || c == '\t' || c == '(') {
			state = START;
			*retp++ = c;
			continue;
		}
	}

	switch (c)	{
		case '=':
		{
			switch (state)	{
			case START:
				if ((tmp = *buf++) == '?') {
					state = CHARS;
				} else {
					*retp++ = c;
					*retp++ = tmp;
					state = HUNT;
				}
				continue;

			case CHARS:
				*charp = '\0';
				charp = charset;
				*retp++ = '=';
				*retp++ = '?';
				while(*charp) *retp++ = *charp++;
				*retp++ = '=';

				charp = charset;
				state = HUNT;
				continue;

			case Q_FIELD:
				state = DEC1;
				continue;

			case DEC1:
				*retp++ = '=';
				continue;
			
			case B_FIELD:
				/* padding */
				continue;

			case END:
				state = HUNT;
				continue;
			}
		}
		break;

		case '?':
		{
			switch(state)	{
			case START:
				*retp++ = c;
				state = HUNT;
				continue;

			case CHARS:
				/* we ignore the charset specification, assuming
				 * iso-8859-1
				 */
				*charp = '\0';

				if (!valid_charset(charset,charp-charset)) {
					charp = charset;
					*retp++ = '=';
					*retp++ = '?';
					while(*charp) *retp++ = *charp++;
					*retp++ = '?';

					charp = charset;
					state = HUNT;
					continue;
				}

				if (header_logging)
					fprintf(stderr,"charset: %s\n", charset);

				state = ENCD;
				continue;

			case ENCD:
				*encp  = '\0';
				if (header_logging)
					fprintf(stderr,"encoding: %s\n",encoding);

				if ((*encoding == 'q') || (*encoding == 'Q'))
				{
					state = Q_FIELD;
				}
				else if ((*encoding == 'b') || (*encoding == 'B'))
				{
				    int c1, c2, c3, c4;

				    state = B_FIELD;

				    while ((*buf != '\0') && (*buf != '\?'))
				    {
					/* find first valid c1 */
					while ((*buf != '\0') && ((c1 = b64_map[(unsigned int)*buf]) == UN))
					    buf++;
					c1 = *buf;

					if (*buf != '\0') buf++;

					/* find second valid c2 */
					while ((*buf != '\0') && ((c2 = b64_map[(unsigned int)*buf]) == UN)) 
					    buf++;
					c2 = *buf;

					if (*buf != '\0') buf++;

					/* find third valid c3 */
					while ((*buf != '\0') && ((c3 = b64_map[(unsigned int)*buf]) == UN)) 
					    buf++;
					c3 = *buf;

					if (*buf != '\0') buf++;

					/* find fourth valid c4 */
					while ((*buf != '\0') && ((c4 = b64_map[(unsigned int)*buf]) == UN)) 
					    buf++;
					c4 = *buf;

					if (*buf != '\0') buf++;

					if ((c1 == '=') || (c2 == '=') || (c1 == '\0') || (c2 == '\0'))
					    {
					    break;
					    }

					*retp++ = ((unsigned int)b64_map[c1] << 2) | (((unsigned int)b64_map[c2]&0x30) >> 4);

					if ((c3 == '=') || (c3 == '\0'))
					    {
					    break;
					    }

					*retp++ = (((unsigned int)b64_map[c2]&0xF) << 4) | (((unsigned int)b64_map[c3]&0x3C) >> 2);

					if ((c4 == '=') || (c4 == '\0'))
					    {
					    break;
					    }

					*retp++ = (((unsigned int)b64_map[c3]&0x3) << 6) | ((unsigned int)b64_map[c4]);
				    }

				    *retp = '\0';
				    if (header_logging)
				    {
					fprintf(stderr,"retbuf: %s\n",retbuf);
				    }
				}
				continue;

			case B_FIELD:
			case Q_FIELD:
				state = END;
				continue;

			case DEC1:
				/* output the consumed char */
				*retp++ = '=';
				state = END;
				continue;

			case DEC2:
				/* output the consumed chars */
				*retp++ = '=';
				*retp++ = c1;
				state = END;
				continue;
			}
		}
		break;

		default:
		{
			if (state == START) {
				*retp++ = c;
				state = HUNT;
				continue;
			}

			if (state == CHARS) {
				*charp++ = c;
				continue;
			}

			if (state == ENCD) {
				*encp++ = c;
				continue;
			}
			
			if ((state == Q_FIELD) && (c == '_')) {
				*retp++ = ' ';
				continue;
			}

			if (state == DEC1) {
				if (isxdigit(c)) {
					c1 = c;
					c = toupper(c1);
					b1 = binhex(c);
					state = DEC2;
					continue;
				} else {
					*retp++ = '=';
					state = Q_FIELD;
				}
				break;
			}

			if (state == DEC2) {
				if (isxdigit(c)) {
					c2 = toupper(c);
					b2 = binhex(c2);
					c = b1 << 4 | b2;
					*retp++ = c;
					state = Q_FIELD;
					continue;
				} else {
					*retp++ = c1;
					state = Q_FIELD;
				}
				break;
			}

			if (state == END) {
				state = HUNT;
			}
		}
		break;
	}
 	*retp++ = c; 
    }
    if (state == CHARS) {

	*charp = '\0';
	charp = charset;
	*retp++ = '=';
	*retp++ = '?';
	while(*charp) *retp++ = *charp++;
    }
    *retp++ = c;	/* remember the '\0' terminator */
    return(retbuf);
}



/*******************************************************************/
void	print_state(state)
/*******************************************************************/
int	state;
{
	switch(state) {
	case	HUNT:
		fprintf(stderr,"state HUNT\n");
		break;

	case	START:
		fprintf(stderr,"state START\n");
		break;

	case	CHARS:
		fprintf(stderr,"state CHARS\n");
		break;

	case	ENCD:
		fprintf(stderr,"state ENCD\n");
		break;

	case	Q_FIELD:
		fprintf(stderr,"state Q_FIELD\n");
		break;

	case	B_FIELD:
		fprintf(stderr,"state B_FIELD\n");
		break;

	case	DEC1:
		fprintf(stderr,"state DEC1\n");
		break;

	case	DEC2:
		fprintf(stderr,"state DEC2\n");
		break;

	case	END:
		fprintf(stderr,"state END\n");
		break;

	default:
		fprintf(stderr,"unknown state; %d\n", state);
		break;
	}
}



/*******************************************************************/
int casncmp(s1, s2, n)
/*******************************************************************/
register const char *s1, *s2;
register n;
{
	if (s1 == s2)
		return(0);
	while ((--n >= 0) && (lower(*s1) == lower(*s2))) {
		s2++;
		if (*s1++ == '\0')
			return (0);
	}
	return ((n < 0) ? 0 : (lower(*s1) - lower(*s2)));
}



/*******************************************************************/
int valid_charset(s,n)
/*******************************************************************/
char *s;
int  n;
{
	if (n < 8) return(0);

	if (n > 8) n=8;

	if (!casncmp(s,"iso-8859", n)) return(1);
	if (!casncmp(s,"us-ascii", n)) return(1);

	return(0);
}



/*******************************************************************/
int decode_quoted_printable(infp, outfp, boundary)
/*******************************************************************/
FILE *infp;
FILE *outfp;
char *boundary;
{
    char linebuf[800];
    char *lbp = linebuf;
    int c;

    if (debug) fprintf(stderr," --- Entry decode_quoted_print --- \n");
    if (boundary && (debug >=2)) fprintf(stderr," --- with boundary: %s\n",boundary);

    while ((c = getc(infp)) != EOF) {
	*lbp++ = c;

	if ( c == '\n')
	{
	    *lbp = '\0'; /* zero terminate the line buf */
	    lbp = linebuf;

	    if (boundary && (linebuf[0] == '-') && (linebuf[1] == '-'))
	    {
		if (!strncmp(linebuf+2,boundary,strlen(linebuf)-3))
		{
		    if (debug >=2)
			fprintf(stderr,"header boundary line found\n");
		    fprintf(stdout,"%s",linebuf);
		    return(1);
		}
	    }

	    while( (c = *lbp++) != 0)
	    {
                if ( c == '=')	{	/* c == '=' */
                    int c1 = *lbp++;
                    int c2;

                    if (c1 == '\0')
                        break;

                    c2 = *lbp++;
                    if (c2 == '\0')
                        break;


                    /* check for soft CRLF */
                    if (c1 == '\r') {
                       /* this is an error? : c2 = getc(infp); */
                        if (c2 != '\n')         /* not a CRLF */
                            lbp--;   /* put back the char after the =<CR> */
                        continue;
                    }

                    /* check for soft newline */
                    if (c1 == '\n') {
                        lbp--;       /* put back the char after the newline */
                        continue;
                    }

                    /* check for == -> = */
                    if (c1 == '=') {
                        putc(c1, outfp);
                        lbp--;       /* put back the char after the == */
                        continue;
                    }

                    /* make sure it's =XX */
                    if (!isxdigit(c1) || !isxdigit(c2))
                        continue;

                    /* we have two hex digits, so decode them */
                    if (isdigit(c1))
                        c1 -= '0';
                    else if (islower(c1)) {
                        c1 -= 'a';
                        c1 += 10;
                    }
                    else {
                        c1 -= 'A';
                        c1 += 10;
                    }
                    if (isdigit(c2))
                        c2 -= '0';
                    else if (islower(c2)) {
                        c2 -= 'a';
                        c2 += 10;
                    }
                    else {
                        c2 -= 'A';
                        c2 += 10;
                    }
                    putc(((c1 << 4) | c2), outfp);
                }
	        else
	            putc(c, outfp);
            }	/* end while */
	    lbp = linebuf;
	}
    }
    return(0);
}





/*******************************************************************/
int nextgetc(infp)
/*******************************************************************/
FILE *infp;
{
    int c;
    while ((c = getc(infp)) != EOF)
    {
	if (c == '-')
	{
	    c = getc(infp);
	    if (c == '-')
	    {
		/* Two '-'s in a row - must be a boundary! */
		if (debug >= 2)
		    fprintf(stderr," ----- boundary line found\n");
		return(127);
	    }
	}

        if (b64_map[c] != UN)
            break;
    }
    return c;
}



/*******************************************************************/
int decode_base64(infp, outfp)
/*******************************************************************/
FILE *infp;
FILE *outfp;
{
    int c1, c2, c3, c4;
    int ret;

    c1 = c2 = c3 = c4 = 0;

    if (debug) fprintf(stderr," --- Entry decode_base64 --- \n");

    for (;;)
    {
	ret = 0;

	if (((c1 = nextgetc(infp)) == 127)
	 || ((c2 = nextgetc(infp)) == 127)
	 || ((c3 = nextgetc(infp)) == 127)
	 || ((c4 = nextgetc(infp)) == 127))
	{
	    if (debug >=2)
		fprintf(stderr," --- header boundary line found: %d\n", ret);
	    putc('-', outfp); putc('-', outfp);
	    return(1);
	}

        if ((c1 == '=') || (c2 == '=') || (c1 == EOF) || (c2 == EOF))
	{
            break;
	}

        putc((((unsigned int)b64_map[c1]<<2) | (((unsigned int)b64_map[c2]&0x30) >>4)), outfp);
        if ((c3 == '=') || (c3 == EOF))
	{
            break;
	}

        putc(((((unsigned int)b64_map[c2]&0XF) << 4) | (((unsigned int)b64_map[c3]&0x3C) >> 2)), outfp);

        if ((c4 == '=') || (c4 == EOF))
	{
            break;
	}

        putc(((((unsigned int)b64_map[c3]&0x03) <<6) | (unsigned int)b64_map[c4]), outfp);
    }
    if (debug >=2)
	fprintf(stderr," --- returned: %d\n", ret);
    return(ret);
}



/*******************************************************************/
void write_cte(transfer_encoding, content_type)
/*******************************************************************/
int transfer_encoding, content_type;
{
    if (content_type == TEXT)
    {
        switch (transfer_encoding) {
	case QP:
	    fprintf(stdout,"X-Content-transfer-encoding: fixed from quoted-printable\n");
	    fprintf(stdout,"Content-Transfer-Encoding: 8bit\n");
	    break;

	case B64:
	    fprintf(stdout,"X-Content-transfer-encoding: fixed from base64\n");
	    fprintf(stdout,"Content-Transfer-Encoding: 8bit\n");
	    break;

	case BIT7:
	    fprintf(stdout,"Content-Transfer-Encoding: 7bit\n");
	    break;

	case BIT8:
	    fprintf(stdout,"Content-Transfer-Encoding: 8bit\n");
	    break;

	default:
	    if (debug >= 3)
		fprintf(stderr,"Transfer-encoding is UNKNOWN\n");
	    break;
	}
    } else /* ! TEXT */
    {
	/* remember to output the Content-transfer-encoding
	 * header field in case type is NOT text.
	 */
	switch (transfer_encoding) {
	case QP:
	    fprintf(stdout,"Content-Transfer-Encoding: quoted-printable\n");
	    break;

	case B64:
	    fprintf(stdout,"Content-Transfer-Encoding: base64\n");
	    break;

	case BIT7:
	    fprintf(stdout,"Content-Transfer-Encoding: 7bit\n");
	    break;

	case BIT8:
	    fprintf(stdout,"Content-Transfer-Encoding: 8bit\n");
	    break;
	}
    }
}
