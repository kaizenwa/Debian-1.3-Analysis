Return-Path: majordom@sunlabs
Return-Path: <majordom@sunlabs>
Received: from sunlabs.eng.sun.com by sage.Eng.Sun.COM (5.x/SMI-SVR4)
	id AA05805; Tue, 4 Jun 1996 00:23:36 -0700
Received: filtered by sunlabs.eng.sun.com (SunLabs-1.2)
	id AA13411; Tue, 4 Jun 1996 00:09:07 -0700
Received: from Eng.Sun.COM (engmail1.Eng.Sun.COM) by sunlabs.eng.sun.com (SunLabs-1.2)
	id AA13390; Tue, 4 Jun 1996 00:08:37 -0700
Received: from mercury.Sun.COM (mercury.EBay.Sun.COM) by Eng.Sun.COM (5.x/SMI-5.3)
	id AA07494; Tue, 4 Jun 1996 00:08:53 -0700
Received: by mercury.Sun.COM (Sun.COM)
	id AAA04655; Tue, 4 Jun 1996 00:08:16 -0700
Received: by firewall.ddeorg.soft.net (5.61/9.3) 
	id AA16443; Tue, 4 Jun 96 12:43:07 +0530
Received: from orion.ddeorg.soft.net by ddeorg.soft.net (5.61/9.3) with SMTP 
	id AA18448; Tue, 4 Jun 96 12:43:06 +0530
Received: by orion.ddeorg.soft.net (4.1/9.7) 
	id AA09850; Tue, 4 Jun 96 12:38:07 IST
Message-Id: <9606040708.AA09850@orion.ddeorg.soft.net>
X-Mailer: exmh version 1.6.6 3/24/96
To: exmh-users@sunlabs.Eng.Sun.COM
Cc: jerry@ora.com
Subject: Mime header-body/transfer-decoding filter available
Date: Tue, 04 Jun 1996 12:38:06 +0530
From: "Frederik H. Andersen" <fha@ddeorg.soft.net>
Sender: owner-exmh-users@sunlabs.Eng.Sun.COM
Precedence: bulk
Content-Type: text
Content-Length: 21529

This is a multipart MIME message.

--===_0_Tue_Jun__4_12:31:40_IST_1996
Content-Type: text/plain
--------
Hi!

I have now (what I believe) is a working version of mimedecode. 

It's a filter to be used as:
mimedecode [-h | -d <debug level>] < encoded_msg  > decoded_msg

I have put it in my .forward file as:
"|/usr/local/bin/mimedecode|/usr/local/lib/mh/slocal -user fha"

It will undo base64 and QP encodings of text-type body parts and do
QP decoding of iso-8859-1 encoded header fields.

This make life a lot easier for 8-bit charset users like most
europeans :-).

I have included the c-code directly. I don't hope its too bulky :-(

Any comments, bug fixes, improvements, etc. are of course most
welcome.

/Frederik


--===_0_Tue_Jun__4_12:31:40_IST_1996
Content-Type: text/plain
Content-Description: mimedecode.c

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
 *      - encoded header fields are decoded assuming QP encoding
 *        and charset iso-8859-1
 *      - part or subparts of content-type text only are decoded
 *      - all other content-types are passed transparently
 *     
 */

#define USAGE "mimedecode [-h | -d <debug level>] < encoded_msg > decoded_msg"

#include <stdio.h>
#include <string.h>

/* Some defines. Should have gone into a file by itself.
 */
#define MIMED_VERSION "mimedecode version 1.1"

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

extern char *decode_header_line();
extern char *optarg;
extern int decode_quoted_printable();
extern int decode_base64();


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
     } else
	if (debug) fprintf(stderr,"parse of message complete\n");
     
}



/*******************************************************************/
static int parse_message(boundary)
/*******************************************************************/
char	*boundary;
{
    int c;
    char linebuf[256];
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

		fprintf(stdout,"%s",linebuf);

		if ((linebuf[0] != '-') || (linebuf[1] != '-'))
		{
		    lbp = linebuf;
		    continue;
		}

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
	parse_message(0);
    }
    else
    {
	if (parse_body(mhead.content_type, mhead.transfer_encoding, boundary)) parse_message(boundary);
    }
    return(0);
}



/*******************************************************************/
static int parse_body(type, encoding, boundary)
/*******************************************************************/
int  type, encoding;
char *boundary;
{
    int c;
    char linebuf[256];
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
		ret = decode_base64(stdin, stdout, boundary);

	    if (ret) return(1);

            while ( (c=getc(stdin)) != EOF ) {
                *lbp++ = c;
                if ( c == '\n') 	/* end of line reached */
                {
	            *lbp ='\0';
		    fprintf(stdout,"%s",linebuf);

		    if ((linebuf[0] != '-') || (linebuf[1] != '-'))
		    {
		        lbp = linebuf;
		        continue;
		    }
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
		    lbp = linebuf;
		    continue;
	        }
            }
        }
	else /* no boundary */
	{
	    if (encoding == QP)
	        decode_quoted_printable(stdin, stdout, 0);
	    else if (encoding == B64)
		decode_base64(stdin, stdout, 0);
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
		fprintf(stdout,"%s",linebuf);

		if ((linebuf[0] != '-') || (linebuf[1] != '-'))
		{
		    lbp = linebuf;
		    continue;
		}
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
		lbp = linebuf;
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
static parse_header(mhp)
/*******************************************************************/
struct mime_header *mhp;
{
	char linebuf[256];
	char *lbp = linebuf;
	char *nlbp;
	int c;
	int header = TRUE;
	char *content;
	char *cp;


        if (debug) fprintf(stderr," -- Entry parse_header -- \n");

	mhp->content_type = UNDEF;
	mhp->transfer_encoding = UNDEF;

	while ( (c=getc(stdin)) != EOF ) {
again:		*lbp++ = c;
		if ( c == '\n') 	/* end of line reached */
		{
		    *lbp = '\0'; /* zero terminate the line buf */

		    if (!casncmp(linebuf,"content-type:",13))
		    {
			/* we are only doing decoding of text types */

			if ( strstr(linebuf, "text/"))
			    mhp->content_type = TEXT;

			else if (  strstr(linebuf, "multipart/"))
			{
			    mhp->content_type = MULT;

			    /* search for the boundary parameter */
			    if (cp = strchr(linebuf, ';'))
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
		    }
		    else if ((mhp->content_type == TEXT) 
			     && !casncmp(linebuf,"content-transfer-encoding:", 26))
		    {
		        /* Replace encoding to 8bit, but only for text 
			 * content types. Remember the encoding in mhp!
			 */

			if (cp = strchr(linebuf, ':'))
                        {
			    /* skip : and remove white space */
			    cp++;
                            while (*cp == ' ' || *cp == '\t') cp++;

                            if (!casncmp(cp,"quoted-printable",16))
                            {
                                if (debug >= 3)
                                    fprintf(stderr,"Transfer-encoding IS QP\n");
                                mhp->transfer_encoding = QP;
				fprintf(stdout,"X-Content-transfer-encoding: fixed from quoted-printable\n");
                            }
			    else if (!casncmp(cp,"base64",6))
                            {
                                if (debug >= 3)
                                    fprintf(stderr,"Transfer-encoding IS B64\n");
                                mhp->transfer_encoding = B64;
				fprintf(stdout,"X-Content-transfer-encoding: fixed from base64\n");
                            }
                            else {
                                if (debug >= 3)
                                    fprintf(stderr,"Transfer-encoding IS UNKNOWN\n");
				mhp->transfer_encoding = UNDEF;
                            }
                        }

		        nlbp = "Content-Transfer-Encoding: 8bit\n";
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

		    fprintf(stdout, "%s", nlbp);

		    lbp = linebuf;

		    /* Have we reached the message body? */
		    if ((c = getc(stdin)) == '\n')
		        return(0);	/* we have! */
		    else
			goto again;
		}
	}
}



/*
 * General format of an encoded header field:
 *
 * header_field: <text>=?<charset>?<encoding>?<encoded_chars>?=<text>
 *
 * Disclaimer: We only handle charset of iso-8859-1 and only QP
 *             encoding!
 *
 */

#define HUNT	0
#define START	1
#define CHARS	2
#define ENCD	3
#define FIELD	4
#define DEC1	5
#define DEC2	6
#define END	7


#define binhex(c) ( (c > '9') ? c-'A'+10 : c-'0')

char retbuf[256];

/*******************************************************************/
static char *decode_header_line(buf)
/*******************************************************************/
char *buf;
{
    int state = HUNT;
    int c, c1, c2, b1, b2, tmp;
    int header = 1;
    char charset[80];
    char *charp = charset;
    char encoding[80];
    char *encp = encoding;
    char *retp = retbuf;


    while ( (c = *buf++) != '\0' ) {
	if ( header_logging == TRUE) print_state(state);

	switch (c)	{
		case '=':
		{
			switch (state)	{
			case HUNT:
				if ((tmp = *buf++) == '?') {
					state = CHARS;
				} else {
					*retp++ = c;
					*retp++ = tmp;
				}
				continue;

			case FIELD:
				state = DEC1;
				continue;

			case DEC1:
				*retp++ = '=';
				continue;

			case END:
				if (header_logging)	{
					*charp = '\0';
					*encp  = '\0';
					fprintf(stderr,"charset: %s\n", charset);
					fprintf(stderr,"encoding: %s\n",encoding);
				}
				state = HUNT;
				continue;
			}
		}
		break;

		case '?':
		{
			switch(state)	{
			case CHARS:
				state = ENCD;
				continue;

			case ENCD:
				state = FIELD;
				continue;

			case FIELD:
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
			if (state == CHARS) {
				/* Here we ought to check the charset 
				 * We assume iso-8859-1
				 */
				*charp++ = c;
				continue;
			}

			if (state == ENCD) {
				/* here we ought to check the encoding 
				 * We assume QP
				 */
				*encp++ = c;
				continue;
			}
			
			if ((state == FIELD) && (c == '_')) {
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
					state = FIELD;
				}
				break;
			}
			if (state == DEC2) {
				if (isxdigit(c)) {
					c2 = toupper(c);
					b2 = binhex(c2);
					c = b1 << 4 | b2;
					*retp++ = c;
					state = FIELD;
					continue;
				} else {
					*retp++ = c1;
					state = FIELD;
				}
				break;
			}
			if (state == END) {
				/* output the consumed char */
				*retp++ = '?';
				state = FIELD;
			}
		}
		break;
	}
	*retp++ = c;
    }
    *retp++ = c;	/* remember the '\0' terminator */
    return(retbuf);
}



/*******************************************************************/
static int	print_state(state)
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

	case	FIELD:
		fprintf(stderr,"state FIELD\n");
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
static int casncmp(s1, s2, n)
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
static int decode_quoted_printable(infp, outfp, boundary)
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

	    if ((linebuf[0] == '-') && (linebuf[1] == '-'))
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



#define UN 255

static unsigned char b64_map[256] =
{
    UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, /* 0x00 - 0x0f, NUL - SI */
    UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,UN, /* 0x10 - 0x1f, DLE - US */
    UN,UN,UN,UN, UN,UN,UN,UN, UN,UN,UN,62, UN,UN,UN,63, /* 0x20 - 0x2f, SP  - / */
    52,53,54,55, 56,57,58,59, 60,61,UN,UN, UN,UN,UN,UN, /* 0x30 - 0x3f, 0   - ? */
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



/*******************************************************************/
static int nextgetc(infp)
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
		/* while ((c = getc(infp)) != '\n');*/
	    }
	}

        if (b64_map[c] != UN)
            break;
    }
    return c;
}



/*******************************************************************/
static int decode_base64(infp, outfp, boundary)
/*******************************************************************/
FILE *infp;
FILE *outfp;
char *boundary;
{
    int c1, c2, c3, c4;
    int ret;

    c1 = c2 = c3 = c4 = 0;

    if (debug) fprintf(stderr," --- Entry decode_base64 --- \n");
    if (boundary && (debug >=2)) fprintf(stderr," --- with boundary: %s\n",boundary);

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


--===_0_Tue_Jun__4_12:31:40_IST_1996--


