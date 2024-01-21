/*
 * lifeconv.c -- image format conversion routines for Xlife
 *
 * By Eric S. Raymond (eric@snark.uu.net)
 *
 * A leading -[RAP] argument forces the conversion format. Otherwise
 * it uses the following rules:
 *
 * 1) If the image won't fit in a 1000x876 screen, leave it alone (thus,
 *    very large patterns like the breeder are left in #A format).
 *
 * 2) Otherwise, convert it to whichever of `P' or `R' format takes up less
 *    disk space. Offsets are generated in the header so that the hot spot is
 *    at the center of the bounding rectangle.
 *
 * Option `p' converts from old P format to new P format.  In old P format,
 * the y offset at which a line was to be loaded was its absolute line number.
 * In the new format, it's the line number relative to the #P control line (if
 * the #P is the first line in the save file the two coincide).
 *
 * All #-constructs other than R, A, and I are passed through.
 *
 * With no file arguments, lifeconv acts as a filter.  If given file arguments,
 * the files will be converted to the new format in place.
 */
#include <stdio.h>
#include <string.h>

typedef int	bool;
#define TRUE	1
#define FALSE	0

#define MARK	'*'
#define SPACE	'.'

/* XMAX should be divisible by 8 (bits in a char) 
 * XMAX x YMAX should be big enough to handle anything reasonable
 */
#define XMAX	2000
#define YMAX	2000

static const char convcomm[]="## converted to new format by oldpconv";

/* image data */
static char	image[XMAX >> 3][YMAX];
static int	xmin, ymin, xmax, ymax, cellcount;
static int	xoff, yoff;

static void mark(x, y)
/* mark a cell and update the bounding-box data */
int	x, y;
{

    image[x >> 3][y] |= 1 << (0x07 - (x & 0x07));
    cellcount++;

    if (x < xmin)
	xmin = x;
    if (y < ymin)
	ymin = y;
    if (x > xmax)
	xmax = x;
    if (y > ymax)
	ymax = y;
}

static int cvimage(name, ifp, mode, ofp)
/* read in an image in A, R or P form */
char	*name;
FILE	*ifp;
char	mode;
FILE	*ofp;
{
    char	buf[BUFSIZ];
    int		x, y, i, j, xcorner, ycorner;
    char	inmode = 'A';

    if (mode == 'p')
    {
	bool converted = FALSE;
	int linenum;

	fprintf(ofp,"%s\n",convcomm);
	linenum=0;
	while (fgets(buf, sizeof(buf), ifp))
	{
	    if (buf[0]=='#')
	    {
		switch(buf[1])
		{
		case 'P':
		    if (converted)
			fprintf(ofp,"%s", buf);
		    else
		    {
			x=y=0; 
			sscanf(buf+2,"%d %d",&x,&y);
			fprintf(ofp,"#P %d %d\n",x,y+linenum); 
		    }
		    break;
		case '#':
		    if (!strncmp(buf,convcomm,strlen(convcomm)))
		    {
			fprintf(stderr,
				"Looks like %s was already converted\n", name);
			converted = TRUE;
		    } 
		    /* break omitted intentionally */
		default: 
		    fprintf(ofp,"%s",buf); 
		}
	    }
	    else
		fprintf(ofp,"%s",buf);

	    linenum++;
	}

	return;
    }


    /* read in a complete image file in any mode */
    memset(image, '\0', sizeof(image));
    cellcount = 0;
    xmin = XMAX; ymin = YMAX; xmax = 0; ymax = 0;
    while (fgets(buf, BUFSIZ, ifp) != (char *)NULL)
    {
	extern char	*strchr();

	if (buf[0] == '#')
	{
	    xoff = yoff = 0;

	    if (strchr("RAI", buf[1]) != (char *)NULL)
	    {
		inmode = buf[1];
		(void) sscanf(buf + 2, "%d %d", &xoff, &yoff);
	    }
	    else
		(void) fputs(buf, ofp);
	}
	else if ((inmode=='A'||inmode=='R') && sscanf(buf, "%d %d", &x,&y)==2)
	{
	    if (x >= XMAX || y >= YMAX)
	    {
		/*
		 * If you get this message, recompile with larger
		 * XMAX and YMAX values
		 */
		(void) fprintf(stderr,
			       "%s: point %d = (%d,%d) out of bounds\n",
			       name, cellcount, x, y);
		return(-1);
	    }
	    else if (inmode == 'R')
		mark(XMAX/2 + xoff + x, YMAX/2 + yoff + y);
	    else
		mark(xoff + x, yoff + y);
	}
	else /* inmode == 'P' */
	{
	    char	*cp;

	    for (cp = buf; *cp; cp++)
		if (*cp == MARK)
		    mark(xoff + cp - buf, yoff + y);
	    y++;
	}
    }

    /* if the mode is \0, we go for the shortest output form */
    if (mode == '\0')
	if (((ymax - ymin + 1) * (xmax - xmin + 1)) > cellcount * 8)
	    mode = 'R';
	else
	    mode = 'P';

    /* undo the hack to accept negative indices */
    if (inmode == 'R' && mode != 'A')
    {
	xcorner = XMAX/2; ycorner = YMAX/2;
	xmin -= XMAX/2; ymin -= YMAX/2;
	xmax -= XMAX/2; ymax -= YMAX/2;
    }
    else
	xcorner = ycorner = 0;

    /* here goes the write side */
    if (mode == 'P')
    {
	(void) fprintf(ofp, "#P %d %d\n", -(xmax-xmin)/2,  -(ymax-ymin)/2);
	for (i = ymin; i <= ymax; i++)
	{
	    for (j = xmin; j <= xmax; j++)
		if (image[(j + xcorner) >> 3][i + ycorner] & (1 << (0x07 - ((j+xcorner) & 0x07))))
		    (void) fputc(MARK, ofp);
		else
		    (void) fputc(SPACE, ofp);
	    (void) fputc('\n', ofp);
	}
    }
    else if (mode == 'R')
    {
	int cx = xmin + (xmax-xmin)/2;
	int cy = ymin + (ymax-ymin)/2;

	(void) fprintf(ofp, "#R\n");
	for (i = ymin; i <= ymax; i++)
	    for (j = xmin; j <= xmax; j++)
		if (image[(j + xcorner) >> 3][i + ycorner] & (1 << (0x07 - ((j+xcorner & 0x07)))))
		    (void) fprintf(ofp, "%3d %3d\n", j - cx, i - cy);
    }
    else /* if (mode == 'A') */
    {
	(void) fprintf(ofp, "#A\n");
	for (i = ymin; i <= ymax; i++)
	    for (j = xmin; j <= xmax; j++)
		if (image[(j + xcorner) >> 3][i + ycorner] & (1 << (0x07 - ((j+xcorner & 0x07)))))
		    (void) fprintf(ofp, "%3d %3d\n", xoff + j, yoff + i);
    }

    return(mode);
}

main(argc, argv)
int argc;
char **argv;
{
    char	convmode = '\0';

    for (argv++, argc--; argc; argv++, argc--)
	if (argv[0][0] == '-'
	    && (argv[0][1] == 'R' || argv[0][1] == 'A'
		|| argv[0][1] == 'P' || argv[0][1] == 'p'))
	{
	    convmode = argv[0][1];
	    if (argc == 1)
		(void) cvimage("stdin", stdin, convmode, stdout);
	}
	else if (strcmp(argv[0], "-") == 0)
	    (void) cvimage("stdin", stdin, convmode, stdout);
	else
	{
	    char	tmpfile[BUFSIZ];
	    FILE	*ifp, *ofp;

	    (void) strcpy(tmpfile, ".lfcnvXXXXXX");
	    (void) mktemp(tmpfile);

	    if ((ifp = fopen(argv[0], "r")) == (FILE *)NULL)
		perror(argv[0]);
	    else if ((ofp = fopen(tmpfile, "w")) == (FILE *)NULL)
		perror(tmpfile);
	    else
	    {
		char	oldname[BUFSIZ];
		int	status = cvimage(argv[0], ifp, convmode, ofp);

		(void) fclose(ifp);
		(void) fclose(ofp);
		if (status < 0)
		    continue;

		(void) fprintf(stderr, "%s: %c\n", argv[0], status);

		if (unlink(argv[0]) != 0)
		    perror(argv[0]);
		else if (link(tmpfile, argv[0]) != 0)
		    perror(argv[0]);
		else if (unlink(tmpfile) != 0)
		    perror(argv[0]);
	    }
        }
}

