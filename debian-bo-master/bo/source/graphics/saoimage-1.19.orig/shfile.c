#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>
#include <X/Xlib.h>

#include "hfiles/image.h"
#include "hfiles/extern.h"

static int x0,y0,x1,y1;

/* Add this pixel to the current pixel list */

addpixel (x, y)
     int x, y;
{
    int ipos, strlen();
    char line[11];
    FILE *stream;
    char *pixlist;

    len = strlen (image->filename);
    if ((reglist = malloc(len+7)) == NULL) {
	fprintf(stderr,"Can't allocate %d byte reglist\n",len+7);
	exit(1);
	}
    strncpy (pixlist,image->filename,len);
    pixlist[len] = 0;
    strcat (imageinfo->pixlist,".plist");

    /* Open file to append */
    if ((stream = fopen (pixlist,"a")) == NULL ) {
	fprintf (stderr,"Can't open file for pixel list %s\n",pixlist);
	return;
	}

    /* Write this point to end of file */
    fprintf (stream,"%5d %5d\n",x,y);
    fclose (stream);

    fprintf (stderr,"Col %d, Row %d  added to pixel list\n", x,y);
    if (pixlist != NULL)
	free (pixlist);

    return;
}


/* Save this pixel position as first region corner */

addreg1 (x, y)
     int x, y;
{
    x0 = x;
    y0 = y;
    fprintf (stderr,"Col %d, Row %d lower left  of region\n", x,y);

    return;
}


/* Save this pixel position as second region corner */

addreg2 (x, y)
     int x, y;
{
    x1 = x;
    y1 = y;
    fprintf (stderr,"Col %d, Row %d upper right of region\n", x,y);

    return;
}

/* write the position and intenity out into the zoom text subwindow */
/* Add current region to the current region list file */

addregion ()
{
    FILE *stream;
    int x,y;
    char *reglist;

    len = strlen (image->filename);
    if ((reglist = malloc(len+7)) == NULL) {
	fprintf(stderr,"Can't allocate %d byte reglist\n",len+7);
	exit(1);
	}
    strncpy (reglist,image->filename,len);
    reglist[len] = 0;
    strcat (imageinfo->reglist,".rlist");

    /* Open file to append */
    if ((stream = fopen (imageinfo->reglist,"a")) == NULL ) {
	fprintf (stderr,"Can't open file for region list %s\n",imageinfo->reglist);
	return;
	}

    if (x1 < x0) {
	x = x1;
	x1 = x0;
	x0 = x; }
    if (y1 < y0) {
	y = y1;
	y1 = y0;
	y0 = y; }

    /* Write this region to end of file */
    fprintf (stream,"%5d %5d %5d %5d\n",x0, x1, y0, y1);
    fclose (stream);

    fprintf (stderr,"(%d %d) (%d %d) region marked\n",x0, y0, x1, y1);

    /* Reset corners to 0 */
    x0 = 0;
    x1 = 0;
    y0 = 0;
    y1 = 0;
    if (reglist != NULL)
	free (reglist);

    return;
}
