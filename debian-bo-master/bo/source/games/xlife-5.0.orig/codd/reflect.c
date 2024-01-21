#include <stdio.h>

#define MAXCOLS	80
#define MAXROWS 132

static depth, width;
static char inpic[MAXROWS][MAXCOLS];

#define getcell(x, y)		inpic[x][y]
#define setcell(x, y, v)	inpic[x][y] = (v)

static reflect(tlx, tly, brx, bry, xaxis)
int tlx, tly, brx, bry;
int	xaxis;
{
    int sx = (brx - tlx);
    int sy = (bry - tly);
    int cx = sx/2;
    int cy = sy/2;
    int x, y;
    int	swap;

    if (xaxis)
    {
	for (x = tlx; x <= brx; x++)
	    for (y = tly; y <= cy; y++)
	    {
		int ynew = tly + sy - (y - tly);

		swap = getcell(y, x);
		setcell(y, x, getcell(ynew, x));
		setcell(ynew, x, swap);
	    }
    }
    else
    {
	for (y = tly; y <= bry; y++)
	    for (x = tlx; x <= cx; x++)
	    {
		int xnew = tlx + sx - (x - tlx);

		swap = getcell(y, x);
		setcell(y, x, getcell(y, xnew));
		setcell(y, xnew, swap);
	    }
    }
}


main()
{
    int	depth = 0, width, i;

    while (fgets(inpic[depth], MAXCOLS, stdin) != (char *)NULL)
	if (inpic[depth][0] != '#')
	    depth++;
    width = strlen(inpic[0]) - 1;

    reflect(0, 0, width - 1, depth - 1, 0);

    for (i = 0; i < depth; i++)
	(void) fputs(inpic[i], stdout);
}
