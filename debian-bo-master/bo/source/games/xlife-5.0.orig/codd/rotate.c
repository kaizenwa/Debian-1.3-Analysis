#include <stdio.h>

#define MAXCOLS	80
#define MAXROWS 132

static depth, width;
static char inpic[MAXROWS][MAXCOLS];

#define getcell(x, y)		inpic[x][y]
#define setcell(x, y, v)	inpic[x][y] = (v)

static rotate(tlx, tly, brx, bry, xaxis)
int tlx, tly, brx, bry;
int	xaxis;
{
    int ox = (brx - tlx) % 2;
    int oy = (bry - tly) % 2;
    int bx = (brx - tlx + 1)/2;
    int by = (bry - tly + 1)/2;
    int x, y;

/*
    printf("Size = (%d, %d), Center = (%d, %d)\n",
	   (brx - tlx + 1), (bry - tly + 1), bx, by);
*/

    for (x = tlx; x <= bx - !ox; x++)
	for (y = tly; y <= by; y++)
	{
	    int swap = getcell(y, x);
	    int rot1x, rot1y, rot2x, rot2y, rot3x, rot3y;

	    rot1x = bx - (y - by); rot1y = by + (x - bx);
	    rot2x = bx - (x - bx); rot2y = by - (y - by);
	    rot3x = bx + (y - by); rot3y = by - (x - bx);

/*
	    printf("(%d, %d) -> (%d, %d) -> (%d, %d) -> (%d, %d)\n",
		   x, y, rot1x, rot1y, rot2x, rot2y, rot3x, rot3y);
*/

	    setcell(y, x, getcell(rot3y, rot3x));
	    setcell(rot3y, rot3x, getcell(rot2y, rot2x));
	    setcell(rot2y, rot2x, getcell(rot1y, rot1x));
	    setcell(rot1y, rot1x, swap);
	}
}


main()
{
    int	depth = 0, width, i;

    while (fgets(inpic[depth], MAXCOLS, stdin) != (char *)NULL)
	if (inpic[depth][0] != '#')
	    depth++;
    width = strlen(inpic[0]) - 1;

    rotate(0, 0, width - 1, depth - 1, 0);

    for (i = 0; i < depth; i++)
	(void) fputs(inpic[i], stdout);
}
