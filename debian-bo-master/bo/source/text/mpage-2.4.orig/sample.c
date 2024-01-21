/*
 * sample.c
 */

/*
 * Copyright (c) 1994-1996 Marcel J.E. Mol, Rijswijk, The Netherlands
 *                    marcel@mesa.nl
 */


# include "mpage.h"


/*
 * Function Declarations
 */
static void do_sample();
static void urshow();
static void lrshow();
static void ulshow();
static void llshow();
static void box();

int
main(argc, argv)
 int argc;
 char **argv;
{
	int currarg;

	/*
	 * examine the environment for PRINTER and MPAGE environment variables
	 */
	if ((currarg = do_env()) == 0) {
		usage(currarg);
		exit(1);
	}
		
	if ((currarg = do_args(argc, argv, 0)) < 0) {
		usage(currarg);
		exit(1);
	}

	do_sample();

        exit(0);
}



static void
do_sample()
{
	printf("%%!PS-mpage-layout\n");
	printf("/Courier findfont 8 scalefont setfont\n");
	printf("0 setlinewidth\n");

	outline_8(stdout);

	urshow(xbase1(), ybase1());
	urshow(xbase1(), ybase2());
	urshow(xbase1(), ybase3());
	urshow(xbase1(), ybase4());
	lrshow(xbase1(), ytop1());
	lrshow(xbase1(), ytop2());
	lrshow(xbase1(), ytop3());
	lrshow(xbase1(), ytop4());
	ulshow(xbase1()+xwid2(), ybase1());
	ulshow(xbase1()+xwid2(), ybase2());
	ulshow(xbase1()+xwid2(), ybase3());
	ulshow(xbase1()+xwid2(), ybase4());
	llshow(xbase1()+xwid2(), ytop1());
	llshow(xbase1()+xwid2(), ytop2());
	llshow(xbase1()+xwid2(), ytop3());
	llshow(xbase1()+xwid2(), ytop4());

	urshow(xbase2(), ybase1());
	urshow(xbase2(), ybase2());
	urshow(xbase2(), ybase3());
	urshow(xbase2(), ybase4());
	lrshow(xbase2(), ytop1());
	lrshow(xbase2(), ytop2());
	lrshow(xbase2(), ytop3());
	lrshow(xbase2(), ytop4());
	ulshow(xbase2()+xwid2(), ybase1());
	ulshow(xbase2()+xwid2(), ybase2());
	ulshow(xbase2()+xwid2(), ybase3());
	ulshow(xbase2()+xwid2(), ybase4());
	llshow(xbase2()+xwid2(), ytop1());
	llshow(xbase2()+xwid2(), ytop2());
	llshow(xbase2()+xwid2(), ytop3());
	llshow(xbase2()+xwid2(), ytop4());
	printf("showpage\n");

        return;
}



static void
urshow(int x, int y)
{

	printf("%%- point %d,%d\n", x, y);
	box(x, y);
	printf("\t%d %d moveto (%d,%d) show\n", x+2, y+2, x, y);
}



static void
lrshow(int x, int y)
{
	printf("%%- point %d,%d\n", x, y);
	box(x, y);
	printf("\t%d %d moveto (%d,%d) show\n", x+2, y-6, x, y);
}



static void
ulshow(int x, int y)
{
	printf("%%- point %d,%d\n", x, y);
	box(x, y);
	printf("\t%d %d moveto\n", x-2, y+2);
	printf("\t(%d,%d) dup stringwidth pop -1 mul 0 rmoveto show\n", x, y);
}



static void
llshow(int x, int y)
{
	printf("%%- point %d,%d\n", x, y);
	box(x, y);
	printf("\t%d %d moveto\n", x-2, y-6);
	printf("\t(%d,%d) dup stringwidth pop -1 mul 0 rmoveto show\n", x, y);
}


static void
box(int x, int y)
{
	printf("\t%d %d moveto %d %d lineto\n", x-1, y, x+1, y);
	printf("\t%d %d moveto %d %d lineto\n", x, y-1, x, y+1);
	printf("\tstroke\n");
}

