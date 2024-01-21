/*
 * page.c
 */

/*
 * mpage:	a program to reduce pages of print so that several pages
 * 	  	of output appear on one printed page.
 *
 * Written by:
 *   ...!uunet!\                       Mark Hahn, Sr Systems Engineer
 *              >pyrdc!mark            Pyramid Technology Corporation
 * ...!pyramid!/                       Vienna, Va    (703)848-2050
 *
 *
 * Copyright (c) 1988 Mark P. Hahn, Herndon, Virginia
 * Copyright (c) 1994-1996 Marcel J.E. Mol, Rijswijk, The Netherlands
 *                    marcel@mesa.nl
 *  
 *     Permission is granted to anyone to make or distribute verbatim
 *     copies of this document as received, in any medium, provided
 *     that this copyright notice is preserved, and that the
 *     distributor grants the recipient permission for further
 *     redistribution as permitted by this notice.
 *
 */


#include "mpage.h"



void
set_page()
{
    switch (opt_page) {
        case PAGE_A4: /* ISO 216 conforming says 595x841 ... */
                media = "A4";
		ps_width = 596;        /* 210 mm */
		ps_height = 842;       /* 297 mm */
                break;
        case PAGE_LETTER:
                media = "Letter";
		ps_width = 612;        /* 8.5 inches */
		ps_height = 792;       /* 11 inches */
                break;
        case PAGE_LEGAL:
                media = "Legal";
		ps_width = 612;        /* 8.5 inches */
		ps_height = 1008;      /* 14 inches */
                break;
        default: 
                fprintf(stderr, "%s: ignoring unknown page format: %d\n",
                                MPAGE, opt_page);
                break;
    }

    return;

} /* set_page */



/*
 * Bases are the base for each page. e.g the corner points for outlines.
 */
   /*  __________________________
    * | ________________________ |
    * ||                        ||
    * ||                        ||
    * ||                 ybase4 ||
    * ||                        Y|
    * ||                        ||
    * ||                        ||
    * ||                 ybase3 ||
    * ||                        Y|
    * ||                        ||
    * ||                        ||
    * ||                 ybase2 ||
    * ||                        Y|
    * ||                        ||
    * ||                        ||
    * ||xbase1   xbase2  ybase1 ||
    * |X___________X____________Y|
    * |__________________________|
    */

int
xbase1()
{
	Debug(DB_POINTS, "%%xbase1: %d\n", sheetmargin_left + sheetheader_left);
	return sheetmargin_left + sheetheader_left;
} /* xbase1 */



int
xbase2()
{
	Debug(DB_POINTS, "%%xbase2: %d\n",
                (ps_width - sheetmargin_left - sheetmargin_right
                          - sheetheader_left - sheetheader_right) / 2 +
                sheetmargin_left + sheetheader_left);
        return  (ps_width - sheetmargin_left - sheetmargin_right
                          - sheetheader_left - sheetheader_right) / 2 +
                sheetmargin_left + sheetheader_left;
} /* xbase2 */



int
ybase1()
{
	Debug(DB_POINTS, "%%ybase1: %d\n",
               sheetmargin_bottom + sheetheader_bottom);
	return sheetmargin_bottom + sheetheader_bottom;
} /* ybase1 */



int
ybase2()
{
	Debug(DB_POINTS, "%%ybase2: %d\n",
	        (ps_height - sheetmargin_bottom - sheetmargin_top
                           - sheetheader_bottom - sheetheader_top) / 4
	        + sheetmargin_bottom + sheetheader_bottom);
	return (ps_height - sheetmargin_bottom - sheetmargin_top
                          - sheetheader_bottom - sheetheader_top) / 4
	        + sheetmargin_bottom + sheetheader_bottom;
} /* ybase2 */



int
ybase3()
{
	Debug(DB_POINTS, "%%ybase3: %d\n",
	        (ps_height - sheetmargin_bottom - sheetmargin_top
                           - sheetheader_bottom - sheetheader_top) / 2
	        + sheetmargin_bottom + sheetheader_bottom);
	return (ps_height - sheetmargin_bottom - sheetmargin_top
                          - sheetheader_bottom - sheetheader_top) / 2
	        + sheetmargin_bottom + sheetheader_bottom;
} /* ybase3 */



int
ybase4()
{
	Debug(DB_POINTS, "%%ybase4: %d\n",
	        (ps_height - sheetmargin_bottom - sheetmargin_top
                           - sheetheader_bottom - sheetheader_top) * 3 / 4
	        + sheetmargin_bottom + sheetheader_bottom);
	return  (ps_height - sheetmargin_bottom - sheetmargin_top
                           - sheetheader_bottom - sheetheader_top) * 3 / 4
	        + sheetmargin_bottom + sheetheader_bottom;
} /* ybase4 */



int
ytop1()
{
	Debug(DB_POINTS, "%%ytop1: %d\n", ybase2());
	/* return ybase1() + yht4(); */
	return ybase2();
} /* ytop1 */



int
ytop2()
{
	Debug(DB_POINTS, "%%ytop2: %d\n", ybase3());
	/* return ybase2() + yht4(); */
	return ybase3();
} /* ytop2 */



int
ytop3()
{
	Debug(DB_POINTS, "%%ytop3: %d\n", ybase4());
	/* return ybase3() + yht4(); */
	return ybase4();
} /* ytop3 */



int
ytop4()
{
	Debug(DB_POINTS, "%%ytop4: %d\n", 
	       ps_height - sheetmargin_bottom - sheetmargin_top
	                 - sheetheader_bottom - sheetheader_top
	                 + sheetmargin_bottom + sheetheader_bottom);
	/* return ybase4() + yht4(); */
	return ps_height - sheetmargin_bottom - sheetmargin_top
	                 - sheetheader_bottom - sheetheader_top
	                 + sheetmargin_bottom + sheetheader_bottom;
} /* ytop4 */



int
xwid1()
{
	Debug(DB_POINTS, "%%xwid1: %d\n",
	       ps_width - sheetmargin_left - sheetmargin_right
	                - sheetheader_left - sheetheader_right);
	return ps_width - sheetmargin_left - sheetmargin_right
	                - sheetheader_left - sheetheader_right;
} /* xwid1 */



int
xwid2()
{
	Debug(DB_POINTS, "%%xwid2: %d\n",
	       (ps_width - sheetmargin_left - sheetmargin_right
	                 - sheetheader_left - sheetheader_right) / 2);
	return (ps_width - sheetmargin_left - sheetmargin_right
	                 - sheetheader_left - sheetheader_right) / 2;
} /* xwid2 */



int
yht1()
{
	Debug(DB_POINTS, "%%yht1: %d\n",
	       ps_height - sheetmargin_top - sheetmargin_bottom
	                 - sheetheader_top - sheetheader_bottom);
	return ps_height - sheetmargin_top - sheetmargin_bottom
	                 - sheetheader_top - sheetheader_bottom;
} /* yht1 */



int
yht2()
{
	Debug(DB_POINTS, "%%yht2: %d\n",
	       (ps_height - sheetmargin_top - sheetmargin_bottom
	                  - sheetheader_top - sheetheader_bottom) / 2);
	return (ps_height - sheetmargin_top - sheetmargin_bottom
	                  - sheetheader_top - sheetheader_bottom) / 2;
} /* yht2 */



int
yht4()
{
	Debug(DB_POINTS, "%%yht4: %d\n",
	       (ps_height - sheetmargin_top - sheetmargin_bottom
	                  - sheetheader_top - sheetheader_bottom) / 4);
	return (ps_height - sheetmargin_top - sheetmargin_bottom
	                  - sheetheader_top - sheetheader_bottom) / 4;
} /* yht4 */



void
outline_1(outfd)
 FILE *outfd;
{
	/* one page outline */
	fprintf(outfd, "0 setlinewidth\n");
        fprintf(outfd, "%d %d moveto 0 %d rlineto\n",
                       xbase1(), ybase1(), yht1());
	fprintf(outfd, "%d 0 rlineto 0 %d rlineto closepath stroke\n",
                       xwid1(), -yht1());

        return;
} /* outline_1 */



void
outline_2(outfd)
 FILE *outfd;
{
	/* two page outline */
	outline_1(outfd);
	fprintf(outfd, "%d %d moveto %d 0 rlineto stroke\n",
                xbase1(), ybase3(), xwid1());

        return;
} /* outline_2 */



void
outline_4(outfd)
 FILE *outfd;
{
	/* four page outline */
	outline_2(outfd);
	fprintf(outfd, "%d %d moveto 0 %d rlineto stroke\n",
                xbase2(), ybase1(), yht1());

        return;
} /* outline_4 */



void
outline_8(outfd)
 FILE *outfd;
{
	/* eight page outline */
	outline_4(outfd);
	fprintf(outfd, "%d %d moveto %d 0 rlineto stroke\n",
                xbase1(), ybase2(), xwid1());
	fprintf(outfd, "%d %d moveto %d 0 rlineto stroke\n",
                xbase1(), ybase4(), xwid1());

        return;
} /* outline_8 */



void
mp_outline(outfd, asheet)
 FILE *outfd;
 struct sheet *asheet;
{

    if (opt_outline) {
        (*asheet->sh_outline)(outfd);
    }

    return;

} /* mp_outline */



void
sheetheader(outfd, fname)
 FILE *outfd;
 char *fname;
{

    if (opt_sheetheader) {
        fprintf(outfd, "gsave\nheaderfont setfont\n");
        switch (orientation) {
            case PORTRAIT:
                sheetheader_top = 14;
                if (*sheethead != '\0') {
                    fprintf(outfd,
                           "%d %d (%s) stringwidth pop sub 2 div add %d moveto",
                            xbase1(), xwid1(), sheethead,
                            ytop4() + fsize / 4);
                    fprintf(outfd, " (%s) show\n", sheethead);
                }
                else {
                    /*
                     * Filename on the left
                     */
                    fprintf(outfd, "%d %d moveto (%s) show\n",
                            xbase1() + 3, ytop4() + fsize / 4, fname);
                    /*
                     * Pagenumber on the right
                     */
                    fprintf(outfd,
                            "%d (Page %d) stringwidth pop sub 3 sub %d moveto",
                            xbase1() + xwid1(), ps_pagenum,
                            ytop4() + fsize / 4);
                    fprintf(outfd, " (Page %d) show\n", ps_pagenum);
                }
                break;
            case LANDSCAPE:
            case LANDSCAPE_PORTRAIT:
                sheetheader_right = 14;
                if (*sheethead != '\0') {
                    fprintf(outfd,
                           "%d %d (%s) stringwidth pop 2 div add moveto",
                            xbase1() + xwid1() + fsize/4,
                            ybase3(), sheethead);
                    fprintf(outfd, " -90 rotate (%s) show\n", sheethead);
                }
                else {
                    /*
                     * Filename on the left
                     */
                    fprintf(outfd, "%d %d moveto -90 rotate (%s) show\n",
                            xbase1() + xwid1() + fsize/4, ytop4() - 3, fname);
                    /*
                     * Pagenumber on the right
                     */
                    fprintf(outfd, "90 rotate %d %d (Page %d) stringwidth pop ",
                                   xbase1() + xwid1() + fsize/4,
                                   ybase1() + 3, ps_pagenum);
                    fprintf(outfd, "add moveto -90 rotate (Page %d) show\n",
                                   ps_pagenum);
                }
                break;
        }
        fprintf(outfd, "grestore\n");
    }

    return;

} /* sheetheader */

