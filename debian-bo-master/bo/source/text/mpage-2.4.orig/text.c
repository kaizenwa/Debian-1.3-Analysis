/*
 * text.c
 */

/*
 * mpage:    a program to reduce pages of print so that several pages
 *           of output appear on one printed sheet.
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
#include <sys/types.h> /* Ultrix... Michael Fulbright msf@as.arizona.edu */
#include <time.h>
#include <sys/stat.h>

/*
 * keeps track of the current location on the sheet.  it is kept global
 * to while file printing process because of form feeds in particular.
 * form feeds change the vertical page location (line number) but not
 * the horizontal location (character column)
 */
struct pageloc {
    int pl_line;
    int pl_col;
    int pl_new_line;
    int pl_new_col;
};

static struct pageloc loc;
static char text[LINESIZE];

static char *file_name;
static int file_pagenum;
static char file_date[LINESIZE];

/*
 * Function Declarations
 */
static int do_text_sheet();
static int text_onepage();
static char *mp_get_text();


/*
 * do_text_doc processes an input stream fd, reducing output to fit on
 * a printed page as decribed by asheet, and prints this on outfd.
 */
void
do_text_doc(fd, asheet, outfd, fname)
 FILE *fd;
 struct sheet *asheet;
 FILE *outfd;
 char * fname;
{
    struct stat file_stat;

    /*
     * initalize the postion on the first printed page
     */
    loc.pl_line = 1;
    loc.pl_col = opt_indent;
    file_name = fname;
    file_pagenum = 0;
    fstat(fileno(fd), &file_stat);
    strftime(file_date, LINESIZE, dateformat, localtime(&file_stat.st_ctime));

    /*
     * while we have input, print a page
     */
    do_sheets(do_text_sheet, fd, asheet, outfd );

    return;

} /* do_text_doc */


    
/*
 * do_text_sheet creates one printed sheet consisting of several reduced pages
 */
static int
do_text_sheet(fd, asheet, outfd)
 FILE *fd;
 struct sheet *asheet;
 FILE *outfd;
{
    int rtn_val = FILE_MORE;
    int peekc;

    if ((peekc = getc(fd)) == EOF)
        return FILE_EOF;
    ungetc(peekc, fd);

    if (((points->pp_origin_x == 0) && !points->skip) || opt_file) {
        /*
         * keep track of the pages printed
         */
        ps_pagenum++;
        fprintf(outfd, "%%%%Page: %d %d\n", ps_pagenum, ps_pagenum);
# ifdef DEBUG
        if (Debug_flag & DB_PSMPAGE)
        fprintf(outfd, "(Page: %d\\n) print flush\n", ps_pagenum);
# endif /* DEBUG */
        fprintf(outfd, "save\n"); /* for better memory usage */

        /*
         * Now is the time to print a sheet header...
         * for now, this has to be done before outline...
         */
        sheetheader(outfd, file_name);

        /*
         * print the page outline, which draws lines and such
         */
        mp_outline(outfd, asheet);

        /*
         * run through the list of base points for putting reduced pages
         * on the printed page
         */
        points = asheet->sh_pagepoints;
    }
    /* while (points->pp_origin_x != 0 && rtn_val == FILE_MORE) {} */
    while ((points->pp_origin_x != 0 || points->skip) && rtn_val == FILE_MORE) {
        /*
         * print one reduced page by moveing to the proper point,
         * turning to the proper aspect, scaling to the proper
         * size, and setting up a clip path to prevent overwritting;
         * the print a reduced page of output
         */
        int pheight;

        file_pagenum++;

        if (points->skip) {
            rtn_val = text_onepage(fd, asheet, outfd);
            points++;
            continue;
        }

        fprintf(outfd, "gsave\n");
# ifdef DEBUG
        if (Debug_flag & DB_PSMPAGE) {
            fprintf(outfd, "(    %d %d translate %d rotate\\n)",
                           points->pp_origin_x(), points->pp_origin_y(),
                           asheet->sh_rotate);
            fprintf(outfd, " print flush\n");
        }
# endif /* DEBUG */
        /*
         * Take position on paper
         */
        fprintf(outfd, "%d %d translate\n",
               points->pp_origin_x(), points->pp_origin_y());
        if (asheet->sh_rotate)
            fprintf(outfd, "%d rotate\n", asheet->sh_rotate);
        /*
         * Clip to logical page
         */
        fprintf(outfd,
           "0 0 moveto 0 %d rlineto %d 0 rlineto 0 %d rlineto closepath clip\n",
           (*asheet->sh_height)(), (*asheet->sh_width)(),
           -(*asheet->sh_height)());
        pheight = asheet->sh_plength * fsize +
                  (opt_mp_header ? HSIZE + 2 : 0);
        /*
         * Scale to logical page
         */
        fprintf(outfd, "%d %d mp_a_x mul div %d %d div scale\n",
               (*asheet->sh_width)(), asheet->sh_cwidth, 
               (*asheet->sh_height)(), pheight);
        /*
         * Draw header bar and print header when needed 
         */
        if (opt_mp_header) {
            int pos = (asheet->sh_plength) * fsize;
            fprintf(outfd, "0 %d moveto %d mp_a_x mul 0 rlineto stroke\n",
                           pos, asheet->sh_cwidth);
            pos += 4;
            fprintf(outfd, "headerfont setfont\n");
            if (opt_header != NULL)
                fprintf(outfd, "3 %d moveto (%s) show\n", pos, opt_header);
            else {
                fprintf(outfd, "3 %d moveto (%s) show\n", pos, file_date);
                fprintf(outfd, "%d mp_a_x mul dup (Page %d) stringwidth pop "
                               "sub 3 sub %d moveto",
                               asheet->sh_cwidth, file_pagenum, pos);
                fprintf(outfd, " (Page %d) show\n", file_pagenum);
                fprintf(outfd, "fnamefont setfont\n");
                fprintf(outfd, "(%s) stringwidth pop sub 2 div %d moveto\n",
                               file_name, pos);
                fprintf(outfd, "(%s) show\n", file_name);
            }
        }

        /*
         * Take pagemargin and font descenders (fsize/4) into account
         * and scale again
         */
        fprintf(outfd, "%d %d translate %d %d div %d %d div scale\n",
               pagemargin_left, pagemargin_bottom + fsize/4,
               (*asheet->sh_width)() - pagemargin_left - pagemargin_right,
               (*asheet->sh_width)(),
               asheet->sh_plength * fsize - pagemargin_top - pagemargin_bottom,
               asheet->sh_plength * fsize);

        fprintf(outfd, "textfont setfont\n");

        /*
         * place one reduced page on the printed page
         */
        rtn_val = text_onepage(fd, asheet, outfd);
        /*
         * clean up this page and move to the next
         */
        fprintf(outfd, "grestore\n");
        points++;
    }
    /*
     * release PS vm used, and eject the sheet
     */
    if (points->pp_origin_x == 0 ||
            (rtn_val == FILE_EOF && opt_file)) {
        fprintf(outfd, "restore\n");
        if (had_ps)
            fprintf(outfd, "showsheet\n");
        else
            fprintf(outfd, "showpage\n");
    }
    /*
     * let the upper level know about the status of possible EOF
     */
    return rtn_val;

} /* do_text_sheet */



/*
 * text_onepage places on page of reduced output on the printed page
 * all scaling, translation, and rotation has already been done before
 */
static int
text_onepage(file, asheet, outfd)
 FILE *file;
 struct sheet *asheet;
 FILE *outfd;
{
    char *text;

    /*
     * Start off with printing any wanted annotation
     */
    if (opt_textbox) {
        fprintf(outfd, "%d setlinewidth\n", textbox.thick);
        fprintf(outfd, "%d mp_a_x mul %d moveto 0 %d rlineto\n",
                       textbox.over, textbox.lift*fsize, textbox.high*fsize);
        fprintf(outfd, "%d mp_a_x mul 0 rlineto 0 %d rlineto closepath stroke\n",
                       textbox.wide, -textbox.high*fsize);
    }

    /*
     * as we move from one page to the next, restart printing text at
     * the head of the page. horziontal location is not reset because
     * form feeds leave the column the same from page to page.
     */
    Debug(DB_ONEPAGE, "%% reseting line to top of page\n", 0);
    loc.pl_line = 1;
    /*
     * keep getting lines of input, until we have filled a page
     */
    while (loc.pl_line <= asheet->sh_plength) {
        text = mp_get_text(file, &loc, asheet);
        Debug(DB_ONEPAGE, "%% text = %d\n", text);
        if (text == 0) {
            return FILE_EOF;
        }
        Debug(DB_ONEPAGE, "%% text = (%s)\n", text);
        Debug(DB_ONEPAGE, "%% loc.pl_line = %d\n", loc.pl_line);
        Debug(DB_ONEPAGE, "%% loc.pl_col = %d\n", loc.pl_col);
        Debug(DB_ONEPAGE, "%% loc.pl_new_line = %d\n",loc.pl_new_line);
        Debug(DB_ONEPAGE, "%% loc.pl_new_col = %d\n", loc.pl_new_col);
        if (text[0] != 0 && !points->skip) {
            switch (loc.pl_col) {
            /* fprintf(outfd, "(%s\\n) print flush\n", text); */
                case 0: putc('0', outfd);
                        break;
                case 1: fprintf(outfd, "mp_a_x");
                        break;
                default: fprintf(outfd, "%d mp_a_x mul", loc.pl_col);
                         break;
            }
            fprintf(outfd, " %d moveto (%s) show\n",
                (asheet->sh_plength - loc.pl_line) * fsize,
                text);
        }
        if (loc.pl_new_line == -1) {
            loc.pl_col = loc.pl_new_col;
            return FILE_MORE;
        }
        loc.pl_line = loc.pl_new_line;
        loc.pl_col = loc.pl_new_col;
    }

    return FILE_MORE;

} /* text_onepage */



static char *
mp_get_text(infile, locp, asheet)
 FILE *infile;
 struct pageloc *locp;
 struct sheet *asheet;
{
    int gathering;
    int tabcnt;
    int ichr;
    static int prevchar=0;
    char *textp;

    textp = text;
    locp->pl_new_line = locp->pl_line;
    locp->pl_new_col = locp->pl_col;

    gathering = 1;
    while (gathering) {
        if (prevchar) {
            ichr = prevchar;
            prevchar = 0;
        }
        else
            ichr = fgetc(infile);
        Debug(DB_GETLINE, "%%called fgetc ichr = %d", ichr);
        Debug(DB_GETLINE, "(%d)\n", EOF);
        /*
         * this prevents nulls in the input from confusing the
         * program logic with truncated strings
         */
        if (ichr == 0) {
            ichr = 1;
        }
        switch (ichr) {
        case EOF:
            gathering = 0;
            return 0;
            /* break; */
        case '\n':
            locp->pl_new_line += 1;
            locp->pl_new_col = opt_indent;
            gathering = 0;
            break;
        case '\r':
            locp->pl_new_col = opt_indent;
            gathering = 0;
            break;
        case '\b':
            locp->pl_new_col -= 1;
            if (locp->pl_new_col < opt_indent) {
                locp->pl_new_col = opt_indent;
            }
            gathering = 0;
            break;
        case '\f':
            locp->pl_new_line = -1;
            gathering = 0;
            break;
        case '\t':
            tabcnt = opt_tabstop -
                     ((locp->pl_new_col - opt_indent) % opt_tabstop);
            locp->pl_new_col += tabcnt;
            gathering = 0;
            break;
/*
 *        case ' ':
 *            locp->pl_new_col += 1;
 *            gathering = 0;
 *            break;
 */
        default: /* keep on gathering if it fits on the line ... */
            if (opt_fold &&
                (locp->pl_new_col >= asheet->sh_cwidth)) {
                prevchar = ichr;
                gathering = 0;
                locp->pl_new_line += 1;
                locp->pl_new_col = opt_indent;
                break;
            }
            if (ichr == ')' || ichr == '(' || ichr == '\\') {
                *textp++ = '\\';
                *textp++ = ichr;
            }
            /* else if (ichr >= ' ' && ichr <= '~') */
            else if (ichr >= first_encoding && ichr <= last_encoding)
                *textp++ = ichr;
            else {
                *textp++ = '\\';
                *textp++ = '2';
                *textp++ = '7';
                *textp++ = '7';
            }
            locp->pl_new_col += 1;
            break;
        }
    }
    *textp = 0;
    /*
     * remove any spaces at the front of the text string by
     * "converting" it to a position change
     */
    textp = text;
    while (*textp && *textp == ' ') {
        /*
         * this affects the starting position of this text string
         * (not the next)
         */
        locp->pl_col += 1;
        textp++;
    }

    return textp;

} /* mp_get_text */

