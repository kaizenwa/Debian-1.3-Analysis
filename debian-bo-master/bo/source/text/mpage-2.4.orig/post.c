/*
 * post.c
 */

/*
 * mpage:    a program to reduce pages of print so that several pages
 *           of output appear on one printed page.
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
#include <string.h>


/*
 * character spaces used throughout for holding the current line from
 * the input file
 */
static char currline[LINESIZE];
static char *file_name;

/*
 * for ps documents, used to remember if we have come across the
 * tailer section.  reset at the beginning of processing for each file
 */
static int ps_at_trailer;

/*
 * this is the type of postscript document we are processing
 */
static int ps_posttype;


int have_showsheet = 0;

char ps_roff_xi [16]; /* to hold the DITROFF xi line... */

static char * tex1;   /* to capture important dvi2ps lines... */
static char * tex2;

/*
 * Function declarations 
 */
static int   ps_gettype();
static void  do_post_doc();
#if 0
static void  do_other_doc();
#endif
static void  ps_copyprolog();
static void  ps_roff_copyprolog();
static void  ps_mpage_copyprolog();
static void  ps_skip_to_page();
static int   do_post_sheet();
static void  ps_sheetsetup();
static int   post_onepage();
static void  do_roff_tailer();
int   ps_check();
void  do_ps_doc();

/*
 * Peek at the first two chacters on the open file and check for the
 * two character postscript flag "%!".  If the file is not postscript
 * then the characters are pushed back into the input stream (hopefully).
 */
int
ps_check(infd)
 FILE *infd;
{
    int firstchar;
    int secondchar;
    
    Debug(DB_PSCHECK, "%%ps_check: in ps_check\n", 0);

    /*
     * eliminate blank files
     */
    if ((firstchar = fgetc(infd)) == EOF) {
        Debug(DB_PSCHECK, "%%ps_check: file is blank\n", 0);
        return 0;
    }

    /*
     * Skip any CTRL-D chars
     * Hope there are no text files starting with ctrl-d's
     */
    while (firstchar == 4)
        firstchar = fgetc(infd);

    /*
     * eliminate non-postscript files
     */
    if (firstchar != '%') {
        Debug(DB_PSCHECK, "%ps_check: 1st char is '%c' not '%'\n", firstchar);
        if (ungetc(firstchar, infd) == EOF) {
            fprintf(stderr, "%s: Lost first character of file ", MPAGE);
            fprintf(stderr, "while checking for postscript\n.");
        }
        return 0;
    }
    Debug(DB_PSCHECK, "%%ps_check: 1st char is '%c'\n", firstchar);
    /*
     * eliminate one character files (containing only a %)
     */
    if ((secondchar = fgetc(infd)) == EOF) {
        Debug(DB_PSCHECK, "%%ps_check: no second char\n", 0);
        if (ungetc(firstchar, infd) == EOF) {
            fprintf(stderr, "%s: Lost first character of file ", MPAGE);
            fprintf(stderr, "while checking for postscript\n.");
        }
        return 0;
    }
    /*
     * eliminate files that don't have the full two character
     * sequence of "%!".
     */
    if (secondchar != '!') {
        Debug(DB_PSCHECK, "%%ps_check: 2nd char is '%c' not '!'\n", secondchar);
        if (ungetc(secondchar, infd) == EOF) {
            fprintf(stderr, "%s: Lost first two characters of ", MPAGE);
            fprintf(stderr, "file while checking for postscript\n.");
            return 0;
        }
        if (ungetc(firstchar, infd) == EOF) {
            fprintf(stderr, "%s: Lost first character of file ", MPAGE);
            fprintf(stderr, "while checking for postscript.\n");
        }
        return 0;
    }
    /*
     * for post script files the first two characters (the "%!") are
     * digested by this routine.  It's just easier than dealing
     * with the problems encounted if the characters can't be ungetc'ed.
     */
    Debug(DB_PSCHECK, "%%ps_check: 2nd char is '%c'\n", secondchar);
    Debug(DB_PSCHECK, "%%ps_check: input is postscript\n", 0);

    return 1;

} /* ps_check */



static int
ps_gettype(fd, outfd)
 FILE *fd;
 FILE *outfd;
{
    int ps_type, end_comments;

    Debug(DB_PSDOC, "%%ps_gettype: in ps_gettype\n", 0);
    /*
     * error check for truncated files
     */
    if (fgets(currline, LINESIZE-1, fd) == NULL) {
        Debug(DB_PSDOC, "%%ps_gettype: got eof on first line\n", 0);
        return PS_NONE;
    }
    /*
     * check for non-conforming postscript
     */
    if (strncmp(currline, PS_FLAG, strlen(PS_FLAG)) != 0) {
        Debug(DB_PSDOC, "%%ps_gettype: no match PS_FLAG \"%s\"\n", currline);
        return PS_OTHER;
    }
    /*
     * we have some form of conforming postscript, try to identify the
     * type
     */
    Debug(DB_PSDOC, "%%ps_gettype: conforming postscript\n", 0);
    end_comments = 0;
    ps_type = PS_CONFORM;
    while (!end_comments) {
        /*
         * if we get end of file then we assume non-conforming PS
         */
        if (fgets(currline, LINESIZE-1, fd) == NULL) {
            Debug(DB_PSDOC, "%%ps_gettype: eof in comments\n", 0);
            return PS_OTHER;
        }
        /*
         * if we have run out of leading comments then assume 
         * conforming PS (because we had a valid "%!" line)
         */
        if (currline[0] != '%') {
            Debug(DB_PSDOC, "%%ps_gettype: out off comments\n", 0);
            fprintf(outfd, "%s", currline);
            return PS_CONFORM;
        }
        /*
         * print out the comment line with an extra % to disguise the comment
         */
        fprintf(outfd, "%%%s", currline);
        /*
         * check for the end of the leading comments section
         */
        if (strncmp(currline, "%%EndComments", 13) == 0)
            end_comments = 1;
        /*
         * Some tricky way to handle MS-windows postscript files...
         * probably doesn't work.
        */
        if (strncmp(currline, "%%Pages:", 8) == 0 &&
            ps_type == PS_MSWINDOWS)
            return ps_type;
        /*
         * once we know the type of PS, we no longer need to keep
         * checking.
         */
        if (ps_type != PS_CONFORM)
            continue;
        /*
         * check for mpage output
         */
        if (!strncmp(currline, "%%Creator: ", 11)) {
            if (!strncmp(currline+11, MPAGE, strlen(MPAGE))) {
                Debug(DB_PSDOC, "%%ps_gettype: mpage document\n", 0);
                ps_type = PS_MPAGE;
            }
            else if (!strncmp(currline+11, "Windows PSCRIPT", 15)) {
                Debug(DB_PSDOC, "%%ps_gettype: windows document\n", 0);
                ps_type = PS_MSWINDOWS;
            }
        }
        /*
         * check for psroff output
         */
        if (strncmp(currline, "%%Title: ", 9) == 0) {
            if (strstr(currline, "ditroff")) {
                Debug(DB_PSDOC, "%%ps_gettype: psroff\n", 0);
                ps_type = PS_PSROFF;
            }
            else if (strstr(currline, ".dvi")) {
                Debug(DB_PSDOC, "%%ps_gettype: dvi2ps\n", 0);
                ps_type = PS_TEX;
            }
        }
    }
#ifdef DEBUG
    if (ps_type == PS_CONFORM) {
        Debug(DB_PSDOC, "%%ps_gettype: unknown type, conforming PS\n", 0);
    }
#endif

    return ps_type;

} /* ps_gettype */



void
do_ps_doc(fd, asheet, outfd, fname)
 FILE *fd;
 struct sheet *asheet;
 FILE *outfd;
 char * fname;
{

    Debug(DB_PSDOC, "%%do_ps_doc: postscript document\n", 0);

    file_name = fname;
    ps_posttype = ps_gettype(fd,outfd);
    Debug(DB_PSDOC, "%%do_ps_doc: document type is %d\n", ps_posttype);
    if (ps_posttype != PS_NONE)
        do_post_doc(fd, asheet, outfd);

    return;

} /* do_ps_doc */



static void
do_post_doc(fd, asheet, outfd)
 FILE *fd;
 struct sheet *asheet;
 FILE *outfd;
{

    ps_at_trailer = FALSE;
    Debug(DB_POST, "%%do_post_doc: prolog\n", 0);
    ps_copyprolog(fd, outfd);
    /*
     * while there is still input, print pages
     */
    Debug(DB_POST, "%%do_post_doc: pages\n", 0);
    do_sheets(do_post_sheet, fd, asheet, outfd);
    Debug(DB_POST, "%%do_post_doc: trailer\n", 0);
    do_roff_tailer(fd, outfd);

    return;

} /* do_post_doc */


#if 0
/* not used yet... */
static void
do_other_doc(fd, asheet, outfd)
 FILE *fd;
 struct sheet *asheet;
 FILE *outfd;
{

    ps_at_trailer = FALSE;
    ps_copyprolog(fd, outfd);

    return;

} /* do_other_doc */
#endif



static void
ps_copyprolog(fd, outfd)
 FILE *fd;
 FILE *outfd;
{

    Debug(DB_PSDOC, "%%ps_copyprolog: adding mpage prolog\n", 0);
    if (!have_showsheet) {
        fprintf(outfd, "/showsheet { showpage } bind def\n");
        fprintf(outfd, "/showpage { } def\n");
        have_showsheet = 1;
    }
    had_ps = 1;
    Debug(DB_PSDOC, "%%ps_copyprolog: copying prolog\n", 0);
    if (ps_posttype == PS_PSROFF) {
        Debug(DB_PSDOC, "%%ps_copyprolog: calling ps_roff_copyprolog\n",0);
        ps_roff_copyprolog(fd, outfd);
        return;
    }
    if (ps_posttype == PS_MPAGE) {
        Debug(DB_PSDOC, "%%ps_copyprolog: calling ps_mpage_copyprolog\n",0);
        ps_mpage_copyprolog(fd, outfd);
        return;
    }
#if 0
    if (ps_posttype == PS_TEX)
        return;
#endif
    while (fgets(currline, LINESIZE-1, fd) != NULL) {
        if (strncmp(currline, "%%Page:", 7) == 0) {
            fprintf(outfd, "%% %s", currline);
            return;
        }
        if (ps_posttype == PS_TEX &&
            strncmp(currline, "TeXDict", 7) == 0) {
            /*
             * Hope all dvi2ps progs work the same:
             * capture the TeX init code so we can run it 'manually' for
             * every page. This is needed as this code sets up a gstate 
             * that conflicts with mpage...
             * This trick seems to work for text, but figures within the dvi
             * file seem to have a mind of their own...
             */
            if (tex1)
                free(tex1);
            tex1 = malloc(strlen(currline)+1);
            strcpy(tex1, currline);
            fgets(currline, LINESIZE-1, fd);
            if (tex2)
                free(tex2);
            tex2 = malloc(strlen(currline)+1);
            strcpy(tex2, currline);
            fgets(currline, LINESIZE-1, fd);
        }
        fprintf(outfd, "%s", currline);
    }
    Debug(DB_PSDOC, "%%ps_copyprolog: eof before %%%%EndProlog\n", 0);
    fprintf(outfd, "%%%%EndProlog\n");

    return;

} /* ps_copyprolog */



static void
ps_roff_copyprolog(fd, outfd)
 FILE *fd;
 FILE *outfd;
{

    Debug(DB_PSDOC, "%%ps_roff_copyprolog: copying psroff prolog\n", 0);
    while(fgets(currline, LINESIZE-1, fd) != NULL) {
 /*       if (strcmp(currline, "xi\n") == 0) */
        if (strstr(currline, "xi\n")) {
            fprintf(outfd, "%%%s", currline); 
            strcpy(ps_roff_xi, currline);
        }
        else if (strncmp(currline, "%%Page:", 7) == 0) {
            fprintf(outfd, "/p { } def\n");
            fprintf(outfd, "/xt { } def\n");
            fprintf(outfd, "/xs { } def\n");
            fprintf(outfd, "%% %s", currline);
            Debug(DB_PSDOC, "%%ps_copyprolog: Done\n", 0);
            return;
        }
        else
            fprintf(outfd, "%s", currline);
    }
    Debug(DB_PSDOC, "%%ps_copyprolog: eof before %%%%EndProlog\n", 0);
    fprintf(outfd, "/p { } def\n");
    fprintf(outfd, "/xt { } def\n");
    fprintf(outfd, "/xs { } def\n");
    fprintf(outfd, "%%%%EndProlog\n");

    return;

} /* ps_roff_copyprolog */



static void
ps_mpage_copyprolog(fd, outfd)
 FILE *fd;
 FILE *outfd;
{

    Debug(DB_PSDOC, "%%ps_mpage_copyprolog: skipping mpage prolog\n", 0);
    while(fgets(currline, LINESIZE-1, fd) != NULL) {
        if (strncmp(currline, "%%Page:", 7) == 0)  {
            fprintf(outfd, "%% %s", currline);
            Debug(DB_PSDOC, "%%ps_copyprolog: Done\n", 0);
            return;
        }
    }
} /* ps_mpage_copyprolog */



static void
ps_skip_to_page(fd)
 FILE *fd;
{

    Debug(DB_PSDOC, "%%ps_skip to page: reading until %%%%Page:\n", 0);
    while(fgets(currline, LINESIZE-1, fd) != NULL) {
        Debug(DB_PSDOC, "%% %s", currline);
        if (strncmp(currline, "%%Page:", 7) == 0)
            return;
    }
    Debug(DB_PSDOC, "%%ps_skip_to_page: eof before %%%%Page:\n", 0);

    return;

} /* ps_skip_to_page */

/* GPN */
/* #define NSCALE	to take care of previous scaling */
#ifdef NSCALE
char *NScale =  "/gpnsavematrix {orgmatrix currentmatrix pop} bind def\n"
                "/gpnrestorematrix {orgmatrix setmatrix} bind def\n"
                "/orgmatrix matrix def\n"
                "gpnsavematrix\n"
                "orgmatrix orgmatrix invertmatrix pop\n"
                "/gpnxs\n"
                "    orgmatrix 0 get 0.0000 eq\n"
                "     {orgmatrix 1 get abs}\n"
                "     {orgmatrix 0 get abs}\n"
                "    ifelse def\n"
                "/gpnys\n"
                "    orgmatrix 3 get 0.0000 eq\n"
                "     {orgmatrix 2 get abs}\n"
                "     {orgmatrix 3 get abs}\n"
                "    ifelse def\n"
                "/gpnxs gpnxs gscurrentresolution 0 get 72 div mul def\n"
                "/gpnys gpnys gscurrentresolution 1 get 72 div mul def\n";
#endif /* NSCALE */

static int
do_post_sheet(fd, asheet, outfd)
 FILE *fd;
 struct sheet *asheet;
 FILE *outfd;
{
    int rtn_val = FILE_MORE;
    int sh_high, sh_wide;

    if ((points->pp_origin_x == 0 && !points->skip) || opt_file) {
        /*
         * keep track of the pages printed
         */
        ps_pagenum++;
        fprintf(outfd, "%%%%Page: %d %d\n", ps_pagenum, ps_pagenum);
# ifdef DEBUG
        if (Debug_flag & DB_PSMPAGE)
            fprintf(outfd, "(Page: %d\\n) print flush\n", ps_pagenum);
# endif /* DEBUG */

        /*
         * Now is the time to print a sheet header...
         * for now, this has to be done before outline...
         */
        sheetheader(outfd, file_name);

        /*
         * print the page outline
         */
        mp_outline(outfd, asheet);

        /*
         * run through the list of base points for putting reduced pages
         * on the printed page
         */
        points = asheet->sh_pagepoints;
    }
    /*while (points->pp_origin_x != 0 && rtn_val == FILE_MORE) {*/
    while ((points->pp_origin_x!=0 || points->skip) && rtn_val == FILE_MORE) {

        /* GPN. skip this page ?*/
     	 if (points->skip!=0)   {
             ps_skip_to_page(fd);
             points++;
             continue;
     	 }

        /*
         * print one reduced page by moveing to the proper point,
         * turning to the proper aspect, scaling to the proper
         * size, and setting up a clip path to prevent overwritting;
         * the print a reduced page of output
         */
        Debug(DB_PSMPAGE, "%%%% %%%%ReducedPageStartsHere\n", outfd);
        fprintf(outfd, "/sheetsave save def\n");
        fprintf(outfd, "gsave\n");
# ifdef DEBUG
        if (Debug_flag & DB_PSMPAGE) {
            fprintf(outfd, "(    %d %d translate %d rotate\\n)",
                            points->pp_origin_x(), points->pp_origin_y(),
                            asheet->sh_rotate);
            fprintf(outfd, " print flush\n");
        }
# endif /* DEBUG */

#ifdef  NSCALE /*GPN*/
        fprintf (outfd, NScale);
        fprintf(outfd, "%d gpnxs mul %d gpnxs mul translate %d rotate\n",
                      points->pp_origin_x(), points->pp_origin_y(),
   	     	      	                           asheet->sh_rotate);

        sh_wide = (*asheet->sh_width)();
        sh_high = (*asheet->sh_height)();
#else /* NSCALE */
        fprintf(outfd, "%d %d translate\n",
                       points->pp_origin_x(), points->pp_origin_y());
   	if (asheet->sh_rotate)
            fprintf(outfd, "%d rotate\n", asheet->sh_rotate);

        /*
         * Take extra page margins into account
         */
        sh_wide = (*asheet->sh_width)() - pagemargin_left - pagemargin_right;
        sh_high = (*asheet->sh_height)() - pagemargin_top - pagemargin_bottom;
#endif

        if (opt_square) {
            int newhigh = sh_high, newwide = sh_wide;

            if (sh_wide * ps_height > sh_high * ps_width)
                newwide = (sh_high * ps_width) / ps_height;
            else
                newhigh = (sh_wide * ps_height) / ps_width;

#ifdef  NSCALE /*GPN*/
    	    fprintf(outfd, "%d gpnxs mul %d gpnys mul translate\n",
#else
            fprintf(outfd, "%d %d translate\n",
#endif

                           (sh_wide - newwide) / 2, (sh_high - newhigh) / 2);
            sh_wide = newwide;
            sh_high = newhigh;
   	}

#ifndef NSCALE
        fprintf(outfd, "%d %d translate\n", pagemargin_left, pagemargin_bottom);
#endif

        fprintf(outfd, "%d %d div %d %d div scale\n",
                       sh_wide, ps_width, sh_high, ps_height);
        /* output the clip path */

#ifdef  NSCALE /*GPN*/
        fprintf(outfd, "0 0 moveto 0 %d gpnys mul lineto %d gpnxs mul"
                       " %d gpnys mul lineto ",
       	       	    ps_height, ps_width, ps_height);
        fprintf(outfd, "%d gpnxs mul 0 lineto\n", ps_width);
#else /* NSCALE */
        fprintf(outfd, "0 0 moveto 0 %d lineto %d %d lineto ",
                       ps_height, ps_width, ps_height);
        fprintf(outfd, "%d 0 lineto\n", ps_width);
#endif

#ifdef  NSCALE /*GPN*/
   	fprintf(outfd, "closepath clip\n");
#else
        fprintf(outfd, "closepath clip newpath\n");
#endif /* NSCALE */
        if (ps_posttype == PS_TEX && tex1)   /* start dvi2ps init every page */
	    fprintf(outfd, "%s%s", tex1, tex2);

        /*
         * do the individual sheet setup
         */
        ps_sheetsetup(outfd);
        /*
         * place one reduce page on the printed page
         */
        rtn_val = post_onepage(fd, asheet, outfd);
        /*
         * clean up after mpage as drawn its page
         */
        fprintf(outfd, "grestore sheetsave restore\n");
        points++;
    }
    /*
     * print the sheet
     */
    if (points->pp_origin_x == 0 || (rtn_val == FILE_EOF && opt_file))
        fprintf(outfd, "showsheet\n");

    /*
     * let the upper level know about the status of possible EOF
     */
    return rtn_val;

} /* do_post_sheet */



static void
ps_sheetsetup(outfd)
 FILE *outfd;
{

    switch (ps_posttype) {
        case PS_PSROFF: fprintf(outfd, "%s", ps_roff_xi);
                        fprintf(outfd, "/p {} def\n");
                        break;
/*
        case PS_MPAGE:  fprintf(outfd, "/showpage {} def\n");
                        break;
*/
    }

        return;

} /* ps_sheetsetup */



static int
post_onepage(fd, asheet, outfd)
 FILE *fd;
 struct sheet *asheet;
 FILE *outfd;
{

    Debug(DB_PSROFF, "%%post_onepage: Begin page\n", 0);
    if (ps_at_trailer) {
        Debug(DB_PSROFF, "%%post_onepage: still at trailer\n", 0);
        return FILE_EOF;
    }
    while(fgets(currline, LINESIZE-1, fd) != NULL) {
        if (strncmp(currline, "%%Page:", 7) == 0) {
            fprintf(outfd, "%% %s", currline);
            Debug(DB_PSROFF, "%%post_onepage: next page\n", 0);
            return FILE_MORE;
        }
        if (strncmp(currline, "%%Trailer", 9) == 0) {
            fprintf(outfd, "%% %s", currline);
            Debug(DB_PSROFF, "%%post_onepage: found trailer\n", 0);
            ps_at_trailer = TRUE;
            return FILE_EOF;
        }
        if (ps_posttype == PS_MPAGE && strncmp(currline, "showsheet", 9) == 0)
            continue;
        if (ps_posttype == PS_TEX &&
            strncmp(currline, "TeXDict", 7) == 0) {
            /*
             * Hope all dvi2ps progs work the same:
             * capture the TeX init code so we can run it 'manually' for
             * every page. This is needed as this code sets up a gstate 
             * that conflicts with mpage...
             * This trick seems to work for text, but figures within the dvi
             * file seem to have a mind of their own...
             */
            if (tex1)
                free(tex1);
            tex1 = malloc(strlen(currline)+1);
            strcpy(tex1, currline);
            fgets(currline, LINESIZE-1, fd);
            if (tex2)
                free(tex2);
            tex2 = malloc(strlen(currline)+1);
            strcpy(tex2, currline);
            fgets(currline, LINESIZE-1, fd);
        }
        fprintf(outfd, "%s", currline);
    }
    Debug(DB_PSROFF, "%%post_onepage: eof\n", 0);

    return FILE_EOF;

} /* post_onepage */



static void
do_roff_tailer(fd, outfd)
 FILE *fd, *outfd;
{
#ifdef DEBUG
        int i = 0;
#endif

    Debug(DB_PSDOC, "%%do_roff_trailer: looking for eof\n", 0);
    while(fgets(currline, LINESIZE-1, fd) != NULL) {
#ifdef DEBUG
        i++;
        Debug(DB_PSDOC, "%%%s", currline);
#endif
        ;
    }
    Debug(DB_PSDOC, "%%do_roff_trailer: tailer of %d lines\n", i);

    return;

} /* do_roff_tailer */

