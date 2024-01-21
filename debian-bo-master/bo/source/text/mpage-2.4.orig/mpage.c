/*
 * mpage.c
 */

/*
 * mpage:    a program to reduce pages of print so that several pages
 *           of output appear on one printed page.
 *
 * Copyright (c) 1988 Mark P. Hahn, Herndon, Virginia
 * Copyright (c) 1994-1996 Marcel J.E. Mol, Rijswijk, The Netherlands
 *                    marcel@mesa.nl

 * Written by:
 *   ...!uunet!\                       Mark Hahn, Sr Systems Engineer
 *              >pyrdc!mark            Pyramid Technology Corporation
 * ...!pyramid!/                       Vienna, Va    (703)848-2050
 *
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
#include <time.h>
#include <fcntl.h>

#include "encoding.h"

/*
 * Function Declarations
 */
static void ps_title();



int
main(argc, argv)
int argc;
char **argv;
{
    FILE *outfd;
    int currarg;
    struct sheet *thelist;
    struct sheet *thesheet;
    char outcommand[LINESIZE]; /* the command which is the output filter */


#ifdef OS2
    /*
     * wildcard expansion from emx package
     */
    _wildcard (&argc, &argv);
#endif

    /*
     * examine the environment for PRINTER (or LPDEST) and MPAGE
     * environment variables
     */
    if ((currarg = do_env()) < 0) {
        usage(currarg);
        exit(1);
    }
        
    if ((currarg = do_args(argc, argv, 0)) < 0) {
        usage(currarg);
        exit(1);
    }

    /*
     * if a print queue was specified then create a print command using
     * the queue, else use standard output.
     */
    if (doprint) {
        if (printque != NULL)
            (void) sprintf(outcommand, "%s %s%s",
                           printprog, printarg, printque);
        else
            (void) strcpy(outcommand, printprog);
        if ((outfd = popen(outcommand, "w")) == NULL) {
            fprintf(stderr, "%s: cannot create pipe for '%s'\n",
                            MPAGE, outcommand);
            perror(MPAGE);
        }
    }
    else
        outfd = stdout;

    /*
     * pick the array of sheet lists based upon the specified option
     */
    if (sheetorder == UPDOWN)
        sheetlist = up_down;
    else
        sheetlist = left_right;

    /*
     * from the array of sheet lists pick the proper sheet list for
     * the given sheetaspect, then select the proper sheet format
     * for the given number of redueced pages per output page
     */
    thelist = sheetlist[sheetaspect];
    thesheet = &(thelist[sheetindex]);

    /* GPN. */
    if (Coli == 1)
        thesheet = &(coli[0]);
    if (Coli == 2)
        thesheet = &(coli[1]);

    orientation = (sheetindex + sheetaspect) & 0x01;

    /*
     * if either lines or columns were specified as options, over
     * the default sheets idea of the number of line or columns
     * per reduced page
     */
    if (opt_lines > 0)
        thesheet->sh_plength = opt_lines;
    if (opt_width > 0)
        thesheet->sh_cwidth = opt_width;

    /*
     * Prepare the textbox parameters when needed
     */
    if (opt_textbox) {
        textbox.over = textmargin_left;
        textbox.lift = textmargin_bottom;
        textbox.wide = thesheet->sh_cwidth  - (textmargin_left+textmargin_right);
        textbox.high = thesheet->sh_plength - (textmargin_top+textmargin_bottom);
    }

    /*
     * if there are arguments left over after processing options, then
     * these are names of files to process, else process the standard
     * input
     */
    if (currarg < argc) {
        ps_title(argv[currarg], outfd);
        for ( ; currarg < argc; currarg++)
            do_file(argv[currarg], thesheet, outfd);
    }
    else {
        ps_title("<stdin>", outfd);
        do_stdin(thesheet, outfd);
    }

    /*
     * in case files are merged on sheets, make sure the last page
     * is printed...
     */
    if (points->pp_origin_x != 0 && !opt_file) {
        if (had_ps)
            fprintf(outfd, "showsheet\n");
        else {
            fprintf(outfd, "restore\n");
            fprintf(outfd, "showpage\n");
        }
    }

    /*
     * having processed all input, print out a PS trailer
     * (timeing stuff stolen from old adobe troff stuff)
     */
    fprintf(outfd, "%%%%Trailer\n");
    if (opt_verbose) {
        /*
        ps_pagenum++;
        fprintf(outfd, "%%%%Page: %d %d\n", ps_pagenum, ps_pagenum);
        */
        fprintf(outfd, "statusdict begin jobname print flush");
        fprintf(outfd, " (: Job finished:\\n) print\n");
        fprintf(outfd, "(\\tmpage time (s) = ) print flush usertime ");
        fprintf(outfd, "mp_stm sub 1000 div ==\n(\\tmpage pages = ) print");
        fprintf(outfd, " flush pagecount mp_pgc sub ==\nend flush\n");
        /* fprintf(outfd, "showpage\n"); */
    }
    fprintf(outfd, "%%%%Pages: %d\n", ps_pagenum);
    if (opt_verbose) {
        fprintf(stderr, "[%s: %d pages, ", MPAGE, ps_pagenum);
        if (doprint) {
            if (printque != NULL)
                fprintf(stderr, "print queue %s]\n", printque);
            else
                fprintf(stderr, "on default printer queue]\n");
        }
        else
            fprintf(stderr, "on <stdout>]\n");
    }
    /*
     * proper clean up to make sure the pipe is flushed
     */
    if (doprint)
        (void) pclose(outfd);

    return 0;

} /* main */



/*
 * ps_title prints a post script header suitable for PS processors
 */
static void
ps_title(name, outfd)
 char *name;
 FILE *outfd;
{
    time_t curr_time;
    char *time_str;
    FILE * charfp;
    char buf[LINESIZE];

    fprintf(outfd, "%%!PS-Adobe-2.0\n");
    fprintf(outfd, "%%%%DocumentFonts: %s Times-Bold\n", fontname);
    fprintf(outfd, "%%%%Title: %s (%s)\n", name, MPAGE);
    fprintf(outfd, "%%%%Creator: %s %s\n", MPAGE, VERSION);
    (void) time(&curr_time);
    time_str = ctime(&curr_time);
    fprintf(outfd, "%%%%CreationDate: %s", time_str);
    fprintf(outfd, "%%%%Orientation: %s\n", orientation ? "Landscape" : "Portrait");
    fprintf(outfd, "%%%%DocumentMedia: %s %d %d\n", media, ps_width, ps_height);
    fprintf(outfd, "%%%%BoundingBox: %d %d %d %d\n",
                   sheetmargin_left, sheetmargin_bottom,
                   ps_width - sheetmargin_right,
                   ps_height - sheetmargin_top);
    fprintf(outfd, "%%%%Pages: (atend)\n");
    fprintf(outfd, "%%%%EndComments\n\n");
    fprintf(outfd, "/mp_stm usertime def\n");
    fprintf(outfd, "/mp_pgc statusdict begin pagecount end def\n");
    fprintf(outfd, "statusdict begin /jobname (%s) def end\n", name);
    if (opt_duplex) {
       fprintf(outfd, "statusdict /setduplexmode known");
       fprintf(outfd, " { statusdict begin true setduplexmode end } if\n");
       if (opt_tumble) {
           fprintf(outfd, "statusdict /settumble known ");
           fprintf(outfd, "{ statusdict begin true settumble end } if\n"); 
       }
    }
    if (opt_encoding) {
        fprintf(outfd,
                "/reencsmalldict 12 dict def "
                "/ReEncodeSmall { reencsmalldict begin\n"
                "/newcodesandnames exch def "
                "/newfontname exch def "
                "/basefontname exch def\n"
                "/basefontdict basefontname findfont def "
                "/newfont basefontdict maxlength dict def\n"
                "basefontdict "
                "{ exch dup /FID ne "
                  "{ dup /Encoding eq "
                    "{ exch dup length array copy newfont 3 1 roll put } "
                    "{ exch newfont 3 1 roll put }\n"
                    "ifelse }\n"
                "  { pop pop }\n"
                "  ifelse } "
                "forall\n"
                "newfont /FontName newfontname put\n"
                "newcodesandnames aload pop "
                "newcodesandnames length 2 idiv\n"
                "{ newfont /Encoding get 3 1 roll put } "
                "repeat\n"
                "newfontname newfont definefont pop "
                "end } def\n");

        fprintf(outfd, "/charvec [\n");

        if (charvec_file != NULL) {
            if ((charfp = fopen(charvec_file, "r")) == NULL) {
                perror(charvec_file);
                exit(1);
            }
            while (fgets(buf, LINESIZE, charfp) != NULL) {
                if (*buf != '%' && *buf != '\n')
                    if (first_encoding == -1) {
                        first_encoding = atoi(buf);
                        last_encoding = atoi(strchr(buf, ' '));
# ifdef DEBUG
                        fprintf(stderr, "first=%d, last=%d\n",
                                        first_encoding, last_encoding);
#endif
                    }
                    else
                        fprintf(outfd, "%s", buf);
            }
        }
        else { /* use internal default encoding */
            int i;

            first_encoding = encoding_table_first;
            last_encoding  = encoding_table_last;
            for (i = 0; i <= last_encoding - first_encoding; i++)
                fprintf(outfd, "%s\n", encoding_table[i]);
        }
        fprintf(outfd, "] def\n");
        fprintf(outfd, "/%s /OurCharSet charvec ReEncodeSmall\n", fontname);
    }
    else {
        first_encoding = ' ';
        last_encoding = '~';
    }

    fprintf(outfd, "/textfont /%s findfont %d scalefont def\n",
                   opt_encoding ? "OurCharSet" : fontname, fsize - 1);
    fprintf(outfd, "/fnamefont /Times-Bold findfont %d scalefont def\n", HSIZE);
    fprintf(outfd, "/headerfont /Times-Bold findfont %d scalefont def\n",
                   HSIZE - 2);
    fprintf(outfd, "textfont setfont\n");
    fprintf(outfd, "(a) stringwidth pop /mp_a_x exch def\n");
# ifdef DEBUG
    if (Debug_flag & DB_PSMPAGE)
        fprintf(outfd, "(\\t'a' length ) print mp_a_x ==\nflush\n");
# endif
    fprintf(outfd, "%%%%EndProlog\n");

    return;

} /* ps_title */

