/*
 * file.c
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


/*
 * do_file converts one file into postscript for output.  The file type is
 * determined then the proper conversion routine is selected.
 */
void
do_file(fname, asheet, outfd)
 char *fname;
 struct sheet *asheet;
 FILE *outfd;
{
    FILE *fd;

    /*
     * if we have the pr option, then we have to assume it's a text file
     */
    if (opt_pr) {
        do_pr_file(fname, asheet, outfd);
        return;
    }
    /*
     * if not using pr(1), open fname and try to figure out what type of
     * file it is
     */
    if ((fd = fopen(fname, "r")) == NULL) {
        fprintf(stderr, "%s: cannot open %s\n", MPAGE, fname);
        perror(MPAGE);
        return;
    }

    /*
     * is input type forced?
     */
    switch (opt_input) {
	case IN_ASCII:  do_text_doc(fd, asheet, outfd, fname);
			break;
	case IN_PS:     do_ps_doc(fd, asheet, outfd, fname);
			break;
        /* Default figure out ourselfes */
    }

    /*
     * check for the cutomary characters that flag a postscript file
     */
    if (ps_check(fd))
        /*
         * found the flag signaling PS input
         */
        do_ps_doc(fd, asheet, outfd, fname);
    else
        /*
         * no postscript flag, print the ascii text
         */
        do_text_doc(fd, asheet, outfd, fname);

    (void) fclose(fd);

    return;

} /* do_file */



/*
 * do_file processes one text file into postscript, but first runs the file
 * through pr(1).
 */
void
do_pr_file(fname, asheet, outfd)
 char *fname;
 struct sheet *asheet;
 FILE *outfd;
{
    FILE *fd;
    char command[LINESIZE];

    /*
     * build the proper command based upon a specified
     * header or not
     */
    if (opt_header != NULL)
        (void)sprintf(command, "%s -l%d -w%d -h \"%s\" %s", prprog,
                  asheet->sh_plength, asheet->sh_cwidth, opt_header, fname);
    else
        (void)sprintf(command, "%s -l%d -w%d %s", prprog,
                  asheet->sh_plength, asheet->sh_cwidth, fname);
    /*
     * open a pipe to the proper pr(1) command, and pr provides
     * us with the input
     */
    if ((fd = popen(command, "r")) == NULL) {
        fprintf(stderr, "%s: cannot create pipe for '%s'\n", MPAGE, command);
        perror(MPAGE);
    }
    else {
        do_text_doc(fd, asheet, outfd, command);
        (void)pclose(fd);
    }

    return;

} /* do_pr_file */



/*
 * do_stdin uses do_????_doc to process the standard input
 */
void
do_stdin(asheet, outfd)
 struct sheet *asheet;
 FILE *outfd;
{
    FILE *fd;
    char command[LINESIZE];
    char tmpfile[LINESIZE];
    char buffer[LINESIZE];
    int incnt, outcnt;

    if (opt_pr) {
        Debug(DB_STDIN, "%%do_stdin: pr option selects text\n", 0);
        /*
         * if pr(1) is to be used we need to read the input
         * and pass it to a pr(1) command which will write
         * a temporary file; this temporary file will then
         * be used as input to the do_doc routine
         */
        (void)strcpy(tmpfile, "/usr/tmp/mpageXXXXXX");
        (void)mktemp(tmpfile);
        if (opt_header != NULL)
            (void)sprintf(command, "pr -l%d -w%d -h \"%s\"> %s",
                      asheet->sh_plength, asheet->sh_cwidth,
                      opt_header, tmpfile);
        else
            (void)sprintf(command, "pr -l%d -w%d > %s",
                      asheet->sh_plength, asheet->sh_cwidth, tmpfile);
        /*
         * open a pipe to the pr(1) command which will create a
         * temporary file for convertin into PS
         */
        if ((fd = popen(command, "w")) == NULL) {
            fprintf(stderr, "%s: cannot create pipe for '%s'\n",
                MPAGE, command);
            perror(MPAGE);
            return;
        }
#ifdef DEBUG
        errno = 0;
        Debug(DB_STDIN, "%% sizeof buffer == %d\n", sizeof buffer);
#endif
        /*
         * read input to mpage and pass it onto the pr(1) command
         */
        do {
            incnt = fread(buffer, 1, sizeof buffer, stdin);
            outcnt = fwrite(buffer, 1, incnt, fd);
            Debug(DB_STDIN, "%% incnt == %d,", incnt);
            Debug(DB_STDIN, " outcnt == %d,", outcnt);
            Debug(DB_STDIN, " errno == %d\n", errno);
        } while (incnt && outcnt);
        Debug(DB_STDIN, "%% Done with while\n", 0);
        (void)pclose(fd);
        Debug(DB_STDIN, "%% closed pipe, looking for tmpfile\n", 0);
        /*
         * now open the temporary file and use do_doc to
         * convert it to PS
         */
        if ((fd = fopen(tmpfile, "r")) == NULL) {
            fprintf(stderr, "%s: cannot open %s\n", MPAGE, tmpfile);
            perror(MPAGE);
        } else {
            Debug(DB_STDIN, "%% got tmpfile, now do_doc\n", 0);
            do_text_doc(fd, asheet, outfd, command);
            (void)fclose(fd);
        }
        /*
         * tidy up by removing our temp file
         */
        Debug(DB_STDIN, "%% now remove '%s'\n", tmpfile);
        (void)unlink(tmpfile);
    }
    else {
        /*
         * check for the cutomary flag at the start of postscript files
         */
        if (ps_check(stdin)) {
            /*
             * found the flag signaling PS input
             */
            Debug(DB_STDIN, "%%do_stdin: is postscript\n", 0);
            do_ps_doc(stdin, asheet, outfd, "stdin");
        }
        else {
            /*
             * no postscript flag, print the ascii text
             */
            Debug(DB_STDIN, "%%do_stdin: not postscript\n", 0);
            do_text_doc(stdin, asheet, outfd, "stdin");
        }
    }

    return;

} /* do_stdin */



/*
 * do_sheets() is called from do_xxx_doc() to render the sheets;
 * it does sheet selection and reversal.
 */
void
do_sheets(sheetfunc, inf, asheet, outf)
    int (*sheetfunc)();
    FILE *inf;
    struct sheet *asheet;
    FILE *outf;
{
    FILE *nullf = NULL;
    register int sheetno;

#define    WANTED(sn) \
    (sn >= opt_first && (opt_alt <= 1 || (sn - opt_first) % opt_alt == 0))

    if (opt_last == 0)
        opt_last = MAXINT;

    if (opt_alt > 1 || opt_first > 1)
        nullf = fopen("/dev/null", "w");

    if (opt_reverse) {
        FILE *revf;
        long *pagebase;
        int pageroom;

        revf = tmpfile();
        if (revf == NULL) {
            fprintf(stderr, "%s: can't create temporary file\n", MPAGE);
            exit(1);
        }
        pageroom = 50;
        pagebase = (long *)malloc(pageroom * sizeof(long));
        if(pagebase == NULL) {
            fprintf(stderr, "%s: can't malloc 50 words\n", MPAGE);
            exit(1);
        }
        pagebase[0] = 0;

        for (sheetno = 1; sheetno <= opt_last; ) {
            if ((*sheetfunc)(inf, asheet, WANTED(sheetno) ? revf : nullf)
                  == FILE_EOF)
                break;

            if (ferror(revf))
                break;

            pagebase[sheetno++] = ftell(revf);
            if (sheetno >= pageroom) {
                pageroom *= 4;
                pagebase = (long *)realloc(pagebase, pageroom * sizeof(long));
                if (pagebase == NULL) {
                    fprintf(stderr, "%s: can't malloc %d words\n",
                                    MPAGE, pageroom);
                    exit(1);
                }
        
            }
        }

        if (ferror(revf))
            fprintf(stderr, "%s: error writing to temporary file\n", MPAGE);
        else {
            pagebase[sheetno] = ftell(revf);
            rewind(revf);

            while (--sheetno >= 0) {
                register int i, n;
                char buf[BUFSIZ];

                fseek(revf, pagebase[sheetno], 0);
                for(i = pagebase[sheetno+1]-pagebase[sheetno]; i>0; i-=n) {
                    n = i < BUFSIZ ? i : BUFSIZ;
                    if (fread(buf, n, 1, revf) != 1) {
                        fprintf(stderr, "%s: Premature EOF on temp file\n",
                        MPAGE);
                        break;
                    }
                    (void) fwrite(buf, n, 1, outf);
                }
            }
        }
        fclose(revf);
        free(pagebase);

    }
    else {
        /* Normal, non-reversed pages */
        sheetno = 1;
        while (sheetno <= opt_last &&
               (*sheetfunc)(inf, asheet, WANTED(sheetno) ?
                        outf : nullf) != FILE_EOF)
            sheetno++;
    }

    if (nullf)
        fclose(nullf);

    return;

} /* do_sheets */
