/**
 * mcvert.c - version 1.05 - 10 January, 1990
 * Written by Doug Moore - Rice University - dougm@rice.edu - April '87

 * Sun bug fixes, assorted stuff - Jim Sasaki, March '89

 * Changed default max_line_size from 2000 to unlimited -
 *                                           Doug Moore, April, '89

 * Sun 3/60 doesn't like odd-sized structs.  Bug fixed - Doug Moore, April, '89
 *                                              - aided by Spencer W. Thomas

 * Didn't handle properly many hqx files combined in one file.  Bug fixed -
 *                                           Doug Moore, June, '89

 * Modified to handle MacBinaryII specification. Jim Van Verth, Sept, '89

 * Fixed a bug when there are blank lines in hqx data, as happens when newline
 * get translated to CRLF and then to \n\n, common for some file transfers.
 * The last hqx line would be lost if the previous line was blank or junk.
 *	Glenn Trewitt, Stanford University, 1990	(1.05)

 * Fixed a bug that occurred when extracting data or resource
 * forks on a Sun 4.  It was a byte alignment problem.
 * Rick Zaccone, zaccone@bucknell.edu.  April 1991.  Version 1.6

 * Fixed:
 *   Sent all "Converting ... " lines to stdout instead of stderr
 *   Changed mactypes.h for HP-UX systems
 *      Alan Danziger, aland@cs.brandeis.edu.  October 1991. Version 1.6.5

 * ----------------------------------------------------------------------------
 * External
 * -----
 * Fixed buffering bug when converting very small MacBinary files to hqx files.
 * Provide helpful usage line.
 * Control "Converting ... " lines separately with -S flag.
 * Make encoding and decoding consistent by ignoring locked and init flags.
 * Clean up some error messages; check for more errors; provide errno on error.
 * Updated the man page.
 * -----
 * Internal
 * -----
 * Reformat source (sorry, local standard used by tools is tab space == 3)
 * Remove compiler warning messages.
 * Rename some variables.
 * Added some comments to code.
 * Added some offsets to struct definitions.
 * Since the makefile has compilation flags,
 *    make the compiles depend on the Makefile.
 * -----
 * Thanks to all who have gone before for creating, maintaining,
 * improving, and providing this program and documentation.
 * -----
 * Joseph Skudlarek  Mentor Graphics  8005 SW Boeckman Rd  Wilsonville OR 97070
 * {Jskud@std.mentorg.com,Joseph_Skudlarek@mentorg.com}
 * ...{uunet,nosun,sequent,attunix,apollo}!mntgfx!Jskud
 * Version 1.70 09Jul92
 * ----------------------------------------------------------------------------

 * ----------------------------------------------------------------------------
 * External
 * -----
 * Added -V (Verbose) option (includes debugging information).
 * Fixed bug converting hqx to MacBinary if last line is ":".
 * Avoided a silent error and quick exit situation.
 * -----
 * Internal
 * -----
 * Got rid of almost all lint (SunOS and HP-UX) error messages.
 * Compiled on SunOs, HP-UX, DomainOS.
 * Incorporated Parag Patel <parag@netcom.com> changes for AU/X.
 *    Here's some diffs for really quick cheap hacks to get mcvert to compile
 *    and run under A/UX.  The main problem was that timeb does not exist, so
 *    I added 2 #ifdef TIMEVAL to use the System-V timeval package instead.
 *    The Makefile just has a -DTIMEVAL and a magic -U_SYSV_SOURCE to get
 *    around a pre-defined type "ulong" in sys/types.h (thanks to Apple).
 * Did more code overhauling:
 *    add lots more comments, rename variables, reformat source.
 * Put code in un_hqx to avoid suspected buffering problem.
 * -----
 * Joseph Skudlarek  Mentor Graphics  8005 SW Boeckman Rd  Wilsonville OR 97070
 * (503) 685-1576 (work)
 * {Jskud@std.MENTORG.Com,Joseph_Skudlarek@MENTORG.Com}
 * ...{uunet,nosun,sequent,attunix,apollo}!mntgfx!Jskud
 * Version 1.80 15Jul92
 * ----------------------------------------------------------------------------

 * ----------------------------------------------------------------------------
 * External
 * -----
 * Made hqx file scan processing much smarter
 *    so, for example, info-mac/comm/qwk-reader.hqx,
 *    complete with extraneous colons in column one, converts correctly
 *    (problem described by Edward John Sabol <es2j+@andrew.cmu.edu>)
 * Avoid silly perror on usage message (prompted by Edward John Sabol)
 * Improve error message regarding improper format
 * Added more caveats to man page
 * -----
 * Internal
 * -----
 * Fixed typo's in printf lines to pass all expected arguments
 *    (pointed out by Bo Holst-Christensen
 *    [holst@diku.dk/dikubhc1@uts.uni-c.dk/holst@login.dkuug.dk])
 * Tweak Makefile to ease shar creation and special case ulong, not A/UX
 * Add yet more comments and debugging code
 * -----
 * Joseph Skudlarek  Mentor Graphics  8005 SW Boeckman Rd  Wilsonville OR 97070
 * (503) 685-1576 (work)
 * {Jskud@std.MENTORG.Com,Joseph_Skudlarek@MENTORG.Com}
 * ...{uunet,nosun,sequent,attunix,apollo}!mntgfx!Jskud
 * Version 1.82 30Jul92
 * ----------------------------------------------------------------------------

 * ----------------------------------------------------------------------------
 * External
 * -----
 * relax exactly 64 characters per incoming hqx file, and
 * handle files without trailing newline, so, eg, 
 *    Telnet2.5docs.sit.hqx now converts correctly
 *       (failure reported by Justin Sullivan <justin@f2.facts.uky.edu>)
 *    now also processes info-mac/app/road-map.hqx correctly
 *       (failure reported by Victor Norton<norton@andy.bgsu.edu>)
 * rework the man page for improved clarity and completeness
 * -----
 * Internal
 * -----
 * avoid warning message from gcc on Sequent Balance mainframe
 *    reported by Justin Sullivan <justin@f2.facts.uky.edu>
 * bump max incoming line length to 2048 from 255
 * add mcvert.ps target to Makefile
 * -----
 * Joseph Skudlarek  Mentor Graphics  8005 SW Boeckman Rd  Wilsonville OR 97070
 * (503) 685-1576 (work)
 * {Jskud@std.MENTORG.Com,Joseph_Skudlarek@MENTORG.Com}
 * ...{uunet,nosun,sequent,attunix,apollo}!mntgfx!Jskud
 * Version 1.83 03Aug92
 * ----------------------------------------------------------------------------

 * ----------------------------------------------------------------------------
 * External
 * -----
 * Found and fixed problem with byte ordering plaguing users of
 *    Sequent's Balance running DYNIX, and DEC computers.
 *    The error message looked something like
 *       hqx_to_bin_fork: writing nnn too many bytes
 * Avoid generating debugging info if not to be printed -- cut runtime in half!
 * Generalize and incorporate -I (info) option processing provided by
 *    Paul Franklin, Computer Enginnering, Univ. of Calif., Davis CA 95616
 *    pdfranklin@ucdavis.edu.
 * Added heuristic to avoid false matches in mail headers.
 *    problem expertly characterized, solution beta tested, and subsequent
 *    improvement suggested by "Jim (J.) Lattanzi" <lattanzi@bnr.ca>
 *    so segmented comp.binaries.mac files (either multi-file or concatenated
 *    single file) should now convert correctly.
 * Added -H switch to disable heuristic.
 * Document heuristic in man page.
 * Fixed (long-standing) bug which precluded -p option from being recognized
 *    and verified decompressing and unpacking of PIT files working.
 *    Thanks to Dave Clemans for providing me with a version of PackIt.
 * Add the version to the extened Usage message emitted by the program.
 * Tune the syntax of the summary in the program and man page.
 * Cleaned up spelling mistakes in the man page.
 * -----
 * Internal
 * -----
 * Close all open streams --
 *    fix for binfile by Paul Franklin <pdfranklin@ucdavis.edu>
 * Incorporate changes suggested by Barry_Wolman@transarc.com
 *    to mactypes.h and Makefile for support of IBM RS/6000 running AIX 3.2
 *    reformat Makefile to avoid long option lines
 * Identify the right Makefile lines for Irix too
 *    suggested by Jack Repenning (jackr@dblues.wpd.sgi.com)
 * Clean up the stream handling and add mopen/mclose
 *    avoid unnecessary /dev/null opens
 *    all file open/close/read/write are checked for success
 * Lower lint content on SunOS and HP-UX.
 *    avoiding all "sometimes ignored" lint messages.
 * Improve modularity with mopen/mclose/converting routines.
 * Tune debugging output information.
 * Verify that passes smoke tests on DomainOS/SunOS/HP-UX/ULTRIX.
 * Reformat these comments to avoid tabs.
 * -----
 * Joseph Skudlarek  Mentor Graphics  8005 SW Boeckman Rd  Wilsonville OR 97070
 * (503) 685-1576 (work)
 * {Jskud@std.MENTORG.Com,Joseph_Skudlarek@MENTORG.Com}
 * ...{uunet,nosun,sequent,attunix,apollo}!mntgfx!Jskud
 * Version 1.87 25Sep92
 * ----------------------------------------------------------------------------

 * ----------------------------------------------------------------------------
 * External
 * -----
 * Add README file which describes how to configure and compile mcvert
 * Handle multiple BinHex4.0 inputs in a single file again (thanks to
 *    <Mark_Larimer@pigeon.cpg.cdc.com> for pointing out this regression)
 * Emit the MacBinary header if verbose (to get create and modify times)
 * -----
 * Internal
 * -----
 * Rename some variables, create mac2unix (time) routine, more comments
 * Keep lint content low
 * Pull unnecessary include of <net/nh.h> -- avoid breaking AIX 3.1
 * Fix = vs == typo dealing with protect bit
 * -----
 * Joseph Skudlarek  Mentor Graphics  8005 SW Boeckman Rd  Wilsonville OR 97070
 * (503) 685-1576 (work)
 * {Jskud@std.MENTORG.Com,Joseph_Skudlarek@MENTORG.Com}
 * ...{uunet,nosun,sequent,attunix,apollo}!mntgfx!Jskud
 * Version 1.88 08Dec92
 * ----------------------------------------------------------------------------

 * ----------------------------------------------------------------------------
 * External
 * -----
 * emit input file names when verbose is enabled
 *    (suggested by franklin@eecs.ucdavis.edu)
 * make it easier to build on AT&T 3B2's
 * -----
 * Internal
 * -----
 * provide compile time switch to avoid bzero and bcopy, and use memset
 *    and memcpy instead (pointed out by linger@drystone.attmail.com, and
 *    requested again by Larry S. Staples <attjp4!lss>)
 * update Makefile to include incantations required for AT&T 3B2's
 * fflush all diagnostic output to ensure correct order when output to a file
 * -----
 * Joseph Skudlarek  Mentor Graphics  8005 SW Boeckman Rd  Wilsonville OR 97070
 * (503) 685-1576 (work)
 * {Jskud@std.MENTORG.Com,Joseph_Skudlarek@MENTORG.Com}
 * ...{uunet,nosun,sequent,attunix,apollo}!mntgfx!Jskud
 * Version 1.89 05Jan93
 * ----------------------------------------------------------------------------

 * ----------------------------------------------------------------------------
 * External
 * -----
 * incorporate MAC_FILETYPE support provided by <root@genome.stanford.edu>
 * minor edits to man page
 * -----
 * Internal
 * -----
 * update Makefile to simplify incantations required for AT&T 3B2's
 * -----
 * Joseph Skudlarek  Mentor Graphics  8005 SW Boeckman Rd  Wilsonville OR 97070
 * (503) 685-1576 (work)
 * {Jskud@std.MENTORG.Com,Joseph_Skudlarek@MENTORG.Com}
 * ...{uunet,nosun,sequent,attunix,apollo}!mntgfx!Jskud
 * Version 1.90 04Mar93
 * ----------------------------------------------------------------------------

 * ----------------------------------------------------------------------------
 * External
 * -----
 * handle -b (both .data and .rsrc at same time) option
 *    ( -> MacBinary suggested by bb@math.ufl.edu)
 * remove anomalous file extension handling
 *    (suggested by bb@math.ufl.edu)
 * regularize MAC_EDITOR (author) and MAC_FILETYPE (file type) handling
 * detect and report important file format errors
 * emit output file name too
 * revise Usage line
 * massively revise man page to reflect changes and generally overhaul
 * -----
 * Internal
 * -----
 * avoid overwriting internal storage (what a chore!)
 *    for example, mcvert -UI *.hqx used to abort with a segmentation violation
 *    symptom reported by franklin@eecs.ucdavis.edu  Thu Sep 24 16:39:21 1992
 * check return values from all getc/putc operations
 * find and fix ancient bug extracting resource fork from MacBinary format
 * identify failing file in EOF error messages
 * clarify and amend distribution and update restrictions
 * expand disclaimer (patterned after INFO-MAC CD-ROM -- Thx, Cliff and Joe!)
 * -----
 * Joseph Skudlarek  Mentor Graphics  8005 SW Boeckman Rd  Wilsonville OR 97070
 * (503) 685-1576 (work)
 * {Jskud@std.MENTORG.Com,Joseph_Skudlarek@MENTORG.Com}
 * ...{uunet,nosun,sequent,attunix,apollo}!mntgfx!Jskud
 * Version 2.00 28Feb93
 * ----------------------------------------------------------------------------

 * ----------------------------------------------------------------------------
 * External
 * -----
 * unixify all relevent output files
 *    (replace suspect characters with _)
 * macify all relevent MacBinary files
 *    (replace suspect chars with -, take first 20 and last 11 if > 31 char)
 *    avoiding Mac file name > 31 chars (Rick Zaccone zaccone@bucknell.edu)
 * always emit the Macintosh file name in the converting messages
 *    since the UNIX file names are now provided by default
 * rework man page to bring it more up to date, added OTHER SOURCES section
 * emit data and rsrc len when printing bin header
 * report the input character, not the mapped character, if avail,
 *    else report mapped value as hex
 * add -VV (Very Verbose) option
 * -----
 * Internal
 * -----
 * distribute mcvert.idraw, a postscript file
 *    describing mcvert options and transformations pictorially,
 *    contributed by Brian Bartholomew - bb@math.ufl.edu
 * create and distribute README-conversion file
 * update Makefile and README to make it more obvious how to build mcvert
 *    problem reported by David Micklethwaite <mickles@cherax.super.csiro.au>
 * make s/S/v/V flag processing serially reusable
 * avoid obsolete ftime on HP-UX, SunOS, DomainOS -- default is now -DTIMEVAL
 *    problem reported by smith@sfu.ca (Richard Smith) and
 *    Adam Harris (harris@cs.uchicago.edu)
 * avoid SGI bug regarding unterminated character constant within #ifdef notdef
 *    problem reported by smith@sfu.ca (Richard Smith)
 * clean up some FILE confusion
 * make the man page work well across platforms
 * re-lint on SunOS and HP-UX
 * add various additional comments
 * -----
 * Joseph Skudlarek  Mentor Graphics  8005 SW Boeckman Rd  Wilsonville OR 97070
 * (503) 685-1576 (work)
 * {Jskud@wv.MentorG.com,Joseph_Skudlarek@MentorG.com}
 * Version 2.09 30Jun93
 * ----------------------------------------------------------------------------

 * ----------------------------------------------------------------------------
 * External
 * -----
 * -----
 * Internal
 * -----
 * incorporate SCO UNIX requirements into the Makefile
 *    info from Fred Lenk, Camarillo, CA [fredgl@tecnet1.jcte.jcs.mil]
 * backout intermediate XOBJ cleanup and continue to do what works for AT&T 3B2
 *    belated & current thanks to Larry S. Staples [attjpn!lss@attibr.att.com]
 *    for providing and proofing the working recipe for AT&T 3B2
 * change OTHER SOURCES to OTHER PROGRAMS in man page, and mention programs
 *    which run on the Mac, including CompactPro, StuffIt, and BinHex 4.0
 * ship the formatted ASCII version of the man page for those without nroff
 * -----
 * Joseph Skudlarek  Mentor Graphics  8005 SW Boeckman Rd  Wilsonville OR 97070
 * (503) 685-1576 (work)
 * {Jskud@wv.MentorG.com,Joseph_Skudlarek@MentorG.com}
 * Version 2.12 19Jul93
 * ----------------------------------------------------------------------------

 * ----------------------------------------------------------------------------
 * External
 * -----
 * -----
 * Internal
 * -----
 * add StuffIt Expander mention to man page
 * suggest using text (-t|-u) if data is text in the data (-d) description
 * incorporate AIX Makefile improvement provided by
 *    DaviD W. Sanderson (dws@ssec.wisc.edu)
 * -----
 * Joseph Skudlarek  Mentor Graphics  8005 SW Boeckman Rd  Wilsonville OR 97070
 * (503) 685-1576 (work)
 * {Jskud@wv.MentorG.com,Joseph_Skudlarek@MentorG.com}
 * Version 2.13 13Sep93
 * ----------------------------------------------------------------------------

 * ----------------------------------------------------------------------------
 * External
 * -----
 * add -P (pipe to stdout) option
 *    capability requested by lentz@rossi.astro.nwu.edu (Robert Lentz)
 * have all info messages go to stderr, not stdout, to faciliate -P
 *    (undo converting msg to stdout from version 1.6.5, dated Oct 1991)
 * update man page to indicate changes
 * -----
 * Internal
 * -----
 * fiddle with info message to make it a bit clearer
 * -----
 * Joseph Skudlarek  Mentor Graphics  8005 SW Boeckman Rd  Wilsonville OR 97070
 * (503) 685-1576 (work)
 * {Jskud@wv.MentorG.com,Joseph_Skudlarek@MentorG.com}
 * Version 2.14 10Nov93
 * ----------------------------------------------------------------------------

 * ----------------------------------------------------------------------------
 * External
 * -----
 * -----
 * Internal
 * -----
 * ensure all source line lengths are less than 80
 *    after someone or something wrapped the 2.14 archive,
 *    and it would not compile (split a character string literal)
 *    [recall that shar usually prepends X, which bumps the line length by 1]
 *    [also, this makes editing with emacs at 80 columns a bit better]
 * add check_linelen to Makefile to ensure compliance
 * -----
 * Joseph Skudlarek  Mentor Graphics  8005 SW Boeckman Rd  Wilsonville OR 97070
 * (503) 685-1576 (work)
 * {Jskud@wv.MentorG.com,Joseph_Skudlarek@MentorG.com}
 * Version 2.15 15Nov93
 * ----------------------------------------------------------------------------

 * ----------------------------------------------------------------------------
 * This program may be freely distributed for non-profit purposes.  It
 * may not be sold, by itself or as part of a collection of software.
 * However, it may be distributed in source form with large
 * collections of freeware and shareware, such as the INFO-MAC CD-ROM,
 * which charge only a modest fee for publishing, but not selling, the
 * software.  It may be freely modified as long as no modified version
 * is independently distributed.  Modifications of interest to all can
 * be incorporated into the program by sending them to me for
 * inclusion and redistribution, or by releasing an updated mcvert to
 * the info-mac archives.  Parts of the code can be used in other
 * programs.  We hope you find mcvert useful, and enjoy using it.
 * ----------------------------------------------------------------------------

 * ----------------------------------------------------------------------------
 * DISCLAIMER -- USE mcvert software AT YOUR OWN RISK
 * This mcvert software is provided "as is" without warrantee of any
 * kind.  The entire risk as to the results and performance of the
 * software is assumed by you, and in no event will we be liable for
 * any consequential, incidental, or indirect damages suffered in the
 * course of using this software.
 * ----------------------------------------------------------------------------

 * ----------------------------------------------------------------------------
 * Things that yet could be done:
 * ---
 * handle incoming BinHex4.0 files with ^M instead of ^J
 * (requested by jonathan brecher brecher@husc.harvard.edu 29Mar93)
 *  eg, right now we toss the entire line if it doesn't start with a colon
 *  but the start of the line could be later on, following a ^M
 * check for more file format errors, eg, MacBinary must be multiple of 128
 * option to avoid .text extension on write (Rick Zaccone zaccone@bucknell.edu)
 * check return values from fputs/fprintf
 * provide header heuristic tuning option: set length and/or same sensitivity
 * ----------------------------------------------------------------------------
 */

/*
 * Naming
 *		DOWNLOAD
 *			=> converting TO MacBinary
 *			=> direction == FORWARDS
 *			=> use un_* routines (un => UNdo encoding?)
 *		UPLOAD
 *			=> converting FROM MacBinary
 *			=> direction == BACKWARDS
 *			=> use re_* routines (re => Really Encode?)
 */

#include "mactypes.h"

/* it would be natural to use an enum here, but avoid "fancy" features */
#define HQX 0
#define TEXT 1
#define DATA 2
#define RSRC 3
#define BOTH 5

#define FORWARDS 0
#define BACKWARDS 1

FILE *devnull;
FILE *convert;
FILE *verbose;
FILE *debug;
int   Debug;

char **hqxnames, **hqxnames_left;
char *dir, *ext, *mac_auth, *mac_type;
int translate_eol;
char *maxlines_str;
int maxlines;

/* used to skip suspect mail header lines */
int suspect_shorter = 12;
int suspect_same = 1;

/* used to avoid writing output files */
int info_only;

/* pipe appropriate output -- write to stdout */
int pipe_out;

char Usage[] = "\
Usage: %s { [option] ... name ...} ...\n\
 version:\t%4.2f\n\
 default:\t-xDqv\n\
\n\
 option:\n\
\t-x\tBinHex        .hqx  <-> MacBinary\n\
\t-u\tText(trans)   .text <-> MacBinary\n\
\t-h\tHost(as is)   .text <-> MacBinary\n\
\t-d\tData          .data <-> MacBinary\n\
\t-r\tResource      .rsrc <-> MacBinary\n\
\t-b\tBoth    .data .rsrc <-> MacBinary\n\
\n\
\t-D\tDownload       Other -> MacBinary\n\
\t-U\tUpload     MacBinary -> Other\n\
\n\
\t-p\tBinHex -> MacBinary => unpack PIT\n\
\t-q\tdisable unpack PIT\n\
\t-t\ttranslate end-of-line chars (useful with -b)\n\
\n\
\t-I\tInformation only (does not write output files)\n\
\t-P\tPipe output to stdout\n\
\t-s\tsilent\n\
\t-S\tSilent about ``Converting ... '' lines too\n\
\t-v\tverbose\n\
\t-V\tVerbose, includes debugging information\n\
\t-VV\tVery Verbose, includes extra debugging information\n\
\t-H\tdisable skip-legal-but-suspect-lines Heuristic\n\
\n\
Environment:\n\
\tMAC_FILETYPE  \tTEXT|????\tMac file type for Text|other\n\
\tMAC_EDITOR    \tMACA|????\tMac creator (author) for Text|other\n\
\tMAC_EXT       \t.bin     \textension for -D\n\
\tMAC_DLOAD_DIR \t.        \tdirectory for -D\n\
\tMAC_LINE_LIMIT\tnone     \tmaximum line length for -Ux\n\
";

char *cmdname;

main(argc, argv)
	int argc;
	char **argv;
{
	char *flags, *getenv();
	int direction, mode, unpit_flag;

	cmdname = argv[0];

	/* Early error and clean exit if missing arguments */
	if (argc < 2) {
		usage();
		/*NOTREACHED*/
	}

	devnull = fopen("/dev/null", "w+");

	argv++;
	argc--;

	convert = stderr;
	verbose = stderr;
	debug = devnull;
	Debug = 0;

	direction = FORWARDS;
	mode = HQX;
	unpit_flag = 0;

	mac_type = getenv("MAC_FILETYPE");
	mac_auth = getenv("MAC_EDITOR");

	if ((ext = getenv("MAC_EXT")) == NULL)
		ext = ".bin";
	if ((dir = getenv("MAC_DLOAD_DIR")) == NULL)
		dir = ".";
	if ((maxlines_str = getenv("MAC_LINE_LIMIT")) == NULL)
		maxlines = 0;
	else {
		maxlines = atoi(maxlines_str);
		if (maxlines < MIN_HQX_LINES) {
			fprintf(stderr, "%s: %s; was %d; reset to %d\n",
				cmdname,
				"warning: MAC_LINE_LIMIT too small",
				maxlines, MIN_HQX_LINES);
			fflush(stderr);
			maxlines = MIN_HQX_LINES;
		}
	}

	/* Make command line arguments globally accessible */
	hqxnames = (char **) calloc((unsigned)argc + 1, sizeof(char *));
	hqxnames_left = hqxnames;
	while (argc--)
		*hqxnames_left++ = *argv++;

	/* Flag the end of the list */
	*hqxnames_left = "-";
	hqxnames_left = hqxnames;

	/* While not at the end of the list */
	while (strcmp(*hqxnames_left, "-")) {
		translate_eol = 0;
		if (hqxnames_left[0][0] == '-') {
			flags = *hqxnames_left++;
			while (*++flags)
				switch (*flags) {
				case 'x':
					mode = HQX;
					break;
				case 'u':
					translate_eol = 1;
					mode = TEXT;
					break;
				case 'd':
					mode = DATA;
					break;
				case 'r':
					mode = RSRC;
					break;
				case 'h':
					translate_eol = 0;
					mode = TEXT;
					break;
				case 'b':
					mode = BOTH;
					break;
				case 't':
					translate_eol = 1;
					break;
				case 'D':
					direction = FORWARDS;
					break;
				case 'U':
					direction = BACKWARDS;
					break;
				case 'q':
					unpit_flag = 0;
					break;
				case 'p':
					unpit_flag = 1;
					break;
				case 'S':
					convert = devnull;
					verbose = devnull;
					debug = devnull;
					Debug = 0;
					break;
				case 's':
					convert = stderr;
					verbose = devnull;
					debug = devnull;
					Debug = 0;
					break;
				case 'v':
					convert = stderr;
					verbose = stderr;
					debug = devnull;
					Debug = 0;
					break;
				case 'V':
					convert = stderr;
					verbose = stderr;
					debug = stderr;
					Debug++;
					break;
				case'H':
					suspect_shorter = suspect_same = 0;
					break;
				case 'I':
					info_only = 1;
					break;
				case 'P':
					pipe_out = 1;
					break;
				default:
					usage();
					/*NOTREACHED*/
				}
		}

		if (direction == BACKWARDS)
			if (mode == HQX && unpit_flag)
				re_hqx();      /* no re_pit() yet */
			else if (mode == HQX)
				re_hqx();
			else
				re_other(mode);
		else if (mode == HQX)
			un_hqx(unpit_flag);
		else
			un_other(mode);
	}

	exit(0);
	/*NOTREACHED*/
}

/* An array useful for CRC calculations that use 0x1021 as the "seed" */
word magic[] = {
    0x0000, 0x1021, 0x2042, 0x3063, 0x4084, 0x50a5, 0x60c6, 0x70e7,
    0x8108, 0x9129, 0xa14a, 0xb16b, 0xc18c, 0xd1ad, 0xe1ce, 0xf1ef,
    0x1231, 0x0210, 0x3273, 0x2252, 0x52b5, 0x4294, 0x72f7, 0x62d6,
    0x9339, 0x8318, 0xb37b, 0xa35a, 0xd3bd, 0xc39c, 0xf3ff, 0xe3de,
    0x2462, 0x3443, 0x0420, 0x1401, 0x64e6, 0x74c7, 0x44a4, 0x5485,
    0xa56a, 0xb54b, 0x8528, 0x9509, 0xe5ee, 0xf5cf, 0xc5ac, 0xd58d,
    0x3653, 0x2672, 0x1611, 0x0630, 0x76d7, 0x66f6, 0x5695, 0x46b4,
    0xb75b, 0xa77a, 0x9719, 0x8738, 0xf7df, 0xe7fe, 0xd79d, 0xc7bc,
    0x48c4, 0x58e5, 0x6886, 0x78a7, 0x0840, 0x1861, 0x2802, 0x3823,
    0xc9cc, 0xd9ed, 0xe98e, 0xf9af, 0x8948, 0x9969, 0xa90a, 0xb92b,
    0x5af5, 0x4ad4, 0x7ab7, 0x6a96, 0x1a71, 0x0a50, 0x3a33, 0x2a12,
    0xdbfd, 0xcbdc, 0xfbbf, 0xeb9e, 0x9b79, 0x8b58, 0xbb3b, 0xab1a,
    0x6ca6, 0x7c87, 0x4ce4, 0x5cc5, 0x2c22, 0x3c03, 0x0c60, 0x1c41,
    0xedae, 0xfd8f, 0xcdec, 0xddcd, 0xad2a, 0xbd0b, 0x8d68, 0x9d49,
    0x7e97, 0x6eb6, 0x5ed5, 0x4ef4, 0x3e13, 0x2e32, 0x1e51, 0x0e70,
    0xff9f, 0xefbe, 0xdfdd, 0xcffc, 0xbf1b, 0xaf3a, 0x9f59, 0x8f78,
    0x9188, 0x81a9, 0xb1ca, 0xa1eb, 0xd10c, 0xc12d, 0xf14e, 0xe16f,
    0x1080, 0x00a1, 0x30c2, 0x20e3, 0x5004, 0x4025, 0x7046, 0x6067,
    0x83b9, 0x9398, 0xa3fb, 0xb3da, 0xc33d, 0xd31c, 0xe37f, 0xf35e,
    0x02b1, 0x1290, 0x22f3, 0x32d2, 0x4235, 0x5214, 0x6277, 0x7256,
    0xb5ea, 0xa5cb, 0x95a8, 0x8589, 0xf56e, 0xe54f, 0xd52c, 0xc50d,
    0x34e2, 0x24c3, 0x14a0, 0x0481, 0x7466, 0x6447, 0x5424, 0x4405,
    0xa7db, 0xb7fa, 0x8799, 0x97b8, 0xe75f, 0xf77e, 0xc71d, 0xd73c,
    0x26d3, 0x36f2, 0x0691, 0x16b0, 0x6657, 0x7676, 0x4615, 0x5634,
    0xd94c, 0xc96d, 0xf90e, 0xe92f, 0x99c8, 0x89e9, 0xb98a, 0xa9ab,
    0x5844, 0x4865, 0x7806, 0x6827, 0x18c0, 0x08e1, 0x3882, 0x28a3,
    0xcb7d, 0xdb5c, 0xeb3f, 0xfb1e, 0x8bf9, 0x9bd8, 0xabbb, 0xbb9a,
    0x4a75, 0x5a54, 0x6a37, 0x7a16, 0x0af1, 0x1ad0, 0x2ab3, 0x3a92,
    0xfd2e, 0xed0f, 0xdd6c, 0xcd4d, 0xbdaa, 0xad8b, 0x9de8, 0x8dc9,
    0x7c26, 0x6c07, 0x5c64, 0x4c45, 0x3ca2, 0x2c83, 0x1ce0, 0x0cc1,
    0xef1f, 0xff3e, 0xcf5d, 0xdf7c, 0xaf9b, 0xbfba, 0x8fd9, 0x9ff8,
    0x6e17, 0x7e36, 0x4e55, 0x5e74, 0x2e93, 0x3eb2, 0x0ed1, 0x1ef0
};


/*
 * calc_crc() --
 *   Compute the MacBinary II-style CRC for the data pointed to by p, with the
 *   crc seeded to seed.
 *
 *   Modified by Jim Van Verth to use the magic array for efficiency.
 */
short
calc_mb_crc(p, len, seed)
	unsigned char *p;
	long len;
	short seed;
{
	short hold;		/* crc computed so far */
	long i;			/* index into data */

	extern unsigned short magic[];	/* the magic array */

	hold = seed;			       /* start with seed */
	for (i = 0; i < len; i++, p++) {
		hold ^= (*p << 8);
		hold = (hold << 8) ^ magic[(unsigned char) (hold >> 8)];
	}

	return (hold);
}				/* calc_crc() */


/* Report a fatal error, and exit, never return */
error(msg, name)
	char msg[], name[];

{
	fprintf(stderr, msg, name);
	(void)putc('\n', stderr);
	perror("\nlast perror (may not be relevant)");
	fprintf(stderr, "%s: exiting\n", cmdname);
	if (debug == stderr)
		abort();
	exit(2);
	/*NOTREACHED*/
}

/* replace illegal Unix characters in file name */
/* make sure host file name doesn't get truncated beyond recognition */
unixify(np)
	register char *np;
{
	register ulong c;

	c = strlen(np);
	if (c > SYSNAMELEN - MAXEXTENSION)
		c = SYSNAMELEN - MAXEXTENSION;
	np[c] = '\0';

	/* pre-decrement to match pre-increment within loop */
	np--;
	while (c = *++np)
		/*
		 * that is, ``if control or blank, or slash, or delete or 8bit''
		 * which is the same as ``if blank, slash, or non graphic''
		*/
		if (c <= ' ' || c == '/' || c > '~')
			*np = '_';
}

/*
 * replace illegal Macintosh characters in file name
 * return resulting length
 *
 * According to Inside Macintosh, IV-90, valid file names
 *	must be [1..31] characters long
 *	must not contain a colon
 *	must contain only printing characters
 */
macify(name, len, translate)
	char *name;
	int len;
	int translate;
{
	register char *np;
	register ulong c;
	char *s, *t;
	char buffer[SYSNAMELEN];

	/* make a copy to ensure null terminated */
	strncpy(buffer, name, len);
	buffer[len] = 0;
	np = buffer;

	if (len < 1)
		error("incoming file name is null", "");
	if (len > 31) {
		/* too long, so just take first 20 and last 11 */
		s = np + 20;
		t = np + len - 11;
		while (*s++ = *t++)
			;
		len = 31;
	}

	if (translate) {
		/* pre-decrement to match pre-increment within loop */
		np--;
		while (c = *++np)

         /*

          * Inside Macintosh, I-246, defines the printable characters
          * for the Macintosh as 0x20 thru 0xD8 less 0x7F.  Yet, the
          * apple character is above this range, at 0xF0, and the diamond
          * character is below this range, at 0x13.  And Adobe Garamond
          * has lots of characgters above 0xD8.
          * 
          * On the other hand, we only translate when processing UNIX
          * file names which are usually ASCII, and ASCII printables are
          * ' ' <= valid <= '~'.
          * 
          * But we want to avoid zapping any characters with the high
          * order bit set, so 8 bit character users are not zinged.  I've
          * never used a SONY-NeWS box, but this one's for you.  But how
          * do we know if/that the UNIX and Macintosh extended characters
          * are the same?
          * 
          * So what to do, what to do?
          * 
          * Let's look at it this way: if the UNIX file name has extended
          * characters in it, they got there for a reason, hopefully on
          * purpose, and we'll not gratuitously modify them.
          * 
          * And it looks like the 6.1.5 Finder running with the 6.0.5
          * System will translate both control characters and colon into
          * a dash, but leave the others alone, so so will we.
          * 
          * I don't know if MacOS, as opposed to the Finder, behaves
          * differently, and it's too late tonight to find out.  Maybe
          * some other time.

	  */

          if (c < ' ' || c == ':' || c == '\177')
				 *np = '-';
	}

	/* copy the resulting string back in place */
	strncpy(name, buffer, len);

	return len;
}

/*
 * Unix time (GMT since 1-1-1970)
 * Mac time (local since 1-1-1904)
 */
#define MACTIMEDIFF 0x7c25b080	/* Mac time of 00:00:00 GMT, Jan 1, 1970 */

/* Convert Unix time to Mac time */
ulong
unix2mac(xtime)
	ulong xtime;
{
#ifdef TIMEVAL
	struct timeval t;
	struct timezone tz;

	gettimeofday(&t, &tz);
	return long2mac(xtime + MACTIMEDIFF
	   - 60 * (tz.tz_minuteswest - 60 * tz.tz_dsttime));
#else
	struct timeb tp;

	ftime(&tp);
	return long2mac(xtime + MACTIMEDIFF
	    - 60 * (tp.timezone - 60 * tp.dstflag));
#endif
}

/* Convert Mac time to Unix time */
ulong
mac2unix(xtime)
	ulong xtime;
{
#ifdef TIMEVAL
	struct timeval t;
	struct timezone tz;

	gettimeofday(&t, &tz);
	return (mac2long(xtime) - MACTIMEDIFF
	   + 60 * (tz.tz_minuteswest - 60 * tz.tz_dsttime));
#else
	struct timeb tp;

	ftime(&tp);
	return (mac2long(xtime) - MACTIMEDIFF
	    + 60 * (tp.timezone - 60 * tp.dstflag));
#endif
}

/*
 * computes the appropriate output files
 * and the appropriate optional suffixes,
 * all depending on the processing "mode"
 */

mode_to_fname_suffix(
	mode, base_fname, data_fname, data_suffix, rsrc_fname, rsrc_suffix
)
	int mode;
	char *base_fname, *data_fname, **data_suffix, *rsrc_fname, **rsrc_suffix;
{

	/* clear names to indicate nothing selected yet */
	*data_fname = *rsrc_fname = 0;

	switch (mode) {
	case TEXT:
		strcpy(data_fname, base_fname);
		*data_suffix = ".text";
		break;
	case DATA:
		strcpy(data_fname, base_fname);
		*data_suffix = ".data";
		break;
	case RSRC:
		strcpy(rsrc_fname, base_fname);
		*rsrc_suffix = ".rsrc";
		break;
	case BOTH:
		strcpy(data_fname, base_fname);
		strcat(data_fname, ".data");
		*data_suffix = "";
		strcpy(rsrc_fname, base_fname);
		strcat(rsrc_fname, ".rsrc");
		*rsrc_suffix = "";
		break;
	default:
		error("Internal error: unexpected mode", "");
		break;
	}
}

/*
	This procedure basically copies the input file(s) to the output
	MacBinary file; in TEXT (translate_eol) mode it changes LF's to
	CR's, and in any mode it forges a Mac info header.  Author type
	comes from the MAC_EDITOR environment variable if it is defined.
 */

un_other(mode)
	int mode;
{
	char data_fname[SYSNAMELEN], rsrc_fname[SYSNAMELEN], binfname[SYSNAMELEN];
	FILE *data_file, *rsrc_file, *binfile;
	char *base_fname, *data_suffix, *rsrc_suffix;
	struct stat data_stbuf, rsrc_stbuf;
	ulong dlen, rlen, mtim, ctim;

	info_header info;
	register ulong b;				/* not character, must hold EOF diagnostic */
	register ulong nchars;
	int extra_chars;
	short crc, calc_mb_crc();
	long len;

	while (hqxnames_left[0][0] != '-') {

		if (strlen(*hqxnames_left) >= SYSNAMELEN)
			error("Error: specified base file name is too long", "");
		base_fname = *hqxnames_left++;

		/* set up file names */
		mode_to_fname_suffix(mode, base_fname,
			data_fname, &data_suffix, rsrc_fname, &rsrc_suffix);

		/* process the data file, if requested */
		dlen = 0;
		if (*data_fname) {
			data_file = mopen(data_fname, data_suffix, "r");
			if (fstat(fileno(data_file), &data_stbuf))
				error("Cannot stat %s", data_fname);
			mtim = unix2mac((ulong)data_stbuf.st_mtime);
			ctim = unix2mac((ulong)data_stbuf.st_ctime);
			dlen = long2mac(data_stbuf.st_size);
		}

		/* process the rsrc file, if requested */
		rlen = 0;
		if (*rsrc_fname) {
			rsrc_file = mopen(rsrc_fname, rsrc_suffix, "r");
			if (fstat(fileno(rsrc_file), &rsrc_stbuf))
				error("Cannot stat %s", rsrc_fname);
			mtim = unix2mac((ulong)rsrc_stbuf.st_mtime);
			ctim = unix2mac((ulong)rsrc_stbuf.st_ctime);
			rlen = long2mac(rsrc_stbuf.st_size);
		}

		/* stuff header data into the info header */

		bzero((char*)&info, sizeof(info_header));

		info.nlen = strlen(base_fname);
		info.nlen = (info.nlen > NAMELEN) ? NAMELEN : info.nlen;
		strncpy((char*)info.name, base_fname, (int)info.nlen);

		/* now make sure the resulting name is valid */
		info.nlen = macify((char*)info.name, (int)info.nlen, 1);

		info.uploadvers = '\201';
		info.readvers = '\201';

		bcopy((char*)&mtim, (char*)info.mtim, 4);
		bcopy((char*)&ctim, (char*)info.ctim, 4);

		bcopy((char*)&dlen, (char*)info.dlen, 4);
		bcopy((char*)&rlen, (char*)info.rlen, 4);

		switch (mode) {
		case TEXT:
			bcopy(mac_type ? mac_type : "TEXT", (char*)info.type, 4);
			bcopy(mac_auth ? mac_auth : "MACA", (char*)info.auth, 4);
			break;
		case DATA:
		case RSRC:
		case BOTH:
			bcopy(mac_type ? mac_type : "????", (char*)info.type, 4);
			bcopy(mac_auth ? mac_auth : "????", (char*)info.auth, 4);
			break;
		default:
			error("Internal error: unexpected mode", "");
			break;
		}

		/* calculate CRC */
		crc = calc_mb_crc((unsigned char*)&info, 124L, 0);
		info.crc[0] = (char) (crc >> 8);
		info.crc[1] = (char) crc;

		/* Create the .bin file and write the info to it */

		len = strlen(dir) + strlen(base_fname) + strlen(ext) + 1;
		if (len >= sizeof(binfname))
			error("Error: generated binfname would be too long", "");
		/*
		 * base_fname does not need to be unixified --
		 * was valid coming in
		 */
		sprintf(binfname, "%s/%s%s", dir, base_fname, ext);
		binfile = mopen(binfname, "", "w");

		converting(info.name, (int)info.nlen, info.type, info.auth);
		print_bin_hdr("Creating", &info);
		if (1 != fwrite((char*)&info, sizeof(info), 1, binfile))
			error("fwrite failed on binfile", "");

		/* pump out the data portion */
		if (*data_fname) {
			nchars = data_stbuf.st_size;
			extra_chars = 127 - (nchars + 127) % 128;

			if (translate_eol)
				while (nchars--) {
					(b = getc(data_file)) == EOF &&
						error("Error: getc failed on data_file", "");
					if (b == LF)
						b = CR;
					putc((char)b, binfile) == EOF &&
						error("Error: putc failed on binfile", "");
				}

			else
				while (nchars--) {
					(b = getc(data_file)) == EOF &&
						error("Error: getc failed on data_file", "");
					putc((char)b, binfile) == EOF &&
						error("Error: putc failed on binfile", "");
				}

			while (extra_chars--) {
				putc(0, binfile) == EOF &&
					error("Error: putc failed on binfile", "");
			}

			mclose(&data_file, "txtfile");
		}

		/* pump out the rsrc portion */
		if (*rsrc_fname) {
			nchars = rsrc_stbuf.st_size;
			extra_chars = 127 - (nchars + 127) % 128;

			while (nchars--) {
				(b = getc(rsrc_file)) == EOF &&
					error("Error: getc failed on rsrc_file", "");
				putc((char)b, binfile) == EOF &&
					error("Error: putc failed on binfile", "");
			}

			while (extra_chars--) {
				putc(0, binfile) == EOF &&
					error("Error: putc failed on binfile", "");
			}

			mclose(&rsrc_file, "txtfile");
		}

		mclose(&binfile, "binfile");
	}
}

/*
	This procedure basically copies the MacBinary input file to the
	output file(s); in TEXT (translate_eol) mode it changes CR's to
	LF's, and in any mode it skikps over the Mac info header.
 */

re_other(mode)
	int mode;
{
	char base_fname[SYSNAMELEN];
	char data_fname[SYSNAMELEN], rsrc_fname[SYSNAMELEN], binfname[SYSNAMELEN];
	FILE *data_file, *rsrc_file, *binfile;
	char *data_suffix, *rsrc_suffix;

	info_header info;
	register ulong b;
	register ulong nchars;
	ulong temp;
	int extra_chars;
	long len;

	while (hqxnames_left[0][0] != '-') {

		/* suck in the MacBinary header */
		if (strlen(*hqxnames_left) >= sizeof(binfname))
			error("Error: specified binfname is too long", "");
		strcpy(binfname, *hqxnames_left++);
		binfile = mopen(binfname, ext, "r");
		if (1 != fread((char*)&info, sizeof(info), 1, binfile))
			error("fread failed on binfile", "");

		/* figure out the target base file name */
		if (info.nlen >= NAMELEN)
			error("Error: corrupt BinHex data format", "");
		strncpy(base_fname, (char*)info.name, (int)info.nlen);
		base_fname[info.nlen] = '\0';
		converting(info.name, (int)info.nlen, info.type, info.auth);
		print_bin_hdr("Reading", &info);

		/* ensure the output files have no bogus UNIX characters */
		unixify(base_fname);
		mode_to_fname_suffix(mode, base_fname,
			data_fname, &data_suffix, rsrc_fname, &rsrc_suffix);

		/* always use suffix on write */

		if (*data_fname) {
			len = strlen(data_fname) + strlen(data_suffix);
			if (len >= sizeof(data_fname))
				error("Error: generated data_fname would be too long", "");
			strcat(data_fname, data_suffix);
			data_file = mopen(data_fname, "", "w");
		}

		if (*rsrc_fname) {
			len = strlen(rsrc_fname) + strlen(rsrc_suffix);
			if (len >= sizeof(rsrc_fname))
				error("Error: generated rsrc_fname would be too long", "");
			strcat(rsrc_fname, rsrc_suffix);
			rsrc_file = mopen(rsrc_fname, "", "w");
		}

		/* process the data fork */
		bcopy((char*)info.dlen, (char *) &temp, 4);
		nchars = temp;
		extra_chars = 127 - (nchars + 127) % 128;

		if (*data_fname) {

			if (translate_eol)
				while (nchars--) {
					(b = getc(binfile)) == EOF &&
						error("Error: getc failed on binfile", "");
					if (b == CR)
						b = LF;
					putc((char)b, data_file) == EOF &&
						error("Error: putc failed on data_file", "");
				}

			else
				while (nchars--) {
					(b = getc(binfile)) == EOF &&
						error("Error: getc failed on binfile", "");
					putc((char)b, data_file) == EOF &&
						error("Error: putc failed on data_file", "");
				}
		
			mclose(&data_file, data_fname);

		} else {

			/* skip the actual data */
			while (nchars--) {
				getc(binfile) == EOF &&
					error("Error: getc failed on binfile", "");
			}
		}

		/* eat the padding to 128 byte boundary */
		while (extra_chars--) {
			getc(binfile) == EOF &&
				error("Error: getc failed on binfile", "");
		}

		/* process the rsrc fork */

		bcopy((char*)info.rlen, (char *) &temp, 4);
		nchars = temp;

		if (*rsrc_fname) {
			while (nchars--) {
				(b = getc(binfile)) == EOF &&
					error("Error: getc failed on binfile", "");
				putc((char)b, rsrc_file) == EOF &&
					error("Error: putc failed on rsrc_file", "");
			}
			mclose(&rsrc_file, rsrc_fname);
		}

		mclose(&binfile, "binfile");
	}
}

usage()
{
	fprintf(stderr, Usage, cmdname, VERSION/100.);
	exit(1);
	/*NOTREACHED*/
}


/* My FileIO routines, to enable clean implementation of info_only */
/* If info_only and open for write, inform user and use devnull instead */
/* If pipe_out and open for write, inform user and use stdout instead */
/* If trying to close devnull or stdout, don't */

FILE *
mopen(name, exten, type)
	char *name;
	char *exten;
	char *type;
{
	FILE *result;

	switch (*type) {
	case 'r':
		result = fopen(name, type);
		if (result == NULL && *exten) {
			/* see if adding the extension would help */
			int len_root = strlen(name);
			int len_ext = strlen(exten);
			char *dotspot = name + len_root - len_ext;
			if (strcmp(exten, dotspot)) {
				if (len_root + len_ext >= SYSNAMELEN)
					error("Error: generated file name would be too long", "");
				strcat(name, exten);
				result = fopen(name, type);
			}
		}
		if (result == NULL)
			error("Cannot open %s for read", name);
		else {
			fprintf(verbose, "\n%-15s%-27s\n",
				"Input file", name);
			fflush(verbose);
		}
		break;

	case 'w':
		if (info_only) {
			fprintf(verbose, " %-14s%-27s -I (info only) specified\n",
				" No output to", name);
			fflush(verbose);
			result = devnull;
		} else if (pipe_out) {
			fprintf(verbose, " %-14s%-27s -P (pipe to stdout) specified\n",
				" stdout, vice", name);
			fflush(verbose);
			result = stdout;
		} else {
			result = fopen(name, type);
			if (result == NULL)
				error("Cannot open %s for write", name);
			else {
				fprintf(verbose, "%-15s%-27s\n",
					"Output file", name);
				fflush(verbose);
			}
		}
		break;

	default:
		fprintf(stderr, "%s: internal error in mopen -- exiting\n", cmdname);
		exit(2);

	}

	return result;
}

mclose(stream_p, name)
	FILE **stream_p;
	char *name;
{
	if (*stream_p && *stream_p != devnull && *stream_p != stdout) {
		if (fclose(*stream_p) == EOF)
			error("Error closing %s", name);
	}

	*stream_p = (FILE *)0;
}

converting(name, len, type, auth)
	byte *name;
	int len;
	byte *type;
	byte *auth;
{
	char buffer[SYSNAMELEN];

	/* make a copy to ensure null terminated */
	strncpy(buffer, (char*)name, len);
	buffer[len] = 0;

	fprintf(convert,
	    "%-15s%-30s type = \"%4.4s\", author = \"%4.4s\"\n",
 	    info_only ? "Inspecting" : "Converting",
		 buffer, type, auth);
	fflush(convert);
}

print_bin_hdr(msg, hdr)
	char *msg;
	info_header *hdr;
{
	ulong otime, xtime;
	long	dlen, rlen;
	bcopy((char*)hdr->ctim, (char*)&otime, 4);
	DEBUG && fprintf(debug,
		"DEBUG: verifying mac2unix/unix2mac: 0x%8lx, 0x%8lx\n",
		otime, unix2mac(mac2unix(otime)));

	bcopy((char*)hdr->dlen, (char*)&dlen, 4);
	bcopy((char*)hdr->rlen, (char*)&rlen, 4);

	DEBUG && fprintf(debug, "\
%s\n\
\tName     %.*s\n\
\tType     %.4s\n\
\tCreator  %.4s\n\
\tDataLen  %ld\n\
\tRsrcLen  %ld\n\
",
		msg,
		hdr->nlen, hdr->name,
		hdr->type,
		hdr->auth,
		mac2long(dlen),
		mac2long(rlen),
		0);

	bcopy((char*)hdr->ctim, (char*)&otime, 4);
	xtime = mac2unix(otime);
	DEBUG && fprintf(debug, "raw ctim: 0x%8lx, mac2unix: 0x%8lx\n",
		otime, xtime);
	DEBUG && fflush(debug);

	fprintf(verbose, "\tCreated  %s", ctime((time_t *)&xtime));
	fflush(verbose);

	bcopy((char*)hdr->mtim, (char*)&otime, 4);
	xtime = mac2unix(otime);
	DEBUG && fprintf(debug, "raw mtim: 0x%8lx, mac2unix: 0x%8lx\n",
		otime, xtime);
	DEBUG && fflush(debug);
	fprintf(verbose, "\tModified %s", ctime((time_t *)&xtime));
	fflush(verbose);

}
