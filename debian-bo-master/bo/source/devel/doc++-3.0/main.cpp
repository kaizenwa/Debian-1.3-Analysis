/*************************************************************************

    DOC++, a C++ (and C) documentation system for LaTeX and HTML

	    Copyright (C) 1996  Roland Wunderling,
				Malte Zoeckler


    DOC++ is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation. This program
    is distributed WITHOUT ANY WARRANTY; without even the implied
    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    See the GNU General Public License for more details.

    If you intend to use DOC++ commercially on a regular basis you
    must contact the authors for an appropriate donation.

 *************************************************************************/


/*@ ----------------------------------------------------------------------------

   Main for class |Doc|

   Identification:
   $Id: main.cpp,v 3.1 1997/02/12 10:40:37 bzfzoeck Exp $

   Program history:
   $Log: main.cpp,v $
   Revision 3.1  1997/02/12 10:40:37  bzfzoeck
   Version 3.01. @Param und @return funktionieren jetxt auch
   fuer globale Fkt. Added -m switch.

   Revision 3.0  1997/02/04 17:49:57  bzfzoeck
   released Version 3.0

   Revision 1.7  1997/02/04 16:52:40  bzfzoeck
   *** empty log message ***

   Revision 1.6  1997/02/04 15:17:45  bzfzoeck
   Tech@ Release (3.0).

   Revision 1.4  1997/01/15 11:59:50  bzfwunde
   - improved LaTeX environment for gifs using doc2tex parser.

// Revision 1.3  1996/12/29  13:37:31  bzfzoeck
// bug fixes.
//
   Revision 1.2  1996/12/16 10:45:11  bzfzoeck
   now really

   Revision 1.16  1996/11/20 16:13:22  bzfwunde
   Bug fixes mainly in java parser.  Did not parse comments correctly.

   Revision 1.14  1996/11/14 14:10:57  bzfwunde
   Bug fixes: SoPlex documentation now works!

// Revision 1.15  1996/11/18  08:49:24  bzfzoeck
// .
//
// Revision 1.14  1996/11/14  14:10:57  bzfwunde
// Bug fixes: SoPlex documentation now works!
//
   Revision 1.13  1996/11/13 14:17:23  bzfwunde
   JavaDoc style comments for C++, C and Java
   New C/C++ parser using only flex

// Revision 1.12  1996/10/15  17:11:37  bzfwunde
// lots of improvements ... at least for the LaTeX output and the
// java and C++ parser.
//
// Revision 1.11  1996/10/14  15:52:46  bzfwunde
// - fixed Java-parser bug
// - added version etc output for TeX
//
// Revision 1.10  1996/10/11  21:03:42  bzfzoeck
// .
//
// Revision 1.9  1996/10/07  11:46:41  bzfzoeck
// Not yet perfect, but java inheritence is kind of implemented.
//
// Revision 1.8  1996/10/07  09:44:45  bzfzoeck
// These are generated automatically.
//
// Revision 1.7  1996/10/07  09:40:46  bzfwunde
// java parser included :-))))
//
// Revision 1.6  1996/09/12  11:29:13  bzfwunde
// completely rewritten parser for C++.
// This one is way easier to understand and contains fewer bugs.
//
// Revision 1.2  1996/06/09  10:15:09  bzfzoeck
// Major changes: doc++ no longer uses dataarray but McDArray instead.
// //@Include: now can handle directories correctly.
// //@Include: now handles some very simple(!) regular expressions,
// 	namely anything that looks like '*.h' or '*.hh' or '*dxx' ...
//
// Revision 1.1.1.1  1996/06/03  17:01:59  bzfzoeck
// Initial doc++ repository
//

   $Date: 1997/02/12 10:40:37 $
    ----------------------------------------------------------------------------
*/

/*@Title:	Documentation	*/
/*@Name:	main()	*/
/*@Memo:	a commentbased documentation system for C++	*/
/*@Doc:
    This is a cool documentation system based on @-Comands within C or C++
    comments.
 */

#include <assert.h>
#include <stdio.h>
#include <fstream.h>

#include "doc.h"
#include "McString.h"
#include "nametable.hh"

#define	GIF_FILE_NAME	"/gifs.db"


/*
 *	exports
 */
FILE	*out ;
int	verb ;
// extern int yy_flex_debug;
extern void doHTML(char *,Entry*);

int	trivialGraphs  = 0 ;
int	showAllMembersDoc  = 0 ;
int	upArrows       = 0 ;
int	withPrivate    = 0 ;
int	showFilenames  = 0 ;
int	doTeX          = 0 ;
int	HTMLsyntax     = 0 ;
int	java           = 0 ;
int	noLatex        = 0 ;
int     withTables     = 0 ;
int     withBorders    = 0 ;
int     javaGraphs     = 1 ;
int     shortFilenames = 0 ;
char*   ownBanner      = 0 ;
char*	texFile        = 0 ;
char*	texOption      = 0 ;
char*	texPackage     = 0 ;
int	noGifs         = 0 ;
int	forceGifs      = 0 ;
int	showInherited  = 1;
int	onlyDocs       = 1 ;	// include only documented entries
int	sortEntries    = 1;
int	alwaysPrintDocSection    = 1;

Entry		*root ;
NameTable	gifs ;
extern void mergeCCFunctions(Entry *);

static void	reNumber( Entry* tp )
{
    Entry* tmp = tp ;
    int	i = 1 ;
    for( tmp = tp->sub ; tmp ; tmp = tmp->next )
    {
	if( MAKE_DOC(tmp) )
	    tmp->subnumber = i++ ;
	reNumber( tmp ) ;
    }
    for( tmp = tp->sub ; tmp ; tmp = tmp->next )
    {
	if( !MAKE_DOC(tmp) )
	    tmp->subnumber = i++ ;
    }
}


main(int argc, char **argv)
{
    root = new Entry ;
    McString&	inputFile = root->program ;

    // test: #yy_flex_debug >= 0 ;# ... bla

    char	Directory[3] = "./" ;
    Directory[1]=PATH_DELIMITER;
    char	*directory  = Directory ;

    int		doListing = 0 ;
    int 	i;
    int		gifNum ;

    out  = stdout ;
    verb = 0 ;

    for(i = 1; i < argc; ++i)
    {
	if(argv[i][0] != '-')
	break ;
	switch(argv[i][1])
	{
	    case 'e':
		if( ++i >= argc )
		{
		    printf( "Ignoring option -ex\n" ) ;
		    --i ;
		}
		else
		{
		    if( argv[i-1][2] == 'f' )
			texFile = argv[i] ;
		    else if( argv[i-1][2] == 'o' )
			texOption = argv[i] ;
		    else if( argv[i-1][2] == 'p' )
			texPackage = argv[i] ;
		}
		break ;
	    case 'v':
		verb = 1 ;
		break ;
	    case 'p':
		withPrivate = 1 ;
		break ;
	    case 'u':
		upArrows = 1 ;
		break ;
	    case 'A':
		onlyDocs = 0 ;
		break ;
	    case 'j':
		javaGraphs = 0 ;
		break ;
	    case 'i':
		showInherited = 0 ;
		break ;
	    case 'G':
		forceGifs = 1 ;
		break ;
	    case 'H':
		HTMLsyntax = 1 ;
		break ;
	    case 'g':
		noGifs = 1 ;
		break ;
	    case 'a':
		withTables = 1 ;
		break ;
	    case 'b':
		withTables = 1 ;
		withBorders = 1 ;
		break ;
	    case 'k':
		trivialGraphs = 1 ;
		break ;
	    case 'm':
		alwaysPrintDocSection = 0 ;
		break ;
	    case 'r':
		shortFilenames = 1 ;
		break ;
	    case 'f':
		showFilenames = 1 ;
		break ;
	    case 'J':
		java = 1 ;
		break ;
	    case 't':
		doTeX = 1 ;
		break ;
	    case 's':
		doListing = 1 ;
		break ;
	    case 'l':
		noLatex = 1 ;
		break ;
	    case 'd':
		if( ++i >= argc )
		{
		    printf( "Ignoring option -d: no name specified\n" ) ;
		    --i ;
		}
		else
		    directory = argv[i] ;
		break ;
	    case 'B':
		if( ++i >= argc )
		{
		    printf( "Ignoring option -B: no name specified\n" ) ;
		    --i ;
		}
		else
		    ownBanner = argv[i] ;
		break ;
	    case 'o':
		if( ++i >= argc )
		{
		    printf( "Ignoring option -o: no filename specified\n" ) ;
		    --i ;
		}
		else
		{
		    FILE*	tmp ;
		    tmp = fopen( argv[i], "w" ) ;
		    if( tmp )
			out = tmp ;
		    else
			printf( "Cannot open output file %s\n", argv[i] ) ;
		}
		break ;
	    case 'h':
		printf( "\n") ;
		printf( "              Welcome to\n") ;
		printf( "\n") ;
		printf( "\n") ;
		printf( "       DDDDDD      OOOOO       CCCCC\n" ) ;
		printf( "       DDDDDDD    OO00000    CCCCCCC\n" ) ;
		printf( "       DD   DD ++ OO   O0 ++ CC     \n" ) ;
		printf( "       DD      ++         ++        \n" ) ;
		printf( "       DD   ++++++++   ++++++++     \n" ) ;
		printf( "       DD   ++++++++   ++++++++     \n" ) ;
		printf( "       DD      ++         ++        \n" ) ;
		printf( "       DD   DD ++ OO   O0 ++ CC     \n" ) ;
		printf( "       DDDDDDD    OOOOOO0    CCCCCCC\n" ) ;
		printf( "       DDDDDD      OOOOO       CCCCC\n" ) ;
		printf( "\n" ) ;
		printf( "                Version 3.01\n" ) ;
		printf( "\n" ) ;
		printf( "  (c) 1995,96 by Malte Zoeckler,\n" );
		printf( "                 Roland Wunderling,\n" );
		printf( "\n" ) ;
		printf( "USAGE:   %s [options] [filelist]\n", argv[0] ) ;
		printf( "\n" ) ;
		printf( "Options:\n" ) ;
		printf( "    -h            this message\n" ) ;
		printf( "    -J            emulate javadoc for Java classes\n" ) ;
		printf( "    -A            document All\n" ) ;
		printf( "    -p            include private members\n" ) ;
		printf( "    -v            verbose mode\n" ) ;
		printf( "    -t            generate TeX output\n" ) ;
		printf( "    -u            upwards arrows in class graph\n" ) ;
		printf( "    -k            keep trivial class graphs\n" ) ;
		printf( "    -H            use HTML syntax for documentation\n" ) ;
		printf( "\n" ) ;
		printf( "Additional options for HTML output:\n" ) ;
		printf( "    -d <name>     set name for the output directoy\n" ) ;
		printf( "    -f            show filenames in man-pages\n" ) ;
		printf( "    -g            do not generate gifs for equations etc.\n");
		printf( "    -G            force generation of gifs\n");
		printf( "    -B <file>     use <file> as banner on html pages\n");
		printf( "    -a            use tables instead of description lists\n");
		printf( "    -b            use tables with borders\n");
		printf( "    -r            use shoRt filenames (for DOS)\n");
		printf( "    -j            suppress java classgraphs\n");
		printf( "    -i            do not show inherited members\n");
		printf( "    -m            don't show all members in doc section\n");

		printf( "\n" ) ;
		printf( "Additional options for TeX output:\n" ) ;
		printf( "    -o <name>     set output file\n" ) ;
		printf( "    -l            do not generate LaTeX environment\n" ) ;
		printf( "    -eo <option>  setup LaTeX style option\n" ) ;
		printf( "    -ep <package> setup LaTeX to use package\n" ) ;
		printf( "    -ef <file>    read LaTeX environment from file\n" ) ;
		printf( "    -s            generate source code listing\n" ) ;
		printf( "\n" ) ;
		printf( "doc++ comes with ABSOLUTELY NO WARRANTY.\n");
		printf( "\n" ) ;
		printf( "This is free software, and you are welcome to redistribute it\n");
		printf( "under the terms and conditions of the GNU PUBLIC LICENSE.\n");
		printf( "\n") ;
		return 0 ;
	    default:
		printf( " ERROR: unknown option. Try -h\n" ) ;
	}
    }

    if( !noGifs  &&  !forceGifs  &&  !doTeX )
    {
	McString	gifDB( directory ) ;
	gifDB += GIF_FILE_NAME ;

	ifstream	gifFile( (const char*) gifDB ) ;
	if( gifFile ) ;
	{
	    gifFile >> gifs ;
	    gifNum = gifs.num() ;
	}
    }


    /*	Read input files into buffer
    */
    if( i >= argc )
    {
	printf("Please specify input file(s)\n") ;
	printf("type 'doc++ -h' for help\n") ;
	exit(-1) ;
    }
    printf("reading files ..." );
    fflush( stdout ) ;
    for( ; i < argc ; ++i )
	readfile( &inputFile, argv[i], 1 ) ;
    printf(" done, read %d bytes !\n",inputFile.length());

    printf("parsing ..." );
    fflush( stdout ) ;
    if( java )
	parseJava( root ) ;
    else
	parseCpp( root ) ;
    printf(" done!\n" ) ;

    if( root->sub  &&  root->sub->next == 0 )
    {
	root = root->sub ;
	delete root->parent ;
	root->parent = 0 ;
    }

    printf("sorting entries ...");
    fflush( stdout ) ;
    makeSubLists( root ) ;
    printf(" done\n");
    printf("resolving references ...");
    fflush( stdout ) ;
    mergeCCFunctions(root);
    reNumber( root ) ;
    root->makeRefs();
    printf(" done\n");


    /*	Create user manual
     */
    if( doTeX )
    {
	if( !doListing )
	    usermanTeX( inputFile, root ) ;
	else
	    listing( inputFile ) ;
    }
    else
    {
	usermanHTML( inputFile, root ) ;
	doHTML(directory,root);
    }

    if( (!noGifs || forceGifs)  &&  !doTeX )
    {
	McString	gifDB( directory ) ;
	gifDB += GIF_FILE_NAME ;
	cerr << "writing gif database file " << (const char*)gifDB << endl ;
	ofstream	gifFile( (const char*) gifDB ) ;
	gifFile << gifs ;
	makeGifs(directory ) ;
    }

    return 0 ;
}
