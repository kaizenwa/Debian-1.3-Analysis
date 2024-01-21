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


/*@ ---------------------------------------------------------------------------
    ---------------------------------------------------------------------------
 */
#ifndef DOC_H
#define DOC_H

#include <stdio.h>

#include "McDArray.h"
#include "McString.h"

#ifdef WIN32
#define PATH_DELIMITER '\\'
#else
#define PATH_DELIMITER '/'
#define stricmp strcasecmp
#endif
extern FILE*	out ;

extern int	setContext (int) ;
extern int	verb ;
extern int	noLatex ;

extern int	java   ;
extern int	alwaysPrintDocSection   ;
extern int	noGifs ;
extern int	trivialGraphs ;
extern int	upArrows ;
extern int	javaGraphs ;
extern int	withTables ;
extern int	withBorders ;
extern int	withPrivate ;
extern int	shortFilenames;
extern int	showInherited;
extern char*    ownBanner;
extern int	onlyDocs ;
extern int	sortEntries ;
extern int	HTMLsyntax ;

enum
{
    EMPTY_SEC=1,
    MANUAL_SEC=2,
    VARIABLE_SEC=4,
    FUNCTION_SEC=8,
    MACRO_SEC=16,
    CLASS_SEC=32,
    UNION_SEC=64,
    INTERFACE_SEC=128,
    PACKAGE_SEC=256,
    TYPEDEF_SEC=16,			// to be changed ...
    ALL_SEC=0x0fff
} ;

enum PROTECTION
{
    PUBL,
    PROT,
    PRIV
} ;

enum LANGUAGE
{
    LANG_UNKNOWN,
    LANG_JAVA,
    LANG_CXX
} ;

struct Entry
{
    /// Is this a java or C++ entry.
    char        language;
    /// Which kind of entry (class,  function, ...)
    unsigned short section ;
    /// Protected, public etc.
    char	protection ;
    char	done;
    /// will this entry get its own html page ?
    char        ownPage;
    Entry	*parent ;
    Entry	*next ;
    Entry	*sub ;
    McString	type ;
    McString	name ;
    McString	args ;
    McString	memo ;
    McString	doc ;
    McString	program ;
    McString	author ;
    McString	version ;
    McDArray<McString*>	see ;
    McDArray<McString*>	param ;
    McDArray<McString*>	exception ;
    McString	retrn ;
    
    McDArray<Entry*>	sublist ;
    //@ McDArray<Entry*>	childs ;
    McDArray<Entry*>	pubChilds ;
    McDArray<Entry*>	proChilds ;
    McDArray<Entry*>	priChilds ;
    McDArray<Entry*>	baseclasses ;		// to be removed !!!
    McDArray<Entry*>	pubBaseclasses ;
    McDArray<Entry*>	proBaseclasses ;
    McDArray<Entry*>	priBaseclasses ;
    
    // base classes that are not documented:
    McDArray<McString*>	otherPubBaseclasses ;
    McDArray<McString*>	otherProBaseclasses ;
    McDArray<McString*>	otherPriBaseclasses ;
    McDArray<McString*>	implements ;
    McDArray<McString*>	extends ;
    McDArray<McString*>	import ;
    Entry*		currentParent ;		// for temporary use in TeX output
    
    /** If this member overrides somthing from a baseclass, this points to
      the overridden member.*/
    
    Entry *override;
    McString	fileName;
    int		startLine ;
    
    // These are the html ified  strings
    char	*htype ;
    char	*hname ;
    char	*hargs ;
    char	*hmemo ;
    char	*hsee ;
    char	*hdoc ;

    McString	file ;
    int		line ;
    int		tex ;
    int		number ;
    int		subnumber ;

    void	dumpNumber( FILE* ) ;
    void	dumpNumber( McString& ) ;

    Entry*	findSub(const char* ) ;
    Entry*	findSub(const char *name,int von,int bis) ;

    /// Is this a class or an interface ?
    int isClass() {
	return(section==CLASS_SEC || section==INTERFACE_SEC);
    }
    ///
    /** Adds #name# to the list of baseclasses and to the list of
      {pub,pro,pri}BaseClasses and to the list of #name#'s subclasses.*/ 
    void	addBaseClass(const char *name,PROTECTION state);
    void	addBaseClass(Entry *name,PROTECTION state);
    void	findBases() ;
    void        removeSub(Entry*);
    void	makeSubList() ;
    ///
    /** This funciton extracts the baseclasses and creates
      the pointer arrays pubBaseClasses etc. */
    void        makeRefs();
    ///
    void	addSubEntry (Entry* e) ;
    ///
    Entry*	newSubEntry () ;

    ///
    /** This function creates a fully qualified name with respect to the 
      current import and package statements*/
    void makeFullName(McString &name);
    
    void getPackage(McString &);
    Entry() ;
} ;

extern Entry*	root ;
extern void	printYYDOC( Entry*, const char* str, int escPrcnt=1 ) ;

/* parsing comands */
extern void	listing    (char*) ;
extern void	usermanHTML(char*, Entry*) ;
extern void	usermanTeX (char*, Entry*) ;
extern void	parseCxx   (Entry* ) ;
extern void	parseCpp   (Entry* ) ;
extern void	parseJava  (Entry* ) ;
extern void	parseDoc   (Entry* ) ;

extern void	getRefNames( McDArray<McString*>, const char* ) ;
extern Entry*	getRefEntry( McString& name, Entry* rot ) ;
extern Entry*	getRefEntry( const char* name, Entry* rot ) ;
extern void	makeSubLists( Entry* rt ) ;
extern void	entry2link( McString& u, Entry* ref,const char *linkname=0);

extern void	makeGifs(const char*) ;
extern void	readfile( McString* in, const char* file, int startLine,
			  const char *directory=0, int scanSubDirs=0) ;

#define	HAS_BASES(entry)			\
	(  entry->otherPubBaseclasses.size()	\
	|| entry->otherProBaseclasses.size()	\
	|| entry->pubBaseclasses.size()		\
	|| entry->proBaseclasses.size() )

#define	MAKE_DOC(entry)				\
	(  entry->sub != 0			\
	|| entry->see.size() > 0		\
	|| entry->author.length() > 0		\
	|| entry->author.length() > 0		\
	|| entry->version.length() > 0		\
	|| entry->param.size() > 0		\
	|| entry->retrn.length() > 0		\
	|| entry->exception.size() > 0	\
	|| entry->doc.length() > 0		\
	|| entry->proChilds.size() > 0		\
	|| entry->pubChilds.size() > 0		\
	|| (entry->pubChilds.size() > 0	 &&  withPrivate)	\
	|| entry->parent->parent == 0		\
	|| HAS_BASES(entry) )

#endif	/* #DOC_H# */
