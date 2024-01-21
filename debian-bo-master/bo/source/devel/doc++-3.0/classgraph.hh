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


/*@ -----------------------------------------------------------------------------

    ClassGraph

    Identification:
    $Id: classgraph.hh,v 3.0 1997/02/04 17:49:08 bzfzoeck Exp $

    Program history:
    $Log: classgraph.hh,v $
    Revision 3.0  1997/02/04 17:49:08  bzfzoeck
    released Version 3.0

    Revision 1.6  1996/12/16 10:44:56  bzfzoeck
    now really

// Revision 1.4  1996/10/11  21:03:36  bzfzoeck
// .
//
// Revision 1.2  1996/06/25  08:39:30  bzfzoeck
// .
//
// Revision 1.1.1.1  1996/06/03  17:02:01  bzfzoeck
// Initial doc++ repository
//

    ----------------------------------------------------------------------------
 */
#ifndef DEFclassgraph	
#define DEFclassgraph


/* Import required system include files
 */
#include <assert.h>
#include <stdlib.h>


/*  ... and classes
 */
#include "McString.h"
#include "doc.h"


//@ ----------------------------------------------------------------------------
/// Class graph structor
struct ClassGraph
{
    	/// pointer to first line of this class graph
    ClassGraph*	firstLine ;

    	/// pointer to next line in this class graph
    ClassGraph*	nextLine ;

    	/// number of spaces to indent
    int		indent ;

    	/// string coding arrows to be typeset before class box
    McString	before ;

    	/// string coding arrows to be typeset after class box
    McString	after ;
    	/** The following characters are used:
	    \begin{description}
	    \item{<blank>}	a Space
	    \item{l}		top left arrow
	    \item{L}		in between left arrow
	    \item{^}		first bottom right line
	    \item{|}		first in between right line
	    \item{r}		bottom right line
	    \item{R}		in between right line
	    \item{.}		private inheritance
	    \item{-}		protected inheritance
	    \item{_}		public inheritance
	    \end{description}
	 */

    	/// pointer to entry defining class to be set into box
    Entry*	entry ;

    	/// if no entry to this box is known, take this name instead
    McString	name ;

	/// 
    void	addBases() ;

	/// 
    void	addAllChilds() ;

	/// 
    void	addDirectChilds() ;

	/// constructor for entry
    ClassGraph( Entry* cls, int ind )
	: firstLine	( this )
	, nextLine	( 0 )
	, indent	( ind )
	, entry		( cls )
    { assert( cls->isClass() ) ; }

	/// constructor for name
    ClassGraph( const char* nm, int ind )
	: firstLine	( this )
	, nextLine	( 0 )
	, indent	( ind )
	, entry		( 0 )
	, name		( nm )
    { }

private:
    void	addChilds( const char*, int ) ;
} ;

#endif	
