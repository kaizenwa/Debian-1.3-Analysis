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
#ifndef	_NameTable_hh
#define	_NameTable_hh

#include <assert.h>
#include <iostream.h>

#include "datahashtable.hh"

//@ ----------------------------------------------------------------------------

//@ManMemo:	maps names to names
/*@Doc:
    Class #NameTable# implements a map from names to names. It allows to store
    or remove names (i.e. #char*#), but does not provide means for manipulating
    stored names.

    All names (i.e. the actual #char# strings) in a #NameTable# are stored in one
    continuous memory block of size #memMax()#. At one time #memSize()# bytes of
    it are used for actually saving names; the remaining memory is free to hold
    additional names. #memRemax()# can be used to reset #memMax()# but not lower
    than to #memSize()#. Method #memPack()# performs a garbage collection to
    gain free memory resulting from removed names.
 */
class NameTable
{
public:
    class Name
    {
    public:
	const char *name;

	friend int operator==(const Name& n1, const Name& n2)
		    { return (strcmp (n1.name, n2.name) == 0); }

	friend ostream& operator<<(ostream& out, const Name& n)
		    { return out << n.name; }

	friend int hashFunction (const Name*);

	int isConsistent () const	{ return (name != 0); }
	Name (const char* str) 		{ name = str; }
    } ;

    DataHashTable<Name,int>	table ;	// hashtable for names
    McDArray<char>		names ;	// memory where to store names

public:
	//@Man:	\ 
	//@ManMemo:	return nr. of names in #NameTable#
    int		num() const				{ return table.num() ; }
	//@ManMemo:	return maximum nr. of names that fit into #NameTable#
    int		max() const				{ return table.max() ; }

	///	does #NameTable# have name #str#?
    int		has( const char* str ) const
		{
			const Name nam(str) ;
			return table.has(nam) ;
		}

	///	return number for #name#
    int		operator[]( const char* str ) const
		{
			const Name nam(str) ;
			return table[nam] ;
		}

    //@Man:	Iteration
    //@{
	///
    const char*	first() const
    		{
		    if( table.first() )
			return table.current()->name ;
		    else
			return 0 ;
		}
	///
    const char*	last() const
    		{
		    if( table.last() )
			return table.current()->name ;
		    else
			return 0 ;
		}
	///
    const char*	next() const
    		{
		    if( table.next() )
			return table.current()->name ;
		    else
			return 0 ;
		}
	///
    const char*	current() const
    		{
		    if( table.current() )
			return table.current()->name ;
		    else
			return 0 ;
		}
	///
    const char*	prev() const
    		{
		    if( table.prev() )
			return table.current()->name ;
		    else
			return 0 ;
		}
    //@}

	/// 
    void	add( int num, const char* name ) ;
	///
    void	clear() ;				

	///
    friend ostream& operator<<(ostream& out, const NameTable& nt) ;
	///
    friend istream& operator>>(istream& out, NameTable& nt) ;

	///
    int		isConsistent() const ;			
	///
    		NameTable() ;
} ;

#endif	
