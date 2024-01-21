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

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <iostream.h>

#include "nametable.hh"
#include "McString.h"

//@ ----------------------------------------------------------------------------

int hashFunction( const NameTable::Name* n )
{
    unsigned int res = 0;
    const char* sptr = n->name;

    while (*sptr)
    {
	res *= 65 ;
	res += *sptr++ - int('0') ;
	res %= 0x0fffffff ;
    }
    return res ;
}

void	NameTable::add( int num, const char* name )
{
    int	n = strlen(name) + 1 ;
    int	i = names.size() ;

    char*	start = names ;
    names.append( n, name ) ;
    int		delta = start - (char*)names ;
    if( delta )
    {
	for( table.first() ; table.current() ; table.next() )
	    ((Name*)table.current())->name -= delta ;
    }

    Name	newName( &names[i] ) ;
    table.add( newName, num ) ;
}

void	NameTable::clear()
{
    table.clear() ;
    names.clear() ;
}

ostream& operator<<(ostream& out, const NameTable& nt)
{
    for( nt.first() ; nt.current() ; nt.next() )
    {
	out << nt[nt.current()] << ':' ;
	out << nt.current() << char(6) << endl ;
    }
    return out ;
}

istream& operator>>(istream& in, NameTable& nt)
{
    int		num ;
    char	c ;
    McString	string ;

    nt.clear() ;
    while( in )
    {
	in >> num ;
	in.get(c) ;
	if( c != ':' )
	    break ;
	string.clear() ;
	do {
	    in.get(c) ;
	    if( c == char(6) )
	    {
		nt.add( num, string ) ;
		break ;
	    }
	    string += c ;
	} while( in ) ;
    }

    return in ;
}

int	NameTable::isConsistent() const
{
    return	names.isConsistent() && table.isConsistent() ;
}

NameTable::NameTable() 
    : table( hashFunction )
    , names( 1000 )
{
}
