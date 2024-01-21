/*@ -----------------------------------------------------------------

 
 
                        This file is part of the class library
 
            SoPlex  --- the Sequential object-oriented simPlex
 
            (c) by      Roland Wunderling
                        Konarad Zuse Zentrum fuer Informationstechnik Berlin
                        Heilbronner Str. 10, 
                        D-10711 Berlin, Germany
 
        There is absolutely no right to redistribute this file without the
        explicite permission by the authour.
 
 
    +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 


    DataHashTable

    Identification:
    $Id: datahashtable.hh,v 3.0 1997/02/04 17:49:19 bzfzoeck Exp $

    Program history:
    $Log: datahashtable.hh,v $
    Revision 3.0  1997/02/04 17:49:19  bzfzoeck
    released Version 3.0

    Revision 1.4  1996/12/16 10:44:59  bzfzoeck
    now really

// Revision 1.2  1996/06/09  10:15:02  bzfzoeck
// Major changes: doc++ no longer uses dataarray but McDArray instead.
// //@Include: now can handle directories correctly.
// //@Include: now handles some very simple(!) regular expressions,
// 	namely anything that looks like '*.h' or '*.hh' or '*dxx' ...
//
// Revision 1.1.1.1  1996/06/03  17:02:01  bzfzoeck
// Initial doc++ repository
//
# Revision 2.5  1996/03/21  11:02:30  bzfwunde
# New Makefile
# Many preformance improvents
#
# Revision 2.4  1996/01/08  12:24:07  bzfwunde
# Moved to new non-GNU generic Makefile environment
#
# Revision 2.3  1995/11/21  16:21:46  bzfwunde
# introduced SUBDIR_INCLUDE
#
# Revision 2.2  1995/10/13  15:30:29  bzfwunde
# minor improvements
#
# Revision 2.1  1995/03/31  15:00:46  bzfwunde
# tested Version running with set packing
#
# Revision 1.9  1995/03/09  15:55:51  bzfwunde
# Tested version: Running even on CRAY :-)
#
# Revision 1.8  1995/03/03  19:13:04  bzfwunde
# complete rewrite
#

    -----------------------------------------------------------------
*/
#ifndef def_datahahstable
#define def_datahashtable


//@ ----------------------------------------------------------------------------
/*      \Section{Imports}
    Import required system include files
 */
#include <iostream.h>
#include <stdlib.h>
#include <assert.h>

#ifndef SUBDIR_INCLUDE
#include "McDArray.h"
#else	// #SUBDIR_INCLUDE#
#include "dataarray/McDArray.h"
#endif	// #SUBDIR_INCLUDE#


//@ ----------------------------------------------------------------------------
/*
    This should be a private subclass of #DataHashTable. However, since cfront
    is not able to compile this constrution, we had move the class to global
    scope.
 */
template <class HashItem, class Info>
struct DataHashTable_Element
{
    HashItem	item ;
    Info	info ;
    enum
    {
	FREE,		// element has never been used
	RELEASED,	// element had been used, but released
	USED 		// element is in use
    }	status ;
} ;


/*@ManDoc: 
    Class #DataHashTable# provides a generic hash table for \Ref{Data Objects},
    i.e. a map that maps arguments called #HashItem#s to values called #Info#s.
    #HashItem# and #Info# types are passed as #template# arguments. #HashItem#s
    must provide a comparision #operator==#.  Further both, the #HashItem# and
    #Info# must be data objects in the sense, that the assignment operator is
    equivalent to a #memcpy()# of the structure and no destructor is required.

    The construction of a #DataHashTable# requires a {\em hash function} that
    assigns every #HashItem# to an integer value.  Provided this, pairs of a
    #HashItem# and a #Info# can be added to the #DataHashTable#. No more
    than one #Info# to the same #HashItem# is possible at a time. The #Info#
    to a #HashItem# can be accessed through the subscript #operator[]# with
    #Info# as subscript.

    A #DataHashTable# can hold up to #max()# entries. This #max()# can be
    specified upon construction and may be reset with #reMax()# later on.
    Further, a value #hashSize# is required. This value must be #< max()# and must
    not have a common dominator with #max()#. If not specified explicitely, it
    is set automatically to a reasonable value. It may be reset with method
    #reHash()#. Note that both, #reMax()#ing and #reHash()#ing renders any
    reference to entries of a #DataHashTable# invalid. This also happens, if the
    #DataHashTable# is #reMax()#ed automatically, if more than #max()# entries
    are added to it.
 */
template <class HashItem, class Info>
class DataHashTable
{
private:
    /*
	The implementation relies on an array of #DataHashTable::Element#s, from
	now on referred to as elements. Upon construction, all elements are
	marked #FREE# in their member #status#. When an entry is added
	to the #DataHashTable#, the hash value is computed by calling #hashval#
	for its #HashItem#. If this array element is unused, it is
	taken right away. Otherwise, the array index is incremented by
	#hashsize# (modulo the element array #size()#) until an unused element
	is found.

	Removing elements is simply done by marking it as #RELEASED#. Hence,
	when searching for an element, the search loop may not stop, when a
	#RELEASED# element is encountered. However, such an element may be
	reused when adding a new element to the #DataHashTable#.

	Further, memory management with resizing of the element array is
	straight forward.
     */
    typedef DataHashTable_Element<HashItem,Info>	Element ;
    McDArray< DataHashTable_Element<HashItem,Info> >	element ;

    int		hashsize ;
    int		thenum ;			// current number of entries
    int		(*hashval) (const HashItem*) ;	// pointer to hash function
    double	factor ;			// memory increment factor
    int		theCurrent ;			// index for iterator.

    int	cFrontBug ;				// #sizeof(Info)#

    /*  Compute a good hashsize as the product of all prime numbers not dividors
	of size() that are <= the maximum dividor of size().
     */
    int		autoHashSize() const
    {
	int	i, j ;
	int	hashsze = 1 ;
	int	size    = element.size() ;
	McDArray<char>	prime(size) ;

	for( i = 2 ; i < size ; ++i )
	    prime[i] = 1 ;

	for( i = 2 ; i < size ; ++i )
	{
	    if( prime[i] )
	    {
		for( j = i ; j < size ; j += i )
		    prime[j] = 0 ;
		if( size % i != 0 )
		{
		    hashsze *= i ;
		    if( hashsze > size )
		    {
			hashsze /= i ;
			break ;
		    }
		}
	    }
	}

	return hashsze ;
    }

    /*  Return index in #element# to #h# or -1, if not present
     */
    int		index( const HashItem& h) const
		{
		    int	i, j ;
		    for
		    (
			i = j = (*hashval)(&h) % element.size() ;
			element[i].status != DataHashTable_Element<HashItem,Info>::FREE ;
		    )
		    {
			if( element[i].item == h )
			    return i ;
			i = (i+hashsize) % element.size() ;
			if( i == j )
			    break ;
		    }
		    return -1 ;
		}

public:
    //@Man:	Inquiry Methods
    //@{
	 //@ManMemo: number of elements that would fit
    int          max () const			{ return element.size() ; }

	 //@ManMemo: number of hashed elements
    int          num () const			{ return thenum ; }

	 //@ManMemo: return hash size, i.e.~the increment when searching for elemnts.
    int          hashSize () const		{ return hashsize ; }
    //@}


    //@Man:	Access Methods
    //@{
         //@ManMemo: Is item #h# is present in #DataHashTable#?
    int		has (const HashItem& h) const	{ return index(h) >= 0 ? 1 : 0 ; }

	 //@ManMemo: return pointer to #Info# to #h# or 0
    Info*	get (const HashItem& h)
		{
		    int	i = index(h) ;
		    return i >= 0 ? &element[i].info : 0 ;
		}

	 //@ManMemo: return pointer to #Info# to #h# or 0
    const Info*	get (const HashItem& h) const
		{
		    int	i = index(h) ;
		    return i >= 0 ? &element[i].info : 0 ;
		}
	 /*@Doc:
	     Returns pointer to #Info# component of #hash# element or zero
	     pointer if element not in table.
	  */

         //@ManMemo:	reference #Info# of #h#
    Info&        operator[](const HashItem& h)		{ return element[index(h)].info ; }
         //@ManMemo:	reference #Info# of #h#
    const Info&  operator[](const HashItem& h) const	{ return element[index(h)].info ; }
	 /*@Doc:
	     Index operator for accessing the #Info# associated to
	     #HashItem item#. It is required, that #h# belongs to the
	     #DataHashTable#, otherwise it core dumps. Methods #has()# or
	     #get()# can be used for inquiring wheater #h# belongs to the
	     #DataHashTable# or not.
	  */
     //@}

    //@Man:	Iteration
    /*@Doc:
	Often it is desired to loop though all elements in a #DataHashTable#.
	This is provided by means of the following 5 methods. They imply an
	arbitray order to all elements currently in the #DataHashTable#. This
	order may change after any non#const# member function invocation. When
	calling one of these methods, a maker is set that serves as reference
	point for the next call.
     */
    //@{
	//@ManMemo:	return first #Item# in hash table and set marker to it
    const HashItem*	first() const
    			{
			    *(int*)&theCurrent = -1 ;
			    return next() ;
			}
	//@ManMemo:	return last #Item# in hash table and set marker to it
    const HashItem*	last() const
    			{
			    *(int*)&theCurrent = element.size() ;
			    return prev() ;
			}
	//@ManMemo:	return #Item# following current marker thereby increasing marker
    const HashItem*	next() const
    			{
			    if( theCurrent < 0 )
				*(int*)&theCurrent = -1 ;
			    while( ++*(int*)&theCurrent < element.size() )
			    {
				if( element[theCurrent].status
				== DataHashTable_Element<HashItem,Info>::USED )
				    return &element[theCurrent].item ;
			    }
			    *(int*)&theCurrent = -1 ;
			    return 0 ;
			}
	//@ManMemo:	return #Item# referenced by current marker
    const HashItem*	current() const
    			{
			    return (theCurrent<0) ? 0 : &element[theCurrent].item ;
			}
	//@ManMemo:	return #Item# preceding current marker thereby decreasing marker
    const HashItem*	prev() const
    			{
			    if( theCurrent > element.size() )
				*(int*)&theCurrent = element.size() ;
			    while( --*(int*)&theCurrent >= 0 )
			    {
				if( element[theCurrent].status
				== DataHashTable_Element<HashItem,Info>::USED )
				    return &element[theCurrent].item ;
			    }
			    return 0 ;
			}
    //@}

    //@Man:	Manipulation Methods
    //@{
	 //@ManMemo:	Add a new entry to hash table
    void	add (const HashItem& h, const Info& x)
		{
		    assert( !has(h) ) ;
		    int i ;
		    if( thenum >= element.size() )
			reMax( int(factor * thenum) + 1 ) ;
		    for
		    (
			i = (*hashval)(&h) % element.size() ;
			element[i].status == DataHashTable_Element<HashItem,Info>::USED ;
			i = (i+hashsize) % element.size()
		    )
			;
		    element[i].status = DataHashTable_Element<HashItem,Info>::USED ;
		    memcpy( &(element[i].info), &x, cFrontBug ) ;
		    memcpy( &(element[i].item), &h, sizeof(HashItem) ) ;
		    ++thenum ;
		}
         /*@Doc:
	     Adds a new entry consisting of #h# and #x# to the #DataHashTable#.
	     No entry to with #HashItem h# must yet be in the #DataHashTable#.
	     After completion, #x# may be accessed via #get# or #operator[]#
	     with #h# as parameter. The #DataHashTable# is #reMax()#ed if
	     it becomes neccessary.
	  */

         //@ManMemo: remove entry to #h#
    void        remove (const HashItem& h)
		{
		    assert( has(h) ) ;
		    element[index(h)].status
			= DataHashTable_Element<HashItem,Info>::RELEASED ;
		    --thenum ;
		}

         //@ManMemo: remove all entries from #DataHashTable#
    void        clear ()
		{
		    for( int i = element.size()-1 ; i >= 0  ; --i )
			element[i].status
			    = DataHashTable_Element<HashItem,Info>::FREE ;
		    thenum = 0 ;
		}

         //@ManMemo: reset #max()# and #hashSize()#
    void        reMax (int nel =-1, int hashsze=0)
		{
		    McDArray< DataHashTable_Element<HashItem,Info> > cpy(element) ;
		    element.resize( nel < num() ? num() : nel ) ;
		    clear() ;
		    if( hashsze < 1 )
			this->hashsize = autoHashSize() ;
		    else
			this->hashsize = hashsze ;
		    for( int i = cpy.size()-1 ; i >= 0 ; --i )
			if( cpy[i].status == DataHashTable_Element<HashItem,Info>::USED )
			    add( cpy[i].item, cpy[i].info ) ;
		}
         /*@Doc:
	     Reset the #max()# of a #DataHashTable# to #nel#. However, if
	     #nel < num()#, it is resized to #num()# only. If #hashsze < 1# a
	     new hash size is computed automatically. Otherwise, the specified
	     value will be taken.
	  */
    //@}

    //@Man:	Miscallaneous
    //@{
        //@Man:
    int		isConsistent () const
		{
		    int	i, tot ;

		    for( i = element.size()-1, tot = 0 ; i >= 0 ; --i )
			if( element[i].status
			== DataHashTable_Element<HashItem,Info>::USED )
			{
			    ++tot ;
			    if( !has( element[i].item ) )
			    {
				cout << "Inconsistency detected in class DataHashTable\n" ; 
				return 0 ;
			    }
			}

		    if( tot != thenum )
		    {
			cout << "Inconsistency detected in class DataHashTable\n" ; 
			return 0 ;
		    }
		    return element.isConsistent() ;
		}

#ifdef DEFINE_OUTPUT_OPERATOR 
//@ManDoc: Output operator, displays all elements currently contained in hash table
    friend ostream&     operator<<(ostream& out,
                          const DataHashTable<HashItem, Info>& h)
        {
	    const HashItem*	item ;
	    for( item = h.first() ; item ; item = h.next() )
		out << "    " << *item << "\t\t" << h[*item] << endl ;
	    return out ;
        }
#endif

        //@ManMemo: default constructor
    DataHashTable
    (
	    //@ManMemo: pointer to hash function
	int	(*f)(const HashItem*),
	    //@ManMemo: hash size
	int	nel      = 256 ,
	    //@ManMemo: factor for increasing data block
	int	hashsze = 0 ,
            //@Manmemo: number of hash elements
	double	incr     = 2.0
    )
	: element(nel)
	, hashval(f)
	, factor (incr)
    {
	cFrontBug = sizeof(Info) ;
	clear() ;
	if( hashsze < 1 )
	    this->hashsize = autoHashSize() ;
	else
	    this->hashsize = hashsze ;
	assert( factor > 1 ) ;
    }
	/*@Doc:
	    Allocates a #DataHashTable# for #nel# entries using #f# as hash
	    function. If #hashsze > 0# #hashSize()# is set to the specified
	    value, otherwise a suitable #hashSize()# is computed automatically.
	    Parameter #incr# is used for memory management: If more than
	    #nel# entries are added to the #DataHashTable#, it will
	    automatically be #reMax()#ed by a factor of #incr#.
	 */
    //@}
} ;


#endif   // def\_datahashtable
