/***************************************************************
 *
 * $Id: McDArray.h,v 3.0 1997/02/04 17:48:53 bzfzoeck Exp $
 *
 * $Log: McDArray.h,v $
 * Revision 3.0  1997/02/04 17:48:53  bzfzoeck
 * released Version 3.0
 *
 * Revision 1.6  1996/12/16 10:44:44  bzfzoeck
 * now really
 *
 * Revision 1.4  1996/09/05  08:04:14  bzfzoeck
 * .
 *
 * Revision 1.12  1996/08/08  16:00:18  bzfzoeck
 * replaced realloc(0,size) by malloc.
 *
 * Revision 1.11  1996/07/16  13:49:02  bzfstall
 * Documentation updated.
 *
 *
 * Revision 1.10  1996/07/01  09:44:31  bzfzoeck
 * Added static to the declarations of index and rindex to prevent
 * the linker from complaining about multiple defined symbols.
 *
 * Revision 1.9  1996/07/01  09:15:17  bzfstall
 * Added global template functions index() and rindex().
 *
 * Revision 1.6  1996/06/17  16:38:32  bzfzoeck
 * Very simple string class added.
 *
 ***************************************************************/
#ifndef MCDARRAY_H
#define MCDARRAY_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

/// Dynamic array for primitive data objects.

/**
  Class #McDArray# provides safe arrays of \Ref{Data Objects}. For general
  C++ objects (i.e. no data objects) class #Array# is provided which manages
  memory in a C++ compliant way.

  The elements of an instance of #McDArray# can be accessed just like
  ordinary C++ array elements by means of the index #operator[]#. Safety is
  provided by
  \begin{itemize}
  \item	automatic memory management in constructor and destructor
  preventing memory leaks
  \item checking of array bounds when accessing elements with the
  indexing #operator[]# (only when compiled without #-DNDEBUG#).
  \end{itemize}

  Moreover, #McDArray#s may easily be extended by #insert#ing or #append#ing
  elements to the #McDArray# or shrunken by #remov#ing elements. Method
  #resize(int n)# resets the #McDArray#s size to #n# thereby possibly
  appending elements or truncating the #McDArray# to the required size.

  A #McDArray#s may be used as arguments for standard C functions requiring
  pointers through a cast operator.

  Internally, a #McDArray# objects allocates a block of memory that fits up
  to #max()# elements, only #size()# of them are used. This makes extension
  and shrinking methods perform better.
*/

template<class T> class McDArray
{
  public:
    /// Copy constructor.
    McDArray(const McDArray& old) : thesize(old.thesize),
	themax(old.themax), memFactor(old.memFactor)
    {
	data = (T*)malloc(themax*sizeof(T));
	if (thesize)
	    memcpy(data, old.data, thesize*sizeof(T));
	assert(isConsistent());
    }

    /// Default constructor.
    /** The constructor allocates an #Array# containing #size#
	uninitialized elements. The internal array is allocated to have
	#max# nonzeros, and the memory extension factor is set to #fac#.
    */
    McDArray(int size=0, int max=0, float fac=1.2) : memFactor(fac)
    {
	thesize = (size < 0) ? 0 : size;
	themax = (max > thesize) ? max : thesize;
	if (themax) data = (T*) malloc(themax * sizeof(T));
	    else data = 0;
	assert(isConsistent());
    }

    /// Destructor.
    ~McDArray() { if (data) free(data); }

    /// Append element #t# to #McDArray#.
    int append(const T& t) {
	return insert(thesize, 1, &t);
    }

    /// Append #n# elements in #t# to #McDArray#.
    int append(int n, const T t[]) {
	return insert(thesize, n, t);
    }

    /// Append all elements in #t# to #McDArray#.
    int append(const McDArray<T>& t) { 
	return insert(thesize, t); 
    }

    /// Append empty slots.
    void appendSpace(int n) {
	resize(thesize + n);
    }

    /// Insert #n# uninitialized elements before #i#-th element.
    int insertSpace(int addBefore, int n=1)
    {
	int j = size();
	int delta = resize(thesize+n);
	
	T* ptrdest = &last();        // new last element
	T* ptrsrc  = ptrdest-n;      // previously the last element was here
	T* inspos  = data+addBefore+n-1;
	
	for (; ptrdest>inspos; ptrdest--, ptrsrc--)
	    *ptrdest = *ptrsrc;
	
	return delta;
    }

    /// Insert #n# elements in #t# before #i#-the element.
    int insert(int addBefore, int n, const T t[]) {
	if (n > 0) {
	    int delta = insertSpace(addBefore, n);
	    memcpy(&(data[addBefore]), t, n*sizeof(T));
	    return delta;
	}
	return 0;
    }

    /// Insert all elements in #t# before #i#-the element.
    int insert(int addBefore, const McDArray<T>& t) 
    {	
	if( t.size() ) {
	    int delta = insertSpace(addBefore, t.size()) ;
	    memcpy(&(data[addBefore]), t.data, t.size()*sizeof(T)) ;
	    return delta ;
	}
	return 0 ;
    }

    /// Insert t in #i#-the element.
    int insert(int addBefore, const T& t) 
    {	
	int delta = insertSpace(addBefore, 1) ;
	data[addBefore]=t;
	return delta ;
    }

    /// Initialize all #size()# elements with t.
    void fill(const T& t)
    {
	T *ptr;
	T *end = data+size();
	for (ptr=data; ptr<end; ptr++) {
	    *ptr=t;
	}
    }

    /// Index operator.
    T&  operator[](int n) {
	assert(n>=0 && n<thesize);
	return data[n] ;
    }

    /// Const version of index operator.
    const T& operator[](int n) const {
	assert(n>=0 && n<thesize);
	return data[n];
    }

    /// Returns reference to last element of array.
    T&  last() {
	assert(thesize>0);
	return data[thesize-1];
    }

    /// Returns const reference to last element of array.
    const T& last() const {
	assert(thesize>0);
	return data[thesize-1];
    }

    /// Returns pointer to array data.
    operator T* () { return data; }

    /// Returns const pointer to array data.
    operator const T* () const  { return data; }

    /// Removes #n# elements starting at #firstIndexToDelete#.
    void remove(int firstIndexToDelete=0, int n=1)
    {
	assert(firstIndexToDelete<thesize && firstIndexToDelete>=0);
	if (firstIndexToDelete+n < thesize)
	    memcpy(&(data[firstIndexToDelete]), &(data[firstIndexToDelete+n]),
		(thesize-(firstIndexToDelete+n))*sizeof(T));
	else n = thesize - firstIndexToDelete;
	thesize -= n;
    }

    /// Removes #n# last elements.
    void removeLast(int n=1)
    {
	assert(n<=size() && n>=0);
	thesize -= n;
    }

    /// Removes all elements.
    void clear() { thesize = 0; }

    /// Returns number of elements in array.
    int size() const { return thesize; }

    /// Set size to #newsize#.
    int resize(int newsize) 
    {
	if (newsize<themax) {
	    if (newsize < 0) thesize = 0;	
		else thesize = newsize;
	    return thesize;
	} else {
	    if (memFactor <=1) memFactor=(float)1.2;
	    return remax((int)(memFactor*newsize), newsize);
	}
    }

    /// Returns maximum number of elements.
    /** This method returns the number of elements which fit into the
	internal buffer of the array. This number may be somewhat larger
	than the actual number of elements in the array as returnd by
	#size()#. */
    int max() const { return themax; }

    /// Reset size of internal buffer.
    /** The value of #max()# is reset to #newMax# thereby setting #size()#
	to #newSize#. However, if #newSize# has a value #< 0# (as the
	default argument does) #size()# remains unchanged and #max()# is
	set to #MAX(size(), newMax)#. Hence, calling #remax()# without
	the default arguments, will reduce the memory consumption to a
	minimum. In no instance #max()# will be set to a value less than 0
	(even if specified). */
    int remax(int newMax=-1, int newSize=-1)
    {
	int oldmax = themax;
	if (newSize >= 0) thesize = newSize;
	if (newMax < newSize) newMax = newSize;
	if (newMax < 0) newMax = thesize;
	if (newMax == themax) return 0;
	themax = newMax;

	if (themax) {
	    if (data)
		data = (T*)realloc( data, themax * sizeof(T) ) ;
	    else
		data = (T*)malloc(themax * sizeof(T) ) ;
	} else {
	    if (data) free(data);
	    data = 0;
	}
	return oldmax-themax;
    }

    /// Consistency check.
    int isConsistent() const {
	if (themax<0 || themax<thesize) {
	    assert(0);
	    return 0;
	}
	return 1;
    }

    /// Assignment operator.
    /** Assigning an lvalue #McDArray# to a rvalue #McDArray# involves
        resizing the lvalue to the rvalues #size()# and copying all
	elements via #memcpy()#. */
    McDArray& operator=(const McDArray& rhs) 
    {
	resize(rhs.size());
	memcpy(data, rhs.data, size()*sizeof(T));
	return *this;
    }

    /// Memory extension factor.
    /** When a #McDArray# is #resize()#d to more than #max()# elements,
	the new value for #max()# is not just set to the new size but
	rather to #memFactor * size()#. */
    float memFactor;

  protected:
    int thesize;	// Number of active elements in array
    int themax;		// Determines size of internal buffer
    T *data;		// Pointer t internal buffer
};

/// Looks for first occurence of given element and returns its index.
template<class T>
static int index(McDArray<T>& array, const T& elem)
{
    for (int i=0; i<array.size(); i++)
	if (array[i] == elem) return i;
    return -1;
}

/// Looks for last occurence of given element and returns its index.
template<class T>
static int rindex(McDArray<T>& array, const T& elem)
{
    for (int i=array.size()-1; i>=0; i--)
	if (array[i] == elem) return i;
    return i;
}

#endif
