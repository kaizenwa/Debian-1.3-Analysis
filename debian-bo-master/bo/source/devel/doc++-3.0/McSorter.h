
#ifndef DEFsorter		// prevent multiple includes
#define DEFsorter
#include <assert.h>

/* \Section{Implementation}
    This is quicksort.
 */
//@Man:	sorting	functions
//@{
/*@ManDoc:
    The template function #sort# sorts an array #t# holding #n# elements of type
    #T# using #c# for comparisions. Class #COMPARATOR# must be provide an
    overloaded #operator()(const T& t1,const T& t2)#, that returns
    \begin{description}
    \item[#<0#]		if #t1# is to appear before #t2#,
    \item[#==0#]	if #t1# and #t2# can appear in any order or
    \item[#>0#]		if #t1# is to appear after #t2#.
    \end{description}
 */
template< class COMPARATOR, class T >
static void	sort( T* t, int end, COMPARATOR& compare, int start = 0 )
{
    int		i0, i1, j ;
    double	c ;

    T	work, mid, tmp ;

    work             = t[start] ;
    t[start]         = t[(start+end)/2] ;
    t[(start+end)/2] = work ;

    mid  = t[start] ;
    work = t[end-1] ;

    for(i0 = i1 = start, j = end-1 ; i1 < j ; )
    {
	c = compare(mid,work) ;
	if( c > 0 )
	{
	    tmp   = t[i0] ;
	    t[i0] = work ;
	    i0++ ;
	    i1++ ;
	    work  = t[i1] ;
	    t[i1] = tmp ;
	}
	else if( c < 0 )
	{
	    t[j] = work ;
	    --j ;
	    work = t[j] ;
	}
	else
	{
	    i1++ ;
	    tmp   = t[i1] ;
	    t[i1] = work ;
	    work  = tmp ;
	}
    }

    if( start < i0-1 )
	sort( t, i0, compare, start ) ;
    if( i1+1 < end )
	sort( t, end, compare, i1+1 ) ;
}

#ifndef	_CRAYMPP
/*@ManDoc:
    The template function #sort# sorts an array #t# holding #n# elements of type
    #T#, which must provide an #operator>#.
 */
template< class T >
void	sort( T* t, int n )
	{
	    struct
	    {
		int	operator()(T i, T j)
			{
			    return (i < j) ? -1 : ((j < i) ? 1 : 0) ;
			}
	    } c ;
	    sort(t, n, c, 0) ;
	}
#endif	//@ _CRAYMPP
//@}


#endif // #DEFsorter#
