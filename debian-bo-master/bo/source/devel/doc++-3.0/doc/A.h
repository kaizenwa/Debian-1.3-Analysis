/** Common base class.
    This could be a long documentation for the common base class.
    Note that protected members are displayed after public ones, even if they
    appear in another order in the source code.

    This is how this documentation has been generated:
    \begin{verbatim}
    /** Common base class.
        This could be a long documentation for the common base class.
        Note that protected members are displayed after public ones, even if they
        appear in another order in the source code.
        This is how this documentation has been generated: * /

    class	CommonBase
    {
    private:
            /// this member shows up only, if you call doc++ with -p
        int	privateMember() ;

    protected:
            /// a protected member variable
        double	variable ;

    public:
            /// a public member function showing links to argument and type classes
        const Derived_Class&	getB( const Intermediate& c ) const ;
    } ;
    \end{verbatim}
 */
class	CommonBase
{
private:
	/// this member shows up only, if you call doc++ with -p
    int	privateMember() ;

protected:
	/// a protected member variable
    double	variable ;

public:
	/// a public member function showing links to argument and type classes
    const Derived_Class&	getB( const Intermediate& c ) const ;
} ;
