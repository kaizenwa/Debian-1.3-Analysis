template<int a, int b>
class Elvis
{
} ;

template<int a>
class Elvis<0>
{
   int geta() { return a ; }
} ;
