// Bug: g++ emits template instances when it shouldn't.
// Special g++ Options: -g -fexternal-templates
// excess errors test - fails on systems that use collect2 XFAIL *-*-*

#pragma implementation "irrelevant_file"
#line 1 "wa.h"
#pragma interface		// ERROR - 
template <class T> inline T min (T a, T b) { return a<b?a:b; }
#line 3 "wa.C" 

main()
{
  min (1, 1); 
}
