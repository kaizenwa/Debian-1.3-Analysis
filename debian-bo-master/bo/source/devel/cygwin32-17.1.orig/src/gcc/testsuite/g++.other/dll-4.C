// Ensure dllexport overrides dllimport.

__declspec (dllimport) int foo1 ();
__declspec (dllexport) int foo1 ();

__declspec (dllexport) int foo2 ();
__declspec (dllimport) int foo2 ();

__declspec (dllexport) int foo1 () { return foo2 (); }
__declspec (dllexport) int foo2 () { return foo1 (); }
