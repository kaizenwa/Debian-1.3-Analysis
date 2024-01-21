// declspec test #1

__declspec (dllimport) void imp ();

__declspec (dllexport) void exp () { imp (); }
