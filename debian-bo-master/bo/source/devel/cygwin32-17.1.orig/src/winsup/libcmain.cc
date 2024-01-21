
#include <windows.h>

/* Allow apps which don't have a main work, as long as they define WinMain */
extern "C" 
{
  extern  int PASCAL WinMain (HANDLE hInstance, HANDLE hPrevInstance,
			     LPSTR lpszCmdLine, int nCmdShow);

  main ()
    {
      HANDLE x = GetModuleHandleA(0);
      WinMain (x, 0, GetCommandLineA(), 1);
    }
};
