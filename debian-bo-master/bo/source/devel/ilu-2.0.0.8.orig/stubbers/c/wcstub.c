/*
Program using WINIO that prompts for working directory
command line args and then calls main.

Dan Larner, 1995
*/

#if (defined(WIN32) && defined(_WINIO))

#include <windows.h>
#include <winio.h>

extern void main (int ac, char **av, char **envp);

/* this global gets set to 0 if we're not on win32s, else 1 
DEFUNCT
int g_i_on_win32s = 0;

int OnWin32s() {

	OSVERSIONINFO osvinfo;

	osvinfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);

	if (GetVersionEx(&osvinfo) == FALSE) // err on safe side
		return 0;

	return osvinfo.dwPlatformId == VER_PLATFORM_WIN32s; 
}	
*/

int CALLBACK WinMain(HANDLE hInstance, HANDLE hPrevInstance, 
    LPSTR lpCmdLine, int nCmdShow) {

   	int i_argc;
	char* argv [64];  /* xxx probably enough args! */
	char szArguments[1024];
	LPSTR pc_args = lpCmdLine;
	int i_in_batch = 0;  /* 1 if we're running in batch mode */

	/* see if we're running in batch mode */
	if ((lpCmdLine != NULL) && strncmp(lpCmdLine, "-batch", 6) == 0) {
	  pc_args = lpCmdLine + 6;
	  i_in_batch = 1;
	}

	if (winio_setmain(hInstance, hPrevInstance, pc_args, nCmdShow,  
    	&i_argc, argv, 64, "C Stubber", "wcstub.exe", szArguments) != 1) 
    	return -1;

	/* defunct - g_i_on_win32s = OnWin32s();  */

	// call main - note that anything in main ... calls fatal if
	// something bad happens
   	main(i_argc, argv, NULL); 

	printf("\nwcstub complete\n");

	if (i_in_batch != 1)
		winio_end(); 	// let user look at output till user closes
	else
		winio_close(); // batch mode things were fine, close up
	 
    return 0;
}

 
#endif /* (defined(WIN32) && defined(_WINIO)) */
