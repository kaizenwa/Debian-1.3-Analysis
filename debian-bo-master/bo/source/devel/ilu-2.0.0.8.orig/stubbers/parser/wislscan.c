/*
Program using WINIO that prompts for working directory
command line args and then calls main.

Dan Larner, 1995
*/

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))

#include <windows.h>
#include <winio.h>

#ifdef WIN16
extern int scan_main (int ac, char **av, char **envp);
#define _MAX_PATH 256
#else
extern int main (int ac, char **av, char **envp);
#endif

int CALLBACK WinMain(HANDLE hInstance, HANDLE hPrevInstance, 
    LPSTR lpCmdLine, int nCmdShow) {
   	int i_argc;
	char* argv [3]; // only takes 1 arg
	char szArguments[_MAX_PATH * 3]; // large in case the user enters extra nonsense
	int i_result;
	if (winio_setmain(hInstance, hPrevInstance, lpCmdLine, nCmdShow,  
    	&i_argc, argv, 3, "ISL Scan", "wislscan.exe", szArguments) != 1) 
    	return -1;
#ifdef WIN16
   	i_result = scan_main(i_argc, argv, NULL); // call main
#else
   	i_result = main(i_argc, argv, NULL); // call main
#endif
	printf("\nwislscan complete\n");
	winio_end(); 	// let user look at output till user closes 
    return i_result;
}

#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */
