/* $Id: srvrw.c,v 1.5 1996/07/09 00:42:13 larner Exp $ */
/* Last edited by Mike Spreitzer January 11, 1996 3:47 pm PST */

/* A crude brute force adaptation of the command line srvr.c program
   into a *very* elementry windows version (adapted from Petzold 
   "Programming Windows 3.1" */

#include <windows.h>

#include <stdio.h>
#include <winio.h>

#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "resource.h"

#include "srvr.h"

#define _MAX_WINDOW_CHARS 8192

/* holds handle of our main window and application instance */
HWND g_h_hwnd;
HANDLE g_h_hInstance;

char g_windowchars[_MAX_WINDOW_CHARS] = "";
char g_c_line[512] = "";

void paintit() {

	static int i_charcount = 10;   /* 10 to allow for safety margin */
	int i_c_line_length;
	RECT        rect ;

	i_c_line_length = strlen(g_c_line);

	if ((i_charcount + i_c_line_length) >= _MAX_WINDOW_CHARS) { 
		 g_windowchars[0] = '\0';
		 i_charcount = 10;
	}
    strcat(g_windowchars, g_c_line);
	i_charcount = i_charcount + i_c_line_length;

	/* force an entire window repaint - really crude here */
	GetClientRect (g_h_hwnd, &rect);
	InvalidateRect(g_h_hwnd, &rect, TRUE);
	SendMessage(g_h_hwnd, WM_PAINT, 0, 0);
    return;
}

/* called to output strings to a window by clnt.c */

void WIN_PRINTF(char *format, ...)
{
    char buf[512];
    va_list arg_ptr;
   
    va_start(arg_ptr, format);
    vsprintf(buf, format, arg_ptr);
    va_end (arg_ptr);
    strcat(g_windowchars, buf);
    paintit();
}

BOOL CALLBACK AboutDlgProc (HWND hDlg, UINT message, UINT wParam, LONG lParam) {
     switch (message) {
          case WM_INITDIALOG :
               return TRUE ;
          case WM_COMMAND :
               switch (wParam) {
			    case IDOK :
			         EndDialog (hDlg, 0) ;
			         return TRUE ;
                }
               break ;
          }
     return FALSE ;
}
                     
                     
/* our post quit message processing */
int after_windows_quit(int status) {
	return status;
}


long CALLBACK WndProc (HWND hwnd, UINT message, UINT wParam, LONG lParam)
     {
     HDC         hdc ;
     PAINTSTRUCT ps ;
     RECT        rect ;
	 static int i_already_serving = 0;
	 static int i_stop = 0;
	 
     switch (message)
          {
          case WM_PAINT :
               hdc = BeginPaint (hwnd, &ps) ;
               GetClientRect (hwnd, &rect) ;
               DrawText (hdc, g_windowchars, -1, &rect, DT_LEFT) ;
               EndPaint (hwnd, &ps) ;
               return 0 ;

          case WM_DESTROY :
               PostQuitMessage (0) ;
               return 0 ;

		  case WM_COMMAND :
		  	switch (LOWORD(wParam)) {
				case ID_ACTION_RUN	:
					if (i_already_serving != 0) {
						MessageBox(hwnd, "Already Serving", "Info", MB_OK | MB_ICONEXCLAMATION);
						break;
					}
					i_already_serving = 1;
					i_stop = 0;
					if (doit(NULL, NULL, ilu_FALSE, ilu_FALSE) != 0) { 
						MessageBox(hwnd, "doserve Unsuccessful", "Error", MB_OK | MB_ICONEXCLAMATION);
					}
					MessageBox(hwnd, "doserve Successful", "Error", MB_OK | MB_ICONEXCLAMATION);
					i_already_serving = 0;					
					break;
				case ID_ACTION_EXIT :
					/* XXX Note:  Under WIN16, something in the tcpip system actually dispatches messages
   					for us while we're blocked in select.  Wherever this dispatch loop is, it
   					seems to not exit when a WM_QUIT is posted, so we'll force our ilu main loop to 
   					stop here by calling ilu_ExitMainLoop on our i_stop variable */  
   					ilu_ExitMainLoop(&i_stop);
					// MessageBox(NULL, "WndProc posted a quit message", "Info", MB_OK);
					PostQuitMessage (0) ;
					/* XXX Note the preceeding works fine under the win16 susbsystem under NT, but doesn't work
   					properly on an actual win16 platform with Microsoft's tcpip ! The select() call in
   					this case doesn't seem to always pay attention to timeouts! So we're going to 
   					just flat out exit on WIN16 */
#ifdef WIN16
					exit(after_windows_quit(0));
#endif
					break;
				case ID_ABOUT_SRVRW :
				 DialogBox(g_h_hInstance, MAKEINTRESOURCE(IDD_DIALOG1),
                         g_h_hwnd, (DLGPROC)AboutDlgProc);
					break;

			}
          }

     return DefWindowProc (hwnd, message, wParam, lParam) ;
     }


int CALLBACK WinMain (HANDLE hInstance, HANDLE hPrevInstance,
                    LPSTR lpszCmdParam, int nCmdShow)
     {
     static char szAppName[] = "iluTest1WindowsServer" ;
     HWND        hwnd ;
     MSG         msg ;
     WNDCLASS    wndclass ;

     if (!hPrevInstance)
          {
          wndclass.style         = CS_HREDRAW | CS_VREDRAW ;
          wndclass.lpfnWndProc   = WndProc ;
          wndclass.cbClsExtra    = 0 ;
          wndclass.cbWndExtra    = 0 ;
          wndclass.hInstance     = hInstance ;
          wndclass.hIcon         = LoadIcon (hInstance, "srvrw_icon") ;
          wndclass.hCursor       = LoadCursor (NULL, IDC_ARROW) ;
          wndclass.hbrBackground = GetStockObject (WHITE_BRUSH) ;
          wndclass.lpszMenuName  = MAKEINTRESOURCE(IDR_MENU1) ;
          wndclass.lpszClassName = szAppName ;

          RegisterClass (&wndclass) ;
          }

     hwnd = CreateWindow (szAppName,       // window class name
                    "iluTest1WindowsServer",   // window caption
                    WS_OVERLAPPEDWINDOW,   // window style
                    CW_USEDEFAULT,         // initial x position
                    CW_USEDEFAULT,         // initial y position
                    CW_USEDEFAULT,         // initial x size
                    CW_USEDEFAULT,         // initial y size
                    NULL,                  // parent window handle
                    NULL,                  // window menu handle
                    hInstance,             // program instance handle
                    NULL) ;                // creation parameters

	 /* save our window handle and app instance in the globals */
	 g_h_hwnd = hwnd;
	 g_h_hInstance = hInstance;
 
 /* blank our window chars */
  g_windowchars[0] = '\0';

#ifdef WIN16     
     /* initialize winsock when under WIN16 */
     ilu_StartupWinsock ();
#endif
 
	 /* create our console for stdio output (e.g. from the ilu runtime) */
	 winio_console(hInstance, hPrevInstance,
            nCmdShow, 0, "serverw Console");

     ShowWindow (hwnd, nCmdShow) ;
     UpdateWindow (hwnd) ;

     while (GetMessage (&msg, NULL, 0, 0))
          {
          TranslateMessage (&msg) ;
          DispatchMessage (&msg) ;
          }
     // MessageBox(NULL, "WinMain left the dispatch loop", "Info", MB_OK);
     return after_windows_quit(msg.wParam) ;
     }

