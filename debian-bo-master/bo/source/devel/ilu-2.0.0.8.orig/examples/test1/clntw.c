/* A crude brute force adaptation of the command line clnt.c program
   into a *very* elementry windows version (adapted from Petzold 
   "Programming Windows 3.1" */

#include <windows.h>

#include <stdio.h>
#include <winio.h>

#include <stdlib.h>
#include <string.h>

#include "resource.h"

#include "clnt.h"

/* holds handle of our main window */
HWND g_h_hwnd;

char* g_pc_windowchars = NULL;

void paintit() {
        RECT rect;

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
    strcat(g_pc_windowchars, buf);
    paintit();
}

long CALLBACK WndProc (HWND hwnd, UINT message, UINT wParam, LONG lParam)
     {
     HDC         hdc ;
     PAINTSTRUCT ps ;
     RECT        rect ;
     static int i_doingtest = 0;
     
     switch (message)
          {
          case WM_PAINT :
               hdc = BeginPaint (hwnd, &ps) ;
               GetClientRect (hwnd, &rect) ;
               DrawText (hdc, g_pc_windowchars, -1, &rect, DT_LEFT) ;
               EndPaint (hwnd, &ps) ;
               return 0 ;

          case WM_DESTROY :
               PostQuitMessage (0) ;
               return 0 ;

		  case WM_COMMAND :
		  	switch (LOWORD(wParam)) {
				case ID_ACTION_RUN	:
					/* on win16, something processes messages for us while we're blocked in 
					a select(), which means we can enter here even though we're already in 
					the middle of a test, so prevent re entering. */
					if (i_doingtest == 0) {
						i_doingtest = 1;
						if (doit() != 0) 
							MessageBox(hwnd, "dotest unsuccessful", "Error", MB_OK | MB_ICONEXCLAMATION);
						/* force an entire window repaint */
  						GetClientRect (g_h_hwnd, &rect);
  						InvalidateRect(g_h_hwnd, &rect, TRUE);
  						SendMessage(g_h_hwnd, WM_PAINT, 0, 0);

						i_doingtest = 0;
					}
					break;
				case ID_ACTION_EXIT :
					PostQuitMessage (0) ;
					break;

			}
          }

     return DefWindowProc (hwnd, message, wParam, lParam) ;
     }


int CALLBACK WinMain (HANDLE hInstance, HANDLE hPrevInstance,
                    LPSTR lpszCmdParam, int nCmdShow)
     {                     
     static char szAppName[] = "iluTest1WindowsClient" ;
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
          wndclass.hIcon         = LoadIcon (hInstance, "clntw_icon") ;
          wndclass.hCursor       = LoadCursor (NULL, IDC_ARROW) ;
          wndclass.hbrBackground = GetStockObject (WHITE_BRUSH) ;
          wndclass.lpszMenuName  = MAKEINTRESOURCE(IDR_MENU1) ;
          wndclass.lpszClassName = szAppName ;

          RegisterClass (&wndclass) ;
          }                                

     hwnd = CreateWindow (szAppName,       // window class name
                    "iluTest1WindowsClient",   // window caption
                    WS_OVERLAPPEDWINDOW,   // window style
                    CW_USEDEFAULT,         // initial x position
                    CW_USEDEFAULT,         // initial y position
                    CW_USEDEFAULT,         // initial x size
                    CW_USEDEFAULT,         // initial y size
                    NULL,                  // parent window handle
                    NULL,                  // window menu handle
                    hInstance,             // program instance handle
                    NULL) ;                // creation parameters

	 /* save our window handle in the global */
	 g_h_hwnd = hwnd;
     
     g_pc_windowchars = (char*) malloc(8192);
     if (g_pc_windowchars == NULL) {
     	MessageBox(NULL, "Couldn't malloc(8192) for g_pc_windowchars", "Error", 
     				MB_ICONSTOP | MB_OK);
     	return msg.wParam;
     }
     g_pc_windowchars[0] = '\0';

#ifdef WIN16     
     /* initialize winsock when under WIN16 */
     ilu_StartupWinsock ();
#endif
     
	 /* create our console for stdio output (e.g. from the ilu runtime) */
	 winio_console(hInstance, hPrevInstance,
            nCmdShow, 0, "clientw Console");

     ShowWindow (hwnd, nCmdShow) ;
     UpdateWindow (hwnd) ;

     while (GetMessage (&msg, NULL, 0, 0))
          {
          TranslateMessage (&msg) ;
          DispatchMessage (&msg) ;
          }
         
     return msg.wParam ;
     }

