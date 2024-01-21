#include <windows.h>
#include <stdlib.h>
#include <iluxport.h>

#define WIN_MSG_LOOP_DELAY_SECS 0
#define WIN_MSG_LOOP_DELAY_MSECS 500

void set_process_windows_messages_alarm (int* pi_stop);

/* sets the pointed to ilu_FineTime to a time i_secs + i_msecs in the future 
   note ilu_FineTimeRate on Windows is 1000 milliseconds */
static void set_alarm_time_from_now (ilu_FineTime* p_finetime, ilu_integer i_secs, ilu_cardinal i_msecs) {

  *p_finetime = ilu_FineTime_Now();		/* set to current time */

  p_finetime->ft_s = p_finetime->ft_s + i_secs;	    /* add seconds from now */

  if (i_msecs + p_finetime->ft_t > ilu_FineTimeRate) { /* if overflow on msec */
  	(p_finetime->ft_s)++;
  	p_finetime->ft_t = i_msecs + p_finetime->ft_t - ilu_FineTimeRate;
  }
  else p_finetime->ft_t = p_finetime->ft_t + i_msecs;	/* add milliseconds from now*/
};


/* called by the mainloop when our g_process_messages_alarm goes off */
/* XXX NOTE: Under WIN16, something in the tcpip system actually dispatches messages
   for us while we're blocked in select, so this routine normally doesn't actually
   end up dispatching many messages under Win16. 
*/
void process_windows_messages (ilu_private rock) {

    extern int after_windows_quit(int status);
	MSG msg;    
	// printf("process_windows_messages entered\n");
	while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE))
	{
	 if (msg.message == WM_QUIT) {
	 	/* if by chance we pickup a quit message in here
	 	   make our main loop stop, & do any necessary post quit operations */
	 	// MessageBox(NULL, "process_windows_messages got a quit message", "Info", MB_OK);
	    ilu_ExitMainLoop((int*)rock);
	    exit(after_windows_quit(msg.wParam));
		}
	 TranslateMessage(&msg);
	 DispatchMessage(&msg);
	}

	/* set up to check messages in the future */
	set_process_windows_messages_alarm (rock);
}


/* set up to check messages in the future */
void set_process_windows_messages_alarm (int* pi_stop) {

	/* used to make the ilu mainloop periodically run a windows message loop */
	static ilu_refany g_process_messages_alarm = NULL;
	static ilu_FineTime g_alarm_time;
    
	if (g_process_messages_alarm == NULL)
		g_process_messages_alarm = ilu_CreateAlarm(); 

	set_alarm_time_from_now(&g_alarm_time, WIN_MSG_LOOP_DELAY_SECS, WIN_MSG_LOOP_DELAY_MSECS);
	ilu_SetAlarm (g_process_messages_alarm, g_alarm_time, process_windows_messages, pi_stop);
}



