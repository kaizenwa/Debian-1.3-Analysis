#define MAX_PROCESSES 10

#ifdef __WIN32__
#include <windows.h>
#include <stdio.h>
extern CRITICAL_SECTION Critical_Section;
extern HANDLE Input_Events[];
#else
extern int Subprocess_Read_fds [MAX_PROCESSES][2];
extern int Max_Subprocess_FD;
#endif

extern int Num_Subprocesses;
extern volatile int Child_Status_Changed_Flag;

extern void read_process_input (int);
extern int jed_close_process (int *);
extern int jed_send_process (int *, char *);
extern int jed_open_process (int *);
extern void jed_get_child_status (void);
extern void jed_kill_process (int);
extern void jed_get_process_mark (int *);
extern void jed_set_process (int *, char *, char *);
extern void jed_send_process_eof (int *);
extern void get_process_input (int *);
extern int jed_signal_process (int *, int *);

#ifdef REAL_UNIX_SYSTEM
extern void jed_block_child_signal (int);
#endif

extern FILE *jed_popen (char *, char *);
extern int jed_pclose (FILE *);
