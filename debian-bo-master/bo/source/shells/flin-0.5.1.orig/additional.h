/*
  #define S_LWSP 1
  #define S_WORD 2
  #define S_QUOTED 3
  #define S_ESCAPED 4
  #define S_TILDE 5
  #define S_DOLLAR 6
  #define S_TRANSLATE 7
*/

enum parse_state  
{
  S_LWSP , S_WORD , S_QUOTED , S_ESCAPED , S_TILDE , S_DOLLAR , S_TRANSLATE 
};

enum rcfile_type
{
  SYSTEM , USER
};

#define rcflag_restricted 0x1
#define rcflag_system_only 0x2

struct rc_commands
{
  char *command;
  int flags;
  void (*runrc)(int, char**);
};

int prep_for_exec(char *exec_line, int *argc, char **argv, int maxargc);
void do_exec(char *exec_line, int argc, char **argv);
void finish_from_exec(int argc, char **argv);
int parseline(char *buffer, int *argc, char **argv, int maxargc);
void handle_children(void);
int processrcfile (char *rcfilename, enum rcfile_type rctype);
void rc_setenv(int argc, char **argv);
void rc_unsetenv(int argc, char **argv);
void rc_exec(int argc, char **argv);
void rc_system(int argc, char **argv);
void rc_pause(int argc, char **argv);
void rc_sleep(int argc, char **argv);
void rc_logging(int argc, char **argv);
void rc_noclobber(int argc, char **argv);
void rc_set_booloption(int argc, char **argv, int *booloption);
void rc_restrict(int argc, char **argv);

