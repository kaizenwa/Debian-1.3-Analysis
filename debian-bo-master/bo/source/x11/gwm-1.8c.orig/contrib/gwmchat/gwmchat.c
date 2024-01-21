/* 
   gwmchat.c
   Author: Anders Holst (aho@nada.kth.se)
   Copyright (C) 1994  Anders Holst 
     This file is copyrighted under the same terms as the rest of GWM
     (see the X Inc license for details). There is no warranty that it
     works. 
   Compiles with one of: 
     gcc -o gwmchat gwmchat.c -lreadline -ltermcap -lX11 
     gcc -o gwmchat gwmchat.c -DNORL -lX11
*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <unistd.h> 
#include <malloc.h>
#include <X11/Xatom.h>
#include <X11/X.h>
#include <X11/Xlib.h> 
#include <signal.h>
#include <fcntl.h> 
#include <sys/wait.h> 

#ifndef NORL
char* readline(char* prompt);
void add_history(char* line);
void rl_refresh_line();
#endif

enum killstat {normalgwm, killedgwm, normalchat, killedchat, anysignal};

Display* display;
char* dispName;
Atom gwmprop;
int waiting = 0;

void die(char* str) 
{ 
  fprintf(stderr, "gwmchat: %s\n", str);
  exit(1);
}

/* The normal "getopt" is not appropriate here */
char* getoptarg(char** argv, char* opt)
{
  while (*argv && strcmp(*argv,opt))
    argv++;
  if (*argv && *(argv+1) && (**(argv+1) != '-'))
    return *(argv+1);
  else
    return 0;
}

/* The normal "getc" and "gets" cannot be called "recursively" 
   (from eg. a signal handler). */
int readc (FILE* stream)
{
  int result;
  unsigned char c;
  result = read (fileno (stream), &c, sizeof (unsigned char));
  if (result == sizeof (unsigned char))
    return (c);
  return (EOF);
}

char* getline(char* buf, int len)
{
  char ch;
  char* bp = buf;
  int i = 1;
  fflush(stdout);
  while ((!feof(stdin)) && ((ch = readc(stdin)) != '\n'))
    if (i++<len)
      *bp++ = ch;
  *bp = 0;
  return buf;
}

void setSignal(void (*func)())
{
  if (signal(SIGINT, func) == BADSIG ||
      signal(SIGQUIT, func) == BADSIG ||
      signal(SIGTSTP, func) == BADSIG)
    die("Setting signals failed");
}

void closeStdin()
{
  if (close(0) == -1)
    die("Coudn't close stdin");
  if (open("/dev/null", O_RDONLY) == -1)
    die("Coudn't open /dev/null");
}

void initDisplay(int argc, char** argv)
{
  dispName = getoptarg(argv, "-d");
  if (!dispName)
    dispName = getenv("DISPLAY");

  if (!dispName)
    die("Could not connect to server. Check your DISPLAY environment variable.\n");
  if (!(display = XOpenDisplay(dispName)))
    die("Could not connect to server. Check your DISPLAY environment variable.\n");
  gwmprop = XInternAtom(display, "GWM_EXECUTE", 0); 
  if (gwmprop == None)
    die("Could not create GWM_EXECUTE property");
  XSelectInput(display, DefaultRootWindow(display), PropertyChangeMask);
}

void sendGwm(char* str)
{
  int len;
  len = strlen(str);
  XChangeProperty(display, DefaultRootWindow(display),
                  gwmprop, XA_STRING, 8, PropModeReplace, str, len);
  XFlush(display); 
}

void awaitGwm(int wait)
{
  XEvent xev;
  int done = 0;
  if (wait) {
    waiting = 1;
    while (!done) {
      XMaskEvent(display, PropertyChangeMask, &xev);
      if (xev.xproperty.atom == gwmprop && xev.xproperty.state == PropertyDelete)
        done = 1;
    }
    waiting = 0;
  }
  while (XCheckMaskEvent(display, PropertyChangeMask, &xev));
}

void chatloop()
{
  char *cmd = NULL;
  char buf[512];
#ifdef NORL
  char buf2[512];
#endif

  while (1) {
#ifndef NORL
    if (cmd) free(cmd);
    cmd = readline("gwm> ");
#else
    fprintf(stdout, "gwm> ");
    cmd = getline(buf2, 512);
#endif
    if (cmd && cmd[0]) {
#ifndef NORL
      add_history(cmd);
#endif
      sprintf(buf, "(? %s)", cmd);
      awaitGwm(0);
      sendGwm(buf); 
      awaitGwm(1);
      sendGwm("t");
      awaitGwm(1);
      fprintf(stdout, "\n");
    } else {
      sendGwm("t");
      awaitGwm(1);
    }
  }

}

void chatSignalHandler(int sig)
{
  int c;
  char buf[20];
  fprintf(stdout, "\n");
  fprintf(stdout, "Really kill GWM ? ");
  c = *getline(buf, 20);
  if (c != 'y' && c != 'Y') {
    if (waiting) 
      sendGwm("t");        /* Jog GWM in case of hanging */
    else {
#ifndef NORL
      rl_refresh_line();
#else
      fprintf(stdout, "gwm> ");
      fflush(stdout);
#endif
    }
    return;
  }
  fprintf(stdout, "Start a shell instead ? ");
  c = *getline(buf, 20);
  if (c != 'y' && c != 'Y')
    exit(0);
  else
    exit(1);
}

void ignore()      /* Not the same as SIG_IGN, but used the same way */
{
  if (waiting) sendGwm("t");    /* Jog GWM to get gwmchat going */
}

int startGwm(int argc, char** argv, int chatpid)
{
  int pid;
  int i;
  char** newargv;
  char** p;
  char *numstr;

  /* Add -k flag, to get notification when gwm is ready */
  newargv = malloc((argc + 5) * sizeof(char*));
  numstr = malloc(20);
  for (i=0, p=newargv; i<argc; i++, p++, argv++)
    *p = *argv;
  *p++ = "-k";
  sprintf(numstr, "%d", chatpid);
  *p++ = numstr;
  *p++ = "-K";
  *p++ = "14";
  *p = 0;
  newargv[0] = "gwm";

  switch (pid = fork()) {
  case -1:
    die("Forking GWM failed");
  case 0:
    closeStdin();
    sigblock(131078); 
    execvp("gwm", newargv);
    die("Failed to start GWM");
  }
  return pid;
}

int startChat(int argc, char** argv)
{
  int pid;
  char* tmp;
  int GWM_kill_pid, GWM_kill_pid_sig;

  /* Reimplement treatment of -k flags */
  if ((tmp = getoptarg(argv, "-k")))
    GWM_kill_pid = atoi(tmp);
  else
    GWM_kill_pid = 0;
  if ((tmp = getoptarg(argv, "-K")))
    GWM_kill_pid_sig = atoi(tmp);
  else
    GWM_kill_pid_sig = SIGALRM;

  switch (pid = fork()) {
  case -1:
    die("Forking Chat failed");
  case 0:
    setSignal(chatSignalHandler);
    signal(SIGALRM, ignore);
    initDisplay(argc, argv);
    sigpause(0);
    if (GWM_kill_pid)
      kill(GWM_kill_pid, GWM_kill_pid_sig);
    chatloop();
    die("Chat failed");
  }
  return pid;
}

void startShell(int argc, char** argv)
{
  char* shell;

  shell = getenv("SHELL");
  if (!shell)
    shell =  "tcsh";

  setSignal(SIG_DFL);
  fprintf(stdout, "Starting a shell.\n");
  argv[0] = shell; argv[1] = 0; 
  execlp(shell, shell, (char*) 0);
  die("Failed to start shell");
}

enum killstat waitfor(int gwmpid, int chatpid)
{
  pid_t pid;
  union wait status;
  
  pid = wait(&status);
  if (pid == chatpid && status.w_status)
    return killedchat;
  if (pid == chatpid)
    return normalchat;
  if (pid == gwmpid && status.w_status)
    return killedgwm;
  if (pid == gwmpid)
    return normalgwm;
  return anysignal;
}  

void main(int argc, char** argv)
{
  int chatpid, gwmpid;

  setSignal(SIG_IGN);

  chatpid = startChat(argc, argv);

  gwmpid = startGwm(argc, argv, chatpid);

  switch (waitfor(gwmpid, chatpid)) {
  case normalgwm:
    fprintf(stderr, "GWM exited normally\n");
    kill(chatpid, SIGKILL); 
    break;
  case killedgwm:
    fprintf(stderr, "GWM was killed\n");
    kill(chatpid, SIGKILL); 
    startShell(argc, argv); 
    break;
  case normalchat:
    fprintf(stderr, "GWM was killed by gwmchat\n");
    kill(gwmpid, SIGKILL); 
    break;
  case killedchat:
    fprintf(stderr, "GWM was killed by gwmchat\n");
    kill(gwmpid, SIGKILL); 
    startShell(argc, argv); 
    break;
  default:
    fprintf(stderr, "Something unexpected happened\n");
    kill(gwmpid, SIGKILL); 
    kill(chatpid, SIGKILL); 
    break;
  }
}
