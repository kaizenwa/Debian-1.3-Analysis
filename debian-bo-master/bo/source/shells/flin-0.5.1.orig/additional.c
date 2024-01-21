/*

   Additional parts for Flin.

   Copyright Stephen Fegan July 1995

 */

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<pwd.h>
#include<sys/types.h>
#include<sys/resource.h>
#include<sys/wait.h>
#include<sys/stat.h>
#include<fcntl.h>
#include<unistd.h>
#include<signal.h>

#include"additional.h"

extern char *progname;
extern int exec_logging;
extern int noclobber;

struct rc_commands recognised_rc_commands[] =
{
   {"restrict", rcflag_system_only, rc_restrict},
   {"setenv", 0, rc_setenv},
   {"unsetenv", 0, rc_unsetenv},
   {"exec", 0, rc_exec},
/*  { "system", 0, rc_system }, */
   {"pause", 0, rc_pause},
   {"sleep", 0, rc_sleep},
   {"logging", 0, rc_logging},
   {"noclobber", 0, rc_noclobber},
   {NULL, 0, NULL},
};

int parseline(char *buffer, int *argc, char **argv, int maxargc)
{
   enum parse_state state = S_LWSP, savedstate;
   int remaining;
   char *here = buffer;
   char *storage, *addhere, *rememberme, *charpointer;
   char lastquote;
   struct passwd *pwd;

   *argc = 0;

   if ((storage = (char *) malloc((strlen(buffer) + 265) * sizeof(char))) == NULL) {
      fprintf(stderr, "Could not malloc storage space !!");
      return -1;
   }
   *storage = '0';

   remaining = strlen(buffer);
   addhere = storage;

   while ((remaining) && (*argc < maxargc)) {
      switch (state) {
      case S_LWSP:
	 switch (*here) {
	 case ' ':
	 case '\t':
	 case '\n':
	    here++, remaining--;
	    break;
	 case '~':
/*            savedstate=state; */
	    rememberme = addhere;
	    state = S_TILDE;
	    *(addhere++) = *(here++);
	    remaining--;
	    break;
	 default:
	    state = S_WORD;
	    break;
	 }
	 break;
      case S_WORD:
	 switch (*here) {
	 case ' ':
	 case '\t':
	 case '\n':
	    state = S_LWSP;
	    *addhere = '\0';
	    if ((*(argv + *argc) =
		 (char *) malloc((strlen(storage) + 1) * sizeof(char))) == NULL) {
	       fprintf(stderr, "Could not malloc storage space !!");
	       return -1;
	    }
	    strcpy(*(argv + *argc), storage);
	    addhere = storage;
	    (*argc)++;
	    here++, remaining--;
	    break;
	 case '\\':
	    savedstate = state;
	    state = S_ESCAPED;
	    here++, remaining--;
	    break;
	 case '"':
	 case '\'':
	    state = S_QUOTED;
	    lastquote = *(here++);
	    remaining--;
	    break;
	 case '$':
/*            savedstate=state; */
	    rememberme = addhere;
	    state = S_DOLLAR;
	    *(addhere++) = *(here++);
	    remaining--;
	    break;
	 default:
	    *(addhere++) = *(here++);
	    remaining--;
	    break;
	 }
	 break;
      case S_QUOTED:
	 if (*here == '\\') {
	    savedstate = state;
	    state = S_ESCAPED;
	    here++, remaining--;
	 } else if (*here == lastquote) {
	    state = S_WORD;
	    here++, remaining--;
	 } else {
	    *(addhere++) = *(here++);
	    remaining--;
	 }
	 break;
      case S_ESCAPED:
	 *(addhere++) = *(here++);
	 remaining--;
	 state = savedstate;
	 break;
      case S_TILDE:
	 switch (*here) {
	 case '\\':
	    savedstate = state;
	    state = S_ESCAPED;
	    here++, remaining--;
	    break;
	 case '/':
	 case ' ':
	 case '\t':
	 case '\n':
	    state = S_WORD;
	    *addhere = '\0';
	    if (addhere == rememberme + 1)
	       pwd = getpwuid(getuid());
	    else
	       pwd = getpwnam(rememberme + 1);
	    if (pwd != NULL) {
	       strcpy(rememberme, pwd->pw_dir);
	       addhere = rememberme + strlen(rememberme);
	    }
	    break;
	 default:
	    *(addhere++) = *(here++);
	    remaining--;
	    break;
	 }
	 break;
      case S_DOLLAR:
	 switch (*here) {
	 case '\\':
	    savedstate = state;
	    state = S_ESCAPED;
	    here++, remaining--;
	    break;
	 case '\'':
	 case '\"':
	 case ' ':
	 case '\t':
	 case '\n':
	    state = S_WORD;
	    *addhere = '\0';
	    if (addhere == rememberme + 1)
	       charpointer = NULL;
	    else
	       charpointer = getenv(rememberme + 1);
	    if (charpointer != NULL) {
	       strcpy(rememberme, charpointer);
	       addhere = rememberme + strlen(rememberme);
	    }
	    break;
	 default:
	    *(addhere++) = *(here++);
	    remaining--;
	    break;
	 }
	 break;
      default:
	 fprintf(stderr, "Weirdness Factor 12\n");
	 return -1;
	 break;
      }
   }

   if (state == S_TILDE) {
      *addhere = '\0';
      if (addhere == (rememberme + 1))
	 pwd = getpwuid(getuid());
      else
	 pwd = getpwnam(rememberme + 1);
      if (pwd != NULL) {
	 strcpy(rememberme, pwd->pw_dir);
	 addhere = rememberme + strlen(rememberme);
      }
   } else if (state == S_DOLLAR) {
      *addhere = '\0';
      if (addhere == rememberme + 1)
	 charpointer = NULL;
      else
	 charpointer = getenv(rememberme + 1);
      if (charpointer != NULL) {
	 strcpy(rememberme, charpointer);
	 addhere = rememberme + strlen(rememberme);
      }
   }
   *addhere = '\0';

   if ((*argc < maxargc) && (strlen(storage) != 0)) {
      if ((*(argv + *argc) = (char *) malloc((strlen(storage) + 1) * sizeof(char))) == NULL) {
	 fprintf(stderr, "Could not malloc storage space !!");
	 return -1;
      }
      strcpy(*(argv + *argc), storage);
      addhere = storage;
      (*argc)++;
   }
   free((void *) storage);
   return *argc;
}

void handle_children(void)
{
   int pid;
   union wait status;
   while ((pid = wait3(&status, WNOHANG, (struct rusage *) 0)) > 0);
   return;
}



int processrcfile(char *rcfilename, enum rcfile_type rctype)
{
   char rcline[256];
   char *argv[128];
   char *rcfilename_full;
   struct rc_commands *this;
   int argc = 0;
   struct passwd *pwd;
   FILE *rcfp;

   switch (rctype) {
   case SYSTEM:
      rcfilename_full = rcfilename;
      break;
   case USER:
      pwd = getpwuid(getuid());
      if (pwd != (struct passwd *) NULL) {
	 rcfilename_full = (char *) malloc
	     ((strlen(pwd->pw_dir) + strlen(rcfilename) + 2) * sizeof(char));
	 if (rcfilename_full == (char *) NULL)
	    rcfilename_full = rcfilename;
	 else {
	    strcpy(rcfilename_full, pwd->pw_dir);
	    strcat(rcfilename_full, "/");
	    strcat(rcfilename_full, rcfilename);
	 }
      } else
	 rcfilename_full = rcfilename;

      break;
   }

   if (access(rcfilename_full, R_OK))
      return -1;

   if ((rcfp = fopen(rcfilename_full, "r")) == NULL) {
      fprintf(stderr, "%s: Could not open rcfile %s\n", progname, rcfilename_full);
      perror(progname);
      return -1;
   }
   while (fgets(rcline, 255, rcfp) != NULL) {
      if (parseline(rcline, &argc, argv, 127) > 0) {
	 if ((argc == 0) || (**argv == '#'))
	    continue;
	 *(argv + argc) = (char *) NULL;

	 for (this = recognised_rc_commands; (this->runrc) != NULL; this++) {
	    if (strcmp((this->command), *argv) == 0) {
	       if ((rctype == USER) &&
		   ((this->flags & rcflag_restricted) ||
		    (this->flags & rcflag_system_only)))
		  fprintf(stderr, "%s: Illegal use of restricted option %s\n",
			  progname, *argv);
	       else
		  (this->runrc) (argc, argv);

	       break;
	    }
	 }
	 if ((this->runrc) == NULL)
	    fprintf(stderr, "%s: %s Unrecognised command\n",
		    rcfilename_full, *argv);
      }
      for (; argc; --argc)
	 free((void *) *(argv + argc));
   }
   fclose(rcfp);
}

void rc_setenv(int argc, char **argv)
{
   if (argc != 3)
      fprintf(stderr, "setenv: Too %s arguments !\n",
	      argc < 3 ? "few" : "many");
   else
      setenv(*(argv + 1), *(argv + 2), 1);

   return;
}

void rc_unsetenv(int argc, char **argv)
{
   if (argc != 2)
      fprintf(stderr, "unsetenv: Too %s arguments !\n",
	      argc < 2 ? "few" : "many");
   else
      unsetenv(*(argv + 1));

   return;
}

void rc_exec(int argc, char **argv)
{
   pid_t pid_child;
   int child_stat;

   if (argc < 2)
      fprintf(stderr, "exec: Too few arguments !\n");

   argv++, argc--;

   do_exec((char *) NULL, argc, argv);

   return;
}

void rc_system(int argc, char **argv)
{
   fprintf(stderr, "In system\n");
}

void rc_pause(int argc, char **argv)
{
   char *printme;

   argv++, argc--;

   for (printme = *argv; argc; argv++, argc--)
      printf("%s ", *argv);
   printf("\n");

   fflush(stdin), fflush(stdout);
   while (getchar() != '\n');
   fflush(stdin);

   return;
}

void rc_sleep(int argc, char **argv)
{
   unsigned int sleeplength;

   if (argc != 2)
      fprintf(stderr, "sleep: Too %s arguments !\n",
	      argc < 2 ? "few" : "many");
   else if (sscanf(*(argv + 1), "%ud", &sleeplength) == 1)
      sleep(sleeplength);

   return;
}

void rc_logging(int argc, char **argv)
{
   rc_set_booloption(argc, argv, &exec_logging);

   return;
}

void rc_noclobber(int argc, char **argv)
{
   rc_set_booloption(argc, argv, &noclobber);

   return;
}

void rc_set_booloption(int argc, char **argv, int *booloption)
{
   char *option = *argv;

   argv++, argc--;

   switch (argc) {
   case 0:
      *booloption = *booloption ? 0 : 1;
      break;
   case 1:
      if ((strcasecmp(*argv, "y") == 0) || (strcasecmp(*argv, "on") == 0))
	 *booloption = 1;
      else if ((strcasecmp(*argv, "n") == 0) ||
	       (strcasecmp(*argv, "off") == 0))
	 *booloption = 0;
      else
	 fprintf(stderr, "%s: Unrecognised option %s\n", option, *argv);
      break;
   default:
      fprintf(stderr, "%s: Too many arguments !\n", option);
      break;
   }

   return;
}

void rc_restrict(int argc, char **argv)
{
   char *option;
   struct rc_commands *this;
   int except = 0;

   option = *argv;
   argv++, argc--;

   while (argc) {
      if ((strcmp(*argv, "except") == 0) && (!except))
	 except = 1;

      else if ((strcmp(*argv, "all") == 0) && (!except))
	 for (this = recognised_rc_commands; (this->runrc) != NULL; this++)
	    this->flags |= rcflag_restricted;

      else if ((strcmp(*argv, "none") == 0) && (!except))
	 for (this = recognised_rc_commands; (this->runrc) != NULL; this++)
	    this->flags &= (!rcflag_restricted);

      else {
	 for (this = recognised_rc_commands; (this->runrc) != NULL; this++)
	    if (strcmp((this->command), *argv) == 0) {
	       if (except)
		  this->flags &= (!rcflag_restricted);
	       else
		  this->flags |= rcflag_restricted;
	       break;
	    }
	 if ((this->runrc) == NULL)
	    fprintf(stderr, "%s: Unrecognised restriction %s\n",
		    option, *argv);
      }
      argv++, argc--;
   }
   return;
}
