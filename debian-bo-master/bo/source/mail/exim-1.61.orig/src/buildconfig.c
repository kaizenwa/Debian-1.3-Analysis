/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


/*************************************************
*       Build configuration header for Exim      *
*************************************************/

/* This auxiliary program builds the file config.h by the following
process:

First it reads makefile, looking for certain OS-specific definitions which it
uses to define macros. Then it reads the defaults file config.h.defaults.

The defaults file contains normal C #define statements for various macros; if
the name of a macro is found in the environment, the environment value replaces
the default. If the default #define does not contain any value, then that macro
is not copied to the created file unless there is some value in the
environment.

This program is compiled and run as part of the Make process and is not
normally called independently. */


#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  char *name;
  int *flag;
} have_item;

static int have_nis = 0;
static int have_nisplus = 0;
static int have_seteuid = 0;
static int have_setresuid = 0;
static int have_ipv6 = 0;

/* If any entry is an initial substring of another, the longer one must
appear first. */

static have_item have_list[] = {
  { "HAVE_NISPLUS",   &have_nisplus },
  { "HAVE_NIS",       &have_nis },
  { "HAVE_SETEUID",   &have_seteuid },
  { "HAVE_SETRESUID", &have_setresuid },
  { "HAVE_IPV6",      &have_ipv6 },
  { NULL, NULL}
};


int main(int argc, char **argv)
{
FILE *base;
FILE *new;
int last_initial = 'A';
int linecount = 0;
char errno_quota[256];
char buffer[256];

if (argc != 1)
  {
  printf("*** Buildconfig: called with incorrect arguments\n");
  exit(1);
  }

new = fopen("config.h", "w");
if (new == NULL)
  {
  printf("*** Buildconfig: failed to open config.h for output\n");
  exit(1);
  }

printf("Building configuration file config.h\n");

fprintf(new, "/*************************************************\n");
fprintf(new, "*           Configuration header for Exim        *\n");
fprintf(new, "*************************************************/\n\n");

fprintf(new, "/* This file was automatically generated from makefile and "
  "config.h.defaults,\n");
fprintf(new, "using values specified in the configuration file Local/Makefile.\n");
fprintf(new, "Do not edit it. Instead, edit Local/Makefile and "
  "rerun make. */\n\n");

/* First, search the makefile for certain settings */

base = fopen("makefile", "r");
if (base == NULL)
  {
  printf("*** Buildconfig: failed to open makefile\n");
  fclose(new);
  exit(1);
  }

errno_quota[0] = 0;    /* no over-riding value set */

while (fgets(buffer, 256, base) != NULL)
  {
  have_item *h;
  char *p = buffer + (int)strlen(buffer);
  linecount++;
  while (p > buffer && isspace(p[-1])) p--;
  *p = 0;
  p = buffer;
  while (isspace(*p)) p++;

  for (h = have_list; h->name != NULL; h++)
    {
    int len = (int)strlen(h->name);
    if (strncmp(p, h->name, len) == 0)
      {
      p += len;
      while (isspace(*p)) p++;
      if (*p++ != '=')
        {
        printf("*** Buildconfig: syntax error in makefile line %d\n", linecount);
        exit(1);
        }
      while (isspace(*p)) p++;
      if (strcmp(p, "YES") == 0 || strcmp(p, "yes") == 0) *(h->flag) = 1;
        else *(h->flag) = 0;   /* Must reset in case multiple instances */
      break;
      }
    }
  if (h != NULL) continue;

  if (strncmp(p, "ERRNO_QUOTA", 11) == 0)
    {
    p += 11;
    while (isspace(*p)) p++;
    if (*p++ != '=')
      {
      printf("*** Buildconfig: syntax error in makefile line %d\n", linecount);
      exit(1);
      }
    while (isspace(*p)) p++;
    strcpy(errno_quota, p);
    }
  }

fprintf(new, "#define HAVE_NIS              %s\n",
  have_nis? "TRUE" : "FALSE");
fprintf(new, "#define HAVE_NISPLUS          %s\n",
  have_nisplus? "TRUE" : "FALSE");
fprintf(new, "#define HAVE_IPV6             %s\n",
  have_ipv6? "TRUE" : "FALSE");
fprintf(new, "#define HAVE_SETEUID          %s\n",
  have_seteuid? "TRUE" : "FALSE");
fprintf(new, "#define HAVE_SETRESUID        %s\n",
  have_setresuid? "TRUE" : "FALSE");
fprintf(new, "#define mac_seteuid(a)        %s\n",
  have_seteuid? "seteuid(a)" : have_setresuid? "setresuid(-1,a,-1)" : "(-1)");
fprintf(new, "#define mac_setegid(a)        %s\n",
  have_seteuid? "setegid(a)" : have_setresuid? "setresgid(-1,a,-1)" : "(-1)");

if (errno_quota[0] != 0)
  fprintf(new, "\n#define ERRNO_QUOTA           %s\n", errno_quota);

fprintf(new, "\n");
fclose(base);


/* Now handle the macros listed in the defaults */

base = fopen("../src/config.h.defaults", "r");
if (base == NULL)
  {
  printf("*** Buildconfig: failed to open ../src/config.h.defaults\n");
  fclose(new);
  exit(1);
  }


while (fgets(buffer, 256, base) != NULL)
  {
  char name[256];
  char *value;
  char *p = buffer;
  char *q = name;
  char *s;

  while (*p == ' ' || *p == '\t') p++;

  if (strncmp(p, "#define ", 8) != 0) continue;

  p += 8;
  while (*p == ' ' || *p == '\t') p++;
  s = p;

  if (*p < last_initial) fprintf(new, "\n");
  last_initial = *p;

  while (*p && (isalnum(*p) || *p == '_')) *q++ = *p++;
  *q = 0;

  if ((value = getenv(name)) != NULL)
    {
    int len;
    *p = 0;
    len = 29 - (int)strlen(buffer);
    fprintf(new, "%s ", buffer);
    while(len-- > 0) fputc(' ', new);
    if (value[0] == '/') fprintf(new, "\"%s\"\n", value);
      else fprintf(new, "%s\n", value);
    if (strcmp(s, "LOG_FILE_PATH") == 0 ||
        strcmp(s, "PID_FILE_PATH") == 0)
      {
      int OK = 0;
      p = strstr(value, "%s");
      if (p != NULL) OK = strstr(p+2, "%s") == NULL;
      if (!OK)
        {
        printf("\n*** %s must contain precisely one occurrence of \"%%s\". "
          "Please review your configuration.\n\n/", s);
        return 1;
        }
      }
    }
  else
    {
    char *t = p;
    while (*p == ' ' || *p == '\t') p++;
    if (*p != '\n') fputs(buffer, new); else
      {
      *t = 0;
      if (strcmp(s, "SPOOL_DIRECTORY") == 0 ||
          strcmp(s, "BIN_DIRECTORY")   == 0 ||
          strcmp(s, "CONFIGURE_FILE")  == 0)
        {
        printf("\n*** %s has not been defined in any of the Makefiles in the\n"
          "    \"Local\" directory. "
          "Please review your configuration.\n\n", s);
        return 1;
        }
      fprintf(new, "/* %s not set */\n", s);
      }
    }
  }

fclose(base);

fprintf(new, "\n/* End of config.h */\n");
fclose(new);
return 0;
}

/* End of buildconfig.c */
