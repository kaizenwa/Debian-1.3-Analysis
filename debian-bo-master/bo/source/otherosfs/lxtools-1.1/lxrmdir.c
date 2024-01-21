/* --------------------------------------------------------------------
   Project: Communication package Linux-HPx00LX Filer
   Module:  lxrmdir.c
   Author:  A. Garzotto
   Started: 30. Nov. 95
   Subject: Delete a directory on the LX
   -------------------------------------------------------------------- */

/* --------------------------------------------------------------------
                             includes
   -------------------------------------------------------------------- */
#include <stdio.h>
#include <stdlib.h>

/* --------------------------------------------------------------------
                         local includes
   -------------------------------------------------------------------- */

#include "pal.h"
#include "palpriv.h"
#include "config.h"


/* --------------------------------------------------------------------
                        display help
   -------------------------------------------------------------------- */

static void help(void)
{
   fprintf(stderr, "USAGE: lxrmdir [options] <directory name>\n");
   fprintf(stderr, "  options: -<n> sets comm port <n>\n");
   fprintf(stderr, "           -b <baud> sets baud rate to <baud>\n");
   exit(1);
}

/* --------------------------------------------------------------------
                        make DOS directory string
   -------------------------------------------------------------------- */

static void makedir(char *dir)
{
   char *p = dir;
   
   while (*p)
   {
      if (*p == '/') *p = '\\';
      p++;
   }
}

/* --------------------------------------------------------------------
                               M A I N
   -------------------------------------------------------------------- */

int main (int argc, char **argv)
{
   int   stat;
   int port = 1;
   int speed = DEF_BAUD;
   FILERCOM *pFiler;
   char dir[256] = "";
   int i = 1, num = 0;
   int ret;
   
   fprintf(stderr, "LXRMDIR %s by A. Garzotto\n\n", VERSION);

   while (i < argc)
   {
      if (argv[i][0] == '-')
      {
	 switch (argv[i][1])
	 {
	  case '1':
	  case '2':
	  case '3':
	  case '4':
	  case '5':
	  case '6':
	  case '7':
	  case '8': port = argv[i][1] - '0'; break;
	  case 'B':
	  case 'b': speed = atoi(argv[++i]); break;
	  default: help(); break;
	 }
      }
      else
	 strcpy(dir, argv[i]);
      i++;      
   }
   if (!*dir) help();
   makedir(dir);
   
   if(!(pFiler = FilerConnect(port, speed, &FlCb))) {
      fprintf(stderr, "\nUnable to connect to palmtop!\n");
      exit(3);
   }

   stat = FilerAskDir(pFiler, dir);
   if(stat== NO_RESPONSE) fprintf(stderr, "\nServer Not responding.\n");

   while (1)
   {
      if (FilerGetDir(pFiler) == CANNOT_GET_ENTRY) break;
      if (pFiler->Attribute & 0x10) num++;
   }
   if (!num)
   {
      fprintf(stderr, "Directory not found: '%s'.\n", dir);
      ret = 1;
   }
   else if (num > 1)
   {
      fprintf(stderr, "Can only delete one directory at once.\n");
      ret = 1;
   }
   else
   {
      stat = FilerDelDir(pFiler, dir);
      if(stat==CANNOT_DELETE_DIR)
      {
	 fprintf(stderr, "\nCould not delete directory!\n");
	 ret = 1;
      }
      else
	 fprintf(stderr, "Directory '%s' is deleted.\n", dir);
    }
   FilerDisconnect(pFiler);
   exit(ret);
}

