/****************************************************************************
 * This module is all original code 
 * by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 ****************************************************************************/
#include <FVWMconfig.h>

#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

#include "fvwm.h"
#include "menus.h"
#include "misc.h"
#include "parse.h"
#include "screen.h"
#include "module.h"

char *fvwm_file = NULL;
#include <fcntl.h>
void AddToModList(char *tline);

struct moduleInfoList
{
  char *data;
  struct moduleInfoList *next;
};

struct moduleInfoList *modlistroot = NULL;
int numfilesread = 0;

void ReadFile(XEvent *eventp,Window junk,FvwmWindow *tmp_win,
		unsigned long context, char *action,int* Module)
{
  char *filename= NULL,*Home, *home_file, *ofilename = NULL;
  char *rest,*tline,line[1000];
  int HomeLen;
  FILE *fd;
  int thisfileno;
  extern Bool Restarting;
  extern XEvent Event;

  thisfileno = numfilesread;
  numfilesread++;

  rest = GetNextToken(action,&ofilename);
  if(ofilename == NULL)
    return;

  filename = ofilename;

  fd = fopen(filename,"r");
  if((fd == NULL)&&(ofilename[0] != '/'))
    {
      /* find the home directory to look in */
      Home = getenv("HOME");
      if (Home == NULL)
	Home = "./";
      HomeLen = strlen(Home);
      home_file = safemalloc(HomeLen + strlen(ofilename)+3);
      strcpy(home_file,Home);
      strcat(home_file,"/");
      strcat(home_file,ofilename);
      filename = home_file;
      fd = fopen(filename,"r");      
    }
  if((fd == NULL)&&(ofilename[0] != '/'))
    {
      if((filename != NULL)&&(filename!= ofilename))
	free(filename);
      /* find the home directory to look in */
      Home = FVWMDIR;
      HomeLen = strlen(Home);
      home_file = safemalloc(HomeLen + strlen(ofilename)+3);
      strcpy(home_file,Home);
      strcat(home_file,"/");
      strcat(home_file,ofilename);
      filename = home_file;
      fd = fopen(filename,"r");      
    }

  if(fd == NULL)
    {
      if((ofilename != filename)&&(filename != NULL))
	{
	  free(filename);
	  filename = NULL;
	}
      if(ofilename != NULL)
	{
	  free(ofilename);
	  ofilename = NULL;
	}
      return;
    }
  if((ofilename != NULL)&&(filename!= ofilename))
     free(ofilename);
  fcntl(fileno(fd), F_SETFD, 1);
  if(fvwm_file != NULL)
    free(fvwm_file);
  fvwm_file = filename;

  tline = fgets(line,(sizeof line)-1,fd);
  while(tline != (char *)0)
    {
      int l;
      while(tline && (l=strlen(line))<sizeof(line) &&
           line[l-1]=='\n' && line[l-2]=='\\')
       {
         tline = fgets(line+l-2,sizeof(line)-l,fd);
       }
      tline=line;
      while(isspace(*tline))tline++;
      if((strlen(&tline[0])>1)&&(tline[0]!='#')&&(tline[0]!='*'))
	ExecuteFunction(tline,tmp_win,eventp,context,*Module);
      if(tline[0] == '*')
	AddToModList(tline);
      tline = fgets(line,(sizeof line)-1,fd);
    }              
  fclose(fd);
}

void AddToModList(char *tline)
{
  struct moduleInfoList *t, *prev, *this;

  /* Find end of list */
  t = modlistroot;
  prev = NULL;

  while(t != NULL)
    {
      prev = t;
      t = t->next;
    }
  

  this = (struct moduleInfoList *)safemalloc(sizeof(struct moduleInfoList));
  this->data = (char *)safemalloc(strlen(tline)+1);
  this->next = NULL;
  strcpy(this->data, tline);  
  if(prev == NULL)
    {
      modlistroot = this;
    }
  else
    prev->next = this;

}
      
void SendDataToModule(XEvent *eventp,Window w,FvwmWindow *tmp_win,
	      unsigned long context, char *action, int *Module)
{
  struct moduleInfoList *t;
  char message[256];
  extern char *IconPath;
#ifdef XPM
  extern char *PixmapPath;
#endif

  sprintf(message,"IconPath %s\n",IconPath);
  SendName(*Module,M_CONFIG_INFO,0,0,0,message);
#ifdef XPM
  sprintf(message,"PixmapPath %s\n",PixmapPath);
  SendName(*Module,M_CONFIG_INFO,0,0,0,message);
#endif
  sprintf(message,"ClickTime %d\n",Scr.ClickTime);
  SendName(*Module,M_CONFIG_INFO,0,0,0,message);

  t = modlistroot;
  while(t != NULL)
    {
      SendName(*Module,M_CONFIG_INFO,0,0,0,t->data);
      t = t->next;
    }  
  SendPacket(*Module,M_END_CONFIG_INFO,0,0,0,0,0,0,0,0);
}
