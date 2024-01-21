/* Copyright (C) 1996 Luis Falcon (lfalcon@csun.edu)  
    
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#include <curses.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <signal.h>
#include <math.h>
#include "./defaults.h"

#define CMD_LINE  fprintf (stderr,"usage : netload device [-t secs] \n")

WINDOW *panel;
WINDOW *meter;
WINDOW *lever;
WINDOW *credit;
WINDOW *INFO;

int INIT = 0;
int old_rec=0;
int old_trans=0;
int temp_load=0;
int max_sofar=0;
char *current_time;

int check_arguments (int c, char *v[]);
void show_load (char *line, int speed,int dormant);
void reset (int);
char *update_watch ();


main (int argc, char *argv[])
{

FILE *source;
char *line;
char content [10000];
double filesize;
int dormant;
int speed;
int argument_length;
char mask [6];
check_arguments (argc, argv);

signal (SIGINT,reset);

if (argc > 2)
  dormant = atoi (argv [3]);
else
  dormant = 1;

if (strncmp (argv[1],"ppp",3) == 0)
   speed = PPP;
else
   speed = ETHER;

line = malloc (100*sizeof (char));

initscr ();
nonl();
intrflush(stdscr, FALSE);
keypad(stdscr, TRUE);


noecho ();
start_color ();

init_pair (COLOR_CYAN,COLOR_CYAN, COLOR_BLACK);
init_pair (COLOR_MAGENTA, COLOR_MAGENTA, COLOR_BLACK );
init_pair (COLOR_RED, COLOR_RED, COLOR_BLACK );
init_pair (COLOR_GREEN, COLOR_GREEN, COLOR_BLACK);
init_pair (COLOR_BLUE, COLOR_BLUE,COLOR_BLACK);

strcpy (mask,argv[1]);
argument_length = strlen (argv[1]);
mask [argument_length]=':';

panel = newwin (15,60,7,10);
meter = newwin (3,50,17,15);
lever = newwin (1,48,18,16);
credit = newwin (1,50,23,23);
INFO = newwin (5,60,1,10);

while (1)
  {
    if ((source=fopen ("/proc/net/dev","r")) == NULL)
	{
	fprintf (stderr, "Couldn't open source file\n");
	endwin ();
	}
    filesize = lseek (source,0,SEEK_END);

    fread (content,filesize,1,source);

    if ((line = strstr (content,mask)) == NULL)
      {
	fprintf (stderr,"Device \"%s\" not found\n",argv[1]);
	endwin ();
	exit (1);
      } 
  
  show_load (line,speed,dormant);
  lseek (source,0,SEEK_SET);
  fclose (source);

  sleep (dormant);
  }


}

int check_arguments (int c,char *v[])
{
if (c < 2 || c > 4 || c == 3)
	{
	CMD_LINE;
	exit (1);
	}
if (c==4 && strcmp (v[2],"-t") != 0)
        {
	CMD_LINE;
	exit (1);
        }
if (c==4 && strcmp (v[2],"-t") == 0 && atoi (v[3]) == 0)
    {
    fprintf (stderr, " Time must be >= 1 sec\n");
    exit (1);
    }

return (0);
}

void show_load (char *line,int speed,int dormant)
{

char device [5];
int packets,rpackets;
int    errors,drop,fifo,frame;
int    rerrors,rdrop,rfifo,rframe,carrier;
int marks;
int rec_load, trans_load;
int total_load;
int abs_totld;
int load_level;

    sscanf (line,"%[^:]: %d %d %d %d %d %d %d %d %d %d %d\n",&device,&rpackets,&rerrors,&rdrop,&rfifo,&rframe,&packets,&errors,&drop,&fifo,&frame,&carrier);

    wmove (INFO,1,1);
    wprintw (INFO,"\t\tPACKETS\tERRORS\tDROP\tFIFO\tFRAME");    
    wmove (INFO,3,1);
    wprintw (INFO,"REC\t\t%d\t  %d\t  %d\t  %d\t  %d",rpackets,rerrors,rdrop,rfifo,rframe);
    wmove (INFO,4,1);
    wprintw (INFO,"TRANS\t\t%d\t  %d\t  %d\t  %d\t  %d",packets,errors,drop,fifo,frame);
    rec_load = rpackets - old_rec; 
    old_rec = rpackets;

    trans_load = packets - old_trans;
    old_trans = packets;

    total_load = (rec_load + trans_load)/dormant;
    
    wattrset (credit,COLOR_PAIR(COLOR_BLUE));
    COLOR_PAIR (COLOR_BLUE);
    wprintw (credit,"By Luis Falcon (lfalcon@csun.edu)");
    wmove (credit,0,0);
    COLOR_PAIR (COLOR_CYAN);
    wattrset (meter,COLOR_PAIR (COLOR_RED));    
    box (meter,0,0);    

if (INIT == 0)
    current_time = update_watch ();
     

if (INIT != 0)
  {
    if (total_load > max_sofar)
	{
        max_sofar = total_load;
	current_time = update_watch ();
	} 
    werase (panel);
    werase (lever);
    wmove (panel,1,5);
    wprintw (panel,"Maximum device load %d (%s)",max_sofar,current_time);
    wmove (panel,2,1);
    whline (panel,0,58); 
    wmove (panel,4,14);
    wattrset (panel,A_BOLD);
    wprintw (panel, "Relative Load in %d sec(s) step",dormant);
    wmove (panel,6,15);
    wattrset (panel,COLOR_PAIR (COLOR_CYAN));
    wprintw (panel, "Received  \t\t %d",rec_load); 
 
    wmove (panel,7,15);
    wprintw (panel, "Transmitted\t\t %d",trans_load);

    wmove (panel,8,15);
    wprintw (panel, "Total load\t\t %d",total_load);

    wmove (panel,13,22);
    wprintw (panel, "device %s",device);

    wattrset (panel,0);
wrefresh (panel);
    
    load_level = total_load/speed;

    if (load_level > 47)
	load_level = 47;

    if (total_load > 0)      
	{
	if (load_level <=16)
		wattrset (lever, A_BOLD);
	if (load_level >16 && load_level < 32)
		{
		wattrset (lever,COLOR_PAIR (COLOR_GREEN));
		COLOR_PAIR (COLOR_GREEN);
		}
	if (load_level > 32)
		{
		wattrset (lever,COLOR_PAIR (COLOR_MAGENTA)); 
		COLOR_PAIR (COLOR_MAGENTA);
		}
      	for (marks=0;marks < load_level;marks++)	
			wechochar (lever,ACS_BLOCK); 
	}
	  
  }
wattrset (lever,0);
wrefresh (meter);

wrefresh (lever);

wrefresh (credit);
wrefresh (INFO);

INIT =1;
}

char *update_watch ()
{
time_t reloj;
char *time_holder;
time (&reloj);
time_holder = ctime (&reloj);
time_holder [24] =0;
return (time_holder);
}

void reset (int signal)
{
endwin ();
printf ("Later...\n");
exit (0);
}


