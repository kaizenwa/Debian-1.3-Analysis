/*
 * display.c - all screen handling functions for the picture option of the
 * file system defragmenter.
 *
 * Copyleft (C) 1994 Alexey Vovenko (vovenko@ixwin.ihep.su)
 */

#include <sys/types.h>
#include <strings.h>
#include <stdlib.h>
#include <ncurses.h>
#include <stdarg.h>
#include <unistd.h>
#include <signal.h>

#include "display.h"

static WINDOW *map_w=NULL;
static WINDOW *legend=NULL,*stats=NULL, *st_line=NULL;

static int map_w_width;
static int map_w_height;
static ulong screen_cells,granularity;
static ushort *screen_map;
       
void _die(char *last_words) {
   fprintf(stderr,last_words);
   exit(1);
}

/* Job control handler is broken in ncurses, so we have to set our
 * own.
 */
void tstp_signal(int dummy) {
   endwin();
   signal(SIGTSTP,SIG_DFL);
   sigsetmask(0);
   /* Put us to stop */
   kill(getpid(),SIGTSTP);     
   
   /* Something has wakened us */
   signal(SIGTSTP,tstp_signal);
   wrefresh(curscr); 
}

void init_screen(ulong blocks) {
   initscr();
   cbreak();
   noecho();
   keypad(stdscr,TRUE);
   if (LINES<10 || COLS<20) 
      _die("Unable to determine screen size, set LINES and COLUMNS in environment\n");
      
   if (COLS < 70)
      _die("Need at lest 80-columns display to work\n");
      
   map_w_width  = COLS;
   map_w_height = LINES-7;
   map_w  = newwin(map_w_height,map_w_width,0,0);
   legend = newwin(6,0,LINES-7,COLS / 2);
   stats  = newwin(6,COLS / 2,LINES-7,0);
   st_line = newwin(0,0,LINES-1,0);
   if (!map_w || !legend || !stats || !st_line) 
          _die("allocating screen map");
   map_w_width  -= 2;         /* border */
   map_w_height -= 2;         /* border */

   screen_cells = map_w_width*map_w_height;
   granularity = blocks / screen_cells;
   if (blocks % screen_cells) 
      granularity++;
   if (granularity == 0)
      granularity = 1;
   
   screen_cells = blocks / granularity;

   screen_map = malloc(screen_cells*sizeof(*screen_map));
   
   if (screen_map == NULL) {
      done_screen(FALSE);
      _die("Out of memory\n");
   }
   memset(screen_map, 0 ,screen_cells);
   signal(SIGINT,SIG_IGN);      /* ignore keyboard interrupt for safety */
   signal(SIGTSTP,tstp_signal);
}

void done_screen(int wait_key) {
   if (!map_w)
        return;
   if (wait_key) {     
       stat_line("Press any key to quit");
       getch();
   }
   stat_line("");
   
   delwin(map_w);
   delwin(legend);        
   delwin(stats);        
   delwin(st_line);
   map_w = stats = st_line = NULL;
   endwin();
}

/* -------------------------- Legend window ------------------------------ */
static struct { 
  ushort attr;
  char  *name;
} legend_names[] = {
   { AT_SUPER, "S superblock"},
   { AT_GROUP, "G Group"},
   { AT_BITMAP,"M bitmap"},
   { AT_INODE, "I inodes"},
   { AT_KERNEL,"K kernel"},
   { AT_DIR,   "D directory"},
   { AT_REG,   "F file"},
   { AT_BAD,   "B bad block"},
   { AT_DATA,  " data block"},
   { 0,        ". free block"},
   { 0,NULL}
};

void display_legend(ushort attr) {
   int y=1,x=2,i;
   
   if (!voyer_mode) 
      return;
   box(legend,ACS_VLINE,ACS_HLINE);
   mvwaddstr(legend,0,COLS/4 - 4," Legend ");
   
   for (i=0; legend_names[i].name!=NULL; i++) {
      if (attr & legend_names[i].attr || legend_names[i].attr==0) {
          if (legend_names[i].attr == AT_DATA) {
              mvwaddch(legend,y++,x,ACS_DIAMOND);
              waddstr (legend," data block");
          }
          else
              mvwaddstr(legend,y++,x,legend_names[i].name);
          if (y>4) 
             if (x==2) {
                y = 1;
                x = 16;
             }
             else      
                break;   
      }
   }             
   if ((attr & AT_FRAG) && (y <= 3)) {   
      x += 2;
      mvwaddstr(legend,y++,x,"Fragmented files are");
      mvwaddstr(legend,y++,x,"highlighted");
   }   
   wrefresh(legend);
}

/* --------------------- Statistic window ------------------------------ */
#define MAX_COMMENTS 4
static int comments_count=0;
void add_comment(char *comment) {
   if (stats==NULL)
       puts(comment);
   else    
       if (comments_count < MAX_COMMENTS) 
          mvwaddstr(stats,++comments_count,2,comment);
}
       
void display_comments(char *title) {
   int i;
   if (stats==NULL) return;
   box(stats,ACS_VLINE,ACS_HLINE);
   i = strlen(title) / 2;
   if (i>0)
      mvwaddstr(stats,0,COLS/4 - i,title);
   if (comments_count < MAX_COMMENTS)
      mvwprintw(stats,++comments_count,2,"%d block%s in each screen cell",
                granularity,granularity==1 ? "" : "s");
   wrefresh(stats);
}
void clear_comments(void) {
   if (stats==NULL) return;
   comments_count = 0;
   wclear(stats);
}

/* -------------------------- Status line -------------------------------- */
void stat_line(const char *fmt, ...) {
   char s[256];
   va_list args;
   va_start(args,fmt);
   vsprintf(s,fmt,args);
   va_end(args);
   if (st_line != NULL) {
        wmove(st_line,0,0);
        waddstr(st_line,s);
        wclrtoeol(st_line);
        wrefresh(st_line);
   }
   else
        puts(s);
}

/* ------------------------- Disk map window ----------------------------- */
void set_attr(ulong block, ushort attr) {
   if (!voyer_mode) 
      return;
   block /= granularity;
   if (block > screen_cells) {
      fprintf(stderr,"set_map: block %lu out of range\n",block);
      return;
   }
   screen_map[block] |= attr; 
}

void clear_attr(ushort attr) {
   int i;
   if (!voyer_mode)
       return;
   if (attr == AT_READ) {
                     /* Hack: If we have read the block than its place 
                      * becomes empty. This is not always correct,
                      * because each cell represents more than 1 disk block.
                      */
      for (i = 0; i < screen_cells; i++) 
          if (screen_map[i] & AT_READ) 
              screen_map[i] &= ~(AT_READ | AT_DATA);
      return;       
   }
   if (attr == AT_WRITE) {
      for (i = 0; i < screen_cells; i++) 
          if (screen_map[i] & AT_WRITE) {
              screen_map[i] &= ~(AT_WRITE);
              screen_map[i] |= AT_DATA; 
          }    
      return;       
   }
   for (i = 0; i < screen_cells; i++) 
       screen_map[i] &= ~attr;
   update_display();    
}

void show_cell(int i) {
  if ((screen_map[i] & AT_FRAG) &&
      !(screen_map[i] & (AT_SUPER | AT_BITMAP | AT_INODE))) 
          wattron(map_w,A_BOLD);   
  else 
          wattroff(map_w,A_BOLD);   
  if (screen_map[i] & AT_SELECTED && 
      !(screen_map[i] & (AT_SUPER | AT_BITMAP | AT_INODE))) 
          wattron(map_w,A_REVERSE);   
  else 
          wattroff(map_w,A_REVERSE);   
                  
  /* A screen cell typically contains data for more than one block.
   * If the blocks are of different types we have to choose the
   * most important one to show it on the screen
   */
  if (screen_map[i] & AT_READ) {
      waddch(map_w,'R');
      return;
  }
  if (screen_map[i] & AT_WRITE) {
      waddch(map_w,'W');
      return;
  }
  if (screen_map[i] & AT_GROUP)
      waddch(map_w,'G');
  else      
     if (screen_map[i] & AT_SUPER)
         waddch(map_w,'S');
     else      
        if (screen_map[i] & AT_BITMAP)
            waddch(map_w,'M');
        else      
           if (screen_map[i] & AT_INODE)
               waddch(map_w,'I');
           else      
               if (screen_map[i] & AT_BAD)
                  waddch(map_w,'B');
               else   
                  if (screen_map[i] & AT_KERNEL)
                     waddch(map_w,'K');
                  else   
                     if (screen_map[i] & AT_DATA)
                         waddch(map_w,ACS_DIAMOND);
                     else
                        if (screen_map[i] & AT_DIR)
                            waddch(map_w,'D');
                        else
                           if (screen_map[i] & AT_REG)
                               waddch(map_w,'F');
                           else
                               waddch(map_w,'.');      
}
       
void update_display(void) {
   int i, row = 0;
   if (!voyer_mode)
        return;
   for (i = 0; i < screen_cells; i++) {
        if ((i % map_w_width) == 0)  {
             wmove(map_w,++row,1);
        }
        show_cell(i);             
   }      
   wrefresh(map_w);
}

void display_map(void) {
   if (!voyer_mode)
        return;
   box(map_w,ACS_VLINE,ACS_HLINE);
   update_display();
}
