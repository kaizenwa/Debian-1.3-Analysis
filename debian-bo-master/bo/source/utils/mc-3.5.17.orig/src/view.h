#ifndef __VIEW_H
#define __VIEW_H

#define  HEX_EDIT               /* Trial of HexEdit for v3.5.10 */

#ifdef WANT_WIDGETS
/* The growing buffers data types */
typedef struct {
    unsigned char *data;
    int  present;		/* Unused, for DOS port maybe */
} block_ptr_t;

enum ViewSide {
    view_side_left,
    view_side_right
};

typedef struct {
    Widget widget;

    char *filename;		/* Name of the file */
    char *command;		/* Command used to pipe data in */
    int view_active;
    int have_frame;
    
    unsigned char *data;	/* Memory area for the file to be viewed */

    /* File information */
    int file;			/* File descriptor (for mmap and munmap) */
    FILE *stdfile;		/* Stdio struct for reading file in parts */
    int reading_pipe;		/* Flag: Reading from pipe(use popen/pclose) */
    long bytes_read;		/* How much of file is read */
    int mmapping;		/* Did we use mmap on the file? */

    /* Display information */
    long last;			/* Last byte shown */
    long last_byte;		/* Last byte of file */
    long first;			/* First byte in file */
    long bottom_first;	/* First byte shown when very last page is displayed */
				/* For the case of WINCH we should reset it to -1 */
    long start_display;		/* First char displayed */
    int  start_col;		/* First displayed column, negative */
#ifdef HEX_EDIT
    int  edit_cursor;           /* HexEdit cursor position in file */
    char hexedit_mode;          /* Hexidecimal editing mode flag */ 
    char nib_shift;             /* A flag for inserting nibbles into bytes */
    enum ViewSide view_side;	/* A flag for the active editing panel */
    int  file_dirty;            /* Number of changes */
    int  start_save;            /* Line start shift between Ascii and Hex */ 
    int  cursor_col;		/* Cursor column */
    int  cursor_row;		/* Cursor row */
    struct hexedit_change_node *change_list;   /* Linked list of changes */
#endif /* HEX_EDIT */

    int dirty;			/* Number of skipped updates */

    /* Mode variables */
    int hex_mode;		/* Hexadecimal mode flag */
    int bytes_per_line;		/* Number of bytes per line in hex mode */
    int viewer_magic_flag;	/* Selected viewer */
    int viewer_nroff_flag;	/* Do we do nroff style highlighting? */
    
    /* Growing buffers information */
    int growing_buffer;		/* Use the growing buffers? */
    block_ptr_t *block_ptr;	/* Pointer to the block pointers */
    int          blocks;	/* The number of blocks in *block_ptr */

    
    /* Search variables */
    int search_start;		/* First character to start searching from */
    int found_len;		/* Length of found string or 0 if none was found */
    char *search_exp;		/* The search expression */
    int  direction;		/* 1= forward; -1 backward */
    void (*last_search)(void *, char *);
                                /* Pointer to the last search command */
    int view_quit;		/* Quit flag */

    int monitor;		/* Monitor file growth (like tail -f) */
    /* Markers */
    int marker;			/* mark to use */
    int marks [10];		/* 10 marks: 0..9 */
    
#ifdef HAVE_TK
    /* Tk version, line cache */
    int  current_line;		/* The current screen line cached */
    char *cache;		/* Current cache */
    char *color_cache;		/* Attributes: keep in sync with cache */
    int  dest;			/* Index in the cache to write to */
    int  cache_len;		/* Length of the cache buffer -1 */
    int  last_col;		/* last column used */
    int  status_shown;		/* Have we show the file information? */
#endif

    int  move_dir;		/* return value from widget:  
				 * 0 do nothing
				 * -1 view previous file
				 * 1 view next file
				 */
    struct stat s;		/* stat for file */
} WView;

#define vwidth (view->widget.cols - (view->have_frame ? 2 : 0))
#define vheight (view->widget.lines - (view->have_frame ? 2 : 0))

/* Creation/initialization of a new view widget */
WView *view_new (int y, int x, int cols, int lines, int is_panel);
int view_init (WView *view, char *_command, char *_file, int start_line);
int view_file (char *filename, int normal, int internal);

/* Internal view routines */
void view_update (WView *view);
void view_labels (WView *view);
int view_event (WView *view, Gpm_Event *event,int *result);
void toggle_wrap_mode (WView *);
void toggle_hex_mode (WView *);
void goto_line (WView *);
void regexp_search_cmd (WView *);
void normal_search_cmd (WView *);

#endif

/* Command: view a file, if _command != NULL we use popen on _command */
/* move direction should be apointer that will hold the direction in which the user */
/* wants to move (-1 previous file, 1 next file, 0 do nothing) */
int view (char *_command, char *_file, int *move_direction, int start_line);

extern int mouse_move_pages_viewer;
extern int max_dirt_limit;
extern int wrap_mode;
extern int have_fast_cpu;
extern int default_hex_mode;
extern int default_magic_flag;
extern int default_nroff_flag;
extern int altered_hex_mode;
extern int altered_magic_flag;
extern int altered_nroff_flag;

void view_adjust_size ();

#ifdef HEX_EDIT

/* A node for building a change list on change_list */
struct hexedit_change_node {
   struct hexedit_change_node *next;
   long                       offset;
   unsigned char              value;
};

#endif /* HEX_EDIT */

#endif	/* __VIEW_H */
