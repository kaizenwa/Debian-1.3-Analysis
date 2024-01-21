/* Intrinsic functions */
#include "config.h"

#include <stdio.h>

#include "slang.h"
#include "_slang.h"

#define MAX_WINDOWS 256

static int Resize_Window_Msg = 0;
static int Redraw_Window_Msg = 1;
static int Destroy_Window_Msg = 2;
#define MAX_WINDOW_PROCS 3

typedef struct Window_Type
{
   int handle;
   int xmin, xmax, ymin, ymax;
   float64 wxmin, wxmax, wymin, wymax;   /* world coord system */
   unsigned long flags;
   struct Window_Type *child, *sister;
   
   SLang_Name_Type *win_proc[MAX_WINDOW_PROCS];   /* window procedures */
}
Window_Type;

Window_Type *Windows[MAX_WINDOWS];

void execute_window_proc (Window_Type *w, int msg)
{
   SLang_Name_Type *n;
   
   if (SLang_Error) return;
   if (NULL == (n = w->win_proc[msg]))
     {
	/* default procedure */
	return;
     }
   
   SLexecute_function (n);
}



int SLang_pop_4_integers (int *a, int *b, int *c, int *d)
{
   if (SLang_pop_integer (d)
       || SLang_pop_integer (c)
       || SLang_pop_integer (b)
       || SLang_pop_integer (a))
     return 1;
   return 0;
}

int SLang_pop_4_floats (float64 *a, float64 *b, float64 *c, float64 *d)
{
   int p2, p3;
   if (SLang_pop_float (d, &p2, &p3)
       || SLang_pop_float (c, &p2, &p3)
       || SLang_pop_float (b, &p2, &p3)
       || SLang_pop_float (a, &p2, &p3))
     return 1;
   return 0;
}


Window_Type *validate_window_handle(int handle)
{
   char err[] = "Window handle invalid.";
   Window_Type *w;
   
   if ((handle < 0) || (handle >= MAX_WINDOWS)
       || (NULL == (w = Windows[handle])))
     {
	SLang_doerror (err);
	return NULL;
     }
   return w;
}



/* define a window and returns a handle to the window.  Points are clipped
 * within the window
 */
int create_window (void)
{
   int xmin, xmax, ymin, ymax;
   Window_Type *w;
   int i;
   
   if (SLang_pop_4_integers(&xmin, &ymin, &xmax, &ymax)) return -1;
   
   for (i = 0; i < MAX_WINDOWS; i++)
     {
	if (Windows[i] == NULL) break;
     }
   if (i == MAX_WINDOWS) return -1;
   
   
   if (NULL == (w = (Window_Type *) SLMALLOC (sizeof(Window_Type)))) 
     {
	SLang_Error = SL_MALLOC_ERROR;
	return -1;
     }
   Windows[i] = w;
   
   /* zero the structure */
   SLMEMSET (w, 0, sizeof (Window_Type));
   
   w->handle = i;
   w->ymin = ymin;
   w->ymax = ymax;
   if (xmax < xmin) 
     {
	w->xmax = xmin;
	w->xmin = xmax;
     }
   else
     {
	w->xmin = xmin;
	w->xmax = xmax;
     }
   
   if (ymax < ymin)
     {
	w->ymax = ymin;
	w->ymin = ymax;
     }
   else
     {
	w->ymax = ymin;
	w->ymin = ymax;
     }
   
   w->wxmin = (float64) w->xmin;
   w->wymin = (float64) w->ymin;
   w->wxmax = (float64) w->xmax;
   w->wymax = (float64) w->ymax;
   
   return w->handle;
}



/* destroy window associated with handle */
void delete_window (void)
{   
   int handle;
   Window_Type *w;
   
   if (SLang_pop_integer (&handle)) return;
   if (NULL == (w = validate_window_handle(handle))) return;
   
   /* need to delete child windows */
   
   execute_window_proc (w, Destroy_Window_Msg);
   
   /* send message to all windows this one obscures to update their contents */
   
   Windows[handle] = NULL;
   SLFREE (w);
   return;
}

/* associates a world coordinate system with the window */
void define_world (void)
{
   int handle;
   float64 xmin, xmax, ymin, ymax;
   Window_Type *w;
   
   if (SLang_pop_integer (&handle) ||
       SLang_pop_4_floats (&xmin, &ymin, &xmax, &ymax)) return;

   if (NULL == (w = validate_window_handle(handle))) return;
   
   w->wxmin = xmin;
   w->wxmax = xmax;
   w->wymin = ymin;
   w->wymax = ymax;
}

void define_window_procedure (int *handle, int *msg, char *proc)
{
   SLang_Name_Type *n;
   Window_Type *w;
   if ((*msg < 0) || (*msg >= MAX_WINDOW_PROCS))
     {
	SLang_doerror ("Invalid window message.");
	return;
     }
   if (NULL == (n = SLang_get_function (proc)))
     {
	SLang_doerror ("Undefined function.");
	return;
     }
   if (NULL == (w = validate_window_handle(*handle))) return;
   
   w->win_proc[*msg] = n;
}


void show_window (int *handle)
{
   Window_Type *w;
   
   if (NULL == (w = validate_window_handle(*handle))) return;

   execute_window_proc (w, Resize_Window_Msg);
   execute_window_proc (w, Redraw_Window_Msg);
}

   
static SLang_Name_Type slwindow_table[] =
{
   MAKE_INTRINSIC(".create_window", create_window, INT_TYPE, 0),
   MAKE_INTRINSIC(".delete_window", delete_window, VOID_TYPE, 0),
   MAKE_INTRINSIC(".define_world", define_world, VOID_TYPE, 0),
   MAKE_INTRINSIC(".show_window", show_window, VOID_TYPE, 1),
   MAKE_INTRINSIC(".define_window_procedure", define_window_procedure, VOID_TYPE, 3),
   
   MAKE_VARIABLE(".RESIZE_WINDOW_MSG", &Resize_Window_Msg, INT_TYPE, 1),
   MAKE_VARIABLE(".REDRAW_WINDOW_MSG", &Redraw_Window_Msg, INT_TYPE, 1),
   MAKE_VARIABLE(".DESTROY_WINDOW_MSG", &Destroy_Window_Msg, INT_TYPE, 1),
   SLANG_END_TABLE
};

int init_SLwindows ()
{
   return SLang_add_table(slwindow_table, "_Windows");
}


   
