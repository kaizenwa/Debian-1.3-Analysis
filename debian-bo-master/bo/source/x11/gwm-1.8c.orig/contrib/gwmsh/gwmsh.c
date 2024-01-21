/* gwmsh.c - Simple minded gwm interface
 *
 * Copyright (C) 1994 Valeriy E. Ushakov
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, you can either send email to this
 * program's author (see below) or write to:
 * 
 *              The Free Software Foundation, Inc.
 *              675 Mass Ave.
 *              Cambridge, MA 02139, USA. 
 * 
 * Please send bug reports, etc. to uwe@niif.spb.su
 * 
 * Description:
 * 
 *   This programm is intended to be a simple line oriented interface
 * to GWM. Besides all, it was written for learning purposes (being my
 * first X hack). Canonical GNU Emacs have no interface to x11 so this
 * might be a kind of quick and dirty solution for this, thou this
 * part is yet to be written.
 * 
 * $Id: gwm.shar,v 1.115 1995/12/08 07:51:55 colas Exp $
 *
 * $Log: gwm.shar,v $
 * Revision 1.115  1995/12/08 07:51:55  colas
 * ********************
 * *** Version 1.8c ***
 * ********************
 *
 * Revision 1.100  1995/05/29  15:56:57  colas
 * simple-win.gwm: new parameters:
 *     label like simple-icon
 *     legend to place the label on sides of window
 *     lpad and rpad: number of () to pad the label with stretchable space
 * bar-max-wdths set by default to 1000
 *
 * John Carr <jfc@MIT.EDU>: patches to supress warnings on AIX/RS_6000/xlc
 * rxterm install fixed once more
 *
 * Revision 1.97  1995/05/16  16:16:36  colas
 * contrib/scripts/find-bar-nils
 *
# Revision 1.5  1995/05/15  22:29:34  colas
# bar can have abitrary shaped backgrounds (shaped tiles)
#
 * Revision 1.95  1995/05/11  17:06:56  colas
 * better spy
 *
 * Revision 1.93  1995/04/26  16:34:51  colas
 * Makefile added in distrib
 *
 * simple-icon.gwm:
 *
 *     - customize item "legend" can now be instead of () or t the strings:
 *       "top" "base" "right" "left" for the positions where you want the string
 *       to appear
 *       e.g: (customize simple-icon any XTerm "left")
 *
 *     - new customization item "label" to provide either a fixed string or a
 *       lambda which will be used to filter the label
 *       must return a non-empty string otherwise the unfiltered label is used
 *       e.g: to supress the Netscape: in netscape icon titles
 *       (customize simple-icon any Netscape
 *           label (lambdaq (s) (match "Netscape: \\(.*\\)$" s 1))
 *       )
 *
 * iconify a window doesnt not loose the window anymore in case of error in wool
 * code
 *
 * Revision 1.92  1995/04/25  14:31:09  colas
 * *** Version 1.7p_beta_2 ***
 *
 * Revision 1.3  1994/09/30  17:36:38  uwe
 * (get_window_propety_string): Is smarter now. It reads some portion of property and make
 * second query only if property value was not read completely at first try.
 * (xmalloc): New function. In case there's no -liberty.
 *
 * Revision 1.2  1994/09/21  20:50:49  uwe
 * Fixed typo in declaration of get_window_property_string argument dpy.
 * It's a Display* -- silly typo.
 *
 * Revision 1.1  1994/09/21  20:35:42  uwe
 * Initial revision
 *
 */

#ifdef sun4                     /* XXX: This belongs elsewhere */
#  define HAVE_ALLOCA_H
#endif

#ifdef HAVE_ALLOCA_H
#  include <alloca.h>
#endif
#include <signal.h>
#include <setjmp.h>

#include <ctype.h>
#if defined(USG) || defined(_POSIX_VERSION)
#  include <string.h>
#else
#  include <strings.h>
#endif
#include <stdio.h>
#include <getopt.h>
#include <varargs.h>

#include <X11/Xlib.h>
#include <X11/Xatom.h>

#ifdef USE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#else
char *readline ();
#endif

#ifndef __STDC__
#  ifdef __GNUC__
#    define const    __const__
#    define volatile __volatile__
#  else /* !__STDC__ && !__GNUC__ */
#    define const
#    define volatile
#  endif
#endif /* !__STDC__ */

#ifdef __GNUC__
#  define NORETURN __attribute__ ((noreturn))
#else
#  define NORETURN
#endif

/* Application name */
char *progname;

/* Command line options */
int under_emacs_p;
int query_only_p;
int use_WINDOWID_p;
char *gwm_single_command = NULL;

/* Type of windows to apply batch commands to */
int all_p;
int icon_p;
int main_p;
int mapped_p;
int stacking_p;
/* any of icon main mapped stacking*/
int all_specific_p;

/* X realted globals */
char *display_name = NULL;      /* Use $DISPLAY by default */

Display *dpy;                   /* Display */
Screen screen_num;              /* Screen number */
Window working_window;          /* Window to communicate with GWM */

Atom XA_GWM_RUNNING;            /* Whether gwm is running */
Atom XA_GWM_EXECUTE;            /* Text for gwm to execute */
Atom XA_GWM_RESULT;             /* To get eval result from gwm */

/* Shell functions */
char *get_gwm_form ();
void gwm_shell () NORETURN;     /* Shell motor function */
void gwmsh_eval_print ();
void gwmsh_eval_print_for_all ();

/* Communication */
Bool gwm_running_p ();
void gwm_execute ();
unsigned char *gwm_get_result ();

/* Window functions */
Bool window_valid_p ();
Bool find_window_in_hierarchy ();
unsigned char *get_window_property_string ();

/* Jump buffer to handle intrs while reading */
jmp_buf intr_while_reading;
void abort_result_waiting_loop ();

/* Auxilary staff */
char *save_optarg ();
volatile void barf () NORETURN;
volatile void die () NORETURN;

char *xmalloc ();
void free ();

/*
 * Function: usage
 *
 *   Prints out on stderr usage summary
 *
 * Returns:     Nothing
 * Parameters:  None
 */
void
usage ()
{
    fputs ("--all      -A   For all windows\n\
--command  -c   Execute the command.\n\
--emacs    -e   Run gwmsh from under emacs. Not intended for ordinary use.\n\
--help     -h   Print this usage and exit successfully.\n\
--icons    -I   For all icons\n\
--main     -W   For all main windows\n\
--mapped   -M   For all mapped\n\
--query    -q   Query if GWM is running\n\
--stacking -S   For all in stacking order\n\
--window   -w   Use $WINDOWID window to communicate with GWM.\n",
           stderr);
}

/*
 * Function: main
 *
 *   I'm not *THAT* bore.
 */
void
main (ac, av)
  int ac;
  char **av;
{
    char *s;

    progname = av[0];
    if (s = rindex (progname, '/'))
        progname = s+1;

    for (;;) {

        static const struct option long_option[] = {
          { "all",     no_argument,       0,               'A'},
          { "command", required_argument, 0,               'c'},
          { "display", required_argument, 0,               'd'},
          { "emacs",   no_argument,       &under_emacs_p,   1 },
          { "help",    no_argument,       0,               'h'},
          { "icon",    no_argument,       0,               'I'},
          { "main",    no_argument,       0,               'W'},
          { "mapped",  no_argument,       0,               'M'},
          { "query",   no_argument,       &query_only_p,    1 },
          { "stacking",no_argument,       0,               'S'},
          { "window",  no_argument,       &use_WINDOWID_p,  1 },
          { NULL, 0, 0, 0}
        };

        int option_index = 0;
        int c = getopt_long_only (ac, av, "c:ehqwAIMSW", long_option, &option_index);

        if (c == EOF)
            break;

    option_switch:
        switch (c) {

        case 0: /* Non-flag long option. */
            /*Pass it along to the code that handles short one */
            c = long_option[option_index].val;
            goto option_switch;
        case 1:
            break;
        case 'c':
            save_optarg (&gwm_single_command);
            break;
        case 'd': /* !!! This is NOT in short options !!! */
            save_optarg (&display_name);
            break;
        case 'e':
            under_emacs_p = 1;
            break;
        case 'h':
            usage ();
            die (0);
            break;
        case 'q':
            query_only_p = 1;
            break;
        case 'w':
            use_WINDOWID_p = 1;
            break;
        case 'A':
            all_p = 1;
            break;
        case 'I':
            icon_p = 1;
            break;
        case 'M':
            mapped_p = 1;
            break;
        case 'S':
            stacking_p = 1;
            break;
        case 'W':
            main_p = 1;
            break;
        case '?':
            usage ();
            die (1);
            break;
        default:
            barf ("panic, getopt returned 0%o\n", c);
        }
    }

    if (main_p && icon_p)
        barf ("-I and -W are mutually exclusive");

    all_specific_p = icon_p || main_p || mapped_p || stacking_p;
    if (all_p && all_specific_p)
        barf ("-A should be the only option");

    dpy = XOpenDisplay (display_name);
    if (dpy == NULL)
        barf ("Cannot open display %s", XDisplayName (display_name));

    /* Check for GWM running */
    if (!gwm_running_p ())
        barf ("GWM is not running on %s", XDisplayName(display_name));
    else if (query_only_p)
        die (0);

    /* Get window we will use to communicate to GWM */
    if (use_WINDOWID_p) {
        extern char *getenv ();
        char *wid = getenv ("WINDOWID");
        if (!wid)
            barf ("No WINDOWID environment variable");
        if (!sscanf (wid, "%ld", &working_window))
            barf ("corrupted $WINDOWID = %ld", wid);
        if (!window_valid_p (working_window))
            barf ( "Invalid window %ld", working_window);
    }
    else {
        working_window = DefaultRootWindow (dpy);
    }

    /* Create atoms to comunicate with GWM */
    XA_GWM_EXECUTE = XInternAtom (dpy, "GWM_EXECUTE", True);
    if (XA_GWM_EXECUTE == None)
        barf ("Can't intern GWM_EXECUTE");
    XA_GWM_RESULT = XInternAtom (dpy, "GWM_RESULT", False);
    if (XA_GWM_RESULT == None)
        barf ("Can't intern GWM_RESULT");

    XSelectInput (dpy, working_window, PropertyChangeMask);

    signal (SIGINT, SIG_IGN);

    /* Ok, we are ready to pass commands to GWM */
    if (all_p || all_specific_p) {
        /* XXX: More consistent batch commands parsing*/
        char *spec = alloca (100); /* XXX: Magic number here */
        spec [0] = '\0';
        if (icon_p)
            strcat (spec, " 'icon");
        else if (main_p)
            strcat (spec, " 'window");
        if (mapped_p)
            strcat (spec, " 'mapped");
        if (stacking_p)
            strcat (spec, " 'stacking-order");

        if (gwm_single_command)
            gwmsh_eval_print_for_all (spec, gwm_single_command);
        else 
            /* XXX: Would wrapping them all together be more logical? */
            while (optind < ac)
                gwmsh_eval_print_for_all (spec, av[optind++]);
    }
    else if (gwm_single_command)
        gwmsh_eval_print (gwm_single_command);
    else
        gwm_shell ();

    die (0);
}

/*
 * Function: gwm_running_p
 *
 *   Check if gwm is running on the display
 *
 * Returns:     Bool
 - 
 * Parameters:  None
 */
Bool
gwm_running_p ()
{
    /* Hidden window GWM creates */
    Window gwm_window;


    Status result;
    Atom actual_type;
    int actual_format;
    unsigned long nitems;
    unsigned long bytes_after;
    Window *gwm_window_id_ptr;

    /* Check for GWM_RUNNING presence */
    XA_GWM_RUNNING = XInternAtom (dpy, "GWM_RUNNING", True);
    if (XA_GWM_RUNNING == None)
        return (False);

    /* Make shure that GWM_RUNNING is valid */
    result = XGetWindowProperty (dpy, DefaultRootWindow (dpy), XA_GWM_RUNNING,
                                 0l, 1l, False, XA_GWM_RUNNING,
                                 &actual_type, &actual_format,
                                 &nitems, &bytes_after,
                                 (unsigned char **) &gwm_window_id_ptr);
    if (!((result == Success)
          && (actual_type == XA_GWM_RUNNING)
          && (actual_format == 32)
          && (nitems == 1)
          && (bytes_after == 0)))
        barf ("XGetWindowProperty failed to read GWM_RUNNING");

    gwm_window = *gwm_window_id_ptr;
    XFree ((void *) gwm_window_id_ptr);

    /* Well, make shure gwm_window exists */
    return (window_valid_p (gwm_window));
}

/*
 * Function: gwm_shell
 *
 *   Shell read - pass to gwm - read from gwm - print loop
 *
 * Returns:     Never
 * Parameters:  None
 */
void
gwm_shell ()
{
    char *form;

    for (;;) {
        form = get_gwm_form ("gwm> ");
        if (!form) {
            putchar ('\n');
            die (0);
        }
        gwmsh_eval_print (form);
    }
}

/*
 * Function: gwmsh_eval_print
 *
 *   Prints out result of text evaluation.
 *
 * Returns:     Nothing
 * Parameters:  text - gwm form to pass to gwm_execute after wrapping
 * it so that result would come to us via GWM_RESULT property.
 */
void
gwmsh_eval_print (text)
  char *text;
{
    char *gwm_command;
    char *gwm_result;

    gwm_command = alloca (strlen (text) + 200); /* XXX: Magic numer here */
    sprintf (gwm_command,
"(set-x-property \"GWM_RESULT\"\
 (with-output-to-string\
 (if (error-occurred (? %s))\
 (? \"Wool Error\"))))", text); 
    gwm_execute (gwm_command, False);
    gwm_result = gwm_get_result ();
    if (gwm_result) {
        fputs (gwm_result, stdout);
        fputc ('\n', stdout);
        free (gwm_result);
    }
}

/*
 * Function: gwmsh_eval_print_for_all
 *
 *   Wrap up text in the loop thru all windows and pass it to
 * gwmsh_eval_print.
 *
 * Returns:     Nothing
 * Parameters:  spec - Loop thru this kind of windows only.
 *              text - Command to be executes for these windows.
 */
void
gwmsh_eval_print_for_all (spec, text)
  char *spec;
  char *text;
{
    char *gwm_command;

    gwm_command = alloca (strlen (text) + 50); /* XXX: magic number here */
    sprintf (gwm_command, "(for window (list-of-windows %s) %s)", spec, text);
    gwmsh_eval_print (gwm_command);
}

/*
 * Function: gwm_execute
 *
 *   Pass text to gwm via GWM_EXECUTE property. Sync with server
 * according to sync_p flag.
 *
 * Returns:     Nothing
 * Parameters:  text   - Pointer to gwm form to be passed to gwm
 *              sync_p - Should we sync with server
 */
void
gwm_execute (text, sync_p)
  char *text;
  Bool sync_p;
{
    XChangeProperty (dpy, working_window, XA_GWM_EXECUTE,
                     XA_STRING, 8, PropModeReplace,
                     text, strlen (text) + 1);
    if (sync_p)
        XSync (dpy, False);
}

/*
 * Function: abort_result_waiting_loop (signal hanldler).
 *
 *   Aborts the gwm_get_result event waiting loop upon SIGINT.
 *
 * Returns:     Nothing
 * Parameters:  None
 */
void
abort_result_waiting_loop ()
{
    longjmp (intr_while_reading, 1);
}

/*
 * Function: gwm_get_result
 *
 *   Looks for the result returned by gwm in GWM_RESULT property.
 *
 * Returns:     char *
 -
 * Parameters:  None
 */
unsigned char *
gwm_get_result ()
{
    XEvent ev;
    unsigned char *gwm_result = NULL;

    if (setjmp (intr_while_reading)) {
        signal (SIGINT, SIG_IGN);
        gwm_result = "Inerrupted";
        goto deliver_result;
    }

    signal (SIGINT, abort_result_waiting_loop);

    /* Get evaluation result from GWM_RESULT property */
    for (;;) {
        XNextEvent (dpy, &ev);
        switch (ev.type) {

        case PropertyNotify:

            if ((ev.xproperty.atom  == XA_GWM_RESULT)
                && (ev.xproperty.state == PropertyNewValue)) {

                gwm_result = get_window_property_string (dpy, working_window,
                                                         XA_GWM_RESULT, True);
                if (!gwm_result)
                    fputs ("Error reading GWM_RESULT.\n", stderr);
                goto deliver_result;
            }
            break; /* to next event */

        default:
            fprintf (stderr, "Unexpected event %d ignored\n", ev.type);
        }
    }
 deliver_result:
    signal (SIGINT, SIG_IGN);
    return gwm_result;
}

/* --------------------------------------
 * Utility functions to deal with windows
 */

/*
 * Function: get_window_property_string
 *
 *   Looks for the given property of type XA_STRING and arbitrary
 * length. If anyone know better way of doing this, please fix.
 *
 * Returns:     char *
 - 
 * Parameters:  dpy      -  Display
 *              w        -  Window
 *              prop     -  Property to be read
 *              delete_p -  Delete it?
 */
#define INITIAL_QUERY_LEN 20l

unsigned char *
get_window_property_string (dpy, w, prop, delete_p)
  Display *dpy;                 /* XXX: shadows global dpy */
  Window w;
  Atom prop;
  Bool delete_p;
{
    Status retcode;
    Atom actual_type;
    int actual_format;
    unsigned long nitems;
    unsigned long bytes_after;
    unsigned char *buf = NULL;

    unsigned long len;
    unsigned long already_read;
    unsigned long real_size_in_bytes;

    unsigned char *prop_val;

    retcode = XGetWindowProperty (dpy, w, prop,
                                  0l, INITIAL_QUERY_LEN, False, XA_STRING,
                                  &actual_type, &actual_format,
                                  &nitems, &bytes_after,
                                  &buf);
    if ((actual_type != XA_STRING)
        || (actual_format != 8)) {
        XFree (buf);
        return NULL;
    }

    real_size_in_bytes = nitems + bytes_after;
    already_read = nitems;
    prop_val = (unsigned char *) xmalloc (real_size_in_bytes + 1);
    memcpy (prop_val, buf, nitems + 1); /* Including trailing null! */
    XFree (buf);

    if (!bytes_after)
        return prop_val;

    len = (bytes_after + 3) / 4;
    retcode = XGetWindowProperty (dpy, w, prop,
                                  INITIAL_QUERY_LEN, len, delete_p, XA_STRING,
                                  &actual_type, &actual_format,
                                  &nitems, &bytes_after,
                                  &buf);
    if ((retcode != Success)
        || (actual_type != XA_STRING)
        || (actual_format != 8)) {
        XFree (buf);
        free (prop_val);
        return NULL;
    }

    memcpy (prop_val + already_read, buf, nitems + 1);
    XFree (buf);
    return prop_val;
}

/*
 * Function: window_valid_p
 *
 *   Whether gived window id is valid
 *
 * Returns:     Bool
 - 
 * Parameters:  w - Window to validate
 */
Bool
window_valid_p (w)
  Window w;
{
    /* XXX: Any simpler way to walidate window ??? */
    return find_window_in_hierarchy (DefaultRootWindow (dpy), w);
}

/*
 * Function: find_window_in_hierarchy
 *
 *   Status of search for the given window in the hierarchy
 *
 * Returns:     Bool
 - 
 * Parameters:  hier_root  - Window hierarchy root
 *              sought_win - Window sought
 */
Bool
find_window_in_hierarchy (hier_root, sought_win)
  Window hier_root;
  Window sought_win;
{
    int found = False;
    int child_no;

    Window root_ignored;
    Window parent_ignored;
    Window *child;
    int n_children;

    XQueryTree (dpy, hier_root, &root_ignored, &parent_ignored, 
                &child, &n_children);

    /* Search it breadth first, for sought_win is most likely the top one */
    for (child_no = 0; child_no < n_children; ++child_no)
        if (child[child_no] == sought_win) {
            found = True;
            goto done;
        }

    /* Hmm. It's not here try to recurse */
    for (child_no = 0; child_no < n_children; ++child_no)
        if (find_window_in_hierarchy (child[child_no], sought_win)) {
            found = True;
            goto done;
        }
 done:
    XFree ((void *) child);
    return (found);
}

/* --------------------------------
 * Misc functions to make life easy
 */

/*
 * Function: get_gwm_form
 *
 *   Generic wrapper for read. One day it might look after parens and
 * allow multiple line commands.
 *
 * Returns:     char *
 - 
 * Parameters:  prompt
 */
char *
get_gwm_form (prompt)
  char *prompt;
{
    static char *line_read = NULL;
    register char *s;

    if (line_read) {
        free (line_read);
        line_read = NULL;
    }
    s = line_read = readline (prompt);
    if (s) {
        while (isspace (*s))
            ++s;
        if (*s)
            add_history (s);
    }
    return s;
}

#ifndef USE_READLINE

#define BUFSIZE 1000

/*
 * Function: readline
 *
 *   Cheap plastic imitation of GNU readline.
 *
 * Returns:     char *
 - 
 * Parameters:  prompt 
 */
char *
readline (prompt)
  char *prompt;
{
    char buf[BUFSIZE+2];
    if (prompt)
        fputs (prompt, stdout);
    return fgets (buf, BUFSIZE, stdin);
}

#endif /* !USE_READLINE */

/*
 * Function: save_optarg
 *
 *   Rather silly function that just copies global optarg where told.
 *
 * Returns:     char *
 - 
 * Parameters:  to - place to copy to
 */
char *
save_optarg (to)
  char **to;
{
    *to = xmalloc (strlen (optarg) + 1);
    return strcpy (*to, optarg);
}

/*
 * Function: barf
 *
 *   Barf and die.
 *
 * Returns:     Never
 * Parameters:  Those of printf
 */
volatile void
barf (va_alist)
  va_dcl
{
    va_list ap;
    register char *format;

    fprintf (stderr, "%s: ", progname);

    va_start (ap);

    format = va_arg (ap, char *);
    vfprintf (stderr, format, ap);
    va_end (ap);
    fputc ('\n', stderr);

    die (1);
}

/*
 * Function: die
 *
 *   Nothing is permanent.
 *
 * Returns:     Never
 * Parameters:  code - to return upon exit
 */
volatile void
die (code)
  int code;
{
    if (dpy != NULL)
        XCloseDisplay (dpy);
    exit (code);
}
#ifdef NO_LIBERTY

/*
 * Function: xmalloc
 *
 *   Malloc wrapper. Exits on malloc failure.
 *
 * Returns:     char *
 - 
 * Parameters:  size -  size
 */
char *
xmalloc (size)
  unsigned size;
{
    char *malloc ();
    char *s = malloc (size);
    if (!s)
        barf ("Out of core");
    return s;
}
#endif
