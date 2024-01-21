/********************************************************************\
**                             _________________________________    **
**   A n t h o n y            |________    __    __    _________|   **
**                                     |  |o_|  |o_|  |             **
**           T h y s s e n           __|   __    __   |__           **
**                                __|   __|  |  |  |__   |__        **
**  `` Dragon Computing! ''    __|   __|     |  |     |__   |__     **
**                            |_____|        |__|        |_____|    **
**                                                                  **
\********************************************************************/
/*
** user-menu.c
**
**   This module reads and handles a user configuration (RC) file defining
** a number of user menus.  The module is application indepentant only
** requiring application specific declarations at the top of the file.
** The following features are provided by this module.
**
**  * The "menu" keyword in the config file (see example below) is used
**    define the menu to which new menu items are to be added. Only the
**    menus named in the ``decl_menus'' structure below can be given by the
**    user. If the menu widget variable pointed to in this structure is
**    currently set to ``None'', then the module assumes it needs to create
**    the menu and does so, using the same menu name the user uses in the
**    config file.
**
**    The order in which menus are created in the config file is not
**    important, and it is even posible for the user to switch back and
**    forth between menus as desired. If the program did not pre-create the
**    menu and the user decided not to use that menu, that menu will not be
**    created.
**
**    If the programmer pre-created the menu widget pointed to, with some
**    items in it also pre-added, the user specified items will be appended
**    to these items.
**
**    It is up to the other parts of the application to popup and popdown
**    the created menus. The actual menu items added by the user however
**    will call an internal routine which calles the function sequence the
**    user wanted for this menu item. (see below)
**    
**  * Each item read from the config file call a number of functions whose
**    names are declared in the ``decl_functs'' structure below.  These
**    functions are to be defined in another module of the application. Each
**    function must be of type `void', and can have upto TWO string
**    arguments.  These arguments are given by the user in the menu
**    configuraion file and certian string substitutions are pre-performed
**    before the function is called (see below).
**
**    If the function fails for some reason and wants to abort the current
**    it can abort the current function sequence on this menu item, it can
**    do so by calling menu_item_abort() before it returns.  This abort
**    function will clear the current function sequence and free the menu
**    handler to accept new menu selections by the user.
**
**    A function that has the input flag set to TRUE is expected to popup a
**    dialog widget of some type before returning. The callbacks for this
**    input widget, after setting appropiate substition variables, is
**    expected to call either  menu_item_abort()  or  menu_item_continue()
**    as appropiate, just before it returns back to the mail application
**    processing loop.  This method will allow the application to ask the
**    user for some input or some confirmation before continuing the menu
**    function sequence, as set by the user in configuration file.
**
**  * The arguments are strings provided by the user in the config file.
**    Each is surrounded by either double quotes `"' or single quotes `''
**    and can contain any number of string substitution ( % ) escapes in a
**    simular fashion to the C library function printf().  These substition
**    escape characters are replaced by string variables, declared in the
**    ``decl_subchars'' structure below, before calling the appropiate
**    function. The substitution charcater will escape itself.
**
**  * The module also provides a `line' keyword as an menu item seperator
**    within the user menu config file. 
**
**  * External Functions provided are :-
**
**    read_user_menu( parent_widget, file_ptr, file_name)
**       Parse the open file, createing the user menus as declared in the
**       top section of this module.
**
**    menu_item_abort()
**       Notify the item handler not to continue the current menu_item
**       when the current routine returns.
**
**    menu_item_continue()
**       Continue processing user functions after the user has returned
**       from a input dialog user function. The only action the caller
**       should do after calling this function is to return back to the
**       main application loop. Any other action performed may have
**       undefined results.
**
** * FUTURE:-
**      * Figure out how to add `title' elements which do not highlight
**      * Addition of user defined Sub-Menus (what a lot of work).
**
** # Example application user menu configuration `RC' file.
** # This may not be valid for the current application.
** #
** # Any line starting with a `#' is a comment line and is ignored
** # Any lines ending with a `\' as the very last character of a 
** # line is continued onto the next line.
** #
** menu "usermenu" "User Menu"
** #   Create the menu `usermenu' if required. This name must match one of
** # declared menu names given below. Any other menu items following will
** # be added to this menu. The second string is optional but if present
** # will be added as a title for the menu.
** #
** line
** #  line adds extra line across the menus
** #
** item "Quit"		quit()
** #   Add a user menu item with the given functions to be called if
** # this menu item is selected.
** #
** item "Save"		save('%f')
** # function arguments are defined by the user but can have substitions
** # In this example  %f  is the current filename within the application
** #
** item "Save As"       input("Save As", '%f') save('%i')
** # Multiple functions can be called, and are executed in the order given.
** # Note that the arguments can use either double or single quotes.
** #
** # In this example input() is a external function which sets the %i to
** # some user input (somehow), while this is happing the current function
** # sequence is put on hold the input widget either continues or aborts
** # the sequence.
** #
** item "Load"   input("File to load", '') \
**               load('%i')
** #  a line can be continued on the next line by backslashing the EOL.
** #
*/
/* ====================================================================== */
/* =================  Application Specific Definitions  ================= */
/* ====================================================================== */

#include "xbmbrowser.h"
#include <ctype.h>

#define QUIT_APP    quit_browser   /* The applications quit routine */
#define SET_WAIT    set_busywait   /* routine to set a wait cursor */
#define CLEAR_WAIT  clear_busywait /* routine to clear a wait cursor */

#define MAX_STRING  1024           /* max length of a string argument */
#define SUBSTITION  '%'            /* the substitution character */
#define MAX_ARGS    2              /* this hardcoded in usr_item_handler() */

/* Declare the menus the users are allowed to define (though may not) */
/* WARNING: the first menu must be the main menu which must be defined */
static struct {
  char       *name;     /* name of this menu the user can define */
  Widget     *menu;     /* ptr to variable holding this user-menu */
} decl_menus[] = {
   { "main",      &menu_main      },  /* main menu -- menu button at top */
   { "global",    &menu_global    },  /* global menu -- button 2 */
   { "bitmap",    &menu_bitmap    },  /* bitmap menu -- but 3 on xbm files */
   { "pixmap",    &menu_pixmap    },  /* pixmap menu */
   { "directory", &menu_directory },  /* directory menu */
   { "other",     &menu_other     },  /* other files menu */
   { NULL, NULL }
};

/* Declare functions the user menu items can call */
/* WARNING: the first function must be the quit function */
static struct {  
  char       *name;        /* the name of function in the RC file */
  void      (*funct)();    /* the function Pointer */
  Cardinal    numargs;     /* number of arguments this function needs */
  Boolean     input;       /* this function performs a input popup sequence */
} decl_functs[] = {
   { "quit",     quit_browser,    0, False },   /* cleanup and exit program */
   { "scan",     scan_images,     0, False },   /* full directory scan */
   { "rescan",   rescan_images,   0, False },   /* fast dir rescan */
   { "chdir",    change_dir,      1, False },   /* change directory */
   { "exec",     exec_string,     1, False },   /* external command */
   { "confirm",  user_confirm,    1, True  },   /* user confirm dialog */
   { "input",    input_string,    2, True  },   /* user input dialog */
   { "selected", file_selected,   0, False },   /* abort on no selection */
   { NULL, NULL, 0 }
};


/* Declare the % substitutions in function arguments
** (NOTE %% is predefined) */
static struct {
  char    subchar;     /* % escaped substition character for function args */
  char   *string;      /* char ptr to substitution string */
} decl_subchars[] = {
   { 'h',  home_dir  },  /* %h = the users home directory */
   { 'd',  dir_name  },  /* %d = the current directory */
   { 'f',  file_name },  /* %f = the file selected in this directory */
   { 'b',  base_name },  /* %b = basename of the selected file */
   { 's',  suffix    },  /* %s = suffix of the selected file (EG: ".xbm" ) */
   { 'i',  input     },  /* %i = User input from input() function */
   { 'D',  init_dir  },  /* %D = initial startup directory */
   { '\0', NULL }
};
/* Full path of selected file  = "%d%f". Also  "%f" = "%b%s" */


/* ====================================================================== */
/* =============  End of Application Specific Definitions  ============== */
/* ====================================================================== */

/* --------------- Menu Handler and Argument Substition ----------------- */

/* define the functions and argument structure each menu item is to call */
typedef struct _Functs {
  void           (*funct)();      /* the function to be called */
  char            *arg[MAX_ARGS]; /* the argument strings the user gave */
  Boolean          input;         /* function popups up a input dialog */
  struct _Functs  *next;          /* next function and args in sequence */
} Functs;

/* --------------------------- */

/* current function list in progress */
static Functs  *next_function = NULL;    /* next function to be called */
static char    *item_in_progress = NULL; /* the menu item selected */

void
menu_item_abort()
/* Abort the current user-item-handler calling sequence */
{
  next_function = NULL;   /* no more functions need to be called */
}


void
menu_item_continue()
/* perferm the string substitions for function arguments and call the
** functions asked for by the user in sequence. This routine is also
** called by the return callbacks of any user input dialogs that
** one of these functions may have popped up.
*/
{
  char         arg[MAX_ARGS][MAX_STRING]; /* arguments to pass to user functs */
  char        *a, *d, *s;        /* string pointers arg, dest, sub-string */
  int          i, j, left, len;  /* temp variables */


  /* while functions are present in the current function sequence */
  SET_WAIT();
  while( next_function != NULL ) {

    /* prepare each argument */
    for( i = 0; i < MAX_ARGS; i++ ) {  
      left = MAX_STRING;          /* count of length of destinition string */
      a = next_function->arg[i];  /* user supplied argument pointer */
      d = arg[i];                 /* set the destination pointer */

      if( a == NULL ) {  /* no argument given */
        *d = '\0';           /* set empty string */
        continue;            /* goto next argument */
      }

      while( *a != '\0' ) {

        /* while next char is the substitution char -- substitute */
        while( *a == SUBSTITION ) {
          a++;  /* skip substitution char */
          /* check is it escapes another substition char */
          if ( *a == SUBSTITION ) {
            *d++ = *a++; continue;
          }
          /* find the external variable to substute */
          for( j = 0; decl_subchars[j].subchar; j++ )
            if( *a == decl_subchars[j].subchar )
              break;
          if( decl_subchars[j].subchar == '\0' ) {
            fprintf(stderr,
                "Substitution %c%c in menu item \"%s\" is invalid\n",
                     SUBSTITION,  *a,  item_in_progress );
            /* just abort current function sequence */
            menu_item_abort();
            goto menu_return;
          }
          a++;  /* skip subitution type character */
          len = strlen( decl_subchars[j].string );
          if( (left -= len) <= 0 )       /* test if it will fit */
            goto argument_overflow;
          strcpy(d, decl_subchars[j].string);
          d += len;   /* increment string pointers */
        }

        /* copy up to the next substition char */
        s = index( a, SUBSTITION );      /* scan for next substition char */
        len =  (s == NULL) ? strlen(a)   /* not found -- just copy rest */
                           : s - a;      /* copy upto the substition char */
        if( (left -= len) <= 0 )         /* test if it will fit */
          goto argument_overflow;
        strncpy(d, a, len);              /* copy from the user argument */
        d += len;                        /* increment string pointers */
        a += len;
      }
      *d = '\0';  /* finish of the string */
    } /* for each argument */

    /* FUNCTION CALL */
    /* Make the actual function call requested by the user */
    /* HARDCODED -- MAX_ARGS is limited by this bit of code */
    (next_function->funct)(arg[0], arg[1]);

    /* if the user called  menu_item_abort()  abort sequence now */
    if( next_function == NULL )
      goto menu_return;

    /* do we have to wait for a popup before continuing? */
    if( next_function->input ) {
      /* Presumably the function just called popped up a input dialog of
      ** some kind, and was successful in doing so.  OK, increment the
      ** next_function pointer and return to the main application loop,
      ** to wait for the dialog to re-call this routine, menu_item_continue(),
      ** when ready to continue the current function sequence.
      */
      next_function = next_function->next;
      goto menu_return;
    }

    /* increment to the next function (if any) */
    next_function = next_function->next;

  } /* while next function is not an empty list */

  /* finished */
  goto menu_return;

argument_overflow:  /* the argument after substitutions is too big! */
  fprintf(stderr, "Argument for user menu function overflows argument\n");
  fprintf(stderr, "buffer after all substutions are made. -- ABORTING\n" );
  /* just abort this function sequence */
  menu_item_abort();

menu_return:
  CLEAR_WAIT();
  return;
}


static void
menu_item_handler(widget, client_data, call_data)
/* Callback for the menu items. This will prepare for the current
** function sequence required by this menu item before calling the
** menu_item_continue() routine above, which actually performs the
** function sequence in order.
** NOTE: the function sequence to be called is expected in the call_data.
*/
  Widget  widget;
  XtPointer client_data, call_data;
{
  char *item_name;

  XtVaGetValues(widget, XtNlabel, &item_name, NULL);  /* get widgets name */

  if( next_function != NULL ) {
    fprintf(stderr, "User menu selection \"%s\" ignored.\n", item_name );
    fprintf(stderr,
           "\"%s\" is currently in progress, waiting for user input.\n",
           item_in_progress );
    return;
  }
  item_in_progress = item_name;
  next_function = (Functs *)client_data;
    
  /* start calling functions */
  menu_item_continue();
}


/* ---------------------- Parsing and Setup Code ------------------------- */

#define IS_SPACE(x)        ( (x)==' ' || (x)=='\t' || (x)=='\r' )
#define IS_ALPHA(x)        ( 'a'<=(x)&&(x)<='z' || 'A'<=(x)&&(x)<='Z' )
#define IS_DIGIT(x)        ( '0'<=(x)&&(x)<='9' )
#define IS_ALPHANUMERIC(x) ( IS_ALPHA(x) || IS_DIGIT(x) )

typedef enum { 
   KEYWORD, STRING, CHAR, EOL
} Token;

static FILE   *file_ptr;                    /* given filename being read */
static char   *configname;                  /* given filename being read */
static char    token_string[MAX_STRING+1];  /* the actual token read */
static int     line = 1;                    /* the current line number */
static Widget  cur_menu = NULL;             /* the current menu for items */
/* static Boolean  firstitem = FALSE;       /* item is the first in menu */

#ifdef PARSE
#define  PARSE_OUTPUT(a)  printf a 
#else
#define  PARSE_OUTPUT(a)  /**/
#endif


/* ----------------------------- */

static char
get()
/* Read a character from the RC file and update the current
** line number as appropiate. NOTE EOF is treated as an
** infinite End-Of-Line. The main loop tests EOF specifically.
*/
{ int c;

  c = fgetc(file_ptr);
  if( c == EOF || c == '\n' ) {
    c = '\n';   line++;
  }
  return c;
}


static void
unget( c )
/* push the character back to be retrived again latter
** Update the current line count as appropiately
*/
  char c;
{
  if( c == '\n' ) line--;
  ungetc( c, file_ptr );
}
  

/* Marco inline function for use within the get_token() function below */
#define read_token_string(test) \
  { int  i = 0;                 \
    while( c = get(), (test) && i < MAX_STRING ) \
      token_string[i++] = c;    \
    if( i >= MAX_STRING )       \
      goto token_overflow;      \
    token_string[i] = '\0';      \
  } 

static Token
get_token()
/* Read the given file stream and return the first valid token found.
** NOTE: This proceedure will just return EOL when the end of file is
** reached
*/
{
  char  c;   /* character just read (not int) */

retry_get_token:
  while( c = get(), IS_SPACE(c) ) ;   /* skip leading space */

  /* test for specific characters */
  switch( c ) {

    /* end-of-line and end-of-file conditions */
    case '\n':
      token_string[0] = '\n';
      token_string[1] = '\0';
      PARSE_OUTPUT(("EOL\n"));
      return EOL;

    /* comment skipping */
    case '#':    /* skip to next line */
      while( c = get(), c != '\n' ) ;  /* loop until EOL */
      unget( c );                      /* push the return back */
      goto retry_get_token;

    /* do special escape of the EOL */
    case '\\': /* get and test EOL */
      if( (c = get()) == '\n' ) 
        goto retry_get_token;
      /* not a EOL! -- return this char */
      unget( c ); /* unget bad char */
      c = '\\';   /* restore backslash */
      break;      /* exit to the CHAR return */
 
    /* single quoted string */
    case '\'': 
      /* skip the quote */
      read_token_string( c != '\'' );
      /* skip the quote */
      PARSE_OUTPUT(("STRING \"%s\"\n", token_string));
      return STRING;

    /* double quoted string */
    case '"': 
      /* skip the quote */
      read_token_string( c != '"' );
      /* skip the quote */
      PARSE_OUTPUT(("STRING \"%s\"\n", token_string));
      return STRING;
  }

  /* test if a keyword is found */
  if( IS_ALPHA(c) ) {
    unget( c ); /* this is the first char of token */
    read_token_string( IS_ALPHANUMERIC(c) );
    unget( c );    /* return this char to input stream */
    PARSE_OUTPUT(("KEYWORD \"%s\"\n", token_string));
    return KEYWORD;
  }

  /* At this point any unreconised character is just returned */
  token_string[0] = c;
  token_string[1] = '\0';
  PARSE_OUTPUT(("CHAR '%s'\n", token_string));
  return CHAR;

token_overflow:  /* handle the event that the current token is too big */
  token_string[40] = '\0';  /* limit the length of the long string */
  fprintf(stderr, "Token found in \"%s\" on line %d is too long to handle!\n",
                     configname, line );
  fprintf(stderr, "Token starts with -->%s...\n", token_string );
  QUIT_APP();
}
/* this marco is no longer needed */
#undef read_token_string


static void
parse_line()
{
  if( cur_menu == NULL ) {
    fprintf(stderr, "Missing menu declaration for line, \"%s\" at line %d.\n",
                        configname, line );
    QUIT_APP();
  }

  XtVaCreateManagedWidget(
           "line", smeLineObjectClass, cur_menu,
           XtNheight,     2,     /* allow multiple lines to be spaced */
           NULL );
}


static void
parse_menu(parent)
/* Parse a `menu' line and create the user menu pre-declared */
  Widget parent;   /* the parent of the menu */
{
  Token     token;
  int       i;              /* junk looping variable */
  Boolean   title = FALSE;  /* is a title present for this menu? */

  token = get_token();
  if( token != STRING ) {
    fprintf(stderr, "Menu name not found in \"%s\" at line %d.\n",
                        configname, line );
    QUIT_APP();
  }

  for( i = 0; decl_menus[i].name; i++ )
    if( strcmp( token_string, decl_menus[i].name ) == 0 )
      break;

  if( decl_menus[i].name == NULL ) {
    fprintf(stderr, "Unknown menu \"%s\", \"%s\" on line %d.\n",
                     token_string, configname, line );
    QUIT_APP();
  }

  switch ( get_token() ) {
  case STRING:  title = TRUE;
                break;
  case EOL:     break;  /* this is valid too */
  default:
    fprintf(stderr,
       "Menu \"%s\", \"%s\" is not a title or EOL, \"%s\" at line %d.\n",
            decl_menus[i].name, token_string, configname, line );
    QUIT_APP();
  }

  /* only create the actual menu if it is not already created */
  if( *(decl_menus[i].menu) == NULL )
    *(decl_menus[i].menu) = XtVaCreatePopupShell(
             decl_menus[i].name, simpleMenuWidgetClass, parent,
             XtNlabel,  title ? token_string : NULL,
             NULL );

  /* add new items to this menu */
  cur_menu = *(decl_menus[i].menu);

  /* add an extra line after a title */
  if( title ) parse_line();
}


static Functs *
parse_item_functs()
{
  Token     token;
  Functs   *functs = NULL;
  Functs  **lastlink = &functs;
  int       startline;
  int       i,j;   /* junk looping variables */

  /* while end of line is not reached */
  while( token = get_token(),  token != EOL ) {

    if( token != KEYWORD ) {
      fprintf(stderr, "Unknown function \"%s\", \"%s\" at line %d.\n",
                           token_string, configname, line );
      QUIT_APP();
    }
    startline = line;  /* save a copy of the line this function starts on */

    for( i = 0; decl_functs[i].name; i++ )
      if( strcmp( token_string, decl_functs[i].name ) == 0 )
        break;

    if( decl_functs[i].name == NULL ) {
      fprintf(stderr, "Unknown function \"%s\", \"%s\" on line %d.\n",
                     token_string, configname, startline );
      QUIT_APP();
    }

    if( decl_functs[i].numargs > MAX_ARGS ) {
      fprintf(stderr,
           "Programming Error, function \"%s\" has too many arguments\n",
                     token_string );
      QUIT_APP();
    }


    /* Malloc, set defaults and link the Functs Structure */
    *lastlink = (Functs *) XtMalloc( sizeof( Functs ) );
    (*lastlink)->next   = NULL;
    (*lastlink)->funct  = decl_functs[i].funct;
    (*lastlink)->input  = decl_functs[i].input;

    /* read in the argument strings supplied by the user */
    for( j = 0; j < MAX_ARGS; j++ )
      (*lastlink)->arg[j] = NULL;

    token = get_token();
    if( token != CHAR || token_string[0] != '(' ) {
      fprintf(stderr, "Function \"%s\" missing '(', \"%s\" at line %d.\n",
                          decl_functs[i].name, configname, startline );
      QUIT_APP();
    }

    /* gather the arguments */
    for( j = 0; j < decl_functs[i].numargs; j++ ) {

      /* get intervening ',' */
      if( j > 0 ) {
        token = get_token();
        if( token != CHAR || token_string[0] != ',' ) {
          if( token == CHAR && token_string[0] == ')' )
            fprintf(stderr,
               "Function \"%s\" requires %d arguments, \"%s\" at line %d.\n",
                decl_functs[i].name,  decl_functs[i].numargs,
                configname,  startline );
          else
            fprintf(stderr,
               "Missing ',' for function \"%s\", \"%s\" at line %d.\n",
                    decl_functs[i].name, configname, startline );
          QUIT_APP();
        }
      }
      
      /* get the argument string and save it in Functs call structure */
      token = get_token();
      if( token != STRING ) {
        fprintf(stderr,
               "Function \"%s\" requires %d arguments, \"%s\" at line %d.\n",
                decl_functs[i].name,  decl_functs[i].numargs,
                configname,  startline );
        QUIT_APP();
      }
      (*lastlink)->arg[j] = XtMalloc( strlen( token_string ) + 1 );
      strcpy( (*lastlink)->arg[j],  token_string );
    } /* for each arg */
 
    token = get_token();
    if( token != CHAR || token_string[0] != ')' ) {
      if ( token == STRING || token == CHAR && token_string[0] != ',' )
        fprintf(stderr,
               "Function \"%s\" only needs %d arguments, \"%s\" at line %d.\n",
                decl_functs[i].name,  decl_functs[i].numargs, 
                configname,  startline );
      else
        fprintf(stderr, "Function \"%s\" missing ')', \"%s\" at line %d.\n",
                          decl_functs[i].name, configname, startline );
      QUIT_APP();
    }

    /* move the pointer to the lastlink of Functs list */
    lastlink = &(*lastlink)->next;

  }  /* loop until EOL */

  return functs;
}


static void
parse_item()
{
  Token  token;
  Widget item;

  if( cur_menu == NULL ) {
    fprintf(stderr, "Missing menu declaration for item, \"%s\" at line %d.\n",
                        configname, line );
    QUIT_APP();
  }

  token = get_token();
  if( token != STRING ) {
    fprintf(stderr, "Item name not found in \"%s\" at line %d.\n",
                        configname, line );
    QUIT_APP();
  }

  /* create item for current menu */
  item = XtVaCreateManagedWidget(
           token_string, smeBSBObjectClass, cur_menu,
           XtNlabel,    token_string,
           XtNjustify,  XtJustifyLeft,
           NULL );
  XtAddCallback(item, XtNcallback, menu_item_handler, parse_item_functs());

}


void
read_user_menus(parent, rc, name)
/* Read the config file and setup all menus required
** from the given file name and descriptor
*/
  Widget  parent;   /* parent widget for user menus */
  FILE   *rc;       /* file to read config from */
  char   *name;     /* config filename for errors */
{
  Token  token;     /* the current token from config file */

  file_ptr   = rc;    /* save these for use by other paseing routines */
  configname = name;

  while( !feof(rc) ) {   /* while not end of file and no error */
    token = get_token();

    switch ( token ) {
    case EOL:
      continue;
    case STRING:
      fprintf(stderr, "Unexpected String \"%s\" in \"%s\" at line %d.\n",
            token_string, configname, line );
      QUIT_APP();
    case CHAR:
      fprintf(stderr, "Unexpected Character `%c' in \"%s\" at line %d.\n",
           token_string[0], configname, line );
      QUIT_APP();
    }

    /* handle KEYWORD */

    if( strcmp( token_string, "menu" ) == 0 ) {
      /* find and create this usermenu */
      parse_menu(parent);
      continue;
    }
    if( strcmp( token_string, "line" ) == 0 ) {
      /* find and create this usermenu */
      parse_line();
      continue;
    }
    if( strcmp( token_string, "item" ) == 0 ) {
      /* find and create this usermenu */
      parse_item();
      continue;
    }

    fprintf(stderr, "Unknown config line `%s' in \"%s\" at line %d.\n",
         token_string, configname, line );
    QUIT_APP();
  }


  /* If no user menu "main" was created -- create a plain quit menu */
  if ( *(decl_menus[0].menu) == NULL ) {
    Widget    item;
    Functs   *quit;
    int       j;

    fprintf(stderr, "WARNING: menu \"main\" not created in \"%s\"\n",
                       configname );
    fprintf(stderr, "         creating default quit menu\n");

    /* create default menu for "main" with a title */
    *(decl_menus[0].menu) = XtVaCreatePopupShell(
             decl_menus[0].name, simpleMenuWidgetClass, parent,
             XtNlabel,  "No Main Menu Found",
             NULL );
    cur_menu = *(decl_menus[0].menu);
    parse_line();

    /* create a quit menu item */
    item = XtVaCreateManagedWidget(
           token_string, smeBSBObjectClass, cur_menu,
           XtNlabel,    "Quit Application..",
           XtNjustify,  XtJustifyLeft,
           NULL );

    /* which calls the quit function (and on others) */
    quit = (Functs *) XtMalloc( sizeof( Functs ) );
    quit->next   = NULL;
    quit->funct  = decl_functs[0].funct;
    quit->input  = decl_functs[0].input;

    /* and which contains no arguments */
    for( j = 0; j < MAX_ARGS; j++ )
      quit->arg[j] = NULL;

    /* assign it to the item in the menu */
    XtAddCallback(item, XtNcallback, menu_item_handler, quit);
  }

}

