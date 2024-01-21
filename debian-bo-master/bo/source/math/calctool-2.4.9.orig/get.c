
/*  @(#)get.c 1.8 89/12/13
 *
 *  Command line and help file routines.
 *
 *  Copyright (c) Rich Burridge.
 *                Sun Microsystems, Australia - All rights reserved.
 *
 *  Permission is given to distribute these sources, as long as the
 *  copyright messages are not removed, and no monies are exchanged.
 *
 *  No responsibility is taken for any errors or inaccuracies inherent
 *  either to the comments or the code of this program, but if
 *  reported to me then an attempt will be made to fix them.
 */

#include <stdio.h>
#include <strings.h>
#include <sys/param.h>
#include <pwd.h>
#include "patchlevel.h"
#include "calctool.h"
#include "color.h"
#include "extern.h"


char *
convert(line)              /* Convert .calctoolrc line to ascii values. */
char *line ;               /* Input line to be converted. */
{
  static char output[MAXLINE] ;   /* Converted output record. */
  int ctrl = 0       ;     /* Set if we are processing a control character. */
  int i ;                  /* Position within input line. */
  int n = 0 ;              /* Position within output line. */

  for (i = 0; i < strlen(line); i++)
    {
           if (line[i] == ' ') continue ;
      else if (line[i] == '\\') ctrl = 1 ;
      else if (ctrl)
        {
          output[n++] = CCTRL(line[i]) ;
          ctrl = 0 ;
        }
      else output[n++] = line[i] ;
    }
  output[n] = '\0' ;
  return(output) ;
}


get_options(argc, argv)           /* Extract command line options. */
int argc ;
char *argv[] ;
{
  char next[MAXLINE] ;    /* The next command line parameter. */

  STRCPY(helpname, HELPNAME) ;    /* Default help filename. */
  STRCPY(x11_display, "") ;       /* Initially no X11 display value. */
  STRCPY(geometry, "") ;          /* Initialy no X11 geometry value. */
  accuracy = 2 ;                  /* Initial accuracy. */
  inv_video = 0 ;                 /* Default is normal video. */
  INC ;
  while (argc > 0)
    {
      if (argv[0][0] == '-')
        switch (argv[0][1])
          {
            case 'a' : INC ;
                       getparam(next, argv, "-a needs accuracy value") ;
                       accuracy = atoi(next) ;
                       if (accuracy < 0 || accuracy > 9)
                         {
                           FPRINTF(stderr,
                                   "%s: accuracy should be in the range 0-9\n",
                                   progname) ;
                           accuracy = 2 ;
                         }
                       break ;
            case 'd' : INC ;                 /* X11 display information. */
                       getparam(x11_display, argv, "-d needs display information") ;                       break ;
            case 'g' : INC ;                 /* X11 geometry information. */
                       getparam(geometry, argv, "-g needs geometry information") ;
                       break ;
            case 'h' : INC ;
                       getparam(helpname, argv, "-h needs helpfile name") ;
                       break ;
            case 'i' : inv_video = 1 ;
                       break ;
            case 'v' : FPRINTF(stderr, "%s version 2.4.%1d\n", progname, PATCHLEVEL) ;
                       break ;
/*  SunView windowing arguments. -Wp, -WP and -Wi are used in the NeWS
 *  implementation to initially position the window and icon.
 */

            case 'W' : switch (argv[0][2])
                         {
                           case 'H' : break ;   /* -WH, no sub-args follow */
                           case 'i' : iconic = 1 ;
                                      break ;   /* -Wi, start as an icon. */
                           case 'g' :           /* -Wg, set default color. */
                           case 'n' : break ;   /* -Wn, no label at all */
                           case 'h' :           /* -Wh, height */
                           case 'I' :           /* -WI "icon filename" */
                           case 'l' :           /* -Wl "some window label" */
                           case 'L' :           /* -Wl "some icon label" */
                           case 't' :           /* Font filename */
                           case 'T' :           /* Icon font filename */
                           case 'w' : INC ;     /* Width, in columns. */
                                      break ;
                           case 'p' : INC ;     /* -Wp xnum ynum */
                                      getparam(next, argv,
                                               "-Wp needs x coordinate") ;
                                      wx = atoi(next) ;
                                      INC ;
                                      getparam(next, argv,
                                               "-Wp needs y coordinate") ;
                                      wy = atoi(next) ;
                                      posspec = 1 ;
                                      break ;
                           case 'P' : INC ;      /* -WP xnum ynum */
                                      getparam(next, argv,
                                               "-WP needs x coordinate") ;
                                      ix = atoi(next) ;
                                      INC ;
                                      getparam(next, argv,
                                               "-WP needs y coordinate") ;
                                      iy = atoi(next) ;
                                      break ;
                           case 's' : INC ; INC ;  /* -Ws xnum ynum */
                                      break ;
                           case 'b' :              /* -Wb r g b (bg color spec)
*/
                           case 'f' : INC ; INC ; INC ;  /* Same, fg color */
                                      break ;
                           default :  FPRINTF(stderr,"%s: -W%c unknown argument\n",
                                                      progname, argv[0][2]) ;
                                      break ;
                         }
                       break ;
            default  : usage() ;
          }
      INC ;
    }
}


getparam(s, argv, errmes)
char *s, *argv[], *errmes ;
{
  if (*argv != NULL && argv[0][0] != '-') STRCPY(s, *argv) ;
  else
    { 
      FPRINTF(stderr,"%s: %s as next argument.\n", progname, errmes) ;
      exit(1) ;                        
    }                                  
}


get_helpfile(helpname)     /* Open helpfile if present. */
char *helpname ;
{
  char *getenv(), name[MAXLINE], *paths, *ptr ;
  int i ;

  i = 0 ;
  ishelp = 1 ;
  if ((hfd = fopen(helpname,"r")) == NULL)
    {
      paths = getenv("PATH") ;
      if ((ptr = paths) && helpname[0] != '/')
        for (;;)
          if (*ptr == ':' || *ptr == '\0')
            {
              if (*ptr == 0) break ;
              name[i++] = '/' ;
              name[i] = '\0' ;
              STRCAT(name, helpname) ;
              if ((hfd = fopen(name, "r")) != NULL) return ;
              if (*ptr == '\0') break ;
              *ptr++ ;
              i = 0 ;
            }
          else name[i++] = *ptr++ ;
      FPRINTF(stderr, "%s: Help file: %s not found\r\n", progname, helpname) ;
      ishelp = 0 ;
    }
}


get_rcfile(name)          /* Read .calctoolrc file. */
char *name ;
{
  char line[MAXLINE] ;    /* Current line from the .calctoolrc file. */
  char tmp[MAXLINE] ;     /* Used to extract function definitions. */
  double cval ;           /* Current constant value being conveted. */
  int i ;                 /* Index to constant or function array. */
  int isval ;             /* Set to 'c' or 'f' for convertable line. */
  int n ;
  FILE *rcfd ;            /* File descriptor for calctool rc file. */

  if ((rcfd = fopen(name, "r")) == NULL) return ;

/*  Process the .calctoolrc file. There are currently four types of
 *  records to look for:
 *
 *  1) Those starting with a hash in the first column are comments.
 *
 *  2) Lines starting with 'c' or 'C' in the first column are
 *     definitions for constants. The cC is followed by a digit in
 *     the range 0-9, then a space. This is followed by a number
 *     in fixed or scientific notation. Following this is an optional
 *     comment, which if found, will be used in the popup menu for
 *     the constants. If the comment is present, there must be at
 *     least one space between this and the preceding number.
 *
 *  3) Those starting with a 'f' or a 'F' in the first column are
 *     definitions for functions. The fF is followed by a digit in
 *     the range 0-9, then a space. This is followed by a function
 *     definition. Following this is an optional comment, which if
 *     found, will be used in the popup menu for the functions.
 *     If the comment is present, there must be at least one space
 *     between this and the preceding function definition.
 *
 *  4) Lines starting with a 'r' or a 'R' in the first column are
 *     definitions for the initial contents of the calculators
 *     memory registers. The rR is followed by a digit in the
 *     range 0-9, then a space. This is followed by a number in
 *     fixed or scientific notation. The rest of the line is ignored.
 *
 *  All other lines are ignored.
 *
 *  Two other things to note. There should be no embedded spaces in
 *  the function definitions, and whenever a backslash is found, that
 *  and the following character signify a control character, for
 *  example \g would be ascii 7.
 */

  while (fgets(line, MAXLINE, rcfd) != NULL)
    {
      isval = 0 ;
           if (line[0] == 'c' || line[0] == 'C') isval = 'c' ;
      else if (line[0] == 'f' || line[0] == 'F') isval = 'f' ;
      else if (line[0] == 'r' || line[0] == 'R') isval = 'r' ;
      if (isval)
        if (line[1] >= '0' && line[1] <= '9' && line[2] == ' ')
          {
            i = char_val(line[1]) ;
            if (isval == 'c')
              {
                n = sscanf(&line[3], "%lf", &cval) ;
                if (n == 1) con_vals[i] = cval ;
              }
            else if (isval == 'f')
              {
                SSCANF(&line[3], "%s", tmp) ;      
                STRCPY(fun_vals[i], convert(tmp)) ;
              }
            else if (isval == 'r')
              {
                n = sscanf(&line[3], "%lf", &cval) ;
                if (n == 1) mem_vals[i] = cval ;
                continue ;
              }
            for (n = 3; n < strlen(line); n++)
              if (line[n] == ' ' || line[n] == '\n')
                {
                  while (line[n] == ' ') n++ ;
                  line[strlen(line)-1] = '\0' ;
                  if (isval == 'c')
                    SPRINTF(con_names[i], "%1d: %g [%s]",
                            i, con_vals[i], &line[n]) ;
                  else
                    SPRINTF(fun_names[i], "%1d: %s [%s]",
                            i, fun_vals[i], &line[n]) ;
                  break ;
                }
          }
    }
  FCLOSE(rcfd) ;
}


read_rcfiles()   /* Read .calctoolrc's from home and current directories. */
{
  char *home ;            /* Pathname for users home directory. */
  char name[MAXLINE] ;    /* Full name of users .calctoolrc file. */
  char pathname[MAXPATHLEN] ;   /* Current working directory. */
  int n ;
  struct passwd *entry ;

  for (n = 0; n < MAXREGS; n++)
    {
      SPRINTF(name, "%1d: %g [%s]", n, con_vals[n], con_names[n]) ;
      STRCPY(con_names[n], name) ;
      STRCPY(fun_vals[n], "") ;       /* Initially empty function strings. */
    }

  if ((home = getenv("HOME")) == NULL)
    {
      if ((entry = getpwuid(getuid())) == NULL) return ;
      home = entry->pw_dir ;
    }
  SPRINTF(name, "%s/%s", home, RCNAME) ;
  get_rcfile(name) ;      /* Read .calctoolrc from users home directory. */

  SPRINTF(name, "%s/%s", getcwd(pathname,MAXPATHLEN-2), RCNAME) ;
  get_rcfile(name) ;      /* Read .calctoolrc file from current directory. */
}


usage()
{
  FPRINTF(stderr, "Usage: %s: [-d display] [-g geometry] ", progname) ;
  FPRINTF(stderr, "[-h helpfile] [-i] [-v] [-Wi] [-Wp x y] [-WP x y]\n") ;
  exit(0) ;
}
