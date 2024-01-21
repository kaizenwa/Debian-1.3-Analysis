/* genmess. Generation of messages.
   Copyright 1994, 1995 Tristan Gingold
		  Written January 1994 by Tristan Gingold
		  
This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
*/
/* genmess allow you to generate easily messages in differents languages.
   From the source file, it generates an header file (.h) and a message 
   file (.c). The header file is design to be included by program.
   
   The input file can have 5 sortes of lines:
   $header "............."
   $hformat "............."
   $literal "............"
   ....... "............."
   #...........
   
   The last one is a blank line.
   Each line must begin with a '$', a '#', a letter or nothing.
   If a line begins with a '#' or with nothing, it is treated as a comment.
   If a line begins with $header, $declare or $hformat a string, enclosed in "
    must follow. 
    For $header lines, this string, without the " is directly copied in the 
    message file and ={ is appended. This is used to declare the array of
    messages.
    For $hformat lines, this string will be used to build the header file. This
    line is optional, and initialized by default, after each $header lines to
    "%d".
    For $declare lines, the string is copied in the header file, with a \n.
   If a line begins with characters ([a-zA-Z_0-9]), the first word is called a
    symbol, and the next string a message. The message is written to the
    message file with a comma. The symbol is appended to the header file:
    #define symbol hformat
    hformat is defined by $hformat and is used for the format of printf. If 
    hformat contains %s, the argument is the message and the message is not
    written to the message file. If hformat contains %d, the argument is the
    number of the message, starting from 0.
   You can't used symbol lines before an $header line.
   Spaces are ' ', '\t' and musr separate two fields.
   
   At the end of the message file, }; is appended.
Exemple:
$header "const char *mess[]"
FIRST "first"
SECOND "second"
   
   Usage: genmess [options] headerfile messagefile inputfile
     options are:
      -a --append: append after the files
    inputfile can be - to use stdin.
   If headerfile or messagefile doesn't exist, they are created. Otherwise,
    genmess appends if -a option is used
   See the source for other details.
*/   

#define _POSIX_C_SOURCE 2
#include <limits.h>
#ifdef __GNU_LIBRARY__
#include <getopt.h>
#else
extern int optind;
extern char *optarg;
#endif
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

FILE *inputfile;
FILE *headerfile;
FILE *message;

#undef LINE_MAX
#ifdef _POSIX2_LINE_MAX
#define LINE_MAX _POSIX2_LINE_MAX
#else
#define LINE_MAX 2048
#endif

char buffer[LINE_MAX];
char header[LINE_MAX];
char hformat[LINE_MAX];
char format[LINE_MAX];

int line = 1;			/* line number */
char *inputname = NULL;		/* input file name */
char *progname;

void
my_fprintf (FILE* stream, char *format, int num, char *arg1, char *arg2)
{
 while (*format)
   {
     if (*format == '%')
       {
         format++;
         switch (*format)
           {
         case '1':
           fputs (arg1, stream);
           break;
         case '2':
           fputs (arg2, stream);
           break;
         case 'n':
           fprintf (stream, "%d", num);
           break;
         case '%':
           fputc ('%', stream);
           break;
         default:
           break;
           }
       }
     else
       fputc (*format, stream);
     format++;
   }
 fputc ('\n', stream);
}

void
fatal (char *m)
{
  if (inputname == NULL)
    fprintf (stderr, "%s", m);
  else
    fprintf (stderr, "%s:%d: %s", inputname, line, m);  
  exit (1);
}

/* Return the offset of the first '"' in a buffer (buf) starting from 'start'
 * and put an \0 after the second '"'. \" are accepted.
 */
int
get_string (char *buf, int start)
{
 int i;
 char delim;
 /* skip the blanks */
 for (; buf[start] == ' ' || buf[start] == '\t'; start++)
   ;
 /* check the fisrt '"' */
 delim = buf[start];
 if (delim == 0)
   return 0;
 i = start + 1;
 while (1)
   {
     if (buf[i] == '\\')
       if (buf[i+1] != '\0' && buf[i+1] != '\n')
         {
           i += 2;
           continue;
         }
       else
         return 0;
     if (buf[i] == delim)
       break;
     if (buf[i] == '\n' || buf[i] == '\0')
       return 0;
     i++;
   }
 buf[++i] = '\0';
 return start;
}

/* Return 1 if c is [A-Za-z0-9_] */
int
is_c_char (char c)
{
  if (c >= 'A' && c <= 'Z')
    return 1;
  if (c >= 'a' && c <= 'z')
    return 1;
  if (c >= '0' && c <= '9')
    return 1;
  if (c == '_')
    return 1;
  return 0;
}

void
usage (void)
{
  fprintf (stderr, "Usage: %s [options] headerfile messagefile inputfile\n", progname);
  fprintf (stderr, "Options are:\n");
  fprintf (stderr, "-a --append\n");
  exit (2);
}

#ifdef __GNU_LIBRARY__
struct option long_options[]= {
{ "append", 0, 0, 'a'},
{ 0, 0, 0, 0}
};
#endif

void
main (int argc, char *argv[])
{
 int i,j;
 int number = 0;
 int noheader = 1;
 int append_flag = 0;
 int all_in_one = 0;	/* message = headerfile */
 int default_header = 1;
 
 progname = argv[0];
 
 if (argc < 4)
   usage ();
   
 i = 1;	/* first argument */
#ifdef __GNU_LIBRARY__
 while ((i = getopt_long (argc, argv, "a", long_options, NULL)) != -1)
#else
 while ((i = getopt (argc, argv, "a")) != -1)
#endif
   {
     switch (i)
       {
     case 'a':
       append_flag = 1;
       break;
     default:
       usage ();
       }
   }
 if (argc - optind != 3)
   usage ();
 if ( (message = fopen (argv[optind+1], append_flag ? "aw" : "w")) == NULL)
   {
     perror("Can't open message file");
     exit(1);
   }
 if (!append_flag || ftell (message) == 0)
   fprintf (message, "/* Automaticaly generated by genmess. Do not edit! */\n");
 if (argv[optind+2][0] == '-' && argv[optind+2][1] == '\0')
   {
     inputfile = stdin;
     inputname = "(standard input)";
   }
 else
   {
     if ( (inputfile = fopen (argv[optind+2], "r")) == NULL)
       {
         perror ("Can't open input file");
         exit (1);
       }
     inputname = argv[optind+1];
   }
  if (argv[optind][0] == '-' && argv[optind][1] == '\0')
    {
      headerfile = message;
      all_in_one = 1;
    }
  else
    {
      if ( (headerfile = fopen (argv[optind], append_flag ? "aw" : "w")) == NULL)
       {
         perror ("Can't open header file");
         exit (1);
       }
     if (!append_flag || ftell (headerfile) == 0)
       fprintf (headerfile, "/* Automaticaly generated by genmess. Do not edit! */\n");
    } 
 /* Main loop: read a line and treat it */
 while(1)
   {
     line++;
     if (fgets (buffer, LINE_MAX, inputfile) == NULL)
       {
         if (feof (inputfile))
           {
             if (!noheader)
               {
                 if (default_header)
                   fprintf (message, "\"\"");
                 fprintf (message, "\n};\n");
               }
             fclose (headerfile);
             if (!all_in_one)
               fclose (message);
             if (stdin != inputfile)
               fclose (inputfile);
             exit (0);
           }
         fatal ("Unexpected EOF\n");
       }
     /* Skip nul lines */
     if (strlen( buffer) == 0)
       continue;
     /* Skip comments */
     if (buffer[0] == '#')
       continue;
     /* Skip blank lines */
     for(i = 0; buffer[i] != '\0'; i++)
       if (buffer[i] != ' ' && buffer[i] != '\n' && buffer[i] != '\t')
         break;
     if (buffer[i] == '\0')
       continue;
     if (buffer[0] == '$')
       {
         if (strncmp (&buffer[1], "header", 6) == 0)
           {
             j = get_string (buffer, 7);
             if (j == 0)
               fatal ("`$header' must be followed by a string (\"xxx\")\n");
             buffer[j++] = '\0';
             /* replace the last '"' by a '\0' */
             buffer[j + strlen(buffer + j) - 1] = '\0';
             if (default_header && !noheader)
               fprintf (message, "\"\"\n};\n");
             strcpy (hformat, "#define %1\t%n"); /* default hformat */
             strcpy (format, "/* %n */ %2,");	 /* default format */
             default_header = 1;
             noheader = 0;
             number = 0;
             fprintf (message,"\n%s = {\n", buffer + j);
             continue;
           }
         else if (strncmp (&buffer[1], "end", 3) == 0)
           {
             if (default_header && !noheader)
               fprintf (message, "\"\"\n};\n");
             strcpy (hformat, "#define %1\t%n"); /* default hformat */
             strcpy (format, "/* %n */ %2,");	 /* default format */
             default_header = 1;
             noheader = 1;
             continue;
           }
         else if (strncmp (&buffer[1], "hformat", 7) == 0)
           {
             j = get_string (buffer, 8);
             if (j == 0)
               fatal ("`$hformat' must be followed by a string (\"xxx\")\n");
             buffer[j++] = '\0';
             /* Replace the second '"' by '\0' */
             buffer[j + strlen (buffer + j) - 1] = '\0';
#if 0
             if (noheader == 1)
               fatal ("Can't defined a hformat before an header\n");
#endif
             strcpy (hformat, buffer + j);
             continue;
           }
         else if (strncmp (&buffer[1], "format", 6) == 0)
           {
             j = get_string (buffer, 7);
             if (j == 0)
               fatal ("`$format' must be followed by a string (\"xxx\")\n");
             buffer[j++] = '\0';
             /* Replace the second '"' by '\0' */
             buffer[j + strlen(buffer + j) - 1] = '\0';
#if 0
             if (noheader == 1)
               fatal ("Can't defined a format before an header\n");
#endif
             strcpy (format, buffer + j);
             default_header = 0;
             continue;
           }
         else if (strncmp (&buffer[1], "declare", 7) == 0)
           {
             j = get_string (buffer, 8);
             if (j == 0)
               fatal ("`$declare' must be followed by a string (\"xxx\")\n");
             buffer[j++] = '\0';
             /* Replace the second '"' by '\0' */
             buffer[j + strlen(buffer + j) - 1] = '\0';
             fprintf (headerfile, "%s\n", buffer + j);
             continue;
           }
         else if (strncmp (&buffer[1], "literal", 7) == 0)
           {
             j = get_string(buffer, 8);
             if (j == 0)
               fatal("`$literal' must be followed by a string (\"xxx\")\n");
             buffer[j++] = '\0';
             /* Replace the second '"' by '\0' */
             buffer[j + strlen (buffer + j) - 1] = '\0';
             fprintf (message, "%s\n", buffer + j);
             continue;
           }
         fatal ("Unknown order\n");
       }
#if 0
     if (noheader == 1)
       fatal ("Get a message before an header\n");
#endif
     /* get symbol */
     for (i = 0; is_c_char (buffer[i]); i++)
       ;
     if (i<2 || buffer[i] == '\0')
       {
         fprintf (stderr,"%s:%d: syntax error\n", argv[1], line);
         exit (1);
       }
     buffer[i++] = '\0';
     
     /* Get the message */
     j = get_string (buffer, i);
     if (j == 0)
       fatal ("Syntax error\n");
       
     my_fprintf (headerfile, hformat, number, buffer, buffer + j);
     my_fprintf (message, format, number, buffer, buffer + j);

     number++;
   }
}     
