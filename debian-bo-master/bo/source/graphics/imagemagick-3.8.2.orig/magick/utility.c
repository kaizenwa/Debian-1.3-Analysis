/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%             U   U  TTTTT  IIIII  L      IIIII  TTTTT  Y   Y                 %
%             U   U    T      I    L        I      T     Y Y                  %
%             U   U    T      I    L        I      T      Y                   %
%             U   U    T      I    L        I      T      Y                   %
%              UUU     T    IIIII  LLLLL  IIIII    T      Y                   %
%                                                                             %
%                                                                             %
%                       ImageMagick Utility Routines                          %
%                                                                             %
%                                                                             %
%                                                                             %
%                             Software Design                                 %
%                               John Cristy                                   %
%                              January 1993                                   %
%                                                                             %
%                                                                             %
%  Copyright 1997 E. I. du Pont de Nemours and Company                        %
%                                                                             %
%  Permission to use, copy, modify, distribute, and sell this software and    %
%  its documentation for any purpose is hereby granted without fee,           %
%  provided that the above Copyright notice appear in all copies and that     %
%  both that Copyright notice and this permission notice appear in            %
%  supporting documentation, and that the name of E. I. du Pont de Nemours    %
%  and Company not be used in advertising or publicity pertaining to          %
%  distribution of the software without specific, written prior               %
%  permission.  E. I. du Pont de Nemours and Company makes no representations %
%  about the suitability of this software for any purpose.  It is provided    %
%  "as is" without express or implied warranty.                               %
%                                                                             %
%  E. I. du Pont de Nemours and Company disclaims all warranties with regard  %
%  to this software, including all implied warranties of merchantability      %
%  and fitness, in no event shall E. I. du Pont de Nemours and Company be     %
%  liable for any special, indirect or consequential damages or any           %
%  damages whatsoever resulting from loss of use, data or profits, whether    %
%  in an action of contract, negligence or other tortious action, arising     %
%  out of or in connection with the use or performance of this software.      %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
*/

/*
  Include declarations.
*/
#include "magick.h"
#include "Colorlist.h"

/*
  Global declarations.
*/
char
  *client_name;

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  A p p e n d I m a g e F o r m a t                                          %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function AppendImageFormat appends the image format type to the filename.
%  If an extension to the file already exists, it is first removed.
%
%  The format of the AppendImageFormat routine is:
%
%       AppendImageFormat(format,filename)
%
%  A description of each parameter follows.
%
%   o  format:  Specifies a pointer to an array of characters.  This is the
%      format of the image.
%
%   o  filename:  Specifies a pointer to an array of characters.  The unique
%      file name is returned in this array.
%
%
*/
Export void AppendImageFormat(const char *format,char *filename)
{
  char
    staging[MaxTextExtent];

  register char
    *p;

  assert(format != (char *) NULL);
  assert(filename != (char *) NULL);
  if ((*format == '\0') || (*filename == '\0'))
    return;
  if (strcmp(filename,"-") == 0)
    {
      (void) sprintf(staging,"%s:%s",format,filename);
      (void) strcpy(filename,staging);
      return;
    }
  p=filename+Extent(filename)-1;
  while ((p > filename) && (*(p-1) != '.'))
    p--;
  if ((p > filename) && (*(p-1) == '.'))
    {
      (void) strcpy(p,format);
      return;
    }
  (void) strcat(filename,".");
  (void) strcat(filename,format);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   C l i e n t N a m e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ClientName removes the path name component and any extensions.
%
%  The format of the ClientName function is:
%
%      ClientName(filename)
%
%  A description of each parameter follows:
%
%    o filename: Specifies a pointer to an character array that contains the
%      filename.
%
%
*/
Export char *ClientName(const char *filename)
{
  register char
    *p;

  static char
    client[MaxTextExtent];

  /*
    Get basename of client.
  */
  assert(filename != (char *) NULL);
  client_name=client;
  (void) strcpy(client_name,filename);
  p=client_name+(Extent(client_name)-1);
  while (p > client_name)
  {
    if (*p == *BasenameSeparator)
      {
        (void) strcpy(client_name,p+1);
        break;
      }
    p--;
  }
  /*
    Delete any extension.
  */
  p=client_name+(Extent(client_name)-1);
  while (p > client_name)
  {
    if (*p == '.')
      {
        *p='\0';
        break;
      }
    p--;
  }
  return(client_name);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   E x p a n d F i l e n a m e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ExpandFilename expands '~' in a filename.
%
%  The format of the ExpandFilename function is:
%
%      ExpandFilename(filename)
%
%  A description of each parameter follows:
%
%    o filename: Specifies a pointer to an character array that contains the
%      filename.
%
%
*/
void ExpandFilename(char *filename)
{
  char
    expanded_filename[MaxTextExtent];

  register char
    *p;

  if (filename == (char *) NULL)
    return;
  if (*filename != '~')
    return;
  (void) strcpy(expanded_filename,filename);
  if (*(filename+1) == '/')
    {
      /*
        Substitute ~ with $HOME.
      */
      p=(char *) getenv("HOME");
      if (p == (char *) NULL)
        p=".";
      (void) strcpy(expanded_filename,p);
      (void) strcat(expanded_filename,filename+1);
    }
  else
    {
#if !defined(vms) && !defined(macintosh) && !defined(WIN32)
      char
        username[MaxTextExtent];

      struct passwd
        *entry;

      /*
        Substitute ~ with home directory from password file.
      */
      (void) strcpy(username,filename+1);
      p=strchr(username,'/');
      if (p != (char *) NULL)
        *p='\0';
      entry=getpwnam(username);
      if (entry == (struct passwd *) NULL)
        return;
      (void) strcpy(expanded_filename,entry->pw_dir);
      if (p != (char *) NULL)
        {
          (void) strcat(expanded_filename,"/");
          (void) strcat(expanded_filename,p+1);
        }
#endif
    }
  (void) strcpy(filename,expanded_filename);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   E x p a n d F i l e n a m e s                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ExpandFilenames checks each argument of the command line vector and
%  expands it if they have a wildcard character.  For example, *.jpg might
%  expand to:  bird.jpg rose.jpg tiki.jpg.
%
%  The format of the ExpandFilenames function is:
%
%      ExpandFilenames(argc,argv)
%
%  A description of each parameter follows:
%
%    o argc: Specifies a pointer to an integer describing the number of
%      elements in the argument vector.
%
%    o argv: Specifies a pointer to a text array containing the command line
%      arguments.
%
%
*/
Export void ExpandFilenames(int *argc,char ***argv)
{
  char
    **filelist,
    home_directory[MaxTextExtent],
    *option,
    **vector,
    working_directory[MaxTextExtent];

  int
    count,
    expanded,
    number_files;

  register char
    *p,
    *q;

  register int
    i,
    j;

  /*
    Allocate argument vector.
  */
  assert(argc != (int *) NULL);
  assert(argv != (char ***) NULL);
  vector=(char **) malloc((*argc+1)*sizeof(char *));
  if (vector == (char **) NULL)
    {
      Warning("Unable to expand filenames",(char *) NULL);
      return;
    }
  /*
    Expand any wildcard filenames.
  */
  (void) getcwd(home_directory,MaxTextExtent-1);
  expanded=False;
  count=0;
  for (i=0; i < *argc; i++)
  {
    option=(*argv)[i];
    vector[count++]=option;
    if ((Extent(option) > 1) && ((*option == '-') || (*option == '+')))
      continue;
    if ((*option == '"') || (*option == '\''))
      continue;
    if (!IsGlob(option))
      {
        /*
          Silently skip directories.
        */
        if (IsDirectory(option))
          count--;
        continue;
      }
    /*
      Get the list of image file names.
    */
    (void) getcwd(working_directory,MaxTextExtent-1);
    for (p=option+Extent(option)-1; p > option; p--)
      if (*p == *BasenameSeparator)
        {
          /*
            Filename includes a directory name.
          */
          q=working_directory;
          for (j=0; j < (p-option+1); j++)
            *q++=option[j];
          *q='\0';
          p++;
          break;
        }
    filelist=ListFiles(working_directory,p,&number_files);
    if (filelist == (char **) NULL)
      continue;
    for (j=0; j < number_files; j++)
      if (!IsDirectory(filelist[j]))
        break;
    if (j == number_files)
      {
        for (j=0; j < number_files; j++)
          free((char *) filelist[j]);
        free((char *) filelist);
        continue;
      }
    /*
      Transfer file list to argument vector.
    */
    vector=(char **)
      realloc(vector,(*argc+count+number_files)*sizeof(char *));
    if (vector == (char **) NULL)
      {
        Warning("Unable to expand filenames",(char *) NULL);
        return;
      }
    count--;
    for (j=0; j < number_files; j++)
    {
      if (IsDirectory(filelist[j]))
        {
          free((char *) filelist[j]);
          continue;
        }
      expanded=True;
      vector[count]=(char *)
        malloc(((p-option)+Extent(filelist[j])+MaxTextExtent+1)*sizeof(char));
      if (vector[count] == (char *) NULL)
        {
          Warning("Unable to expand filenames",(char *) NULL);
          for ( ; j < number_files; j++)
            free((char *) filelist[j]);
          free((char *) filelist);
          return;
        }
      (void) sprintf(vector[count],"%.*s%s",p-option,option,filelist[j]);
      free((char *) filelist[j]);
      count++;
    }
    free((char *) filelist);
  }
  (void) chdir(home_directory);
  if (!expanded)
    {
      free((char *) vector);
      return;
    }
  *argc=count;
  *argv=vector;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   G l o b E x p r e s s i o n                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function GlobExpression returns True if the expression matches the pattern.
%
%  The format of the GlobExpression function is:
%
%      GlobExpression(expression,pattern)
%
%  A description of each parameter follows:
%
%    o expression: Specifies a pointer to a text string containing a file name.
%
%    o pattern: Specifies a pointer to a text string containing a pattern.
%
%
*/
int GlobExpression(char *expression,const char *pattern)
{
  ImageInfo
    image_info;

  unsigned int
    done,
    exempt;

  /*
    Return on empty pattern or '*'.
  */
  if (pattern == (char *) NULL)
    return(True);
  if (Extent(pattern) == 0)
    return(True);
  if (strcmp(pattern,"*") == 0)
    return(True);
  /*
    Determine if pattern is a subimage, i.e. img0001.pcd[2].
  */
  GetImageInfo(&image_info);
  (void) strcpy(image_info.filename,pattern);
  SetImageInfo(&image_info,True);
  exempt=(strcmp(image_info.magick,"VID") == 0) ||
    (image_info.subimage && (strcmp(expression,image_info.filename) == 0));
  free((char *) image_info.filename);
  if (exempt)
    return(False);
  /*
    Evaluate glob expression.
  */
  done=False;
  while ((*pattern != '\0') && !done)
  {
    if (*expression == '\0')
      if ((*pattern != '{') && (*pattern != '*'))
        break;
    switch (*pattern)
    {
      case '\\':
      {
        pattern++;
        if (*pattern != '\0')
          pattern++;
        break;
      }
      case '*':
      {
        int
          status;

        pattern++;
        status=False;
        while ((*expression != '\0') && !status)
          status=GlobExpression((char *) expression++,pattern);
        if (status)
          {
            while (*expression != '\0')
              expression++;
            while (*pattern != '\0')
              pattern++;
          }
        break;
      }
      case '[':
      {
        char
          c;

        pattern++;
        for ( ; ; )
        {
          if ((*pattern == '\0') || (*pattern == ']'))
            {
              done=True;
              break;
            }
          if (*pattern == '\\')
            {
              pattern++;
              if (*pattern == '\0')
                {
                  done=True;
                  break;
                }
             }
          if (*(pattern+1) == '-')
            {
              c=(*pattern);
              pattern+=2;
              if (*pattern == ']')
                {
                  done=True;
                  break;
                }
              if (*pattern == '\\')
                {
                  pattern++;
                  if (*pattern == '\0')
                    {
                      done=True;
                      break;
                    }
                }
              if ((*expression < c) || (*expression > *pattern))
                {
                  pattern++;
                  continue;
                }
            }
          else
            if (*pattern != *expression)
              {
                pattern++;
                continue;
              }
          pattern++;
          while ((*pattern != ']') && (*pattern != '\0'))
          {
            if ((*pattern == '\\') && (*(pattern+1) != '\0'))
              pattern++;
            pattern++;
          }
          if (*pattern != '\0')
            {
              pattern++;
              expression++;
            }
          break;
        }
        break;
      }
      case '?':
      {
        pattern++;
        expression++;
        break;
      }
      case '{':
      {
        int
          match;

        register char
          *p;

        pattern++;
        while ((*pattern != '}') && (*pattern != '\0'))
        {
          p=expression;
          match=True;
          while ((*p != '\0') && (*pattern != '\0') &&
                 (*pattern != ',') && (*pattern != '}') && match)
          {
            if (*pattern == '\\')
              pattern++;
            match=(*pattern == *p);
            p++;
            pattern++;
          }
          if (*pattern == '\0')
            {
              match=False;
              done=True;
              break;
            }
          else
            if (match)
              {
                expression=p;
                while ((*pattern != '}') && (*pattern != '\0'))
                {
                  pattern++;
                  if (*pattern == '\\')
                    {
                      pattern++;
                      if (*pattern == '}')
                        pattern++;
                    }
                }
              }
            else
              {
                while ((*pattern != '}') && (*pattern != ',') &&
                       (*pattern != '\0'))
                {
                  pattern++;
                  if (*pattern == '\\')
                    {
                      pattern++;
                      if ((*pattern == '}') || (*pattern == ','))
                        pattern++;
                    }
                }
              }
            if (*pattern != '\0')
              pattern++;
          }
        break;
      }
      default:
      {
        if (*expression != *pattern)
          done=True;
        else
          {
            expression++;
            pattern++;
          }
      }
    }
  }
  while (*pattern == '*')
    pattern++;
  return((*expression == '\0') && (*pattern == '\0'));
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  I s A c c e s s i b l e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function IsAccessible returns True if the file as defined by filename is
%  accessible.
%
%  The format of the IsAccessible routine is:
%
%       status=IsAccessible(filename)
%
%  A description of each parameter follows.
%
%   o  status:  Function IsAccessible returns True is the file as defined by
%      filename is accessible, otherwise False is returned.
%
%   o  filename:  Specifies a pointer to an array of characters.  The unique
%      file name is returned in this array.
%
%
*/
Export unsigned int IsAccessible(const char *filename)
{
  FILE
    *file;

  /*
    Return False if the file cannot be opened.
  */
  assert(filename != (char *) NULL);
  file=fopen(filename,ReadBinaryType);
  if (file == (FILE *) NULL)
    return(False);
  (void) fclose(file);
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  I s D i r e c t o r y                                                      %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function IsDirectory returns True if the file as defined by filename is
%  a directory.  Once MetroWerks write a stat(2) function, we can remove the
%  chdir(2) function.
%
%  The format of the IsAccessible routine is:
%
%       status=IsDirectory(filename)
%
%  A description of each parameter follows.
%
%   o  status:  Function IsDirectory returns True is the file as defined by
%      filename is a directory, otherwise False is returned.
%
%   o  filename:  Specifies a pointer to an array of characters.  The unique
%      file name is returned in this array.
%
%
*/
unsigned int IsDirectory(const char *filename)
{
  int
    status;

#if !defined(WIN32)
  struct stat
    file_info;

  status=stat(filename,&file_info);
  if (status != 0)
    return(False);
  return(S_ISDIR(file_info.st_mode));
#else
  char
    current_directory[MaxTextExtent];

  (void) getcwd(current_directory,MaxTextExtent-1);
  status=chdir(filename);
  if (status == 0)
    (void) chdir(current_directory);
  return(status == 0);
#endif
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   L i s t C o l o r s                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ListColors reads the X client color database and returns a list
%  of colors contained in the database sorted in ascending alphabetic order.
%
%  The format of the ListColors function is:
%
%      filelist=ListColors(pattern,number_colors)
%
%  A description of each parameter follows:
%
%    o filelist: Function ListColors returns a list of colors contained
%      in the database.  If the database cannot be read, a NULL list is
%      returned.
%
%    o pattern: Specifies a pointer to a text string containing a pattern.
%
%    o number_colors:  This integer returns the number of colors in the list.
%
%
*/

static int ColorCompare(const void *x,const void *y)
{
  register char
    **p,
    **q;

  p=(char **) x;
  q=(char **) y;
  return(strcmp(*p,*q));
}

char **ListColors(const char *pattern,int *number_colors)
{
  char
    color[MaxTextExtent],
    **colorlist,
    text[MaxTextExtent];

  FILE
    *database;

  int
    blue,
    count,
    green,
    red;

  unsigned int
    max_colors;

  /*
    Allocate color list.
  */
  assert(pattern != (char *) NULL);
  assert(number_colors != (int *) NULL);
  max_colors=sizeof(Colorlist)/sizeof(XColorlist);
  colorlist=(char **) malloc(max_colors*sizeof(char *));
  if (colorlist == (char **) NULL)
    {
      Warning("Unable to read color name database","Memory allocation failed");
      return((char **) NULL);
    }
  /*
    Open database.
  */
  *number_colors=0;
  database=fopen(RGBColorDatabase,"r");
  if (database == (FILE *) NULL)
    {
      register XColorlist
        *p;

      /*
        Can't find server color database-- use our color list.
      */
      for (p=Colorlist; p->name != (char *) NULL; p++)
        if (GlobExpression(p->name,pattern))
          {
            colorlist[*number_colors]=(char *) malloc(Extent(p->name)+1);
            if (colorlist[*number_colors] == (char *) NULL)
              break;
            (void) strcpy(colorlist[*number_colors],p->name);
            (*number_colors)++;
          }
      return(colorlist);
    }
  while (fgets(text,MaxTextExtent-1,database) != (char *) NULL)
  {
    count=sscanf(text,"%d %d %d %[^\n]\n",&red,&green,&blue,color);
    if (count != 4)
      continue;
    if (GlobExpression(color,pattern))
      {
        if (*number_colors >= max_colors)
          {
            max_colors<<=1;
            colorlist=(char **)
              realloc((char **) colorlist,max_colors*sizeof(char *));
            if (colorlist == (char **) NULL)
              {
                Warning("Unable to read color name database",
                  "Memory allocation failed");
                (void) fclose(database);
                return((char **) NULL);
              }
          }
        colorlist[*number_colors]=(char *) malloc(Extent(color)+1);
        if (colorlist[*number_colors] == (char *) NULL)
          break;
        (void) strcpy(colorlist[*number_colors],color);
        (*number_colors)++;
      }
  }
  (void) fclose(database);
  /*
    Sort colorlist in ascending order.
  */
  qsort((void *) colorlist,*number_colors,sizeof(char **),
    (int (*)(const void *, const void *)) ColorCompare);
  return(colorlist);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   L i s t F i l e s                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ListFiles reads the directory specified and returns a list
%  of filenames contained in the directory sorted in ascending alphabetic
%  order.
%
%  The format of the ListFiles function is:
%
%      filelist=ListFiles(directory,pattern,number_entries)
%
%  A description of each parameter follows:
%
%    o filelist: Function ListFiles returns a list of filenames contained
%      in the directory.  If the directory specified cannot be read or it is
%      a file a NULL list is returned.
%
%    o directory: Specifies a pointer to a text string containing a directory
%      name.
%
%    o pattern: Specifies a pointer to a text string containing a pattern.
%
%    o number_entries:  This integer returns the number of filenames in the
%      list.
%
%
*/

static int FileCompare(const void *x,const void *y)
{
  register char
    **p,
    **q;

  p=(char **) x;
  q=(char **) y;
  return(strcmp(*p,*q));
}

char **ListFiles(char *directory,const char *pattern,int *number_entries)
{
  char
    **filelist;

  DIR
    *current_directory;

  int
    status;

  struct dirent
    *entry;

  unsigned int
    max_entries;

  /*
    Open directory.
  */
  assert(directory != (char *) NULL);
  assert(pattern != (char *) NULL);
  assert(number_entries != (int *) NULL);
  *number_entries=0;
  status=chdir(directory);
  if (status != 0)
    return((char **) NULL);
  (void) getcwd(directory,MaxTextExtent-1);
  current_directory=opendir(directory);
  if (current_directory == (DIR *) NULL)
    return((char **) NULL);
  /*
    Allocate filelist.
  */
  max_entries=2048;
  filelist=(char **) malloc(max_entries*sizeof(char *));
  if (filelist == (char **) NULL)
    {
      (void) closedir(current_directory);
      return((char **) NULL);
    }
  /*
    Save the current and change to the new directory.
  */
  (void) chdir(directory);
  entry=readdir(current_directory);
  while (entry != (struct dirent *) NULL)
  {
    if (*entry->d_name == '.')
      {
        entry=readdir(current_directory);
        continue;
      }
    if (IsDirectory(entry->d_name) || GlobExpression(entry->d_name,pattern))
      {
        if (*number_entries >= max_entries)
          {
            max_entries<<=1;
            filelist=(char **)
              realloc((char **) filelist,max_entries*sizeof(char *));
            if (filelist == (char **) NULL)
              {
                (void) closedir(current_directory);
                return((char **) NULL);
              }
          }
        filelist[*number_entries]=(char *) malloc(Extent(entry->d_name)+2);
        if (filelist[*number_entries] == (char *) NULL)
          break;
        (void) strcpy(filelist[*number_entries],entry->d_name);
        if (IsDirectory(entry->d_name))
          (void) strcat(filelist[*number_entries],DirectorySeparator);
        (*number_entries)++;
      }
    entry=readdir(current_directory);
  }
  (void) closedir(current_directory);
  /*
    Sort filelist in ascending order.
  */
  qsort((void *) filelist,*number_entries,sizeof(char **),
    (int (*)(const void *, const void *)) FileCompare);
  return(filelist);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  L o c a l e F i l e n a m e                                                %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
l
%  Function LocaleFilename replaces the contents of the string pointed to
%  by filename by a unique file name relative to the directory.
%
%  The format of the LocaleFilename routine is:
%
%       LocaleFilename(filename)
%
%  A description of each parameter follows.
%
%   o  filename:  Specifies a pointer to an array of characters.  The unique
%      file name is returned in this array.
%
%
*/
Export void LocaleFilename(char *filename)
{
  register char
    *p,
    *q;

  assert(filename != (char *) NULL);
  p=filename+Extent(filename)-1;
  while ((*p != *BasenameSeparator) && (p >= filename))
    p--;
  p++;
  TemporaryFilename(p);
  q=filename+Extent(filename)-1;
  while ((*q != *BasenameSeparator) && (q >= p))
    q--;
  q++;
  (void) strcpy(p,q);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  L S B F i r s t R e a d L o n g                                            %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function LSBFirstReadLong reads a long value as a 32 bit quantity in
%  least-significant byte first order.
%
%  The format of the LSBFirstReadLong routine is:
%
%       value=LSBFirstReadLong(file)
%
%  A description of each parameter follows.
%
%    o value:  Function LSBFirstReadLong returns an unsigned long read from
%      the file.
%
%   o  file:  Specifies the file to read the data from.
%
%
*/
unsigned long LSBFirstReadLong(FILE *file)
{
  unsigned char
    buffer[4];

  unsigned int
    status;

  unsigned long
    value;

  assert(file != (FILE *) NULL);
  status=ReadData((char *) buffer,1,4,file);
  if (status == False)
    return((unsigned long) ~0);
  value=(unsigned int) (buffer[3] << 24);
  value|=(unsigned int) (buffer[2] << 16);
  value|=(unsigned int) (buffer[1] << 8);
  value|=(unsigned int) (buffer[0]);
  return(value);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  L S B F i r s t R e a d S h o r t                                          %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function LSBFirstReadShort reads a short value as a 16 bit quantity in
%  least-significant byte first order.
%
%  The format of the LSBFirstReadShort routine is:
%
%       value=LSBFirstReadShort(file)
%
%  A description of each parameter follows.
%
%    o value:  Function LSBFirstReadShort returns an unsigned short read from
%      the file.
%
%   o  file:  Specifies the file to read the data from.
%
%
*/
unsigned short LSBFirstReadShort(FILE *file)
{
  unsigned char
    buffer[2];

  unsigned int
    status;

  unsigned short
    value;

  assert(file != (FILE *) NULL);
  status=ReadData((char *) buffer,1,2,file);
  if (status == False)
    return((unsigned short) ~0);
  value=(unsigned short) (buffer[1] << 8);
  value|=(unsigned short) (buffer[0]);
  return(value);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  L S B F i r s t W r i t e L o n g                                          %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function LSBFirstWriteLong writes a long value as a 32 bit quantity in
%  least-significant byte first order.
%
%  The format of the LSBFirstWriteLong routine is:
%
%       LSBFirstWriteLong(value,file)
%
%  A description of each parameter follows.
%
%   o  value:  Specifies the value to write.
%
%   o  file:  Specifies the file to write the data to.
%
%
*/
void LSBFirstWriteLong(const unsigned long value,FILE *file)
{
  unsigned char
    buffer[4];

  assert(file != (FILE *) NULL);
  buffer[0]=(unsigned char) (value);
  buffer[1]=(unsigned char) ((value) >> 8);
  buffer[2]=(unsigned char) ((value) >> 16);
  buffer[3]=(unsigned char) ((value) >> 24);
  (void) fwrite((char *) buffer,1,4,file);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  L S B F i r s t W r i t e S h o r t                                        %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function LSBFirstWriteShort writes a long value as a 16 bit quantity in
%  least-significant byte first order.
%
%  The format of the LSBFirstWriteShort routine is:
%
%       LSBFirstWriteShort(value,file)
%
%  A description of each parameter follows.
%
%   o  value:  Specifies the value to write.
%
%   o  file:  Specifies the file to write the data to.
%
%
*/
void LSBFirstWriteShort(const unsigned int value,FILE *file)
{
  unsigned char
    buffer[2];

  assert(file != (FILE *) NULL);
  buffer[0]=(unsigned char) (value);
  buffer[1]=(unsigned char) ((value) >> 8);
  (void) fwrite((char *) buffer,1,2,file);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  M S B F i r s t O r d e r L o n g                                          %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function MSBFirstOrderLong converts a least-significant byte first buffer
%  of integers to most-significant byte first.
%
%  The format of the MSBFirstOrderLong routine is:
%
%       MSBFirstOrderLong(p,length);
%
%  A description of each parameter follows.
%
%   o  p:  Specifies a pointer to a buffer of integers.
%
%   o  length:  Specifies the length of the buffer.
%
%
*/
void MSBFirstOrderLong(register char *p,const unsigned int length)
{
  register char
    c,
    *q,
    *sp;

  assert(p != (char *) NULL);
  q=p+length;
  while (p < q)
  {
    sp=p+3;
    c=(*sp);
    *sp=(*p);
    *p++=c;
    sp=p+1;
    c=(*sp);
    *sp=(*p);
    *p++=c;
    p+=2;
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  M S B F i r s t O r d e r S h o r t                                        %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function MSBFirstOrderShort converts a least-significant byte first buffer
%  of integers to most-significant byte first.
%
%  The format of the MSBFirstOrderShort routine is:
%
%       MSBFirstOrderLongShort(p,length);
%
%  A description of each parameter follows.
%
%   o  p:  Specifies a pointer to a buffer of integers.
%
%   o  length:  Specifies the length of the buffer.
%
%
*/
void MSBFirstOrderShort(register char *p,const unsigned int length)
{
  register char
    c,
    *q;

  assert(p != (char *) NULL);
  q=p+length;
  while (p < q)
  {
    c=(*p);
    *p=(*(p+1));
    p++;
    *p++=c;
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  M S B F i r s t R e a d S h o r t                                          %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function MSBFirstReadShort reads a short value as a 16 bit quantity in
%  most-significant byte first order.
%
%  The format of the MSBFirstReadShort routine is:
%
%       value=MSBFirstReadShort(file)
%
%  A description of each parameter follows.
%
%    o value:  Function MSBFirstReadShort returns an unsigned short read from
%      the file.
%
%   o  file:  Specifies the file to read the data from.
%
%
*/
unsigned short MSBFirstReadShort(FILE *file)
{
  unsigned char
    buffer[2];

  unsigned int
    status;

  unsigned short
    value;

  assert(file != (FILE *) NULL);
  status=ReadData((char *) buffer,1,2,file);
  if (status == False)
    return((unsigned short) ~0);
  value=(unsigned int) (buffer[0] << 8);
  value|=(unsigned int) (buffer[1]);
  return(value);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  M S B F i r s t R e a d L o n g                                            %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function MSBFirstReadLong reads a long value as a 32 bit quantity in
%  most-significant byte first order.
%
%  The format of the MSBFirstReadLong routine is:
%
%       value=MSBFirstReadLong(file)
%
%  A description of each parameter follows.
%
%    o value:  Function MSBFirstReadLong returns an unsigned long read from
%      the file.
%
%   o  file:  Specifies the file to read the data from.
%
%
*/
unsigned long MSBFirstReadLong(FILE *file)
{
  unsigned char
    buffer[4];

  unsigned int
    status;

  unsigned long
    value;

  assert(file != (FILE *) NULL);
  status=ReadData((char *) buffer,1,4,file);
  if (status == False)
    return((unsigned long) ~0);
  value=(unsigned int) (buffer[0] << 24);
  value|=(unsigned int) (buffer[1] << 16);
  value|=(unsigned int) (buffer[2] << 8);
  value|=(unsigned int) (buffer[3]);
  return(value);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  M S B F i r s t W r i t e L o n g                                          %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function MSBFirstWriteLong writes a long value as a 32 bit quantity in
%  most-significant byte first order.
%
%  The format of the MSBFirstWriteLong routine is:
%
%       MSBFirstWriteLong(value,file)
%
%  A description of each parameter follows.
%
%   o  value:  Specifies the value to write.
%
%   o  file:  Specifies the file to write the data to.
%
%
*/
void MSBFirstWriteLong(const unsigned long value,FILE *file)
{
  unsigned char
    buffer[4];

  assert(file != (FILE *) NULL);
  buffer[0]=(unsigned char) ((value) >> 24);
  buffer[1]=(unsigned char) ((value) >> 16);
  buffer[2]=(unsigned char) ((value) >> 8);
  buffer[3]=(unsigned char) (value);
  (void) fwrite((char *) buffer,1,4,file);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  M S B F i r s t W r i t e S h o r t                                        %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function MSBFirstWriteShort writes a long value as a 16 bit quantity in
%  most-significant byte first order.
%
%  The format of the MSBFirstWriteShort routine is:
%
%       MSBFirstWriteShort(value,file)
%
%  A description of each parameter follows.
%
%   o  value:  Specifies the value to write.
%
%   o  file:  Specifies the file to write the data to.
%
%
*/
void MSBFirstWriteShort(const unsigned int value,FILE *file)
{
  unsigned char
    buffer[2];

  assert(file != (FILE *) NULL);
  buffer[0]=(unsigned char) ((value) >> 8);
  buffer[1]=(unsigned char) (value);
  (void) fwrite((char *) buffer,1,2,file);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  M u l t i l i n e C e n s u s                                              %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function MultilineCensus returns the number of lines within a label.  A line
%  is represented by a \n character.
%
%  The format of the MultilineCenus routine is:
%
%       MultilineCenus(label)
%
%  A description of each parameter follows.
%
%   o  label:  This character string is the label.
%
%
*/
int MultilineCensus(const char *label)
{
  int
    number_lines;

  /*
    Determine the number of lines within this label.
  */
  if (label == (char *) NULL)
    return(0);
  for (number_lines=1; *label != '\0'; label++)
    if (*label == '\n')
      number_lines++;
  return(number_lines);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  P o s t s c r i p t G e o m e t r y                                        %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function PostscriptGeometry replaces any page mneumonic with the equivalent
%  size in picas.
%
%  The format of the PostscriptGeometry routine is:
%
%       geometry=PostscriptGeometry(page)
%
%  A description of each parameter follows.
%
%   o  page:  Specifies a pointer to an array of characters.  The string is
%      either a Postscript page name (e.g. A4) or a postscript page geometry
%      (e.g. 612x792+36+36).
%
%
*/
Export char *PostscriptGeometry(const char *page)
{
  static char
    *PageSizes[][2]=
    {
      { "LETTER", "612x792" },
      { "TABLOID", "792x1224" },
      { "LEDGER", "1224x792" },
      { "LEGAL", " 612x1008" },
      { "STATEMENT", "396x612" },
      { "EXECUTIVE", "540x720" },
      { "A3", "842x1191" },
      { "A4", "595x842" },
      { "A5", "421x595" },
      { "B4", "729x1032" },
      { "B5", "516x729" },
      { "FOLIO", "612x936" },
      { "QUARTO", "610x780" },
      { "10x14", "720x1008" },
      { (char *) NULL, (char *) NULL }
    };

  char
    c,
    *geometry;

  register char
    *p;

  register int
    i;

  /*
    Allocate page geometry memory.
  */
  assert(page != (char *) NULL);
  geometry=(char *) malloc((Extent(page)+MaxTextExtent)*sizeof(char));
  if (geometry == (char *) NULL)
    {
       Warning("Unable to translate page geometry","Memory allocation failed");
       return((char *) NULL);
    }
  /*
    Comparison is case insensitive.
  */
  (void) strcpy(geometry,page);
  if (!isdigit(*geometry))
    for (p=geometry; *p != '\0'; p++)
    {
      c=(*p);
      if (islower(c))
        *p=toupper(c);
    }
  /*
    Comparison is case insensitive.
  */
  for (i=0; *PageSizes[i] != (char *) NULL; i++)
    if (strncmp(PageSizes[i][0],geometry,Extent(PageSizes[i][0])) == 0)
      {
        /*
          Replace mneumonic with the equivalent size in dots-per-inch.
        */
        (void) strcpy(geometry,PageSizes[i][1]);
        (void) strcat(geometry,page+Extent(PageSizes[i][0]));
        break;
      }
  return(geometry);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d D a t a                                                            %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadData reads data from the image file and returns it.  If it
%  cannot read the requested number of items, False is returned indicating
%  an error.
%
%  The format of the ReadData routine is:
%
%      status=ReadData(data,size,number_items,file)
%
%  A description of each parameter follows:
%
%    o status:  Function ReadData returns True if all the data requested
%      is obtained without error, otherwise False.
%
%    o data:  Specifies an area to place the information reuested from
%      the file.
%
%    o size:  Specifies an integer representing the length of an
%      individual item to be read from the file.
%
%    o number_items:  Specifies an integer representing the number of items
%      to read from the file.
%
%    o file:  Specifies a file to read the data.
%
%
*/
unsigned int ReadData(char *data,const unsigned int size,
  const unsigned int number_items,FILE *file)
{
  long
    bytes,
    count;

  assert(data != (char *) NULL);
  assert(file != (FILE *) NULL);
  count=0;
  for (bytes=size*number_items; bytes > 0; bytes-=count)
  {
    count=(long) fread(data,1,bytes,file);
    if (count <= 0)
      return(False);
    data+=count;
  }
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d D a t a B l o c k                                                  %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadDataBlock reads data from the image file and returns it.  The
%  amount of data is determined by first reading a count byte.  If
%  ReadDataBlock cannot read the requested number of items, `-1' is returned
%  indicating an error.
%
%  The format of the ReadData routine is:
%
%      status=ReadData(data,file)
%
%  A description of each parameter follows:
%
%    o status:  Function ReadData returns the number of characters read
%      unless there is an error, otherwise `-1'.
%
%    o data:  Specifies an area to place the information reuested from
%      the file.
%
%    o file:  Specifies a file to read the data.
%
%
*/
int ReadDataBlock(char *data,FILE *file)
{
  unsigned char
    count;

  unsigned int
    status;

  assert(data != (char *) NULL);
  assert(file != (FILE *) NULL);
  status=ReadData((char *) &count,1,1,file);
  if (status == False)
    return(-1);
  if (count == 0)
    return(0);
  status=ReadData(data,1,(unsigned int) count,file);
  if (status == False)
    return(-1);
  return(count);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  S t r i n g T o L i s t                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function StringToList converts a text string into a list by segmenting the
%  text string at each carriage return discovered.  The list is converted to
%  HEX characters if any control characters are discovered within the text
%  string.
%
%  The format of the StringToList routine is:
%
%      list=StringToList(text)
%
%  A description of each parameter follows:
%
%    o list:  Function StringToList returns the string list unless an error
%      occurs, otherwise NULL.
%
%    o text:  Specifies the string to segment into a list.
%
%
*/
Export char **StringToList(char *text)
{
  char
    **textlist;

  register char
    *p,
    *q;

  register int
    i;

  unsigned int
    lines;

  if (text == (char *) NULL)
    return((char **) NULL);
  for (p=text; *p != '\0'; p++)
    if (iscntrl(*p) && !isspace(*p))
      break;
  if (*p == '\0')
    {
      /*
        Convert string to an ASCII list.
      */
      lines=1;
      for (p=text; *p != '\0'; p++)
        if (*p == '\n')
          lines++;
      textlist=(char **) malloc((lines+1)*sizeof(char *));
      if (textlist == (char **) NULL)
        {
          Warning("Unable to convert text","Memory allocation failed");
          return((char **) NULL);
        }
      p=text;
      for (i=0; i < lines; i++)
      {
        for (q=p; *q != '\0'; q++)
          if ((*q == '\r') || (*q == '\n'))
            break;
        textlist[i]=(char *) malloc((q-p+1)*sizeof(char));
        if (textlist[i] == (char *) NULL)
          {
            Warning("Unable to convert text","Memory allocation failed");
            return((char **) NULL);
          }
        (void) strncpy(textlist[i],p,q-p);
        textlist[i][q-p]='\0';
        if (*q == '\r')
          q++;
        p=q+1;
      }
    }
  else
    {
      char
        hex_string[MaxTextExtent];

      register int
        j;

      /*
        Convert string to a HEX list.
      */
      lines=(Extent(text)/0x14)+1;
      textlist=(char **) malloc((lines+1)*sizeof(char *));
      if (textlist == (char **) NULL)
        {
          Warning("Unable to convert text","Memory allocation failed");
          return((char **) NULL);
        }
      p=text;
      for (i=0; i < lines; i++)
      {
        textlist[i]=(char *) malloc(900*sizeof(char));
        if (textlist[i] == (char *) NULL)
          {
            Warning("Unable to convert text","Memory allocation failed");
            return((char **) NULL);
          }
        (void) sprintf(textlist[i],"0x%08x: ",(unsigned int) (i*0x14));
        q=textlist[i]+Extent(textlist[i]);
        for (j=1; j <= Min(Extent(p),0x14); j++)
        {
          (void) sprintf(hex_string,"%02x",(unsigned int) (*(p+j)));
          (void) strcpy(q,hex_string);
          q+=2;
          if ((j % 0x04) == 0)
            *q++=' ';
        }
        for (; j <= 0x14; j++)
        {
          *q++=' ';
          *q++=' ';
          if ((j % 0x04) == 0)
            *q++=' ';
        }
        *q++=' ';
        for (j=1; j <= Min(Extent(p),0x14); j++)
        {
          if (isprint(*p))
            *q++=(*p);
          else
            *q++='-';
          p++;
        }
        *q='\0';
      }
    }
  textlist[i]=(char *) NULL;
  return(textlist);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   S t r i p                                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function Strip strips the whitespace from the beginning and end of a string
%  of characters.
%
%  The format of the Strip routine is:
%
%     Strip(data)
%
%  A description of each parameter follows:
%
%    o data: Specifies an array of characters.
%
%
*/
void Strip(char *data)
{
  long
    count;

  register char
    *p,
    *q;

  register int
    i;

  assert(data != (char *) NULL);
  if (*data == '\0')
    return;
  p=data;
  while (isspace(*p))
    p++;
  q=data+Extent(data)-1;
  while (isspace(*q) && (q > p))
    q--;
  count=q-p+1;
  q=data;
  for (i=0; i < count; i++)
    *q++=(*p++);
  *q='\0';
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  T e m p o r a r y F i l e n a m e                                          %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function TemporaryFilename replaces the contents of the string pointed to
%  by filename by a unique file name.
%
%  The format of the TemporaryFilename routine is:
%
%       TemporaryFilename(filename)
%
%  A description of each parameter follows.
%
%   o  filename:  Specifies a pointer to an array of characters.  The unique
%      file name is returned in this array.
%
%
*/
Export void TemporaryFilename(char *filename)
{
  char
    *directory;

  assert(filename != (char *) NULL);
#if !defined(vms) && !defined(macintosh) && !defined(WIN32)
  directory=(char *) getenv("TMPDIR");
  if (directory == (char *) NULL)
    directory=TemporaryDirectory;
  (void) sprintf(filename,TemporaryTemplate,directory);
  (void) mktemp(filename);
#else
  (void) tmpnam(filename);
#endif
}
