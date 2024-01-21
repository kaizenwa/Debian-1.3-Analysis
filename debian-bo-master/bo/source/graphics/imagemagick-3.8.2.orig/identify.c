/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%           IIIII  DDDD   EEEEE  N   N  TTTTT  IIIII  FFFFF  Y   Y            %
%             I    D   D  E      NN  N    T      I    F       Y Y             %
%             I    D   D  EEE    N N N    T      I    FFF      Y              %
%             I    D   D  E      N  NN    T      I    F        Y              %
%           IIIII  DDDD   EEEEE  N   N    T    IIIII  F        Y              %
%                                                                             %
%                                                                             %
%               Identify an Image Format and Characteristics.                 %
%                                                                             %
%                                                                             %
%                                                                             %
%                           Software Design                                   %
%                             John Cristy                                     %
%                            September 1994                                   %
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
%  Identify describes the format and characteristics of one or more image
%  files.  It will also report if an image is incomplete or corrupt.
%
%
*/

/*
  Include declarations.
*/
#include "magick.h"
#include "version.h"

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   U s a g e                                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Procedure Usage displays the program usage;
%
%  The format of the Usage routine is:
%
%      Usage(client_name)
%
%    o client_name: a character string representing the name of the client
%      program.
%
%
*/
static void Usage(const char *client_name)
{
  char
    **p;

  static char
    *options[]=
    {
      "-verbose            print detailed information about the image",
      (char *) NULL
    };

  (void) printf("Version: %s\n\n",Version);
  (void) printf("Usage: %s [ options ... ] file [ file ... ]\n",client_name);
  (void) printf("\nWhere options include:\n");
  for (p=options; *p != (char *) NULL; p++)
    (void) printf("  %s\n",*p);
  (void) printf(
    "\nChange '-' to '+' in any option above to reverse its effect.  For\n");
  (void) printf(
    "example,  specify +verbose to display a one description of each image.\n");
  exit(1);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%    M a i n                                                                  %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
*/
int main(int argc,char **argv)
{
  char
    *client_name,
    *option;

  Image
    *image,
    *next_image;

  ImageInfo
    image_info;

  register int
    i;

  unsigned int
    count,
    verbose;

  /*
    Initialize program variables.
  */
  ReadCommandlLine(argc,&argv);
  client_name=ClientName(*argv);
  if (argc < 2)
    Usage(client_name);
  count=0;
  verbose=False;
  /*
    Identify an image.
  */
  GetImageInfo(&image_info);
  image_info.size=DefaultTileGeometry;
  ExpandFilenames(&argc,&argv);
  for (i=1; i < argc; i++)
  {
    option=argv[i];
    if (strncmp("-h",option,2) == 0)
      Usage(client_name);
    if (strncmp("-?",option,2) == 0)
      Usage(client_name);
    if (strncmp("-v",argv[i],2) == 0)
      {
        verbose=True;
        continue;
      }
    (void) strcpy(image_info.filename,argv[i]);
    image=ReadImage(&image_info);
    if (image == (Image *) NULL)
      continue;
    image->columns=image->magick_columns;
    image->rows=image->magick_rows;
    do
    {
      next_image=image->next;
      if (image->scene == 0)
        image->scene=count++;
      if (verbose)
        NumberColors(image,(FILE *) NULL);
      DescribeImage(image,stdout,verbose);
      if (next_image != (Image *) NULL)
        image=next_image;
    } while (next_image != (Image *) NULL);
    while (image->previous != (Image *) NULL)
      image=image->previous;
    DestroyImages(image);
  }
  exit(0);
  return(False);
}
