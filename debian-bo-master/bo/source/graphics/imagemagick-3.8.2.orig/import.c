/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                 IIIII  M   M  PPPP    OOO   RRRR   TTTTT                    %
%                   I    MM MM  P   P  O   O  R   R    T                      %
%                   I    M M M  PPPP   O   O  RRRR     T                      %
%                   I    M   M  P      O   O  R R      T                      %
%                 IIIII  M   M  P       OOO   R  R     T                      %
%                                                                             %
%                                                                             %
%             Import X11 image to a machine independent format.               %
%                                                                             %
%                                                                             %
%                                                                             %
%                           Software Design                                   %
%                             John Cristy                                     %
%                              July 1992                                      %
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
%  Import is an X Window System window dumping utility.  Import allows X
%  users to store window images in a specially formatted dump file.  This
%  file can then be read by the Display utility for redisplay, printing,
%  editing, formatting, archiving, image processing, etc.  The target
%  window can be specified by id or name or be selected by clicking the
%  mouse in the desired window.  The keyboard bell is rung once at the
%  beginning of the dump and twice when the dump is completed.
%
%  The import program command syntax is:
%
%  Usage: import [options ...] [file]
%
%  Where options include:
%    -border             include image borders in the output image
%    -colors value       preferred number of colors in the image
%    -colorspace type    GRAY, OHTA, RGB, XYZ, YCbCr, YIQ, YPbPr, or YUV
%    -comment string     annotate image with comment
%    -compress type      RunlengthEncoded or Zip
%    -crop geometry      preferred size and location of the cropped image
%    -delay seconds      pause before selecting target window
%    -density geometry   vertical and horizontal density of the image
%    -descend            obtain image by descending window hierarchy
%    -display server     X server to contact
%    -dispose method     GIF disposal method
%    -dither             apply Floyd/Steinberg error diffusion to image
%    -frame              include window manager frame
%    -geometry geometry  perferred size or location of the image
%    -interlace type     None, Line, Plane, or Partition
%    -label name         assign a label to an image
%    -monochrome         transform image to black and white
%    -negate             apply color inversion to image
%    -page geometry      size and location of the Postscript page
%    -quality value      JPEG quality setting
%    -rotate degrees     apply Paeth rotation to the image
%    -scene value        image scene number
%    -screen             select image from root window
%    -transparent color  make this color transparent within the image
%    -treedepth value    depth of the color classification tree
%    -verbose            print detailed information about the image
%    -window id          select window with this id or name
%
%  Change '-' to '+' in any option above to reverse its effect.
%  For example, +frame means do not window manager frame.
%
%  By default, 'file' is written in the Postscript image format.  To specify
%  a particular image format, precede the filename with an image format
%  name and a colon (i.e. ps:image) or specify the image type as the
%  filename suffix (i.e. image.ps).  Specify 'file' as '-' for standard
%  input or output.
%
%
*/

/*
  Include declarations.
*/
#include "magick.h"
#include "version.h"

/*
  Define declarations.
*/
#define DefaultImportName  "magick.ps"

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
      "-border             include image borders in the output image",
      "-colors value       preferred number of colors in the image",
      "-colorspace type    GRAY, OHTA, RGB, XYZ, YCbCr, YIQ, YPbPr, or YUV",
      "-comment string     annotate image with comment",
      "-compress type      RunlengthEncoded or Zip",
      "-crop geometry      preferred size and location of the cropped image",
      "-delay seconds      pause before selecting target window",
      "-density geometry   vertical and horizontal density of the image",
      "-descend            obtain image by descending window hierarchy",
      "-display server     X server to contact",
      "-dispose method     GIF disposal method",
      "-dither             apply Floyd/Steinberg error diffusion to image",
      "-frame              include window manager frame",
      "-geometry geometry  perferred size or location of the image",
      "-interlace type     None, Line, Plane, or Partition",
      "-label name         assign a label to an image",
      "-monochrome         transform image to black and white",
      "-negate             apply color inversion to image",
      "-page geometry      size and location of the Postscript page",
      "-quality value      JPEG quality setting",
      "-rotate degrees     apply Paeth rotation to the image",
      "-scene value        image scene number",
      "-treedepth value    depth of the color classification tree",
      "-transparent color   make this color transparent within the image",
      "-screen             select image from root window",
      "-verbose            print detailed information about the image",
      "-window id          select window with this id or name",
      (char *) NULL
    };

  (void) printf("Version: %s\n\n",Version);
  (void) printf("Usage: %s [options ...] [file]\n",client_name);
  (void) printf("\nWhere options include:\n");
  for (p=options; *p != (char *) NULL; p++)
    (void) printf("  %s\n",*p);
  (void) printf(
    "\nChange '-' to '+' in any option above to reverse its effect.\n");
  (void) printf(
    "For example, +frame means do not include window manager frame.\n");
  (void) printf(
  "\nBy default, 'file' is written in the Postscript image format.  To\n");
  (void) printf(
    "specify a particular image format, precede the filename with an image\n");
  (void) printf(
    "format name and a colon (i.e. ps:image) or specify the image type as\n");
  (void) printf(
    "the filename suffix (i.e. image.ps).  Specify 'file' as '-' for\n");
  (void) printf("standard input or output.\n");
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
int main(int argc,char *argv[])
{
  char
    *client_name,
    *filename,
    *option,
    *resource_value,
    *target_window;

  Display
    *display;

  Image
    *image;

  ImageInfo
    image_info;

  int
    i,
    x;

  unsigned int
    borders,
    descend,
    frame,
    scene,
    screen;

  XResourceInfo
    resource_info;

  XrmDatabase
    resource_database;

  /*
    Connect to X server.
  */
  ReadCommandlLine(argc,&argv);
  client_name=ClientName(*argv);
  image_info.server_name=(char *) NULL;
  for (i=1; i < argc; i++)
  {
    /*
      Check command line for server name.
    */
    option=argv[i];
    if ((Extent(option) == 1) || ((*option != '-') && (*option != '+')))
      continue;
    if (strcmp("display",option+1) == 0)
      {
        /*
          User specified server name.
        */
        i++;
        if (i == argc)
          Error("Missing server name on -display",(char *) NULL);
        image_info.server_name=argv[i];
        break;
      }
    if (strncmp("help",option+1,2) == 0)
      Usage(client_name);
  }
  display=XOpenDisplay(image_info.server_name);
  if (display == (Display *) NULL)
    Error("Unable to connect to X server",XDisplayName(image_info.server_name));
  /*
    Set our forgiving error handler.
  */
  XSetErrorHandler(XError);
  /*
    Get user defaults from X resource database.
  */
  resource_database=XGetResourceDatabase(display,client_name);
  GetImageInfo(&image_info);
  XGetResourceInfo(resource_database,client_name,&resource_info);
  resource_value=
    XGetResourceClass(resource_database,client_name,"border","False");
  borders=IsTrue(resource_value);
  image_info.density=XGetResourceClass(resource_database,client_name,"density",
    (char *) NULL);
  resource_value=
    XGetResourceClass(resource_database,client_name,"descend","False");
  descend=IsTrue(resource_value);
  resource_value=
    XGetResourceClass(resource_database,client_name,"frame","False");
  frame=IsTrue(resource_value);
  resource_value=
    XGetResourceClass(resource_database,client_name,"interlace","plane");
  image_info.interlace=UndefinedInterlace;
  if (Latin1Compare("none",resource_value) == 0)
    image_info.interlace=NoneInterlace;
  if (Latin1Compare("line",resource_value) == 0)
    image_info.interlace=LineInterlace;
  if (Latin1Compare("plane",resource_value) == 0)
    image_info.interlace=PlaneInterlace;
  if (Latin1Compare("partition",resource_value) == 0)
    image_info.interlace=PartitionInterlace;
  if (image_info.interlace == UndefinedInterlace)
    Warning("Unrecognized interlace type",resource_value);
  image_info.page=XGetResourceClass(resource_database,client_name,
    "pageGeometry",(char *) NULL);
  resource_value=
    XGetResourceClass(resource_database,client_name,"quality","85");
  image_info.quality=atoi(resource_value);
  resource_value=XGetResourceClass(resource_database,client_name,"rotate","0");
  resource_value=XGetResourceClass(resource_database,client_name,"scene","0");
  scene=atoi(resource_value);
  resource_value=
    XGetResourceClass(resource_database,client_name,"screen","False");
  screen=IsTrue(resource_value);
  resource_value=
    XGetResourceClass(resource_database,client_name,"verbose","False");
  image_info.verbose=IsTrue(resource_value);
  /*
    Check command syntax.
  */
  filename=(char *) NULL;
  target_window=(char *) NULL;
  for (i=1; i < argc; i++)
  {
    option=argv[i];
    if ((Extent(option) < 2) || ((*option != '-') && (*option != '+')))
      filename=argv[i];
    else
      switch(*(option+1))
      {
        case 'b':
        {
          if (strcmp("border",option+1) == 0)
            {
              borders=(*option == '-');
              break;
            }
          if (strncmp("bordercolor",option+1,7) == 0)
            {
              resource_info.border_color=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing color on -bordercolor",(char *) NULL);
                  resource_info.border_color=argv[i];
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'c':
        {
          if (strncmp("colors",option+1,7) == 0)
            {
              resource_info.number_colors=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing colors on -colors",(char *) NULL);
                  resource_info.number_colors=atoi(argv[i]);
                }
              break;
            }
          if (strncmp("colorspace",option+1,7) == 0)
            {
              resource_info.colorspace=RGBColorspace;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing type on -colorspace",(char *) NULL);
                  option=argv[i];
                  resource_info.colorspace=UndefinedColorspace;
                  if (Latin1Compare("gray",option) == 0)
                    {
                      resource_info.colorspace=GRAYColorspace;
                      resource_info.number_colors=256;
                      resource_info.tree_depth=8;
                    }
                  if (Latin1Compare("ohta",option) == 0)
                    resource_info.colorspace=OHTAColorspace;
                  if (Latin1Compare("rgb",option) == 0)
                    resource_info.colorspace=RGBColorspace;
                  if (Latin1Compare("transparent",option) == 0)
                    resource_info.colorspace=TransparentColorspace;
                  if (Latin1Compare("xyz",option) == 0)
                    resource_info.colorspace=XYZColorspace;
                  if (Latin1Compare("ycbcr",option) == 0)
                    resource_info.colorspace=YCbCrColorspace;
                  if (Latin1Compare("yiq",option) == 0)
                    resource_info.colorspace=YIQColorspace;
                  if (Latin1Compare("ypbpr",option) == 0)
                    resource_info.colorspace=YPbPrColorspace;
                  if (Latin1Compare("yuv",option) == 0)
                    resource_info.colorspace=YUVColorspace;
                  if (resource_info.colorspace == UndefinedColorspace)
                    Error("Invalid colorspace type on -colorspace",option);
                }
              break;
            }
          if (strncmp("comment",option+1,4) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing comment on -comment",(char *) NULL);
                }
              break;
            }
          if (strncmp("compress",option+1,3) == 0)
            {
              image_info.compression=NoCompression;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing type on -compress",(char *) NULL);
                  option=argv[i];
                  if (Latin1Compare("runlengthencoded",option) == 0)
                    image_info.compression=RunlengthEncodedCompression;
                  else
                    if (Latin1Compare("zip",option) == 0)
                      image_info.compression=ZipCompression;
                    else
                      Error("Invalid compression type on -compress",option);
                }
              break;
            }
          if (strncmp("crop",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    Error("Missing geometry on -crop",(char *) NULL);
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'd':
        {
          if (strncmp("delay",option+1,3) == 0)
            {
              resource_info.delay=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing seconds on -delay",(char *) NULL);
                  resource_info.delay=atoi(argv[i]);
                }
              break;
            }
          if (strncmp("density",option+1,3) == 0)
            {
              image_info.density=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    Error("Missing geometry on -density",(char *) NULL);
                  image_info.density=argv[i];
                }
              break;
            }
          if (strncmp("descend",option+1,3) == 0)
            {
              descend=(*option == '-');
              break;
            }
          if (strcmp("display",option+1) == 0)
            {
              image_info.server_name=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing server name on -display",(char *) NULL);
                  image_info.server_name=argv[i];
                }
              break;
            }
          if (strncmp("dispose",option+1,5) == 0)
            {
              image_info.dispose=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing method on -dispose",(char *) NULL);
                  image_info.dispose=PostscriptGeometry(argv[i]);
                }
              break;
            }
          if (strncmp("dither",option+1,3) == 0)
            {
              image_info.dither=(*option == '-');
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'f':
        {
          argv[i]="-ignore";  /* resolve option confict */
          frame=(*option == '-');
          break;
        }
        case 'g':
        {
          if (strncmp("geometry",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    Error("Missing geometry on -geometry",(char *) NULL);
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'h':
        {
          if (strncmp("help",option+1,2) == 0)
            {
              Usage(client_name);
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'i':
        {
          if (strncmp("interlace",option+1,3) == 0)
            {
              image_info.interlace=NoneInterlace;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing type on -interlace",(char *) NULL);
                  option=argv[i];
                  image_info.interlace=UndefinedInterlace;
                  if (Latin1Compare("none",option) == 0)
                    image_info.interlace=NoneInterlace;
                  if (Latin1Compare("line",option) == 0)
                    image_info.interlace=LineInterlace;
                  if (Latin1Compare("plane",option) == 0)
                    image_info.interlace=PlaneInterlace;
                  if (Latin1Compare("partition",option) == 0)
                    image_info.interlace=PartitionInterlace;
                  if (image_info.interlace == UndefinedInterlace)
                    Error("Invalid interlace type on -interlace",option);
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'l':
        {
          if (strncmp("label",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing label name on -label",(char *) NULL);
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'm':
        {
          if (strncmp("monochrome",option+1,2) == 0)
            {
              image_info.monochrome=(*option == '-');
              if (image_info.monochrome)
                {
                  resource_info.number_colors=2;
                  resource_info.tree_depth=8;
                  resource_info.colorspace=GRAYColorspace;
                }
              break;
            }
          Error("Unrecognized option",option);
        }
        case 'n':
        {
          if (strncmp("negate",option+1,2) == 0)
            break;
          Error("Unrecognized option",option);
        }
        case 'p':
        {
          if (strncmp("page",option+1,3) == 0)
            {
              image_info.page=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing page geometry on -page",(char *) NULL);
                  image_info.page=PostscriptGeometry(argv[i]);
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'q':
        {
          if (strncmp("quality",option+1,2) == 0)
            {
              image_info.quality=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing quality on -quality",(char *) NULL);
                  image_info.quality=atoi(argv[i]);
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'r':
        {
          if (strncmp("rotate",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                     Error("Missing degrees on -rotate",(char *) NULL);
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 's':
        {
          if (strncmp("scene",option+1,3) == 0)
            {
              i++;
              if ((i == argc) || !sscanf(argv[i],"%d",&x))
                Error("Missing scene on -scene",(char *) NULL);
              scene=atoi(argv[i]);
              break;
            }
          if (strncmp("screen",option+1,3) == 0)
            {
              screen=(*option == '-');
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 't':
        {
          if (strncmp("transparent",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing color on -transparent",(char *) NULL);
                }
              break;
            }
          if (strncmp("treedepth",option+1,3) == 0)
            {
              resource_info.tree_depth=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing depth on -treedepth",(char *) NULL);
                  resource_info.tree_depth=atoi(argv[i]);
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'w':
        {
          i++;
          if (i == argc)
            Error("Missing id, name, or 'root' on -window",(char *) NULL);
          target_window=argv[i];
          break;
        }
        case 'v':
        {
          image_info.verbose=(*option == '-');
          break;
        }
        case '?':
        {
          Usage(client_name);
          break;
        }
        default:
        {
          Error("Unrecognized option",option);
          break;
        }
      }
  }
  if (filename == (char *) NULL)
    filename=DefaultImportName;
  /*
    Read image from X server.
  */
  if (target_window != (char *) NULL)
    (void) strcpy(image_info.filename,target_window);
  if (resource_info.delay != 0)
    (void) sleep(resource_info.delay);
  image=ReadXImage(&image_info,frame,borders,screen,descend);
  if (image == (Image *) NULL)
    exit(1);
  (void) strcpy(image->magick,"X");
  /*
    Transmogrify image as defined by the image processing options.
  */
  MogrifyImage(&image_info,argc,argv,&image);
  /*
    Write import X image.
  */
  image->scene=scene;
  (void) strcpy(image_info.filename,filename);
  (void) strcpy(image->magick,"PS");
  (void) strcpy(image->filename,filename);
  (void) WriteImage(&image_info,image);
  if (image_info.verbose)
    DescribeImage(image,stderr,False);
  exit(0);
  return(False);
}
