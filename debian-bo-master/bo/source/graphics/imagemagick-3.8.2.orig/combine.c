/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                CCCC   OOO   M   M  BBBB   IIIII  N   N  EEEEE               %
%               C      O   O  MM MM  B   B    I    NN  N  E                   %
%               C      O   O  M M M  BBBB     I    N N N  EEE                 %
%               C      O   O  M   M  B   B    I    N  NN  E                   %
%                CCCC   OOO   M   N  BBBB   IIIII  N   N  EEEEE               %
%                                                                             %
%                                                                             %
%                        Digitally combine two images.                        %
%                                                                             %
%                                                                             %
%                                                                             %
%                              Software Design                                %
%                                John Cristy                                  %
%                               January 1993                                  %
%                                                                             %
%                                                                             %
%  Copyright 1997 E. I. Dupont de Nemours and Company                         %
%                                                                             %
%  Permission to use, copy, modify, distribute, and sell this software and    %
%  its documentation for any purpose is hereby granted without fee,           %
%  provided that the above Copyright notice appear in all copies and that     %
%  both that Copyright notice and this permission notice appear in            %
%  supporting documentation, and that the name of E. I. Dupont de Nemours     %
%  and Company not be used in advertising or publicity pertaining to          %
%  distribution of the software without specific, written prior               %
%  permission.  E. I. Dupont de Nemours and Company makes no representations  %
%  about the suitability of this software for any purpose.  It is provided    %
%  "as is" without express or implied warranty.                               %
%                                                                             %
%  E. I. Dupont de Nemours and Company disclaims all warranties with regard   %
%  to this software, including all implied warranties of merchantability      %
%  and fitness, in no event shall E. I. Dupont de Nemours and Company be      %
%  liable for any special, indirect or consequential damages or any           %
%  damages whatsoever resulting from loss of use, data or profits, whether    %
%  in an action of contract, negligence or other tortious action, arising     %
%  out of or in connection with the use or performance of this software.      %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  The combine program syntax is:
%
%  Usage: combine [options ...] image composite [mask] combined
%
%  Where options include:
%    -blend value        blend the two images a given percent
%    -colors value       preferred number of colors in the image
%    -compose operator   composite operator
%    -colorspace type    GRAY, OHTA, RGB, XYZ, YCbCr, YIQ, YPbPr, or YUV
%    -comment string     annotate image with comment
%    -compress type      RunlengthEncoded or Zip
%    -density geometry   vertical and horizontal density of the image
%    -displace geometry  shift image pixels as defined by a displacement map
%    -display server     obtain image or font from this X server
%    -dispose method     GIF disposal method
%    -dither             apply Floyd/Steinberg error diffusion to image
%    -font name          X11 font for displaying text
%    -geometry geometry  perferred size or location of the image
%    -gravity direction  which direction to gravitate towards
%    -interlace type     None, Line, Plane, or Partition
%    -label name         assign a label to an image
%    -matte              store matte channel if the image has one
%    -monochrome         transform image to black and white
%    -negate              apply color inversion to image
%    -page geometry      size and location of the Postscript page
%    -quality value      JPEG quality setting
%    -scene value        image scene number
%    -size geometry      width and height of image
%    -stereo             combine two images to form red-green stereo image
%    -tile               repeat composite operation across image
%    -treedepth value    depth of the color classification tree
%    -verbose            print detailed information about the image
%
%  Change '-' to '+' in any option above to reverse its effect.  For
%  example,  specify +matte to store the image without its matte channel.
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
      "-blend value        blend the two images a given percent",
      "-colors value       preferred number of colors in the image",
      "-colorspace type    GRAY, OHTA, RGB, XYZ, YCbCr, YIQ, YPbPr, or YUV",
      "-comment string     annotate image with comment",
      "-compose operator   composite operator",
      "-compress type      RunlengthEncoded or Zip",
      "-density geometry   vertical and horizontal density of the image",
      "-displace geometry  shift image pixels as defined by a displacement map",
      "-display server     obtain image or font from this X server",
      "-dispose method     GIF disposal method",
      "-dither             apply Floyd/Steinberg error diffusion to image",
      "-font name          X11 font for displaying text",
      "-geometry geometry  perferred size or location of the image",
      "-gravity direction  which direction to gravitate towards",
      "-interlace type     None, Line, Plane, or Partition",
      "-label name         assign a label to an image",
      "-matte              store matte channel if the image has one",
      "-monochrome         transform image to black and white",
      "-negate              apply color inversion to image",
      "-page geometry      size and location of the Postscript page",
      "-quality value      JPEG quality setting",
      "-scene value        image scene number",
      "-size geometry      width and height of image",
      "-stereo             combine two images to form red-green stereo image",
      "-tile               repeat composite operation across image",
      "-treedepth value    depth of the color classification tree",
      "-verbose            print detailed information about the image",
      (char *) NULL
    };

  (void) printf("Version: %s\n\n",Version);
  (void) printf("Usage: %s [options ...] image composite [mask] combined\n",
    client_name);
  (void) printf("\nWhere options include:\n");
  for (p=options; *p != (char *) NULL; p++)
    (void) printf("  %s\n",*p);
  (void) printf(
    "\nChange '-' to '+' in any option above to reverse its effect.  For\n");
  (void) printf(
    "example,  specify +matte to store the image without an matte channel.\n");
  exit(1);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  M a i n                                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
*/
int main(int argc,char *argv[])
{
#define NotInitialized  (unsigned int) (~0)

  char
    *client_name,
    *displacement_geometry,
    *filename,
    *geometry,
    *option,
    *write_filename;

  CompositeOperator
    compose;

  double
    blend;

  Image
    *combined_image,
    *composite_image,
    *image,
    *mask_image;

  ImageInfo
    image_info;

  int
    gravity,
    x,
    y;

  register int
    i;

  unsigned int
    colorspace,
    stereo,
    tile;

  /*
    Initialize program variables.
  */
  ReadCommandlLine(argc,&argv);
  client_name=ClientName(*argv);
  if (argc < 4)
    Usage(client_name);
  /*
    Read image and convert to MIFF format.
  */
  blend=0.0;
  colorspace=RGBColorspace;
  compose=ReplaceCompositeOp;
  composite_image=(Image *) NULL;
  displacement_geometry=(char *) NULL;
  geometry=(char *) NULL;
  gravity=NorthWestGravity;
  GetImageInfo(&image_info);
  image=(Image *) NULL;
  mask_image=(Image *) NULL;
  stereo=False;
  tile=False;
  write_filename=argv[argc-1];
  /*
    Check command syntax.
  */
  filename=(char *) NULL;
  for (i=1; i < (argc-1); i++)
  {
    option=argv[i];
    if ((Extent(option) < 2) || ((*option != '-') && (*option != '+')))
      {
        /*
          Read input images.
        */
        filename=argv[i];
        (void) strcpy(image_info.filename,filename);
        if (image == (Image *) NULL)
          {
            image=ReadImage(&image_info);
            if (image == (Image *) NULL)
              exit(1);
            continue;
          }
        if (mask_image != (Image *) NULL)
          Error("input images already specified",filename);
        if (composite_image == (Image *) NULL)
          {
            composite_image=ReadImage(&image_info);
            if (composite_image == (Image *) NULL)
              exit(1);
            continue;
          }
        mask_image=ReadImage(&image_info);
        if (mask_image == (Image *) NULL)
          exit(1);
      }
    else
      switch(*(option+1))
      {
        case 'b':
        {
          if (strncmp("blend",option+1,3) == 0)
            {
              blend=0.0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing value on -blend",(char *) NULL);
                  blend=atof(argv[i]);
                  compose=BlendCompositeOp;
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
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing colors on -colors",(char *) NULL);
                }
              break;
            }
          if (strncmp("colorspace",option+1,7) == 0)
            {
              colorspace=RGBColorspace;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing type on -colorspace",(char *) NULL);
                  option=argv[i];
                  colorspace=UndefinedColorspace;
                  if (Latin1Compare("gray",option) == 0)
                    colorspace=GRAYColorspace;
                  if (Latin1Compare("ohta",option) == 0)
                    colorspace=OHTAColorspace;
                  if (Latin1Compare("rgb",option) == 0)
                    colorspace=RGBColorspace;
                  if (Latin1Compare("transparent",option) == 0)
                    colorspace=TransparentColorspace;
                  if (Latin1Compare("xyz",option) == 0)
                    colorspace=XYZColorspace;
                  if (Latin1Compare("ycbcr",option) == 0)
                    colorspace=YCbCrColorspace;
                  if (Latin1Compare("yiq",option) == 0)
                    colorspace=YIQColorspace;
                  if (Latin1Compare("ypbpr",option) == 0)
                    colorspace=YPbPrColorspace;
                  if (Latin1Compare("yuv",option) == 0)
                    colorspace=YUVColorspace;
                  if (colorspace == UndefinedColorspace)
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
          if (strncmp("compose",option+1,5) == 0)
            {
              compose=ReplaceCompositeOp;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing type on -compose",(char *) NULL);
                  option=argv[i];
                  compose=UndefinedCompositeOp;
                  if (Latin1Compare("over",option) == 0)
                    compose=OverCompositeOp;
                  if (Latin1Compare("in",option) == 0)
                    compose=InCompositeOp;
                  if (Latin1Compare("out",option) == 0)
                    compose=OutCompositeOp;
                  if (Latin1Compare("atop",option) == 0)
                    compose=AtopCompositeOp;
                  if (Latin1Compare("xor",option) == 0)
                    compose=XorCompositeOp;
                  if (Latin1Compare("plus",option) == 0)
                    compose=PlusCompositeOp;
                  if (Latin1Compare("minus",option) == 0)
                    compose=MinusCompositeOp;
                  if (Latin1Compare("add",option) == 0)
                    compose=AddCompositeOp;
                  if (Latin1Compare("subtract",option) == 0)
                    compose=SubtractCompositeOp;
                  if (Latin1Compare("difference",option) == 0)
                    compose=DifferenceCompositeOp;
                  if (Latin1Compare("bumpmap",option) == 0)
                    compose=BumpmapCompositeOp;
                  if (Latin1Compare("replace",option) == 0)
                    compose=ReplaceCompositeOp;
                  if (compose == UndefinedCompositeOp)
                    Error("Invalid compose type on -compose",option);
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
          Error("Unrecognized option",option);
          break;
        }
        case 'd':
        {
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
          if (strcmp("displace",option+1) == 0)
            {
              displacement_geometry=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%f",(float *) &x))
                    Error("Missing geometry on -displace",(char *) NULL);
                  displacement_geometry=argv[i];
                  compose=DisplaceCompositeOp;
                }
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
          image_info.font=(char *) NULL;
          if (*option == '-')
            {
              i++;
              if (i == argc)
                Error("Missing font name on -font",(char *) NULL);
              image_info.font=argv[i];
            }
          break;
        }
        case 'g':
        {
          if (strncmp("geometry",option+1,2) == 0)
            {
              geometry=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    Error("Missing geometry on -geometry",(char *) NULL);
                  geometry=argv[i];
                }
              break;
            }
          if (strncmp("gravity",option+1,2) == 0)
            {
              gravity=NorthWestGravity;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing type on -gravity",(char *) NULL);
                  option=argv[i];
                  gravity=(-1);
                  if (Latin1Compare("Forget",option) == 0)
                    gravity=ForgetGravity;
                  if (Latin1Compare("NorthWest",option) == 0)
                    gravity=NorthWestGravity;
                  if (Latin1Compare("North",option) == 0)
                    gravity=NorthGravity;
                  if (Latin1Compare("NorthEast",option) == 0)
                    gravity=NorthEastGravity;
                  if (Latin1Compare("West",option) == 0)
                    gravity=WestGravity;
                  if (Latin1Compare("Center",option) == 0)
                    gravity=CenterGravity;
                  if (Latin1Compare("East",option) == 0)
                    gravity=EastGravity;
                  if (Latin1Compare("SouthWest",option) == 0)
                    gravity=SouthWestGravity;
                  if (Latin1Compare("South",option) == 0)
                    gravity=SouthGravity;
                  if (Latin1Compare("SouthEast",option) == 0)
                    gravity=SouthEastGravity;
                  if (Latin1Compare("Static",option) == 0)
                    gravity=StaticGravity;
                  if (gravity == (-1))
                    Error("Invalid gravity type on -gravity",option);
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
          if (strncmp("matte",option+1,5) == 0)
            break;
          if (strncmp("monochrome",option+1,2) == 0)
            break;
          Error("Unrecognized option",option);
          break;
        }
        case 'n':
        {
          if (strncmp("negate",option+1,3) == 0)
            break;
          Error("Unrecognized option",option);
          break;
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
              image_info.quality=atoi(DefaultImageQuality);
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
        case 's':
        {
          if (strncmp("scene",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing scene number on -scene",(char *) NULL);
                }
              break;
            }
          if (strncmp("size",option+1,2) == 0)
            {
              image_info.size=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    Error("Missing geometry on -size",(char *) NULL);
                  image_info.size=argv[i];
                }
              break;
            }
          if (strncmp("stereo",option+1,2) == 0)
            {
              stereo=(*option == '-');
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 't':
        {
          if (strncmp("tile",option+1,2) == 0)
            {
              tile=(*option == '-');
              break;
            }
          Error("Unrecognized option",option);
          if (strncmp("treedepth",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing depth on -treedepth",(char *) NULL);
                }
              break;
            }
          Error("Unrecognized option",option);
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
  if ((image == (Image *) NULL) || (composite_image == (Image *) NULL))
    Usage(client_name);
  if (mask_image != (Image *) NULL)
    {
      CompositeImage(composite_image,AddMaskCompositeOp,mask_image,0,0);
      DestroyImage(mask_image);
    }
  if (compose == BlendCompositeOp)
    {
      register RunlengthPacket
        *p;

      unsigned short
        index;

      /*
        Create mattes for blending.
      */
      index=(unsigned short)
        (DownScale(MaxRGB)-(((int) DownScale(MaxRGB)*blend)/100));
      image->class=DirectClass;
      image->matte=True;
      p=image->pixels;
      for (i=0; i < image->packets; i++)
      {
        p->index=index;
        p++;
      }
      index=(unsigned short) (DownScale(MaxRGB)-index);
      composite_image->class=DirectClass;
      composite_image->matte=True;
      p=composite_image->pixels;
      for (i=0; i < composite_image->packets; i++)
      {
        p->index=index;
        p++;
      }
    }
  if (compose == DisplaceCompositeOp)
    composite_image->geometry=displacement_geometry;
  /*
    Combine image.
  */
  if (stereo)
    combined_image=StereoImage(image,composite_image);
  else
    if (tile)
      {
        /*
          Tile the composite image.
        */
        for (y=0; y < image->rows; y+=composite_image->rows)
          for (x=0; x < image->columns; x+=composite_image->columns)
            CompositeImage(image,compose,composite_image,x,y);
        combined_image=image;
      }
    else
      {
        unsigned int
          size;

        /*
          Digitally composite image.
        */
        x=0;
        y=0;
        if (geometry != (char *) NULL)
          (void) XParseGeometry(geometry,&x,&y,&size,&size);
        switch (gravity)
        {
          case NorthWestGravity:
            break;
          case NorthGravity:
          {
            x+=(image->columns-composite_image->columns) >> 1;
            break;
          }
          case NorthEastGravity:
          {
            x+=image->columns-composite_image->columns;
            break;
          }
          case WestGravity:
          {
            y+=(image->rows-composite_image->rows) >> 1;
            break;
          }
          case ForgetGravity:
          {
            char
              geometry[MaxTextExtent];

            /*
              Stretch composite to the same size as the image.
            */
            (void) sprintf(geometry,"%ux%u",image->columns,image->rows);
            TransformImage(&composite_image,(char *) NULL,geometry);
            break;
          }
          case StaticGravity:
          case CenterGravity:
          default:
          {
            x+=(image->columns-composite_image->columns) >> 1;
            y+=(image->rows-composite_image->rows) >> 1;
            break;
          }
          case EastGravity:
          {
            x+=image->columns-composite_image->columns;
            y+=(image->rows-composite_image->rows) >> 1;
            break;
          }
          case SouthWestGravity:
          {
            y+=image->rows-composite_image->rows;
            break;
          }
          case SouthGravity:
          {
            x+=(image->columns-composite_image->columns) >> 1;
            y+=image->rows-composite_image->rows;
            break;
          }
          case SouthEastGravity:
          {
            x+=image->columns-composite_image->columns;
            y+=image->rows-composite_image->rows;
            break;
          }
        }
        CompositeImage(image,compose,composite_image,x,y);
        combined_image=image;
      }
  if (combined_image == (Image *) NULL)
    exit(1);
  /*
    Write image.
  */
  (void) strcpy(combined_image->filename,write_filename);
  /*
    Transmogrify image as defined by the image processing options.
  */
  MogrifyImage(&image_info,argc,argv,&combined_image);
  (void) WriteImage(&image_info,combined_image);
  if (image_info.verbose)
    DescribeImage(combined_image,stderr,False);
  exit(0);
  return(False);
}
