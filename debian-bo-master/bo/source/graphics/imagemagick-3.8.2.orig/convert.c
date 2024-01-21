/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                CCCC   OOO   N   N  V   V  EEEEE  RRRR   TTTTT               %
%               C      O   O  NN  N  V   V  E      R   R    T                 %
%               C      O   O  N N N  V   V  EEE    RRRR     T                 %
%               C      O   O  N  NN   V V   E      R R      T                 %
%                CCCC   OOO   N   N    V    EEEEE  R  R     T                 %
%                                                                             %
%                                                                             %
%                Convert an image from one format to another.                 %
%                                                                             %
%                                                                             %
%                                                                             %
%                              Software Design                                %
%                                John Cristy                                  %
%                                April 1992                                   %
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
%  Convert converts an input file using one image format to an output file
%  with a differing image format.
%
%  The convert program syntax is:
%
%  Usage: convert [options ...] input_file output_file
%
%  Where options include:
%    -adjoin              join images into a single multi-image file
%    -average             average a set of images
%    -blur factor         apply a filter to blur the image
%    -border geometry     surround image with a border of color
%    -box color           color for annotation bounding box
%    -charcoal factor     simulate a charcoal drawing
%    -colorize value      colorize the image with the pen color
%    -colors value        preferred number of colors in the image
%    -colorspace type     GRAY, OHTA, RGB, XYZ, YCbCr, YIQ, YPbPr, or YUV
%    -comment string      annotate image with comment
%    -compress type       RunlengthEncoded or Zip
%    -contrast            enhance or reduce the image contrast
%    -crop geometry       preferred size and location of the cropped image
%    -cycle amount        cycle the image colormap
%    -delay value         display the next image after pausing
%    -density geometry    vertical and horizontal density of the image
%    -despeckle           reduce the speckles within an image"
%    -display server      obtain image or font from this X server
%    -dispose method      GIF disposal method
%    -dither              apply Floyd/Steinberg error diffusion to image
%    -draw string         annotate the image with a graphic primitive
%    -edge factor         apply a filter to detect edges in the image
%    -emboss              emboss an image
%    -enhance             apply a digital filter to enhance a noisy image
%    -equalize            perform histogram equalization to an image
%    -flip                flip image in the vertical direction
%    -flop                flop image in the horizontal direction
%    -font name           X11 font for displaying text
%    -frame geometry      surround image with an ornamental border
%    -gamma value         level of gamma correction
%    -geometry geometry   perferred size or location of the image
%    -implode amount      implode image pixels about the center
%    -interlace type      None, Line, Plane, or Partition
%    -label name          assign a label to an image
%    -linewidth value     width of line in pixels
%    -loop iterations     add Netscape loop extension to your GIF animation
%    -map filename        transform image colors to match this set of colors
%    -matte               store matte channel if the image has one
%    -modulate value      vary the brightness, saturation and hue
%    -monochrome          transform image to black and white
%    -negate              apply color inversion to image
%    -noise               add or reduce noise in an image
%    -normalize           transform image to span the full range of colors
%    -opaque color        change this color to the pen color
%    -page geometry       size and location of the Postscript page
%    -paint radius        simulate an oil painting
%    -pen color           color for annotating or changing opaque color
%    -pointsize value     pointsize of Postscript font
%    -quality value       JPEG quality setting
%    -raise value         lighten/darken image edges to create a 3-D effect
%    -region geometry     apply options to a portion of the image
%    -roll geometry       roll an image vertically or horizontally
%    -rotate degrees      apply Paeth rotation to the image
%    -sample geometry     scale image with pixel sampling
%    -scene value         image scene number
%    -segment values      segment an image
%    -shade degrees       shade the image using a distant light source
%    -sharpen factor      apply a filter to sharpen the image
%    -shear geometry      slide one edge of the image along the X or Y axis
%    -size geometry       width and height of image
%    -solarize thresold   negate all pixels above the threshold level
%    -spread amount       displace image pixels by a random amount
%    -swirl degrees       swirl image pixels about the center
%    -texture filename    name of texture to tile onto the image background
%    -threshold value     threshold the image
%    -transparent color   make this color transparent within the image
%    -treedepth value     depth of the color classification tree
%    -undercolor geometry control undercolor removal and black generation
%    -verbose             print detailed information about the image
%
%  Change '-' to '+' in any option above to reverse its effect.  For
%  example,  specify +matte to store the image without its matte channel.
%
%  By default, the image format of `file' is determined by its magic
%  number.  To specify a particular image format, precede the filename
%  with an image format name and a colon (i.e. ps:image) or specify the
%  image type as the filename suffix (i.e. image.ps).  Specify 'file' as
%  '-' for standard input or output.
%
%  Convert recognizes the following image formats:
%
%    Tag   Description
%    ---------------------------------------------------
%    AVS   AVS X image file.
%    BMP   Microsoft Windows bitmap image file.
%    CMYK  Raw cyan, magenta, yellow, and black bytes.
%    DCX   ZSoft IBM PC multi-page Paintbrush file
%    DIB   Microsoft Windows bitmap image file.
%    EPS   Adobe Encapsulated PostScript file.
%    EPS2  Adobe Level II Encapsulated PostScript file.
%    EPSF  Adobe Encapsulated PostScript file.
%    EPSI  Adobe Encapsulated PostScript Interchange format.
%    FAX   Group 3.
%    FITS  Flexible Image Transport System.
%    GIF   CompuServe graphics interchange format; 8-bit color.
%    GIF87 CompuServe graphics interchange format; 8-bit color (version 87a).
%    GRADATION gradual passing from one shade to another.
%    GRANITE granite texture
%    GRAY  Raw gray bytes.
%    HDF   Hierarchical Data Format.
%    HISTOGRAM
%    HTML  Hypertext Markup Language with a client-side image map
%    JBIG  Joint Bi-level Image experts Group file interchange format.
%    JPEG  Joint Photographic Experts Group JFIF format; compressed 24-bit color.
%    MAP   colormap intensities and indices.
%    MATTE Raw matte bytes.
%    MIFF  Magick image file format.
%    MONO  Bi-level bitmap in least-significant-byte (LSB) first order.
%    MPEG  Motion Picture Experts Group file interchange format.
%    MTV   MTV Raytracing image format
%    NETSCAPE
%          Netscape 216 color cube.
%    NULL  NULL image.
%    PBM   Portable bitmap format (black and white).
%    PCD   Photo CD.
%    PCX   ZSoft IBM PC Paintbrush file.
%    PDF   Portable Document Format.
%    PGM   Portable graymap format (gray scale).
%    PICT  Apple Macintosh QuickDraw/PICT file.
%    PLASMA plasma fractal image.
%    PNG   Portable Network Graphics.
%    PNM   Portable anymap.
%    PPM   Portable pixmap format (color).
%    PREVIEW
%    PS    Adobe PostScript file.
%    PS2   Adobe Level II PostScript file.
%    RAD   Radiance image file.
%    RGB   Raw red, green, and blue bytes.
%    RGBA  Raw red, green, blue, and matte bytes.
%    RLA   Alias/Wavefront image file; read only
%    RLE   Utah Run length encoded image file; read only.
%    SGI   Irix RGB image file.
%    SHTML Hypertext Markup Language with a client-side image map
%    SUN   SUN Rasterfile.
%    TEXT  raw text file; read only.
%    TGA   Truevision Targa image file.
%    TIFF  Tagged Image File Format.
%    UYVY  16bit/pixel interleaved YUV (e.g. used by AccomWSD)
%    TILE  tile image with a texture.
%    VICAR read only.
%    VID   Visual Image Directory.
%    VIFF  Khoros Visualization image file.
%    X     select image from X server screen.
%    XC    constant image of X server color.
%    XBM   X Windows system bitmap, black and white only.
%    XPM   X Windows system pixmap file (color).
%    XWD   X Windows system window dump file (color).
%    YUV   CCIR 601 1:1:1 file.
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
    *ImageTypes[]=
    {
      "Tag   Description",
      "------------------------------------------------------------",
      "AVS   AVS X image file.",
      "BMP   Microsoft Windows bitmap image file.",
      "CMYK  Raw cyan, magenta, yellow, and black bytes.",
      "DCX   ZSoft IBM PC multi-page Paintbrush file",
      "DIB   Microsoft Windows bitmap image file.",
      "EPS   Adobe Encapsulated PostScript file.",
      "EPS2  Adobe Level II Encapsulated PostScript file.",
      "EPSF  Adobe Encapsulated PostScript file.",
      "EPSI  Adobe Encapsulated PostScript Interchange format.",
      "FAX   Group 3.",
      "FITS  Flexible Image Transport System.",
      "GIF   CompuServe graphics interchange format; 8-bit color.",
      "GIF87 CompuServe graphics interchange format; 8-bit color (version 87a).",
      "GRADATION gradual passing from one shade to another.",
      "GRANITE granite texture.",
      "GRAY  Raw gray bytes.",
      "HDF   Hierarchical Data Format.",
      "HISTOGRAM",
      "HTML  Hypertext Markup Language with a client-side image map",
      "JBIG  Joint Bi-level Image experts Group file interchange format.",
      "JPEG  Joint Photographic Experts Group JFIF format; compressed 24-bit color.",
      "MAP   colormap intensities and indices.",
      "MATTE Raw matte bytes.",
      "MIFF  Magick image file format.",
      "MONO  Bi-level bitmap in least-significant-byte (LSB) first order.",
      "MPEG  Motion Picture Experts Group file interchange format.",
      "MTV   MTV Raytracing image format.",
      "NETSCAPE  Netscape 216 color cube.",
      "NULL  NULL image.",
      "PBM   Portable bitmap format (black and white).",
      "PCD   Photo CD.",
      "PCX   ZSoft IBM PC Paintbrush file.",
      "PDF   Portable Document Format.",
      "PGM   Portable graymap format (gray scale).",
      "PICT  Apple Macintosh QuickDraw/PICT file.",
      "PLASMA plasma fractal image.",
      "PNG   Portable Network Graphics.",
      "PNM   Portable anymap.",
      "PPM   Portable pixmap format (color).",
      "PREVIEW",
      "PS    Adobe PostScript file.",
      "PS2   Adobe Level II PostScript file.",
      "RAD   Radiance image file.",
      "RGB   Raw red, green, and blue bytes.",
      "RGBA  Raw red, green, blue, and matte bytes.",
      "RLA   Alias/Wavefront image file; read only.",
      "RLE   Utah Run length encoded image file; read only.",
      "SGI   Irix RGB image file.",
      "SUN   SUN Rasterfile.",
      "TEXT  raw text file; read only.",
      "TGA   Truevision Targa image file.",
      "TIFF  Tagged Image File Format.",
      "UYVY  16bit/pixel interleaved YUV.",
      "TILE  tile image with a texture.",
      "VICAR read only.",
      "VID   Visual Image Directory.",
      "VIFF  Khoros Visualization image file.",
      "X     select image from X server screen.",
      "XC    constant image of X server color.",
      "XBM   X Windows system bitmap, black and white only.",
      "XPM   X Windows system pixmap file (color).",
      "XWD   X Windows system window dump file (color).",
      "YUV   CCIR 601 1:1:1 file.",
      (char *) NULL,
    },
    *options[]=
    {
      "-adjoin              join images into a single multi-image file",
      "-average             average a set of images",
      "-blur factor         apply a filter to blur the image",
      "-border geometry     surround image with a border of color",
      "-box color           color for annotation bounding box",
      "-charcoal factor     simulate a charcoal drawing",
      "-colorize value      colorize the image with the pen color",
      "-colors value        preferred number of colors in the image",
      "-colorspace type     GRAY, OHTA, RGB, XYZ, YCbCr, YIQ, YPbPr, or YUV",
      "-comment string      annotate image with comment",
      "-compress type       RunlengthEncoded or Zip",
      "-contrast            enhance or reduce the image contrast",
      "-crop geometry       preferred size and location of the cropped image",
      "-cycle amount        cycle the image colormap",
      "-delay value         display the next image after pausing",
      "-density geometry    vertical and horizontal density of the image",
      "-despeckle           reduce the speckles within an image",
      "-display server      obtain image or font from this X server",
      "-dispose method      GIF disposal method",
      "-dither              apply Floyd/Steinberg error diffusion to image",
      "-draw string         annotate the image with a graphic primitive",
      "-edge factor         apply a filter to detect edges in the image",
      "-emboss              emboss an image",
      "-enhance             apply a digital filter to enhance a noisy image",
      "-equalize            perform histogram equalization to an image",
      "-flip                flip image in the vertical direction",
      "-flop                flop image in the horizontal direction",
      "-font name           X11 font for displaying text",
      "-frame geometry      surround image with an ornamental border",
      "-gamma value         level of gamma correction",
      "-geometry geometry   perferred size or location of the image",
      "-implode amount      implode image pixels about the center",
      "-interlace type      None, Line, Plane, or Partition",
      "-linewidth value     width of line in pixels",
      "-label name          assign a label to an image",
      "-loop iterations     add Netscape loop extension to your GIF animation",
      "-map filename        transform image colors to match this set of colors",
      "-matte               store matte channel if the image has one",
      "-modulate value      vary the brightness, saturation, and hue",
      "-monochrome          transform image to black and white",
      "-negate              apply color inversion to image",
      "-noise               add or reduce noise in an image",
      "-normalize           transform image to span the full range of colors",
      "-opaque color        change this color to the pen color",
      "-page geometry       size and location of the Postscript page",
      "-paint radius        simulate an oil painting",
      "-pen color           color for annotating or changing opaque color",
      "-pointsize value     pointsize of Postscript font",
      "-quality value       JPEG quality setting",
      "-raise value         lighten/darken image edges to create a 3-D effect",
      "-region geometry     apply options to a portion of the image",
      "-roll geometry       roll an image vertically or horizontally",
      "-rotate degrees      apply Paeth rotation to the image",
      "-sample geometry     scale image with pixel sampling",
      "-scene value         image scene number",
      "-segment values      segment an image",
      "-shade degrees       shade the image using a distant light source",
      "-sharpen factor      apply a filter to sharpen the image",
      "-shear geometry      slide one edge of the image along the X or Y axis",
      "-size geometry       width and height of image",
      "-solarize thresold   negate all pixels above the threshold level",
      "-spread amount       displace image pixels by a random amount",
      "-swirl degrees       swirl image pixels about the center",
      "-texture filename    name of texture to tile onto the image background",
      "-threshold value     threshold the image",
      "-transparent color   make this color transparent within the image",
      "-treedepth value     depth of the color classification tree",
      "-undercolor geometry control undercolor removal and black generation",
      "-verbose             print detailed information about the image",
      (char *) NULL
    };

  (void) printf("Version: %s\n\n",Version);
  (void) printf("Usage: %s [options ...] input_file output_file\n",
    client_name);
  (void) printf("\nWhere options include:\n");
  for (p=options; *p != (char *) NULL; p++)
    (void) printf("  %s\n",*p);
  (void) printf(
    "\nChange '-' to '+' in any option above to reverse its effect.  For\n");
  (void) printf(
    "example,  specify +matte to store the image without an matte channel.\n");
  (void) printf(
    "\nBy default, the image format of `file' is determined by its magic\n");
  (void) printf(
    "number.  To specify a particular image format, precede the filename\n");
  (void) printf(
    "with an image format name and a colon (i.e. ps:image) or specify the\n");
  (void) printf(
    "image type as the filename suffix (i.e. image.ps).  Specify 'file' as\n");
  (void) printf("'-' for standard input or output.\n");
  (void) printf("\nThe following image formats are recognized: \n\n");
  for (p=ImageTypes; *p != (char *) NULL; p++)
    (void) printf("  %s\n",*p);
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
    *filename,
    *option;

  Image
    *image,
    *next_image;

  ImageInfo
    image_info;

  int
    x;

  register int
    i;

  unsigned int
    average,
    global_colormap,
    scene;

  /*
    Initialize program variables.
  */
  ReadCommandlLine(argc,&argv);
  client_name=ClientName(*argv);
  if (argc < 3)
    Usage(client_name);
  /*
    Read image and convert to MIFF format.
  */
  average=False;
  filename=(char *) NULL;
  image=(Image *) NULL;
  global_colormap=False;
  GetImageInfo(&image_info);
  option=(char *) NULL;
  scene=0;
  /*
    Parse command-line arguments.
  */
  ExpandFilenames(&argc,&argv);
  for (i=1; i < (argc-1); i++)
  {
    option=argv[i];
    if ((Extent(option) < 2) || ((*option != '-') && (*option != '+')))
      {
        /*
          Read input image.
        */
        filename=argv[i];
        (void) strcpy(image_info.filename,filename);
        next_image=ReadImage(&image_info);
        if (next_image == (Image *) NULL)
          continue;
        MogrifyImages(&image_info,i,argv,&next_image);
        if (image == (Image *) NULL)
          image=next_image;
        else
          {
            /*
              Link image into image list.
            */
            next_image->previous=image;
            image->next=next_image;
            while (image->next != (Image *) NULL)
              image=image->next;
          }
      }
    else
      switch(*(option+1))
      {
        case 'a':
        {
          if (strncmp("adjoin",option+1,2) == 0)
            {
              image_info.adjoin=(*option == '-');
              break;
            }
          if (strncmp("average",option+1,2) == 0)
            {
              average=(*option == '-');
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'b':
        {
          if (strncmp("blur",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%f",(float *) &x))
                    Error("Missing factor on -blur",(char *) NULL);
                }
              break;
            }
          if (strncmp("border",option+1,7) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    Error("Missing geometry on -border",(char *) NULL);
                }
              break;
            }
          if (strncmp("bordercolor",option+1,7) == 0)
            {
              if ((i == argc) || !sscanf(argv[i],"%d",&x))
                {
                  i++;
                  if (i == argc)
                    Error("Missing color on -bordercolor",(char *) NULL);
                }
              break;
            }
          if (strncmp("box",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing color on -box",(char *) NULL);
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'c':
        {
          if (strncmp("charcoal",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%f",(float *) &x))
                    Error("Missing factor on -charcoal",(char *) NULL);
                }
              break;
            }
          if (strncmp("colorize",option+1,7) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing value on -colorsize",(char *) NULL);
                }
              break;
            }
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
              if (*option == '-')
                {
                  unsigned int
                    colorspace;

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
          if (strncmp("compress",option+1,4) == 0)
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
          if (strncmp("contrast",option+1,3) == 0)
            break;
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
          if (strncmp("cycle",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing amount on -cycle",(char *) NULL);
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
              image_info.delay=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing delay on -delay",(char *) NULL);
                  image_info.delay=PostscriptGeometry(argv[i]);
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
          if (strncmp("despeckle",option+1,3) == 0)
            break;
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
          if (strncmp("draw",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing primitive on -draw",(char *) NULL);
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'e':
        {
          if (strncmp("edge",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%f",(float *) &x))
                    Error("Missing factor on -edge",(char *) NULL);
                }
              break;
            }
          if (strncmp("emboss",option+1,2) == 0)
            break;
          if (strncmp("enhance",option+1,2) == 0)
            break;
          if (strncmp("equalize",option+1,2) == 0)
            break;
          Error("Unrecognized option",option);
          break;
        }
        case 'f':
        {
          if (strncmp("flip",option+1,3) == 0)
            break;
          if (strncmp("flop",option+1,3) == 0)
            break;
          if (strncmp("font",option+1,2) == 0)
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
          if (strncmp("frame",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    Error("Missing geometry on -frame",(char *) NULL);
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'g':
        {
          if (strncmp("gamma",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%f",(float *) &x))
                    Error("Missing value on -gamma",(char *) NULL);
                }
              break;
            }
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
          if (strncmp("implode",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%f",(float *) &x))
                    Error("Missing amount on -implode",(char *) NULL);
                }
              break;
            }
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
          if (strncmp("linewidth",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing size on -linewidth",(char *) NULL);
                }
              break;
            }
          if (strncmp("loop",option+1,2) == 0)
            {
              image_info.iterations=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing iterations on -loop",(char *) NULL);
                  image_info.iterations=PostscriptGeometry(argv[i]);
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'm':
        {
          if (strncmp("map",option+1,3) == 0)
            {
              global_colormap=(*option == '+');
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing file name on -map",(char *) NULL);
                }
              break;
            }
          if (strcmp("matte",option+1) == 0)
            break;
          if (strncmp("mattecolor",option+1,6) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing color on -mattecolor",(char *) NULL);
                }
              break;
            }
          if (strncmp("modulate",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%f",(float *) &x))
                    Error("Missing value on -modulate",(char *) NULL);
                }
              break;
            }
          if (strncmp("monochrome",option+1,4) == 0)
            {
              image_info.monochrome=(*option == '-');
              break;
            }
          Error("Unrecognized option",option);
        }
        case 'n':
        {
          if (strncmp("negate",option+1,3) == 0)
            break;
          if (strncmp("noise",option+1,3) == 0)
            {
              if (*option == '+')
                {
                  i++;
                  if (i == argc)
                    Error("Missing type on +noise",(char *) NULL);
                  option=argv[i];
                  if ((Latin1Compare("uniform",option) != 0) &&
                      (Latin1Compare("gaussian",option) != 0) &&
                      (Latin1Compare("multiplicative",option) != 0) &&
                      (Latin1Compare("impulse",option) != 0) &&
                      (Latin1Compare("laplacian",option) != 0) &&
                      (Latin1Compare("poisson",option) != 0))
                    Error("Invalid noise type on +noise",option);
                }
              break;
            }
          if (strncmp("normalize",option+1,3) == 0)
            break;
          Error("Unrecognized option",option);
          break;
        }
        case 'o':
        {
          if (strncmp("opaque",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing color on -opaque",(char *) NULL);
                }
              break;
            }
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
          if (strncmp("paint",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing radius on -paint",(char *) NULL);
                }
              break;
            }
          if (strncmp("pen",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing color on -pen",(char *) NULL);
                }
              break;
            }
          if (strncmp("pointsize",option+1,2) == 0)
            {
              image_info.pointsize=atoi(DefaultPointSize);
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing size on -pointsize",(char *) NULL);
                  image_info.pointsize=atoi(argv[i]);
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
        case 'r':
        {
          if (strncmp("raise",option+1,2) == 0)
            {
              i++;
              if ((i == argc) || !sscanf(argv[i],"%d",&x))
                Error("Missing bevel width on -raise",(char *) NULL);
              break;
            }
          if (strncmp("region",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    Error("Missing geometry on -region",(char *) NULL);
                }
              break;
            }
          if (strncmp("roll",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    Error("Missing geometry on -roll",(char *) NULL);
                }
              break;
            }
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
          if (strncmp("sample",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    Error("Missing geometry on -sample",(char *) NULL);
                }
              break;
            }
          if (strncmp("scene",option+1,3) == 0)
            {
              scene=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing scene number on -scene",(char *) NULL);
                }
              scene=atoi(argv[i]);
              break;
            }
          if (strncmp("segment",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%f",(float *) &x))
                    Error("Missing threshold on -segment",(char *) NULL);
                }
              break;
            }
          if (strncmp("shade",option+1,5) == 0)
            {
              i++;
              if ((i == argc) || !sscanf(argv[i],"%d",&x))
                Error("Missing azimuth on -shade",(char *) NULL);
              break;
            }
          if (strncmp("sharpen",option+1,5) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%f",(float *) &x))
                    Error("Missing factor on -sharpen",(char *) NULL);
                }
              break;
            }
          if (strncmp("shear",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%f",(float *) &x))
                    Error("Missing shear geometry on -shear",(char *) NULL);
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
          if (strncmp("solarize",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing threshold on -solarize",(char *) NULL);
                }
              break;
            }
          if (strncmp("spread",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing amount on -spread",(char *) NULL);
                }
              break;
            }
          if (strncmp("swirl",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%f",(float *) &x))
                    Error("Missing degrees on -swirl",(char *) NULL);
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 't':
        {
          if (strncmp("texture",option+1,5) == 0)
            {
              image_info.texture=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    Error("Missing filename on -texture",(char *) NULL);
                  image_info.texture=argv[i];
                }
              break;
            }
          if (strncmp("threshold",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    Error("Missing value on -threshold",(char *) NULL);
                }
              break;
            }
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
        case 'u':
        {
          if (strncmp("undercolor",option+1,2) == 0)
            {
              image_info.undercolor=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%f",(float *) &x))
                    Error("Missing undercolor geometry on -undercolor",
                      (char *) NULL);
                  image_info.undercolor=argv[i];
                }
              break;
            }
          Error("Unrecognized option",option);
          break;
        }
        case 'v':
        {
          if (strncmp("verbose",option+1,1) == 0)
            {
              image_info.verbose=(*option == '-');
              break;
            }
          Error("Unrecognized option",option);
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
  if (image == (Image *) NULL)
    Error("Missing an image file name",(char *) NULL);
  /*
    Write images.
  */
  if ((Extent(option) > 2) && ((*option == '-') || (*option == '+')))
    MogrifyImages(&image_info,i,argv,&image);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  if (average)
    {
      Image
        *averaged_image;

      /*
        Average a set of images.
      */
      averaged_image=AverageImages(image);
      if (averaged_image != (Image *) NULL)
        {
          DestroyImages(image);
          image=averaged_image;
        }
    }
  if (global_colormap)
    MapImages(image,(Image *) NULL,image_info.dither);
  (void) strcpy(image_info.filename,argv[i]);
  SetImageInfo(&image_info,True);
  do
  {
    /*
      Transmogrify image as defined by the image processing options.
    */
    (void) strcpy(image->filename,argv[i]);
    image->scene=scene++;
    if (!image_info.adjoin)
      (void) WriteImage(&image_info,image);
    next_image=image->next;
    if (next_image != (Image *) NULL)
      image=next_image;
  } while (next_image != (Image *) NULL);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  if (image_info.adjoin)
    (void) WriteImage(&image_info,image);
  if (image_info.verbose)
    DescribeImage(image,stderr,False);
  exit(0);
  return(False);
}
