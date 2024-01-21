/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                   DDDD   EEEEE   CCCC   OOO   DDDD   EEEEE                  %
%                   D   D  E      C      O   O  D   D  E                      %
%                   D   D  EEE    C      O   O  D   D  EEE                    %
%                   D   D  E      C      O   O  D   D  E                      %
%                   DDDD   EEEEE   CCCC   OOO   DDDD   EEEEE                  %
%                                                                             %
%                                                                             %
%                    Utility Routines to Read Image Formats                   %
%                                                                             %
%                                                                             %
%                             Software Design                                 %
%                               John Cristy                                   %
%                              January 1992                                   %
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
%  Functions in this library convert to and from `alien' image formats to the
%  MIFF image format.
%
%
*/

/*
  Include declarations.
*/
#include "magick.h"
#include "XWDFile.h"
#include "plug-ins.h"

/*
  Define declarations.
*/
#define LoadImageText  "  Loading image...  "
#define PrematureExit(message,image) \
{ \
  Warning(message,image->filename); \
  DestroyImages(image); \
  return((Image *) NULL); \
}

/*
  Function prototypes.
*/
static Image
  *ReadMIFFImage(const ImageInfo *),
  *ReadPNMImage(const ImageInfo *),
  *ReadPSImage(const ImageInfo *),
  *ReadXCImage(const ImageInfo *);

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d A V S I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadAVSImage reads a AVS X image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadAVSImage routine is:
%
%      image=ReadAVSImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadAVSImage returns a pointer to the image after
%      reading. A null image is returned if there is a a memory shortage or if
%      the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadAVSImage(const ImageInfo *image_info)
{
  typedef struct _AVSHeader
  {
    int
      width,
      height;
  } AVSHeader;

  AVSHeader
    avs_header;

  Image
    *image;

  Quantum
    blue,
    green,
    red;

  register int
    x,
    y;

  register RunlengthPacket
    *q;

  unsigned int
    packets,
    status;

  unsigned short
    index;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Read AVS image.
  */
  status=ReadData((char *) &avs_header,1,(unsigned int) sizeof(AVSHeader),
    image->file);
  if (status == False)
    PrematureExit("Not a AVS image file",image);
  do
  {
    /*
      Initialize image structure.
    */
    image->columns=avs_header.width;
    image->rows=avs_header.height;
    image->packets=0;
    packets=Max((image->columns*image->rows+4) >> 3,1);
    image->pixels=(RunlengthPacket *) malloc(packets*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      PrematureExit("Unable to allocate memory",image);
    /*
      Convert AVS raster image to runlength-encoded packets.
    */
    q=image->pixels;
    q->length=MaxRunlength;
    for (y=0; y < image->rows; y++)
    {
      for (x=0; x < image->columns; x++)
      {
        index=UpScale(fgetc(image->file));
        image->matte|=index != Transparent;
        red=UpScale(fgetc(image->file));
        green=UpScale(fgetc(image->file));
        blue=UpScale(fgetc(image->file));
        if ((red == q->red) && (green == q->green) && (blue == q->blue) &&
            (index == q->index) && ((int) q->length < MaxRunlength))
          q->length++;
        else
          {
            if (image->packets != 0)
              q++;
            image->packets++;
            if (image->packets == packets)
              {
                packets<<=1;
                image->pixels=(RunlengthPacket *) realloc((char *)
                  image->pixels,packets*sizeof(RunlengthPacket));
                if (image->pixels == (RunlengthPacket *) NULL)
                  PrematureExit("Unable to allocate memory",image);
                q=image->pixels+image->packets-1;
              }
            q->red=red;
            q->green=green;
            q->blue=blue;
            q->index=index;
            q->length=0;
          }
      }
      ProgressMonitor(LoadImageText,y,image->rows);
    }
    image->pixels=(RunlengthPacket *)
      realloc((char *) image->pixels,image->packets*sizeof(RunlengthPacket));
    status=ReadData((char *) &avs_header,1,(unsigned int) sizeof(AVSHeader),
      image->file);
    if (status == True)
      {
        /*
          Allocate image structure.
        */
        image->next=AllocateImage(image_info);
        if (image->next == (Image *) NULL)
          {
            DestroyImages(image);
            return((Image *) NULL);
          }
        (void) strcpy(image->next->filename,image_info->filename);
        image->next->file=image->file;
        image->next->scene=image->scene+1;
        image->next->previous=image;
        image=image->next;
      }
  } while (status == True);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  CloseImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d B M P I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadBMPImage reads a Microsoft Windows bitmap image file and
%  returns it.  It allocates the memory necessary for the new Image structure
%  and returns a pointer to the new image.
%
%  The format of the ReadBMPImage routine is:
%
%      image=ReadBMPImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadBMPImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadBMPImage(const ImageInfo *image_info)
{
  typedef struct _BMPHeader
  {
    unsigned long
      file_size;

    unsigned short
      reserved[2];

    unsigned long
      offset_bits,
      size,
      width,
      height;

    unsigned short
      planes,
      bit_count;

    unsigned long
      compression,
      image_size,
      x_pixels,
      y_pixels,
      number_colors,
      colors_important;
  } BMPHeader;

  BMPHeader
    bmp_header;

  Image
    *image;

  long
    start_position;

  register int
    bit,
    i,
    x,
    y;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  unsigned char
    *bmp_data,
    *bmp_pixels,
    magick[12];

  unsigned int
    bytes_per_line,
    image_size,
    status;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Determine if this is a BMP file.
  */
  status=ReadData((char *) magick,1,2,image->file);
  do
  {
    /*
      Verify BMP identifier.
    */
    start_position=ftell(image->file)-2;
    if ((status == False) || (strncmp((char *) magick,"BM",2) != 0))
      PrematureExit("Not a BMP image file",image);
    bmp_header.file_size=LSBFirstReadLong(image->file);
    bmp_header.reserved[0]=LSBFirstReadShort(image->file);
    bmp_header.reserved[1]=LSBFirstReadShort(image->file);
    bmp_header.offset_bits=LSBFirstReadLong(image->file);
    bmp_header.size=LSBFirstReadLong(image->file);
    if (bmp_header.size == 12)
      {
        /*
          OS/2 BMP image file.
        */
        bmp_header.width=(unsigned long) LSBFirstReadShort(image->file);
        bmp_header.height=(unsigned long) LSBFirstReadShort(image->file);
        bmp_header.planes=LSBFirstReadShort(image->file);
        bmp_header.bit_count=LSBFirstReadShort(image->file);
        bmp_header.x_pixels=0;
        bmp_header.y_pixels=0;
        bmp_header.number_colors=0;
        bmp_header.compression=0;
        bmp_header.image_size=0;
      }
    else
      {
        /*
          Microsoft Windows BMP image file.
        */
        bmp_header.width=LSBFirstReadLong(image->file);
        bmp_header.height=LSBFirstReadLong(image->file);
        bmp_header.planes=LSBFirstReadShort(image->file);
        bmp_header.bit_count=LSBFirstReadShort(image->file);
        bmp_header.compression=LSBFirstReadLong(image->file);
        bmp_header.image_size=LSBFirstReadLong(image->file);
        bmp_header.x_pixels=LSBFirstReadLong(image->file);
        bmp_header.y_pixels=LSBFirstReadLong(image->file);
        bmp_header.number_colors=LSBFirstReadLong(image->file);
        bmp_header.colors_important=LSBFirstReadLong(image->file);
        for (i=0; i < ((int) bmp_header.size-40); i++)
          (void) fgetc(image->file);
      }
    if (bmp_header.bit_count < 24)
      {
        unsigned char
          *bmp_colormap;

        unsigned int
          packet_size;

        /*
          Read BMP raster colormap.
        */
        image->class=PseudoClass;
        image->colors=(unsigned int) bmp_header.number_colors;
        if (image->colors == 0)
          image->colors=1 << bmp_header.bit_count;
        image->colormap=(ColorPacket *)
          malloc(image->colors*sizeof(ColorPacket));
        bmp_colormap=(unsigned char *)
          malloc(4*image->colors*sizeof(unsigned char));
        if ((image->colormap == (ColorPacket *) NULL) ||
            (bmp_colormap == (unsigned char *) NULL))
          PrematureExit("Unable to allocate memory",image);
        packet_size=4;
        if (bmp_header.size == 12)
          packet_size=3;
        (void) ReadData((char *) bmp_colormap,packet_size,image->colors,
          image->file);
        p=bmp_colormap;
        for (i=0; i < image->colors; i++)
        {
          image->colormap[i].blue=UpScale(*p++);
          image->colormap[i].green=UpScale(*p++);
          image->colormap[i].red=UpScale(*p++);
          if (bmp_header.size != 12)
            p++;
        }
        free((char *) bmp_colormap);
      }
    /*
      Read image data.
    */
    while (ftell(image->file) < (start_position+bmp_header.offset_bits))
      (void) fgetc(image->file);
    image_size=
      ((bmp_header.width*bmp_header.bit_count+31)/32)*4*bmp_header.height;
    if ((bmp_header.image_size == 0) || (bmp_header.image_size > image_size))
      bmp_header.image_size=image_size;
    bmp_data=(unsigned char *)
      malloc(bmp_header.image_size*sizeof(unsigned char));
    if (bmp_data == (unsigned char *) NULL)
      PrematureExit("Unable to allocate memory",image);
    (void) ReadData((char *) bmp_data,1,(unsigned int) bmp_header.image_size,
      image->file);
    bmp_pixels=bmp_data;
    if (bmp_header.compression != 0)
      {
        unsigned int
          packets;

        /*
          Convert run-length encoded raster pixels.
        */
        packets=(unsigned int)
          (((bmp_header.width*bmp_header.bit_count+31)/32)*4*bmp_header.height);
        if (bmp_header.compression == 2)
          packets<<=1;
        bmp_pixels=(unsigned char *) malloc(packets*sizeof(unsigned char));
        if (bmp_pixels == (unsigned char *) NULL)
          PrematureExit("Unable to allocate memory",image);
        (void) BMPDecodeImage(bmp_data,bmp_pixels,
          (unsigned int) bmp_header.compression,(unsigned int) bmp_header.width,
          (unsigned int) bmp_header.height);
        if (bmp_header.compression == 2)
          bmp_header.bit_count<<=1;
        free((char *) bmp_data);
      }
    /*
      Initialize image structure.
    */
    image->columns=(unsigned int) bmp_header.width;
    image->rows=(unsigned int) bmp_header.height;
    image->units=PixelsPerCentimeterResolution;
    image->x_resolution=bmp_header.x_pixels/100.0;
    image->y_resolution=bmp_header.y_pixels/100.0;
    image->packets=image->columns*image->rows;
    image->pixels=(RunlengthPacket *)
      malloc(image->packets*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      PrematureExit("Unable to allocate memory",image);
    /*
      Convert BMP raster image to runlength-encoded packets.
    */
    bytes_per_line=((image->columns*bmp_header.bit_count+31)/32)*4;
    switch (bmp_header.bit_count)
    {
      case 1:
      {
        /*
          Convert bitmap scanline to runlength-encoded color packets.
        */
        for (y=image->rows-1; y >= 0; y--)
        {
          p=bmp_pixels+(image->rows-y-1)*bytes_per_line;
          q=image->pixels+(y*image->columns);
          for (x=0; x < ((int) image->columns-7); x+=8)
          {
            for (bit=0; bit < 8; bit++)
            {
              q->index=((*p) & (0x80 >> bit) ? 0x01 : 0x00);
              q->length=0;
              q++;
            }
            p++;
          }
          if ((image->columns % 8) != 0)
            {
              for (bit=0; bit < (image->columns % 8); bit++)
              {
                q->index=((*p) & (0x80 >> bit) ? 0x01 : 0x00);
                q->length=0;
                q++;
              }
              p++;
            }
          ProgressMonitor(LoadImageText,image->rows-y-1,image->rows);
        }
        SyncImage(image);
        break;
      }
      case 4:
      {
        /*
          Convert PseudoColor scanline to runlength-encoded color packets.
        */
        for (y=image->rows-1; y >= 0; y--)
        {
          p=bmp_pixels+(image->rows-y-1)*bytes_per_line;
          q=image->pixels+(y*image->columns);
          for (x=0; x < ((int) image->columns-1); x+=2)
          {
            q->index=(*p >> 4) & 0xf;
            q->length=0;
            q++;
            q->index=(*p) & 0xf;
            q->length=0;
            p++;
            q++;
          }
          if ((image->columns % 2) != 0)
            {
              q->index=(*p >> 4) & 0xf;
              q->length=0;
              q++;
              p++;
            }
          ProgressMonitor(LoadImageText,image->rows-y-1,image->rows);
        }
        SyncImage(image);
        break;
      }
      case 8:
      {
        /*
          Convert PseudoColor scanline to runlength-encoded color packets.
        */
        if (bmp_header.compression == 1)
          bytes_per_line=image->columns;
        for (y=image->rows-1; y >= 0; y--)
        {
          p=bmp_pixels+(image->rows-y-1)*bytes_per_line;
          q=image->pixels+(y*image->columns);
          for (x=0; x < image->columns; x++)
          {
            q->index=(*p++);
            q->length=0;
            q++;
          }
          ProgressMonitor(LoadImageText,image->rows-y-1,image->rows);
        }
        SyncImage(image);
        break;
      }
      case 24:
      {
        /*
          Convert DirectColor scanline to runlength-encoded color packets.
        */
        for (y=image->rows-1; y >= 0; y--)
        {
          p=bmp_pixels+(image->rows-y-1)*bytes_per_line;
          q=image->pixels+(y*image->columns);
          for (x=0; x < image->columns; x++)
          {
            q->index=0;
            if (image->matte)
              q->index=UpScale(*p++);
            q->blue=UpScale(*p++);
            q->green=UpScale(*p++);
            q->red=UpScale(*p++);
            q->length=0;
            q++;
          }
          ProgressMonitor(LoadImageText,image->rows-y-1,image->rows);
        }
        break;
      }
      default:
        PrematureExit("Not a BMP image file",image);
    }
    free((char *) bmp_pixels);
    CompressImage(image);
    /*
      Proceed to next image.
    */
    status=ReadData((char *) magick,1,2,image->file);
    if ((status == True) && (strncmp((char *) magick,"BM",2) == 0))
      {
        /*
          Allocate image structure.
        */
        image->next=AllocateImage(image_info);
        if (image->next == (Image *) NULL)
          {
            DestroyImages(image);
            return((Image *) NULL);
          }
        (void) strcpy(image->next->filename,image_info->filename);
        image->next->file=image->file;
        image->next->scene=image->scene+1;
        image->next->previous=image;
        image=image->next;
      }
  } while ((status == True) && (strncmp((char *) magick,"BM",2) == 0));
  while (image->previous != (Image *) NULL)
    image=image->previous;
  CloseImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d C G M I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadCGMImage reads a Computer Graphic Meta image file
%  and returns it.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  The format of the ReadCGMImage routine is:
%
%      image=ReadCGMImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadCGMImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadCGMImage(const ImageInfo *image_info)
{
  char
    filename[MaxTextExtent];

  Image
    *image,
    *next_image,
    *proxy_image;

  /*
    Allocate image structure.
  */
  proxy_image=AllocateImage(image_info);
  if (proxy_image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,proxy_image,ReadBinaryType);
  if (proxy_image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",proxy_image);
  CloseImage(proxy_image);
  DestroyImage(proxy_image);
  /*
    Use ralcgm to convert Macintosh CGM image.
  */
  (void) strcpy(filename,image_info->filename);
  (void) sprintf(image_info->filename,CGMCommand,filename);
  image=ReadPSImage(image_info);
  if (image == (Image *) NULL)
    {
      Warning("CGM translation failed",image_info->filename);
      return((Image *) NULL);
    }
  /*
    Assign proper filename.
  */
  do
  {
    (void) strcpy(image->filename,filename);
    next_image=image->next;
    if (next_image != (Image *) NULL)
      image=next_image;
  } while (next_image != (Image *) NULL);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d C M Y K I m a g e                                                  %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadCMYKImage reads an image of raw cyan, magenta, yellow, and
%  black bytes and returns it.  It allocates the memory necessary for the new
%  Image structure and returns a pointer to the new image.
%
%  The format of the ReadCMYKImage routine is:
%
%      image=ReadCMYKImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadCMYKImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadCMYKImage(const ImageInfo *image_info)
{
  Image
    *image;

  int
    count,
    x,
    x_offset,
    y,
    y_offset;

  register int
    i;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  unsigned char
    *scanline;

  unsigned int
    black,
    cyan,
    height,
    magenta,
    packet_size,
    yellow,
    width;

  unsigned short
    value;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Determine width and height, e.g. 640x512.
  */
  width=512;
  height=512;
  x_offset=0;
  y_offset=0;
  x=0;
  if (image_info->size != (char *) NULL)
    (void) XParseGeometry(image_info->size,&x,&y,&width,&height);
  if (image_info->interlace != PartitionInterlace)
    {
      /*
        Open image file.
      */
      OpenImage(image_info,image,ReadBinaryType);
      if (image->file == (FILE *) NULL)
        PrematureExit("Unable to open file",image);
      for (i=0; i < x; i++)
        (void) fgetc(image->file);
    }
  /*
    Allocate memory for a scanline.
  */
  packet_size=4*(QuantumDepth >> 3);
  scanline=(unsigned char *)
    malloc(packet_size*image->columns*sizeof(unsigned char));
  if (scanline == (unsigned char *) NULL)
    PrematureExit("Unable to allocate memory",image);
  do
  {
    /*
      Initialize image structure.
    */
    image->columns=width;
    image->rows=height;
    if (image_info->tile != (char *) NULL)
      (void) XParseGeometry(image_info->tile,&x_offset,&y_offset,
        &image->columns,&image->rows);
    image->packets=image->columns*image->rows;
    image->pixels=(RunlengthPacket *)
      malloc(image->packets*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      PrematureExit("Unable to allocate memory",image);
    /*
      Convert raster image to runlength-encoded packets.
    */
    switch (image_info->interlace)
    {
      case NoneInterlace:
      default:
      {
        /*
          No interlacing:  CMYKCMYKCMYKCMYKCMYK...
        */
        for (y=0; y < y_offset; y++)
          (void) ReadData((char *) scanline,packet_size,width,image->file);
        q=image->pixels;
        for (y=0; y < image->rows; y++)
        {
          if ((y > 0) || (image->previous == (Image *) NULL))
            (void) ReadData((char *) scanline,packet_size,width,image->file);
          p=scanline+packet_size*x_offset;
          for (x=0; x < image->columns; x++)
          {
            ReadQuantum(q->red,p);
            ReadQuantum(q->green,p);
            ReadQuantum(q->blue,p);
            ReadQuantum(q->index,p);
            q->length=0;
            q++;
          }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
        break;
      }
      case LineInterlace:
      {
        /*
          Line interlacing:  CCC...MMM...YYY...KKK...CCC...MMM...YYY...KKK...
        */
        packet_size=image->depth >> 3;
        for (y=0; y < y_offset; y++)
          (void) ReadData((char *) scanline,packet_size,width,image->file);
        for (y=0; y < image->rows; y++)
        {
          if ((y > 0) || (image->previous == (Image *) NULL))
            (void) ReadData((char *) scanline,packet_size,width,image->file);
          p=scanline+packet_size*x_offset;
          q=image->pixels+y*image->columns;
          for (x=0; x < image->columns; x++)
          {
            ReadQuantum(q->red,p);
            q->length=0;
            q++;
          }
          (void) ReadData((char *) scanline,packet_size,width,image->file);
          p=scanline+packet_size*x_offset;
          q=image->pixels+y*image->columns;
          for (x=0; x < image->columns; x++)
          {
            ReadQuantum(q->green,p);
            q++;
          }
          (void) ReadData((char *) scanline,packet_size,width,image->file);
          p=scanline+packet_size*x_offset;
          q=image->pixels+y*image->columns;
          for (x=0; x < image->columns; x++)
          {
            ReadQuantum(q->blue,p);
            q++;
          }
          (void) ReadData((char *) scanline,packet_size,width,image->file);
          p=scanline+packet_size*x_offset;
          q=image->pixels+y*image->columns;
          for (x=0; x < image->columns; x++)
          {
            ReadQuantum(q->index,p);
            q++;
          }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
        break;
      }
      case PlaneInterlace:
      case PartitionInterlace:
      {
        /*
          Plane interlacing:  CCCCCC...MMMMMM...YYYYYY...KKKKKK...
        */
        if (image_info->interlace == PartitionInterlace)
          {
            AppendImageFormat("C",image->filename);
            OpenImage(image_info,image,ReadBinaryType);
            if (image->file == (FILE *) NULL)
              PrematureExit("Unable to open file",image);
          }
        packet_size=image->depth >> 3;
        for (y=0; y < y_offset; y++)
          (void) ReadData((char *) scanline,packet_size,width,image->file);
        i=0;
        q=image->pixels;
        for (y=0; y < image->rows; y++)
        {
          if ((y > 0) || (image->previous == (Image *) NULL))
            (void) ReadData((char *) scanline,packet_size,width,image->file);
          p=scanline+packet_size*x_offset;
          for (x=0; x < image->columns; x++)
          {
            ReadQuantum(q->red,p);
            q->length=0;
            q++;
          }
          ProgressMonitor(LoadImageText,i++,image->rows << 2);
        }
        if (image_info->interlace == PartitionInterlace)
          {
            CloseImage(image);
            AppendImageFormat("M",image->filename);
            OpenImage(image_info,image,ReadBinaryType);
            if (image->file == (FILE *) NULL)
              PrematureExit("Unable to open file",image);
          }
        q=image->pixels;
        for (y=0; y < (height-image->rows); y++)
          (void) ReadData((char *) scanline,packet_size,width,image->file);
        for (y=0; y < image->rows; y++)
        {
          (void) ReadData((char *) scanline,packet_size,width,image->file);
          p=scanline+packet_size*x_offset;
          for (x=0; x < image->columns; x++)
          {
            ReadQuantum(q->green,p);
            q++;
          }
          ProgressMonitor(LoadImageText,i++,image->rows << 2);
        }
        if (image_info->interlace == PartitionInterlace)
          {
            CloseImage(image);
            AppendImageFormat("Y",image->filename);
            OpenImage(image_info,image,ReadBinaryType);
            if (image->file == (FILE *) NULL)
              PrematureExit("Unable to open file",image);
          }
        q=image->pixels;
        for (y=0; y < (height-image->rows); y++)
          (void) ReadData((char *) scanline,packet_size,width,image->file);
        for (y=0; y < image->rows; y++)
        {
          (void) ReadData((char *) scanline,packet_size,width,image->file);
          p=scanline+packet_size*x_offset;
          for (x=0; x < image->columns; x++)
          {
            ReadQuantum(q->blue,p);
            q++;
          }
          ProgressMonitor(LoadImageText,i++,image->rows << 2);
        }
        if (image_info->interlace == PartitionInterlace)
          {
            CloseImage(image);
            AppendImageFormat("K",image->filename);
            OpenImage(image_info,image,ReadBinaryType);
            if (image->file == (FILE *) NULL)
              PrematureExit("Unable to open file",image);
          }
        q=image->pixels;
        for (y=0; y < (height-image->rows); y++)
          (void) ReadData((char *) scanline,packet_size,width,image->file);
        for (y=0; y < image->rows; y++)
        {
          (void) ReadData((char *) scanline,packet_size,width,image->file);
          p=scanline+packet_size*x_offset;
          for (x=0; x < image->columns; x++)
          {
            ReadQuantum(q->index,p);
            q++;
          }
          ProgressMonitor(LoadImageText,i++,image->rows << 2);
        }
        if (image_info->interlace == PartitionInterlace)
          (void) strcpy(image->filename,image_info->filename);
        break;
      }
    }
    /*
      Transform image from CMYK to RGB.
    */
    q=image->pixels;
    for (y=0; y < image->rows; y++)
    {
      for (x=0; x < image->columns; x++)
      {
        cyan=q->red;
        magenta=q->green;
        yellow=q->blue;
        black=q->index;
        if ((cyan+black) > MaxRGB)
          q->red=0;
        else
          q->red=MaxRGB-(cyan+black);
        if ((magenta+black) > MaxRGB)
          q->green=0;
        else
          q->green=MaxRGB-(magenta+black);
        if ((yellow+black) > MaxRGB)
          q->blue=0;
        else
          q->blue=MaxRGB-(yellow+black);
        q->index=0;
        q->length=0;
        q++;
      }
    }
    CompressImage(image);
    /*
      Proceed to next image.
    */
    count=ReadData((char *) scanline,packet_size,width,image->file);
    if (count > 0)
      {
        /*
          Allocate next image structure.
        */
        image->next=AllocateImage(image_info);
        if (image->next == (Image *) NULL)
          {
            DestroyImages(image);
            return((Image *) NULL);
          }
        (void) strcpy(image->next->filename,image_info->filename);
        image->next->file=image->file;
        image->next->scene=image->scene+1;
        image->next->previous=image;
        image=image->next;
      }
  } while (count > 0);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  free((char *) scanline);
  CloseImage(image);
  return(image);
}
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d D P S I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadDPSImage reads a Adobe Postscript image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadDPSImage routine is:
%
%      image=ReadDPSImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadDPSImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
#ifdef HasDPS
static Image *ReadDPSImage(const ImageInfo *image_info)
{
  Display
    *display;

  float
    pixels_per_point;

  Image
    *image;

  int
    sans,
    status,
    x,
    y;

  Pixmap
    pixmap;

  register int
    i;

  register RunlengthPacket
    *p;

  register unsigned long
    pixel;

  Screen
    *screen;

  XColor
    *colors;

  XImage
    *dps_image;

  XRectangle
    bounding_box,
    pixel_size;

  XResourceInfo
    resource_info;

  XrmDatabase
    resource_database;

  XStandardColormap
    *map_info;

  XVisualInfo
    *visual_info;

  /*
    Open X server connection.
  */
  display=XOpenDisplay(image_info->server_name);
  if (display == (Display *) NULL)
    return((Image *) NULL);
  /*
    Set our forgiving error handler.
  */
  XSetErrorHandler(XError);
  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    return((Image *) NULL);
  /*
    Get user defaults from X resource database.
  */
  resource_database=XGetResourceDatabase(display,client_name);
  XGetResourceInfo(resource_database,client_name,&resource_info);
  /*
    Allocate standard colormap.
  */
  map_info=XAllocStandardColormap();
  if (map_info == (XStandardColormap *) NULL)
    Warning("Unable to create standard colormap","Memory allocation failed");
  else
    {
      /*
        Initialize visual info.
      */
      resource_info.visual_type="default";
      visual_info=XBestVisualInfo(display,map_info,&resource_info);
      map_info->colormap=(Colormap) NULL;
    }
  if ((map_info == (XStandardColormap *) NULL) ||
      (visual_info == (XVisualInfo *) NULL))
    {
      DestroyImage(image);
      XFreeResources(display,visual_info,map_info,(XPixelInfo *) NULL,
        (XFontStruct *) NULL,&resource_info,(XWindowInfo *) NULL);
      return((Image *) NULL);
    }
  /*
    Create a pixmap the appropriate size for the image.
  */
  screen=ScreenOfDisplay(display,visual_info->screen);
  pixels_per_point=XDPSPixelsPerPoint(screen);
  if (image_info->density != (char *) NULL)
    {
      float
        x_resolution,
        y_resolution;

      int
        count;

      x_resolution=72.0*XDPSPixelsPerPoint(screen);
      count=sscanf(image_info->density,"%fx%f",&x_resolution,&y_resolution);
      if (count != 2)
        y_resolution=x_resolution;
      pixels_per_point=Min(x_resolution,y_resolution)/72.0;
    }
  status=XDPSCreatePixmapForEPSF((DPSContext) NULL,screen,image->file,
    visual_info->depth,pixels_per_point,&pixmap,&pixel_size,&bounding_box);
  if ((status == dps_status_failure) || (status == dps_status_no_extension))
    {
      DestroyImage(image);
      XFreeResources(display,visual_info,map_info,(XPixelInfo *) NULL,
        (XFontStruct *) NULL,&resource_info,(XWindowInfo *) NULL);
      return((Image *) NULL);
    }
  /*
    Rasterize the file into the pixmap.
  */
  status=XDPSImageFileIntoDrawable((DPSContext) NULL,screen,pixmap,image->file,
    pixel_size.height,visual_info->depth,&bounding_box,-bounding_box.x,
    -bounding_box.y,pixels_per_point,True,False,True,&sans);
  if (status != dps_status_success)
    {
      DestroyImage(image);
      XFreeResources(display,visual_info,map_info,(XPixelInfo *) NULL,
        (XFontStruct *) NULL,&resource_info,(XWindowInfo *) NULL);
      return((Image *) NULL);
    }
  /*
    Initialize DPS X image.
  */
  dps_image=XGetImage(display,pixmap,0,0,pixel_size.width,pixel_size.height,
    AllPlanes,ZPixmap);
  XFreePixmap(display,pixmap);
  if (dps_image == (XImage *) NULL)
    {
      DestroyImage(image);
      XFreeResources(display,visual_info,map_info,(XPixelInfo *) NULL,
        (XFontStruct *) NULL,&resource_info,(XWindowInfo *) NULL);
      return((Image *) NULL);
    }
  /*
    Get the colormap colors.
  */
  colors=(XColor *) malloc(visual_info->colormap_size*sizeof(XColor));
  if (colors == (XColor *) NULL)
    {
      DestroyImage(image);
      XDestroyImage(dps_image);
      XFreeResources(display,visual_info,map_info,(XPixelInfo *) NULL,
        (XFontStruct *) NULL,&resource_info,(XWindowInfo *) NULL);
      return((Image *) NULL);
    }
  if ((visual_info->class != DirectColor) &&
      (visual_info->class != TrueColor))
    for (i=0; i < visual_info->colormap_size; i++)
    {
      colors[i].pixel=i;
      colors[i].pad=0;
    }
  else
    {
      unsigned long
        blue,
        blue_bit,
        green,
        green_bit,
        red,
        red_bit;

      /*
        DirectColor or TrueColor visual.
      */
      red=0;
      green=0;
      blue=0;
      red_bit=visual_info->red_mask & (~(visual_info->red_mask)+1);
      green_bit=visual_info->green_mask & (~(visual_info->green_mask)+1);
      blue_bit=visual_info->blue_mask & (~(visual_info->blue_mask)+1);
      for (i=0; i < visual_info->colormap_size; i++)
      {
        colors[i].pixel=red | green | blue;
        colors[i].pad=0;
        red+=red_bit;
        if (red > visual_info->red_mask)
          red=0;
        green+=green_bit;
        if (green > visual_info->green_mask)
          green=0;
        blue+=blue_bit;
        if (blue > visual_info->blue_mask)
          blue=0;
      }
    }
  XQueryColors(display,XDefaultColormap(display,visual_info->screen),colors,
    visual_info->colormap_size);
  /*
    Convert X image to MIFF format.
  */
  if ((visual_info->class != TrueColor) &&
      (visual_info->class != DirectColor))
    image->class=PseudoClass;
  image->columns=dps_image->width;
  image->rows=dps_image->height;
  image->packets=image->columns*image->rows;
  image->pixels=(RunlengthPacket *)
    malloc(image->packets*sizeof(RunlengthPacket));
  if (image->pixels == (RunlengthPacket *) NULL)
    {
      DestroyImage(image);
      free((char *) colors);
      XDestroyImage(dps_image);
      XFreeResources(display,visual_info,map_info,(XPixelInfo *) NULL,
        (XFontStruct *) NULL,&resource_info,(XWindowInfo *) NULL);
      return((Image *) NULL);
    }
  p=image->pixels;
  switch (image->class)
  {
    case DirectClass:
    default:
    {
      register unsigned long
        color,
        index;

      unsigned long
        blue_mask,
        blue_shift,
        green_mask,
        green_shift,
        red_mask,
        red_shift;

      /*
        Determine shift and mask for red, green, and blue.
      */
      red_mask=visual_info->red_mask;
      red_shift=0;
      while ((red_mask & 0x01) == 0)
      {
        red_mask>>=1;
        red_shift++;
      }
      green_mask=visual_info->green_mask;
      green_shift=0;
      while ((green_mask & 0x01) == 0)
      {
        green_mask>>=1;
        green_shift++;
      }
      blue_mask=visual_info->blue_mask;
      blue_shift=0;
      while ((blue_mask & 0x01) == 0)
      {
        blue_mask>>=1;
        blue_shift++;
      }
      /*
        Convert X image to DirectClass packets.
      */
      if ((visual_info->colormap_size > 0) &&
          (visual_info->class == DirectColor))
        for (y=0; y < image->rows; y++)
        {
          for (x=0; x < image->columns; x++)
          {
            pixel=XGetPixel(dps_image,x,y);
            index=(pixel >> red_shift) & red_mask;
            p->red=XDownScale(colors[index].red);
            index=(pixel >> green_shift) & green_mask;
            p->green=XDownScale(colors[index].green);
            index=(pixel >> blue_shift) & blue_mask;
            p->blue=XDownScale(colors[index].blue);
            p->index=0;
            p->length=0;
            p++;
          }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
      else
        for (y=0; y < image->rows; y++)
        {
          for (x=0; x < image->columns; x++)
          {
            pixel=XGetPixel(dps_image,x,y);
            color=(pixel >> red_shift) & red_mask;
            p->red=XDownScale((color*65535L)/red_mask);
            color=(pixel >> green_shift) & green_mask;
            p->green=XDownScale((color*65535L)/green_mask);
            color=(pixel >> blue_shift) & blue_mask;
            p->blue=XDownScale((color*65535L)/blue_mask);
            p->index=0;
            p->length=0;
            p++;
          }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
      break;
    }
    case PseudoClass:
    {
      /*
        Create colormap.
      */
      image->colors=visual_info->colormap_size;
      image->colormap=(ColorPacket *) malloc(image->colors*sizeof(ColorPacket));
      if (image->colormap == (ColorPacket *) NULL)
        {
          DestroyImage(image);
          free((char *) colors);
          XDestroyImage(dps_image);
          XFreeResources(display,visual_info,map_info,(XPixelInfo *) NULL,
            (XFontStruct *) NULL,&resource_info,(XWindowInfo *) NULL);
          return((Image *) NULL);
        }
      for (i=0; i < image->colors; i++)
      {
        image->colormap[colors[i].pixel].red=XDownScale(colors[i].red);
        image->colormap[colors[i].pixel].green=XDownScale(colors[i].green);
        image->colormap[colors[i].pixel].blue=XDownScale(colors[i].blue);
      }
      /*
        Convert X image to PseudoClass packets.
      */
      for (y=0; y < image->rows; y++)
      {
        for (x=0; x < image->columns; x++)
        {
          p->index=(unsigned short) XGetPixel(dps_image,x,y);
          p->length=0;
          p++;
        }
        ProgressMonitor(LoadImageText,y,image->rows);
      }
      SyncImage(image);
      break;
    }
  }
  if (image->class == PseudoClass)
    CompressColormap(image);
  free((char *) colors);
  XDestroyImage(dps_image);
  /*
    Rasterize matte image.
  */
  status=XDPSCreatePixmapForEPSF((DPSContext) NULL,screen,image->file,1,
    pixels_per_point,&pixmap,&pixel_size,&bounding_box);
  if ((status != dps_status_failure) && (status != dps_status_no_extension))
    {
      status=XDPSImageFileIntoDrawable((DPSContext) NULL,screen,pixmap,
        image->file,pixel_size.height,1,&bounding_box,-bounding_box.x,
        -bounding_box.y,pixels_per_point,True,True,True,&sans);
      if (status == dps_status_success)
        {
          XImage
            *matte_image;

          /*
            Initialize image matte.
          */
          matte_image=XGetImage(display,pixmap,0,0,pixel_size.width,
            pixel_size.height,AllPlanes,ZPixmap);
          XFreePixmap(display,pixmap);
          if (matte_image != (XImage *) NULL)
            {
              image->class=DirectClass;
              image->matte=True;
              p=image->pixels;
              for (y=0; y < image->rows; y++)
                for (x=0; x < image->columns; x++)
                {
                  p->index=Opaque;
                  if (!XGetPixel(matte_image,x,y))
                    p->index=Transparent;
                  p++;
                }
              XDestroyImage(matte_image);
            }
        }
    }
  /*
    Free resources.
  */
  XFreeResources(display,visual_info,map_info,(XPixelInfo *) NULL,
    (XFontStruct *) NULL,&resource_info,(XWindowInfo *) NULL);
  CompressImage(image);
  CloseImage(image);
  return(image);
}
#else
static Image *ReadDPSImage(const ImageInfo *image_info)
{
  Warning("Cannot read DPS images",image_info->filename);
  return((Image *) NULL);
}
#endif

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d F A X I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadFAXImage reads a Group 3 FAX image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadFAXImage routine is:
%
%      image=ReadFAXImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadFAXImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadFAXImage(const ImageInfo *image_info)
{
  Image
    *image;

  int
    x,
    y;

  unsigned int
    height,
    status,
    width;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Determine width and height, e.g. 640x512.
  */
  width=2592;
  height=3508;
  if (image_info->size != (char *) NULL)
    (void) XParseGeometry(image_info->size,&x,&y,&width,&height);
  /*
    Initialize image structure.
  */
  image->class=PseudoClass;
  image->columns=width;
  image->rows=height;
  image->packets=Max((image->columns*image->rows+8) >> 4,1);
  image->pixels=(RunlengthPacket *)
    malloc(image->packets*sizeof(RunlengthPacket));
  image->colors=2;
  image->colormap=(ColorPacket *) malloc(image->colors*sizeof(ColorPacket));
  if ((image->pixels == (RunlengthPacket *) NULL) ||
      (image->colormap == (ColorPacket *) NULL))
    PrematureExit("Unable to allocate memory",image);
  /*
    Monochrome colormap.
  */
  image->colormap[0].red=MaxRGB;
  image->colormap[0].green=MaxRGB;
  image->colormap[0].blue=MaxRGB;
  image->colormap[1].red=0;
  image->colormap[1].green=0;
  image->colormap[1].blue=0;
  status=HuffmanDecodeImage(image);
  if (status == False)
    PrematureExit("Unable to read image data",image);
  CloseImage(image);
  TransformImage(&image,"0x0",(char *) NULL);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e a d F I T S I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadFITSImage reads a FITS image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadFITSImage routine is:
%
%      image=ReadFITSImage(image_info)
%
%  A description of each parameter follows:
%
%    o image: Function ReadFITSImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or if
%      the image cannot be read.
%
%    o filename: Specifies the name of the image to read.
%
%
*/
static Image *ReadFITSImage(const ImageInfo *image_info)
{
  typedef struct _FITSHeader
  {
    unsigned int
      simple;

    int
      bits_per_pixel;

    unsigned int
      number_of_axis,
      columns,
      rows,
      depth;

    double
      min_data,
      max_data,
      zero,
      scale;
  } FITSHeader;

  char
    long_quantum[8],
    keyword[MaxTextExtent],
    value[MaxTextExtent];

  double
    pixel,
    scale,
    scaled_pixel;

  FITSHeader
    fits_header;

  Image
    *image;

  int
    j,
    packet_size,
    y;

  long
    count,
    quantum;

  register int
    c,
    i,
    x;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  unsigned char
    *fits_pixels;

  unsigned int
    status,
    value_expected;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Initialize image header.
  */
  fits_header.simple=False;
  fits_header.bits_per_pixel=8;
  fits_header.columns=1;
  fits_header.rows=1;
  fits_header.depth=1;
  fits_header.min_data=0.0;
  fits_header.max_data=0.0;
  fits_header.zero=0.0;
  fits_header.scale=1.0;
  /*
    Decode image header.
  */
  c=fgetc(image->file);
  count=1;
  if (c == EOF)
    {
      DestroyImage(image);
      return((Image *) NULL);
    }
  for ( ; ; )
  {
    if (!isalnum(c))
      {
        c=fgetc(image->file);
        count++;
      }
    else
      {
        register char
          *p;

        /*
          Determine a keyword and its value.
        */
        p=keyword;
        do
        {
          if ((p-keyword) < (MaxTextExtent-1))
            *p++=(char) c;
          c=fgetc(image->file);
          count++;
        } while (isalnum(c) || (c == '_'));
        *p='\0';
        if (strcmp(keyword,"END") == 0)
          break;
        value_expected=False;
        while (isspace(c) || (c == '='))
        {
          if (c == '=')
            value_expected=True;
          c=fgetc(image->file);
          count++;
        }
        if (value_expected == False)
          continue;
        p=value;
        while (isalnum(c) || (c == '-') || (c == '+') || (c == '.'))
        {
          if ((p-value) < (MaxTextExtent-1))
            *p++=(char) c;
          c=fgetc(image->file);
          count++;
        }
        *p='\0';
        /*
          Assign a value to the specified keyword.
        */
        if (strcmp(keyword,"SIMPLE") == 0)
          fits_header.simple=(*value == 'T') || (*value == 't');
        if (strcmp(keyword,"BITPIX") == 0)
          fits_header.bits_per_pixel=(unsigned int) atoi(value);
        if (strcmp(keyword,"NAXIS") == 0)
          fits_header.number_of_axis=(unsigned int) atoi(value);
        if (strcmp(keyword,"NAXIS1") == 0)
          fits_header.columns=(unsigned int) atoi(value);
        if (strcmp(keyword,"NAXIS2") == 0)
          fits_header.rows=(unsigned int) atoi(value);
        if (strcmp(keyword,"NAXIS3") == 0)
          fits_header.depth=(unsigned int) atoi(value);
        if (strcmp(keyword,"DATAMAX") == 0)
          fits_header.max_data=atof(value);
        if (strcmp(keyword,"DATAMIN") == 0)
          fits_header.min_data=atof(value);
        if (strcmp(keyword,"BZERO") == 0)
          fits_header.zero=atof(value);
        if (strcmp(keyword,"BSCALE") == 0)
          fits_header.scale=atof(value);
      }
    while (isspace(c))
    {
      c=fgetc(image->file);
      count++;
    }
  }
  while (count > 2880)
    count-=2880;
  for ( ; count < 2880; count++)
    (void) fgetc(image->file);
  /*
    Verify that required image information is defined.
  */
  if ((!fits_header.simple) || (fits_header.number_of_axis < 1) ||
      (fits_header.number_of_axis > 4) ||
      (fits_header.columns*fits_header.rows) == 0)
    PrematureExit("image type not supported",image);
  /*
    Create linear colormap.
  */
  image->columns=fits_header.columns;
  image->rows=fits_header.rows;
  image->class=PseudoClass;
  image->colors=MaxRGB+1;
  image->colormap=(ColorPacket *) malloc(image->colors*sizeof(ColorPacket));
  if (image->colormap == (ColorPacket *) NULL)
    PrematureExit("Unable to open file",image);
  for (i=0; i < image->colors; i++)
  {
    image->colormap[i].red=(Quantum) UpScale(i);
    image->colormap[i].green=(Quantum) UpScale(i);
    image->colormap[i].blue=(Quantum) UpScale(i);
  }
  /*
    Initialize image structure.
  */
  image->packets=image->columns*image->rows;
  image->pixels=(RunlengthPacket *)
    malloc(image->packets*sizeof(RunlengthPacket));
  packet_size=fits_header.bits_per_pixel/8;
  if (packet_size < 0)
    packet_size=(-packet_size);
  fits_pixels=(unsigned char *)
    malloc(image->packets*packet_size*sizeof(unsigned char));
  if ((image->pixels == (RunlengthPacket *) NULL) ||
      (fits_pixels == (unsigned char *) NULL))
    PrematureExit("Unable to allocate memory",image);
  /*
    Convert FITS pixels to runlength-encoded packets.
  */
  status=ReadData((char *) fits_pixels,(unsigned int) packet_size,
    image->packets,image->file);
  if (status == False)
    Warning("Insufficient image data in file",image->filename);
  if ((fits_header.min_data == 0.0) && (fits_header.max_data == 0.0))
    {
      /*
        Determine minimum and maximum intensity.
      */
      p=fits_pixels;
      long_quantum[0]=(*p);
      quantum=(*p++);
      for (j=0; j < (packet_size-1); j++)
      {
        long_quantum[j+1]=(*p);
        quantum=(quantum << 8) | (*p++);
      }
      pixel=(double) quantum;
      if (fits_header.bits_per_pixel == -32)
        pixel=(double) (*((float *) &quantum));
      if (fits_header.bits_per_pixel == -64)
        pixel=(double) (*((double *) long_quantum));
      fits_header.min_data=pixel*fits_header.scale+fits_header.zero;
      fits_header.max_data=pixel*fits_header.scale+fits_header.zero;
      for (i=1; i < image->packets; i++)
      {
        long_quantum[0]=(*p);
        quantum=(*p++);
        for (j=0; j < (packet_size-1); j++)
        {
          long_quantum[j+1]=(*p);
          quantum=(quantum << 8) | (*p++);
        }
        pixel=(double) quantum;
        if (fits_header.bits_per_pixel == -32)
          pixel=(double) (*((float *) &quantum));
        if (fits_header.bits_per_pixel == -64)
          pixel=(double) (*((double *) long_quantum));
        scaled_pixel=pixel*fits_header.scale+fits_header.zero;
        if (scaled_pixel < fits_header.min_data)
          fits_header.min_data=scaled_pixel;
        if (scaled_pixel > fits_header.max_data)
          fits_header.max_data=scaled_pixel;
      }
    }
  /*
    Convert FITS pixels to runlength-encoded packets.
  */
  scale=1.0;
  if (fits_header.min_data != fits_header.max_data)
    scale=(MaxRGB+1)/(fits_header.max_data-fits_header.min_data);
  p=fits_pixels;
  q=image->pixels;
  for (y=0; y < image->rows; y++)
  {
    for (x=0; x < image->columns; x++)
    {
      long_quantum[0]=(*p);
      quantum=(*p++);
      for (j=0; j < (packet_size-1); j++)
      {
        long_quantum[j+1]=(*p);
        quantum=(quantum << 8) | (*p++);
      }
      pixel=(double) quantum;
      if (fits_header.bits_per_pixel == -32)
        pixel=(double) (*((float *) &quantum));
      if (fits_header.bits_per_pixel == -64)
        pixel=(double) (*((double *) long_quantum));
      scaled_pixel=scale*
        (pixel*fits_header.scale+fits_header.zero-fits_header.min_data);
      while (scaled_pixel < 0)
        scaled_pixel+=(MaxRGB+1);
      while (scaled_pixel > MaxRGB)
        scaled_pixel-=(MaxRGB+1);
      q->index=(unsigned short) scaled_pixel;
      q->length=0;
      q++;
    }
    ProgressMonitor(LoadImageText,y,image->rows);
  }
  free((char *) fits_pixels);
  SyncImage(image);
  CompressColormap(image);
  CompressImage(image);
  CloseImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d G I F I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadGIFImage reads a Compuserve Graphics image file and returns it.
%  It allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadGIFImage routine is:
%
%      image=ReadGIFImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadGIFImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      an error occurs.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadGIFImage(ImageInfo *image_info)
{
#define BitSet(byte,bit)  (((byte) & (bit)) == (bit))
#define LSBFirstOrder(x,y)  (((y) << 8) | (x))

  char
    *comments,
    geometry[MaxTextExtent];

  Image
    *image;

  int
    status,
    x,
    y;

  RectangleInfo
    page_info;

  register int
    i;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  short int
    transparency_index;

  unsigned char
    background,
    c,
    flag,
    *global_colormap,
    header[MaxTextExtent],
    magick[12];

  unsigned int
    delay,
    dispose,
    global_colors,
    image_count,
    iterations;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Determine if this is a GIF file.
  */
  status=ReadData((char *) magick,1,6,image->file);
  if ((status == False) || ((strncmp((char *) magick,"GIF87",5) != 0) &&
      (strncmp((char *) magick,"GIF89",5) != 0)))
    PrematureExit("Not a GIF image file",image);
  global_colors=0;
  global_colormap=(unsigned char *) NULL;
  page_info.width=LSBFirstReadShort(image->file);
  page_info.height=LSBFirstReadShort(image->file);
  (void) ReadData((char *) &flag,1,1,image->file);
  (void) ReadData((char *) &background,1,1,image->file);
  (void) ReadData((char *) &c,1,1,image->file);  /* reserved */
  if (BitSet(flag,0x80))
    {
      /*
        Read global colormap.
      */
      global_colors=1 << ((flag & 0x07)+1);
      global_colormap=(unsigned char *)
        malloc(3*global_colors*sizeof(unsigned char));
      if (global_colormap == (unsigned char *) NULL)
        PrematureExit("Unable to read image colormap file",image);
      (void) ReadData((char *) global_colormap,3,global_colors,image->file);
    }
  comments=(char *) NULL;
  delay=0;
  dispose=0;
  iterations=1;
  transparency_index=(-1);
  image_count=0;
  for ( ; ; )
  {
    status=ReadData((char *) &c,1,1,image->file);
    if (status == False)
      break;
    if (c == ';')
      break;  /* terminator */
    if (c == '!')
      {
        /*
          GIF Extension block.
        */
        status=ReadData((char *) &c,1,1,image->file);
        if (status == False)
          PrematureExit("Unable to read extention block",image);
        switch (c)
        {
          case 0xf9:
          {
            /*
              Read Graphics Control extension.
            */
            while (ReadDataBlock((char *) header,image->file) > 0);
            dispose=header[0] >> 2;
            delay=(header[2] << 8) | header[1];
            if ((header[0] & 0x01) == 1)
              transparency_index=header[3];
            break;
          }
          case 0xfe:
          {
            int
              length;

            /*
              Read Comment extension.
            */
            for ( ; ; )
            {
              length=ReadDataBlock((char *) header,image->file);
              if (length <= 0)
                break;
              if (comments != (char *) NULL)
                comments=(char *) realloc((char *) comments,
                  (Extent(comments)+length+1)*sizeof(char));
              else
                {
                  comments=(char *) malloc((length+1)*sizeof(char));
                  if (comments != (char *) NULL)
                    *comments='\0';
                }
              if (comments == (char *) NULL)
                PrematureExit("Unable to allocate memory",image);
              header[length]='\0';
              (void) strcat(comments,(char *) header);
            }
            break;
          }
          case 0xff:
          {
            /*
              Read Netscape Loop extension.
            */
            while (ReadDataBlock((char *) header,image->file) > 0);
            iterations=(header[2] << 8) | header[1];
            break;
          }
          default:
          {
            while (ReadDataBlock((char *) header,image->file) > 0);
            break;
          }
        }
      }
    if (c != ',')
      continue;
    if (image_count != 0)
      {
        /*
          Allocate image structure.
        */
        image->next=AllocateImage(image_info);
        if (image->next == (Image *) NULL)
          {
            DestroyImages(image);
            return((Image *) NULL);
          }
        (void) strcpy(image->next->filename,image_info->filename);
        image->next->file=image->file;
        image->next->scene=image->scene+1;
        image->next->previous=image;
        image=image->next;
      }
    image_count++;
    /*
      Read image attributes.
    */
    image->class=PseudoClass;
    page_info.x=LSBFirstReadShort(image->file);
    page_info.y=LSBFirstReadShort(image->file);
    image->columns=LSBFirstReadShort(image->file);
    image->rows=LSBFirstReadShort(image->file);
    image->packets=image->columns*image->rows;
    if (image->packets == 0)
      PrematureExit("image size is 0",image);
    /*
      Allocate image.
    */
    if (image->pixels != (RunlengthPacket *) NULL)
      free((char *) image->pixels);
    image->pixels=(RunlengthPacket *)
      malloc(image->packets*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      PrematureExit("Unable to allocate memory",image);
    image->comments=comments;
    comments=(char *) NULL;
    (void) sprintf(geometry,"%ux%u%+d%+d",page_info.width,page_info.height,
      page_info.x,page_info.y);
    image->page=PostscriptGeometry(geometry);
    image->dispose=dispose;
    dispose=0;
    image->delay=delay;
    delay=0;
    image->iterations=iterations;
    iterations=1;
    /*
      Inititialize colormap.
    */
    (void) ReadData((char *) &flag,1,1,image->file);
    image->interlace=BitSet(flag,0x40) ? PlaneInterlace : NoneInterlace;
    image->colors=!BitSet(flag,0x80) ? global_colors : 1 << ((flag & 0x07)+1);
    image->colormap=(ColorPacket *) malloc(image->colors*sizeof(ColorPacket));
    if (image->colormap == (ColorPacket *) NULL)
      PrematureExit("Unable to allocate memory",image);
    if (!BitSet(flag,0x80))
      {
        /*
          Use global colormap.
        */
        p=global_colormap;
        for (i=0; i < image->colors; i++)
        {
          image->colormap[i].red=UpScale(*p++);
          image->colormap[i].green=UpScale(*p++);
          image->colormap[i].blue=UpScale(*p++);
        }
        image->background_color=image->colormap[background];
      }
    else
      {
        unsigned char
          *colormap;

        /*
          Read local colormap.
        */
        colormap=(unsigned char *)
          malloc(3*image->colors*sizeof(unsigned char));
        if (colormap == (unsigned char *) NULL)
          PrematureExit("Unable to allocate memory",image);
        (void) ReadData((char *) colormap,3,image->colors,image->file);
        p=colormap;
        for (i=0; i < image->colors; i++)
        {
          image->colormap[i].red=UpScale(*p++);
          image->colormap[i].green=UpScale(*p++);
          image->colormap[i].blue=UpScale(*p++);
        }
        free((char *) colormap);
      }
    /*
      Decode image.
    */
    status=GIFDecodeImage(image);
    if (image->interlace != NoneInterlace)
      {
        Image
          *interlaced_image;

        int
          pass;

        register RunlengthPacket
          *p;

        static int
          interlace_rate[4] = { 8, 8, 4, 2 },
          interlace_start[4] = { 0, 4, 2, 1 };

        /*
          Interlace image.
        */
        image_info->interlace=LineInterlace;
        image->orphan=True;
        interlaced_image=CopyImage(image,image->columns,image->rows,True);
        image->orphan=False;
        if (interlaced_image == (Image *) NULL)
          PrematureExit("Unable to allocate memory",image);
        p=interlaced_image->pixels;
        q=image->pixels;
        for (pass=0; pass < 4; pass++)
        {
          y=interlace_start[pass];
          while (y < image->rows)
          {
            q=image->pixels+(y*image->columns);
            for (x=0; x < image->columns; x++)
            {
              *q=(*p);
              p++;
              q++;
            }
            y+=interlace_rate[pass];
          }
        }
        DestroyImage(interlaced_image);
      }
    if (transparency_index >= 0)
      {
        /*
          Create matte channel.
        */
        q=image->pixels;
        for (i=0; i < image->packets; i++)
        {
          if (q->index != (unsigned short) transparency_index)
            q->index=Opaque;
          else
            q->index=Transparent;
          q++;
        }
        transparency_index=(-1);
        image->class=DirectClass;
        image->matte=True;
      }
    if (status == False)
      {
        Warning("Corrupt GIF image",image->filename);
        break;
      }
    CompressImage(image);
    if (image_info->subrange != 0)
      if ((image->scene+1) < image_info->subimage)
        {
          Image
            subimage;

          /*
            Destroy image.
          */
          subimage=(*image);
          image->file=(FILE *) NULL;
          DestroyImage(image);
          image=AllocateImage(image_info);
          if (image == (Image *) NULL)
            return((Image *) NULL);
          image->file=subimage.file;
          image->scene=subimage.scene+1;
          image_count=0;
        }
      else
        if ((image->scene+1) >= (image_info->subimage+image_info->subrange-1))
          break;
  }
  if (global_colormap != (unsigned char *) NULL)
    free((char *) global_colormap);
  if (image->pixels == (RunlengthPacket *) NULL)
    PrematureExit("Corrupt GIF image or subimage not found",image);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  CloseImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d G R A D A T I O N I m a g e                                        %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadGRADATIONImage creates a gradation image and initializes it to
%  the X server color range as specified by the filename.  It allocates the
%  memory necessary for the new Image structure and returns a pointer to the
%  new image.
%
%  The format of the ReadGRADATIONImage routine is:
%
%      image=ReadGRADATIONImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadGRADATIONImage returns a pointer to the image after
%      creating it. A null image is returned if there is a a memory shortage
%      or if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadGRADATIONImage(const ImageInfo *image_info)
{
  char
    colorname[MaxTextExtent];

  double
    brightness,
    brightness_step,
    hue,
    hue_step,
    saturation,
    saturation_step;

  Image
    *image;

  int
    x,
    y;

  register RunlengthPacket
    *q;

  unsigned int
    height,
    width;

  XColor
    color;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Determine width and height, e.g. 640x512.
  */
  width=512;
  height=512;
  if (image_info->size != (char *) NULL)
    (void) XParseGeometry(image_info->size,&x,&y,&width,&height);
  /*
    Initialize Image structure.
  */
  (void) strcpy(image->filename,image_info->filename);
  image->columns=width;
  image->rows=height;
  image->packets=image->columns*image->rows;
  image->pixels=(RunlengthPacket *)
    malloc(image->packets*sizeof(RunlengthPacket));
  if (image->pixels == (RunlengthPacket *) NULL)
    PrematureExit("Unable to allocate memory",image);
  /*
    Determine (Hue, Saturation, Brightness) gradient.
  */
  (void) strcpy(colorname,image_info->filename);
  (void) sscanf(image_info->filename,"%[^-]",colorname);
  (void) XQueryColorDatabase(colorname,&color);
  TransformHSL(XDownScale(color.red),XDownScale(color.green),
    XDownScale(color.blue),&hue,&saturation,&brightness);
  (void) strcpy(colorname,"white");
  if (Intensity(color) > 32767)
    (void) strcpy(colorname,"black");
  (void) sscanf(image_info->filename,"%*[^-]-%s",colorname);
  (void) XQueryColorDatabase(colorname,&color);
  TransformHSL(XDownScale(color.red),XDownScale(color.green),
    XDownScale(color.blue),&hue_step,&saturation_step,&brightness_step);
  hue_step=(hue_step-hue)/(double) image->packets;
  saturation_step=(saturation_step-saturation)/(double) image->packets;
  brightness_step=(brightness_step-brightness)/(double) image->packets;
  /*
    Initialize image pixels.
  */
  q=image->pixels;
  for (y=0; y < image->rows; y++)
  {
    for (x=0; x < image->columns; x++)
    {
      HSLTransform(hue,saturation,brightness,&q->red,&q->green,&q->blue);
      q->index=0;
      q->length=0;
      q++;
      hue+=hue_step;
      saturation+=saturation_step;
      brightness+=brightness_step;
    }
    ProgressMonitor(LoadImageText,y,image->rows);
  }
  CompressImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d G R A Y I m a g e                                                  %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadGRAYImage reads an image of raw grayscale bytes and returns it.
%  It allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadGRAYImage routine is:
%
%      image=ReadGRAYImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadGRAYImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadGRAYImage(const ImageInfo *image_info)
{
  Image
    *image;

  int
    count,
    x,
    x_offset,
    y,
    y_offset;

  register int
    i;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  unsigned char
    *scanline;

  unsigned int
    height,
    packet_size,
    packets,
    width;

  unsigned short
    index,
    value;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Determine width and height, e.g. 640x512.
  */
  width=512;
  height=512;
  x_offset=0;
  y_offset=0;
  if (image_info->size != (char *) NULL)
    {
      x=0;
      (void) XParseGeometry(image_info->size,&x,&y,&width,&height);
      for (i=0; i < x; i++)
        (void) fgetc(image->file);
    }
  /*
    Allocate memory for a scanline.
  */
  packet_size=image->depth >> 3;
  scanline=(unsigned char *)
    malloc(packet_size*image->columns*sizeof(unsigned char));
  if (scanline == (unsigned char *) NULL)
    PrematureExit("Unable to allocate memory",image);
  do
  {
    /*
      Initialize image structure.
    */
    image->columns=width;
    image->rows=height;
    if (image_info->tile != (char *) NULL)
      (void) XParseGeometry(image_info->tile,&x_offset,&y_offset,
        &image->columns,&image->rows);
    image->packets=0;
    packets=Max((image->columns*image->rows+2) >> 2,1);
    image->pixels=(RunlengthPacket *) malloc(packets*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      PrematureExit("Unable to allocate memory",image);
    /*
      Create linear colormap.
    */
    image->class=PseudoClass;
    image->colors=1 << image->depth;
    image->colormap=(ColorPacket *) malloc(image->colors*sizeof(ColorPacket));
    if (image->colormap == (ColorPacket *) NULL)
      PrematureExit("Unable to allocate memory",image);
    for (i=0; i < image->colors; i++)
    {
      image->colormap[i].red=(Quantum) i;
      image->colormap[i].green=(Quantum) i;
      image->colormap[i].blue=(Quantum) i;
    }
    /*
      Convert raster image to runlength-encoded packets.
    */
    for (y=0; y < y_offset; y++)
      (void) ReadData((char *) scanline,packet_size,width,image->file);
    q=image->pixels;
    q->length=MaxRunlength;
    for (y=0; y < image->rows; y++)
    {
      if ((y > 0) || (image->previous == (Image *) NULL))
        (void) ReadData((char *) scanline,packet_size,width,image->file);
      p=scanline+packet_size*x_offset;
      for (x=0; x < image->columns; x++)
      {
        ReadQuantum(index,p);
        if ((index == q->index) && ((int) q->length < MaxRunlength))
          q->length++;
        else
          {
            if (image->packets != 0)
              q++;
            image->packets++;
            if (image->packets == packets)
              {
                packets<<=1;
                image->pixels=(RunlengthPacket *) realloc((char *)
                  image->pixels,packets*sizeof(RunlengthPacket));
                if (image->pixels == (RunlengthPacket *) NULL)
                  {
                    free((char *) scanline);
                    PrematureExit("Unable to allocate memory",image);
                  }
                q=image->pixels+image->packets-1;
              }
            q->index=index;
            q->length=0;
          }
      }
      ProgressMonitor(LoadImageText,y,image->rows);
    }
    image->pixels=(RunlengthPacket *)
      realloc((char *) image->pixels,image->packets*sizeof(RunlengthPacket));
    SyncImage(image);
    CompressColormap(image);
    /*
      Proceed to next image.
    */
    count=ReadData((char *) scanline,packet_size,width,image->file);
    if (count > 0)
      {
        /*
          Allocate next image structure.
        */
        image->next=AllocateImage(image_info);
        if (image->next == (Image *) NULL)
          {
            DestroyImages(image);
            return((Image *) NULL);
          }
        (void) strcpy(image->next->filename,image_info->filename);
        image->next->file=image->file;
        image->next->scene=image->scene+1;
        image->next->previous=image;
        image=image->next;
      }
  } while (count > 0);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  free((char *) scanline);
  CloseImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d H D F I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadHDFImage reads a Hierarchical Data Format image file and
%  returns it.  It allocates the memory necessary for the new Image structure
%  and returns a pointer to the new image.
%
%  The format of the ReadHDFImage routine is:
%
%      image=ReadHDFImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadHDFImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
#ifdef HasHDF
static Image *ReadHDFImage(ImageInfo *image_info)
{
#include "hdf.h"
#undef BSD
#undef LOCAL

  ClassType
    class;

  Image
    *image;

  int
    interlace,
    is_palette,
    status,
    y;

  int32
    height,
    length,
    width;

  register int
    i,
    x;

  register unsigned char
    *p;

  register RunlengthPacket
    *q;

  uint16
    reference;

  unsigned char
    *hdf_pixels;

  unsigned int
    packet_size;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  CloseImage(image);
  /*
    Read HDF image.
  */
  class=DirectClass;
  status=DF24getdims(image->filename,&width,&height,&interlace);
  if (status == -1)
    {
      class=PseudoClass;
      status=DFR8getdims(image->filename,&width,&height,&is_palette);
    }
  if (status == -1)
    PrematureExit("Image file or does not contain any image data",image);
  do
  {
    /*
      Initialize image structure.
    */
    image->class=class;
    image->columns=width;
    image->rows=height;
    image->packets=image->columns*image->rows;
    packet_size=1;
    if (image->class == DirectClass)
      packet_size=3;
    hdf_pixels=(unsigned char *)
      malloc(packet_size*image->packets*sizeof(unsigned char));
    image->pixels=(RunlengthPacket *)
      malloc(image->packets*sizeof(RunlengthPacket));
    if ((hdf_pixels == (unsigned char *) NULL) ||
        (image->pixels == (RunlengthPacket *) NULL))
      PrematureExit("Unable to allocate memory",image);
    q=image->pixels;
    if (image->class == PseudoClass)
      {
        unsigned char
          *hdf_palette;

        /*
          Create colormap.
        */
        hdf_palette=(unsigned char *) malloc(768*sizeof(unsigned char));
        image->colors=256;
        image->colormap=(ColorPacket *)
          malloc(image->colors*sizeof(ColorPacket));
        if ((hdf_palette == (unsigned char *) NULL) ||
            (image->colormap == (ColorPacket *) NULL))
          PrematureExit("Unable to allocate memory",image);
        (void) DFR8getimage(image->filename,hdf_pixels,(int) image->columns,
          (int) image->rows,hdf_palette);
        reference=DFR8lastref();
        /*
          Convert HDF raster image to PseudoClass runlength-encoded packets.
        */
        p=hdf_palette;
        if (is_palette)
          for (i=0; i < 256; i++)
          {
            image->colormap[i].red=UpScale(*p++);
            image->colormap[i].green=UpScale(*p++);
            image->colormap[i].blue=UpScale(*p++);
          }
        else
          for (i=0; i < image->colors; i++)
          {
            image->colormap[i].red=(Quantum) UpScale(i);
            image->colormap[i].green=(Quantum) UpScale(i);
            image->colormap[i].blue=(Quantum) UpScale(i);
          }
        free((char *) hdf_palette);
        p=hdf_pixels;
        for (y=0; y < image->rows; y++)
        {
          for (x=0; x < image->columns; x++)
          {
            q->index=(*p++);
            q->length=0;
            q++;
          }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
        SyncImage(image);
      }
    else
      {
        int
          y;

        /*
          Convert HDF raster image to DirectClass runlength-encoded packets.
        */
        (void) DF24getimage(image->filename,(void *) hdf_pixels,image->columns,
          image->rows);
        reference=DF24lastref();
        p=hdf_pixels;
        image->interlace=interlace ? PlaneInterlace : NoneInterlace;
        q=image->pixels;
        for (y=0; y < image->rows; y++)
        {
          for (x=0; x < image->columns; x++)
          {
            q->red=UpScale(*p++);
            q->green=UpScale(*p++);
            q->blue=UpScale(*p++);
            q->index=0;
            q->length=0;
            q++;
          }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
      }
    length=DFANgetlablen(image->filename,DFTAG_RIG,reference);
    if (length > 0)
      {
        /*
          Read the image label.
        */
        length+=MaxTextExtent;
        image->label=(char *) malloc(length*sizeof(char));
        if (image->label != (char *) NULL)
          DFANgetlabel(image->filename,DFTAG_RIG,reference,image->label,length);
      }
    length=DFANgetdesclen(image->filename,DFTAG_RIG,reference);
    if (length > 0)
      {
        /*
          Read the image comments.
        */
        length+=MaxTextExtent;
        image->comments=(char *) malloc(length*sizeof(char));
        if (image->comments != (char *) NULL)
          DFANgetdesc(image->filename,DFTAG_RIG,reference,image->comments,
            length);
      }
    free((char *) hdf_pixels);
    CompressImage(image);
    class=DirectClass;
    status=DF24getdims(image->filename,&width,&height,&interlace);
    if (status == -1)
      {
        class=PseudoClass;
        status=DFR8getdims(image->filename,&width,&height,&is_palette);
      }
    if (status != -1)
      {
        /*
          Allocate image structure.
        */
        image->next=AllocateImage(image_info);
        if (image->next == (Image *) NULL)
          {
            DestroyImages(image);
            return((Image *) NULL);
          }
        (void) strcpy(image->next->filename,image_info->filename);
        image->next->file=image->file;
        image->next->scene=image->scene+1;
        image->next->previous=image;
        image=image->next;
      }
  } while (status != -1);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  return(image);
}
#else
static Image *ReadHDFImage(const ImageInfo *image_info)
{
  Warning("HDF library is not available",image_info->filename);
  return(ReadMIFFImage(image_info));
}
#endif

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d H I S T O G R A M I m a g e                                        %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadHISTOGRAMImage reads a HISTOGRAM image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadHISTOGRAMImage routine is:
%
%      image=ReadHISTOGRAMImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadHISTOGRAMImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadHISTOGRAMImage(const ImageInfo *image_info)
{
  Image
    *image;

  image=ReadMIFFImage(image_info);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d H T M L I m a g e                                                  %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadHTMLImage reads a HTML image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadHTMLImage routine is:
%
%      image=ReadHTMLImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadHTMLImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadHTMLImage(const ImageInfo *image_info)
{
  Image
    *image;

  Warning("Cannot read HTML images",image_info->filename);
  image=ReadMIFFImage(image_info);
  return(image);
}

#ifdef HasJBIG
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d J B I G I m a g e                                                  %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadJBIGImage reads a JBIG image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadJBIGImage routine is:
%
%      image=ReadJBIGImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadJBIGImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadJBIGImage(const ImageInfo *image_info)
{
#define MaxBufferSize  8192

  Image
    *image;

  int
    status,
    x,
    y;

  long
    length;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  register unsigned short
    index;

  size_t
    count;

  struct jbg_dec_state
    jbig_info;

  unsigned char
    bit,
    buffer[MaxBufferSize];

  unsigned int
    byte,
    height,
    packets,
    width;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Determine maximum width and height, e.g. 640x512.
  */
  width=65535L;
  height=65535L;
  if (image_info->size != (char *) NULL)
    (void) XParseGeometry(image_info->size,&x,&y,&width,&height);
  /*
    Read JBIG file.
  */
  jbg_dec_init(&jbig_info);
  jbg_dec_maxsize(&jbig_info,(unsigned long) width,(unsigned long) height);
  status=JBG_EAGAIN;
  do
  {
    length=(long) fread(buffer,1,MaxBufferSize,image->file);
    if (length == 0)
      break;
    p=buffer;
    count=0;
    while ((length > 0) && ((status == JBG_EAGAIN) || (status == JBG_EOK)))
    {
      status=jbg_dec_in(&jbig_info,p,length,&count);
      p+=count;
      length-=count;
    }
  } while ((status == JBG_EAGAIN) || (status == JBG_EOK));
  /*
    Initialize image structure.
  */
  image->columns=(unsigned int) jbg_dec_getwidth(&jbig_info);
  image->rows=(unsigned int) jbg_dec_getheight(&jbig_info);
  image->packets=0;
  packets=Max((image->columns*image->rows+8) >> 4,1);
  image->pixels=(RunlengthPacket *) malloc(packets*sizeof(RunlengthPacket));
  if (image->pixels == (RunlengthPacket *) NULL)
    PrematureExit("Unable to allocate memory",image);
  /*
    Create colormap.
  */
  image->class=PseudoClass;
  image->colors=2;
  image->colormap=(ColorPacket *) malloc(image->colors*sizeof(ColorPacket));
  if (image->colormap == (ColorPacket *) NULL)
    PrematureExit("Unable to allocate memory",image);
  image->colormap[0].red=0;
  image->colormap[0].green=0;
  image->colormap[0].blue=0;
  image->colormap[1].red=MaxRGB;
  image->colormap[1].green=MaxRGB;
  image->colormap[1].blue=MaxRGB;
  image->x_resolution=300;
  image->y_resolution=300;
  /*
    Convert X bitmap image to runlength-encoded packets.
  */
  byte=0;
  p=jbg_dec_getimage(&jbig_info,0);
  q=image->pixels;
  q->length=MaxRunlength;
  for (y=0; y < image->rows; y++)
  {
    bit=0;
    for (x=0; x < image->columns; x++)
    {
      if (bit == 0)
        byte=(*p++);
      index=(byte & 0x80) ? 0 : 1;
      if ((index == q->index) && ((int) q->length < MaxRunlength))
        q->length++;
      else
        {
          if (image->packets != 0)
            q++;
          image->packets++;
          if (image->packets == packets)
            {
              packets<<=1;
              image->pixels=(RunlengthPacket *) realloc((char *) image->pixels,
                packets*sizeof(RunlengthPacket));
              if (image->pixels == (RunlengthPacket *) NULL)
                {
                  jbg_dec_free(&jbig_info);
                  PrematureExit("Unable to allocate memory",image);
                }
              q=image->pixels+image->packets-1;
            }
          q->index=index;
          q->length=0;
        }
      bit++;
      byte<<=1;
      if (bit == 8)
        bit=0;
    }
    ProgressMonitor(LoadImageText,y,image->rows);
  }
  jbg_dec_free(&jbig_info);
  SyncImage(image);
  image->pixels=(RunlengthPacket *)
    realloc((char *) image->pixels,image->packets*sizeof(RunlengthPacket));
  CloseImage(image);
  return(image);
}
#else
static Image *ReadJBIGImage(const ImageInfo *image_info)
{
  Warning("JBIG library is not available",image_info->filename);
  return(ReadMIFFImage(image_info));
}
#endif

#ifdef HasJPEG
static Image
  *image;

static jmp_buf
  error_recovery;

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d J P E G I m a g e                                                  %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadJPEGImage reads a JPEG image file and returns it.  It allocates
%  the memory necessary for the new Image structure and returns a pointer to
%  the new image.
%
%  The format of the ReadJPEGImage routine is:
%
%      image=ReadJPEGImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadJPEGImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o filename:  Specifies the name of the jpeg image to read.
%
%
*/

static unsigned int GetCharacter(j_decompress_ptr jpeg_info)
{
  struct jpeg_source_mgr
    *data;

  data=jpeg_info->src;
  if (data->bytes_in_buffer == 0)
    (*data->fill_input_buffer) (jpeg_info);
  data->bytes_in_buffer--;
  return(GETJOCTET(*data->next_input_byte++));
}

static boolean CommentHandler(j_decompress_ptr jpeg_info)
{
  long int
    length;

  register char
    *p;

  /*
    Determine length of comment.
  */
  length=GetCharacter(jpeg_info) << 8;
  length+=GetCharacter(jpeg_info);
  length-=2;
  if (image->comments != (char *) NULL)
    image->comments=(char *) realloc((char *) image->comments,
      (unsigned int) (Extent(image->comments)+length+1)*sizeof(char));
  else
    {
      image->comments=(char *)
        malloc((unsigned int) (length+1)*sizeof(char));
      if (image->comments != (char *) NULL)
        *image->comments='\0';
    }
  if (image->comments == (char *) NULL)
    {
      Warning("Memory allocation error",(char *) NULL);
      return(False);
    }
  /*
    Read comment.
  */
  p=image->comments+Extent(image->comments);
  while (--length >= 0)
    *p++=GetCharacter(jpeg_info);
  *p='\0';
  return(True);
}

static void EmitMessage(j_common_ptr jpeg_info,int level)
{
  char
    message[JMSG_LENGTH_MAX];

  struct jpeg_error_mgr
    *jpeg_error;

  jpeg_error=jpeg_info->err;
  (jpeg_error->format_message) (jpeg_info,message);
  if (level < 0)
    {
      if ((jpeg_error->num_warnings == 0) || (jpeg_error->trace_level >= 3))
        Warning((char *) message,image->filename);
      jpeg_error->num_warnings++;
    }
  else
    if (jpeg_error->trace_level >= level)
      Warning((char *) message,image->filename);
}

static void ErrorExit(j_common_ptr jpeg_info)
{
  EmitMessage(jpeg_info,0);
  longjmp(error_recovery,1);
}

static Image *ReadJPEGImage(const ImageInfo *image_info)
{
  int
    x,
    y;

  JSAMPLE
    *jpeg_pixels;

  JSAMPROW
    scanline[1];

  Quantum
    blue,
    green,
    red;

  register int
    i;

  register JSAMPLE
    *p;

  register RunlengthPacket
    *q;

  struct jpeg_decompress_struct
    jpeg_info;

  struct jpeg_error_mgr
    jpeg_error;

  unsigned int
    packets;

  unsigned short
    index;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Initialize image structure.
  */
  jpeg_info.err=jpeg_std_error(&jpeg_error);
  jpeg_info.err->emit_message=EmitMessage;
  jpeg_info.err->error_exit=ErrorExit;
  image->pixels=(RunlengthPacket *) NULL;
  jpeg_pixels=(JSAMPLE *) NULL;
  if (setjmp(error_recovery))
    {
      /*
        JPEG image is corrupt.
      */
      if (jpeg_pixels != (JSAMPLE *) NULL)
        {
          free((char *) jpeg_pixels);
          jpeg_destroy_decompress(&jpeg_info);
        }
      DestroyImage(image);
      return((Image *) NULL);
    }
  jpeg_create_decompress(&jpeg_info);
  jpeg_set_marker_processor(&jpeg_info,JPEG_COM,CommentHandler);
  jpeg_stdio_src(&jpeg_info,image->file);
  (void) jpeg_read_header(&jpeg_info,True);
  if (jpeg_info.saw_JFIF_marker)
    {
      /*
        Set image resolution.
      */
      image->x_resolution=jpeg_info.X_density;
      image->y_resolution=jpeg_info.Y_density;
      if (jpeg_info.density_unit == 1)
        image->units=PixelsPerInchResolution;
      if (jpeg_info.density_unit == 2)
        image->units=PixelsPerCentimeterResolution;
    }
  if (image_info->size != (char *) NULL)
    {
      unsigned int
        height,
        width;

      unsigned long
        scale_factor;

      /*
        Let the JPEG library subsample for us.
      */
      jpeg_calc_output_dimensions(&jpeg_info);
      image->magick_columns=jpeg_info.output_width;
      image->magick_rows=jpeg_info.output_height;
      width=jpeg_info.output_width;
      height=jpeg_info.output_height;
      (void) ParseImageGeometry(image_info->size,&x,&y,&width,&height);
      if (width == 0)
        width=1;
      scale_factor=UpShift(jpeg_info.output_width)/width;
      if (height == 0)
        height=1;
      if (scale_factor > (UpShift(jpeg_info.output_height)/height))
        scale_factor=UpShift(jpeg_info.output_height)/height;
      jpeg_info.scale_denom=DownShift(scale_factor);
      jpeg_calc_output_dimensions(&jpeg_info);
    }
#if (JPEG_LIB_VERSION >= 61)
  jpeg_info.dct_method=JDCT_FLOAT;
  image->interlace=jpeg_info.progressive_mode ? PlaneInterlace : NoneInterlace;
#endif
  jpeg_start_decompress(&jpeg_info);
  image->columns=jpeg_info.output_width;
  image->rows=jpeg_info.output_height;
  image->packets=0;
  packets=Max((image->columns*image->rows+2) >> 2,1);
  image->pixels=(RunlengthPacket *) malloc(packets*sizeof(RunlengthPacket));
  jpeg_pixels=(JSAMPLE *)
    malloc(jpeg_info.output_components*image->columns*sizeof(JSAMPLE));
  if ((image->pixels == (RunlengthPacket *) NULL) ||
      (jpeg_pixels == (JSAMPLE *) NULL))
    PrematureExit("Unable to allocate memory",image);
  if (jpeg_info.out_color_space == JCS_GRAYSCALE)
    {
      /*
        Initialize grayscale colormap.
      */
      image->class=PseudoClass;
      image->colors=256;
      image->colormap=(ColorPacket *) malloc(image->colors*sizeof(ColorPacket));
      if (image->colormap == (ColorPacket *) NULL)
        PrematureExit("Unable to allocate memory",image);
      for (i=0; i < image->colors; i++)
      {
        image->colormap[i].red=UpScale(i);
        image->colormap[i].green=UpScale(i);
        image->colormap[i].blue=UpScale(i);
      }
    }
  /*
    Convert JPEG pixels to runlength-encoded packets.
  */
  red=0;
  green=0;
  blue=0;
  index=0;
  scanline[0]=(JSAMPROW) jpeg_pixels;
  q=image->pixels;
  q->length=MaxRunlength;
  for (y=0; y < image->rows; y++)
  {
    (void) jpeg_read_scanlines(&jpeg_info,scanline,1);
    p=jpeg_pixels;
    for (x=0; x < image->columns; x++)
    {
      if (jpeg_info.data_precision > QuantumDepth)
        {
          if (jpeg_info.out_color_space == JCS_GRAYSCALE)
            index=GETJSAMPLE(*p++) >> 4;
          else
            {
              red=(Quantum) (GETJSAMPLE(*p++) >> 4);
              green=(Quantum) (GETJSAMPLE(*p++) >> 4);
              blue=(Quantum) (GETJSAMPLE(*p++) >> 4);
              if (jpeg_info.out_color_space == JCS_CMYK)
                index=(Quantum) (GETJSAMPLE(*p++) >> 4);
            }
         }
       else
         if (jpeg_info.out_color_space == JCS_GRAYSCALE)
           index=GETJSAMPLE(*p++);
         else
           {
             red=(Quantum) UpScale(GETJSAMPLE(*p++));
             green=(Quantum) UpScale(GETJSAMPLE(*p++));
             blue=(Quantum) UpScale(GETJSAMPLE(*p++));
             if (jpeg_info.out_color_space == JCS_CMYK)
               index=(Quantum) UpScale(GETJSAMPLE(*p++));
           }
      if (jpeg_info.out_color_space == JCS_CMYK)
        {
          index=MAXJSAMPLE-index;
          if ((int) (red-index) < 0)
            red=0;
          else
            red-=index;
          if ((int) (green-index) < 0)
            green=0;
          else
            green-=index;
          if ((int) (blue-index) < 0)
            blue=0;
          else
            blue-=index;
          index=0;
        }
      if ((red == q->red) && (green == q->green) && (blue == q->blue) &&
          (index == q->index) && ((int) q->length < MaxRunlength))
        q->length++;
      else
        {
          if (image->packets != 0)
            q++;
          image->packets++;
          if (image->packets == packets)
            {
              packets<<=1;
              image->pixels=(RunlengthPacket *)
                realloc((char *) image->pixels,packets*sizeof(RunlengthPacket));
              if (image->pixels == (RunlengthPacket *) NULL)
                {
                  free((char *) jpeg_pixels);
                  jpeg_destroy_decompress(&jpeg_info);
                  PrematureExit("Unable to allocate memory",image);
                }
              q=image->pixels+image->packets-1;
            }
          q->red=red;
          q->green=green;
          q->blue=blue;
          q->index=index;
          q->length=0;
        }
    }
    ProgressMonitor(LoadImageText,y,image->rows);
  }
  image->pixels=(RunlengthPacket *)
    realloc((char *) image->pixels,image->packets*sizeof(RunlengthPacket));
  if (image->class == PseudoClass)
    {
      SyncImage(image);
      CompressColormap(image);
    }
  /*
    Free memory.
  */
  free((char *) jpeg_pixels);
  (void) jpeg_finish_decompress(&jpeg_info);
  jpeg_destroy_decompress(&jpeg_info);
  CloseImage(image);
  return(image);
}
#else
static Image *ReadJPEGImage(const ImageInfo *image_info)
{
  Warning("JPEG library is not available",image_info->filename);
  return(ReadMIFFImage(image_info));
}
#endif

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d L O G O I m a g e                                                  %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadLOGOImage reads a LOGO image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadLOGOImage routine is:
%
%      image=ReadLOGOImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadLOGOImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadLOGOImage(ImageInfo *image_info)
{
#include "logo.h"

  char
    *filename,
    logo_filename[MaxTextExtent];

  FILE
    *file;

  Image
    *image;

  register int
    i;

  register unsigned char
    *p;

  unsigned int
    extent;

  /*
    Open temporary output file.
  */
  TemporaryFilename(logo_filename);
  file=fopen(logo_filename,WriteBinaryType);
  if (file == (FILE *) NULL)
    {
      Warning("Unable to write file",logo_filename);
      return(ReadXCImage(image_info));
    }
  p=LogoImage;
  extent=LogoImageExtent;
  if (strcmp(image_info->magick,"GRANITE") == 0)
    {
      p=GraniteImage;
      extent=GraniteImageExtent;
    }
  if (strcmp(image_info->magick,"NETSCAPE") == 0)
    {
      p=NetscapeImage;
      extent=NetscapeImageExtent;
    }
  for (i=0; i < LogoImageExtent; i++)
  {
    (void) fputc((char) *p,file);
    p++;
  }
  if (ferror(file))
    {
      Warning("An error has occurred writing to file",logo_filename);
      (void) fclose(file);
      (void) remove(logo_filename);
      return(ReadXCImage(image_info));
    }
  (void) fclose(file);
  filename=image_info->filename;
  image_info->filename=logo_filename;
  image=ReadGIFImage(image_info);
  image_info->filename=filename;
  if (image != (Image *) NULL)
    (void) strcpy(image->filename,image_info->filename);
  (void) remove(logo_filename);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d M A P I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadMAPImage reads an image of raw RGB colormap and colormap index
%  bytes and returns it.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  The format of the ReadMAPImage routine is:
%
%      image=ReadMAPImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadMAPImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadMAPImage(const ImageInfo *image_info)
{
  Image
    *image;

  int
    colors;

  register int
    i;

  register unsigned char
    *p;

  unsigned char
    *colormap;

  unsigned int
    height,
    packet_size,
    status,
    width;

  unsigned short
    value;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Determine width, height, and number of colors, e.g. 640x512+256.
  */
  width=512;
  height=512;
  colors=256;
  if (image_info->size != (char *) NULL)
    (void) XParseGeometry(image_info->size,&colors,&colors,&width,&height);
  /*
    Initialize image structure.
  */
  image->class=PseudoClass;
  image->compression=NoCompression;
  image->columns=width;
  image->rows=height;
  image->colors=colors;
  image->packets=image->columns*image->rows;
  packet_size=3*(image->depth >> 3);
  colormap=(unsigned char *)
    malloc(packet_size*image->colors*sizeof(unsigned char));
  image->colormap=(ColorPacket *) malloc(image->colors*sizeof(ColorPacket));
  image->packed_pixels=(unsigned char *)
    malloc(image->packets*packet_size*(image->depth >> 3));
  if ((colormap == (unsigned char *) NULL) ||
      (image->colormap == (ColorPacket *) NULL))
    PrematureExit("Unable to allocate memory",image);
  /*
    Read image colormap.
  */
  (void) ReadData((char *) colormap,1,image->colors*packet_size,image->file);
  p=colormap;
  for (i=0; i < image->colors; i++)
  {
    ReadQuantum(image->colormap[i].red,p);
    ReadQuantum(image->colormap[i].green,p);
    ReadQuantum(image->colormap[i].blue,p);
  }
  free((char *) colormap);
  /*
    Convert raster image to runlength-encoded packets.
  */
  packet_size=1;
  if (image->colors > 256)
    packet_size++;
  if (image->packed_pixels != (unsigned char *) NULL)
    free((char *) image->packed_pixels);
  image->packed_pixels=(unsigned char *)
    malloc(image->packets*packet_size);
  if (image->packed_pixels == (unsigned char *) NULL)
    PrematureExit("Unable to allocate memory",image);
  (void) ReadData((char *) image->packed_pixels,packet_size,image->packets,
    image->file);
  status=RunlengthDecodeImage(image);
  if (status == False)
    {
      DestroyImages(image);
      return((Image *) NULL);
    }
  CompressImage(image);
  CloseImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d M A T T E I m a g e                                                %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadMATTEImage reads an image of raw matte bytes and returns it.
%  It allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadMATTEImage routine is:
%
%      image=ReadMATTEImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadMATTEImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadMATTEImage(const ImageInfo *image_info)
{
  Image
    *image;

  int
    x,
    x_offset,
    y,
    y_offset;

  register int
    i;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  unsigned char
    *scanline;

  unsigned int
    height,
    packet_size,
    packets,
    width;

  unsigned short
    index,
    value;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Determine width and height, e.g. 640x512.
  */
  width=512;
  height=512;
  x_offset=0;
  y_offset=0;
  if (image_info->size != (char *) NULL)
    {
      x=0;
      (void) XParseGeometry(image_info->size,&x,&y,&width,&height);
      for (i=0; i < x; i++)
        (void) fgetc(image->file);
    }
  /*
    Initialize image structure.
  */
  image->matte=True;
  image->columns=width;
  image->rows=height;
  packet_size=image->depth >> 3;
  scanline=(unsigned char *)
    malloc(packet_size*image->columns*sizeof(unsigned char));
  if (image_info->tile != (char *) NULL)
    (void) XParseGeometry(image_info->tile,&x_offset,&y_offset,&image->columns,
      &image->rows);
  image->packets=0;
  packets=Max((image->columns*image->rows+2) >> 2,1);
  image->pixels=(RunlengthPacket *) malloc(packets*sizeof(RunlengthPacket));
  if ((scanline == (unsigned char *) NULL) ||
      (image->pixels == (RunlengthPacket *) NULL))
    PrematureExit("Unable to allocate memory",image);
  /*
    Convert raster image to runlength-encoded packets.
  */
  for (y=0; y < y_offset; y++)
    (void) ReadData((char *) scanline,packet_size,width,image->file);
  q=image->pixels;
  q->length=MaxRunlength;
  for (y=0; y < image->rows; y++)
  {
    (void) ReadData((char *) scanline,packet_size,width,image->file);
    p=scanline+packet_size*x_offset;
    for (x=0; x < image->columns; x++)
    {
      ReadQuantum(index,p);
      if ((index == q->index) && ((int) q->length < MaxRunlength))
        q->length++;
      else
        {
          if (image->packets != 0)
            q++;
          image->packets++;
          if (image->packets == packets)
            {
              packets<<=1;
              image->pixels=(RunlengthPacket *) realloc((char *) image->pixels,
                packets*sizeof(RunlengthPacket));
              if (image->pixels == (RunlengthPacket *) NULL)
                {
                  free((char *) scanline);
                  PrematureExit("Unable to allocate memory",image);
                }
              q=image->pixels+image->packets-1;
            }
          q->red=0;
          q->green=0;
          q->blue=0;
          q->index=index;
          q->length=0;
        }
    }
    ProgressMonitor(LoadImageText,y,image->rows);
  }
  image->pixels=(RunlengthPacket *)
    realloc((char *) image->pixels,image->packets*sizeof(RunlengthPacket));
  free((char *) scanline);
  CloseImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e a d M I F F I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadMIFFImage reads a MIFF image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadMIFFImage routine is:
%
%      image=ReadMIFFImage(filename)
%
%  A description of each parameter follows:
%
%    o image: Function ReadMIFFImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadMIFFImage(const ImageInfo *image_info)
{
  char
    keyword[MaxTextExtent],
    value[MaxTextExtent];

  Image
    *image;

  register int
    c,
    i;

  register unsigned char
    *p;

  unsigned int
    length,
    packet_size,
    status;

  unsigned long
    count,
    packets;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  image->depth=8;
  /*
    Decode image header;  header terminates one character beyond a ':'.
  */
  c=fgetc(image->file);
  if (c == EOF)
    {
      DestroyImage(image);
      return((Image *) NULL);
    }
  do
  {
    /*
      Decode image header;  header terminates one character beyond a ':'.
    */
    image->compression=NoCompression;
    while (isgraph(c) && (c != ':'))
    {
      register char
        *p;

      if (c == '{')
        {
          /*
            Read comment-- any text between { }.
          */
          if (image->comments != (char *) NULL)
            {
              length=Extent(image->comments);
              p=image->comments+length;
            }
          else
            {
              length=MaxTextExtent;
              image->comments=(char *) malloc(length*sizeof(char));
              p=image->comments;
            }
          for ( ; image->comments != (char *) NULL; p++)
          {
            c=fgetc(image->file);
            if ((c == EOF) || (c == '}'))
              break;
            if ((p-image->comments+1) >= length)
              {
                *p='\0';
                length<<=1;
                image->comments=(char *)
                  realloc((char *) image->comments,length*sizeof(char));
                if (image->comments == (char *) NULL)
                  break;
                p=image->comments+Extent(image->comments);
              }
            *p=(unsigned char) c;
          }
          if (image->comments == (char *) NULL)
            PrematureExit("Unable to allocate memory",image);
          *p='\0';
          c=fgetc(image->file);
        }
      else
        if (isalnum(c))
          {
            /*
              Determine a keyword and its value.
            */
            p=keyword;
            do
            {
              if ((p-keyword) < (MaxTextExtent-1))
                *p++=(char) c;
              c=fgetc(image->file);
            } while (isalnum(c));
            *p='\0';
            while (isspace(c) || (c == '='))
              c=fgetc(image->file);
            p=value;
            if (c != '"')
              while (!isspace(c) && (c != EOF))
              {
                if ((p-value) < (MaxTextExtent-1))
                  *p++=(char) c;
                c=fgetc(image->file);
              }
            else
              {
                c=fgetc(image->file);
                while ((c != '"') && (c != EOF))
                {
                  if ((p-value) < (MaxTextExtent-1))
                    *p++=(char) c;
                  c=fgetc(image->file);
                }
              }
            *p='\0';
            /*
              Assign a value to the specified keyword.
            */
            if (strcmp(keyword,"class") == 0)
              if (strcmp(value,"PseudoClass") == 0)
                image->class=PseudoClass;
              else
                if (strcmp(value,"DirectClass") == 0)
                  image->class=DirectClass;
                else
                  image->class=UndefinedClass;
            if (strcmp(keyword,"colors") == 0)
              image->colors=(unsigned int) atoi(value);
            if (strcmp(keyword,"compression") == 0)
              if ((strcmp(value,"Zip") == 0) || (strcmp(value,"Zlib") == 0))
                image->compression=ZipCompression;
              else
                if (strcmp(value,"RunlengthEncoded") == 0)
                  image->compression=RunlengthEncodedCompression;
                else
                  image->compression=UndefinedCompression;
            if (strcmp(keyword,"columns") == 0)
              image->columns=(unsigned int) atoi(value);
            if (strcmp(keyword,"delay") == 0)
              image->delay=atoi(value);
            if (strcmp(keyword,"depth") == 0)
              image->depth=atoi(value) <= 8 ? 8 : 16;
            if (strcmp(keyword,"gamma") == 0)
              image->gamma=atof(value);
            if (strcmp(keyword,"id") == 0)
              if (strcmp(value,"ImageMagick") == 0)
                image->id=ImageMagickId;
              else
                image->id=UndefinedId;
            if (strcmp(keyword,"label") == 0)
              {
                image->label=(char *) malloc(Extent(value)+1*sizeof(char));
                if (image->label == (char *) NULL)
                  PrematureExit("Unable to allocate memory",image);
                (void) strcpy(image->label,value);
              }
            if ((strcmp(keyword,"matte") == 0) ||
                (strcmp(keyword,"alpha") == 0))
              if ((strcmp(value,"True") == 0) || (strcmp(value,"true") == 0))
                image->matte=True;
              else
                image->matte=False;
            if (strcmp(keyword,"montage") == 0)
              {
                image->montage=(char *) malloc(Extent(value)+1*sizeof(char));
                if (image->montage == (char *) NULL)
                  PrematureExit("Unable to allocate memory",image);
                (void) strcpy(image->montage,value);
              }
            if (strcmp(keyword,"packets") == 0)
              image->packets=(unsigned int) atoi(value);
            if (strcmp(keyword,"rows") == 0)
              image->rows=(unsigned int) atoi(value);
            if (strcmp(keyword,"scene") == 0)
              image->scene=(unsigned int) atoi(value);
            if (strcmp(keyword,"signature") == 0)
              {
                image->signature=(char *)
                  malloc((Extent(value)+1)*sizeof(char));
                if (image->signature == (char *) NULL)
                  PrematureExit("Unable to allocate memory",image);
                (void) strcpy(image->signature,value);
              }
          }
        else
          c=fgetc(image->file);
      while (isspace(c))
        c=fgetc(image->file);
    }
    (void) fgetc(image->file);
    /*
      Verify that required image information is defined.
    */
    if ((image->id == UndefinedId) || (image->class == UndefinedClass) ||
        (image->compression == UndefinedCompression) || (image->columns == 0) ||
        (image->rows == 0))
      PrematureExit("Incorrect image header in file",image);
    if (image->montage != (char *) NULL)
      {
        register char
          *p;

        /*
          Image directory.
        */
        image->directory=(char *) malloc(MaxTextExtent*sizeof(char));
        if (image->directory == (char *) NULL)
          PrematureExit("Unable to read image data",image);
        p=image->directory;
        do
        {
          *p='\0';
          if (((Extent(image->directory)+1) % MaxTextExtent) == 0)
            {
              /*
                Allocate more memory for the image directory.
              */
              image->directory=(char *) realloc((char *) image->directory,
                (Extent(image->directory)+MaxTextExtent+1)*sizeof(char));
              if (image->directory == (char *) NULL)
                PrematureExit("Unable to read image data",image);
              p=image->directory+Extent(image->directory);
            }
          c=fgetc(image->file);
          *p++=(unsigned char) c;
        } while (c != '\0');
      }
    if (image->class == PseudoClass)
      {
        unsigned int
          colors;

        unsigned short
          value;

        /*
          PseudoClass image cannot have matte data.
        */
        if (image->matte)
          PrematureExit("Matte images must be DirectClass",image);
        /*
          Create image colormap.
        */
        colors=image->colors;
        if (colors == 0)
          colors=256;
        image->colormap=(ColorPacket *) malloc(colors*sizeof(ColorPacket));
        if (image->colormap == (ColorPacket *) NULL)
          PrematureExit("Unable to allocate memory",image);
        if (image->colors == 0)
          for (i=0; i < colors; i++)
          {
            image->colormap[i].red=(Quantum) UpScale(i);
            image->colormap[i].green=(Quantum) UpScale(i);
            image->colormap[i].blue=(Quantum) UpScale(i);
            image->colors++;
          }
        else
          {
            unsigned char
              *colormap;

            /*
              Read image colormap from file.
            */
            packet_size=3*(image->depth >> 3);
            colormap=(unsigned char *)
              malloc(packet_size*image->colors*sizeof(unsigned char));
            if (colormap == (unsigned char *) NULL)
              PrematureExit("Unable to allocate memory",image);
            (void) ReadData((char *) colormap,1,packet_size*image->colors,
              image->file);
            p=colormap;
            for (i=0; i < image->colors; i++)
            {
              ReadQuantum(image->colormap[i].red,p);
              ReadQuantum(image->colormap[i].green,p);
              ReadQuantum(image->colormap[i].blue,p);
            }
            free((char *) colormap);
          }
      }
    /*
      Determine packed packet size.
    */
    if (image->class == PseudoClass)
      {
        image->packet_size=1;
        if (image->colors > 256)
          image->packet_size++;
      }
    else
      {
        image->packet_size=3*(image->depth >> 3);
        if (image->matte)
          image->packet_size++;
      }
    if (image->compression == RunlengthEncodedCompression)
      image->packet_size++;
    packet_size=image->packet_size;
    if (image->compression == ZipCompression)
      packet_size=1;
    /*
      Allocate image pixels.
    */
    if (image->compression == NoCompression)
      image->packets=image->columns*image->rows;
    packets=image->packets;
    if (image->packets == 0)
      packets=image->columns*image->rows;
    image->packed_pixels=(unsigned char *)
      malloc((unsigned int) packets*packet_size*sizeof(unsigned char));
    if (image->packed_pixels == (unsigned char *) NULL)
      PrematureExit("Unable to allocate memory",image);
    /*
      Read image pixels from file.
    */
    if ((image->compression != RunlengthEncodedCompression) ||
        (image->packets != 0))
      (void) ReadData((char *) image->packed_pixels,1,(unsigned int)
        packets*packet_size,image->file);
    else
      {
        /*
          Number of runlength packets is unspecified.
        */
        count=0;
        p=image->packed_pixels;
        do
        {
          (void) ReadData((char *) p,1,packet_size,image->file);
          image->packets++;
          p+=(packet_size-1);
          count+=(*p+1);
          p++;
        }
        while (count < (image->columns*image->rows));
      }
    if (image->compression ==  ZipCompression)
      {
        int
          status;

        unsigned char
          *compressed_pixels;

        /*
          Uncompress image pixels with Zip encoding.
        */
        compressed_pixels=image->packed_pixels;
        packets=image->columns*image->rows*image->packet_size;
        image->packed_pixels=(unsigned char *)
          malloc((packets+8)*sizeof(unsigned char));
        if (image->packed_pixels == (unsigned char *) NULL)
          PrematureExit("Unable to allocate memory",image);
        status=True;
#ifdef HasPNG
        status=uncompress(image->packed_pixels,&packets,compressed_pixels,
          image->packets);
#endif
        image->packets=(unsigned int) (packets/image->packet_size);
        free((char *) compressed_pixels);
        if (status)
          PrematureExit("Unable to Zip uncompress image",image);
      }
    /*
      Unpack the packed image pixels into runlength-encoded pixel packets.
    */
    status=RunlengthDecodeImage(image);
    if (status == False)
      {
        DestroyImages(image);
        return((Image *) NULL);
      }
    /*
      Proceed to next image.
    */
    do
    {
      c=fgetc(image->file);
    } while (!isgraph(c) && (c != EOF));
    if (c != EOF)
      {
        /*
          Allocate image structure.
        */
        image->next=AllocateImage(image_info);
        if (image->next == (Image *) NULL)
          {
            DestroyImages(image);
            return((Image *) NULL);
          }
        (void) strcpy(image->next->filename,image_info->filename);
        image->next->file=image->file;
        image->next->scene=image->scene+1;
        image->next->previous=image;
        image=image->next;
      }
  } while (c != EOF);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  CloseImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d M O N O I m a g e                                                  %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadMONOImage reads an image of raw bites in LSB order and returns
%  it.  It allocates the memory necessary for the new Image structure and
%  returns a pointer to the new image.
%
%  The format of the ReadMONOImage routine is:
%
%      image=ReadMONOImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadMONOImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadMONOImage(const ImageInfo *image_info)
{
  Image
    *image;

  int
    x,
    x_offset,
    y,
    y_offset;

  register int
    i;

  register RunlengthPacket
    *q;

  unsigned char
    bit,
    byte;

  unsigned int
    height,
    packets,
    width;

  unsigned short
    index;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Determine width and height, e.g. 640x512.
  */
  width=512;
  height=512;
  x_offset=0;
  y_offset=0;
  if (image_info->size != (char *) NULL)
    {
      x=0;
      (void) XParseGeometry(image_info->size,&x,&y,&width,&height);
      for (i=0; i < x; i++)
        (void) fgetc(image->file);
    }
  /*
    Initialize image structure.
  */
  image->columns=width;
  image->rows=height;
  if (image_info->tile != (char *) NULL)
    (void) XParseGeometry(image_info->tile,&x_offset,&y_offset,&image->columns,
      &image->rows);
  image->packets=0;
  packets=Max((image->columns*image->rows+8) >> 4,1);
  image->pixels=(RunlengthPacket *) malloc(packets*sizeof(RunlengthPacket));
  image->class=PseudoClass;
  image->colors=2;
  image->colormap=(ColorPacket *) malloc(image->colors*sizeof(ColorPacket));
  if ((image->pixels == (RunlengthPacket *) NULL) ||
      (image->colormap == (ColorPacket *) NULL))
    PrematureExit("Unable to allocate memory",image);
  for (i=0; i < image->colors; i++)
  {
    image->colormap[i].red=(MaxRGB*i)/(image->colors-1);
    image->colormap[i].green=(MaxRGB*i)/(image->colors-1);
    image->colormap[i].blue=(MaxRGB*i)/(image->colors-1);
  }
  /*
    Convert bi-level image to runlength-encoded packets.
  */
  for (y=0; y < ((y_offset*image->columns+7) >> 3); y++)
    (void) fgetc(image->file);
  byte=0;
  q=image->pixels;
  q->length=MaxRunlength;
  for (y=0; y < image->rows; y++)
  {
    bit=0;
    for (x=0; y < ((x_offset+7) >> 3); x++)
      (void) fgetc(image->file);
    for (x=0; x < image->columns; x++)
    {
      if (bit == 0)
        byte=fgetc(image->file);
      index=(byte & 0x01) ? 0 : 1;
      if ((index == q->index) && ((int) q->length < MaxRunlength))
        q->length++;
      else
        {
          if (image->packets != 0)
            q++;
          image->packets++;
          if (image->packets == packets)
            {
              packets<<=1;
              image->pixels=(RunlengthPacket *) realloc((char *)
                image->pixels,packets*sizeof(RunlengthPacket));
              if (image->pixels == (RunlengthPacket *) NULL)
                PrematureExit("Unable to allocate memory",image);
              q=image->pixels+image->packets-1;
            }
          q->index=index;
          q->length=0;
        }
      bit++;
      if (bit == 8)
        bit=0;
      byte>>=1;
    }
    ProgressMonitor(LoadImageText,y,image->rows);
  }
  SyncImage(image);
  CloseImage(image);
  return(image);
}
#ifdef HasMPEG

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d M P E G I m a g e                                                  %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadMPEGImage reads a MPEG image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadMPEGImage routine is:
%
%      image=ReadMPEGImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadMPEGImage returns a pointer to the image after
%      reading. A null image is returned if there is a a memory shortage or if
%      the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadMPEGImage(const ImageInfo *image_info)
{
  Image
    *image;

  ImageDesc
    mpeg_info;

  int
    y;

  register int
    x;

  register unsigned char
    *p;

  register RunlengthPacket
    *q;

  unsigned char
    *mpeg_pixels;

  unsigned int
    number_frames,
    status;

  unsigned long
    lsb_first;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Allocate MPEG pixels.
  */
  (void) OpenMPEG(image->file,&mpeg_info);
  number_frames=(unsigned int)
    (8*image->filesize/mpeg_info.Width/mpeg_info.Height);
  mpeg_pixels=(unsigned char *) malloc(mpeg_info.Size*sizeof(unsigned char));
  if (mpeg_pixels == (unsigned char *) NULL)
    PrematureExit("Unable to allocate memory",image);
  /*
    Read MPEG image.
  */
  status=GetMPEGFrame((char *) mpeg_pixels);
  if (image_info->subrange != 0)
    while (image->scene < image_info->subimage)
    {
      /*
        Skip to next image.
      */
      image->scene++;
      status=GetMPEGFrame((char *) mpeg_pixels);
      if (status == False)
        break;
      ProgressMonitor(LoadImageText,image->scene,number_frames);
    }
  if (status == False)
    {
      free((char *) mpeg_pixels);
      PrematureExit("Corrupt MPEG image",image);
    }
  while (status == True)
  {
    /*
      Initialize image structure.
    */
    image->columns=mpeg_info.Width;
    image->rows=mpeg_info.Height-8;
    image->packets=image->columns*image->rows;
    image->pixels=(RunlengthPacket *)
      malloc(image->packets*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      PrematureExit("Unable to allocate memory",image);
    /*
      Convert MPEG raster image to runlength-encoded packets.
    */
    p=mpeg_pixels;
    q=image->pixels;
    lsb_first=1;
    if (*(char *) &lsb_first)
      for (y=0; y < image->rows; y++)
      {
        for (x=0; x < image->columns; x++)
        {
          q->red=UpScale(*p++);
          q->green=UpScale(*p++);
          q->blue=UpScale(*p++);
          q->length=0;
          p++;
          q++;
        }
      }
    else
      for (y=0; y < image->rows; y++)
      {
        for (x=0; x < image->columns; x++)
        {
          p++;
          q->blue=UpScale(*p++);
          q->green=UpScale(*p++);
          q->red=UpScale(*p++);
          q->length=0;
          q++;
        }
      }
    if (image_info->verbose)
      DescribeImage(image,stderr,False);
    CompressImage(image);
    if (image_info->subrange != 0)
      if (image->scene >= (image_info->subimage+image_info->subrange-1))
        break;
    status=GetMPEGFrame((char *) mpeg_pixels);
    if (status == True)
      {
        /*
          Allocate image structure.
        */
        image->next=AllocateImage(image_info);
        if (image->next == (Image *) NULL)
          {
            DestroyImages(image);
            return((Image *) NULL);
          }
        (void) strcpy(image->next->filename,image_info->filename);
        image->next->file=image->file;
        image->next->scene=image->scene+1;
        image->next->previous=image;
        image=image->next;
      }
    if ((status == False) || (image->scene == number_frames))
      number_frames=image->scene+1;
    ProgressMonitor(LoadImageText,image->scene,number_frames);
  }
  free((char *) mpeg_pixels);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  CloseImage(image);
  return(image);
}
#else
static Image *ReadMPEGImage(const ImageInfo *image_info)
{
  Warning("MPEG library is not available",image_info->filename);
  return(ReadMIFFImage(image_info));
}
#endif

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d M T V I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadMTVImage reads a MTV image file and returns it.  It allocates
%  the memory necessary for the new Image structure and returns a pointer to
%  the new image.
%
%  The format of the ReadMTVImage routine is:
%
%      image=ReadMTVImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadMTVImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadMTVImage(const ImageInfo *image_info)
{
  Image
    *image;

  int
    count,
    y;

  Quantum
    blue,
    green,
    red;

  register int
    x;

  register RunlengthPacket
    *q;

  unsigned int
    columns,
    packets,
    rows;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Read MTV image.
  */
  count=fscanf(image->file,"%u %u\n",&columns,&rows);
  if (count == 0)
    PrematureExit("Not a MTV image file",image);
  do
  {
    /*
      Initialize image structure.
    */
    image->columns=columns;
    image->rows=rows;
    image->packets=0;
    packets=Max((image->columns*image->rows+4) >> 3,1);
    image->pixels=(RunlengthPacket *) malloc(packets*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      PrematureExit("Unable to allocate memory",image);
    /*
      Convert MTV raster image to runlength-encoded packets.
    */
    q=image->pixels;
    q->length=MaxRunlength;
    for (y=0; y < image->rows; y++)
    {
      for (x=0; x < image->columns; x++)
      {
        red=UpScale(fgetc(image->file));
        green=UpScale(fgetc(image->file));
        blue=UpScale(fgetc(image->file));
        if ((red == q->red) && (green == q->green) && (blue == q->blue) &&
            ((int) q->length < MaxRunlength))
          q->length++;
        else
          {
            if (image->packets != 0)
              q++;
            image->packets++;
            if (image->packets == packets)
              {
                packets<<=1;
                image->pixels=(RunlengthPacket *) realloc((char *)
                  image->pixels,packets*sizeof(RunlengthPacket));
                if (image->pixels == (RunlengthPacket *) NULL)
                  PrematureExit("Unable to allocate memory",image);
                q=image->pixels+image->packets-1;
              }
            q->red=red;
            q->green=green;
            q->blue=blue;
            q->index=0;
            q->length=0;
          }
      }
      ProgressMonitor(LoadImageText,y,image->rows);
    }
    image->pixels=(RunlengthPacket *)
      realloc((char *) image->pixels,image->packets*sizeof(RunlengthPacket));
    /*
      Proceed to next image.
    */
    count=fscanf(image->file,"%u %u\n",&columns,&rows);
    if (count > 0)
      {
        /*
          Allocate next image structure.
        */
        image->next=AllocateImage(image_info);
        if (image->next == (Image *) NULL)
          {
            DestroyImages(image);
            return((Image *) NULL);
          }
        (void) strcpy(image->next->filename,image_info->filename);
        image->next->file=image->file;
        image->next->scene=image->scene+1;
        image->next->previous=image;
        image=image->next;
      }
  } while (count > 0);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  CloseImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d N U L L I m a g e                                                  %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadNULLImage reads a NULL image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadNULLImage routine is:
%
%      image=ReadNULLImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadNULLImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadNULLImage(const ImageInfo *image_info)
{
  return(ReadXCImage(image_info));
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d P C D I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadPCDImage reads a Photo CD image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.  Much of the PCD decoder was derived from
%  the program hpcdtoppm(1) by Hadmut Danisch.
%
%  The format of the ReadPCDImage routine is:
%
%      image=ReadPCDImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadPCDImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *OverviewImage(const ImageInfo *image_info,Image *image)
{
  char
    *commands[3],
    *resource_value;

  Display
    *display;

  Image
    *montage_image;

  ImageInfo
    local_info;

  XMontageInfo
    montage_info;

  XResourceInfo
    resource_info;

  XrmDatabase
    resource_database;

  /*
    Initialize to the default values.
  */
  XGetMontageInfo(&montage_info);
  resource_database=(XrmDatabase) NULL;
  resource_info.background_color=DefaultTileBackground;
  resource_info.border_width=atoi(DefaultTileBorderwidth);
  resource_info.colorspace=RGBColorspace;
  resource_info.dither=False;
  resource_info.foreground_color=DefaultTileForeground;
  resource_info.font=DefaultFont;
  resource_info.image_geometry=DefaultTileGeometry;
  resource_info.gravity=CenterGravity;
  resource_info.matte_color=DefaultTileMatte;
  resource_info.monochrome=False;
  resource_info.number_colors=0;
  resource_info.server_name=(char *) NULL;
  resource_info.title=(char *) NULL;
  resource_info.tree_depth=0;
  /*
    Open X server connection.
  */
  display=XOpenDisplay(image_info->server_name);
  if (display != (Display *) NULL)
    {
      /*
        Set our forgiving error handler.
      */
      XSetErrorHandler(XError);
      /*
        Get user defaults from X resource database.
      */
      resource_database=XGetResourceDatabase(display,client_name);
      XGetResourceInfo(resource_database,client_name,&resource_info);
      resource_info.background_color=XGetResourceInstance(resource_database,
        client_name,"background",DefaultTileBackground);
      resource_value=XGetResourceClass(resource_database,client_name,
        "borderWidth",DefaultTileBorderwidth);
      resource_info.border_width=atoi(resource_value);
      resource_info.foreground_color=XGetResourceInstance(resource_database,
        client_name,"foreground",DefaultTileForeground);
      resource_value=
        XGetResourceClass(resource_database,client_name,"frame","True");
      montage_info.frame=IsTrue(resource_value);
      resource_info.image_geometry=XGetResourceInstance(resource_database,
        client_name,"imageGeometry",DefaultTileGeometry);
      resource_info.matte_color=XGetResourceInstance(resource_database,
        client_name,"mattecolor",DefaultTileMatte);
      resource_value=XGetResourceClass(resource_database,client_name,
        "pointsize",DefaultPointSize);
      montage_info.pointsize=atoi(resource_value);
      resource_value=
        XGetResourceClass(resource_database,client_name,"shadow","True");
      montage_info.shadow=IsTrue(resource_value);
      montage_info.tile=XGetResourceClass(resource_database,client_name,"tile",
        montage_info.tile);
      XCloseDisplay(display);
    }
  /*
    Create image tiles.
  */
  local_info=(*image_info);
  commands[0]=client_name;
  commands[1]="-label";
  commands[2]=DefaultTileLabel;
  MogrifyImages(&local_info,3,commands,&image);
  commands[1]="-geometry";
  commands[2]=resource_info.image_geometry;
  MogrifyImages(&local_info,3,commands,&image);
  /*
    Create the PCD Overview image.
  */
  (void) strcpy(montage_info.filename,image_info->filename);
  montage_image=XMontageImages(&resource_info,&montage_info,image);
  if (montage_image == (Image *) NULL)
    PrematureExit("Unable to allocate memory",image);
  CompressImage(montage_image);
  return(montage_image);
}

static Image *ReadPCDImage(const ImageInfo *image_info)
{
  Image
    *image;

  long int
    offset;

  register int
    i;

  register RunlengthPacket
    *p;

  register unsigned char
    *c1,
    *c2,
    *y;

  unsigned char
    *chroma1,
    *chroma2,
    *header,
    *luma;

  unsigned int
    height,
    number_images,
    overview,
    rotate,
    status,
    subimage,
    width;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Determine if this is a PCD file.
  */
  header=(unsigned char *) malloc(3*0x800*sizeof(unsigned char));
  if (header == (unsigned char *) NULL)
    PrematureExit("Unable to allocate memory",image);
  status=ReadData((char *) header,1,3*0x800,image->file);
  overview=strncmp((char *) header,"PCD_OPA",7) == 0;
  if ((status == False) ||
      ((strncmp((char *) header+0x800,"PCD",3) != 0) && !overview))
    PrematureExit("Not a PCD image file",image);
  rotate=header[0x0e02] & 0x03;
  number_images=(header[10] << 8) | header[11];
  free((char *) header);
  subimage=3;
  if (image_info->subrange != 0)
    subimage=image_info->subimage;
  if (image_info->size != (char *) NULL)
    {
      int
        x,
        y;

      /*
        Determine which image size to extract.
      */
      width=768;
      height=512;
      (void) XParseGeometry(image_info->size,&x,&y,&width,&height);
      for (subimage=1; subimage <= 6; subimage++)
      {
        if ((width <= 192) && (height <= 128))
          break;
        width>>=1;
        height>>=1;
      }
    }
  if (overview)
    subimage=1;
  /*
    Initialize image structure.
  */
  width=192;
  height=128;
  for (i=1; i < Min(subimage,3); i++)
  {
    width<<=1;
    height<<=1;
  }
  image->columns=width;
  image->rows=height;
  for ( ; i < subimage; i++)
  {
    image->columns<<=1;
    image->rows<<=1;
  }
  /*
    Allocate luma and chroma memory.
  */
  image->packets=image->columns*image->rows;
  chroma1=(unsigned char *) malloc((image->packets+1)*sizeof(unsigned char));
  chroma2=(unsigned char *) malloc((image->packets+1)*sizeof(unsigned char));
  luma=(unsigned char *) malloc((image->packets+1)*sizeof(unsigned char));
  if ((chroma1 == (unsigned char *) NULL) ||
      (chroma2 == (unsigned char *) NULL) || (luma == (unsigned char *) NULL))
    PrematureExit("Unable to allocate memory",image);
  /*
    Advance to image data.
  */
  offset=93;
  if (overview)
    offset=2;
  else
    if (subimage == 2)
      offset=20;
    else
      if (subimage == 1)
        offset=1;
  for (i=0; i < (offset*0x800); i++)
    (void) fgetc(image->file);
  if (overview)
    {
      Image
        *overview_image;

      MonitorHandler
        handler;

      register int
        j;

      /*
        Read thumbnails from overview image.
      */
      for (j=1; j <= number_images; j++)
      {
        handler=SetMonitorHandler((MonitorHandler) NULL);
        (void) sprintf(image->filename,"images/img%04d.pcd",j);
        (void) sprintf(image->magick_filename,"images/img%04d.pcd",j);
        image->scene=j;
        image->columns=width;
        image->rows=height;
        image->packets=image->columns*image->rows;
        image->pixels=(RunlengthPacket *)
          malloc(image->packets*sizeof(RunlengthPacket));
        if (image->pixels == (RunlengthPacket *) NULL)
          PrematureExit("Unable to allocate memory",image);
        y=luma;
        c1=chroma1;
        c2=chroma2;
        for (i=0; i < height; i+=2)
        {
          (void) ReadData((char *) y,1,width,image->file);
          y+=image->columns;
          (void) ReadData((char *) y,1,width,image->file);
          y+=image->columns;
          (void) ReadData((char *) c1,1,width >> 1,image->file);
          c1+=image->columns;
          (void) ReadData((char *) c2,1,width >> 1,image->file);
          c2+=image->columns;
        }
        Upsample(image->columns >> 1,image->rows >> 1,image->columns,chroma1);
        Upsample(image->columns >> 1,image->rows >> 1,image->columns,chroma2);
        /*
          Transfer luminance and chrominance channels.
        */
        p=image->pixels;
        y=luma;
        c1=chroma1;
        c2=chroma2;
        for (i=0; i < image->packets; i++)
        {
          p->red=UpScale(*y++);
          p->green=UpScale(*c1++);
          p->blue=UpScale(*c2++);
          p->index=0;
          p->length=0;
          p++;
        }
        TransformRGBImage(image,YCCColorspace);
        CompressImage(image);
        if (j < number_images)
          {
            /*
              Allocate image structure.
            */
            image->next=AllocateImage(image_info);
            if (image->next == (Image *) NULL)
              {
                DestroyImages(image);
                return((Image *) NULL);
              }
            image->next->file=image->file;
            image->next->previous=image;
            image=image->next;
          }
        (void) SetMonitorHandler(handler);
        ProgressMonitor(LoadImageText,j-1,number_images);
      }
      free(chroma2);
      free(chroma1);
      free(luma);
      while (image->previous != (Image *) NULL)
        image=image->previous;
      overview_image=OverviewImage(image_info,image);
      return(overview_image);
    }
  /*
    Allocate image pixels.
  */
  image->pixels=(RunlengthPacket *)
    malloc(image->packets*sizeof(RunlengthPacket));
  if (image->pixels == (RunlengthPacket *) NULL)
    PrematureExit("Unable to allocate memory",image);
  /*
    Read interleaved image.
  */
  y=luma;
  c1=chroma1;
  c2=chroma2;
  for (i=0; i < height; i+=2)
  {
    (void) ReadData((char *) y,1,width,image->file);
    y+=image->columns;
    (void) ReadData((char *) y,1,width,image->file);
    y+=image->columns;
    (void) ReadData((char *) c1,1,width >> 1,image->file);
    c1+=image->columns;
    (void) ReadData((char *) c2,1,width >> 1,image->file);
    c2+=image->columns;
  }
  if (subimage >= 4)
    {
      /*
        Recover luminance deltas for 1536x1024 image.
      */
      Upsample(768,512,image->columns,luma);
      Upsample(384,256,image->columns,chroma1);
      Upsample(384,256,image->columns,chroma2);
      image->rows=1024;
      for (i=0; i < (4*0x800); i++)
        (void) fgetc(image->file);
      status=PCDDecodeImage(image,luma,chroma1,chroma2);
      if ((subimage >= 5) && status)
        {
          /*
            Recover luminance deltas for 3072x2048 image.
          */
          Upsample(1536,1024,image->columns,luma);
          Upsample(768,512,image->columns,chroma1);
          Upsample(768,512,image->columns,chroma2);
          image->rows=2048;
          offset=ftell(image->file)/0x800+12;
          (void) fseek(image->file,offset*0x800,0);
          status=PCDDecodeImage(image,luma,chroma1,chroma2);
          if ((subimage >= 6) && status)
            {
              /*
                Recover luminance deltas for 6144x4096 image (vaporware).
              */
              Upsample(3072,2048,image->columns,luma);
              Upsample(1536,1024,image->columns,chroma1);
              Upsample(1536,1024,image->columns,chroma2);
              image->rows=4096;
            }
        }
    }
  Upsample(image->columns >> 1,image->rows >> 1,image->columns,chroma1);
  Upsample(image->columns >> 1,image->rows >> 1,image->columns,chroma2);
  /*
    Transfer luminance and chrominance channels.
  */
  p=image->pixels;
  y=luma;
  c1=chroma1;
  c2=chroma2;
  for (i=0; i < image->packets; i++)
  {
    p->red=UpScale(*y++);
    p->green=UpScale(*c1++);
    p->blue=UpScale(*c2++);
    p->index=0;
    p->length=0;
    p++;
    if (QuantumTick(i,image))
      ProgressMonitor(LoadImageText,i,image->packets);
  }
  free(chroma2);
  free(chroma1);
  free(luma);
  TransformRGBImage(image,YCCColorspace);
  if ((rotate == 1) || (rotate == 3))
    {
      double
        degrees;

      Image
        *rotated_image;

      /*
        Rotate image.
      */
      degrees=rotate == 1 ? -90.0 : 90.0;
      image->orphan=True;
      rotated_image=RotateImage(image,degrees,False,True);
      image->orphan=False;
      if (rotated_image != (Image *) NULL)
        {
          DestroyImage(image);
          image=rotated_image;
        }
    }
  CompressImage(image);
  CloseImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d P C L I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadPCLImage reads a Page Control Language image file and returns
%  it.  It allocates the memory necessary for the new Image structure and
%  returns a pointer to the new image.
%
%  The format of the ReadPCLImage routine is:
%
%      image=ReadPCLImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadPCLImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadPCLImage(const ImageInfo *image_info)
{
  Image
    *image;

  Warning("Cannot read PCL images",image_info->filename);
  image=ReadMIFFImage(image_info);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d P C X I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadPCXImage reads a ZSoft IBM PC Paintbrush file and returns it.
%  It allocates the memory necessary for the new Image structure and returns
%  a pointer to the new image.
%
%  The format of the ReadPCXImage routine is:
%
%      image=ReadPCXImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadPCXImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadPCXImage(const ImageInfo *image_info)
{
  typedef struct _PCXHeader
  {
    unsigned char
      identifier,
      version,
      encoding,
      bits_per_pixel;

    short int
      left,
      top,
      right,
      bottom,
      horizontal_resolution,
      vertical_resolution;

    unsigned char
      reserved,
      planes;

    short int
      bytes_per_line,
      palette_info;

    unsigned char
      colormap_signature;
  } PCXHeader;

  PCXHeader
    pcx_header;

  Image
    *image;

  int
    bits,
    count,
    id,
    mask,
    packets,
    pcx_packets;

  Quantum
    blue,
    green,
    red;

  register int
    i,
    x,
    y;

  register RunlengthPacket
    *q;

  register unsigned char
    *p,
    *r;

  unsigned char
    packet,
    *pcx_colormap,
    *pcx_pixels,
    *scanline;

  unsigned int
    status;

  unsigned long
    *page_table;

  unsigned short
    index;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Determine if this is a PCX file.
  */
  page_table=(unsigned long *) NULL;
  if (strcmp(image_info->magick,"DCX") == 0)
    {
      unsigned long
        magic;

      /*
        Read the DCX page table.
      */
      magic=LSBFirstReadLong(image->file);
      if (magic != 987654321)
        PrematureExit("Not a DCX image file",image);
      page_table=(unsigned long *) malloc(1024*sizeof(unsigned long));
      if (page_table == (unsigned long *) NULL)
        PrematureExit("Unable to allocate memory",image);
      for (id=0; id < 1024; id++)
      {
        page_table[id]=LSBFirstReadLong(image->file);
        if (page_table[id] == 0)
          break;
      }
    }
  if (page_table != (unsigned long *) NULL)
    (void) fseek(image->file,(long) page_table[0],0);
  status=ReadData((char *) &pcx_header.identifier,1,1,image->file);
  for (id=1; id < 1024; id++)
  {
    /*
      Verify PCX identifier.
    */
    (void) ReadData((char *) &pcx_header.version,1,1,image->file);
    if ((status == False) || (pcx_header.identifier != 0x0a) ||
        ((pcx_header.version != 2) && (pcx_header.version != 5)))
      PrematureExit("Not a PCX image file",image);
    (void) ReadData((char *) &pcx_header.encoding,1,1,image->file);
    (void) ReadData((char *) &pcx_header.bits_per_pixel,1,1,image->file);
    pcx_header.left=LSBFirstReadShort(image->file);
    pcx_header.top=LSBFirstReadShort(image->file);
    pcx_header.right=LSBFirstReadShort(image->file);
    pcx_header.bottom=LSBFirstReadShort(image->file);
    pcx_header.horizontal_resolution=LSBFirstReadShort(image->file);
    pcx_header.vertical_resolution=LSBFirstReadShort(image->file);
    /*
      Read PCX raster colormap.
    */
    image->columns=(pcx_header.right-pcx_header.left)+1;
    image->rows=(pcx_header.bottom-pcx_header.top)+1;
    image->units=PixelsPerInchResolution;
    image->x_resolution=pcx_header.horizontal_resolution;
    image->y_resolution=pcx_header.vertical_resolution;
    image->packets=image->columns*image->rows;
    image->colors=16;
    image->colormap=(ColorPacket *) malloc(256*sizeof(ColorPacket));
    pcx_colormap=(unsigned char *) malloc(3*256*sizeof(unsigned char));
    if ((image->colormap == (ColorPacket *) NULL) ||
        (pcx_colormap == (unsigned char *) NULL))
      PrematureExit("Unable to allocate memory",image);
    (void) ReadData((char *) pcx_colormap,3,image->colors,image->file);
    p=pcx_colormap;
    for (i=0; i < image->colors; i++)
    {
      image->colormap[i].red=UpScale(*p++);
      image->colormap[i].green=UpScale(*p++);
      image->colormap[i].blue=UpScale(*p++);
    }
    (void) ReadData((char *) &pcx_header.reserved,1,1,image->file);
    (void) ReadData((char *) &pcx_header.planes,1,1,image->file);
    if ((pcx_header.bits_per_pixel != 8) || (pcx_header.planes == 1))
      image->class=PseudoClass;
    pcx_header.bytes_per_line=LSBFirstReadShort(image->file);
    pcx_header.palette_info=LSBFirstReadShort(image->file);
    for (i=0; i < 58; i++)
      (void) fgetc(image->file);
    /*
      Read image data.
    */
    image->packets=0;
    packets=Max((image->columns*image->rows+4) >> 3,1);
    if (pcx_header.bits_per_pixel == 1)
      packets=Max((image->columns*image->rows+8) >> 4,1);
    image->pixels=(RunlengthPacket *) malloc(packets*sizeof(RunlengthPacket));
    pcx_packets=image->rows*pcx_header.bytes_per_line*pcx_header.planes;
    pcx_pixels=(unsigned char *) malloc(pcx_packets*sizeof(unsigned char));
    scanline=(unsigned char *)
      malloc(image->columns*pcx_header.planes*sizeof(unsigned char));
    if ((image->pixels == (RunlengthPacket *) NULL) ||
        (pcx_pixels == (unsigned char *) NULL) ||
        (scanline == (unsigned char *) NULL))
      PrematureExit("Unable to allocate memory",image);
    /*
      Uncompress image data.
    */
    p=pcx_pixels;
    while (pcx_packets > 0)
    {
      packet=fgetc(image->file);
      if ((packet & 0xc0) != 0xc0)
        {
          *p++=packet;
          pcx_packets--;
          continue;
        }
      count=packet & 0x3f;
      for (packet=fgetc(image->file); count > 0; count--)
      {
        *p++=packet;
        pcx_packets--;
        if (pcx_packets == 0)
          break;
      }
    }
    if (image->class == DirectClass)
      image->matte=pcx_header.planes > 3;
    else
      if (pcx_header.version == 5)
        {
          /*
            Initialize image colormap.
          */
          image->colors=1 << (pcx_header.bits_per_pixel*pcx_header.planes);
          if (image->colors > 256)
            PrematureExit("PCX colormap exceeded 256 colors",image);
          if (image->colors > 16)
            {
              /*
                256 color images have their color map at the end of the file.
              */
              (void) ReadData((char *) &pcx_header.colormap_signature,1,1,
                image->file);
              (void) ReadData((char *) pcx_colormap,3,image->colors,
                image->file);
              p=pcx_colormap;
              for (i=0; i < image->colors; i++)
              {
                image->colormap[i].red=UpScale(*p++);
                image->colormap[i].green=UpScale(*p++);
                image->colormap[i].blue=UpScale(*p++);
              }
            }
          else
            if (Intensity(image->colormap[0]) == Intensity(image->colormap[1]))
              if (image->colors == 2)
                {
                  /*
                    Monochrome colormap.
                  */
                  image->colormap[0].red=MaxRGB;
                  image->colormap[0].green=MaxRGB;
                  image->colormap[0].blue=MaxRGB;
                  image->colormap[1].red=0;
                  image->colormap[1].green=0;
                  image->colormap[1].blue=0;
                }
          free((char *) pcx_colormap);
        }
    /*
      Convert PCX raster image to runlength-encoded packets.
    */
    red=0;
    green=0;
    blue=0;
    index=0;
    q=image->pixels;
    q->length=MaxRunlength;
    for (y=0; y < image->rows; y++)
    {
      p=pcx_pixels+(y*pcx_header.bytes_per_line*pcx_header.planes);
      r=scanline;
      if (image->class == DirectClass)
        for (i=0; i < (int) pcx_header.planes; i++)
        {
          r=scanline+i;
          for (x=0; x < pcx_header.bytes_per_line; x++)
          {
            switch (i)
            {
              case 0:
              {
                *r=UpScale(*p++);
                break;
              }
              case 1:
              {
                *r=UpScale(*p++);
                break;
              }
              case 2:
              {
                *r=UpScale(*p++);
                break;
              }
              case 3:
              default:
              {
                *r=UpScale(*p++);
                break;
              }
            }
            r+=pcx_header.planes;
          }
        }
      else
        if (pcx_header.planes > 1)
          {
            for (x=0; x < image->columns; x++)
              *r++=0;
            for (i=0; i < (int) pcx_header.planes; i++)
            {
              r=scanline;
              for (x=0; x < pcx_header.bytes_per_line; x++)
              {
                 bits=(*p++);
                 for (mask=0x80; mask != 0; mask>>=1)
                 {
                   if (bits & mask)
                     *r|=1 << i;
                   r++;
                 }
               }
            }
          }
        else
          switch (pcx_header.bits_per_pixel)
          {
            case 1:
            {
              register int
                bit;

              for (x=0; x < ((int) image->columns-7); x+=8)
              {
                for (bit=7; bit >= 0; bit--)
                  *r++=((*p) & (0x01 << bit) ? 0x01 : 0x00);
                p++;
              }
              if ((image->columns % 8) != 0)
                {
                  for (bit=7; bit >= (8-(image->columns % 8)); bit--)
                    *r++=((*p) & (0x01 << bit) ? 0x01 : 0x00);
                  p++;
                }
              break;
            }
            case 2:
            {
              for (x=0; x < ((int) image->columns-3); x+=4)
              {
                *r++=(*p >> 6) & 0x3;
                *r++=(*p >> 4) & 0x3;
                *r++=(*p >> 2) & 0x3;
                *r++=(*p) & 0x3;
                p++;
              }
              if ((image->columns % 4) != 0)
                {
                  for (i=3; i >= (4-(image->columns % 4)); i--)
                    *r++=(*p >> (i*2)) & 0x03;
                  p++;
                }
              break;
            }
            case 4:
            {
              for (x=0; x < ((int) image->columns-1); x+=2)
              {
                *r++=(*p >> 4) & 0xf;
                *r++=(*p) & 0xf;
                p++;
              }
              if ((image->columns % 2) != 0)
                *r++=(*p++ >> 4) & 0xf;
              break;
            }
            case 8:
            {
              for (x=0; x < image->columns; x++)
                *r++=(*p++);
              break;
            }
            default:
              break;
          }
      /*
        Transfer image scanline.
      */
      r=scanline;
      for (x=0; x < image->columns; x++)
      {
        if (image->class == PseudoClass)
          index=(*r++);
        else
          {
            red=UpScale(*r++);
            green=UpScale(*r++);
            blue=UpScale(*r++);
            if (image->matte)
              index=UpScale(*r++);
          }
        if ((red == q->red) && (green == q->green) && (blue == q->blue) &&
            (index == q->index) && ((int) q->length < MaxRunlength))
          q->length++;
        else
          {
            if (image->packets != 0)
              q++;
            image->packets++;
            if (image->packets == packets)
              {
                packets<<=1;
                image->pixels=(RunlengthPacket *) realloc((char *)
                  image->pixels,packets*sizeof(RunlengthPacket));
                if (image->pixels == (RunlengthPacket *) NULL)
                  {
                    free((char *) scanline);
                    PrematureExit("Unable to allocate memory",image);
                  }
                q=image->pixels+image->packets-1;
              }
            q->red=red;
            q->green=green;
            q->blue=blue;
            q->index=index;
            q->length=0;
          }
      }
      ProgressMonitor(LoadImageText,y,image->rows);
    }
    if (image->class == PseudoClass)
      {
        SyncImage(image);
        CompressColormap(image);
      }
    free((char *) scanline);
    free((char *) pcx_pixels);
    image->pixels=(RunlengthPacket *) realloc((char *) image->pixels,
      image->packets*sizeof(RunlengthPacket));
    /*
      Proceed to next image.
    */
    if (page_table == (unsigned long *) NULL)
      break;
    if (page_table[id] == 0)
      break;
    (void) fseek(image->file,(long) page_table[id],0);
    status=ReadData((char *) &pcx_header.identifier,1,1,image->file);
    if ((status == True) && (pcx_header.identifier == 0x0a))
      {
        /*
          Allocate image structure.
        */
        image->next=AllocateImage(image_info);
        if (image->next == (Image *) NULL)
          {
            DestroyImages(image);
            return((Image *) NULL);
          }
        (void) strcpy(image->next->filename,image_info->filename);
        image->next->file=image->file;
        image->next->scene=image->scene+1;
        image->next->previous=image;
        image=image->next;
      }
  }
  if (page_table != (unsigned long *) NULL)
    free((char *) page_table);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  CloseImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d P D F I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadPDFImage reads a Portable Document Format image file and
%  returns it.  It allocates the memory necessary for the new Image structure
%  and returns a pointer to the new image.
%
%  The format of the ReadPDFImage routine is:
%
%      image=ReadPDFImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadPDFImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadPDFImage(const ImageInfo *image_info)
{
#define MediaBox  "/MediaBox ["

  char
    command[MaxTextExtent],
    density[MaxTextExtent],
    *device,
    filename[MaxTextExtent],
    options[MaxTextExtent],
    postscript_filename[MaxTextExtent];

  FILE
    *file;

  float
    dx_resolution,
    dy_resolution,
    lower_x,
    lower_y,
    upper_x,
    upper_y,
    x_resolution,
    y_resolution;

  Image
    *image,
    *next_image;

  ImageInfo
    local_info;

  int
    count,
    status;

  long int
    filesize;

  register char
    *p;

  register int
    c,
    i;

  unsigned int
    height,
    width;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Open temporary output file.
  */
  TemporaryFilename(postscript_filename);
  file=fopen(postscript_filename,WriteBinaryType);
  if (file == (FILE *) NULL)
    PrematureExit("Unable to write file",image);
  /*
    Set the page geometry.
  */
  *options='\0';
  if (image_info->page != (char *) NULL)
    {
      (void) strcat(options," -g");
      (void) strcat(options,image_info->page);
    }
  else
    for (p=command; ; )
    {
      c=fgetc(image->file);
      if (c == EOF)
        break;
      (void) fputc(c,file);
      *p++=c;
      if ((c != '\n') && (c != '\r') && ((p-command) < (MaxTextExtent-1)))
        continue;
      *p='\0';
      p=command;
      /*
        Continue unless this is a MediaBox statement.
      */
      if (strncmp(MediaBox,command,Extent(MediaBox)) != 0)
        continue;
      count=sscanf(command,"/MediaBox [ %f %f %f %f",&lower_x,&lower_y,
        &upper_x,&upper_y);
      if (count != 4)
        continue;
      if ((lower_x > upper_x) || (lower_y > upper_y))
        continue;
      /*
        Determine bounding box.
      */
      dx_resolution=72.0;
      dy_resolution=72.0;
      x_resolution=72.0;
      (void) strcpy(density,PSDensityGeometry);
      count=sscanf(density,"%fx%f",&x_resolution,&y_resolution);
      if (image_info->density != (char *) NULL)
        count=sscanf(image_info->density,"%fx%f",&x_resolution,&y_resolution);
      if (count != 2)
        y_resolution=x_resolution;
      if (image_info->page != (char *) NULL)
        continue;
      /*
        Set Postscript render geometry.
      */
      width=(unsigned int) (upper_x-lower_x+1);
      if ((float) ((int) upper_x) != upper_x)
        width++;
      height=(unsigned int) (upper_y-lower_y+1);
      if ((float) ((int) upper_y) != upper_y)
        height++;
      (void) sprintf(options,"-g%ux%u",
        (unsigned int) (((width*x_resolution)/dx_resolution)+0.5),
        (unsigned int) (((height*y_resolution)/dy_resolution)+0.5));
      break;
    }
  for ( ; ; )
  {
    c=fgetc(image->file);
    if (c == EOF)
      break;
    (void) fputc(c,file);
  }
  if (ferror(file))
    {
      Warning("An error has occurred writing to file",postscript_filename);
      (void) fclose(file);
      return((Image *) NULL);
    }
  (void) fclose(file);
  CloseImage(image);
  filesize=image->filesize;
  DestroyImage(image);
  /*
    Determine if the density options is specified.
  */
  (void) strcat(options," -r");
  if (image_info->density == (char *) NULL)
    (void) strcat(options,PSDensityGeometry);
  else
    (void) strcat(options,image_info->density);
  if (image_info->subrange != 0)
    {
      (void) sprintf(options,"%s -dFirstPage=%u",options,image_info->subimage);
      (void) sprintf(options,"%s -dLastPage=%u",options,image_info->subimage+
        image_info->subrange-1);
    }
  /*
    Use Ghostscript to convert Postscript image.
  */
  device=PostscriptColorDevice;
  if (image_info->monochrome)
    device=PostscriptMonoDevice;
  (void) strcpy(filename,image_info->filename);
  for (i=0; i < 50; i++)
  {
    /*
      Ghostscript eats % characters.
    */
    TemporaryFilename(image_info->filename);
    if (strchr(image_info->filename,'%') == (char *) NULL)
      break;
  }
  (void) sprintf(command,PostscriptCommand,device,options,image_info->filename,
    postscript_filename);
  status=SystemCommand(command);
  if (status)
    {
      /*
        Pre GS 3.51 does not support the pnmraw device.
      */
      (void) sprintf(command,PostscriptCommand,"ppmraw",options,
        image_info->filename,postscript_filename);
      status=SystemCommand(command);
    }
  if (status)
    {
      Warning("Portable Document translation failed",image_info->filename);
      (void) remove(postscript_filename);
      return((Image *) NULL);
    }
  local_info=(*image_info);
  image=ReadImage(&local_info);
  (void) remove(postscript_filename);
  (void) remove(image_info->filename);
  if (image == (Image *) NULL)
    {
      Warning("Portable Document translation failed",image_info->filename);
      return((Image *) NULL);
    }
  do
  {
    (void) strcpy(image->filename,filename);
    image->filesize=filesize;
    next_image=image->next;
    if (next_image != (Image *) NULL)
      image=next_image;
  } while (next_image != (Image *) NULL);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d P I C T I m a g e                                                  %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadPICTImage reads an Apple Macintosh QuickDraw/PICT image file
%  and returns it.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  The format of the ReadPICTImage routine is:
%
%      image=ReadPICTImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadPICTImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadPICTImage(const ImageInfo *image_info)
{
  char
    filename[MaxTextExtent];

  Image
    *image,
    *next_image,
    *proxy_image;

  /*
    Allocate image structure.
  */
  proxy_image=AllocateImage(image_info);
  if (proxy_image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,proxy_image,ReadBinaryType);
  if (proxy_image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",proxy_image);
  CloseImage(proxy_image);
  DestroyImage(proxy_image);
  /*
    Use picttoppm to convert Macintosh PICT image.
  */
  (void) strcpy(filename,image_info->filename);
  (void) sprintf(image_info->filename,PICTCommand,filename);
  image=ReadPNMImage(image_info);
  if (image == (Image *) NULL)
    {
      Warning("PICT translation failed",image_info->filename);
      return((Image *) NULL);
    }
  /*
    Assign proper filename.
  */
  do
  {
    (void) strcpy(image->filename,filename);
    next_image=image->next;
    if (next_image != (Image *) NULL)
      image=next_image;
  } while (next_image != (Image *) NULL);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d P L A S M A I m a g e                                              %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadPLASMAImage creates a plasma fractal image.  The image is
%  initialized to to the X server color as specified by the filename.
%
%  The format of the ReadPLASMAImage routine is:
%
%      image=ReadPLASMAImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadPLASMAImage returns a pointer to the image after
%      creating it. A null image is returned if there is a a memory shortage
%      or if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadPLASMAImage(const ImageInfo *image_info)
{
#define PlasmaImageText  "  Applying image plasma...  "
#define PlasmaPixel(x,y) \
{ \
  p=PixelOffset(x,y); \
  p->red=(Quantum) (rand() % (MaxRGB+1)); \
  p->green=(Quantum) (rand() % (MaxRGB+1)); \
  p->blue=(Quantum) (rand() % (MaxRGB+1)); \
}

  Image
    *image;

  register int
    i;

  SegmentInfo
    segment_info;

  unsigned int
    depth,
    max_depth;

  /*
    Recursively apply plasma to the image.
  */
  image=ReadGRADATIONImage(image_info);
  if (image == (Image *) NULL)
    return(image);
  if (!UncompressImage(image))
    return(image);
  image->class=DirectClass;
  for (i=0; i < image->packets; i++)
    image->pixels[i].index=(Opaque-Transparent) >> 1;
  segment_info.x1=0;
  segment_info.y1=0;
  segment_info.x2=image->columns-1;
  segment_info.y2=image->rows-1;
  srand(time(0));
  if (strcmp(image_info->filename,"fractal") == 0)
    {
      register RunlengthPacket
        *p;

      /*
        Seed pixels before recursion.
      */
      PlasmaPixel(segment_info.x1,segment_info.y1);
      PlasmaPixel(segment_info.x1,(segment_info.y1+segment_info.y2)/2);
      PlasmaPixel(segment_info.x1,segment_info.y2);
      PlasmaPixel((segment_info.x1+segment_info.x2)/2,segment_info.y1);
      PlasmaPixel((segment_info.x1+segment_info.x2)/2,
        (segment_info.y1+segment_info.y2)/2);
      PlasmaPixel((segment_info.x1+segment_info.x2)/2,segment_info.y2);
      PlasmaPixel(segment_info.x2,segment_info.y1);
      PlasmaPixel(segment_info.x2,(segment_info.y1+segment_info.y2)/2);
      PlasmaPixel(segment_info.x2,segment_info.y2);
    }
  i=Max(image->columns,image->rows) >> 1;
  for (max_depth=0; i != 0; max_depth++)
    i>>=1;
  for (depth=1; ; depth++)
  {
    ProgressMonitor(PlasmaImageText,depth,max_depth);
    if (PlasmaImage(image,&segment_info,0,depth))
      break;
  }
  return(image);
}

#ifdef HasPNG
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d P N G I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadPNGImage reads a Portable Network Graphics image file and
%  returns it.  It allocates the memory necessary for the new Image structure
%  and returns a pointer to the new image.
%
%  The format of the ReadPNGImage routine is:
%
%      image=ReadPNGImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadPNGImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/

static void PNGError(png_struct *ping,png_const_charp message)
{
  Warning(message,(char *) NULL);
  longjmp(ping->jmpbuf,1);
}

static void PNGWarning(png_struct *ping,png_const_charp message)
{
  Warning(message,(char *) NULL);
}

static Image *ReadPNGImage(const ImageInfo *image_info)
{
#define PNGTextChunk(i,value) \
{ \
  length=ping_info->text[i].text_length; \
  if (value != (char *) NULL) \
    value=(char *) realloc((char *) value,strlen(value)+length+1); \
  else \
    { \
      value=(char *) malloc(length+1); \
      if (value != (char *) NULL) \
        *value='\0'; \
    } \
  if (value == (char *) NULL) \
    PrematureExit("Unable to allocate memory",image); \
  (void) strncat(value,ping_info->text[i].text,length); \
  value[length]='\0'; \
}

  ColorPacket
    transparent_color;

  Image
    *image;

  register int
    i,
    x,
    y;

  register unsigned char
    *p;

  register RunlengthPacket
    *q;

  png_info
    *end_info,
    *ping_info;

  png_struct
    *ping;

  unsigned char
    *png_pixels,
    **scanlines;

  unsigned int
    length,
    packets;

  unsigned short
    index,
    value;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Allocate the PNG structures
  */
  ping=png_create_read_struct(PNG_LIBPNG_VER_STRING,(void *) NULL,PNGError,
    PNGWarning);
  if (ping == (png_struct *) NULL)
    PrematureExit("Unable to allocate memory",image);
  ping_info=png_create_info_struct(ping);
  if (ping_info == (png_info *) NULL)
    {
      png_destroy_read_struct(&ping,(png_info **) NULL,(png_info **) NULL);
      PrematureExit("Unable to allocate memory",image);
    }
  end_info=png_create_info_struct(ping);
  if (end_info == (png_info *) NULL)
    {
      png_destroy_read_struct(&ping,&ping_info,(png_info **) NULL);
      PrematureExit("Unable to allocate memory",image);
    }
  image->pixels=(RunlengthPacket *) NULL;
  png_pixels=(unsigned char *) NULL;
  scanlines=(unsigned char **) NULL;
  if (setjmp(ping->jmpbuf))
    {
      /*
        PNG image is corrupt.
      */
      png_destroy_read_struct(&ping,&ping_info,&end_info);
      if (scanlines != (unsigned char **) NULL)
        free((char *) scanlines);
      if (png_pixels != (unsigned char *) NULL)
        free((char *) png_pixels);
      CloseImage(image);
      if ((image->columns == 0) || (image->rows == 0))
        {
          DestroyImage(image);
          return((Image *) NULL);
        }
      return(image);
    }
  /*
    Prepare PNG for reading.
  */
  png_init_io(ping,image->file);
  png_read_info(ping,ping_info);
  image->depth=ping_info->bit_depth;
  if (ping_info->bit_depth < 8)
    {
      if ((ping_info->color_type != PNG_COLOR_TYPE_PALETTE) &&
          (ping_info->color_type != PNG_COLOR_TYPE_GRAY))
        png_set_packing(ping);
      image->depth=8;
    }
  if (ping_info->valid & PNG_INFO_gAMA)
    image->gamma=ping_info->gamma;
  if (ping_info->valid & PNG_INFO_pHYs)
    {
      /*
        Set image resolution.
      */
      image->x_resolution=ping_info->x_pixels_per_unit;
      image->y_resolution=ping_info->y_pixels_per_unit;
      if (ping_info->phys_unit_type == PNG_RESOLUTION_METER)
        {
          image->units=PixelsPerCentimeterResolution;
          image->x_resolution=ping_info->x_pixels_per_unit/100.0;
          image->y_resolution=ping_info->y_pixels_per_unit/100.0;
        }
    }
  if (ping_info->valid & PNG_INFO_bKGD)
    {
      /*
        Set image background color.
      */
      image->background_color.red=ping_info->background.red;
      image->background_color.green=ping_info->background.green;
      image->background_color.blue=ping_info->background.blue;
      if (ping_info->bit_depth > QuantumDepth)
        {
          image->background_color.red=XDownScale(ping_info->background.red);
          image->background_color.green=XDownScale(ping_info->background.green);
          image->background_color.blue=XDownScale(ping_info->background.blue);
        }
    }
  if (ping_info->valid & PNG_INFO_tRNS)
    {
      /*
        Image has a transparent background.
      */
      transparent_color.red=ping_info->trans_values.red;
      transparent_color.green=ping_info->trans_values.green;
      transparent_color.blue=ping_info->trans_values.blue;
      transparent_color.index=ping_info->trans_values.gray;
      if (ping_info->bit_depth > QuantumDepth)
        {
          transparent_color.red=XDownScale(ping_info->trans_values.red);
          transparent_color.green=XDownScale(ping_info->trans_values.green);
          transparent_color.blue=XDownScale(ping_info->trans_values.blue);
          transparent_color.index=XDownScale(ping_info->trans_values.gray);
        }
    }
  png_read_update_info(ping,ping_info);
  /*
    Initialize image structure.
  */
  image->columns=(unsigned int) ping_info->width;
  image->rows=(unsigned int) ping_info->height;
  image->packets=0;
  packets=Max((image->columns*image->rows+4) >> 3,1);
  if (ping_info->bit_depth == 1)
    packets=Max((image->columns*image->rows+8) >> 4,1);
  image->pixels=(RunlengthPacket *) malloc(packets*sizeof(RunlengthPacket));
  png_pixels=(unsigned char *)
    malloc(ping_info->rowbytes*image->rows*sizeof(Quantum));
  scanlines=(unsigned char **) malloc(image->rows*sizeof(unsigned char *));
  if ((image->pixels == (RunlengthPacket *) NULL) ||
      (png_pixels == (unsigned char *) NULL) ||
      (scanlines == (unsigned char **) NULL))
    PrematureExit("Unable to allocate memory",image);
  if ((ping_info->color_type == PNG_COLOR_TYPE_PALETTE) ||
      (ping_info->color_type == PNG_COLOR_TYPE_GRAY))
    {
      /*
        Initialize image colormap.
      */
      image->class=PseudoClass;
      image->colors=1 << Min(ping_info->bit_depth,QuantumDepth);
      if (ping_info->color_type == PNG_COLOR_TYPE_PALETTE)
        image->colors=ping_info->num_palette;
      image->colormap=(ColorPacket *) malloc(image->colors*sizeof(ColorPacket));
      if (image->colormap == (ColorPacket *) NULL)
        PrematureExit("Unable to allocate memory",image);
      for (i=0; i < image->colors; i++)
      {
        image->colormap[i].red=(MaxRGB*i)/Max(image->colors-1,1);
        image->colormap[i].green=(MaxRGB*i)/Max(image->colors-1,1);
        image->colormap[i].blue=(MaxRGB*i)/Max(image->colors-1,1);
      }
      if (ping_info->color_type == PNG_COLOR_TYPE_PALETTE)
        for (i=0; i < image->colors; i++)
        {
          image->colormap[i].red=UpScale(ping_info->palette[i].red);
          image->colormap[i].green=UpScale(ping_info->palette[i].green);
          image->colormap[i].blue=UpScale(ping_info->palette[i].blue);
        }
    }
  /*
    Read image scanlines.
  */
  for (i=0; i < image->rows; i++)
    scanlines[i]=png_pixels+(i*ping_info->rowbytes);
  png_read_image(ping,scanlines);
  png_read_end(ping,ping_info);
  /*
    Convert PNG pixels to runlength-encoded packets.
  */
  q=image->pixels;
  q->length=MaxRunlength;
  if (image->class == DirectClass)
    {
      Quantum
        blue,
        green,
        red;

      /*
        Convert image to DirectClass runlength-encoded packets.
      */
      if ((ping_info->color_type == PNG_COLOR_TYPE_RGB_ALPHA) ||
          (ping_info->color_type == PNG_COLOR_TYPE_GRAY_ALPHA) ||
          (ping_info->valid & PNG_INFO_tRNS))
        image->matte=True;
      for (y=0; y < image->rows; y++)
      {
        p=scanlines[y];
        for (x=0; x < image->columns; x++)
        {
          ReadQuantum(red,p);
          green=red;
          blue=red;
          if (ping_info->color_type != PNG_COLOR_TYPE_GRAY_ALPHA)
            {
              ReadQuantum(green,p);
              ReadQuantum(blue,p);
            }
          index=Opaque;
          if ((ping_info->color_type == PNG_COLOR_TYPE_RGB_ALPHA) ||
              (ping_info->color_type == PNG_COLOR_TYPE_GRAY_ALPHA))
            ReadQuantum(index,p);
          if (ping_info->valid & PNG_INFO_tRNS)
            if ((red == transparent_color.red) &&
                (green == transparent_color.green) &&
                (blue == transparent_color.blue))
              index=Transparent;
          if ((red == q->red) && (green == q->green) && (blue == q->blue) &&
              (index == q->index) && ((int) q->length < MaxRunlength))
            q->length++;
          else
            {
              if (image->packets != 0)
                q++;
              image->packets++;
              if (image->packets == packets)
                {
                  packets<<=1;
                  image->pixels=(RunlengthPacket *) realloc((char *)
                    image->pixels,packets*sizeof(RunlengthPacket));
                  if (image->pixels == (RunlengthPacket *) NULL)
                    {
                      free((char *) png_pixels);
                      PrematureExit("Unable to allocate memory",image);
                    }
                  q=image->pixels+image->packets-1;
                }
              q->red=red;
              q->green=green;
              q->blue=blue;
              q->index=index;
              q->length=0;
            }
        }
        ProgressMonitor(LoadImageText,y,image->rows);
      }
    }
  else
    {
      Quantum
        *quantum_scanline;

      register Quantum
        *r;

      /*
        Convert image to PseudoClass runlength-encoded packets.
      */
      quantum_scanline=(Quantum *) malloc(image->columns*sizeof(Quantum));
      if (quantum_scanline == (Quantum *) NULL)
        PrematureExit("Unable to allocate memory",image);
      for (y=0; y < image->rows; y++)
      {
        p=scanlines[y];
        r=quantum_scanline;
        switch (ping_info->bit_depth)
        {
          case 1:
          {
            register int
              bit;

            for (x=0; x < ((int) image->columns-7); x+=8)
            {
              for (bit=7; bit >= 0; bit--)
                *r++=((*p) & (0x01 << bit) ? 0x01 : 0x00);
              p++;
            }
            if ((image->columns % 8) != 0)
              {
                for (bit=7; bit >= (8-(image->columns % 8)); bit--)
                  *r++=((*p) & (0x01 << bit) ? 0x01 : 0x00);
                p++;
              }
            break;
          }
          case 2:
          {
            for (x=0; x < ((int) image->columns-3); x+=4)
            {
              *r++=(*p >> 6) & 0x3;
              *r++=(*p >> 4) & 0x3;
              *r++=(*p >> 2) & 0x3;
              *r++=(*p) & 0x3;
              p++;
            }
            if ((image->columns % 4) != 0)
              {
                for (i=3; i >= (4-(image->columns % 4)); i--)
                  *r++=(*p >> (i*2)) & 0x03;
                p++;
              }
            break;
          }
          case 4:
          {
            for (x=0; x < ((int) image->columns-1); x+=2)
            {
              *r++=(*p >> 4) & 0xf;
              *r++=(*p) & 0xf;
              p++;
            }
            if ((image->columns % 2) != 0)
              *r++=(*p++ >> 4) & 0xf;
            break;
          }
          case 8:
          {
            for (x=0; x < image->columns; x++)
              *r++=(*p++);
            break;
          }
          case 16:
          {
            for (x=0; x < image->columns; x++)
            {
              ReadQuantum(*r,p);
              r++;
            }
            break;
          }
          default:
            break;
        }
        /*
          Transfer image scanline.
        */
        r=quantum_scanline;
        for (x=0; x < image->columns; x++)
        {
          index=(*r++);
          if ((index == q->index) && ((int) q->length < MaxRunlength))
            q->length++;
          else
            {
              if (image->packets != 0)
                q++;
              image->packets++;
              if (image->packets == packets)
                {
                  packets<<=1;
                  image->pixels=(RunlengthPacket *) realloc((char *)
                    image->pixels,packets*sizeof(RunlengthPacket));
                  if (image->pixels == (RunlengthPacket *) NULL)
                    {
                      free((char *) quantum_scanline);
                      PrematureExit("Unable to allocate memory",image);
                    }
                  q=image->pixels+image->packets-1;
                }
              q->index=index;
              q->length=0;
            }
        }
        ProgressMonitor(LoadImageText,y,image->rows);
      }
      if (image->class == PseudoClass)
        SyncImage(image);
      free((char *) quantum_scanline);
      if (ping_info->valid & PNG_INFO_tRNS)
        {
          /*
            Image has a transparent background.
          */
          image->class=DirectClass;
          image->matte=True;
          q=image->pixels;
          for (i=0; i < image->packets; i++)
          {
            index=q->index;
            q->index=Opaque;
            if (ping_info->color_type == PNG_COLOR_TYPE_PALETTE)
              {
                if (index < ping_info->num_trans)
                  q->index=UpScale(ping_info->trans[index]);
              }
            else
              if (index == transparent_color.index)
                q->index=Transparent;
            q++;
          }
        }
    }
  image->pixels=(RunlengthPacket *)
    realloc((char *) image->pixels,image->packets*sizeof(RunlengthPacket));
  if (ping_info->num_text > 0)
    for (i=0; i < ping_info->num_text; i++)
    {
      if (strcmp(ping_info->text[i].key,"Comment") == 0)
        PNGTextChunk(i,image->comments);
      if (strcmp(ping_info->text[i].key,"Delay") == 0)
        {
          char
            *delay;

          delay=(char *) NULL;
          PNGTextChunk(i,delay);
          image->delay=atoi(delay);
          free(delay);
        }
      if (strcmp(ping_info->text[i].key,"Description") == 0)
        PNGTextChunk(i,image->comments);
      if (strcmp(ping_info->text[i].key,"Directory") == 0)
        PNGTextChunk(i,image->directory);
      if (strcmp(ping_info->text[i].key,"Label") == 0)
        PNGTextChunk(i,image->label);
      if (strcmp(ping_info->text[i].key,"Montage") == 0)
        PNGTextChunk(i,image->montage);
      if (strcmp(ping_info->text[i].key,"Scene") == 0)
        {
          char
            *scene;

          scene=(char *) NULL;
          PNGTextChunk(i,scene);
          image->scene=atoi(scene);
          free(scene);
        }
      if (strcmp(ping_info->text[i].key,"Signature") == 0)
        PNGTextChunk(i,image->signature);
      if (strcmp(ping_info->text[i].key,"Title") == 0)
        PNGTextChunk(i,image->label);
    }
  /*
    Free memory.
  */
  png_destroy_read_struct(&ping,&ping_info,&end_info);
  free((char *) png_pixels);
  CloseImage(image);
  return(image);
}
#else
static Image *ReadPNGImage(const ImageInfo *image_info)
{
  Warning("PNG library is not available",image_info->filename);
  return(ReadMIFFImage(image_info));
}
#endif

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d P N M I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadPNMImage reads a Portable Anymap image file and returns it.
%  It allocates the memory necessary for the new Image structure and returns
%  a pointer to the new image.
%
%  The format of the ReadPNMImage routine is:
%
%      image=ReadPNMImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadPNMImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/

static unsigned int PNMInteger(Image *image,const unsigned int base)
{
  int
    c;

  unsigned int
    value;

  /*
    Skip any leading whitespace.
  */
  do
  {
    c=fgetc(image->file);
    if (c == EOF)
      return(0);
    if (c == '#')
      {
        register char
          *p;

        unsigned int
          length;

        /*
          Read comment.
        */
        if (image->comments != (char *) NULL)
          {
            length=Extent(image->comments);
            p=image->comments+length;
          }
        else
          {
            length=MaxTextExtent;
            image->comments=(char *) malloc(length*sizeof(char));
            p=image->comments;
          }
        for ( ; image->comments != (char *) NULL; p++)
        {
          if ((p-image->comments+2) >= length)
            {
              *p='\0';
              length<<=1;
              image->comments=(char *)
                realloc((char *) image->comments,length*sizeof(char));
              if (image->comments == (char *) NULL)
                break;
              p=image->comments+Extent(image->comments);
            }
          c=fgetc(image->file);
          if ((c == EOF) || (c == '\n'))
            break;
          *p=(unsigned char) c;
        }
        if (image->comments == (char *) NULL)
          {
            Warning("Memory allocation error",(char *) NULL);
            return(0);
          }
        *p++='\n';
        *p='\0';
      }
  } while (!isdigit(c));
  if (base == 2)
    return(c-'0');
  /*
    Evaluate number.
  */
  value=0;
  do
  {
    value*=10;
    value+=c-'0';
    c=fgetc(image->file);
    if (c == EOF)
      return(0);
  }
  while (isdigit(c));
  return(value);
}

static Image *ReadPNMImage(const ImageInfo *image_info)
{
#define MaxRawValue  255

  char
    format;

  Image
    *image;

  int
    y;

  Quantum
    *scale;

  register int
    i,
    x;

  register RunlengthPacket
    *q;

  unsigned int
    max_value,
    packets,
    status;

  unsigned short
    blue,
    green,
    index,
    red;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,"r");
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Read PNM image.
  */
  status=ReadData((char *) &format,1,1,image->file);
  do
  {
    /*
      Verify PNM identifier.
    */
    if ((status == False) || (format != 'P'))
      PrematureExit("Not a PNM image file",image);
    /*
      Initialize image structure.
    */
    format=fgetc(image->file);
    if (format == '7')
      (void) PNMInteger(image,10);
    image->columns=PNMInteger(image,10);
    image->rows=PNMInteger(image,10);
    if ((image->columns*image->rows) == 0)
      PrematureExit("Unable to read image: image dimensions are zero",image);
    image->packets=0;
    packets=Max((image->columns*image->rows+4) >> 3,1);
    if ((format == '1') || (format == '4'))
      packets=Max((image->columns*image->rows+8) >> 4,1);
    image->pixels=(RunlengthPacket *) malloc(packets*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      PrematureExit("Unable to allocate memory",image);
    if ((format == '1') || (format == '4'))
      max_value=1;  /* bitmap */
    else
      max_value=PNMInteger(image,10);
    scale=(Quantum *) NULL;
    if ((format != '3') && (format != '6'))
      {
        /*
          Create colormap.
        */
        image->class=PseudoClass;
        image->colors=Min(max_value,MaxRGB)+1;
        image->colormap=(ColorPacket *)
          malloc(image->colors*sizeof(ColorPacket));
        if (image->colormap == (ColorPacket *) NULL)
          PrematureExit("Unable to allocate memory",image);
        if (format != '7')
          for (i=0; i < image->colors; i++)
          {
            image->colormap[i].red=(MaxRGB*i)/(image->colors-1);
            image->colormap[i].green=(MaxRGB*i)/(image->colors-1);
            image->colormap[i].blue=(MaxRGB*i)/(image->colors-1);
          }
        else
          {
            /*
              Initialize 332 colormap.
            */
            i=0;
            for (red=0; red < 8; red++)
              for (green=0; green < 8; green++)
                for (blue=0; blue < 4; blue++)
                {
                  image->colormap[i].red=(Quantum) (red*MaxRGB)/7;
                  image->colormap[i].green=(Quantum) (green*MaxRGB)/7;
                  image->colormap[i].blue=(Quantum) (blue*MaxRGB)/3;
                  i++;
                }
          }
      }
    if (max_value != MaxRGB)
      {
        /*
          Compute pixel scaling table.
        */
        scale=(Quantum *) malloc((max_value+1)*sizeof(Quantum));
        if (scale == (Quantum *) NULL)
          PrematureExit("Unable to allocate memory",image);
        for (i=0; i <= max_value; i++)
          scale[i]=(Quantum) ((i*MaxRGB+(max_value >> 1))/max_value);
      }
    /*
      Convert PNM pixels to runlength-encoded MIFF packets.
    */
    q=image->pixels;
    q->length=MaxRunlength;
    switch (format)
    {
      case '1':
      {
        /*
          Convert PBM image to runlength-encoded packets.
        */
        for (y=0; y < image->rows; y++)
        {
          for (x=0; x < image->columns; x++)
          {
            index=!PNMInteger(image,2);
            if ((index == q->index) && ((int) q->length < MaxRunlength))
              q->length++;
            else
              {
                if (image->packets != 0)
                  q++;
                image->packets++;
                if (image->packets == packets)
                  {
                    packets<<=1;
                    image->pixels=(RunlengthPacket *) realloc((char *)
                      image->pixels,packets*sizeof(RunlengthPacket));
                    if (image->pixels == (RunlengthPacket *) NULL)
                      PrematureExit("Unable to allocate memory",image);
                    q=image->pixels+image->packets-1;
                  }
                q->index=index;
                q->length=0;
              }
          }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
        break;
      }
      case '2':
      {
        /*
          Convert PGM image to runlength-encoded packets.
        */
        for (y=0; y < image->rows; y++)
        {
          for (x=0; x < image->columns; x++)
          {
            index=PNMInteger(image,10);
            if (scale != (Quantum *) NULL)
              index=scale[index];
            if ((index == q->index) && ((int) q->length < MaxRunlength))
              q->length++;
            else
              {
                if (image->packets != 0)
                  q++;
                image->packets++;
                if (image->packets == packets)
                  {
                    packets<<=1;
                    image->pixels=(RunlengthPacket *) realloc((char *)
                      image->pixels,packets*sizeof(RunlengthPacket));
                    if (image->pixels == (RunlengthPacket *) NULL)
                      PrematureExit("Unable to allocate memory",image);
                    q=image->pixels+image->packets-1;
                  }
                q->index=index;
                q->length=0;
              }
          }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
        break;
      }
      case '3':
      {
        /*
          Convert PNM image to runlength-encoded packets.
        */
        for (y=0; y < image->rows; y++)
        {
          for (x=0; x < image->columns; x++)
          {
            red=PNMInteger(image,10);
            green=PNMInteger(image,10);
            blue=PNMInteger(image,10);
            if (scale != (Quantum *) NULL)
              {
                red=scale[red];
                green=scale[green];
                blue=scale[blue];
              }
            if ((red == q->red) && (green == q->green) && (blue == q->blue) &&
                ((int) q->length < MaxRunlength))
              q->length++;
            else
              {
                if (image->packets != 0)
                  q++;
                image->packets++;
                if (image->packets == packets)
                  {
                    packets<<=1;
                    image->pixels=(RunlengthPacket *) realloc((char *)
                      image->pixels,packets*sizeof(RunlengthPacket));
                    if (image->pixels == (RunlengthPacket *) NULL)
                      PrematureExit("Unable to allocate memory",image);
                    q=image->pixels+image->packets-1;
                  }
                q->red=red;
                q->green=green;
                q->blue=blue;
                q->index=0;
                q->length=0;
            }
          }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
        (void) IsPseudoClass(image);
        break;
      }
      case '4':
      {
        unsigned char
          bit,
          byte;

        unsigned int
          x,
          y;

        /*
          Convert PBM raw image to runlength-encoded packets.
        */
        for (y=0; y < image->rows; y++)
        {
          bit=0;
          byte=0;
          for (x=0; x < image->columns; x++)
          {
            if (bit == 0)
              byte=fgetc(image->file);
            index=(byte & 0x80) ? 0 : 1;
            if ((index == q->index) && ((int) q->length < MaxRunlength))
              q->length++;
            else
              {
                if (image->packets != 0)
                  q++;
                image->packets++;
                if (image->packets == packets)
                  {
                    packets<<=1;
                    image->pixels=(RunlengthPacket *) realloc((char *)
                      image->pixels,packets*sizeof(RunlengthPacket));
                    if (image->pixels == (RunlengthPacket *) NULL)
                      PrematureExit("Unable to allocate memory",image);
                    q=image->pixels+image->packets-1;
                  }
                q->index=index;
                q->length=0;
              }
            bit++;
            if (bit == 8)
              bit=0;
            byte<<=1;
          }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
        break;
      }
      case '5':
      case '7':
      {
        /*
          Convert PGM raw image to runlength-encoded packets.
        */
        for (y=0; y < image->rows; y++)
        {
          for (x=0; x < image->columns; x++)
          {
            if (max_value <= MaxRawValue)
              index=fgetc(image->file);
            else
              index=LSBFirstReadShort(image->file);
            if ((index == q->index) && ((int) q->length < MaxRunlength))
              q->length++;
            else
              {
                if (image->packets != 0)
                  q++;
                image->packets++;
                if (image->packets == packets)
                  {
                    packets<<=1;
                    image->pixels=(RunlengthPacket *) realloc((char *)
                      image->pixels,packets*sizeof(RunlengthPacket));
                    if (image->pixels == (RunlengthPacket *) NULL)
                      PrematureExit("Unable to allocate memory",image);
                    q=image->pixels+image->packets-1;
                  }
                q->index=index;
                q->length=0;
              }
          }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
        break;
      }
      case '6':
      {
        /*
          Convert PNM raster image to runlength-encoded packets.
        */
        for (y=0; y < image->rows; y++)
        {
          for (x=0; x < image->columns; x++)
          {
            if (max_value <= MaxRawValue)
              {
                red=fgetc(image->file);
                green=fgetc(image->file);
                blue=fgetc(image->file);
              }
            else
              {
                red=LSBFirstReadShort(image->file);
                green=LSBFirstReadShort(image->file);
                blue=LSBFirstReadShort(image->file);
              }
            if (scale != (Quantum *) NULL)
              {
                red=scale[red];
                green=scale[green];
                blue=scale[blue];
              }
            if ((red == q->red) && (green == q->green) && (blue == q->blue) &&
                ((int) q->length < MaxRunlength))
              q->length++;
            else
              {
                if (image->packets != 0)
                  q++;
                image->packets++;
                if (image->packets == packets)
                  {
                    packets<<=1;
                    image->pixels=(RunlengthPacket *) realloc((char *)
                      image->pixels,packets*sizeof(RunlengthPacket));
                    if (image->pixels == (RunlengthPacket *) NULL)
                      PrematureExit("Unable to allocate memory",image);
                      q=image->pixels+image->packets-1;
                  }
                q->red=red;
                q->green=green;
                q->blue=blue;
                q->index=0;
                q->length=0;
              }
          }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
        (void) IsPseudoClass(image);
        break;
      }
      default:
        PrematureExit("Not a PNM image file",image);
    }
    if (scale != (Quantum *) NULL)
      free((char *) scale);
    if (image->class == PseudoClass)
      {
        SyncImage(image);
        CompressColormap(image);
      }
    image->pixels=(RunlengthPacket *)
      realloc((char *) image->pixels,image->packets*sizeof(RunlengthPacket));
    /*
      Proceed to next image.
    */
    if ((format == '1') || (format == '2') || (format == '3'))
      do
      {
        /*
          Skip to end of line.
        */
        status=ReadData(&format,1,1,image->file);
        if (status == False)
          break;
      } while (format != '\n');
    status=ReadData((char *) &format,1,1,image->file);
    if ((status == True) && (format == 'P'))
      {
        /*
          Allocate image structure.
        */
        image->next=AllocateImage(image_info);
        if (image->next == (Image *) NULL)
          {
            DestroyImages(image);
            return((Image *) NULL);
          }
        (void) strcpy(image->next->filename,image_info->filename);
        image->next->file=image->file;
        image->next->scene=image->scene+1;
        image->next->previous=image;
        image=image->next;
      }
  } while ((status == True) && (format == 'P'));
  while (image->previous != (Image *) NULL)
    image=image->previous;
  CloseImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d P S I m a g e                                                      %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadPSImage reads a Adobe Postscript image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadPSImage routine is:
%
%      image=ReadPSImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadPSImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadPSImage(const ImageInfo *image_info)
{
#define BoundingBox  "%%BoundingBox:"
#define DocumentMedia  "%%DocumentMedia:"
#define PageBoundingBox  "%%PageBoundingBox:"

  char
    command[MaxTextExtent],
    density[MaxTextExtent],
    *device,
    filename[MaxTextExtent],
    options[MaxTextExtent],
    postscript_filename[MaxTextExtent],
    translate_geometry[MaxTextExtent];

  FILE
    *file;

  float
    dx_resolution,
    dy_resolution,
    lower_x,
    lower_y,
    upper_x,
    upper_y,
    x_resolution,
    y_resolution;

  Image
    *image,
    *next_image;

  ImageInfo
    local_info;

  int
    c,
    count,
    status;

  long int
    filesize;

  register char
    *p;

  register int
    i;

  unsigned int
    height,
    width;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Open temporary output file.
  */
  TemporaryFilename(postscript_filename);
  file=fopen(postscript_filename,WriteBinaryType);
  if (file == (FILE *) NULL)
    PrematureExit("Unable to write file",image);
  (void) sprintf(translate_geometry,"%f %f translate\n              ",0.0,0.0);
  (void) fputs(translate_geometry,file);
  /*
    Set the page geometry.
  */
  lower_x=0;
  lower_y=0;
  dx_resolution=72.0;
  dy_resolution=72.0;
  x_resolution=72.0;
  (void) strcpy(density,PSDensityGeometry);
  count=sscanf(density,"%fx%f",&x_resolution,&y_resolution);
  if (image_info->density != (char *) NULL)
    count=sscanf(image_info->density,"%fx%f",&x_resolution,&y_resolution);
  if (count != 2)
    y_resolution=x_resolution;
  *options='\0';
  if (image_info->page != (char *) NULL)
    {
      int
        x,
        y;

      /*
        Set page geometry.
      */
      (void) XParseGeometry(PSPageGeometry,&x,&y,&width,&height);
      (void) XParseGeometry(image_info->page,&x,&y,&width,&height);
      (void) sprintf(options,"-g%ux%u",
        (unsigned int) (((width*x_resolution)/dx_resolution)+0.5),
        (unsigned int) (((height*y_resolution)/dy_resolution)+0.5));
    }
  else
    for (p=command; ; )
    {
      c=fgetc(image->file);
      if (c == EOF)
        break;
      (void) fputc(c,file);
      *p++=c;
      if ((c != '\n') && (c != '\r') && ((p-command) < (MaxTextExtent-1)))
        continue;
      *p='\0';
      p=command;
      /*
        Parse a bounding box statement.
      */
      count=0;
      if (strncmp(BoundingBox,command,Extent(BoundingBox)) == 0)
        count=sscanf(command,"%%%%BoundingBox: %f %f %f %f",&lower_x,&lower_y,
          &upper_x,&upper_y);
      if (strncmp(DocumentMedia,command,Extent(DocumentMedia)) == 0)
        count=
          sscanf(command,"%%%%DocumentMedia: %*s %f %f",&upper_x,&upper_y)+2;
      if (strncmp(PageBoundingBox,command,Extent(PageBoundingBox)) == 0)
        count=sscanf(command,"%%%%PageBoundingBox: %f %f %f %f",
          &lower_x,&lower_y,&upper_x,&upper_y);
      if (count != 4)
        continue;
      if ((lower_x > upper_x) || (lower_y > upper_y))
        continue;
      /*
        Set Postscript render geometry.
      */
      (void) sprintf(translate_geometry,"%f %f translate\n",-lower_x,-lower_y);
      width=(unsigned int) (upper_x-lower_x+1);
      if ((float) ((int) upper_x) != upper_x)
        width++;
      height=(unsigned int) (upper_y-lower_y+1);
      if ((float) ((int) upper_y) != upper_y)
        height++;
      (void) sprintf(options,"-g%ux%u",
        (unsigned int) (((width*x_resolution)/dx_resolution)+0.5),
        (unsigned int) (((height*y_resolution)/dy_resolution)+0.5));
      break;
    }
  for ( ; ; )
  {
    c=fgetc(image->file);
    if (c == EOF)
      break;
    (void) fputc(c,file);
  }
  if (ferror(file))
    {
      Warning("An error has occurred writing to file",postscript_filename);
      (void) fclose(file);
      return((Image *) NULL);
    }
  (void) fseek(file,0,0);
  (void) fputs(translate_geometry,file);
  (void) fclose(file);
  CloseImage(image);
  filesize=image->filesize;
  DestroyImage(image);
  /*
    Determine if density options is specified.
  */
  (void) strcat(options," -r");
  if (image_info->density == (char *) NULL)
    (void) strcat(options,PSDensityGeometry);
  else
    (void) strcat(options,image_info->density);
  /*
    Use Ghostscript to convert Postscript image.
  */
  device=PostscriptColorDevice;
  if (image_info->monochrome)
    device=PostscriptMonoDevice;
  (void) strcpy(filename,image_info->filename);
  for (i=0; i < 50; i++)
  {
    /*
      Ghostscript eats % characters.
    */
    TemporaryFilename(image_info->filename);
    if (strchr(image_info->filename,'%') == (char *) NULL)
      break;
  }
  (void) sprintf(command,PostscriptCommand,device,options,image_info->filename,
    postscript_filename);
  status=SystemCommand(command);
  if (status)
    {
      /*
        Pre GS 3.51 does not support the pnmraw device.
      */
      (void) sprintf(command,PostscriptCommand,"ppmraw",options,
        image_info->filename,postscript_filename);
      status=SystemCommand(command);
    }
  if (!IsAccessible(image_info->filename))
    {
      /*
        Ghostscript requires a showpage operator.
      */
      file=fopen(postscript_filename,"a");
      if (file == (FILE *) NULL)
        PrematureExit("Unable to write file",image);
      (void) fputs("showpage\n",file);
      (void) fclose(file);
      status=SystemCommand(command);
    }
  if (!IsAccessible(image_info->filename))
    {
      /*
        It's possible that the -g option caused a problem.
      */
      (void) strcpy(options,"-r");
      if (image_info->density == (char *) NULL)
        (void) strcat(options,PSDensityGeometry);
      else
        (void) strcat(options,image_info->density);
      (void) sprintf(command,PostscriptCommand,device,options,
        image_info->filename,postscript_filename);
      status=SystemCommand(command);
    }
  (void) remove(postscript_filename);
  if (status)
    {
      /*
        Ghostscript has failed-- try the Display Postscript Extension.
      */
      (void) strcpy(image_info->filename,filename);
      image=ReadDPSImage(image_info);
      if (image != (Image *) NULL)
        return(image);
      Warning("Postscript translation failed",image_info->filename);
      return((Image *) NULL);
    }
  local_info=(*image_info);
  image=ReadImage(&local_info);
  (void) remove(image_info->filename);
  if (image == (Image *) NULL)
    {
      Warning("Postscript translation failed",image_info->filename);
      return((Image *) NULL);
    }
  do
  {
    (void) strcpy(image->filename,filename);
    image->filesize=filesize;
    next_image=image->next;
    if (next_image != (Image *) NULL)
      image=next_image;
  } while (next_image != (Image *) NULL);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d R A D I A N C E I m a g e                                          %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadRADIANCEImage reads a RADIANCE image file and returns it.
%  It allocates the memory necessary for the new Image structure and returns
%  a pointer to the new image.
%
%  The format of the ReadRADIANCEImage routine is:
%
%      image=ReadRADIANCEImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadRADIANCEImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadRADIANCEImage(const ImageInfo *image_info)
{
  char
    command[MaxTextExtent],
    filename[MaxTextExtent];

  Image
    *image,
    *next_image;

  int
    status;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  CloseImage(image);
  /*
    Use ra_ppm to convert RADIANCE image.
  */
  (void) strcpy(filename,image_info->filename);
  TemporaryFilename(image_info->filename);
  (void) sprintf(command,"ra_ppm -g 1.0 %s %s",filename,image_info->filename);
  status=SystemCommand(command);
  if (status)
    PrematureExit("RADIANCE translation failed",image);
  DestroyImage(image);
  image=ReadPNMImage(image_info);
  (void) remove(image_info->filename);
  if (image == (Image *) NULL)
    PrematureExit("RADIANCE translation failed",image);
  /*
    Assign proper filename.
  */
  do
  {
    (void) strcpy(image->filename,filename);
    next_image=image->next;
    if (next_image != (Image *) NULL)
      image=next_image;
  } while (next_image != (Image *) NULL);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d R G B I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadRGBImage reads an image of raw red, green, and blue bytes and
%  returns it.  It allocates the memory necessary for the new Image structure
%  and returns a pointer to the new image.
%
%  The format of the ReadRGBImage routine is:
%
%      image=ReadRGBImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadRGBImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadRGBImage(const ImageInfo *image_info)
{
  Image
    *image;

  int
    count,
    x,
    x_offset,
    y,
    y_offset;

  register int
    i;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  unsigned char
    *scanline;

  unsigned int
    height,
    packet_size,
    width;

  unsigned short
    value;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Determine width and height, e.g. 640x512.
  */
  width=512;
  height=512;
  x_offset=0;
  y_offset=0;
  x=0;
  if (image_info->size != (char *) NULL)
    (void) XParseGeometry(image_info->size,&x,&y,&width,&height);
  if (image_info->interlace != PartitionInterlace)
    {
      /*
        Open image file.
      */
      OpenImage(image_info,image,ReadBinaryType);
      if (image->file == (FILE *) NULL)
        PrematureExit("Unable to open file",image);
      for (i=0; i < x; i++)
        (void) fgetc(image->file);
    }
  /*
    Allocate memory for a scanline.
  */
  packet_size=3*(image->depth >> 3);
  if (strcmp(image_info->magick,"RGBA") == 0)
    {
      image->matte=True;
      packet_size=4*(image->depth >> 3);
    }
  scanline=(unsigned char *)
    malloc(packet_size*image->columns*sizeof(unsigned char));
  if (scanline == (unsigned char *) NULL)
    PrematureExit("Unable to allocate memory",image);
  do
  {
    /*
      Initialize image structure.
    */
    image->columns=width;
    image->rows=height;
    if (image_info->tile != (char *) NULL)
      (void) XParseGeometry(image_info->tile,&x_offset,&y_offset,
        &image->columns,&image->rows);
    image->packets=image->columns*image->rows;
    image->pixels=(RunlengthPacket *)
      malloc(image->packets*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      PrematureExit("Unable to allocate memory",image);
    /*
      Convert raster image to runlength-encoded packets.
    */
    switch (image_info->interlace)
    {
      case NoneInterlace:
      default:
      {
        /*
          No interlacing:  RGBRGBRGBRGBRGBRGB...
        */
        for (y=0; y < y_offset; y++)
          (void) ReadData((char *) scanline,packet_size,width,image->file);
        q=image->pixels;
        for (y=0; y < image->rows; y++)
        {
          if ((y > 0) || (image->previous == (Image *) NULL))
            (void) ReadData((char *) scanline,packet_size,width,image->file);
          p=scanline+packet_size*x_offset;
          for (x=0; x < image->columns; x++)
          {
            ReadQuantum(q->red,p);
            ReadQuantum(q->green,p);
            ReadQuantum(q->blue,p);
            q->index=0;
            if (image->matte)
              ReadQuantum(q->index,p);
            q->length=0;
            q++;
          }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
        break;
      }
      case LineInterlace:
      {
        /*
          Line interlacing:  RRR...GGG...BBB...RRR...GGG...BBB...
        */
        packet_size=image->depth >> 3;
        for (y=0; y < y_offset; y++)
          (void) ReadData((char *) scanline,packet_size,width,image->file);
        for (y=0; y < image->rows; y++)
        {
          if ((y > 0) || (image->previous == (Image *) NULL))
            (void) ReadData((char *) scanline,packet_size,width,image->file);
          p=scanline+packet_size*x_offset;
          q=image->pixels+y*image->columns;
          for (x=0; x < image->columns; x++)
          {
            ReadQuantum(q->red,p);
            q->index=0;
            q->length=0;
            q++;
          }
          (void) ReadData((char *) scanline,packet_size,width,image->file);
          p=scanline+packet_size*x_offset;
          q=image->pixels+y*image->columns;
          for (x=0; x < image->columns; x++)
          {
            ReadQuantum(q->green,p);
            q++;
          }
          (void) ReadData((char *) scanline,packet_size,width,image->file);
          p=scanline+packet_size*x_offset;
          q=image->pixels+y*image->columns;
          for (x=0; x < image->columns; x++)
          {
            ReadQuantum(q->blue,p);
            q++;
          }
          if (image->matte)
            {
              (void) ReadData((char *) scanline,packet_size,width,image->file);
              p=scanline+packet_size*x_offset;
              q=image->pixels+y*image->columns;
              for (x=0; x < image->columns; x++)
              {
                ReadQuantum(q->index,p);
                q++;
              }
            }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
        break;
      }
      case PlaneInterlace:
      case PartitionInterlace:
      {
        unsigned int
          span;

        /*
          Plane interlacing:  RRRRRR...GGGGGG...BBBBBB...
        */
        if (image_info->interlace == PartitionInterlace)
          {
            AppendImageFormat("R",image->filename);
            OpenImage(image_info,image,ReadBinaryType);
            if (image->file == (FILE *) NULL)
              PrematureExit("Unable to open file",image);
          }
        packet_size=image->depth >> 3;
        for (y=0; y < y_offset; y++)
          (void) ReadData((char *) scanline,packet_size,width,image->file);
        i=0;
        span=image->rows*(image->matte ? 4 : 3);
        q=image->pixels;
        for (y=0; y < image->rows; y++)
        {
          if ((y > 0) || (image->previous == (Image *) NULL))
            (void) ReadData((char *) scanline,packet_size,width,image->file);
          p=scanline+packet_size*x_offset;
          for (x=0; x < image->columns; x++)
          {
            ReadQuantum(q->red,p);
            q->index=0;
            q->length=0;
            q++;
          }
          ProgressMonitor(LoadImageText,i++,span);
        }
        if (image_info->interlace == PartitionInterlace)
          {
            CloseImage(image);
            AppendImageFormat("G",image->filename);
            OpenImage(image_info,image,ReadBinaryType);
            if (image->file == (FILE *) NULL)
              PrematureExit("Unable to open file",image);
          }
        q=image->pixels;
        for (y=0; y < (height-image->rows); y++)
          (void) ReadData((char *) scanline,packet_size,width,image->file);
        for (y=0; y < image->rows; y++)
        {
          (void) ReadData((char *) scanline,packet_size,width,image->file);
          p=scanline+packet_size*x_offset;
          for (x=0; x < image->columns; x++)
          {
            ReadQuantum(q->green,p);
            q++;
          }
          ProgressMonitor(LoadImageText,i++,span);
        }
        if (image_info->interlace == PartitionInterlace)
          {
            CloseImage(image);
            AppendImageFormat("B",image->filename);
            OpenImage(image_info,image,ReadBinaryType);
            if (image->file == (FILE *) NULL)
              PrematureExit("Unable to open file",image);
          }
        q=image->pixels;
        for (y=0; y < (height-image->rows); y++)
          (void) ReadData((char *) scanline,packet_size,width,image->file);
        for (y=0; y < image->rows; y++)
        {
          (void) ReadData((char *) scanline,packet_size,width,image->file);
          p=scanline+packet_size*x_offset;
          for (x=0; x < image->columns; x++)
          {
            ReadQuantum(q->blue,p);
            q++;
          }
          ProgressMonitor(LoadImageText,i++,span);
        }
        if (image->matte)
          {
            /*
              Read matte channel.
            */
            if (image_info->interlace == PartitionInterlace)
              {
                CloseImage(image);
                AppendImageFormat("A",image->filename);
                OpenImage(image_info,image,ReadBinaryType);
                if (image->file == (FILE *) NULL)
                  PrematureExit("Unable to open file",image);
              }
            q=image->pixels;
            for (y=0; y < (height-image->rows); y++)
              (void) ReadData((char *) scanline,packet_size,width,image->file);
            for (y=0; y < image->rows; y++)
            {
              (void) ReadData((char *) scanline,packet_size,width,image->file);
              p=scanline+packet_size*x_offset;
              for (x=0; x < image->columns; x++)
              {
                ReadQuantum(q->index,p);
                q++;
              }
              ProgressMonitor(LoadImageText,i++,span);
            }
          }
        if (image_info->interlace == PartitionInterlace)
          (void) strcpy(image->filename,image_info->filename);
        break;
      }
    }
    CompressImage(image);
    /*
      Proceed to next image.
    */
    count=ReadData((char *) scanline,packet_size,width,image->file);
    if (count > 0)
      {
        /*
          Allocate next image structure.
        */
        image->next=AllocateImage(image_info);
        if (image->next == (Image *) NULL)
          {
            DestroyImages(image);
            return((Image *) NULL);
          }
        (void) strcpy(image->next->filename,image_info->filename);
        image->next->file=image->file;
        image->next->scene=image->scene+1;
        image->next->previous=image;
        image=image->next;
      }
  } while (count > 0);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  free((char *) scanline);
  CloseImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d R L A I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadRLAImage reads a run-length encoded Wavefront RLA image file
%  and returns it.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  Note:  This module was contributed by Lester Vecsey (master@internexus.net).
%
%  The format of the ReadRLAImage routine is:
%
%      image=ReadRLAImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadRLAImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadRLAImage(const ImageInfo *image_info)
{
  typedef struct _WindowFrame
  {
    short
      left,
      right,
      bottom,
      top;
  } WindowFrame;

  typedef struct _RLAHeader
  {
    WindowFrame
      window,
      active_window;

    short
      frame,
      storage_type,
      number_channels,
      number_matte_channels,
      number_auxillary_channels,
      revision;

    char
      gamma[16],
      red_primary[24],
      green_primary[24],
      blue_primary[24],
      white_point[24];

    long
      job_number;

    char
      name[128],
      description[128],
      program[64],
      machine[32],
      user[32],
      date[20],
      aspect[24],
      aspect_ratio[8],
      chan[32];

    short
      field;

    char
      time[12],
      filter[32];

    short
      bits_per_channel,
      matte_type,
      matte_bits,
      auxillary_type,
      auxillary_bits;

    char
      auxillary[32],
      space[36];

    long
      next;
  } RLAHeader;

  Image
    *image;

  int
    length,
    runlength,
    y;

  long
    *scanlines;

  register int
    i;

  register RunlengthPacket
    *q;

  RLAHeader
    rla_header;

  unsigned char
    byte;

  unsigned int
    channel;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  rla_header.window.left=MSBFirstReadShort(image->file);
  rla_header.window.right=MSBFirstReadShort(image->file);
  rla_header.window.bottom=MSBFirstReadShort(image->file);
  rla_header.window.top=MSBFirstReadShort(image->file);
  rla_header.active_window.left=MSBFirstReadShort(image->file);
  rla_header.active_window.right=MSBFirstReadShort(image->file);
  rla_header.active_window.bottom=MSBFirstReadShort(image->file);
  rla_header.active_window.top=MSBFirstReadShort(image->file);
  rla_header.frame=MSBFirstReadShort(image->file);
  rla_header.storage_type=MSBFirstReadShort(image->file);
  rla_header.number_channels=MSBFirstReadShort(image->file);
  if (rla_header.number_channels == 0)
    rla_header.number_channels=3;
  rla_header.number_matte_channels=MSBFirstReadShort(image->file);
  rla_header.number_auxillary_channels=MSBFirstReadShort(image->file);
  rla_header.revision=MSBFirstReadShort(image->file);
  (void) ReadData((char *) rla_header.gamma,16,1,image->file);
  (void) ReadData((char *) rla_header.red_primary,24,1,image->file);
  (void) ReadData((char *) rla_header.green_primary,24,1,image->file);
  (void) ReadData((char *) rla_header.blue_primary,24,1,image->file);
  (void) ReadData((char *) rla_header.white_point,24,1,image->file);
  rla_header.job_number=(long) MSBFirstReadLong(image->file);
  (void) ReadData((char *) rla_header.name,128,1,image->file);
  (void) ReadData((char *) rla_header.description,128,1,image->file);
  (void) ReadData((char *) rla_header.program,64,1,image->file);
  (void) ReadData((char *) rla_header.machine,32,1,image->file);
  (void) ReadData((char *) rla_header.user,32,1,image->file);
  (void) ReadData((char *) rla_header.date,20,1,image->file);
  (void) ReadData((char *) rla_header.aspect,24,1,image->file);
  (void) ReadData((char *) rla_header.aspect_ratio,8,1,image->file);
  (void) ReadData((char *) rla_header.chan,32,1,image->file);
  rla_header.field=MSBFirstReadShort(image->file);
  (void) ReadData((char *) rla_header.time,12,1,image->file);
  (void) ReadData((char *) rla_header.filter,32,1,image->file);
  rla_header.bits_per_channel=MSBFirstReadShort(image->file);
  rla_header.matte_type=MSBFirstReadShort(image->file);
  rla_header.matte_bits=MSBFirstReadShort(image->file);
  rla_header.auxillary_type=MSBFirstReadShort(image->file);
  rla_header.auxillary_bits=MSBFirstReadShort(image->file);
  (void) ReadData((char *) rla_header.auxillary,32,1,image->file);
  (void) ReadData((char *) rla_header.space,36,1,image->file);
  rla_header.next=(long) MSBFirstReadLong(image->file);
  /*
    Initialize image structure.
  */
  image->columns=rla_header.active_window.right-rla_header.active_window.left;
  image->rows=rla_header.active_window.top-rla_header.active_window.bottom;
  image->packets=image->columns*image->rows;
  image->depth=QuantumDepth;
  image->matte=rla_header.number_channels > 3;
  scanlines=(long *) malloc(image->rows*sizeof(long));
  image->pixels=(RunlengthPacket *)
    malloc(image->packets*sizeof(RunlengthPacket));
  if (image->pixels == (RunlengthPacket *) NULL)
    PrematureExit("Unable to allocate memory",image);
  if (*rla_header.description != '\0')
    {
      /*
        RLA image comment.
      */
      image->comments=(char *)
        malloc((Extent(rla_header.description)+1)*sizeof(char));
      if (image->comments == (char *) NULL)
        PrematureExit("Unable to allocate memory",image)
      (void) strcpy(image->comments,rla_header.description);
    }
  /*
    Read offsets to each scanline data.
  */
  for (i=0; i < image->rows; i++)
    scanlines[i]=(long) MSBFirstReadLong(image->file);
  /*
    Read image data.
  */
  q=image->pixels;
  for (y=0; y < image->rows; y++)
  {
    (void) fseek(image->file,scanlines[image->rows-y-1],0);
    for (channel=0; channel < rla_header.number_channels; channel++)
    {
      length=MSBFirstReadShort(image->file);
      q=image->pixels+(y*image->columns);
      while (length > 0)
      {
        (void) ReadData((char *) &byte,1,1,image->file);
        runlength=byte;
        if (byte > 127)
          runlength=byte-256;
        length--;
        if (length == 0)
          break;
        if (runlength < 0)
          {
            while (runlength < 0)
            {
              (void) ReadData((char *) &byte,1,1,image->file);
              length--;
              switch (channel)
              {
                case 0:
                {
                  q->red=UpScale(byte);
                  q->index=0;
                  q->length=0;
                  break;
                }
                case 1:
                {
                  q->green=UpScale(byte);
                  break;
                }
                case 2:
                {
                  q->blue=UpScale(byte);
                  break;
                }
                case 3:
                default:
                {
                  q->index=UpScale(byte);
                  break;
                }
              }
              q++;
              runlength++;
            }
            continue;
          }
        (void) ReadData((char *) &byte,1,1,image->file);
        length--;
        runlength++;
        do
        {
          switch (channel)
          {
            case 0:
            {
              q->red=UpScale(byte);
              q->index=0;
              q->length=0;
              break;
            }
            case 1:
            {
              q->green=UpScale(byte);
              break;
            }
            case 2:
            {
              q->blue=UpScale(byte);
              break;
            }
            case 3:
            default:
            {
              q->index=UpScale(byte);
              break;
            }
          }
          q++;
          runlength--;
        }
        while (runlength > 0);
      }
    }
    ProgressMonitor(LoadImageText,y,image->rows);
  }
  CompressImage(image);
  CloseImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d R L E I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadRLEImage reads a run-length encoded Utah Raster Toolkit
%  image file and returns it.  It allocates the memory necessary for the new
%  Image structure and returns a pointer to the new image.
%
%  The format of the ReadRLEImage routine is:
%
%      image=ReadRLEImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadRLEImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadRLEImage(const ImageInfo *image_info)
{
#define SkipLinesOp  0x01
#define SetColorOp  0x02
#define SkipPixelsOp  0x03
#define ByteDataOp  0x05
#define RunDataOp  0x06
#define EOFOp  0x07

  char
    magick[12];

  Image
    *image;

  int
    opcode,
    operand,
    status,
    x,
    y;

  register int
    i,
    j;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  unsigned char
    background_color[256],
    *colormap,
    pixel,
    plane,
    *rle_pixels;

  unsigned int
    bits_per_pixel,
    flags,
    map_length,
    number_colormaps,
    number_planes;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Determine if this is a RLE file.
  */
  status=ReadData((char *) magick,1,2,image->file);
  if ((status == False) || (strncmp(magick,"\122\314",2) != 0))
    PrematureExit("Not a RLE image file",image);
  do
  {
    /*
      Read image header.
    */
    (void) LSBFirstReadShort(image->file);
    (void) LSBFirstReadShort(image->file);
    image->columns=LSBFirstReadShort(image->file);
    image->rows=LSBFirstReadShort(image->file);
    image->packets=image->columns*image->rows;
    flags=fgetc(image->file);
    image->matte=flags & 0x04;
    number_planes=fgetc(image->file);
    bits_per_pixel=fgetc(image->file);
    number_colormaps=fgetc(image->file);
    map_length=1 << fgetc(image->file);
    if ((number_planes == 0) || (number_planes == 2) || (bits_per_pixel != 8) ||
        (image->columns == 0))
      PrematureExit("Unsupported RLE image file",image);
    if (flags & 0x02)
      {
        /*
          No background color-- initialize to black.
        */
        for (i=0; i < number_planes; i++)
          background_color[i]=(unsigned char) 0;
        (void) fgetc(image->file);
      }
    else
      {
        /*
          Initialize background color.
        */
        p=background_color;
        for (i=0; i < number_planes; i++)
          *p++=(unsigned char) fgetc(image->file);
      }
    if ((number_planes & 0x01) == 0)
      (void) fgetc(image->file);
    colormap=(unsigned char *) NULL;
    if (number_colormaps != 0)
      {
        /*
          Read image colormaps.
        */
        colormap=(unsigned char *)
          malloc(number_colormaps*map_length*sizeof(unsigned char));
        if (colormap == (unsigned char *) NULL)
          PrematureExit("Unable to allocate memory",image);
        p=colormap;
        for (i=0; i < number_colormaps; i++)
          for (j=0; j < map_length; j++)
            *p++=XDownScale(LSBFirstReadShort(image->file));
      }
    if (flags & 0x08)
      {
        unsigned int
          length;

        /*
          Read image comment.
        */
        length=LSBFirstReadShort(image->file);
        image->comments=(char *) malloc(length*sizeof(char));
        if (image->comments == (char *) NULL)
          PrematureExit("Unable to allocate memory",image);
        (void) ReadData((char *) image->comments,1,length-1,image->file);
        image->comments[length-1]='\0';
        if ((length & 0x01) == 0)
          (void) fgetc(image->file);
      }
    /*
      Allocate RLE pixels.
    */
    if (image->matte)
      number_planes++;
    rle_pixels=(unsigned char *)
      malloc(image->packets*number_planes*sizeof(unsigned char));
    if (rle_pixels == (unsigned char *) NULL)
      PrematureExit("Unable to allocate memory",image);
    if ((flags & 0x01) && !(flags & 0x02))
      {
        /*
          Set background color.
        */
        p=rle_pixels;
        for (i=0; i < image->packets; i++)
        {
          if (!image->matte)
            for (j=0; j < number_planes; j++)
              *p++=background_color[j];
          else
            {
              for (j=0; j < (number_planes-1); j++)
                *p++=background_color[j];
              *p++=0;  /* initialize matte channel */
            }
        }
      }
    /*
      Read runlength-encoded image.
    */
    plane=0;
    x=0;
    y=0;
    opcode=fgetc(image->file);
    do
    {
      switch (opcode & 0x3f)
      {
        case SkipLinesOp:
        {
          operand=fgetc(image->file);
          if (opcode & 0x40)
            operand=LSBFirstReadShort(image->file);
          x=0;
          y+=operand;
          break;
        }
        case SetColorOp:
        {
          operand=fgetc(image->file);
          plane=operand;
          if (plane == 255)
            plane=number_planes-1;
          x=0;
          break;
        }
        case SkipPixelsOp:
        {
          operand=fgetc(image->file);
          if (opcode & 0x40)
            operand=LSBFirstReadShort(image->file);
          x+=operand;
          break;
        }
        case ByteDataOp:
        {
          operand=fgetc(image->file);
          if (opcode & 0x40)
            operand=LSBFirstReadShort(image->file);
          p=rle_pixels+((image->rows-y-1)*image->columns*number_planes)+
            x*number_planes+plane;
          operand++;
          for (i=0; i < operand; i++)
          {
            pixel=fgetc(image->file);
            if ((y < image->rows) && ((x+i) < image->columns))
              *p=pixel;
            p+=number_planes;
          }
          if (operand & 0x01)
            (void) fgetc(image->file);
          x+=operand;
          break;
        }
        case RunDataOp:
        {
          operand=fgetc(image->file);
          if (opcode & 0x40)
            operand=LSBFirstReadShort(image->file);
          pixel=fgetc(image->file);
          (void) fgetc(image->file);
          operand++;
          p=rle_pixels+((image->rows-y-1)*image->columns*number_planes)+
            x*number_planes+plane;
          for (i=0; i < operand; i++)
          {
            if ((y < image->rows) && ((x+i) < image->columns))
              *p=pixel;
            p+=number_planes;
          }
          x+=operand;
          break;
        }
        default:
          break;
      }
      opcode=fgetc(image->file);
    } while (((opcode & 0x3f) != EOFOp) && (opcode != EOF));
    if (number_colormaps != 0)
      {
        unsigned int
          mask;

        /*
          Apply colormap transformation to image.
        */
        mask=(map_length-1);
        p=rle_pixels;
        if (number_colormaps == 1)
          for (i=0; i < image->packets; i++)
          {
            *p=(unsigned char) colormap[*p & mask];
            p++;
          }
        else
          if ((number_planes >= 3) && (number_colormaps >= 3))
            for (i=0; i < image->packets; i++)
              for (j=0; j < number_planes; j++)
              {
                *p=(unsigned char) colormap[j*map_length+(*p & mask)];
                p++;
              }
      }
    /*
      Initialize image structure.
    */
    image->pixels=(RunlengthPacket *)
      malloc(image->packets*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      PrematureExit("Unable to allocate memory",image);
    q=image->pixels;
    if (number_planes >= 3)
      {
        /*
          Convert raster image to DirectClass runlength-encoded packets.
        */
        p=rle_pixels;
        for (y=0; y < image->rows; y++)
        {
          for (x=0; x < image->columns; x++)
          {
            q->red=UpScale(*p++);
            q->green=UpScale(*p++);
            q->blue=UpScale(*p++);
            q->index=0;
            if (image->matte)
              q->index=UpScale(*p++);
            q->length=0;
            q++;
          }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
      }
    else
      {
        /*
          Create colormap.
        */
        image->class=PseudoClass;
        if (number_colormaps == 0)
          map_length=256;
        image->colors=map_length;
        image->colormap=(ColorPacket *)
          malloc(image->colors*sizeof(ColorPacket));
        if (image->colormap == (ColorPacket *) NULL)
          PrematureExit("Unable to allocate memory",image);
        p=colormap;
        if (number_colormaps == 0)
          for (i=0; i < image->colors; i++)
          {
            /*
              Grayscale.
            */
            image->colormap[i].red=(MaxRGB*i)/(image->colors-1);
            image->colormap[i].green=(MaxRGB*i)/(image->colors-1);
            image->colormap[i].blue=(MaxRGB*i)/(image->colors-1);
          }
        else
          if (number_colormaps == 1)
            for (i=0; i < image->colors; i++)
            {
              /*
                Pseudocolor.
              */
              image->colormap[i].red=(Quantum) UpScale(i);
              image->colormap[i].green=(Quantum) UpScale(i);
              image->colormap[i].blue=(Quantum) UpScale(i);
            }
          else
            for (i=0; i < image->colors; i++)
            {
              image->colormap[i].red=UpScale(*p);
              image->colormap[i].green=UpScale(*(p+map_length));
              image->colormap[i].blue=UpScale(*(p+map_length*2));
              p++;
            }
        p=rle_pixels;
        if (!image->matte)
          {
            /*
              Convert raster image to PseudoClass runlength-encoded packets.
            */
            for (y=0; y < image->rows; y++)
            {
              for (x=0; x < image->columns; x++)
              {
                q->index=(unsigned short) (*p++);
                q->length=0;
                q++;
              }
              ProgressMonitor(LoadImageText,y,image->rows);
            }
            SyncImage(image);
          }
        else
          {
            /*
              Image has a matte channel-- promote to DirectClass.
            */
            for (y=0; y < image->rows; y++)
            {
              for (x=0; x < image->columns; x++)
              {
                q->red=image->colormap[*p++].red;
                q->green=image->colormap[*p++].green;
                q->blue=image->colormap[*p++].blue;
                q->index=UpScale(*p++);
                q->length=0;
                q++;
              }
              ProgressMonitor(LoadImageText,y,image->rows);
            }
            free(image->colormap);
            image->colormap=(ColorPacket *) NULL;
            image->class=DirectClass;
            image->colors=0;
          }
      }
    if (number_colormaps != 0)
      free((char *) colormap);
    free((char *) rle_pixels);
    CompressImage(image);
    /*
      Proceed to next image.
    */
    (void) fgetc(image->file);
    status=ReadData((char *) magick,1,2,image->file);
    if ((status == True) && (strncmp(magick,"\122\314",2) == 0))
      {
        /*
          Allocate next image structure.
        */
        image->next=AllocateImage(image_info);
        if (image->next == (Image *) NULL)
          {
            DestroyImages(image);
            return((Image *) NULL);
          }
        (void) strcpy(image->next->filename,image_info->filename);
        image->next->file=image->file;
        image->next->scene=image->scene+1;
        image->next->previous=image;
        image=image->next;
      }
  } while ((status == True) && (strncmp(magick,"\122\314",2) == 0));
  while (image->previous != (Image *) NULL)
    image=image->previous;
  CloseImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d S G I I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadSGIImage reads a SGI RGB image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadSGIImage routine is:
%
%      image=ReadSGIImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadSGIImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/

static void SGIDecode(unsigned char *packets,unsigned char *pixels)
{
  unsigned char
    count,
    pixel;

  for ( ; ;)
  {
    pixel=(*packets++);
    count=pixel & 0x7f;
    if (count == 0)
      break;
    if (pixel & 0x80)
      for ( ; count != 0; count--)
      {
        *pixels=(*packets++);
        pixels+=4;
      }
    else
      {
        pixel=(*packets++);
        for ( ; count != 0; count--)
        {
          *pixels=pixel;
          pixels+=4;
        }
      }
  }
}

static Image *ReadSGIImage(const ImageInfo *image_info)
{
  typedef struct _SGIHeader
  {
    unsigned short
      magic;

    unsigned char
      storage,
      bytes_per_pixel;

    unsigned short
      dimension,
      columns,
      rows,
      depth;

    unsigned long
      minimum_value,
      maximum_value;

    unsigned char
      filler[492];
  } SGIHeader;

  Image
    *image;

  SGIHeader
    iris_header;

  register int
    i,
    x,
    y,
    z;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  unsigned char
    *iris_pixels;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Read SGI raster header.
  */
  iris_header.magic=MSBFirstReadShort(image->file);
  do
  {
    /*
      Verify SGI identifier.
    */
    if (iris_header.magic != 0x01DA)
      PrematureExit("Not a SGI RGB image",image);
    iris_header.storage=fgetc(image->file);
    iris_header.bytes_per_pixel=fgetc(image->file);
    if (iris_header.bytes_per_pixel != 1)
      PrematureExit("Image must have 1 byte per pixel channel",image);
    iris_header.dimension=MSBFirstReadShort(image->file);
    iris_header.columns=MSBFirstReadShort(image->file);
    iris_header.rows=MSBFirstReadShort(image->file);
    iris_header.depth=MSBFirstReadShort(image->file);
    iris_header.minimum_value=MSBFirstReadLong(image->file);
    iris_header.maximum_value=MSBFirstReadLong(image->file);
    (void) ReadData((char *) iris_header.filler,1,
      (unsigned int) sizeof(iris_header.filler),image->file);
    /*
      Allocate SGI pixels.
    */
    iris_pixels=(unsigned char *)
      malloc(4*iris_header.columns*iris_header.rows*sizeof(unsigned char));
    if (iris_pixels == (unsigned char *) NULL)
      PrematureExit("Unable to allocate memory",image);
    if (iris_header.storage != 0x01)
      {
        unsigned char
          *scanline;

        /*
          Read standard image format.
        */
        scanline=(unsigned char *)
          malloc(iris_header.columns*sizeof(unsigned char));
        if (scanline == (unsigned char *) NULL)
          PrematureExit("Unable to allocate memory",image);
        for (z=0; z < (int) iris_header.depth; z++)
        {
          p=iris_pixels+z;
          for (y=0; y < (int) iris_header.rows; y++)
          {
            (void) ReadData((char *) scanline,1,iris_header.columns,
              image->file);
            for (x=0; x < (int) iris_header.columns; x++)
            {
              *p=scanline[x];
              p+=4;
            }
          }
        }
        free((char *) scanline);
      }
    else
      {
        unsigned char
          *packets;

        unsigned int
          data_order;

        unsigned long
          offset,
          *offsets,
          *runlength;

        /*
          Read runlength-encoded image format.
        */
        offsets=(unsigned long *)
          malloc(iris_header.rows*iris_header.depth*sizeof(unsigned long));
        packets=(unsigned char *)
          malloc(((iris_header.columns << 1)+10)*sizeof(unsigned char));
        runlength=(unsigned long *)
          malloc(iris_header.rows*iris_header.depth*sizeof(unsigned long));
        if ((offsets == (unsigned long *) NULL) ||
            (packets == (unsigned char *) NULL) ||
            (runlength == (unsigned long *) NULL))
          PrematureExit("Unable to allocate memory",image);
        for (i=0; i < (int) (iris_header.rows*iris_header.depth); i++)
          offsets[i]=MSBFirstReadLong(image->file);
        for (i=0; i < (int) (iris_header.rows*iris_header.depth); i++)
          runlength[i]=MSBFirstReadLong(image->file);
        /*
          Check data order.
        */
        offset=0;
        data_order=0;
        for (y=0; ((y < (int) iris_header.rows) && !data_order); y++)
          for (z=0; ((z < (int) iris_header.depth) && !data_order); z++)
          {
            if (offsets[y+z*iris_header.rows] < offset)
              data_order=1;
            offset=offsets[y+z*iris_header.rows];
          }
        offset=512+4*((iris_header.rows*iris_header.depth) << 1);
        if (data_order == 1)
          {
            for (z=0; z < (int) iris_header.depth; z++)
            {
              p=iris_pixels;
              for (y=0; y < (int) iris_header.rows; y++)
              {
                if (offset != offsets[y+z*iris_header.rows])
                  {
                    offset=offsets[y+z*iris_header.rows];
                    (void) fseek(image->file,(int) offset,0);
                  }
                (void) ReadData((char *) packets,1,
                  (unsigned int) runlength[y+z*iris_header.rows],image->file);
                offset+=runlength[y+z*iris_header.rows];
                SGIDecode(packets,p+z);
                p+=(iris_header.columns*4);
              }
            }
          }
        else
          {
            p=iris_pixels;
            for (y=0; y < (int) iris_header.rows; y++)
            {
              for (z=0; z < (int) iris_header.depth; z++)
              {
                if (offset != offsets[y+z*iris_header.rows])
                  {
                    offset=offsets[y+z*iris_header.rows];
                    (void) fseek(image->file,(int) offset,0);
                  }
                (void) ReadData((char *) packets,1,
                  (unsigned int) runlength[y+z*iris_header.rows],image->file);
                offset+=runlength[y+z*iris_header.rows];
                SGIDecode(packets,p+z);
              }
              p+=(iris_header.columns*4);
            }
          }
        free(runlength);
        free(packets);
        free(offsets);
      }
    /*
      Initialize image structure.
    */
    image->matte=iris_header.depth == 4;
    image->columns=iris_header.columns;
    image->rows=iris_header.rows;
    image->packets=image->columns*image->rows;
    image->pixels=(RunlengthPacket *)
      malloc(image->packets*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      PrematureExit("Unable to allocate memory",image);
    /*
      Convert SGI raster image to runlength-encoded packets.
    */
    q=image->pixels;
    if (iris_header.depth >= 3)
      {
        /*
          Convert SGI image to DirectClass runlength-encoded packets.
        */
        for (y=0; y < image->rows; y++)
        {
          p=iris_pixels+((image->rows-1)-y)*(image->columns*4);
          for (x=0; x < image->columns; x++)
          {
            q->red=UpScale(*p);
            q->green=UpScale(*(p+1));
            q->blue=UpScale(*(p+2));
            q->index=UpScale(*(p+3));
            q->length=0;
            p+=4;
            q++;
          }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
      }
    else
      {
        unsigned short
          index;

        /*
          Create grayscale map.
        */
        image->class=PseudoClass;
        image->colors=256;
        image->colormap=(ColorPacket *)
          malloc(image->colors*sizeof(ColorPacket));
        if (image->colormap == (ColorPacket *) NULL)
          PrematureExit("Unable to allocate memory",image);
        for (i=0; i < image->colors; i++)
        {
          image->colormap[i].red=(Quantum) UpScale(i);
          image->colormap[i].green=(Quantum) UpScale(i);
          image->colormap[i].blue=(Quantum) UpScale(i);
        }
        /*
          Convert SGI image to PseudoClass runlength-encoded packets.
        */
        for (y=0; y < image->rows; y++)
        {
          p=iris_pixels+((image->rows-1)-y)*(image->columns*4);
          for (x=0; x < image->columns; x++)
          {
            index=(unsigned short) (*p);
            q->index=index;
            q->length=0;
            p+=4;
            q++;
          }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
        SyncImage(image);
      }
    free((char *) iris_pixels);
    CompressImage(image);
    /*
      Proceed to next image.
    */
    iris_header.magic=MSBFirstReadShort(image->file);
    if (iris_header.magic == 0x01DA)
      {
        /*
          Allocate image structure.
        */
        image->next=AllocateImage(image_info);
        if (image->next == (Image *) NULL)
          {
            DestroyImages(image);
            return((Image *) NULL);
          }
        (void) strcpy(image->next->filename,image_info->filename);
        image->next->file=image->file;
        image->next->scene=image->scene+1;
        image->next->previous=image;
        image=image->next;
      }
  } while (iris_header.magic == 0x01DA);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  CloseImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d S U N I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadSUNImage reads a SUN image file and returns it.  It allocates
%  the memory necessary for the new Image structure and returns a pointer to
%  the new image.
%
%  The format of the ReadSUNImage routine is:
%
%      image=ReadSUNImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadSUNImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadSUNImage(const ImageInfo *image_info)
{
#define RMT_EQUAL_RGB  1
#define RMT_NONE  0
#define RMT_RAW  2
#define RT_STANDARD  1
#define RT_ENCODED  2
#define RT_FORMAT_RGB  3

  typedef struct _SUNHeader
  {
    unsigned long
      magic,
      width,
      height,
      depth,
      length,
      type,
      maptype,
      maplength;
  } SUNHeader;

  Image
    *image;

  register int
    bit,
    i,
    x,
    y;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  SUNHeader
    sun_header;

  unsigned char
    *sun_data,
    *sun_pixels;

  unsigned int
    bytes_per_line,
    status;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Read SUN raster header.
  */
  sun_header.magic=MSBFirstReadLong(image->file);
  do
  {
    /*
      Verify SUN identifier.
    */
    if (sun_header.magic != 0x59a66a95)
      PrematureExit("Not a SUN raster image",image);
    sun_header.width=MSBFirstReadLong(image->file);
    sun_header.height=MSBFirstReadLong(image->file);
    sun_header.depth=MSBFirstReadLong(image->file);
    sun_header.length=MSBFirstReadLong(image->file);
    sun_header.type=MSBFirstReadLong(image->file);
    sun_header.maptype=MSBFirstReadLong(image->file);
    sun_header.maplength=MSBFirstReadLong(image->file);
    switch (sun_header.maptype)
    {
      case RMT_NONE:
      {
        if (sun_header.depth < 24)
          {
            /*
              Create linear color ramp.
            */
            image->colors=1 << sun_header.depth;
            image->colormap=(ColorPacket *)
              malloc(image->colors*sizeof(ColorPacket));
            if (image->colormap == (ColorPacket *) NULL)
              PrematureExit("Unable to allocate memory",image);
            for (i=0; i < image->colors; i++)
            {
              image->colormap[i].red=(MaxRGB*i)/(image->colors-1);
              image->colormap[i].green=(MaxRGB*i)/(image->colors-1);
              image->colormap[i].blue=(MaxRGB*i)/(image->colors-1);
            }
          }
        break;
      }
      case RMT_EQUAL_RGB:
      {
        unsigned char
          *sun_colormap;

        /*
          Read SUN raster colormap.
        */
        image->colors=(unsigned int) sun_header.maplength/3;
        image->colormap=(ColorPacket *)
          malloc(image->colors*sizeof(ColorPacket));
        sun_colormap=(unsigned char *)
          malloc(image->colors*sizeof(unsigned char));
        if ((image->colormap == (ColorPacket *) NULL) ||
            (sun_colormap == (unsigned char *) NULL))
          PrematureExit("Unable to allocate memory",image);
        (void) ReadData((char *) sun_colormap,1,image->colors,image->file);
        for (i=0; i < image->colors; i++)
          image->colormap[i].red=UpScale(sun_colormap[i]);
        (void) ReadData((char *) sun_colormap,1,image->colors,image->file);
        for (i=0; i < image->colors; i++)
          image->colormap[i].green=UpScale(sun_colormap[i]);
        (void) ReadData((char *) sun_colormap,1,image->colors,image->file);
        for (i=0; i < image->colors; i++)
          image->colormap[i].blue=UpScale(sun_colormap[i]);
        free((char *) sun_colormap);
        break;
      }
      case RMT_RAW:
      {
        unsigned char
          *sun_colormap;

        /*
          Read SUN raster colormap.
        */
        sun_colormap=(unsigned char *)
          malloc(sun_header.maplength*sizeof(unsigned char));
        if (sun_colormap == (unsigned char *) NULL)
          PrematureExit("Unable to allocate memory",image);
        (void) ReadData((char *) sun_colormap,1,(unsigned int)
          sun_header.maplength,image->file);
        free((char *) sun_colormap);
        break;
      }
      default:
        PrematureExit("Colormap type is not supported",image);
    }
    sun_data=(unsigned char *) malloc(sun_header.length*sizeof(unsigned char));
    if (sun_data == (unsigned char *) NULL)
      PrematureExit("Unable to allocate memory",image);
    status=ReadData((char *) sun_data,1,(unsigned int) sun_header.length,
      image->file);
    if ((status == False) && (sun_header.type != RT_ENCODED))
      PrematureExit("Unable to read image data",image);
    sun_pixels=sun_data;
    if (sun_header.type == RT_ENCODED)
      {
        unsigned int
          width,
          height;

        /*
          Read run-length encoded raster pixels.
        */
        width=(unsigned int) (sun_header.width*(((sun_header.depth-1) >> 3)+1));
        height=(unsigned int) sun_header.height;
        bytes_per_line=2*(sun_header.width*sun_header.depth+15)/16;
        sun_pixels=(unsigned char *)
          malloc(bytes_per_line*height*sizeof(unsigned char));
        if (sun_pixels == (unsigned char *) NULL)
          PrematureExit("Unable to allocate memory",image);
        (void) SUNDecodeImage(sun_data,sun_pixels,bytes_per_line,height);
        free((char *) sun_data);
      }
    /*
      Initialize image structure.
    */
    image->matte=(sun_header.depth == 32);
    image->class=(sun_header.depth < 24 ? PseudoClass : DirectClass);
    image->columns=(unsigned int) sun_header.width;
    image->rows=(unsigned int) sun_header.height;
    image->packets=image->columns*image->rows;
    image->pixels=(RunlengthPacket *)
      malloc(image->packets*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      PrematureExit("Unable to allocate memory",image);
    /*
      Convert SUN raster image to runlength-encoded packets.
    */
    p=sun_pixels;
    q=image->pixels;
    if (sun_header.depth == 1)
      for (y=0; y < image->rows; y++)
      {
        /*
          Convert bitmap scanline to runlength-encoded color packets.
        */
        for (x=0; x < (image->columns >> 3); x++)
        {
          for (bit=7; bit >= 0; bit--)
          {
            q->index=((*p) & (0x01 << bit) ? 0x00 : 0x01);
            q->length=0;
            q++;
          }
          p++;
        }
        if ((image->columns % 8) != 0)
          {
            for (bit=7; bit >= (8-(image->columns % 8)); bit--)
            {
              q->index=((*p) & (0x01 << bit) ? 0x00 : 0x01);
              q->length=0;
              q++;
            }
            p++;
          }
        if ((((image->columns/8)+(image->columns % 8 ? 1 : 0)) % 2) != 0)
          p++;
        ProgressMonitor(LoadImageText,y,image->rows);
      }
    else
      if (image->class == PseudoClass)
        for (y=0; y < image->rows; y++)
        {
          /*
            Convert PseudoColor scanline to runlength-encoded color packets.
          */
          for (x=0; x < image->columns; x++)
          {
            q->index=(*p++);
            q->length=0;
            q++;
          }
          if ((image->columns % 2) != 0)
            p++;
          ProgressMonitor(LoadImageText,y,image->rows);
        }
      else
        for (y=0; y < image->rows; y++)
        {
          /*
            Convert DirectColor scanline to runlength-encoded color packets.
          */
          for (x=0; x < image->columns; x++)
          {
            q->index=0;
            if (image->matte)
              q->index=UpScale(*p++);
            if (sun_header.type == RT_STANDARD)
              {
                q->blue=UpScale(*p++);
                q->green=UpScale(*p++);
                q->red=UpScale(*p++);
              }
            else
              {
                q->red=UpScale(*p++);
                q->green=UpScale(*p++);
                q->blue=UpScale(*p++);
              }
            if (image->colors != 0)
              {
                q->red=image->colormap[q->red].red;
                q->green=image->colormap[q->green].green;
                q->blue=image->colormap[q->blue].blue;
              }
            q->length=0;
            q++;
          }
          if (((image->columns % 2) != 0) && (image->matte == False))
            p++;
          ProgressMonitor(LoadImageText,y,image->rows);
        }
    free((char *) sun_pixels);
    if (image->class == PseudoClass)
      {
        SyncImage(image);
        CompressColormap(image);
      }
    CompressImage(image);
    /*
      Proceed to next image.
    */
    sun_header.magic=MSBFirstReadLong(image->file);
    if (sun_header.magic == 0x59a66a95)
      {
        /*
          Allocate image structure.
        */
        image->next=AllocateImage(image_info);
        if (image->next == (Image *) NULL)
          {
            DestroyImages(image);
            return((Image *) NULL);
          }
        (void) strcpy(image->next->filename,image_info->filename);
        image->next->file=image->file;
        image->next->scene=image->scene+1;
        image->next->previous=image;
        image=image->next;
      }
  } while (sun_header.magic == 0x59a66a95);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  CloseImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d T G A I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadTGAImage reads a Truevision TGA image file and returns it.
%  It allocates the memory necessary for the new Image structure and returns
%  a pointer to the new image.
%
%  The format of the ReadTGAImage routine is:
%
%      image=ReadTGAImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadTGAImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadTGAImage(const ImageInfo *image_info)
{
#define TGAColormap 1
#define TGARGB 2
#define TGAMonochrome 3
#define TGARLEColormap  9
#define TGARLERGB  10
#define TGARLEMonochrome  11

  typedef struct _TGAHeader
  {
    unsigned char
      id_length,
      colormap_type,
      image_type;

    unsigned short
      colormap_index,
      colormap_length;

    unsigned char
      colormap_size;

    unsigned short
      x_origin,
      y_origin,
      width,
      height;

    unsigned char
      pixel_size,
      attributes;
  } TGAHeader;

  Image
    *image;

  Quantum
    blue,
    green,
    red;

  register int
    i,
    x,
    y;

  register RunlengthPacket
    *q;

  TGAHeader
    tga_header;

  unsigned char
    j,
    k,
    runlength;

  unsigned int
    base,
    flag,
    offset,
    real,
    skip,
    status;

  unsigned short
    index;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Read TGA header information.
  */
  status=ReadData((char *) &tga_header.id_length,1,1,image->file);
  tga_header.colormap_type=fgetc(image->file);
  tga_header.image_type=fgetc(image->file);
  do
  {
    if ((status == False) || (tga_header.image_type == 0) ||
        (tga_header.image_type > 11))
      PrematureExit("Not a TGA image file",image);
    tga_header.colormap_index=LSBFirstReadShort(image->file);
    tga_header.colormap_length=LSBFirstReadShort(image->file);
    tga_header.colormap_size=fgetc(image->file);
    tga_header.x_origin=LSBFirstReadShort(image->file);
    tga_header.y_origin=LSBFirstReadShort(image->file);
    tga_header.width=LSBFirstReadShort(image->file);
    tga_header.height=LSBFirstReadShort(image->file);
    tga_header.pixel_size=fgetc(image->file);
    tga_header.attributes=fgetc(image->file);
    /*
      Initialize image structure.
    */
    image->matte=tga_header.pixel_size == 32;
    image->columns=tga_header.width;
    image->rows=tga_header.height;
    image->packets=image->columns*image->rows;
    image->pixels=(RunlengthPacket *)
      malloc(image->packets*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      PrematureExit("Unable to allocate memory",image);
    if (tga_header.id_length != 0)
      {
        /*
          TGA image comment.
        */
        image->comments=(char *)
          malloc((tga_header.id_length+1)*sizeof(char));
        if (image->comments == (char *) NULL)
          PrematureExit("Unable to allocate memory",image);
        (void) ReadData(image->comments,1,tga_header.id_length,image->file);
        image->comments[tga_header.id_length]='\0';
      }
    red=0;
    green=0;
    blue=0;
    if (tga_header.colormap_type != 0)
      {
        /*
          Read TGA raster colormap.
        */
        if ((tga_header.image_type == TGARLEColormap) ||
            (tga_header.image_type == TGARLERGB))
          image->class=PseudoClass;
        image->colors=tga_header.colormap_length;
        image->colormap=(ColorPacket *)
          malloc(image->colors*sizeof(ColorPacket));
        if (image->colormap == (ColorPacket *) NULL)
          PrematureExit("Unable to allocate memory",image);
        for (i=0; i < image->colors; i++)
        {
          switch (tga_header.colormap_size)
          {
            case 8:
            default:
            {
              /*
                Gray scale.
              */
              red=UpScale(fgetc(image->file));
              green=red;
              blue=red;
              break;
            }
            case 15:
            case 16:
            {
              /*
                5 bits each of red green and blue.
              */
              j=fgetc(image->file);
              k=fgetc(image->file);
              red=(Quantum) ((MaxRGB*((int) (k & 0x7c) >> 2))/31);
              green=(Quantum)
                ((MaxRGB*(((int) (k & 0x03) << 3)+((int) (j & 0xe0) >> 5)))/31);
              blue=(Quantum) ((MaxRGB*((int) (j & 0x1f)))/31);
              break;
            }
            case 32:
            case 24:
            {
              /*
                8 bits each of blue, green and red.
              */
              blue=UpScale(fgetc(image->file));
              green=UpScale(fgetc(image->file));
              red=UpScale(fgetc(image->file));
              break;
            }
          }
          image->colormap[i].red=red;
          image->colormap[i].green=green;
          image->colormap[i].blue=blue;
        }
      }
    /*
      Convert TGA pixels to runlength-encoded packets.
    */
    base=0;
    flag=0;
    index=0;
    skip=False;
    real=0;
    runlength=0;
    offset=0;
    q=image->pixels;
    for (i=0; i < image->packets; i++)
    {
      q->red=0;
      q->green=0;
      q->blue=0;
      q->index=0;
      q->length=0;
      q++;
    }
    for (y=0; y < image->rows; y++)
    {
      real=offset;
      if (((unsigned char) (tga_header.attributes & 0x20) >> 5) == 0)
        real=image->rows-real-1;
      q=image->pixels+(real*image->columns);
      for (x=0; x < image->columns; x++)
      {
        if ((tga_header.image_type == TGARLEColormap) ||
            (tga_header.image_type == TGARLERGB) ||
            (tga_header.image_type == TGARLEMonochrome))
          if (runlength != 0)
            {
              runlength--;
              skip=flag != 0;
            }
          else
            {
              status=ReadData((char *) &runlength,1,1,image->file);
              if (status == False)
                PrematureExit("Unable to read image data",image);
              flag=runlength & 0x80;
              if (flag != 0)
                runlength-=128;
              skip=False;
            }
        if (!skip)
          switch (tga_header.pixel_size)
          {
            case 8:
            default:
            {
              /*
                Gray scale.
              */
              index=fgetc(image->file);
              if (tga_header.colormap_type == 0)
                {
                  red=(Quantum) UpScale(index);
                  green=(Quantum) UpScale(index);
                  blue=(Quantum) UpScale(index);
                }
              else
                {
                  red=image->colormap[index].red;
                  green=image->colormap[index].green;
                  blue=image->colormap[index].blue;
                }
              break;
            }
            case 15:
            case 16:
            {
              /*
                5 bits each of red green and blue.
              */
              j=fgetc(image->file);
              k=fgetc(image->file);
              red=(Quantum) ((MaxRGB*((int) (k & 0x7c) >> 2))/31);
              green=(Quantum)
                ((MaxRGB*(((int) (k & 0x03) << 3)+((int) (j & 0xe0) >> 5)))/31);
              blue=(Quantum) ((MaxRGB*((int) (j & 0x1f)))/31);
              index=((unsigned short) k << 8)+j;
              break;
            }
            case 24:
            case 32:
            {
              /*
                8 bits each of blue green and red.
              */
              blue=UpScale(fgetc(image->file));
              green=UpScale(fgetc(image->file));
              red=UpScale(fgetc(image->file));
              if (tga_header.pixel_size == 32)
                index=Opaque-UpScale(fgetc(image->file));
              break;
            }
          }
        if (status == False)
          PrematureExit("Unable to read image data",image);
        q->red=red;
        q->green=green;
        q->blue=blue;
        q->index=index;
        q->length=0;
        q++;
      }
      if (((unsigned char) (tga_header.attributes & 0xc0) >> 6) == 4)
        offset+=4;
      else
        if (((unsigned char) (tga_header.attributes & 0xc0) >> 6) == 2)
          offset+=2;
        else
          offset++;
      if (offset >= image->rows)
        {
          base++;
          offset=base;
        }
      if (feof(image->file))
        break;
      ProgressMonitor(LoadImageText,y,image->rows);
    }
    (void) IsGrayImage(image);
    if (image->class == PseudoClass)
      SyncImage(image);
    CompressImage(image);
    /*
      Proceed to next image.
    */
    status=ReadData((char *) &tga_header.id_length,1,1,image->file);
    tga_header.colormap_type=fgetc(image->file);
    tga_header.image_type=fgetc(image->file);
    status&=((tga_header.image_type != 0) && (tga_header.image_type <= 11));
    if (status == True)
      {
        /*
          Allocate image structure.
        */
        image->next=AllocateImage(image_info);
        if (image->next == (Image *) NULL)
          {
            DestroyImages(image);
            return((Image *) NULL);
          }
        (void) strcpy(image->next->filename,image_info->filename);
        image->next->file=image->file;
        image->next->scene=image->scene+1;
        image->next->previous=image;
        image=image->next;
      }
  } while (status == True);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  CloseImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d T E X T I m a g e                                                  %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadTEXTImage reads a text file and returns it as an image.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadTEXTImage routine is:
%
%      image=ReadTEXTImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadTEXTImage returns a pointer to the image after
%      reading. A null image is returned if there is a a memory shortage or if
%      the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadTEXTImage(const ImageInfo *image_info)
{
  AnnotateInfo
    annotate_info;

  char
    *background_color,
    *foreground_color,
    geometry[MaxTextExtent],
    text[MaxTextExtent],
    *text_status;

  Display
    *display;

  Image
    *image;

  int
    offset,
    x,
    y;

  register int
    i;

  register RunlengthPacket
    *p;

  RunlengthPacket
    background;

  unsigned int
    height,
    width;

  XColor
    color;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,"r");
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Initialize Image structure.
  */
  (void) XParseGeometry(TextPageGeometry,&x,&y,&width,&height);
  if (image_info->size != (char *) NULL)
    (void) XParseGeometry(image_info->size,&x,&y,&width,&height);
  else
    {
      char
        density[MaxTextExtent];

      float
        dx_resolution,
        dy_resolution,
        x_resolution,
        y_resolution;

      int
        count;

      /*
        Determine bounding box.
      */
      dx_resolution=72.0;
      dy_resolution=72.0;
      x_resolution=72.0;
      (void) strcpy(density,PSDensityGeometry);
      count=sscanf(density,"%fx%f",&x_resolution,&y_resolution);
      if (image_info->density != (char *) NULL)
        count=sscanf(image_info->density,"%fx%f",&x_resolution,&y_resolution);
      if (count != 2)
        y_resolution=x_resolution;
      width=(unsigned int) (((width*x_resolution)/dx_resolution)+0.5);
      height=(unsigned int) (((height*y_resolution)/dy_resolution)+0.5);
    }
  image->columns=width;
  image->rows=height;
  image->packets=image->columns*image->rows;
  image->pixels=(RunlengthPacket *)
    malloc(image->packets*sizeof(RunlengthPacket));
  if (image->pixels == (RunlengthPacket *) NULL)
    PrematureExit("Unable to allocate memory",image);
  /*
    Initialize text image to background color.
  */
  background_color=DefaultTextBackground;
  foreground_color=DefaultTextForeground;
  display=XOpenDisplay(image_info->server_name);
  if (display != (Display *) NULL)
    {
      XrmDatabase
        resource_database;

      /*
        Set background and pen color.
      */
      XSetErrorHandler(XError);
      resource_database=XGetResourceDatabase(display,client_name);
      background_color=XGetResourceInstance(resource_database,client_name,
        "background",background_color);
      foreground_color=XGetResourceInstance(resource_database,client_name,
        "foreground",foreground_color);
      XCloseDisplay(display);
    }
  /*
    Initialize image annotation info.
  */
  GetAnnotateInfo(&annotate_info);
  annotate_info.server_name=image_info->server_name;
  annotate_info.font=image_info->font;
  annotate_info.text=text;
  annotate_info.geometry=geometry;
  annotate_info.pen=foreground_color;
  /*
    Initialize text image to background color.
  */
  (void) XQueryColorDatabase(background_color,&color);
  background.red=XDownScale(color.red);
  background.green=XDownScale(color.green);
  background.blue=XDownScale(color.blue);
  background.index=0;
  p=image->pixels;
  for (i=0; i < image->packets; i++)
    *p++=background;
  if (image_info->texture != (char *) NULL)
    TextureImage(image,image_info->texture);
  /*
    Annotate the text image.
  */
  offset=0;
  for ( ; ; )
  {
    /*
      Annotate image with text.
    */
    ProgressMonitor(LoadImageText,(unsigned int) ftell(image->file),
      (unsigned int) image->filesize);
    text_status=fgets(text,MaxTextExtent-1,image->file);
    if (text_status == (char *) NULL)
      break;
    if (Extent(annotate_info.text) > 0)
      annotate_info.text[Extent(annotate_info.text)-1]='\0';
    (void) sprintf(annotate_info.geometry,"%+d%+d",x,y+offset);
    AnnotateImage(image,&annotate_info);
    offset+=annotate_info.pointsize;
    if (((y << 1)+offset+annotate_info.pointsize) < image->rows)
      continue;
    /*
      Page is full-- allocate next image structure.
    */
    CompressImage(image);
    if (image_info->texture == (char *) NULL)
      {
        QuantizeInfo
          quantize_info;

        GetQuantizeInfo(&quantize_info);
        QuantizeImage(&quantize_info,image);
      }
    image->orphan=True;
    image->next=CopyImage(image,image->columns,image->rows,False);
    image->orphan=False;
    if (image->next == (Image *) NULL)
      {
        Warning("Unable to annotate image","Memory allocation error");
        break;
      }
    (void) strcpy(image->next->filename,image_info->filename);
    image->next->file=image->file;
    image->next->filesize=image->filesize;
    image->next->scene=image->scene+1;
    image->next->previous=image;
    image=image->next;
    /*
      Initialize text image to background color.
    */
    p=image->pixels;
    for (i=0; i < image->packets; i++)
      *p++=background;
    if (image_info->texture != (char *) NULL)
      TextureImage(image,image_info->texture);
    offset=0;
  }
  CompressImage(image);
  if (image_info->texture == (char *) NULL)
    {
      QuantizeInfo
        quantize_info;

     GetQuantizeInfo(&quantize_info);
     QuantizeImage(&quantize_info,image);
   }
  while (image->previous != (Image *) NULL)
    image=image->previous;
  CloseImage(image);
  return(image);
}

#ifdef HasTIFF

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d T I F F I m a g e                                                  %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadTIFFImage reads a Tagged image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadTIFFImage routine is:
%
%      image=ReadTIFFImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadTIFFImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/

static void TIFFWarningMessage(const char *module,const char *format,
  va_list warning)
{
  char
    message[MaxTextExtent];

  register char
    *p;

  p=message;
  if (module != (char *) NULL)
    {
      (void) sprintf(p,"%s: ",module);
      p+=Extent(message);
    }
  vsprintf(p,format,warning);
  (void) strcat(p,".");
  Warning(message,(char *) NULL);
}

static Image *ReadTIFFImage(const ImageInfo *image_info)
{
  char
    *text;

  Image
    *image;

  int
    range;

  Quantum
    blue,
    green,
    red;

  register int
    i,
    x,
    y;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  TIFF
    *tiff;

  uint16
    extra_samples,
    *sample_info;

  unsigned char
    *scanline;

  unsigned int
    height,
    method,
    packets,
    status,
    width;

  unsigned short
    bits_per_sample,
    index,
    interlace,
    max_sample_value,
    min_sample_value,
    pages,
    photometric,
    samples_per_pixel,
    units,
    value;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  if ((image->file == stdin) || image->pipe)
    {
      FILE
        *file;

      int
        c;

      /*
        Copy standard input or pipe to temporary file.
      */
      TemporaryFilename(image_info->filename);
      file=fopen(image_info->filename,WriteBinaryType);
      if (file == (FILE *) NULL)
        PrematureExit("Unable to write file",image);
      c=fgetc(image->file);
      while (c != EOF)
      {
        (void) putc(c,file);
        c=fgetc(image->file);
      }
      (void) fclose(file);
      (void) strcpy(image->filename,image_info->filename);
      image->temporary=True;
    }
  CloseImage(image);
  TIFFSetErrorHandler(TIFFWarningMessage);
  TIFFSetWarningHandler(TIFFWarningMessage);
  tiff=TIFFOpen(image->filename,ReadBinaryType);
  if (tiff == (TIFF *) NULL)
    PrematureExit("Unable to open file",image);
  if (image_info->subrange != 0)
    while (image->scene < image_info->subimage)
    {
      /*
        Skip to next image.
      */
      image->scene++;
      status=TIFFReadDirectory(tiff);
      if (status == False)
        PrematureExit("Unable to read subimage",image);
    }
  do
  {
    if (image_info->verbose)
      TIFFPrintDirectory(tiff,stderr,False);
    TIFFGetField(tiff,TIFFTAG_IMAGEWIDTH,&width);
    TIFFGetField(tiff,TIFFTAG_IMAGELENGTH,&height);
    TIFFGetFieldDefaulted(tiff,TIFFTAG_PLANARCONFIG,&interlace);
    TIFFGetFieldDefaulted(tiff,TIFFTAG_BITSPERSAMPLE,&bits_per_sample);
    TIFFGetFieldDefaulted(tiff,TIFFTAG_MINSAMPLEVALUE,&min_sample_value);
    TIFFGetFieldDefaulted(tiff,TIFFTAG_MAXSAMPLEVALUE,&max_sample_value);
    TIFFGetFieldDefaulted(tiff,TIFFTAG_PHOTOMETRIC,&photometric);
    TIFFGetFieldDefaulted(tiff,TIFFTAG_SAMPLESPERPIXEL,&samples_per_pixel);
    TIFFGetFieldDefaulted(tiff,TIFFTAG_RESOLUTIONUNIT,&units);
    TIFFGetFieldDefaulted(tiff,TIFFTAG_XRESOLUTION,&image->x_resolution);
    TIFFGetFieldDefaulted(tiff,TIFFTAG_YRESOLUTION,&image->y_resolution);
    /*
      Allocate memory for the image and pixel buffer.
    */
    image->columns=width;
    image->rows=height;
    if (units == RESUNIT_INCH)
      image->units=PixelsPerInchResolution;
    if (units == RESUNIT_CENTIMETER)
      image->units=PixelsPerCentimeterResolution;
    image->depth=bits_per_sample;
    if (bits_per_sample < 8)
      image->depth=8;
    image->packets=0;
    packets=Max((image->columns*image->rows+4) >> 3,1);
    if (bits_per_sample == 1)
      packets=Max((image->columns*image->rows+8) >> 4,1);
    image->pixels=(RunlengthPacket *) malloc(packets*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      {
        TIFFClose(tiff);
        PrematureExit("Unable to allocate memory",image);
      }
    TIFFGetFieldDefaulted(tiff,TIFFTAG_PAGENUMBER,&value,&pages);
    image->scene=value;
    text=(char *) NULL;
    TIFFGetField(tiff,TIFFTAG_PAGENAME,&text);
    if (text != (char *) NULL)
      {
        image->label=(char *)
          malloc((unsigned int) (Extent(text)+1)*sizeof(char));
        if (image->label == (char *) NULL)
          {
            TIFFClose(tiff);
            PrematureExit("Unable to allocate memory",image);
          }
        (void) strcpy(image->label,text);
      }
    text=(char *) NULL;
    TIFFGetField(tiff,TIFFTAG_IMAGEDESCRIPTION,&text);
    if (text != (char *) NULL)
      {
        image->comments=(char *)
          malloc((unsigned int) (Extent(text)+1)*sizeof(char));
        if (image->comments == (char *) NULL)
          {
            TIFFClose(tiff);
            PrematureExit("Unable to allocate memory",image);
          }
        (void) strcpy(image->comments,text);
      }
    range=max_sample_value-min_sample_value;
    if (range < 0)
      range=max_sample_value;
    q=image->pixels;
    q->length=MaxRunlength;
    method=0;
    if ((samples_per_pixel > 1) || TIFFIsTiled(tiff))
      {
        method=2;
        if ((samples_per_pixel >= 3) && (photometric == PHOTOMETRIC_RGB) &&
            (interlace == PLANARCONFIG_CONTIG))
          method=1;
      }
    switch (method)
    {
      case 0:
      {
        Quantum
          *quantum_scanline;

        register Quantum
          *r;

        /*
          Convert TIFF image to PseudoClass MIFF image.
        */
        image->class=PseudoClass;
        image->colors=range+1;
        if (bits_per_sample > QuantumDepth)
          image->colors=MaxRGB+1;
        image->colormap=(ColorPacket *)
          malloc(image->colors*sizeof(ColorPacket));
        quantum_scanline=(Quantum *) malloc(width*sizeof(Quantum));
        scanline=(unsigned char *) malloc(TIFFScanlineSize(tiff)+1);
        if ((image->colormap == (ColorPacket *) NULL) ||
            (quantum_scanline == (Quantum *) NULL) ||
            (scanline == (unsigned char *) NULL))
          {
            TIFFClose(tiff);
            PrematureExit("Unable to allocate memory",image);
          }
        /*
          Create colormap.
        */
        switch (photometric)
        {
          case PHOTOMETRIC_MINISBLACK:
          {
            for (i=0; i < image->colors; i++)
            {
              image->colormap[i].red=(MaxRGB*i)/(image->colors-1);
              image->colormap[i].green=(MaxRGB*i)/(image->colors-1);
              image->colormap[i].blue=(MaxRGB*i)/(image->colors-1);
            }
            break;
          }
          case PHOTOMETRIC_MINISWHITE:
          {
            unsigned int
              colors;

            colors=image->colors;
            for (i=0; i < image->colors; i++)
            {
              image->colormap[colors-i-1].red=(MaxRGB*i)/(image->colors-1);
              image->colormap[colors-i-1].green=(MaxRGB*i)/(image->colors-1);
              image->colormap[colors-i-1].blue=(MaxRGB*i)/(image->colors-1);
            }
            break;
          }
          case PHOTOMETRIC_PALETTE:
          {
            long
              range;

            unsigned short
              *blue_colormap,
              *green_colormap,
              *red_colormap;

            TIFFGetField(tiff,TIFFTAG_COLORMAP,&red_colormap,&green_colormap,
              &blue_colormap);
            range=256L;  /* might be old style 8-bit colormap */
            for (i=0; i < image->colors; i++)
              if ((red_colormap[i] >= 256) || (green_colormap[i] >= 256) ||
                  (blue_colormap[i] >= 256))
                {
                  range=65535L;
                  break;
                }
            for (i=0; i < image->colors; i++)
            {
              image->colormap[i].red=(Quantum)
                ((long) (MaxRGB*red_colormap[i])/range);
              image->colormap[i].green=(Quantum)
                ((long) (MaxRGB*green_colormap[i])/range);
              image->colormap[i].blue=(Quantum)
                ((long) (MaxRGB*blue_colormap[i])/range);
            }
            break;
          }
          default:
            break;
        }
        /*
          Convert image to PseudoClass runlength-encoded packets.
        */
        for (y=0; y < image->rows; y++)
        {
          TIFFReadScanline(tiff,(char *) scanline,y,0);
          p=scanline;
          r=quantum_scanline;
          switch (bits_per_sample)
          {
            case 1:
            {
              register int
                bit;

              for (x=0; x < ((int) width-7); x+=8)
              {
                for (bit=7; bit >= 0; bit--)
                  *r++=((*p) & (0x01 << bit) ? 0x01 : 0x00);
                p++;
              }
              if ((width % 8) != 0)
                {
                  for (bit=7; bit >= (8-(width % 8)); bit--)
                    *r++=((*p) & (0x01 << bit) ? 0x01 : 0x00);
                  p++;
                }
              break;
            }
            case 2:
            {
              for (x=0; x < ((int) width-3); x+=4)
              {
                *r++=(*p >> 6) & 0x3;
                *r++=(*p >> 4) & 0x3;
                *r++=(*p >> 2) & 0x3;
                *r++=(*p) & 0x3;
                p++;
              }
              if ((width % 4) != 0)
                {
                  for (i=3; i >= (4-(width % 4)); i--)
                    *r++=(*p >> (i*2)) & 0x03;
                  p++;
                }
              break;
            }
            case 4:
            {
              for (x=0; x < ((int) width-1); x+=2)
              {
                *r++=(*p >> 4) & 0xf;
                *r++=(*p) & 0xf;
                p++;
              }
              if ((width % 2) != 0)
                *r++=(*p++ >> 4) & 0xf;
              break;
            }
            case 8:
            {
              for (x=0; x < width; x++)
                *r++=(*p++);
              break;
            }
            case 16:
            {
              for (x=0; x < image->columns; x++)
              {
                ReadQuantum(*r,p);
                r++;
              }
              break;
            }
            default:
              break;
          }
          /*
            Transfer image scanline.
          */
          r=quantum_scanline;
          for (x=0; x < image->columns; x++)
          {
            index=(*r++);
            if ((index == q->index) && ((int) q->length < MaxRunlength))
              q->length++;
            else
              {
                if (image->packets != 0)
                  q++;
                image->packets++;
                if (image->packets == packets)
                  {
                    packets<<=1;
                    image->pixels=(RunlengthPacket *) realloc((char *)
                      image->pixels,packets*sizeof(RunlengthPacket));
                    if (image->pixels == (RunlengthPacket *) NULL)
                      {
                        free((char *) scanline);
                        free((char *) quantum_scanline);
                        TIFFClose(tiff);
                        PrematureExit("Unable to allocate memory",image);
                      }
                    q=image->pixels+image->packets-1;
                  }
                q->index=index;
                q->length=0;
              }
          }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
        free((char *) scanline);
        free((char *) quantum_scanline);
        if (image->class == PseudoClass)
          {
            SyncImage(image);
            CompressColormap(image);
          }
        break;
      }
      case 1:
      {
        /*
          Convert TIFF image to DirectClass MIFF image.
        */
        scanline=(unsigned char *) malloc((TIFFScanlineSize(tiff) << 1)+1);
        if (scanline == (unsigned char *) NULL)
          {
            TIFFClose(tiff);
            PrematureExit("Unable to allocate memory",image);
          }
        TIFFGetFieldDefaulted(tiff,TIFFTAG_EXTRASAMPLES,&extra_samples,
          &sample_info);
        image->matte=(extra_samples == 1) &&
          ((*sample_info == EXTRASAMPLE_ASSOCALPHA) ||
           (*sample_info == EXTRASAMPLE_UNASSALPHA));
        for (y=0; y < image->rows; y++)
        {
          TIFFReadScanline(tiff,(char *) scanline,y,0);
          if (bits_per_sample == 4)
            {
              register unsigned char
                *r;

              width=TIFFScanlineSize(tiff);
              p=scanline+width-1;
              r=scanline+(width << 1)-1;
              for (x=0; x < (int) width; x++)
              {
                *r--=((*p) & 0xf) << 4;
                *r--=((*p >> 4) & 0xf) << 4;
                p--;
              }
            }
          p=scanline;
          for (x=0; x < image->columns; x++)
          {
            ReadQuantum(red,p);
            ReadQuantum(green,p);
            ReadQuantum(blue,p);
            index=0;
            if (samples_per_pixel == 4)
              ReadQuantum(index,p);
            if ((red == q->red) && (green == q->green) && (blue == q->blue) &&
                (index == q->index) && ((int) q->length < MaxRunlength))
              q->length++;
            else
              {
                if (image->packets != 0)
                  q++;
                image->packets++;
                if (image->packets == packets)
                  {
                    packets<<=1;
                    image->pixels=(RunlengthPacket *) realloc((char *)
                      image->pixels,packets*sizeof(RunlengthPacket));
                    if (image->pixels == (RunlengthPacket *) NULL)
                      {
                        TIFFClose(tiff);
                        free((char *) scanline);
                        PrematureExit("Unable to allocate memory",image);
                      }
                    q=image->pixels+image->packets-1;
                  }
                q->red=red;
                q->green=green;
                q->blue=blue;
                q->index=index;
                q->length=0;
              }
          }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
        free((char *) scanline);
        break;
      }
      case 2:
      default:
      {
        register uint32
          *p,
          *pixels;

        /*
          Convert TIFF image to DirectClass MIFF image.
        */
        TIFFGetFieldDefaulted(tiff,TIFFTAG_EXTRASAMPLES,&extra_samples,
          &sample_info);
        image->matte=
          ((extra_samples == 1) && (sample_info[0] == EXTRASAMPLE_ASSOCALPHA));
        pixels=(uint32 *)
          malloc((image->columns*image->rows+image->columns)*sizeof(uint32));
        if (pixels == (uint32 *) NULL)
          {
            TIFFClose(tiff);
            PrematureExit("Unable to allocate memory",image);
          }
        status=TIFFReadRGBAImage(tiff,image->columns,image->rows,pixels,0);
        if (status == False)
          {
            free((char *) pixels);
            TIFFClose(tiff);
            PrematureExit("Unable to read image",image);
          }
        /*
          Convert image to DirectClass runlength-encoded packets.
        */
        for (y=image->rows-1; y >= 0; y--)
        {
          p=pixels+y*image->columns;
          for (x=0; x < image->columns; x++)
          {
            red=UpScale(TIFFGetR(*p));
            green=UpScale(TIFFGetG(*p));
            blue=UpScale(TIFFGetB(*p));
            index=image->matte ? UpScale(TIFFGetA(*p)) : 0;
            if ((red == q->red) && (green == q->green) && (blue == q->blue) &&
                (index == q->index) && ((int) q->length < MaxRunlength))
              q->length++;
            else
              {
                if (image->packets != 0)
                  q++;
                image->packets++;
                if (image->packets == packets)
                  {
                    packets<<=1;
                    image->pixels=(RunlengthPacket *) realloc((char *)
                      image->pixels,packets*sizeof(RunlengthPacket));
                    if (image->pixels == (RunlengthPacket *) NULL)
                      {
                        free((char *) pixels);
                        TIFFClose(tiff);
                        PrematureExit("Unable to allocate memory",image);
                      }
                    q=image->pixels+image->packets-1;
                  }
                q->red=red;
                q->green=green;
                q->blue=blue;
                q->index=index;
                q->length=0;
              }
            p++;
          }
          ProgressMonitor(LoadImageText,image->rows-y-1,image->rows);
        }
        free((char *) pixels);
        (void) IsPseudoClass(image);
        break;
      }
    }
    image->pixels=(RunlengthPacket *)
      realloc((char *) image->pixels,image->packets*sizeof(RunlengthPacket));
    /*
      Proceed to next image.
    */
    if (image_info->subrange != 0)
      if (image->scene >= (image_info->subimage+image_info->subrange-1))
        break;
    status=TIFFReadDirectory(tiff);
    if (status == True)
      {
        /*
          Allocate image structure.
        */
        image->next=AllocateImage(image_info);
        if (image->next == (Image *) NULL)
          {
            DestroyImages(image);
            return((Image *) NULL);
          }
        (void) strcpy(image->next->filename,image_info->filename);
        image->next->file=image->file;
        image->next->scene=image->scene+1;
        image->next->previous=image;
        image=image->next;
      }
  } while (status == True);
  TIFFClose(tiff);
  if (image->temporary)
    (void) remove(image_info->filename);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  return(image);
}
#else
static Image *ReadTIFFImage(const ImageInfo *image_info)
{
  Warning("TIFF library is not available",image_info->filename);
  return(ReadMIFFImage(image_info));
}
#endif

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d T I L E I m a g e                                                  %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadTILEImage tiles a texture on an image.  It allocates the
%  memory necessary for the new Image structure and returns a pointer to the
%  new image.
%
%  The format of the ReadTILEImage routine is:
%
%      image=ReadTILEImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadTILEImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadTILEImage(ImageInfo *image_info)
{
  Image
    *image,
    *tiled_image;

  int
    x,
    y;

  unsigned int
    height,
    width;

  tiled_image=ReadImage(image_info);
  if (tiled_image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Determine width and height, e.g. 640x512.
  */
  width=512;
  height=512;
  if (image_info->size != (char *) NULL)
    (void) XParseGeometry(image_info->size,&x,&y,&width,&height);
  /*
    Initialize Image structure.
  */
  tiled_image->orphan=True;
  image=CopyImage(tiled_image,width,height,False);
  tiled_image->orphan=False;
  if (image == (Image *) NULL)
    PrematureExit("Unable to allocate memory",tiled_image);
  (void) strcpy(image->filename,image_info->filename);
  /*
    Tile texture onto image.
  */
  for (y=0; y < image->rows; y+=tiled_image->rows)
  {
    for (x=0; x < image->columns; x+=tiled_image->columns)
      CompositeImage(image,ReplaceCompositeOp,tiled_image,x,y);
    ProgressMonitor(LoadImageText,y,image->columns);
  }
  DestroyImage(tiled_image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d U Y V Y I m a g e                                                  %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadUYVYImage reads an image in the UYVY (16bit/pixel) format
%  and returns it.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  The format of the ReadUYVYImage routine is:
%
%      image=ReadUYVYImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadUYVYImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadUYVYImage(const ImageInfo *image_info)
{
  Image
    *image;

  int
    x,
    y;

  register int
    i;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  unsigned char
    *uyvy_pixels;

  unsigned int
    height,
    width;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  (void) strcpy(image->filename,image_info->filename);
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image)
  /*
    Determine width and height, e.g. 640x512.
  */
  width=512;
  height=512;
  x=0;
  if (image_info->size != (char *) NULL)
    (void) XParseGeometry(image_info->size,&x,&y,&width,&height);
  for (i=0; i < x; i++)
    (void) fgetc(image->file);
  /*
    Read data.
  */
  image->columns=width;
  image->rows=height;
  image->packets=image->columns*image->rows;
  uyvy_pixels=(unsigned char *)
    malloc((2*width*height)*sizeof(unsigned char));
  image->pixels=(RunlengthPacket *)
    malloc(image->packets*sizeof(RunlengthPacket));
  if ((uyvy_pixels == (unsigned char *) NULL) ||
      (image->pixels == (RunlengthPacket *) NULL))
    PrematureExit("Unable to allocate memory",image);
  (void) ReadData((char *) uyvy_pixels,1,2*width*height,image->file);
  /*
    Accumulate UYVY, then unpack into two pixels.
  */
  p=uyvy_pixels;
  q=image->pixels;
  for (i=0; i < (image->packets >> 1); i++)
  {
    q->red=UpScale(p[1]);
    q->green=UpScale(p[0]);
    q->blue=UpScale(p[2]);
    q->index=0;
    q->length=0;
    q++;
    q->red=UpScale(p[3]);
    q->green=UpScale(p[0]);
    q->blue=UpScale(p[2]);
    q->index=0;
    q->length=0;
    q++;
    p+=4;
    if (QuantumTick(i,image))
      ProgressMonitor(LoadImageText,i,image->packets >> 1);
  }
  free((char *) uyvy_pixels);
  TransformRGBImage(image,YCbCrColorspace);
  CompressImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e a d V I C A R I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadVICARImage reads a VICAR image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadVICARImage routine is:
%
%      image=ReadVICARImage(image_info)
%
%  A description of each parameter follows:
%
%    o image: Function ReadVICARImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or if
%      the image cannot be read.
%
%    o filename: Specifies the name of the image to read.
%
%
*/
static Image *ReadVICARImage(const ImageInfo *image_info)
{
  char
    keyword[MaxTextExtent],
    value[MaxTextExtent];

  Image
    *image;

  int
    c,
    y;

  long
    count;

  register int
    i,
    x;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  unsigned char
    *vicar_pixels;

  unsigned int
    header_length,
    status,
    value_expected;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Decode image header.
  */
  c=fgetc(image->file);
  count=1;
  if (c == EOF)
    {
      DestroyImage(image);
      return((Image *) NULL);
    }
  header_length=0;
  while (isgraph(c) && ((image->columns*image->rows) == 0))
  {
    if (!isalnum(c))
      {
        c=fgetc(image->file);
        count++;
      }
    else
      {
        register char
          *p;

        /*
          Determine a keyword and its value.
        */
        p=keyword;
        do
        {
          if ((p-keyword) < (MaxTextExtent-1))
            *p++=(char) c;
          c=fgetc(image->file);
          count++;
        } while (isalnum(c) || (c == '_'));
        *p='\0';
        value_expected=False;
        while (isspace(c) || (c == '='))
        {
          if (c == '=')
            value_expected=True;
          c=fgetc(image->file);
          count++;
        }
        if (value_expected == False)
          continue;
        p=value;
        while (isalnum(c))
        {
          if ((p-value) < (MaxTextExtent-1))
            *p++=(char) c;
          c=fgetc(image->file);
          count++;
        }
        *p='\0';
        /*
          Assign a value to the specified keyword.
        */
        if (strcmp(keyword,"LABEL_RECORDS") == 0)
          header_length=(unsigned int) atoi(value);
        if (strcmp(keyword,"LBLSIZE") == 0)
          header_length=(unsigned int) atoi(value);
        if (strcmp(keyword,"RECORD_BYTES") == 0)
          image->columns=(unsigned int) atoi(value);
        if (strcmp(keyword,"NS") == 0)
          image->columns=(unsigned int) atoi(value);
        if (strcmp(keyword,"LINES") == 0)
          image->rows=(unsigned int) atoi(value);
        if (strcmp(keyword,"NL") == 0)
          image->rows=(unsigned int) atoi(value);
      }
    while (isspace(c))
    {
      c=fgetc(image->file);
      count++;
    }
  }
  /*
    Read the rest of the header.
  */
  while (count < header_length)
  {
    c=fgetc(image->file);
    count++;
  }
  /*
    Verify that required image information is defined.
  */
  if ((image->columns*image->rows) == 0)
    PrematureExit("image size is zero",image);
  /*
    Create linear colormap.
  */
  image->class=PseudoClass;
  image->colors=256;
  image->colormap=(ColorPacket *) malloc(image->colors*sizeof(ColorPacket));
  if (image->colormap == (ColorPacket *) NULL)
    PrematureExit("Unable to allocate memory",image);
  for (i=0; i < image->colors; i++)
  {
    image->colormap[i].red=(Quantum) UpScale(i);
    image->colormap[i].green=(Quantum) UpScale(i);
    image->colormap[i].blue=(Quantum) UpScale(i);
  }
  /*
    Initialize image structure.
  */
  image->packets=image->columns*image->rows;
  image->pixels=(RunlengthPacket *)
    malloc(image->packets*sizeof(RunlengthPacket));
  vicar_pixels=(unsigned char *) malloc(image->packets*sizeof(unsigned char));
  if ((image->pixels == (RunlengthPacket *) NULL) ||
      (vicar_pixels == (unsigned char *) NULL))
    PrematureExit("Unable to read image data",image);
  /*
    Convert VICAR pixels to runlength-encoded packets.
  */
  status=ReadData((char *) vicar_pixels,1,image->packets,image->file);
  if (status == False)
    PrematureExit("Insufficient image data in file",image);
  /*
    Convert VICAR pixels to runlength-encoded packets.
  */
  p=vicar_pixels;
  q=image->pixels;
  for (y=0; y < image->rows; y++)
  {
    for (x=0; x < image->columns; x++)
    {
      q->index=(unsigned short) *p;
      q->length=0;
      p++;
      q++;
    }
    ProgressMonitor(LoadImageText,y,image->rows);
  }
  free((char *) vicar_pixels);
  SyncImage(image);
  CompressColormap(image);
  CompressImage(image);
  CloseImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d V I D I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadVIDImage reads one of more images and creates a Visual Image
%  Directory file.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  The format of the ReadVIDImage routine is:
%
%      image=ReadVIDImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadVIDImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadVIDImage(const ImageInfo *image_info)
{
#define ClientName  "montage"

  char
    *commands[5],
    **filelist,
    **list,
    *resource_value;

  Display
    *display;

  Image
    *image,
    *montage_image,
    *next_image;

  ImageInfo
    local_info;

  int
    number_files;

  register int
    i;

  XMontageInfo
    montage_info;

  XResourceInfo
    resource_info;

  XrmDatabase
    resource_database;

  /*
    Expand the filename.
  */
  list=(char **) malloc(sizeof(char *));
  if (list == (char **) NULL)
    {
      Warning("Memory allocation error",(char *) NULL);
      return((Image *) NULL);
    }
  list[0]=(char *) malloc(Extent(image_info->filename)+1);
  if (list[0] == (char *) NULL)
    {
      Warning("Memory allocation error",(char *) NULL);
      return((Image *) NULL);
    }
  (void) strcpy(list[0],image_info->filename);
  number_files=1;
  filelist=list;
  ExpandFilenames(&number_files,&filelist);
  if (number_files == 0)
    {
      Warning("VID translation failed",image_info->filename);
      return((Image *) NULL);
    }
  /*
    Open X server connection.
  */
  resource_info.background_color=DefaultTileBackground;
  resource_info.border_width=atoi(DefaultTileBorderwidth);
  resource_info.foreground_color=DefaultTileForeground;
  resource_info.gravity=CenterGravity;
  resource_info.image_geometry=DefaultTileGeometry;
  resource_info.matte_color=DefaultTileMatte;
  resource_info.title=(char *) NULL;
  display=XOpenDisplay(image_info->server_name);
  if (display != (Display *) NULL)
    {
      /*
        Set our forgiving error handler.
      */
      XSetErrorHandler(XError);
      /*
        Get user defaults from X resource database.
      */
      resource_database=XGetResourceDatabase(display,client_name);
      XGetResourceInfo(resource_database,ClientName,&resource_info);
      resource_info.background_color=XGetResourceInstance(resource_database,
        ClientName,"background",DefaultTileBackground);
      resource_value=XGetResourceClass(resource_database,ClientName,
        "borderWidth",DefaultTileBorderwidth);
      resource_info.border_width=atoi(resource_value);
      resource_info.font=image_info->font;
      resource_info.foreground_color=XGetResourceInstance(resource_database,
        ClientName,"foreground",DefaultTileForeground);
      resource_info.image_geometry=XGetResourceInstance(resource_database,
        ClientName,"imageGeometry",DefaultTileGeometry);
      resource_info.matte_color=XGetResourceInstance(resource_database,
        ClientName,"mattecolor",DefaultTileMatte);
      XCloseDisplay(display);
    }
  resource_info.server_name=image_info->server_name;
  /*
    Read each image and convert them to a tile.
  */
  image=(Image *) NULL;
  local_info=(*image_info);
  commands[0]=client_name;
  commands[1]="-label";
  commands[2]=DefaultTileLabel;
  for (i=0; i < number_files; i++)
  {
    if (local_info.size == (char *) NULL)
      local_info.size=resource_info.image_geometry;
    local_info.filename=filelist[i];
    *local_info.magick='\0';
    next_image=ReadImage(&local_info);
    free((char *) filelist[i]);
    if (next_image != (Image *) NULL)
      {
        commands[3]="-geometry";
        commands[4]=resource_info.image_geometry;
        MogrifyImages(&local_info,5,commands,&next_image);
        if (image == (Image *) NULL)
          image=next_image;
        else
          {
            image->next=next_image;
            image->next->previous=image;
            image=image->next;
          }
      }
    ProgressMonitor(LoadImageText,i,number_files);
  }
  free((char *) filelist);
  if (image == (Image *) NULL)
    {
      Warning("VID translation failed",image_info->filename);
      return((Image *) NULL);
    }
  while (image->previous != (Image *) NULL)
    image=image->previous;
  /*
    Create the visual image directory.
  */
  XGetMontageInfo(&montage_info);
  montage_info.texture="granite:";
  (void) strcpy(montage_info.filename,image_info->filename);
  montage_image=XMontageImages(&resource_info,&montage_info,image);
  if (montage_image == (Image *) NULL)
    {
      Warning("VID translation failed",image_info->filename);
      return((Image *) NULL);
    }
  free((char *) list[0]);
  free((char *) list);
  return(montage_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e a d V I F F I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadVIFFImage reads a Khoros Visualization image file and returns
%  it.  It allocates the memory necessary for the new Image structure and
%  returns a pointer to the new image.
%
%  The format of the ReadVIFFImage routine is:
%
%      image=ReadVIFFImage(image_info)
%
%  A description of each parameter follows:
%
%    o image: Function ReadVIFFImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or if
%      the image cannot be read.
%
%    o filename: Specifies the name of the image to read.
%
%
*/
static Image *ReadVIFFImage(const ImageInfo *image_info)
{
#define VFF_CM_genericRGB  15
#define VFF_CM_ntscRGB  1
#define VFF_CM_NONE  0
#define VFF_DEP_DECORDER  0x4
#define VFF_DEP_NSORDER  0x8
#define VFF_DES_RAW  0
#define VFF_LOC_IMPLICIT  1
#define VFF_MAPTYP_NONE  0
#define VFF_MAPTYP_1_BYTE  1
#define VFF_MS_NONE  0
#define VFF_MS_ONEPERBAND  1
#define VFF_MS_SHARED  3
#define VFF_TYP_BIT  0
#define VFF_TYP_1_BYTE  1
#define VFF_TYP_2_BYTE  2
#define VFF_TYP_4_BYTE  4

  typedef struct _ViffHeader
  {
    unsigned char
      identifier,
      file_type,
      release,
      version,
      machine_dependency,
      reserve[3];

    char
      comment[512];

    unsigned long
      rows,
      columns,
      subrows;

    long
      x_offset,
      y_offset;

    float
      x_pixel_size,
      y_pixel_size;

    unsigned long
      location_type,
      location_dimension,
      number_of_images,
      number_data_bands,
      data_storage_type,
      data_encode_scheme,
      map_scheme,
      map_storage_type,
      map_rows,
      map_columns,
      map_subrows,
      map_enable,
      maps_per_cycle,
      color_space_model;
  } ViffHeader;

  Image
    *image;

  register int
    bit,
    i,
    x,
    y;

  register Quantum
    *p;

  register RunlengthPacket
    *q;

  unsigned char
    buffer[7],
    *viff_pixels;

  unsigned int
    bytes_per_pixel,
    status;

  unsigned long
    packets;

  ViffHeader
    viff_header;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Read VIFF header (1024 bytes).
  */
  status=ReadData((char *) &viff_header.identifier,1,1,image->file);
  do
  {
    /*
      Verify VIFF identifier.
    */
    if ((status == False) || ((unsigned char) viff_header.identifier != 0xab))
      PrematureExit("Not a VIFF raster",image);
    /*
      Initialize VIFF image.
    */
    (void) ReadData((char *) buffer,1,7,image->file);
    viff_header.file_type=buffer[0];
    viff_header.release=buffer[1];
    viff_header.version=buffer[2];
    viff_header.machine_dependency=buffer[3];
    (void) ReadData((char *) viff_header.comment,1,512,image->file);
    viff_header.comment[511]='\0';
    if (Extent(viff_header.comment) > 4)
      {
        image->comments=(char *)
          malloc((unsigned int) (Extent(viff_header.comment)+1)*sizeof(char));
        if (image->comments == (char *) NULL)
          PrematureExit("Unable to allocate memory",image);
        (void) strcpy(image->comments,viff_header.comment);
      }
    if ((viff_header.machine_dependency == VFF_DEP_DECORDER) ||
        (viff_header.machine_dependency == VFF_DEP_NSORDER))
      {
        viff_header.rows=LSBFirstReadLong(image->file);
        viff_header.columns=LSBFirstReadLong(image->file);
        viff_header.subrows=LSBFirstReadLong(image->file);
        viff_header.x_offset=(long) LSBFirstReadLong(image->file);
        viff_header.y_offset=(long) LSBFirstReadLong(image->file);
        viff_header.x_pixel_size=(float) LSBFirstReadLong(image->file);
        viff_header.y_pixel_size=(float) LSBFirstReadLong(image->file);
        viff_header.location_type=LSBFirstReadLong(image->file);
        viff_header.location_dimension=LSBFirstReadLong(image->file);
        viff_header.number_of_images=LSBFirstReadLong(image->file);
        viff_header.number_data_bands=LSBFirstReadLong(image->file);
        viff_header.data_storage_type=LSBFirstReadLong(image->file);
        viff_header.data_encode_scheme=LSBFirstReadLong(image->file);
        viff_header.map_scheme=LSBFirstReadLong(image->file);
        viff_header.map_storage_type=LSBFirstReadLong(image->file);
        viff_header.map_rows=LSBFirstReadLong(image->file);
        viff_header.map_columns=LSBFirstReadLong(image->file);
        viff_header.map_subrows=LSBFirstReadLong(image->file);
        viff_header.map_enable=LSBFirstReadLong(image->file);
        viff_header.maps_per_cycle=LSBFirstReadLong(image->file);
        viff_header.color_space_model=LSBFirstReadLong(image->file);
      }
    else
      {
        viff_header.rows=MSBFirstReadLong(image->file);
        viff_header.columns=MSBFirstReadLong(image->file);
        viff_header.subrows=MSBFirstReadLong(image->file);
        viff_header.x_offset=(long) MSBFirstReadLong(image->file);
        viff_header.y_offset=(long) MSBFirstReadLong(image->file);
        viff_header.x_pixel_size=(float) MSBFirstReadLong(image->file);
        viff_header.y_pixel_size=(float) MSBFirstReadLong(image->file);
        viff_header.location_type=MSBFirstReadLong(image->file);
        viff_header.location_dimension=MSBFirstReadLong(image->file);
        viff_header.number_of_images=MSBFirstReadLong(image->file);
        viff_header.number_data_bands=MSBFirstReadLong(image->file);
        viff_header.data_storage_type=MSBFirstReadLong(image->file);
        viff_header.data_encode_scheme=MSBFirstReadLong(image->file);
        viff_header.map_scheme=MSBFirstReadLong(image->file);
        viff_header.map_storage_type=MSBFirstReadLong(image->file);
        viff_header.map_rows=MSBFirstReadLong(image->file);
        viff_header.map_columns=MSBFirstReadLong(image->file);
        viff_header.map_subrows=MSBFirstReadLong(image->file);
        viff_header.map_enable=MSBFirstReadLong(image->file);
        viff_header.maps_per_cycle=MSBFirstReadLong(image->file);
        viff_header.color_space_model=MSBFirstReadLong(image->file);
      }
    for (i=0; i < 420; i++)
      (void) fgetc(image->file);
    /*
      Verify that we can read this VIFF image.
    */
    if ((viff_header.columns*viff_header.rows) == 0)
      PrematureExit("Image column or row size is not supported",image);
    if ((viff_header.data_storage_type != VFF_TYP_BIT) &&
        (viff_header.data_storage_type != VFF_TYP_1_BYTE) &&
        (viff_header.data_storage_type != VFF_TYP_2_BYTE) &&
        (viff_header.data_storage_type != VFF_TYP_4_BYTE))
      PrematureExit("Data storage type is not supported",image);
    if (viff_header.data_encode_scheme != VFF_DES_RAW)
      PrematureExit("Data encoding scheme is not supported",image);
    if ((viff_header.map_storage_type != VFF_MAPTYP_NONE) &&
        (viff_header.map_storage_type != VFF_MAPTYP_1_BYTE))
      PrematureExit("Map storage type is not supported",image);
    if ((viff_header.color_space_model != VFF_CM_NONE) &&
        (viff_header.color_space_model != VFF_CM_ntscRGB) &&
        (viff_header.color_space_model != VFF_CM_genericRGB))
      PrematureExit("Colorspace model is not supported",image);
    if (viff_header.location_type != VFF_LOC_IMPLICIT)
      {
        Warning("Location type is not supported",image->filename);
        DestroyImages(image);
        return((Image *) NULL);
      }
    if (viff_header.number_of_images != 1)
      PrematureExit("Number of images is not supported",image);
    switch (viff_header.map_scheme)
    {
      case VFF_MS_NONE:
      {
        if (viff_header.number_data_bands < 3)
          {
            /*
              Create linear color ramp.
            */
            if (viff_header.data_storage_type == VFF_TYP_BIT)
              image->colors=2;
            else
              image->colors=1 << (viff_header.number_data_bands*QuantumDepth);
            image->colormap=(ColorPacket *)
              malloc(image->colors*sizeof(ColorPacket));
            if (image->colormap == (ColorPacket *) NULL)
              PrematureExit("Unable to allocate memory",image);
            for (i=0; i < image->colors; i++)
            {
              image->colormap[i].red=(MaxRGB*i)/(image->colors-1);
              image->colormap[i].green=(MaxRGB*i)/(image->colors-1);
              image->colormap[i].blue=(MaxRGB*i)/(image->colors-1);
            }
          }
        break;
      }
      case VFF_MS_ONEPERBAND:
      case VFF_MS_SHARED:
      {
        unsigned char
          *viff_colormap;

        /*
          Read VIFF raster colormap.
        */
        image->colors=(unsigned int) viff_header.map_columns;
        image->colormap=(ColorPacket *)
          malloc(image->colors*sizeof(ColorPacket));
        viff_colormap=(unsigned char *)
          malloc(image->colors*sizeof(unsigned char));
        if ((image->colormap == (ColorPacket *) NULL) ||
            (viff_colormap == (unsigned char *) NULL))
          PrematureExit("Unable to allocate memory",image);
        (void) ReadData((char *) viff_colormap,1,image->colors,image->file);
        for (i=0; i < image->colors; i++)
        {
          image->colormap[i].red=UpScale(viff_colormap[i]);
          image->colormap[i].green=UpScale(viff_colormap[i]);
          image->colormap[i].blue=UpScale(viff_colormap[i]);
        }
        if (viff_header.map_rows > 1)
          {
            (void) ReadData((char *) viff_colormap,1,image->colors,image->file);
            for (i=0; i < image->colors; i++)
              image->colormap[i].green=UpScale(viff_colormap[i]);
          }
        if (viff_header.map_rows > 2)
          {
            (void) ReadData((char *) viff_colormap,1,image->colors,image->file);
            for (i=0; i < image->colors; i++)
              image->colormap[i].blue=UpScale(viff_colormap[i]);
          }
        free((char *) viff_colormap);
        break;
      }
      default:
        PrematureExit("Colormap type is not supported",image);
    }
    /*
      Allocate VIFF pixels.
    */
    bytes_per_pixel=1;
    if (viff_header.data_storage_type == VFF_TYP_2_BYTE)
      bytes_per_pixel=2;
    if (viff_header.data_storage_type == VFF_TYP_4_BYTE)
      bytes_per_pixel=4;
    if (viff_header.data_storage_type == VFF_TYP_BIT)
      packets=((viff_header.columns+7) >> 3)*viff_header.rows;
    else
      packets=
        viff_header.columns*viff_header.rows*viff_header.number_data_bands;
    viff_pixels=(unsigned char *)
      malloc(bytes_per_pixel*packets*sizeof(Quantum));
    if (viff_pixels == (unsigned char *) NULL)
      PrematureExit("Unable to allocate memory",image);
    (void) ReadData((char *) viff_pixels,bytes_per_pixel,(unsigned int) packets,
      image->file);
    switch (viff_header.data_storage_type)
    {
      int
        max_value,
        min_value,
        value;

      register Quantum
        *q;

      unsigned long
        scale_factor;

      case VFF_TYP_1_BYTE:
      {
        register unsigned char
          *p;

        if (QuantumDepth == 8)
          break;
        /*
          Scale integer pixels to [0..MaxRGB].
        */
        p=viff_pixels;
        q=(Quantum *) viff_pixels;
        p+=packets-1;
        q+=packets-1;
        for (i=0; i < packets; i++)
        {
          value=UpScale(*p);
          *q=(Quantum) value;
          p--;
          q--;
        }
        break;
      }
      case VFF_TYP_2_BYTE:
      {
        register short int
          *p;

        /*
          Ensure the header byte-order is most-significant byte first.
        */
        if ((viff_header.machine_dependency == VFF_DEP_DECORDER) ||
            (viff_header.machine_dependency == VFF_DEP_NSORDER))
          MSBFirstOrderShort((char *) &viff_header,
            (unsigned int) (bytes_per_pixel*packets));
        /*
          Determine scale factor.
        */
        p=(short int *) viff_pixels;
        max_value=(*p);
        min_value=(*p);
        for (i=0; i < packets; i++)
        {
          if (*p > max_value)
            max_value=(*p);
          else
            if (*p < min_value)
              min_value=(*p);
          p++;
        }
        if ((min_value == 0) && (max_value == 0))
          scale_factor=0;
        else
          if (min_value == max_value)
            {
              scale_factor=UpShift(MaxRGB)/min_value;
              min_value=0;
            }
          else
            scale_factor=UpShift(MaxRGB)/(max_value-min_value);
        /*
          Scale integer pixels to [0..MaxRGB].
        */
        p=(short int *) viff_pixels;
        q=(Quantum *) viff_pixels;
        for (i=0; i < packets; i++)
        {
          value=DownShift((*p-min_value)*scale_factor);
          if (value > MaxRGB)
            value=MaxRGB;
          else
            if (value < 0)
              value=0;
          *q=(Quantum) value;
          p++;
          q++;
        }
        break;
      }
      case VFF_TYP_4_BYTE:
      {
        register int
          *p;

        /*
          Ensure the header byte-order is most-significant byte first.
        */
        if ((viff_header.machine_dependency == VFF_DEP_DECORDER) ||
            (viff_header.machine_dependency == VFF_DEP_NSORDER))
          MSBFirstOrderLong((char *) &viff_header,
            (unsigned int) (bytes_per_pixel*packets));
        /*
          Determine scale factor.
        */
        p=(int *) viff_pixels;
        max_value=(*p);
        min_value=(*p);
        for (i=0; i < packets; i++)
        {
          if (*p > max_value)
            max_value=(*p);
          else
            if (*p < min_value)
              min_value=(*p);
          p++;
        }
        if ((min_value == 0) && (max_value == 0))
          scale_factor=0;
        else
          if (min_value == max_value)
            {
              scale_factor=UpShift(MaxRGB)/min_value;
              min_value=0;
            }
          else
            scale_factor=UpShift(MaxRGB)/(max_value-min_value);
        /*
          Scale integer pixels to [0..MaxRGB].
        */
        p=(int *) viff_pixels;
        q=(Quantum *) viff_pixels;
        for (i=0; i < packets; i++)
        {
          value=DownShift((*p-min_value)*scale_factor);
          if (value > MaxRGB)
            value=MaxRGB;
          else
            if (value < 0)
              value=0;
          *q=(unsigned char) value;
          p++;
          q++;
        }
        break;
      }
    }
    /*
      Initialize image structure.
    */
    image->matte=(viff_header.number_data_bands == 4);
    image->class=
      (viff_header.number_data_bands < 3 ? PseudoClass : DirectClass);
    image->columns=(unsigned int) viff_header.rows;
    image->rows=(unsigned int) viff_header.columns;
    image->packets=image->columns*image->rows;
    image->pixels=(RunlengthPacket *)
      malloc(image->packets*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      PrematureExit("Unable to allocate memory",image);
    /*
      Convert VIFF raster image to runlength-encoded packets.
    */
    p=(Quantum *) viff_pixels;
    q=image->pixels;
    if (viff_header.data_storage_type == VFF_TYP_BIT)
      {
        unsigned int
          polarity;

        /*
          Convert bitmap scanline to runlength-encoded color packets.
        */
        polarity=(viff_header.machine_dependency == VFF_DEP_DECORDER) ||
          (viff_header.machine_dependency == VFF_DEP_NSORDER);
        for (y=0; y < image->rows; y++)
        {
          /*
            Convert bitmap scanline to runlength-encoded color packets.
          */
          for (x=0; x < (image->columns >> 3); x++)
          {
            for (bit=0; bit < 8; bit++)
            {
              q->index=((*p) & (0x01 << bit) ? polarity : !polarity);
              q->length=0;
              q++;
            }
            p++;
          }
          if ((image->columns % 8) != 0)
            {
              for (bit=0; bit < (image->columns % 8); bit++)
              {
                q->index=((*p) & (0x01 << bit) ? polarity : !polarity);
                q->length=0;
                q++;
              }
              p++;
            }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
      }
    else
      if (image->class == PseudoClass)
        for (y=0; y < image->rows; y++)
        {
          /*
            Convert PseudoColor scanline to runlength-encoded color packets.
          */
          for (x=0; x < image->columns; x++)
          {
            q->index=(*p++);
            q->length=0;
            q++;
          }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
      else
        {
          unsigned long
            offset;

          /*
            Convert DirectColor scanline to runlength-encoded color packets.
          */
          offset=image->columns*image->rows;
          for (y=0; y < image->rows; y++)
          {
            for (x=0; x < image->columns; x++)
            {
              q->red=(*p);
              q->green=(*(p+offset));
              q->blue=(*(p+offset*2));
              if (image->colors != 0)
                {
                  q->red=image->colormap[q->red].red;
                  q->green=image->colormap[q->green].green;
                  q->blue=image->colormap[q->blue].blue;
                }
              q->index=(unsigned short) (image->matte ? (*(p+offset*3)) : 0);
              q->length=0;
              p++;
              q++;
            }
            ProgressMonitor(LoadImageText,y,image->rows);
          }
        }
    free((char *) viff_pixels);
    if (image->class == PseudoClass)
      {
        SyncImage(image);
        CompressColormap(image);
      }
    CompressImage(image);
    /*
      Proceed to next image.
    */
    status=ReadData((char *) &viff_header.identifier,1,1,image->file);
    if ((status == True) && (viff_header.identifier == 0xab))
      {
        /*
          Allocate image structure.
        */
        image->next=AllocateImage(image_info);
        if (image->next == (Image *) NULL)
          {
            DestroyImages(image);
            return((Image *) NULL);
          }
        (void) strcpy(image->next->filename,image_info->filename);
        image->next->file=image->file;
        image->next->scene=image->scene+1;
        image->next->previous=image;
        image=image->next;
      }
  } while ((status == True) && (viff_header.identifier == 0xab));
  while (image->previous != (Image *) NULL)
    image=image->previous;
  CloseImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e a d X I m a g e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Procedure ReadXImage reads an image from an X window.
%
%  The format of the ReadXImage routine is:
%
%      image=ReadXImage(image_info,frame,borders,screen,descend)
%
%  A description of each parameter follows:
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o frame: Specifies whether to include the window manager frame with the
%      image.
%
%    o borders: Specifies whether borders pixels are to be saved with
%      the image.
%
%    o screen: Specifies whether the GetImage request used to obtain the image
%      should be done on the root window, rather than directly on the specified
%      window.
%
%    o descend: If this option is zero, check to see if the WM_COLORMAP_WINDOWS
%      property is set or if XListInstalledColormaps returns more than one
%      colormap.  If so, the image is obtained by descending the window
%      hierarchy and reading each subwindow and its colormap.
%
%
*/
Export Image *ReadXImage(ImageInfo *image_info,const unsigned int frame,
  const unsigned int borders,const unsigned int screen,unsigned int descend)
{
  Colormap
    *colormaps;

  Display
    *display;

  Image
    *image;

  int
    status,
    x;

  RectangleInfo
    crop_info;

  Window
    *children,
    client,
    prior_target,
    root,
    target;

  XTextProperty
    window_name;

  /*
    Open X server connection.
  */
  display=XOpenDisplay(image_info->server_name);
  if (display == (Display *) NULL)
    {
      Warning("Unable to connect to X server",
        XDisplayName(image_info->server_name));
      return((Image *) NULL);
    }
  /*
    Set our forgiving error handler.
  */
  XSetErrorHandler(XError);
  /*
    Select target window.
  */
  crop_info.x=0;
  crop_info.y=0;
  crop_info.width=0;
  crop_info.height=0;
  root=XRootWindow(display,XDefaultScreen(display));
  target=(Window) NULL;
  if ((image_info->filename != (char *) NULL) &&
      (*image_info->filename != '\0'))
    if (Latin1Compare(image_info->filename,"root") == 0)
      target=root;
    else
      {
        /*
          Select window by ID or name.
        */
        if (isdigit(*image_info->filename))
          target=XWindowByID(display,root,(Window) strtol(image_info->filename,
            (char **) NULL,0));
        if (target == (Window) NULL)
          target=XWindowByName(display,root,image_info->filename);
        if (target == (Window) NULL)
          Warning("No window with specified id exists",image_info->filename);
      }

  /*
    If target window is not defined, interactively select one.
  */
  prior_target=target;
  if (target == (Window) NULL)
    target=XSelectWindow(display,&crop_info);
  client=target;   /* obsolete */
  if (target != root)
    {
      unsigned int
        d;

      status=XGetGeometry(display,target,&root,&x,&x,&d,&d,&d,&d);
      if (status != 0)
        {
          for ( ; ; )
          {
            Window
              parent;

            /*
              Find window manager frame.
            */
            status=XQueryTree(display,target,&root,&parent,&children,&d);
            if (status && (children != (Window *) NULL))
              XFree((char *) children);
            if (!status || (parent == (Window) NULL) || (parent == root))
              break;
            target=parent;
          }
          /*
            Get client window.
          */
          client=XClientWindow(display,target);
          if (!frame)
            target=client;
          if (!frame && prior_target)
            target=prior_target;
          XRaiseWindow(display,target);
          XDelay(display,SuspendTime << 4);
        }
    }
  if (screen)
    {
      int
        y;

      Window
        child;

      XWindowAttributes
        window_attributes;

      /*
        Obtain window image directly from screen.
      */
      status=XGetWindowAttributes(display,target,&window_attributes);
      if (status == False)
        {
          Warning("Unable to read X window attributes",image_info->filename);
          XCloseDisplay(display);
          return((Image *) NULL);
        }
      XTranslateCoordinates(display,target,root,0,0,&x,&y,&child);
      crop_info.x=x;
      crop_info.y=y;
      crop_info.width=window_attributes.width;
      crop_info.height=window_attributes.height;
      if (borders)
        {
          /*
            Include border in image.
          */
          crop_info.x-=window_attributes.border_width;
          crop_info.y-=window_attributes.border_width;
          crop_info.width+=window_attributes.border_width << 1;
          crop_info.height+=window_attributes.border_width << 1;
        }
      target=root;
    }
  if (descend)
    {
      int
        number_colormaps,
        number_windows;

      /*
        If WM_COLORMAP_WINDOWS property is set or multiple colormaps, descend.
      */
      descend=False;
      number_windows=0;
      status=XGetWMColormapWindows(display,target,&children,&number_windows);
      if ((status == True) && (number_windows > 0))
        {
          descend=True;
          XFree ((char *) children);
        }
      colormaps=XListInstalledColormaps(display,target,&number_colormaps);
      if (number_colormaps > 0)
        {
          if (number_colormaps > 1)
            descend=True;
          XFree((char *) colormaps);
        }
    }
  /*
    Alert the user not to alter the screen.
  */
  XBell(display,0);
  /*
    Get image by window id.
  */
  XGrabServer(display);
  image=XGetWindowImage(display,target,borders,descend);
  XUngrabServer(display);
  if (image == (Image *) NULL)
    Warning("Unable to read X window image",image_info->filename);
  else
    {
      (void) strcpy(image->filename,image_info->filename);
      if ((crop_info.width != 0) && (crop_info.height != 0))
        {
          Image
            *cropped_image;

          /*
            Crop image as defined by the cropping rectangle.
          */
          cropped_image=CropImage(image,&crop_info);
          if (cropped_image != (Image *) NULL)
            {
              DestroyImage(image);
              image=cropped_image;
            }
        }
      status=XGetWMName(display,target,&window_name);
      if (status == True)
        {
          if ((image_info->filename != (char *) NULL) &&
              (*image_info->filename == '\0'))
            {
              /*
                Initialize image filename.
              */
              (void) strncpy(image->filename,(char *) window_name.value,
                (int) window_name.nitems);
              image->filename[window_name.nitems]='\0';
            }
          XFree((void *) window_name.value);
        }
    }
  /*
    Alert the user we're done.
  */
  XBell(display,0);
  XBell(display,0);
  CompressImage(image);
  XCloseDisplay(display);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d X B M I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadXBMImage reads an X11 bitmap image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadXBMImage routine is:
%
%      image=ReadXBMImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadXBMImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static int XBMInteger(FILE *file,short int *hex_digits)
{
  int
    c,
    flag,
    value;

  value=0;
  flag=0;
  for ( ; ; )
  {
    c=fgetc(file);
    if (c == EOF)
      {
        value=(-1);
        break;
      }
    c&=0xff;
    if (isxdigit(c))
      {
        value=(value << 4)+hex_digits[c];
        flag++;
        continue;
      }
    if ((hex_digits[c]) < 0 && flag)
      break;
  }
  return(value);
}

static Image *ReadXBMImage(const ImageInfo *image_info)
{
  char
    buffer[MaxTextExtent],
    name[MaxTextExtent];

  Image
    *image;

  register int
    x,
    y;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  short int
    hex_digits[256];

  unsigned char
    bit,
    *data;

  unsigned int
    byte,
    bytes_per_line,
    packets,
    padding,
    value,
    version;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Read X bitmap header.
  */
  while (fgets(buffer,MaxTextExtent-1,image->file) != (char *) NULL)
    if (sscanf(buffer,"#define %s %u",name,&image->columns) == 2)
      if ((strlen(name) >= 6) && (strcmp(name+strlen(name)-6,"_width") == 0))
          break;
  while (fgets(buffer,MaxTextExtent-1,image->file) != (char *) NULL)
    if (sscanf(buffer,"#define %s %u",name,&image->rows) == 2)
      if ((strlen(name) >= 7) && (strcmp(name+strlen(name)-7,"_height") == 0))
          break;
  /*
    Scan until hex digits.
  */
  version=11;
  while (fgets(buffer,MaxTextExtent-1,image->file) != (char *) NULL)
  {
    if (sscanf(buffer,"static short %s = {",name) == 1)
      version=10;
    else
      if (sscanf(buffer,"static unsigned char %s = {",name) == 1)
        version=11;
      else
        if (sscanf(buffer,"static char %s = {",name) == 1)
          version=11;
        else
          continue;
    p=(unsigned char *) strrchr(name,'_');
    if (p == (unsigned char *) NULL)
      p=(unsigned char *) name;
    else
      p++;
    if (strcmp("bits[]",(char *) p) == 0)
      break;
  }
  if ((image->columns == 0) || (image->rows == 0) || feof(image->file))
    PrematureExit("XBM file is not in the correct format",image);
  /*
    Initialize image structure.
  */
  image->packets=image->columns*image->rows;
  image->class=PseudoClass;
  image->colors=2;
  image->colormap=(ColorPacket *) malloc(image->colors*sizeof(ColorPacket));
  image->pixels=(RunlengthPacket *)
    malloc(image->packets*sizeof(RunlengthPacket));
  padding=0;
  if ((image->columns % 16) && ((image->columns % 16) < 9)  && (version == 10))
    padding=1;
  bytes_per_line=(image->columns+7)/8+padding;
  packets=bytes_per_line*image->rows;
  data=(unsigned char *) malloc(packets*sizeof(unsigned char *));
  if ((image->colormap == (ColorPacket *) NULL) ||
      (image->pixels == (RunlengthPacket *) NULL) ||
      (data == (unsigned char *) NULL))
    PrematureExit("Unable to allocate memory",image);
  /*
    Initialize colormap.
  */
  image->colormap[0].red=0;
  image->colormap[0].green=0;
  image->colormap[0].blue=0;
  image->colormap[1].red=MaxRGB;
  image->colormap[1].green=MaxRGB;
  image->colormap[1].blue=MaxRGB;
  /*
    Initialize hex values.
  */
  hex_digits['0']=0;
  hex_digits['1']=1;
  hex_digits['2']=2;
  hex_digits['3']=3;
  hex_digits['4']=4;
  hex_digits['5']=5;
  hex_digits['6']=6;
  hex_digits['7']=7;
  hex_digits['8']=8;
  hex_digits['9']=9;
  hex_digits['A']=10;
  hex_digits['B']=11;
  hex_digits['C']=12;
  hex_digits['D']=13;
  hex_digits['E']=14;
  hex_digits['F']=15;
  hex_digits['a']=10;
  hex_digits['b']=11;
  hex_digits['c']=12;
  hex_digits['d']=13;
  hex_digits['e']=14;
  hex_digits['f']=15;
  hex_digits['x']=0;
  hex_digits[' ']=(-1);
  hex_digits[',']=(-1);
  hex_digits['}']=(-1);
  hex_digits['\n']=(-1);
  hex_digits['\t']=(-1);
  /*
    Read hex image data.
  */
  p=data;
  if (version == 10)
    for (x=0; x < packets; (x+=2))
    {
      value=XBMInteger(image->file,hex_digits);
      *p++=value;
      if (!padding || ((x+2) % bytes_per_line))
        *p++=value >> 8;
    }
  else
    for (x=0; x < packets; x++)
    {
      value=XBMInteger(image->file,hex_digits);
      *p++=value;
    }
  /*
    Convert X bitmap image to runlength-encoded packets.
  */
  byte=0;
  p=data;
  q=image->pixels;
  for (y=0; y < image->rows; y++)
  {
    bit=0;
    for (x=0; x < image->columns; x++)
    {
      if (bit == 0)
        byte=(*p++);
      q->index=byte & 0x01 ? 0 : 1;
      q->length=0;
      q++;
      bit++;
      byte>>=1;
      if (bit == 8)
        bit=0;
    }
    ProgressMonitor(LoadImageText,y,image->rows);
  }
  SyncImage(image);
  CompressImage(image);
  CloseImage(image);
  free((char *) data);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d X C I m a g e                                                      %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadXCImage creates a constant image and initializes it to the
%  X server color as specified by the filename.  It allocates the memory
%  necessary for the new Image structure and returns a pointer to the new
%  image.
%
%  The format of the ReadXCImage routine is:
%
%      image=ReadXCImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadXCImage returns a pointer to the image after
%      creating it. A null image is returned if there is a a memory shortage
%      or if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadXCImage(const ImageInfo *image_info)
{
  Image
    *image;

  int
    x,
    y;

  register int
    i;

  register RunlengthPacket
    *q;

  unsigned int
    height,
    width;

  XColor
    color;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Determine width and height, e.g. 640x512.
  */
  width=512;
  height=512;
  if (image_info->size != (char *) NULL)
    (void) XParseGeometry(image_info->size,&x,&y,&width,&height);
  /*
    Initialize Image structure.
  */
  (void) strcpy(image->filename,image_info->filename);
  image->columns=width;
  image->rows=height;
  image->packets=((Max(image->columns*image->rows,1)-1)/(MaxRunlength+1))+1;
  image->pixels=(RunlengthPacket *)
    malloc(image->packets*sizeof(RunlengthPacket));
  image->class=PseudoClass;
  image->colors=1;
  image->colormap=(ColorPacket *) malloc(image->colors*sizeof(ColorPacket));
  if ((image->pixels == (RunlengthPacket *) NULL) ||
      (image->colormap == (ColorPacket *) NULL))
    PrematureExit("Unable to allocate memory",image);
  /*
    Initialize colormap.
  */
  (void) XQueryColorDatabase(image_info->filename,&color);
  image->colormap[0].red=XDownScale(color.red);
  image->colormap[0].green=XDownScale(color.green);
  image->colormap[0].blue=XDownScale(color.blue);
  q=image->pixels;
  for (i=0; i < image->packets; i++)
  {
    q->index=0;
    q->length=MaxRunlength;
    q++;
    if (QuantumTick(i,image))
      ProgressMonitor(LoadImageText,i,image->packets);
  }
  q--;
  q->length=image->columns*image->rows-(MaxRunlength+1)*(image->packets-1)-1;
  SyncImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d X P M I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadXPMImage reads an X11 pixmap image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadXPMImage routine is:
%
%      image=ReadXPMImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadXPMImage returns a pointer to the image after
%      creating it. A null image is returned if there is a a memory shortage
%      or if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/

static char *ParseColor(char *data)
{
#define NumberTargets  6

  static char
    *targets[NumberTargets] = { "c ", "g ", "g4 ", "m ", "b ", "s " };

  register char
     *p,
     *q,
     *r;

  register int
    i;

  for (i=0; i < NumberTargets; i++)
  {
    r=data;
    for (q=targets[i]; *r != '\0'; r++)
    {
      if (*r != *q)
        continue;
      if (!isspace(*(r-1)))
        continue;
      p=r;
      for ( ; ; )
      {
        if (*q == '\0')
          return(r);
        if (*p++ != *q++)
          break;
      }
      q=targets[i];
    }
  }
  return((char *) NULL);
}

static Image *ReadXPMImage(const ImageInfo *image_info)
{
  char
    key[MaxTextExtent],
    target[MaxTextExtent],
    **textlist,
    *xpm_buffer;

  Image
    *image;

  int
    count,
    length,
    x,
    y;

  register char
    *p,
    *q;

  register int
    i,
    j;

  register RunlengthPacket
    *r;

  unsigned int
    width;

  XColor
    color;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,"r");
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
    Read XPM file.
  */
  length=MaxTextExtent;
  xpm_buffer=(char *) malloc(length*sizeof(char));
  p=xpm_buffer;
  if (xpm_buffer != (char *) NULL)
    while (fgets(p,MaxTextExtent-1,image->file) != (char *) NULL)
    {
      if (*p == '#')
        continue;
      if ((*p == '}') && (*(p+1) == ';'))
        break;
      p+=Extent(p);
      if ((p-xpm_buffer+MaxTextExtent) < length)
        continue;
      *p='\0';
      length<<=1;
      xpm_buffer=(char *) realloc((char *) xpm_buffer,length*sizeof(char));
      if (xpm_buffer == (char *) NULL)
        break;
      p=xpm_buffer+Extent(xpm_buffer);
    }
  if (xpm_buffer == (char *) NULL)
    PrematureExit("Unable to allocate memory",image);
  /*
    Remove comments.
  */
  *p='\0';
  for (p=xpm_buffer; *p != '\0'; p++)
  {
    if ((*p != '/') || (*(p+1) != '*'))
      continue;
    for (q=p+2; *q != '\0'; q++)
      if ((*q == '*') && (*(q+1) == '/'))
        break;
    (void) strcpy(p,q+2);
  }
  /*
    Remove unquoted characters.
  */
  i=0;
  for (p=xpm_buffer; *p != '\0'; p++)
  {
    if (*p != '"')
      continue;
    for (q=p+1; *q != '\0'; q++)
      if (*q == '"')
        break;
    (void) strncpy(xpm_buffer+i,p+1,q-p-1);
    i+=q-p-1;
    xpm_buffer[i++]='\n';
    p+=q-p;
  }
  xpm_buffer[i]='\0';
  textlist=StringToList(xpm_buffer);
  free(xpm_buffer);
  if (textlist == (char **) NULL)
    PrematureExit("Unable to allocate memory",image);
  /*
    Read hints.
  */
  count=sscanf(textlist[0],"%u %u %u %u",&image->columns,&image->rows,
    &image->colors,&width);
  if ((count != 4) || (width > 2) ||
      ((image->columns*image->rows*image->colors) == 0))
    {
      for (i=0; textlist[i] != (char *) NULL; i++)
        free((char *) textlist[i]);
      free((char *) textlist);
      PrematureExit("Not a XPM image file",image);
    }
  /*
    Initialize image structure.
  */
  image->class=PseudoClass;
  image->colormap=(ColorPacket *) malloc(image->colors*sizeof(ColorPacket));
  image->packets=image->columns*image->rows;
  image->pixels=(RunlengthPacket *)
    malloc(image->packets*sizeof(RunlengthPacket));
  if ((image->colormap == (ColorPacket *) NULL) ||
      (image->pixels == (RunlengthPacket *) NULL))
    {
      for (i=0; textlist[i] != (char *) NULL; i++)
        free((char *) textlist[i]);
      free((char *) textlist);
      PrematureExit("Unable to allocate memory",image);
    }
  /*
    Read image colormap.
  */
  i=1;
  for (j=0; j < image->colors; j++)
  {
    p=textlist[i++];
    if (p == (char *) NULL)
      break;
    image->colormap[j].key[width]='\0';
    (void) strncpy(image->colormap[j].key,p,width);
    /*
      Parse color.
    */
    (void) strcpy(target,"gray");
    q=ParseColor(p+width);
    if (q != (char *) NULL)
      {
        while (!isspace(*q) && (*q != '\0'))
          q++;
        (void) strcpy(target,q);
        q=ParseColor(target);
        if (q != (char *) NULL)
          *q='\0';
      }
    Strip(target);
    image->colormap[j].flags=Latin1Compare(target,"none") == 0;
    if (image->colormap[j].flags)
      {
        image->class=DirectClass;
        image->matte=True;
        (void) strcpy(target,"gray");
      }
    (void) XQueryColorDatabase(target,&color);
    image->colormap[j].red=XDownScale(color.red);
    image->colormap[j].green=XDownScale(color.green);
    image->colormap[j].blue=XDownScale(color.blue);
  }
  if (j < image->colors)
    {
      for (i=0; textlist[i] != (char *) NULL; i++)
        free((char *) textlist[i]);
      free((char *) textlist);
      PrematureExit("Corrupt XPM image file",image);
    }
  /*
    Read image pixels.
  */
  j=0;
  key[width]='\0';
  r=image->pixels;
  for (y=0; y < image->rows; y++)
  {
    p=textlist[i++];
    if (p == (char *) NULL)
      break;
    for (x=0; x < image->columns; x++)
    {
      (void) strncpy(key,p,width);
      if (strcmp(key,image->colormap[j].key) != 0)
        for (j=0; j < (image->colors-1); j++)
          if (strcmp(key,image->colormap[j].key) == 0)
            break;
      r->red=image->colormap[j].red;
      r->green=image->colormap[j].green;
      r->blue=image->colormap[j].blue;
      if (image->class == PseudoClass)
        r->index=j;
      else
        if (image->colormap[j].flags)
          r->index=Transparent;
        else
          r->index=Opaque;
      r->length=0;
      r++;
      p+=width;
    }
  }
  if (y < image->rows)
    {
      for (i=0; textlist[i] != (char *) NULL; i++)
        free((char *) textlist[i]);
      free((char *) textlist);
      PrematureExit("Corrupt XPM image file",image);
    }
  /*
    Free resources.
  */
  for (i=0; textlist[i] != (char *) NULL; i++)
    free((char *) textlist[i]);
  free((char *) textlist);
  CompressImage(image);
  CloseImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d X W D I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadXWDImage reads an X Window System window dump image file and
%  returns it.  It allocates the memory necessary for the new Image structure
%  and returns a pointer to the new image.
%
%  The format of the ReadXWDImage routine is:
%
%      image=ReadXWDImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadXWDImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadXWDImage(const ImageInfo *image_info)
{
  Image
    *image;

  int
    status,
    x,
    y;

  register int
    i;

  register RunlengthPacket
    *q;

  register unsigned long
    pixel;

  unsigned long
    lsb_first;

  unsigned int
    packets;

  unsigned short
    index;

  XColor
    *colors;

  XImage
    *ximage;

  XWDFileHeader
    header;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  OpenImage(image_info,image,ReadBinaryType);
  if (image->file == (FILE *) NULL)
    PrematureExit("Unable to open file",image);
  /*
     Read in header information.
  */
  status=ReadData((char *) &header,sz_XWDheader,1,image->file);
  if (status == False)
    PrematureExit("Unable to read dump file header",image);
  /*
    Ensure the header byte-order is most-significant byte first.
  */
  lsb_first=1;
  if (*(char *) &lsb_first)
    MSBFirstOrderLong((char *) &header,sz_XWDheader);
  /*
    Check to see if the dump file is in the proper format.
  */
  if (header.file_version != XWD_FILE_VERSION)
    PrematureExit("XWD file format version mismatch",image);
  if (header.header_size < sz_XWDheader)
    PrematureExit("XWD header size is too small",image);
  packets=(header.header_size-sz_XWDheader);
  image->comments=(char *) malloc((packets+1)*sizeof(char));
  if (image->comments == (char *) NULL)
    PrematureExit("Unable to allocate memory",image);
  status=ReadData((char *) image->comments,1,packets,image->file);
  image->comments[packets]='\0';
  if (status == False)
    PrematureExit("Unable to  read window name from dump file",image);
  /*
    Initialize the X image.
  */
  ximage=(XImage *) malloc(sizeof(XImage));
  if (ximage == (XImage *) NULL)
    PrematureExit("Unable to allocate memory",image);
  ximage->depth=header.pixmap_depth;
  ximage->format=header.pixmap_format;
  ximage->xoffset=header.xoffset;
  ximage->data=(char *) NULL;
  ximage->width=header.pixmap_width;
  ximage->height=header.pixmap_height;
  ximage->bitmap_pad=header.bitmap_pad;
  ximage->bytes_per_line=header.bytes_per_line;
  ximage->byte_order=header.byte_order;
  ximage->bitmap_unit=header.bitmap_unit;
  ximage->bitmap_bit_order=header.bitmap_bit_order;
  ximage->bits_per_pixel=header.bits_per_pixel;
  ximage->red_mask=header.red_mask;
  ximage->green_mask=header.green_mask;
  ximage->blue_mask=header.blue_mask;
  status=XInitImage(ximage);
  if (status == False)
    PrematureExit("Invalid XWD header",image);
  /*
    Read colormap.
  */
  colors=(XColor *) NULL;
  if (header.ncolors != 0)
    {
      XWDColor
        color;

      colors=(XColor *) malloc((unsigned int) header.ncolors*sizeof(XColor));
      if (colors == (XColor *) NULL)
        PrematureExit("Unable to allocate memory",image);
      for (i=0; i < header.ncolors; i++)
      {
        status=ReadData((char *) &color,sz_XWDColor,1,image->file);
        if (status == False)
          PrematureExit("Unable to read color map from dump file",image);
        colors[i].pixel=color.pixel;
        colors[i].red=color.red;
        colors[i].green=color.green;
        colors[i].blue=color.blue;
        colors[i].flags=color.flags;
      }
      /*
        Ensure the header byte-order is most-significant byte first.
      */
      lsb_first=1;
      if (*(char *) &lsb_first)
        for (i=0; i < header.ncolors; i++)
        {
          MSBFirstOrderLong((char *) &colors[i].pixel,sizeof(unsigned long));
          MSBFirstOrderShort((char *) &colors[i].red,3*sizeof(unsigned short));
        }
    }
  /*
    Allocate the pixel buffer.
  */
  if (ximage->format == ZPixmap)
    packets=ximage->bytes_per_line*ximage->height;
  else
    packets=ximage->bytes_per_line*ximage->height*ximage->depth;
  ximage->data=(char *) malloc(packets*sizeof(unsigned char));
  if (ximage->data == (char *) NULL)
    PrematureExit("Unable to allocate memory",image);
  status=ReadData(ximage->data,1,packets,image->file);
  if (status == False)
    PrematureExit("Unable to read dump pixmap",image);
  /*
    Convert image to MIFF format.
  */
  image->columns=ximage->width;
  image->rows=ximage->height;
  if ((colors == (XColor *) NULL) || (ximage->red_mask != 0) ||
      (ximage->green_mask != 0) || (ximage->blue_mask != 0))
    image->class=DirectClass;
  else
    image->class=PseudoClass;
  image->colors=header.ncolors;
  image->packets=0;
  packets=Max((image->columns*image->rows+4) >> 3,1);
  image->pixels=(RunlengthPacket *) malloc(packets*sizeof(RunlengthPacket));
  if (image->pixels == (RunlengthPacket *) NULL)
    PrematureExit("Unable to allocate memory",image);
  q=image->pixels;
  q->length=MaxRunlength;
  switch (image->class)
  {
    case DirectClass:
    default:
    {
      register unsigned long
        color;

      unsigned int
        blue,
        green,
        red;

      unsigned long
        blue_mask,
        blue_shift,
        green_mask,
        green_shift,
        red_mask,
        red_shift;

      /*
        Determine shift and mask for red, green, and blue.
      */
      red_mask=ximage->red_mask;
      red_shift=0;
      while ((red_mask & 0x01) == 0)
      {
        red_mask>>=1;
        red_shift++;
      }
      green_mask=ximage->green_mask;
      green_shift=0;
      while ((green_mask & 0x01) == 0)
      {
        green_mask>>=1;
        green_shift++;
      }
      blue_mask=ximage->blue_mask;
      blue_shift=0;
      while ((blue_mask & 0x01) == 0)
      {
        blue_mask>>=1;
        blue_shift++;
      }
      /*
        Convert X image to DirectClass packets.
      */
      if (image->colors != 0)
        for (y=0; y < image->rows; y++)
        {
          for (x=0; x < image->columns; x++)
          {
            pixel=XGetPixel(ximage,x,y);
            index=(unsigned short) ((pixel >> red_shift) & red_mask);
            red=XDownScale(colors[index].red);
            index=(unsigned short) ((pixel >> green_shift) & green_mask);
            green=XDownScale(colors[index].green);
            index=(unsigned short) ((pixel >> blue_shift) & blue_mask);
            blue=XDownScale(colors[index].blue);
            if ((red == q->red) && (green == q->green) && (blue == q->blue) &&
                ((int) q->length < MaxRunlength))
              q->length++;
            else
              {
                if (image->packets != 0)
                  q++;
                image->packets++;
                if (image->packets == packets)
                  {
                    packets<<=1;
                    image->pixels=(RunlengthPacket *) realloc((char *)
                      image->pixels,packets*sizeof(RunlengthPacket));
                    if (image->pixels == (RunlengthPacket *) NULL)
                      PrematureExit("Unable to allocate memory",image);
                    q=image->pixels+image->packets-1;
                  }
                q->red=red;
                q->green=green;
                q->blue=blue;
                q->index=0;
                q->length=0;
              }
          }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
      else
        for (y=0; y < image->rows; y++)
        {
          for (x=0; x < image->columns; x++)
          {
            pixel=XGetPixel(ximage,x,y);
            color=(pixel >> red_shift) & red_mask;
            red=XDownScale((color*65535L)/red_mask);
            color=(pixel >> green_shift) & green_mask;
            green=XDownScale((color*65535L)/green_mask);
            color=(pixel >> blue_shift) & blue_mask;
            blue=XDownScale((color*65535L)/blue_mask);
            if ((red == q->red) && (green == q->green) && (blue == q->blue) &&
                ((int) q->length < MaxRunlength))
              q->length++;
            else
              {
                if (image->packets != 0)
                  q++;
                image->packets++;
                if (image->packets == packets)
                  {
                    packets<<=1;
                    image->pixels=(RunlengthPacket *) realloc((char *)
                      image->pixels,packets*sizeof(RunlengthPacket));
                    if (image->pixels == (RunlengthPacket *) NULL)
                      PrematureExit("Unable to allocate memory",image);
                    q=image->pixels+image->packets-1;
                  }
                q->red=red;
                q->green=green;
                q->blue=blue;
                q->index=0;
                q->length=0;
              }
          }
          ProgressMonitor(LoadImageText,y,image->rows);
        }
      break;
    }
    case PseudoClass:
    {
      /*
        Convert X image to PseudoClass packets.
      */
      image->colormap=(ColorPacket *) malloc(image->colors*sizeof(ColorPacket));
      if (image->colormap == (ColorPacket *) NULL)
        PrematureExit("Unable to allocate memory",image);
      for (i=0; i < image->colors; i++)
      {
        image->colormap[i].red=XDownScale(colors[i].red);
        image->colormap[i].green=XDownScale(colors[i].green);
        image->colormap[i].blue=XDownScale(colors[i].blue);
      }
      for (y=0; y < image->rows; y++)
      {
        for (x=0; x < image->columns; x++)
        {
          pixel=XGetPixel(ximage,x,y);
          index=(unsigned short) pixel;
          if ((index == q->index) && ((int) q->length < MaxRunlength))
            q->length++;
          else
            {
              if (image->packets != 0)
                q++;
              image->packets++;
              if (image->packets == packets)
                {
                  packets<<=1;
                  image->pixels=(RunlengthPacket *) realloc((char *)
                    image->pixels,packets*sizeof(RunlengthPacket));
                  if (image->pixels == (RunlengthPacket *) NULL)
                    PrematureExit("Unable to allocate memory",image);
                  q=image->pixels+image->packets-1;
                }
              q->index=index;
              q->length=0;
            }
        }
        ProgressMonitor(LoadImageText,y,image->rows);
      }
      SyncImage(image);
      CompressColormap(image);
      break;
    }
  }
  image->pixels=(RunlengthPacket *)
    realloc((char *) image->pixels,image->packets*sizeof(RunlengthPacket));
  /*
    Free image and colormap.
  */
  if (header.ncolors != 0)
    free((char *) colors);
  free(ximage->data);
  free(ximage);
  CloseImage(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  R e a d Y U V I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadYUVImage reads an image with digital YUV (CCIR 601 4:1:1) bytes
%  and returns it.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  The format of the ReadYUVImage routine is:
%
%      image=ReadYUVImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Function ReadYUVImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *ReadYUVImage(const ImageInfo *image_info)
{
  char
    filename[MaxTextExtent];

  Image
    *image,
    *zoomed_image;

  int
    x,
    y;

  register int
    i;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  unsigned char
    *uv_pixels,
    *y_pixels;

  unsigned int
    height,
    width;

  /*
    Allocate image structure.
  */
  zoomed_image=AllocateImage(image_info);
  if (zoomed_image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Determine width and height, e.g. 640x512.
  */
  width=512;
  height=512;
  x=0;
  if (image_info->size != (char *) NULL)
    (void) XParseGeometry(image_info->size,&x,&y,&width,&height);
  if (image_info->interlace != PartitionInterlace)
    {
      /*
        Open image file.
      */
      OpenImage(image_info,zoomed_image,ReadBinaryType);
      if (zoomed_image->file == (FILE *) NULL)
        PrematureExit("Unable to open file",zoomed_image)
      for (i=0; i < x; i++)
        (void) fgetc(zoomed_image->file);
    }
  /*
    Read Y channel.
  */
  if (image_info->interlace == PartitionInterlace)
    {
      AppendImageFormat("Y",zoomed_image->filename);
      OpenImage(image_info,zoomed_image,ReadBinaryType);
      if (zoomed_image->file == (FILE *) NULL)
        PrematureExit("Unable to open file",zoomed_image);
    }
  zoomed_image->columns=width >> 1;
  zoomed_image->rows=height >> 1;
  zoomed_image->packets=zoomed_image->columns*zoomed_image->rows;
  uv_pixels=(unsigned char *)
    malloc(zoomed_image->packets*sizeof(unsigned char));
  y_pixels=(unsigned char *)
    malloc(4*zoomed_image->packets*sizeof(unsigned char));
  zoomed_image->pixels=(RunlengthPacket *)
    malloc(zoomed_image->packets*sizeof(RunlengthPacket));
  if ((uv_pixels == (unsigned char *) NULL) ||
      (y_pixels == (unsigned char *) NULL) ||
      (zoomed_image->pixels == (RunlengthPacket *) NULL))
    PrematureExit("Unable to allocate memory",zoomed_image);
  (void) ReadData((char *) y_pixels,4,zoomed_image->packets,zoomed_image->file);
  /*
    Read U channel.
  */
  if (image_info->interlace == PartitionInterlace)
    {
      CloseImage(zoomed_image);
      AppendImageFormat("U",zoomed_image->filename);
      OpenImage(image_info,zoomed_image,ReadBinaryType);
      if (zoomed_image->file == (FILE *) NULL)
        PrematureExit("Unable to open file",zoomed_image);
    }
  ProgressMonitor(LoadImageText,100,400);
  (void) ReadData((char *) uv_pixels,1,zoomed_image->packets,
    zoomed_image->file);
  p=uv_pixels;
  q=zoomed_image->pixels;
  for (i=0; i < zoomed_image->packets; i++)
  {
    q->green=UpScale(*p);
    q->index=0;
    q->length=0;
    p++;
    q++;
  }
  /*
    Read V channel.
  */
  if (image_info->interlace == PartitionInterlace)
    {
      CloseImage(zoomed_image);
      AppendImageFormat("V",zoomed_image->filename);
      OpenImage(image_info,zoomed_image,ReadBinaryType);
      if (zoomed_image->file == (FILE *) NULL)
        PrematureExit("Unable to open file",zoomed_image);
    }
  ProgressMonitor(LoadImageText,200,400);
  (void) ReadData((char *) uv_pixels,1,zoomed_image->packets,
    zoomed_image->file);
  p=uv_pixels;
  q=zoomed_image->pixels;
  for (i=0; i < zoomed_image->packets; i++)
  {
    q->blue=UpScale(*p);
    p++;
    q++;
  }
  CloseImage(zoomed_image);
  free((char *) uv_pixels);
  /*
    Scale image.
  */
  ProgressMonitor(LoadImageText,300,400);
  zoomed_image->orphan=True;
  image=MagnifyImage(zoomed_image);
  zoomed_image->orphan=False;
  DestroyImage(zoomed_image);
  if (image == (Image *) NULL)
    PrematureExit("Unable to allocate memory",image);
  p=y_pixels;
  q=image->pixels;
  for (i=0; i < (image->columns*image->rows); i++)
  {
    q->red=UpScale(*p);
    p++;
    q++;
  }
  free((char *) y_pixels);
  TransformRGBImage(image,YCbCrColorspace);
  (void) strcpy(image->filename,filename);
  CompressImage(image);
  if (image_info->interlace == PartitionInterlace)
    (void) strcpy(image->filename,image_info->filename);
  ProgressMonitor(LoadImageText,400,400);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e a d I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ReadImage reads an image and returns it.  It allocates
%  the memory necessary for the new Image structure and returns a pointer to
%  the new image.  By default, the image format is determined by its magic
%  number. To specify a particular image format, precede the filename with an
%  explicit image format name and a colon (i.e.  ps:image) or as the filename
%  suffix  (i.e. image.ps).
%
%  The format of the ReadImage routine is:
%
%      image=ReadImage(image_info)
%
%  A description of each parameter follows:
%
%    o image: Function ReadImage returns a pointer to the image after
%      reading.  A null image is returned if there is a a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadImage(ImageInfo *image_info)
{
  char
    magic_number[MaxTextExtent];

  Image
    decode_image,
    *image;

  ImageInfo
    decode_info;

  register char
    *p;

  register int
    i;

  assert(image_info != (ImageInfo *) NULL);
  assert(image_info->filename != (char *) NULL);
  SetImageInfo(image_info,False);
  decode_info=(*image_info);
  decode_image.temporary=strcmp(decode_info.magick,"TMP") == 0;
  if ((strncmp(decode_info.magick,"FILE",4) == 0) ||
      (strncmp(decode_info.magick,"FTP",3) == 0) ||
      (strncmp(decode_info.magick,"GOPHER",6) == 0) ||
      (strncmp(decode_info.magick,"HTTP",4) == 0))
    {
      char
        command[MaxTextExtent],
        filename[MaxTextExtent];

      /*
        Retrieve image as specified with a WWW uniform resource locator.
      */
      decode_image.temporary=True;
      TemporaryFilename(filename);
      (void) sprintf(command,WWWCommand,decode_info.magick,decode_info.filename,
        filename);
      (void) SystemCommand(command);
      SetImageInfo(&decode_info,False);
      (void) strcpy(decode_info.filename,filename);
    }
  if (!decode_info.affirm || (strncmp(decode_info.magick,"SGI",3) == 0) ||
      (strncmp(decode_info.magick,"PCD",3) == 0))
    {
      /*
        Determine type from image magic number.
      */
      for (i=0 ; i < sizeof(magic_number); i++)
        magic_number[i]='\0';
      (void) strcpy(decode_image.filename,decode_info.filename);
      OpenImage(image_info,&decode_image,ReadBinaryType);
      if (decode_image.file != (FILE *) NULL)
        if ((decode_image.file == stdin) || decode_image.pipe)
          {
            FILE
              *file;

            int
              c;

            /*
              Copy standard input or pipe to temporary file.
            */
            decode_image.temporary=True;
            TemporaryFilename(decode_image.filename);
            decode_info.filename=decode_image.filename;
            file=fopen(decode_image.filename,WriteBinaryType);
            if (file == (FILE *) NULL)
              {
                Warning("Unable to write file",decode_info.filename);
                return((Image *) NULL);
              }
            c=fgetc(decode_image.file);
            while (c != EOF)
            {
              (void) putc(c,file);
              c=fgetc(decode_image.file);
            }
            (void) fclose(file);
            CloseImage(&decode_image);
            OpenImage(image_info,&decode_image,ReadBinaryType);
          }
      if (decode_image.file != (FILE *) NULL)
        {
          /*
            Read magic number.
          */
          (void) ReadData(magic_number,(unsigned int) sizeof(char),
            (unsigned int) sizeof(magic_number),decode_image.file);
          if (((unsigned char) magic_number[0] == 0xff) &&
              ((unsigned char) magic_number[1] == 0xff))
            {
              register int
                i;

              /*
                For PCD image type, skip to byte 2048.
              */
              for (i=0; i < (int) (0x800-sizeof(magic_number)); i++)
                (void) fgetc(decode_image.file);
              (void) ReadData(magic_number,(unsigned int) sizeof(char),
                (unsigned int) sizeof(magic_number),decode_image.file);
            }
          CloseImage(&decode_image);
        }
      /*
        Determine the image format.
      */
      magic_number[MaxTextExtent-1]='\0';
      if (strncmp(magic_number,"BM",2) == 0)
        (void) strcpy(decode_info.magick,"BMP");
      if (strncmp(magic_number,"IT0",3) == 0)
        (void) strcpy(decode_info.magick,"FITS");
      if (strncmp(magic_number,"\261\150\336\72",4) == 0)
        (void) strcpy(decode_info.magick,"DCX");
      if (strncmp(magic_number,"SIMPLE",6) == 0)
        (void) strcpy(decode_info.magick,"FITS");
      if (strncmp(magic_number,"GIF8",4) == 0)
        (void) strcpy(decode_info.magick,"GIF");
      if (strncmp(magic_number,"\016\003\023\001",4) == 0)
        (void) strcpy(decode_info.magick,"HDF");
      if ((strncmp(magic_number,"<HTML",5) == 0) ||
          (strncmp(magic_number,"<html",5) == 0))
        (void) strcpy(decode_info.magick,"HTML");
      if (strncmp(magic_number,"\001\332",2) == 0)
        (void) strcpy(decode_info.magick,"SGI");
      if (strncmp(magic_number,"\377\330\377",3) == 0)
        (void) strcpy(decode_info.magick,"JPEG");
      if (strncmp(magic_number,"id=ImageMagick",14) == 0)
        (void) strcpy(decode_info.magick,"MIFF");
      if ((magic_number[0] == 0x00) && (magic_number[1] == 0x00))
        if ((magic_number[2] == 0x01) && (magic_number[3] == (char) 0xb3))
          (void) strcpy(decode_info.magick,"MPEG");
      if (strncmp(magic_number,"PCD_",4) == 0)
        (void) strcpy(decode_info.magick,"PCD");
      if (strncmp(magic_number,"\12\2",2) == 0)
        (void) strcpy(decode_info.magick,"PCX");
      if (strncmp(magic_number,"\12\5",2) == 0)
        (void) strcpy(decode_info.magick,"PCX");
      if (strncmp(magic_number,"%!PDF",5) == 0)
        (void) strcpy(decode_info.magick,"PDF");
      if ((*magic_number == 'P') && isdigit(magic_number[1]))
        (void) strcpy(decode_info.magick,"PNM");
      if (strncmp(magic_number,"\211PNG\r\n\032\n",8) == 0)
        (void) strcpy(decode_info.magick,"PNG");
      if (strncmp(magic_number,"%!",2) == 0)
        (void) strcpy(decode_info.magick,"PS");
      if (strncmp(magic_number,"#?RADIANCE",10) == 0)
        (void) strcpy(decode_info.magick,"RAD");
      if (strncmp(magic_number,"\122\314",2) == 0)
        (void) strcpy(decode_info.magick,"RLE");
      if (strncmp(magic_number,"\131\246\152\225",4) == 0)
        (void) strcpy(decode_info.magick,"SUN");
      if ((strncmp(magic_number,"\115\115\000\052",4) == 0) ||
          (strncmp(magic_number,"\111\111\052\000",4) == 0))
        (void) strcpy(decode_info.magick,"TIFF");
      if ((strncmp(magic_number,"LBLSIZE",7) == 0) ||
          (strncmp(magic_number,"NJPL1I",6) == 0))
        (void) strcpy(decode_info.magick,"VICAR");
      if (strncmp(magic_number,"\253\1",2) == 0)
        (void) strcpy(decode_info.magick,"VIFF");
      p=strchr(magic_number,'#');
      if (p != (char *) NULL)
        if (strncmp(p,"#define",7) == 0)
          (void) strcpy(decode_info.magick,"XBM");
      if (strncmp(magic_number,"/* XPM */",9) == 0)
        (void) strcpy(decode_info.magick,"XPM");
      if ((magic_number[1] == 0x00) && (magic_number[2] == 0x00))
        if ((magic_number[5] == 0x00) && (magic_number[6] == 0x00))
          if ((magic_number[4] == 0x07) || (magic_number[7] == 0x07))
            (void) strcpy(decode_info.magick,"XWD");
    }
  /*
    Call appropriate image reader based on image type.
  */
  switch (*decode_info.magick)
  {
    case 'A':
    {
      if (strcmp(decode_info.magick,"AVS") == 0)
        {
          image=ReadAVSImage(&decode_info);
          break;
        }
      image=ReadMIFFImage(&decode_info);
      break;
    }
    case 'B':
    {
      if (strcmp(decode_info.magick,"BIE") == 0)
        {
          image=ReadJBIGImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"BMP") == 0)
        {
          image=ReadBMPImage(&decode_info);
          break;
        }
      image=ReadMIFFImage(&decode_info);
      break;
    }
    case 'C':
    {
      if (strcmp(decode_info.magick,"CGM") == 0)
        {
          image=ReadCGMImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"CMYK") == 0)
        {
          image=ReadCMYKImage(&decode_info);
          break;
        }
      image=ReadMIFFImage(&decode_info);
      break;
    }
    case 'D':
    {
      if (strncmp(decode_info.magick,"DCX",3) == 0)
        {
          image=ReadPCXImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"DIB") == 0)
        {
          image=ReadBMPImage(&decode_info);
          break;
        }
      image=ReadMIFFImage(&decode_info);
      break;
    }
    case 'E':
    {
      if (strncmp(decode_info.magick,"EPS",3) == 0)
        {
          image=ReadPSImage(&decode_info);
          break;
        }
      image=ReadMIFFImage(&decode_info);
      break;
    }
    case 'F':
    {
      if (strcmp(decode_info.magick,"FAX") == 0)
        {
          image=ReadFAXImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"FITS") == 0)
        {
          image=ReadFITSImage(&decode_info);
          break;
        }
      image=ReadMIFFImage(&decode_info);
      break;
    }
    case 'G':
    {
      if (strncmp(decode_info.magick,"GIF",3) == 0)
        {
          image=ReadGIFImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"GRADATION") == 0)
        {
          image=ReadGRADATIONImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"GRANITE") == 0)
        {
          image=ReadLOGOImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"GRAY") == 0)
        {
          image=ReadGRAYImage(&decode_info);
          break;
        }
      if (strncmp(decode_info.magick,"G3",2) == 0)
        {
          image=ReadFAXImage(&decode_info);
          break;
        }
      image=ReadMIFFImage(&decode_info);
      break;
    }
    case 'H':
    {
      if (strcmp(decode_info.magick,"H") == 0)
        {
          image=ReadLOGOImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"HDF") == 0)
        {
          image=ReadHDFImage(&decode_info);
          break;
        }
      if (strncmp(decode_info.magick,"HISTOGRAM",4) == 0)
        {
          image=ReadHISTOGRAMImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"HTML") == 0)
        {
          image=ReadHTMLImage(&decode_info);
          break;
        }
      image=ReadMIFFImage(&decode_info);
      break;
    }
    case 'J':
    {
      if (strcmp(decode_info.magick,"JBIG") == 0)
        {
          image=ReadJBIGImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"JPEG") == 0)
        {
          image=ReadJPEGImage(&decode_info);
          break;
        }
      image=ReadMIFFImage(&decode_info);
      break;
    }
    case 'L':
    {
      if (strcmp(decode_info.magick,"LOGO") == 0)
        {
          image=ReadLOGOImage(&decode_info);
          break;
        }
      image=ReadMIFFImage(&decode_info);
      break;
    }
    case 'M':
    {
      if (strcmp(decode_info.magick,"MAP") == 0)
        {
          image=ReadMAPImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"MATTE") == 0)
        {
          image=ReadMATTEImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"MONO") == 0)
        {
          image=ReadMONOImage(&decode_info);
          break;
        }
      if ((strcmp(decode_info.magick,"MPEG") == 0) ||
          (strcmp(decode_info.magick,"MPG") == 0))
        {
          image=ReadMPEGImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"MTV") == 0)
        {
          image=ReadMTVImage(&decode_info);
          break;
        }
      image=ReadMIFFImage(&decode_info);
      break;
    }
    case 'N':
    {
      if (strcmp(decode_info.magick,"NETSCAPE") == 0)
        {
          image=ReadLOGOImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"NULL") == 0)
        {
          image=ReadNULLImage(&decode_info);
          break;
        }
      image=ReadMIFFImage(&decode_info);
      break;
    }
    case 'P':
    {
      if ((strcmp(decode_info.magick,"PBM") == 0) ||
          (strcmp(decode_info.magick,"PGM") == 0) ||
          (strcmp(decode_info.magick,"PNM") == 0) ||
          (strcmp(decode_info.magick,"PPM") == 0))
        {
          image=ReadPNMImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"PCD") == 0)
        {
          image=ReadPCDImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"PCL") == 0)
        {
          image=ReadPCLImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"PCX") == 0)
        {
          image=ReadPCXImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"PDF") == 0)
        {
          image=ReadPDFImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"PICT") == 0)
        {
          image=ReadPICTImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"PCD") == 0)
        {
          image=ReadPCDImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"PLASMA") == 0)
        {
          image=ReadPLASMAImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"PM") == 0)
        {
          image=ReadXPMImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"PNG") == 0)
        {
          image=ReadPNGImage(&decode_info);
          break;
        }
      if (strncmp(decode_info.magick,"PS",2) == 0)
        {
          image=ReadPSImage(&decode_info);
          break;
        }
      image=ReadMIFFImage(&decode_info);
      break;
    }
    case 'R':
    {
      if (strcmp(decode_info.magick,"RAD") == 0)
        {
          image=ReadRADIANCEImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"RAS") == 0)
        {
          image=ReadSUNImage(&decode_info);
          break;
        }
      if (strncmp(decode_info.magick,"RGB",3) == 0)
        {
          image=ReadRGBImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"RLA") == 0)
        {
          image=ReadRLAImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"RLE") == 0)
        {
          image=ReadRLEImage(&decode_info);
          break;
        }
      image=ReadMIFFImage(&decode_info);
      break;
    }
    case 'S':
    {
      if (strcmp(decode_info.magick,"SGI") == 0)
        {
          image=ReadSGIImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"SHTML") == 0)
        {
          image=ReadHTMLImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"SUN") == 0)
        {
          image=ReadSUNImage(&decode_info);
          break;
        }
      image=ReadMIFFImage(&decode_info);
      break;
    }
    case 'T':
    {
      if (strcmp(decode_info.magick,"TEXT") == 0)
        {
          image=ReadTEXTImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"TGA") == 0)
        {
          image=ReadTGAImage(&decode_info);
          break;
        }
      if (strncmp(decode_info.magick,"TIF",3) == 0)
        {
          image=ReadTIFFImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"TILE") == 0)
        {
          image=ReadTILEImage(&decode_info);
          break;
        }
      image=ReadMIFFImage(&decode_info);
      break;
    }
    case 'U':
    {
      if (strcmp(decode_info.magick,"UYVY") == 0)
        {
          image=ReadUYVYImage(&decode_info);
          break;
        }
      image=ReadMIFFImage(&decode_info);
      break;
    }
    case 'V':
    {
      if (strcmp(decode_info.magick,"VICAR") == 0)
        {
          image=ReadVICARImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"VID") == 0)
        {
          if (decode_info.affirm)
            image=ReadVIDImage(&decode_info);
          else
            image=ReadMIFFImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"VIFF") == 0)
        {
          image=ReadVIFFImage(&decode_info);
          break;
        }
      image=ReadMIFFImage(&decode_info);
      break;
    }
    case 'X':
    {
      if (strcmp(decode_info.magick,"X") == 0)
        {
          image=ReadXImage(&decode_info,False,False,False,False);
          break;
        }
      if (strcmp(decode_info.magick,"XC") == 0)
        {
          image=ReadXCImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"XBM") == 0)
        {
          image=ReadXBMImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"XPM") == 0)
        {
          image=ReadXPMImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"XV") == 0)
        {
          image=ReadVIFFImage(&decode_info);
          break;
        }
      if (strcmp(decode_info.magick,"XWD") == 0)
        {
          image=ReadXWDImage(&decode_info);
          break;
        }
      image=ReadMIFFImage(&decode_info);
      break;
    }
    case 'Y':
    {
      if (strcmp(decode_info.magick,"YUV") == 0)
        {
          image=ReadYUVImage(&decode_info);
          break;
        }
      image=ReadMIFFImage(&decode_info);
      break;
    }
    default:
    {
      (void) strcpy(decode_info.magick,"MIFF");
      image=ReadMIFFImage(&decode_info);
    }
  }
  if (decode_image.temporary)
    (void) remove(decode_info.filename);
  if (image != (Image *) NULL)
    if ((image->next != (Image *) NULL) && (image_info->tile != (char *) NULL))
      {
        int
          count,
          offset,
          retain;

        /*
          User specified subimages (e.g. image.miff[1,3,4]).
        */
        for ( ; ; )
        {
          retain=False;
          image_info->subimage=atoi(image_info->tile);
          for (p=image_info->tile; *p != '\0'; p+=Max(offset,1))
          {
            offset=0;
            count=sscanf(p,"%u%n-%u%n",&image_info->subimage,&offset,
              &image_info->subrange,&offset);
            if (count == 1)
              image_info->subrange=image_info->subimage;
            retain|=((image->scene >= image_info->subimage) &&
              (image->scene <= image_info->subrange));
          }
          if (image->next != (Image *) NULL)
            {
              image=image->next;
              if (!retain)
                DestroyImage(image->previous);
              continue;
            }
          if (image->previous != (Image *) NULL)
            {
              image=image->previous;
              if (!retain)
                DestroyImage(image->next);
              break;
            }
          if (!retain)
            {
              DestroyImage(image);
              image=(Image *) NULL;
            }
          break;
        }
        if (image == (Image *) NULL)
          {
            Warning("Subimage specification returns no images",
              image_info->filename);
            return((Image *) NULL);
          }
        while (image->previous != (Image *) NULL)
          image=image->previous;
      }
  if (image != (Image *) NULL)
    {
      if (image->status)
        Warning("An error has occurred reading from file",image->filename);
      if (decode_image.temporary)
        (void) strcpy(image->filename,image_info->filename);
      (void) strcpy(image->magick_filename,image_info->filename);
      if (image->comments == (char *) NULL)
        CommentImage(image,"  Imported from %m image: %f");
      if (image->magick_columns == 0)
        image->magick_columns=image->columns;
      if (image->magick_rows == 0)
        image->magick_rows=image->rows;
      image_info->interlace=decode_info.interlace;
      if (image->class == PseudoClass)
        if (IsMonochromeImage(image))
          {
            image->background_color.red=MaxRGB;
            image->background_color.green=MaxRGB;
            image->background_color.blue=MaxRGB;
          }
    }
  return(image);
}
