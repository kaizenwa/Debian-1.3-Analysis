/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                       IIIII  M   M   AAA   GGGG  EEEEE                      %
%                         I    MM MM  A   A G      E                          %
%                         I    M M M  AAAAA G  GG  EEE                        %
%                         I    M   M  A   A G   G  E                          %
%                       IIIII  M   M  A   A  GGGG  EEEEE                      %
%                                                                             %
%                                                                             %
%                          ImageMagick Image Routines                         %
%                                                                             %
%                                                                             %
%                                                                             %
%                               Software Design                               %
%                                 John Cristy                                 %
%                                  July 1992                                  %
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
%
*/

/*
  Include declarations.
*/
#include "magick.h"
#include "formats.h"
#include "Colorlist.h"

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   A l l o c a t e I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function AllocateImage allocates an Image structure and initializes each
%  field to a default value.
%
%  The format of the AllocateImage routine is:
%
%      allocated_image=AllocateImage(image_info)
%
%  A description of each parameter follows:
%
%    o allocated_image: Function AllocateImage returns a pointer to an image
%      structure initialized to default values.  A null image is returned if
%      there is a memory shortage.
%
%    o image_info: Specifies a pointer to a ImageInfo structure.
%
%
*/
Export Image *AllocateImage(const ImageInfo *image_info)
{
  Image
    *allocated_image;

  XColor
    color;

  /*
    Allocate image structure.
  */
  allocated_image=(Image *) malloc(sizeof(Image));
  if (allocated_image == (Image *) NULL)
    {
      Warning("Unable to allocate image","Memory allocation failed");
      return((Image *) NULL);
    }
  /*
    Initialize Image structure.
  */
  allocated_image->file=(FILE *) NULL;
  allocated_image->status=False;
  allocated_image->temporary=False;
  *allocated_image->filename='\0';
  allocated_image->filesize=0;
  allocated_image->pipe=False;
  (void) strcpy(allocated_image->magick,"MIFF");
  allocated_image->comments=(char *) NULL;
  allocated_image->label=(char *) NULL;
  allocated_image->text=(char *) NULL;
  allocated_image->id=UndefinedId;
  allocated_image->class=DirectClass;
  allocated_image->matte=False;
  allocated_image->compression=RunlengthEncodedCompression;
  allocated_image->columns=0;
  allocated_image->rows=0;
  allocated_image->depth=QuantumDepth;
  allocated_image->interlace=DefaultInterlace;
  allocated_image->scene=0;
  allocated_image->units=UndefinedResolution;
  allocated_image->x_resolution=0.0;
  allocated_image->y_resolution=0.0;
  allocated_image->montage=(char *) NULL;
  allocated_image->directory=(char *) NULL;
  allocated_image->colormap=(ColorPacket *) NULL;
  allocated_image->colorspace=RGBColorspace;
  allocated_image->colors=0;
  allocated_image->gamma=0.0;
  allocated_image->normalized_maximum_error=0.0;
  allocated_image->normalized_mean_error=0.0;
  allocated_image->mean_error_per_pixel=0;
  allocated_image->total_colors=0;
  allocated_image->signature=(char *) NULL;
  allocated_image->pixels=(RunlengthPacket *) NULL;
  allocated_image->packet=(RunlengthPacket *) NULL;
  allocated_image->packets=0;
  allocated_image->packet_size=0;
  allocated_image->packed_pixels=(unsigned char *) NULL;
  *allocated_image->magick_filename='\0';
  allocated_image->magick_columns=0;
  allocated_image->magick_rows=0;
  allocated_image->magick_time=time((time_t *) NULL);
  allocated_image->geometry=(char *) NULL;
  allocated_image->page=(char *) NULL;
  allocated_image->dispose=0;
  allocated_image->delay=0;
  allocated_image->iterations=1;
  (void) XQueryColorDatabase(BackgroundColor,&color);
  allocated_image->background_color.red=XDownScale(color.red);
  allocated_image->background_color.green=XDownScale(color.green);
  allocated_image->background_color.blue=XDownScale(color.blue);
  allocated_image->background_color.index=0;
  (void) XQueryColorDatabase(BorderColor,&color);
  allocated_image->border_color.red=XDownScale(color.red);
  allocated_image->border_color.green=XDownScale(color.green);
  allocated_image->border_color.blue=XDownScale(color.blue);
  allocated_image->border_color.index=0;
  (void) XQueryColorDatabase(MatteColor,&color);
  allocated_image->matte_color.red=XDownScale(color.red);
  allocated_image->matte_color.green=XDownScale(color.green);
  allocated_image->matte_color.blue=XDownScale(color.blue);
  allocated_image->matte_color.index=0;
  if (image_info != (ImageInfo *) NULL)
    {
      (void) strcpy(allocated_image->filename,image_info->filename);
      (void) strcpy(allocated_image->magick,image_info->magick);
    }
  allocated_image->orphan=False;
  allocated_image->previous=(Image *) NULL;
  allocated_image->list=(Image *) NULL;
  allocated_image->next=(Image *) NULL;
  return(allocated_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   A n n o t a t e I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function AnnotateImage annotates an image with test.  Optionally the
%  annotation can include the image filename, type, width, height, or scene
%  number by embedding special format characters.  Embed %f for filename,
%  %m for magick, %w for width, %h for height, %s for scene number, or \n
%  for newline.  For example,
%
%     %f  %wx%h
%
%  produces an image annotation of
%
%     bird.miff  512x480
%
%  for an image titled bird.miff and whose width is 512 and height is 480.
%
%  The format of the AnnotateImage routine is:
%
%      AnnotateImage(image,annotate_info)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%    o annotate_info: The address of a AnnotateInfo structure.
%
%
*/
void AnnotateImage(Image *image,AnnotateInfo *annotate_info)
{
  char
    *text,
    **textlist;

  FILE
    *file;

  int
    flags,
    status,
    x,
    y;

  register char
    *p,
    *q;

  register int
    i,
    j;

  static AnnotateInfo
    cache_info;

  static Display
    *display = (Display *) NULL;

  static unsigned int
    font_type = 0;

  static XAnnotateInfo
    xannotate_info;

  static XFontStruct
    *font_info;

  static XPixelInfo
    pixel_info;

  static XResourceInfo
    resource_info;

  static XrmDatabase
    resource_database;

  static XStandardColormap
    *map_info;

  static XVisualInfo
    *visual_info;

  unsigned int
    height,
    indirection,
    length,
    width;

  /*
    Ensure the annotation info is valid.
  */
  assert(image != (Image *) NULL);
  assert(annotate_info != (AnnotateInfo *) NULL);
  if (annotate_info->text == (char *) NULL)
    return;
  if (*annotate_info->text == '\0')
    return;
  indirection=(*annotate_info->text == '@');
  if (indirection)
    {
      int
        c;

      /*
        Read text from a file.
      */
      file=(FILE *) fopen(annotate_info->text+1,"r");
      if (file == (FILE *) NULL)
        {
          Warning("Unable to read text file",annotate_info->text+1);
          return;
        }
      length=MaxTextExtent;
      annotate_info->text=(char *) malloc(length);
      for (q=annotate_info->text; annotate_info->text != (char *) NULL; q++)
      {
        c=fgetc(file);
        if (c == EOF)
          break;
        if ((q-annotate_info->text+1) >= length)
          {
            *q='\0';
            length<<=1;
            annotate_info->text=(char *) realloc(annotate_info->text,length);
            if (annotate_info->text == (char *) NULL)
              break;
            q=annotate_info->text+Extent(annotate_info->text);
          }
        *q=(unsigned char) c;
      }
      (void) fclose(file);
      if (annotate_info->text == (char *) NULL)
        {
          Warning("Unable to annotate image","Memory allocation failed");
          return;
        }
      *q='\0';
    }
  /*
    Allocate and initialize image text.
  */
  p=annotate_info->text;
  length=Extent(annotate_info->text)+MaxTextExtent;
  image->text=(char *) malloc(length);
  for (q=image->text; image->text != (char *) NULL; p++)
  {
    *q='\0';
    if (*p == '\0')
      break;
    if ((q-image->text+MaxTextExtent) >= length)
      {
        length<<=1;
        image->text=(char *) realloc((char *) image->text,length);
        if (image->text == (char *) NULL)
          break;
        q=image->text+Extent(image->text);
      }
    /*
      Process formatting characters in text.
    */
    if ((*p == '\\') && (*(p+1) == 'n'))
      {
        *q++='\n';
        p++;
        continue;
      }
    if (*p != '%')
      {
        *q++=(*p);
        continue;
      }
    p++;
    switch (*p)
    {
      case 'b':
      {
        (void) sprintf(q,"%ld",image->filesize/1000);
        q=image->text+Extent(image->text);
        break;
      }
      case 'f':
      {
        register char
          *p;

        /*
          Label segment is the base of the filename.
        */
        if (Extent(image->magick_filename) == 0)
          break;
        p=image->magick_filename+Extent(image->magick_filename)-1;
        while ((p > image->magick_filename) && (*(p-1) != '/'))
          p--;
        (void) strcpy(q,p);
        q+=Extent(p);
        break;
      }
      case 'h':
      {
        (void) sprintf(q,"%u",image->magick_rows);
        q=image->text+Extent(image->text);
        break;
      }
      case 'm':
      {
        (void) strcpy(q,image->magick);
        q+=Extent(image->magick);
        break;
      }
      case 's':
      {
        (void) sprintf(q,"%u",image->scene);
        q=image->text+Extent(image->text);
        break;
      }
      case 'w':
      {
        (void) sprintf(q,"%u",image->magick_columns);
        q=image->text+Extent(image->text);
        break;
      }
      default:
      {
        *q++='%';
        *q++=(*p);
        break;
      }
    }
  }
  if (image->text == (char *) NULL)
    {
      Warning("Unable to annotate image","Memory allocation failed");
      return;
    }
  *q++='\0';
  *q++='\0';
  if (indirection)
    free((char *) annotate_info->text);
  textlist=StringToList(image->text);
  free(image->text);
  image->text=(char *) NULL;
  if (textlist == (char **) NULL)
    return;
  length=Extent(textlist[0]);
  for (i=0; textlist[i] != (char *) NULL; i++)
    if (Extent(textlist[i]) > length)
      length=Extent(textlist[i]);
  text=(char *) malloc(length+4);
  if (text == (char *) NULL)
    {
      Warning("Unable to annotate image","Memory allocation error");
      return;
    }
  /*
    Get annotate geometry.
  */
  x=0;
  y=0;
  width=image->columns;
  height=annotate_info->pointsize;
  flags=NoValue;
  if (annotate_info->geometry != (char *) NULL)
    {
      flags=XParseGeometry(annotate_info->geometry,&x,&y,&width,&height);
      if ((flags & XNegative) != 0)
        x+=image->columns;
      if ((flags & YNegative) != 0)
        y+=image->rows;
    }
  switch (font_type)
  {
    case 0:
    default:
    {
      /*
        Open X server connection.
      */
      if (display != (Display *) NULL)
        break;
      display=XOpenDisplay(annotate_info->server_name);
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
          resource_info.close_server=False;
          resource_info.colormap=PrivateColormap;
          if (annotate_info->font != (char *) NULL)
            resource_info.font=annotate_info->font;
          if (annotate_info->box != (char *) NULL)
            resource_info.background_color=annotate_info->box;
          if (annotate_info->pen != (char *) NULL)
            resource_info.foreground_color=annotate_info->pen;
          map_info=XAllocStandardColormap();
          if (map_info == (XStandardColormap *) NULL)
            Warning("Unable to create standard colormap",
              "Memory allocation failed");
          /*
            Initialize visual info.
          */
          visual_info=XBestVisualInfo(display,map_info,&resource_info);
          if (visual_info == (XVisualInfo *) NULL)
            Warning("Unable to get visual",resource_info.visual_type);
          map_info->colormap=(Colormap) NULL;
          pixel_info.pixels=(unsigned long *) NULL;
          pixel_info.gamma_map=(XColor *) NULL;
          /*
            Initialize Standard Colormap info.
          */
          XGetMapInfo(visual_info,XDefaultColormap(display,visual_info->screen),
            map_info);
          XGetPixelInfo(display,visual_info,map_info,&resource_info,
            (Image *) NULL,&pixel_info);
          pixel_info.annotate_context=XDefaultGC(display,visual_info->screen);
          /*
            Initialize font info.
          */
          font_info=XBestFont(display,&resource_info,False);
          if (font_info == (XFontStruct *) NULL)
            Warning("Unable to load font",resource_info.font);
          if ((map_info == (XStandardColormap *) NULL) ||
              (visual_info == (XVisualInfo *) NULL) ||
              (font_info == (XFontStruct *) NULL))
            {
              XFreeResources(display,visual_info,map_info,&pixel_info,
                font_info,&resource_info,(XWindowInfo *) NULL);
              display=(Display *) NULL;
            }
          cache_info=(*annotate_info);
          break;
        }
      Warning("Unable to load X server fonts","substituting Postscript fonts");
      font_type++;
    }
    case 1:
    {
      char
        filename[MaxTextExtent],
        page[MaxTextExtent];

      Image
        *annotate_image;

      ImageInfo
        image_info;

      register RunlengthPacket
        *q;

      unsigned int
        matte,
        polarity;

      XColor
        box_color,
        pen_color;

      /*
        X server fonts are not available, use Postscript to annotate.
      */
      (void) XQueryColorDatabase(annotate_info->box,&box_color);
      (void) XQueryColorDatabase(annotate_info->pen,&pen_color);
      GetImageInfo(&image_info);
      TemporaryFilename(filename);
      image_info.monochrome=True;
      (void) sprintf(page,"%ux%u",height*length,height << 1);
      image_info.page=page;
      if (annotate_info->font == (char *) NULL)
        annotate_info->font=DefaultFont;
      for (i=0; textlist[i] != (char *) NULL; i++)
      {
        if ((x >= image->columns) || (y >= image->rows))
          break;
        if (*textlist[i] == '\0')
          {
            free(textlist[i]);
            y+=annotate_info->pointsize;
            continue;
          }
        (void) strcpy(text,textlist[i]);
        for (j=(Extent(textlist[i])-1) >> 1; j >= 0; j--)
        {
          (void) strcpy(image_info.filename,filename);
          file=fopen(filename,WriteBinaryType);
          if (file == (FILE *) NULL)
            break;
          (void) fprintf(file,"%%!PS-Adobe-3.0\n");
          (void) fprintf(file,"/%s findfont %u scalefont setfont\n",
            annotate_info->font,height);
          (void) fprintf(file,"0 %u moveto (%s) show\n",height,text);
          (void) fprintf(file,"showpage\n");
          (void) fclose(file);
          annotate_image=ReadImage(&image_info);
          if (annotate_image == (Image *) NULL)
            break;
          TransformImage(&annotate_image,"0x0",(char *) NULL);
          if (annotate_image->columns < width)
            break;
          DestroyImage(annotate_image);
          (void) strcpy(text,textlist[i]);
          (void) strcpy(text+j,"...");
          (void) strcat(text,textlist[i]+Extent(textlist[i])-j-1);
        }
        (void) remove(filename);
        free(textlist[i]);
        if (annotate_image == (Image *) NULL)
          {
            Warning("Unable to annotate image",(char *) NULL);
            break;
          }
        /*
          Composite text onto the image.
        */
        polarity=0;
        if (annotate_image->class == PseudoClass)
          polarity=Intensity(annotate_image->colormap[0]) < (MaxRGB >> 1);
        annotate_image->class=DirectClass;
        annotate_image->matte=True;
        q=annotate_image->pixels;
        for (j=0; j < annotate_image->packets; j++)
        {
          if (q->index == polarity)
            {
              q->red=XDownScale(box_color.red);
              q->green=XDownScale(box_color.green);
              q->blue=XDownScale(box_color.blue);
              q->index=
                annotate_info->box == (char *) NULL ? Transparent : Opaque;
            }
          else
            {
              q->red=XDownScale(pen_color.red);
              q->green=XDownScale(pen_color.green);
              q->blue=XDownScale(pen_color.blue);
              q->index=
                annotate_info->pen == (char *) NULL ? Transparent : Opaque;
            }
          q++;
        }
        matte=image->matte;
        CompositeImage(image,OverCompositeOp,annotate_image,x+
          (annotate_info->center ? (width >> 1)-(annotate_image->columns >> 1) :
          0),y);
        image->matte=matte;
        y+=annotate_info->pointsize;
        DestroyImage(annotate_image);
      }
      free(text);
      for ( ; textlist[i] != (char *) NULL; i++)
        free(textlist[i]);
      free((char *) textlist);
      return;
    }
  }
  /*
    Initialize annotate info.
  */
  XGetAnnotateInfo(&xannotate_info);
  if (cache_info.font != annotate_info->font)
    {
      /*
        Font name has changed.
      */
      XFreeFont(display,font_info);
      if (annotate_info->font != (char *) NULL)
        resource_info.font=annotate_info->font;
      font_info=XBestFont(display,&resource_info,False);
      if (font_info == (XFontStruct *) NULL)
        Warning("Unable to load font",resource_info.font);
    }
  if ((flags & HeightValue) == 0)
    height=font_info->ascent+font_info->descent;
  xannotate_info.font_info=font_info;
  xannotate_info.text=text;
  xannotate_info.x=x;
  xannotate_info.y=y;
  xannotate_info.width=width;
  xannotate_info.height=font_info->ascent+font_info->descent;
  annotate_info->pointsize=font_info->ascent+font_info->descent;
  if ((annotate_info->pen != (char *) NULL) &&
      (annotate_info->box != (char *) NULL))
    xannotate_info.stencil=OpaqueStencil;
  else
    if (annotate_info->pen != (char *) NULL)
      xannotate_info.stencil=ForegroundStencil;
    else
      xannotate_info.stencil=BackgroundStencil;
  if ((cache_info.box != annotate_info->box) ||
      (cache_info.pen != annotate_info->pen))
    {
      /*
        Pen color has changed.
      */
      if (annotate_info->box != (char *) NULL)
        resource_info.background_color=annotate_info->box;
      if (annotate_info->pen != (char *) NULL)
        resource_info.foreground_color=annotate_info->pen;
      XGetPixelInfo(display,visual_info,map_info,&resource_info,(Image *) NULL,
        &pixel_info);
    }
  /*
    Annotate the text image.
  */
  for (i=0; textlist[i] != (char *) NULL; i++)
  {
    if ((x >= image->columns) || (y >= image->rows))
      break;
    if (*textlist[i] == '\0')
      {
        free(textlist[i]);
        xannotate_info.y+=height;
        y+=height;
        continue;
      }
    (void) strcpy(text,textlist[i]);
    for (j=(Extent(textlist[i])-1) >> 1; j >= 0; j--)
    {
      xannotate_info.width=(height*XTextWidth(font_info,text,Extent(text)))/
        xannotate_info.height;
      if (xannotate_info.width < width)
        break;
      (void) strcpy(text,textlist[i]);
      (void) strcpy(text+j,"...");
      (void) strcat(text,textlist[i]+Extent(textlist[i])-j-1);
    }
    free(textlist[i]);
    (void) sprintf(xannotate_info.geometry,"%ux%u%+d%+d",xannotate_info.width,
      height,(int) (xannotate_info.x+(annotate_info->center ? (width >> 1)-
      (xannotate_info.width >> 1) : 0)),xannotate_info.y);
    xannotate_info.width=XTextWidth(font_info,text,Extent(text));
    status=XAnnotateImage(display,&pixel_info,&xannotate_info,image);
    if (status == 0)
      {
        Warning("Unable to xannotate image","Memory allocation error");
        break;
      }
    xannotate_info.y+=height;
    y+=height;
  }
  /*
    Free resources.
  */
  cache_info=(*annotate_info);
  free(text);
  for ( ; textlist[i] != (char *) NULL; i++)
    free(textlist[i]);
  free((char *) textlist);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     A v e r a g e I m a g e s                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function AverageImages averages a set of images.  All the input images must
%  be the same size in pixels.
%
%  The format of the AverageImage routine is:
%
%      AverageImages(images)
%
%  A description of each parameter follows:
%
%    o images: The address of a structure of type Image;  returned from
%      ReadImage.
%
%
*/
Export Image *AverageImages(Image *images)
{
  Image
    *averaged_image,
    *next_image;

  long
    blue,
    green,
    red;

  unsigned short
    index;

  register int
    i;

  register RunlengthPacket
    *q;

  /*
    Ensure the images are uncompressed.
  */
  assert(images != (Image *) NULL);
  next_image=images;
  while (next_image != (Image *) NULL)
  {
    if ((next_image->columns != images->columns) ||
        (next_image->rows != images->rows))
      {
        Warning("Unable to average image","images are not the same size");
        return((Image *) NULL);
      }
    if (!UncompressImage(next_image))
      return((Image *) NULL);
    next_image=next_image->next;
  }
  /*
    Initialize average image attributes.
  */
  images->orphan=True;
  averaged_image=CopyImage(images,images->columns,images->rows,False);
  images->orphan=False;
  if (averaged_image == (Image *) NULL)
    {
      Warning("Unable to average image","Memory allocation failed");
      return((Image *) NULL);
    }
  averaged_image->class=DirectClass;
  q=averaged_image->pixels;
  for (i=0; i < averaged_image->packets; i++)
  {
    red=0;
    green=0;
    blue=0;
    index=0;
    next_image=images;
    while (next_image != (Image *) NULL)
    {
      if (i < next_image->packets)
        {
          red+=next_image->pixels[i].red;
          green+=next_image->pixels[i].green;
          blue+=next_image->pixels[i].blue;
          index++;
        }
      next_image=next_image->next;
    }
    q->red=(Quantum) ((red+(long) (index >> 1))/(long) index);
    q->green=(Quantum) ((green+(long) (index >> 1))/(long) index);
    q->blue=(Quantum) ((blue+(long) (index >> 1))/(long) index);
    q->index=0;
    q->length=0;
    q++;
  }
  return(averaged_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   B o r d e r I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function BorderImage takes an image and puts a border around it of a
%  particular color.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  The format of the BorderImage routine is:
%
%      bordered_image=BorderImage(image,border_info)
%
%  A description of each parameter follows:
%
%    o bordered_image: Function BorderImage returns a pointer to the bordered
%      image.  A null image is returned if there is a a memory shortage.
%
%    o image: The address of a structure of type Image.
%
%    o border_info: Specifies a pointer to a XRectangle which defines the
%      border region.
%
*/
Image *BorderImage(Image *image,RectangleInfo *border_info)
{
#define BorderImageText  "  Adding border to image...  "

  Image
    *bordered_image;

  register int
    x,
    y;

  register RunlengthPacket
    *p,
    *q;

  RunlengthPacket
    border;

  /*
    Initialize bordered image attributes.
  */
  assert(image != (Image *) NULL);
  assert(border_info != (RectangleInfo *) NULL);
  bordered_image=CopyImage(image,image->columns+(border_info->width << 1),
    image->rows+(border_info->height << 1),False);
  if (bordered_image == (Image *) NULL)
    {
      Warning("Unable to border image","Memory allocation failed");
      return((Image *) NULL);
    }
  /*
    Initialize border color.
  */
  border.red=image->border_color.red;
  border.green=image->border_color.green;
  border.blue=image->border_color.blue;
  border.index=image->border_color.index;
  border.length=0;
  /*
    Copy image and put border around it.
  */
  q=bordered_image->pixels;
  for (y=0; y < border_info->height; y++)
    for (x=0; x < bordered_image->columns; x++)
      *q++=border;
  p=image->pixels;
  image->runlength=p->length+1;
  for (y=0; y < image->rows; y++)
  {
    /*
      Initialize scanline with border color.
    */
    for (x=0; x < border_info->width; x++)
      *q++=border;
    /*
      Transfer scanline.
    */
    for (x=0; x < image->columns; x++)
    {
      if (image->runlength != 0)
        image->runlength--;
      else
        {
          p++;
          image->runlength=p->length;
        }
      *q=(*p);
      if (image->matte && (p->index == Transparent))
        *q=border;
      q->length=0;
      q++;
    }
    x=0;
    while (x < (bordered_image->columns-image->columns-border_info->width))
    {
      *q++=border;
      x++;
    }
    ProgressMonitor(BorderImageText,y,image->rows);
  }
  for (y=(bordered_image->rows-image->rows-border_info->height-1); y >= 0; y--)
    for (x=0; x < bordered_image->columns; x++)
      *q++=border;
  return(bordered_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   C h o p I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ChopImage creates a new image that is a subregion of an existing
%  one.  It allocates the memory necessary for the new Image structure and
%  returns a pointer to the new image.
%
%  The format of the ChopImage routine is:
%
%      chop_image=ChopImage(image,chop_info)
%
%  A description of each parameter follows:
%
%    o chop_image: Function ChopImage returns a pointer to the chop
%      image.  A null image is returned if there is a a memory shortage or
%      if the image width or height is zero.
%
%    o image: The address of a structure of type Image.
%
%    o chop_info: Specifies a pointer to a RectangleInfo which defines the
%      region of the image to crop.
%
%
*/
Export Image *ChopImage(Image *image,RectangleInfo *chop_info)
{
#define ChopImageText  "  Chopping image...  "

  Image
    *chopped_image;

  register int
    x,
    y;

  register RunlengthPacket
    *p,
    *q;

  unsigned int
    height;

  /*
    Check chop geometry.
  */
  assert(image != (Image *) NULL);
  assert(chop_info != (RectangleInfo *) NULL);
  if (((chop_info->x+(int) chop_info->width) < 0) ||
      ((chop_info->y+(int) chop_info->height) < 0) ||
      (chop_info->x > (int) image->columns) ||
      (chop_info->y > (int) image->rows))
    {
      Warning("Unable to chop image","geometry does not contain image");
      return((Image *) NULL);
    }
  if ((chop_info->x+(int) chop_info->width) > (int) image->columns)
    chop_info->width=(unsigned int) ((int) image->columns-chop_info->x);
  if ((chop_info->y+(int) chop_info->height) > (int) image->rows)
    chop_info->height=(unsigned int) ((int) image->rows-chop_info->y);
  if (chop_info->x < 0)
    {
      chop_info->width-=(unsigned int) (-chop_info->x);
      chop_info->x=0;
    }
  if (chop_info->y < 0)
    {
      chop_info->height-=(unsigned int) (-chop_info->y);
      chop_info->y=0;
    }
  /*
    Initialize chop image attributes.
  */
  chopped_image=CopyImage(image,image->columns-chop_info->width,
    image->rows-chop_info->height,False);
  if (chopped_image == (Image *) NULL)
    {
      Warning("Unable to chop image","Memory allocation failed");
      return((Image *) NULL);
    }
  /*
    Extract chop image.
  */
  p=image->pixels;
  image->runlength=p->length+1;
  q=chopped_image->pixels;
  for (y=0; y < chop_info->y; y++)
    for (x=0; x < image->columns; x++)
    {
      if (image->runlength != 0)
        image->runlength--;
      else
        {
          p++;
          image->runlength=p->length;
        }
      if ((x < chop_info->x) || (x >= (chop_info->x+chop_info->width)))
        {
          *q=(*p);
          q->length=0;
          q++;
        }
    }
  /*
    Skip pixels up to the chop image.
  */
  for (x=0; x < (chop_info->height*image->columns); x++)
    if (image->runlength != 0)
      image->runlength--;
    else
      {
        p++;
        image->runlength=p->length;
      }
  /*
    Extract chop image.
  */
  height=image->rows-(chop_info->y+chop_info->height);
  for (y=0; y < height; y++)
  {
    for (x=0; x < image->columns; x++)
    {
      if (image->runlength != 0)
        image->runlength--;
      else
        {
          p++;
          image->runlength=p->length;
        }
      if ((x < chop_info->x) || (x >= (chop_info->x+chop_info->width)))
        {
          *q=(*p);
          q->length=0;
          q++;
        }
    }
    ProgressMonitor(ChopImageText,y,height);
  }
  return(chopped_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   C l o s e I m a g e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function CloseImage closes a file associated with the image.  If the
%  filename prefix is '|', the file is a pipe and is closed with PipeClose.
%
%  The format of the CloseImage routine is:
%
%      CloseImage(image)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%
*/
void CloseImage(Image *image)
{
  /*
    Close image file.
  */
  assert(image != (Image *) NULL);
  if (image->file == (FILE *) NULL)
    return;
  image->status=ferror(image->file);
#if !defined(vms) && !defined(macintosh) && !defined(WIN32)
  if (image->pipe)
    (void) pclose(image->file);
  else
#endif
    if ((image->file != stdin) && (image->file != stdout))
      (void) fclose(image->file);
  image->file=(FILE *) NULL;
  if (!image->orphan)
    do
    {
      image->file=(FILE *) NULL;
      image=image->next;
    }
    while (image != (Image *) NULL);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   C o l o r F l o o d f i l l I m a g e                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ColorFloodfillImage floodfills the designated area with a color.
%  The floodfill algorithm is strongly based on a similiar algorithm in
%  "Graphics Gems" by Paul Heckbert.
%
%  The format of the ColorFloodfillImage routine is:
%
%      ColorFloodfillImage(image,x,y,xcolor,delta)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%    o x,y: Unsigned integers representing the current location of the pen.
%
%    o xcolor: A XColor structure.  This is the RGB value of the target color.
%
%    o delta: This is the allowed variance in color (fuzzy color).
%
%
*/
Export void ColorFloodfillImage(Image *image,int x,int y,
  const ColorPacket *color,const int delta)
{
  int
    offset,
    skip,
    start,
    x1,
    x2;

  register RunlengthPacket
    *pixel;

  register XSegment
    *p;

  RunlengthPacket
    target;

  XSegment
    *segment_stack;

  /*
    Check boundary conditions.
  */
  assert(image != (Image *) NULL);
  assert(color != (ColorPacket *) NULL);
  if ((y < 0) || (y >= image->rows))
    return;
  if ((x < 0) || (x >= image->columns))
    return;
  target=image->pixels[y*image->columns+x];
  if (ColorMatch(*color,target,delta))
    return;
  /*
    Allocate segment stack.
  */
  segment_stack=(XSegment *) malloc(MaxStacksize*sizeof(XSegment));
  if (segment_stack == (XSegment *) NULL)
    {
      Warning("Unable to recolor image","Memory allocation failed");
      return;
    }
  /*
    Push initial segment on stack.
  */
  start=0;
  p=segment_stack;
  Push(y,x,x,1);
  Push(y+1,x,x,-1);
  while (p > segment_stack)
  {
    /*
      Pop segment off stack.
    */
    p--;
    x1=p->x1;
    x2=p->x2;
    offset=p->y2;
    y=p->y1+offset;
    /*
      Recolor neighboring pixels.
    */
    for (x=x1; x >= 0 ; x--)
    {
      pixel=image->pixels+(y*image->columns+x);
      if (!ColorMatch(*pixel,target,delta))
        break;
      pixel->red=color->red;
      pixel->green=color->green;
      pixel->blue=color->blue;
    }
    skip=x >= x1;
    if (!skip)
      {
        start=x+1;
        if (start < x1)
          Push(y,start,x1-1,-offset);
        x=x1+1;
      }
    do
    {
      if (!skip)
        {
          for ( ; x < image->columns; x++)
          {
            pixel=image->pixels+(y*image->columns+x);
            if (!ColorMatch(*pixel,target,delta))
              break;
            pixel->red=color->red;
            pixel->green=color->green;
            pixel->blue=color->blue;
          }
          Push(y,start,x-1,offset);
          if (x > (x2+1))
            Push(y,x2+1,x-1,-offset);
        }
      skip=False;
      for (x++; x <= x2 ; x++)
      {
        pixel=image->pixels+(y*image->columns+x);
        if (ColorMatch(*pixel,target,delta))
          break;
      }
      start=x;
    } while (x <= x2);
  }
  free((char *) segment_stack);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     C o l o r i z e I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ColorizeImage colorizes an image with the pen color.  The amount
%  of the coloring is controled with the opacity levels.
%
%  The format of the ColorizeImage routine is:
%
%      ColorizeImage(image,opaque_color,pen_color)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o opaque_color,
%      pen_color: A character string that contain an X11 color string.
%
%
*/
void ColorizeImage(Image *image,char *opacity,char *pen_color)
{
#define ColorizeImageText  "  Colorizing the image...  "

  ColorPacket
    target;

  int
    blue_opacity,
    count,
    green_opacity,
    red_opacity;

  long
    value;

  register int
    i;

  register RunlengthPacket
    *p;

  unsigned int
    status;

  XColor
    target_color;

  /*
    Determine RGB values of the pen color.
  */
  assert(image != (Image *) NULL);
  if (opacity == (char *) NULL)
    return;
  status=XQueryColorDatabase(pen_color,&target_color);
  if (status == False)
    return;
  target.red=XDownScale(target_color.red);
  target.green=XDownScale(target_color.green);
  target.blue=XDownScale(target_color.blue);
  status=XQueryColorDatabase(pen_color,&target_color);
  if (status == False)
    return;
  red_opacity=100;
  green_opacity=100;
  blue_opacity=100;
  count=sscanf(opacity,"%d/%d/%d",&red_opacity,&green_opacity,&blue_opacity);
  if (count == 1)
    {
      if (red_opacity == 0)
        return;
      green_opacity=red_opacity;
      blue_opacity=red_opacity;
    }
  switch (image->class)
  {
    case DirectClass:
    default:
    {
      /*
        Colorize DirectClass image.
      */
      p=image->pixels;
      for (i=0; i < image->packets; i++)
      {
        value=(long) (p->red*(100-red_opacity)+target.red*red_opacity)/100;
        if (value > MaxRGB)
          p->red=MaxRGB;
        else
          if (value < 0)
            p->red=0;
          else
            p->red=(Quantum) value;
        value=(long)
          (p->green*(100-green_opacity)+target.green*green_opacity)/100;
        if (value > MaxRGB)
          p->green=MaxRGB;
        else
          if (value < 0)
            p->green=0;
          else
            p->green=(Quantum) value;
        value=(long) (p->blue*(100-blue_opacity)+target.blue*blue_opacity)/100;
        if (value > MaxRGB)
          p->blue=MaxRGB;
        else
          if (value < 0)
            p->blue=0;
          else
            p->blue=(Quantum) value;
        p++;
        if (QuantumTick(i,image))
          ProgressMonitor(ColorizeImageText,i,image->packets);
      }
      break;
    }
    case PseudoClass:
    {
      /*
        Colorize PseudoClass image.
      */
      for (i=0; i < image->colors; i++)
      {
        value=(long)
          (image->colormap[i].red*(100-red_opacity)+target.red*red_opacity)/100;
        if (value > MaxRGB)
          image->colormap[i].red=MaxRGB;
        else
          if (value < 0)
            image->colormap[i].red=0;
          else
            image->colormap[i].red=(Quantum) value;
        value=(long) (image->colormap[i].green*(100-green_opacity)+
          target.green*green_opacity)/100;
        if (value > MaxRGB)
          image->colormap[i].green=MaxRGB;
        else
          if (value < 0)
            image->colormap[i].green=0;
          else
            image->colormap[i].green=(Quantum) value;
        value=(long) (image->colormap[i].blue*(100-blue_opacity)+
          target.blue*blue_opacity)/100;
        if (value > MaxRGB)
          image->colormap[i].blue=MaxRGB;
        else
          if (value < 0)
            image->colormap[i].blue=0;
          else
            image->colormap[i].blue=(Quantum) value;
      }
      SyncImage(image);
      break;
    }
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   C o m m e n t I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function CommentImage initializes an image comment.  Optionally the
%  comment can include the image filename, type, width, height, or scene
%  number by embedding special format characters.  Embed %f for filename,
%  %m for magick, %w for width, %h for height, %s for scene number, or \n
%  for newline.  For example,
%
%     %f  %wx%h
%
%  produces an image comment of
%
%     bird.miff  512x480
%
%  for an image titled bird.miff and whose width is 512 and height is 480.
%
%  The format of the CommentImage routine is:
%
%      CommentImage(image,comments)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%    o comments: The address of a character string containing the comment
%      format.
%
%
*/
Export void CommentImage(Image *image,char *comments)
{
  register char
    *p,
    *q;

  unsigned int
    indirection,
    length;

  assert(image != (Image *) NULL);
  if (image->comments != (char *) NULL)
    free((char *) image->comments);
  image->comments=(char *) NULL;
  if (comments == (char *) NULL)
    return;
  if (*comments == '\0')
    return;
  indirection=(*comments == '@');
  if (indirection)
    {
      FILE
        *file;

      int
        c;

      /*
        Read comments from a file.
      */
      file=(FILE *) fopen(comments+1,"r");
      if (file == (FILE *) NULL)
        {
          Warning("Unable to read comments file",comments+1);
          return;
        }
      length=MaxTextExtent;
      comments=(char *) malloc(length);
      for (q=comments; comments != (char *) NULL; q++)
      {
        c=fgetc(file);
        if (c == EOF)
          break;
        if ((q-comments+1) >= length)
          {
            *q='\0';
            length<<=1;
            comments=(char *) realloc((char *) comments,length);
            if (comments == (char *) NULL)
              break;
            q=comments+Extent(comments);
          }
        *q=(unsigned char) c;
      }
      (void) fclose(file);
      if (comments == (char *) NULL)
        {
          Warning("Unable to comments image","Memory allocation failed");
          return;
        }
      *q='\0';
    }
  /*
    Allocate and initialize image comments.
  */
  p=comments;
  length=Extent(comments)+MaxTextExtent;
  image->comments=(char *) malloc(length);
  for (q=image->comments; image->comments != (char *) NULL; p++)
  {
    *q='\0';
    if (*p == '\0')
      break;
    if ((q-image->comments+MaxTextExtent) >= length)
      {
        length<<=1;
        image->comments=(char *) realloc((char *) image->comments,length);
        if (image->comments == (char *) NULL)
          break;
        q=image->comments+Extent(image->comments);
      }
    /*
      Process formatting characters in comments.
    */
    if ((*p == '\\') && (*(p+1) == 'n'))
      {
        *q++='\n';
        p++;
        continue;
      }
    if (*p != '%')
      {
        *q++=(*p);
        continue;
      }
    p++;
    switch (*p)
    {
      case 'b':
      {
        (void) sprintf(q,"%ld",image->filesize/1000);
        q=image->comments+Extent(image->comments);
        break;
      }
      case 'f':
      {
        register char
          *p;

        /*
          Label segment is the base of the filename.
        */
        if (Extent(image->magick_filename) == 0)
          break;
        p=image->magick_filename+Extent(image->magick_filename)-1;
        while ((p > image->magick_filename) && (*(p-1) != '/'))
          p--;
        (void) strcpy(q,p);
        q+=Extent(p);
        break;
      }
      case 'h':
      {
        (void) sprintf(q,"%u",image->magick_rows);
        q=image->comments+Extent(image->comments);
        break;
      }
      case 'm':
      {
        (void) strcpy(q,image->magick);
        q+=Extent(image->magick);
        break;
      }
      case 's':
      {
        (void) sprintf(q,"%u",image->scene);
        q=image->comments+Extent(image->comments);
        break;
      }
      case 'w':
      {
        (void) sprintf(q,"%u",image->magick_columns);
        q=image->comments+Extent(image->comments);
        break;
      }
      default:
      {
        *q++='%';
        *q++=(*p);
        break;
      }
    }
  }
  if (image->comments == (char *) NULL)
    {
      Warning("Unable to comment image","Memory allocation failed");
      return;
    }
  *q='\0';
  if (indirection)
    free((char *) comments);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   C o m p r e s s C o l o r m a p                                           %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function CompressColormap compresses an image colormap removing any
%  unused color entries.
%
%  The format of the CompressColormap routine is:
%
%      CompressColormap(image)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%
*/
void CompressColormap(Image *image)
{
  ColorPacket
    *colormap;

  int
    number_colors;

  register int
    i;

  register RunlengthPacket
    *p;

  register unsigned short
    index;

  /*
    Determine if colormap can be compressed.
  */
  assert(image != (Image *) NULL);
  if (image->class != PseudoClass)
    return;
  number_colors=image->colors;
  for (i=0; i < image->colors; i++)
    image->colormap[i].flags=False;
  image->colors=0;
  p=image->pixels;
  for (i=0; i < image->packets; i++)
  {
    if (!image->colormap[p->index].flags)
      {
        image->colormap[p->index].index=image->colors;
        image->colormap[p->index].flags=True;
        image->colors++;
      }
    p++;
  }
  if (image->colors == number_colors)
    return;  /* no unused entries */
  /*
    Compress colormap.
  */
  colormap=(ColorPacket *) malloc(image->colors*sizeof(ColorPacket));
  if (colormap == (ColorPacket *) NULL)
    {
      Warning("Unable to compress colormap","Memory allocation failed");
      image->colors=number_colors;
      return;
    }
  for (i=0; i < number_colors; i++)
    if (image->colormap[i].flags)
      {
        index=image->colormap[i].index;
        colormap[index].red=image->colormap[i].red;
        colormap[index].green=image->colormap[i].green;
        colormap[index].blue=image->colormap[i].blue;
      }
  /*
    Remap pixels.
  */
  p=image->pixels;
  for (i=0; i < image->packets; i++)
  {
    p->index=image->colormap[p->index].index;
    p++;
  }
  free((char *) image->colormap);
  image->colormap=colormap;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   C o m p r e s s I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function CompressImage compresses an image to the minimum number of
%  runlength-encoded packets.
%
%  The format of the CompressImage routine is:
%
%      CompressImage(image)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%
*/
Export void CompressImage(Image *image)
{
  register int
    i;

  register RunlengthPacket
    *p,
    *q;

  /*
    Compress image.
  */
  assert(image != (Image *) NULL);
  if (image == (Image *) NULL)
    return;
  p=image->pixels;
  image->runlength=p->length+1;
  image->packets=0;
  q=image->pixels;
  q->length=MaxRunlength;
  if (image->matte)
    for (i=0; i < (image->columns*image->rows); i++)
    {
      if (image->runlength != 0)
        image->runlength--;
      else
        {
          p++;
          image->runlength=p->length;
        }
      if ((p->red == q->red) && (p->green == q->green) &&
          (p->blue == q->blue) && (p->index == q->index) &&
          ((int) q->length < MaxRunlength))
        q->length++;
      else
        {
          if (image->packets != 0)
            q++;
          image->packets++;
          *q=(*p);
          q->length=0;
        }
    }
  else
    for (i=0; i < (image->columns*image->rows); i++)
    {
      if (image->runlength != 0)
        image->runlength--;
      else
        {
          p++;
          image->runlength=p->length;
        }
      if ((p->red == q->red) && (p->green == q->green) &&
          (p->blue == q->blue) && ((int) q->length < MaxRunlength))
        q->length++;
      else
        {
          if (image->packets != 0)
            q++;
          image->packets++;
          *q=(*p);
          q->length=0;
        }
    }
  image->pixels=(RunlengthPacket *)
    realloc((char *) image->pixels,image->packets*sizeof(RunlengthPacket));
  /*
    Runlength-encode only if it takes up less space than no compression.
  */
  if (image->class == DirectClass)
    {
      if (image->packets >= ((image->columns*image->rows*3) >> 2))
        image->compression=NoCompression;
      return;
    }
  if (image->packets >= ((image->columns*image->rows) >> 1))
    image->compression=NoCompression;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   C o m p o s i t e I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function CompositeImage returns the second image composited onto the
%  first at the specified offsets.
%
%  The format of the CompositeImage routine is:
%
%      CompositeImage(image,compose,composite_image,x_offset,y_offset)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%    o compose: Specifies an image composite operator.
%
%    o composite_image: The address of a structure of type Image.
%
%    o x_offset: An integer that specifies the column offset of the composited
%      image.
%
%    o y_offset: An integer that specifies the row offset of the composited
%      image.
%
%
*/
Export void CompositeImage(Image *image,const CompositeOperator compose,
  Image *composite_image,const int x_offset,const int y_offset)
{
#define CompositeImageText  "  Compositing image...  "

  long
    blue,
    green,
    index,
    red;

  Quantum
    shade;

  register int
    i,
    x,
    y;

  register RunlengthPacket
    *p,
    *q;

  /*
    Check composite geometry.
  */
  assert(image != (Image *) NULL);
  assert(composite_image != (Image *) NULL);
  if (((x_offset+(int) image->columns) < 0) ||
      ((y_offset+(int) image->rows) < 0) ||
      (x_offset > (int) image->columns) || (y_offset > (int) image->rows))
    {
      Warning("Unable to composite image","geometry does not contain image");
      return;
    }
  /*
    Image must be uncompressed.
  */
  if (!UncompressImage(image))
    return;
  switch (compose)
  {
    case XorCompositeOp:
    case PlusCompositeOp:
    case MinusCompositeOp:
    case AddCompositeOp:
    case SubtractCompositeOp:
    case DifferenceCompositeOp:
    case BumpmapCompositeOp:
    case MatteReplaceCompositeOp:
    case AddMaskCompositeOp:
    case BlendCompositeOp:
      break;
    case DisplaceCompositeOp:
    {
      double
        x_displace,
        y_displace;

      float
        horizontal_scale,
        vertical_scale;

      Image
        *displaced_image;

      register RunlengthPacket
        *r;

      /*
        Allocate the displaced image.
      */
      composite_image->orphan=True;
      displaced_image=CopyImage(composite_image,composite_image->columns,
        composite_image->rows,False);
      composite_image->orphan=False;
      if (displaced_image == (Image *) NULL)
        {
          Warning("Unable to display image","Memory allocation failed");
          return;
        }
      horizontal_scale=20.0;
      vertical_scale=20.0;
      if (composite_image->geometry != (char *) NULL)
        {
          int
            count;

          /*
            Determine the horizontal and vertical displacement scale.
          */
          count=sscanf(composite_image->geometry,"%fx%f\n",
            &horizontal_scale,&vertical_scale);
          if (count == 1)
            vertical_scale=horizontal_scale;
        }
      /*
        Shift image pixels as defined by a displacement map.
      */
      p=composite_image->pixels;
      composite_image->runlength=p->length+1;
      r=displaced_image->pixels;
      for (y=0; y < composite_image->rows; y++)
      {
        if (((y_offset+y) < 0) || ((y_offset+y) >= image->rows))
          continue;
        q=image->pixels+(y_offset+y)*image->columns+x_offset;
        for (x=0; x < composite_image->columns; x++)
        {
          if (composite_image->runlength != 0)
            composite_image->runlength--;
          else
            {
              p++;
              composite_image->runlength=p->length;
            }
          if (((x_offset+x) < 0) || ((x_offset+x) >= image->columns))
            {
              q++;
              continue;
            }
          x_displace=(horizontal_scale*((float) Intensity(*p)-
            ((MaxRGB+1) >> 1)))/((MaxRGB+1) >> 1);
          y_displace=x_displace;
          if (composite_image->matte)
            y_displace=(vertical_scale*((float) p->index-
              ((MaxRGB+1) >> 1)))/((MaxRGB+1) >> 1);
          *r=Interpolate(image,q,x_offset+x+x_displace,y_offset+y+y_displace);
          r->length=0;
          q++;
          r++;
        }
      }
      composite_image=displaced_image;
      break;
    }
    case ReplaceCompositeOp:
    {
      /*
        Promote image to DirectClass if colormaps differ.
      */
      if (image->class == PseudoClass)
        if ((composite_image->class == DirectClass) ||
            (composite_image->colors != image->colors))
          image->class=DirectClass;
        else
          {
            int
              status;

            status=memcmp((char *) composite_image->colormap,
              (char *) image->colormap,composite_image->colors*
              sizeof(ColorPacket));
            if (status != 0)
              image->class=DirectClass;
          }
      if (image->matte && !composite_image->matte)
        {
          p=composite_image->pixels;
          for (i=0; i < composite_image->packets; i++)
          {
            p->index=Opaque;
            p++;
          }
          composite_image->class=DirectClass;
          composite_image->matte=True;
        }
      break;
    }
    default:
    {
      /*
        Initialize image matte data.
      */
      if (!image->matte)
        {
          q=image->pixels;
          for (i=0; i < image->packets; i++)
          {
            q->index=Opaque;
            q++;
          }
          image->class=DirectClass;
          image->matte=True;
        }
      if (!composite_image->matte)
        {
          p=composite_image->pixels;
          red=p->red;
          green=p->green;
          blue=p->blue;
          if (IsMonochromeImage(composite_image))
            {
              red=composite_image->background_color.red;
              green=composite_image->background_color.green;
              blue=composite_image->background_color.blue;
            }
          for (i=0; i < composite_image->packets; i++)
          {
            p->index=Opaque;
            if ((p->red == red) && (p->green == green) &&
                (p->blue == blue))
              p->index=Transparent;
            p++;
          }
          composite_image->class=DirectClass;
          composite_image->matte=True;
        }
      break;
    }
  }
  /*
    Initialize composited image.
  */
  p=composite_image->pixels;
  composite_image->runlength=p->length+1;
  for (y=0; y < composite_image->rows; y++)
  {
    if (((y_offset+y) < 0) || ((y_offset+y) >= image->rows))
      continue;
    q=image->pixels+(y_offset+y)*image->columns+x_offset;
    for (x=0; x < composite_image->columns; x++)
    {
      if (composite_image->runlength != 0)
        composite_image->runlength--;
      else
        {
          p++;
          composite_image->runlength=p->length;
        }
      if (((x_offset+x) < 0) || ((x_offset+x) >= image->columns))
        {
          q++;
          continue;
        }
      switch (compose)
      {
        case OverCompositeOp:
        default:
        {
          if (p->index == Transparent)
            {
              red=q->red;
              green=q->green;
              blue=q->blue;
              index=q->index;
            }
          else
            if (p->index == Opaque)
              {
                red=p->red;
                green=p->green;
                blue=p->blue;
                index=p->index;
              }
            else
              {
                red=(long) (p->red*Opaque+q->red*(Opaque-p->index))/Opaque;
                green=(long)
                  (p->green*Opaque+q->green*(Opaque-p->index))/Opaque;
                blue=(long) (p->blue*Opaque+q->blue*(Opaque-p->index))/Opaque;
                index=(long)
                  (p->index*Opaque+q->index*(Opaque-p->index))/Opaque;
              }
          break;
        }
        case InCompositeOp:
        {
          red=(long) (p->red*q->index)/Opaque;
          green=(long) (p->green*q->index)/Opaque;
          blue=(long) (p->blue*q->index)/Opaque;
          index=(long) (p->index*q->index)/Opaque;
          break;
        }
        case OutCompositeOp:
        {
          red=(long) (p->red*(Opaque-q->index))/Opaque;
          green=(long) (p->green*(Opaque-q->index))/Opaque;
          blue=(long) (p->blue*(Opaque-q->index))/Opaque;
          index=(long) (p->index*(Opaque-q->index))/Opaque;
          break;
        }
        case AtopCompositeOp:
        {
          red=(long) (p->red*q->index+q->red*(Opaque-p->index))/Opaque;
          green=(long) (p->green*q->index+q->green*(Opaque-p->index))/Opaque;
          blue=(long) (p->blue*q->index+q->blue*(Opaque-p->index))/Opaque;
          index=(long) (p->index*q->index+q->index*(Opaque-p->index))/Opaque;
          break;
        }
        case XorCompositeOp:
        {
          red=(long) (p->red*(Opaque-q->index)+q->red*(Opaque-p->index))/Opaque;
          green=(long)
            (p->green*(Opaque-q->index)+q->green*(Opaque-p->index))/Opaque;
          blue=(long)
            (p->blue*(Opaque-q->index)+q->blue*(Opaque-p->index))/Opaque;
          index=(long)
            (p->index*(Opaque-q->index)+q->index*(Opaque-p->index))/Opaque;
          break;
        }
        case PlusCompositeOp:
        {
          red=(long) p->red+(long) q->red;
          green=(long) p->green+(long) q->green;
          blue=(long) p->blue+(long) q->blue;
          index=(long) p->index+(long) q->index;
          break;
        }
        case MinusCompositeOp:
        {
          red=(long) p->red-(long) q->red;
          green=(long) p->green-(long) q->green;
          blue=(long) p->blue-(long) q->blue;
          index=Opaque;
          break;
        }
        case AddCompositeOp:
        {
          red=(long) p->red+(long) q->red;
          if (red > Opaque)
            red-=(Opaque+1);
          green=(long) p->green+(long) q->green;
          if (green > Opaque)
            green-=(Opaque+1);
          blue=(long) p->blue+(long) q->blue;
          if (blue > Opaque)
            blue-=(Opaque+1);
          index=(long) p->index+(long) q->index;
          if (index > Opaque)
            index-=(Opaque+1);
          break;
        }
        case SubtractCompositeOp:
        {
          red=(long) p->red-(long) q->red;
          if (red < 0)
            red+=(Opaque+1);
          green=(long) p->green-(long) q->green;
          if (green < 0)
            green+=(Opaque+1);
          blue=(long) p->blue-(long) q->blue;
          if (blue < 0)
            blue+=(Opaque+1);
          index=(long) p->index-(long) q->index;
          if (index < 0)
            index+=(Opaque+1);
          break;
        }
        case DifferenceCompositeOp:
        {
          red=AbsoluteValue((long) p->red-(long) q->red);
          green=AbsoluteValue((long) p->green-(long) q->green);
          blue=AbsoluteValue((long) p->blue-(long) q->blue);
          index=AbsoluteValue((long) p->index-(long) q->index);
          break;
        }
        case BumpmapCompositeOp:
        {
          shade=Intensity(*p);
          red=(long) (q->red*shade)/Opaque;
          green=(long) (q->green*shade)/Opaque;
          blue=(long) (q->blue*shade)/Opaque;
          index=(long) (q->index*shade)/Opaque;
          break;
        }
        case ReplaceCompositeOp:
        {
          red=p->red;
          green=p->green;
          blue=p->blue;
          index=p->index;
          break;
        }
        case MatteReplaceCompositeOp:
        {
          red=q->red;
          green=q->green;
          blue=q->blue;
          index=p->index;
          break;
        }
        case AddMaskCompositeOp:
        {
          red=q->red;
          green=q->green;
          blue=q->blue;
          index=DownScale(Intensity(*p));
          break;
        }
        case BlendCompositeOp:
        {
          red=(long) (p->red*p->index+q->red*q->index)/Opaque;
          green=(long) (p->green*p->index+q->green*q->index)/Opaque;
          blue=(long) (p->blue*p->index+q->blue*q->index)/Opaque;
          index=Opaque;
          break;
        }
        case DisplaceCompositeOp:
        {
          red=p->red;
          green=p->green;
          blue=p->blue;
          index=p->index;
          break;
        }
      }
      if (red > MaxRGB)
        q->red=MaxRGB;
      else
        if (red < 0)
          q->red=0;
        else
          q->red=(Quantum) red;
      if (green > MaxRGB)
        q->green=MaxRGB;
      else
        if (green < 0)
          q->green=0;
        else
          q->green=(Quantum) green;
      if (blue > MaxRGB)
        q->blue=MaxRGB;
      else
        if (blue < 0)
          q->blue=0;
        else
          q->blue=(Quantum) blue;
      if (index > Opaque)
        q->index=Opaque;
      else
        if (index < Transparent)
          q->index=Transparent;
        else
          q->index=(unsigned short) index;
      q->length=0;
      q++;
    }
    ProgressMonitor(CompositeImageText,y,composite_image->rows);
  }
  if (compose == BlendCompositeOp)
    image->matte=False;
  if (compose == DisplaceCompositeOp)
    {
      image->matte=False;
      DestroyImage(composite_image);
    }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     C o n t r a s t I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ContrastImage enhances the intensity differences between the
%  lighter and darker elements of the image.
%
%  The format of the ContrastImage routine is:
%
%      ContrastImage(image,sharpen)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o sharpen: If True, the intensity is increased otherwise it is
%      decreased.
%
%
*/
void ContrastImage(Image *image,const unsigned int sharpen)
{
#define DullContrastImageText  "  Dulling image contrast...  "
#define SharpenContrastImageText  "  Sharpening image contrast...  "

  int
    sign;

  register int
    i;

  register RunlengthPacket
    *p;

  assert(image != (Image *) NULL);
  sign=sharpen ? 1 : -1;
  switch (image->class)
  {
    case DirectClass:
    default:
    {
      /*
        Contrast enhance DirectClass image.
      */
      p=image->pixels;
      for (i=0; i < image->packets; i++)
      {
        Contrast(sign,&p->red,&p->green,&p->blue);
        p++;
        if (QuantumTick(i,image))
          if (sharpen)
            ProgressMonitor(SharpenContrastImageText,i,image->packets);
          else
            ProgressMonitor(DullContrastImageText,i,image->packets);
      }
      break;
    }
    case PseudoClass:
    {
      /*
        Contrast enhance PseudoClass image.
      */
      for (i=0; i < image->colors; i++)
        Contrast(sign,&image->colormap[i].red,&image->colormap[i].green,
          &image->colormap[i].blue);
      SyncImage(image);
      break;
    }
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   C o p y I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function CopyImage returns a copy of all fields of the input image.  The
%  the pixel memory is allocated but the pixel data is not copied.
%
%  The format of the CopyImage routine is:
%
%      copy_image=CopyImage(image,columns,rows,copy_pixels)
%
%  A description of each parameter follows:
%
%    o copy_image: Function CopyImage returns a pointer to the image after
%      copying.  A null image is returned if there is a memory shortage.
%
%    o image: The address of a structure of type Image.
%
%    o columns: An integer that specifies the number of columns in the copied
%      image.
%
%    o rows: An integer that specifies the number of rows in the copied
%      image.
%
%    o copy_pixels: Specifies whether the pixel data is copied.  Must be
%      either True or False;
%
%
*/
Export Image *CopyImage(Image *image,const unsigned int columns,
  const unsigned int rows,const unsigned int copy_pixels)
{
  Image
    *copy_image;

  register int
    i;

  /*
    Allocate image structure.
  */
  assert(image != (Image *) NULL);
  copy_image=(Image *) malloc(sizeof(Image));
  if (copy_image == (Image *) NULL)
    return((Image *) NULL);
  *copy_image=(*image);
  if (image->comments != (char *) NULL)
    {
      /*
        Allocate and copy the image comments.
      */
      copy_image->comments=(char *)
        malloc((unsigned int) Extent(image->comments)+1);
      if (copy_image->comments == (char *) NULL)
        return((Image *) NULL);
      (void) strcpy(copy_image->comments,image->comments);
    }
  if (image->label != (char *) NULL)
    {
      /*
        Allocate and copy the image label.
      */
      copy_image->label=(char *) malloc((unsigned int) Extent(image->label)+1);
      if (copy_image->label == (char *) NULL)
        return((Image *) NULL);
      (void) strcpy(copy_image->label,image->label);
    }
  copy_image->columns=columns;
  copy_image->rows=rows;
  copy_image->montage=(char *) NULL;
  copy_image->directory=(char *) NULL;
  if (image->colormap != (ColorPacket *) NULL)
    {
      /*
        Allocate and copy the image colormap.
      */
      copy_image->colormap=(ColorPacket *)
        malloc(image->colors*sizeof(ColorPacket));
      if (copy_image->colormap == (ColorPacket *) NULL)
        return((Image *) NULL);
      for (i=0; i < image->colors; i++)
        copy_image->colormap[i]=image->colormap[i];
    }
  if (image->signature != (char *) NULL)
    {
      /*
        Allocate and copy the image signature.
      */
      copy_image->signature=(char *)
        malloc((unsigned int) Extent(image->signature)+1);
      if (copy_image->signature == (char *) NULL)
        return((Image *) NULL);
      (void) strcpy(copy_image->signature,image->signature);
    }
  /*
    Allocate the image pixels.
  */
  if (copy_pixels)
    copy_image->pixels=(RunlengthPacket *)
      malloc((unsigned int) image->packets*sizeof(RunlengthPacket));
  else
    {
      copy_image->packets=copy_image->columns*copy_image->rows;
      copy_image->pixels=(RunlengthPacket *)
        malloc((unsigned int) copy_image->packets*sizeof(RunlengthPacket));
    }
  if (copy_image->pixels == (RunlengthPacket *) NULL)
    return((Image *) NULL);
  if (copy_pixels)
    {
      register RunlengthPacket
        *p,
        *q;

      /*
        Copy image pixels.
      */
      p=image->pixels;
      q=copy_image->pixels;
      for (i=0; i < image->packets; i++)
      {
        *q=(*p);
        p++;
        q++;
      }
    }
  copy_image->packed_pixels=(unsigned char *) NULL;
  if (image->page != (char *) NULL)
    {
      /*
        Allocate and copy the image page.
      */
      copy_image->page=(char *) malloc((unsigned int) Extent(image->page)+1);
      if (copy_image->page == (char *) NULL)
        return((Image *) NULL);
      (void) strcpy(copy_image->page,image->page);
    }
  if (image->orphan)
    {
      copy_image->file=(FILE *) NULL;
      copy_image->previous=(Image *) NULL;
      copy_image->next=(Image *) NULL;
    }
  else
    {
      /*
        Link image into image list.
      */
      if (copy_image->previous != (Image *) NULL)
        copy_image->previous->next=copy_image;
      if (copy_image->next != (Image *) NULL)
        copy_image->next->previous=copy_image;
    }
  copy_image->orphan=False;
  return(copy_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   C r o p I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function CropImage creates a new image that is a subregion of an existing
%  one.  It allocates the memory necessary for the new Image structure and
%  returns a pointer to the new image.  This routine is optimized to perserve
%  the runlength encoding.  That is, the cropped image will always use less
%  memory than the original.
%
%  The format of the CropImage routine is:
%
%      cropped_image=CropImage(image,crop_info)
%
%  A description of each parameter follows:
%
%    o cropped_image: Function CropImage returns a pointer to the cropped
%      image.  A null image is returned if there is a a memory shortage or
%      if the image width or height is zero.
%
%    o image: The address of a structure of type Image.
%
%    o crop_info: Specifies a pointer to a RectangleInfo which defines the
%      region of the image to crop.
%
%
*/
Export Image *CropImage(Image *image,RectangleInfo *crop_info)
{
#define CropImageText  "  Cropping image...  "
#define DeltaX  16

  char
    geometry[MaxTextExtent];

  Image
    *cropped_image;

  int
    x,
    y;

  register RunlengthPacket
    *p,
    *q;

  unsigned int
    height,
    width;

  /*
    Check crop geometry.
  */
  assert(image != (Image *) NULL);
  assert(crop_info != (RectangleInfo *) NULL);
  if (((crop_info->x+(int) crop_info->width) < 0) ||
      ((crop_info->y+(int) crop_info->height) < 0) ||
      (crop_info->x > (int) image->columns) ||
      (crop_info->y > (int) image->rows))
    {
      Warning("Unable to crop image","geometry does not contain image");
      return((Image *) NULL);
    }
  if ((crop_info->x+(int) crop_info->width) > (int) image->columns)
    crop_info->width=(unsigned int) ((int) image->columns-crop_info->x);
  if ((crop_info->y+(int) crop_info->height) > (int) image->rows)
    crop_info->height=(unsigned int) ((int) image->rows-crop_info->y);
  if (crop_info->x < 0)
    {
      crop_info->width-=(unsigned int) (-crop_info->x);
      crop_info->x=0;
    }
  if (crop_info->y < 0)
    {
      crop_info->height-=(unsigned int) (-crop_info->y);
      crop_info->y=0;
    }
  if ((crop_info->width == 0) && (crop_info->height == 0))
    {
      register int
        i;

      RunlengthPacket
        corners[4];

      /*
        Set bounding box to the image dimensions.
      */
      crop_info->width=0;
      crop_info->height=0;
      crop_info->x=image->columns;
      crop_info->y=image->rows;
      p=image->pixels;
      image->runlength=p->length+1;
      corners[0]=(*p);
      for (i=1; i <= (image->rows*image->columns); i++)
      {
        if (image->runlength != 0)
          image->runlength--;
        else
          {
            p++;
            image->runlength=p->length;
          }
        if (i == image->columns)
          corners[1]=(*p);
        if (i == (image->rows*image->columns-image->columns+1))
          corners[2]=(*p);
        if (i == (image->rows*image->columns))
          corners[3]=(*p);
      }
      p=image->pixels;
      image->runlength=p->length+1;
      for (y=0; y < image->rows; y++)
      {
        for (x=0; x < image->columns; x++)
        {
          if (image->runlength != 0)
            image->runlength--;
          else
            {
              p++;
              image->runlength=p->length;
            }
          if (!ColorMatch(*p,corners[0],DeltaX))
            if (x < crop_info->x)
              crop_info->x=x;
          if (!ColorMatch(*p,corners[1],DeltaX))
            if (x > crop_info->width)
              crop_info->width=x;
          if (!ColorMatch(*p,corners[0],DeltaX))
            if (y < crop_info->y)
              crop_info->y=y;
          if (!ColorMatch(*p,corners[2],DeltaX))
            if (y > crop_info->height)
              crop_info->height=y;
        }
      }
      if ((crop_info->width != 0) || (crop_info->height != 0))
        {
          crop_info->width-=crop_info->x-1;
          crop_info->height-=crop_info->y-1;
        }
    }
  if ((crop_info->width == 0) || (crop_info->height == 0))
    {
      Warning("Unable to crop image","geometry dimensions are zero");
      return((Image *) NULL);
    }
  if ((crop_info->width == image->columns) &&
      (crop_info->height == image->rows) && (crop_info->x == 0) &&
      (crop_info->y == 0))
    return((Image *) NULL);
  /*
    Initialize cropped image attributes.
  */
  cropped_image=CopyImage(image,crop_info->width,crop_info->height,True);
  if (cropped_image == (Image *) NULL)
    {
      Warning("Unable to crop image","Memory allocation failed");
      return((Image *) NULL);
    }
  /*
    Set cropped page geometry.
  */
  x=0;
  y=0;
  width=crop_info->width;
  height=crop_info->height;
  if (cropped_image->page != (char *) NULL)
    (void) XParseGeometry(cropped_image->page,&x,&y,&width,&height);
  (void) sprintf(geometry,"%ux%u%+d%+d",width,height,x+crop_info->x,
    y+crop_info->y);
  cropped_image->page=PostscriptGeometry(geometry);
  /*
    Skip pixels up to the cropped image.
  */
  p=image->pixels;
  image->runlength=p->length+1;
  for (x=0; x < (crop_info->y*image->columns+crop_info->x); x++)
    if (image->runlength != 0)
      image->runlength--;
    else
      {
        p++;
        image->runlength=p->length;
      }
  /*
    Extract cropped image.
  */
  cropped_image->packets=0;
  q=cropped_image->pixels;
  q->red=0;
  q->green=0;
  q->blue=0;
  q->index=0;
  q->length=MaxRunlength;
  for (y=0; y < (cropped_image->rows-1); y++)
  {
    /*
      Transfer scanline.
    */
    for (x=0; x < cropped_image->columns; x++)
    {
      if (image->runlength != 0)
        image->runlength--;
      else
        {
          p++;
          image->runlength=p->length;
        }
      if ((p->red == q->red) && (p->green == q->green) &&
          (p->blue == q->blue) && (p->index == q->index) &&
          ((int) q->length < MaxRunlength))
        q->length++;
      else
        {
          if (cropped_image->packets != 0)
            q++;
          cropped_image->packets++;
          *q=(*p);
          q->length=0;
        }
    }
    /*
      Skip to next scanline.
    */
    for (x=0; x < (int) (image->columns-cropped_image->columns); x++)
      if (image->runlength != 0)
        image->runlength--;
      else
        {
          p++;
          image->runlength=p->length;
        }
    ProgressMonitor(CropImageText,y,cropped_image->rows-1);
  }
  /*
    Transfer last scanline.
  */
  for (x=0; x < cropped_image->columns; x++)
  {
    if (image->runlength != 0)
      image->runlength--;
    else
      {
        p++;
        image->runlength=p->length;
      }
    if ((p->red == q->red) && (p->green == q->green) &&
        (p->blue == q->blue) && (p->index == q->index) &&
        ((int) q->length < MaxRunlength))
      q->length++;
    else
      {
        if (cropped_image->packets != 0)
          q++;
        cropped_image->packets++;
        *q=(*p);
        q->length=0;
      }
  }
  cropped_image->pixels=(RunlengthPacket *) realloc((char *)
    cropped_image->pixels,cropped_image->packets*sizeof(RunlengthPacket));
  return(cropped_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     C y c l e I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function CycleColormapImage cycles the image colormap by a specified
%  amount.
%
%  The format of the CycleColormapImage routine is:
%
%      CycleColormapImage(image,amount)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o amount:  An unsigned value that specifies the offset of the colormap.
%
%
*/
void CycleColormapImage(Image *image,int amount)
{
#define CycleColormapImageText  "  Cycling image...  "

  int
    index;

  register RunlengthPacket
    *q;

  register unsigned int
    i;

  assert(image != (Image *) NULL);
  if (image->class == DirectClass)
    {
      QuantizeInfo
        quantize_info;

      GetQuantizeInfo(&quantize_info);
      quantize_info.number_colors=MaxColormapSize;
      QuantizeImage(&quantize_info,image);
    }
  q=image->pixels;
  for (i=0; i < image->packets; i++)
  {
    index=((int) q->index+amount) % image->colors;
    if (index < 0)
      index+=image->colors;
    q->index=(unsigned short) index;
    q++;
  }
  SyncImage(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   D e s c r i b e I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function DescribeImage describes an image by printing its attributes to
%  stderr.
%
%  The format of the DescribeImage routine is:
%
%      DescribeImage(image,file,verbose)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%    o file: send the image attributes to this file.
%
%    o verbose: an unsigned value other than zero prints detailed information
%      about the image.
%
%
*/
Export void DescribeImage(Image *image,FILE *file,const unsigned int verbose)
{
  char
    **textlist;

  Image
    *p;

  register int
    i;

  unsigned int
    count;

  assert(image != (Image *) NULL);
  assert(file != (FILE *) NULL);
  if (!verbose)
    {
      /*
        Display detailed info about the image.
      */
      if (*image->magick_filename != '\0')
        if (strcmp(image->magick_filename,image->filename) != 0)
          (void) fprintf(file,"%s=>",image->magick_filename);
       if ((image->previous == (Image *) NULL) &&
           (image->next == (Image *) NULL) && (image->scene == 0))
        (void) fprintf(file,"%s ",image->filename);
      else
        (void) fprintf(file,"%s[%u] ",image->filename,image->scene);
      if ((image->magick_columns != 0) || (image->magick_rows != 0))
        if ((image->magick_columns != image->columns) ||
            (image->magick_rows != image->rows))
          (void) fprintf(file,"%ux%u=>",image->magick_columns,
            image->magick_rows);
      if (image->page == (char *) NULL)
        (void) fprintf(file,"%ux%u ",image->columns,image->rows);
      else
        {
          int
            x,
            y;

          unsigned int
            sans;

          (void) XParseGeometry(image->page,&x,&y,&sans,&sans);
          (void) fprintf(file,"%ux%u%+d%+d ",image->columns,image->rows,x,y);
        }
      if (image->class == DirectClass)
        {
          (void) fprintf(file,"DirectClass ");
          if (image->total_colors != 0)
            (void) fprintf(file,"%luc ",image->total_colors);
        }
      else
        if (image->total_colors <= image->colors)
          (void) fprintf(file,"PseudoClass %uc ",image->colors);
        else
          {
            (void) fprintf(file,"PseudoClass %lu=>%uc ",image->total_colors,
              image->colors);
            (void) fprintf(file,"%u/%.6f/%.6fe ",image->mean_error_per_pixel,
              image->normalized_mean_error,image->normalized_maximum_error);
          }
      if (image->filesize != 0)
        (void) fprintf(file,"%ldb ",image->filesize);
      (void) fprintf(file,"%s %lds\n",image->magick,time((time_t *) NULL)-
        image->magick_time+1);
      return;
    }
  /*
    Display verbose info about the image.
  */
  (void) fprintf(file,"Image: %s\n",image->filename);
  if (image->class == DirectClass)
    (void) fprintf(file,"  class: DirectClass\n");
  else
    (void) fprintf(file,"  class: PseudoClass\n");
  if (image->class == DirectClass)
    {
      if (image->total_colors > 0)
        (void) fprintf(file,"  colors: %lu\n",image->total_colors);
    }
  else
    if (image->total_colors <= image->colors)
      (void) fprintf(file,"  colors: %u\n",image->colors);
    else
      (void) fprintf(file,"  colors: %lu=>%u\n",image->total_colors,
        image->colors);
  if (image->class == DirectClass)
    {
      if (image->total_colors < 1024)
        NumberColors(image,file);
    }
  else
    {
      char
        name[MaxTextExtent];

      ColorPacket
        *p;

      double
        distance_squared,
        min_distance;

      int
        distance;

      register int
        i;

      register XColorlist
        *q;

      /*
        Display image colormap.
      */
      p=image->colormap;
      for (i=0; i < image->colors; i++)
      {
        (void) fprintf(file,"    %d: (%3d,%3d,%3d)  #%02x%02x%02x",
          i,p->red,p->green,p->blue,(unsigned int) p->red,
          (unsigned int) p->green,(unsigned int) p->blue);
        min_distance=3.0*65536.0*65536.0;
        for (q=Colorlist; q->name != (char *) NULL; q++)
        {
          distance=(int) DownScale(p->red)-(int) q->red;
          distance_squared=(unsigned int) (distance*distance);
          distance=(int) DownScale(p->green)-(int) q->green;
          distance_squared+=(unsigned int) (distance*distance);
          distance=(int) DownScale(p->blue)-(int) q->blue;
          distance_squared+=(unsigned int) (distance*distance);
          if (distance_squared < min_distance)
            {
              min_distance=distance_squared;
              (void) strcpy(name,q->name);
            }
        }
        (void) fprintf(file,"  ");
        if (min_distance < 16)
          {
            if (min_distance > 0)
              (void) fprintf(file,"~");
            (void) fprintf(file,"%s",name);
          }
        (void) fprintf(file,"\n");
        p++;
      }
    }
  if (image->signature != (char *) NULL)
    (void) fprintf(file,"  signature: %s\n",image->signature);
  if (image->matte)
    (void) fprintf(file,"  matte: True\n");
  else
    (void) fprintf(file,"  matte: False\n");
  if (image->gamma != 0.0)
    (void) fprintf(file,"  gamma: %f\n",image->gamma);
  if (image->packets < (image->columns*image->rows))
    (void) fprintf(file,"  runlength packets: %u of %u\n",image->packets,
      image->columns*image->rows);
  (void) fprintf(file,"  geometry: %ux%u\n",image->columns,image->rows);
  if ((image->x_resolution != 0.0) && (image->y_resolution != 0.0))
    {
      /*
        Display image resolution.
      */
      (void) fprintf(file,"  resolution: %gx%g",image->x_resolution,
        image->y_resolution);
      if (image->units == UndefinedResolution)
        (void) fprintf(file," pixels\n");
      else
        if (image->units == PixelsPerInchResolution)
          (void) fprintf(file," pixels/inch\n");
        else
          if (image->units == PixelsPerCentimeterResolution)
            (void) fprintf(file," pixels/centimeter\n");
          else
            (void) fprintf(file,"\n");
    }
  (void) fprintf(file,"  depth: %u\n",image->depth);
  if (image->filesize != 0)
    (void) fprintf(file,"  bytes: %ld\n",image->filesize);
  if (image->interlace)
    (void) fprintf(file,"  interlaced: True\n");
  else
    (void) fprintf(file,"  interlaced: False\n");
  if (image->page != (char *) NULL)
    (void) fprintf(file,"  page geometry: %s\n",image->page);
  if (image->dispose)
    (void) fprintf(file,"  dispose method: %d\n",image->dispose);
  if (image->delay)
    (void) fprintf(file,"  delay: %d\n",image->delay);
  if (image->iterations != 1)
    (void) fprintf(file,"  iterations: %d\n",image->iterations);
  (void) fprintf(file,"  format: %s\n",image->magick);
  p=image;
  while (p->previous != (Image *) NULL)
    p=p->previous;
  for (count=1; p->next != (Image *) NULL; count++)
    p=p->next;
  if (count > 1)
    (void) fprintf(file,"  scene: %u of %u\n",image->scene,count);
  else
    if (image->scene != 0)
      (void) fprintf(file,"  scene: %u\n",image->scene);
  if (image->label != (char *) NULL)
    (void) fprintf(file,"  label: %s\n",image->label);
  if (image->comments != (char *) NULL)
    {
      /*
        Display image comment.
      */
      (void) fprintf(file,"  comments:\n");
      textlist=StringToList(image->comments);
      if (textlist != (char **) NULL)
        {
          for (i=0; textlist[i] != (char *) NULL; i++)
          {
            (void) fprintf(file,"  %s\n",textlist[i]);
            free(textlist[i]);
          }
          free((char *) textlist);
        }
    }
  if (image->montage != (char *) NULL)
    (void) fprintf(file,"  montage: %s\n",image->montage);
  if (image->directory != (char *) NULL)
    {
      char
        filename[MaxTextExtent];

      Image
        *tile;

      ImageInfo
        image_info;

      register char
        *p,
        *q;

      /*
        Display visual image directory.
      */
      GetImageInfo(&image_info);
      image_info.filename=filename;
      image_info.size="64x64";
      (void) fprintf(file,"  directory:\n");
      for (p=image->directory; *p != '\0'; p++)
      {
        q=p;
        while ((*q != '\n') && (*q != '\0'))
          q++;
        (void) strncpy(image_info.filename,p,q-p);
        image_info.filename[q-p]='\0';
        p=q;
        (void) fprintf(file,"    %s",image_info.filename);
        tile=ReadImage(&image_info);
        if (tile == (Image *) NULL)
          {
            (void) fprintf(file,"\n");
            continue;
          }
        (void) fprintf(file," %ux%u %s\n",tile->magick_columns,
          tile->magick_rows,tile->magick);
        if (tile->comments != (char *) NULL)
          {
            /*
              Display tile comment.
            */
            textlist=StringToList(tile->comments);
            if (textlist != (char **) NULL)
              {
                for (i=0; textlist[i] != (char *) NULL; i++)
                {
                  (void) fprintf(file,"    %s\n",textlist[i]);
                  free(textlist[i]);
                }
                free((char *) textlist);
              }
          }
        DestroyImage(tile);
      }
    }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   D e s t r o y I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function DestroyImage deallocates memory associated with an image.
%
%  The format of the DestroyImage routine is:
%
%      DestroyImage(image)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%
*/
Export void DestroyImage(Image *image)
{
  /*
    Close image.
  */
  assert(image != (Image *) NULL);
  if (image->file != (FILE *) NULL)
    CloseImage(image);
  /*
    Deallocate the image comments.
  */
  if (image->comments != (char *) NULL)
    free((char *) image->comments);
  /*
    Deallocate the image label.
  */
  if (image->label != (char *) NULL)
    free((char *) image->label);
  /*
    Deallocate the image montage directory.
  */
  if (image->montage != (char *) NULL)
    free((char *) image->montage);
  if (image->directory != (char *) NULL)
    free((char *) image->directory);
  /*
    Deallocate the image colormap.
  */
  if (image->colormap != (ColorPacket *) NULL)
    free((char *) image->colormap);
  /*
    Deallocate the image signature.
  */
  if (image->signature != (char *) NULL)
    free((char *) image->signature);
  /*
    Deallocate the image pixels.
  */
  if (image->pixels != (RunlengthPacket *) NULL)
    free((char *) image->pixels);
  if (image->packed_pixels != (unsigned char *) NULL)
    free((char *) image->packed_pixels);
  /*
    Deallocate the image page geometry.
  */
  if (image->page != (char *) NULL)
    free((char *) image->page);
  if (!image->orphan)
    {
      /*
        Unlink from linked list.
      */
      if (image->previous != (Image *) NULL)
        if (image->next != (Image *) NULL)
          image->previous->next=image->next;
        else
          image->previous->next=(Image *) NULL;
      if (image->next != (Image *) NULL)
        if (image->previous != (Image *) NULL)
          image->next->previous=image->previous;
        else
          image->next->previous=(Image *) NULL;
    }
  /*
    Deallocate the image structure.
  */
  free((char *) image);
  image=(Image *) NULL;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   D e s t r o y I m a g e s                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function DestroyImages deallocates memory associated with a linked list
%  of images.
%
%  The format of the DestroyImages routine is:
%
%      DestroyImages(image)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%
*/
Export void DestroyImages(Image *image)
{
  Image
    *next_image;

  /*
    Proceed to the top of the image list.
  */
  assert(image != (Image *) NULL);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  do
  {
    /*
      Destroy this image.
    */
    next_image=image->next;
    if (next_image != (Image *)NULL)
      next_image->previous=(Image *)NULL;
    DestroyImage(image);
    image=next_image;
  } while (image != (Image *) NULL);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   D r a w I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function DrawImage draws a primitive (line, rectangle, ellipse) on the
%  image.
%
%  The format of the DrawImage routine is:
%
%      DrawImage(image,annotate_info)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%    o annotate_info: The address of a DrawInfo structure.
%
%
*/
void DrawImage(Image *image,AnnotateInfo *annotate_info)
{
#define DrawImageText  "  Drawing on image...  "

  char
    keyword[MaxTextExtent];

  int
    n,
    y;

  PrimitiveInfo
    *primitive_info;

  PrimitiveType
    primitive;

  register char
    *p;

  register int
    i,
    j,
    x;

  register RunlengthPacket
    *q;

  unsigned int
    indirection,
    length,
    number_coordinates;

  XColor
    pen_color;

  /*
    Ensure the annotation info is valid.
  */
  assert(image != (Image *) NULL);
  assert(annotate_info != (AnnotateInfo *) NULL);
  if (annotate_info->primitive == (char *) NULL)
    return;
  if (!UncompressImage(image))
    return;
  indirection=(*annotate_info->primitive == '@');
  if (indirection)
    {
      FILE
        *file;

      int
        c;

      register char
        *q;

      /*
        Read text from a file.
      */
      file=(FILE *) fopen(annotate_info->primitive+1,"r");
      if (file == (FILE *) NULL)
        {
          Warning("Unable to read primitive file",annotate_info->primitive+1);
          return;
        }
      length=MaxTextExtent;
      annotate_info->primitive=(char *) malloc(length);
      q=annotate_info->primitive;
      while (annotate_info->primitive != (char *) NULL)
      {
        c=fgetc(file);
        if (c == EOF)
          break;
        if ((q-annotate_info->primitive+1) >= length)
          {
            *q='\0';
            length<<=1;
            annotate_info->primitive=(char *)
              realloc(annotate_info->primitive,length);
            if (annotate_info->primitive == (char *) NULL)
              break;
            q=annotate_info->primitive+Extent(annotate_info->primitive);
          }
        *q++=(unsigned char) c;
      }
      (void) fclose(file);
      if (annotate_info->primitive == (char *) NULL)
        {
          Warning("Unable to draw image","Memory allocation failed");
          return;
        }
      *q='\0';
    }
  /*
    Allocate primitive info memory.
  */
  number_coordinates=2048;
  primitive_info=(PrimitiveInfo *)
    malloc(number_coordinates*sizeof(PrimitiveInfo));
  annotate_info->geometry=(char *) malloc(MaxTextExtent*sizeof(char));
  annotate_info->text=(char *)
    malloc(Extent(annotate_info->primitive)*sizeof(char));
  if ((primitive_info == (PrimitiveInfo *) NULL) ||
      (annotate_info->geometry == (char *) NULL) ||
      (annotate_info->text == (char *) NULL))
    {
      if (indirection)
        free((char *) annotate_info->primitive);
      Warning("Unable to draw image","Memory allocation failed");
      return;
    }
  /*
    Parse the primitive attributes.
  */
  (void) XQueryColorDatabase(annotate_info->pen,&pen_color);
  image->class=DirectClass;
  primitive=UndefinedPrimitive;
  p=annotate_info->primitive;
  for (i=0; *p != '\0'; )
  {
    /*
      Define primitive.
    */
    while (isspace(*p))
      p++;
    for (x=0; isalpha(*p); x++)
    {
      keyword[x]=(*p);
      p++;
    }
    primitive=UndefinedPrimitive;
    if (Latin1Compare("Point",keyword) == 0)
      primitive=PointPrimitive;
    if (Latin1Compare("Line",keyword) == 0)
      primitive=LinePrimitive;
    if (Latin1Compare("Rectangle",keyword) == 0)
      primitive=RectanglePrimitive;
    if (Latin1Compare("FillRectangle",keyword) == 0)
      primitive=FillRectanglePrimitive;
    if (Latin1Compare("Circle",keyword) == 0)
      primitive=EllipsePrimitive;
    if (Latin1Compare("FillCircle",keyword) == 0)
      primitive=FillEllipsePrimitive;
    if (Latin1Compare("Polygon",keyword) == 0)
      primitive=PolygonPrimitive;
    if (Latin1Compare("FillPolygon",keyword) == 0)
      primitive=FillPolygonPrimitive;
    if (Latin1Compare("Color",keyword) == 0)
      primitive=ColorPrimitive;
    if (Latin1Compare("Matte",keyword) == 0)
      primitive=MattePrimitive;
    if (Latin1Compare("Text",keyword) == 0)
      primitive=TextPrimitive;
    if (Latin1Compare("Image",keyword) == 0)
      primitive=ImagePrimitive;
    if (primitive == UndefinedPrimitive)
      break;
    j=i;
    for (x=0; *p != '\0'; x++)
    {
      /*
        Define points.
      */
      while (isspace(*p))
        p++;
      if (!IsGeometry(p))
        break;
      primitive_info[i].primitive=primitive;
      primitive_info[i].coordinates=0;
      (void) sscanf(p,"%d%d%n",&primitive_info[i].x,&primitive_info[i].y,&n);
      (void) sscanf(p,"%d,%d%n",&primitive_info[i].x,&primitive_info[i].y,&n);
      (void) sscanf(p,"%d, %d%n",&primitive_info[i].x,&primitive_info[i].y,&n);
      (void) sscanf(p,"%d %d%n",&primitive_info[i].x,&primitive_info[i].y,&n);
      p+=n;
      i++;
      if (i < (number_coordinates-1))
        continue;
      number_coordinates<<=1;
      primitive_info=(PrimitiveInfo *)
        realloc(primitive_info,number_coordinates*sizeof(PrimitiveInfo));
      if (primitive_info != (PrimitiveInfo *) NULL)
        continue;
      if (indirection)
        free((char *) annotate_info->primitive);
      free((char *) annotate_info->geometry);
      free((char *) annotate_info->text);
      Warning("Unable to draw image","Memory allocation failed");
      return;
    }
    primitive_info[j].coordinates=x;
    primitive_info[j].method=FloodfillMethod;
    if ((primitive == ColorPrimitive) || (primitive == MattePrimitive))
      {
        /*
          Define method.
        */
        while (isspace(*p))
          p++;
        for (x=0; isalpha(*p); x++)
        {
          keyword[x]=(*p);
          p++;
        }
        if (Latin1Compare("point",keyword) == 0)
          primitive_info[j].method=PointMethod;
        else
          if (Latin1Compare("replace",keyword) == 0)
            primitive_info[j].method=ReplaceMethod;
          else
            if (Latin1Compare("floodfill",keyword) == 0)
              primitive_info[j].method=FloodfillMethod;
            else
              if (Latin1Compare("reset",keyword) == 0)
                primitive_info[j].method=ResetMethod;
              else
                primitive=UndefinedPrimitive;
        while (*p != '\0')
          p++;
      }
    primitive_info[j].text=(char *) NULL;
    if ((primitive == TextPrimitive) || (primitive == ImagePrimitive))
      {
        primitive_info[j].text=p;
        if (*p == '"')
          for (p++; (*p != '"') && (*p != '\0'); p++);
        else
          for (p++; (*p != ' ') && (*p != '\0'); p++);
        if (*p != '\0')
          p++;
      }
  }
  primitive_info[i].primitive=UndefinedPrimitive;
  if (primitive == UndefinedPrimitive)
    {
      Warning("Non-conforming drawing primitive definition",p);
      free((char *) primitive_info);
      if (indirection)
        free((char *) annotate_info->primitive);
      free((char *) annotate_info->geometry);
      free((char *) annotate_info->text);
      return;
    }
  /*
    Draw the primitive on the image.
  */
  q=image->pixels;
  for (y=0; y < image->rows; y++)
  {
    for (x=0; x < image->columns; x++)
    {
      if (InsidePrimitive(primitive_info,annotate_info,x,y,image))
        {
          q->red=XDownScale(pen_color.red);
          q->green=XDownScale(pen_color.green);
          q->blue=XDownScale(pen_color.blue);
        }
      q++;
    }
    ProgressMonitor(DrawImageText,y,image->rows);
  }
  free((char *) primitive_info);
  if (indirection)
    free((char *) annotate_info->primitive);
  free((char *) annotate_info->geometry);
  free((char *) annotate_info->text);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     E q u a l i z e I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function EqualizeImage performs histogram equalization on the reference
%  image.
%
%  The format of the EqualizeImage routine is:
%
%      EqualizeImage(image)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%
*/
void EqualizeImage(Image *image)
{
#define EqualizeImageText  "  Equalizing image...  "

  Quantum
    *equalize_map;

  register int
    i,
    j;

  register RunlengthPacket
    *p;

  unsigned int
    high,
    *histogram,
    low,
    *map;

  /*
    Allocate and initialize histogram arrays.
  */
  assert(image != (Image *) NULL);
  histogram=(unsigned int *) malloc((MaxRGB+1)*sizeof(unsigned int));
  map=(unsigned int *) malloc((MaxRGB+1)*sizeof(unsigned int));
  equalize_map=(Quantum *) malloc((MaxRGB+1)*sizeof(Quantum));
  if ((histogram == (unsigned int *) NULL) || (map == (unsigned int *) NULL) ||
      (equalize_map == (Quantum *) NULL))
    {
      Warning("Unable to equalize image","Memory allocation failed");
      return;
    }
  /*
    Form histogram.
  */
  for (i=0; i <= MaxRGB; i++)
    histogram[i]=0;
  p=image->pixels;
  for (i=0; i < image->packets; i++)
  {
    histogram[Intensity(*p)]+=(p->length+1);
    p++;
  }
  /*
    Integrate the histogram to get the equalization map.
  */
  j=0;
  for (i=0; i <= MaxRGB; i++)
  {
    j+=histogram[i];
    map[i]=j;
  }
  free((char *) histogram);
  if (map[MaxRGB] == 0)
    {
      free((char *) equalize_map);
      free((char *) map);
      return;
    }
  /*
    Equalize.
  */
  low=map[0];
  high=map[MaxRGB];
  for (i=0; i <= MaxRGB; i++)
    equalize_map[i]=(Quantum)
      ((((double) (map[i]-low))*MaxRGB)/Max(high-low,1));
  free((char *) map);
  /*
    Stretch the histogram.
  */
  switch (image->class)
  {
    case DirectClass:
    default:
    {
      /*
        Equalize DirectClass packets.
      */
      p=image->pixels;
      for (i=0; i < image->packets; i++)
      {
        p->red=equalize_map[p->red];
        p->green=equalize_map[p->green];
        p->blue=equalize_map[p->blue];
        p++;
        if (QuantumTick(i,image))
          ProgressMonitor(EqualizeImageText,i,image->packets);
      }
      break;
    }
    case PseudoClass:
    {
      /*
        Equalize PseudoClass packets.
      */
      for (i=0; i < image->colors; i++)
      {
        image->colormap[i].red=equalize_map[image->colormap[i].red];
        image->colormap[i].green=equalize_map[image->colormap[i].green];
        image->colormap[i].blue=equalize_map[image->colormap[i].blue];
      }
      SyncImage(image);
      break;
    }
  }
  free((char *) equalize_map);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   F l i p I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function FlipImage creates a new image that reflects each scanline in the
%  vertical direction It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  The format of the FlipImage routine is:
%
%      flipped_image=FlipImage(image)
%
%  A description of each parameter follows:
%
%    o flipped_image: Function FlipImage returns a pointer to the image
%      after reflecting.  A null image is returned if there is a memory
%      shortage.
%
%    o image: The address of a structure of type Image.
%
%
*/
Image *FlipImage(Image *image)
{
#define FlipImageText  "  Flipping image...  "

  Image
    *flipped_image;

  register RunlengthPacket
    *p,
    *q,
    *s;

  register unsigned int
    x,
    y;

  RunlengthPacket
    *scanline;

  /*
    Initialize flipped image attributes.
  */
  assert(image != (Image *) NULL);
  flipped_image=CopyImage(image,image->columns,image->rows,False);
  if (flipped_image == (Image *) NULL)
    {
      Warning("Unable to flip image","Memory allocation failed");
      return((Image *) NULL);
    }
  /*
    Allocate scan line buffer and column offset buffers.
  */
  scanline=(RunlengthPacket *) malloc(image->columns*sizeof(RunlengthPacket));
  if (scanline == (RunlengthPacket *) NULL)
    {
      Warning("Unable to reflect image","Memory allocation failed");
      DestroyImage(flipped_image);
      return((Image *) NULL);
    }
  /*
    Flip each row.
  */
  p=image->pixels;
  image->runlength=p->length+1;
  q=flipped_image->pixels+flipped_image->packets-1;
  for (y=0; y < flipped_image->rows; y++)
  {
    /*
      Read a scan line.
    */
    s=scanline;
    for (x=0; x < image->columns; x++)
    {
      if (image->runlength != 0)
        image->runlength--;
      else
        {
          p++;
          image->runlength=p->length;
        }
      *s=(*p);
      s++;
    }
    /*
      Flip each column.
    */
    s=scanline+image->columns;
    for (x=0; x < flipped_image->columns; x++)
    {
      s--;
      *q=(*s);
      q->length=0;
      q--;
    }
    ProgressMonitor(FlipImageText,y,flipped_image->rows);
  }
  free((char *) scanline);
  return(flipped_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   F l o p I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function FlopImage creates a new image that reflects each scanline in the
%  horizontal direction It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  The format of the FlopImage routine is:
%
%      flopped_image=FlopImage(image)
%
%  A description of each parameter follows:
%
%    o flopped_image: Function FlopImage returns a pointer to the image
%      after reflecting.  A null image is returned if there is a memory
%      shortage.
%
%    o image: The address of a structure of type Image.
%
%
*/
Image *FlopImage(Image *image)
{
#define FlopImageText  "  Flopping image...  "

  Image
    *flopped_image;

  register RunlengthPacket
    *p,
    *q,
    *s;

  register unsigned int
    x,
    y;

  RunlengthPacket
    *scanline;

  /*
    Initialize flopped image attributes.
  */
  assert(image != (Image *) NULL);
  flopped_image=CopyImage(image,image->columns,image->rows,False);
  if (flopped_image == (Image *) NULL)
    {
      Warning("Unable to reflect image","Memory allocation failed");
      return((Image *) NULL);
    }
  /*
    Allocate scan line buffer and column offset buffers.
  */
  scanline=(RunlengthPacket *) malloc(image->columns*sizeof(RunlengthPacket));
  if (scanline == (RunlengthPacket *) NULL)
    {
      Warning("Unable to reflect image","Memory allocation failed");
      DestroyImage(flopped_image);
      return((Image *) NULL);
    }
  /*
    Flop each row.
  */
  p=image->pixels;
  image->runlength=p->length+1;
  q=flopped_image->pixels;
  for (y=0; y < flopped_image->rows; y++)
  {
    /*
      Read a scan line.
    */
    s=scanline;
    for (x=0; x < image->columns; x++)
    {
      if (image->runlength != 0)
        image->runlength--;
      else
        {
          p++;
          image->runlength=p->length;
        }
      *s=(*p);
      s++;
    }
    /*
      Flop each column.
    */
    s=scanline+image->columns;
    for (x=0; x < flopped_image->columns; x++)
    {
      s--;
      *q=(*s);
      q->length=0;
      q++;
    }
    ProgressMonitor(FlopImageText,y,flopped_image->rows);
  }
  free((char *) scanline);
  return(flopped_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   F r a m e I m a g e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function FrameImage takes an image and puts a frame around it of a
%  particular color.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  The format of the FrameImage routine is:
%
%      framed_image=FrameImage(image,frame_info)
%
%  A description of each parameter follows:
%
%    o framed_image: Function FrameImage returns a pointer to the framed
%      image.  A null image is returned if there is a a memory shortage.
%
%    o image: The address of a structure of type Image.
%
%    o frame_info: Specifies a pointer to a FrameInfo structure which
%      defines the framed region.
%
%
*/
Image *FrameImage(Image *image,FrameInfo *frame_info)
{
#define FrameImageText  "  Adding frame to image...  "

  Image
    *framed_image;

  int
    height,
    width;

  register int
    x,
    y;

  register RunlengthPacket
    *p,
    *q;

  RunlengthPacket
    accentuate,
    highlight,
    matte,
    shadow,
    trough;

  unsigned int
    bevel_width;

  /*
    Check frame geometry.
  */
  assert(image != (Image *) NULL);
  assert(frame_info != (FrameInfo *) NULL);
  if ((frame_info->outer_bevel < 0) || (frame_info->inner_bevel < 0))
    {
      Warning("Unable to frame image","bevel width is negative");
      return((Image *) NULL);
    }
  bevel_width=frame_info->outer_bevel+frame_info->inner_bevel;
  width=(int) frame_info->width-frame_info->x-bevel_width;
  height=(int) frame_info->height-frame_info->y-bevel_width;
  if ((width < image->columns) || (height < image->rows))
    {
      Warning("Unable to frame image","frame is less than image size");
      return((Image *) NULL);
    }
  /*
    Initialize framed image attributes.
  */
  framed_image=CopyImage(image,frame_info->width,frame_info->height,False);
  if (framed_image == (Image *) NULL)
    {
      Warning("Unable to frame image","Memory allocation failed");
      return((Image *) NULL);
    }
  image->class=DirectClass;
  /*
    Initialize 3D effects color.
  */
  matte.red=image->matte_color.red;
  matte.green=image->matte_color.green;
  matte.blue=image->matte_color.blue;
  matte.index=Opaque;
  matte.length=0;
  accentuate.red=(unsigned int) (matte.red*AccentuateModulate+
    (MaxRGB-AccentuateModulate)*MaxRGB)/MaxRGB;
  accentuate.green=(unsigned int) (matte.green*AccentuateModulate+
    (MaxRGB-AccentuateModulate)*MaxRGB)/MaxRGB;
  accentuate.blue=(unsigned int) (matte.blue*AccentuateModulate+
    (MaxRGB-AccentuateModulate)*MaxRGB)/MaxRGB;
  accentuate.index=Opaque;
  accentuate.length=0;
  highlight.red=(unsigned int) (matte.red*HighlightModulate+
    (MaxRGB-HighlightModulate)*MaxRGB)/MaxRGB;
  highlight.green=(unsigned int) (matte.green*HighlightModulate+
    (MaxRGB-HighlightModulate)*MaxRGB)/MaxRGB;
  highlight.blue=(unsigned int) (matte.blue*HighlightModulate+
    (MaxRGB-HighlightModulate)*MaxRGB)/MaxRGB;
  highlight.index=Opaque;
  highlight.length=0;
  shadow.red=(unsigned int) (matte.red*ShadowModulate)/MaxRGB;
  shadow.green=(unsigned int) (matte.green*ShadowModulate)/MaxRGB;
  shadow.blue=(unsigned int) (matte.blue*ShadowModulate)/MaxRGB;
  shadow.index=Opaque;
  shadow.length=0;
  trough.red=(unsigned int) (matte.red*TroughModulate)/MaxRGB;
  trough.green=(unsigned int) (matte.green*TroughModulate)/MaxRGB;
  trough.blue=(unsigned int) (matte.blue*TroughModulate)/MaxRGB;
  trough.index=Opaque;
  trough.length=0;
  /*
    Put an ornamental border around the image.
  */
  q=framed_image->pixels;
  for (y=0; y < frame_info->outer_bevel; y++)
  {
    for (x=0; x < (int) (framed_image->columns-y); x++)
      if (x < y)
        *q++=highlight;
      else
        *q++=accentuate;
    for ( ; x < framed_image->columns; x++)
      *q++=shadow;
  }
  for (y=0; y < (int) (frame_info->y-bevel_width); y++)
  {
    for (x=0; x < frame_info->outer_bevel; x++)
      *q++=highlight;
    for (x=0; x < (int) (framed_image->columns-(frame_info->outer_bevel << 1)); x++)
      *q++=matte;
    for (x=0; x < frame_info->outer_bevel; x++)
      *q++=shadow;
  }
  for (y=0; y < frame_info->inner_bevel; y++)
  {
    for (x=0; x < frame_info->outer_bevel; x++)
      *q++=highlight;
    for (x=0; x < (int) (frame_info->x-bevel_width); x++)
      *q++=matte;
    for (x=0; x < (int) (image->columns+(frame_info->inner_bevel << 1)-y); x++)
      if (x < y)
        *q++=shadow;
      else
        *q++=trough;
    for ( ; x < (image->columns+(frame_info->inner_bevel << 1)); x++)
      *q++=highlight;
    width=frame_info->width-frame_info->x-image->columns-bevel_width;
    for (x=0; x < width; x++)
      *q++=matte;
    for (x=0; x < frame_info->outer_bevel; x++)
      *q++=shadow;
  }
  p=image->pixels;
  image->runlength=p->length+1;
  for (y=0; y < image->rows; y++)
  {
    /*
      Initialize scanline with border color.
    */
    for (x=0; x < frame_info->outer_bevel; x++)
      *q++=highlight;
    for (x=0; x < (int) (frame_info->x-bevel_width); x++)
      *q++=matte;
    for (x=0; x < frame_info->inner_bevel; x++)
      *q++=shadow;
    /*
      Transfer scanline.
    */
    for (x=0; x < image->columns; x++)
    {
      if (image->runlength != 0)
        image->runlength--;
      else
        {
          p++;
          image->runlength=p->length;
        }
      *q=(*p);
      q->length=0;
      q++;
    }
    for (x=0; x < frame_info->inner_bevel; x++)
      *q++=highlight;
    width=frame_info->width-frame_info->x-image->columns-bevel_width;
    for (x=0; x < width; x++)
      *q++=matte;
    for (x=0; x < frame_info->outer_bevel; x++)
      *q++=shadow;
    ProgressMonitor(FrameImageText,y,image->rows);
  }
  for (y=frame_info->inner_bevel-1; y >= 0; y--)
  {
    for (x=0; x < frame_info->outer_bevel; x++)
      *q++=highlight;
    for (x=0; x < (int) (frame_info->x-bevel_width); x++)
      *q++=matte;
    for (x=0; x < y; x++)
      *q++=shadow;
    for ( ; x < (image->columns+(frame_info->inner_bevel << 1)); x++)
      if (x >= (image->columns+(frame_info->inner_bevel << 1)-y))
        *q++=highlight;
      else
        *q++=accentuate;
    width=frame_info->width-frame_info->x-image->columns-bevel_width;
    for (x=0; x < width; x++)
      *q++=matte;
    for (x=0; x < frame_info->outer_bevel; x++)
      *q++=shadow;
  }
  height=frame_info->height-frame_info->y-image->rows-bevel_width;
  for (y=0; y < height; y++)
  {
    for (x=0; x < frame_info->outer_bevel; x++)
      *q++=highlight;
    for (x=0; x < (int) (framed_image->columns-(frame_info->outer_bevel << 1)); x++)
      *q++=matte;
    for (x=0; x < frame_info->outer_bevel; x++)
      *q++=shadow;
  }
  for (y=frame_info->outer_bevel-1; y >= 0; y--)
  {
    for (x=0; x < y; x++)
      *q++=highlight;
    for ( ; x < framed_image->columns; x++)
      if (x >= (framed_image->columns-y))
        *q++=shadow;
      else
        *q++=trough;
  }
  return(framed_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     G a m m a I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function GammaImage converts the reference image to gamma corrected colors.
%
%  The format of the GammaImage routine is:
%
%      GammaImage(image,gamma)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o gamma: A character string indicating the level of gamma correction.
%
%
*/
void GammaImage(Image *image,char *gamma)
{
#define GammaImageText  "  Gamma correcting the image...  "

  ColorPacket
    *gamma_map;

  double
    blue_gamma,
    green_gamma,
    red_gamma;

  int
    count;

  register int
    i;

  register RunlengthPacket
    *p;

  assert(image != (Image *) NULL);
  if (gamma == (char *) NULL)
    return;
  red_gamma=1.0;
  green_gamma=1.0;
  blue_gamma=1.0;
  count=sscanf(gamma,"%lf,%lf,%lf",&red_gamma,&green_gamma,&blue_gamma);
  count=sscanf(gamma,"%lf/%lf/%lf",&red_gamma,&green_gamma,&blue_gamma);
  if (count == 1)
    {
      if (red_gamma == 1.0)
        return;
      green_gamma=red_gamma;
      blue_gamma=red_gamma;
    }
  /*
    Allocate and initialize gamma maps.
  */
  gamma_map=(ColorPacket *) malloc((MaxRGB+1)*sizeof(ColorPacket));
  if (gamma_map == (ColorPacket *) NULL)
    {
      Warning("Unable to gamma image","Memory allocation failed");
      return;
    }
  for (i=0; i <= MaxRGB; i++)
  {
    gamma_map[i].red=0;
    gamma_map[i].green=0;
    gamma_map[i].blue=0;
  }
  /*
    Initialize gamma table.
  */
  for (i=0; i <= MaxRGB; i++)
  {
    if (red_gamma != 0.0)
      gamma_map[i].red=(Quantum)
        ((pow((double) i/MaxRGB,1.0/red_gamma)*MaxRGB)+0.5);
    if (green_gamma != 0.0)
      gamma_map[i].green=(Quantum)
        ((pow((double) i/MaxRGB,1.0/green_gamma)*MaxRGB)+0.5);
    if (blue_gamma != 0.0)
      gamma_map[i].blue=(Quantum)
        ((pow((double) i/MaxRGB,1.0/blue_gamma)*MaxRGB)+0.5);
  }
  switch (image->class)
  {
    case DirectClass:
    default:
    {
      /*
        Gamma-correct DirectClass image.
      */
      p=image->pixels;
      for (i=0; i < image->packets; i++)
      {
        p->red=gamma_map[p->red].red;
        p->green=gamma_map[p->green].green;
        p->blue=gamma_map[p->blue].blue;
        p++;
        if (QuantumTick(i,image))
          ProgressMonitor(GammaImageText,i,image->packets);
      }
      break;
    }
    case PseudoClass:
    {
      /*
        Gamma-correct PseudoClass image.
      */
      for (i=0; i < image->colors; i++)
      {
        image->colormap[i].red=gamma_map[image->colormap[i].red].red;
        image->colormap[i].green=gamma_map[image->colormap[i].green].green;
        image->colormap[i].blue=gamma_map[image->colormap[i].blue].blue;
      }
      SyncImage(image);
      break;
    }
  }
  if (image->gamma != 0.0)
    image->gamma*=(red_gamma+green_gamma+blue_gamma)/3.0;
  free((char *) gamma_map);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   G e t A n n o t a t e I n f o                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function GetAnnotateInfo initializes the AnnotateInfo structure.
%
%  The format of the GetAnnotateInfo routine is:
%
%      GetAnnotateInfo(annotate_info)
%
%  A description of each parameter follows:
%
%    o annotate_info: Specifies a pointer to a AnnotateInfo structure.
%
%
*/
void GetAnnotateInfo(AnnotateInfo *annotate_info)
{
  assert(annotate_info != (AnnotateInfo *) NULL);
  annotate_info->server_name=(char *) NULL;
  annotate_info->font=(char *) NULL;
  annotate_info->pointsize=atoi(DefaultPointSize);
  annotate_info->box=(char *) NULL;
  annotate_info->pen=(char *) NULL;
  annotate_info->geometry=(char *) NULL;
  annotate_info->text=(char *) NULL;
  annotate_info->primitive=(char *) NULL;
  annotate_info->linewidth=1;
  annotate_info->center=False;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   G e t I m a g e I n f o                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function GetImageInfo initializes the ImageInfo structure.
%
%  The format of the GetImageInfo routine is:
%
%      GetImageInfo(image_info)
%
%  A description of each parameter follows:
%
%    o image_info: Specifies a pointer to a ImageInfo structure.
%
%
*/
Export void GetImageInfo(ImageInfo *image_info)
{
  assert(image_info != (ImageInfo *) NULL);
  *image_info->magick='\0';
  image_info->filename=(char *) malloc(MaxTextExtent);
  if (image_info->filename == (char *) NULL)
    Error("Unable to get image info","Memory allocation failed");
  *image_info->filename='\0';
  image_info->affirm=False;
  image_info->subimage=0;
  image_info->subrange=0;
  image_info->server_name=(char *) NULL;
  image_info->font=(char *) NULL;
  image_info->size=(char *) NULL;
  image_info->tile=(char *) NULL;
  image_info->density=(char *) NULL;
  image_info->page=(char *) NULL;
  image_info->dispose=(char *) NULL;
  image_info->delay=(char *) NULL;
  image_info->iterations=(char *) NULL;
  image_info->texture=(char *) NULL;
  image_info->adjoin=True;
  image_info->compression=RunlengthEncodedCompression;
#ifdef HasPNG
  image_info->compression=ZipCompression;
#endif
  image_info->dither=True;
  image_info->interlace=DefaultInterlace;
  image_info->monochrome=False;
  image_info->pointsize=atoi(DefaultPointSize);
  image_info->quality=atoi(DefaultImageQuality);
  image_info->verbose=False;
  image_info->preview_type=GammaPreview;
  image_info->undercolor=(char *) NULL;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     I s G e o m e t r y                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function IsGeometry returns True if the geometry specification is valid
%  as determined by XParseGeometry.
%
%  The format of the IsGeometry routine is:
%
%      status=IsGeometry(geometry)
%
%  A description of each parameter follows:
%
%    o status: Function IsGeometry returns True if the image is gray_scale
%      otherwise False is returned.
%
%    o geometry: This string is the geometry specification.
%
%
*/
Export unsigned int IsGeometry(char *geometry)
{
  float
    value;

  int
    x,
    y;

  unsigned int
    flags,
    height,
    width;

  if (geometry == (char *) NULL)
    return(NoValue);
  flags=XParseGeometry(geometry,&x,&y,&width,&height);
  return(flags || sscanf(geometry,"%f",&value));
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     I s G r a y I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function IsGrayImage returns True if the image is gray_scale otherwise
%  False is returned.  If the image is DirectClass and gray_scale, it is demoted
%  to PseudoClass.
%
%  The format of the IsGrayImage routine is:
%
%      status=IsGrayImage(image)
%
%  A description of each parameter follows:
%
%    o status: Function IsGrayImage returns True if the image is gray_scale
%      otherwise False is returned.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%
*/
unsigned int IsGrayImage(Image *image)
{
  register int
    i;

  unsigned int
    gray_scale;

  /*
    Determine if image is gray_scale.
  */
  assert(image != (Image *) NULL);
  gray_scale=True;
  switch (image->class)
  {
    case DirectClass:
    default:
    {
      register RunlengthPacket
        *p;

      if (image->matte)
        return(False);
      p=image->pixels;
      for (i=0; i < image->packets; i++)
      {
        if (!IsGray(*p))
          {
            gray_scale=False;
            break;
          }
        p++;
      }
      if (gray_scale)
        {
          QuantizeInfo
            quantize_info;

          GetQuantizeInfo(&quantize_info);
          quantize_info.colorspace=GRAYColorspace;
          QuantizeImage(&quantize_info,image);
          SyncImage(image);
        }
      break;
    }
    case PseudoClass:
    {
      for (i=0; i < image->colors; i++)
        if (!IsGray(image->colormap[i]))
          {
            gray_scale=False;
            break;
          }
      break;
    }
  }
  return(gray_scale);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   L a b e l I m a g e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function LabelImage initializes an image label.  Optionally the label
%  can include the image filename, type, width, height, or scene number by
%  embedding special format characters.  Embed %f for filename, %m for
%  magick, %w for width, %h for height, or %s for scene number.  For
%  example,
%
%     %f  %wx%h
%
%  produces an image label of
%
%     bird.miff  512x480
%
%  for an image titled bird.miff and whose width is 512 and height is 480.
%
%  The format of the LabelImage routine is:
%
%      LabelImage(image,label)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%    o label: The address of a character string containing the label format.
%
%
*/
void LabelImage(Image *image,char *label)
{
  register char
    *p,
    *q;

  unsigned int
    indirection,
    length;

  assert(image != (Image *) NULL);
  if (image->label != (char *) NULL)
    free((char *) image->label);
  image->label=(char *) NULL;
  if (label == (char *) NULL)
    return;
  if (*label == '\0')
    return;
  indirection=(*label == '@');
  if (indirection)
    {
      FILE
        *file;

      int
        c;

      /*
        Read label from a file.
      */
      file=(FILE *) fopen(label+1,"r");
      if (file == (FILE *) NULL)
        {
          Warning("Unable to read label file",label+1);
          return;
        }
      length=MaxTextExtent;
      label=(char *) malloc(length);
      for (q=label; label != (char *) NULL; q++)
      {
        c=fgetc(file);
        if (c == EOF)
          break;
        if ((q-label+1) >= length)
          {
            *q='\0';
            length<<=1;
            label=(char *) realloc((char *) label,length);
            if (label == (char *) NULL)
              break;
            q=label+Extent(label);
          }
        *q=(unsigned char) c;
      }
      (void) fclose(file);
      if (label == (char *) NULL)
        {
          Warning("Unable to label image","Memory allocation failed");
          return;
        }
      *q='\0';
    }
  /*
    Allocate and initialize image label.
  */
  p=label;
  length=Extent(label)+MaxTextExtent;
  image->label=(char *) malloc(length);
  for (q=image->label; image->label != (char *) NULL; p++)
  {
    *q='\0';
    if (*p == '\0')
      break;
    if ((q-image->label+MaxTextExtent) >= length)
      {
        length<<=1;
        image->label=(char *) realloc((char *) image->label,length);
        if (image->label == (char *) NULL)
          break;
        q=image->label+Extent(image->label);
      }
    /*
      Process formatting characters in label.
    */
    if ((*p == '\\') && (*(p+1) == 'n'))
      {
        *q++='\n';
        p++;
        continue;
      }
    if (*p != '%')
      {
        *q++=(*p);
        continue;
      }
    p++;
    switch (*p)
    {
      case 'b':
      {
        (void) sprintf(q,"%ld",image->filesize/1000);
        q=image->label+Extent(image->label);
        break;
      }
      case 'f':
      {
        register char
          *p;

        /*
          Label segment is the base of the filename.
        */
        if (Extent(image->magick_filename) == 0)
          break;
        p=image->magick_filename+Extent(image->magick_filename)-1;
        while ((p > image->magick_filename) && (*(p-1) != '/'))
          p--;
        (void) strcpy(q,p);
        q+=Extent(p);
        break;
      }
      case 'h':
      {
        (void) sprintf(q,"%u",image->magick_rows);
        q=image->label+Extent(image->label);
        break;
      }
      case 'm':
      {
        (void) strcpy(q,image->magick);
        q+=Extent(image->magick);
        break;
      }
      case 's':
      {
        (void) sprintf(q,"%u",image->scene);
        q=image->label+Extent(image->label);
        break;
      }
      case 'w':
      {
        (void) sprintf(q,"%u",image->magick_columns);
        q=image->label+Extent(image->label);
        break;
      }
      default:
      {
        *q++='%';
        *q++=(*p);
        break;
      }
    }
  }
  if (image->label == (char *) NULL)
    {
      Warning("Unable to label image","Memory allocation failed");
      return;
    }
  *q='\0';
  if (indirection)
    free((char *) label);
}
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     L i s t T o G r o u p I m a g e                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ListToGroupImage converts a linked list of images to a sequential
%  array.
%
%  The format of the ListToGroupImage routine is:
%
%      images=ListToGroupImage(images,number_images)
%
%  A description of each parameter follows:
%
%    o images: Function ListToGroupImage converts a linked list of images to
%      a sequential array and returns the array..
%
%    o images: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o number_images:  A pointer to an unsigned integer.  The number of images
%      in the image array is returned here.
%
%
*/
Export Image **ListToGroupImage(Image *image,unsigned int *number_images)
{
  Image
    **images,
    *next_image;

  register int
    i;

  /*
    Determine the number of images in the list.
  */
  assert(image != (Image *) NULL);
  assert(number_images != (unsigned int *) NULL);
  next_image=image;
  for (i=0; next_image != (Image *) NULL; i++)
    next_image=next_image->next;
  images=(Image **) malloc(i*sizeof(Image *));
  if (images == (Image **) NULL)
    {
      Warning("Unable to convert image list","Memory allocation failed");
      return((Image **) NULL);
    }
  *number_images=i;
  /*
    Add each image in the linked list to the group.
  */
  next_image=image;
  for (i=0; next_image != (Image *) NULL; i++)
  {
    images[i]=next_image;
    next_image=next_image->next;
  }
  return(images);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   M a g n i f y I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function MagnifyImage creates a new image that is a integral size greater
%  than an existing one.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  MagnifyImage scans the reference image to create a magnified image by
%  bilinear interpolation.  The magnified image columns and rows become:
%
%    number_columns << 1
%    number_rows << 1
%
%  The format of the MagnifyImage routine is:
%
%      magnified_image=MagnifyImage(image)
%
%  A description of each parameter follows:
%
%    o magnified_image: Function MagnifyImage returns a pointer to the image
%      after magnification.  A null image is returned if there is a a memory
%      shortage.
%
%    o image: The address of a structure of type Image.
%
%
*/
Image *MagnifyImage(Image *image)
{
#define MagnifyImageText  "  Magnifying the image...  "

  Image
    *magnified_image;

  int
    y;

  register int
    x;

  register RunlengthPacket
    *p,
    *q,
    *r;

  /*
    Initialize magnified image attributes.
  */
  assert(image != (Image *) NULL);
  magnified_image=CopyImage(image,image->columns << 1,image->rows << 1,False);
  if (magnified_image == (Image *) NULL)
    {
      Warning("Unable to zoom image","Memory allocation failed");
      return((Image *) NULL);
    }
  magnified_image->class=DirectClass;
  /*
    Initialize zoom image pixels.
  */
  p=image->pixels;
  image->runlength=p->length+1;
  q=magnified_image->pixels;
  for (y=0; y < image->rows; y++)
  {
    for (x=0; x < image->columns; x++)
    {
      if (image->runlength != 0)
        image->runlength--;
      else
        {
          p++;
          image->runlength=p->length;
        }
      *q=(*p);
      q->length=0;
      q++;
    }
    q+=image->columns;
  }
  /*
    Magnify each row.
  */
  for (y=0; y < image->rows; y++)
  {
    p=magnified_image->pixels+(image->rows-1-y)*magnified_image->columns+
      (image->columns-1);
    q=magnified_image->pixels+((image->rows-1-y) << 1)*magnified_image->columns+
      ((image->columns-1) << 1);
    *q=(*p);
    *(q+1)=(*(p));
    for (x=1; x < image->columns; x++)
    {
      p--;
      q-=2;
      *q=(*p);
      (q+1)->red=(((int) p->red)+((int) (p+1)->red)+1) >> 1;
      (q+1)->green=(((int) p->green)+((int) (p+1)->green)+1) >> 1;
      (q+1)->blue=(((int) p->blue)+((int) (p+1)->blue)+1) >> 1;
      (q+1)->index=(((int) p->index)+((int) (p+1)->index)+1) >> 1;
      (q+1)->length=0;
    }
  }
  for (y=0; y < (image->rows-1); y++)
  {
    p=magnified_image->pixels+(y << 1)*magnified_image->columns;
    q=p+magnified_image->columns;
    r=q+magnified_image->columns;
    for (x=0; x < (image->columns-1); x++)
    {
      q->red=(((int) p->red)+((int) r->red)+1) >> 1;
      q->green=(((int) p->green)+((int) r->green)+1) >> 1;
      q->blue=(((int) p->blue)+((int) r->blue)+1) >> 1;
      q->index=(((int) p->index)+((int) r->index)+1) >> 1;
      q->length=0;
      (q+1)->red=(((int) p->red)+((int) (p+2)->red)+((int) r->red)+
        ((int) (r+2)->red)+2) >> 2;
      (q+1)->green=(((int) p->green)+((int) (p+2)->green)+((int) r->green)+
        ((int) (r+2)->green)+2) >> 2;
      (q+1)->blue=(((int) p->blue)+((int) (p+2)->blue)+((int) r->blue)+
        ((int) (r+2)->blue)+2) >> 2;
      (q+1)->index=(((int) p->index)+((int) (p+2)->index)+((int) r->index)+
        ((int) (r+2)->index)+2) >> 2;
      (q+1)->length=0;
      q+=2;
      p+=2;
      r+=2;
    }
    q->red=(((int) p->red)+((int) r->red)+1) >> 1;
    q->green=(((int) p->green)+((int) r->green)+1) >> 1;
    q->blue=(((int) p->blue)+((int) r->blue)+1) >> 1;
    q->index=(((int) p->index)+((int) r->index)+1) >> 1;
    q->length=0;
    p++;
    q++;
    r++;
    q->red=(((int) p->red)+((int) r->red)+1) >> 1;
    q->green=(((int) p->green)+((int) r->green)+1) >> 1;
    q->blue=(((int) p->blue)+((int) r->blue)+1) >> 1;
    q->index=(((int) p->index)+((int) r->index)+1) >> 1;
    q->length=0;
    p++;
    q++;
    r++;
    ProgressMonitor(MagnifyImageText,y,image->rows);
  }
  p=magnified_image->pixels+(2*image->rows-2)*magnified_image->columns;
  q=magnified_image->pixels+(2*image->rows-1)*magnified_image->columns;
  for (x=0; x < image->columns; x++)
  {
    *q++=(*p++);
    *q++=(*p++);
  }
  return(magnified_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   M a t t e F l o o d f i l l I m a g e                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function MatteFloodfillImage floodfills the designated area with a matte
%  value.  The floodfill algorithm is strongly based on a similiar algorithm in
%  "Graphics Gems" by Paul Heckbert.
%
%  The format of the MatteFloodfillImage routine is:
%
%      MatteFloodfillImage(image,x,y,matte,delta)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%    o x,y: Unsigned integers representing the current location of the pen.
%
%    o matte: A integer value representing the amount of transparency.
%
%    o delta: This is the allowed variance in color (fuzzy color).
%
%
*/
Export void MatteFloodfillImage(Image *image,int x,int y,
  const unsigned int matte,const int delta)
{
  int
    offset,
    skip,
    start,
    x1,
    x2;

  register RunlengthPacket
    *pixel;

  register XSegment
    *p;

  RunlengthPacket
    target;

  XSegment
    *segment_stack;

  /*
    Check boundary conditions.
  */
  assert(image != (Image *) NULL);
  if ((y < 0) || (y >= image->rows))
    return;
  if ((x < 0) || (x >= image->columns))
    return;
  target=image->pixels[y*image->columns+x];
  if (target.index == (unsigned short) matte)
    return;
  /*
    Allocate segment stack.
  */
  segment_stack=(XSegment *) malloc(MaxStacksize*sizeof(XSegment));
  if (segment_stack == (XSegment *) NULL)
    {
      Warning("Unable to floodfill","Memory allocation failed");
      return;
    }
  /*
    Push initial segment on stack.
  */
  start=0;
  p=segment_stack;
  Push(y,x,x,1);
  Push(y+1,x,x,-1);
  while (p > segment_stack)
  {
    /*
      Pop segment off stack.
    */
    p--;
    x1=p->x1;
    x2=p->x2;
    offset=p->y2;
    y=p->y1+offset;
    /*
      Update matte information in neighboring pixels.
    */
    for (x=x1; x >= 0 ; x--)
    {
      pixel=image->pixels+(y*image->columns+x);
      if (!MatteMatch(*pixel,target,delta))
        break;
      pixel->index=(unsigned short) matte;
    }
    skip=x >= x1;
    if (!skip)
      {
        start=x+1;
        if (start < x1)
          Push(y,start,x1-1,-offset);
        x=x1+1;
      }
    do
    {
      if (!skip)
        {
          for ( ; x < image->columns; x++)
          {
            pixel=image->pixels+(y*image->columns+x);
            if (!MatteMatch(*pixel,target,delta))
              break;
            pixel->index=(unsigned short) matte;
          }
          Push(y,start,x-1,offset);
          if (x > (x2+1))
            Push(y,x2+1,x-1,-offset);
        }
      skip=False;
      for (x++; x <= x2 ; x++)
      {
        pixel=image->pixels+(y*image->columns+x);
        if (MatteMatch(*pixel,target,delta))
          break;
      }
      start=x;
    } while (x <= x2);
  }
  free((char *) segment_stack);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   M i n i f y I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function MinifyImage creates a new image that is a integral size less than
%  an existing one.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  MinifyImage scans the reference image to create a minified image by computing
%  the weighted average of a 4x4 cell centered at each reference pixel.  The
%  target pixel requires two columns and two rows of the reference pixels.
%  Therefore the minified image columns and rows become:
%
%    number_columns/2
%    number_rows/2
%
%  Weights assume that the importance of neighboring pixels is negately
%  proportional to the square of their distance from the target pixel.
%
%  The scan only processes pixels that have a full set of neighbors.  Pixels
%  in the top, bottom, left, and right pairs of rows and columns are omitted
%  from the scan.
%
%  The format of the MinifyImage routine is:
%
%      minified_image=MinifyImage(image)
%
%  A description of each parameter follows:
%
%    o minified_image: Function MinifyImage returns a pointer to the image
%      after reducing.  A null image is returned if there is a a memory
%      shortage or if the image size is less than IconSize*2.
%
%    o image: The address of a structure of type Image.
%
%
*/
Image *MinifyImage(Image *image)
{
#define Minify(weight) \
  total_red+=(weight)*(s->red); \
  total_green+=(weight)*(s->green); \
  total_blue+=(weight)*(s->blue); \
  total_matte+=(weight)*(s->index); \
  s++;
#define MinifyImageText  "  Minifying image...  "

  Image
    *minified_image;

  register RunlengthPacket
    *p,
    *q,
    *s,
    *s0,
    *s1,
    *s2,
    *s3;

  register unsigned int
    x;

  RunlengthPacket
    *scanline;

  unsigned int
    y;

  unsigned int
    blue,
    green,
    packets,
    red;

  unsigned long
    total_matte,
    total_blue,
    total_green,
    total_red;

  unsigned short
    index;

  assert(image != (Image *) NULL);
  if ((image->columns < 4) || (image->rows < 4))
    {
      Warning("Unable to reduce image","image size must exceed 3x3");
      return((Image *) NULL);
    }
  /*
    Initialize minified image attributes.
  */
  packets=Max(image->packets >> 2,1);
  minified_image=CopyImage(image,packets,1,False);
  if (minified_image == (Image *) NULL)
    {
      Warning("Unable to reduce image","Memory allocation failed");
      return((Image *) NULL);
    }
  minified_image->class=DirectClass;
  minified_image->columns=image->columns >> 1;
  minified_image->rows=image->rows >> 1;
  minified_image->packets=0;
  /*
    Allocate image buffer and scanline buffer for 4 rows of the image.
  */
  scanline=(RunlengthPacket *)
    malloc(4*(image->columns+1)*sizeof(RunlengthPacket));
  if (scanline == (RunlengthPacket *) NULL)
    {
      Warning("Unable to reduce image","Memory allocation failed");
      DestroyImage(minified_image);
      return((Image *) NULL);
    }
  /*
    Preload the first 2 rows of the image.
  */
  p=image->pixels;
  image->runlength=p->length+1;
  for (x=0; x < (4*(image->columns+1)); x++)
    scanline[x]=(*p);
  s=scanline;
  for (x=0; x < (image->columns << 1); x++)
  {
    if (image->runlength != 0)
      image->runlength--;
    else
      {
        p++;
        image->runlength=p->length;
      }
    *s=(*p);
    s++;
  }
  /*
    Reduce each row.
  */
  p=image->pixels;
  image->runlength=p->length+1;
  q=minified_image->pixels;
  q->red=0;
  q->green=0;
  q->blue=0;
  q->index=0;
  q->length=MaxRunlength;
  for (y=0; y < (image->rows-1); y+=2)
  {
    /*
      Initialize sliding window pointers.
    */
    s0=scanline+image->columns*((y+0) % 4);
    s1=scanline+image->columns*((y+1) % 4);
    s2=scanline+image->columns*((y+2) % 4);
    s3=scanline+image->columns*((y+3) % 4);
    /*
      Read another scan line.
    */
    s=s2;
    for (x=0; x < image->columns; x++)
    {
      if (image->runlength != 0)
        image->runlength--;
      else
        {
          p++;
          image->runlength=p->length;
        }
      *s=(*p);
      s++;
    }
    /*
      Read another scan line.
    */
    s=s3;
    for (x=0; x < image->columns; x++)
    {
      if (image->runlength != 0)
        image->runlength--;
      else
        {
          p++;
          image->runlength=p->length;
        }
      *s=(*p);
      s++;
    }
    for (x=0; x < (image->columns-1); x+=2)
    {
      /*
        Compute weighted average of target pixel color components.

        These particular coefficients total to 128.  Use 128/2-1 or 63 to
        insure correct round off.
      */
      total_red=0;
      total_green=0;
      total_blue=0;
      total_matte=0;
      s=s0;
      Minify(3); Minify(7);  Minify(7);  Minify(3);
      s=s1;
      Minify(7); Minify(15); Minify(15); Minify(7);
      s=s2;
      Minify(7); Minify(15); Minify(15); Minify(7);
      s=s3;
      Minify(3); Minify(7);  Minify(7);  Minify(3);
      s0+=2;
      s1+=2;
      s2+=2;
      s3+=2;
      red=(Quantum) ((total_red+63) >> 7);
      green=(Quantum) ((total_green+63) >> 7);
      blue=(Quantum) ((total_blue+63) >> 7);
      index=(unsigned short) ((total_matte+63) >> 7);
      if ((red == q->red) && (green == q->green) && (blue == q->blue) &&
          (index == q->index) && ((int) q->length < MaxRunlength))
        q->length++;
      else
        {
          if (minified_image->packets != 0)
            q++;
          minified_image->packets++;
          if (minified_image->packets == packets)
            {
              packets<<=1;
              minified_image->pixels=(RunlengthPacket *) realloc((char *)
                minified_image->pixels,packets*sizeof(RunlengthPacket));
              if (minified_image->pixels == (RunlengthPacket *) NULL)
                {
                  Warning("Unable to reduce image","Memory allocation failed");
                  DestroyImage(minified_image);
                  return((Image *) NULL);
                }
              q=minified_image->pixels+minified_image->packets-1;
            }
          q->red=red;
          q->green=green;
          q->blue=blue;
          q->index=index;
          q->length=0;
        }
    }
    ProgressMonitor(MinifyImageText,y,image->rows-1);
  }
  minified_image->pixels=(RunlengthPacket *) realloc((char *)
    minified_image->pixels,minified_image->packets*sizeof(RunlengthPacket));
  free((char *) scanline);
  return(minified_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     M o d u l a t e I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ModulateImage modulates the hue, saturation, and brightness of an
%  image.
%
%  The format of the ModulateImage routine is:
%
%      ModulateImage(image,modulate)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o modulate: A character string indicating the percent change in hue,
%      saturation, and brightness.
%
%
*/
void ModulateImage(Image *image,char *modulate)
{
#define ModulateImageText  "  Modulating image...  "

  double
    percent_brightness,
    percent_hue,
    percent_saturation;

  register int
    i;

  register RunlengthPacket
    *p;

  /*
    Initialize gamma table.
  */
  assert(image != (Image *) NULL);
  if (modulate == (char *) NULL)
    return;
  percent_hue=0.0;
  percent_brightness=0.0;
  percent_saturation=0.0;
  (void) sscanf(modulate,"%lf,%lf,%lf",&percent_brightness,&percent_saturation,
    &percent_hue);
  (void) sscanf(modulate,"%lf/%lf/%lf",&percent_brightness,&percent_saturation,
    &percent_hue);
  switch (image->class)
  {
    case DirectClass:
    default:
    {
      /*
        Modulate the color for a DirectClass image.
      */
      p=image->pixels;
      for (i=0; i < image->packets; i++)
      {
        Modulate(percent_hue,percent_saturation,percent_brightness,
          &p->red,&p->green,&p->blue);
        p++;
        if (QuantumTick(i,image))
          ProgressMonitor(ModulateImageText,i,image->packets);
      }
      break;
    }
    case PseudoClass:
    {
      /*
        Modulate the color for a PseudoClass image.
      */
      for (i=0; i < image->colors; i++)
        Modulate(percent_hue,percent_saturation,percent_brightness,
          &image->colormap[i].red,&image->colormap[i].green,
          &image->colormap[i].blue);
      SyncImage(image);
      break;
    }
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     M o g r i f y I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function MogrifyImage applies image processing options to an image as
%  prescribed by command line options.
%
%  The format of the MogrifyImage routine is:
%
%      MogrifyImage(image_info,argc,argv,image)
%
%  A description of each parameter follows:
%
%    o image_info: Specifies a pointer to a ImageInfo structure.
%
%    o argc: Specifies a pointer to an integer describing the number of
%      elements in the argument vector.
%
%    o argv: Specifies a pointer to a text array containing the command line
%      arguments.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%
*/
Export void MogrifyImage(ImageInfo *image_info,int argc,char **argv,
  Image **image)
{
  AnnotateInfo
    annotate_info;

  char
    *option;

  Image
    *region_image;

  int
    flags,
    x,
    y;

  QuantizeInfo
    quantize_info;

  register int
    i;

  unsigned int
    height,
    width;

  XColor
    target_color;

  /*
    Initialize routine variables.
  */
  assert(image_info != (ImageInfo *) NULL);
  assert(image != (Image **) NULL);
  GetAnnotateInfo(&annotate_info);
  GetQuantizeInfo(&quantize_info);
  quantize_info.number_colors=0;
  quantize_info.tree_depth=0;
  quantize_info.dither=True;
  if (image_info->monochrome)
    if (!IsMonochromeImage(*image))
      {
        quantize_info.number_colors=2;
        quantize_info.tree_depth=8;
        quantize_info.colorspace=GRAYColorspace;
      }
  region_image=(Image *) NULL;
  /*
    Transmogrify the image.
  */
  for (i=1; i < argc; i++)
  {
    option=argv[i];
    if ((Extent(option) <= 1) || ((*option != '-') && (*option != '+')))
      continue;
    if (strncmp("-blur",option,4) == 0)
      {
        double
          factor;

        Image
          *blurred_image;

        /*
          Blur an image.
        */
        factor=atof(argv[++i]);
        blurred_image=BlurImage(*image,factor);
        if (blurred_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=blurred_image;
          }
        continue;
      }
    if (strcmp("-border",option) == 0)
      {
        Image
          *bordered_image;

        RectangleInfo
          border_info;

        /*
          Surround image with a border of solid color.
        */
        border_info.width=0;
        border_info.height=0;
        flags=XParseGeometry(argv[++i],&border_info.x,&border_info.y,
          &border_info.width,&border_info.height);
        if ((flags & HeightValue) == 0)
          border_info.height=border_info.width;
        bordered_image=BorderImage(*image,&border_info);
        if (bordered_image != (Image *) NULL)
          {
            DestroyImage(*image);
            bordered_image->class=DirectClass;
            *image=bordered_image;
          }
        continue;
      }
    if (strncmp("-bordercolor",option,8) == 0)
      {
        /*
          Determine RGB values of the border color.
        */
        (void) XQueryColorDatabase(argv[++i],&target_color);
        (*image)->border_color.red=XDownScale(target_color.red);
        (*image)->border_color.green=XDownScale(target_color.green);
        (*image)->border_color.blue=XDownScale(target_color.blue);
        continue;
      }
    if (strncmp("-box",option,3) == 0)
      {
        annotate_info.box=argv[++i];
        continue;
      }
    if (strncmp("-charcoal",option,3) == 0)
      {
        char
          *commands[8];

        QuantizeInfo
          local_info;

        /*
          Charcoal drawing.
        */
        i++;
        GetQuantizeInfo(&local_info);
        local_info.dither=quantize_info.dither;
        local_info.colorspace=GRAYColorspace;
        QuantizeImage(&local_info,*image);
        SyncImage(*image);
        commands[0]=client_name;
        commands[1]="-edge";
        commands[2]=argv[i];
        commands[3]="-blur";
        commands[4]=argv[i];
        commands[5]="-normalize";
        commands[6]="-negate";
        commands[7]="-grayscale";
        MogrifyImage(image_info,8,commands,image);
        continue;
      }
    if (strncmp("-colorize",option,8) == 0)
      {
        ColorizeImage(*image,argv[++i],annotate_info.pen);
        continue;
      }
    if (strcmp("-colors",option) == 0)
      {
        quantize_info.number_colors=atoi(argv[++i]);
        continue;
      }
    if (strncmp("-colorspace",option,8) == 0)
      {
        i++;
        option=argv[i];
        if (Latin1Compare("gray",option) == 0)
          {
            quantize_info.colorspace=GRAYColorspace;
            if (quantize_info.number_colors == 0)
              quantize_info.number_colors=256;
            quantize_info.tree_depth=8;
          }
        if (Latin1Compare("ohta",option) == 0)
          quantize_info.colorspace=OHTAColorspace;
        if (Latin1Compare("rgb",option) == 0)
          quantize_info.colorspace=RGBColorspace;
        if (Latin1Compare("transparent",option) == 0)
          quantize_info.colorspace=TransparentColorspace;
        if (Latin1Compare("xyz",option) == 0)
          quantize_info.colorspace=XYZColorspace;
        if (Latin1Compare("ycbcr",option) == 0)
          quantize_info.colorspace=YCbCrColorspace;
        if (Latin1Compare("yiq",option) == 0)
          quantize_info.colorspace=YIQColorspace;
        if (Latin1Compare("ypbpr",option) == 0)
          quantize_info.colorspace=YPbPrColorspace;
        if (Latin1Compare("yuv",option) == 0)
          quantize_info.colorspace=YUVColorspace;
        continue;
      }
    if (strncmp("comment",option+1,4) == 0)
      {
        if (*option == '-')
          CommentImage(*image,argv[++i]);
        else
          CommentImage(*image,(char *) NULL);
        continue;
      }
    if (strncmp("contrast",option+1,3) == 0)
      {
        ContrastImage(*image,(unsigned int) (*option == '-'));
        continue;
      }
    if (strncmp("-crop",option,3) == 0)
      {
        TransformImage(image,argv[++i],(char *) NULL);
        continue;
      }
    if (strncmp("-cycle",option,3) == 0)
      {
        /*
          Cycle an image colormap.
        */
        CycleColormapImage(*image,atoi(argv[++i]));
        continue;
      }
    if (strncmp("delay",option+1,3) == 0)
      {
        (*image)->delay=atoi(argv[++i]);
        continue;
      }
    if (strncmp("-despeckle",option,4) == 0)
      {
        Image
          *despeckled_image;

        /*
          Reduce the speckles within an image.
        */
        despeckled_image=DespeckleImage(*image);
        if (despeckled_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=despeckled_image;
          }
        continue;
      }
    if (strncmp("-display",option,6) == 0)
      {
        annotate_info.server_name=argv[++i];
        image_info->server_name=argv[i];
        continue;
      }
    if (strncmp("dispose",option+1,5) == 0)
      {
        (*image)->dispose=atoi(argv[++i]);
        continue;
      }
    if (strncmp("dither",option+1,3) == 0)
      {
        quantize_info.dither=(*option == '-');
        continue;
      }
    if (strncmp("-draw",option,3) == 0)
      {
        annotate_info.primitive=argv[++i];
        DrawImage(*image,&annotate_info);
        continue;
      }
    if (strncmp("-edge",option,3) == 0)
      {
        double
          factor;

        Image
          *edged_image;

        /*
          Detect edges in the image.
        */
        factor=atof(argv[++i]);
        edged_image=EdgeImage(*image,factor);
        if (edged_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=edged_image;
          }
        continue;
      }
    if (strncmp("-emboss",option,3) == 0)
      {
        Image
          *embossed_image;

        /*
          Emboss image.
        */
        embossed_image=EmbossImage(*image);
        if (embossed_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=embossed_image;
          }
        continue;
      }
    if (strncmp("-enhance",option,3) == 0)
      {
        Image
          *enhanced_image;

        /*
          Enhance image.
        */
        enhanced_image=EnhanceImage(*image);
        if (enhanced_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=enhanced_image;
          }
        continue;
      }
    if (strncmp("-equalize",option,3) == 0)
      {
        EqualizeImage(*image);
        continue;
      }
    if (strncmp("-flip",option,4) == 0)
      {
        Image
          *flipped_image;

        /*
          Flip image scanlines.
        */
        flipped_image=FlipImage(*image);
        if (flipped_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=flipped_image;
          }
        continue;
      }
    if (strncmp("-flop",option,4) == 0)
      {
        Image
          *flopped_image;

        /*
          Flop image scanlines.
        */
        flopped_image=FlopImage(*image);
        if (flopped_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=flopped_image;
          }
        continue;
      }
    if (strncmp("-font",option,3) == 0)
      {
        annotate_info.font=argv[++i];
        continue;
      }
    if (strcmp("-frame",option) == 0)
      {
        Image
          *framed_image;

        FrameInfo
          frame_info;

        /*
          Surround image with an ornamental border.
        */
        frame_info.width=0;
        frame_info.height=0;
        flags=XParseGeometry(argv[++i],&frame_info.outer_bevel,
          &frame_info.inner_bevel,&frame_info.width,&frame_info.height);
        if ((flags & HeightValue) == 0)
          frame_info.height=frame_info.width;
        if ((flags & XValue) == 0)
          frame_info.outer_bevel=(frame_info.width >> 2)+1;
        if ((flags & YValue) == 0)
          frame_info.inner_bevel=frame_info.outer_bevel;
        frame_info.x=frame_info.width;
        frame_info.y=frame_info.height;
        frame_info.width=(*image)->columns+(frame_info.width << 1);
        frame_info.height=(*image)->rows+(frame_info.height << 1);
        framed_image=FrameImage(*image,&frame_info);
        if (framed_image != (Image *) NULL)
          {
            DestroyImage(*image);
            framed_image->class=DirectClass;
            *image=framed_image;
          }
        continue;
      }
    if (strncmp("-gamma",option,3) == 0)
      {
        GammaImage(*image,argv[++i]);
        continue;
      }
    if (strncmp("-geometry",option,4) == 0)
      {
        TransformImage(image,(char *) NULL,argv[++i]);
        annotate_info.geometry=argv[i];
        continue;
      }
    if (strncmp("-implode",option,4) == 0)
      {
        double
          amount;

        Image
          *imploded_image;

        /*
          Implode image.
        */
        amount=atof(argv[++i]);
        imploded_image=ImplodeImage(*image,amount);
        if (imploded_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=imploded_image;
          }
        continue;
      }
    if (strncmp("interlace",option+1,3) == 0)
      {
        image_info->interlace=NoneInterlace;
        if (*option == '-')
          {
            option=argv[++i];
            if (Latin1Compare("none",option) == 0)
              image_info->interlace=NoneInterlace;
            if (Latin1Compare("line",option) == 0)
              image_info->interlace=LineInterlace;
            if (Latin1Compare("plane",option) == 0)
              image_info->interlace=PlaneInterlace;
            if (Latin1Compare("partition",option) == 0)
              image_info->interlace=PartitionInterlace;
          }
        continue;
      }
    if (strncmp("label",option+1,2) == 0)
      {
        if (*option == '-')
          LabelImage(*image,argv[++i]);
        else
          LabelImage(*image,(char *) NULL);
        continue;
      }
    if (strncmp("-linewidth",option,3) == 0)
      {
        annotate_info.linewidth=atoi(argv[++i]);
        continue;
      }
    if (strncmp("loop",option+1,2) == 0)
      {
        (*image)->iterations=atoi(argv[++i]);
        continue;
      }
    if (strcmp("-map",option) == 0)
      {
        Image
          *map_image;

        ImageInfo
          local_info;

        /*
          Transform image colors to match this set of colors.
        */
        local_info=(*image_info);
        (void) strcpy(local_info.filename,argv[++i]);
        map_image=ReadImage(&local_info);
        if (map_image != (Image *) NULL)
          {
            MapImage(*image,map_image,local_info.dither);
            DestroyImage(map_image);
          }
        continue;
      }
    if (strcmp("matte",option+1) == 0)
      {
        (*image)->matte=(*option == '-');
        continue;
      }
    if (strncmp("-mattecolor",option,7) == 0)
      {
        /*
          Determine RGB values of the border color.
        */
        (void) XQueryColorDatabase(argv[++i],&target_color);
        (*image)->matte_color.red=XDownScale(target_color.red);
        (*image)->matte_color.green=XDownScale(target_color.green);
        (*image)->matte_color.blue=XDownScale(target_color.blue);
        continue;
      }
    if (strncmp("-modulate",option,4) == 0)
      {
        ModulateImage(*image,argv[++i]);
        continue;
      }
    if (strncmp("negate",option+1,3) == 0)
      {
        NegateImage(*image,*option == '+');
        continue;
      }
    if (strncmp("noise",option+1,4) == 0)
      {
        Image
          *noisy_image;

        /*
          Reduce noise in image.
        */
        if (*option == '-')
          noisy_image=ReduceNoiseImage(*image);
        else
          {
            NoiseType
              noise_type;

            option=argv[++i];
            noise_type=UniformNoise;
            if (Latin1Compare("gaussian",option) == 0)
              noise_type=GaussianNoise;
            if (Latin1Compare("multiplicative",option) == 0)
              noise_type=MultiplicativeGaussianNoise;
            if (Latin1Compare("impulse",option) == 0)
              noise_type=ImpulseNoise;
            if (Latin1Compare("laplacian",option) == 0)
              noise_type=LaplacianNoise;
            if (Latin1Compare("poisson",option) == 0)
              noise_type=PoissonNoise;
            noisy_image=AddNoiseImage(*image,noise_type);
          }
        if (noisy_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=noisy_image;
          }
        continue;
      }
    if (strncmp("-normalize",option,4) == 0)
      {
        NormalizeImage(*image);
        continue;
      }
    if (strncmp("-opaque",option,3) == 0)
      {
        OpaqueImage(*image,argv[++i],annotate_info.pen);
        continue;
      }
    if (strncmp("page",option+1,3) == 0)
      {
        if ((*image)->page != (char *) NULL)
          free((char *) (*image)->page);
        (*image)->page=(char *) NULL;
        if (*option == '+')
          continue;
        (*image)->page=PostscriptGeometry(argv[++i]);
        continue;
      }
    if (strncmp("-paint",option,4) == 0)
      {
        Image
          *painted_image;

        /*
          Oil paint image.
        */
        painted_image=OilPaintImage(*image,atoi(argv[++i]));
        if (painted_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=painted_image;
          }
        continue;
      }
    if (strcmp("-pen",option) == 0)
      {
        annotate_info.pen=argv[++i];
        continue;
      }
    if (strncmp("-pointsize",option,3) == 0)
      {
        annotate_info.pointsize=atoi(argv[++i]);
        continue;
      }
    if (strncmp("-normalize",option,4) == 0)
      {
        NormalizeImage(*image);
        continue;
      }
    if (strncmp("raise",option+1,2) == 0)
      {
        RectangleInfo
          raise_info;

        /*
          Surround image with a raise of solid color.
        */
        raise_info.width=0;
        raise_info.height=0;
        flags=XParseGeometry(argv[++i],&raise_info.x,&raise_info.y,
          &raise_info.width,&raise_info.height);
        if ((flags & HeightValue) == 0)
          raise_info.height=raise_info.width;
        RaiseImage(*image,&raise_info,*option == '-');
        continue;
      }
    if (strncmp("region",option+1,2) == 0)
      {
        if (region_image != (Image *) NULL)
          {
            /*
              Composite region.
            */
            XParseGeometry(region_image->geometry,&x,&y,&width,&height);
            CompositeImage(region_image,ReplaceCompositeOp,*image,x,y);
            DestroyImage(*image);
            *image=region_image;
          }
        if (*option == '+')
          continue;
        region_image=CopyImage(*image,(*image)->columns,(*image)->rows,True);
        if (region_image == (Image *) NULL)
          continue;
        region_image->geometry=argv[++i];
        TransformImage(image,region_image->geometry,(char *) NULL);
        continue;
      }
    if (strncmp("-roll",option,4) == 0)
      {
        Image
          *rolled_image;

        /*
          Roll image.
        */
        x=0;
        y=0;
        flags=XParseGeometry(argv[++i],&x,&y,&width,&height);
        rolled_image=RollImage(*image,x,y);
        if (rolled_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=rolled_image;
          }
        continue;
      }
    if (strncmp("-rotate",option,4) == 0)
      {
        double
          degrees;

        Image
          *rotated_image;

        /*
          Check for conditional image rotation.
        */
        i++;
        if (strchr(argv[i],'>') != (char *) NULL)
          if ((*image)->columns <= (*image)->rows)
            break;
        if (strchr(argv[i],'<') != (char *) NULL)
          if ((*image)->columns >= (*image)->rows)
            break;
        /*
          Rotate image.
        */
        degrees=atof(argv[i]);
        rotated_image=RotateImage(*image,degrees,False,True);
        if (rotated_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=rotated_image;
          }
        continue;
      }
    if (strncmp("-sample",option,3) == 0)
      {
        Image
          *sampled_image;

        /*
          Sample image with pixel replication.
        */
        width=(*image)->columns;
        height=(*image)->rows;
        (void) ParseImageGeometry(argv[++i],&x,&y,&width,&height);
        sampled_image=SampleImage(*image,width,height);
        if (sampled_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=sampled_image;
          }
        continue;
      }
    if (strncmp("sans",option+1,2) == 0)
      if (*option == '-')
        i++;
    if (strcmp("-scene",option) == 0)
      {
        (*image)->scene=atoi(argv[++i]);
        continue;
      }
    if (strncmp("-segment",option,4) == 0)
      {
        float
          cluster_threshold,
          smoothing_threshold;

        /*
          Segment image.
        */
        cluster_threshold=1.0;
        smoothing_threshold=1.5;
        (void) sscanf(argv[++i],"%fx%f",&cluster_threshold,
          &smoothing_threshold);
        SegmentImage(*image,quantize_info.colorspace,image_info->verbose,
          (double) cluster_threshold,(double) smoothing_threshold);
        SyncImage(*image);
        continue;
      }
    if (strncmp("shade",option+1,5) == 0)
      {
        float
          azimuth,
          elevation;

        Image
          *shaded_image;

        /*
          Shade image.
        */
        azimuth=30.0;
        elevation=30.0;
        if (*option == '-')
          (void) sscanf(argv[++i],"%fx%f",&azimuth,&elevation);
        shaded_image=ShadeImage(*image,*option == '-',(double) azimuth,
          (double) elevation);
        if (shaded_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=shaded_image;
          }
        continue;
      }
    if (strncmp("-sharpen",option,5) == 0)
      {
        double
          factor;

        Image
          *sharpened_image;

        /*
          Sharpen an image.
        */
        factor=atof(argv[++i]);
        sharpened_image=SharpenImage(*image,factor);
        if (sharpened_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=sharpened_image;
          }
        continue;
      }
    if (strncmp("-shear",option,4) == 0)
      {
        float
          x_shear,
          y_shear;

        Image
          *sheared_image;

        /*
          Shear image.
        */
        x_shear=0.0;
        y_shear=0.0;
        (void) sscanf(argv[++i],"%fx%f",&x_shear,&y_shear);
        sheared_image=
          ShearImage(*image,(double) x_shear,(double) y_shear,False);
        if (sheared_image != (Image *) NULL)
          {
            DestroyImage(*image);
            sheared_image->class=DirectClass;
            *image=sheared_image;
          }
        continue;
      }
    if (strncmp("-solarize",option,3) == 0)
      {
        SolarizeImage(*image,atof(argv[++i]));
        continue;
      }
    if (strncmp("-spread",option,3) == 0)
      {
        unsigned int
          amount;

        Image
          *spread_image;

        /*
          Spread an image.
        */
        amount=atoi(argv[++i]);
        spread_image=SpreadImage(*image,amount);
        if (spread_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=spread_image;
          }
        continue;
      }
    if (strncmp("-swirl",option,3) == 0)
      {
        double
          degrees;

        Image
          *swirled_image;

        /*
          Swirl image.
        */
        degrees=atof(argv[++i]);
        swirled_image=SwirlImage(*image,degrees);
        if (swirled_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=swirled_image;
          }
        continue;
      }
    if (strncmp("-threshold",option,3) == 0)
      {
        ThresholdImage(*image,argv[++i]);
        continue;
      }
    if (strncmp("-transparent",option,4) == 0)
      {
        TransparentImage(*image,argv[++i]);
        continue;
      }
    if (strncmp("-treedepth",option,4) == 0)
      {
        quantize_info.tree_depth=atoi(argv[++i]);
        continue;
      }
  }
  if (quantize_info.number_colors != 0)
    {
      /*
        Reduce the number of colors in the image.
      */
      if (((*image)->class == DirectClass) ||
          ((*image)->colors > quantize_info.number_colors) ||
          (quantize_info.colorspace == GRAYColorspace))
        QuantizeImage(&quantize_info,*image);
      /*
        Measure quantization error.
      */
      if (image_info->verbose)
        QuantizationError(*image);
      SyncImage(*image);
    }
  if (region_image != (Image *) NULL)
    {
      /*
        Composite region.
      */
      XParseGeometry(region_image->geometry,&x,&y,&width,&height);
      CompositeImage(region_image,ReplaceCompositeOp,*image,x,y);
      DestroyImage(*image);
      *image=region_image;
    }
  if ((*image)->packets == ((*image)->columns*(*image)->rows))
    CompressImage(*image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     M o g r i f y I m a g e s                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function MogrifyImages applies image processing options to a sequence of
%  images as prescribed by command line options.
%
%  The format of the MogrifyImage routine is:
%
%      MogrifyImages(image_info,argc,argv,images)
%
%  A description of each parameter follows:
%
%    o image_info: Specifies a pointer to a ImageInfo structure.
%
%    o argc: Specifies a pointer to an integer describing the number of
%      elements in the argument vector.
%
%    o argv: Specifies a pointer to a text array containing the command line
%      arguments.
%
%    o images: The address of a structure of type Image;  returned from
%      ReadImage.
%
%
*/
Export void MogrifyImages(ImageInfo *image_info,int argc,char **argv,
  Image **images)
{
#define MogrifyImageText  "  Transforming images...  "

  Image
    *image,
    *mogrify_image;

  register int
    i;

  MonitorHandler
    handler;

  unsigned int
    number_images;

  assert(image_info != (ImageInfo *) NULL);
  assert(images != (Image **) NULL);
  image=(*images);
  for (number_images=1; image->next != (Image *) NULL; number_images++)
    image=image->next;
  ProgressMonitor(MogrifyImageText,0,number_images);
  handler=SetMonitorHandler((MonitorHandler) NULL);
  MogrifyImage(image_info,argc,argv,images);
  (void) SetMonitorHandler(handler);
  image=(*images);
  mogrify_image=(*images)->next;
  if (image_info->verbose)
    DescribeImage(image,stderr,False);
  for (i=1; mogrify_image != (Image *) NULL; i++)
  {
    handler=SetMonitorHandler((MonitorHandler) NULL);
    MogrifyImage(image_info,argc,argv,&mogrify_image);
    image->next=mogrify_image;
    image->next->previous=image;
    image=image->next;
    if (image_info->verbose)
      DescribeImage(mogrify_image,stderr,False);
    mogrify_image=mogrify_image->next;
    (void) SetMonitorHandler(handler);
    ProgressMonitor(MogrifyImageText,i,number_images);
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     N e g a t e I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function NegateImage negates the colors in the reference image.  The
%  Grayscale option means that only grayscale values within the image are
%  negated.
%
%  The format of the NegateImage routine is:
%
%      NegateImage(image,grayscale)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%
*/
void NegateImage(Image *image,unsigned int grayscale)
{
#define NegateImageText  "  Negating the image colors...  "

  register int
    i;

  register RunlengthPacket
    *p;

  assert(image != (Image *) NULL);
  switch (image->class)
  {
    case DirectClass:
    default:
    {
      /*
        Negate DirectClass packets.
      */
      p=image->pixels;
      for (i=0; i < image->packets; i++)
      {
        if (grayscale)
          if ((p->red != p->green) || (p->green != p->blue))
            continue;
        p->red=(~p->red);
        p->green=(~p->green);
        p->blue=(~p->blue);
        p++;
        if (QuantumTick(i,image))
          ProgressMonitor(NegateImageText,i,image->packets);
      }
      break;
    }
    case PseudoClass:
    {
      /*
        Negate PseudoClass packets.
      */
      for (i=0; i < image->colors; i++)
      {
        if (grayscale)
          if ((image->colormap[i].red != image->colormap[i].green) ||
              (image->colormap[i].green != image->colormap[i].blue))
            continue;
        image->colormap[i].red=(~image->colormap[i].red);
        image->colormap[i].green=(~image->colormap[i].green);
        image->colormap[i].blue=(~image->colormap[i].blue);
      }
      SyncImage(image);
      break;
    }
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     N o r m a l i z e I m a g e                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function NormalizeImage normalizes the pixel values to span the full
%  range of color values.  This is a contrast enhancement technique.
%
%  The format of the NormalizeImage routine is:
%
%      NormalizeImage(image)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%
*/
void NormalizeImage(Image *image)
{
#define NormalizeImageText  "  Normalizing image...  "

  int
    histogram[MaxRGB+1],
    threshold_intensity;

  Quantum
    gray_value,
    normalize_map[MaxRGB+1];

  register int
    i,
    intensity;

  register RunlengthPacket
    *p;

  unsigned int
    high,
    low;

  /*
    Form histogram.
  */
  assert(image != (Image *) NULL);
  for (i=0; i <= MaxRGB; i++)
    histogram[i]=0;
  p=image->pixels;
  for (i=0; i < image->packets; i++)
  {
    gray_value=Intensity(*p);
    histogram[gray_value]+=p->length+1;
    p++;
  }
  /*
    Find the histogram boundaries by locating the 1 percent levels.
  */
  threshold_intensity=(image->columns*image->rows)/100;
  intensity=0;
  for (low=0; low < MaxRGB; low++)
  {
    intensity+=histogram[low];
    if (intensity > threshold_intensity)
      break;
  }
  intensity=0;
  for (high=MaxRGB; high != 0; high--)
  {
    intensity+=histogram[high];
    if (intensity > threshold_intensity)
      break;
  }
  if (low == high)
    {
      /*
        Unreasonable contrast;  use zero threshold to determine boundaries.
      */
      threshold_intensity=0;
      intensity=0;
      for (low=0; low < MaxRGB; low++)
      {
        intensity+=histogram[low];
        if (intensity > threshold_intensity)
          break;
      }
      intensity=0;
      for (high=MaxRGB; high != 0; high--)
      {
        intensity+=histogram[high];
        if (intensity > threshold_intensity)
          break;
      }
      if (low == high)
        return;  /* zero span bound */
    }
  /*
    Stretch the histogram to create the normalized image mapping.
  */
  for (i=0; i <= MaxRGB; i++)
    if (i < (int) low)
      normalize_map[i]=0;
    else
      if (i > (int) high)
        normalize_map[i]=MaxRGB;
      else
        normalize_map[i]=(MaxRGB-1)*(i-low)/(high-low);
  /*
    Normalize the image.
  */
  switch (image->class)
  {
    case DirectClass:
    default:
    {
      /*
        Normalize DirectClass image.
      */
      p=image->pixels;
      for (i=0; i < image->packets; i++)
      {
        p->red=normalize_map[p->red];
        p->green=normalize_map[p->green];
        p->blue=normalize_map[p->blue];
        p++;
        if (QuantumTick(i,image))
          ProgressMonitor(NormalizeImageText,i,image->packets);
      }
      break;
    }
    case PseudoClass:
    {
      /*
        Normalize PseudoClass image.
      */
      for (i=0; i < image->colors; i++)
      {
        image->colormap[i].red=normalize_map[image->colormap[i].red];
        image->colormap[i].green=normalize_map[image->colormap[i].green];
        image->colormap[i].blue=normalize_map[image->colormap[i].blue];
      }
      SyncImage(image);
      break;
    }
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     O p a g u e I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function OpaqueImage changes the color of an opaque pixel to the pen color.
%
%  The format of the OpaqueImage routine is:
%
%      OpaqueImage(image,opaque_color,pen_color)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o opaque_color,
%      pen_color: A character string that contain an X11 color string.
%
%
*/
void OpaqueImage(Image *image,char *opaque_color,char *pen_color)
{
#define OpaqueImageText  "  Setting opaque color in the image...  "

  ColorPacket
    target;

  register int
    i;

  unsigned int
    status;

  XColor
    target_color;

  /*
    Determine RGB values of the opaque color.
  */
  assert(image != (Image *) NULL);
  status=XQueryColorDatabase(opaque_color,&target_color);
  if (status == False)
    return;
  target.red=XDownScale(target_color.red);
  target.green=XDownScale(target_color.green);
  target.blue=XDownScale(target_color.blue);
  status=XQueryColorDatabase(pen_color,&target_color);
  if (status == False)
    return;
  /*
    Make image color opaque.
  */
  switch (image->class)
  {
    case DirectClass:
    default:
    {
      register RunlengthPacket
        *p;

      /*
        Make DirectClass image opaque.
      */
      p=image->pixels;
      for (i=0; i < image->packets; i++)
      {
        if (ColorMatch(*p,target,DeltaX))
          {
            p->red=XDownScale(target_color.red);
            p->green=XDownScale(target_color.green);
            p->blue=XDownScale(target_color.blue);
          }
        p++;
        if (QuantumTick(i,image))
          ProgressMonitor(OpaqueImageText,i,image->packets);
      }
      break;
    }
    case PseudoClass:
    {
      register ColorPacket
        *p;

      /*
        Make PseudoClass image opaque.
      */
      p=image->colormap;
      for (i=0; i < image->colors; i++)
      {
        if (ColorMatch(*p,target,DeltaX))
          {
            p->red=XDownScale(target_color.red);
            p->green=XDownScale(target_color.green);
            p->blue=XDownScale(target_color.blue);
          }
        p++;
        if (QuantumTick(i,image))
          ProgressMonitor(OpaqueImageText,i,image->packets);
      }
      break;
    }
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   O p e n I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function OpenImage open a file associated with the image.  A file name of
%  '-' sets the file to stdin for type 'r' and stdout for type 'w'.  If the
%  filename suffix is '.gz' or '.Z', the image is decompressed for type 'r'
%  and compressed for type 'w'.  If the filename prefix is '|', it is piped
%  to or from a system command.
%
%  The format of the OpenImage routine is:
%
%      OpenImage(image_info,image,type)
%
%  A description of each parameter follows:
%
%    o image_info: Specifies a pointer to a ImageInfo structure.
%
%    o image: The address of a structure of type Image.
%
%    o type: 'r' for reading; 'w' for writing.
%
*/
void OpenImage(const ImageInfo *image_info,Image *image,const char *type)
{
  char
    filename[MaxTextExtent];

  assert(image_info != (ImageInfo *) NULL);
  assert(image != (Image *) NULL);
  assert(type != (char *) NULL);
  (void) strcpy(filename,image->filename);
  if (*filename != '|')
    if ((Extent(filename) > 3) &&
        (strcmp(filename+Extent(filename)-3,".gz") == 0))
      {
        /*
          Uncompress/compress image file with GNU compress utilities.
        */
        if (*type == 'r')
          (void) sprintf(filename,GunzipCommand,image->filename);
        else
          (void) sprintf(filename,GzipCommand,image->filename);
      }
    else
      if ((Extent(filename) > 2) &&
          (strcmp(filename+Extent(filename)-2,".Z") == 0))
        {
          /*
            Uncompress/compress image file with UNIX compress utilities.
          */
          if (*type == 'r')
            (void) sprintf(filename,UncompressCommand,image->filename);
          else
            (void) sprintf(filename,CompressCommand,image->filename);
        }
  /*
    Open image file.
  */
  image->pipe=False;
  if (strcmp(filename,"-") == 0)
    image->file=(*type == 'r') ? stdin : stdout;
  else
#if !defined(vms) && !defined(macintosh) && !defined(WIN32)
    if (*filename == '|')
      {
        char
          mode[MaxTextExtent];

        /*
          Pipe image to or from a system command.
        */
        if (*type == 'w')
          (void) signal(SIGPIPE,SIG_IGN);
        (void) strncpy(mode,type,1);
        image->file=(FILE *) popen(filename+1,mode);
        image->pipe=True;
      }
    else
#endif
      {
        if ((*type == 'w') && !image_info->adjoin)
          if ((image->previous != (Image *) NULL) ||
              (image->next != (Image *) NULL))
            {
              /*
                Form filename for multi-part images.
              */
              (void) sprintf(filename,image->filename,image->scene);
              if (strcmp(filename,image->filename) == 0)
                (void) sprintf(filename,"%s.%u",image->filename,image->scene);
              if (image->next != (Image *) NULL)
                (void) strcpy(image->next->magick,image->magick);
              (void) strcpy(image->filename,filename);
            }
#if defined(macintosh)
        if (*type == 'w')
          {
            OSType
              filetype;

            Str255
              name;

            (void) strcpy((char *) name,filename);
            CtoPstr((char *) name);
            filetype='    ';
            (void) strncpy((char *) &filetype,image_info->magick,
              Min(Extent(image_info->magick),4));
            Create(name,0,'8BIM',filetype);
          }
#endif
        image->file=(FILE *) fopen(filename,type);
        if (image->file != (FILE *) NULL)
          {
            (void) fseek(image->file,0L,2);
            image->filesize=ftell(image->file);
            (void) fseek(image->file,0L,0);
          }
      }
  image->status=False;
  if (*type == 'r')
    {
      image->next=(Image *) NULL;
      image->previous=(Image *) NULL;
    }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   P a r s e I m a g e G e o m e t r y                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ParseImageGeometry parse a geometry specification and returns the
%  width and height values.
%
%  The format of the ParseImageGeometry routine is:
%
%      flags=ParseImageGeometry(image_geometry,x,y,width,height)
%
%  A description of each parameter follows:
%
%    o flags:  Function ParseImageGeometry returns a bitmask that indicates
%      which of the four values (width, height, xoffset, and yoffset) were
%      actually found in the string, and whether the x and y values are
%      negative.
%
%    o image_geometry:  Specifies a character string representing the geometry
%      specification.
%
%    o x,y:  A pointer to an integer.  The x and y offset as determined by
%      the geometry specification is returned here.
%
%    o width,height:  A pointer to an unsigned integer.  The width and height
%      as determined by the geometry specification is returned here.
%
%
*/
Export int ParseImageGeometry(char *image_geometry,int *x, int *y,
  unsigned int *width,unsigned int *height)
{
  char
    geometry[MaxTextExtent];

  int
    flags;

  register char
    *p;

  unsigned int
    aspect_ratio,
    former_height,
    former_width,
    greater,
    less,
    percentage;

  /*
    Ensure the image geometry is valid.
  */
  assert(x != (int *) NULL);
  assert(y != (int *) NULL);
  assert(width != (unsigned int *) NULL);
  assert(height != (unsigned int *) NULL);
  if (image_geometry == (char *) NULL)
    return(NoValue);
  /*
    Remove whitespaces and % and ! characters from geometry specification.
  */
  (void) strcpy(geometry,image_geometry);
  aspect_ratio=True;
  greater=False;
  less=False;
  percentage=False;
  p=geometry;
  while (Extent(p) > 0)
  {
    if (isspace(*p))
      (void) strcpy(p,p+1);
    else
      switch (*p)
      {
        case '%':
        {
          percentage=True;
          (void) strcpy(p,p+1);
          break;
        }
        case '!':
        {
          aspect_ratio=False;
          (void) strcpy(p,p+1);
          break;
        }
        case '<':
        {
          less=True;
          (void) strcpy(p,p+1);
          break;
        }
        case '>':
        {
          greater=True;
          (void) strcpy(p,p+1);
          break;
        }
        case '~':
        {
          aspect_ratio=False;
          greater=False;
          less=False;
          percentage=False;
          (void) strcpy(p,p+1);
          break;
        }
        default:
          p++;
      }
  }
  /*
    Parse geometry using XParseGeometry.
  */
  former_width=(*width);
  former_height=(*height);
  flags=XParseGeometry(geometry,x,y,width,height);
  if (((flags & WidthValue) != 0) && (flags & HeightValue) == 0)
    *height=(*width);
  if (percentage)
    {
      int
        count;

      float
        x_scale,
        y_scale;

      /*
        Geometry is a percentage of the image size.
      */
      x_scale=(*width);
      y_scale=(*height);
      count=sscanf(geometry,"%fx%f",&x_scale,&y_scale);
      if (count == 1)
        y_scale=x_scale;
      *width=Max((unsigned int) ((x_scale*former_width)/100.0),1);
      *height=Max((unsigned int) ((y_scale*former_height)/100.0),1);
      former_width=(*width);
      former_height=(*height);
    }
  if (aspect_ratio)
    {
      unsigned long
        scale_factor;

      /*
        Respect aspect ratio of the image.
      */
      scale_factor=UpShift(1);
      if ((former_width*former_height) != 0)
        if (((flags & WidthValue) != 0) && (flags & HeightValue) != 0)
          {
            scale_factor=UpShift(*width)/former_width;
            if (scale_factor > (UpShift(*height)/former_height))
              scale_factor=UpShift(*height)/former_height;
          }
        else
          if ((flags & WidthValue) != 0)
            scale_factor=UpShift(*width)/former_width;
          else
            scale_factor=UpShift(*height)/former_height;
      *width=Max(DownShift(former_width*scale_factor),1);
      *height=Max(DownShift(former_height*scale_factor),1);
    }
  if (greater)
    if ((former_width < *width) && (former_height < *height))
      {
        *width=former_width;
        *height=former_height;
      }
  if (less)
    if ((former_width > *width) && (former_height > *height))
      {
        *width=former_width;
        *height=former_height;
      }
  return(flags);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     R G B T r a n s f o r m I m a g e                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function RGBTransformImage converts the reference image from RGB to
%  an alternate colorspace.  The transformation matrices are not the standard
%  ones: the weights are rescaled to normalized the range of the transformed
%  values to be [0..MaxRGB].
%
%  The format of the RGBTransformImage routine is:
%
%      RGBTransformImage(image,colorspace)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o colorspace: An unsigned integer value that indicates which colorspace
%      to transform the image.
%
%
*/
void RGBTransformImage(Image *image,const unsigned int colorspace)
{
#define RGBTransformImageText  "  Transforming image colors...  "
#define X 0
#define Y (MaxRGB+1)
#define Z (MaxRGB+1)*2

  long
    tx,
    ty,
    tz,
    *x,
    *y,
    *z;

  Quantum
    *range_table;

  register int
    blue,
    green,
    i,
    red;

  register Quantum
    *range_limit;

  register RunlengthPacket
    *p;

  assert(image != (Image *) NULL);
  if ((colorspace == RGBColorspace) || (colorspace == TransparentColorspace))
    return;
  if (colorspace == GRAYColorspace)
    {
      /*
        Return if the image is already gray_scale.
      */
      p=image->pixels;
      for (i=0; i < image->packets; i++)
      {
        if ((p->red != p->green) || (p->green != p->blue))
          break;
        p++;
      }
      if (i == image->packets)
        return;
    }
  /*
    Allocate the tables.
  */
  x=(long *) malloc(3*(MaxRGB+1)*sizeof(long));
  y=(long *) malloc(3*(MaxRGB+1)*sizeof(long));
  z=(long *) malloc(3*(MaxRGB+1)*sizeof(long));
  range_table=(Quantum *) malloc(4*(MaxRGB+1)*sizeof(Quantum));
  if ((x == (long *) NULL) || (y == (long *) NULL) ||
      (z == (long *) NULL) || (range_table == (Quantum *) NULL))
    {
      Warning("Unable to transform color space","Memory allocation failed");
      return;
    }
  /*
    Pre-compute conversion tables.
  */
  for (i=0; i <= MaxRGB; i++)
  {
    range_table[i]=0;
    range_table[i+(MaxRGB+1)]=(Quantum) i;
    range_table[i+(MaxRGB+1)*2]=MaxRGB;
  }
  for (i=0; i <= MaxRGB; i++)
    range_table[i+(MaxRGB+1)*3]=MaxRGB;
  range_limit=range_table+(MaxRGB+1);
  tx=0;
  ty=0;
  tz=0;
  switch (colorspace)
  {
    case GRAYColorspace:
    {
      /*
        Initialize GRAY tables:

          G = 0.29900*R+0.58600*G+0.11400*B
      */
      for (i=0; i <= MaxRGB; i++)
      {
        x[i+X]=UpShifted(0.29900)*i;
        y[i+X]=UpShifted(0.58600)*i;
        z[i+X]=UpShifted(0.11400)*i;
        x[i+Y]=UpShifted(0.29900)*i;
        y[i+Y]=UpShifted(0.58600)*i;
        z[i+Y]=UpShifted(0.11400)*i;
        x[i+Z]=UpShifted(0.29900)*i;
        y[i+Z]=UpShifted(0.58600)*i;
        z[i+Z]=UpShifted(0.11400)*i;
      }
      break;
    }
    case OHTAColorspace:
    {
      /*
        Initialize OHTA tables:

          I1 = 0.33333*R+0.33334*G+0.33333*B
          I2 = 0.50000*R+0.00000*G-0.50000*B
          I3 =-0.25000*R+0.50000*G-0.25000*B

        I and Q, normally -0.5 through 0.5, are normalized to the range 0
        through MaxRGB.
      */
      ty=UpShifted((MaxRGB+1) >> 1);
      tz=UpShifted((MaxRGB+1) >> 1);
      for (i=0; i <= MaxRGB; i++)
      {
        x[i+X]=UpShifted(0.33333)*i;
        y[i+X]=UpShifted(0.33334)*i;
        z[i+X]=UpShifted(0.33333)*i;
        x[i+Y]=UpShifted(0.50000)*i;
        y[i+Y]=0;
        z[i+Y]=(-UpShifted(0.50000))*i;
        x[i+Z]=(-UpShifted(0.25000))*i;
        y[i+Z]=UpShifted(0.50000)*i;
        z[i+Z]=(-UpShifted(0.25000))*i;
      }
      break;
    }
    case XYZColorspace:
    {
      /*
        Initialize CIE XYZ tables:

          X = 0.412453*X+0.357580*Y+0.180423*Z
          Y = 0.212671*X+0.715160*Y+0.072169*Z
          Z = 0.019334*X+0.119193*Y+0.950227*Z
      */
      for (i=0; i <= MaxRGB; i++)
      {
        x[i+X]=UpShifted(0.412453)*i;
        y[i+X]=UpShifted(0.357580)*i;
        z[i+X]=UpShifted(0.180423)*i;
        x[i+Y]=UpShifted(0.212671)*i;
        y[i+Y]=UpShifted(0.715160)*i;
        z[i+Y]=UpShifted(0.072169)*i;
        x[i+Z]=UpShifted(0.019334)*i;
        y[i+Z]=UpShifted(0.119193)*i;
        z[i+Z]=UpShifted(0.950227)*i;
      }
      break;
    }
    case YCbCrColorspace:
    {
      /*
        Initialize YCbCr tables:

          Y =  0.299000*R+0.586000*G+0.114000*B
          Cb= -0.172586*R-0.338828*G+0.511414*B
          Cr=  0.511414*R-0.428246*G-0.083168*B

        Cb and Cr, normally -0.5 through 0.5, are normalized to the range 0
        through MaxRGB.
      */
      ty=UpShifted((MaxRGB+1) >> 1);
      tz=UpShifted((MaxRGB+1) >> 1);
      for (i=0; i <= MaxRGB; i++)
      {
        x[i+X]=UpShifted(0.299000)*i;
        y[i+X]=UpShifted(0.586000)*i;
        z[i+X]=UpShifted(0.114000)*i;
        x[i+Y]=(-UpShifted(0.172586))*i;
        y[i+Y]=(-UpShifted(0.338828))*i;
        z[i+Y]=UpShifted(0.511414)*i;
        x[i+Z]=UpShifted(0.511414)*i;
        y[i+Z]=(-UpShifted(0.428246))*i;
        z[i+Z]=(-UpShifted(0.083168))*i;
      }
      break;
    }
    case YCCColorspace:
    {
      /*
        Initialize YCC tables:

          Y =  0.29900*R+0.58600*G+0.11400*B
          C1= -0.29900*R-0.58600*G+0.88600*B
          C2=  0.70100*R-0.58600*G-0.11400*B

        YCC is scaled by 1.3584.  C1 zero is 156 and C2 is at 137.
      */
      ty=UpShifted((unsigned int) UpScale(156));
      tz=UpShifted((unsigned int) UpScale(137));
      for (i=0; i <= (int) (0.018*MaxRGB); i++)
      {
        x[i+X]=(long) (UpShifted(0.29900/1.3584)*0.018*MaxRGB*i);
        y[i+X]=(long) (UpShifted(0.58600/1.3584)*0.018*MaxRGB*i);
        z[i+X]=(long) (UpShifted(0.11400/1.3584)*0.018*MaxRGB*i);
        x[i+Y]=(long) ((-UpShifted(0.29900/2.2179))*0.018*MaxRGB*i);
        y[i+Y]=(long) ((-UpShifted(0.58600/2.2179))*0.018*MaxRGB*i);
        z[i+Y]=(long) (UpShifted(0.88600/2.2179)*0.018*MaxRGB*i);
        x[i+Z]=(long) (UpShifted(0.70100/1.8215)*0.018*MaxRGB*i);
        y[i+Z]=(long) ((-UpShifted(0.58600/1.8215))*0.018*MaxRGB*i);
        z[i+Z]=(long) ((-UpShifted(0.11400/1.8215))*0.018*MaxRGB*i);
      }
      for ( ; i <= MaxRGB; i++)
      {
        x[i+X]=(long) (UpShifted(0.29900/1.3584)*(1.099*i-0.099));
        y[i+X]=(long) (UpShifted(0.58600/1.3584)*(1.099*i-0.099));
        z[i+X]=(long) (UpShifted(0.11400/1.3584)*(1.099*i-0.099));
        x[i+Y]=(long) ((-UpShifted(0.29900/2.2179))*(1.099*i-0.099));
        y[i+Y]=(long) ((-UpShifted(0.58600/2.2179))*(1.099*i-0.099));
        z[i+Y]=(long) (UpShifted(0.88600/2.2179)*(1.099*i-0.099));
        x[i+Z]=(long) (UpShifted(0.70100/1.8215)*(1.099*i-0.099));
        y[i+Z]=(long) ((-UpShifted(0.58600/1.8215))*(1.099*i-0.099));
        z[i+Z]=(long) ((-UpShifted(0.11400/1.8215))*(1.099*i-0.099));
      }
      break;
    }
    case YIQColorspace:
    {
      /*
        Initialize YIQ tables:

          Y = 0.29900*R+0.58600*G+0.11400*B
          I = 0.50000*R-0.23000*G-0.27000*B
          Q = 0.20200*R-0.50000*G+0.29800*B

        I and Q, normally -0.5 through 0.5, are normalized to the range 0
        through MaxRGB.
      */
      ty=UpShifted((MaxRGB+1) >> 1);
      tz=UpShifted((MaxRGB+1) >> 1);
      for (i=0; i <= MaxRGB; i++)
      {
        x[i+X]=UpShifted(0.29900)*i;
        y[i+X]=UpShifted(0.58600)*i;
        z[i+X]=UpShifted(0.11400)*i;
        x[i+Y]=UpShifted(0.50000)*i;
        y[i+Y]=(-UpShifted(0.23000))*i;
        z[i+Y]=(-UpShifted(0.27000))*i;
        x[i+Z]=UpShifted(0.20200)*i;
        y[i+Z]=(-UpShifted(0.50000))*i;
        z[i+Z]=UpShifted(0.29800)*i;
      }
      break;
    }
    case YPbPrColorspace:
    {
      /*
        Initialize YPbPr tables:

          Y =  0.299000*R+0.587000*G+0.114000*B
          Pb= -0.168736*R-0.331264*G+0.500000*B
          Pr=  0.500000*R-0.418688*G-0.081312*B

        Pb and Pr, normally -0.5 through 0.5, are normalized to the range 0
        through MaxRGB.
      */
      ty=UpShifted((MaxRGB+1) >> 1);
      tz=UpShifted((MaxRGB+1) >> 1);
      for (i=0; i <= MaxRGB; i++)
      {
        x[i+X]=UpShifted(0.299000)*i;
        y[i+X]=UpShifted(0.587000)*i;
        z[i+X]=UpShifted(0.114000)*i;
        x[i+Y]=(-UpShifted(0.168736))*i;
        y[i+Y]=(-UpShifted(0.331264))*i;
        z[i+Y]=UpShifted(0.500000)*i;
        x[i+Z]=UpShifted(0.500000)*i;
        y[i+Z]=(-UpShifted(0.418688))*i;
        z[i+Z]=(-UpShifted(0.081312))*i;
      }
      break;
    }
    case YUVColorspace:
    default:
    {
      /*
        Initialize YUV tables:

          Y =  0.29900*R+0.58600*G+0.11400*B
          U = -0.14740*R-0.28950*G+0.43690*B
          V =  0.61500*R-0.51500*G-0.10000*B

        U and V, normally -0.5 through 0.5, are normalized to the range 0
        through MaxRGB.  Note that U = 0.493*(B-Y), V = 0.877*(R-Y).
      */
      ty=UpShifted((MaxRGB+1) >> 1);
      tz=UpShifted((MaxRGB+1) >> 1);
      for (i=0; i <= MaxRGB; i++)
      {
        x[i+X]=UpShifted(0.29900)*i;
        y[i+X]=UpShifted(0.58600)*i;
        z[i+X]=UpShifted(0.11400)*i;
        x[i+Y]=(-UpShifted(0.14740))*i;
        y[i+Y]=(-UpShifted(0.28950))*i;
        z[i+Y]=UpShifted(0.43690)*i;
        x[i+Z]=UpShifted(0.61500)*i;
        y[i+Z]=(-UpShifted(0.51500))*i;
        z[i+Z]=(-UpShifted(0.10000))*i;
      }
      break;
    }
  }
  /*
    Convert from RGB.
  */
  switch (image->class)
  {
    case DirectClass:
    default:
    {
      /*
        Convert DirectClass image.
      */
      p=image->pixels;
      for (i=0; i < image->packets; i++)
      {
        red=p->red;
        green=p->green;
        blue=p->blue;
        p->red=range_limit[DownShift(x[red+X]+y[green+X]+z[blue+X]+tx)];
        p->green=range_limit[DownShift(x[red+Y]+y[green+Y]+z[blue+Y]+ty)];
        p->blue=range_limit[DownShift(x[red+Z]+y[green+Z]+z[blue+Z]+tz)];
        p++;
        if (QuantumTick(i,image))
          ProgressMonitor(RGBTransformImageText,i,image->packets);
      }
      break;
    }
    case PseudoClass:
    {
      /*
        Convert PseudoClass image.
      */
      for (i=0; i < image->colors; i++)
      {
        red=image->colormap[i].red;
        green=image->colormap[i].green;
        blue=image->colormap[i].blue;
        image->colormap[i].red=
          range_limit[DownShift(x[red+X]+y[green+X]+z[blue+X]+tx)];
        image->colormap[i].green=
          range_limit[DownShift(x[red+Y]+y[green+Y]+z[blue+Y]+ty)];
        image->colormap[i].blue=
          range_limit[DownShift(x[red+Z]+y[green+Z]+z[blue+Z]+tz)];
      }
      SyncImage(image);
      break;
    }
  }
  /*
    Free allocated memory.
  */
  free((char *) range_table);
  free((char *) z);
  free((char *) y);
  free((char *) x);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R o l l I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function RollImage rolls an image vertically and horizontally.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the RollImage routine is:
%
%      rolled_image=RollImage(image,x_offset,y_offset)
%
%  A description of each parameter follows:
%
%    o rolled_image: Function RollImage returns a pointer to the image after
%      rolling.  A null image is returned if there is a memory shortage.
%
%    o image: The address of a structure of type Image.
%
%    o x_offset: An integer that specifies the number of columns to roll
%      in the horizontal direction.
%
%    o y_offset: An integer that specifies the number of rows to roll in the
%      vertical direction.
%
%
*/
Image *RollImage(Image *image,int x_offset,int y_offset)
{
#define RollImageText  "  Rolling image...  "

  Image
    *rolled_image;

  int
    y;

  register int
    x;

  register RunlengthPacket
    *p,
    *q;

  /*
    Initialize rolled image attributes.
  */
  assert(image != (Image *) NULL);
  rolled_image=CopyImage(image,image->columns,image->rows,False);
  if (rolled_image == (Image *) NULL)
    {
      Warning("Unable to roll image","Memory allocation failed");
      return((Image *) NULL);
    }
  /*
    Roll image.
  */
  x_offset%=(int) image->columns;
  if (x_offset < 0)
    x_offset+=(int) image->columns;
  y_offset%=(int) image->rows;
  if (y_offset < 0)
    y_offset+=(int) image->rows;
  p=image->pixels;
  image->runlength=p->length+1;
  for (y=0; y < image->rows; y++)
  {
    /*
      Transfer scanline.
    */
    for (x=0; x < image->columns; x++)
    {
      if (image->runlength != 0)
        image->runlength--;
      else
        {
          p++;
          image->runlength=p->length;
        }
      q=rolled_image->pixels+((y_offset+y) % image->rows)*image->columns+
        ((x+x_offset) % image->columns);
      *q=(*p);
      q->length=0;
    }
    ProgressMonitor(RollImageText,y,image->rows);
  }
  return(rolled_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   S a m p l e I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function SampleImage creates a new image that is a scaled size of an
%  existing one using pixel sampling.  It allocates the memory necessary
%  for the new Image structure and returns a pointer to the new image.
%
%  The format of the SampleImage routine is:
%
%      sampled_image=SampleImage(image,columns,rows)
%
%  A description of each parameter follows:
%
%    o sampled_image: Function SampleImage returns a pointer to the image after
%      scaling.  A null image is returned if there is a memory shortage.
%
%    o image: The address of a structure of type Image.
%
%    o columns: An integer that specifies the number of columns in the sampled
%      image.
%
%    o rows: An integer that specifies the number of rows in the sampled
%      image.
%
%
*/
Image *SampleImage(Image *image,unsigned int columns,unsigned int rows)
{
#define SampleImageText  "  Sampling image...  "

  Image
    *sampled_image;

  int
    y;

  register RunlengthPacket
    *p,
    *q,
    *s;

  register int
    x;

  RunlengthPacket
    *scanline;

  unsigned int
    *x_offset,
    *y_offset;

  unsigned long
    scale_factor;

  assert(image != (Image *) NULL);
  if ((columns == 0) || (rows == 0))
    {
      Warning("Unable to sample image","image dimensions are zero");
      return((Image *) NULL);
    }
  /*
    Initialize sampled image attributes.
  */
  sampled_image=CopyImage(image,columns,rows,False);
  if (sampled_image == (Image *) NULL)
    {
      Warning("Unable to sample image","Memory allocation failed");
      return((Image *) NULL);
    }
  /*
    Allocate scan line buffer and column offset buffers.
  */
  scanline=(RunlengthPacket *) malloc(image->columns*sizeof(RunlengthPacket));
  x_offset=(unsigned int *) malloc(sampled_image->columns*sizeof(unsigned int));
  y_offset=(unsigned int *) malloc(sampled_image->rows*sizeof(unsigned int));
  if ((scanline == (RunlengthPacket *) NULL) ||
      (x_offset == (unsigned int *) NULL) ||
      (y_offset == (unsigned int *) NULL))
    {
      Warning("Unable to sample image","Memory allocation failed");
      DestroyImage(sampled_image);
      return((Image *) NULL);
    }
  /*
    Initialize column pixel offsets.
  */
  scale_factor=UpShift(image->columns-1)/sampled_image->columns;
  columns=0;
  for (x=0; x < sampled_image->columns; x++)
  {
    x_offset[x]=DownShift((x+1)*scale_factor)-(int) columns;
    columns+=x_offset[x];
  }
  /*
    Initialize row pixel offsets.
  */
  scale_factor=UpShift(image->rows-1)/sampled_image->rows;
  rows=0;
  for (y=0; y < sampled_image->rows; y++)
  {
    y_offset[y]=DownShift((y+1)*scale_factor)-(int) rows;
    rows+=y_offset[y];
  }
  y_offset[sampled_image->rows-1]=0;
  /*
    Preload first scanline.
  */
  p=image->pixels;
  image->runlength=p->length+1;
  s=scanline;
  for (x=0; x < image->columns; x++)
  {
    if (image->runlength != 0)
      image->runlength--;
    else
      {
        p++;
        image->runlength=p->length;
      }
    *s=(*p);
    s->length=0;
    s++;
  }
  /*
    Sample each row.
  */
  q=sampled_image->pixels;
  for (y=0; y < sampled_image->rows; y++)
  {
    /*
      Sample each column.
    */
    s=scanline;
    for (x=0; x < sampled_image->columns; x++)
    {
      *q=(*s);
      q++;
      s+=x_offset[x];
    }
    if (y_offset[y] != 0)
      {
        /*
          Skip a scan line.
        */
        if (y_offset[y] > 1)
          for (x=0; x < (image->columns*(y_offset[y]-1)); x++)
            if (image->runlength != 0)
              image->runlength--;
            else
              {
                p++;
                image->runlength=p->length;
              }
        /*
          Read a scan line.
        */
        s=scanline;
        for (x=0; x < image->columns; x++)
        {
          if (image->runlength != 0)
            image->runlength--;
          else
            {
              p++;
              image->runlength=p->length;
            }
          *s=(*p);
          s->length=0;
          s++;
        }
      }
    ProgressMonitor(SampleImageText,y,sampled_image->rows);
  }
  free((char *) scanline);
  free((char *) x_offset);
  free((char *) y_offset);
  return(sampled_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   S c a l e I m a g e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ScaleImage creates a new image that is a scaled size of an
%  existing one.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.  To scale a scanline
%  from x pixels to y pixels, each new pixel represents x/y old pixels.  To
%  read x/y pixels, read (x/y rounded up) pixels but only count the required
%  fraction of the last old pixel read in your new pixel.  The remainder
%  of the old pixel will be counted in the next new pixel.
%
%  The scaling algorithm was suggested by rjohnson@shell.com and is adapted
%  from pnmscale(1) of PBMPLUS by Jef Poskanzer.
%
%  The format of the ScaleImage routine is:
%
%      scaled_image=ScaleImage(image,columns,rows)
%
%  A description of each parameter follows:
%
%    o scaled_image: Function ScaleImage returns a pointer to the image after
%      scaling.  A null image is returned if there is a memory shortage.
%
%    o image: The address of a structure of type Image.
%
%    o columns: An integer that specifies the number of columns in the scaled
%      image.
%
%    o rows: An integer that specifies the number of rows in the scaled
%      image.
%
%
*/
Image *ScaleImage(Image *image,const unsigned int columns,
  const unsigned int rows)
{
#define ScaleImageText  "  Scaling image...  "

  typedef struct ScaledPacket
  {
    long
      red,
      green,
      blue,
      index;
  } ScaledPacket;

  Image
    *scaled_image;

  int
    next_row,
    number_rows;

  long
    x_scale,
    x_span;

  register RunlengthPacket
    *p,
    *q;

  register ScaledPacket
    *s,
    *t;

  register unsigned int
    x;

  ScaledPacket
    *scaled_scanline,
    *scanline,
    *y_vector,
    *x_vector;

  unsigned int
    packets,
    y;

  unsigned long
    blue,
    green,
    index,
    red,
    scale_factor;

  assert(image != (Image *) NULL);
  if ((columns == 0) || (rows == 0))
    {
      Warning("Unable to scale image","image dimensions are zero");
      return((Image *) NULL);
    }
  /*
    Initialize scaled image attributes.
  */
  scale_factor=UpShift(columns*rows)/(image->columns*image->rows);
  packets=Max(DownShift(image->packets*scale_factor),1);
  scaled_image=CopyImage(image,packets,1,False);
  if (scaled_image == (Image *) NULL)
    {
      Warning("Unable to scale image","Memory allocation failed");
      return((Image *) NULL);
    }
  scaled_image->class=DirectClass;
  scaled_image->columns=columns;
  scaled_image->rows=rows;
  scaled_image->packets=0;
  /*
    Allocate memory.
  */
  x_vector=(ScaledPacket *) malloc(image->columns*sizeof(ScaledPacket));
  scanline=x_vector;
  if (scaled_image->rows != image->rows)
    scanline=(ScaledPacket *) malloc(image->columns*sizeof(ScaledPacket));
  scaled_scanline=(ScaledPacket *)
    malloc(scaled_image->columns*sizeof(ScaledPacket));
  y_vector=(ScaledPacket *) malloc(image->columns*sizeof(ScaledPacket));
  if ((x_vector == (ScaledPacket *) NULL) ||
      (scanline == (ScaledPacket *) NULL) ||
      (scaled_scanline == (ScaledPacket *) NULL) ||
      (y_vector == (ScaledPacket *) NULL))
    {
      Warning("Unable to scale image","Memory allocation failed");
      DestroyImage(scaled_image);
      return((Image *) NULL);
    }
  /*
    Scale image.
  */
  index=0;
  number_rows=0;
  next_row=True;
  x_scale=UpShift(scaled_image->rows)/image->rows;
  x_span=UpShift(1);
  for (x=0; x < image->columns; x++)
  {
    y_vector[x].red=0;
    y_vector[x].green=0;
    y_vector[x].blue=0;
    y_vector[x].index=0;
  }
  p=image->pixels;
  image->runlength=p->length+1;
  q=scaled_image->pixels;
  q->red=0;
  q->green=0;
  q->blue=0;
  q->index=0;
  q->length=MaxRunlength;
  for (y=0; y < scaled_image->rows; y++)
  {
    if (scaled_image->rows == image->rows)
      for (x=0; x < image->columns; x++)
      {
        /*
          Read a new scanline.
        */
        if (image->runlength != 0)
          image->runlength--;
        else
          {
            p++;
            image->runlength=p->length;
          }
        x_vector[x].red=p->red;
        x_vector[x].green=p->green;
        x_vector[x].blue=p->blue;
        x_vector[x].index=p->index;
      }
    else
      {
        /*
          Scale Y direction.
        */
        while (x_scale < x_span)
        {
          if (next_row && (number_rows < image->rows))
            {
              /*
                Read a new scanline.
              */
              for (x=0; x < image->columns; x++)
              {
                if (image->runlength != 0)
                  image->runlength--;
                else
                  {
                    p++;
                    image->runlength=p->length;
                  }
                x_vector[x].red=p->red;
                x_vector[x].green=p->green;
                x_vector[x].blue=p->blue;
                x_vector[x].index=p->index;
              }
              number_rows++;
            }
          for (x=0; x < image->columns; x++)
          {
            y_vector[x].red+=x_scale*x_vector[x].red;
            y_vector[x].green+=x_scale*x_vector[x].green;
            y_vector[x].blue+=x_scale*x_vector[x].blue;
            y_vector[x].index+=x_scale*x_vector[x].index;
          }
          x_span-=x_scale;
          x_scale=UpShift(scaled_image->rows)/image->rows;
          next_row=True;
        }
        if (next_row && (number_rows < image->rows))
          {
            /*
              Read a new scanline.
            */
            for (x=0; x < image->columns; x++)
            {
              if (image->runlength != 0)
                image->runlength--;
              else
                {
                  p++;
                  image->runlength=p->length;
                }
              x_vector[x].red=p->red;
              x_vector[x].green=p->green;
              x_vector[x].blue=p->blue;
              x_vector[x].index=p->index;
            }
            number_rows++;
            next_row=False;
          }
        s=scanline;
        for (x=0; x < image->columns; x++)
        {
          red=DownShift(y_vector[x].red+x_span*x_vector[x].red);
          green=DownShift(y_vector[x].green+x_span*x_vector[x].green);
          blue=DownShift(y_vector[x].blue+x_span*x_vector[x].blue);
          index=DownShift(y_vector[x].index+x_span*x_vector[x].index);
          s->red=(Quantum) (red > MaxRGB ? MaxRGB : red);
          s->green=(Quantum) (green > MaxRGB ? MaxRGB : green);
          s->blue=(Quantum) (blue > MaxRGB ? MaxRGB : blue);
          s->index=(unsigned short)
            (index > MaxColormapSize ? MaxColormapSize : index);
          s++;
          y_vector[x].red=0;
          y_vector[x].green=0;
          y_vector[x].blue=0;
          y_vector[x].index=0;
        }
        x_scale-=x_span;
        if (x_scale == 0)
          {
            x_scale=UpShift(scaled_image->rows)/image->rows;
            next_row=True;
          }
        x_span=UpShift(1);
      }
    if (scaled_image->columns == image->columns)
      {
        /*
          Transfer scanline to scaled image.
        */
        s=scanline;
        for (x=0; x < scaled_image->columns; x++)
        {
          if ((s->red == q->red) && (s->green == q->green) &&
              (s->blue == q->blue) && (s->index == q->index) &&
              ((int) q->length < MaxRunlength))
            q->length++;
          else
            {
              if (scaled_image->packets != 0)
                q++;
              scaled_image->packets++;
              if (scaled_image->packets == packets)
                {
                  packets<<=1;
                  scaled_image->pixels=(RunlengthPacket *) realloc((char *)
                    scaled_image->pixels,packets*sizeof(RunlengthPacket));
                  if (scaled_image->pixels == (RunlengthPacket *) NULL)
                    {
                      Warning("Unable to scale image",
                        "Memory allocation failed");
                      DestroyImage(scaled_image);
                      return((Image *) NULL);
                    }
                  q=scaled_image->pixels+scaled_image->packets-1;
                }
              q->red=(Quantum) s->red;
              q->green=(Quantum) s->green;
              q->blue=(Quantum) s->blue;
              q->index=(unsigned short) s->index;
              q->length=0;
            }
          s++;
        }
      }
    else
      {
        int
          next_column;

        long
          y_scale,
          y_span;

        /*
          Scale X direction.
        */
        red=0;
        green=0;
        blue=0;
        next_column=False;
        y_span=UpShift(1);
        s=scanline;
        t=scaled_scanline;
        for (x=0; x < image->columns; x++)
        {
          y_scale=UpShift(scaled_image->columns)/image->columns;
          while (y_scale >= y_span)
          {
            if (next_column)
              {
                red=0;
                green=0;
                blue=0;
                index=0;
                t++;
              }
            red=DownShift(red+y_span*s->red);
            green=DownShift(green+y_span*s->green);
            blue=DownShift(blue+y_span*s->blue);
            index=DownShift(index+y_span*s->index);
            t->red=(Quantum) (red > MaxRGB ? MaxRGB : red);
            t->green=(Quantum) (green > MaxRGB ? MaxRGB : green);
            t->blue=(Quantum) (blue > MaxRGB ? MaxRGB : blue);
            t->index=(unsigned short)
              (index > MaxColormapSize ? MaxColormapSize : index);
            y_scale-=y_span;
            y_span=UpShift(1);
            next_column=True;
          }
        if (y_scale > 0)
          {
            if (next_column)
              {
                red=0;
                green=0;
                blue=0;
                index=0;
                next_column=False;
                t++;
              }
            red+=y_scale*s->red;
            green+=y_scale*s->green;
            blue+=y_scale*s->blue;
            index+=y_scale*s->index;
            y_span-=y_scale;
          }
        s++;
      }
      if (y_span > 0)
        {
          s--;
          red+=y_span*s->red;
          green+=y_span*s->green;
          blue+=y_span*s->blue;
          index+=y_span*s->index;
        }
      if (!next_column)
        {
          red=DownShift(red);
          green=DownShift(green);
          blue=DownShift(blue);
          index=DownShift(index);
          t->red=(Quantum) (red > MaxRGB ? MaxRGB : red);
          t->green=(Quantum) (green > MaxRGB ? MaxRGB : green);
          t->blue=(Quantum) (blue > MaxRGB ? MaxRGB : blue);
          t->index=(unsigned short) (index > MaxRGB ? MaxRGB : index);
        }
      /*
        Transfer scanline to scaled image.
      */
      t=scaled_scanline;
      for (x=0; x < scaled_image->columns; x++)
      {
        if ((t->red == q->red) && (t->green == q->green) &&
            (t->blue == q->blue) && (t->index == q->index) &&
            ((int) q->length < MaxRunlength))
          q->length++;
        else
          {
            if (scaled_image->packets != 0)
              q++;
            scaled_image->packets++;
            if (scaled_image->packets == packets)
              {
                packets<<=1;
                scaled_image->pixels=(RunlengthPacket *) realloc((char *)
                  scaled_image->pixels,packets*sizeof(RunlengthPacket));
                if (scaled_image->pixels == (RunlengthPacket *) NULL)
                  {
                    Warning("Unable to scale image","Memory allocation failed");
                    DestroyImage(scaled_image);
                    return((Image *) NULL);
                  }
                q=scaled_image->pixels+scaled_image->packets-1;
              }
            q->red=(Quantum) t->red;
            q->green=(Quantum) t->green;
            q->blue=(Quantum) t->blue;
            q->index=(unsigned short) t->index;
            q->length=0;
          }
        t++;
      }
    }
    ProgressMonitor(ScaleImageText,y,scaled_image->rows);
  }
  scaled_image->pixels=(RunlengthPacket *) realloc((char *)
    scaled_image->pixels,scaled_image->packets*sizeof(RunlengthPacket));
  /*
    Free allocated memory.
  */
  free((char *) y_vector);
  free((char *) scaled_scanline);
  if (scanline != x_vector)
    free((char *) scanline);
  free((char *) x_vector);
  return(scaled_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   S e t I m a g e I n f o                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function SetImageInfo initializes the `magick' field of the ImageInfo
%  structure.  It is set to a type of image format based on the prefix or
%  suffix of the filename.  For example, `ps:image' returns PS indicating
%  a Postscript image.  JPEG is returned for this filename: `image.jpg'.
%  The filename prefix has precedance over the suffix.  Use an optional index
%  enclosed in brackets after a file name to specify a desired subimage of a
%  multi-resolution image format like Photo CD (e.g. img0001.pcd[4]).
%
%  The format of the SetImageInfo routine is:
%
%      SetImageInfo(image_info,recify)
%
%  A description of each parameter follows:
%
%    o image_info: Specifies a pointer to a ImageInfo structure.
%
%    o rectify: an unsigned value other than zero rectifies the attribute for
%      multi-frame support (user may want multi-frame but image format may not
%      support it).
%
%
*/
Export void SetImageInfo(ImageInfo *image_info,unsigned int rectify)
{
  char
    c,
    magick[MaxTextExtent];

  register char
    *p,
    *q;

  register int
    i;

  /*
    Look for 'image.format' in filename.
  */
  assert(image_info != (ImageInfo *) NULL);
  *magick='\0';
  p=image_info->filename+Extent(image_info->filename)-1;
  if (*p == ']')
    for (q=p-1; q > image_info->filename; q--)
    {
      if (*q != '[')
        continue;
      image_info->tile=(char *) malloc((p-q)*sizeof(char));
      if (image_info->tile == (char *) NULL)
        break;
      (void) strncpy(image_info->tile,q+1,p-q-1);
      image_info->tile[p-q-1]='\0';
      if (!IsGeometry(image_info->tile))
        {
          free(image_info->tile);
          image_info->tile=(char *) NULL;
          break;
        }
      if (strchr(image_info->tile,',') == (char *) NULL)
        {
          /*
            Sub-image specified (e.g. img0001.pcd[4]).
          */
          image_info->subimage=atoi(image_info->tile);
          image_info->subrange=atoi(image_info->tile);
          (void) sscanf(image_info->tile,"%u-%u",&image_info->subimage,
            &image_info->subrange);
          image_info->subrange-=image_info->subimage-1;
        }
      *q='\0';
      p=q;
      break;
    }
  while ((*p != '.') && (p > image_info->filename))
    p--;
  if ((strcmp(p,".gz") == 0) || (strcmp(p,".Z") == 0))
    do
    {
      p--;
    } while ((*p != '.') && (p > image_info->filename));
  if ((*p == '.') && (Extent(p) < sizeof(magick)))
    {
      /*
        User specified image format.
      */
      (void) strcpy(magick,p+1);
      for (q=magick; *q != '\0'; q++)
      {
        if (*q == '.')
          {
            *q='\0';
            break;
          }
        c=(*q);
        if (islower(c))
          *q=toupper(c);
      }
      for (i=0; ImageFormats[i][0] != (char *) NULL; i++)
        if (strcmp(magick,ImageFormats[i][0]) == 0)
          {
            /*
              SGI and RGB are ambiguous;  TMP must be set explicitly.
            */
            if (((strncmp(image_info->magick,"SGI",3) != 0) ||
                 (strcmp(ImageFormats[i][0],"RGB") != 0)) &&
                (strcmp(ImageFormats[i][0],"TMP") != 0))
              (void) strcpy(image_info->magick,magick);
            break;
          }
    }
  /*
    Look for explicit 'format:image' in filename.
  */
  image_info->affirm=False;
  p=image_info->filename;
  while ((*p != ':') && (*p != '\0'))
    p++;
  if ((*p == ':') && ((p-image_info->filename) < sizeof(magick)))
    {
      /*
        User specified image format.
      */
      (void) strncpy(magick,image_info->filename,p-image_info->filename);
      magick[p-image_info->filename]='\0';
      for (q=magick; *q != '\0'; q++)
      {
        c=(*q);
        if (islower(c))
          *q=toupper(c);
      }
      for (i=0; ImageFormats[i][0] != (char *) NULL; i++)
        if (strcmp(magick,ImageFormats[i][0]) == 0)
          {
            /*
              Strip off image format prefix.
            */
            p++;
            (void) strcpy(image_info->filename,p);
            (void) strcpy(image_info->magick,magick);
            if (strcmp(magick,"TMP") != 0)
              image_info->affirm=True;
            break;
          }
    }
  /*
    Rectify multi-image file support.
  */
  if (rectify)
    for (i=0; ImageFormats[i][0] != (char *) NULL; i++)
      if (strcmp(image_info->magick,ImageFormats[i][0]) == 0)
        image_info->adjoin&=IsTrue(ImageFormats[i][1]);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   S o r t C o l o r m a p B y I n t e n t s i t y                           %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function SortColormapByIntensity sorts the colormap of a PseudoClass image
%  by decreasing color intensity.
%
%  The format of the SortColormapByIntensity routine is:
%
%      SortColormapByIntensity(image)
%
%  A description of each parameter follows:
%
%    o image: A pointer to a Image structure.
%
%
*/

static int IntensityCompare(const void *x,const void *y)
{
  ColorPacket
    *color_1,
    *color_2;

  color_1=(ColorPacket *) x;
  color_2=(ColorPacket *) y;
  return((int) Intensity(*color_2)-(int) Intensity(*color_1));
}

void SortColormapByIntensity(Image *image)
{
  register int
    i;

  register RunlengthPacket
    *p;

  register unsigned short
    index;

  unsigned short
    *pixels;

  assert(image != (Image *) NULL);
  if (image->class != PseudoClass)
    return;
  /*
    Allocate memory for pixel indexes.
  */
  pixels=(unsigned short *) malloc(image->colors*sizeof(unsigned short));
  if (pixels == (unsigned short *) NULL)
    {
      Warning("Unable to sort colormap","Memory allocation failed");
      return;
    }
  /*
    Assign index values to colormap entries.
  */
  for (i=0; i < image->colors; i++)
    image->colormap[i].index=(unsigned short) i;
  /*
    Sort image colormap by decreasing color popularity.
  */
  qsort((void *) image->colormap,(int) image->colors,sizeof(ColorPacket),
    (int (*)(const void *, const void *)) IntensityCompare);
  /*
    Update image colormap indexes to sorted colormap order.
  */
  for (i=0; i < image->colors; i++)
    pixels[image->colormap[i].index]=(unsigned short) i;
  p=image->pixels;
  for (i=0; i < image->packets; i++)
  {
    index=pixels[p->index];
    p->red=image->colormap[index].red;
    p->green=image->colormap[index].green;
    p->blue=image->colormap[index].blue;
    p->index=index;
    p++;
  }
  free((char *) pixels);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   S t e r e o I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function StereoImage combines two images and produces a single image that
%  is the composite of a left and right image of a stereo pair.  The left
%  image is converted to gray_scale and written to the red channel of the
%  stereo image.  The right image is converted to gray_scale and written to the
%  blue channel of the stereo image.  View the composite image with red-blue
%  glasses to create a stereo effect.
%
%  The format of the StereoImage routine is:
%
%      stereo_image=StereoImage(left_image,right_image)
%
%  A description of each parameter follows:
%
%    o stereo_image: Function StereoImage returns a pointer to the stereo
%      image.  A null image is returned if there is a memory shortage.
%
%    o left_image: The address of a structure of type Image.
%
%    o right_image: The address of a structure of type Image.
%
%
*/
Export Image *StereoImage(Image *left_image,Image *right_image)
{
#define StereoImageText  "  Stereo image...  "

  Image
    *stereo_image;

  int
    y;

  QuantizeInfo
    quantize_info;

  register int
    x;

  register RunlengthPacket
    *p,
    *q,
    *r;

  assert(left_image != (Image *) NULL);
  assert(right_image != (Image *) NULL);
  if ((left_image->columns != right_image->columns) ||
      (left_image->rows != right_image->rows))
    {
      Warning("Unable to create stereo image",
        "left and right image sizes differ");
      return((Image *) NULL);
    }
  /*
    Initialize stereo image attributes.
  */
  stereo_image=CopyImage(left_image,left_image->columns,left_image->rows,False);
  if (stereo_image == (Image *) NULL)
    {
      Warning("Unable to create stereo image","Memory allocation failed");
      return((Image *) NULL);
    }
  stereo_image->class=DirectClass;
  /*
    Copy left image to red channel and right image to blue channel.
  */
  GetQuantizeInfo(&quantize_info);
  quantize_info.colorspace=GRAYColorspace;
  QuantizeImage(&quantize_info,left_image);
  SyncImage(left_image);
  p=left_image->pixels;
  left_image->runlength=p->length+1;
  QuantizeImage(&quantize_info,right_image);
  SyncImage(right_image);
  q=right_image->pixels;
  right_image->runlength=q->length+1;
  r=stereo_image->pixels;
  for (y=0; y < stereo_image->rows; y++)
  {
    for (x=0; x < stereo_image->columns; x++)
    {
      if (left_image->runlength != 0)
        left_image->runlength--;
      else
        {
          p++;
          left_image->runlength=p->length;
        }
      if (right_image->runlength != 0)
        right_image->runlength--;
      else
        {
          q++;
          right_image->runlength=q->length;
        }
      r->red=(unsigned int) (p->red*12) >> 4;
      r->green=0;
      r->blue=q->blue;
      r->index=0;
      r->length=0;
      r++;
    }
    ProgressMonitor(StereoImageText,y,stereo_image->rows);
  }
  return(stereo_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   S y n c I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function SyncImage initializes the red, green, and blue intensities of each
%  pixel as defined by the colormap index.
%
%  The format of the SyncImage routine is:
%
%      SyncImage(image)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%
*/
Export void SyncImage(Image *image)
{
  register int
    i;

  register RunlengthPacket
    *p;

  register unsigned short
    index;

  assert(image != (Image *) NULL);
  if (image->class == DirectClass)
    return;
  for (i=0; i < image->colors; i++)
  {
    image->colormap[i].index=0;
    image->colormap[i].flags=0;
  }
  p=image->pixels;
  for (i=0; i < image->packets; i++)
  {
    index=p->index;
    p->red=image->colormap[index].red;
    p->green=image->colormap[index].green;
    p->blue=image->colormap[index].blue;
    p++;
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     T e x t u r e I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function TextureImage layers a texture onto the background of an image.
%
%  The format of the TextureImage routine is:
%
%      TextureImage(image,filename)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o filename: This file contains the texture to layer on the background.
%
%
*/
void TextureImage(Image *image,char *filename)
{
#define TextureImageText  "  Appling image texture...  "

  Image
    *texture_image;

  ImageInfo
    texture_info;

  int
    x,
    y;

  assert(image != (Image *) NULL);
  if (filename == (char *) NULL)
    return;
  /*
    Read the texture image.
  */
  GetImageInfo(&texture_info);
  (void) strcpy(texture_info.filename,filename);
  texture_image=ReadImage(&texture_info);
  if (texture_image == (Image *) NULL)
    return;
  /*
    Tile texture onto the image background.
  */
  for (y=0; y < image->rows; y+=texture_image->rows)
  {
    for (x=0; x < image->columns; x+=texture_image->columns)
      CompositeImage(image,ReplaceCompositeOp,texture_image,x,y);
    ProgressMonitor(TextureImageText,y,image->rows);
  }
  DestroyImage(texture_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     T h r e s h o l d I m a g e                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ThresholdImage thresholds the reference image.
%
%  The format of the ThresholdImage routine is:
%
%      ThresholdImage(image,threshold)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o threshold: A character string indicating the threshold value.
%
%
*/
void ThresholdImage(Image *image,char *threshold)
{
#define ThresholdImageText  "  Threshold the image...  "

  ColorPacket
    *colormap;

  int
    intensity_threshold;

  register int
    i;

  register RunlengthPacket
    *p;

  assert(image != (Image *) NULL);
  if (threshold == (char *) NULL)
    return;
  /*
    Threshold image.
  */
  intensity_threshold=atoi(threshold);
  colormap=(ColorPacket *) malloc(2*sizeof(ColorPacket));
  if (colormap == (ColorPacket *) NULL)
    {
      Warning("Unable to allocate image","Memory allocation failed");
      return;
    }
  if (image->colormap != (ColorPacket *) NULL)
    free((char *) image->colormap);
  image->class=PseudoClass;
  image->colors=2;
  image->colormap=colormap;
  image->colormap[0].red=0;
  image->colormap[0].green=0;
  image->colormap[0].blue=0;
  image->colormap[1].red=MaxRGB;
  image->colormap[1].green=MaxRGB;
  image->colormap[1].blue=MaxRGB;
  p=image->pixels;
  for (i=0; i < image->packets; i++)
  {
    p->index=Intensity(*p) < intensity_threshold ? 0 : 1;
    p++;
    if (QuantumTick(i,image))
      ProgressMonitor(ThresholdImageText,i,image->packets);
  }
  SyncImage(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   T r a n s f o r m I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function TransformImage creates a new image that is a transformed size of
%  of existing one as specified by the crop and image geometries.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  If a crop geometry is specified a subregion of the image is obtained.
%  If the specified image size, as defined by the image and scale geometries,
%  is smaller than the actual image size, the image is first minified to an
%  integral of the specified image size with an antialias digital filter.  The
%  image is then scaled to the exact specified image size with pixel
%  replication.  If the specified image size is greater than the actual image
%  size, the image is first enlarged to an integral of the specified image
%  size with bilinear interpolation.  The image is then scaled to the exact
%  specified image size with pixel replication.
%
%  The format of the TransformImage routine is:
%
%      TransformImage(image,crop_geometry,image_geometry)
%
%  A description of each parameter follows:
%
%    o image: The address of an address of a structure of type Image.  The
%      transformed image is returned as this parameter.
%
%    o crop_geometry: Specifies a pointer to a crop geometry string.
%      This geometry defines a subregion of the image.
%
%    o image_geometry: Specifies a pointer to a image geometry string.
%      The specified width and height of this geometry string are absolute.
%
%
*/
Export void TransformImage(Image **image,char *crop_geometry,
  char *image_geometry)
{
  Image
    *transformed_image;

  int
    flags,
    x,
    y;

  unsigned int
    height,
    sharpen,
    width;

  assert(image != (Image **) NULL);
  transformed_image=(*image);
  if (crop_geometry != (char *) NULL)
    {
      Image
        *cropped_image;

      RectangleInfo
        crop_info;

      /*
        Crop image to a user specified size.
      */
      crop_info.x=0;
      crop_info.y=0;
      flags=
        XParseGeometry(crop_geometry,&crop_info.x,&crop_info.y,&width,&height);
      if ((flags & WidthValue) == 0)
        width=(unsigned int) ((int) transformed_image->columns-crop_info.x);
      if ((flags & HeightValue) == 0)
        height=(unsigned int) ((int) transformed_image->rows-crop_info.y);
      if ((flags & XNegative) != 0)
        crop_info.x+=transformed_image->columns-width;
      if ((flags & YNegative) != 0)
        crop_info.y+=transformed_image->rows-height;
      if (strchr(crop_geometry,'%') != (char *) NULL)
        {
          /*
            Crop geometry is relative to image size.
          */
          (void) ParseImageGeometry(crop_geometry,&x,&y,&width,&height);
          if (width > transformed_image->columns)
            width=transformed_image->columns;
          if (height > transformed_image->rows)
            height=transformed_image->rows;
          crop_info.x=width >> 1;
          crop_info.y=height >> 1;
          width=transformed_image->columns-width;
          height=transformed_image->rows-height;
          flags|=XValue | YValue;
        }
      crop_info.width=width;
      crop_info.height=height;
      if ((width == 0) || (height == 0) ||
          ((flags & XValue) != 0) || ((flags & YValue) != 0))
        cropped_image=CropImage(transformed_image,&crop_info);
      else
        {
          Image
            *next_image;

          /*
            Crop repeatedly to create uniform subimages.
          */
          next_image=(Image *) NULL;
          cropped_image=(Image *) NULL;
          for (y=0; y < transformed_image->rows; y+=height)
          {
            for (x=0; x < transformed_image->columns; x+=width)
            {
              crop_info.width=width;
              crop_info.height=height;
              crop_info.x=x;
              crop_info.y=y;
              next_image=CropImage(transformed_image,&crop_info);
              if (next_image == (Image *) NULL)
                break;
              if (cropped_image == (Image *) NULL)
                cropped_image=next_image;
              else
                {
                  next_image->previous=cropped_image;
                  cropped_image->next=next_image;
                  cropped_image=cropped_image->next;
                }
            }
            if (next_image == (Image *) NULL)
              break;
          }
        }
      if (cropped_image != (Image *) NULL)
        {
          DestroyImage(transformed_image);
          while (cropped_image->previous != (Image *) NULL)
            cropped_image=cropped_image->previous;
          transformed_image=cropped_image;
        }
    }
  /*
    Scale image to a user specified size.
  */
  width=transformed_image->columns;
  height=transformed_image->rows;
  (void) ParseImageGeometry(image_geometry,&x,&y,&width,&height);
  sharpen=(width*height) < (transformed_image->rows*transformed_image->columns);
  if ((transformed_image->columns != width) ||
      (transformed_image->rows != height))
    {
      Image
        *zoomed_image;

      /*
        Zoom image.
      */
      zoomed_image=ZoomImage(transformed_image,width,height,MitchellFilter);
      if (zoomed_image == (Image *) NULL)
        zoomed_image=ScaleImage(transformed_image,width,height);
      if (zoomed_image != (Image *) NULL)
        {
          DestroyImage(transformed_image);
          transformed_image=zoomed_image;
        }
    }
  if (sharpen)
    if ((transformed_image->columns >= 3) && (transformed_image->rows >= 3))
      {
        Image
          *sharpened_image;

        /*
          Sharpen image.
        */
        sharpened_image=SharpenImage(transformed_image,SharpenFactor);
        if (sharpened_image != (Image *) NULL)
          {
            DestroyImage(transformed_image);
            transformed_image=sharpened_image;
          }
      }
  *image=transformed_image;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     T r a n s f o r m R G B I m a g e                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function TransformRGBImage converts the reference image from an alternate
%  colorspace.  The transformation matrices are not the standard ones:  the
%  weights are rescaled to normalized the range of the transformed values to
%  be [0..MaxRGB].
%
%  The format of the TransformRGBImage routine is:
%
%      TransformRGBImage(image,colorspace)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o colorspace: An unsigned integer value that indicates the colorspace
%      the image is currently in.  On return the image is in the RGB
%      color space.
%
%
*/
void TransformRGBImage(Image *image,const unsigned int colorspace)
{
#define B (MaxRGB+1)*2
#define G (MaxRGB+1)
#define R 0
#define TransformRGBImageText  "  Transforming image colors...  "

  static Quantum
    PCDMap[348] =  /* Photo CD information beyond 100% white, Gamma 2.2 */
    {
        0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  11,  12,  13,  14,
       15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,
       29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,
       43,  44,  45,  46,  47,  48,  49,  50,  51,  52,  53,  54,  55,  55,
       56,  57,  58,  59,  60,  61,  62,  63,  64,  65,  66,  66,  67,  68,
       69,  70,  71,  72,  73,  74,  75,  76,  76,  77,  78,  79,  80,  81,
       82,  83,  84,  84,  85,  86,  87,  88,  89,  90,  91,  92,  92,  93,
       94,  95,  96,  97,  98,  99,  99, 100, 101, 102, 103, 104, 105, 106,
      106, 107, 108, 109, 110, 111, 112, 113, 114, 114, 115, 116, 117, 118,
      119, 120, 121, 122, 122, 123, 124, 125, 126, 127, 128, 129, 129, 130,
      131, 132, 133, 134, 135, 136, 136, 137, 138, 139, 140, 141, 142, 142,
      143, 144, 145, 146, 147, 148, 148, 149, 150, 151, 152, 153, 153, 154,
      155, 156, 157, 158, 158, 159, 160, 161, 162, 163, 164, 165, 165, 166,
      167, 168, 169, 170, 171, 172, 173, 173, 174, 175, 176, 177, 178, 178,
      179, 180, 181, 182, 182, 183, 184, 185, 186, 186, 187, 188, 189, 190,
      191, 192, 193, 194, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203,
      204, 205, 205, 206, 207, 208, 209, 210, 210, 211, 212, 213, 214, 215,
      216, 216, 217, 218, 219, 220, 221, 221, 222, 223, 224, 225, 225, 226,
      227, 228, 228, 229, 230, 230, 231, 232, 233, 233, 234, 235, 235, 236,
      237, 237, 238, 239, 239, 240, 241, 241, 242, 242, 243, 243, 244, 244,
      245, 245, 245, 246, 246, 247, 247, 247, 248, 248, 248, 249, 249, 249,
      250, 250, 250, 250, 251, 251, 251, 251, 252, 252, 252, 252, 252, 252,
      253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 254, 254, 254, 254,
      254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254,
      254, 254, 254, 254, 254, 254, 255, 255, 255, 255, 255, 255
    };

  long
    *blue,
    *green,
    *red;

  Quantum
    *range_table;

  register int
    i,
    x,
    y,
    z;

  register Quantum
    *range_limit;

  register RunlengthPacket
    *p;

  assert(image != (Image *) NULL);
  if ((colorspace == RGBColorspace) || (colorspace == GRAYColorspace) ||
      (colorspace == TransparentColorspace))
    return;
  /*
    Allocate the tables.
  */
  red=(long *) malloc(3*(MaxRGB+1)*sizeof(long));
  green=(long *) malloc(3*(MaxRGB+1)*sizeof(long));
  blue=(long *) malloc(3*(MaxRGB+1)*sizeof(long));
  range_table=(Quantum *) malloc(4*(MaxRGB+1)*sizeof(Quantum));
  if ((red == (long *) NULL) || (green == (long *) NULL) ||
      (blue == (long *) NULL) || (range_table == (Quantum *) NULL))
    {
      Warning("Unable to transform color space","Memory allocation failed");
      return;
    }
  /*
    Initialize tables.
  */
  for (i=0; i <= MaxRGB; i++)
  {
    range_table[i]=0;
    range_table[i+(MaxRGB+1)]=(Quantum) i;
    range_table[i+(MaxRGB+1)*2]=MaxRGB;
  }
  for (i=0; i <= MaxRGB; i++)
    range_table[i+(MaxRGB+1)*3]=MaxRGB;
  range_limit=range_table+(MaxRGB+1);
  switch (colorspace)
  {
    case OHTAColorspace:
    {
      /*
        Initialize OHTA tables:

          R = I1+1.00000*I2-0.66668*I3
          G = I1+0.00000*I2+1.33333*I3
          B = I1-1.00000*I2-0.66668*I3

        I and Q, normally -0.5 through 0.5, must be normalized to the range 0
        through MaxRGB.
      */
      for (i=0; i <= MaxRGB; i++)
      {
        red[i+R]=UpShifted(1.00000)*i;
        green[i+R]=UpShifted(1.0000*0.5)*((i << 1)-MaxRGB);
        blue[i+R]=(-UpShifted(0.66668*0.5))*((i << 1)-MaxRGB);
        red[i+G]=UpShifted(1.00000)*i;
        green[i+G]=0;
        blue[i+G]=UpShifted(1.33333*0.5)*((i << 1)-MaxRGB);
        red[i+B]=UpShifted(1.00000)*i;
        green[i+B]=(-UpShifted(1.00000*0.5))*((i << 1)-MaxRGB);
        blue[i+B]=(-UpShifted(0.66668*0.5))*((i << 1)-MaxRGB);
      }
      break;
    }
    case XYZColorspace:
    {
      /*
        Initialize CIE XYZ tables:

          R =  3.240479*R-1.537150*G-0.498535*B
          G = -0.969256*R+1.875992*G+0.041556*B
          B =  0.055648*R-0.204043*G+1.057311*B
      */
      for (i=0; i <= MaxRGB; i++)
      {
        red[i+R]=UpShifted(3.240479)*i;
        green[i+R]=(-UpShifted(1.537150))*i;
        blue[i+R]=(-UpShifted(0.498535))*i;
        red[i+G]=(-UpShifted(0.969256))*i;
        green[i+G]=UpShifted(1.875992)*i;
        blue[i+G]=UpShifted(0.041556)*i;
        red[i+B]=UpShifted(0.055648)*i;
        green[i+B]=(-UpShifted(0.204043))*i;
        blue[i+B]=UpShifted(1.057311)*i;
      }
      break;
    }
    case YCbCrColorspace:
    {
      /*
        Initialize YCbCr tables:

          R = Y            +1.370707*Cr
          G = Y-0.336453*Cb-0.698195*Cr
          B = Y+1.732445*Cb

        Cb and Cr, normally -0.5 through 0.5, must be normalized to the range 0
        through MaxRGB.
      */
      for (i=0; i <= MaxRGB; i++)
      {
        red[i+R]=UpShifted(1.000000)*i;
        green[i+R]=0;
        blue[i+R]=UpShifted(1.370707*0.5)*((i << 1)-MaxRGB);
        red[i+G]=UpShifted(1.000000)*i;
        green[i+G]=(-UpShifted(0.336453*0.5))*((i << 1)-MaxRGB);
        blue[i+G]=(-UpShifted(0.698195*0.5))*((i << 1)-MaxRGB);
        red[i+B]=UpShifted(1.000000)*i;
        green[i+B]=UpShifted(1.732445*0.5)*((i << 1)-MaxRGB);
        blue[i+B]=0;
      }
      break;
    }
    case YCCColorspace:
    {
      /*
        Initialize YCC tables:

          R = Y            +1.340762*C2
          G = Y-0.317038*C1-0.682243*C2
          B = Y+1.632639*C1

        YCC is scaled by 1.3584.  C1 zero is 156 and C2 is at 137.
      */
      for (i=0; i <= MaxRGB; i++)
      {
        red[i+R]=UpShifted(1.3584)*i;
        green[i+R]=0;
        blue[i+R]=UpShifted(1.8215)*(i-UpScale(137));
        red[i+G]=UpShifted(1.3584)*i;
        green[i+G]=(-(UpShifted(0.194*2.2179)*(i-UpScale(156))));
        blue[i+G]=(-(UpShifted(0.509*1.8215)*(i-UpScale(137))));
        red[i+B]=UpShifted(1.3584)*i;
        green[i+B]=UpShifted(2.2179)*(i-UpScale(156));
        blue[i+B]=0;
        range_table[i+(MaxRGB+1)]=(Quantum) UpScale(PCDMap[DownScale(i)]);
      }
      for ( ; i < UpScale(348); i++)
        range_table[i+(MaxRGB+1)]=(Quantum) UpScale(PCDMap[DownScale(i)]);
      break;
    }
    case YIQColorspace:
    {
      /*
        Initialize YIQ tables:

          R = 0.97087*Y+1.17782*I+0.59800*Q
          G = 0.97087*Y-0.28626*I-0.72851*Q
          B = 0.97087*Y-1.27870*I+1.72801*Q

        I and Q, normally -0.5 through 0.5, must be normalized to the range 0
        through MaxRGB.
      */
      for (i=0; i <= MaxRGB; i++)
      {
        red[i+R]=UpShifted(0.97087)*i;
        green[i+R]=UpShifted(1.17782*0.5)*((i << 1)-MaxRGB);
        blue[i+R]=UpShifted(0.59800*0.5)*((i << 1)-MaxRGB);
        red[i+G]=UpShifted(0.97087)*i;
        green[i+G]=(-UpShifted(0.28626*0.5))*((i << 1)-MaxRGB);
        blue[i+G]=(-UpShifted(0.72851*0.5))*((i << 1)-MaxRGB);
        red[i+B]=UpShifted(0.97087)*i;
        green[i+B]=(-UpShifted(1.27870*0.5))*((i << 1)-MaxRGB);
        blue[i+B]=UpShifted(1.72801*0.5)*((i << 1)-MaxRGB);
      }
      break;
    }
    case YPbPrColorspace:
    {
      /*
        Initialize YPbPr tables:

          R = Y            +1.402000*C2
          G = Y-0.344136*C1+0.714136*C2
          B = Y+1.772000*C1

        Pb and Pr, normally -0.5 through 0.5, must be normalized to the range 0
        through MaxRGB.
      */
      for (i=0; i <= MaxRGB; i++)
      {
        red[i+R]=UpShifted(1.000000)*i;
        green[i+R]=0;
        blue[i+R]=UpShifted(1.402000*0.5)*((i << 1)-MaxRGB);
        red[i+G]=UpShifted(1.000000)*i;
        green[i+G]=(-UpShifted(0.344136*0.5))*((i << 1)-MaxRGB);
        blue[i+G]=UpShifted(0.714136*0.5)*((i << 1)-MaxRGB);
        red[i+B]=UpShifted(1.000000)*i;
        green[i+B]=UpShifted(1.772000*0.5)*((i << 1)-MaxRGB);
        blue[i+B]=0;
      }
      break;
    }
    case YUVColorspace:
    default:
    {
      /*
        Initialize YUV tables:

          R = Y          +1.13980*V
          G = Y-0.39380*U-0.58050*V
          B = Y+2.02790*U

        U and V, normally -0.5 through 0.5, must be normalized to the range 0
        through MaxRGB.
      */
      for (i=0; i <= MaxRGB; i++)
      {
        red[i+R]=UpShifted(1.00000)*i;
        green[i+R]=0;
        blue[i+R]=UpShifted(1.13980*0.5)*((i << 1)-MaxRGB);
        red[i+G]=UpShifted(1.00000)*i;
        green[i+G]=(-UpShifted(0.39380*0.5))*((i << 1)-MaxRGB);
        blue[i+G]=(-UpShifted(0.58050*0.5))*((i << 1)-MaxRGB);
        red[i+B]=UpShifted(1.00000)*i;
        green[i+B]=UpShifted(2.02790*0.5)*((i << 1)-MaxRGB);
        blue[i+B]=0;
      }
      break;
    }
  }
  /*
    Convert to RGB.
  */
  switch (image->class)
  {
    case DirectClass:
    default:
    {
      /*
        Convert DirectClass image.
      */
      p=image->pixels;
      for (i=0; i < image->packets; i++)
      {
        x=p->red;
        y=p->green;
        z=p->blue;
        p->red=range_limit[DownShift(red[x+R]+green[y+R]+blue[z+R])];
        p->green=range_limit[DownShift(red[x+G]+green[y+G]+blue[z+G])];
        p->blue=range_limit[DownShift(red[x+B]+green[y+B]+blue[z+B])];
        p++;
        if (QuantumTick(i,image))
          ProgressMonitor(TransformRGBImageText,i,image->packets);
      }
      break;
    }
    case PseudoClass:
    {
      /*
        Convert PseudoClass image.
      */
      for (i=0; i < image->colors; i++)
      {
        x=image->colormap[i].red;
        y=image->colormap[i].green;
        z=image->colormap[i].blue;
        image->colormap[i].red=
          range_limit[DownShift(red[x+R]+green[y+R]+blue[z+R])];
        image->colormap[i].green=
          range_limit[DownShift(red[x+G]+green[y+G]+blue[z+G])];
        image->colormap[i].blue=
          range_limit[DownShift(red[x+B]+green[y+B]+blue[z+B])];
      }
      p=image->pixels;
      for (i=0; i < image->packets; i++)
      {
        x=p->red;
        y=p->green;
        z=p->blue;
        p->red=range_limit[DownShift(red[x+R]+green[y+R]+blue[z+R])];
        p->green=range_limit[DownShift(red[x+G]+green[y+G]+blue[z+G])];
        p->blue=range_limit[DownShift(red[x+B]+green[y+B]+blue[z+B])];
        p++;
      }
      break;
    }
  }
  /*
    Free allocated memory.
  */
  free((char *) range_table);
  free((char *) blue);
  free((char *) green);
  free((char *) red);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     T r a n s p a r e n t I m a g e                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function TransparentImage creates a matte image associated with the
%  image.  All pixel locations are initially set to opaque.  Any pixel
%  that matches the specified color are set to transparent.
%
%  The format of the TransparentImage routine is:
%
%      TransparentImage(image,color)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o color: A character string that contain an X11 color string.
%
%
*/
Export void TransparentImage(Image *image,char *color)
{
#define DeltaX  16
#define TransparentImageText  "  Setting transparent color in the image...  "

  ColorPacket
    target;

  register int
    i;

  register RunlengthPacket
    *p;

  unsigned int
    status;

  XColor
    target_color;

  /*
    Determine RGB values of the transparent color.
  */
  assert(image != (Image *) NULL);
  status=XQueryColorDatabase(color,&target_color);
  if (status == False)
    return;
  target.red=XDownScale(target_color.red);
  target.green=XDownScale(target_color.green);
  target.blue=XDownScale(target_color.blue);
  /*
    Make image color transparent.
  */
  if (!image->matte)
    {
      /*
        Initialize image matte to opaque.
      */
      image->class=DirectClass;
      image->matte=True;
      p=image->pixels;
      for (i=0; i < image->packets; i++)
      {
        p->index=Opaque;
        p++;
      }
    }
  p=image->pixels;
  for (i=0; i < image->packets; i++)
  {
    if (ColorMatch(*p,target,DeltaX))
      p->index=Transparent;
    p++;
    if (QuantumTick(i,image))
      ProgressMonitor(TransparentImageText,i,image->packets);
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   U n c o m p r e s s I m a g e                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function UncompressImage uncompresses runlength-encoded pixels packets to
%  a rectangular array of pixels.
%
%  The format of the UncompressImage routine is:
%
%      status=UncompressImage(image)
%
%  A description of each parameter follows:
%
%    o status: Function UncompressImage returns True if the image is
%      uncompressed otherwise False.
%
%    o image: The address of a structure of type Image.
%
%
*/
Export unsigned int UncompressImage(Image *image)
{
  int
    length;

  register int
    i,
    j;

  register RunlengthPacket
    *p,
    *q;

  RunlengthPacket
    *uncompressed_pixels;

  assert(image != (Image *) NULL);
  if (image->packets == (image->columns*image->rows))
    return(True);
  /*
    Uncompress runlength-encoded packets.
  */
  uncompressed_pixels=(RunlengthPacket *) realloc((char *) image->pixels,
    image->columns*image->rows*sizeof(RunlengthPacket));
  if (uncompressed_pixels == (RunlengthPacket *) NULL)
    {
      Warning("Unable to uncompress image","Memory allocation failed");
      return(False);
    }
  p=uncompressed_pixels+(image->packets-1);
  q=uncompressed_pixels+(image->columns*image->rows-1);
  for (i=0; i < image->packets; i++)
  {
    length=p->length;
    for (j=0; j <= length; j++)
    {
      *q=(*p);
      q->length=0;
      q--;
    }
    p--;
  }
  image->packets=image->columns*image->rows;
  image->pixels=uncompressed_pixels;
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   Z o o m I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ZoomImage creates a new image that is a scaled size of an
%  existing one.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.  The Point filter gives
%  fast pixel replication, Triangle is equivalent to bi-linear interpolation,
%  and Mitchel giver slower, very high-quality results.
%
%  The format of the ZoomImage routine is:
%
%      zoomed_image=ZoomImage(image,columns,rows,filter)
%
%  A description of each parameter follows:
%
%    o zoomed_image: Function ZoomImage returns a pointer to the image after
%      scaling.  A null image is returned if there is a memory shortage.
%
%    o image: The address of a structure of type Image.
%
%    o columns: An integer that specifies the number of columns in the zoomed
%      image.
%
%    o rows: An integer that specifies the number of rows in the scaled
%      image.
%
%    o filter: This unsigned integer is the filter type to used to zoom
%      the image.
%
%
*/

#define ZoomImageText  "  Zooming image...  "

static double Box(double x)
{
  if ((x > -0.5) && (x <= 0.5))
    return(1.0);
  return(0.0);
}

static double Mitchell(double x)
{
  double
    b,
    c;

  b=1.0/3.0;
  c=1.0/3.0;
  if (x < 0)
    x=(-x);
  if (x < 1.0)
    {
      x=(((12.0-9.0*b-6.0*c)*(x*x*x))+((-18.0+12.0*b+6.0*c)*x*x)+(6.0-2.0*b))/
        6.0;
      return(x);
    }
 if (x < 2.0)
   {
     x=(((-1.0*b-6.0*c)*(x*x*x))+((6.0*b+30.0*c)*x*x)+((-12.0*b-48.0*c)*x)+
       (8.0*b+24.0*c))/6.0;
     return(x);
   }
  return(0.0);
}

static double Triangle(double x)
{
  if (x < 0.0)
    x=(-x);
  if (x < 1.0)
    return(1.0-x);
  return(0.0);
}

static void HorizontalFilter(Image *source,Image *destination,double x_factor,
  double (*FilterFunction)(double),ContributionInfo *contribution_info,
  Quantum *range_limit,double width,unsigned int span,unsigned int *quantum)
{
  double
    center,
    scale_factor;

  int
    n,
    x;

  long
    blue_weight,
    green_weight,
    index_weight,
    red_weight,
    weight;

  register int
    i,
    j,
    y;

  register RunlengthPacket
    *p,
    *q;

  /*
    Apply filter to zoom horizontally from source to destination.
  */
  scale_factor=1.0;
  if (x_factor < 1.0)
    {
      width/=x_factor;
      scale_factor/=x_factor;
    }
  for (x=0; x < destination->columns; x++)
  {
    n=0;
    center=(double) (x+0.5)/x_factor;
    for (i=(int) (center-width+0.5); i < (int) (center+width+0.5); i++)
    {
      j=i;
      if (j < 0)
        j=(-j);
      else
        if (j >= source->columns)
          j=(source->columns << 1)-j-1;
      if (j >= source->columns)
        j=0;
      contribution_info[n].pixel=j;
      contribution_info[n].weight=
        UpShifted(FilterFunction((i-center+0.5)/scale_factor)/scale_factor);
      n++;
    }
    q=destination->pixels+x;
    for (y=0; y < destination->rows; y++)
    {
      blue_weight=0;
      green_weight=0;
      red_weight=0;
      index_weight=0;
      for (i=0; i < n; i++)
      {
        weight=contribution_info[i].weight;
        p=source->pixels+(y*source->columns)+contribution_info[i].pixel;
        red_weight+=weight*p->red;
        green_weight+=weight*p->green;
        blue_weight+=weight*p->blue;
        index_weight+=weight*p->index;
      }
      q->red=range_limit[DownShift(red_weight)];
      q->green=range_limit[DownShift(green_weight)];
      q->blue=range_limit[DownShift(blue_weight)];
      if (index_weight > UpShift(Opaque))
        q->index=Opaque;
      else
        if (index_weight < UpShift(Transparent))
          q->index=Transparent;
        else
          q->index=DownShift(index_weight);
      q->length=0;
      q+=destination->columns;
    }
    ProgressMonitor(ZoomImageText,*quantum,span);
    (*quantum)++;
  }
}

static void VerticalFilter(Image *source,Image *destination,double y_factor,
  double (*FilterFunction)(double),ContributionInfo *contribution_info,
  Quantum *range_limit,double width,unsigned int span,unsigned int *quantum)
{
  double
    center,
    scale_factor;

  int
    n,
    y;

  long
    blue_weight,
    green_weight,
    index_weight,
    red_weight,
    weight;

  register int
    i,
    j,
    x;

  register RunlengthPacket
    *p,
    *q;

  /*
    Apply filter to zoom vertically from source to destination.
  */
  scale_factor=1.0;
  if (y_factor < 1.0)
    {
      width/=y_factor;
      scale_factor/=y_factor;
    }
  q=destination->pixels;
  for (y=0; y < destination->rows; y++)
  {
    n=0;
    center=(double) (y+0.5)/y_factor;
    for (i=(int) (center-width+0.5); i < (int) (center+width+0.5); i++)
    {
      j=i;
      if (j < 0)
        j=(-j);
      else
        if (j >= source->rows)
          j=(source->rows << 1)-j-1;
      if (j >= source->rows)
        j=0;
      contribution_info[n].pixel=j;
      contribution_info[n].weight=
        UpShifted(FilterFunction((i-center+0.5)/scale_factor)/scale_factor);
      n++;
    }
    for (x=0; x < destination->columns; x++)
    {
      blue_weight=0;
      green_weight=0;
      red_weight=0;
      index_weight=0;
      for (i=0; i < n; i++)
      {
        weight=contribution_info[i].weight;
        p=source->pixels+(contribution_info[i].pixel*source->columns)+x;
        red_weight+=weight*p->red;
        green_weight+=weight*p->green;
        blue_weight+=weight*p->blue;
        index_weight+=weight*p->index;
      }
      q->red=range_limit[DownShift(red_weight)];
      q->green=range_limit[DownShift(green_weight)];
      q->blue=range_limit[DownShift(blue_weight)];
      if (index_weight > UpShift(Opaque))
        q->index=Opaque;
      else
        if (index_weight < UpShift(Transparent))
          q->index=Transparent;
        else
          q->index=DownShift(index_weight);
      q->length=0;
      q++;
    }
    ProgressMonitor(ZoomImageText,*quantum,span);
    (*quantum)++;
  }
}

Export Image *ZoomImage(Image *image,const unsigned int columns,
  const unsigned int rows,const FilterType filter)
{
  ContributionInfo
    *contribution_info;

  double
    (*FilterFunction)(double),
    filter_width,
    width,
    x_factor,
    y_factor;

  Image
    *source_image,
    *zoomed_image;

  Quantum
    *range_table;

  register int
     i;

  register Quantum
    *range_limit;

  unsigned int
    quantum,
    span;

  assert(image != (Image *) NULL);
  if ((columns == 0) || (rows == 0))
    {
      Warning("Unable to zoom image","image dimensions are zero");
      return((Image *) NULL);
    }
  /*
    Image must be uncompressed.
  */
  if (!UncompressImage(image))
    return((Image *) NULL);
  /*
    Initialize zoomed image attributes.
  */
  zoomed_image=CopyImage(image,columns,rows,False);
  if (zoomed_image == (Image *) NULL)
    {
      Warning("Unable to zoom image","Memory allocation failed");
      return((Image *) NULL);
    }
  zoomed_image->class=DirectClass;
  image->orphan=True;
  if (zoomed_image->rows >= image->rows)
    source_image=CopyImage(image,zoomed_image->columns,image->rows,False);
  else
    source_image=CopyImage(image,image->columns,zoomed_image->rows,False);
  image->orphan=False;
  if (source_image == (Image *) NULL)
    {
      Warning("Unable to zoom image","Memory allocation failed");
      DestroyImage(zoomed_image);
      return((Image *) NULL);
    }
  /*
    Allocate the range table.
  */
  range_table=(Quantum *) malloc(3*(MaxRGB+1)*sizeof(Quantum));
  if (range_table == (Quantum *) NULL)
    {
      Warning("Unable to zoom image","Memory allocation failed");
      DestroyImage(source_image);
      DestroyImage(zoomed_image);
      return((Image *) NULL);
    }
  /*
    Pre-compute conversion tables.
  */
  for (i=0; i <= MaxRGB; i++)
  {
    range_table[i]=0;
    range_table[i+(MaxRGB+1)]=(Quantum) i;
    range_table[i+(MaxRGB+1)*2]=MaxRGB;
  }
  range_limit=range_table+(MaxRGB+1);
  /*
    Allocate filter info list.
  */
  switch (filter)
  {
    case BoxFilter:
    {
      FilterFunction=Box;
      filter_width=0.5;
      break;
    }
    case TriangleFilter:
    {
      FilterFunction=Triangle;
      filter_width=1.0;
      break;
    }
    case MitchellFilter:
    default:
    {
      FilterFunction=Mitchell;
      filter_width=2.0;
      break;
    }
  }
  x_factor=(double) zoomed_image->columns/(double) image->columns;
  y_factor=(double) zoomed_image->rows/(double) image->rows;
  width=Max(filter_width/x_factor,filter_width/y_factor);
  if (width < filter_width)
    width=filter_width;
  contribution_info=(ContributionInfo *)
    malloc((int) (width*2+1)*sizeof(ContributionInfo));
  if (contribution_info == (ContributionInfo *) NULL)
    {
      Warning("Unable to zoom image","Memory allocation failed");
      free((char *) range_table);
      DestroyImage(source_image);
      DestroyImage(zoomed_image);
      return((Image *) NULL);
    }
  /*
    Zoom image.
  */
  quantum=0;
  if (zoomed_image->rows >= image->rows)
    {
      span=source_image->columns+zoomed_image->rows;
      HorizontalFilter(image,source_image,x_factor,FilterFunction,
        contribution_info,range_limit,filter_width,span,&quantum);
      VerticalFilter(source_image,zoomed_image,y_factor,FilterFunction,
        contribution_info,range_limit,filter_width,span,&quantum);
    }
  else
    {
      span=zoomed_image->columns+source_image->columns;
      VerticalFilter(image,source_image,y_factor,FilterFunction,
        contribution_info,range_limit,filter_width,span,&quantum);
      HorizontalFilter(source_image,zoomed_image,x_factor,FilterFunction,
        contribution_info,range_limit,filter_width,span,&quantum);
    }
  /*
    Free allocated memory.
  */
  free((char *) contribution_info);
  free((char *) range_table);
  DestroyImage(source_image);
  return(zoomed_image);
}
