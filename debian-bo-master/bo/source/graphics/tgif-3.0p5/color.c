/*
 * Author:      William Chia-Wei Cheng (william@cs.ucla.edu)
 *
 * Copyright (C) 1990-1996, William Chia-Wei Cheng.
 *
 * Permission limited to the use, copy, display, distribute without
 * charging for a fee, and produce derivative works of "tgif" and
 * its documentation for not-for-profit purpose is hereby granted by
 * the Author, provided that the above copyright notice appears in
 * all copies made of "tgif" and that both the copyright notice
 * and this permission notice appear in supporting documentation,
 * and that the name of the Author not be used in advertising or
 * publicity pertaining to distribution of the software without
 * specific, written prior permission.  The Author makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied
 * warranty.  All other rights (including, but not limited to, the
 * right to sell "tgif", the right to sell derivative works of
 * "tgif", and the right to distribute "tgif" for a fee) are
 * reserved by the Author.
 *
 * THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, INDIRECT
 * OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#ifndef lint
static char RCSid[] =
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/color.c,v 3.0 1996/05/06 16:04:13 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "const.h"
#include "types.h"

#include "choice.e"
#include "cmd.e"
#ifndef _NO_EXTERN
#include "color.e"
#endif
#include "dialog.e"
#include "drawing.e"
#include "file.e"
#include "font.e"
#include "imgproc.e"
#include "mainloop.e"
#include "mark.e"
#include "menu.e"
#include "msg.e"
#include "obj.e"
#include "page.e"
#include "pattern.e"
#include "raster.e"
#include "select.e"
#include "setup.e"
#include "text.e"
#include "util.e"
#include "xpixmap.e"

extern int	atoi ARGS_DECL((char *));

#define COLORSTRLEN 80

#define WHITE_COLOR_INDEX 8
#define BLACK_COLOR_INDEX 9

#define MAX_VERTICAL_BTNS 6

int	maxColors = MAXCOLORS;
int	defaultColorIndex = 9;
int	colorIndex = 0;
int	* colorPixels = NULL;
int	* xorColorPixels = NULL;
int	* colorLayerOn = NULL;
char	myFgColorStr[COLORSTRLEN];
char	myBgColorStr[COLORSTRLEN];
char	* * colorMenuItems = NULL;
XColor	* tgifColors=NULL;
XColor	* tgifRequestedColors=NULL;
XColor	myBgColor;
int	maxRGB = 0;
int	colorDump = FALSE;
int	useLocalRGBTxt=FALSE;
int	printUsingRequestedColor=FALSE;
int	colorLayers=FALSE;
int	needToRedrawColorWindow=FALSE;
int	initColorDontReload=FALSE;
int	gnUpdatePixelObjCount=0;


static int colorWindowFirstIndex=0;
static int colorItemH=12;

static int	canChangeAttrColor=FALSE;
static int	defaultColorIndexInXDefaults = FALSE;
static int	allocatedMaxColors = MAXCOLORS;

static char *defaultColorMenuItems[MAXCOLORS]={
   "magenta", "red", "green", "blue", "yellow", "pink", "cyan", "CadetBlue",
   "white", "black", "DarkSlateGray"
};

void DefaultColorArrays (Entries, ForePixels, Valid, InitRV, StatusStr)
   int	Entries, * * ForePixels, * * Valid, * * InitRV;
   char	* * * StatusStr;
{
   register int	i, * fore_pixels, pixel, * valid, * init_rv;

   pixel = myFgPixel;
   *ForePixels = fore_pixels = (int*)malloc(Entries*sizeof(int));
   if (*ForePixels == NULL) FailAllocMessage();
   *Valid = valid = (int*)malloc(Entries*sizeof(int));
   if (*Valid == NULL) FailAllocMessage();
   *InitRV = init_rv = (int*)malloc(Entries*sizeof(int));
   if (*InitRV == NULL) FailAllocMessage();
   for (i = 0; i < Entries; i++) {
      *fore_pixels++ = pixel;
      *valid++ = TRUE;
      *init_rv++ = FALSE;
   }
   if (StatusStr != NULL) {
      *StatusStr = (char**)malloc(Entries*sizeof(char*));
      if (*StatusStr == NULL) FailAllocMessage();
      for (i=0; i < Entries; i++) {
         (*StatusStr)[i] = (char*)malloc((MAXSTRING+1)*sizeof(char));
         if ((*StatusStr)[i] == NULL) FailAllocMessage();
         *(*StatusStr)[i] = '\0';
      }
   }
}

struct LocalColorRec {
   char *name, *spec;
   int len;
};

static struct LocalColorRec *privateColorInfo=NULL;

int TgifParseColor(psz_color, p_color)
   char *psz_color;
   XColor *p_color;
{
   struct LocalColorRec *ptr;
   int len;

   if (!useLocalRGBTxt || *psz_color == '#') {
      return (int)XParseColor(mainDisplay, mainColormap, psz_color, p_color);
   }
   len = strlen(psz_color);
   for (ptr=privateColorInfo; ptr->name != NULL; ptr++) {
      if (len == ptr->len && strcmp(psz_color, ptr->name) == 0) {
         if (ptr->spec != NULL) {
            return (int)XParseColor(mainDisplay, mainColormap, ptr->spec,
                  p_color);
         }
         break;
      }
   }
   return (int)XParseColor(mainDisplay, mainColormap, psz_color, p_color);
}

static
int ParseAndAllocColorByName (colorname, color, red_req, green_req, blue_req)
   char			* colorname;
   XColor		* color;
   unsigned short	* red_req, * green_req, * blue_req;
{
   Colormap	colormap;

   if (!TgifParseColor(colorname, color))
   {
      fprintf (stderr, "Warning:  can not parse color '%s'\n", colorname);
      return (FALSE);
   }
   if (red_req != NULL) *red_req = color->red;
   if (green_req != NULL) *green_req = color->green;
   if (blue_req != NULL) *blue_req = color->blue;
   if (!XAllocColor(mainDisplay, mainColormap, color))
   {
      if (newColormapUsed)
      {
         fprintf (stderr, "Warning:  can not allocate color '%s'\n", colorname);
         return (FALSE);
      }
      colormap = XCopyColormapAndFree (mainDisplay, mainColormap);
      mainColormap = colormap;
      newColormapUsed = TRUE;
      if (mainWindow != None)
         XSetWindowColormap (mainDisplay, mainWindow, mainColormap);
      if (!XAllocColor(mainDisplay, mainColormap, color))
      {
         fprintf (stderr, "Warning:  can not allocate color '%s'\n", colorname);
         return (FALSE);
      }
   }
   return (TRUE);
}

static
int MyStrCmp (s1, s2)
   register char	* s1, * s2;
{
   for ( ; *s1 != '\0' && *s2 != '\0'; s1++, s2++)
      if (*s1 != *s2)
      {
         if (*s1 >= 'A' && *s1 <= 'Z')
         {
            if (*s1-'A'+'a' != *s2) return (*s1 - *s2);
         }
         else if (*s2 >= 'A' && *s2 <= 'Z')
         {
            if (*s2-'A'+'a' != *s1) return (*s1 - *s2);
         }
         else
            return (*s1 - *s2);
      }
   return (0);
}

static
void InitLocalRGBTxt()
{
   char *c_ptr;

   useLocalRGBTxt = FALSE;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"LocalRGBTxt")) != NULL) {
      char *fname=UtilStrDup(c_ptr);
      FILE *fp;

      if (fname == NULL) { FailAllocMessage(); return; }
      if ((fp=fopen(fname, "r")) != NULL) {
         int num_lines=0, ok=TRUE, line_num=0, info_index=0;
         char *buf;

         while (fgets(gszMsgBox, sizeof(gszMsgBox), fp) != NULL) {
            int len=strlen(gszMsgBox);

            if (len > 0 && gszMsgBox[len-1] != '\n') {
               while (fgets(gszMsgBox, sizeof(gszMsgBox), fp) != NULL) {
                  len = strlen(gszMsgBox);
                  if (len > 0 && gszMsgBox[len-1] == '\n') break;
               }
            }
            num_lines++;
         }
         rewind(fp);
         if ((privateColorInfo=(struct LocalColorRec *)malloc(
               (num_lines+1)*sizeof(struct LocalColorRec))) == NULL) {
            FailAllocMessage();
            ok = FALSE;
         }
         while ((buf=UtilGetALine(fp)) != NULL) {
            char *red_str, *green_str, *blue_str, *color_name;

            line_num++;
            if ((red_str=strtok(buf, " \t\n\r")) != NULL &&
                  (green_str=strtok(NULL, " \t\n\r")) != NULL &&
                  (blue_str=strtok(NULL, " \t\n\r")) != NULL &&
                  (color_name=strtok(NULL, "\t\n\r")) != NULL) {
               struct LocalColorRec *color_info_ptr;
               int red, green, blue;

               color_info_ptr = (&privateColorInfo[info_index]);
               while (*color_name == ' ' || *color_name == '\t') color_name++;
               if (sscanf(red_str, "%d", &red) == 1 &&
                     sscanf(green_str, "%d", &green) == 1 &&
                     sscanf(blue_str, "%d", &blue) == 1 &&
                     red >= 0 && red <= 0xff && green >= 0 && green <= 0xff &&
                     blue >= 0 && blue <= 0xff && *color_name != '\0') {
                  color_info_ptr->name = UtilStrDup(color_name);
                  color_info_ptr->len = strlen(color_info_ptr->name);
                  sprintf(gszMsgBox, "#%02x%02x%02x", red, green, blue);
                  color_info_ptr->spec = UtilStrDup(gszMsgBox);
                  info_index++;
               } else {
                  fprintf(stderr, "Malformed line %1d in '%s'.\n", line_num,
                        fname);
               }
            } else {
               fprintf(stderr, "Malformed line %1d in '%s'.\n", line_num,
                     fname);
            }
            privateColorInfo[info_index].name = NULL;
            privateColorInfo[info_index].len = 0;
            free(buf);
         }
         fclose(fp);
         if (ok) useLocalRGBTxt = TRUE;
      } else {
         fprintf(stderr, "Invalid file '%s' specified by X default %s*%s.\n",
               fname, TOOL_NAME, "LocalRGBTxt");
      }
      free(fname);
   }
   printUsingRequestedColor = FALSE;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"PrintUsingRequestedColor")) !=
         NULL && (strcmp(c_ptr, "true") == 0 || strcmp(c_ptr, "True") == 0)) {
      printUsingRequestedColor = TRUE;
   }
}

void InitColor()
{
   int i, index, looking_for_default_color, tmp_max;
   XColor color;
   char buf[80], * c_ptr, fg_color[80], bg_color[80], brdr_color[80];
   long bg_gray=(long)0;
   int bg_allocated=FALSE, fg_allocated=FALSE, brdr_allocated=FALSE;

   InitLocalRGBTxt();

   strcpy(fg_color, "Black");
   strcpy(bg_color, "White");
   strcpy(brdr_color, "Black");
   if (!initColorDontReload) {
      reverseVideo = FALSE;
      if (cmdLineRV != INVALID) {
         reverseVideo = cmdLineRV;
      } else if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"ReverseVideo")) !=
            NULL) {
         reverseVideo = FALSE;
         if (UtilStrICmp(c_ptr, "on") == 0 || UtilStrICmp(c_ptr, "true") == 0) {
            reverseVideo = TRUE;
         }
      }
      colorDump = FALSE;
      if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"InitialPrintInColor")) !=
            NULL) {
         if (UtilStrICmp(c_ptr, "true") == 0) {
            colorDump = TRUE;
         }
      }
   }
   defaultColorIndexInXDefaults = FALSE;
   defaultColorIndex = 9;
   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "DefaultColorIndex"))
         != NULL) {
      defaultColorIndexInXDefaults = TRUE;
      defaultColorIndex = atoi(c_ptr);
   }
   maxColors = MAXCOLORS;
   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "MaxColors")) != NULL) {
      maxColors = atoi(c_ptr);
      if (maxColors <= 0) {
         fprintf(stderr, "Invalid %s*MaxColors: '%s', %1d is used.\n",
               TOOL_NAME, c_ptr, MAXCOLORS);
         maxColors = MAXCOLORS;
      }
   }

   if (colorDisplay)
   {
      if (((cmdLineForeground != NULL && cmdLineBackground == NULL) ||
            (cmdLineForeground == NULL && cmdLineBackground != NULL)) &&
            reverseVideo)
      {
         fprintf (stderr, "Normal video mode assumed since %s is %s.\n",
               cmdLineForeground == NULL ? "-bg" : "-fg",
               "specified in the command line");
         reverseVideo = FALSE;
      }
      if (cmdLineForeground != NULL)
         strcpy (fg_color, cmdLineForeground);
      else if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"Foreground")) != NULL)
      {
         if (reverseVideo)
            strcpy (bg_color, c_ptr);
         else
            strcpy (fg_color, c_ptr);
      }
      else if (reverseVideo)
         strcpy (fg_color, "white");
      else
         strcpy (fg_color, "black");

      if (cmdLineBackground != NULL)
         strcpy (bg_color, cmdLineBackground);
      else if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"Background")) != NULL)
      {
         if (reverseVideo)
            strcpy (fg_color, c_ptr);
         else
            strcpy (bg_color, c_ptr);
      }
      else if (reverseVideo)
         strcpy (bg_color, "black");
      else
         strcpy (bg_color, "white");

      if (cmdLineBorder != NULL)
         strcpy (brdr_color, cmdLineBorder);
      else if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"BorderColor")) != NULL)
         strcpy (brdr_color, c_ptr);
      else
         strcpy (brdr_color, fg_color);
   }
   else
   {
      if (reverseVideo)
      {
         strcpy (fg_color, "white");
         strcpy (bg_color, "black");
      }
      else
      {
         strcpy (fg_color, "black");
         strcpy (bg_color, "white");
      }
      strcpy (brdr_color, fg_color);
   }

   if (maxColors <= 0)
   {
      fprintf (stderr, "0 colors specified (must be at least 1).\n");
      exit (-1);
   }

   colorPixels = (int*)malloc(maxColors*sizeof(int));
   xorColorPixels = (int*)malloc(maxColors*sizeof(int));
   colorLayerOn = (int*)malloc(maxColors*sizeof(int));
   colorMenuItems = (char**)malloc(maxColors*sizeof(char*));
   if (colorPixels == NULL || xorColorPixels == NULL || colorLayerOn == NULL ||
         colorMenuItems == NULL) {
      FailAllocMessage();
   }

   tgifColors = (XColor*)malloc(maxColors*sizeof(XColor));
   tgifRequestedColors = (XColor*)malloc(maxColors*sizeof(XColor));
   if (tgifColors == NULL || tgifRequestedColors == NULL) FailAllocMessage();

   allocatedMaxColors = maxColors;
   for (i = 0; i < allocatedMaxColors; i++) {
      colorMenuItems[i] = (char*)malloc(COLORSTRLEN*sizeof(char));
      if (colorMenuItems[i] == NULL) FailAllocMessage();
   }

   for (i = 0; i < maxColors; i++) {
      sprintf (buf, "Color%1d", i);
      if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME, buf)) != NULL) {
         strcpy(colorMenuItems[i], c_ptr);
      } else if (i < MAXCOLORS) {
         strcpy(colorMenuItems[i], defaultColorMenuItems[i]);
      } else {
         fprintf(stderr, "Could not GetDefault %s*%s\n", TOOL_NAME, buf);
         exit(-1);
      }
      colorLayerOn[i] = TRUE;
   }

   if (colorDisplay)
   {
      index = 0;
      for (i = 0; i < maxColors; i++)
      {
         unsigned short	red_req, green_req, blue_req;

         if (!ParseAndAllocColorByName (colorMenuItems[i], &color,
               &red_req, &green_req, &blue_req))
         {
            fprintf (stderr, "%s %1d:  '%s'!  %s aborted!\n",
                  "Fail to allocate color number", i, colorMenuItems[i],
                  TOOL_NAME);
            exit (-1);
         }

         if (i != index) strcpy (colorMenuItems[index], colorMenuItems[i]);

         colorPixels[index] = color.pixel;

         tgifColors[index].red = color.red;
         tgifColors[index].green = color.green;
         tgifColors[index].blue = color.blue;

         tgifRequestedColors[index].red = red_req;
         tgifRequestedColors[index].green = green_req;
         tgifRequestedColors[index].blue = blue_req;

         if (UtilStrICmp (colorMenuItems[i], fg_color) == 0)
         {
            myFgPixel = color.pixel;
            strcpy (myFgColorStr, fg_color);

            fg_allocated = TRUE;
         }
         if (UtilStrICmp (colorMenuItems[i], bg_color) == 0)
         {
            myBgPixel = color.pixel;
            myBgColor.pixel = color.pixel;
            myBgColor.red = color.red;
            myBgColor.green = color.green;
            myBgColor.blue = color.blue;
            strcpy (myBgColorStr, bg_color);

            bg_allocated = TRUE;
            bg_gray = (((long)tgifColors[i].red)<<2) +
                  (((long)tgifColors[i].green)<<3) + ((long)tgifColors[i].blue);
         }
         if (UtilStrICmp (colorMenuItems[i], brdr_color) == 0)
         {
            myBorderPixel = color.pixel;
            brdr_allocated = TRUE;
         }
         index++;
      }
      maxColors = index;
      if (maxColors <= 0)
      {
         fprintf (stderr, "0 colors specified (must be at least 1).\n");
         exit (-1);
      }
      looking_for_default_color = FALSE;
      if (defaultColorIndexInXDefaults)
      {
         if (defaultColorIndexInXDefaults && defaultColorIndex >= maxColors)
         {
            fprintf (stderr, "Warning:  DefaultColorIndex >= MaxColors, ");
            fprintf (stderr, "Use 0 for DefaultColorIndex\n");
            defaultColorIndex = 0;
         }
      }
      else
      {
         looking_for_default_color = TRUE;
         for (i = 0; i < maxColors; i++)
         {
            if (MyStrCmp (fg_color, colorMenuItems[i]) == 0)
            {
               defaultColorIndex = i;
               looking_for_default_color = FALSE;
               break;
            }
         }
      }
      if (!fg_allocated)
      {
         if (!ParseAndAllocColorByName (fg_color, &color, NULL, NULL, NULL))
         {
            fprintf (stderr, "Fail to allocate the '%s' color!  Abort!\n",
                  fg_color);
            exit (-1);
         }
         myFgPixel = color.pixel;
         strcpy (myFgColorStr, fg_color);
      }
      if (!bg_allocated)
      {
         if (!ParseAndAllocColorByName (bg_color, &color, NULL, NULL, NULL))
         {
            fprintf (stderr, "Fail to allocate the '%s' color!  Abort!\n",
                  bg_color);
            exit (-1);
         }
         myBgPixel = color.pixel;
         myBgColor.pixel = color.pixel;
         myBgColor.red = color.red;
         myBgColor.green = color.green;
         myBgColor.blue = color.blue;
         strcpy (myBgColorStr, bg_color);
         if (looking_for_default_color)
            bg_gray = (((long)color.red)<<2) + (((long)color.green)<<3) +
                  ((long)color.blue);
      }
      if (looking_for_default_color)
      {
         long	val, val1;

         defaultColorIndex = 0;
         val = (((long)tgifColors[0].red)<<2) +
               (((long)tgifColors[0].green)<<3) + ((long)tgifColors[0].blue);
         if (bg_gray >= 0x67ff9) /* (0xffff<<1) + (0xffff<<2) + (0x7fff) */
         {
            for (i = 1; i < maxColors; i++)
            {
               val1 = (((long)tgifColors[i].red)<<2) +
                     (((long)tgifColors[i].green)<<3) +
                     ((long)tgifColors[i].blue);
               if (val > val1)
               {
                  val = val1;
                  defaultColorIndex = i;
               }
            }
         }
         else
         {
            for (i = 1; i < maxColors; i++)
            {
               val1 = (((long)tgifColors[i].red)<<2) +
                     (((long)tgifColors[i].green)<<3) +
                     ((long)tgifColors[i].blue);
               if (val < val1)
               {
                  val = val1;
                  defaultColorIndex = i;
               }
            }
         }
      }
      if (!brdr_allocated)
      {
         if (!ParseAndAllocColorByName (brdr_color, &color, NULL, NULL, NULL))
         {
            fprintf (stderr, "Fail to allocate the '%s' color!  Abort!\n",
                  brdr_color);
            exit (-1);
         }
         myBorderPixel = color.pixel;
      }
      for (i = 0; i < maxColors; i++)
         xorColorPixels[i] = colorPixels[i] ^ myBgPixel;
      colorIndex = defaultColorIndex;
   }
   else
   {  /* !colorDisplay */
      if (!ParseAndAllocColorByName (fg_color, &color, NULL, NULL, NULL))
      {
         fprintf (stderr, "Fail to allocate the '%s' color!  Abort!\n",
               fg_color);
         exit (-1);
      }
      myFgPixel = color.pixel;
      strcpy (myFgColorStr, fg_color);

      if (!ParseAndAllocColorByName (bg_color, &color, NULL, NULL, NULL))
      {
         fprintf (stderr, "Fail to allocate the '%s' color!  Abort!\n",
               bg_color);
         exit (-1);
      }
      myBgPixel = color.pixel;
      strcpy (myBgColorStr, bg_color);

      if (!ParseAndAllocColorByName (brdr_color, &color, NULL, NULL, NULL))
      {
         fprintf (stderr, "Fail to allocate the '%s' color!  Abort!\n",
               brdr_color);
         exit (-1);
      }
      myBorderPixel = color.pixel;

      for (i = 0; i < maxColors; i++)
      {
         unsigned short	red_req, green_req, blue_req;

         if (!ParseAndAllocColorByName (colorMenuItems[i], &color,
               &red_req, &green_req, &blue_req))
         {
            fprintf (stderr, "%s %1d:  '%s'!  %s aborted!\n",
                  "Fail to allocate color number", i, colorMenuItems[i],
                  TOOL_NAME);
            exit (-1);
         }

         tgifColors[i].red = color.red;
         tgifColors[i].green = color.green;
         tgifColors[i].blue = color.blue;

         tgifRequestedColors[i].red = red_req;
         tgifRequestedColors[i].green = green_req;
         tgifRequestedColors[i].blue = blue_req;

         if (UtilStrICmp (colorMenuItems[i], bg_color) == 0)
            colorPixels[i] = myBgPixel;
         else
            colorPixels[i] = myFgPixel;
         xorColorPixels[i] = myFgPixel ^ myBgPixel;
      }
      colorIndex = defaultColorIndex;
   }
   if (!ParseAndAllocColorByName ("black", &color, NULL, NULL, NULL))
   {
      fprintf (stderr, "Fail to allocate the 'black' color!  Abort!\n");
      exit (-1);
   }
   tmp_max = max(((int)color.red),max(((int)color.green),((int)color.blue)));
#ifndef DONTFREECOLORS
   XFreeColors (mainDisplay, mainColormap, &(color.pixel), 1, 0);
#endif
   if (tmp_max > maxRGB) maxRGB = tmp_max;
   if (!ParseAndAllocColorByName ("white", &color, NULL, NULL, NULL))
   {
      fprintf (stderr, "Fail to allocate the 'white' color!  Abort!\n");
      exit (-1);
   }
   tmp_max = max(((int)color.red),max(((int)color.green),((int)color.blue)));
#ifndef DONTFREECOLORS
   XFreeColors (mainDisplay, mainColormap, &(color.pixel), 1, 0);
#endif
   if (tmp_max > maxRGB) maxRGB = tmp_max;
   if (tmp_max == 0)
      fprintf (stderr, "Warning:  Unexpected maximum RGB intensity of 0.");

   if ((c_ptr = XGetDefault (mainDisplay,TOOL_NAME,"RubberBandColor"))!=NULL &&
            ParseAndAllocColorByName (c_ptr, &color, NULL, NULL, NULL))
      xorOne = color.pixel^myBgPixel;
   else
      xorOne = myFgPixel^myBgPixel;

   xorZero = myBgPixel;
   if (myFgPixel == myBgPixel)
      fprintf (stderr, "%s.\n%s.\n",
            "Warning: foreground and background colors are the same",
            "         You may not be able to see much");

   if (!initColorDontReload) {
      canChangeAttrColor = FALSE;
      if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"CanChangeAttrColor")) !=
            NULL) {
         if (UtilStrICmp(c_ptr, "true") == 0) {
            canChangeAttrColor = TRUE;
         }
      }
      colorLayers = FALSE;
      if (colorDisplay && (c_ptr=XGetDefault(mainDisplay, TOOL_NAME,
            "ColorLayers")) != NULL) {
         if (UtilStrICmp(c_ptr, "true") == 0) {
            colorLayers = TRUE;
         }
      }
   }
}

int OneColorObject (ObjPtr, ColorIndex)
   struct ObjRec	* ObjPtr;
   int			* ColorIndex;
{
   register struct ObjRec	* obj_ptr;

   for (obj_ptr=ObjPtr->detail.r->last; obj_ptr!=NULL; obj_ptr=obj_ptr->prev)
      switch (obj_ptr->type)
      {
         case OBJ_POLY:
            if (*ColorIndex == INVALID)
            {
               if ((obj_ptr->detail.p->fill != NONEPAT &&
                     obj_ptr->detail.p->fill != BACKPAT) ||
                     (obj_ptr->detail.p->pen != NONEPAT &&
                     obj_ptr->detail.p->pen != BACKPAT))
                  *ColorIndex = obj_ptr->color;
            }
            else if (obj_ptr->color != *ColorIndex)
            {
               if ((obj_ptr->detail.p->fill != NONEPAT &&
                     obj_ptr->detail.p->fill != BACKPAT) ||
                     (obj_ptr->detail.p->pen != NONEPAT &&
                     obj_ptr->detail.p->pen != BACKPAT))
                  return (FALSE);
            }
            break;
         case OBJ_BOX:
            if (*ColorIndex == INVALID)
            {
               if ((obj_ptr->detail.b->fill != NONEPAT &&
                     obj_ptr->detail.b->fill != BACKPAT) ||
                     (obj_ptr->detail.b->pen != NONEPAT &&
                     obj_ptr->detail.b->pen != BACKPAT))
                  *ColorIndex = obj_ptr->color;
            }
            else if (obj_ptr->color != *ColorIndex)
            {
               if ((obj_ptr->detail.b->fill != NONEPAT &&
                     obj_ptr->detail.b->fill != BACKPAT) ||
                     (obj_ptr->detail.b->pen != NONEPAT &&
                     obj_ptr->detail.b->pen != BACKPAT))
                  return (FALSE);
            }
            break;
         case OBJ_OVAL:
            if (*ColorIndex == INVALID)
            {
               if ((obj_ptr->detail.o->fill != NONEPAT &&
                     obj_ptr->detail.o->fill != BACKPAT) ||
                     (obj_ptr->detail.o->pen != NONEPAT &&
                     obj_ptr->detail.o->pen != BACKPAT))
                  *ColorIndex = obj_ptr->color;
            }
            else if (obj_ptr->color != *ColorIndex)
            {
               if ((obj_ptr->detail.o->fill != NONEPAT &&
                     obj_ptr->detail.o->fill != BACKPAT) ||
                     (obj_ptr->detail.o->pen != NONEPAT &&
                     obj_ptr->detail.o->pen != BACKPAT))
                  return (FALSE);
            }
            break;
         case OBJ_TEXT:
            if (*ColorIndex == INVALID)
            {
               if ((obj_ptr->detail.t->fill != NONEPAT &&
                     obj_ptr->detail.t->fill != BACKPAT) ||
                     (obj_ptr->detail.t->pen != NONEPAT &&
                     obj_ptr->detail.t->pen != BACKPAT))
                  *ColorIndex = obj_ptr->color;
            }
            else if (obj_ptr->color != *ColorIndex)
            {
               if ((obj_ptr->detail.t->fill != NONEPAT &&
                     obj_ptr->detail.t->fill != BACKPAT) ||
                     (obj_ptr->detail.t->pen != NONEPAT &&
                     obj_ptr->detail.t->pen != BACKPAT))
                  return (FALSE);
            }
            break;
         case OBJ_POLYGON:
            if (*ColorIndex == INVALID)
            {
               if ((obj_ptr->detail.g->fill != NONEPAT &&
                     obj_ptr->detail.g->fill != BACKPAT) ||
                     (obj_ptr->detail.g->pen != NONEPAT &&
                     obj_ptr->detail.g->pen != BACKPAT))
                  *ColorIndex = obj_ptr->color;
            }
            else if (obj_ptr->color != *ColorIndex)
            {
               if ((obj_ptr->detail.g->fill != NONEPAT &&
                     obj_ptr->detail.g->fill != BACKPAT) ||
                     (obj_ptr->detail.g->pen != NONEPAT &&
                     obj_ptr->detail.g->pen != BACKPAT))
                  return (FALSE);
            }
            break;
         case OBJ_ARC:
            if (*ColorIndex == INVALID)
            {
               if ((obj_ptr->detail.a->fill != NONEPAT &&
                     obj_ptr->detail.a->fill != BACKPAT) ||
                     (obj_ptr->detail.a->pen != NONEPAT &&
                     obj_ptr->detail.a->pen != BACKPAT))
                  *ColorIndex = obj_ptr->color;
            }
            else if (obj_ptr->color != *ColorIndex)
            {
               if ((obj_ptr->detail.a->fill != NONEPAT &&
                     obj_ptr->detail.a->fill != BACKPAT) ||
                     (obj_ptr->detail.a->pen != NONEPAT &&
                     obj_ptr->detail.a->pen != BACKPAT))
                  return (FALSE);
            }
            break;
         case OBJ_RCBOX:
            if (*ColorIndex == INVALID)
            {
               if ((obj_ptr->detail.rcb->fill != NONEPAT &&
                     obj_ptr->detail.rcb->fill != BACKPAT) ||
                     (obj_ptr->detail.rcb->pen != NONEPAT &&
                     obj_ptr->detail.rcb->pen != BACKPAT))
                  *ColorIndex = obj_ptr->color;
            }
            else if (obj_ptr->color != *ColorIndex)
            {
               if ((obj_ptr->detail.rcb->fill != NONEPAT &&
                     obj_ptr->detail.rcb->fill != BACKPAT) ||
                     (obj_ptr->detail.rcb->pen != NONEPAT &&
                     obj_ptr->detail.rcb->pen != BACKPAT))
                  return (FALSE);
            }
            break;
         case OBJ_XBM:
            if (*ColorIndex == INVALID)
            {
               if (obj_ptr->detail.xbm->fill != NONEPAT &&
                     obj_ptr->detail.xbm->fill != BACKPAT)
                  *ColorIndex = obj_ptr->color;
            }
            else if (obj_ptr->color != *ColorIndex)
            {
               if (obj_ptr->detail.xbm->fill != NONEPAT &&
                     obj_ptr->detail.xbm->fill != BACKPAT)
                  return (FALSE);
            }
            break;

         case OBJ_XPM:
            if (*ColorIndex == INVALID)
            {
               *ColorIndex = obj_ptr->color;
            }
            else if (obj_ptr->color != *ColorIndex)
            {
               return (FALSE);
            }
            break;

         case OBJ_GROUP:
         case OBJ_SYM:
         case OBJ_ICON:
            if (!OneColorObject (obj_ptr, ColorIndex))
               return (FALSE);
            break;

      }
   return (TRUE);
}

int ChangeObjColor(ObjPtr, ColorIndex)
   struct ObjRec *ObjPtr;
   int ColorIndex;
{
   register struct ObjRec *obj_ptr;
   register struct AttrRec *attr_ptr;
   int changed=FALSE, icon_color_index;

   switch (ObjPtr->type) {
   case OBJ_POLY:
   case OBJ_BOX:
   case OBJ_OVAL:
   case OBJ_TEXT:
   case OBJ_POLYGON:
   case OBJ_ARC:
   case OBJ_RCBOX:
   case OBJ_XBM:
   case OBJ_XPM:
      if (ObjPtr->color != ColorIndex) {
         ObjPtr->color = ColorIndex;
         if (ObjPtr->type == OBJ_XPM) {
            /* RecolorXPmObj(ObjPtr, ColorIndex); */
         }
         changed = TRUE;
      }
      break;

   case OBJ_GROUP:
   case OBJ_SYM:
      for (obj_ptr = ObjPtr->detail.r->last; obj_ptr != NULL;
            obj_ptr = obj_ptr->prev) {
         if (ChangeObjColor(obj_ptr, ColorIndex)) {
            changed = TRUE;
         }
      }
      break;

   case OBJ_ICON:
      icon_color_index = INVALID;
      if (OneColorObject(ObjPtr, &icon_color_index) &&
            icon_color_index != ColorIndex) {
         for (obj_ptr = ObjPtr->detail.r->last; obj_ptr != NULL;
               obj_ptr = obj_ptr->prev) {
            if (ChangeObjColor(obj_ptr, ColorIndex)) {
               changed = TRUE;
            }
         }
      }
      break;
   }
   if (canChangeAttrColor) {
      for (attr_ptr=ObjPtr->fattr; attr_ptr != NULL; attr_ptr=attr_ptr->next) {
         if (attr_ptr->obj->color != ColorIndex) {
            attr_ptr->obj->color = ColorIndex;
            changed = TRUE;
         }
      }
   }
   return changed;
}

void ChangeAllSelColor(ColorIndex, HighLight)
   int ColorIndex, HighLight;
{
   register struct SelRec *sel_ptr;
   int changed=FALSE, dont_do_obj=FALSE;
   int saved_sticky_menu_selection=stickyMenuSelection;
   int dont_check_text=FALSE;
   XGCValues values;

   if (topSel != NULL && topSel == botSel && topSel->obj->type == OBJ_XPM) {
      /* if the only selected object is an OBJ_XPM, chang the current color */
      stickyMenuSelection = TRUE;
      if (gnInImageProc) dont_check_text = TRUE;
      dont_do_obj = TRUE;
   }
   if (topSel == NULL || stickyMenuSelection) {
      int text_obj_created=FALSE, text_cursor_shown=FALSE;

      if (!dont_check_text) {
         if (!(curChoice == DRAWTEXT && textCursorShown)) {
            text_cursor_shown = textCursorShown;
            text_obj_created = TieLooseEnds();
         }
      }
      colorIndex = ColorIndex;
      if (colorLayers && !colorLayerOn[colorIndex]) {
         if (topSel == NULL) {
            sprintf(gszMsgBox,
                  "Invisible color %1d (%s) is selected for drawing.",
                  colorIndex, colorMenuItems[colorIndex]);
            Msg(gszMsgBox);
         }
      }
      ShowColor(TRUE);
      if (!PRTGIF && colorLayers && needToRedrawColorWindow) {
         RedrawColorWindow();
      }
      if (!dont_check_text) {
         if (curChoice == DRAWTEXT && textCursorShown) {
            text_cursor_shown = textCursorShown;
            if (curTextObj->color != colorIndex) {
               curTextObj->color = colorIndex;
               curTextModified = TRUE;
               RedrawCurText();
               if (cycleThroughChoice) {
                  SetPushedFontValue(PUSH_COLORINDEX, colorIndex);
               }
            }
         } else {
            textCursorShown = FALSE;
         }
      }
      if (topSel == NULL) dont_do_obj = TRUE;
   }
   stickyMenuSelection = saved_sticky_menu_selection;
   sprintf(gszMsgBox, "Color set to '%s'.", colorMenuItems[ColorIndex]);
   Msg(gszMsgBox);
   if (dont_do_obj) return;

   values.foreground = colorPixels[ColorIndex];
   values.function = GXcopy;
   values.fill_style = FillSolid;
   XChangeGC(mainDisplay, drawGC,
         GCForeground | GCFunction | GCFillStyle, &values);

   if (HighLight) HighLightReverse();
   StartCompositeCmd();
   for (sel_ptr=botSel; sel_ptr != NULL; sel_ptr=sel_ptr->prev) {
      PrepareToReplaceAnObj(sel_ptr->obj);
      if (ChangeObjColor(sel_ptr->obj, ColorIndex)) {
         changed = TRUE;
         RecordReplaceAnObj(sel_ptr->obj);
      } else {
         AbortPrepareCmd(CMD_REPLACE);
      }
   }
   EndCompositeCmd();

   if (changed) {
      int need_to_update_sel_bbox=FALSE;

      if (colorLayers) {
         struct SelRec *next_sel;

         for (sel_ptr=topSel; sel_ptr != NULL; sel_ptr=next_sel) {
            struct ObjRec *obj_ptr=sel_ptr->obj;

            next_sel = sel_ptr->next;
            obj_ptr->tmp_parent = NULL;
            if (!ObjInVisibleLayer(obj_ptr)) {
               need_to_update_sel_bbox = TRUE;
               if (sel_ptr->prev == NULL) {
                  topSel = sel_ptr->next;
               } else {
                  sel_ptr->prev->next = sel_ptr->next;
               }
               if (sel_ptr->next == NULL) {
                  botSel = sel_ptr->prev;
               } else {
                  sel_ptr->next->prev = sel_ptr->prev;
               }
               free(sel_ptr);
            }
         }
      }
      SetFileModified(TRUE);
      RedrawAnArea(botObj, selLtX-GRID_ABS_SIZE(1),
            selLtY-GRID_ABS_SIZE(1), selRbX+GRID_ABS_SIZE(1),
            selRbY+GRID_ABS_SIZE(1));
      if (need_to_update_sel_bbox) UpdSelBBox();
   }
   if (HighLight) HighLightForward();
}

void SetUpColorMenuPixmap (fore_colors, init_rv, pixmap, rows, cols)
   int		* * fore_colors, * * init_rv, * rows, * cols;
   Pixmap	* * pixmap;
{
   register int	i;

   *pixmap = (Pixmap*)malloc(maxColors*sizeof(Pixmap));
   *fore_colors = (int*)malloc(maxColors*sizeof(int));
   *init_rv = (int*)malloc(maxColors*sizeof(int));
   if (*pixmap == NULL || *fore_colors == NULL || *init_rv == NULL) {
      FailAllocMessage();
   }
   for (i = 0; i < maxColors; i++) {
      (*pixmap)[i] = patPixmap[SOLIDPAT];
      (*fore_colors)[i] = colorPixels[i];
      (*init_rv)[i] = FALSE;
   }
   *cols = ((maxColors % 10)==0) ? (int)(maxColors/10) : (int)(maxColors/10)+1;
   *rows = (maxColors <= 10) ? maxColors : 10;
}

int ColorMenu (X, Y, TrackMenubar)
   int	X, Y, TrackMenubar;
{
   register int	i;
   Pixmap	* pixmap;
   int		index, * fore_colors, * init_rv, rows, cols;
   char		* * desc;

   if (!colorDisplay) return (INVALID);

   desc = (char**)malloc((maxColors+1)*sizeof(char*));
   if (desc == NULL) FailAllocMessage();
   for (i=0; i < maxColors; i++) {
      desc[i] = (char*)malloc(80*sizeof(char));
      if (desc[i] == NULL) FailAllocMessage();
      sprintf(desc[i], "Set color to '%s'", colorMenuItems[i]);
   }
   desc[i] = NULL;
   SetUpColorMenuPixmap (&fore_colors, &init_rv, &pixmap, &rows, &cols);
   activeMenu = MENU_COLOR;
   index = PxMpMenuLoop (X, Y, choiceImageW, choiceImageH, rows, cols,
         maxColors, fore_colors, pixmap, init_rv, desc, MULTICOLOR,
         TrackMenubar);
   free(pixmap);

   if (index >= 0) ChangeAllSelColor (index, TRUE);
   if (desc != NULL)
   {
      for (i=0; i < maxColors; i++) {
         if (desc[i] != NULL) {
            free(desc[i]);
         }
      }
      free(desc);
   }
   return (index);
}

void CleanUpColors ()
{
   register int	i;

   if (privateColorInfo != NULL) {
      struct LocalColorRec *ptr;

      for (ptr=privateColorInfo; ptr->name != NULL; ptr++) {
         free(ptr->name);
         if (ptr->spec != NULL) free(ptr->spec);
      }
      free(privateColorInfo);
      privateColorInfo = NULL;
   }
   if (colorLayerOn != NULL) free(colorLayerOn);
   free(colorPixels);
   free(xorColorPixels);
   colorLayerOn = colorPixels = xorColorPixels = NULL;
   free(tgifColors);
   free(tgifRequestedColors);
   tgifColors = tgifRequestedColors = NULL;
   for (i = 0; i < maxColors; i++) free(colorMenuItems[i]);
   free(colorMenuItems);
   colorMenuItems = NULL;

   maxColors = MAXCOLORS;
   defaultColorIndex = 9;
   colorIndex = 0;
}

static
int DrawAVerticalTab(color_index, x, y, skip)
   int color_index, x, y, skip;
{
   char s[20];
   XPoint v[5];
   int offset;

   if (skip) return colorItemH;

   offset = ((colorItemH-7)>>1);
   v[0].x = v[4].x = x;   v[0].y = v[4].y = y;
   v[1].x = x+scrollBarW; v[1].y = y;
   v[2].x = x+scrollBarW; v[2].y = y+colorItemH;
   v[3].x = x;            v[3].y = y+colorItemH;

   XSetForeground(mainDisplay, defaultGC, colorPixels[color_index]);
   XFillPolygon(mainDisplay, colorWindow, defaultGC, v, 5, Convex,
         CoordModeOrigin);
   XSetForeground(mainDisplay, defaultGC, myBgPixel);
   XFillRectangle(mainDisplay, colorWindow, defaultGC, x+offset, y+offset,
         7, 7);
   XSetForeground(mainDisplay, defaultGC, myFgPixel);
   XDrawRectangle(mainDisplay, colorWindow, defaultGC, x+offset, y+offset,
         7, 7);
   if (colorLayerOn[color_index]) {
      XDrawLine(mainDisplay, colorWindow, defaultGC, x+offset, y+offset,
            x+offset+7, y+offset+7);
      XDrawLine(mainDisplay, colorWindow, defaultGC, x+offset,
            y+offset+7, x+offset+7, y+offset);
   }
   XDrawRectangle(mainDisplay, colorWindow, defaultGC, x, y,
         scrollBarW, colorItemH);
   return colorItemH;
}

void RedrawColorWindow()
{
   int i, x=0, y;
   char s[20];

   needToRedrawColorWindow = FALSE;
   if (!colorLayers || colorWindow == None) return;

   XClearArea(mainDisplay, colorWindow, 0, 0, scrollBarW, colorWindowH, FALSE);

   y = (scrollBarW*MAX_VERTICAL_BTNS);
   for (i=0; i < maxColors; i++) {
      if (i >= colorWindowFirstIndex) {
         int h=0;

         h = DrawAVerticalTab(i, x, y, FALSE);
         y += h;
      }
   }
   for (i=0, y=0; i < MAX_VERTICAL_BTNS; i++, y += scrollBarW) {
      XSetTSOrigin(mainDisplay, rasterGC, 0, y);
      XSetStipple(mainDisplay, rasterGC, scrollPixmap[i+4]);
      XFillRectangle(mainDisplay, colorWindow, rasterGC,
            0, y, scrollBarW, scrollBarW);
   }
   XSetTSOrigin(mainDisplay, rasterGC, 0, 0);
}

static
void SetAllColorLayersState(on_state)
   int on_state;
{
   int i, changed=FALSE;

   for (i=0; i < maxColors; i++) {
      if (colorLayerOn[i] != on_state) {
         changed = TRUE;
         colorLayerOn[i] = on_state;
      }
   }
   sprintf(gszMsgBox, "All color layers are no %s.",
         on_state ? "visible" : "invisible");
   Msg(gszMsgBox);
   if (changed) {
      MakeQuiescent();
      RedrawColorWindow();
      ClearAndRedrawDrawWindow();
   }
}

static
void HandleClickInColorWindow(button_ev)
   XButtonEvent *button_ev;
{
   int i, index, total, x, offset;

   index = (int)((button_ev->y-1) / scrollBarW);
   if (index < 0) index = 0;

   switch (index+4) {
   case SCROLL_UPEND:
   case SCROLL_UP:
   case SCROLL_CHECKALL:
   case SCROLL_UNCHECKALL:
   case SCROLL_DOWN:
   case SCROLL_DOWNEND:
      if (button_ev->button != Button1) return;
      break;
   default: break;
   }

   switch (index+4) {
   case SCROLL_UPEND:
      colorWindowFirstIndex = 0;
      RedrawColorWindow();
      break;
   case SCROLL_UP:
      if (colorWindowFirstIndex > 0) {
         colorWindowFirstIndex--;
         RedrawColorWindow();
      }
      break;
   case SCROLL_CHECKALL: SetAllColorLayersState(TRUE); break;
   case SCROLL_UNCHECKALL: SetAllColorLayersState(FALSE); break;
   case SCROLL_DOWN:
      if (colorWindowFirstIndex < maxColors-1) {
         colorWindowFirstIndex++;
         RedrawColorWindow();
      }
      break;
   case SCROLL_DOWNEND:
      total = colorWindowH-(scrollBarW*MAX_VERTICAL_BTNS);
      colorWindowFirstIndex = maxColors-1;
      for (i=maxColors-1; i >= 0; i--) {
         int w=DrawAVerticalTab(i, 0, 0, TRUE);

         total -= w;
         if (total > 0) {
            colorWindowFirstIndex = i;
         } else {
            break;
         }
      }
      RedrawColorWindow();
      break;
   default:
      offset = button_ev->y-(scrollBarW*MAX_VERTICAL_BTNS);
      for (i=0; i < maxColors; i++) {
         if (i >= colorWindowFirstIndex) {
            int h=DrawAVerticalTab(i, 0, 0, TRUE);

            if (h >= offset) {
               if (button_ev->button == Button1) {
                  colorLayerOn[i] = !colorLayerOn[i];
                  sprintf(gszMsgBox, "Color '%s' (layer %1d) is turned %s",
                        colorMenuItems[i], i, colorLayerOn[i] ? "on" : "off");
                  SetStringStatus(gszMsgBox);
                  if (i == colorIndex) {
                     sprintf(gszMsgBox,
                           "Invisible color %1d (%s) is selected for drawing.",
                           colorIndex, colorMenuItems[colorIndex]);
                     Msg(gszMsgBox);
                  }
                  MakeQuiescent();
                  RedrawColorWindow();
                  ClearAndRedrawDrawWindow();
               } else if (button_ev->button == Button3) {
                  int save_sticky_menu_selection=stickyMenuSelection;

                  stickyMenuSelection = TRUE;
                  ChangeAllSelColor(i, TRUE);
                  stickyMenuSelection = save_sticky_menu_selection;
               }
               break;
            }
            offset -= h;
         }
      }
      break;
   }
}

static struct MouseStatusStrRec colorMouseStatus[] = {
   { "Shift All Tabs Down", "(none)", "(none)" },
   { "Shift Tabs Down", "(none)", "(none)" },
   { "Check All", "(none)", "(none)" },
   { "Uncheck All", "(none)", "(none)" },
   { "Shift Tabs Up", "(none)", "(none)" },
   { "Shift All Tabs Up", "(none)", "(none)" },
   { NULL, NULL, NULL }
};

static
void HandleMotionInColorWindow(motion_ev)
   XMotionEvent *motion_ev;
{
   int i, index, total, x, offset;

   index = (int)((motion_ev->y-1) / scrollBarW);
   if (index < 0) index = 0;

   switch (index+4) {
   case SCROLL_UPEND:
   case SCROLL_UP:
   case SCROLL_CHECKALL:
   case SCROLL_UNCHECKALL:
   case SCROLL_DOWN:
   case SCROLL_DOWNEND:
      SetMouseStatus(colorMouseStatus[index].l, colorMouseStatus[index].m,
            colorMouseStatus[index].r);
      break;
   default:
      offset = motion_ev->y-(scrollBarW*MAX_VERTICAL_BTNS);
      for (i=0; i < maxColors; i++) {
         if (i >= colorWindowFirstIndex) {
            int h=DrawAVerticalTab(i, 0, 0, TRUE);

            if (h >= offset) {
               char left_str[80], right_str[80];

               sprintf(left_str, "Turn color '%s' (layer %1d) %s",
                     colorMenuItems[i], i, colorLayerOn[i] ? "off" : "on");
               sprintf(right_str, "Set color to '%s'", colorMenuItems[i]);
               SetMouseStatus(left_str, "(none)", right_str);
               break;
            }
            offset -= h;
         }
      }
      break;
   }
}

void ColorEventHandler(input)
   XEvent *input;
{
   XEvent ev;

   if (!colorLayers) return;

   if (input->type == Expose) {
      while (XCheckWindowEvent(mainDisplay, colorWindow, ExposureMask, &ev)) ;
      RedrawColorWindow();
   } else if (input->type == EnterNotify) {
      SetMouseStatus("", "", "");
   } else if (input->type == MotionNotify) {
      while (XCheckWindowEvent(mainDisplay,colorWindow,PointerMotionMask,&ev)) ;
      HandleMotionInColorWindow(&input->xmotion);
   } else if (input->type == ButtonPress) {
      XButtonEvent *button_ev=(&(input->xbutton));

      if (button_ev->button == Button1 || button_ev->button == Button3) {
         HandleClickInColorWindow(button_ev);
      }
   }
}

void RedrawColorDummyWindow()
{
}

void ColorDummyEventHandler(input)
   XEvent *input;
{
   if (!colorLayers) return;

   if (input->type == Expose) {
      XEvent ev;

      while (XCheckWindowEvent(mainDisplay,colorDummyWindow,ExposureMask,&ev)) ;
      RedrawColorDummyWindow();
   } else if (input->type == EnterNotify) {
      SetMouseStatus("(none)", "(none)", "(none)");
   }
}

int UpdatePixel(ObjPtr, OldColorStr)
   struct ObjRec *ObjPtr;
   char **OldColorStr;
{
   register int c, i, r;
   int ncolors, new_alloc, index, changed, len;
   int picture_changed=FALSE;
   char msg[MAXSTRING];
   unsigned long pixel, *from_pixels, *to_pixels;
   struct ObjRec *obj_ptr;
   struct XPmRec *xpm_ptr;
   struct AttrRec *attr_ptr;

   switch (ObjPtr->type) {
   case OBJ_GROUP:
   case OBJ_SYM:
   case OBJ_ICON:
      for (obj_ptr=ObjPtr->detail.r->last; obj_ptr!=NULL;
            obj_ptr=obj_ptr->prev) {
         if (UpdatePixel(obj_ptr, OldColorStr)) {
            picture_changed = TRUE;
         }
      }
      break;
   case OBJ_XPM:
      changed = FALSE;
      xpm_ptr = ObjPtr->detail.xpm;
      ncolors = xpm_ptr->ncolors;
      from_pixels = (unsigned long *)malloc(ncolors*sizeof(unsigned long));
      to_pixels = (unsigned long *)malloc(ncolors*sizeof(unsigned long));
      if (from_pixels == NULL || to_pixels == NULL) FailAllocMessage();
      for (i = 0; i < ncolors; i++) {
         from_pixels[i] = xpm_ptr->pixels[i];
         index = QuickFindColorIndex(NULL, xpm_ptr->color_str[i],
               &new_alloc, TRUE);
         if (index == INVALID) {
            sprintf(msg, "Can not allocate color '%s', use '%s' instead.",
                  xpm_ptr->color_str[i], colorMenuItems[colorIndex]);
            Msg(msg);
            allocColorFailed = TRUE;

            len = strlen(colorMenuItems[colorIndex]);
            free(xpm_ptr->color_str[i]);
            xpm_ptr->color_str[i] = (char*)malloc((len+1)*sizeof(char));
            if (xpm_ptr->color_str[i] == NULL) FailAllocMessage();
            strcpy(xpm_ptr->color_str[i], colorMenuItems[colorIndex]);

            xpm_ptr->pixels[i] = colorPixels[colorIndex];
         } else {
            if (xpm_ptr->pixels[i] != colorPixels[index]) {
               changed = TRUE;
            }
            xpm_ptr->pixels[i] = colorPixels[index];
         }
         to_pixels[i] = xpm_ptr->pixels[i];
      }
      if (changed) {
         int image_w=xpm_ptr->image_w, image_h=xpm_ptr->image_h;
         XImage *image=xpm_ptr->image;
         Pixmap pixmap=xpm_ptr->pixmap;

         if (xpm_ptr->cached_pixmap != None) {
            XFreePixmap(mainDisplay, xpm_ptr->cached_pixmap);
         }
         xpm_ptr->cached_pixmap = None;
         if (xpm_ptr->cached_bitmap != None) {
            XFreePixmap(mainDisplay, xpm_ptr->cached_bitmap);
         }
         xpm_ptr->cached_bitmap = None;
         xpm_ptr->cached_color = (-1);
         xpm_ptr->cached_zoom = 0;
         xpm_ptr->cached_rotate = INVALID;

         sprintf(gszMsgBox, "Updating pixels (%1d - %1dx%1d)...",
               ++gnUpdatePixelObjCount, image_w, image_h);
         SetStringStatus(gszMsgBox);
         XSync(mainDisplay, False);
         for (r=0; r<image_h; r++) {
            for (c=0; c<image_w; c++) {
               pixel = XGetPixel(image, c, r);
               for (i=0; i < ncolors; i++) {
                  if (from_pixels[i] == pixel) {
                     XPutPixel(image, c, r, to_pixels[i]);
                     break;
                  }
               }
            }
         }
         XPutImage(mainDisplay,pixmap,xpmGC,image,0,0,0,0,image_w,image_h);
         picture_changed = TRUE;
      }
      free(from_pixels);
      free(to_pixels);
      break;
   default:
      index = QuickFindColorIndex(NULL, OldColorStr[ObjPtr->color],
            &new_alloc, TRUE);
      if (index != ObjPtr->color) picture_changed = TRUE;
      if (index == INVALID) {
         sprintf(msg, "Can not allocate color '%s', use '%s' instead.",
               OldColorStr[ObjPtr->color], colorMenuItems[colorIndex]);
         Msg(msg);
         allocColorFailed = TRUE;
         ObjPtr->color = colorIndex;
      } else {
         ObjPtr->color = index;
      }
      break;
   }
   for (attr_ptr=ObjPtr->fattr; attr_ptr != NULL; attr_ptr=attr_ptr->next) {
      if (UpdatePixel(attr_ptr->obj, OldColorStr)) {
         picture_changed = TRUE;
      }
   }
   return picture_changed;
}

void UpdateXPmObjects(ObjPtr)
   struct ObjRec *ObjPtr;
{
   struct ObjRec *obj_ptr;
   struct XPmRec *xpm_ptr;

   switch (ObjPtr->type) {
   case OBJ_GROUP:
   case OBJ_SYM:
   case OBJ_ICON:
      for (obj_ptr=ObjPtr->detail.r->last; obj_ptr!=NULL;
            obj_ptr=obj_ptr->prev) {
         UpdateXPmObjects(obj_ptr);
      }
      break;
   case OBJ_XPM:
      xpm_ptr = ObjPtr->detail.xpm;
      if (xpm_ptr->image == NULL) {
         xpm_ptr->image = XGetImage(mainDisplay, xpm_ptr->pixmap, 0, 0,
               xpm_ptr->image_w, xpm_ptr->image_h, AllPlanes, ZPixmap);
      }
      break;
   }
}

int FlushColormap()
{
   register int i;
   int old_allocated_colors, changed=FALSE;
   char **old_color_str;
   struct ObjRec *obj_ptr;

#ifdef DONT_FREE_COLORMAP
   Msg("Colormap not reseted due to the DONT_FREE_COLORMAP compile flag.");
   return FALSE;
#endif
   if (!newColormapUsed) {
      Colormap colormap=XCopyColormapAndFree(mainDisplay, mainColormap);

      mainColormap = colormap;
      newColormapUsed = TRUE;
      XSetWindowColormap(mainDisplay, mainWindow, mainColormap);
   }
   if (newColormapUsed) {
      struct PageRec *page_ptr;
      int saved_color_layers;

      for (page_ptr=firstPage; page_ptr != NULL; page_ptr=page_ptr->next) {
         for (obj_ptr=page_ptr->bot; obj_ptr!=NULL; obj_ptr=obj_ptr->prev) {
            UpdateXPmObjects(obj_ptr);
         }
      }
      old_color_str = (char**)malloc(maxColors*sizeof(char*));
      if (old_color_str == NULL) FailAllocMessage();
      old_allocated_colors = maxColors;
      for (i = 0; i < maxColors; i++) {
         old_color_str[i] =
               (char*)malloc((strlen(colorMenuItems[i])+1)*sizeof(char));
         if (old_color_str[i] == NULL) FailAllocMessage();
         strcpy(old_color_str[i], colorMenuItems[i]);
      }
      initColorDontReload = TRUE;
      CleanUpColors();
      XFreeColormap(mainDisplay, mainColormap);
      mainColormap = DefaultColormap(mainDisplay, mainScreen);
      XSetWindowColormap(mainDisplay, mainWindow, mainColormap);
      newColormapUsed = FALSE;

      saved_color_layers = colorLayers;
      InitColor();
      initColorDontReload = FALSE;
      colorLayers = saved_color_layers;

      ShowColor(TRUE);

      SaveStatusStrings();
      gnUpdatePixelObjCount = 0;
      for (page_ptr=firstPage; page_ptr != NULL; page_ptr=page_ptr->next) {
         for (obj_ptr=page_ptr->bot; obj_ptr!=NULL; obj_ptr=obj_ptr->prev) {
            if (UpdatePixel(obj_ptr, old_color_str)) {
               changed = TRUE;
            }
         }
      }
      RestoreStatusStrings();
      for (i = 0; i < old_allocated_colors; i++) free(old_color_str[i]);
      free(old_color_str);

      DestroySubMenu(MENU_COLOR);
      if (colorLayers) {
         RedrawColorWindow();
      }
      if (changed) ClearAndRedrawDrawWindow();
      return TRUE;
   }
   return FALSE;
}

void AddColor()
{
   char spec[MAXSTRING+1], *c_ptr;

   *spec = '\0';
   if (Dialog("Please enter colors to add:", NULL, spec) == INVALID) return;
   UtilTrimBlanks(spec);
   if (*spec == '\0') return;
   for (c_ptr=strtok(spec, ", \t\n\r"); c_ptr != NULL && *c_ptr != '\0';
         c_ptr=strtok(NULL, ", \t\n\r")) {
      int new_alloc;
      int index=QuickFindColorIndex(NULL, c_ptr, &new_alloc, FALSE);

      if (index == INVALID) {
         sprintf(gszMsgBox, "%s '%s' %s.",
               "Fail to allocate the", c_ptr, "color");
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         return;
      } else {
         ChangeAllSelColor(index, FALSE);
      }
   }
}

void DumpRGBColorLine(FP, ColorIndex, Indent, EndOfLine)
   FILE *FP;
   int ColorIndex, Indent, EndOfLine;
{
   register int i;

   if (colorDump) {
      for (i=0; i < Indent; i++) fprintf (FP, " ");
      if (tgifColors[ColorIndex].red == 0 &&
            tgifColors[ColorIndex].green == 0 &&
            tgifColors[ColorIndex].blue == 0) {
         fprintf (FP, "0 setgray");
      } else if (tgifColors[ColorIndex].red == maxRGB &&
            tgifColors[ColorIndex].green == maxRGB &&
            tgifColors[ColorIndex].blue == maxRGB) {
         fprintf (FP, "1 setgray");
      } else {
         fprintf (FP, "%.3f %.3f %.3f setrgbcolor",
               ((float)tgifColors[ColorIndex].red/maxRGB),
               ((float)tgifColors[ColorIndex].green/maxRGB),
               ((float)tgifColors[ColorIndex].blue/maxRGB));
      }
      fprintf(FP, "%c", EndOfLine ? '\n' : ' ');
   } else {
      for (i=0; i < Indent; i++) fprintf (FP, " ");
      fprintf (FP, "0 setgray\n");
   }
}

