/*
  Include declarations.
*/
#ifndef _MAGICK_H
#define _MAGICK_H

#if defined(__hpux)
#define _HPUX_SOURCE  1
#endif
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <time.h>
#if !defined(macintosh)
#include <sys/types.h>
#include <sys/stat.h>
#else
#include <SIOUX.h>
#include <console.h>
#include <unix.h>
#include <types.h>
#include <stat.h>
#endif
#undef index

/*
  ImageMagick include declarations.
*/
#if defined(__cplusplus) || defined(c_plusplus)
#define class  c_class
#endif
#include "image.h"
#include "gems.h"
#include "compress.h"
#include "utility.h"
#include "monitor.h"
#include "error.h"
#include "X.h"
#include "widget.h"
#include "PreRvIcccm.h"

/*
  Define declarations.
*/
#define AbsoluteValue(x)  ((x) < 0 ? -(x) : (x))
#define DownShift(x) (((int) ((x)+(1L << 13))) >> 14)
#define Extent(string)  ((int) strlen(string))
#define False  0
#define Max(x,y)  (((x) > (y)) ? (x) : (y))
#define Min(x,y)  (((x) < (y)) ? (x) : (y))
#ifndef M_PI
#define M_PI  3.14159265358979323846
#endif
#define QuantumTick(i,image) \
  (((i+1) == image->packets) || ((i % image->rows) == 0))
#define Swap(x,y) ((x)^=(y), (y)^=(x), (x)^=(y))
#ifndef STDIN_FILENO
#define STDIN_FILENO  0
#endif
#define True  1
#define UpShift(x) ((int) (x) << 14)
#define UpShifted(x) ((int) ((x)*(1L << 14)+0.5))

/*
  Review these definitions and change them to suit your local requirements.
*/
#define CGMCommand  "|ralcgm -d ps %s - /dev/null"
#define CompressCommand  "|compress -c > %s"
#define DefaultDisplayGamma  "2.2"
#define DefaultFont  "Helvetica"
#define DefaultImageQuality  "75"
#define DefaultInterlace  PlaneInterlace
#define DefaultPointSize  "18"
#define DefaultPreviewGeometry  "204x204+10+10"
#define DefaultPreviewMatte  "#fff"
#define DefaultPreviewPageGeometry  "3x3"
#define DefaultTextBackground  "#ffffff"
#define DefaultTextForeground  "#000000"
#define DefaultTileBackground  "#696e7e"
#define DefaultTileBorderwidth "10"
#define DefaultTileForeground  "#600"
#define DefaultTileGeometry  "120x120+20+10>"
#define DefaultTileLabel  "%f"
#define DefaultTileMatte  "#bdbdbd"
#define DefaultTilePageGeometry  "5x4"
#define DocumentationURL  \
  "http://www.wizards.dupont.com/cristy/ImageMagick.html"
#define GunzipCommand  "|gzip -cdfq %s"
#define GzipCommand  "|gzip -cf > %s"
#define PICTCommand  "|picttoppm %s"
#define ReadBinaryType  "rb"
#define TemporaryDirectory  "/usr/tmp"
#define UncompressCommand  "|uncompress -c %s"
#define UndoCache  "16"
#define WriteBinaryType  "wb"
#define WWWCommand  "/usr/local/bin/GET %s:%s > %s 2>&1"
/*
  Review these machine specific definitions.
*/
#if !defined(vms) && !defined(macintosh) && !defined(WIN32)
#define ApplicationDefaults  "/usr/lib/X11/app-defaults/"
#define BrowseCommand  "netscape %s &"
#define EditorCommand  "xterm -title \"Edit Image Comment\" -e vi %s"
#define Export
#define LauncherCommand  "xpaint %s"
#define LaunchFormat  "pnm"
#define PostscriptColorDevice  "pnmraw"
#define PostscriptCommand \
  "gs -sDEVICE=%s -q -dNOPAUSE -dSAFER %s -sOutputFile=%s -- %s -c quit"
#define PostscriptMonoDevice  "pbmraw"
#define PreferencesDefaults  "~/."
#define PrinterCommand  "lp -c %s"
#define PrinterFormat  "ps"
#define ReadCommandlLine(argc,argv)
#define RGBColorDatabase  "/usr/lib/X11/rgb.txt"
#define ShowImageCommand \
  "display %s -immutable -window_group 0x%lx -title \"%s of %s\" tmp:%s &"
#else
#if defined(vms)
#define ApplicationDefaults  "decw$system_defaults:"
#define BrowseCommand "mosaic %s"
#define EditorCommand  "cre/term/wait edit/tpu %s"
#define Export
#define LauncherCommand  "xpaint %s"
#define LaunchFormat  "pnm"
#define PostscriptColorDevice  "pnmraw"
#define PostscriptCommand \
  "gs \"-sDEVICE=%s\" -q \"-dNOPAUSE\" \"-dSAFER\" \"%s\" \"-sOutputFile=%s\" -- \"%s\" \"-c\" \"quit\""
#define PostscriptMonoDevice  "pbmraw"
#define PreferencesDefaults  "decw$user_defaults:"
#define PrinterCommand  "print/delete %s"
#define PrinterFormat  "ps"
#define ReadCommandlLine(argc,argv)
#define RGBColorDatabase  "sys$common:[sysmgr]decw$rgb.dat"
#define ShowImageCommand \
  "display %s -immutable -window_group 0x%lx -title \"%s of %s\" tmp:%s"
#endif
#if defined(macintosh)
#define ApplicationDefaults  "/usr/lib/X11/app-defaults/"
#define BrowseCommand  "netscape %s"
#define EditorCommand  "xterm -title \"Edit Image Comment\" -e vi %s"
#define Export
#define HasTIFF
#define HasJPEG
#define HasPNG
#define LauncherCommand  "xpaint %s"
#define LaunchFormat  "pnm"
#define PostscriptColorDevice  "pnmraw"
#define PostscriptCommand \
  " -sDEVICE=%s -q -dNOPAUSE -dSAFER %s -sOutputFile=\"%s\" -- \"%s\" -c quit"
#define PostscriptMonoDevice  "pbmraw"
#define PreferencesDefaults  "~/."
#define PrinterCommand  "lp -c %s"
#define PrinterFormat  "ps"
#define ReadCommandlLine(argc,argv)  argc=ccommand(argv); puts(Version);
#define RGBColorDatabase  "/usr/lib/X11/rgb.txt"
#define ShowImageCommand \
  "display %s -immutable -window_group 0x%lx -title \"%s of %s\" tmp:%s"
#endif
#if defined(WIN32)
#define ApplicationDefaults  "/usr/lib/X11/app-defaults/"
#define BrowseCommand  \
  "c:/Program Files/Plus!/Microsoft Internet/iexplore %s &"
#define EditorCommand  "notepad %s"
#define Export  __declspec(dllexport)
#define HasTIFF
#define HasJPEG
#define HasPNG
#define LauncherCommand  "mspaint %s"
#define LaunchFormat  "bmp"
#define PostscriptColorDevice  "pcx24b"
#define PostscriptCommand \
  "gswin32 -sDEVICE=%s -q -dNOPAUSE -dSAFER %s -sOutputFile=%s -- %s -c quit |"
#define PostscriptMonoDevice  "pcxmono"
#define PreferencesDefaults  "~/."
#define PrinterCommand  "print %s"
#define PrinterFormat  "pcl"
#define ReadCommandlLine(argc,argv)
#define RGBColorDatabase  "../xlib/lib/X11/rgb.txt"
#define ShowImageCommand \
  "display %s -immutable -window_group 0x%lx -title \"%s of %s\" tmp:%s &"
#endif
#endif

/*
  Page geometries:
*/
#define PCLPageGeometry  "612x792+43+43"
#define PCLDensityGeometry  "75x75"
#define PSDensityGeometry  "72x72"
#define PSPageGeometry  "612x792+43+43"
#define TextPageGeometry  "612x792+43+43"
/*
  3D effects.
*/
#define AccentuateModulate  UpScale(80)
#define HighlightModulate  UpScale(125)
#define ShadowModulate  UpScale(135)
#define DepthModulate  UpScale(185)
#define TroughModulate  UpScale(110)

#endif
