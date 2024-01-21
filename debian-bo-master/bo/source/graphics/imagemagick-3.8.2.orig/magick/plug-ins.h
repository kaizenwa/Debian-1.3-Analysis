/*
  Plug-in include declaractions.
*/
#ifdef HasDPS
#include <DPS/dpsXclient.h>
#include <DPS/dpsXpreview.h>
#endif
#ifdef HasJBIG
#include "jbig.h"
#endif
#ifdef HasJPEG
#include <setjmp.h>
#include "jpeglib.h"
#include "jerror.h"
#endif
#ifdef HasMPEG
#undef BitmapPad
#include "mpeg.h"
#endif
#ifdef HasPNG
#include "png.h"
#endif
#ifdef HasTIFF
#include "tiffio.h"
#endif
