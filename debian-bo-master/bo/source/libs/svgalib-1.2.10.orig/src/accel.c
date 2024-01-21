
#include "vga.h"
#include "timing.h"
#include "accel.h"


int accel_screenpitch;
int accel_bytesperpixel;
int accel_screenpitchinbytes;
int accel_mode;
int accel_bitmaptransparency;

void InitializeAcceleratorInterface(ModeInfo * modeinfo)
{
    accel_screenpitch = modeinfo->lineWidth / modeinfo->bytesPerPixel;
    accel_bytesperpixel = modeinfo->bytesPerPixel;
    accel_screenpitchinbytes = modeinfo->lineWidth;
    accel_mode = BLITS_SYNC;
    accel_bitmaptransparency = 0;
}
