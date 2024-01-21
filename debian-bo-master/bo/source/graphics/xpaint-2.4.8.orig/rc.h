/* $Id: rc.h,v 1.2 1996/04/19 09:19:06 torsten Exp $ */

typedef struct {
    int freed;
    int ncolors;
    char **colors;
    Boolean *colorFlags;
    Pixel *colorPixels;
    int nimages;
    Image **images;
    int nbrushes;
    Image **brushes;
} RCInfo;

RCInfo *ReadDefaultRC(void);
RCInfo *ReadRC(char *);
void FreeRC(RCInfo *);
