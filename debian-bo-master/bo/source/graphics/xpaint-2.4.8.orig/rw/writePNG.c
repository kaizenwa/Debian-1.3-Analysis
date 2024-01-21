/* +-------------------------------------------------------------------+ */
/* | Copyright 1996, Greg Roelofs (newt@pobox.com)                     | */
/* | Last revised:  21 August 1996                                     | */
/* +-------------------------------------------------------------------+ */

/* $Id: writePNG.c,v 1.7 1996/08/29 05:24:57 torsten Exp $ */

#include <stdio.h>
#include <stdlib.h>
#include "png.h"
#include "image.h"
#include "rwTable.h"

#ifndef Trace
#  ifdef PNG_DEBUG
#    define Trace(x)   fprintf x ; fflush(stderr)
#  else
#    define Trace(x)
#  endif
#endif

extern void compressColormap(Image *image);


static int
WritePNG(char *file, Image *image, int interlace_type)
{
    FILE  *fp = fopen(file, "wb");
    char software_text[40];
    int i;
    png_structp  png_ptr;
    png_infop  info_ptr;
    png_textp  software;


    Trace((stderr, "\nGRR WritePNG:  %d x %d, scale = %d\n",
      image->width, image->height, image->scale));
    if (!fp)
        return 1;

    png_ptr = (png_structp)malloc(sizeof (png_struct));
    if (!png_ptr)
        return 1;

    info_ptr = (png_infop)malloc(sizeof (png_info));
    if (!info_ptr) {
        free(png_ptr);
        return 1;
    }

    if (setjmp(png_ptr->jmpbuf)) {    
        png_write_destroy(png_ptr);
        free(info_ptr);
        free(png_ptr);
        fclose(fp);
        return 1;
    }

    png_info_init(info_ptr);
    png_write_init(png_ptr);
    png_init_io(png_ptr, fp);

    info_ptr->width = image->width;
    info_ptr->height = image->height;

    if (image->isBW) {
        if (image->maskData) {
            info_ptr->color_type = PNG_COLOR_TYPE_GRAY_ALPHA;
            info_ptr->bit_depth = 8;   /* promote to full grayscale */
        } else {
            info_ptr->color_type = PNG_COLOR_TYPE_GRAY;
            info_ptr->bit_depth = 1;
        }
        Trace((stderr, "GRR WritePNG:  B/W, bit_depth = %d\n",
          info_ptr->bit_depth));

    } else if (image->isGrey) {
        info_ptr->color_type = image->maskData? PNG_COLOR_TYPE_GRAY_ALPHA :
                                                PNG_COLOR_TYPE_GRAY;
        if (image->cmapPacked)
            info_ptr->bit_depth = 8;
        else {
            Trace((stderr,
              "GRR WritePNG:  isGrey: cmapSize = %d (before compressing), ",
              image->cmapSize));
            compressColormap(image);
            Trace((stderr, "%d (after)\n", image->cmapSize));
            if (image->cmapSize > 16)
                info_ptr->bit_depth = 8;
            else if (image->cmapSize > 4)
                info_ptr->bit_depth = 4;
            else if (image->cmapSize > 2)
                info_ptr->bit_depth = 2;
            else
                info_ptr->bit_depth = 1;
            Trace((stderr, "GRR WritePNG:  isGrey: picked bit_depth = %d\n",
              info_ptr->bit_depth));
        }

    } else if (image->scale == 3) {
        Image *cmapImage;

        /* try compressing image to palette mode, but don't force if too big */
        if (!image->maskData)   /* can't store alpha mask with palette image */
            cmapImage = ImageCompress(image, 256, 1);

        if (cmapImage) {
            image = cmapImage;  /* original was deleted in ImageCompress() */
        } else {
            info_ptr->color_type = image->maskData? PNG_COLOR_TYPE_RGB_ALPHA :
                                                    PNG_COLOR_TYPE_RGB;
            info_ptr->bit_depth = 8;
            Trace((stderr, "GRR WritePNG:  RGB, bit_depth = 8\n"));
        }
    }

/*
	GRR ReadPNG:  reading file /home1/xpaint-images/cdrom.png
	GRR ReadPNG:  width = 34, height = 33
	GRR ReadPNG:  PNG_COLOR_TYPE_PALETTE
	GRR ReadPNG:  34 x 33, scale = 1
	GRR ReadPNG end:  34 x 33, scale = 1

	GRR WritePNG:  34 x 33, scale = 3
	GRR WritePNG:  palette, bit_depth = 4, num_palette = 12
 */

    /* either we're on an 8-bit or smaller display, or image->scale was 3 and
     * ImageCompress() worked
     */
    if (image->scale == 1) {
        info_ptr->color_type = PNG_COLOR_TYPE_PALETTE;
        if (image->maskData) {
            fprintf(stderr,
              "WritePNG:  can't use alpha mask with colormapped image\n");
            fflush(stderr);
        }
        Trace((stderr, "GRR WritePNG:  palette, bit_depth = "));
        if (!image->cmapPacked)
            compressColormap(image);
        if (image->cmapSize > 16)
            info_ptr->bit_depth = 8;
        else if (image->cmapSize > 4)
            info_ptr->bit_depth = 4;
        else if (image->cmapSize > 2)
            info_ptr->bit_depth = 2;
        else
            info_ptr->bit_depth = 1;
        info_ptr->valid |= PNG_INFO_PLTE;
        info_ptr->num_palette = image->cmapSize;
        info_ptr->palette = (png_colorp)image->cmapData;  /* seems to work... */
        Trace((stderr, "%d, num_palette = %d\n", info_ptr->bit_depth,
          info_ptr->num_palette));
#if 0
        for (i = 0;  i < image->cmapSize;  ++i) {
            info_ptr->palette[i].red = 
            info_ptr->palette[i].green = 
            info_ptr->palette[i].blue = 
        }
#endif
    }

    info_ptr->interlace_type = interlace_type;

    /* set the file gamma */
    info_ptr->valid |= PNG_INFO_gAMA;
#ifdef DISPLAY_GAMMA
    info_ptr->gamma = 1.0 / DISPLAY_GAMMA;
#else
    info_ptr->gamma = 0.45;   /* default: assume PC-like system */
#endif

    /* info_ptr->valid |= PNG_INFO_tEXt;               DOES NOT EXIST */
    /* only one text comment:  Software */
    software = (png_textp)malloc(sizeof(png_text));
    if (software) {
        /* room for up to 40 characters: */
        sprintf(software_text, "XPaint %s", XPAINT_VERSION);
        software->compression = -1;
        software->key = "Software";
        software->text = software_text;
        software->text_length = strlen(software->text);
        info_ptr->num_text = 1;
        info_ptr->text = software;
    } else {
        /* couldn't malloc:  oh well */
        info_ptr->num_text = 0;
        info_ptr->text = NULL;
    }

    info_ptr->valid |= PNG_INFO_tIME;
    png_convert_from_time_t(&info_ptr->mod_time, time(NULL));

    png_write_flush(png_ptr);
    png_write_info(png_ptr, info_ptr);

    png_set_packing(png_ptr);

    if (image->maskData) {
        /* alpha channel version */
        fprintf(stderr, "WritePNG:  sorry, can't write alpha images yet\n");
        fflush(stderr);
        png_write_destroy(png_ptr);
        free(info_ptr);
        free(png_ptr);
        fclose(fp);
        return 1;
    } else {
        int rowbytes = image->scale * image->width;
        png_bytep *row_pointers;

	if ((row_pointers = (png_bytep *)
	     malloc(image->height * sizeof(png_bytep))) == 0) {
	    RWSetMsg("Out of memory in WritePNG()");
	    return 1;
	}

        for (i = 0;  i < image->height;  ++i)
            row_pointers[i] = (png_bytep)image->data + i*rowbytes;

        png_write_image(png_ptr, row_pointers);
    }

    png_write_end(png_ptr, NULL);
    png_write_destroy(png_ptr);

    if (software)
        free(software);   /* we LOVE free software!! */

    free(info_ptr);
    free(png_ptr);   /* necessary?? */
    fclose(fp);

    return 0;
}



int
WritePNGn(char *file, Image *image)
{
    return WritePNG(file, image, 0);
}



int
WritePNGi(char *file, Image *image)
{
    return WritePNG(file, image, 1);
}
