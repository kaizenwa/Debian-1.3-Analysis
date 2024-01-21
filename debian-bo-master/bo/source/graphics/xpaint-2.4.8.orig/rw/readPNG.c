/* +-------------------------------------------------------------------+ */
/* | Copyright 1996, Greg Roelofs (newt@pobox.com)                     | */
/* | Last revised:  21 August 1996                                     | */
/* +-------------------------------------------------------------------+ */

/* $Id: readPNG.c,v 1.7 1996/08/29 05:24:48 torsten Exp $ */

#include <stdio.h>
#include <stdlib.h>
#include "png.h"
#include "image.h"
#include "rwTable.h"

#ifndef TRUE
#  define TRUE 1
#  define FALSE 0
#endif

#ifndef Trace
#  ifdef PNG_DEBUG
#    define Trace(x)   fprintf x ; fflush(stderr)
#  else
#    define Trace(x)
#  endif
#endif


int
TestPNG(char *file)  /* gets called a LOT on the first image:  brushes? */
{
    char header[8];
    FILE *fp = fopen(file, "rb");   /* libpng requires ANSI; so do we */

    if (!fp)
        return 0;

    fread(header, 1, 8, fp);
    fclose(fp);

    return png_check_sig(header, 8);
}



Image *
ReadPNG(char *file)
{
    FILE          *fp;
    png_structp   png_ptr;
    png_infop     info_ptr;
    int           i, hasAlpha=FALSE;
    int           width, height, level, bit_depth, npasses;
    Image         *image = NULL;


    Trace((stderr, "\nGRR ReadPNG:  reading file %s\n", file));
    if ((fp = fopen(file, "rb")) == (FILE *)NULL) {
        RWSetMsg("Error opening input file");
        return NULL;
    }

    png_ptr = (png_structp)malloc(sizeof(png_struct));
    if (!png_ptr) {
        RWSetMsg("Error allocating PNG png_ptr memory");
        fclose(fp);
        return NULL;
    }

    info_ptr = (png_infop)malloc(sizeof(png_info));
    if (!info_ptr) {
        RWSetMsg("Error allocating PNG info_ptr memory");
        free(png_ptr);
        fclose(fp);
        return NULL;
    }

    if (setjmp(png_ptr->jmpbuf)) {
        RWSetMsg("Error setting longjmp error handler");
        png_read_destroy(png_ptr, info_ptr, (png_info *)0);
        free(png_ptr);
        free(info_ptr);
        fclose(fp);
        return NULL;
    }

    png_info_init(info_ptr);
    png_read_init(png_ptr);
    png_init_io(png_ptr, fp);
    png_read_info(png_ptr, info_ptr);

    width = info_ptr->width;
    height = info_ptr->height;

    Trace((stderr, "GRR ReadPNG:  width = %d, height = %d\n", width, height));
    switch (info_ptr->color_type) {

        case PNG_COLOR_TYPE_PALETTE:
            Trace((stderr, "GRR ReadPNG:  PNG_COLOR_TYPE_PALETTE\n"));
            image = ImageNewCmap(width, height, info_ptr->num_palette);
            for (i = 0;  i < info_ptr->num_palette;  ++i)
                ImageSetCmap(image, i, info_ptr->palette[i].red,
                  info_ptr->palette[i].green, info_ptr->palette[i].blue);
            /* GRR:  still need to get image data into `image' */
            break;

        case PNG_COLOR_TYPE_RGB:
            Trace((stderr, "GRR ReadPNG:  PNG_COLOR_TYPE_RGB\n"));
            if (info_ptr->bit_depth == 16) {
                RWSetMsg("Stripping 48-bit RGB image to 24 bits");
                fprintf(stderr,
                  "ReadPNG:  stripping 48-bit RGB image to 24 bits\n");
                fflush(stderr);
                png_set_strip_16(png_ptr);
            }
            image = ImageNew(width, height);
            break;

        case PNG_COLOR_TYPE_GRAY:   /* treat grayscale as special colormap */
            Trace((stderr, "GRR ReadPNG:  PNG_COLOR_TYPE_GRAY\n"));
            bit_depth = info_ptr->bit_depth;
            if (info_ptr->bit_depth == 16) {
                RWSetMsg("Stripping 16-bit grayscale image to 8 bits");
                fprintf(stderr,
                  "ReadPNG:  stripping 16-bit grayscale image to 8 bits\n");
                fflush(stderr);
                png_set_strip_16(png_ptr);
                bit_depth = 8;
            }
            image = ImageNewCmap(width, height, 1 << bit_depth);
            switch (bit_depth) {
                case 1:
                    image->isBW = TRUE;
                    ImageSetCmap(image, 0,   0,   0,   0);
                    ImageSetCmap(image, 1, 255, 255, 255);
                    break;
                case 2:
                    image->isGrey = TRUE;
                    ImageSetCmap(image, 0,   0,   0,   0);
                    ImageSetCmap(image, 1,  85,  85,  85);  /* 255/3 */
                    ImageSetCmap(image, 2, 170, 170, 170);
                    ImageSetCmap(image, 3, 255, 255, 255);
                    break;
                case 4:
                    image->isGrey = TRUE;
                    for (i = 0;  i < 16;  ++i) {
                        level = i * 17;  /* 255/15 */
                        ImageSetCmap(image, i, level, level, level);
                    }
                    break;
                case 8:
                    image->isGrey = TRUE;
                    for (i = 0;  i < 256;  ++i)
                        ImageSetCmap(image, i, i, i, i);
                    break;
            }
            break;

        case PNG_COLOR_TYPE_RGB_ALPHA:
            Trace((stderr, "GRR ReadPNG:  PNG_COLOR_TYPE_RGB_ALPHA\n"));
            if (info_ptr->bit_depth == 16) {
                RWSetMsg("Stripping 64-bit RGBA image to 32 bits");
                png_set_strip_16(png_ptr);
            }
            /* need to split alpha and RGB/gray the hard way, sigh */
            hasAlpha = TRUE;
            image = ImageNew(width, height);
            ImageMakeMask(image);   /* maskData is always 1 byte deep */
            break;

        case PNG_COLOR_TYPE_GRAY_ALPHA:
            Trace((stderr, "GRR ReadPNG:  PNG_COLOR_TYPE_GRAY_ALPHA\n"));
            if (info_ptr->bit_depth == 16) {
                RWSetMsg("Stripping 32-bit gray+alpha image to 16 bits");
                png_set_strip_16(png_ptr);
            }
            hasAlpha = TRUE;
            image = ImageNewGrey(width, height);
            ImageMakeMask(image);   /* maskData is always 1 byte deep */
            break;

        default:
            fprintf(stderr, "ReadPNG:  unknown image type (%d)\n",
              info_ptr->color_type);
            fflush(stderr);
            RWSetMsg("Unknown PNG image type");
            png_read_destroy(png_ptr, info_ptr, (png_infop)NULL);
            free(png_ptr);
            free(info_ptr);
            fclose(fp);
            return image;  /* NULL */
    }

    if (info_ptr->bit_depth < 8)
        png_set_packing(png_ptr);

    if (info_ptr->interlace_type)
        npasses = png_set_interlace_handling(png_ptr);

    png_read_update_info(png_ptr, info_ptr);

    if (hasAlpha) {
        png_bytep *row_pointers, png_data;

	if ((row_pointers = (png_bytep *)
	     malloc(height * sizeof(png_bytep))) == 0) {
	    RWSetMsg("Out of memory in ReadPNG()");
	    return NULL;
	}
	
        /* very inefficient (memory-wise) to allocate entire image again, but
         * no easy way around it:  libpng returns the image and alpha channel
         * interspersed, and interlaced alpha images just make matters worse
         */
        png_data = (png_bytep)malloc(height*info_ptr->rowbytes);
        if (!png_data) {
            RWSetMsg("Unable to allocate temporary storage for alpha image");
            fprintf(stderr, "ReadPNG error:  unable to malloc png_data\n");
            fflush(stderr);
            ImageDelete(image);
            png_read_destroy(png_ptr, info_ptr, (png_infop)NULL);
            free(png_ptr);
            free(info_ptr);
            fclose(fp);
            return NULL;
        }

        /* only bit depths of 8 and 16 support alpha channels */
        for (i = 0;  i < height;  ++i)
            row_pointers[i] = (png_bytep)png_data + i*info_ptr->rowbytes;

        png_read_image(png_ptr, row_pointers);

        if (info_ptr->color_type == PNG_COLOR_TYPE_RGB_ALPHA) {
            png_bytep png=png_data, rgb=image->data, alpha=image->maskData;

            for (i = 0;  i < height*width;  ++i) {
                *rgb++   = *png++;
                *rgb++   = *png++;
                *rgb++   = *png++;
                *alpha++ = *png++;  /* GRR:  invert?  doesn't seem to be used */
            }
        } else {  /* PNG_COLOR_TYPE_GRAY_ALPHA */
            png_bytep png=png_data, gray=image->data, alpha=image->maskData;

            for (i = 0;  i < height*width;  ++i) {
                *gray++  = *png++;
                *alpha++ = *png++;  /* GRR:  invert? */
            }
        }
        free(png_data);   /* whew */

    } else {  /* no alpha channel */
        png_bytep *row_pointers;

	if ((row_pointers = (png_bytep *)
	     malloc(height * sizeof(png_bytep))) == 0) {
	    RWSetMsg("Out of memory in ReadPNG()");
	    return NULL;
	}
        for (i = 0;  i < height;  ++i)
            row_pointers[i] = (png_bytep)image->data + i*info_ptr->rowbytes;

        png_read_image(png_ptr, row_pointers);

    } /* end if (hasAlpha) */

    /* GRR:  ideally should read all other info (text chunks, etc.) and save
     *       whatever ones make sense for the possible output file...but we
     *       don't, both because we're too darned lazy to bother and because
     *       XPaint is too simple-minded to be able to use it
     */

    png_read_end(png_ptr, info_ptr);
    png_read_destroy(png_ptr, info_ptr, (png_infop)NULL);
    free(png_ptr);
    free(info_ptr);
    fclose(fp);

    return image;

} /* end function ReadPNG() */
