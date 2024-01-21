/* This software is Copyright 1995, 1996 by Karl-Johan Johnsson
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ANY USE OF THIS
 * SOFTWARE IS AT THE USERS OWN RISK.
 */
typedef struct {
    unsigned char	pixel;
    unsigned char	r;
    unsigned char	g;
    unsigned char	b;
} CMAP_ENTRY;

extern CMAP_ENTRY	*cmap;
extern int		 cmap_size;

extern void	color_init(Display*);
extern void	alloc_colors(void);
extern Pixmap	put_8_image(unsigned char*, long, long);
extern Pixmap	put_24_image(unsigned char*, long, long);
extern Pixmap	put_grey_image(unsigned char*, long, long);
extern Pixmap	put_cmapped_image(unsigned char*, long, long,
				  CMAP_ENTRY*, unsigned int);
extern Pixel	get_closest_color(XColor*);
