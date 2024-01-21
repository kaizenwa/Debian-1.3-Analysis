
#include <stdio.h>

#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/define.h"	/* YES, NO, MIN, MAX and more */
#include "hfiles/struct.h"	/* define the works */
#include "hfiles/extern.h"	/* extern main parameter structures */
#include "hfiles/rtcmd.h"	/* command protocol and ID's */


#define SOP_Count	56


int si_command(cmd)
			cmdrec *cmd;
{

	switch ( cmd->command ) {

	case NOPER:	break;


	case PIXEL:
		if ( cmd->count == 1 ) 
			si_pixel(cmd->count, cmd->mode, &cmd->x);
		else;
			/* read in a list of pixels  */
			/* saqhack_dispix(cmd.count, */
		break;

	case FILER:
		si_filer(cmd);
		break;

	case FRAME:
	default:
		fprintf(stderr, "si_command: bad command.\n");
	}

	return 1;

}

int	ITypeMap[] = { SOP_Count };



si_filer(cmd)
			cmdrec *cmd;
{
	switch ( cmd->mode ) {
	case INITI:
		si_init(cmd);
		break;

	default:
		fprintf(stderr, "si_filer: bad mode.\n");
	};
}

si_init(cmd)
			cmdrec *cmd;
{
	img.file_type = ITypeMap[cmd->x];
	if ( !img.file_type ) img.filename = "Counts read from InterNet"; 
	img.fd = 0;

	img.filerows = cmd->dx;
	img.filecols = cmd->dy;

	new_display();
	make_scalemap(0, 10);
	buffer.scale_min = 0;
	buffer.scale_max = 10;
}	



si_pixel(count, mode, list)
			int count;
			int mode;
			position *list;
{
	while ( count-- ) {
		switch ( mode ) {

		case SET:
			fprintf(stderr, "Set a pixel at %d, %d to %d\n"
				, list->x, list->y, list->z);

			break;
		case ADD:
			disp_pixel(list->x, list->y,
				buffer.shortbuf
				  [list->x + coord.buf.width * list->y]
				 += list->z);

			break;
		case XOR:

		default:
			fprintf(stderr, "mode %d at %d, %d to %d"
				, mode, list->x, list->y, list->z);
			fprintf(stderr, "si_pixel: bad mode\n");
		};
	};
}



disp_pixel(x, y, value)
			int x, y, value;
{
		float xr, yr;

  		unsigned long original;
  		GC gc, set_gc();

/*	i_transform(&coord.buftodisp, x, y, &xr, &yr);

	x = xr;  y = yr;
*/

	if ( 	   ( x < 0 ) 
		|| ( y < 0 ) 
		|| ( x > coord.disp.X2i ) 
		|| ( y > coord.disp.Y2i ) ) return; 

	dispbox.image.data[x + y * coord.disp.width] = value 
		= buffer.scalemap[value];

  	original = color.gcset.disp.foreground;			/* From MvH */

  	color.gcset.disp.foreground = (unsigned long)value;
  	gc = set_gc(&color.gcset.disp);
  	XDrawPoint(dispbox.display, dispbox.ID, gc, x, y);

  	color.gcset.disp.foreground = original;

/*	check n pix/ sec

	check tot pix

	? write

	check pixels / kbytes

	? partition

	write
*/
}



	


