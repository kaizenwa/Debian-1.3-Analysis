
// This include is meant to be the body of the function ResetXX,
// which is implemented exactly the same for different resolutions. For
// compatibility reasons this is controlled by Defines instead of templates.
// Therefor the following type definitions are set to the required values,
// while compiling that function:
//
// #define DATA_TYPE unsigned char
// #define DATA_PAD  1


DATA_TYPE	*xdata;

// create buffer for Image-Data
// there is an optimization in the rotation-routine, which sometimes tries
// to access data beyond the allocated image, that might lead to a segmentation
// violation. Therefore, it might be good to allocated some additional
// rows of data for the image.
	offset_bytes=xwidth*offset_rows*sizeof(DATA_TYPE);
	xdata=new DATA_TYPE[xwidth*(xheight+2*offset_rows)];
	{	DATA_TYPE	*xdata_run=xdata;
		unsigned long blk_pixel=BlackPixel(dpy,scr);
		for (int i=xwidth*(xheight+2*offset_rows);i>0;i--) {
			*xdata_run++=(DATA_TYPE)blk_pixel;
		}
	}
	xdata+=(offset_bytes/sizeof(DATA_TYPE));

	if (!xdata) {
		fprintf(stderr,"not enough memory for XImage-data");
		exit(-1);
	}

// create the XImage
	ximage = XCreateImage(dpy, DefaultVisual(dpy,scr),
				DefaultDepth(dpy,scr), ZPixmap, 0,
				(char*)xdata, xwidth, xheight, 8*DATA_PAD, xwidth*sizeof(DATA_TYPE));

	if (!ximage) {
		fprintf(stderr,"\n*** can't allocate ximage.\n" );
		exit(0);
	}

// copy data from original image and inserting pixel values on the fly
	if (Width()==xwidth&&Height()==xheight) {
		register DATA_TYPE	*copy = xdata;
		register const byte	*org  = Data();
		register int	j,i;

		for (i=0; i<Height(); i++) {
			for (j=0; j<Width(); j++) 
				*copy++ = (DATA_TYPE)gif_cols[*org++];
		}
	}
	else {
		for (int y=0;y<xheight;y++) {
			register const byte	*org  = Data() + (y*Height()/xheight) * Width();
			register DATA_TYPE *copy = xdata + y * xwidth;
	
			if (xwidth<Width()) {
				register int x;
				register int delta = Width()/2;
	
				for (x=Width();x>0;x--) {
					delta-=xwidth;
					if (delta<0) {
						delta+=Width();
						*copy++ = (DATA_TYPE)gif_cols[*org];
					}
					org++;
				}
			}
			else {
				register int x;
				register int delta = xwidth/2;
	
				for (x=xwidth;x>0;x--) {
					delta-=Width();
					*copy++ = (DATA_TYPE)gif_cols[*org];
					if (delta<0) {
						delta+=xwidth;
						org++;
					}
				}
			}
		}
	}

	ximage->data = (char*)xdata;

//	XPutImage(dpy,pixmap,p->gc_all,ximage,0,0,0,0,xwidth,xheight);
