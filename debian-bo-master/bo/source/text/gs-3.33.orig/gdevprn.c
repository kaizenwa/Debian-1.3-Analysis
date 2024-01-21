/* Copyright (C) 1990, 1995 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gdevprn.c */
/* Generic printer driver support */
#include "gdevprn.h"
#include "gp.h"
#include "gsparam.h"

/* Define the scratch file name prefix for mktemp */
#define SCRATCH_TEMPLATE gp_scratch_file_name_prefix

/* Internal routine for opening a scratch file */
private int
open_scratch(char *fname, FILE **pfile)
{	char fmode[4];
	strcpy(fmode, "w+");
	strcat(fmode, gp_fmode_binary_suffix);
	*pfile = gp_open_scratch_file(SCRATCH_TEMPLATE, fname, fmode);
	if ( *pfile == NULL )
	   {	eprintf1("Could not open the scratch file %s.\n", fname);
		return_error(gs_error_invalidfileaccess);
	   }
	return 0;
}

/* ---------------- Standard device procedures ---------------- */

/* Macros for casting the pdev argument */
#define ppdev ((gx_device_printer *)pdev)
#define pmemdev ((gx_device_memory *)pdev)
#define pcldev ((gx_device_clist *)pdev)

/* Define the standard printer procedure vector. */
gx_device_procs prn_std_procs =
  prn_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close);

/* ------ Open/close ------ */

/* Forward references */
private int gdev_prn_alloc(P1(gx_device *));
private int gdev_prn_free(P1(gx_device *));

/* Open a generic printer device. */
/* Specific devices may wish to extend this. */
int
gdev_prn_open(gx_device *pdev)
{	int code;
	ppdev->file = NULL;
	code = gdev_prn_alloc(pdev);
	if ( code < 0 )
	  return code;
	if ( ppdev->OpenOutputFile )
	  code = gdev_prn_open_printer(pdev, 1);
	return code;
}

/* Allocate buffer space and initialize the device. */
/* We break this out as a separate procedure so that resetting */
/* the page dimensions doesn't have to close and reopen the device. */
private int
gdev_prn_alloc(gx_device *pdev)
{	const gx_device_memory *mdev =
	  gdev_mem_device_for_bits(pdev->color_info.depth);
	ulong mem_space;
	byte *base = 0;
	char *left = 0;

	if ( mdev == 0 )
	  return_error(gs_error_rangecheck);
	memset(ppdev->skip, 0, sizeof(ppdev->skip));
	ppdev->orig_procs = pdev->std_procs;
	ppdev->ccfile = ppdev->cbfile = NULL;
	mem_space = gdev_mem_bitmap_size(pmemdev);
	if ( mem_space >= ppdev->max_bitmap ||
	     mem_space != (uint)mem_space ||	/* too big to allocate */
	     (base = (byte *)gs_malloc((uint)mem_space, 1, "printer buffer")) == 0 ||	/* can't allocate */
	     (PRN_MIN_MEMORY_LEFT != 0 &&
	      (left = gs_malloc(PRN_MIN_MEMORY_LEFT, 1, "printer memory left")) == 0)	/* not enough left */
	   )
	{	/* Buffer the image in a command list. */
		uint space;
		int code;
		/* Release the buffer if we allocated it. */
		if ( base != 0 )
		  gs_free((char *)base, (uint)mem_space, 1,
			  "printer buffer(open)");
		for ( space = ppdev->use_buffer_space; ; )
		{	base = (byte *)gs_malloc(space, 1,
						 "command list buffer");
			if ( base != 0 ) break;
			if ( (space >>= 1) < PRN_MIN_BUFFER_SPACE )
			  return_error(gs_error_VMerror);	/* no hope */
		}
open_c:		pcldev->data = base;
		pcldev->data_size = space;
		pcldev->target = pdev;
		gs_make_mem_device(&pcldev->mdev, mdev, 0, 0, pdev);
		ppdev->buf = base;
		ppdev->buffer_space = space;
		/* Try opening the command list, to see if we allocated */
		/* enough buffer space. */
		code = (*gs_clist_device_procs.open_device)((gx_device *)pcldev);
		if ( code < 0 )
		{	/* If there wasn't enough room, and we haven't */
			/* already shrunk the buffer, try enlarging it. */
			if ( code == gs_error_limitcheck &&
			     space >= ppdev->use_buffer_space
			   )
			{	gs_free((char *)base, space, 1,
					"command list buffer(retry open)");
				space <<= 1;
				base = (byte *)gs_malloc(space, 1,
					 "command list buffer(retry open)");
				ppdev->buf = base;
				if ( base != 0 )
				  goto open_c;
			}
			/* Fall through with code < 0 */
		}
		if ( code < 0 ||
		     (code = open_scratch(ppdev->ccfname, &ppdev->ccfile)) < 0 ||
		     (code = open_scratch(ppdev->cbfname, &ppdev->cbfile)) < 0
		   )
		{	/* Clean up before exiting */
			gdev_prn_free(pdev);
			return code;
		}
		pcldev->cfile = ppdev->ccfile;
		pcldev->bfile = ppdev->cbfile;
		pcldev->bfile_end_pos = 0;
		ppdev->std_procs = gs_clist_device_procs;
	}
	else
	{	/* Render entirely in memory. */
		/* Release the leftover memory. */
		gs_free(left, PRN_MIN_MEMORY_LEFT, 1,
			"printer memory left");
		ppdev->buffer_space = 0;
		pmemdev->base = base;
		ppdev->std_procs = mdev->std_procs;
		ppdev->std_procs.get_page_device =
		  gx_page_device_get_page_device;
	}
	/* Synthesize the procedure vector. */
	/* Rendering operations come from the memory or clist device, */
	/* non-rendering come from the printer device. */
#define copy_proc(p) set_dev_proc(ppdev, p, ppdev->orig_procs.p)
	copy_proc(get_initial_matrix);
	copy_proc(output_page);
	copy_proc(close_device);
	copy_proc(map_rgb_color);
	copy_proc(map_color_rgb);
	copy_proc(get_params);
	copy_proc(put_params);
	copy_proc(map_cmyk_color);
	copy_proc(get_xfont_procs);
	copy_proc(get_xfont_device);
	copy_proc(map_rgb_alpha_color);
	copy_proc(get_page_device);
#undef copy_proc
	return (*dev_proc(pdev, open_device))(pdev);
}

/* Generic closing for the printer device. */
/* Specific devices may wish to extend this. */
int
gdev_prn_close(gx_device *pdev)
{	gdev_prn_free(pdev);
	if ( ppdev->file != NULL )
	{	if ( ppdev->file != stdout )
		  gp_close_printer(ppdev->file, ppdev->fname);
		ppdev->file = NULL;
	}
	return 0;
}

/* Free buffer space and related objects, the inverse of alloc. */
/* Again, we break this out for resetting page dimensions. */
private int
gdev_prn_free(gx_device *pdev)
{	if ( ppdev->ccfile != NULL )
	{	fclose(ppdev->ccfile);
		ppdev->ccfile = NULL;
		unlink(ppdev->ccfname);
	}
	if ( ppdev->cbfile != NULL )
	{	fclose(ppdev->cbfile);
		ppdev->cbfile = NULL;
		unlink(ppdev->cbfname);
	}
	if ( ppdev->buffer_space != 0 )
	{	/* Free the buffer */
		gs_free((char *)ppdev->buf, (uint)ppdev->buffer_space, 1,
			"command list buffer(close)");
		ppdev->buf = 0;
		ppdev->buffer_space = 0;
	}
	else
	{	/* Free the memory device bitmap */
		gs_free((char *)pmemdev->base,
			(uint)gdev_mem_bitmap_size(pmemdev),
			1, "printer buffer(close)");
		pmemdev->base = 0;
	}
	pdev->std_procs = ppdev->orig_procs;
	return 0;
}

/* ------ Get/put parameters ------ */

/* Get parameters.  Printer devices add several more parameters */
/* to the default set. */
int
gdev_prn_get_params(gx_device *pdev, gs_param_list *plist)
{	int code = gx_default_get_params(pdev, plist);
	gs_param_string ofns;

	if ( code < 0 ) return code;
	code = (ppdev->NumCopies_set ?
		param_write_int(plist, "NumCopies", &ppdev->NumCopies) :
		param_write_null(plist, "NumCopies"));
	if ( code < 0 ) return code;
	code = param_write_bool(plist, "OpenOutputFile", &ppdev->OpenOutputFile);
	if ( code < 0 ) return code;
	code = param_write_long(plist, "BufferSpace", &ppdev->use_buffer_space);
	if ( code < 0 ) return code;
	code = param_write_long(plist, "MaxBitmap", &ppdev->max_bitmap);
	if ( code < 0 ) return code;
	ofns.data = (const byte *)ppdev->fname,
	  ofns.size = strlen(ppdev->fname),
	  ofns.persistent = false;
	return param_write_string(plist, "OutputFile", &ofns);
}

/* Put parameters. */
int
gdev_prn_put_params(gx_device *pdev, gs_param_list *plist)
{	int ecode = 0;
	int code;
	const char _ds *param_name;
	bool is_open = pdev->is_open;
	int nci = ppdev->NumCopies;
	bool oof = ppdev->OpenOutputFile;
	bool ncnull = false;
	int width = pdev->width;
	int height = pdev->height;
	long buffer_space = ppdev->use_buffer_space;
	long max_bitmap = ppdev->max_bitmap;
	long bsl = buffer_space;
	long mbl = max_bitmap;
	gs_param_string ofs;
	gs_param_dict mdict;

	switch ( code = param_read_int(plist, (param_name = "NumCopies"), &nci) )
	{
	case 0:
		if ( nci < 0 )
		  ecode = gs_error_rangecheck;
		else
		  break;
		goto nce;
	default:
		if ( (code = param_read_null(plist, param_name)) == 0 )
		  {	ncnull = true;
			break;
		  }
		ecode = code;	/* can't be 1 */
nce:		param_signal_error(plist, param_name, ecode);
	case 1:
		break;
	}

	switch ( code = param_read_bool(plist, (param_name = "OpenOutputFile"), &oof) )
	{
	default:
		ecode = code;
		param_signal_error(plist, param_name, ecode);
	case 0:
	case 1:
		break;
	}

	switch ( code = param_read_long(plist, (param_name = "BufferSpace"), &bsl) )
	{
	case 0:
		if ( bsl < 10000 )
		  ecode = gs_error_rangecheck;
		else
		  break;
		goto bse;
	default:
		ecode = code;
bse:		param_signal_error(plist, param_name, ecode);
	case 1:
		break;
	}

	switch ( code = param_read_long(plist, (param_name = "MaxBitmap"), &mbl) )
	{
	case 0:
		if ( mbl < 10000 )
		  ecode = gs_error_rangecheck;
		else
		  break;
		goto mbe;
	default:
		ecode = code;
mbe:		param_signal_error(plist, param_name, ecode);
	case 1:
		break;
	}

	switch ( code = param_read_string(plist, (param_name = "OutputFile"), &ofs) )
	{
	case 0:
		if ( ofs.size >= prn_fname_sizeof )
		  ecode = gs_error_limitcheck;
		else
		  break;
		goto ofe;
	default:
		ecode = code;
ofe:		param_signal_error(plist, param_name, ecode);
	case 1:
		ofs.data = 0;
		break;
	}

	/* Read InputAttributes and OutputAttributes just for the type */
	/* check and to indicate that they aren't undefined. */
#define read_media(pname)\
	switch ( code = param_begin_read_dict(plist, (param_name = pname), &mdict, true) )\
	  {\
	  case 0:\
		param_end_read_dict(plist, pname, &mdict);\
		break;\
	  default:\
		ecode = code;\
		param_signal_error(plist, param_name, ecode);\
	  case 1:\
		;\
	  }

	read_media("InputAttributes");
	read_media("OutputAttributes");

	if ( ecode < 0 )
	  return ecode;
	/* Prevent gx_default_put_params from closing the printer. */
	pdev->is_open = false;
	code = gx_default_put_params(pdev, plist);
	pdev->is_open = is_open;
	if ( code < 0 )
	  return code;

	if ( nci != ppdev->NumCopies )
	  {	ppdev->NumCopies = nci;
		ppdev->NumCopies_set = true;
	  }
	else if ( ncnull )
	  ppdev->NumCopies_set = false;
	ppdev->OpenOutputFile = oof;
	/* If necessary, free and reallocate the printer memory. */
	if ( bsl != buffer_space || mbl != max_bitmap ||
	     pdev->width != width || pdev->height != height
	   )
	  {	if ( is_open )
		  gdev_prn_free(pdev);
		ppdev->use_buffer_space = bsl;
		ppdev->max_bitmap = mbl;
		if ( is_open )
		  {	ecode = code = gdev_prn_alloc(pdev);
			if ( code < 0 )
			  {	/* Try to put things back as they were. */
				ppdev->use_buffer_space = buffer_space;
				ppdev->max_bitmap = max_bitmap;
				gx_device_set_width_height(pdev,
							   width, height);
				code = gdev_prn_alloc(pdev);
				if ( code < 0 )
				  {	/* Disaster!  We can't get back. */
					pdev->is_open = false;
					return code;
				  }
				return ecode;
			  }
		  }
	  }
	if ( ofs.data != 0 )
	  {	/* Close the file if it's open. */
		if ( ppdev->file != NULL && ppdev->file != stdout )
		  gp_close_printer(ppdev->file, ppdev->fname);
		ppdev->file = NULL;
		memcpy(ppdev->fname, ofs.data, ofs.size);
		ppdev->fname[ofs.size] = 0;
	  }

	/* If the device is open and OpenOutputFile is true, */
	/* open the OutputFile now.  (If the device isn't open, */
	/* this will happen when it is opened.) */
	if ( pdev->is_open && oof )
	  {	code = gdev_prn_open_printer(pdev, 1);
		if ( code < 0 )
		  return code;
	  }

	return 0;
}

/* Put InputAttributes and OutputAttributes. */
int
gdev_prn_input_page_size(int index, gs_param_dict *pdict,
  floatp width_points, floatp height_points)
{	input_media media;
	media.PageSize[0] = width_points;
	media.PageSize[1] = height_points;
	media.MediaColor = 0;
	media.MediaWeight = 0;
	media.MediaType = 0;
	return gdev_prn_input_media(index, pdict, &media);
}
private int
finish_media(gs_param_list *mlist, gs_param_name key, const char *media_type)
{	int code = 0;
	if ( media_type != 0 )
	  {	gs_param_string as;
		param_string_from_string(as, media_type);
		code = param_write_string(mlist, key, &as);
	  }
	return code;
}
int
gdev_prn_input_media(int index, gs_param_dict *pdict, const input_media *pim)
{	char key[25];
	gs_param_dict mdict;
	int code;
	gs_param_string as;

	sprintf(key, "%d", index);
	mdict.size = 4;
	code = param_begin_write_dict(pdict->list, key, &mdict, false);
	if ( code < 0 )
	  return code;
	if ( pim->PageSize[0] != 0 && pim->PageSize[1] != 0 )
	  {	gs_param_float_array psa;
		psa.data = pim->PageSize, psa.size = 2, psa.persistent = false;
		code = param_write_float_array(mdict.list, "PageSize",
					       &psa);
		if ( code < 0 )
		  return code;
	  }
	if ( pim->MediaColor != 0 )
	  {	param_string_from_string(as, pim->MediaColor);
		code = param_write_string(mdict.list, "MediaColor",
					  &as);
		if ( code < 0 )
		  return code;
	  }
	if ( pim->MediaWeight != 0 )
	  {	/* We do the following silly thing in order to avoid */
		/* having to work around the 'const' in the arg list. */
		float weight = pim->MediaWeight;
		code = param_write_float(mdict.list, "MediaWeight",
					 &weight);
		if ( code < 0 )
		  return code;
	  }
	code = finish_media(mdict.list, "MediaType", pim->MediaType);
	if ( code < 0 )
	  return code;
	return param_end_write_dict(pdict->list, key, &mdict);
}
int
gdev_prn_output_media(int index, gs_param_dict *pdict, const output_media *pom)
{	char key[25];
	gs_param_dict mdict;
	int code;

	sprintf(key, "%d", index);
	mdict.size = 4;
	code = param_begin_write_dict(pdict->list, key, &mdict, false);
	if ( code < 0 )
	  return code;
	code = finish_media(mdict.list, "OutputType", pom->OutputType);
	if ( code < 0 )
	  return code;
	return param_end_write_dict(pdict->list, key, &mdict);
}

/* ------ Others ------ */

/* Generic routine to send the page to the printer. */
int
gdev_prn_output_page(gx_device *pdev, int num_copies, int flush)
{	int code, outcode, closecode, errcode;

	code = gdev_prn_open_printer(pdev, 1);
	if ( code < 0 )
	  return code;

	/* Print the accumulated page description. */
	outcode = (*ppdev->print_page_copies)(ppdev, ppdev->file, num_copies);
	errcode = (ferror(ppdev->file) ? gs_error_ioerror : 0);
	closecode = gdev_prn_close_printer(pdev);

	if ( ppdev->buffer_space ) /* reinitialize clist for writing */
	  code = (*gs_clist_device_procs.output_page)(pdev, num_copies, flush);

	if ( outcode < 0 )
	  return outcode;
	if ( errcode < 0 )
	  return_error(errcode);
	if ( closecode < 0 )
	  return closecode;
	return code;
}

/* Print multiple copies of a page by calling print_page multiple times. */
int
gx_default_print_page_copies(gx_device_printer *pdev, FILE *prn_stream,
  int num_copies)
{	int i = num_copies;
	int code = 0;
	while ( code >= 0 && i-- > 0 )
	  code = (*pdev->print_page)(pdev, prn_stream);
	return code;
}

/* ---------------- Driver services ---------------- */

/* Open the current page for printing. */
int
gdev_prn_open_printer(gx_device *pdev, int binary_mode)
{	char *fname = ppdev->fname;
	char pfname[prn_fname_sizeof + 10];
	char *pnpos = strchr(fname, '%');
	if ( pnpos != NULL )
	{	long count1 = ppdev->PageCount + 1;
		sprintf(pfname, fname,
			(pnpos[1] == 'l' ? count1 : (int)count1));
		fname = pfname;
	}
	if ( ppdev->file == NULL )
	{	if ( !strcmp(fname, "-") )
			ppdev->file = stdout;
		else
		{	ppdev->file = gp_open_printer(fname, binary_mode);
			if ( ppdev->file == NULL )
				return_error(gs_error_invalidfileaccess);
		}
		ppdev->file_is_new = true;
	}
	else
		ppdev->file_is_new = false;
	return 0;
}

/* Copy a scan line from the buffer to the printer. */
int
gdev_prn_get_bits(gx_device_printer *pdev, int y, byte *str, byte **actual_data)
{	int code = (*dev_proc(pdev, get_bits))((gx_device *)pdev, y, str, actual_data);
	uint line_size = gdev_prn_raster(pdev);
	int last_bits = -(pdev->width * pdev->color_info.depth) & 7;
	if ( code < 0 ) return code;
	if ( last_bits != 0 )
	{	byte *dest = (actual_data != 0 ? *actual_data : str);
		dest[line_size - 1] &= 0xff << last_bits;
	}
	return 0;
}
/* Copy scan lines to a buffer.  Return the number of scan lines, */
/* or <0 if error. */
int
gdev_prn_copy_scan_lines(gx_device_printer *pdev, int y, byte *str, uint size)
{	uint line_size = gdev_prn_raster(pdev);
	int count = size / line_size;
	int i;
	byte *dest = str;
	count = min(count, pdev->height - y);
	for ( i = 0; i < count; i++, dest += line_size )
	{	int code = gdev_prn_get_bits(pdev, y + i, dest, NULL);
		if ( code < 0 ) return code;
	}
	return count;
}

/* Close the current page. */
int
gdev_prn_close_printer(gx_device *pdev)
{	if ( strchr(ppdev->fname, '%') )	/* file per page */
	   {	gp_close_printer(ppdev->file, ppdev->fname);
		ppdev->file = NULL;
	   }
	return 0;
}
