#ifndef lint
static char SccsConstId[] = "%W%  %G%";
#endif

/* Module:	Constant.h
 * Purpose:	Named constants to identify modes and requests
 * Modified:	{0} Michael VanHilst	initial version	     31 December 1987
 *		{1} MVH	Some names changed for X11 version	  18 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

/* special flag values to recognize a unique condition */
#define INOTSET -0x2000000

/* major mode identifiers */
#define BOP	0x001		/* black and white halftone color maps */
#define COP	0x002		/* cursors */
#define EOP	0x003		/* environment controls */
#define GOP	0x004		/* color graph manipulation */
#define MOP	0x005		/* color maps */
#define ROP	0x006		/* region controls */
#define SOP	0x007		/* image reading and scaling */
#define VOP	0x008		/* color manipulation */
#define XOP	0x009		/* magnifier window */
#define ZOP	0x00a		/* pan and zoom */

/* environment control flags */
#define EOP_Verbose	0x0010
#define EOP_Raise	0x0020
#define EOP_Reset	0x0030
#define EOP_Track	0x0040
#define EOP_ScopeTrack	0x0050
#define EOP_TextTrack	0x0060
#define EOP_ColorTrack	0x0070
#define EOP_Output	0x0080
#define EOP_NewFile	0x0090
#define EOP_Exit	0x0100

/* image drawing flags  - use code numbers */
#define SOP_Linear	0x0001
#define SOP_Wrap	0x0002
#define SOP_Sqrt	0x0003
#define SOP_Log		0x0004
#define SOP_HistEq	0x0005
#define SOP_Zscale	0x0007
#define SOP_Histogram	0x0010
#define SOP_Blink	0x0100

#define SOP_ZoomNone	0x000b
#define SOP_ZoomSamp	0x000c
#define SOP_ZoomMax	0x000d
#define SOP_ZoomSum	0x000e
#define SOP_ZoomAv	0x000f

/* image reading file and pipe packet types */
#define SOP_Array	0x0010		/* raster array */
#define SOP_FITS	0x0020		/* FlexibleImageTransferSystem */
#define SOP_SAOCCD	0x0030		/* SAO CCD FITS-like array */
#define SOP_IRAF	0x0040		/* IRAF imh file header */
#define SOP_IPC		0x0050		/* Einstein satellite IPC */
#define SOP_HRI		0x0060		/* Einstein satellite HRI */
#define SOP_ROSAT	0x0070		/* Roentgen satellite raw */
#define SOP_HOPR	0x0080		/* ROSAT HRI intermediate */
#define SOP_PROS	0x0090		/* IRAF pipe with PROS regions */
#define SOP_Imtool	0x00a0		/* IRAF pipe with NOAO lists */
#define SOP_MIDAS	0x00b0
#define SOP_Logo	0x00c0
#define SOP_Giff	0x00d0		/* compressed */
#define SOP_RLE		0x00e0		/* Utah RunLengthEncoded */
#define SOP_HDF		0x00f0		/* NCSA image format */

/* Array types for image data */
#define ARR_None	0x0000
#define ARR_U1		0x0001
#define ARR_I2		0x0002
#define ARR_U2		0x0003
#define ARR_I4		0x0004
#define ARR_R4		0x0014
#define ARR_R8		0x0018
#define ARR_ASC_I	0x0021
#define ARR_ASC_R	0x0022
#define ARR_ASC_Sc	0x0023

/* color modifying flags */
#define VOP_Halftone	0x0001
#define VOP_Cells	0x0002
#define VOP_Overlay	0x0004
#define VOP_PseudoColor 0x0010
#define VOP_StaticColor 0x0020

#define VOP_ContBias	0x1000
#define VOP_ThreshSat	0x2000
#define VOP_gamma	0x4000
#define VOP_Invert	0x0080

#define MOP_Store	0x0010
#define MOP_Recall	0x0020
#define MOP_Write	0x0030
#define MOP_Read	0x0040

#define MOP_GrayScale	0x0000
#define MOP_Init_A	0x0100
#define MOP_Init_B	0x0200
#define MOP_Init_C	0x0300
#define MOP_Init_D	0x0400
#define MOP_Init_E	0x0500
#define MOP_Init_F	0x0600
#define MOP_Init_G	0x0700
#define MOP_Init_H	0x0800
#define MOP_SignOff	0x1000

/* BLACK AND WHITE HALFTONING */
#define BOP_Dither	0x0010
#define BOP_Diffuse	0x0020
#define BOP_ImPress	0x0030
#define BOP_PostScript	0x0040
#define BOP_Invert	0x0080
#define BOP_Matrix1	0x0100		/* WEIGHTING MATRICES FOR DITHER */
#define BOP_Matrix2	0x0200
#define BOP_SignOff	0x1000

/* REGION FLAGS */
#define ROP_Appnd       0x0001
#define ROP_Cycle       0x0002
#define ROP_Label       0x0003
#define ROP_Omit        0x0004
#define ROP_Reset       0x0005
#define ROP_View        0x0006
#define ROP_Read        0x0007
#define ROP_Write       0x0008
#define ROP_Tody	0x0100
#define ROP_Eric	0x0200

/* DISPLAY ZOOMING  - use bits */
#define ZOP_ZPan	0x00ff
#define ZOP_unZPan	0xff00

#define ZOP_ZPan14	0x0020
#define ZOP_ZPan12	0x0010
#define ZOP_ZPan1	0x0001
#define ZOP_ZPan2	0x0002
#define ZOP_ZPan4	0x0004
#define ZOP_Center	0x0100

/* CURSOR TYPES */
#define COP_Shape	0x0f00
#define COP_Point	0x0100
#define COP_Polygon	0x0200
#define COP_Box		0x0300
#define COP_Circle	0x0400
#define COP_Ellipse	0x0500
#define COP_Arrow	0x0600
#define COP_Text	0x0700
#define COP_Bitmap	0x0800
#define COP_Annuli	0x1000
#define COP_PieSlice	0x2000
#define COP_SetOrigin	0x4000
#define COP_Orthogonal	0x8000
#define COP_Print	0x0010

/* ZOOMBOX SCOPE ZOOM */
#define XOP_Scope	0x000f
#define XOP_unScope	0xfff0

#define XOP_Scope1	0x0001
#define XOP_Scope2	0x0002
#define XOP_Scope4	0x0004
#define XOP_Scope8	0x0008
#define XOP_View	-1
#define XOP_Text	-2
