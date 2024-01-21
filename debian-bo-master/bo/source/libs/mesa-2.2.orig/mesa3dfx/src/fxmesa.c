/*
 * 3Dfx/Mesa interface.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * V0.13 - David Bucciarelli (tech.hmw@plus.it) Humanware s.r.l.
 *         now glBlendFunc() works for all glBlendFunc without DST_ALPHA (because the alpha buffer is not yet implemented) 
 *         now fxMesaCreateContext() accept resolution and refresh rate
 *         fixed a bug for texture mapping: the w (alias z) must be set also without depth buffer
 *         fixed a bug for texture image with width!=256
 *         written texparam()
 *         written all point, line and triangle functions for all possible supported contexts and
 *         the driver is slight faster with points, lines and small triangles
 *         fixed a small bug in fx/fxmesa.h (glOrtho)
 *
 * V0.12 - David Bucciarelli (tech.hmw@plus.it) Humanware s.r.l.
 *         glDepthFunc supported
 *         introduced a trick to discover the far plane distance (see fxMesaSetFar and fx/fxmesa.h)
 *         now the wbuffer works with homogeneous coordinate (and it doesn't work with a glOrtho projection :)
 *         solved several problems with homogeneous coordinate and texture mapping
 *         fixed a bug in all line functions
 *         fixed a clear framebuffer bug
 *         solved a display list/teximg problem (but use glBindTexture: it is several times faster)
 *
 * V0.11 - David Bucciarelli (tech.hmw@plus.it) Humanware s.r.l.
 *         introduced texture mapping support (not yet finished !)
 *         tested with Mesa2.2b6
 *         the driver is faster 
 *         written glFlush/glFinish
 *         the driver print a lot of info about the Glide lib
 *
 * v0.1  - David Bucciarelli (tech.hmw@plus.it) Humanware s.r.l.
 *         Initial revision
 *
 */

#ifdef FX

#include <stdio.h>
#include "context.h"
#include "types.h"
#include "xform.h"

#include "macros.h"
#include "vb.h"
#include "texture.h"

#include "fx/fxmesa.h"

#define MAXNUM_TEX		128

#define VSETXY(v,i) { (v).x = (float)VB->Win[i][0]; (v).y = (float)VB->Win[i][1]; }

#define VSETZ(v,i) { (v).oow = 1.0/(VB->Clip[i][3]*fxwscale); }

#define VSETST(v,i) {	(v).tmuvtx[0].sow = 255.0*(VB->TexCoord[i][0])*(v).oow; \
						(v).tmuvtx[0].tow = 255.0*(VB->TexCoord[i][1])*(v).oow; }

#define VSETRGBA(v,shift,i) {	(v).r = (float) (VB->Color[i][0]>>(shift)); \
								(v).g = (float) (VB->Color[i][1]>>(shift)); \
								(v).b = (float) (VB->Color[i][2]>>(shift)); \
								(v).a = (float) (VB->Color[i][3]>>(shift)); }

#define FIXCOORD(v)	{		long tmpx,tmpy; \
							tmpx = (v).x * 16; \
							tmpy = (v).y * 16; \
							(v).x = tmpx / 16.0; \
							(v).y = tmpy / 16.0; \
					}

#define FXCOLOR(r,g,b,a) (( ((unsigned int)(a))<<24 )|( ((unsigned int)(b))<<16 )|( ((unsigned int)(g))<<8 )|(r))


#define FUNC_DEPTH		0x01
#define FUNC_SMOOTH		0x02
#define FUNC_TEX_DECAL	0x04
#define FUNC_TEX_MOD	0x08

typedef struct {
	GrLOD_t smallLOD; 
	GrLOD_t largeLOD;
	int levelsdefined;
} texinfo;

struct fx_mesa_context {
   GLcontext *gl_ctx;		    /* the core Mesa context */
   GLvisual *gl_vis;		    /* describes the color buffer */
   GLframebuffer *gl_buffer;	/* the ancillary buffers */

   GLint width, height;		    /* size of color buffer */

   GrColor_t color;

   GrColor_t clearc;
   GrAlpha_t cleara;
};

static GrMipMapId_t texid[MAXNUM_TEX];
static texinfo ti[MAXNUM_TEX];

static fxMesaContext fxMesa = NULL;    /* the current context */

float fxwscale=1.0;

/**********************************************************************/
/*****                 Miscellaneous functions                    *****/
/**********************************************************************/

/* return buffer size information */
static void buffer_size(GLcontext *ctx, GLuint *width, GLuint *height)
{
   int colors;

#if defined(DEBUG_FXMESA)
   printf("fxmesa: buffer_size(...)\n");
#endif

   *width = fxMesa->width;
   *height = fxMesa->height;
}


/* Set current drawing color */
static void set_color(GLcontext *ctx, GLubyte red, GLubyte green,
                       GLubyte blue, GLubyte alpha )
{
#if defined(DEBUG_FXMESA)
   printf("fxmesa: set_color(%d,%d,%d,%d)\n",red,green,blue,alpha);
#endif

   fxMesa->color=FXCOLOR(red,green,blue,alpha);
}


/* implements glClearColor() */
static void clear_color(GLcontext *ctx, GLubyte red, GLubyte green,
                         GLubyte blue, GLubyte alpha )
{
#if defined(DEBUG_FXMESA)
   printf("fxmesa: clear_color(%d,%d,%d,%d)\n",red,green,blue,alpha);
#endif
 
   fxMesa->clearc=FXCOLOR(red,green,blue,255);
   fxMesa->cleara=alpha;
}


/* clear the frame buffer */
static void clear(GLcontext *ctx, GLboolean all,
                   GLint x, GLint y, GLint width, GLint height )
{

#if defined(DEBUG_FXMESA)
	printf("fxmesa: clear(%d,%d,%d,%d)\n",x,y,width,height);
#endif

	grDepthMask(FXFALSE);

   	grBufferClear(fxMesa->clearc,fxMesa->cleara,GR_WDEPTHVALUE_FARTHEST);

	if(ctx->Depth.Test)
		grDepthMask(FXTRUE);
}


/*  set the buffer used in double buffering */
static GLboolean set_buffer(GLcontext *ctx, GLenum mode )
{
#if defined(DEBUG_FXMESA)
   printf("fxmesa: set_buffer(%d)\n",mode);
#endif

   if (mode==GL_FRONT) {
     grRenderBuffer(GR_BUFFER_FRONTBUFFER);
   }
   else if (mode==GL_BACK) {
       grRenderBuffer(GR_BUFFER_BACKBUFFER);
   }
   else {
     return GL_FALSE;
   }
   
   return GL_TRUE;
}


/**********************************************************************/
/*****               Rasterization                                *****/
/**********************************************************************/

/*
 * This function is called to skip drawing points the driver doesn't know how to handle
 */ 
static void NULL_points(GLcontext *ctx, GLuint first, GLuint last)
{
#if defined(DEBUG_FXMESA)
   printf("fxmesa: NULL_points()\n");
#endif

   return;
}

static void rgba_flat_points(GLcontext *ctx, GLuint first, GLuint last)
{
#define FUNCNAME "rgba_flat_line"

#include "fxpnt.h"
}

static void rgba_smooth_points(GLcontext *ctx, GLuint first, GLuint last)
{
#define FUNCNAME "rgba_smooth_line"
#define PNT_SMOOTH

#include "fxpnt.h"
}

static void rgba_texdecal_points(GLcontext *ctx, GLuint first, GLuint last)
{
#define FUNCNAME "rgba_texdecal_line"
#define PNT_TEX

#include "fxpnt.h"
}

static void rgba_texmod_smooth_points(GLcontext *ctx, GLuint first, GLuint last)
{
#define FUNCNAME "rgba_texmod_smooth_line"
#define PNT_SMOOTH
#define PNT_TEX

#include "fxpnt.h"
}

static void rgba_texmod_flat_points(GLcontext *ctx, GLuint first, GLuint last)
{
#define FUNCNAME "rgba_texmod_flat_line"
#define PNT_TEX
#define PNT_DEPTH

#include "fxpnt.h"
}

static void zrgba_flat_points(GLcontext *ctx, GLuint first, GLuint last)
{
#define FUNCNAME "zrgba_flat_line"
#define PNT_DEPTH

#include "fxpnt.h"
}

static void zrgba_smooth_points(GLcontext *ctx, GLuint first, GLuint last)
{
#define FUNCNAME "zrgba_smooth_line"
#define PNT_SMOOTH
#define PNT_DEPTH

#include "fxpnt.h"
}

static void zrgba_texdecal_points(GLcontext *ctx, GLuint first, GLuint last)
{
#define FUNCNAME "zrgba_texdecal_line"
#define PNT_TEX
#define PNT_DEPTH

#include "fxpnt.h"
}

static void zrgba_texmod_smooth_points(GLcontext *ctx, GLuint first, GLuint last)
{
#define FUNCNAME "zrgba_texmod_smooth_line"
#define PNT_SMOOTH
#define PNT_TEX
#define PNT_DEPTH

#include "fxpnt.h"
}

static void zrgba_texmod_flat_points(GLcontext *ctx, GLuint first, GLuint last)
{
#define FUNCNAME "zrgba_texmod_flat_line"
#define PNT_TEX
#define PNT_DEPTH

#include "fxpnt.h"
}

static points_func choose_points_function(GLcontext *ctx)
{
   int t=0;
   
#if defined(DEBUG_FXMESA)
   printf("fxmesa: choose_points_function()\n");
#endif

   if(ctx->RenderMode!=GL_RENDER) 
     return NULL;
   
   if(ctx->Texture.Enabled) {
	   switch(ctx->Texture.EnvMode) {
	   case GL_DECAL:
		   t|=FUNC_TEX_DECAL;
		   break;
	   case GL_MODULATE:
		   t|=FUNC_TEX_MOD;
		   break;
	   case GL_BLEND:
		   /* TO DO */
		   return NULL_points;
		   break;
	   }
   }
   
   if(!ctx->MonoPixels)
	   t|=FUNC_SMOOTH;
   
   if(ctx->Depth.Test || ctx->Fog.Enabled)
	   t|=FUNC_DEPTH;

   switch(t) {
   case 0:
	   return rgba_flat_points;
	   break;
   case FUNC_DEPTH:
	   return zrgba_flat_points;
	   break;
   case FUNC_SMOOTH:
	   return rgba_smooth_points;
	   break;
   case FUNC_TEX_DECAL:
   case FUNC_SMOOTH|FUNC_TEX_DECAL:
		 return rgba_texdecal_points;
	   break;
	 case FUNC_DEPTH|FUNC_TEX_DECAL:
   case FUNC_SMOOTH|FUNC_DEPTH|FUNC_TEX_DECAL:
	   return zrgba_texdecal_points;
	   break;
   case FUNC_TEX_MOD:
	   return rgba_texmod_flat_points;
	   break;
   case FUNC_SMOOTH|FUNC_DEPTH:
	   return zrgba_smooth_points;
	   break;
   case FUNC_SMOOTH|FUNC_TEX_MOD:
	   return rgba_texmod_smooth_points;
	   break;
   case FUNC_DEPTH|FUNC_TEX_MOD:
	   return zrgba_texmod_flat_points;
	   break;
   case FUNC_SMOOTH|FUNC_DEPTH|FUNC_TEX_MOD:
	   return zrgba_texmod_smooth_points;
	   break;
   }

   return NULL_points;
}

/************************************************************************/
/*********************  lines  ******************************************/
/************************************************************************/

/* 
 * this function is called for lines that aren't supported by the driver
 */
static void NULL_line(GLcontext *ctx, GLuint v1, GLuint v2, GLuint pv)
{
#if defined(DEBUG_FXMESA)
	printf("fxmesa: NULL_line()\n");
#endif

	return;
}

static void rgba_flat_line(GLcontext *ctx, GLuint v1, GLuint v2, GLuint pv)
{
#define FUNCNAME "rgba_flat_line"

#include "fxline.h"
}

static void rgba_smooth_line(GLcontext *ctx, GLuint v1, GLuint v2, GLuint pv)
{
#define FUNCNAME "rgba_smooth_line"
#define LN_SMOOTH

#include "fxline.h"
}

static void rgba_texdecal_line(GLcontext *ctx, GLuint v1, GLuint v2, GLuint pv)
{
#define FUNCNAME "rgba_texdecal_line"
#define LN_TEX

#include "fxline.h"
}

static void rgba_texmod_smooth_line(GLcontext *ctx, GLuint v1, GLuint v2, GLuint pv)
{
#define FUNCNAME "rgba_texmod_smooth_line"
#define LN_SMOOTH
#define LN_TEX

#include "fxline.h"
}

static void rgba_texmod_flat_line(GLcontext *ctx, GLuint v1, GLuint v2, GLuint pv)
{
#define FUNCNAME "rgba_texmod_flat_line"
#define LN_TEX
#define LN_DEPTH

#include "fxline.h"
}

static void zrgba_flat_line(GLcontext *ctx, GLuint v1, GLuint v2, GLuint pv)
{
#define FUNCNAME "zrgba_flat_line"
#define LN_DEPTH

#include "fxline.h"
}

static void zrgba_smooth_line(GLcontext *ctx, GLuint v1, GLuint v2, GLuint pv)
{
#define FUNCNAME "zrgba_smooth_line"
#define LN_SMOOTH
#define LN_DEPTH

#include "fxline.h"
}

static void zrgba_texdecal_line(GLcontext *ctx, GLuint v1, GLuint v2, GLuint pv)
{
#define FUNCNAME "zrgba_texdecal_line"
#define LN_TEX
#define LN_DEPTH

#include "fxline.h"
}

static void zrgba_texmod_smooth_line(GLcontext *ctx, GLuint v1, GLuint v2, GLuint pv)
{
#define FUNCNAME "zrgba_texmod_smooth_line"
#define LN_SMOOTH
#define LN_TEX
#define LN_DEPTH

#include "fxline.h"
}

static void zrgba_texmod_flat_line(GLcontext *ctx, GLuint v1, GLuint v2, GLuint pv)
{
#define FUNCNAME "zrgba_texmod_flat_line"
#define LN_TEX
#define LN_DEPTH

#include "fxline.h"
}

static line_func choose_line_function(GLcontext *ctx)
{
   int t=0;
   
#if defined(DEBUG_FXMESA)
   printf("fxmesa: choose_line_function()\n");
#endif

   if(ctx->RenderMode!=GL_RENDER) 
     return NULL;
   
   if(ctx->Texture.Enabled) {
	   switch(ctx->Texture.EnvMode) {
	   case GL_DECAL:
		   t|=FUNC_TEX_DECAL;
		   break;
	   case GL_MODULATE:
		   t|=FUNC_TEX_MOD;
		   break;
	   case GL_BLEND:
		   /* TO DO */
		   return NULL_line;
		   break;
	   }
   }
   
   if(ctx->Light.ShadeModel==GL_SMOOTH)
	   t|=FUNC_SMOOTH;
   
   if(ctx->Depth.Test || ctx->Fog.Enabled)
	   t|=FUNC_DEPTH;

   switch(t) {
   case 0:
	   return rgba_flat_line;
	   break;
   case FUNC_DEPTH:
	   return zrgba_flat_line;
	   break;
   case FUNC_SMOOTH:
	   return rgba_smooth_line;
	   break;
   case FUNC_TEX_DECAL:
   case FUNC_SMOOTH|FUNC_TEX_DECAL:
		 return rgba_texdecal_line;
	   break;
	 case FUNC_DEPTH|FUNC_TEX_DECAL:
   case FUNC_SMOOTH|FUNC_DEPTH|FUNC_TEX_DECAL:
	   return zrgba_texdecal_line;
	   break;
   case FUNC_TEX_MOD:
	   return rgba_texmod_flat_line;
	   break;
   case FUNC_SMOOTH|FUNC_DEPTH:
	   return zrgba_smooth_line;
	   break;
   case FUNC_SMOOTH|FUNC_TEX_MOD:
	   return rgba_texmod_smooth_line;
	   break;
   case FUNC_DEPTH|FUNC_TEX_MOD:
	   return zrgba_texmod_flat_line;
	   break;
   case FUNC_SMOOTH|FUNC_DEPTH|FUNC_TEX_MOD:
	   return zrgba_texmod_smooth_line;
	   break;
   }

   return NULL_line;
}

/************************************************************************/
/*********************  triangles  **************************************/
/************************************************************************/

/* 
 * this function is called for triangles we don't support
 */
static void NULL_triangle(GLcontext *ctx, GLuint v1, GLuint v2, GLuint v3, GLuint pv)
{
#if defined(DEBUG_FXMESA)
   printf("fxmesa: NULL_triangle()\n");
#endif

  return;
}

static void rgba_flat_triangle(GLcontext *ctx, GLuint v1, GLuint v2, GLuint v3, GLuint pv)
{
#define FUNCNAME "rgba_flat_triangle"

#include "fxtri.h"
}

static void rgba_smooth_triangle(GLcontext *ctx, GLuint v1, GLuint v2, GLuint v3, GLuint pv)
{
#define FUNCNAME "rgba_smooth_triangle"
#define TRI_SMOOTH

#include "fxtri.h"
}

static void rgba_texdecal_triangle(GLcontext *ctx, GLuint v1, GLuint v2, GLuint v3, GLuint pv)
{
#define FUNCNAME "rgba_texdecal_triangle"
#define TRI_TEX

#include "fxtri.h"
}

static void rgba_texmod_smooth_triangle(GLcontext *ctx, GLuint v1, GLuint v2, GLuint v3, GLuint pv)
{
#define FUNCNAME "rgba_texmod_smooth_triangle"
#define TRI_SMOOTH
#define TRI_TEX

#include "fxtri.h"
}

static void rgba_texmod_flat_triangle(GLcontext *ctx, GLuint v1, GLuint v2, GLuint v3, GLuint pv)
{
#define FUNCNAME "rgba_texmod_flat_triangle"
#define TRI_TEX
#define TRI_DEPTH

#include "fxtri.h"
}

static void zrgba_flat_triangle(GLcontext *ctx, GLuint v1, GLuint v2, GLuint v3, GLuint pv)
{
#define FUNCNAME "zrgba_flat_triangle"
#define TRI_DEPTH

#include "fxtri.h"
}

static void zrgba_smooth_triangle(GLcontext *ctx, GLuint v1, GLuint v2, GLuint v3, GLuint pv)
{
#define FUNCNAME "zrgba_smooth_triangle"
#define TRI_SMOOTH
#define TRI_DEPTH

#include "fxtri.h"
}

static void zrgba_texdecal_triangle(GLcontext *ctx, GLuint v1, GLuint v2, GLuint v3, GLuint pv)
{
#define FUNCNAME "zrgba_texdecal_triangle"
#define TRI_TEX
#define TRI_DEPTH

#include "fxtri.h"
}

static void zrgba_texmod_smooth_triangle(GLcontext *ctx, GLuint v1, GLuint v2, GLuint v3, GLuint pv)
{
#define FUNCNAME "zrgba_texmod_smooth_triangle"
#define TRI_SMOOTH
#define TRI_TEX
#define TRI_DEPTH

#include "fxtri.h"
}

static void zrgba_texmod_flat_triangle(GLcontext *ctx, GLuint v1, GLuint v2, GLuint v3, GLuint pv)
{
#define FUNCNAME "zrgba_texmod_flat_triangle"
#define TRI_TEX
#define TRI_DEPTH

#include "fxtri.h"
}

static triangle_func choose_triangle_function(GLcontext *ctx)
{
   int t=0;

#if defined(DEBUG_FXMESA)
   printf("fxmesa: choose_triangle_function(...)\n");
#endif

   if(ctx->RenderMode!=GL_RENDER) 
     return NULL;
   
   if(ctx->Texture.Enabled) {
	   switch(ctx->Texture.EnvMode) {
	   case GL_DECAL:
		   t|=FUNC_TEX_DECAL;
		   break;
	   case GL_MODULATE:
		   t|=FUNC_TEX_MOD;
		   break;
	   case GL_BLEND:
		   /* TO DO */
		   return NULL_triangle;
		   break;
	   }
   }
   
   if(ctx->Light.ShadeModel==GL_SMOOTH)
	   t|=FUNC_SMOOTH;
   
   if(ctx->Depth.Test || ctx->Fog.Enabled)
	   t|=FUNC_DEPTH;

   switch(t) {
   case 0:
	   return rgba_flat_triangle;
	   break;
   case FUNC_DEPTH:
	   return zrgba_flat_triangle;
	   break;
   case FUNC_SMOOTH:
	   return rgba_smooth_triangle;
	   break;
   case FUNC_TEX_DECAL:
   case FUNC_SMOOTH|FUNC_TEX_DECAL:
		 return rgba_texdecal_triangle;
	   break;
	 case FUNC_DEPTH|FUNC_TEX_DECAL:
   case FUNC_SMOOTH|FUNC_DEPTH|FUNC_TEX_DECAL:
	   return zrgba_texdecal_triangle;
	   break;
   case FUNC_TEX_MOD:
	   return rgba_texmod_flat_triangle;
	   break;
   case FUNC_SMOOTH|FUNC_DEPTH:
	   return zrgba_smooth_triangle;
	   break;
   case FUNC_SMOOTH|FUNC_TEX_MOD:
	   return rgba_texmod_smooth_triangle;
	   break;
   case FUNC_DEPTH|FUNC_TEX_MOD:
	   return zrgba_texmod_flat_triangle;
	   break;
   case FUNC_SMOOTH|FUNC_DEPTH|FUNC_TEX_MOD:
	   return zrgba_texmod_smooth_triangle;
	   break;
   }

   return NULL_triangle;
}

/************************************************************************/
/**************** 3D depth buffer functions *****************************/
/************************************************************************/

/* this is a no-op, since the z-buffer is in hardware */
static void alloc_depth_buffer(GLcontext *ctx)
{
#if defined(DEBUG_FXMESA)
   printf("fxmesa: alloc_depth_buffer()\n");
#endif

	 	grDepthMask(FXTRUE);
}

static void clear_depth_buffer(GLcontext *ctx)
{
#if defined(DEBUG_FXMESA)
	printf("fxmesa: clear()\n");
#endif

	grDepthMask(FXTRUE);
	grColorMask(FXFALSE,FXFALSE);

  	/* I don't know how to convert ctx->Depth.Clear */
	grBufferClear(fxMesa->clearc,fxMesa->cleara,GR_WDEPTHVALUE_FARTHEST);

	grColorMask(FXTRUE,FXFALSE);
}

/************************************************************************/
/************************************************************************/
/************************************************************************/

static void finish(GLcontext *ctx)
{
	grSstIdle();
}

/************************************************************************/
/*************************** Texture Mapping ****************************/
/************************************************************************/

static void texenv(GLcontext *ctx, GLenum pname, const GLfloat *param)
{
	/* ;) */
}

static void texparam(GLcontext *ctx, GLenum target, GLuint texObject,
					 GLenum pname, const GLfloat *params)
{
	GLenum param=(GLenum)(GLint)params[0];

#if defined(DEBUG_FXMESA)
	printf("fxmesa: texparam(%d)\n",texObject);
#endif

	if(target!=GL_TEXTURE_2D) {
#ifndef FX_SILENT
		fprintf(stderr,"3Dfx Driver: unsupported texture in texparam()\n");
#endif
		return;
	}

	switch(pname) {

		case GL_TEXTURE_MIN_FILTER:
			switch(param) {
			case GL_NEAREST:
				guTexChangeAttributes(texid[texObject],-1,-1,-1,GR_MIPMAP_DISABLE,-1,-1,-1,-1,-1,GR_TEXTUREFILTER_POINT_SAMPLED,-1);
				break;
			case GL_LINEAR:
				guTexChangeAttributes(texid[texObject],-1,-1,-1,GR_MIPMAP_DISABLE,-1,-1,-1,-1,-1,GR_TEXTUREFILTER_BILINEAR,-1);
				break;
			case GL_NEAREST_MIPMAP_NEAREST:
				guTexChangeAttributes(texid[texObject],-1,-1,-1,GR_MIPMAP_NEAREST,-1,-1,-1,-1,-1,GR_TEXTUREFILTER_POINT_SAMPLED,-1);
				break;
			case GL_NEAREST_MIPMAP_LINEAR:
				guTexChangeAttributes(texid[texObject],-1,-1,-1,GR_MIPMAP_NEAREST,-1,-1,-1,-1,-1,GR_TEXTUREFILTER_BILINEAR,-1);
				break;
			case GL_LINEAR_MIPMAP_LINEAR:
				guTexChangeAttributes(texid[texObject],-1,-1,-1,GR_MIPMAP_NEAREST_DITHER,-1,-1,-1,-1,-1,GR_TEXTUREFILTER_BILINEAR,-1);
				break;
			}
			break;

		case GL_TEXTURE_MAG_FILTER:
			switch(param) {
			case GL_NEAREST:
				guTexChangeAttributes(texid[texObject],-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,GR_TEXTUREFILTER_POINT_SAMPLED);
				break;
			case GL_LINEAR:
				guTexChangeAttributes(texid[texObject],-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,GR_TEXTUREFILTER_BILINEAR);
				break;
			}
			break;

		case GL_TEXTURE_WRAP_S:
			switch(param) {
			case GL_CLAMP:
				guTexChangeAttributes(texid[texObject],-1,-1,-1,-1,-1,-1,-1,GR_TEXTURECLAMP_CLAMP,-1,-1,-1);
				break;
			case GL_REPEAT:
				guTexChangeAttributes(texid[texObject],-1,-1,-1,-1,-1,-1,-1,GR_TEXTURECLAMP_WRAP,-1,-1,-1);
				break;
			}
			break;

		case GL_TEXTURE_WRAP_T:
			switch(param) {
			case GL_CLAMP:
				guTexChangeAttributes(texid[texObject],-1,-1,-1,-1,-1,-1,-1,-1,GR_TEXTURECLAMP_CLAMP,-1,-1);
				break;
			case GL_REPEAT:
				guTexChangeAttributes(texid[texObject],-1,-1,-1,-1,-1,-1,-1,-1,GR_TEXTURECLAMP_WRAP,-1,-1);
				break;
			}
			break;

		case GL_TEXTURE_BORDER_COLOR:
			/* TO DO */
			break;
	}

	guTexSource(texid[texObject]);
}

static void texdel(GLcontext *ctx, GLuint texObject)
{
	/* ;) */
}

static void texbind(GLcontext *ctx, GLenum target, GLuint texObject)
{
#if defined(DEBUG_FXMESA)
	printf("fxmesa: texbind(%d)\n",texObject);
#endif

	guTexSource(texid[texObject]);
}

static int logbase2(int n)
{
   GLint i = 1;
   GLint log2 = 0;

   if (n<0) {
      return -1;
   }

   while ( n > i ) {
      i *= 2;
      log2++;
   }
   if (i != n) {
      return -1;
   }
   else {
      return log2;
   }
}

static void texalloc(GLcontext *ctx, GLuint texObject, int w, int h)
{
	static GrLOD_t lod[9]={GR_LOD_256,GR_LOD_128,GR_LOD_64,GR_LOD_32,GR_LOD_16,GR_LOD_8,GR_LOD_4,GR_LOD_2,GR_LOD_1};
	GrLOD_t l;

	l=lod[8-logbase2(w)];

	texid[texObject]=guTexAllocateMemory(GR_TMU0,GR_MIPMAPLEVELMASK_BOTH,w,h,
										GR_TEXFMT_RGB_565, GR_MIPMAP_NEAREST,l,l,GR_ASPECT_1x1,
										GR_TEXTURECLAMP_WRAP,GR_TEXTURECLAMP_WRAP,GR_TEXTUREFILTER_BILINEAR,
										GR_TEXTUREFILTER_BILINEAR,0.0,FXFALSE);

	if(texid[texObject]==GR_NULL_MIPMAP_HANDLE) {
		fprintf(stderr,"3Dfx Driver: out of texture memory !\n");
		grGlideShutdown();
		exit(-1);
	}

	ti[texObject].smallLOD=l; 
	ti[texObject].largeLOD=l;
	ti[texObject].levelsdefined=0;
}

static int texsupported(GLenum target, GLint internalFormat, const struct gl_texture_image *image)
{
	if(target!=GL_TEXTURE_2D)
		return GL_FALSE;

/*	if(internalFormat!=GL_RGB)
		return GL_FALSE;*/

	if((image->Width!=image->Height) || (image->Width>256))
		return GL_FALSE;

	if(logbase2(image->Width)==(-1))
		return GL_FALSE;

	return GL_TRUE;

}

static void teximg(GLcontext *ctx, GLenum target,
				   GLuint texObject, GLint level, GLint internalFormat,
				   const struct gl_texture_image *image)
{
	unsigned short *src,*srctxpy;
	int x,y;
	unsigned char r,g,b,*data;
	static GrLOD_t lod[9]={GR_LOD_256,GR_LOD_128,GR_LOD_64,GR_LOD_32,GR_LOD_16,GR_LOD_8,GR_LOD_4,GR_LOD_2,GR_LOD_1};
	GrLOD_t lodlev;

#if defined(DEBUG_FXMESA)
	printf("fxmesa: teximg(...,%d,%d,%d,%d...)\n",target,internalFormat,image->Width,image->Height);
#endif

	if(texsupported(target,internalFormat,image)) {
		if(texid[texObject]==GR_NULL_MIPMAP_HANDLE)
			texalloc(ctx,texObject,image->Width,image->Height);

		if(!(ti[texObject].levelsdefined & (1<<level))) {
			if(!(srctxpy=src=(unsigned short *)malloc(sizeof(unsigned short)*image->Width*image->Height))) {
				fprintf(stderr,"3Dfx Driver: out of memory !\n");
				grGlideShutdown();
				exit(-1);
			}

			data=image->Data;
			for(y=0;y<image->Height;y++)
				for(x=0;x<image->Width;x++) {
					r=data[(x+y*image->Height)*3];
					g=data[(x+y*image->Height)*3+1];
					b=data[(x+y*image->Height)*3+2];

					src[x+y*image->Height]=(unsigned short)
						( ((unsigned short)0xf8 & r) <<(11-3)) |
						( ((unsigned short)0xfc & g) <<(5-3+1)) |
						( ((unsigned short)0xf8 & b) >> 3); 
				}

			lodlev=lod[8-logbase2(image->Width)];

			if(lodlev>ti[texObject].smallLOD) {
				guTexChangeAttributes(texid[texObject],-1,-1,-1,-1,lodlev,-1,-1,-1,-1,-1,-1);
				ti[texObject].smallLOD=lodlev;
			}

			if(lodlev<ti[texObject].largeLOD) {
				guTexChangeAttributes(texid[texObject],-1,-1,-1,-1,-1,lodlev,-1,-1,-1,-1,-1);
				ti[texObject].largeLOD=lodlev;
			}

			guTexDownloadMipMapLevel(texid[texObject],lodlev,&src);

			free(srctxpy);

			ti[texObject].levelsdefined|=(1<<level);
		}

		guTexSource(texid[texObject]);
	}
#ifndef FX_SILENT
	else
		fprintf(stderr,"3Dfx Driver: unsupported texture in teximg()\n");
#endif

}

/************************************************************************/
/************************************************************************/
/************************************************************************/

static void setup_fx_units(GLcontext *ctx)
{
	GrAlphaBlendFnc_t sfact,dfact;

	if(ctx->Color.BlendEnabled) {
		if(ctx->Light.ShadeModel==GL_SMOOTH)
			guAlphaSource(GR_ALPHASOURCE_ITERATED_ALPHA);
		else
			guAlphaSource(GR_ALPHASOURCE_CC_ALPHA);

		switch(ctx->Color.BlendSrc) {
		case GL_ZERO:
			sfact=GR_BLEND_ZERO;
			break;
		case GL_ONE:
			sfact=GR_BLEND_ONE;
			break;
		case GL_DST_COLOR:
			sfact=GR_BLEND_DST_COLOR;
			break;
		case GL_ONE_MINUS_DST_COLOR:
			sfact=GR_BLEND_ONE_MINUS_DST_COLOR;
			break;
		case GL_SRC_ALPHA:
			sfact=GR_BLEND_SRC_ALPHA;
			break;
		case GL_ONE_MINUS_SRC_ALPHA:
			sfact=GR_BLEND_ONE_MINUS_SRC_ALPHA;
			break;
		case GL_SRC_ALPHA_SATURATE:
			sfact=GR_BLEND_ALPHA_SATURATE;
			break;
		case GL_SRC_COLOR:
		case GL_ONE_MINUS_SRC_COLOR:
		case GL_DST_ALPHA:
		case GL_ONE_MINUS_DST_ALPHA:
			/* USELESS or TO DO */
			sfact=GR_BLEND_ONE;
			break;
		}

		switch(ctx->Color.BlendDst) {
		case GL_ZERO:
			dfact=GR_BLEND_ZERO;
			break;
		case GL_ONE:
			dfact=GR_BLEND_ONE;
			break;
		case GL_SRC_COLOR:
			dfact=GR_BLEND_SRC_COLOR;
			break;
		case GL_ONE_MINUS_SRC_COLOR:
			dfact=GR_BLEND_ONE_MINUS_SRC_COLOR;
			break;
		case GL_SRC_ALPHA:
			dfact=GR_BLEND_SRC_ALPHA;
			break;
		case GL_ONE_MINUS_SRC_ALPHA:
			dfact=GR_BLEND_ONE_MINUS_SRC_ALPHA;
			break;
		case GL_SRC_ALPHA_SATURATE:
		case GL_DST_COLOR:
		case GL_ONE_MINUS_DST_COLOR:
		case GL_DST_ALPHA:
		case GL_ONE_MINUS_DST_ALPHA:
			/* USELESS or TO DO */
			sfact=GR_BLEND_ZERO;
			break;
		}

		grAlphaBlendFunction(sfact,dfact,GR_BLEND_ONE,GR_BLEND_ZERO);

		grColorMask(FXTRUE,FXFALSE);
	} else {
		grAlphaBlendFunction(GR_BLEND_ONE,GR_BLEND_ZERO,GR_BLEND_ONE,GR_BLEND_ZERO);
		grColorMask(FXTRUE,FXFALSE);
	}

	if(ctx->Texture.Enabled) {
		switch(ctx->Texture.EnvMode) {
		case GL_DECAL:
			guAlphaSource(GR_ALPHASOURCE_CC_ALPHA);
			guColorCombineFunction(GR_COLORCOMBINE_DECAL_TEXTURE);
			grTexCombineFunction(GR_TMU0,GR_TEXTURECOMBINE_DECAL);
			break;
		case GL_MODULATE:
			if(ctx->Light.ShadeModel==GL_SMOOTH)
				guColorCombineFunction(GR_COLORCOMBINE_TEXTURE_TIMES_ITRGB);
			else
				guColorCombineFunction(GR_COLORCOMBINE_TEXTURE_TIMES_CCRGB);

			grTexCombineFunction(GR_TMU0,GR_TEXTURECOMBINE_DECAL);
			break;
		case GL_BLEND:
			/* TO DO */
			break;
		}
	} else {
		if(ctx->Light.ShadeModel==GL_SMOOTH)
			guColorCombineFunction(GR_COLORCOMBINE_ITRGB);
		else
			guColorCombineFunction(GR_COLORCOMBINE_CCRGB);
				 
		grTexCombineFunction(GR_TMU0,GR_TEXTURECOMBINE_ZERO);
	}

	if(ctx->Depth.Test) {
		grDepthBufferMode(GR_DEPTHBUFFER_WBUFFER); 

		switch(ctx->Depth.Func) {
		case GL_NEVER:
			grDepthBufferFunction(GR_CMP_NEVER);
   		break;
		case GL_LESS:
			grDepthBufferFunction(GR_CMP_LESS);
   		break;
		case GL_GEQUAL:
			grDepthBufferFunction(GR_CMP_GEQUAL);
   		break;
		case GL_LEQUAL:
			grDepthBufferFunction(GR_CMP_LEQUAL);
   		break;
		case GL_GREATER:
   		grDepthBufferFunction(GR_CMP_GREATER);
  		break;
		case GL_NOTEQUAL:
			grDepthBufferFunction(GR_CMP_NOTEQUAL);
   		break;
		case GL_EQUAL:
			grDepthBufferFunction(GR_CMP_EQUAL);
   		break;
		case GL_ALWAYS:
			grDepthBufferFunction(GR_CMP_ALWAYS);
			break;
		}

		grDepthMask(FXTRUE);
	} else {
		grDepthBufferFunction(GR_CMP_ALWAYS);
		grDepthMask(FXFALSE);
	}
}


static void fx_mesa_setup_dd_pointers(GLcontext *ctx)
{
#if defined(DEBUG_FXMESA)
   printf("fxmesa: fx_mesa_setup_dd_pointers()\n");
#endif

   ctx->Driver.UpdateState = fx_mesa_setup_dd_pointers;

   ctx->Driver.ClearIndex = NULL;
   ctx->Driver.ClearColor = clear_color;
   ctx->Driver.Clear = clear;

   ctx->Driver.Index = NULL;
   ctx->Driver.Color = set_color;

   ctx->Driver.SetBuffer = set_buffer;
   ctx->Driver.GetBufferSize = buffer_size;

   ctx->Driver.AllocDepthBuffer = alloc_depth_buffer;
   ctx->Driver.ClearDepthBuffer = clear_depth_buffer;

   /* acc. functions*/

	 setup_fx_units(ctx);

   ctx->Driver.PointsFunc = choose_points_function(ctx);
   ctx->Driver.LineFunc = choose_line_function(ctx);
   ctx->Driver.TriangleFunc = choose_triangle_function(ctx);

   ctx->Driver.Finish=ctx->Driver.Flush=finish;

   ctx->Driver.TexEnv=texenv;
   ctx->Driver.TexImage=teximg;
   ctx->Driver.TexParameter=texparam;
   ctx->Driver.BindTexture=texbind;
   ctx->Driver.DeleteTexture=texdel;

   ctx->Driver.WriteColorSpan       = NULL;
   ctx->Driver.WriteMonocolorSpan   = NULL;
   ctx->Driver.WriteColorPixels     = NULL;
   ctx->Driver.WriteMonocolorPixels = NULL;
   ctx->Driver.WriteIndexSpan       = NULL;
   ctx->Driver.WriteMonoindexSpan   = NULL;
   ctx->Driver.WriteIndexPixels     = NULL;
   ctx->Driver.WriteMonoindexPixels = NULL;

   ctx->Driver.ReadIndexSpan = NULL;
   ctx->Driver.ReadColorSpan = NULL;
   ctx->Driver.ReadIndexPixels = NULL;
   ctx->Driver.ReadColorPixels = NULL;
}


void fxMesaSetFar(float f)
{
	fxwscale=65535.0/fabs(f);
}


/*
 * Create a new FX/Mesa context and return a handle to it.
 */
fxMesaContext fxMesaCreateContext(GrScreenResolution_t res, GrScreenRefresh_t ref)
{
	fxMesaContext ctx;
	GrHwConfiguration hwconfig;
	int i;

#if defined(DEBUG_FXMESA)
	printf("fxmesa: fxMesaCreateContext()\n");
#endif

#ifndef FX_SILENT
	fprintf(stderr,"Mesa 3Dfx Voodoo Device Driver V0.11\nWritten by David Bucciarelli (tech.hmw@plus.it)\n");
#endif

	grGlideInit();
	if (grSstQueryHardware(&hwconfig)) {
#ifndef FX_SILENT
		char buf[80];
		grGlideGetVersion(buf);
#endif
		grSstSelect(0);
		if(!grSstOpen(res, ref, GR_COLORFORMAT_ABGR, GR_ORIGIN_LOWER_LEFT, GR_SMOOTHING_ENABLE, 2))
			return NULL;

#ifndef FX_SILENT
		fprintf(stderr,"Using %s\nNumber of boards: %d\nGlide screen size: %dx%d\n",buf,hwconfig.num_sst,grSstScreenWidth(),grSstScreenHeight());
#endif
	} else {
		printf("ERROR: no Voodoo Graphics!\n");

		return NULL;
	}

  ctx=(fxMesaContext)malloc(sizeof(struct fx_mesa_context));
  if(!ctx)
		return NULL;


  ctx->width=grSstScreenWidth();
  ctx->height=grSstScreenHeight();

	grColorMask(FXTRUE,FXFALSE);

	for(i=0;i<MAXNUM_TEX;i++)
		texid[i]=GR_NULL_MIPMAP_HANDLE;

	 ctx->gl_vis = gl_create_visual( GL_TRUE,
																	 GL_FALSE,
																	 GL_TRUE,
																	 16,   /* depth_size */
																	 8,    /* stencil_size */
																	 16,   /* accum_size */
																	 0,
																	 255.0,
																	 255.0,
																	 255.0,
																	 255.0 );

	 ctx->gl_ctx = gl_create_context( ctx->gl_vis,
																		NULL,  /* share list context */
																		(void *) ctx
																	);

	 ctx->gl_buffer = gl_create_framebuffer( ctx->gl_vis );

	 return ctx;
}


/*
 * Destroy the given FX/Mesa context.
 */
void fxMesaDestroyContext( fxMesaContext ctx )
{

#if defined(DEBUG_FXMESA)
	printf("fxmesa: fxMesaDestroyContext()\n");
#endif

	if (ctx) {
		gl_destroy_visual( ctx->gl_vis );
		gl_destroy_context( ctx->gl_ctx );
		gl_destroy_framebuffer( ctx->gl_buffer );
		free( ctx );
		if (ctx==fxMesa) {
#ifndef FX_SILENT
			GrSstPerfStats_t st;

			grSstPerfStats(&st);

			fprintf(stderr,"# pixels processed (minus buffer clears): %u\n",st.pixelsIn);
			fprintf(stderr,"# pixels not drawn due to chroma key test failure: %u\n",st.chromaFail);
			fprintf(stderr,"# pixels not drawn due to depth test failure: %u\n",st.zFuncFail);
			fprintf(stderr,"# pixels not drawn due to alpha test failure: %u\n",st.aFuncFail);
			fprintf(stderr,"# pixels drawn (including buffer clears and LFB writes): %u\n",st.pixelsOut);
			fprintf(stderr,"Free texture memory: %u bytes\n",guTexMemQueryAvail(GR_TMU0));
#endif

			fxMesa = NULL;
			grGlideShutdown();
      }
   }
}



/*
 * Make the specified FX/Mesa context the current one.
 */
void fxMesaMakeCurrent( fxMesaContext ctx )
{

#if defined(DEBUG_FXMESA)
   printf("fxmesa: fxMesaMakeCurrent()\n");
#endif

   fxMesa = ctx;
   gl_make_current( ctx->gl_ctx, ctx->gl_buffer );

   fx_mesa_setup_dd_pointers(ctx->gl_ctx);   
   gl_Viewport(ctx->gl_ctx, 0, 0, ctx->width, ctx->height );
}


/*
 * Return a handle to the current FX/Mesa context.
 */
fxMesaContext fxMesaGetCurrentContext( void )
{

#if defined(DEBUG_FXMESA)
   printf("fxmesa: fxMesaContext()\n");
#endif

   return fxMesa;
}


/*
 * Swap front/back buffers for current context if double buffered.
 */
void fxMesaSwapBuffers(void)
{
#if defined(DEBUG_FXMESA)
   printf("fxmesa: fxMesaSwapBuffers()\n");
#endif

	grBufferSwap(1);
}

#else

/*
 * Need this to provide at least one external definition.
 */

int gl_fx_dummy_function(void)
{
   return 0;
}

#endif  /* FX */
