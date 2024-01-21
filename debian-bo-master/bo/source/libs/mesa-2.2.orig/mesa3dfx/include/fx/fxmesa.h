/*
 * Mesa 3-D graphics library
 * Version:  1.2
 * Copyright (C) 1995-1996  Brian Paul  (brianp@ssec.wisc.edu)
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
 */

/*
 * FXMesa
 *
 */

#ifndef FXMESA_H
#define FXMESA_H

#include <glide.h>

#ifdef __cplusplus
extern "C" {
#endif


typedef struct fx_mesa_context *fxMesaContext;


extern fxMesaContext fxMesaCreateContext(GrScreenResolution_t, GrScreenRefresh_t);

extern void fxMesaDestroyContext(fxMesaContext ctx);

extern void fxMesaMakeCurrent(fxMesaContext ctx);

extern fxMesaContext fxMesaGetCurrentContext( void );

extern void fxMesaSwapBuffers(void);

extern void fxMesaSetFar(float f);

#define glOrtho(a,b,c,d,n,f) { fxMesaSetFar(f); glOrtho(a,b,c,d,n,f); }
#define gluOrtho2D(a,b,c,d) { fxMesaSetFar(1.0); glOrtho(a,b,c,d,-1.0,1.0); }
#define glFrustrum(a,b,c,d,n,f) { fxMesaSetFar(f); gluPerspective(a,b,n,f); }
#define gluPerspective(a,b,n,f) { fxMesaSetFar(f); gluPerspective(a,b,n,f); }

#ifdef __cplusplus
}
#endif


#endif

