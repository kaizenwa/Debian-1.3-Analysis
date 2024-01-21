
/*
 * XKOBO, a video-oriented game
 * Copyright (C) 1995,1996  Akira Higuchi
 *     a-higuti@math.hokudai.ac.jp
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 * 
 */

#include "xlchip.h"
     
win_chip::win_chip()
{
    int i;
    set_gc = NULL;
    and_gc = NULL;
    or_gc = NULL;
    csx = 0;
    csy = 0;
    p_store = 0;
    chip = 0;
    chip_mask = 0;
    for (i=0; i<SPRITE_MAX; i++){
         sprite[i].cx = 0;
         sprite[i].cy = 0;
         sprite[i].x = 0;
         sprite[i].y = 0;
         sprite[i].h = 0;
         sprite[i].v = 0;
    }
    spr_max = sprite;
    store_policy = 0;
}

win_chip::~win_chip()
{
    if (or_gc){
        DBGP(or_gc);
        XFreeGC(disp, or_gc);
    }
    if (and_gc){
        DBGP(and_gc);
        XFreeGC(disp, and_gc);
    }
    if (set_gc){
        DBGP(set_gc);
        XFreeGC(disp, set_gc);
    }
    if (chip_mask){
        DBGP(chip_mask);
        XFreePixmap(disp, chip_mask);
    }
    if (chip){
        DBGP(chip);
        XFreePixmap(disp, chip);
    }
    if (p_store){
        DBGP(p_store);
        XFreePixmap(disp, p_store);
    }
} 

void win_chip::make(win *back,int wx,int wy, int sizex,int sizey,
                    int vsizex, int vsizey, int csizex, int csizey, int policy)
{
    this->win_scroll::make(back,wx,wy,sizex,sizey,vsizex,vsizey);
    csx = csizex;
    csy = csizey;
    store_policy = policy;   /* 0 : speed   1 : memory */
    if (p_store){
        DBGP(p_store);
        XFreePixmap(disp, p_store);
    }
    if (store_policy == 0){
        DBGP(p_store =
             XCreatePixmap(disp,w,vsx,vsy,DefaultDepth(disp,scr)));
    }
    else {
        DBGP(p_store =
             XCreatePixmap(disp,w,sizex,sizey,DefaultDepth(disp,scr)));
    }
    if (chip){
        DBGP(chip);
        XFreePixmap(disp, chip);
    }
    DBGP(chip      = XCreatePixmap(disp,w,csx,csy,DefaultDepth(disp,scr)));
    if (chip_mask){
        DBGP(chip_mask);
        XFreePixmap(disp, chip_mask);
    }
    DBGP(chip_mask = XCreatePixmap(disp,w,csx,csy,DefaultDepth(disp,scr)));
    XGCValues gcv;
    gcv.graphics_exposures = False;
    if (set_gc){
        DBGP(set_gc);
        XFreeGC(disp, set_gc);
    }
    DBGP(set_gc = XCreateGC(disp,w, GCGraphicsExposures, &gcv));
    gcv.graphics_exposures = False;
    gcv.function = GXand;
    if (and_gc){
        DBGP(and_gc);
        XFreeGC(disp, and_gc);
    }
    DBGP(and_gc = XCreateGC(disp,w,GCFunction | GCGraphicsExposures, &gcv));
    gcv.graphics_exposures = False;
    gcv.function = GXor;
    if (or_gc){
        DBGP(or_gc);
        XFreeGC(disp, or_gc);
    }
    DBGP(or_gc = XCreateGC(disp,w,GCFunction | GCGraphicsExposures, &gcv));
}

void win_chip::torus_copy_from_chip_and_store(int x, int y, int h, int v,
                                              int x1, int x2)
{
    this->copy_from_chip_and_store(x,y,h,v,x1,x2);
    if (x1 < sx){
        this->copy_from_chip_and_store(x,y,h,v,x1+vsx-sx,x2);
        if (x2 < sy){
            this->copy_from_chip_and_store(x,y,h,v,x1,x2+vsy-sy);
            this->copy_from_chip_and_store(x,y,h,v,x1+vsx-sx,x2+vsy-sy);
        }
    }
    else if (x2 < sy){
        this->copy_from_chip_and_store(x,y,h,v,x1,x2+vsy-sy);
    }
}

void win_chip::copy_from_chip_and_store(int x, int y, int h, int v,
                                              int x1, int x2)
{
    XCopyArea(disp,chip,p,     set_gc,x,y,h,v,x1,x2);
    if (store_policy == 0){
        XCopyArea(disp,chip,p_store,set_gc,x,y,h,v,x1,x2);
    }
}

int win_chip::clip(int& cx, int& cy, int& x, int& y, int& h, int& v)
{
    int x2 = x + h;
    int y2 = y + h;
    if (x < vx){
        cx += (vx - x);
        x = vx;
    }
    if (y < vy){
        cy += (vy - y);
        y = vy;
    }
    if (x2 > vx + sx) x2 = vx + sx;
    if (y2 > vy + sy) y2 = vy + sy;
    h = x2 - x;
    v = y2 - y;
    if ((h < 0) || (v < 0)){
        h = 0;
        v = 0;
        return 1;
    }
    return 0;
}

void win_chip::store()
{
    if (store_policy == 0)
        XCopyArea(disp, p, p_store, set_gc, 0, 0, vsx, vsy, 0, 0);
}

void win_chip::set_position(int vposx, int vposy)
{
    vx = vposx;
    vy = vposy;
    _sprite *spp;
    if (store_policy == 0){
        /*  policy : speed */
        for (spp=sprite; spp<spr_max; spp++){
            XCopyArea(disp, chip_mask, p, and_gc,
                      spp->cx, spp->cy, spp->h, spp->v, spp->x, spp->y);
            XCopyArea(disp, chip     , p, or_gc,
                      spp->cx, spp->cy, spp->h, spp->v, spp->x, spp->y);
        }
        XCopyArea(disp,p,w,gc,vx,vy,sx,sy,0,0);
        for (spp=sprite; spp<spr_max; spp++)
            XCopyArea(disp, p_store, p, set_gc,
                      spp->x, spp->y, spp->h, spp->v, spp->x, spp->y);
        spr_max = sprite;
        XFlush(disp);
    }
    else {
        /*  policy : memory */
        XCopyArea(disp,p,p_store,gc,vx,vy,sx,sy,0,0);
        for (spp=sprite; spp<spr_max; spp++){
            if (!(this->clip(spp->cx, spp->cy, spp->x, spp->y,
                             spp->h, spp->v))){
                XCopyArea(disp, chip_mask, p, and_gc,
                          spp->cx, spp->cy, spp->h, spp->v, spp->x, spp->y);
                XCopyArea(disp, chip     , p, or_gc,
                          spp->cx, spp->cy, spp->h, spp->v, spp->x, spp->y);
            }
        }
        XCopyArea(disp,p,w,gc,vx,vy,sx,sy,0,0);
        for (spp=sprite; spp<spr_max; spp++)
            XCopyArea(disp, p_store, p, set_gc,
                      (spp->x) - vx, (spp->y) - vy,
                      spp->h, spp->v, spp->x, spp->y);
        spr_max = sprite;
        XFlush(disp);
    }
}
