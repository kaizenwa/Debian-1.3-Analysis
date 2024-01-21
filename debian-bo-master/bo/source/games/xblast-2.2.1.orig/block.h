/*
 * Programm XBLAST V1.2.14 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * May 9th 1996
 * started August 1993
 *
 * File: block.h
 * include file for block.c
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public Licences as published
 * by the Free Software Foundation; either version 2; or (at your option)
 * any later version
 *
 * This program is distributed in the hope that it will entertaining,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILTY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
 * Publis License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef _BLOCK_H
#define _BLOCK_H


/*
 * indices of block bitmaps
 */
#define BLKaroLight      0
#define BLKaroDark       1
#define BLPyramid        2
#define BLWall           3
#define BLExtra          4
#define BLRange          5
#define BLBomb           6
#define BLChest          7
#define BLTrap           8
#define BLHex            9
#define BLHexExtra       10
#define BLHexWall        11
#define BLLegoFloor      12
#define BLLegoWhite      13
#define BLLegoBlack      14
#define BLScoreLeftUp    15
#define BLScoreLeftDown  16
#define BLScoreMidUp     17
#define BLScoreMidDown   18
#define BLScoreRightUp   19
#define BLScoreRightDown 20
#define BLBricks         20
#define BLScoreFloor     21
#define BLIronFloor      22
#define BLDarkBlock      23
#define BLDarkHouse      24
#define BLLightHouse     25
#define BLCityFree       26
#define BLControlNum     27
#define BLControlAlpha   28
#define BLDisplay        29
#define BLSphereLight    30
#define BLSphereDark     31
#define BLSphereHalf     32
#define BLInvincible     33
#define BLTemple         34
#define BLRemoteControl  35
#define BLBookShelf      36
#define BLChessFloor     37
#define BLChessSphere    38
#define BLPumpkin        39
#define BLRIP            40
#define BLDarkWay        41
#define BLKaroLight_S    42
#define BLExtra_O        43
#define BLKaroDark_S     44
#define BLChest_O        45
#define BLIronFloor_S    46
#define BLBricks_O       47
#define BLSphereHalf_S   48
#define BLSphereLight_O  49
#define BLHexExtra_O     50
#define BLChessFloor_S   51
#define BLChessSphere_O  52
#define BLLegoFloor_S    53
#define BLLegoBlack_O    54
#define BLCityFree_S     55
#define BLLightHouse_O   56
#define BLDarkWay_S      57
#define BLPumpkin_O      58
#define BLKick           59
#define BLPyramidRise    60
#define BLWallRise       61
#define BLDarkBlockRise  62
#define BLRIPRise        63
#define BLDarkHouseRise  64
#define BLIgnite         65
#define BLBox            66
#define BLButtonFloor    67
#define BLDarkButton     68
#define BLTeleport       69
#define BLMrBeamFree     70
#define BLMrBeamBear     71
#define BLMrBeamBearExp  72
#define BLMrBeamTv       73
#define BLQ3aBeam        74
#define BLAirPump        75
#define BLNapalm         76
#define BLRockFloor      77
#define BLRockFloor_S    78
#define BLPit            79
#define BLWeight         80
#define BLPitRise        81
#define BLWeightRise     82
#define BLGhost          83
#define BLGhostSqRise    84
#define BLGhostSq        85
#define BLGhostCiRise    86
#define BLGhostCi        87
#define BLFirecracker    88
#define BLConstruction   89
#define BLScoreStep      90
#define BLScoreDrop      91
#define BLSyringe        92
#define MAX_BLOCK_TILES  93

#ifndef _BLOCK_C

extern BitmapStruct title_bitmap;
extern BitmapStruct text_bg_bitmap;
extern BitmapStruct block_tile[MAX_BLOCK_TILES];
extern BitmapStruct block_addon[MAX_BLOCK_TILES];

#endif

#endif
/*
 * end of file block.h
 */

