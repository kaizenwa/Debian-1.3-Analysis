/*
 * Author:      Renato Santana, <renato@nce.ufrj.br> in Januray, 1996.
 *
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/wb3.e,v 3.0 1996/05/06 16:13:02 william Exp $
 */

#ifdef _TGIF_WB

#ifndef _WB3_E_
#define _WB3_E_

extern void	ReadPoly ARGS_DECL((char*, struct ObjRec**));
extern void	ReadPolygon ARGS_DECL((char*, struct ObjRec**));
extern int	ReadSmoothHingeFromBuff ARGS_DECL((char*, int, int, char*));
extern void	ReadText ARGS_DECL((char*, struct ObjRec**));
extern void	AddStrRec ARGS_DECL((struct StrRec*, struct StrRec*,
		                     struct StrRec*));
extern void	SaveToBuffer ARGS_DECL((char*, struct ObjRec*, int, int));
extern void	ReadGroup ARGS_DECL((char*, int, struct ObjRec**));

#endif /*_WB3_E_*/

#endif    /* _TGIF_WB */
