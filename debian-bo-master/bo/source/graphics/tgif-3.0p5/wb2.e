/*
 * Author:      Renato Santana, <renato@nce.ufrj.br> in January, 1996.
 *
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/wb2.e,v 3.0 1996/05/06 16:12:58 william Exp $
 */

#ifdef _TGIF_WB

#ifndef _WB2_E_
#define _WB2_E_

extern void	RecordPolyToBuffer ARGS_DECL((struct ObjRec*, char*));
extern void	SaveSmoothHingeToBuff ARGS_DECL((char*, int, int, char*));
extern void	RecordBoxToBuffer ARGS_DECL((struct ObjRec*, char*));
extern void	RecordOvalToBuffer ARGS_DECL((struct ObjRec*, char*));
extern void	RecordTextToBuffer ARGS_DECL((struct ObjRec*, char*));
extern void	RecordArcToBuffer ARGS_DECL((struct ObjRec*, char*));
extern void	RecordPolygonToBuffer ARGS_DECL((struct ObjRec*, char*));
extern void	RecordGroupToBuffer ARGS_DECL((struct ObjRec*, char*, int));
extern void	RecordRCBoxToBuffer ARGS_DECL((struct ObjRec*, char*));
extern void	SaveAttrsToBuff ARGS_DECL((char*, struct AttrRec*));
extern void	SaveAttrToBuff ARGS_DECL((char*, struct AttrRec*));
extern void	SaveStringToBuff ARGS_DECL((char*, char*));
extern void	SaveTextObjToBuff ARGS_DECL((char*, struct ObjRec*));
extern int	ReadAttrFromBuff ARGS_DECL((char*, struct AttrRec**));
extern void	CopyObj ARGS_DECL((struct ObjRec*, struct ObjRec*));

#endif /*_WB2_E_*/

#endif    /* _TGIF_WB */
