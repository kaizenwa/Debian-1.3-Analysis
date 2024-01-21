/*
 * Author:      Renato Santana, <renato@nce.ufrj.br> in Januray, 1996.
 *
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/wb1.e,v 3.0 1996/05/06 16:12:55 william Exp $
 */

#ifdef _TGIF_WB

#ifndef _WB1_E_
#define _WB1_E_

extern int	ClientServer;

extern void	WhiteBoard ARGS_DECL((void));
extern void	CheckConForRecord ARGS_DECL((int CmdType,
		                             struct SubCmdRec *SubCmdPtr,
		                             struct SelRec *TopSel, int NumObjs,
		                             struct SelRec *TopSelBefore));
extern int	CheckClientServer ARGS_DECL((void));

#endif /*_WB1_E_*/

#endif    /* _TGIF_WB */
