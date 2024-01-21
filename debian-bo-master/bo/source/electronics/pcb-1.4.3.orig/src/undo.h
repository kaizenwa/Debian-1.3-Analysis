/*
 *                            COPYRIGHT
 *
 *  PCB, interactive printed circuit board design
 *  Copyright (C) 1994,1995,1996 Thomas Nau
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Contact addresses for paper mail and Email:
 *  Thomas Nau, Schlehenweg 15, 88471 Baustetten, Germany
 *  Thomas.Nau@rz.uni-ulm.de
 *
 *  RCS: $Id: undo.h,v 143.1 1996/09/16 09:09:01 nau Exp $
 */

/* prototypes for undo routines
 */

#ifndef	__UNDO_INCLUDED__
#define	__UNDO_INCLUDED__

#include "global.h"

/* ---------------------------------------------------------------------------
 * define several commands which are supprted for undo
 */
#define	UNDO_CHANGENAME				0x0001	/* change of names */
#define	UNDO_COPY					0x0002	/* copy objects */
#define	UNDO_MOVE					0x0004	/* moving objects */
#define	UNDO_REMOVE					0x0008	/* removing objects */
#define	UNDO_REMOVE_POINT			0x0010	/* removing polygon/... points */
#define	UNDO_INSERT_POINT			0x0020	/* inserting polygon/... points */
#define	UNDO_ROTATE					0x0040	/* rotations */
#define	UNDO_CREATE					0x0080	/* creation of objects */
#define	UNDO_MOVETOLAYER			0x0100	/* moving objects to */
											/* different layers */

Boolean	Undo(void);
void	IncrementUndoSerialNumber(void);
void	SaveUndoSerialNumber(void);
void	RestoreUndoSerialNumber(void);
void	ClearUndoList(Boolean);
void	MoveObjectToRemoveUndoList(int, void *, void *, void *);
void	AddObjectToRemovePointUndoList(int, void *, void *, void *);
void	AddObjectToInsertPointUndoList(int, void *, void *, void *);
void	AddObjectToCopyUndoList(int, void *, void *, void *);
void	AddObjectToMoveUndoList(int, void *, void *, void *,
			Position, Position);
void	AddObjectToChangeNameUndoList(int, void *,void *, void *, char *);
void	AddObjectToRotateUndoList(int, void *, void *,void *,
			Position, Position, BYTE);
void	AddObjectToCreateUndoList(int, void *, void *,void *);
void	AddObjectToMoveToLayerUndoList(int, void *, void *, void *);
void	LockUndo(void);
void	UnlockUndo(void);
 
#endif
