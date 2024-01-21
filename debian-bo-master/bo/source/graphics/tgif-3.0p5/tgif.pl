%
% Author:      William Chia-Wei Cheng (william@cs.ucla.edu)
%
% Copyright (C) 1992-1993, William Cheng.
%
% Permission limited to the use, copy, modify, and distribute this software
% and its documentation for any purpose is hereby granted by the Author without
% fee, provided that the above copyright notice appear in all copies and
% that both the copyright notice and this permission notice appear in
% supporting documentation, and that the name of the Author not be used
% in advertising or publicity pertaining to distribution of the software
% without specific, written prior permission.  The Author makes no
% representations about the suitability of this software for any purpose.
% It is provided "as is" without express or implied warranty.  All other
% rights (including the right to sell "tgif" and the right to sell derivative
% works of tgif) are reserved by the Author.
%
% THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
% INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
% EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, INDIRECT OR
% CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
% USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
% OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
% PERFORMANCE OF THIS SOFTWARE.
%
% @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/tgif.pl,v 3.0 1996/05/06 16:12:25 william Exp $
%

% 1) Call tgif_clean/0 each time before step (2).
% 2) Consult a '.obj' or a '.sym' file.
% 3) Call tgif_state/2.  This must be called before any other tgif goals
%    are called.
% 4) Trace any other goals.

tgif_state(FileVersion,Obj) :- tgif_state(FileVersion,Obj,_).

tgif_state(FileVersion,Obj,Parms) :-
	(	var(Obj) -> OutputObj = true,
		current_predicate(state,
			state(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
	;	OutputObj = false
	),
	Obj = state(_PageStyle,FileVersion,_OrigX,_OrigY,_Zoom,
		_GridSize,_Grid,_Color,_HoriAlign,_VertAlign,_LineWidth,
		_LineStyle,_ObjFill,_PenPat,_TextJust,_Font,
		_TextStyle,_TextSize),
	tgif_chk_output(OutputObj,Obj),
	FileVersion =< 3,
	Parms = [page_style=_PageStyle,file_version=FileVersion,
		orig_x=_OrigX,orig_y=_OrigY,zoom=_Zoom,
		english_grid_size=_GridSize,snap_on=_Grid,color=_Color,
		h_align=_HoriAlign,v_align=_VertAlign,line_width=_LineWidth,
		line_style=_LineStyle,obj_fill=_ObjFill,pen_pat=_PenPat,
		text_just=_TextJust,font=_Font,text_style=_TextStyle,
		text_size=_TextSize],
	!, abolish(tgif_file_version/1),
	!, assert(tgif_file_version(FileVersion)).
tgif_state(FileVersion,Obj,Parms) :-
	(	var(Obj) -> OutputObj = true,
		current_predicate(state,
			state(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
	;	OutputObj = false
	),
	Obj = state(_PageStyle,FileVersion,_OrigX,_OrigY,_Zoom,
		_GridSize,_Grid,_Color,_HoriAlign,_VertAlign,_LineWidth,
		_Spline,_LineStyle,_ObjFill,_PenPat,_TextJust,_Font,
		_TextStyle,_TextSize),
	tgif_chk_output(OutputObj,Obj),
	FileVersion =< 7,
	Parms = [page_style=_PageStyle,file_version=FileVersion,
		orig_x=_OrigX,orig_y=_OrigY,zoom=_Zoom,
		english_grid_size=_GridSize,snap_on=_Grid,color=_Color,
		h_align=_HoriAlign,v_align=_VertAlign,line_width=_LineWidth,
		line_type=_Spline,
		line_style=_LineStyle,obj_fill=_ObjFill,pen_pat=_PenPat,
		text_just=_TextJust,font=_Font,text_style=_TextStyle,
		text_size=_TextSize],
	!, abolish(tgif_file_version/1),
	!, assert(tgif_file_version(FileVersion)).
tgif_state(FileVersion,Obj,Parms) :-
	(	var(Obj) -> OutputObj = true,
		current_predicate(state,
			state(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
	;	OutputObj = false
	),
	Obj = state(_PageStyle,FileVersion,_OrigX,_OrigY,_Zoom,
		_GridSize,_Grid,_Color,_HoriAlign,_VertAlign,_LineWidth,
		_Spline,_LineStyle,_ObjFill,_PenPat,_TextJust,_Font,
		_TextStyle,_TextSize,_TextDPI),
	tgif_chk_output(OutputObj,Obj),
	FileVersion =< 8,
	Parms = [page_style=_PageStyle,file_version=FileVersion,
		orig_x=_OrigX,orig_y=_OrigY,zoom=_Zoom,
		english_grid_size=_GridSize,snap_on=_Grid,color=_Color,
		h_align=_HoriAlign,v_align=_VertAlign,line_width=_LineWidth,
		line_type=_Spline,
		line_style=_LineStyle,obj_fill=_ObjFill,pen_pat=_PenPat,
		text_just=_TextJust,font=_Font,text_style=_TextStyle,
		text_size=_TextSize,text_dpi=_TextDPI],
	!, abolish(tgif_file_version/1),
	!, assert(tgif_file_version(FileVersion)).
tgif_state(FileVersion,Obj,Parms) :-
	(	var(Obj) -> OutputObj = true,
		current_predicate(state,
			state(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
	;	OutputObj = false
	),
	Obj = state(_PageStyle,FileVersion,_OrigX,_OrigY,_Zoom,
		_GridSize,_Grid,_Color,_HoriAlign,_VertAlign,_LineWidth,
		_Spline,_LineStyle,_ObjFill,_PenPat,_TextJust,_Font,
		_TextStyle,_TextSize,_TextDPI,_Dash),
	tgif_chk_output(OutputObj,Obj),
	FileVersion =< 11,
	Parms = [page_style=_PageStyle,file_version=FileVersion,
		orig_x=_OrigX,orig_y=_OrigY,zoom=_Zoom,
		english_grid_size=_GridSize,snap_on=_Grid,color=_Color,
		h_align=_HoriAlign,v_align=_VertAlign,line_width=_LineWidth,
		line_type=_Spline,
		line_style=_LineStyle,obj_fill=_ObjFill,pen_pat=_PenPat,
		text_just=_TextJust,font=_Font,text_style=_TextStyle,
		text_size=_TextSize,text_dpi=_TextDPI,line_dash=_Dash],
	!, abolish(tgif_file_version/1),
	!, assert(tgif_file_version(FileVersion)).
tgif_state(FileVersion,Obj,Parms) :-
	(	var(Obj) -> OutputObj = true,
		current_predicate(state,
			state(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
	;	OutputObj = false
	),
	Obj = state(_PageStyle,FileVersion,_OrigX,_OrigY,_Zoom,
		_EnglishGridSize,_Grid,_Color,_HoriAlign,_VertAlign,_LineWidth,
		_Spline,_LineStyle,_ObjFill,_PenPat,_TextJust,_Font,
		_TextStyle,_TextSize,_TextDPI,_Dash,_GridSystem,
		_MetricGridSize),
	tgif_chk_output(OutputObj,Obj),
	FileVersion =< 12,
	Parms = [page_style=_PageStyle,file_version=FileVersion,
		orig_x=_OrigX,orig_y=_OrigY,zoom=_Zoom,
		english_grid_size=_GridSize,snap_on=_Grid,color=_Color,
		h_align=_HoriAlign,v_align=_VertAlign,line_width=_LineWidth,
		line_type=_Spline,
		line_style=_LineStyle,obj_fill=_ObjFill,pen_pat=_PenPat,
		text_just=_TextJust,font=_Font,text_style=_TextStyle,
		text_size=_TextSize,text_dpi=_TextDPI,line_dash=_Dash,
		grid_system=_GridSystem,metric_grid=_MetricGridSize],
	!, abolish(tgif_file_version/1),
	!, assert(tgif_file_version(FileVersion)).
tgif_state(FileVersion,Obj,Parms) :-
	(	var(Obj) -> OutputObj = true,
		current_predicate(state,
			state(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
	;	OutputObj = false
	),
	Obj = state(_PageStyle,FileVersion,_OrigX,_OrigY,_Zoom,
		_EnglishGridSize,_Grid,_Color,_HoriAlign,_VertAlign,_LineWidth,
		_Spline,_LineStyle,_ObjFill,_PenPat,_TextJust,_Font,
		_TextStyle,_TextSize,_TextDPI,_Dash,_GridSystem,
		_MetricGridSize,_TextVSpace),
	tgif_chk_output(OutputObj,Obj),
	FileVersion =< 13,
	Parms = [page_style=_PageStyle,file_version=FileVersion,
		orig_x=_OrigX,orig_y=_OrigY,zoom=_Zoom,
		english_grid_size=_GridSize,snap_on=_Grid,color=_Color,
		h_align=_HoriAlign,v_align=_VertAlign,line_width=_LineWidth,
		line_type=_Spline,
		line_style=_LineStyle,obj_fill=_ObjFill,pen_pat=_PenPat,
		text_just=_TextJust,font=_Font,text_style=_TextStyle,
		text_size=_TextSize,text_dpi=_TextDPI,line_dash=_Dash,
		grid_system=_GridSystem,metric_grid=_MetricGridSize,
		text_v_space=_TextVSpace],
	!, abolish(tgif_file_version/1),
	!, assert(tgif_file_version(FileVersion)).
tgif_state(FileVersion,Obj,Parms) :-
	(	var(Obj) -> OutputObj = true,
		current_predicate(state,
			state(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
	;	OutputObj = false
	),
	Obj = state(_PageStyle,FileVersion,_PrintMag,_OrigX,_OrigY,_Zoom,
		_EnglishGridSize,_Grid,_Color,_HoriAlign,_VertAlign,_LineWidth,
		_Spline,_LineStyle,_ObjFill,_PenPat,_TextJust,_Font,
		_TextStyle,_TextSize,_TextDPI,_Dash,_GridSystem,
		_MetricGridSize,_TextVSpace),
	tgif_chk_output(OutputObj,Obj),
	FileVersion =< 18,
	Parms = [page_style=_PageStyle,file_version=FileVersion,
		print_mag=_PrintMag,
		orig_x=_OrigX,orig_y=_OrigY,zoom=_Zoom,
		english_grid_size=_GridSize,snap_on=_Grid,color=_Color,
		h_align=_HoriAlign,v_align=_VertAlign,line_width=_LineWidth,
		line_type=_Spline,
		line_style=_LineStyle,obj_fill=_ObjFill,pen_pat=_PenPat,
		text_just=_TextJust,font=_Font,text_style=_TextStyle,
		text_size=_TextSize,text_dpi=_TextDPI,line_dash=_Dash,
		grid_system=_GridSystem,metric_grid=_MetricGridSize,
		text_v_space=_TextVSpace],
	!, abolish(tgif_file_version/1),
	!, assert(tgif_file_version(FileVersion)).
tgif_state(FileVersion,Obj,Parms) :-
	(	var(Obj) -> OutputObj = true,
		current_predicate(state,
			state(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
	;	OutputObj = false
	),
	Obj = state(_PageStyle,FileVersion,_PrintMag,_OrigX,_OrigY,_Zoom,
		_EnglishGridSize,_Grid,_Color,_HoriAlign,_VertAlign,_LineWidth,
		_Spline,_LineStyle,_ObjFill,_PenPat,_TextJust,_Font,
		_TextStyle,_TextSize,_TextDPI,_Dash,_GridSystem,
		_MetricGridSize,_TextVSpace,_ZoomIn),
	tgif_chk_output(OutputObj,Obj),
	FileVersion =< 19,
	Parms = [page_style=_PageStyle,file_version=FileVersion,
		print_mag=_PrintMag,
		orig_x=_OrigX,orig_y=_OrigY,zoom=_Zoom,
		english_grid_size=_GridSize,snap_on=_Grid,color=_Color,
		h_align=_HoriAlign,v_align=_VertAlign,line_width=_LineWidth,
		line_type=_Spline,
		line_style=_LineStyle,obj_fill=_ObjFill,pen_pat=_PenPat,
		text_just=_TextJust,font=_Font,text_style=_TextStyle,
		text_size=_TextSize,text_dpi=_TextDPI,line_dash=_Dash,
		grid_system=_GridSystem,metric_grid=_MetricGridSize,
		text_v_space=_TextVSpace,zoom_in=_ZoomIn],
	!, abolish(tgif_file_version/1),
	!, assert(tgif_file_version(FileVersion)).
tgif_state(FileVersion,Obj,Parms) :-
	(	var(Obj) -> OutputObj = true,
		current_predicate(state,
			state(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
	;	OutputObj = false
	),
	Obj = state(_PageStyle,FileVersion,_PrintMag,_OrigX,_OrigY,_Zoom,
		_EnglishGridSize,_Grid,_Color,_HoriAlign,_VertAlign,_LineWidth,
		_Spline,_LineStyle,_ObjFill,_PenPat,_TextJust,_Font,
		_TextStyle,_TextSize,_TextDPI,_Dash,_GridSystem,
		_MetricGridSize,_TextVSpace,_ZoomIn,_GridShown,_MoveMode),
	tgif_chk_output(OutputObj,Obj),
	FileVersion =< 21,
	Parms = [page_style=_PageStyle,file_version=FileVersion,
		print_mag=_PrintMag,
		orig_x=_OrigX,orig_y=_OrigY,zoom=_Zoom,
		english_grid_size=_GridSize,snap_on=_Grid,color=_Color,
		h_align=_HoriAlign,v_align=_VertAlign,line_width=_LineWidth,
		line_type=_Spline,
		line_style=_LineStyle,obj_fill=_ObjFill,pen_pat=_PenPat,
		text_just=_TextJust,font=_Font,text_style=_TextStyle,
		text_size=_TextSize,text_dpi=_TextDPI,line_dash=_Dash,
		grid_system=_GridSystem,metric_grid=_MetricGridSize,
		text_v_space=_TextVSpace,zoom_in=_ZoomIn,grid_shown=_GridShown,
		move_mode=_ModeMode],
	!, abolish(tgif_file_version/1),
	!, assert(tgif_file_version(FileVersion)).
tgif_state(FileVersion,Obj,Parms) :-
	(	var(Obj) -> OutputObj = true,
		current_predicate(state,
			state(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
	;	OutputObj = false
	),
	Obj = state(_PageStyle,FileVersion,_PrintMag,_OrigX,_OrigY,_Zoom,
		_EnglishGridSize,_Grid,_Color,_HoriAlign,_VertAlign,_LineWidth,
		_Spline,_LineStyle,_ObjFill,_PenPat,_TextJust,_Font,
		_TextStyle,_TextSize,_TextDPI,_Dash,_GridSystem,
		_MetricGridSize,_TextVSpace,_ZoomIn,_GridShown,_MoveMode,
		_TextRotate,_RCBoxRadius),
	tgif_chk_output(OutputObj,Obj),
	FileVersion =< 26,
	Parms = [page_style=_PageStyle,file_version=FileVersion,
		print_mag=_PrintMag,
		orig_x=_OrigX,orig_y=_OrigY,zoom=_Zoom,
		english_grid_size=_GridSize,snap_on=_Grid,color=_Color,
		h_align=_HoriAlign,v_align=_VertAlign,line_width=_LineWidth,
		line_type=_Spline,
		line_style=_LineStyle,obj_fill=_ObjFill,pen_pat=_PenPat,
		text_just=_TextJust,font=_Font,text_style=_TextStyle,
		text_size=_TextSize,text_dpi=_TextDPI,line_dash=_Dash,
		grid_system=_GridSystem,metric_grid=_MetricGridSize,
		text_v_space=_TextVSpace,zoom_in=_ZoomIn,grid_shown=_GridShown,
		move_mode=_ModeMode,text_rotate=_TextRotate,
		rcb_radius=_RCBoxRadius],
	!, abolish(tgif_file_version/1),
	!, assert(tgif_file_version(FileVersion)).
tgif_state(FileVersion,Obj,Parms) :-
	(	var(Obj) -> OutputObj = true,
		current_predicate(state,
			state(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
	;	OutputObj = false
	),
	Obj = state(_PageStyle,FileVersion,_PrintMag,_OrigX,_OrigY,_Zoom,
		_EnglishGridSize,_Grid,_Color,_HoriAlign,_VertAlign,_LineWidth,
		_Spline,_LineStyle,_ObjFill,_PenPat,_TextJust,_Font,
		_TextStyle,_TextSize,_TextDPI,_Dash,_GridSystem,
		_MetricGridSize,_TextVSpace,_ZoomIn,_GridShown,_MoveMode,
		_TextRotate,_RCBoxRadius,_UseGray),
	tgif_chk_output(OutputObj,Obj),
	FileVersion =< 27,
	Parms = [page_style=_PageStyle,file_version=FileVersion,
		print_mag=_PrintMag,
		orig_x=_OrigX,orig_y=_OrigY,zoom=_Zoom,
		english_grid_size=_GridSize,snap_on=_Grid,color=_Color,
		h_align=_HoriAlign,v_align=_VertAlign,line_width=_LineWidth,
		line_type=_Spline,
		line_style=_LineStyle,obj_fill=_ObjFill,pen_pat=_PenPat,
		text_just=_TextJust,font=_Font,text_style=_TextStyle,
		text_size=_TextSize,text_dpi=_TextDPI,line_dash=_Dash,
		grid_system=_GridSystem,metric_grid=_MetricGridSize,
		text_v_space=_TextVSpace,zoom_in=_ZoomIn,grid_shown=_GridShown,
		move_mode=_ModeMode,text_rotate=_TextRotate,
		rcb_radius=_RCBoxRadius,use_gray=_UseGray],
	!, abolish(tgif_file_version/1),
	!, assert(tgif_file_version(FileVersion)).
tgif_state(FileVersion,Obj,Parms) :-
	% Mstsuda's Version
	(	var(Obj) -> OutputObj = true,
		current_predicate(state,
			state(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
	;	OutputObj = false
	),
	Obj = state(_PageStyle,FileVersion,_PrintMag,_OrigX,_OrigY,_Zoom,
		_EnglishGridSize,_Grid,_Color,_HoriAlign,_VertAlign,_LineWidth,
		_Spline,_LineStyle,_ObjFill,_PenPat,_TextJust,_Font,
		_TextStyle,_TextSize,_TextDPI,_Dash,_GridSystem,
		_MetricGridSize,_TextVSpace,_ZoomIn,_GridShown,_MoveMode,
		_TextRotate,_RCBoxRadius,_UseGray,_PaperCol,_PaperRow,
		_CurPageNum,_LastPageNum),
	tgif_chk_output(OutputObj,Obj),
	FileVersion =< 28,
	Parms = [page_style=_PageStyle,file_version=FileVersion,
		print_mag=_PrintMag,
		orig_x=_OrigX,orig_y=_OrigY,zoom=_Zoom,
		english_grid_size=_GridSize,snap_on=_Grid,color=_Color,
		h_align=_HoriAlign,v_align=_VertAlign,line_width=_LineWidth,
		line_type=_Spline,
		line_style=_LineStyle,obj_fill=_ObjFill,pen_pat=_PenPat,
		text_just=_TextJust,font=_Font,text_style=_TextStyle,
		text_size=_TextSize,text_dpi=_TextDPI,line_dash=_Dash,
		grid_system=_GridSystem,metric_grid=_MetricGridSize,
		text_v_space=_TextVSpace,zoom_in=_ZoomIn,grid_shown=_GridShown,
		move_mode=_ModeMode,text_rotate=_TextRotate,
		rcb_radius=_RCBoxRadius,use_gray=_UseGray,
		papge_col=_PaperCol,papge_row=_PaperRow,
		cur_page_num=_CurPageNum,last_page_num=_LastPageNum],
	!, abolish(tgif_file_version/1),
	!, assert(tgif_file_version(FileVersion)).
tgif_state(FileVersion,Obj,Parms) :-
	% Mstsuda's Version
	(	var(Obj) -> OutputObj = true,
		current_predicate(state,
			state(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
	;	OutputObj = false
	),
	Obj = state(_PageStyle,FileVersion,_PrintMag,_OrigX,_OrigY,_Zoom,
		_EnglishGridSize,_Grid,_Color,_HoriAlign,_VertAlign,_LineWidth,
		_Spline,_LineStyle,_ObjFill,_PenPat,_TextJust,_Font,
		_TextStyle,_TextSize,_TextDPI,_Dash,_GridSystem,
		_MetricGridSize,_TextVSpace,_ZoomIn,_GridShown,_MoveMode,
		_TextRotate,_RCBoxRadius,_UseGray,_PageLayoutMode,_PageArg1,
		_PageArg2,_PageLineShownInTileMode),
	tgif_chk_output(OutputObj,Obj),
	FileVersion =< 29,
	(	(_PageLayoutMode == 0) -> % stack mode
		_CurPageNum = _PageArg1, _LastPageNum = _PageArg2,
		_PaperRow = 1, _PaperCol = 1
	;	_PaperCol = _PageArg1, _PaperRow = _PageArg2,
		_CurPageNum = 1, _LastPageNum = 1
	),
	Parms = [page_style=_PageStyle,file_version=FileVersion,
		print_mag=_PrintMag,
		orig_x=_OrigX,orig_y=_OrigY,zoom=_Zoom,
		english_grid_size=_GridSize,snap_on=_Grid,color=_Color,
		h_align=_HoriAlign,v_align=_VertAlign,line_width=_LineWidth,
		line_type=_Spline,
		line_style=_LineStyle,obj_fill=_ObjFill,pen_pat=_PenPat,
		text_just=_TextJust,font=_Font,text_style=_TextStyle,
		text_size=_TextSize,text_dpi=_TextDPI,line_dash=_Dash,
		grid_system=_GridSystem,metric_grid=_MetricGridSize,
		text_v_space=_TextVSpace,zoom_in=_ZoomIn,grid_shown=_GridShown,
		move_mode=_ModeMode,text_rotate=_TextRotate,
		rcb_radius=_RCBoxRadius,use_gray=_UseGray,
		page_layout_mode=_PageLayoutMode,
		page_line_shown_in_tile_mode=_PageLineShownInTileMode,
		papge_col=_PaperCol,papge_row=_PaperRow,
		cur_page_num=_CurPageNum,last_page_num=_LastPageNum],
	!, abolish(tgif_file_version/1),
	!, assert(tgif_file_version(FileVersion)).
tgif_state(FileVersion,Obj,Parms) :-
	% Mstsuda's Version
	(	var(Obj) -> OutputObj = true,
		current_predicate(state,
			state(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
	;	OutputObj = false
	),
	Obj = state(_PageStyle,FileVersion,_PrintMag,_OrigX,_OrigY,_Zoom,
		_EnglishGridSize,_Grid,_Color,_HoriAlign,_VertAlign,_LineWidth,
		_Spline,_LineStyle,_ObjFill,_PenPat,_TextJust,_FontName,
		_TextStyle,_PointSize,0,_Dash,_GridSystem,
		_MetricGridSize,_TextVSpace,_ZoomIn,_GridShown,_MoveMode,
		_TextRotate,_RCBoxRadius,_UseGray,_PageLayoutMode,_PageArg1,
		_PageArg2,_PageLineShownInTileMode),
	tgif_chk_output(OutputObj,Obj),
	FileVersion =< 30,
	(	(_PageLayoutMode == 0) -> % stack mode
		_CurPageNum = _PageArg1, _LastPageNum = _PageArg2,
		_PaperRow = 1, _PaperCol = 1
	;	_PaperCol = _PageArg1, _PaperRow = _PageArg2,
		_CurPageNum = 1, _LastPageNum = 1
	),
	Parms = [page_style=_PageStyle,file_version=FileVersion,
		print_mag=_PrintMag,
		orig_x=_OrigX,orig_y=_OrigY,zoom=_Zoom,
		english_grid_size=_GridSize,snap_on=_Grid,color=_Color,
		h_align=_HoriAlign,v_align=_VertAlign,line_width=_LineWidth,
		line_type=_Spline,
		line_style=_LineStyle,obj_fill=_ObjFill,pen_pat=_PenPat,
		text_just=_TextJust,font_name=_FontName,text_style=_TextStyle,
		point_size=_PointSize,line_dash=_Dash,
		grid_system=_GridSystem,metric_grid=_MetricGridSize,
		text_v_space=_TextVSpace,zoom_in=_ZoomIn,grid_shown=_GridShown,
		move_mode=_ModeMode,text_rotate=_TextRotate,
		rcb_radius=_RCBoxRadius,use_gray=_UseGray,
		page_layout_mode=_PageLayoutMode,
		page_line_shown_in_tile_mode=_PageLineShownInTileMode,
		papge_col=_PaperCol,papge_row=_PaperRow,
		cur_page_num=_CurPageNum,last_page_num=_LastPageNum],
	!, abolish(tgif_file_version/1),
	!, assert(tgif_file_version(FileVersion)).
tgif_state(FileVersion,Obj,Parms) :-
	% Mstsuda's Version
	(	var(Obj) -> OutputObj = true,
		current_predicate(state,
			state(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
	;	OutputObj = false
	),
	Obj = state(_PageStyle,FileVersion,_PrintMag,_OrigX,_OrigY,_Zoom,
		_EnglishGridSize,_Grid,_Color,_HoriAlign,_VertAlign,_LineWidth,
		_Spline,_LineStyle,_ObjFill,_PenPat,_TextJust,_FontName,
		_TextStyle,_PointSize,0,_Dash,_GridSystem,
		_MetricGridSize,_TextVSpace,_ZoomIn,_GridShown,_MoveMode,
		_TextRotate,_RCBoxRadius,_UseGray,_PageLayoutMode,_PageArg1,
		_PageArg2,_PageLineShownInTileMode,_ColorDump),
	tgif_chk_output(OutputObj,Obj),
	FileVersion =< 31,
	(	(_PageLayoutMode == 0) -> % stack mode
		_CurPageNum = _PageArg1, _LastPageNum = _PageArg2,
		_PaperRow = 1, _PaperCol = 1
	;	_PaperCol = _PageArg1, _PaperRow = _PageArg2,
		_CurPageNum = 1, _LastPageNum = 1
	),
	Parms = [page_style=_PageStyle,file_version=FileVersion,
		print_mag=_PrintMag,
		orig_x=_OrigX,orig_y=_OrigY,zoom=_Zoom,
		english_grid_size=_GridSize,snap_on=_Grid,color=_Color,
		h_align=_HoriAlign,v_align=_VertAlign,line_width=_LineWidth,
		line_type=_Spline,
		line_style=_LineStyle,obj_fill=_ObjFill,pen_pat=_PenPat,
		text_just=_TextJust,font_name=_FontName,text_style=_TextStyle,
		point_size=_PointSize,line_dash=_Dash,
		grid_system=_GridSystem,metric_grid=_MetricGridSize,
		text_v_space=_TextVSpace,zoom_in=_ZoomIn,grid_shown=_GridShown,
		move_mode=_ModeMode,text_rotate=_TextRotate,
		rcb_radius=_RCBoxRadius,use_gray=_UseGray,
		page_layout_mode=_PageLayoutMode,
		page_line_shown_in_tile_mode=_PageLineShownInTileMode,
		papge_col=_PaperCol,papge_row=_PaperRow,
		cur_page_num=_CurPageNum,last_page_num=_LastPageNum,
		color_dump=_ColorDump],
	!, abolish(tgif_file_version/1),
	!, assert(tgif_file_version(FileVersion)).
tgif_state(FileVersion,Obj,Parms) :-
	% Mstsuda's Version
	(	var(Obj) -> OutputObj = true,
		current_predicate(state,
			state(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
	;	OutputObj = false
	),
	Obj = state(_PageStyle,FileVersion,_PrintMag,_OrigX,_OrigY,_Zoom,
		_EnglishGridSize,_Grid,_Color,_HoriAlign,_VertAlign,_LineWidth,
		_Spline,_LineStyle,_ObjFill,_PenPat,_TextJust,_FontName,
		_TextStyle,_PointSize,0,_Dash,_GridSystem,
		_MetricGridSize,_TextVSpace,_ZoomIn,_GridShown,_MoveMode,
		_TextRotate,_RCBoxRadius,_UseGray,_PageLayoutMode,_PageArg1,
		_PageArg2,_PageLineShownInTileMode,_ColorDump,
		_OnePageWidth,_OnePageHeight),
	tgif_chk_output(OutputObj,Obj),
	FileVersion >= 32,
	(	(_PageLayoutMode == 0) -> % stack mode
		_CurPageNum = _PageArg1, _LastPageNum = _PageArg2,
		_PaperRow = 1, _PaperCol = 1
	;	_PaperCol = _PageArg1, _PaperRow = _PageArg2,
		_CurPageNum = 1, _LastPageNum = 1
	),
	Parms = [page_style=_PageStyle,file_version=FileVersion,
		print_mag=_PrintMag,
		orig_x=_OrigX,orig_y=_OrigY,zoom=_Zoom,
		english_grid_size=_GridSize,snap_on=_Grid,color=_Color,
		h_align=_HoriAlign,v_align=_VertAlign,line_width=_LineWidth,
		line_type=_Spline,
		line_style=_LineStyle,obj_fill=_ObjFill,pen_pat=_PenPat,
		text_just=_TextJust,font_name=_FontName,text_style=_TextStyle,
		point_size=_PointSize,line_dash=_Dash,
		grid_system=_GridSystem,metric_grid=_MetricGridSize,
		text_v_space=_TextVSpace,zoom_in=_ZoomIn,grid_shown=_GridShown,
		move_mode=_ModeMode,text_rotate=_TextRotate,
		rcb_radius=_RCBoxRadius,use_gray=_UseGray,
		page_layout_mode=_PageLayoutMode,
		page_line_shown_in_tile_mode=_PageLineShownInTileMode,
		papge_col=_PaperCol,papge_row=_PaperRow,
		cur_page_num=_CurPageNum,last_page_num=_LastPageNum,
		color_dump=_ColorDump,one_page_width=_OnePageWidth,
		one_page_height=_OnePageHeight],
	!, abolish(tgif_file_version/1),
	!, assert(tgif_file_version(FileVersion)).

% --------------------------------------------------------------------- %

tgif_text(Obj) :- tgif_text(Obj,_).

tgif_text(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 2, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = text(_Color,_X,_Y,_Font,_TextStyle,_TextSize,_NumLines,_TextJust,
		StrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,x=_X,y=_Y,font=_Font,text_style=_TextStyle,
		text_size=_TextSize,num_lines=_NumLines,text_just=_TextJust,
		strs=StrList],
	tgif_strs(StrList).
tgif_text(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 6, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = text(_Color,_X,_Y,_Font,_TextStyle,_TextSize,_NumLines,_TextJust,
		_TextRotate,_PenPat,StrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,x=_X,y=_Y,font=_Font,text_style=_TextStyle,
		text_size=_TextSize,num_lines=_NumLines,text_just=_TextJust,
		text_rotate=_TextRotate,pen_pat=_PenPat,strs=StrList],
	tgif_strs(StrList).
tgif_text(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 7, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = text(_Color,_X,_Y,_Font,_TextStyle,_TextSize,_NumLines,_TextJust,
		_TextRotate,_PenPat,_BBoxW,_BBoxH,StrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,x=_X,y=_Y,font=_Font,text_style=_TextStyle,
		text_size=_TextSize,num_lines=_NumLines,text_just=_TextJust,
		text_rotate=_TextRotate,pen_pat=_PenPat,bbox_w=_BBoxW,
		bbox_h=_BBoxH,strs=StrList],
	tgif_strs(StrList).
tgif_text(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 9, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = text(_Color,_X,_Y,_Font,_TextStyle,_TextSize,_NumLines,_TextJust,
		_TextRotate,_PenPat,_BBoxW,_BBoxH,_Id,_TextDPI,StrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,x=_X,y=_Y,font=_Font,text_style=_TextStyle,
		text_size=_TextSize,num_lines=_NumLines,text_just=_TextJust,
		text_rotate=_TextRotate,pen_pat=_PenPat,bbox_w=_BBoxW,
		bbox_h=_BBoxH,id=_Id,text_dpi=_TextDPI,strs=StrList],
	tgif_strs(StrList).
tgif_text(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 10, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = text(_Color,_X,_Y,_Font,_TextStyle,_TextSize,_NumLines,_TextJust,
		_TextRotate,_PenPat,_BBoxW,_BBoxH,_Id,_TextDPI,_Asc,_Des,
		StrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,x=_X,y=_Y,font=_Font,text_style=_TextStyle,
		text_size=_TextSize,num_lines=_NumLines,text_just=_TextJust,
		text_rotate=_TextRotate,pen_pat=_PenPat,bbox_w=_BBoxW,
		bbox_h=_BBoxH,id=_Id,text_dpi=_TextDPI,asc=_Asc,des=_Des,
		strs=StrList],
	tgif_strs(StrList).
tgif_text(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 12, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = text(_Color,_X,_Y,_Font,_TextStyle,_TextSize,_NumLines,_TextJust,
		_TextRotate,_PenPat,_BBoxW,_BBoxH,_Id,_TextDPI,_Asc,_Des,
		_ObjFill,StrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,x=_X,y=_Y,font=_Font,text_style=_TextStyle,
		text_size=_TextSize,num_lines=_NumLines,text_just=_TextJust,
		text_rotate=_TextRotate,pen_pat=_PenPat,bbox_w=_BBoxW,
		bbox_h=_BBoxH,id=_Id,text_dpi=_TextDPI,asc=_Asc,des=_Des,
		obj_fill=_ObjFill,strs=StrList],
	tgif_strs(StrList).
tgif_text(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 13, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = text(_Color,_X,_Y,_Font,_TextStyle,_TextSize,_NumLines,_TextJust,
		_TextRotate,_PenPat,_BBoxW,_BBoxH,_Id,_TextDPI,_Asc,_Des,
		_ObjFill,_VSpace,StrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,x=_X,y=_Y,font=_Font,text_style=_TextStyle,
		text_size=_TextSize,num_lines=_NumLines,text_just=_TextJust,
		text_rotate=_TextRotate,pen_pat=_PenPat,bbox_w=_BBoxW,
		bbox_h=_BBoxH,id=_Id,text_dpi=_TextDPI,asc=_Asc,des=_Des,
		obj_fill=_ObjFill,v_space=_VSpace,strs=StrList],
	tgif_strs(StrList).
tgif_text(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 25, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = text(_Color,_X,_Y,_Font,_TextStyle,_TextSize,_NumLines,_TextJust,
		_TextRotate,_PenPat,_BBoxW,_BBoxH,_Id,_TextDPI,_Asc,_Des,
		_ObjFill,_VSpace,_Rotation,StrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,x=_X,y=_Y,font=_Font,text_style=_TextStyle,
		text_size=_TextSize,num_lines=_NumLines,text_just=_TextJust,
		text_rotate=_TextRotate,pen_pat=_PenPat,bbox_w=_BBoxW,
		bbox_h=_BBoxH,id=_Id,text_dpi=_TextDPI,asc=_Asc,des=_Des,
		obj_fill=_ObjFill,v_space=_VSpace,rotation=_Rotation,
		strs=StrList],
	tgif_strs(StrList).
tgif_text(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 29, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = text(_Color,_X,_Y,_Font,_TextStyle,_TextSize,_NumLines,_TextJust,
		_TextRotate,_PenPat,_BBoxW,_BBoxH,_Id,_TextDPI,_Asc,_Des,
		_ObjFill,_VSpace,_Rotation,_Locked,StrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,x=_X,y=_Y,font=_Font,text_style=_TextStyle,
		text_size=_TextSize,num_lines=_NumLines,text_just=_TextJust,
		text_rotate=_TextRotate,pen_pat=_PenPat,bbox_w=_BBoxW,
		bbox_h=_BBoxH,id=_Id,text_dpi=_TextDPI,asc=_Asc,des=_Des,
		obj_fill=_ObjFill,v_space=_VSpace,rotation=_Rotation,
		locked=_Locked,strs=StrList],
	tgif_strs(StrList).
tgif_text(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 31, !,
	% From version 30 on, the PointSize is the actual size;
	%	the TextSize in older versions is a size index.
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = text(_Color,_X,_Y,_FontName,_TextStyle,_PointSize,_NumLines,
		_TextJust,_TextRotate,_PenPat,_BBoxW,_BBoxH,_Id,0,_Asc,_Des,
		_ObjFill,_VSpace,_Rotation,_Locked,StrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,x=_X,y=_Y,font_name=_FontName,
		text_style=_TextStyle,point_size=_PointSize,num_lines=_NumLines,
		text_just=_TextJust,text_rotate=_TextRotate,pen_pat=_PenPat,
		bbox_w=_BBoxW,bbox_h=_BBoxH,id=_Id,asc=_Asc,des=_Des,
		obj_fill=_ObjFill,v_space=_VSpace,rotation=_Rotation,
		locked=_Locked,strs=StrList],
	tgif_strs(StrList).
tgif_text(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion >= 33, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = text(_Color,_X,_Y,_FontName,_TextStyle,_PointSize,_NumLines,
		_TextJust,_TextRotate,_PenPat,_BBoxW,_BBoxH,_Id,0,_Asc,_Des,
		_ObjFill,_VSpace,_Rotation,_Locked,_UnderlineOn,_Underline,
		_MinLBearing,_MaxRExtra,_DoubleByte,_Direction,
		_CustomFontName,StrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,x=_X,y=_Y,font_name=_FontName,
		text_style=_TextStyle,point_size=_PointSize,num_lines=_NumLines,
		text_just=_TextJust,text_rotate=_TextRotate,pen_pat=_PenPat,
		bbox_w=_BBoxW,bbox_h=_BBoxH,id=_Id,asc=_Asc,des=_Des,
		obj_fill=_ObjFill,v_space=_VSpace,rotation=_Rotation,
		locked=_Locked,underline_on=_UnderlineOn,underline=_Underline,
		min_lbearing=_MinLBearing,max_rextra=_MaxRExtra,
		double_byte=_DoubleByte,direction=_Direction,
		custom_font_name=_CustomFontName,strs=StrList],
	tgif_strs(StrList).

% --------------------------------------------------------------------- %

tgif_box(Obj) :- tgif_box(Obj,_).

tgif_box(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 7, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = box(_Color,_X1,_Y1,_X2,_Y2,_ObjFill,_LineWidth,_PenPat),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
		obj_fill=_ObjFill,line_width=_LineWidth,pen_pat=_PenPat].
tgif_box(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 8, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = box(_Color,_X1,_Y1,_X2,_Y2,_ObjFill,_LineWidth,_PenPat,_Id,
		AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
		obj_fill=_ObjFill,line_width=_LineWidth,pen_pat=_PenPat,
		id=_Id,attrs=AttrList],
	tgif_attrs(AttrList).
tgif_box(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 13, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = box(_Color,_X1,_Y1,_X2,_Y2,_ObjFill,_LineWidth,_PenPat,_Id,_Dash,
		AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
		obj_fill=_ObjFill,line_width=_LineWidth,pen_pat=_PenPat,
		id=_Id,line_dash=_Dash,attrs=AttrList],
	tgif_attrs(AttrList).
tgif_box(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 25, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = box(_Color,_X1,_Y1,_X2,_Y2,_ObjFill,_LineWidth,_PenPat,_Id,_Dash,
		_Rotation,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
		obj_fill=_ObjFill,line_width=_LineWidth,pen_pat=_PenPat,
		id=_Id,line_dash=_Dash,rotation=_Rotation,attrs=AttrList],
	tgif_attrs(AttrList).
tgif_box(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion >= 26, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = box(_Color,_X1,_Y1,_X2,_Y2,_ObjFill,_LineWidth,_PenPat,_Id,_Dash,
		_Rotation,_Locked,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
		obj_fill=_ObjFill,line_width=_LineWidth,pen_pat=_PenPat,
		id=_Id,line_dash=_Dash,rotation=_Rotation,locked=_Locked,
		attrs=AttrList],
	tgif_attrs(AttrList).

% --------------------------------------------------------------------- %

tgif_oval(Obj) :- tgif_oval(Obj,_).

tgif_oval(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 7, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = oval(_Color,_LeftTopX,_LeftTopY,_RightBotX,_RightBotY,_ObjFill,
		_LineWidth,_PenPat),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,ltx=_LeftTopX,lty=_LeftTopY,rbx=_RightBotX,
		rby=_RightBotY,obj_fill=_ObjFill,line_width=_LineWidth,
		pen_pat=_PenPat].
tgif_oval(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 8, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = oval(_Color,_LeftTopX,_LeftTopY,_RightBotX,_RightBotY,_ObjFill,
		_LineWidth,_PenPat,_Id,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,ltx=_LeftTopX,lty=_LeftTopY,rbx=_RightBotX,
		rby=_RightBotY,obj_fill=_ObjFill,line_width=_LineWidth,
		pen_pat=_PenPat,id=_Id,attrs=AttrList],
	tgif_attrs(AttrList).
tgif_oval(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 13, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = oval(_Color,_LeftTopX,_LeftTopY,_RightBotX,_RightBotY,_ObjFill,
		_LineWidth,_PenPat,_Id,_Dash,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,ltx=_LeftTopX,lty=_LeftTopY,rbx=_RightBotX,
		rby=_RightBotY,obj_fill=_ObjFill,line_width=_LineWidth,
		pen_pat=_PenPat,id=_Id,line_dash=_Dash,attrs=AttrList],
	tgif_attrs(AttrList).
tgif_oval(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 25, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = oval(_Color,_LeftTopX,_LeftTopY,_RightBotX,_RightBotY,_ObjFill,
		_LineWidth,_PenPat,_Id,_Dash,_Rotation,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,ltx=_LeftTopX,lty=_LeftTopY,rbx=_RightBotX,
		rby=_RightBotY,obj_fill=_ObjFill,line_width=_LineWidth,
		pen_pat=_PenPat,id=_Id,line_dash=_Dash,rotation=_Rotation,
		attrs=AttrList],
	tgif_attrs(AttrList).
tgif_oval(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion >= 26, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = oval(_Color,_LeftTopX,_LeftTopY,_RightBotX,_RightBotY,_ObjFill,
		_LineWidth,_PenPat,_Id,_Dash,_Rotation,_Locked,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,ltx=_LeftTopX,lty=_LeftTopY,rbx=_RightBotX,
		rby=_RightBotY,obj_fill=_ObjFill,line_width=_LineWidth,
		pen_pat=_PenPat,id=_Id,line_dash=_Dash,rotation=_Rotation,
		locked=_Locked,attrs=AttrList],
	tgif_attrs(AttrList).

% --------------------------------------------------------------------- %

tgif_poly(Obj) :- tgif_poly(Obj,_).

tgif_poly(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 3, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = poly(_Color,_NumVs,_Vs,_LineStyle,_LineWidth,_PenPat,_Id,
		AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,num_vs=_NumVs,vs=_Vs,line_style=_LineStyle,
		line_width=_LineWidth,pen_pat=_PenPat,id=_Id,attrs=AttrList],
	tgif_attrs(AttrList).
tgif_poly(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 4, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = poly(_Color,_NumVs,_Vs,_LineStyle,_LineWidth,_PenPat,_Id,_Spline,
		AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,num_vs=_NumVs,vs=_Vs,line_style=_LineStyle,
		line_width=_LineWidth,pen_pat=_PenPat,id=_Id,line_type=_Spilne,
		attrs=AttrList],
	tgif_attrs(AttrList).
tgif_poly(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 8, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = poly(_Color,_NumVs,_Vs,_LineStyle,_LineWidth,_PenPat,_Id,_Spline,
		_ObjFill,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,num_vs=_NumVs,vs=_Vs,line_style=_LineStyle,
		line_width=_LineWidth,pen_pat=_PenPat,id=_Id,line_type=_Spilne,
		obj_fill=_ObjFill,attrs=AttrList],
	tgif_attrs(AttrList).
tgif_poly(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 13, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = poly(_Color,_NumVs,_Vs,_LineStyle,_LineWidth,_PenPat,_Id,_Spline,
		_ObjFill,_Dash,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,num_vs=_NumVs,vs=_Vs,line_style=_LineStyle,
		line_width=_LineWidth,pen_pat=_PenPat,id=_Id,line_type=_Spilne,
		obj_fill=_ObjFill,line_dash=_Dash,attrs=AttrList],
	tgif_attrs(AttrList).
tgif_poly(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 16, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = poly(_Color,_NumVs,_Vs,_LineStyle,_LineWidth,_PenPat,_Id,_Spline,
		_ObjFill,_Dash,_Rotation,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,num_vs=_NumVs,vs=_Vs,line_style=_LineStyle,
		line_width=_LineWidth,pen_pat=_PenPat,id=_Id,line_type=_Spilne,
		obj_fill=_ObjFill,line_dash=_Dash,rotation=_Rotation,
		attrs=AttrList],
	tgif_attrs(AttrList).
tgif_poly(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 25, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = poly(_Color,_NumVs,_Vs,_LineStyle,_LineWidth,_PenPat,_Id,_Spline,
		_ObjFill,_Dash,_Rotation,_ArrowHeadW,_ArrowHeadH,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,num_vs=_NumVs,vs=_Vs,line_style=_LineStyle,
		line_width=_LineWidth,pen_pat=_PenPat,id=_Id,line_type=_Spilne,
		obj_fill=_ObjFill,line_dash=_Dash,rotation=_Rotation,
		arrow_head_w=_ArrowHeadW,arrow_head_h=_ArrowHeadH,
		attrs=AttrList],
	tgif_attrs(AttrList).
tgif_poly(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 30, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = poly(_Color,_NumVs,_Vs,_LineStyle,_LineWidth,_PenPat,_Id,_Spline,
		_ObjFill,_Dash,_Rotation,_ArrowHeadW,_ArrowHeadH,_Locked,
		AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,num_vs=_NumVs,vs=_Vs,line_style=_LineStyle,
		line_width=_LineWidth,pen_pat=_PenPat,id=_Id,line_type=_Spilne,
		obj_fill=_ObjFill,line_dash=_Dash,rotation=_Rotation,
		arrow_head_w=_ArrowHeadW,arrow_head_h=_ArrowHeadH,
		locked=_Locked,attrs=AttrList],
	tgif_attrs(AttrList).
tgif_poly(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion >= 31, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = poly(_Color,_NumVs,_Vs,_LineStyle,_LineWidth,_PenPat,_Id,_Spline,
		_ObjFill,_Dash,_Rotation,_ArrowHeadW,_ArrowHeadH,_Locked,
		_SmoothSpecStr,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,num_vs=_NumVs,vs=_Vs,line_style=_LineStyle,
		line_width=_LineWidth,pen_pat=_PenPat,id=_Id,line_type=_Spilne,
		obj_fill=_ObjFill,line_dash=_Dash,rotation=_Rotation,
		arrow_head_w=_ArrowHeadW,arrow_head_h=_ArrowHeadH,
		locked=_Locked,smooth_spec=_SmoothSpecStr,attrs=AttrList],
	tgif_attrs(AttrList).

% --------------------------------------------------------------------- %

tgif_polygon(Obj) :- tgif_polygon(Obj,_).

tgif_polygon(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 3, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = polygon(_Color,_NumVs,_Vs,_ObjFill,_LineWidth,_PenPat),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,num_vs=_NumVs,vs=_Vs,obj_fill=_ObjFill,
		line_width=_LineWidth,pen_pat=_PenPat].
tgif_polygon(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 7, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = polygon(_Color,_NumVs,_Vs,_ObjFill,_LineWidth,_PenPat,_Spline),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,num_vs=_NumVs,vs=_Vs,obj_fill=_ObjFill,
		line_width=_LineWidth,pen_pat=_PenPat,line_type=_Spline].
tgif_polygon(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 8, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = polygon(_Color,_NumVs,_Vs,_ObjFill,_LineWidth,_PenPat,_Spline,
		_Id,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,num_vs=_NumVs,vs=_Vs,obj_fill=_ObjFill,
		line_width=_LineWidth,pen_pat=_PenPat,line_type=_Spline,
		id=_Id,attrs=AttrList],
	tgif_attrs(AttrList).
tgif_polygon(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 13, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = polygon(_Color,_NumVs,_Vs,_ObjFill,_LineWidth,_PenPat,_Spline,
		_Id,_Dash,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,num_vs=_NumVs,vs=_Vs,obj_fill=_ObjFill,
		line_width=_LineWidth,pen_pat=_PenPat,line_type=_Spline,
		id=_Id,line_dash=_Dash,attrs=AttrList],
	tgif_attrs(AttrList).
tgif_polygon(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 25, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = polygon(_Color,_NumVs,_Vs,_ObjFill,_LineWidth,_PenPat,_Spline,
		_Id,_Dash,_Rotation,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,num_vs=_NumVs,vs=_Vs,obj_fill=_ObjFill,
		line_width=_LineWidth,pen_pat=_PenPat,line_type=_Spline,
		id=_Id,line_dash=_Dash,rotation=_Rotation,attrs=AttrList],
	tgif_attrs(AttrList).
tgif_polygon(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 30, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = polygon(_Color,_NumVs,_Vs,_ObjFill,_LineWidth,_PenPat,_Spline,
		_Id,_Dash,_Rotation,_Locked,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,num_vs=_NumVs,vs=_Vs,obj_fill=_ObjFill,
		line_width=_LineWidth,pen_pat=_PenPat,line_type=_Spline,
		id=_Id,line_dash=_Dash,rotation=_Rotation,locked=_Locked,
		attrs=AttrList],
	tgif_attrs(AttrList).
tgif_polygon(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion >= 31, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = polygon(_Color,_NumVs,_Vs,_ObjFill,_LineWidth,_PenPat,_Spline,
		_Id,_Dash,_Rotation,_Locked,_SmoothSpecStr,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,num_vs=_NumVs,vs=_Vs,obj_fill=_ObjFill,
		line_width=_LineWidth,pen_pat=_PenPat,line_type=_Spline,
		id=_Id,line_dash=_Dash,rotation=_Rotation,locked=_Locked,
		smooth_spec=_SmoothSpecStr,attrs=AttrList],
	tgif_attrs(AttrList).

% --------------------------------------------------------------------- %

tgif_rcbox(Obj) :- tgif_rcbox(Obj,_).

tgif_rcbox(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 13, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = rcbox(_Color,_X1,_Y1,_X2,_Y2,_ObjFill,_LineWidth,_PenPat,_Dash,
		_Radius,_Id,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
		obj_fill=_ObjFill,line_width=_LineWidth,pen_pat=_PenPat,
		line_dash=_Dash,radius=_Radius,id=_Id,attrs=AttrList],
	tgif_attrs(AttrList).
tgif_rcbox(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 25, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = rcbox(_Color,_X1,_Y1,_X2,_Y2,_ObjFill,_LineWidth,_PenPat,_Dash,
		_Radius,_Id,_Rotation,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
		obj_fill=_ObjFill,line_width=_LineWidth,pen_pat=_PenPat,
		line_dash=_Dash,radius=_Radius,id=_Id,rotation=_Rotation,
		attrs=AttrList],
	tgif_attrs(AttrList).
tgif_rcbox(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion >= 26, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = rcbox(_Color,_X1,_Y1,_X2,_Y2,_ObjFill,_LineWidth,_PenPat,_Dash,
		_Radius,_Id,_Rotation,_Locked,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
		obj_fill=_ObjFill,line_width=_LineWidth,pen_pat=_PenPat,
		line_dash=_Dash,radius=_Radius,id=_Id,rotation=_Rotation,
		locked=_Locked,attrs=AttrList],
	tgif_attrs(AttrList).

% --------------------------------------------------------------------- %

tgif_arc(Obj) :- tgif_arc(Obj,_).

tgif_arc(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 13, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = arc(_Color,_ObjFill,_LineWidth,_PenPat,_Dash,_LtX,_LtY,_Xc,_Yc,
		_X1,_Y1,_X2,_Y2,_Dir,_W,_H,_Angle1,_Angle2,_Id,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,obj_fill=_ObjFill,line_width=_LineWidth,
		pen_pat=_PenPat,line_dash=_Dash,ltx=_LtX,lty=_LtY,
		xc=_Xc,yc=_Yc,x1=_X1,y1=_Y1,x2=_X2,y2=_Y2,clock_wise=_Dir,
		major_axis=_W,minor_axis_H,angle1=_Angle1,angle2=_Angle2,
		id=_Id,attrs=AttrList],
	tgif_attrs(AttrList).
tgif_arc(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 15, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = arc(_Color,_ObjFill,_LineWidth,_PenPat,_Dash,_LtX,_LtY,_Xc,_Yc,
		_X1,_Y1,_X2,_Y2,_Dir,_W,_H,_Angle1,_Angle2,_Id,_Rotation,
		AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,obj_fill=_ObjFill,line_width=_LineWidth,
		pen_pat=_PenPat,line_dash=_Dash,ltx=_LtX,lty=_LtY,
		xc=_Xc,yc=_Yc,x1=_X1,y1=_Y1,x2=_X2,y2=_Y2,clock_wise=_Dir,
		major_axis=_W,minor_axis_H,angle1=_Angle1,angle2=_Angle2,
		id=_Id,rotation=_Rotation,attrs=AttrList],
	tgif_attrs(AttrList).
tgif_arc(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 16, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = arc(_Color,_ObjFill,_LineWidth,_PenPat,_Dash,_LtX,_LtY,_Xc,_Yc,
		_X1,_Y1,_X2,_Y2,_Dir,_W,_H,_Angle1,_Angle2,_Id,_Rotation,
		_Style,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,obj_fill=_ObjFill,line_width=_LineWidth,
		pen_pat=_PenPat,line_dash=_Dash,ltx=_LtX,lty=_LtY,
		xc=_Xc,yc=_Yc,x1=_X1,y1=_Y1,x2=_X2,y2=_Y2,clock_wise=_Dir,
		major_axis=_W,minor_axis_H,angle1=_Angle1,angle2=_Angle2,
		id=_Id,rotation=_Rotation,line_style=_Style,attrs=AttrList],
	tgif_attrs(AttrList).
tgif_arc(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 25, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = arc(_Color,_ObjFill,_LineWidth,_PenPat,_Dash,_LtX,_LtY,_Xc,_Yc,
		_X1,_Y1,_X2,_Y2,_Dir,_W,_H,_Angle1,_Angle2,_Id,_Rotation,
		_Style,_ArrowHeadW,_ArrowHeadH,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,obj_fill=_ObjFill,line_width=_LineWidth,
		pen_pat=_PenPat,line_dash=_Dash,ltx=_LtX,lty=_LtY,
		xc=_Xc,yc=_Yc,x1=_X1,y1=_Y1,x2=_X2,y2=_Y2,clock_wise=_Dir,
		major_axis=_W,minor_axis_H,angle1=_Angle1,angle2=_Angle2,
		id=_Id,rotation=_Rotation,line_style=_Style,
		arrow_head_w=_ArrowHeadW,arrow_head_h=_ArrowHeadH,
		attrs=AttrList],
	tgif_attrs(AttrList).
tgif_arc(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion >= 26, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = arc(_Color,_ObjFill,_LineWidth,_PenPat,_Dash,_LtX,_LtY,_Xc,_Yc,
		_X1,_Y1,_X2,_Y2,_Dir,_W,_H,_Angle1,_Angle2,_Id,_Rotation,
		_Style,_ArrowHeadW,_ArrowHeadH,_Locked,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,obj_fill=_ObjFill,line_width=_LineWidth,
		pen_pat=_PenPat,line_dash=_Dash,ltx=_LtX,lty=_LtY,
		xc=_Xc,yc=_Yc,x1=_X1,y1=_Y1,x2=_X2,y2=_Y2,clock_wise=_Dir,
		major_axis=_W,minor_axis_H,angle1=_Angle1,angle2=_Angle2,
		id=_Id,rotation=_Rotation,line_style=_Style,
		arrow_head_w=_ArrowHeadW,arrow_head_h=_ArrowHeadH,
		locked=_Locked,attrs=AttrList],
	tgif_attrs(AttrList).

% --------------------------------------------------------------------- %

tgif_xbm(Obj) :- tgif_xbm(Obj,_).

tgif_xbm(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 13, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = xbm(_Color,_X1,_Y1,_X2,_Y2,_ObjFill,_Id,_BitmapStr,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,obj_fill=_ObjFill,
		id=_Id,xbm_str=_BitmapStr,attrs=AttrList],
	tgif_attrs(AttrList).
tgif_xbm(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 22, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = xbm(_Color,_X1,_Y1,_X2,_Y2,_ObjFill,_Id,_Rotation,_BitmapStr,
		AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,obj_fill=_ObjFill,
		id=_Id,rotation=_Rotation,xbm_str=_BitmapStr,attrs=AttrList],
	tgif_attrs(AttrList).
tgif_xbm(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 23, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = xbm(_Color,_X1,_Y1,_X2,_Y2,_ObjFill,_Id,_Rotation,_ImageW,_ImageH,
		_Rotate,_Flip,_BitmapStr,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,obj_fill=_ObjFill,
		id=_Id,rotation=_Rotation,image_w=_ImageW,image_h=_ImageH,
		rotate=_Rotate,flip=_Flip,xbm_str=_BitmapStr,attrs=AttrList],
	tgif_attrs(AttrList).
tgif_xbm(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 25, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	(	_RealType = 0, _NoBitmap = 0,
		Obj = xbm(_Color,_X1,_Y1,_X2,_Y2,_ObjFill,_Id,_Rotation,
			_ImageW,_ImageH,_Rotate,_Flip,_RealType,_LLX,_LLY,
			_URX,_URY,_NoBitmap,_Date,_File,_BitmapStr,AttrList),
		tgif_chk_output(OutputObj,Obj),
		Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
			obj_fill=_ObjFill,id=_Id,rotation=_Rotation,
			image_w=_ImageW,image_h=_ImageH,rotate=_Rotate,
			flip=_Flip,real_type='xbm',llx=_LLX,lly=_LLY,
			urx=_URX,ury=_URY,no_bitmap=_NoBitmap,
			xbm_str=_BitmapStr,attrs=AttrList]
	|	_RealType = 1, _NoBitmap = 0,
		Obj = xbm(_Color,_X1,_Y1,_X2,_Y2,_ObjFill,_Id,_Rotation,
			_ImageW,_ImageH,_Rotate,_Flip,_RealType,_LLX,_LLY,
			_URX,_URY,_NoBitmap,_Date,_File,_BitmapStr,AttrList),
		tgif_chk_output(OutputObj,Obj),
		Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
			obj_fill=_ObjFill,id=_Id,rotation=_Rotation,
			image_w=_ImageW,image_h=_ImageH,rotate=_Rotate,
			flip=_Flip,real_type='eps',llx=_LLX,lly=_LLY,
			urx=_URX,ury=_URY,no_bitmap=_NoBitmap,
			date=_Date,file=_File,xbm_str=_BitmapStr,attrs=AttrList]
	|	_RealType = 1, _NoBitmap = 1,
		Obj = xbm(_Color,_X1,_Y1,_X2,_Y2,_ObjFill,_Id,_Rotation,
			_ImageW,_ImageH,_Rotate,_Flip,_RealType,_LLX,_LLY,
			_URX,_URY,_NoBitmap,_Date,_File,AttrList),
		tgif_chk_output(OutputObj,Obj),
		Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
			obj_fill=_ObjFill,id=_Id,rotation=_Rotation,
			image_w=_ImageW,image_h=_ImageH,rotate=_Rotate,
			flip=_Flip,real_type='eps',llx=_LLX,lly=_LLY,
			urx=_URX,ury=_URY,no_bitmap=_NoBitmap,
			date=_Date,file=_File,attrs=AttrList]
	),
	tgif_attrs(AttrList).
tgif_xbm(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 28, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	(	_RealType = 0, _NoBitmap = 0,
		Obj = xbm(_Color,_X1,_Y1,_X2,_Y2,_ObjFill,_Id,_Rotation,
			_ImageW,_ImageH,_Rotate,_Flip,_RealType,_LLX,_LLY,
			_URX,_URY,_NoBitmap,_Locked,_Date,_File,_BitmapStr,
			AttrList),
		tgif_chk_output(OutputObj,Obj),
		Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
			obj_fill=_ObjFill,id=_Id,rotation=_Rotation,
			image_w=_ImageW,image_h=_ImageH,rotate=_Rotate,
			flip=_Flip,real_type='xbm',llx=_LLX,lly=_LLY,
			urx=_URX,ury=_URY,no_bitmap=_NoBitmap,locked=_Locked,
			date=_Date,file=_File,xbm_str=_BitmapStr,attrs=AttrList]
	|	_RealType = 1, _NoBitmap = 0,
		Obj = xbm(_Color,_X1,_Y1,_X2,_Y2,_ObjFill,_Id,_Rotation,
			_ImageW,_ImageH,_Rotate,_Flip,_RealType,_LLX,_LLY,
			_URX,_URY,_NoBitmap,_Locked,_Date,_File,_BitmapStr,
			AttrList),
		tgif_chk_output(OutputObj,Obj),
		Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
			obj_fill=_ObjFill,id=_Id,rotation=_Rotation,
			image_w=_ImageW,image_h=_ImageH,rotate=_Rotate,
			flip=_Flip,real_type='eps',llx=_LLX,lly=_LLY,
			urx=_URX,ury=_URY,no_bitmap=_NoBitmap,locked=_Locked,
			date=_Date,file=_File,xbm_str=_BitmapStr,attrs=AttrList]
	|	_RealType = 1, _NoBitmap = 1,
		Obj = xbm(_Color,_X1,_Y1,_X2,_Y2,_ObjFill,_Id,_Rotation,
			_ImageW,_ImageH,_Rotate,_Flip,_RealType,_LLX,_LLY,
			_URX,_URY,_NoBitmap,_Locked,_Date,_File,AttrList),
		tgif_chk_output(OutputObj,Obj),
		Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
			obj_fill=_ObjFill,id=_Id,rotation=_Rotation,
			image_w=_ImageW,image_h=_ImageH,rotate=_Rotate,
			flip=_Flip,real_type='eps',llx=_LLX,lly=_LLY,
			urx=_URX,ury=_URY,no_bitmap=_NoBitmap,locked=_Locked,
			date=_Date,file=_File,attrs=AttrList]
	),
	tgif_attrs(AttrList).
tgif_xbm(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion >= 29, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	(	_RealType = 0, _NoBitmap = 0, _SaveEPSF = 0,
		Obj = xbm(_Color,_X1,_Y1,_X2,_Y2,_ObjFill,_Id,_Rotation,
			_ImageW,_ImageH,_Rotate,_Flip,_RealType,_LLX,_LLY,
			_URX,_URY,_NoBitmap,_Locked,_SaveEPSF,
			_Date,_File,_BitmapStr,AttrList),
		tgif_chk_output(OutputObj,Obj),
		Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
			obj_fill=_ObjFill,id=_Id,rotation=_Rotation,
			image_w=_ImageW,image_h=_ImageH,rotate=_Rotate,
			flip=_Flip,real_type='xbm',llx=_LLX,lly=_LLY,
			urx=_URX,ury=_URY,no_bitmap=_NoBitmap,locked=_Locked,
			save_epsf=_SaveEPSF,
			date=_Date,file=_File,xbm_str=_BitmapStr,attrs=AttrList]
	|	_RealType = 1, _NoBitmap = 0, _SaveEPSF = 0,
		Obj = xbm(_Color,_X1,_Y1,_X2,_Y2,_ObjFill,_Id,_Rotation,
			_ImageW,_ImageH,_Rotate,_Flip,_RealType,_LLX,_LLY,
			_URX,_URY,_NoBitmap,_Locked,_SaveEPSF,
			_Date,_File,_BitmapStr,AttrList),
		tgif_chk_output(OutputObj,Obj),
		Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
			obj_fill=_ObjFill,id=_Id,rotation=_Rotation,
			image_w=_ImageW,image_h=_ImageH,rotate=_Rotate,
			flip=_Flip,real_type='eps',llx=_LLX,lly=_LLY,
			urx=_URX,ury=_URY,no_bitmap=_NoBitmap,locked=_Locked,
			save_epsf=_SaveEPSF,
			date=_Date,file=_File,xbm_str=_BitmapStr,attrs=AttrList]
	|	_RealType = 1, _NoBitmap = 1, _SaveEPSF = 0,
		Obj = xbm(_Color,_X1,_Y1,_X2,_Y2,_ObjFill,_Id,_Rotation,
			_ImageW,_ImageH,_Rotate,_Flip,_RealType,_LLX,_LLY,
			_URX,_URY,_NoBitmap,_Locked,_SaveEPSF,
			_Date,_File,AttrList),
		tgif_chk_output(OutputObj,Obj),
		Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
			obj_fill=_ObjFill,id=_Id,rotation=_Rotation,
			image_w=_ImageW,image_h=_ImageH,rotate=_Rotate,
			flip=_Flip,real_type='eps',llx=_LLX,lly=_LLY,
			urx=_URX,ury=_URY,no_bitmap=_NoBitmap,locked=_Locked,
			save_epsf=_SaveEPSF,
			date=_Date,file=_File,attrs=AttrList]
	|	_RealType = 1, _NoBitmap = 0, _SaveEPSF = 1,
		Obj = xbm(_Color,_X1,_Y1,_X2,_Y2,_ObjFill,_Id,_Rotation,
			_ImageW,_ImageH,_Rotate,_Flip,_RealType,_LLX,_LLY,
			_URX,_URY,_NoBitmap,_Locked,_SaveEPSF,
			_Date,_File,_NumEPSFLines,_EPSFLines,_BitmapStr,
			AttrList),
		tgif_chk_output(OutputObj,Obj),
		length(_EPSFLines,_NumEPSFLines),
		Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
			obj_fill=_ObjFill,id=_Id,rotation=_Rotation,
			image_w=_ImageW,image_h=_ImageH,rotate=_Rotate,
			flip=_Flip,real_type='eps',llx=_LLX,lly=_LLY,
			urx=_URX,ury=_URY,no_bitmap=_NoBitmap,locked=_Locked,
			save_epsf=_SaveEPSF,num_epsf_lines=_NumEPSFLines,
			epsf_lines=_EPSFLines,date=_Date,file=_File,
			xbm_str=_BitmapStr,attrs=AttrList]
	),
	tgif_attrs(AttrList).

% --------------------------------------------------------------------- %

tgif_xpm(Obj) :- tgif_xpm(Obj,_).

tgif_xpm(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 13, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = xpm(_Color,_X1,_Y1,_X2,_Y2,_UnUsedObjFill,_NumColors,_Id,
		ColorNames,Pixels,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
		obj_fill=_UnUsedObjFill,num_colors=_NumColors,id=_Id,
		color_names=ColorNames,pixels=Pixels,attrs=AttrList],
	tgif_color_info(ColorNames),
	tgif_pixels(Pixels),
	tgif_attrs(AttrList).
tgif_xpm(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 14, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = xpm(_Color,_X1,_Y1,_X2,_Y2,_UnUsedObjFill,_NumColors,_Id,
		_Rotation,ColorNames,Pixels,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
		obj_fill=_UnUsedObjFill,num_colors=_NumColors,id=_Id,
		rotation=_Rotation,color_names=ColorNames,pixels=Pixels,
		attrs=AttrList],
	tgif_color_info(ColorNames),
	tgif_pixels(Pixels),
	tgif_attrs(AttrList).
tgif_xpm(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 22, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = xpm(_Color,_X1,_Y1,_X2,_Y2,_UnUsedObjFill,_NumColors,
		_CharsPerPixel,_FirstPixelIsBg,_Id,_Rotation,ColorNames,Pixels,
		AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
		obj_fill=_UnUsedObjFill,num_colors=_NumColors,
		chars_per_pixel=_CharsPerPixel,
		first_pixel_is_bg=_FirstPixelIsBg,id=_Id,
		rotation=_Rotation,color_names=ColorNames,pixels=Pixels,
		attrs=AttrList],
	tgif_color_info(ColorNames),
	tgif_pixels(Pixels),
	tgif_attrs(AttrList).
tgif_xpm(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 25, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = xpm(_Color,_X1,_Y1,_X2,_Y2,_UnUsedObjFill,_NumColors,
		_CharsPerPixel,_FirstPixelIsBg,_Id,_Rotation,_ImageW,_ImageH,
		_Rotate,_Flip,ColorNames,Pixels,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
		obj_fill=_UnUsedObjFill,num_colors=_NumColors,
		chars_per_pixel=_CharsPerPixel,
		first_pixel_is_bg=_FirstPixelIsBg,id=_Id,rotation=_Rotation,
		image_w=_ImageW,image_h=_ImageH,rotate=_Rotate,flip=_Flip,
		color_names=ColorNames,pixels=Pixels,attrs=AttrList],
	tgif_color_info(ColorNames),
	tgif_pixels(Pixels),
	tgif_attrs(AttrList).
tgif_xpm(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion >= 26, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = xpm(_Color,_X1,_Y1,_X2,_Y2,_UnUsedObjFill,_NumColors,
		_CharsPerPixel,_FirstPixelIsBg,_Id,_Rotation,_ImageW,_ImageH,
		_Rotate,_Flip,_Locked,ColorNames,Pixels,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [color=_Color,ltx=_X1,lty=_Y1,rbx=_X2,rby=_Y2,
		obj_fill=_UnUsedObjFill,num_colors=_NumColors,
		chars_per_pixel=_CharsPerPixel,
		first_pixel_is_bg=_FirstPixelIsBg,id=_Id,rotation=_Rotation,
		image_w=_ImageW,image_h=_ImageH,rotate=_Rotate,flip=_Flip,
		locked=_Locked,color_names=ColorNames,pixels=Pixels,
		attrs=AttrList],
	tgif_color_info(ColorNames),
	tgif_pixels(Pixels),
	tgif_attrs(AttrList).

% --------------------------------------------------------------------- %

tgif_group(Obj) :- tgif_group(Obj,_).

tgif_group(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 20, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = group(ObjList,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [objs=ObjList,attrs=AttrList],
	tgif_objs(ObjList),
	tgif_attrs(AttrList).
tgif_group(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 25, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = group(ObjList,_Id,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [objs=ObjList,id=_Id,attrs=AttrList],
	tgif_objs(ObjList),
	tgif_attrs(AttrList).
tgif_group(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion >= 26, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = group(ObjList,_Id,_Locked,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [objs=ObjList,id=_Id,locked=_Locked,attrs=AttrList],
	tgif_objs(ObjList),
	tgif_attrs(AttrList).

% --------------------------------------------------------------------- %

tgif_sym(Obj) :- tgif_sym(Obj,_).

tgif_sym(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 20, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = sym(ObjList,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [objs=ObjList,attrs=AttrList],
	tgif_objs(ObjList),
	tgif_attrs(AttrList).
tgif_sym(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 25, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = sym(ObjList,_Id,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [objs=ObjList,id=_Id,attrs=AttrList],
	tgif_objs(ObjList),
	tgif_attrs(AttrList).
tgif_sym(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion >= 26, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = sym(ObjList,_Id,_Locked,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [objs=ObjList,id=_Id,locked=_Locked,attrs=AttrList],
	tgif_objs(ObjList),
	tgif_attrs(AttrList).

% --------------------------------------------------------------------- %

tgif_icon(Obj) :- tgif_icon(Obj,_).

tgif_icon(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 12, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = icon(ObjList,_Name,_Id,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [name=_Name,id=_Id,objs=ObjList,attrs=AttrList],
	tgif_objs(ObjList),
	tgif_attrs(AttrList).
tgif_icon(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion =< 25, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = icon(ObjList,_Name,_Id,_Rotation,_Flip,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [name=_Name,id=_Id,rotation=_Rotation,objs=ObjList,
		attrs=AttrList],
	tgif_objs(ObjList),
	tgif_attrs(AttrList).
tgif_icon(Obj,Parms) :-
	current_predicate(tgif_file_version,tgif_file_version(_)),
	tgif_file_version(FileVersion),
	FileVersion >= 26, !,
	( var(Obj) -> OutputObj = true ; OutputObj = false ),
	Obj = icon(ObjList,_Name,_Id,_Rotation,_Flip,_Locked,AttrList),
	tgif_chk_output(OutputObj,Obj),
	Parms = [name=_Name,id=_Id,rotation=_Rotation,locked=_Locked,
		objs=ObjList,attrs=AttrList],
	tgif_objs(ObjList),
	tgif_attrs(AttrList).

% ======================== support routines =========================== %

tgif_clean :- tgif_clean(state).

tgif_real_clean :-
	tgif_clean(state),
	tgif_clean(text), tgif_clean(box), tgif_clean(oval), tgif_clean(poly),
	tgif_clean(polygon), tgif_clean(rcbox), tgif_clean(arc),
	tgif_clean(xbm), tgif_clean(xpm),
	tgif_clean(group), tgif_clean(sym), tgif_clean(icon).

tgif_clean(Type) :-
	current_predicate(Type,Term), functor(Term,_F,A), abolish(Type,A), fail.
tgif_clean(_Type) :- !.

tgif_chk_output(true,Obj) :- !,
	functor(Obj,Functor,Arity), functor(DummyObj,Functor,Arity),
	current_predicate(Functor,DummyObj),
	!, call(Obj).
tgif_chk_output(_OutputObj,_Obj).

% --------------------------------------------------------------------- %

tgif_strs([]) :- !.
tgif_strs([Str|Strs]) :- atom_chars(_Line,Str), !, tgif_strs(Strs).

% --------------------------------------------------------------------- %

tgif_obj(Obj) :- var(Obj), !, tgif_obj(_,Obj).
tgif_obj(Obj) :- functor(Obj,Functor,_Arity), tgif_obj(Functor,Obj).

tgif_obj(text,Obj) :- tgif_text(Obj).
tgif_obj(box,Obj) :- tgif_box(Obj).
tgif_obj(oval,Obj) :- tgif_oval(Obj).
tgif_obj(poly,Obj) :- tgif_poly(Obj).
tgif_obj(polygon,Obj) :- tgif_polygon(Obj).
tgif_obj(rcbox,Obj) :- tgif_rcbox(Obj).
tgif_obj(arc,Obj) :- tgif_arc(Obj).
tgif_obj(xbm,Obj) :- tgif_xbm(Obj).
tgif_obj(xpm,Obj) :- tgif_xpm(Obj).
tgif_obj(group,Obj) :- tgif_group(Obj).
tgif_obj(sym,Obj) :- tgif_sym(Obj).
tgif_obj(icon,Obj) :- tgif_icon(Obj).

tgif_objs([]) :- !.
tgif_objs([Obj|Objs]) :- tgif_obj(Obj), !, tgif_objs(Objs).

% --------------------------------------------------------------------- %

tgif_attrs([]) :- !.
tgif_attrs([Attr|Attrs]) :- tgif_attr(Attr), !, tgif_attrs(Attrs).

tgif_attr(Attr) :- tgif_attr(Attr,_).

tgif_attr(Attr,Parms) :-
	Attr = attr(_Name,_Value,_ShowAll,_NameShown,_Inherited,TextObj),
	tgif_text(TextObj),
	Parms = [name=_Name,value=_Value,show_all=_ShowAll,
		name_shown=_NameShown,inherited=_Inherited,text_obj=TextObj],
	tgif_text(TextObj).

% --------------------------------------------------------------------- %

tgif_color_info([]) :- !.
tgif_color_info([ColorChar,ColorName|ColorInfos]) :-
	atom_chars(_Char,ColorChar),
	atom_chars(_Color,ColorName),
	!, tgif_color_info(ColorInfos).

% --------------------------------------------------------------------- %

tgif_pixels([]) :- !.
tgif_pixels([_RowOfChar|Pixels]) :- !, tgif_pixels(Pixels).
