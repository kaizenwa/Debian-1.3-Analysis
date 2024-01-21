#!/bin/sh

if [ $# -ne 2 ]; then
	echo usage: $0 directory extension
	exit 1
fi

if ! cd $1; then
	echo $0: can not cd to $1
	exit 1
fi

ext=$2

function fixman {
	if [ $# -lt 2 ]; then
		echo fixman: called with too few args
		exit 1
	fi

	local base=$1
	shift 1

	for f in $*; do
		if ! ln -sf $base.$ext $f.$ext; then
			echo fixman: cant ln $base.$ext to $f.$ext
		fi
	done
}

fixman 3DBorder Tk_Get3DBorder Tk_Draw3DRectangle Tk_Fill3DRectangle Tk_Draw3DPolygon Tk_Fill3DPolygon Tk_3DVerticalBevel Tk_3DHorizontalBevel Tk_SetBackgroundFromBorder Tk_NameOf3DBorder Tk_3DBorderColor Tk_3DBorderGC Tk_Free3DBorder
fixman BackgdErr Tk_BackgroundError
fixman BindTable Tk_CreateBindingTable Tk_DeleteBindingTable Tk_CreateBinding Tk_DeleteBinding Tk_GetBinding Tk_GetAllBindings Tk_DeleteAllBindings Tk_BindEvent
fixman CanvPsY Tk_CanvasPsY Tk_CanvasPsBitmap Tk_CanvasPsColor Tk_CanvasPsFont Tk_CanvasPsPath Tk_CanvasPsStipple
fixman CanvTkwin Tk_CanvasTkwin Tk_CanvasGetCoord Tk_CanvasDrawableCoords Tk_CanvasSetStippleOrigin Tk_CanvasWindowCoords Tk_CanvasEventuallyRedraw
fixman CanvTxtInfo Tk_CanvasTextInfo
fixman Clipboard Tk_ClipboardClear Tk_ClipboardAppend
fixman ClrSelect Tk_ClearSelection
fixman ConfigWidg Tk_ConfigureWidget Tk_Offset Tk_ConfigureInfo Tk_ConfigureValue Tk_FreeOptions
fixman ConfigWind Tk_ConfigureWindow Tk_MoveWindow Tk_ResizeWindow Tk_MoveResizeWindow Tk_SetWindowBorderWidth Tk_ChangeWindowAttributes Tk_SetWindowBackground Tk_SetWindowBackgroundPixmap Tk_SetWindowBorder Tk_SetWindowBorderPixmap Tk_SetWindowColormap Tk_DefineCursor Tk_UndefineCursor
fixman CoordToWin Tk_CoordsToWindow
fixman CrtErrHdlr Tk_CreateErrorHandler Tk_DeleteErrorHandler
fixman CrtGenHdlr Tk_CreateGenericHandler Tk_DeleteGenericHandler
fixman CrtImgType Tk_CreateImageType
fixman CrtItemType Tk_CreateItemType Tk_GetItemTypes
fixman CrtMainWin Tk_CreateMainWindow Tk_CreateWindow Tk_CreateWindowFromPath Tk_DestroyWindow Tk_MakeWindowExist
fixman CrtPhImgFmt Tk_CreatePhotoImageFormat
fixman CrtSelHdlr Tk_CreateSelHandler Tk_DeleteSelHandler
fixman DeleteImg Tk_DeleteImage
fixman DoOneEvent Tk_DoOneEvent Tk_MainLoop Tk_HandleEvent
fixman DoWhenIdle Tk_DoWhenIdle Tk_CancelIdleCall
fixman DrawFocHlt Tk_DrawFocusHighlight
fixman EventHndlr Tk_CreateEventHandler Tk_DeleteEventHandler
fixman EventInit Tk_EventInit
fixman FileHndlr Tk_CreateFileHandler Tk_CreateFileHandler2 Tk_DeleteFileHandler
fixman FindPhoto Tk_FindPhoto Tk_PhotoPutBlock Tk_PhotoPutZoomedBlock Tk_PhotoGetImage Tk_PhotoBlank Tk_PhotoExpand Tk_PhotoGetSize Tk_PhotoSetSize
fixman FreeXId Tk_FreeXId
fixman GeomReq Tk_GeometryRequest Tk_SetInternalBorder
fixman GetAnchor Tk_GetAnchor Tk_NameOfAnchor
fixman GetBitmap Tk_GetBitmap Tk_DefineBitmap Tk_NameOfBitmap Tk_SizeOfBitmap Tk_FreeBitmap Tk_GetBitmapFromData
fixman GetCapStyl Tk_GetCapStyle Tk_NameOfCapStyle
fixman GetClrmap Tk_GetColormap Tk_FreeColormap
fixman GetColor Tk_GetColor Tk_GetColorByValue Tk_NameOfColor Tk_FreeColor
fixman GetCursor Tk_GetCursor Tk_GetCursorFromData Tk_NameOfCursor Tk_FreeCursor
fixman GetFontStr Tk_GetFontStruct Tk_NameOfFontStruct Tk_FreeFontStruct
fixman GetGC Tk_GetGC Tk_FreeGC
fixman GetImage Tk_GetImage Tk_RedrawImage Tk_SizeOfImage Tk_FreeImage
fixman GetJoinStl Tk_GetJoinStyle Tk_NameOfJoinStyle
fixman GetJustify Tk_GetJustify Tk_NameOfJustify
fixman GetOption Tk_GetOption
fixman GetPixels Tk_GetPixels Tk_GetScreenMM
fixman GetPixmap Tk_GetPixmap Tk_FreePixmap
fixman GetRelief Tk_GetRelief Tk_NameOfRelief
fixman GetRootCrd Tk_GetRootCoords
fixman GetScroll Tk_GetScrollInfo
fixman GetSelect Tk_GetSelection
fixman GetUid Tk_GetUid Tk_Uid
fixman GetVRoot Tk_GetVRootGeometry
fixman GetVisual Tk_GetVisual
fixman IdToWindow Tk_IdToWindow
fixman ImgChanged Tk_ImageChanged
fixman InternAtom Tk_InternAtom Tk_GetAtomName
fixman MainWin Tk_MainWindow
fixman MaintGeom Tk_MaintainGeometry Tk_UnmaintainGeometry
fixman ManageGeom Tk_ManageGeometry
fixman MapWindow Tk_MapWindow Tk_UnmapWindow
fixman MoveToplev Tk_MoveToplevelWindow
fixman Name Tk_Name Tk_PathName Tk_NameToWindow
fixman NameOfImg Tk_NameOfImage
fixman OwnSelect Tk_OwnSelection
fixman ParseArgv Tk_ParseArgv
fixman Preserve Tk_Preserve Tk_Release Tk_EventuallyFree
fixman Restack Tk_RestackWindow
fixman RestrictEv Tk_RestrictEvents
fixman SetAppName Tk_SetAppName
fixman SetClass Tk_SetClass Tk_Class
fixman SetGrid Tk_SetGrid Tk_UnsetGrid
fixman SetVisual Tk_SetWindowVisual
fixman Sleep Tk_Sleep
fixman StrictMotif Tk_StrictMotif
fixman TimerHndlr Tk_CreateTimerHandler Tk_DeleteTimerHandler
fixman WindowId Tk_WindowId Tk_Parent Tk_Display Tk_DisplayName Tk_ScreenNumber Tk_Screen Tk_X Tk_Y Tk_Width Tk_Height Tk_Changes Tk_Attributes Tk_IsMapped Tk_IsTopLevel Tk_ReqWidth Tk_ReqHeight Tk_InternalBorderWidth Tk_Visual Tk_Depth Tk_Colormap
