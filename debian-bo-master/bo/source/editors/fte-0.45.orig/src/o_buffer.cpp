/*    o_buffer.cpp
 *
 *    Copyright (c) 1994-1996, Marko Macek
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 */

#include "fte.h"

SearchReplaceOptions LSearch = { 0 };

EViewPort *EBuffer::CreateViewPort(EView *V) {
    V->Port = new EEditPort(this, V);
    AddView(V);
    
    if (Loaded == 0) {
        Load();

#ifdef CONFIG_OBJ_MESSAGES
        if (CompilerMsgs)
            CompilerMsgs->FindFileErrors(this);
#endif

#ifdef CONFIG_HISTORY
        int r, c;
        
        if (RetrieveFPos(FileName, r, c) == 1)
            SetNearPosR(c, r);
        V->Port->ReCenter = 1;
#endif
    }
    return V->Port;
}

EEditPort::EEditPort(EBuffer *B, EView *V): EViewPort(V) {
    Buffer = B;
    Rows = Cols = 0;
    OldTP.Row = -1;
    OldTP.Col = -1;
    GetPos();
    TP = B->TP;
    CP = B->CP;
    if (V && V->MView && V->MView->Win) {
        V->MView->ConQuerySize(&Cols, &Rows);
        Rows--;
    }
}

EEditPort::~EEditPort() {
    StorePos();
}

void EEditPort::Resize(int Width, int Height) {
    Cols = Width;
    Rows = Height - 1;
    RedrawAll();
}

int EEditPort::SetTop(int Col, int Line) {
    int A, B;
    
    if (Line >= Buffer->VCount) Line = Buffer->VCount - 1;
    if (Line < 0) Line = 0;
    
    A = Line;
    B = Line + Rows;
    
    TP.Row = Line;
    TP.Col = Col;
    
    if (A >= Buffer->VCount) A = Buffer->VCount - 1;
    if (B >= Buffer->VCount) {
        B = Buffer->VCount - 1;
    }
    Buffer->Draw(Buffer->VToR(A), -1);
    return 1;
}

void EEditPort::StorePos() {
    Buffer->CP = CP;
    Buffer->TP = TP;
}

void EEditPort::GetPos() {
    CP = Buffer->CP;
    TP = Buffer->TP;
}

void EEditPort::ScrollY(int Delta) {
    // optimization
    // no need to scroll (clear) entire window which we are about to redraw
    if (Delta >= Rows || -Delta >= Rows)
        return ;

    if (Delta < 0) {
        Delta = -Delta;
        if (Delta > Rows) return;
        View->MView->ConScroll(csDown, 0, 0, Cols, Rows, hcPlain_Background, Delta);
    } else {
        if (Delta > Rows) return;
        View->MView->ConScroll(csUp, 0, 0, Cols, Rows, hcPlain_Background, Delta);
    }
}

void EEditPort::DrawLine(int L, TDrawBuffer B) {
    if (L < TP.Row) return;
    if (L >= TP.Row + Rows) return;
    if (View->MView->Win->GetViewContext() == View->MView)
        View->MView->ConPutBox(0, L - TP.Row, Cols, 1, B);
    //    printf("%d %d (%d %d %d %d)\n", 0, L - TP.Row, view->sX, view->sY, view->sW, view->sH);
}

void EEditPort::RedrawAll() {
    Buffer->Draw(TP.Row, -1);
    ///    Redraw(0, 0, Cols, Rows);
}

int EBuffer::GetContext() { 
    return CONTEXT_FILE;
}

void EEditPort::HandleEvent(TEvent &Event) { 
    EViewPort::HandleEvent(Event);
    switch (Event.What) {
    case evKeyDown:
        {
            char Ch;
            if (GetCharFromEvent(Event, &Ch)) {
                if (Buffer->BeginMacro() == 0)
                    return ;
                Buffer->TypeChar(Ch);
                Event.What = evNone;
	    }
        }
        break;
    case evCommand:
        switch (Event.Msg.Command) {
        case cmVScrollUp:
            Buffer->ScrollDown(1);
            Event.What = evNone;
            break;
        case cmVScrollDown:
            Buffer->ScrollUp(1);
            Event.What = evNone;
            break;
        case cmVScrollPgUp:
            Buffer->ScrollDown(Rows);
            Event.What = evNone;
            break;
        case cmVScrollPgDn:
            Buffer->ScrollUp(Rows);
            Event.What = evNone;
            break;
        case cmVScrollMove:
            {
                int ypos;
                
//                fprintf(stderr, "Pos = %d\n\x7", Event.Msg.Param1);
                ypos = Buffer->CP.Row - TP.Row;
                Buffer->SetNearPos(Buffer->CP.Col, Event.Msg.Param1 + ypos);
                SetTop(TP.Col, Event.Msg.Param1);
                RedrawAll();
            }
            Event.What = evNone;
            break;
        case cmHScrollLeft:
            Buffer->ScrollRight(1);
            Event.What = evNone;
            break;
        case cmHScrollRight:
            Buffer->ScrollLeft(1);
            Event.What = evNone;
            break;
        case cmHScrollPgLt:
            Buffer->ScrollRight(Cols);
            Event.What = evNone;
            break;
        case cmHScrollPgRt:
            Buffer->ScrollLeft(Cols);
            Event.What = evNone;
            break;
        case cmHScrollMove:
            {
                int xpos;
                
                xpos = Buffer->CP.Col - TP.Col;
                Buffer->SetNearPos(Event.Msg.Param1 + xpos, Buffer->CP.Row);
                SetTop(Event.Msg.Param1, TP.Row);
                RedrawAll();
            }
            Event.What = evNone;
            break;
        }
        break;
    case evMouseDown:
    case evMouseMove:
    case evMouseAuto:
    case evMouseUp:
        HandleMouse(Event);
        break;
    }
}
void EEditPort::HandleMouse(TEvent &Event) {
    int x, y, xx, yy, W, H;
    
    View->MView->ConQuerySize(&W, &H);
    
    x = Event.Mouse.X;
    y = Event.Mouse.Y;
    
    if (Event.What != evMouseDown || y < H - 1) {
        xx = x + TP.Col;
        yy = y + TP.Row;
        if (yy >= Buffer->VCount) yy = Buffer->VCount - 1;
        if (yy < 0) yy = 0;
        if (xx < 0) xx = 0;
        
        switch (Event.What) {
        case evMouseDown:
            if (Event.Mouse.Y == H - 1)
                break;
            if (View->MView->Win->CaptureMouse(1))
                View->MView->MouseCaptured = 1;
            else
                break;
            
            View->MView->MouseMoved = 0;
            
            if (Event.Mouse.Buttons == 1) {
                Buffer->SetNearPos(xx, yy);
                switch (Event.Mouse.Count % 5) {
                case 1:
                    break;
                case 2:
                    Buffer->BlockSelectWord();
                    break;
                case 3:
                    Buffer->BlockSelectLine();
                    break;
                case 4:
                    Buffer->BlockSelectPara();
                    break;
                }
                //            Window->Buffer->Redraw();
                if (SystemClipboard) {
                    Buffer->NextCommand();
                    Buffer->BlockCopy(0);
                }
                Event.What = evNone;
            } else if (Event.Mouse.Buttons == 2) {
                Buffer->SetNearPos(xx, yy);
            }
            break;
        case evMouseAuto:
        case evMouseMove:
            if (View->MView->MouseCaptured) {
                if (Event.Mouse.Buttons == 1) {
                    if (!View->MView->MouseMoved) {
                        if (Event.Mouse.KeyMask == kfCtrl) Buffer->BlockMarkColumn();
                        else if (Event.Mouse.KeyMask == kfAlt) Buffer->BlockMarkLine();
                        else Buffer->BlockMarkStream();
                        Buffer->BlockUnmark();
                        if (Event.What == evMouseMove)
                            View->MView->MouseMoved = 1;
                    }
                    Buffer->BlockExtendBegin();
                    Buffer->SetNearPos(xx, yy);
                    Buffer->BlockExtendEnd();
                } else if (Event.Mouse.Buttons == 2) {
                    if (Event.Mouse.KeyMask == kfAlt) {
                    } else {
                        Buffer->SetNearPos(xx, yy);
                    }
                }

                Event.What = evNone;
            }
            break;
/*        case evMouseAuto:
            if (View->MView->MouseCaptured) {
                Event.What = evNone;
            }
            break;*/
        case evMouseUp:
            if (View->MView->MouseCaptured)
                View->MView->Win->CaptureMouse(0);
            else
                break;
            View->MView->MouseCaptured = 0;
            if (Event.Mouse.Buttons == 1) {
                if (View->MView->MouseMoved)
                    if (SystemClipboard) {
                        Buffer->NextCommand();
                        Buffer->BlockCopy(0);
                    }
            }
            if (Event.Mouse.Buttons == 2) {
                if (!View->MView->MouseMoved) {
                    EEventMap *Map = View->MView->Win->GetEventMap();
                    char *MName = 0;
                    
                    if (Map)
                        MName = Map->GetMenu(EM_LocalMenu);
                    if (MName == 0)
                        MName = "Local";
                    View->MView->Win->Parent->PopupMenu(MName);
                }
            }
            if (Event.Mouse.Buttons == 4) {
                if (SystemClipboard) {
                    Buffer->NextCommand();
                    if (Event.Mouse.KeyMask == 0) 
                        Buffer->BlockPasteStream();
                    else if (Event.Mouse.KeyMask == kfCtrl)
                        Buffer->BlockPasteColumn();
                    else if (Event.Mouse.KeyMask == kfAlt)
                        Buffer->BlockPasteLine();
                }
            }
            Event.What = evNone;
            break;
        }
    } 
}

void EEditPort::UpdateView() {
    Buffer->Redraw();
}

void EEditPort::RepaintView() {
    RedrawAll();
}

void EEditPort::UpdateStatus() {
}

void EEditPort::RepaintStatus() {
    //Buffer->Redraw();
}

EEventMap *EBuffer::GetEventMap() {
    return FindActiveMap(Mode);
}

int EBuffer::BeginMacro() {
    return NextCommand();
}

int EBuffer::ExecCommand(int Command, ExState &State) {
    switch (Command) {
    case ExMoveUp:                return MoveUp();
    case ExMoveDown:              return MoveDown();
    case ExMoveLeft:              return MoveLeft();
    case ExMoveRight:             return MoveRight();
    case ExMovePrev:              return MovePrev();
    case ExMoveNext:              return MoveNext();
    case ExMoveWordLeft:          return MoveWordLeft();
    case ExMoveWordRight:         return MoveWordRight();
    case ExMoveWordPrev:          return MoveWordPrev();
    case ExMoveWordNext:          return MoveWordNext();
    case ExMoveWordEndLeft:       return MoveWordEndLeft();
    case ExMoveWordEndRight:      return MoveWordEndRight();
    case ExMoveWordEndPrev:       return MoveWordEndPrev();
    case ExMoveWordEndNext:       return MoveWordEndNext();
    case ExMoveLineStart:         return MoveLineStart();
    case ExMoveLineEnd:           return MoveLineEnd();
    case ExMovePageStart:         return MovePageStart();
    case ExMovePageEnd:           return MovePageEnd();
    case ExMovePageUp:            return MovePageUp();
    case ExMovePageDown:          return MovePageDown();
    case ExMovePageLeft:          return MovePageLeft();
    case ExMovePageRight:         return MovePageEnd();
    case ExMoveFileStart:         return MoveFileStart();
    case ExMoveFileEnd:           return MoveFileEnd();
    case ExMoveBlockStart:        return MoveBlockStart();
    case ExMoveBlockEnd:          return MoveBlockEnd();
    case ExMoveFirstNonWhite:     return MoveFirstNonWhite();
    case ExMoveLastNonWhite:      return MoveLastNonWhite();
    case ExMovePrevEqualIndent:   return MovePrevEqualIndent();
    case ExMoveNextEqualIndent:   return MoveNextEqualIndent();
    case ExMovePrevTab:           return MovePrevTab();
    case ExMoveNextTab:           return MoveNextTab();
    case ExMoveTabStart:          return MoveTabStart();
    case ExMoveTabEnd:            return MoveTabEnd();
    case ExMoveLineTop:           return MoveLineTop();
    case ExMoveLineCenter:        return MoveLineCenter();
    case ExMoveLineBottom:        return MoveLineBottom();
    case ExScrollLeft:            return ScrollLeft(State);
    case ExScrollRight:           return ScrollRight(State);
    case ExScrollDown:            return ScrollDown(State);
    case ExScrollUp:              return ScrollUp(State);
    case ExKillLine:              return KillLine();
    case ExKillChar:              return KillChar();
    case ExKillCharPrev:          return KillCharPrev();
    case ExKillWord:              return KillWord();
    case ExKillWordPrev:          return KillWordPrev();
    case ExKillToLineStart:       return KillToLineStart();
    case ExKillToLineEnd:         return KillToLineEnd();
    case ExKillBlock:             return KillBlock();
    case ExBackSpace:             return BackSpace();
    case ExDelete:                return Delete();
    case ExCharCaseUp:            return CharCaseUp();
    case ExCharCaseDown:          return CharCaseDown();
    case ExCharCaseToggle:        return CharCaseToggle();
    case ExLineCaseUp:            return LineCaseUp();
    case ExLineCaseDown:          return LineCaseDown();
    case ExLineCaseToggle:        return LineCaseToggle();
    case ExLineInsert:            return LineInsert();
    case ExLineAdd:               return LineAdd();
    case ExLineSplit:             return LineSplit();
    case ExLineJoin:              return LineJoin();
    case ExLineNew:               return LineNew();
    case ExLineIndent:            return LineIndent();
    case ExLineTrim:              return LineTrim();
    case ExInsertSpacesToTab:     return InsertSpacesToTab(0);
    case ExInsertTab:             return InsertTab();
    case ExInsertSpace:           return InsertSpace();
    case ExWrapPara:              
#ifdef CONFIG_WORDWRAP
        return WrapPara();
#else
        return ErFAIL;
#endif
    case ExInsPrevLineChar:       return InsPrevLineChar();
    case ExInsPrevLineToEol:      return InsPrevLineToEol();
    case ExLineDuplicate:         return LineDuplicate();
    case ExBlockBegin:            return BlockBegin();
    case ExBlockEnd:              return BlockEnd();
    case ExBlockUnmark:           return BlockUnmark();
    case ExBlockCut:              return BlockCut(0);
    case ExBlockCopy:             return BlockCopy(0);
    case ExBlockCutAppend:        return BlockCut(1);
    case ExBlockCopyAppend:       return BlockCopy(1);
    case ExClipClear:             return ClipClear();
    case ExBlockPaste:            return BlockPaste();
    case ExBlockKill:             return BlockKill();
    case ExBlockIndent:           return BlockIndent();
    case ExBlockUnindent:         return BlockUnindent();
    case ExBlockClear:            return BlockClear();
    case ExBlockMarkStream:       return BlockMarkStream();
    case ExBlockMarkLine:         return BlockMarkLine();
    case ExBlockMarkColumn:       return BlockMarkColumn();
    case ExBlockCaseUp:           return BlockCaseUp();
    case ExBlockCaseDown:         return BlockCaseDown();
    case ExBlockCaseToggle:       return BlockCaseToggle();
    case ExBlockExtendBegin:      return BlockExtendBegin();
    case ExBlockExtendEnd:        return BlockExtendEnd();
    case ExBlockReIndent:         return BlockReIndent();
    case ExBlockSelectWord:       return BlockSelectWord();
    case ExBlockSelectLine:       return BlockSelectLine();
    case ExBlockSelectPara:       return BlockSelectPara();
    case ExBlockUnTab:            return BlockUnTab();
    case ExBlockEnTab:            return BlockEnTab();
#ifdef CONFIG_UNDOREDO
    case ExUndo:                  return Undo();
    case ExRedo:                  return Redo();
#else
    case ExUndo:                  return ErFAIL;
    case ExRedo:                  return ErFAIL;
#endif
    case ExMatchBracket:          return MatchBracket();
    case ExMovePrevPos:           return MovePrevPos();
    case ExMoveSavedPosCol:       return MoveSavedPosCol();
    case ExMoveSavedPosRow:       return MoveSavedPosRow();
    case ExMoveSavedPos:          return MoveSavedPos();
    case ExSavePos:               return SavePos();
    case ExCompleteWord:          return CompleteWord();
    case ExBlockPasteStream:      return BlockPasteStream();
    case ExBlockPasteLine:        return BlockPasteLine();
    case ExBlockPasteColumn:      return BlockPasteColumn();
    case ExShowPosition:          return ShowPosition();
    case ExFoldCreate:            return FoldCreate(VToR(CP.Row));
    case ExFoldDestroy:           return FoldDestroy(VToR(CP.Row));
    case ExFoldDestroyAll:        return FoldDestroyAll();
    case ExFoldPromote:           return FoldPromote(VToR(CP.Row));
    case ExFoldDemote:            return FoldDemote(VToR(CP.Row));
    case ExFoldOpen:              return FoldOpen(VToR(CP.Row));
    case ExFoldOpenNested:        return FoldOpenNested();
    case ExFoldClose:             return FoldClose(VToR(CP.Row));
    case ExFoldOpenAll:           return FoldOpenAll();
    case ExFoldCloseAll:          return FoldCloseAll();
    case ExFoldToggleOpenClose:   return FoldToggleOpenClose();
    case ExMoveFoldTop:           return MoveFoldTop();
    case ExMoveFoldPrev:          return MoveFoldPrev();
    case ExMoveFoldNext:          return MoveFoldNext();
    case ExFileSave:              return Save();
    case ExFilePrint:             return FilePrint();
    case ExBlockPrint:            return BlockPrint();
    case ExHilitWord:
#ifdef CONFIG_WORD_HILIT
        return HilitWord();
#else
        return ErFAIL;
#endif
    case ExSearchWordPrev:        return SearchWord(SEARCH_BACK | SEARCH_NEXT);
    case ExSearchWordNext:        return SearchWord(SEARCH_NEXT);
    case ExHilitMatchBracket:     return HilitMatchBracket();
    case ExToggleAutoIndent:      return ToggleAutoIndent();
    case ExToggleInsert:          return ToggleInsert();
    case ExToggleExpandTabs:      return ToggleExpandTabs();
    case ExToggleShowTabs:        return ToggleShowTabs();
    case ExToggleUndo:            return ToggleUndo();
    case ExToggleReadOnly:        return ToggleReadOnly();
    case ExToggleKeepBackups:     return ToggleKeepBackups();
    case ExToggleMatchCase:       return ToggleMatchCase();
    case ExToggleBackSpKillTab:   return ToggleBackSpKillTab();
    case ExToggleDeleteKillTab:   return ToggleDeleteKillTab();
    case ExToggleSpaceTabs:       return ToggleSpaceTabs();
    case ExToggleIndentWithTabs:  return ToggleIndentWithTabs();
    case ExToggleBackSpUnindents: return ToggleBackSpUnindents();
    case ExToggleWordWrap:        return ToggleWordWrap();
    case ExToggleTrim:            return ToggleTrim();
    case ExToggleShowMarkers:     return ToggleShowMarkers();
    case ExSetLeftMargin:         return SetLeftMargin();
    case ExSetRightMargin:        return SetRightMargin();
        
        // stuff with UI
    case ExMoveToLine:          return MoveToLine(State);
    case ExMoveToColumn:        return MoveToColumn(State);
    case ExFoldCreateByRegexp:  return FoldCreateByRegexp(State);
#ifdef CONFIG_BOOKMARKS
    case ExPlaceBookmark:       return PlaceBookmark(State);
    case ExRemoveBookmark:      return RemoveBookmark(State);
    case ExGotoBookmark:        return GotoBookmark(State);
#else
    case ExPlaceBookmark:       return ErFAIL;
    case ExRemoveBookmark:      return ErFAIL;
    case ExGotoBookmark:        return ErFAIL;
#endif
    case ExInsertString:        return InsertString(State);
    case ExSelfInsert:          return SelfInsert(State);
    case ExFileReload:          return FileReload(State);
    case ExFileSaveAs:          return FileSaveAs(State);
    case ExFileWriteTo:         return FileWriteTo(State);
    case ExBlockRead:           return BlockRead(State);
    case ExBlockReadStream:     return BlockReadStream(State);
    case ExBlockReadLine:       return BlockReadLine(State);
    case ExBlockReadColumn:     return BlockReadColumn(State);
    case ExBlockWrite:          return BlockWrite(State);
    case ExBlockSort:           return BlockSort(0);
    case ExBlockSortReverse:    return BlockSort(1);
    case ExFind:                return Find(State);
    case ExFindReplace:         return FindReplace(State);
    case ExFindRepeat:          return FindRepeat(State);
    case ExFindRepeatOnce:      return FindRepeatOnce(State);
    case ExFindRepeatReverse:   return FindRepeatReverse(State);
    case ExSearch:              return Search(State);
    case ExSearchB:             return SearchB(State);
    case ExSearchRx:            return SearchRx(State);
    case ExSearchAgain:         return SearchAgain(State);
    case ExSearchAgainB:        return SearchAgainB(State);
    case ExSearchReplace:       return SearchReplace(State);
    case ExSearchReplaceB:      return SearchReplaceB(State);
    case ExSearchReplaceRx:     return SearchReplaceRx(State);
    case ExInsertChar:          return InsertChar(State);
    case ExTypeChar:            return TypeChar(State);
    case ExChangeMode:          return ChangeMode(State);
    //case ExChangeKeys:          return ChangeKeys(State);
    case ExChangeFlags:         return ChangeFlags(State);
    case ExChangeTabSize:       return ChangeTabSize(State);
    case ExChangeLeftMargin:    return ChangeLeftMargin(State);
    case ExChangeRightMargin:   return ChangeRightMargin(State);
    case ExASCIITable:          
#ifdef CONFIG_I_ASCII
        return ASCIITable(State);
#else
        return ErFAIL;
#endif
    case ExCharTrans:           return CharTrans(State);
    case ExLineTrans:           return LineTrans(State);
    case ExBlockTrans:          return BlockTrans(State);

#ifdef CONFIG_TAGS
    case ExTagFind:             return FindTag(State);
    case ExTagFindWord:         return FindTagWord();
#endif

    case ExBlockMarkFunction:   return BlockMarkFunction();
    case ExIndentFunction:      return IndentFunction();
    case ExMoveFunctionPrev:    return MoveFunctionPrev();
    case ExMoveFunctionNext:    return MoveFunctionNext();
    case ExInsertDate:      	return InsertDate(State);
    case ExInsertUid:           return InsertUid();
    }
    return EModel::ExecCommand(Command, State);
}

void EBuffer::HandleEvent(TEvent &Event) {
    EModel::HandleEvent(Event);
}

int EBuffer::MoveToLine(ExState &State) {
    int No = 0;
    
    if (State.GetIntParam(&No) == 0) {
        char Num[10];

        sprintf(Num, "%d", VToR(CP.Row) + 1);
        if (View->MView->Win->GetStr("Goto Line", sizeof(Num), Num, HIST_POSITION) == 0)
            return 0;
        No = atol(Num);
    }
    return SetNearPosR(CP.Col, No - 1);
}

int EBuffer::MoveToColumn(ExState &State) {
    int No = 0;
    
    if (State.GetIntParam(&No) == 0) {
        char Num[10];
        
        sprintf(Num, "%d", CP.Col + 1);
        if (View->MView->Win->GetStr("Goto Column", 8, Num, HIST_POSITION) == 0) return 0;
        No = atol(Num);
    }
    return SetNearPos(No - 1, CP.Row);
}

int EBuffer::FoldCreateByRegexp(ExState &State) {
    char strbuf[1024] = "";
    
    if (State.GetStrParam(strbuf, sizeof(strbuf)) == 0) {
        if (View->MView->Win->GetStr("Create Fold Regexp", sizeof(strbuf), strbuf, HIST_REGEXP) == 0) return 0;
    }
    return FoldCreateByRegexp(strbuf);
}

#ifdef CONFIG_BOOKMARKS
int EBuffer::PlaceBookmark(ExState &State) {
    char name[256] = "";
    EPoint P = CP;
    
    P.Row = VToR(P.Row);
    
    if (State.GetStrParam(name, sizeof(name)) == 0)
        if (View->MView->Win->GetStr("Place Bookmark", sizeof(name), name, HIST_BOOKMARK) == 0) return 0;
    return PlaceBookmark(name, P);
}

int EBuffer::RemoveBookmark(ExState &State) {
    char name[256] = "";
    
    if (State.GetStrParam(name, sizeof(name)) == 0)
        if (View->MView->Win->GetStr("Remove Bookmark", sizeof(name), name, HIST_BOOKMARK) == 0) return 0;
    return RemoveBookmark(name);
}

int EBuffer::GotoBookmark(ExState &State) {
    char name[256] = "";
    
    if (State.GetStrParam(name, sizeof(name)) == 0)
        if (View->MView->Win->GetStr("Goto Bookmark", sizeof(name), name, HIST_BOOKMARK) == 0) return 0;
    return GotoBookmark(name);
}
#endif

int EBuffer::InsertChar(ExState &State) {
    char Ch;
    int No;

    if (State.GetIntParam(&No) == 0) {
        TEvent E;
        E.What = evKeyDown;
        E.Key.Code = View->MView->Win->GetChar(0);
        if (!GetCharFromEvent(E, &Ch)) return 0;
        No = Ch;
    }
    if (No < 0 || No > 255) return 0;
    Ch = char(No);
    return InsertChar(Ch);
}

int EBuffer::TypeChar(ExState &State) {
    char Ch;
    int No;

    if (State.GetIntParam(&No) == 0) {
        TEvent E;
        E.What = evKeyDown;
        E.Key.Code = View->MView->Win->GetChar(0);
        if (!GetCharFromEvent(E, &Ch)) return 0;
        No = Ch;
    }
    if (No < 0 || No > 255) return 0;
    Ch = char(No);
    return TypeChar(Ch);
}

int EBuffer::InsertString(ExState &State) {
    char strbuf[1024] = "";
    
    if (State.GetStrParam(strbuf, sizeof(strbuf)) == 0) {
        if (View->MView->Win->GetStr("Insert String", sizeof(strbuf), strbuf, HIST_DEFAULT) == 0)
            return 0;
    }
    return InsertString(strbuf, strlen(strbuf));
}

extern int LastEventChar;

int EBuffer::SelfInsert(ExState &State) {
    if (LastEventChar != -1)
        return TypeChar(char(LastEventChar));
    return 0;
}

int EBuffer::FileReload(ExState &State) {
    if (Modified) {
        switch (View->MView->Win->Choice(GPC_ERROR, "File Modified",
                       2,
                       "&Reload",
                       "&Cancel",
                       "%s", FileName))
        {
        case 0:
            break;
        case 1:
        case -1:
        default:
            return 0;
        }
    }
//    GetNewNumber();
    return Reload();
}

int EBuffer::FileSaveAs(char *FName) {
    char Name[MAXPATH];
    
    if (ExpandPath(FName, Name) == -1) {
        View->MView->Win->Choice(GPC_ERROR, "Error", 1, "O&K", "Invalid path: %s.", FName);
        return 0;
    }
    if (FindFile(Name) == 0) {
	if (FileExists(Name)) {
            switch (View->MView->Win->Choice(GPC_ERROR, "File Exists",
                           2,
                           "&Overwrite",
                           "&Cancel",
                           "%s", Name))
            {
            case 0:
                break;
            case 1:
            case -1:
            default:
                return 0;
                
            }
	}
        free(FileName);
        FileName = strdup(Name);
        UpdateTitle();
        return Save();
    } else {
        View->MView->Win->Choice(GPC_ERROR, "Error", 1, "O&K", "Already editing '%s.'", Name);
        return 0;
    }
}

int EBuffer::FileSaveAs(ExState &State) {
    char FName[MAXPATH];
    
    strcpy(FName, FileName);
    if (State.GetStrParam(FName, sizeof(FName)) == 0)
        if (View->MView->Win->GetFile("Save As", sizeof(FName), FName, HIST_PATH, GF_SAVEAS) == 0)
            return 0;
    return FileSaveAs(FName);
}

int EBuffer::FileWriteTo(char *FName) {
    char Name[MAXPATH];
    
    if (ExpandPath(FName, Name) == -1) {
        View->MView->Win->Choice(GPC_ERROR, "Error", 1, "O&K", "Invalid path: %s.", FName);
        return 0;
    }
    if (FindFile(Name) == 0) {
	if (FileExists(Name)) {
            switch (View->MView->Win->Choice(GPC_ERROR, "File Exists",
                           2,
                           "&Overwrite",
                           "&Cancel",
                           "%s", Name))
            {
            case 0:
                break;
            case 1:
            case -1:
            default:
                return 0;
                
            }
	}
        return SaveTo(Name);
    } else {
        View->MView->Win->Choice(GPC_ERROR, "Error", 1, "O&K", "Already editing '%s.'", Name);
        return 0;
    }
}

int EBuffer::FileWriteTo(ExState &State) {
    char FName[MAXPATH];
    
    strcpy(FName, FileName);
    if (State.GetStrParam(FName, sizeof(FName)) == 0)
        if (View->MView->Win->GetFile("Write To", sizeof(FName), FName, HIST_PATH, GF_SAVEAS) == 0) return 0;
    return FileWriteTo(FName);
}

int EBuffer::BlockReadX(ExState &State, int blockMode) {
    char Name[MAXPATH];
    char FName[MAXPATH];

    if (JustDirectory(FileName, FName) == -1) return 0;
    SlashDir(FName);
    if (State.GetStrParam(FName, sizeof(FName)) == 0)
        if (View->MView->Win->GetFile("Read block", sizeof(FName), FName, HIST_PATH, GF_OPEN) == 0) return 0;

    if (ExpandPath(FName, Name) == -1) {
        View->MView->Win->Choice(GPC_ERROR, "Error", 1, "O&K", "Invalid path: %s.", FName);
        return 0;
    }
    return BlockReadFrom(FName, blockMode);
}

int EBuffer::BlockRead(ExState &State) {
    return BlockReadX(State, BlockMode);
}

int EBuffer::BlockReadStream(ExState &State) {
    return BlockReadX(State, bmStream);
}

int EBuffer::BlockReadLine(ExState &State) {
    return BlockReadX(State, bmLine);
}

int EBuffer::BlockReadColumn(ExState &State) {
    return BlockReadX(State, bmColumn);
}

int EBuffer::BlockWrite(ExState &State) {
    char Name[MAXPATH];
    char FName[MAXPATH];
    int Append = 0;

    if (JustDirectory(FileName, FName) == -1) return 0;
    SlashDir(FName);
    if (State.GetStrParam(FName, sizeof(FName)) == 0)
        if (View->MView->Win->GetFile("Write block", sizeof(FName), FName, HIST_PATH, GF_SAVEAS) == 0)
            return 0;
    
    if (ExpandPath(FName, Name) == -1) {
        View->MView->Win->Choice(GPC_ERROR, "Error", 1, "O&K", "Invalid path: %s.", FName);
        return 0;
    }
    if (FindFile(Name) == 0) {
        if (FileExists(Name)) {
            switch (View->MView->Win->Choice(GPC_ERROR, "File Exists",
                           3,
                           "&Overwrite",
                           "&Append",
                           "&Cancel",
                           "%s", Name))
            {
            case 0:
                break;
            case 1:
                Append = 1;
                break;
            case 2:
            case -1:
            default:
                return 0;
                
            }
        }
    } else {
        View->MView->Win->Choice(GPC_ERROR, "Error", 1, "O&K", "Already editing '%s.'", Name);
        return 0;
    }
    return BlockWriteTo(Name, Append);
}

int EBuffer::Find(ExState &State) {
    char find[MAXSEARCH+1] = "";
    char options[32] = "";
    
    if (State.GetStrParam(find, sizeof(find)) != 0) {
        if (State.GetStrParam(options, sizeof(options)) == 0)
            strcpy(options, BFS(this, BFS_DefFindOpt));

        LSearch.ok = 0;
        strcpy(LSearch.strSearch, find);
        LSearch.strReplace[0] = 0;
        LSearch.Options = 0;
        if (ParseSearchOptions(0, options, LSearch.Options) == 0) return 0;
        LSearch.ok = 1;
    } else if ((HaveGUIDialogs & GUIDLG_FIND) && GUIDialogs) {
        LSearch.ok = 0;
        LSearch.strSearch[0] = 0;
        LSearch.strReplace[0] = 0;
        LSearch.Options = 0;
        if (BFS(this, BFS_DefFindOpt))
            strcpy(options, BFS(this, BFS_DefFindOpt));
        if (ParseSearchOptions(0, options, LSearch.Options) == 0)
            LSearch.Options = 0;
        
        if (DLGGetFind(View->MView->Win, LSearch) == 0)
            return 0;
    } else {
        if (BFS(this, BFS_DefFindOpt))
            strcpy(options, BFS(this, BFS_DefFindOpt));
        if (View->MView->Win->GetStr("Find", sizeof(find), find, HIST_SEARCH) == 0) return 0;
        if (View->MView->Win->GetStr("Options (abcdgijrx)", sizeof(options), options, HIST_SEARCHOPT) == 0) return 0;
        
        LSearch.ok = 0;
        strcpy(LSearch.strSearch, find);
        LSearch.strReplace[0] = 0;
        LSearch.Options = 0;
        if (ParseSearchOptions(0, options, LSearch.Options) == 0) return 0;
        LSearch.ok = 1;
    }
    if (LSearch.ok == 0) return 0;
    LSearch.Options |= SEARCH_CENTER;
    if (Find(LSearch) == 0) return 0;
    return 1;
}

int EBuffer::FindReplace(ExState &State) {
    char find[MAXSEARCH+1] = "";
    char replace[MAXSEARCH+1] = "";
    char options[32] = "";
    
    if (State.GetStrParam(find, sizeof(find)) != 0) {
        if (State.GetStrParam(replace, sizeof(replace)) == 0)
            return 0;
        if (State.GetStrParam(options, sizeof(options)) == 0)
            return 0;

        LSearch.ok = 0;
        strcpy(LSearch.strSearch, find);
        strcpy(LSearch.strReplace, replace);
        LSearch.Options = 0;
        if (ParseSearchOptions(1, options, LSearch.Options) == 0) return 0;
        LSearch.Options |= SEARCH_REPLACE;
        LSearch.ok = 1;
    } else if ((HaveGUIDialogs & GUIDLG_FINDREPLACE) && GUIDialogs) {
        LSearch.ok = 0;
        LSearch.strSearch[0] = 0;
        LSearch.strReplace[0] = 0;
        LSearch.Options = 0;
        if (BFS(this, BFS_DefFindReplaceOpt))
            strcpy(options, BFS(this, BFS_DefFindReplaceOpt));
        if (ParseSearchOptions(1, options, LSearch.Options) == 0)
            LSearch.Options = 0;
        if (DLGGetFindReplace(View->MView->Win, LSearch) == 0)
            return 0;
    } else {
        if (BFS(this, BFS_DefFindReplaceOpt))
            strcpy(options, BFS(this, BFS_DefFindReplaceOpt));
        if (State.GetStrParam(find, sizeof(find)) == 0)
            if (View->MView->Win->GetStr("Find", sizeof(find), find, HIST_SEARCH) == 0) return 0;
        if (State.GetStrParam(replace, sizeof(replace)) == 0)
            if (View->MView->Win->GetStr("Replace", sizeof(replace), replace, HIST_SEARCH) == 0) return 0;
        if (State.GetStrParam(options, sizeof(options)) == 0)
            if (View->MView->Win->GetStr("Options (abcdgijrnx)", sizeof(options), options, HIST_SEARCHOPT) == 0) return 0;
        
        LSearch.ok = 0;
        strcpy(LSearch.strSearch, find);
        strcpy(LSearch.strReplace, replace);
        LSearch.Options = 0;
        if (ParseSearchOptions(1, options, LSearch.Options) == 0) return 0;
        LSearch.Options |= SEARCH_REPLACE;
        LSearch.ok = 1;
    }
    if (LSearch.ok == 0) return 0;
    LSearch.Options |= SEARCH_CENTER;
    if (Find(LSearch) == 0) return 0;
    return 1;
}

int EBuffer::FindRepeat(ExState &State) {
    if (LSearch.ok == 0) return Find(State);
    LSearch.Options |= SEARCH_NEXT;
    LSearch.Options &= ~SEARCH_GLOBAL;
    if (Find(LSearch) == 0) return 0;
    return 1;
}

int EBuffer::FindRepeatReverse(ExState &State) {
    int rc;
    
    if (LSearch.ok == 0) return Find(State);
    LSearch.Options |= SEARCH_NEXT;
    LSearch.Options &= ~SEARCH_GLOBAL;
    LSearch.Options ^= SEARCH_BACK;
    rc = Find(LSearch);
    LSearch.Options ^= SEARCH_BACK;
    return rc;
}

int EBuffer::FindRepeatOnce(ExState &State) {
    if (LSearch.ok == 0) return Find(State);
    LSearch.Options |= SEARCH_NEXT;
    LSearch.Options &= ~SEARCH_GLOBAL;
    LSearch.Options &= ~SEARCH_ALL;
    if (Find(LSearch) == 0) return 0;
    return 1;
}

int EBuffer::ChangeMode(ExState &State) {
    char Mode[32] = "";
    int rc;

    if (State.GetStrParam(Mode, sizeof(Mode)) == 0)
        if (View->MView->Win->GetStr("Mode", sizeof(Mode), Mode, HIST_SETUP) == 0) return 0;

    rc = ChangeMode(Mode);
    FullRedraw();
    return rc;
}

int EBuffer::ChangeKeys(ExState &State) {
    int rc;
    char Mode[32] = "";

    if (State.GetStrParam(Mode, sizeof(Mode)) == 0)
        if (View->MView->Win->GetStr("Mode", sizeof(Mode), Mode, HIST_SETUP) == 0) return 0;

    rc = ChangeKeys(Mode);
    FullRedraw();
    return rc;
}

int EBuffer::ChangeFlags(ExState &State) {
    int rc;
    char Mode[32] = "";

    if (State.GetStrParam(Mode, sizeof(Mode)) == 0)
        if (View->MView->Win->GetStr("Mode", sizeof(Mode), Mode, HIST_SETUP) == 0) return 0;

    rc = ChangeFlags(Mode);
    FullRedraw();
    return rc;
}

int EBuffer::ChangeTabSize(ExState &State) {
    int No;
    
    if (State.GetIntParam(&No) == 0) {
        char Num[10];
        
        sprintf(Num, "%d", BFI(this, BFI_TabSize));
        if (View->MView->Win->GetStr("TabSize", sizeof(Num), Num, HIST_SETUP) == 0) return 0;
        No = atol(Num);
    }
    if (No < 1) return 0;
    if (No > 32) return 0;
    BFI(this, BFI_TabSize) = No;
    FullRedraw();
    return 1;
}

int EBuffer::ChangeRightMargin(ExState &State) {
    char Num[10];
    int No;
    
    if (State.GetIntParam(&No) == 0) {
        sprintf(Num, "%d", BFI(this, BFI_RightMargin) + 1);
        if (View->MView->Win->GetStr("RightMargin", sizeof(Num), Num, HIST_SETUP) == 0) return 0;
        No = atol(Num) - 1;
    }
    if (No <= 1) return 0;
    BFI(this, BFI_RightMargin) = No;
    Msg(INFO, "RightMargin set to %d.", No + 1);
    return 1;
}

int EBuffer::ChangeLeftMargin(ExState &State) {
    char Num[10];
    int No;
    
    if (State.GetIntParam(&No) == 0) {
        sprintf(Num, "%d", BFI(this, BFI_LeftMargin) + 1);
        if (View->MView->Win->GetStr("LeftMargin", sizeof(Num), Num, HIST_SETUP) == 0) return 0;
        No = atol(Num) - 1;
    }
    if (No < 0) return 0;
    BFI(this, BFI_LeftMargin) = No;
    Msg(INFO, "LeftMargin set to %d.", No + 1);
    return 1;
}


int EBuffer::CanQuit() {
    if (Modified)
        return 0;
    else
        return 1;
}
    
int EBuffer::ConfQuit(GxView *V) {
    if (Modified) {
        switch (V->Choice(GPC_ERROR,
                          "File Modified",
                          4,
                          "&Save",
                          "&As",
                          "&Discard",
                          "&Cancel",
                          "%s", FileName))
        {
        case 0: /* Save */
            if (Save() == 0) return 0;
            break;
        case 1: /* As */
            {
                char FName[MAXPATH];
                strcpy(FName, FileName);
                if (V->GetFile("Save As", sizeof(FName), FName, HIST_PATH, GF_SAVEAS) == 0) return 0;
                if (FileSaveAs(FName) == 0) return 0;
            }
            break;
        case 2: /* Discard */
            break;
        case 3: /* Cancel */
        case -1:
        default:
            return 0;
        }
    }
    return 1;
}

void EBuffer::GetName(char *AName, int MaxLen) { 
    strncpy(AName, FileName, MaxLen);
    AName[MaxLen - 1] = 0;
}

void EBuffer::GetPath(char *APath, int MaxLen) {
    JustDirectory(FileName, APath);
}

void EBuffer::GetInfo(char *AInfo, int MaxLen) {
    sprintf(AInfo, 
            "%2d %04d:%03d%c%-150s ",
            ModelNo, 
            1 + CP.Row, 1 + CP.Col, 
            Modified ? '*': ' ', 
            FileName);
}

void EBuffer::GetTitle(char *ATitle, int MaxLen, char *ASTitle, int SMaxLen) {
    char *p;
    
    strncpy(ATitle, FileName, MaxLen - 1);
    ATitle[MaxLen - 1] = 0;
    p = strrchr(FileName, SLASH);
    if (p) {
        strncpy(ASTitle, p + 1, SMaxLen - 1);
        ASTitle[SMaxLen - 1] = 0;
    } else {
        strncpy(ASTitle, FileName, SMaxLen - 1);
        ASTitle[SMaxLen - 1] = 0;
    }
}

#ifdef CONFIG_I_ASCII
int EBuffer::ASCIITable(ExState &State) {
    int rc;
    
    rc = View->MView->Win->PickASCII();
    if (rc != -1) 
        return InsertChar(char(rc));
    
    return 0;
}
#endif

int EBuffer::ScrollLeft(ExState &State) {
    int Cols;
    
    if (State.GetIntParam(&Cols) == 0)
        Cols = 8;
    return ScrollLeft(Cols);
}

int EBuffer::ScrollRight(ExState &State) {
    int Cols;
    
    if (State.GetIntParam(&Cols) == 0)
        Cols = 8;
    return ScrollRight(Cols);
}

int EBuffer::ScrollDown(ExState &State) {
    int Rows;
    
    if (State.GetIntParam(&Rows) == 0)
        Rows = 1;
    return ScrollDown(Rows);
}

int EBuffer::ScrollUp(ExState &State) {
    int Rows;
    
    if (State.GetIntParam(&Rows) == 0)
        Rows = 1;
    return ScrollUp(Rows);
}

#ifdef CONFIG_TAGS
int EBuffer::FindTag(ExState &State) {
    char Tag[MAXSEARCH] = "";
    
    if (State.GetStrParam(Tag, sizeof(Tag)) == 0)
        if (View->MView->Win->GetStr("Find tag", sizeof(Tag), Tag, HIST_SEARCH) == 0) return 0;
    
    if (TagFind(this, View, Tag) == 0) {
        Msg(INFO, "Tag '%s' not found.", Tag);
        return 0;
    }
    return 1;

}
#endif

// these two will probably be replaced in the future
int EBuffer::InsertDate(ExState& state) {
    char buf[50], *p;
    time_t t;
    
    time(&t);
    p = ctime(&t);
    //** 012345678901234567890123
    //** Wed Jan 02 02:23:54 1991
    sprintf(buf, "%.10s %.4s", p, p + 20);
    return InsertString(buf, strlen(buf));
}


int EBuffer::InsertUid() {
    char *p = getenv("USER");
    if (p == 0) p = getenv("NAME");
    if (p == 0) p = getenv("ID");
    if (p == 0) {
        Msg(INFO, "User ID not set ($USER).");
        return 0;
    }
    return InsertString(p, strlen(p));
}
