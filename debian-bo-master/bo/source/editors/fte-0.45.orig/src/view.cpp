/*    view.cpp
 *
 *    Copyright (c) 1994-1996, Marko Macek
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 */

#include "fte.h"

EView *ActiveView = 0;

extern BufferView *BufferList;

EView::EView(EModel *AModel) {
    if (ActiveView) {
        Prev = ActiveView;
        Next = ActiveView->Next;
        Prev->Next = this;
        Next->Prev = this;
    } else
        Prev = Next = this;
    ActiveView = this;
    Model = AModel;
    NextView = 0;
    Port = 0;
    MView = 0;
    CurMsg = 0;
    if (Model)
        Model->CreateViewPort(this);
}

EView::~EView() {
    if (Next != this) {
        Prev->Next = Next;
        Next->Prev = Prev;
        if (ActiveView == this)
            ActiveView = Next;
    } else
        ActiveView = 0;
    if (MView)
        MView->View = 0;
    if (Model)
        Model->RemoveView(this);
    if (Port)
        delete Port;
}

int EView::CanQuit() {
    if (Model)
        return Model->CanQuit();
    else
        return 1;
}

void EView::FocusChange(int GetFocus) {
    if (GetFocus) {
        if (Model->View)
            Model->View->Port->GetPos();
        Model->CreateViewPort(this);
    } else {
        if (Model) {
            Model->RemoveView(this);
            delete Port;
            if (Model->View)
                Model->View->Port->StorePos();
        }
    }
}

void EView::Resize(int Width, int Height) {
    if (Port)
        Port->Resize(Width, Height);
}

void EView::SetModel(EModel *AModel) {
    Model = AModel;
    ActiveModel = Model;
}

void EView::SelectModel(EModel *AModel) {
    if (Model != AModel) {
        if (Model)
            FocusChange(0);
        SetModel(AModel);
        if (Model)
            FocusChange(1);
    }
}

void EView::SwitchToModel(EModel *AModel) {
    if (Model != AModel) {
        if (Model)
            FocusChange(0);
        
        AModel->Prev->Next = AModel->Next;
        AModel->Next->Prev = AModel->Prev;

        if (Model) {
            AModel->Next = Model;
            AModel->Prev = Model->Prev;
            AModel->Prev->Next = AModel;
            Model->Prev = AModel;
        } else {
            AModel->Next = AModel->Prev = AModel;
        }

        SetModel(AModel);
        
        if (Model)
            FocusChange(1);
    }
}

void EView::Activate(int GotFocus) {
    if (Model && Model->View != this && Port) {
        Model->SelectView(this);
        if (GotFocus) {
            Port->StorePos();
        } else {
            Port->GetPos();
        }
        Port->RepaintView();
        if (GotFocus)
            ActiveView = this;
    }
}

int EView::GetContext() {
    return Model ? Model->GetContext() : 0;
}

EEventMap *EView::GetEventMap() { 
    return Model ? Model->GetEventMap() : 0;
}

int EView::BeginMacro() {
    return Model ? Model->BeginMacro() : 0;
}

int EView::ExecCommand(int Command, ExState &State) {
    switch (Command) {
    case ExSwitchTo:            return SwitchTo(State);
    case ExFilePrev:            return FilePrev();
    case ExFileNext:            return FileNext();
    case ExFileLast:            return FileLast();
    case ExFileOpen:            return FileOpen(State);
    case ExFileOpenInMode:      return FileOpenInMode(State);
    case ExFileSaveAll:         return FileSaveAll();
        
    case ExListRoutines:        
#ifdef CONFIG_OBJ_ROUTINE
        return ViewRoutines(State);
#else
        return ErFAIL;
#endif
        
    case ExDirOpen:             
#ifdef CONFIG_OBJ_DIRECTORY
        return DirOpen(State);
#else
        return ErFAIL;
#endif
        
#ifdef CONFIG_OBJ_MESSAGES
    case ExViewMessages:        return ViewMessages(State);
    case ExCompile:             return Compile(State);
    case ExCompilePrevError:    return CompilePrevError(State);
    case ExCompileNextError:    return CompileNextError(State);
#else
    case ExViewMessages:        return ErFAIL;
    case ExCompile:             return ErFAIL;
    case ExCompilePrevError:    return ErFAIL;
    case ExCompileNextError:    return ErFAIL;
#endif
        
    case ExViewBuffers:         return ViewBuffers(State);
        
    case ExShowKey:             return ShowKey(State);
    case ExToggleSysClipboard:  return ToggleSysClipboard(State);
    case ExSetPrintDevice:      return SetPrintDevice(State);
    case ExShowVersion:         return ShowVersion();
    case ExViewModeMap:         return ViewModeMap(State);
    case ExClearMessages:       return ClearMessages();

#ifdef CONFIG_TAGS
    case ExTagNext:             return TagNext(this);
    case ExTagPrev:             return TagPrev(this);
    case ExTagPop:             return TagPop(this);
    case ExTagClear:            TagClear(); return 1;
    case ExTagLoad:             return TagLoad(State);
#endif
    }
    return Model ? Model->ExecCommand(Command, State) : 0;
}

void EView::HandleEvent(TEvent &Event) {
    if (Model)
        Model->HandleEvent(Event);
    if (Port)
        Port->HandleEvent(Event);
    if (Event.What == evCommand) {
        switch (Event.Msg.Command) {
        case cmDroppedFile:
            {
                char *file = (char *)Event.Msg.Param2;

#ifdef CONFIG_OBJ_DIRECTORY
                if (IsDirectory(file))
                    OpenDir(file);
#endif
                MultiFileLoad(file, NULL, this);
            }
            break;
        }
    }
}

void EView::UpdateView() {
    if (Port)
        Port->UpdateView();
}

void EView::RepaintView() {
    if (Port)
        Port->RepaintView();
}

void EView::UpdateStatus() {
    if (Port)
        Port->UpdateStatus();
}

void EView::RepaintStatus() {
    if (Port)
        Port->RepaintStatus();
}

void EView::DeleteModel(EModel *M) {
    EView *V;
    EModel *M1;
    char s[256];
    
    if (M == 0)
        return;
    
    M->GetName(s, sizeof(s));
    Msg(INFO, "Closing %s.", s);
    
    V = ActiveView = this;
    while (V) {
        M1 = V->Model;
        if (M1 == M) {
            if (M->Next != M)
                V->SelectModel(M->Next);
            else
                V->SelectModel(0);
        }
        V = V->Next;
        if (V == ActiveView)
            break;
    }
    delete M;
    SetMsg(0);
    return;
}

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

int EView::FilePrev() {
    if (Model) {
        SelectModel(Model->Prev);
        return 1;
    }
    return 0;
}

int EView::FileNext() {
    if (Model) {
        SelectModel(Model->Next);
        return 1;
    }
    return 0;
}

int EView::FileLast() {
    if (Model) {
        SwitchToModel(Model->Next);
        return 1;
    }
    return 0;
}

int EView::SwitchTo(ExState &State) {
    EModel *M;
    int No;
    
    if (State.GetIntParam(&No) == 0) {
        char str[10] = "";
        
        if (MView->Win->GetStr("Obj.Number", sizeof(str), (char *)&str, 0) == 0) return 0;
        No = atoi(str);
    }
    M = Model;
    while (M) {
        if (M->ModelNo == No) {
            SwitchToModel(M);
            return 1;
        }
        M = M->Next;
        if (M == Model)
            return 0;
    }
    return 0;
}


int EView::FileSaveAll() {
    EModel *M = Model;
    while (M) {
        if (M->GetContext() == CONTEXT_FILE) {
            EBuffer *B = (EBuffer *)M;
            if (B->Modified) {
                if (B->Save() == 0) return 0;
            }
        }
        M = M->Next;
        if (M == Model) break;
    }
    return 1;
}

int EView::FileOpen(ExState &State) {
    char FName[MAXPATH];
    
    if (State.GetStrParam(FName, sizeof(FName)) == 0) {
        if (GetDefaultDirectory(Model, FName, sizeof(FName)) == 0)
            return 0;
        if (MView->Win->GetFile("Open file", sizeof(FName), FName, HIST_PATH, GF_OPEN) == 0) return 0;
    }
#ifdef CONFIG_OBJ_DIRECTORY
    if (IsDirectory(FName))
        return OpenDir(FName);
#endif
    return MultiFileLoad(FName, NULL, this);
}

int EView::FileOpenInMode(ExState &State) {
    char Mode[32] = "";
    char FName[MAXPATH];
    
    if (State.GetStrParam(Mode, sizeof(Mode)) == 0)
        if (MView->Win->GetStr("Mode", sizeof(Mode), Mode, HIST_SETUP) != 1) return 0;
    
    if (FindMode(Mode) == 0) {
        MView->Win->Choice(GPC_ERROR, "Error", 1, "O&K", "Invalid mode '%s'", Mode);
        return 0;
    }
    
    if (GetDefaultDirectory(Model, FName, sizeof(FName)) == 0)
        return 0;
    if (State.GetStrParam(FName, sizeof(FName)) == 0)
        if (MView->Win->GetFile("Open file", sizeof(FName), FName, HIST_PATH, GF_OPEN) == 0) return 0;
#ifdef CONFIG_OBJ_DIRECTORY
    {
        int l = strlen(FName);
        
        if (l > 1 && ISSLASH(FName[l - 1]))
            return OpenDir(FName);
    }
#endif
    return MultiFileLoad(FName, Mode, this);
}

int EView::SetPrintDevice(ExState &State) {
    char Dev[MAXPATH];
    
    strcpy(Dev, PrintDevice);
    if (State.GetStrParam(Dev, sizeof(Dev)) == 0)
        if (MView->Win->GetStr("Print to", sizeof(Dev), Dev, HIST_SETUP) == 0) return 0;
    
    strcpy(PrintDevice, Dev);
    return 1;
}

int EView::ToggleSysClipboard(ExState &State) {
    SystemClipboard = SystemClipboard ? 0 : 1;
    Msg(INFO, "SysClipboard is now %s.", SystemClipboard ? "ON" : "OFF");
    return 1;
}

int EView::ShowKey(ExState &State) {
    char buf[100];
    KeySel ks;
    
    ks.Mask = 0;
    ks.Key = MView->Win->GetChar(0);
    
    GetKeyName(buf, ks);
    Msg(INFO, "Key: '%s' - '%8X'", buf, ks.Key);
    return 1;
}

void EView::Msg(int level, char *s, ...) {
    va_list ap;
    
    va_start(ap, s);
    vsprintf(msgbuftmp, s, ap);
    va_end(ap);
    
    if (level != BUSY)
        SetMsg(msgbuftmp);
}

void EView::SetMsg(char *Msg) {
    if (CurMsg)
        free(CurMsg);
    CurMsg = 0;
    if (Msg && strlen(Msg))
        CurMsg = strdup(Msg);
    if (CurMsg && Msg && MView) {
        TDrawBuffer B;
        char SColor;
        int Cols, Rows;
        
        MView->ConQuerySize(&Cols, &Rows);
        
        if (MView->IsActive())
            SColor = hcStatus_Active;
        else
            SColor = hcStatus_Normal;
        
        MoveChar(B, 0, Cols, ' ', SColor, Cols);
        MoveStr(B, 0, Cols, CurMsg, SColor, Cols);
        if (MView->Win->GetStatusContext() == MView)
            MView->ConPutBox(0, Rows - 1, Cols, 1, B);
        //printf("%s\n", Msg);
    }
}

int EView::ViewBuffers(ExState &State) {
    if (BufferList == 0) {
        BufferList = new BufferView(&ActiveModel);
        SwitchToModel(BufferList);
    } else {
        BufferList->UpdateList();
        BufferList->Row = 1;
        SwitchToModel(BufferList);
        return 1;
    }
    return 0;
}

#ifdef CONFIG_OBJ_ROUTINE
int EView::ViewRoutines(ExState &State) {
    //int rc = 1;
    //RoutineView *routines;
    EModel *M;
    EBuffer *Buffer;
    
    M = Model;
    if (M->GetContext() != CONTEXT_FILE)
        return 0;
    Buffer = (EBuffer *)M;
    
    if (Buffer->Routines == 0) {
        if (BFS(Buffer, BFS_RoutineRegexp) == 0) {
            MView->Win->Choice(GPC_ERROR, "Error", 1, "O&K", "No routine regexp.");
            return 0;
        }
        Buffer->Routines = new RoutineView(&ActiveModel, Buffer);
        if (Buffer->Routines == 0)
            return 0;
    } else {
        Buffer->Routines->UpdateList();
    }
    SwitchToModel(Buffer->Routines);
    return 1;
}
#endif

#ifdef CONFIG_OBJ_DIRECTORY
int EView::DirOpen(ExState &State) {
    char Path[MAXPATH];
    
    if (State.GetStrParam(Path, sizeof(Path)) == 0)
        if (GetDefaultDirectory(Model, Path, sizeof(Path)) == 0)
            return 0;
    return OpenDir(Path);
}

int EView::OpenDir(char *Path) {
    char XPath[MAXPATH];
    EDirectory *dir = 0;
    
    if (ExpandPath(Path, XPath) == -1)
        return 0;
    {
        EModel *x = Model;
        while (x) {
            if (x->GetContext() == CONTEXT_DIRECTORY) {
                if (filecmp(((EDirectory *)x)->Path, XPath) == 0)
                {
                    dir = (EDirectory *)x;
                    break;
                }
            }
            x = x->Next;
            if (x == Model)
                break;
        }
    }
    if (dir == 0) 
        dir = new EDirectory(&ActiveModel, XPath);
    SelectModel(dir);
    return 1;
}
#endif

#ifdef CONFIG_OBJ_MESSAGES
int EView::Compile(ExState &State) {
    static char Cmd[256] = "";
    char Command[256] = "";
    char Dir[MAXPATH] = "";
    EMessages *msgs;
    //int mode;
    
    if (CompilerMsgs != 0 && CompilerMsgs->Running) {
        Msg(INFO, "Already running...");
        return 0;
    } 
    
    if (State.GetStrParam(Command, sizeof(Command)) == 0) {
        if (Cmd[0] == 0)
            strcpy(Cmd, CompileCommand);
        
        if (MView->Win->GetStr("Compile", sizeof(Cmd), Cmd, HIST_COMPILE) == 0) return 0;
        
        strcpy(Command, Cmd);
    } else {
        if (MView->Win->GetStr("Compile", sizeof(Command), Command, HIST_COMPILE) == 0) return 0;
    }
    
    if (CompilerMsgs != 0) {
        strcpy(Dir, CompilerMsgs->Directory);
        CompilerMsgs->RunPipe(Dir, Command);
        msgs = CompilerMsgs;
    } else {
        if (GetDefaultDirectory(Model, Dir, sizeof(Dir)) == 0)
            return 0;
        
        msgs = new EMessages(&ActiveModel, Dir, Command);
    }
    SwitchToModel(msgs);
    return 1;
}

int EView::ViewMessages(ExState &State) {
    if (CompilerMsgs != 0) {
        SwitchToModel(CompilerMsgs);
        return 1;
    }
    return 0;
}

int EView::CompilePrevError(ExState &State) {
    if (CompilerMsgs != 0)
        return CompilerMsgs->CompilePrevError(this);
    return 0;
}

int EView::CompileNextError(ExState &State) {
    if (CompilerMsgs != 0)
        return CompilerMsgs->CompileNextError(this);
    return 0;
}
#endif

int EView::ShowVersion() {
    MView->Win->Choice(0, "About", 1, "O&K", PROGRAM " " VERSION " " COPYRIGHT);
    return 1;
}

int EView::ViewModeMap(ExState &State) {
    if (TheEventMapView != 0)
        TheEventMapView->ViewMap(GetEventMap());
    else
        new EventMapView(&ActiveModel, GetEventMap());
    if (TheEventMapView != 0)
        SwitchToModel(TheEventMapView);
    else
        return 0;
    return 1;
}

int EView::ClearMessages() {
    if (CompilerMsgs != 0 && CompilerMsgs->Running) {
        Msg(INFO, "Running...");
        return 0;
    } 
    if (CompilerMsgs != 0) {
        CompilerMsgs->FreeErrors();
        CompilerMsgs->UpdateList();
    }
    return 1;
}

#ifdef CONFIG_TAGS
int EView::TagLoad(ExState &State) {
    char Tag[MAXPATH];
    char FullTag[MAXPATH];
    
    if (ExpandPath("tags", Tag) == -1)
        return 0;
    if (State.GetStrParam(Tag, sizeof(Tag)) == 0)
        if (MView->Win->GetFile("Load tags", sizeof(Tag), Tag, HIST_TAGFILES, GF_OPEN) == 0) return 0;

    if (ExpandPath(Tag, FullTag) == -1)
        return 0;

    if (!FileExists(FullTag)) {
        Msg(INFO, "Tag file '%s' not found.", FullTag);
    }
    
    return ::TagLoad(FullTag);
}
#endif
