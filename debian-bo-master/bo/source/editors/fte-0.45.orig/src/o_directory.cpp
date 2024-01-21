/*    o_directory.cpp
 *
 *    Copyright (c) 1994-1996, Marko Macek
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 */

#include "fte.h"

#ifdef CONFIG_OBJ_DIRECTORY
EDirectory::EDirectory(EModel **ARoot, char *aPath): EList(ARoot, aPath) {
    char XPath[MAXPATH];
    
    Files = 0;
    FCount = 0;
    SearchLen = 0;
    ExpandPath(aPath, XPath);
    Slash(XPath, 1);
    Path = strdup(XPath);
    RescanList();
}

EDirectory::~EDirectory() {
    if (Files) {
        for (int i = 0; i < FCount; i++)
            delete Files[i];
        free(Files);
    }
    free(Path);
}

EEventMap *EDirectory::GetEventMap() {
    return FindEventMap("DIRECTORY");
}

void EDirectory::DrawLine(PCell B, int Line, int Col, ChColor color, int Width) {
    char s[1024];

    MoveCh(B, ' ', color, Width);
    if (Files && Line >= 0 && Line < FCount) {
        int Year, Mon, Day, Hour, Min, Sec;
        struct tm *t;
        time_t tim;
        
        tim = Files[Line]->MTime();
        t = localtime(&tim);
        
        if (t) {
            Year = t->tm_year + 1900;
            Mon = t->tm_mon + 1;
            Day = t->tm_mday;
            Hour = t->tm_hour;
            Min = t->tm_min;
            Sec = t->tm_sec;
        } else {
            Year = Mon = Day = Hour = Min = Sec = 0;
        }
        
        sprintf(s, " %04d/%02d/%02d %02d:%02d:%02d %8ld %.500s%c",
                Year, Mon, Day, Hour, Min, Sec,
                Files[Line]->Size(),
                Files[Line]->Name(),
                (Files[Line]->Type() == fiDIRECTORY)? '\\' : ' ');
        if (Col < strlen(s))
            MoveStr(B, 0, Width, s + Col, color, Width);
    }
}

int FileNameCmp(const void *a, const void *b) {
    FileInfo *A = *(FileInfo **)a;
    FileInfo *B = *(FileInfo **)b;
    
    if (!(A->Type() == fiDIRECTORY) && (B->Type() == fiDIRECTORY))
        return 1;
    
    if ((A->Type() == fiDIRECTORY) && !(B->Type() == fiDIRECTORY))
        return -1;
    
    return filecmp(A->Name(), B->Name());
}

void EDirectory::RescanList() {
    char Dir[256];
    char Name[256];
    int DirCount = 0;
    int SizeCount = 0;
    FileFind *ff;
    FileInfo *fi;
    int rc;

    if (Files)
        FreeList();

    Count = 0;
    FCount = 0;
    if (JustDirectory(Path, Dir) != 0) return;
    JustFileName(Path, Name);

    ff = new FileFind(Dir, "*", ffDIRECTORY | ffHIDDEN);
    if (ff == 0)
        return ;

    rc = ff->FindFirst(&fi);
    while (rc == 0) {
        assert(fi != 0);
        if (strcmp(fi->Name(), ".") != 0) {
            Files = (FileInfo **)realloc((void *)Files, ((FCount | 255) + 1) * sizeof(FileInfo *));
            if (Files == 0) return;
            Files[FCount] = fi;

            SizeCount += Files[FCount]->Size();
            if (fi->Type() == fiDIRECTORY && (strcmp(fi->Name(), "..") != 0))
                DirCount++;
            Count++;
            FCount++;
        } else
            delete fi;
        rc = ff->FindNext(&fi);
    }
    delete ff;
    
    {
        char CTitle[256];
        
        sprintf(CTitle, "%d files%c%d dirs%c%d bytes%c%-200.200s",
                FCount, ConGetDrawChar(DCH_V),
                DirCount, ConGetDrawChar(DCH_V),
                SizeCount, ConGetDrawChar(DCH_V),
                Dir);
        SetTitle(CTitle);
    }
    qsort(Files, FCount, sizeof(FileInfo *), FileNameCmp);
}

void EDirectory::FreeList() {
    if (Files) {
        for (int i = 0; i < FCount; i++)
            delete Files[i];
        free(Files);
    }
    Files = 0;
}

int EDirectory::ExecCommand(int Command, ExState &State) {
    switch (Command) {
    case ExActivateInOtherWindow:
        SearchLen = 0;
        Msg(INFO, "");
        if (Files && Row >= 0 && Row < FCount) {
            if (Files[Row]->Type() == fiDIRECTORY) {
            } else {
                return FmLoad(Files[Row]->Name(), View->Next);
            }
        }
        return ErFAIL;

    case ExRescan:
        if (RescanDir() == 0)
            return ErFAIL;
        return ErOK;
        
    case ExDirGoUp:
        SearchLen = 0;
        Msg(INFO, "");
        FmChDir(SDOT SDOT);
        return ErOK;
        
    case ExDirGoDown:
        SearchLen = 0;
        Msg(INFO, "");
        if (Files && Row >= 0 && Row < FCount) {
            if (Files[Row]->Type() == fiDIRECTORY) {
                FmChDir(Files[Row]->Name());
                return ErOK;
            }
        }
        return ErFAIL;
        
    case ExDirGoto:
        SearchLen = 0;
        Msg(INFO, "");
        return ChangeDir(State);
    
    case ExDirGoRoot:
        SearchLen = 0;
        Msg(INFO, "");
        FmChDir(SSLASH);
        return ErOK;
    }
    return EList::ExecCommand(Command, State);
}

int EDirectory::Activate(int No) {
    SearchLen = 0;
    Msg(INFO, "");
    if (Files && No >= 0 && No < FCount) {
        if (Files[No]->Type() == fiDIRECTORY) {
            FmChDir(Files[No]->Name());
            return 0;
        } else {
            return FmLoad(Files[No]->Name(), View);
        }
    }
    return 1;
}

void EDirectory::HandleEvent(TEvent &Event) {
    EModel::HandleEvent(Event);
    switch (Event.What) {
    case evKeyDown:
        switch (kbCode(Event.Key.Code)) {
        case kbBackSp:
            if (SearchLen > 0) {
                SearchName[--SearchLen] = 0;
                Row = SearchPos[SearchLen];
                Msg(INFO, "Search: [%s]", SearchName);
            } else
                Msg(INFO, "");
            break;
        case kbEsc:
            Msg(INFO, "");
            break;
        default:
            if (isAscii(Event.Key.Code) && (SearchLen < MAXISEARCH)) {
                char Ch = (char) Event.Key.Code;
                int Found;
                
                SearchPos[SearchLen] = Row;
                SearchName[SearchLen] = Ch;
                SearchName[++SearchLen] = 0;
                Found = 0;
                for (int i = Row; i < FCount; i++) {
                    if (strnicmp(SearchName, Files[i]->Name(), SearchLen) == 0) {
                        Row = i;
                        Found = 1;
                        break;
                    }
                }
                if (Found == 0)
                    SearchName[--SearchLen] = 0;
                Msg(INFO, "Search: [%s]", SearchName);
            }
            break;
        }
    }
}

int EDirectory::RescanDir() {
    char CName[256] = "";

    if (Row >= 0 && Row < FCount)
        strcpy(CName, Files[Row]->Name());
    Row = 0;
    RescanList();
    if (CName[0] != 0) {
        for (int i = 0; i < FCount; i++) {
            if (filecmp(Files[i]->Name(), CName) == 0)
            {
                Row = i;
                break;
            }
        }
    }
    return 1;
}

int EDirectory::FmChDir(char *Name) {
    char Dir[256];
    char CName[256] = "";
    
    if (strcmp(Name, SSLASH) == 0) {
        JustRoot(Path, Dir);
    } else if (strcmp(Name, SDOT SDOT) == 0) {
        Slash(Path, 0);
        JustFileName(Path, CName);
        JustDirectory(Path, Dir);
    } else {
        JustDirectory(Path, Dir);
        Slash(Dir, 1);
        strcat(Dir, Name);
    }
    Slash(Dir, 1);
    free(Path);
    Path = strdup(Dir);
    Row = 0;
    RescanList();
    if (CName[0] != 0) {
        for (int i = 0; i < FCount; i++) {
            if (filecmp(Files[i]->Name(), CName) == 0)
            {
                Row = i;
                break;
            }
        }
    }
    UpdateTitle();
    return 1;
}

int EDirectory::FmLoad(char *Name, EView *XView) {
    char FilePath[256];
    
    JustDirectory(Path, FilePath);
    Slash(FilePath, 1);
    strcat(FilePath, Name);
    return FileLoad(FilePath, NULL, XView);
}

void EDirectory::GetName(char *AName, int MaxLen) {
    strncpy(AName, Path, MaxLen);
    AName[MaxLen - 1] = 0;
    Slash(AName, 0);
}

void EDirectory::GetPath(char *APath, int MaxLen) {
    strncpy(APath, Path, MaxLen);
    APath[MaxLen - 1] = 0;
    Slash(APath, 0);
}

void EDirectory::GetInfo(char *AInfo, int MaxLen) {
    sprintf(AInfo, 
            "%2d %04d/%03d %-150s",
            ModelNo,
            Row + 1, FCount,
            Path);
}

void EDirectory::GetTitle(char *ATitle, int MaxLen, char *ASTitle, int SMaxLen) {
    
    strncpy(ATitle, Path, MaxLen - 1);
    ATitle[MaxLen - 1] = 0;
    
    {
        char P[MAXPATH];
        strcpy(P, Path);
        Slash(P, 0);
        
        JustDirectory(P, ASTitle);
        Slash(ASTitle, 1);
    }
}

int EDirectory::ChangeDir(ExState &State) {
    char Dir[MAXPATH];
    char Dir2[MAXPATH];
    
    if (State.GetStrParam(Dir, sizeof(Dir)) == 0) {
        strcpy(Dir, Path);
        if (View->MView->Win->GetStr("Set directory", sizeof(Dir), Dir, HIST_PATH) == 0)
            return 0;
    }
    if (ExpandPath(Dir, Dir2) == -1)
        return 0;
    if (Path)
        free(Path);
    Path = strdup(Dir2);
    Row = -1;
    UpdateTitle();
    return RescanDir();
}

int EDirectory::GetContext() { return CONTEXT_DIRECTORY; }
char *EDirectory::FormatLine(int Line) { return 0; };
int EDirectory::CanActivate(int Line) { return 1; }
#endif
