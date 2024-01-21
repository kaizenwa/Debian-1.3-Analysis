/*    e_file.cpp
 *
 *    Copyright (c) 1994-1996, Marko Macek
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 */

#include "fte.h"

EBuffer *FindFile(char *FileName) {
    EModel *M;
    EBuffer *B;
    
    M = ActiveModel;
    while (M) {
        if (M->GetContext() == CONTEXT_FILE) {
            B = (EBuffer *)M;
            if (filecmp(B->FileName, FileName) == 0) { return B; }
        }
        M = M->Next;
        if (M == ActiveModel) break;
    }
    return 0;
}

#if 0
static void SwitchModel(EModel *AModel) {
    if (AModel != AcgiveModel && MM && AModel) {
        AModel->Prev->Next = AModel->Next;
        AModel->Next->Prev = AModel->Prev;

        AModel->Next = MM;
        AModel->Prev = MM->Prev;
        AModel->Prev->Next = AModel;
        MM->Prev = AModel;
        MM = AModel;
    }
}
#endif

int FileLoad(char *FileName, char *Mode, EView *View) {
    char Name[MAXPATH];
    EBuffer *B;

    assert(View != 0);
    
    if (ExpandPath(FileName, Name) == -1) {
        View->MView->Win->Choice(GPC_ERROR, "Error", 1, "O&K", "Invalid path: %s.", FileName);
        return 0;
    }
    B = FindFile(Name);
    if (B) {
        if (Mode != 0)
            B->SetFileName(Name, Mode);
        View->SwitchToModel(B);
        return 1;
    }
    B = new EBuffer(&ActiveModel, Name);
    B->SetFileName(Name, Mode);

    View->SwitchToModel(B);
    return 1;
}

int MultiFileLoad(char *FileName, char *Mode, EView *View) {
    char fX[MAXPATH];
    int count = 0;
    char FPath[MAXPATH];
    char FName[MAXPATH];
    FileFind *ff;
    FileInfo *fi;
    int rc;

    assert(View != 0);

    JustDirectory(FileName, fX);
    if (fX[0] == 0) strcpy(fX, ".");
    JustFileName(FileName, FName);
    if (ExpandPath(fX, FPath) == -1) return 0;
    Slash(FPath, 1);

    ff = new FileFind(FPath, FName, ffHIDDEN | ffFULLPATH);
    if (ff == 0)
        return 0;
    rc = ff->FindFirst(&fi);
    while (rc == 0) {
        count++;
        if (FileLoad(fi->Name(), Mode, View) == 0) {
            delete fi;
            delete ff;
            return 0;
        }
        delete fi;
        rc = ff->FindNext(&fi);
    }
    delete ff;
    if (count == 0)
        return FileLoad(FileName, Mode, View);
    return 1;
}
