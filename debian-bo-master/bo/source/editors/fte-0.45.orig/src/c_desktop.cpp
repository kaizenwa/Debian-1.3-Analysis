/*    c_desktop.cpp
 *
 *    Copyright (c) 1994-1996, Marko Macek
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 */

#include "fte.h"

#ifdef CONFIG_DESKTOP

#define DESKTOP_VER "FTE Desktop 1\n"

char DesktopFileName[256] = "";

int SaveDesktop(char *FileName) {
    FILE *fp;
    EModel *M;
    
    fp = fopen(FileName, "w");
    if (fp == 0)
        return 0;
    
    setvbuf(fp, FileBuffer, _IOFBF, sizeof(FileBuffer));
    
    fprintf(fp, DESKTOP_VER);
    
    M = ActiveModel;
    while (M) {
        switch(M->GetContext()) {
        case CONTEXT_FILE:
            {
                EBuffer *B = (EBuffer *)M;
                fprintf(fp, "F|%s\n", B->FileName);
            }
            break;
#ifdef CONFIG_OBJ_DIRECTORY
        case CONTEXT_DIRECTORY:
            {
                EDirectory *D = (EDirectory *)M;
                fprintf(fp, "D|%s\n", D->Path);
            }
            break;
#endif
        }
        M = M->Next;
        if (M == ActiveModel)
            break;
    }
#ifdef CONFIG_TAGS
    TagsSave(fp);
#endif
    fclose(fp);
    return 1;
}

int LoadDesktop(char *FileName) {
    FILE *fp;
    char line[512];
    char *p, *e;

#ifdef CONFIG_TAGS
    TagClear();
#endif
    
    fp = fopen(FileName, "r");
    if (fp == 0)
        return 0;

    //setvbuf(fp, FileBuffer, _IOFBF, sizeof(FileBuffer));
    
    if (fgets(line, sizeof(line), fp) == 0 ||
        strcmp(line, DESKTOP_VER) != 0)
    {
        fclose(fp);
        return 0;
    }
    while (fgets(line, sizeof(line), fp) != 0) {
        e = strchr(line, '\n');
        if (e == 0)
            break;
        *e = 0;
        if (line[0] == 'F' && line[1] == '|') { // file
            p = line + 2;
            
            FileLoad(p, 0, ActiveView);
            assert(ActiveModel != 0);
            ActiveView->SwitchToModel(ActiveModel->Next);
#ifdef CONFIG_OBJ_DIRECTORY
        } else if (line[0] == 'D' && line[1] == '|') { // directory
            p = line + 2;
            
            new EDirectory(&ActiveModel, p);
            assert(ActiveModel != 0);
            ActiveView->SwitchToModel(ActiveModel);
            ActiveView->SwitchToModel(ActiveModel->Next);
#endif
#ifdef CONFIG_TAGS
        } else if (line[0] == 'T' && line[1] == '|') { // tag file
            TagsAdd(line + 2);
#endif
        }
    }
    fclose(fp);
    return 1;
}

#endif
