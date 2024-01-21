/*    e_loadsave.cpp
 *
 *    Copyright (c) 1994-1996, Marko Macek
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 */

#include "fte.h"

int EBuffer::Load() {
    return LoadFrom(FileName);
}

int EBuffer::Reload() {
    int R = VToR(CP.Row), C = CP.Col;
    
    if (LoadFrom(FileName) == 0) 
        return 0;
    SetNearPosR(C, R);
    return 1;
}

int EBuffer::Save() {
    if (BFI(this, BFI_ReadOnly)) {
        Msg(ERROR, "File is read-only.");
        return 0;
    }
    return SaveTo(FileName);
}

char FileBuffer[RWBUFSIZE];

int EBuffer::LoadFrom(char *AFileName) {
    int fd;
    int len = 0;
    unsigned long Chars = 0, Lines = 0;
    char *p, *e, *m = 0;
    int lm = 0;
    int lf;
    int SaveUndo, SaveReadOnly;
    int strip = BFI(this, BFI_StripChar);
    int lchar = BFI(this, BFI_LineChar);
    int margin = BFI(this, BFI_LoadMargin);
    
    FileOk = 0;
    fd = open(AFileName, O_RDONLY | O_BINARY);
    if (fd == -1) {
        Msg(INFO, "New file %s.", AFileName);
        Loaded = 1;
        return 0;
    }
    Loading = 1;
    Clear();
    BlockUnmark();
    SaveUndo = BFI(this, BFI_Undo);
    SaveReadOnly = BFI(this, BFI_ReadOnly);
    BFI(this, BFI_Undo) = 0;
    BFI(this, BFI_ReadOnly) = 0;
    
    while ((len = read(fd, FileBuffer, sizeof(FileBuffer))) > 0) {
        p = FileBuffer;
        if (len > 0) do {
            if (lchar != -1) {
                e = (char *)memchr((void *)p, lchar, FileBuffer - p + len);
                if (e == 0) {
                    e = FileBuffer + len;
                    lf = 0;
                } else
                    lf = 1;
            } else if (margin != -1) {
                if (FileBuffer + len >= p + margin) {
                    e = p + margin;
                    lf = 1;
                } else {
                    e = FileBuffer + len;
                    lf = 0;
                }
            } else {
                e = FileBuffer + len;
                lf = 0;
            }
            m = (char *)realloc((void *)m, (lm + e - p) | CHAR_TRESHOLD);
            if (m == 0)
                goto fail;
            memcpy((void *)(m + lm), p, e - p);
            lm += e - p;
            Chars += e - p;
            if (lf) {
                if (lm == 0 && m == 0 && (m = (char *)malloc(CHAR_TRESHOLD)) == 0)
                    goto fail;
                if (RCount == RAllocated)
                    if (Allocate(RCount ? (RCount * 2) : 1) == 0)
                        goto fail;
                if ((LL[RCount++] = new ELine((char *)m, lm)) == 0)
                    goto fail;
                RGap = RCount;
                
                lm = 0;
                m = 0;
                Lines++;
            }
            p = e;
            if (lchar != -1) 
                p++;
        } while (lf);
        Msg(INFO, "Loading: %d lines, %d bytes.", Lines, Chars);
    }
    if (lm == 0 && m == 0 && (m = (char *)malloc(CHAR_TRESHOLD)) == 0)
        goto fail;
    if (RCount == RAllocated)
        if (Allocate(RCount ? (RCount * 2) : 1) == 0)
            goto fail;
    if ((LL[RCount++] = new ELine(m, lm)) == 0)
        goto fail;
    RGap = RCount;
    
    VCount = RCount;
    VGap = VCount;
    if (AllocVis(VCount ? VCount : 1) == 0)
        goto fail;
    
    memset(VV, 0, VCount * sizeof(int));
    
    if (strip != -1) {
        int l;
        for (l = 0; l < RCount; l++) {
            if (LL[l]->Count > 0)
                if (LL[l]->Chars[LL[l]->Count - 1] == strip)
                    LL[l]->Count--;
        }
    }
    if ((BFI(this, BFI_SaveFolds) != 0)) {
        int len_start = 0, len_end = 0;
        int level = 0, open = 0;
        int l;
        int pos = -1;
        char foldnum[3] = "00";
        
        if (BFS(this, BFS_CommentStart) == 0) len_start = 0;
        else len_start = strlen(BFS(this, BFS_CommentStart));
        if (BFS(this, BFS_CommentEnd) == 0) len_end = 0;
        else len_end = strlen(BFS(this, BFS_CommentEnd));
        
        for (l = RCount - 1; l >= 0; l--) {
            if (LL[l]->Count >= len_start + len_end + 6) {
                open = -1;
                level = -1;
                if (BFI(this, BFI_SaveFolds) == 1) {
                    pos = 0;
                } else if (BFI(this, BFI_SaveFolds) == 2) {
                    pos = LL[l]->Count - len_start - 6 - len_end;
                }
                if (((len_start == 0) ||
                     (memcmp(LL[l]->Chars + pos,
                             BFS(this, BFS_CommentStart), len_start) == 0)
                    ) &&
                    ((len_end == 0) ||
                     (memcmp(LL[l]->Chars + pos + len_start + 6,
                             BFS(this, BFS_CommentEnd), len_end) == 0))
                   )
                {
                    if (memcmp(LL[l]->Chars + pos + len_start,
                               "FOLD",4) == 0)
                    {
                        open = 1;
                    } else if (memcmp(LL[l]->Chars + pos + len_start,
                                      "fold", 4) == 0) {
                        open = 0;
                    } else
                        open = -1;

                    foldnum[0] = LL[l]->Chars[pos + len_start + 4];
                    foldnum[1] = LL[l]->Chars[pos + len_start + 4 + 1];
                    if (1 != sscanf(foldnum, "%2d", &level))
                        level = -1;
                    
                    if (!isdigit(LL[l]->Chars[pos + len_start + 4]) ||
                        !isdigit(LL[l]->Chars[pos + len_start + 5]))
                        level = -1;
                    
                    if (open != -1 && level != -1 && open < 100) {
                        int f;
                        
                        if (FoldCreate(l) == 0) goto fail;
                        f = FindFold(l);
                        assert(f != -1);
                        FF[f].level = (char)(level & 0xFF);
                        if (open == 0)
                            if (FoldClose(l) == 0) goto fail;
                        memmove(LL[l]->Chars + pos,
                                LL[l]->Chars + pos + len_start + len_end + 6,
                                LL[l]->Count - pos - len_start - len_end - 6);
                        LL[l]->Count -= len_start + len_end + 6;
                    }
                }
            }
        }
    }
    if (SetPosR(0, 0) == 0) return 0;
    BFI(this, BFI_Undo) = SaveUndo;
    BFI(this, BFI_ReadOnly) = SaveReadOnly;
    close(fd);
    
    FileOk = 1;
    if (stat(FileName, &FileStatus) == -1) {
	memset(&FileStatus, 0, sizeof(FileStatus));
	FileOk = 0;
    }
    Modified = 0;
    Loading = 0;
    Loaded = 1;
    Draw(0, -1);
    Msg(INFO, "Loaded %s.", AFileName);
    return 1;
fail:
    BFI(this, BFI_Undo) = SaveUndo;
    BFI(this, BFI_ReadOnly) = SaveReadOnly;
    close(fd);
    Loading = 0;
    Draw(0, -1);
    View->MView->Win->Choice(GPC_ERROR, "Error", 1, "O&K", "Error loading %s.", AFileName);
    return 0;
}

int EBuffer::SaveTo(char *AFileName) {
    char ABackupName[MAXPATH];
    struct stat StatBuf;
    
    FILE *fp;
    int l;
    PELine L;
    unsigned long ByteCount = 0, OldCount = 0;
  
    int f;
    char fold[64];
    int foldlen = 0;

    if (FileOk && (stat(FileName, &StatBuf) == 0)) {
	if (FileStatus.st_size != StatBuf.st_size ||
	    FileStatus.st_mtime != StatBuf.st_mtime)
        {
            switch (View->MView->Win->Choice(GPC_ERROR, "File Changed on Disk",
                           2,
                           "&Save",
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
    }
    
    if (RCount <= 0) return 0;
    Msg(INFO, "Backing up %s...", AFileName);
    if (MakeBackup(AFileName, (char *)ABackupName) == 0) {
        View->MView->Win->Choice(GPC_ERROR, "Error", 1, "O&K", "Could not create backup file.");
        return 0;
    }
    Msg(INFO, "Writing %s...", AFileName);
    
    fp = fopen(AFileName, "wb");
    if (fp == 0) goto erroropen;
    
    setvbuf(fp, FileBuffer, _IOFBF, sizeof(FileBuffer));
    
    for (l = 0; l < RCount; l++) {
        L = RLine(l);
        f = FindFold(l);
        
        // format fold
        if ((f != -1) && (BFI(this, BFI_SaveFolds) != 0)) {
            foldlen = 0;
            if (BFS(this, BFS_CommentStart) != 0) {
                strcpy(fold + foldlen, BFS(this, BFS_CommentStart));
                foldlen += strlen(BFS(this, BFS_CommentStart));
            }
            foldlen += sprintf(fold + foldlen,
                               FF[f].open ? "FOLD%02d" : "fold%02d",
                               FF[f].level);
            if (BFS(this, BFS_CommentEnd) != 0) {
                strcpy(fold + foldlen, BFS(this, BFS_CommentEnd));
                foldlen += strlen(BFS(this, BFS_CommentEnd));
            }
            ByteCount += foldlen;
        }
        
        // write bol fold
        if (f != -1 && BFI(this, BFI_SaveFolds) == 1)
            if (fwrite(fold, 1, foldlen, fp) != foldlen) goto fail;

        // write data
        if (fwrite(L->Chars, 1, L->Count, fp) != L->Count)
            goto fail;
        ByteCount += L->Count;
        
        // write eof fold
        if (f != -1 && BFI(this, BFI_SaveFolds) == 2)
            if (fwrite(fold, 1, foldlen, fp) != foldlen) goto fail;
        
        // write eol
        if ((l < RCount - 1) || BFI(this, BFI_ForceNewLine)) {
            if (BFI(this, BFI_AddCR) == 1) {
                if (fputc(13, fp) < 0) goto fail;
                ByteCount++;
            }
            if (BFI(this, BFI_AddLF) == 1) {
                if (fputc(10, fp) < 0) goto fail;
                ByteCount++;
            }
        }
        if (ByteCount > OldCount + 65536) {
            Msg(INFO, "Saving: %d lines, %d bytes.", l, ByteCount);
            OldCount = ByteCount;
        }
    }
    if (fclose(fp) != 0) goto fail;
    Modified = 0;
    FileOk = 1;
    if (stat(FileName, &FileStatus) == -1) {
	memset(&FileStatus, 0, sizeof(FileStatus));
	FileOk = 0;
    }
    Msg(INFO, "Wrote %s.", AFileName);
    if (BFI(this, BFI_KeepBackups) == 0) {
        unlink(ABackupName);
    }
    return 1;
fail:
    fclose(fp);
    unlink(AFileName);
    if (rename(ABackupName, AFileName) == -1) {
        View->MView->Win->Choice(GPC_ERROR, "Error", 1, "O&K", "Error renaming backup file to original!");
    } else {
        View->MView->Win->Choice(GPC_ERROR, "Error", 1, "O&K", "Error writing file, backup restored.");
    }
    return 0;
erroropen:
    View->MView->Win->Choice(GPC_ERROR, "Error", 1, "O&K", "Error writing %s.", AFileName);
    return 0;
}
