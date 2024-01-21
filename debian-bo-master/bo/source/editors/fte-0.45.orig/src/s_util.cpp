
/*    s_util.cpp
 *
 *    Copyright (c) 1994-1996, Marko Macek
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 */

#include "fte.h"

#define BUF_SZ 65536

extern RxNode *CompletionFilter;

// should use DosCopy under OS/2...
int copyfile(char *f1, char *f2) { // from F1 to F2
    void *buffer;
    int fd1, fd2;
    int rd;

    buffer = malloc(BUF_SZ);
    if (buffer == 0) return -1;

    if ((fd1 = open(f1, O_RDONLY | O_BINARY)) == -1) {
        free(buffer);
        return -1;
    }
    if ((fd2 = open(f2, O_WRONLY | O_BINARY | O_CREAT | O_TRUNC, 0666)) == -1) {
        close(fd1);
        free(buffer);
        return -1;
    }
    while ((rd = read(fd1, buffer, BUF_SZ)) > 0) {
        if (write(fd2, buffer, rd) != rd) {
            close(fd1);
            close(fd2);
            free(buffer);
            unlink(f2);
            return -1;
        }
    }
    close(fd2);
    close(fd1);
    free(buffer);
    return 0;
}

char *MakeBackup(char *FileName, char *NewName) {
//    static char NewName[260];
    int l;
    
    /* try 1 */
    strcpy(NewName, FileName);
    strcat(NewName, "~");
    if (access(NewName, 0) == 0)                 // Backup already exists?
        unlink(NewName);                         // Then delete the file..
    if (access(FileName, 0) != 0)                // Original found?
        return NewName;
    if (copyfile(FileName, (char *) NewName) == 0)
        return NewName;
    if (errno == 2)
        return (char *) NewName; /* file not found */
    
    /* try 2: 8.3 */
    strcpy(NewName, FileName);
    l = strlen(NewName);
    NewName[l-1] = '~';
    if (strcmp(NewName, FileName) == 0)
        return NULL;
    if (access(NewName, 0) == 0)                   // Backup exists?
        unlink(NewName);                           // Then delete;
    if (access(FileName, 0) != 0)                  // Original exists?
        return NewName;                            // If not-> return base..
    if (copyfile(FileName, (char *) NewName) == 0)
        return NewName;
    if (errno == 2)
        return (char *) NewName;
    return NULL;
}

int GetCharFromEvent(TEvent &E, char *Ch) {
    *Ch = 0;
    if (isAscii(E.Key.Code)) {
        *Ch = (char)E.Key.Code;
        return 1;
    }
    if (kbCode(E.Key.Code) == kbEsc) { *Ch = 27; return 1; }
    if (kbCode(E.Key.Code) == kbEnter) { *Ch = 13; return 1; }
    if (kbCode(E.Key.Code) == (kbEnter | kfCtrl)) { *Ch = 10; return 1; }
    if (kbCode(E.Key.Code) == kbBackSp) { *Ch = 8; return 1; }
    if (kbCode(E.Key.Code) == (kbBackSp | kfCtrl)) { *Ch = 127; return 1; }
    if (kbCode(E.Key.Code) == kbTab) { *Ch = 9; return 1; }
    if (kbCode(E.Key.Code) == kbDel) { *Ch = 127; return 1; }
    if (keyType(E.Key.Code) == kfCtrl) {
        if (keyCode(E.Key.Code) < 32) {
            *Ch = (char) (E.Key.Code & 0x1F);
            return 1;
        }
    }
    return 0;
}

int CompletePath(char *Base, char *Match, int Count) {
    char Name[MAXPATH];
    char *dirp;
    char *namep;
    int len, count = 0;
    char cname[MAXPATH];
    int hascname = 0;
    RxMatchRes RM;
    FileFind *ff;
    FileInfo *fi;
    int rc;

    if (strcmp(Base, "")  == 0) strcpy(Base, ".");
    if (ExpandPath(Base, Name) != 0) return -1;
//    SlashDir(Name);
    dirp = Name;
    namep = strrchr(Name, SLASH);
    if (namep == Name) {
        dirp = SSLASH;
        namep = Name + 1;
    } else if (namep == NULL) {
        namep = Name;
        dirp = SDOT;
    } else {
        *namep = 0;
        namep++;
    }
    
    len = strlen(namep);
    strcpy(Match, dirp);
    SlashDir(Match);
    cname[0] = 0;

    ff = new FileFind(dirp, "*",
                      ffDIRECTORY | ffHIDDEN
#if defined(USE_DIRENT)
                      | ffFAST // for SPEED
#endif
                     );
    if (ff == 0)
        return 0;
    rc = ff->FindFirst(&fi);
    while (rc == 0) {
        char *dname = fi->Name();

        // filter out unwanted files
        if ((strcmp(dname, ".") != 0) &&
            (strcmp(dname, "..") != 0) &&
            (!CompletionFilter || RxExec(CompletionFilter, dname, strlen(dname), dname, &RM) != 1))
        {
            if ((
#if defined(UNIX)
                strncmp
#else // os2, nt, ...
                strnicmp
#endif
                (namep, dname, len) == 0)
                && (dname[0] != '.' || namep[0] == '.'))
            {
                count++;
                if (Count == count) {
                    Slash(Match, 1);
                    strcat(Match, dname);
                    if (
#if defined(USE_DIRENT) // for SPEED
                        IsDirectory(Match)
#else
                        fi->Type() == fiDIRECTORY
#endif
                       )
                        Slash(Match, 1);
                } else if (Count == -1) {
                    
                    if (!hascname) {
                        strcpy(cname, dname);
                        hascname = 1;
                    } else {
                        int o = 0;
#ifdef UNIX
                        while (cname[o] && dname[o] && (cname[o] == dname[o])) o++;
#endif
#if defined(OS2) || defined(NT) || defined(DOS) || defined(DOSP32)
                        while (cname[o] && dname[o] && (toupper(cname[o]) == toupper(dname[o]))) o++;
#endif
                        cname[o] = 0;
                    }
                }
            }
        }
        delete fi;
        rc = ff->FindNext(&fi);
    }
    delete ff;
    if (Count == -1) {
        Slash(Match, 1);
        strcat(Match, cname);
        if (count == 1) SlashDir(Match);
    }
    return count;
}

int UnTabStr(char *dest, int maxlen, char *source, int slen) {
    char *p = dest;
    int i;
    int pos = 0;

    maxlen--;
    for (i = 0; i < slen; i++) {
        if (maxlen > 0) {
            if (source[i] == '\t') {
                do {
                    if (maxlen > 0) {
                        *p++ = ' ';
                        maxlen--;
                    }
                    pos++;
                } while (pos & 0x7);
            } else {
                *p++ = source[i];
                pos++;
                maxlen--;
            }
        } else
            break;
    }

    dest[pos] = 0;
    return pos;
}
