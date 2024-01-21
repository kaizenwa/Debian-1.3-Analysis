/*
 * Author:      William Chia-Wei Cheng (william@cs.ucla.edu)
 *
 * Copyright (C) 1990-1996, William Chia-Wei Cheng.
 *
 * Permission limited to the use, copy, display, distribute without
 * charging for a fee, and produce derivative works of "tgif" and
 * its documentation for not-for-profit purpose is hereby granted by
 * the Author, provided that the above copyright notice appears in
 * all copies made of "tgif" and that both the copyright notice
 * and this permission notice appear in supporting documentation,
 * and that the name of the Author not be used in advertising or
 * publicity pertaining to distribution of the software without
 * specific, written prior permission.  The Author makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied
 * warranty.  All other rights (including, but not limited to, the
 * right to sell "tgif", the right to sell derivative works of
 * "tgif", and the right to distribute "tgif" for a fee) are
 * reserved by the Author.
 *
 * THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, INDIRECT
 * OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#ifndef lint
static char RCSid[] =
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/names.c,v 3.0 1996/05/06 16:06:13 william Exp $";
#endif

#include <sys/types.h>
#ifdef VMS
#include "vms_comp.h"
#define DIR_ENTRY struct dirent
#else
#ifdef ibm
#include <sys/dir.h>
#define DIR_ENTRY struct direct
#else
#ifdef apollo
#include <sys/dir.h>
#define DIR_ENTRY struct direct
#else
#ifdef NeXT
#include <sys/dir.h>
#define DIR_ENTRY struct direct
#else
#ifdef luna88k
#include <sys/dir.h>
#define DIR_ENTRY struct direct
#else
#ifdef sequent
#include <sys/dir.h>
#define DIR_ENTRY struct direct
#else
#include <dirent.h>
#define DIR_ENTRY struct dirent
#endif
#endif
#endif
#endif
#endif
#endif
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include "const.h"
#include "types.h"

#include "auxtext.e"
#include "box.e"
#include "button.e"
#include "choose.e"
#include "cutpaste.e"
#include "cursor.e"
#include "dialog.e"
#include "drawing.e"
#include "file.e"
#include "font.e"
#include "import.e"
#include "mainloop.e"
#include "mainmenu.e"
#include "menu.e"
#include "msg.e"
#ifndef _NO_EXTERN
#include "names.e"
#endif
#include "navigate.e"
#include "raster.e"
#include "rect.e"
#include "remote.e"
#include "setup.e"
#include "util.e"
#include "xpixmap.e"

extern char *getenv ARGS_DECL((char*));
extern int atoi ARGS_DECL((char*));

char curDomainName[MAXPATHLENGTH+1];
char curDomainPath[MAXPATHLENGTH+1];
char curDir[MAXPATHLENGTH+1];
char curLocalDir[MAXPATHLENGTH+1];
char curImportDir[MAXPATHLENGTH+1];
char curSymDir[MAXPATHLENGTH+1];

int doubleClickInterval=300;
int importFromLibrary=FALSE;
int curDirIsLocal=TRUE;

int domainInResource=TRUE;

int ignoreDirectoryFlag=FALSE;

static DspList *symbolList=NULL;
static int numSymbols=0;
static DspList *dirList=NULL;
static int numDirEntries=0;

static DspList *topOfSymLinkList=NULL, *topOfDirLinkList=NULL;

static
char *ReadPath(path_str, dir_str)
   char *path_str, *dir_str;
{
   register char *s1, *s2;

   s1 = path_str;
   while (*s1==' ' || *s1=='\t' || *s1=='\n') s1++;
   if (*s1 == '~') {
      strcpy(dir_str, homeDir);
      s2 = &dir_str[strlen(dir_str)];
      s1++;
   } else {
      s2 = dir_str;
   }
   for ( ; *s1 != '\0' && *s1 != ':'; s1++) {
      if (*s1 == '\\') {
         strcpy(s1, s1+1);
      } else {
         *s2++ = *s1;
      }
   }
   *s2 = '\0';
   if (*s1 == ':') s1++;
   return s1;
}

static
void CleanUpSymPath()
{
   if (symPath != NULL) {
      int i;

      for (i=0; i < symPathNumEntries; i++) {
         if (symPath[i] != NULL) {
            free(symPath[i]);
         }
      }
      if (symPath != NULL) free(symPath);
      symPath = NULL;
   }
}

void ParseSymPath(path_str)
   char *path_str;
{
   int i, path_len=0;
   char *s, dir_str[MAXPATHLENGTH+1];

   CleanUpSymPath();

   for (i=0, s=path_str; *s != '\0'; ) {
      s = ReadPath(s, dir_str);
      UtilTrimBlanks(dir_str);
      if (dir_str != '\0') i++;
   }
   symPath = (char**)malloc(i*sizeof(char*));
   if (symPath == NULL) FailAllocMessage();
   symPathNumEntries = i;
   *curDomainPath = '\0';
   for (i=0, s=path_str; *s != '\0'; ) {
      s = ReadPath(s, dir_str);
      UtilTrimBlanks(dir_str);
      if (dir_str != '\0') {
         symPath[i] = (char*)malloc((MAXPATHLENGTH+1)*sizeof(char));
         if (symPath[i] == NULL) FailAllocMessage();
         strcpy(symPath[i], dir_str);
         if (path_len == 0) {
            sprintf(&curDomainPath[path_len], "%s", dir_str);
            path_len += strlen(dir_str);
         } else {
            sprintf(&curDomainPath[path_len], ":%s", dir_str);
            path_len += strlen(dir_str)+1;
         }
         i++;
      }
   }
}

static
int LargerStr(S1, S2)
   register char	* S1, * S2;
   /* returns TRUE if S1 > S2 */
{
   while (*S1 == *S2 && *S1 != '\0' && *S2 != '\0') { S1++; S2++; }

   return (*S1 > *S2);
}

static
DspList *SymbolListing()
{
   int len, path_index, count = 0, reject, sym_ext_len;
   char path[MAXPATHLENGTH], sym_ext_str[MAXSTRING];
   DspList *dsp_ptr, *head_ptr, *tail_ptr, *p, *p1;
   DIR *dirp;
   DIR_ENTRY *d;

   head_ptr = tail_ptr = NULL;

   sprintf(sym_ext_str, ".%s", SYM_FILE_EXT);
   sym_ext_len = strlen(sym_ext_str);
   for (path_index = 0; path_index < symPathNumEntries; path_index++) {
      strcpy(path, symPath[path_index]);
      if (strcmp(".", path) == 0) {
         if (curDirIsLocal) {
            strcpy(path, curDir);
         } else {
            strcpy(path, curLocalDir);
         }
      }
      if ((dirp=opendir(path)) == NULL) {
         sprintf(gszMsgBox, "%s '%s' %s.",
               "Cannot open the", path, "directory for reading");
         Msg(gszMsgBox);
         continue;
      }
      while ((d=readdir(dirp)) != NULL) {
         len = strlen(d->d_name);
         if (len > sym_ext_len &&
               (strcmp(sym_ext_str, &d->d_name[len-sym_ext_len]) == 0)) {
            d->d_name[len-sym_ext_len] = '\0';
         } else {
            continue;
         }
         if (head_ptr == NULL) {
            head_ptr = tail_ptr = (DspList*)malloc(sizeof(DspList));
            if (head_ptr == NULL) FailAllocMessage();
            memset(head_ptr, 0, sizeof(DspList));
            UtilStrCpy(head_ptr->itemstr, sizeof(head_ptr->itemstr), d->d_name);
            UtilStrCpy(head_ptr->pathstr, sizeof(head_ptr->pathstr), path);
         } else {
            p1 = NULL;
            reject = FALSE;
            for (p=head_ptr; p != NULL; p = p->next) {
               if (strcmp(d->d_name, p->itemstr) == 0) {
                  reject = TRUE;
                  break;
               } else if (LargerStr (d->d_name, p->itemstr)) {
                  p1 = p;
               } else {
                  break;
               }
            }
            if (reject) continue;
   
            dsp_ptr = (DspList*)malloc(sizeof(DspList));
            if (dsp_ptr == NULL) FailAllocMessage();
            memset(dsp_ptr, 0, sizeof(DspList));
            dsp_ptr->next = p;
            UtilStrCpy(dsp_ptr->itemstr, sizeof(dsp_ptr->itemstr), d->d_name);
            UtilStrCpy(dsp_ptr->pathstr, sizeof(dsp_ptr->pathstr), path);

            if (p == NULL) {
               /* dsp_ptr has the largest element */
               tail_ptr->next = dsp_ptr;
               tail_ptr = dsp_ptr;
            } else if (p1 == NULL) {
               head_ptr = dsp_ptr;
            } else {
               p1->next = dsp_ptr;
            }
         }
         count++;
      }
      closedir(dirp);
   }
   numSymbols = count;
   return head_ptr;
}

static
void BuildSymbolList()
{
   int i;
   DspList *dsp_ptr, *next_dsp;

   if (symbolList != NULL) free(symbolList);

   symbolList = (DspList*)malloc(numSymbols*sizeof(DspList));
   if (symbolList == NULL) FailAllocMessage();
   memset(symbolList, 0, numSymbols*sizeof(DspList));
   dsp_ptr = topOfSymLinkList;
   for (i=0; i < numSymbols; i++, dsp_ptr = next_dsp) {
      next_dsp = dsp_ptr->next;
      UtilStrCpy(symbolList[i].itemstr, sizeof(symbolList[i].itemstr),
            dsp_ptr->itemstr);
      UtilStrCpy(symbolList[i].pathstr, sizeof(symbolList[i].pathstr),
            dsp_ptr->pathstr);
      symbolList[i].next = &symbolList[i+1];
      free(dsp_ptr);
   }
   symbolList[numSymbols-1].next = NULL;
   topOfSymLinkList = NULL;
}

static
DspList *DirListing(Path, ExtStr, OtherExtStr)
   char *Path, *ExtStr, *OtherExtStr;
{
   DspList *dsp_ptr, *head_ptr, *tail_ptr, *p, *p1;
   DIR *dirp;
   DIR_ENTRY *d;
   int len, ext_len, count = 0;
   char path[MAXPATHLENGTH], s[MAXPATHLENGTH], *ext_str=NULL;
   struct stat stat_buf;

   if (ExtStr != NULL && *ExtStr != '\0') {
      if (OtherExtStr != NULL && *OtherExtStr != '\0') {
         ext_len = (strlen(ExtStr)<<1)+3+strlen(OtherExtStr);
         if ((ext_str=SetUpExtStr(ext_len, ExtStr, OtherExtStr)) == NULL) {
            return NULL;
         }
      } else {
         ext_len = (strlen(ExtStr)<<1)+3;
         if ((ext_str=SetUpExtStr(ext_len, ExtStr, "")) == NULL) {
            return NULL;
         }
      }
   } else if (OtherExtStr != NULL && *OtherExtStr != '\0') {
      ext_len = strlen(OtherExtStr)+3;
      if ((ext_str=SetUpExtStr(ext_len, "", OtherExtStr)) == NULL) {
         return NULL;
      }
   }
   ext_len = (ext_str == NULL ? 0 : strlen(ext_str));
   if (*Path == '\0') {
      strcpy(path, "/");
      if ((dirp=opendir(path)) == NULL) {
         sprintf(gszMsgBox, "%s '%s' %s.",
               "Fail to open the", path, "directory for reading");
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         if (ext_str != NULL) free(ext_str);
         return NULL;
      }
   } else {
      strcpy(path, Path);
      if ((dirp=opendir(path)) == NULL) {
         sprintf(gszMsgBox, "%s '%s' %s.",
               "Fail to open the", path, "directory for reading");
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         if (ext_str != NULL) free(ext_str);
         return NULL;
      }
      strcat (path, "/");
   }

#ifdef VMS
   /* Fake the .. entry for VMS */
   head_ptr = tail_ptr = (DspList*)malloc(sizeof(DspList));
   if (head_ptr == NULL) FailAllocMessage();
   memset(head_ptr, 0, sizeof(DspList));
   head_ptr->directory = TRUE;
   UtilStrCpy(head_ptr->itemstr, sizeof(head_ptr->itemstr), "../");
   head_ptr->next = NULL;
   count = 1;
#else
   head_ptr = tail_ptr = NULL;
#endif /* VMS */

   while ((d=readdir(dirp)) != NULL) {
      len = strlen(d->d_name);
      if (ext_len == 0) {
#ifdef VMS
         if (len > 4 && (strcmp(".dir", &d->d_name[len-4]) == 0)) {
            d->d_name[len-4] = '/';
            d->d_name[len-4+1] = '\0';
            dsp_ptr = (DspList*)malloc(sizeof(DspList));
            if (dsp_ptr == NULL) FailAllocMessage();
            memset(dsp_ptr, 0, sizeof(DspList));
            dsp_ptr->directory = TRUE;
            UtilStrCpy(dsp_ptr->itemstr, sizeof(dsp_ptr->itemstr), d->d_name);
         } else if (strcmp(d->d_name, ".") == 0) {
#else
         if (strcmp(d->d_name, ".") == 0) {
#endif /* VMS */
            continue;
         } else {
            sprintf(s, "%s%s", path, d->d_name);
            stat(s, &stat_buf);
            dsp_ptr = (DspList*)malloc(sizeof(DspList));
            if (dsp_ptr == NULL) FailAllocMessage();
            memset(dsp_ptr, 0, sizeof(DspList));
            if (stat_buf.st_mode & S_IFDIR) {
               dsp_ptr->directory = TRUE;
               strcat(d->d_name, "/");
            } else {
               dsp_ptr->directory = FALSE;
            }
            UtilStrCpy(dsp_ptr->itemstr, sizeof(dsp_ptr->itemstr), d->d_name);
         }
      } else if (ExtensionMatch(ext_str, d->d_name)) {
         dsp_ptr = (DspList*)malloc(sizeof(DspList));
         if (dsp_ptr == NULL) FailAllocMessage();
         memset(dsp_ptr, 0, sizeof(DspList));
         dsp_ptr->directory = FALSE;
         UtilStrCpy(dsp_ptr->itemstr, sizeof(dsp_ptr->itemstr), d->d_name);
#ifdef VMS
      } else if (len > 4 && (strcmp(".dir", &d->d_name[len-4]) == 0)) {
         d->d_name[len-4] = '/';
         d->d_name[len-4+1] = '\0';
         dsp_ptr = (DspList*)malloc(sizeof(DspList));
         if (dsp_ptr == NULL) FailAllocMessage();
         memset(dsp_ptr, 0, sizeof(DspList));
         dsp_ptr->directory = TRUE;
         UtilStrCpy(dsp_ptr->itemstr, sizeof(dsp_ptr->itemstr), d->d_name);
#endif /* VMS */
      } else if (strcmp(d->d_name, ".") == 0) {
         continue;
      } else {
         sprintf(s, "%s%s", path, d->d_name);
         stat(s, &stat_buf);
         if (stat_buf.st_mode & S_IFDIR) {
            dsp_ptr = (DspList*)malloc(sizeof(DspList));
            if (dsp_ptr == NULL) FailAllocMessage();
            memset(dsp_ptr, 0, sizeof(DspList));
            dsp_ptr->directory = TRUE;
            strcat(d->d_name, "/");
            UtilStrCpy(dsp_ptr->itemstr, sizeof(dsp_ptr->itemstr), d->d_name);
         } else {
            continue;
         }
      }
      if (head_ptr == NULL) {
         head_ptr = tail_ptr = dsp_ptr;
      } else {
         p1 = NULL;
         for (p = head_ptr; p != NULL; p = p->next) {
            if (LargerStr(d->d_name, p->itemstr)) {
               p1 = p;
            } else {
               break;
            }
         }
         dsp_ptr->next = p;
         if (p == NULL) {
            /* dsp_ptr has the largest element */
            tail_ptr->next = dsp_ptr;
            tail_ptr = dsp_ptr;
         } else if (p1 == NULL) {
            head_ptr = dsp_ptr;
         } else {
            p1->next = dsp_ptr;
         }
      }
      count++;
   }
   closedir(dirp);
   numDirEntries = count;
   if (ext_str != NULL) free(ext_str);
   return head_ptr;
}

static
void BuildDirList()
{
   int i;
   DspList *dsp_ptr, *next_dsp;

   if (topOfDirLinkList != NULL) {
      if (dirList != NULL) free(dirList);

      dirList = (DspList*)malloc(numDirEntries*sizeof(DspList));
      if (dirList == NULL) FailAllocMessage();
      memset(dirList, 0, numDirEntries*sizeof(DspList));
      dsp_ptr = topOfDirLinkList;
      for (i=0; i < numDirEntries; i++, dsp_ptr = next_dsp) {
         next_dsp = dsp_ptr->next;
         UtilStrCpy(dirList[i].itemstr, sizeof(dirList[i].itemstr),
               dsp_ptr->itemstr);
         UtilStrCpy(dirList[i].pathstr, sizeof(dirList[i].pathstr),
               dsp_ptr->pathstr);
         dirList[i].directory = dsp_ptr->directory;
         dirList[i].next = &dirList[i+1];
         free(dsp_ptr);
      }
      dirList[numDirEntries-1].next = NULL;
      topOfDirLinkList = NULL;
   }
}

static
void ConvertToUpperCase(InStr, OutStr)
   register char *InStr, *OutStr;
{
   for ( ; *InStr != '\0'; InStr++, OutStr++) {
      *OutStr = (*InStr>='a' && *InStr<='z') ? *InStr-'a'+'A' : *InStr;
   }
   *OutStr = '\0';
}

void InitNames()
{
   int default_found = FALSE;
   char *c_ptr, domain_str[20], sym_path[80], cap_tool_name[MAXSTRING];

   InitNamesInfo();

   ignoreDirectoryFlag = FALSE;

   *curDomainName = '\0';
   *curDomainPath = '\0';
   *curSymDir = '\0';
   strcpy(curDir, bootDir);
   strcpy(curImportDir, bootDir);

   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "DoubleClickInterval")) !=
         NULL) {
      doubleClickInterval = atoi(c_ptr);
   } else {
      doubleClickInterval = 300;
   }
   warpToWinCenter = TRUE;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"WarpToWinCenter")) != NULL) {
      if (UtilStrICmp(c_ptr, "false") == 0) {
         warpToWinCenter = FALSE;
      }
   }
   importFromLibrary = FALSE;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"ImportFromLibrary")) != NULL) {
      if (UtilStrICmp(c_ptr, "true") == 0) {
         importFromLibrary = TRUE;
      }
   }
   domainInResource = TRUE;
   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "DefaultDomain")) != NULL) {
      int default_domain=atoi(c_ptr);

      sprintf(domain_str, "DomainPath%1d", default_domain);
      if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, domain_str)) != NULL) {
         char	* c_ptr1;

         while (*c_ptr==' ' || *c_ptr=='\t' || *c_ptr=='\n') c_ptr++;
         if (*c_ptr != '\0' && (c_ptr1=strchr (c_ptr, ':')) != NULL) {
            int	len=c_ptr1-c_ptr;

            strncpy(curDomainName, c_ptr, len);
            curDomainName[len] = '\0';
            c_ptr = (++c_ptr1);
            default_found = TRUE;
         }
      }
      else
      {
         sprintf(domain_str, "Domain%1d", default_domain);
         if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, domain_str)) != NULL) {
            if (*c_ptr != '\0') {
               ConvertToUpperCase(TOOL_NAME, cap_tool_name);
               strcpy(curDomainName, c_ptr);
               sprintf(sym_path, "%s_%s", cap_tool_name, c_ptr);
               default_found = TRUE;
               domainInResource = FALSE;
            }
         }
      }
   }
   if (default_found) {
      if (domainInResource) {
         ParseSymPath(c_ptr);
      } else if ((c_ptr=getenv(sym_path)) == NULL ||
            ((int)strlen(c_ptr)) >= MAXPATHLENGTH-1) {
         ParseSymPath(".");
      } else {
         ParseSymPath(c_ptr);
      }
   } else {
      ParseSymPath(".");
   }
   if ((topOfSymLinkList=SymbolListing()) != NULL) BuildSymbolList();
}

void UpdateDirInfo()
{
   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   if (curDirIsLocal) {
      if ((topOfDirLinkList=DirListing(curDir, OBJ_FILE_EXT, "")) != NULL) {
         BuildDirList();
      }
   } else if ((topOfDirLinkList=DirListing(curLocalDir, OBJ_FILE_EXT, "")) !=
         NULL) {
      BuildDirList();
   }
   SetDefaultCursor(mainWindow);
   SetDefaultCursor(drawWindow);
}

void UpdateSymInfo()
{
   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   if ((topOfSymLinkList = SymbolListing()) != NULL) BuildSymbolList();
   SetDefaultCursor(mainWindow);
   SetDefaultCursor(drawWindow);
}

void CleanUpNames()
{
   CleanUpNamesInfo();

   CleanUpSymPath();
   if (symbolList != NULL) free(symbolList);
   if (dirList != NULL) free(dirList);
   symbolList = dirList = NULL;
   ignoreDirectoryFlag = FALSE;
}

char **MakeNameDspItemArray(Entries, DLPtr)
   int Entries;
   DspList *DLPtr;
{
   int i, j, len;
   char **dsp_ptr, *c_ptr;

   if (Entries == 0) return NULL;

   dsp_ptr = (char**)malloc(Entries*sizeof(char*));
   if (dsp_ptr == NULL) FailAllocMessage();
   c_ptr = (char*)malloc(Entries*(MAXPATHLENGTH+1)*sizeof(char));
   if (c_ptr == NULL) FailAllocMessage();
   for (i=0; i < Entries; i++, DLPtr=DLPtr->next) {
      dsp_ptr[i] = c_ptr;
      len = strlen(DLPtr->itemstr);
      if (!ignoreDirectoryFlag && !DLPtr->directory) {
         for (j = len; j >= 0 && DLPtr->itemstr[j] != '/'; j--) ;
         if (j >= 0) {
            strcpy(c_ptr, (&(DLPtr->itemstr[j]))+1);
         } else {
            strcpy(c_ptr, DLPtr->itemstr);
         }
      } else {
         strcpy(c_ptr, DLPtr->itemstr);
      }
      c_ptr += MAXPATHLENGTH;
   }
   return dsp_ptr;
}

static int lengthOfLongestItem=INVALID;

static
char **MakeLongNameDspItemArray(Entries, DLPtr)
   int Entries;
   DspList *DLPtr;
{
   int i, len;
   char **dsp_ptr, *c_ptr;

   lengthOfLongestItem = INVALID;

   if (Entries == 0) return NULL;

   dsp_ptr = (char**)malloc(Entries*sizeof(char*));
   if (dsp_ptr == NULL) FailAllocMessage();
   c_ptr = (char*)malloc(Entries*(MAXPATHLENGTH+1)*sizeof(char));
   if (c_ptr == NULL) FailAllocMessage();
   for (i=0; i < Entries; i++, DLPtr=DLPtr->next) {
      dsp_ptr[i] = c_ptr;
      len = strlen(DLPtr->itemstr);
      strcpy(c_ptr, DLPtr->itemstr);
      if (len > lengthOfLongestItem) lengthOfLongestItem = len;

      c_ptr += MAXPATHLENGTH;
   }
   return dsp_ptr;
}

struct DirNamesInfoRec {
   char ext_str[81], other_ext_str[81];
} dirNamesInfo;

static
int GetNamesEntries(p_dsp_ptr, p_entries, pn_num_entries, pn_marked_index,
      cur_buf, p_void)
   DspList **p_dsp_ptr;
   char ***p_entries, *cur_buf;
   int *pn_num_entries, *pn_marked_index;
   void *p_void;
{
   struct DirNamesInfoRec *p_dninfo=(struct DirNamesInfoRec*)p_void;
   DspList *dsp_ptr, *next_dsp;

   for (dsp_ptr=topOfDirLinkList; dsp_ptr != NULL; dsp_ptr=next_dsp) {
      next_dsp = dsp_ptr->next;
      free(dsp_ptr);
   }
   topOfDirLinkList = NULL;

   *p_dsp_ptr = NULL;
   *p_entries = NULL;
   *pn_num_entries = 0;
   *pn_marked_index = INVALID;

   if (*cur_buf == '\0') {
      /* this can only happen if the change_to_root */
      return TRUE;
   } else {
      sprintf(gszMsgBox,
            "Getting directory listing of '%s', please wait ...", cur_buf);
      SetStringStatus(gszMsgBox);
      XSync(mainDisplay, False);
      if ((topOfDirLinkList=DirListing(cur_buf,
            p_dninfo->ext_str, p_dninfo->other_ext_str)) == NULL) {
         RestoreStatusStrings();
         return FALSE;
      }
      *pn_num_entries = numDirEntries;
      if (topOfDirLinkList == NULL) {
         *p_entries = MakeNameDspItemArray(*pn_num_entries, dirList);
      } else {
         *p_entries = MakeNameDspItemArray(*pn_num_entries, topOfDirLinkList);
      }
   }
   return TRUE;
}

static
int NamesAfterLoop(p_dsp_ptr, p_entries, pn_num_entries, pn_marked_index,
      cur_buf, btn_id, selected_index, p_void)
   DspList **p_dsp_ptr;
   char ***p_entries, *cur_buf;
   int *pn_num_entries, *pn_marked_index, btn_id, selected_index;
   void *p_void;
{
   if (*p_dsp_ptr != NULL) free(*p_dsp_ptr);
   if (*p_entries != NULL) {
      free(**p_entries);
      free(*p_entries);
   }
   *p_dsp_ptr = NULL;
   *p_entries = NULL;
   *pn_num_entries = 0;

   switch (btn_id) {
   case BUTTON_OK: break;
   case BUTTON_SETDIR: return FALSE;
   case BUTTON_CANCEL: break;
   }
   return TRUE;
}

static
int DirNames(TopStr, ExtStr, SelStr, JustSetDir)
   char *TopStr, *ExtStr, *SelStr;
   int *JustSetDir;
{
   char win_name[128], other_ext_str[MAXSTRING+1], selected_str[MAXSTRING+1];
   int selected_index=INVALID, selected_btn_id;
   struct DirNamesInfoRec dninfo;

   *SelStr = '\0';
   *JustSetDir = FALSE;

   memset(&dninfo, 0, sizeof(struct DirNamesInfoRec));

   UtilStrCpy(dirNamesInfo.ext_str, sizeof(dirNamesInfo.ext_str), ExtStr);
   UtilStrCpy(dirNamesInfo.other_ext_str, sizeof(dirNamesInfo.other_ext_str),
         ExtStr);

   if (ExtStr != NULL && strcmp(ExtStr, EPSF_FILE_EXT) == 0) {
      sprintf(dirNamesInfo.other_ext_str, ".%s;.epsi", PS_FILE_EXT);
   } else if (ExtStr != NULL && strcmp(ExtStr, OBJ_FILE_EXT) == 0) {
      sprintf(dirNamesInfo.other_ext_str, ".%s", SYM_FILE_EXT);
   } else if (ExtStr != NULL && strcmp(ExtStr, "gif") == 0) {
      strcpy(dirNamesInfo.other_ext_str, ".GIF");
   } else {
      *dirNamesInfo.other_ext_str = '\0';
   }
   ResetNamesInfo();
   NamesSetTitle(TopStr);
   NamesAddButton("OK", BUTTON_OK);
   NamesAddButton("SETDIR", BUTTON_SETDIR);
   NamesAddButton("CANCEL", BUTTON_CANCEL);
   NamesSetEntries(NULL, NULL, 0, TRUE, INVALID, 0);
   NamesSetStyle(NAMES_SELECT_FILE, NAMES_LOOP_MANY);
   NamesSetCallback((GetEntriesFunc*)GetNamesEntries,
         (AfterLoopFunc*)NamesAfterLoop);
   if (importingFile) {
      NamesSetDir(curImportDir);
   } else if (curDirIsLocal) {
      NamesSetDir(curDir);
   } else {
      NamesSetDir(curLocalDir);
   }
   if (importingFile) {
      Msg("Click on 'SETDIR' to set Import directory.");
   } else {
      Msg("Click on 'SETDIR' to set Open/SaveAs directory.");
   }
   sprintf(win_name, "%s - %s", TOOL_NAME, TopStr);
   selected_btn_id = Names(win_name, &selected_index,
         selected_str, sizeof(selected_str), &dirNamesInfo);
   if (selected_btn_id == BUTTON_OK) {
      *JustSetDir = FALSE;
      strcpy(SelStr, selected_str);
      if (FileIsRemote(selected_str)) {
         /* index != INVALID means SelStr is valid */
         return 0;
      }
      return selected_index;
   } else if (selected_btn_id == BUTTON_SETDIR) {
      if (DirIsRemote(selected_str)) {
         *SelStr = '\0';
         *JustSetDir = FALSE;
         sprintf(gszMsgBox, "SETDIR not supported for '%s'.", selected_str);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      } else {
         int len=strlen(selected_str);

         if (len > 0 && selected_str[len-1] == '/') {
            selected_str[--len] = '\0';
         }
         strcpy(SelStr, selected_str);
         *JustSetDir = TRUE;
      }
   }
   return INVALID;
}

static
int ChooseAName(TopStr, entries, num_entries, marked_index)
   char *TopStr, **entries;
   int num_entries, marked_index;
{
   char win_name[128];
   int selected_index=INVALID;

   ResetNamesInfo();
   NamesSetTitle(TopStr);
   NamesAddButton("OK", BUTTON_OK);
   NamesAddButton("CANCEL", BUTTON_CANCEL);
   NamesSetEntries(NULL, entries, num_entries, TRUE, marked_index, 0);
   NamesSetStyle(NAMES_COMPLEX_SELECT_NAME, NAMES_LOOP_ONCE);
   sprintf(win_name, "%s - %s", TOOL_NAME, TopStr);
   if (Names(win_name, &selected_index, NULL, 0, NULL) == BUTTON_OK) {
      return selected_index;
   }
   return INVALID;
}

int SelectFileName(MsgStr, SelStr)
   char *MsgStr, *SelStr;
{
   int index, saved_num_dir_entries, just_set_dir;
   DspList *dsp_ptr, *next_dsp;
   char saved_cur_dir[MAXPATHLENGTH];

   if (curDirIsLocal) {
      strcpy(saved_cur_dir, curDir);
   } else {
      strcpy(saved_cur_dir, curLocalDir);
   }
   saved_num_dir_entries = numDirEntries;
   if ((index=DirNames(MsgStr,OBJ_FILE_EXT,SelStr,&just_set_dir)) == INVALID) {
      if (just_set_dir) {
         strcpy(curDir, SelStr);
         BuildDirList();
         if (strcmp(saved_cur_dir, curDir) != 0 && DirInSymPath(".")) {
            UpdateSymInfo();
         }
         RedrawTitleWindow();
         sprintf(gszMsgBox, "%s '%s'.\n\n(%s '%s'.)",
               "Current open/save directory change to", curDir,
               "Current import directory remains", curImportDir);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         Msg("");
      } else {
         numDirEntries = saved_num_dir_entries;
         for (dsp_ptr=topOfDirLinkList; dsp_ptr != NULL; dsp_ptr=next_dsp) {
            next_dsp = dsp_ptr->next;
            free(dsp_ptr);
         }
         topOfDirLinkList = NULL;
      }
      *SelStr = '\0';
      return INVALID;
   }
   BuildDirList();
   Msg("");
   return index;
}

int SelectFileNameToPaste(MsgStr, SelStr)
   char *MsgStr, *SelStr;
{
   int index, saved_num_dir_entries, just_set_dir;
   DspList *dsp_ptr, *next_dsp;
   char saved_cur_dir[MAXPATHLENGTH];

   if (curDirIsLocal) {
      strcpy(saved_cur_dir, curDir);
   } else {
      strcpy(saved_cur_dir, curLocalDir);
   }
   saved_num_dir_entries = numDirEntries;
   if ((index=DirNames(MsgStr,NULL,SelStr,&just_set_dir)) == INVALID) {
      if (just_set_dir) {
         strcpy(curDir, SelStr);
         BuildDirList();
         if (strcmp(saved_cur_dir, curDir) != 0 && DirInSymPath(".")) {
            UpdateSymInfo();
         }
         RedrawTitleWindow();
         sprintf(gszMsgBox, "%s '%s'.\n\n(%s '%s'.)",
               "Current open/save directory change to", curDir,
               "Current import directory remains", curImportDir);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         Msg("");
      } else {
         numDirEntries = saved_num_dir_entries;
         for (dsp_ptr=topOfDirLinkList; dsp_ptr != NULL; dsp_ptr=next_dsp) {
            next_dsp = dsp_ptr->next;
            free(dsp_ptr);
         }
         topOfDirLinkList = NULL;
      }
      *SelStr = '\0';
      return INVALID;
   }
   BuildDirList();
   Msg("");
   return index;
}

int SelectFileNameToImport(MsgStr, ExtStr, SelStr)
   char *MsgStr, *ExtStr, *SelStr;
{
   int index, saved_num_dir_entries, just_set_dir;
   DspList *dsp_ptr, *next_dsp;

   saved_num_dir_entries = numDirEntries;
   if ((index=DirNames(MsgStr, ExtStr, SelStr, &just_set_dir)) == INVALID) {
      if (just_set_dir) {
         strcpy(curImportDir, SelStr);
         sprintf(gszMsgBox, "Importing directory changed to %s.",
               curImportDir);
         Msg(gszMsgBox);
         sprintf(gszMsgBox, "%s '%s'.\n\n(%s '%s'.)",
               "Current importing directory changed to", curImportDir,
               "Current open/save directory remains", curDir);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      }
      *SelStr = '\0';
   } else {
      Msg("");
   }
   numDirEntries = saved_num_dir_entries;
   for (dsp_ptr=topOfDirLinkList; dsp_ptr != NULL; dsp_ptr=next_dsp) {
      next_dsp = dsp_ptr->next;
      free(dsp_ptr);
   }
   topOfDirLinkList = NULL;

   return index;
}

int GetSymbolPath(SymName, PathName)
   char *SymName, *PathName;
{
   int i;

   if (symbolList == NULL) return FALSE;

   for (i = 0; i < numSymbols; i++) {
      if (strcmp (SymName, symbolList[i].itemstr) == 0) {
         strcpy(PathName, symbolList[i].pathstr);
         return TRUE;
      }
   }
   return FALSE;
}

int NameInCurDir(FileName)
   char *FileName;
{
   int i;

   if (dirList == NULL) return (FALSE);

   for (i = 0; i < numDirEntries; i++) {
      if (dirList[i].directory && strcmp(FileName, dirList[i].itemstr) == 0) {
         return TRUE;
      }
   }
   return FALSE;
}

int DirInSymPath(DirName)
   char *DirName;
{
   register int i;

   if (symPath == NULL) return FALSE;

   for (i = 0; i < symPathNumEntries; i++) {
      if (strcmp(DirName, symPath[i]) == 0) {
         return TRUE;
      }
   }
   return FALSE;
}

static char **tmpDomainName=NULL;

static
DspList *DomainListing(pn_num_entries)
   int *pn_num_entries;
{
   int i, seen_examples=FALSE;
   char s[MAXSTRING], *c_ptr;
   DspList *dsp_ptr, *head_ptr, *tail_ptr, *p, *p1, *next_dsp;

   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "MaxDomains")) == NULL) {
      *pn_num_entries = 1;
   } else {
      *pn_num_entries = atoi(c_ptr)+1;
   }
   tmpDomainName = (char**)malloc((*pn_num_entries)*sizeof(char*));
   if (tmpDomainName == NULL) FailAllocMessage();
   memset(tmpDomainName, 0, (*pn_num_entries)*sizeof(char*));

   head_ptr = tail_ptr = NULL;
   for (i = 0; i < (*pn_num_entries); i++) {
      if (i != (*pn_num_entries)-1) {
         char *c_ptr1;

         if (domainInResource) {
            sprintf(s, "DomainPath%1d", i);
            if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, s)) != NULL) {
               while (*c_ptr==' ' || *c_ptr=='\t' || *c_ptr=='\n') c_ptr++;
            }
         } else {
            sprintf(s, "Domain%1d", i);
            c_ptr = XGetDefault(mainDisplay, TOOL_NAME, s);
         }
         if (c_ptr == NULL || *c_ptr == '\0') {
            if (domainInResource) {
               sprintf(gszMsgBox, "%s:\n\nCannot find %s*DomainPath%1d.",
                     "Fail to generate a list of domain names",
                     TOOL_NAME, i);
            } else {
               sprintf(gszMsgBox, "%s:\n\nCannot find %s*Domain%1d.",
                     "Fail to generate a list of domain names",
                     TOOL_NAME, i);
            }
            MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
            for ( ; head_ptr != NULL; head_ptr = next_dsp) {
               next_dsp = head_ptr->next;
               free(head_ptr);
            }
            return (NULL);
         }
         dsp_ptr = (DspList*)malloc(sizeof(DspList));
         if (dsp_ptr == NULL) FailAllocMessage();
         memset(dsp_ptr, 0, sizeof(DspList));
         tmpDomainName[i] = (char*)malloc((strlen(c_ptr)+1)*sizeof(char));
         if (tmpDomainName[i] == NULL) FailAllocMessage();
         if ((c_ptr1=strchr(c_ptr, ':')) != NULL) {
            int	len=c_ptr1-c_ptr;

            strncpy(dsp_ptr->itemstr, c_ptr, len);
            strncpy(tmpDomainName[i], c_ptr, len);
            dsp_ptr->itemstr[len] = '\0';
            tmpDomainName[i][len] = '\0';
         } else {
            UtilStrCpy(dsp_ptr->itemstr, sizeof(dsp_ptr->itemstr), c_ptr);
            strcpy(tmpDomainName[i], c_ptr);
         }
         if (strcmp(dsp_ptr->itemstr, "Examples") == 0) {
            seen_examples = TRUE;
         }
      } else if (seen_examples) {
         (*pn_num_entries)--;
         break;
      } else {
         dsp_ptr = (DspList*)malloc(sizeof(DspList));
         if (dsp_ptr == NULL) FailAllocMessage();
         memset(dsp_ptr, 0, sizeof(DspList));
         UtilStrCpy(dsp_ptr->itemstr, sizeof(dsp_ptr->itemstr), "Examples");
         tmpDomainName[i] = (char*)malloc((strlen("Examples")+1)*sizeof(char));
         if (tmpDomainName[i] == NULL) FailAllocMessage();
         strcpy(tmpDomainName[i], "Examples");
      }

      if (head_ptr == NULL) {
         head_ptr = tail_ptr = dsp_ptr;
      } else {
         p1 = NULL;
         for (p = head_ptr; p != NULL; p = p->next) {
            if (LargerStr(dsp_ptr->itemstr, p->itemstr)) {
               p1 = p;
            } else {
               break;
            }
         }
         dsp_ptr->next = p;
         if (p == NULL) {  /* dsp_ptr has the largest element */
            tail_ptr->next = dsp_ptr;
            tail_ptr = dsp_ptr;
         } else if (p1 == NULL) {
            head_ptr = dsp_ptr;
         } else {
            p1->next = dsp_ptr;
         }
      }
   }
   return head_ptr;
}

int SelectDomain(SelStr)
   char *SelStr;
{
   int i, index, num_entries=0;
   char **entries=NULL;
   DspList *dsp_ptr, *next_dsp;

   Msg("Generating list of domain names, please wait ...");

   if ((dsp_ptr=DomainListing(&num_entries)) == NULL) {
      if (tmpDomainName != NULL) {
         for (i=0; i < num_entries; i++) {
            if (tmpDomainName[i] != NULL) {
               free(tmpDomainName[i]);
            }
         }
         free(tmpDomainName);
      }
      tmpDomainName = NULL;
      Msg("Fail to get a list of domain names.");
      *SelStr = '\0';
      return INVALID;
   }
   entries = MakeNameDspItemArray(num_entries, dsp_ptr);
   if ((index=ChooseAName("Please select a new DOMAIN ...", entries,
         num_entries, INVALID)) == INVALID) {
      *SelStr = '\0';
   } else {
      strcpy(SelStr, entries[index]);
   }
   for ( ; dsp_ptr != NULL; dsp_ptr = next_dsp) {
      next_dsp = dsp_ptr->next;
      free(dsp_ptr);
   }
   free(*entries);
   free(entries);

   Msg("");

   index = INVALID;
   if (*SelStr != '\0' && num_entries > 0 && tmpDomainName != NULL) {
      for (i=0; i < num_entries; i++) {
         if (tmpDomainName[i] != NULL && strcmp(tmpDomainName[i],SelStr)==0) {
            index = i;
            break;
         }
      }
   }
   if (tmpDomainName != NULL) {
      for (i=0; i < num_entries; i++) {
         if (tmpDomainName[i] != NULL) {
            free(tmpDomainName[i]);
         }
      }
      free(tmpDomainName);
      tmpDomainName = NULL;
   }
   return index;
}

static char	* oldDomain=NULL, * oldDir=NULL;

static
DspList *SymDirListing(pn_marked_index, pn_num_entries)
   int *pn_marked_index, *pn_num_entries;
{
   int i, checking=FALSE;
   DspList *dsp_ptr, *head_ptr, *tail_ptr;

   *pn_marked_index = INVALID;
   if (oldDomain!=NULL && oldDir!=NULL && strcmp(oldDomain,curDomainName)==0) {
      checking = TRUE;
   }
   head_ptr = tail_ptr = NULL;
   for (i = 0; i < symPathNumEntries; i++) {
      dsp_ptr = (DspList*)malloc(sizeof(DspList));
      if (dsp_ptr == NULL) FailAllocMessage();
      memset(dsp_ptr, 0, sizeof(DspList));
      dsp_ptr->next = NULL;
      UtilStrCpy(dsp_ptr->itemstr, sizeof(dsp_ptr->itemstr), symPath[i]);

      if (head_ptr == NULL) {
         head_ptr = tail_ptr = dsp_ptr;
      } else {
         tail_ptr->next = dsp_ptr;
         tail_ptr = dsp_ptr;
      }
      if (checking && strcmp(oldDir, symPath[i]) == 0) {
         checking = FALSE;
         *pn_marked_index = i;
      }
   }
   *pn_num_entries = symPathNumEntries;
   return head_ptr;
}

int SelectSymDir(SelStr)
   char *SelStr;
{
   int index, num_entries=0, marked_index=INVALID;
   char **entries=NULL, msg[MAXSTRING+1];
   DspList *dsp_ptr, *next_dsp;

   *SelStr = '\0';
   if ((dsp_ptr=SymDirListing(&num_entries, &marked_index)) == NULL) {
      return INVALID;
   }
   entries = MakeLongNameDspItemArray(num_entries, dsp_ptr);

   sprintf (msg, "Please select a DIRECTORY in the '%s' domain ...",
         curDomainName);
   if ((index=ChooseAName(msg, entries, num_entries, marked_index)) !=
         INVALID) {
      strcpy(SelStr, entries[index]);
      if (oldDomain != NULL) free(oldDomain);
      if (oldDir != NULL) free(oldDir);
      oldDomain = (char*)malloc((strlen(curDomainName)+1)*sizeof(char));
      if (oldDomain == NULL) FailAllocMessage();
      oldDir = (char*)malloc((strlen(SelStr)+1)*sizeof(char));
      if (oldDir == NULL) FailAllocMessage();
      strcpy(oldDomain, curDomainName);
      strcpy(oldDir, SelStr);
   }
   for ( ; dsp_ptr != NULL; dsp_ptr = next_dsp) {
      next_dsp = dsp_ptr->next;
      free(dsp_ptr);
   }
   free(*entries);
   free(entries);

   Msg ("");
   return index;
}

static
DspList *BitmapListing(ExtStr,OtherExtStr)
   char *ExtStr, *OtherExtStr;
{
   int len, path_index, count = 0, reject, ext_len;
   char path[MAXPATHLENGTH], ext_str[MAXSTRING];
   DspList *dsp_ptr, *head_ptr, *tail_ptr, *p, *p1;
   DIR *dirp;
   DIR_ENTRY *d;

   head_ptr = tail_ptr = NULL;

   sprintf(ext_str, ".%s", ExtStr);
   ext_len = strlen(ext_str);
   for (path_index=0; path_index < symPathNumEntries; path_index++) {
      strcpy(path, symPath[path_index]);
      if (strcmp(".", path) == 0) {
         if (curDirIsLocal) {
            strcpy(path, curDir);
         } else {
            strcpy(path, curLocalDir);
         }
      }
      if ((dirp=opendir(path)) == NULL) {
         sprintf(gszMsgBox, "%s '%s' %s.",
               "Cannot open the", path, "directory for reading");
         Msg(gszMsgBox);
         continue;
      }
      while ((d=readdir(dirp)) != NULL) {
         len = strlen(d->d_name);
         if (!((len > ext_len &&
               UtilStrICmp(ext_str,&d->d_name[len-ext_len]) == 0) ||
               ExtensionMatch(OtherExtStr, d->d_name))) {
            continue;
         }
         if (head_ptr == NULL) {
            head_ptr = tail_ptr = (DspList*)malloc(sizeof(DspList));
            if (head_ptr == NULL) FailAllocMessage();
            memset(head_ptr, 0, sizeof(DspList));
            UtilStrCpy(head_ptr->itemstr, sizeof(head_ptr->itemstr), d->d_name);
            UtilStrCpy(head_ptr->pathstr, sizeof(head_ptr->pathstr), path);
         } else {
            p1 = NULL;
            reject = FALSE;
            for (p=head_ptr; p != NULL; p = p->next) {
               if (strcmp(d->d_name, p->itemstr) == 0) {
                  reject = TRUE;
                  break;
               } else if (LargerStr(d->d_name, p->itemstr)) {
                  p1 = p;
               } else {
                  dsp_ptr = (DspList*)malloc(sizeof(DspList));
                  if (dsp_ptr == NULL) FailAllocMessage();
                  memset(dsp_ptr, 0, sizeof(DspList));
                  UtilStrCpy(dsp_ptr->itemstr, sizeof(dsp_ptr->itemstr),
                        d->d_name);
                  UtilStrCpy(dsp_ptr->pathstr, sizeof(dsp_ptr->pathstr), path);
                  break;
               }
            }
            if (reject) continue;
   
            dsp_ptr = (DspList*)malloc(sizeof(DspList));
            if (dsp_ptr == NULL) FailAllocMessage();
            memset(dsp_ptr, 0, sizeof(DspList));
            dsp_ptr->next = p;
            UtilStrCpy(dsp_ptr->itemstr, sizeof(dsp_ptr->itemstr), d->d_name);
            UtilStrCpy(dsp_ptr->pathstr, sizeof(dsp_ptr->pathstr), path);

            if (p == NULL) {
               /* dsp_ptr has the largest element */
               tail_ptr->next = dsp_ptr;
               tail_ptr = dsp_ptr;
            } else if (p1 == NULL) {
               head_ptr = dsp_ptr;
            } else {
               p1->next = dsp_ptr;
            }
         }
         count++;
      }
      closedir(dirp);
   }
   numSymbols = count;
   return head_ptr;
}

int SelectFromLibrary(MsgStr, ExtStr, SelStr, PathStr)
   char *MsgStr, *ExtStr, *SelStr, *PathStr;
{
   int index, num_entries=0;
   char **entries=NULL;
   char s[MAXPATHLENGTH], other_ext_str[MAXSTRING];
   DspList *dsp_ptr, *next_dsp;

   *other_ext_str = '\0';

   if (strcmp(ExtStr, XBM_FILE_EXT) == 0) {
      Msg("Generating list of bitmap names, please wait ...");
   } else if (strcmp(ExtStr, XPM_FILE_EXT) == 0) {
      Msg("Generating list of pixmap names, please wait ...");
   } else if (strcmp(ExtStr, OBJ_FILE_EXT) == 0) {
      Msg("Generating list of object file names, please wait ...");
      sprintf(other_ext_str, ".%s", SYM_FILE_EXT);
   } else if (strcmp(ExtStr, SYM_FILE_EXT) == 0) {
      Msg("Generating list of symbol names, please wait ...");
   } else if (strcmp(ExtStr, EPSF_FILE_EXT) == 0) {
      Msg("Generating list of EPS file names, please wait ...");
      sprintf(other_ext_str, ".%s;.epsi", PS_FILE_EXT);
   } else {
      sprintf(gszMsgBox, "Generating list of %s file names, please wait ...",
            ExtStr);
      Msg(gszMsgBox);
      sprintf(other_ext_str, ".GIF");
   }
   if ((topOfSymLinkList=BitmapListing(ExtStr,other_ext_str)) == NULL) {
      sprintf(gszMsgBox, "No '%s' files found.", ExtStr);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      *SelStr = *PathStr = '\0';
      return INVALID;
   }
   BuildSymbolList();

   num_entries = numSymbols;
   entries = MakeNameDspItemArray(num_entries, symbolList);

   if ((index=ChooseAName(MsgStr, entries, num_entries, INVALID)) == INVALID) {
      *SelStr = *PathStr = '\0';
   } else {
      strcpy(SelStr, entries[index]);
      strcpy(PathStr, symbolList[index].pathstr);
   }

   for (dsp_ptr=topOfSymLinkList; dsp_ptr != NULL; dsp_ptr=next_dsp) {
      next_dsp = dsp_ptr->next;
      free(dsp_ptr);
   }
   free(*entries);
   free(entries);

   Msg("");
   UpdateSymInfo();
   return index;
}

static
void ShrinkName(FileName)
   char *FileName;
{
   char *c_ptr, *real_ptr, real_name[MAXPATHLENGTH];

   if (*FileName != '/') return;

   strcpy(real_name, FileName);
   real_ptr = real_name;
   c_ptr = (&real_name[1]);

   while (*c_ptr != '\0') {
      if (*c_ptr == '.') {
         if (*(c_ptr+1) == '.') {
            if (*(c_ptr+2) == '/') {
               /* "../" */
               if (real_ptr != real_name) {
                  while (*(--real_ptr) != '/') ;
               }
               c_ptr += 3;
            } else {
               while (*c_ptr != '\0' && *c_ptr != '/') {
                  *(++real_ptr) = *c_ptr++;
               }
               if (*c_ptr == '/') {
                  c_ptr++;
                  *(++real_ptr) = '/';
               }
            }
         } else if (*(c_ptr+1) == '/') {
            /* "./" */
            c_ptr += 2;
         } else {
            while (*c_ptr != '\0' && *c_ptr != '/') {
               *(++real_ptr) = *c_ptr++;
            }
            if (*c_ptr == '/') {
               c_ptr++;
               *(++real_ptr) = '/';
            }
         }
#ifndef apollo
      } else if (*c_ptr == '/') {
         c_ptr++;
#endif
      } else {
         while (*c_ptr != '\0' && *c_ptr != '/') {
            *(++real_ptr) = *c_ptr++;
         }
         if (*c_ptr == '/') {
            c_ptr++;
            *(++real_ptr) = '/';
         }
      }
   }
   *(++real_ptr) = '\0';
   strcpy(FileName, real_name);
}

void SetCurDir(FileName)
   char *FileName;
{
   int i;
   char file_name[MAXPATHLENGTH+1];

   strcpy(file_name, FileName);
   ShrinkName(file_name);

   if (curDirIsLocal && FileIsRemote(FileName)) {
      strcpy(curLocalDir, curDir);
      if (autoHyperSpaceOnRemote) {
         inHyperSpace = TRUE;
         Msg("Entering hyperspace...");
      }
   } else if (!curDirIsLocal && !FileIsRemote(FileName)) {
      *curLocalDir = '\0';
   }
   for (i=strlen(file_name)-1; i>=0 && file_name[i]!='/'; i--) ;

   if (i < 0) {
      TwoLineMsg("Error:  No '/' found in SetCurDir().",
                  "        curDir set to '.'.");
      strcpy(curDir, ".");
      strcpy(curFileName, FileName);
   } else {
      strcpy(curFileName, &file_name[i+1]);
      file_name[i] = '\0';
      strcpy(curDir, file_name);
   }
   curDirIsLocal = (!DirIsRemote(curDir));
   RedrawDummyWindow1();
}

void SetCurSymDir(FileName)
   char *FileName;
{
   int i;
   char file_name[MAXPATHLENGTH+1];

   strcpy(file_name, FileName);
   ShrinkName(file_name);

   for (i = strlen(file_name)-1; i>=0 && file_name[i]!='/'; i--) ;

   if (i < 0) {
      TwoLineMsg("Error:  No '/' found in SetCurSymDir().",
                  "        curSymDir set to '.'.");
      strcpy(curSymDir, ".");
      strcpy(curFileName, FileName);
   } else {
      strcpy(curFileName, &file_name[i+1]);
      file_name[i] = '\0';
      strcpy(curSymDir, file_name);
   }
}

void SetCurImportDir(FileName)
   char *FileName;
{
   int i;
   char file_name[MAXPATHLENGTH+1];

   strcpy(file_name, FileName);
   ShrinkName(file_name);

   for (i = strlen(file_name)-1; i>=0 && file_name[i]!='/'; i--) ;

   if (i < 0) {
      TwoLineMsg("Error:  No '/' found in SetCurImportDir().",
                  "        curImportDir set to '.'.");
      strcpy(curImportDir, ".");
   } else {
      file_name[i] = '\0';
      strcpy(curImportDir, file_name);
   }
}
