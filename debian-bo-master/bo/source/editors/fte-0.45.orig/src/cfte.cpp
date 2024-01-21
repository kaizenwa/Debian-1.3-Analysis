/*    cfte.cpp
 *
 *    Copyright (c) 1994-1996, Marko Macek
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <stdarg.h>
#include <ctype.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "ftever.h"
#include "sysdep.h"
#include "feature.h"
#include "c_fconfig.h"
#include "s_files.h"
#include "c_mode.h"

typedef struct {
    char *Name;
} ExMacro;

int CMacros = 0;
ExMacro *Macros = 0;

FILE *output = 0;
int lntotal = 0;
long offset = -1;
long pos = 0;
char XTarget[256] = "";


#include "c_commands.h"
#include "c_cmdtab.h"

typedef struct _CurPos {
    int sz;
    char *a;
    char *c;
    char *z;
    int line;
    char *name; // filename
} CurPos;

void cleanup(int xerrno) {
    fclose(output);
    if (XTarget[0] != 0)
        unlink(XTarget);
    exit (xerrno);
}

void Fail(CurPos &cp, char *s, ...) {
    va_list ap;
    char msgbuf[1024];
    
    va_start(ap, s);
    vsprintf(msgbuf, s, ap);
    va_end(ap);
    
    fprintf(stderr, "%s:%d: Error: %s\n", cp.name, cp.line, msgbuf);
    cleanup(1);
}

int LoadFile(char *WhereName, char *CfgName, int Level);
 
int main(int argc, char **argv) {
    char Cur[MAXPATH];
    char Source[MAXPATH];
    char Target[MAXPATH];
    char *p = argv[1];
    long null = 0;
    int n = 1;
    
    fprintf(stderr, PROG_CFTE " " VERSION " " COPYRIGHT "\n");
    if (argc < 2 || argc > 4) {
        fprintf(stderr, "Usage: " PROG_CFTE " [-o<offset>] config/main.fte [fte-new.cnf]\n");
        exit(1);
    }
    if (strncmp(p, "-o", 2) == 0) {
        p += 2;
        offset = atol(p);
        n++;
    }
    if (n == 1 && argc == 4) {
        fprintf(stderr, "Invalid option '%s'\n", argv[1]);
        exit(1);
    }
    strcpy(Source, argv[n++]);
    strcpy(Target, "fte-new.cnf");
    if (n < argc)
        strcpy(Target, argv[n++]);

    JustDirectory(Target, XTarget);
    Slash(XTarget, 1);
    sprintf(XTarget + strlen(XTarget), "cfte%ld.tmp", (long)getpid());
    output = fopen(XTarget, "wb");
    if (output == 0) {
        fprintf(stderr, "Cannot create '%s', errno=%d!", XTarget, errno);
        cleanup(1);
    }
    
    if (fwrite(&null, sizeof(long), 1, output) != 1) {
        fprintf(stderr, "Disk full!");
        cleanup(1);
    }
    null = VERNUM;
    if (fwrite(&null, sizeof(long), 1, output) != 1) {
        fprintf(stderr, "Disk full!");
        cleanup(1);
    }
    pos = 2 * sizeof(long);
    
    fprintf(stderr, "Compiling to '%s'\n", Target);
    ExpandPath(".", Cur);
    Slash(Cur, 1);
    if (LoadFile(Cur, Source, 0) != 0) {
        fprintf(stderr, "\nCompile failed\n");
        cleanup(1);
    }
    
    null = CONFIG_ID;
    fseek(output, 0, SEEK_SET);
    fwrite(&null, sizeof(long), 1, output);
    fclose(output);

    if (unlink(Target) != 0 && errno != ENOENT) {
        fprintf(stderr, "Remove of '%s' failed, result left in %s, errno=%d\n",
                Target, XTarget, errno);
        exit(1);
    }
        
    if (rename(XTarget, Target) != 0) {
        fprintf(stderr, "Rename of '%s' to '%s' failed, errno=%d\n",
                XTarget, Target, errno);
        exit(1);
    }
    
    fprintf(stderr, "\nDone.\n");
    return 0;
}

#define MODE_BFI(x) { #x, BFI_##x }
#define MODE_BFS(x) { #x, BFS_##x }
#define MODE_FLG(x) { #x, FLAG_##x }
#define EVENT_FLG(x) { #x, EM_##x }
#define COLORIZE_FLG(x) { #x, COL_##x }
#define HILIT_CLR(x) { #x, CLR_##x }

typedef struct _OrdLookup {
    char *Name;
    int num;
} OrdLookup;

OrdLookup mode_num[] = {
MODE_BFI(AutoIndent),
MODE_BFI(Insert),
MODE_BFI(DrawOn),
MODE_BFI(HilitOn),
MODE_BFI(ExpandTabs),
MODE_BFI(Trim),
MODE_BFI(TabSize),
MODE_BFI(ShowTabs),
MODE_BFI(LineChar),
MODE_BFI(StripChar),
MODE_BFI(AddLF),
MODE_BFI(AddCR),
MODE_BFI(ForceNewLine),
MODE_BFI(HardMode),
MODE_BFI(Undo),
MODE_BFI(ReadOnly),
MODE_BFI(AutoSave),
MODE_BFI(KeepBackups),
MODE_BFI(LoadMargin),
MODE_BFI(UndoLimit),
MODE_BFI(MatchCase),
MODE_BFI(BackSpKillTab),
MODE_BFI(DeleteKillTab),
MODE_BFI(BackSpUnindents),
MODE_BFI(SpaceTabs),
MODE_BFI(IndentWithTabs),
MODE_BFI(LeftMargin),
MODE_BFI(RightMargin),
MODE_BFI(SeeThruSel),
MODE_BFI(WordWrap),
MODE_BFI(ShowMarkers),
MODE_BFI(CursorThroughTabs),
MODE_BFI(SaveFolds),
MODE_BFI(MultiLineHilit),
MODE_BFI(AutoHilitParen),
MODE_BFI(Abbreviations),
MODE_BFI(BackSpKillBlock),
MODE_BFI(DeleteKillBlock),
MODE_BFI(PersistentBlocks),
MODE_BFI(InsertKillBlock),
MODE_BFI(UndoMoves),
{ 0, 0 },
};

OrdLookup mode_string[] = {
MODE_BFI(Colorizer),
MODE_BFI(IndentMode),
MODE_BFS(RoutineRegexp),
MODE_BFS(DefFindOpt),
MODE_BFS(DefFindReplaceOpt),
MODE_BFS(CommentStart),
MODE_BFS(CommentEnd),
MODE_BFS(WordChars),
MODE_BFS(FileNameRx),
MODE_BFS(FirstLineRx),
MODE_BFI(EventMap),
{ 0, 0 },
};

OrdLookup global_num[] = {
#ifdef CONFIG_INDENT_C
MODE_FLG(C_Indent),
MODE_FLG(C_BraceOfs),
MODE_FLG(C_CaseOfs),
MODE_FLG(C_CaseDelta),
MODE_FLG(C_ClassOfs),
MODE_FLG(C_ClassDelta),
MODE_FLG(C_ColonOfs),
MODE_FLG(C_CommentOfs),
MODE_FLG(C_CommentDelta),
#endif
#ifdef CONFIG_INDENT_REXX
MODE_FLG(REXX_Indent),
MODE_FLG(REXX_Do_Offset),
#endif
MODE_FLG(ScreenSizeX),
MODE_FLG(ScreenSizeY),
MODE_FLG(CursorInsertStart),
MODE_FLG(CursorInsertEnd),
MODE_FLG(CursorOverStart),
MODE_FLG(CursorOverEnd),
MODE_FLG(SysClipboard),
MODE_FLG(OpenAfterClose),
MODE_FLG(ShowVScroll),
MODE_FLG(ShowHScroll),
MODE_FLG(ScrollBarWidth),
MODE_FLG(SelectPathname),
MODE_FLG(ShowToolBar),
MODE_FLG(ShowMenuBar),
MODE_FLG(KeepHistory),
MODE_FLG(LoadDesktopOnEntry),
MODE_FLG(SaveDesktopOnExit),
MODE_FLG(KeepMessages),
MODE_FLG(ScrollBorderX),
MODE_FLG(ScrollBorderY),
MODE_FLG(ScrollJumpX),
MODE_FLG(ScrollJumpY),
MODE_FLG(GUIDialogs),
MODE_FLG(PMDisableAccel),
MODE_FLG(SevenBit),
MODE_FLG(WeirdScroll),
MODE_FLG(LoadDesktopMode),
{ 0, 0 },
};

OrdLookup global_string[] = {
MODE_FLG(DefaultModeName),
MODE_FLG(CompletionFilter),
MODE_FLG(PrintDevice),
MODE_FLG(CompileCommand),
MODE_FLG(WindowFont),
{ 0, 0 },
};

OrdLookup event_string[] = {
EVENT_FLG(MainMenu),
EVENT_FLG(LocalMenu),
{ 0, 0 },
};

OrdLookup colorize_string[] = {
COLORIZE_FLG(SyntaxParser),
{ 0, 0 },
};

OrdLookup hilit_colors[] = {
HILIT_CLR(Normal),
HILIT_CLR(Keyword),
HILIT_CLR(String),
HILIT_CLR(Comment),
HILIT_CLR(CPreprocessor),
HILIT_CLR(Regexp),
HILIT_CLR(Header),
HILIT_CLR(Quotes),
HILIT_CLR(Number),
HILIT_CLR(HexNumber),
HILIT_CLR(OctalNumber),
HILIT_CLR(FloatNumber),
HILIT_CLR(Function),
HILIT_CLR(Command),
HILIT_CLR(Tag),
HILIT_CLR(Punctuation),
HILIT_CLR(New),
HILIT_CLR(Old),
HILIT_CLR(Changed),
HILIT_CLR(Control),
HILIT_CLR(Separator),
HILIT_CLR(Variable),
HILIT_CLR(Symbol),
HILIT_CLR(Directive),
HILIT_CLR(Label),
HILIT_CLR(Special),
HILIT_CLR(QuoteDelim),
HILIT_CLR(RegexpDelim),
{ 0, 0 },
};

int Lookup(OrdLookup *where, char *what) {
    int i;
    
    for (i = 0; where[i].Name != 0; i++) {
        if (strcmp(what, where[i].Name) == 0)
            return where[i].num;
    }
//    fprintf(stderr, "\nBad name: %s (i = %d)\n", what, i);
    return -1;
}

#define slen(s) ((s) ? (strlen(s) + 1) : 0)

void PutObj(CurPos &cp, int xtag, int xlen, void *obj) {
    unsigned char tag = (unsigned char)xtag;
    unsigned short len = (unsigned short)xlen;
    
    if (fwrite(&tag, sizeof(unsigned char), 1, output) != 1 ||
        fwrite(&len, sizeof(unsigned short), 1, output) != 1 ||
        fwrite(obj, 1, len, output) != len)
    {
        Fail(cp, "Disk full!");
    }
    pos += 3 + len;
    if (offset != -1 && pos >= offset) {
        Fail(cp, "Error location found at %ld", pos);
    }
}

int LoadFile(char *WhereName, char *CfgName, int Level = 1);

#define P_EOF         0  // end of file
#define P_SYNTAX      1  // unknown
#define P_WORD        2  // a-zA-Z_
#define P_NUMBER      3  // 0-9
#define P_STRING      4  // "'`
#define P_ASSIGN      5  // =
#define P_EOS         6  // ;
#define P_KEYSPEC     7  // []
#define P_OPENBRACE   8  // {
#define P_CLOSEBRACE  9  // }
#define P_COLON      10  // :
#define P_COMMA      11  // ,
#define P_QUEST      12

#define K_UNKNOWN     0
#define K_MODE        1
#define K_KEY         2
#define K_COLOR       3
#define K_KEYWORD     4
#define K_OBJECT      5
#define K_MENU        6
#define K_ITEM        7
#define K_SUBMENU     8
#define K_COMPILERX   9
#define K_EXTERN     10
#define K_INCLUDE    11
#define K_SUB        12
#define K_EVENTMAP   13
#define K_COLORIZE   14
#define K_ABBREV     15
#define K_HSTATE     16
#define K_HTRANS     17
#define K_HWORDS     18
#define K_SUBMENUCOND 19
#define K_HWTYPE     20

typedef char Word[64];

OrdLookup CfgKW[] = {
{ "mode", K_MODE },
{ "eventmap", K_EVENTMAP },
{ "key", K_KEY },
{ "color", K_COLOR },
{ "keyword", K_KEYWORD },
{ "object", K_OBJECT },
{ "menu", K_MENU },
{ "item", K_ITEM },
{ "submenu", K_SUBMENU },
{ "CompileRx", K_COMPILERX },
{ "extern", K_EXTERN },
{ "include", K_INCLUDE },
{ "sub", K_SUB },
{ "colorize", K_COLORIZE },
{ "abbrev", K_ABBREV },
{ "h_state", K_HSTATE },
{ "h_trans", K_HTRANS },
{ "h_words", K_HWORDS },
{ "h_wtype", K_HWTYPE },
{ "submenucond", K_SUBMENUCOND },
{ 0, 0 },
};

int Parse(CurPos &cp) {
    while (cp.c < cp.z) {
        switch (*cp.c) {
#ifndef UNIX
        case '\x1A': /* ^Z :-* */ return P_EOF;
#endif
        case '#':
            while (cp.c < cp.z && *cp.c != '\n') cp.c++;
            break;
	case '\n':
            cp.line++;
            lntotal++;
	case ' ': 
	case '\t': 
	case '\r': 
	    cp.c++;
	    break;
        case '=': return P_ASSIGN;
        case ';': return P_EOS;
	case ',': return P_COMMA;
	case ':': return P_COLON;
        case '\'': 
        case '"': 
        case '`': 
        case '/': return P_STRING;
        case '[': return P_KEYSPEC;
        case '{': return P_OPENBRACE;
        case '}': return P_CLOSEBRACE;
        case '?': return P_QUEST;
        case '-': case '+':
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9': return P_NUMBER;
        default:
            if ((*cp.c >= 'a' && *cp.c <= 'z') ||
                (*cp.c >= 'A' && *cp.c <= 'Z') ||
                (*cp.c == '_'))
                return P_WORD;
            else
                return P_SYNTAX;
        }
    }
    return P_EOF;
}

int GetWord(CurPos &cp, char *w) {
    char *p = w;
    int len = 0;
    
    while (len < int(sizeof(Word)) && cp.c < cp.z &&
           ((*cp.c >= 'a' && *cp.c <= 'z') ||
            (*cp.c >= 'A' && *cp.c <= 'Z') ||
            (*cp.c >= '0' && *cp.c <= '9') ||
            (*cp.c == '_')))
    {
        *p++ = *cp.c++;
        len++;
    }
    if (len == sizeof(Word)) return -1;
    *p = 0;
    return 0;
}

void GetOp(CurPos &cp, int what) {
    switch (what) {
    case P_COMMA:
    case P_OPENBRACE:
    case P_CLOSEBRACE:
    case P_ASSIGN:
    case P_EOS:
    case P_COLON:
    case P_QUEST:
        cp.c++;
        break;
    }
}

char *GetString(CurPos &cp) {
    char c = *cp.c;
    char *p = cp.c;
    char *d = cp.c;
    
    if (c == '[') c = ']';
    
    cp.c++; // skip '"`
    while (cp.c < cp.z) {
        if (*cp.c == '\\') {
            if (c == '/')
                *p++ = *cp.c;
            cp.c++;
	    if (cp.c == cp.z) return 0;
            if (c == '"') {
                switch (*cp.c) {
                case 't': *cp.c = '\t'; break;
                case 'r': *cp.c = '\r'; break;
                case 'n': *cp.c = '\n'; break;
                case 'b': *cp.c = '\b'; break;
                case 'v': *cp.c = '\v'; break;
                case 'a': *cp.c = '\a'; break;
                }
            }
	} else if (*cp.c == c) {
	    cp.c++;
	    *p = 0;
	    return d;
	} else if (*cp.c == '\n') return 0;
	if (*cp.c == '\n') cp.line++;
	if (*cp.c == '\r') {
	    cp.c++;
	    if (cp.c == cp.z) return 0;
	}
	*p++ = *cp.c++;
    }
    return 0;
}

int GetNumber(CurPos &cp) {
    int value = 0;
    int neg = 0;
    
    if (cp.c < cp.z && *cp.c == '-' || *cp.c == '+') {
	if (*cp.c == '-') neg = 1;
	cp.c++;
    }
    while (cp.c < cp.z && (*cp.c >= '0' && *cp.c <= '9')) {
	value = value * 10 + (*cp.c - '0');
	cp.c++;
    }
    return neg ? -value : value;
}

int CmdNum(char *Cmd) {
    int i;
    
    for (i = 0; 
	 i < int(sizeof(Command_Table) / sizeof(Command_Table[0]));
	 i++)
	if (strcmp(Cmd, Command_Table[i].Name) == 0)
	    return Command_Table[i].CmdId;
    for (i = 0; i < CMacros; i++)
        if (Macros[i].Name && (strcmp(Cmd, Macros[i].Name)) == 0)
            return i | CMD_EXT;
    return 0; // Nop
}

int NewCommand(char *Name) {
    if (Name == 0)
        Name = "";
    Macros = (ExMacro *) realloc(Macros, sizeof(ExMacro) * (1 + CMacros));
    Macros[CMacros].Name = strdup(Name);
    CMacros++;
    return CMacros - 1;
}

int ParseCommands(CurPos &cp, char *Name) {
    Word cmd;
    int p;
    long Cmd = NewCommand(Name) | CMD_EXT;
    
    long cnt;
    long ign = 0;
    
    PutObj(cp, CF_INT, sizeof(long), &Cmd);
    GetOp(cp, P_OPENBRACE);
    cnt = 1;
    while (1) {
        p = Parse(cp);
        if (p == P_CLOSEBRACE) break;
        if (p == P_EOF) Fail(cp, "Unexpected EOF");
        
        if (p == P_NUMBER) {
            long num = GetNumber(cp);
            if (Parse(cp) != P_COLON) {
                PutObj(cp, CF_INT, sizeof(long), &num);
            } else {
                cnt = num;
                GetOp(cp, P_COLON);
            }
        } else if (p == P_WORD) {
            long Command;
            
            if (GetWord(cp, cmd) == -1) Fail(cp, "Syntax error");
            Command = CmdNum(cmd);
            if (Command == 0)
                Fail(cp, "Unrecognised command: %s", cmd);
            PutObj(cp, CF_COMMAND, sizeof(long), &Command);
            PutObj(cp, CF_INT, sizeof(long), &cnt);
            PutObj(cp, CF_INT, sizeof(long), &ign);
            ign = 0;
            cnt = 1;
        } else if (p == P_STRING) {
            char *s = GetString(cp);
            PutObj(cp, CF_STRING, slen(s), s);
        } else if (p == P_QUEST) {
            ign = 1;
            GetOp(cp, P_QUEST);
        } else if (p == P_EOS) {
            GetOp(cp, P_EOS);
            cnt = 1;
        } else
            Fail(cp, "Syntax error");
    }
    GetOp(cp, P_CLOSEBRACE);
    return 0;
}

int ParseConfigFile(CurPos &cp, char *filename) {
    Word w = "";
    char *s = 0;
    int p = 0;
    
    Word ObjName = "", UpMode = "";
    
    while (1) {
        p = Parse(cp);
        
        switch (p) {
        case P_WORD:
            if (GetWord(cp, w) != 0) Fail(cp, "Syntax error");
            switch (Lookup(CfgKW, w)) {
            case K_SUB:
                {
                    Word Name;
                    
                    if (Parse(cp) != P_WORD) Fail(cp, "Syntax error");
                    if (GetWord(cp, Name) != 0) Fail(cp, "Syntax error");
                    PutObj(cp, CF_SUB, slen(Name), Name);
                    if (Parse(cp) != P_OPENBRACE) Fail(cp, "'{' expected");
                    GetOp(cp, P_OPENBRACE);
                    if (ParseCommands(cp, strdup(Name)) == -1) 
                        Fail(cp, "Parse failed");
                    PutObj(cp, CF_END, 0, 0);
                }
                break;
            case K_MENU:
                    
                {
                    Word MenuName;
                    //int menu = -1, item = -1;
                    
                    if (Parse(cp) != P_WORD) Fail(cp, "Syntax error");;
		    if (GetWord(cp, MenuName) != 0) Fail(cp, "Syntax error");;
                    if (Parse(cp) != P_OPENBRACE) Fail(cp, "'{' expected");
                    GetOp(cp, P_OPENBRACE);
                    
                    PutObj(cp, CF_MENU, slen(MenuName), MenuName);
                    
                    while (1) {
                        p = Parse(cp);
                        if (p == P_CLOSEBRACE) break;
                        if (p == P_EOF) Fail(cp, "Unexpected EOF");
                        if (p != P_WORD) Fail(cp, "Syntax error");
                        
                        if (GetWord(cp, w) != 0) Fail(cp, "Parse failed");
                        switch (Lookup(CfgKW, w)) {
                        case K_ITEM: // menu::item
                            switch (Parse(cp)) {
                            case P_EOS:
                                PutObj(cp, CF_ITEM, 0, 0);
                                break;
                            case P_STRING:
                                s = GetString(cp);
                                PutObj(cp, CF_ITEM, slen(s), s);
                                break;
                            default:
                                Fail(cp, "Syntax error");;
                            }
                            if (Parse(cp) == P_EOS) {
                                GetOp(cp, P_EOS);
                                break;
                            }
                            if (Parse(cp) != P_OPENBRACE) 
                                Fail(cp, "'{' expected");
                            
                            PutObj(cp, CF_MENUSUB, 0, 0);
                            if (ParseCommands(cp, 0) == -1) 
                                Fail(cp, "Parse failed");
                            PutObj(cp, CF_END, 0, 0);
                            break;
                        case K_SUBMENU: // menu::submenu
                            if (Parse(cp) != P_STRING) 
                                Fail(cp, "String expected");
                            s = GetString(cp);
                            if (Parse(cp) != P_COMMA) 
                                Fail(cp, "',' expected");
                            GetOp(cp, P_COMMA);
                            if (Parse(cp) != P_WORD) 
                                Fail(cp, "Syntax error");
                            if (GetWord(cp, w) == -1) 
                                Fail(cp, "Parse failed");
                            
                            PutObj(cp, CF_SUBMENU, slen(s), s);
                            PutObj(cp, CF_STRING, slen(w), w);
                            if (Parse(cp) != P_EOS) 
                                Fail(cp, "';' expected");
			    GetOp(cp, P_EOS);
                            break;

                        case K_SUBMENUCOND: // menu::submenu
                            if (Parse(cp) != P_STRING) 
                                Fail(cp, "String expected");
                            s = GetString(cp);
                            if (Parse(cp) != P_COMMA) 
                                Fail(cp, "',' expected");
                            GetOp(cp, P_COMMA);
                            if (Parse(cp) != P_WORD) 
                                Fail(cp, "Syntax error");
                            if (GetWord(cp, w) == -1) 
                                Fail(cp, "Parse failed");
                            
                            PutObj(cp, CF_SUBMENUCOND, slen(s), s);
                            PutObj(cp, CF_STRING, slen(w), w);
                            if (Parse(cp) != P_EOS) 
                                Fail(cp, "';' expected");
                            GetOp(cp, P_EOS);
                            break;
                        default:  // menu::
                            Fail(cp, "Syntax error");
                        }
                    }
                    GetOp(cp, P_CLOSEBRACE);
                    PutObj(cp, CF_END, 0, 0);
                }
                break;
            case K_EVENTMAP:
                {
                    if (Parse(cp) != P_WORD) Fail(cp, "Syntax error");
                    if (GetWord(cp, ObjName) != 0) Fail(cp, "Parse failed");
                    PutObj(cp, CF_EVENTMAP, slen(ObjName), ObjName);

                    UpMode[0] = 0;
		    if (Parse(cp) == P_COLON) {
			GetOp(cp, P_COLON);
			if (Parse(cp) != P_WORD) Fail(cp, "Syntax error");
                        if (GetWord(cp, UpMode) != 0) Fail(cp, "Parse failed");
		    }
                    PutObj(cp, CF_PARENT, slen(UpMode), UpMode);
                    if (Parse(cp) != P_OPENBRACE) Fail(cp, "'{' expected");
                    GetOp(cp, P_OPENBRACE);
                    
                    while (1) {
                        p = Parse(cp);
                        if (p == P_CLOSEBRACE) break;
                        if (p == P_EOF) Fail(cp, "Unexpected EOF");
                        if (p != P_WORD) Fail(cp, "Syntax error");
                        
                        if (GetWord(cp, w) != 0) Fail(cp, "Parse failed");
                        switch (Lookup(CfgKW, w)) {
			case K_KEY: // mode::key
			    if (Parse(cp) != P_KEYSPEC) Fail(cp, "'[' expected");
                            s = GetString(cp);
                            PutObj(cp, CF_KEY, slen(s), s);
                            if (Parse(cp) != P_OPENBRACE) Fail(cp, "'{' expected");
                            PutObj(cp, CF_KEYSUB, 0, 0);
                            if (ParseCommands(cp, 0) == -1) Fail(cp, "Parse failed");
                            PutObj(cp, CF_END, 0, 0);
                            break;
                        
                        case K_ABBREV:
                            if (Parse(cp) != P_STRING) Fail(cp, "String expected");
                            s = GetString(cp);
                            PutObj(cp, CF_ABBREV, slen(s), s);
                            switch (Parse(cp)) {
                            case P_OPENBRACE:
                                PutObj(cp, CF_KEYSUB, 0, 0);
                                if (ParseCommands(cp, 0) == -1) Fail(cp, "Parse failed");
                                PutObj(cp, CF_END, 0, 0);
                                break;
                            case P_STRING:
                                s = GetString(cp);
                                PutObj(cp, CF_STRING, slen(s), s);
                                if (Parse(cp) != P_EOS) Fail(cp, "';' expected");
                                GetOp(cp, P_EOS);
                                break;
                            default:
                                Fail(cp, "Syntax error");
                            }
                            break;

			default:  // mode::
			    if (Parse(cp) != P_ASSIGN) Fail(cp, "'=' expected");
			    GetOp(cp, P_ASSIGN);
			    
			    switch (Parse(cp)) {
                                /*			    case P_NUMBER:
                                 {
                                 long var;
                                 long num;
                                 
                                 num = GetNumber(cp);
                                 var = LookupEventNumber(w);
                                 if (var == -1) return -1;
                                 PutObj(cp, CF_SETVAR, sizeof(long), &var);
                                 PutObj(cp, CF_INT, sizeof(long), &num);
                                 }
                                 break;*/
                            case P_STRING:
				{
                                    long var;
                                    
				    s = GetString(cp);
                                    if (s == 0) Fail(cp, "String expected");
                                    var = Lookup(event_string, w);
                                    if (var == -1) Fail(cp, "Lookup of '%s' failed", w);
                                    PutObj(cp, CF_SETVAR, sizeof(long), &var);
                                    PutObj(cp, CF_STRING, slen(s), s);
				}
				break;
			    default:
				return -1;
			    }
			    if (Parse(cp) != P_EOS) return -1;
			    GetOp(cp, P_EOS);
                            break;
                        }
                    }
                    GetOp(cp, P_CLOSEBRACE);
                    PutObj(cp, CF_END, 0, 0);
                }
                break;
                
            case K_COLORIZE:
                {
                    long LastState = -1;
                    
                    if (Parse(cp) != P_WORD) Fail(cp, "Syntax error");
                    if (GetWord(cp, ObjName) != 0) Fail(cp, "Parse failed");
                    PutObj(cp, CF_COLORIZE, slen(ObjName), ObjName);

                    UpMode[0] = 0;
		    if (Parse(cp) == P_COLON) {
			GetOp(cp, P_COLON);
			if (Parse(cp) != P_WORD) Fail(cp, "Syntax error");
                        if (GetWord(cp, UpMode) != 0) Fail(cp, "Parse failed");
		    }
                    PutObj(cp, CF_PARENT, slen(UpMode), UpMode);
                    if (Parse(cp) != P_OPENBRACE) Fail(cp, "'{' expected");
                    GetOp(cp, P_OPENBRACE);
                    
                    while (1) {
                        p = Parse(cp);
                        if (p == P_CLOSEBRACE) break;
                        if (p == P_EOF) Fail(cp, "Unexpected EOF");
                        if (p != P_WORD) Fail(cp, "Syntax error");
                        
                        if (GetWord(cp, w) != 0) Fail(cp, "Parse failed");
                        switch (Lookup(CfgKW, w)) {
                        case K_COLOR: // mode::color
                            if (Parse(cp) != P_OPENBRACE) Fail(cp, "'{' expected");
			    GetOp(cp, P_OPENBRACE);
                            PutObj(cp, CF_COLOR, 0, 0);
                                
			    while (1) {
                                char *sname, *svalue;
                                long cidx;
                                
				if (Parse(cp) == P_CLOSEBRACE) break;
                                if (Parse(cp) != P_OPENBRACE) Fail(cp, "'{' expected");
				GetOp(cp, P_OPENBRACE);
				if (Parse(cp) != P_STRING) Fail(cp, "String expected");
                                sname = GetString(cp);
                                if ((cidx = Lookup(hilit_colors, sname)) == -1)
                                    Fail(cp, "Lookup of '%s' failed", sname);
                                PutObj(cp, CF_INT, sizeof(long), &cidx);
                                if (Parse(cp) != P_COMMA) 
                                    Fail(cp, "',' expected");
				GetOp(cp, P_COMMA);
				if (Parse(cp) != P_STRING) Fail(cp, "String expected");
                                svalue = GetString(cp);
                                PutObj(cp, CF_STRING, slen(svalue), svalue);
                                if (Parse(cp) != P_CLOSEBRACE) Fail(cp, "'}' expected");
				GetOp(cp, P_CLOSEBRACE);
				if (Parse(cp) != P_COMMA)
				    break;
				else
				    GetOp(cp, P_COMMA);
			    }
                            if (Parse(cp) != P_CLOSEBRACE) Fail(cp, "'}' expected");
			    GetOp(cp, P_CLOSEBRACE);
			    if (Parse(cp) != P_EOS) Fail(cp, "';' expected");
                            GetOp(cp, P_EOS);
                            PutObj(cp, CF_END, 0, 0);
			    break;
                          
                        case K_KEYWORD: // mode::keyword
			    {
				char *colorstr, *kname;
				//int color;
				
				if (Parse(cp) != P_STRING) Fail(cp, "String expected");
				colorstr = GetString(cp);
                                if (Parse(cp) != P_OPENBRACE) Fail(cp, "'{' expected");
                                GetOp(cp, P_OPENBRACE);
                                
                                PutObj(cp, CF_KEYWORD, slen(colorstr), colorstr);

				while (1) {
				    if (Parse(cp) == P_CLOSEBRACE) break;
				    if (Parse(cp) != P_STRING) Fail(cp, "String expected");
                                    kname = GetString(cp);
                                    PutObj(cp, CF_STRING, slen(kname), kname);
				    
				    if (Parse(cp) != P_COMMA)
					break;
				    else
					GetOp(cp, P_COMMA);
                                }
			    }
                            if (Parse(cp) != P_CLOSEBRACE) Fail(cp, "'}' expected");
                            GetOp(cp, P_CLOSEBRACE);
			    if (Parse(cp) != P_EOS) Fail(cp, "';' expected");
                            GetOp(cp, P_EOS);
                            PutObj(cp, CF_END, 0, 0);
                            break;

                        case K_HSTATE:
                            {
                                long stateno;
                                char *cname;
                                long cidx;

                                if (Parse(cp) != P_NUMBER) Fail(cp, "state index expected");
                                stateno = GetNumber(cp);
                                if (stateno != LastState + 1) Fail(cp, "invalid state index");

                                if (Parse(cp) != P_OPENBRACE) Fail(cp, "'{' expected");
                                GetOp(cp, P_OPENBRACE);
                                PutObj(cp, CF_HSTATE, sizeof(long), &stateno);

                                if (Parse(cp) != P_STRING) Fail(cp, "String expected");
                                cname = GetString(cp);
                                if ((cidx = Lookup(hilit_colors, cname)) == -1)
                                    Fail(cp, "Lookup of '%s' failed", cname);
                                PutObj(cp, CF_INT, sizeof(long), &cidx);
                                if (Parse(cp) != P_CLOSEBRACE) Fail(cp, "'}' expected");
                                GetOp(cp, P_CLOSEBRACE);
                                LastState = stateno;
                            }
                            break;

                        case K_HTRANS:
                            {
                                long next_state;
                                char *opts, *match;
                                long match_opts;
                                char *cname;
                                long cidx;
                                
                                if (Parse(cp) != P_OPENBRACE) Fail(cp, "'{' expected");
                                GetOp(cp, P_OPENBRACE);

                                if (Parse(cp) != P_NUMBER) Fail(cp, "next_state index expected");
                                next_state = GetNumber(cp);
                                if (Parse(cp) != P_COMMA) Fail(cp, "',' expected");
                                GetOp(cp, P_COMMA);
                                if (Parse(cp) != P_STRING) Fail(cp, "match options expected");
                                opts = GetString(cp);
                                if (Parse(cp) != P_COMMA) Fail(cp, "',' expected");
                                GetOp(cp, P_COMMA);
                                if (Parse(cp) != P_STRING) Fail(cp, "match string expected");
                                match = GetString(cp);
                                PutObj(cp, CF_HTRANS, sizeof(long), &next_state);
                                match_opts = 0;
                                if (strchr(opts, '^')) match_opts |= MATCH_MUST_BOL;
                                if (strchr(opts, '$')) match_opts |= MATCH_MUST_EOL;
                                //if (strchr(opts, 'b')) match_opts |= MATCH_MUST_BOLW;
                                //if (strchr(opts, 'e')) match_opts |= MATCH_MUST_EOLW;
                                if (strchr(opts, 'i')) match_opts |= MATCH_NO_CASE;
                                if (strchr(opts, 's')) match_opts |= MATCH_SET;
                                if (strchr(opts, 'S')) match_opts |= MATCH_NOTSET;
                                if (strchr(opts, '-')) match_opts |= MATCH_NOGRAB;
                                if (strchr(opts, '<')) match_opts |= MATCH_TAGASNEXT;
                                if (strchr(opts, '>')) match_opts &= ~MATCH_TAGASNEXT;
                                //if (strchr(opts, '!')) match_opts |= MATCH_NEGATE;
                                if (strchr(opts, 'q')) match_opts |= MATCH_QUOTECH;
                                if (strchr(opts, 'Q')) match_opts |= MATCH_QUOTEEOL;

                                if (Parse(cp) != P_COMMA) Fail(cp, "',' expected");
                                GetOp(cp, P_COMMA);
                                if (Parse(cp) != P_STRING) Fail(cp, "String expected");
                                cname = GetString(cp);
                                if ((cidx = Lookup(hilit_colors, cname)) == -1)
                                    Fail(cp, "Lookup of '%s' failed", cname);

                                PutObj(cp, CF_INT, sizeof(long), &match_opts);
                                PutObj(cp, CF_INT, sizeof(long), &cidx);
                                PutObj(cp, CF_STRING, slen(match), match);
                                
                                if (Parse(cp) != P_CLOSEBRACE) Fail(cp, "'}' expected");
                                GetOp(cp, P_CLOSEBRACE);
                            }
                            break;

                        case K_HWTYPE:
                            if (Parse(cp) != P_OPENBRACE) Fail(cp, "'{' expected");
                            GetOp(cp, P_OPENBRACE);
                            
                            {
                                long options = 0;
                                long nextKwdMatchedState;
                                long nextKwdNotMatchedState;
                                long nextKwdNoCharState;
                                char *opts;
                                char *wordChars;


                                if (Parse(cp) != P_NUMBER) Fail(cp, "next_state index expected");
                                nextKwdMatchedState = GetNumber(cp);
                                
                                if (Parse(cp) != P_COMMA) Fail(cp, "',' expected");
                                GetOp(cp, P_COMMA);

                                if (Parse(cp) != P_NUMBER) Fail(cp, "next_state index expected");
                                nextKwdNotMatchedState = GetNumber(cp);
                                
                                if (Parse(cp) != P_COMMA) Fail(cp, "',' expected");
                                GetOp(cp, P_COMMA);

                                if (Parse(cp) != P_NUMBER) Fail(cp, "next_state index expected");
                                nextKwdNoCharState = GetNumber(cp);
                                
                                if (Parse(cp) != P_COMMA) Fail(cp, "',' expected");
                                GetOp(cp, P_COMMA);
                                
                                if (Parse(cp) != P_STRING) Fail(cp, "String expected");
                                opts = GetString(cp);
                                if (strchr(opts, 'i')) options |= STATE_NOCASE;
                                if (strchr(opts, '<')) options |= STATE_TAGASNEXT;
                                if (strchr(opts, '>')) options &= ~STATE_TAGASNEXT;
                                if (strchr(opts, '-')) options |= STATE_NOGRAB;

                                if (Parse(cp) != P_COMMA) Fail(cp, "',' expected");
                                GetOp(cp, P_COMMA);
                                
                                if (Parse(cp) != P_STRING) Fail(cp, "String expected");
                                wordChars = GetString(cp);

                                PutObj(cp, CF_HWTYPE, 0, 0);
                                PutObj(cp, CF_INT, sizeof(long), &nextKwdMatchedState);
                                PutObj(cp, CF_INT, sizeof(long), &nextKwdNotMatchedState);
                                PutObj(cp, CF_INT, sizeof(long), &nextKwdNoCharState);
                                PutObj(cp, CF_INT, sizeof(long), &options);
                                PutObj(cp, CF_STRING, slen(wordChars), wordChars);
                            }
                            if (Parse(cp) != P_CLOSEBRACE) Fail(cp, "'}' expected");
                            GetOp(cp, P_CLOSEBRACE);
                            break;
                            
                        case K_HWORDS:
                            {
                                char *colorstr, *kname;
                                //int color;

                                if (Parse(cp) != P_STRING) Fail(cp, "String expected");
                                colorstr = GetString(cp);

                                if (Parse(cp) != P_OPENBRACE) Fail(cp, "'{' expected");
                                GetOp(cp, P_OPENBRACE);
                                
                                PutObj(cp, CF_HWORDS, slen(colorstr), colorstr);
                                
                                while (1) {
                                    if (Parse(cp) == P_CLOSEBRACE) break;
                                    if (Parse(cp) != P_STRING) Fail(cp, "String expected");
                                    kname = GetString(cp);
                                    PutObj(cp, CF_STRING, slen(kname), kname);
                                    
                                    if (Parse(cp) != P_COMMA)
                                        break;
                                    else
                                        GetOp(cp, P_COMMA);
                                }
                            }
                            if (Parse(cp) != P_CLOSEBRACE) Fail(cp, "'}' expected");
                            GetOp(cp, P_CLOSEBRACE);
                            
                            PutObj(cp, CF_END, 0, 0);
                            break;

                        default:
                            if (Parse(cp) != P_ASSIGN) Fail(cp, "'=' expected");
                            GetOp(cp, P_ASSIGN);
			    switch (Parse(cp)) {
                                /*			    case P_NUMBER:
                                 {
                                 long var;
                                 long num;
                                 
                                 num = GetNumber(cp);
                                 var = LookupColorizeNumber(w);
                                 if (var == -1) return -1;
                                 PutObj(cp, CF_SETVAR, sizeof(long), &var);
                                 PutObj(cp, CF_INT, sizeof(long), &num);
                                 }
                                 break;*/
                            case P_STRING:
				{
                                    long var;
                                    
				    s = GetString(cp);
                                    if (s == 0) Fail(cp, "Parse failed");
                                    var = Lookup(colorize_string, w);
                                    if (var == -1) 
                                        Fail(cp, "Lookup of '%s' failed", w);
                                    PutObj(cp, CF_SETVAR, sizeof(long), &var);
                                    PutObj(cp, CF_STRING, slen(s), s);
				}
				break;
			    default:
				return -1;
			    }
			    if (Parse(cp) != P_EOS) Fail(cp, "';' expected");
			    GetOp(cp, P_EOS);
                            break;
                        }
                    }
                    GetOp(cp, P_CLOSEBRACE);
                    PutObj(cp, CF_END, 0, 0);
                }
                break;
                
	    case K_MODE: // mode::
                {
                    if (Parse(cp) != P_WORD) Fail(cp, "Syntax error");
                    if (GetWord(cp, ObjName) != 0) Fail(cp, "Parse failed");
                    PutObj(cp, CF_MODE, slen(ObjName), ObjName);

                    UpMode[0] = 0;
		    if (Parse(cp) == P_COLON) {
			GetOp(cp, P_COLON);
			if (Parse(cp) != P_WORD) Fail(cp, "Syntax error");
                        if (GetWord(cp, UpMode) != 0) Fail(cp, "Parse failed");
		    }
                    PutObj(cp, CF_PARENT, slen(UpMode), UpMode);
                    if (Parse(cp) != P_OPENBRACE) Fail(cp, "'{' expected");
                    GetOp(cp, P_OPENBRACE);
                    
                    while (1) {
                        p = Parse(cp);
                        if (p == P_CLOSEBRACE) break;
                        if (p == P_EOF) Fail(cp, "Unexpected EOF");
                        if (p != P_WORD) Fail(cp, "Syntax error");
                        
                        if (GetWord(cp, w) != 0) Fail(cp, "Parse failed");
                        //switch (Lookup(CfgKW, w)) {
			//default:  // mode::
			    if (Parse(cp) != P_ASSIGN) Fail(cp, "'=' expected");
			    GetOp(cp, P_ASSIGN);
			    switch (Parse(cp)) {
			    case P_NUMBER:
                                {
                                    long var;
				    long num;
				    
                                    num = GetNumber(cp);
                                    var = Lookup(mode_num, w);
                                    if (var == -1)
                                        Fail(cp, "Lookup of '%s' failed", w);
                                    PutObj(cp, CF_SETVAR, sizeof(long), &var);
                                    PutObj(cp, CF_INT, sizeof(long), &num);
				}
				break;
			    case P_STRING:
				{
                                    long var;
                                    
				    s = GetString(cp);
                                    if (s == 0) Fail(cp, "Parse failed");
                                    var = Lookup(mode_string, w);
                                    if (var == -1)
                                        Fail(cp, "Lookup of '%s' filed", w);
                                    PutObj(cp, CF_SETVAR, sizeof(long), &var);
                                    PutObj(cp, CF_STRING, slen(s), s);
				}
				break;
			    default:
				return -1;
			    }
                            if (Parse(cp) != P_EOS) Fail(cp, "';' expected");
			    GetOp(cp, P_EOS);
                        //    break;
                        //}
                    }
                    GetOp(cp, P_CLOSEBRACE);
                    PutObj(cp, CF_END, 0, 0);
                }
                break;
            case K_OBJECT:
		{
		    if (Parse(cp) != P_WORD) Fail(cp, "Syntax error");
		    if (GetWord(cp, ObjName) != 0) Fail(cp, "Parse failed");
                    if (Parse(cp) != P_OPENBRACE) Fail(cp, "'{' expected");
                    GetOp(cp, P_OPENBRACE);
                    
                    PutObj(cp, CF_OBJECT, slen(ObjName), ObjName);
                    
                    while (1) {
                        p = Parse(cp);
                        if (p == P_CLOSEBRACE) break;
                        if (p == P_EOF) Fail(cp, "Unexpected EOF");
                        if (p != P_WORD) Fail(cp, "Syntax error");
                        
                        if (GetWord(cp, w) != 0) Fail(cp, "Parse failed");
                        switch (Lookup(CfgKW, w)) {
                        case K_COLOR: // mode::color
                            if (Parse(cp) != P_OPENBRACE) Fail(cp, "'{' expected");
			    GetOp(cp, P_OPENBRACE);
                            PutObj(cp, CF_COLOR, 0, 0);
                                
			    while (1) {
				char *sname, *svalue;
                                
				if (Parse(cp) == P_CLOSEBRACE) break;
                                if (Parse(cp) != P_OPENBRACE) Fail(cp, "'{' expected");
				GetOp(cp, P_OPENBRACE);
				if (Parse(cp) != P_STRING) Fail(cp, "String expected");
                                sname = GetString(cp);
                                PutObj(cp, CF_STRING, slen(sname), sname);
				if (Parse(cp) != P_COMMA) Fail(cp, "',' expected");
				GetOp(cp, P_COMMA);
				if (Parse(cp) != P_STRING) Fail(cp, "String expected");
                                svalue = GetString(cp);
                                PutObj(cp, CF_STRING, slen(svalue), svalue);
                                if (Parse(cp) != P_CLOSEBRACE) Fail(cp, "'}' expected");
				GetOp(cp, P_CLOSEBRACE);
				if (Parse(cp) != P_COMMA)
				    break;
				else
				    GetOp(cp, P_COMMA);
			    }
                            if (Parse(cp) != P_CLOSEBRACE) Fail(cp, "'}' expected");
			    GetOp(cp, P_CLOSEBRACE);
                            if (Parse(cp) != P_EOS) Fail(cp, "';' expected");
                            GetOp(cp, P_EOS);
                            PutObj(cp, CF_END, 0, 0);
			    break;
                          
                          case K_COMPILERX:
                            {
                                long file, line, msg;
                                char *regexp;
                                
                                if (Parse(cp) != P_ASSIGN) Fail(cp, "'=' expected");
                                GetOp(cp, P_ASSIGN);
                                if (Parse(cp) != P_OPENBRACE) Fail(cp, "'{' expected");
                                GetOp(cp, P_OPENBRACE);
                                if (Parse(cp) != P_NUMBER) Fail(cp, "Number expected");
                                file = GetNumber(cp);
                                if (Parse(cp) != P_COMMA) Fail(cp, "',' expected");
                                GetOp(cp, P_COMMA);
                                if (Parse(cp) != P_NUMBER) Fail(cp, "Number expected");
                                line = GetNumber(cp);
                                if (Parse(cp) != P_COMMA) Fail(cp, "',' expected");
                                GetOp(cp, P_COMMA);
                                if (Parse(cp) != P_NUMBER) Fail(cp, "Number expected");
                                msg = GetNumber(cp);
                                if (Parse(cp) != P_COMMA) Fail(cp, "',' expected");
                                GetOp(cp, P_COMMA);
                                if (Parse(cp) != P_STRING) Fail(cp, "String expected");
				regexp = GetString(cp);
                                if (Parse(cp) != P_CLOSEBRACE) Fail(cp, "'}' expected");
                                GetOp(cp, P_CLOSEBRACE);
                                PutObj(cp, CF_COMPRX, 0, 0);
                                PutObj(cp, CF_INT, sizeof(long), &file);
                                PutObj(cp, CF_INT, sizeof(long), &line);
                                PutObj(cp, CF_INT, sizeof(long), &msg);
                                PutObj(cp, CF_REGEXP, slen(regexp), regexp);
                                if (Parse(cp) != P_EOS) Fail(cp, "';' expected");
                                GetOp(cp, P_EOS);
                            }
                            break;
                          default:  // mode::
                            if (Parse(cp) != P_ASSIGN) Fail(cp, "'=' expected");
			    GetOp(cp, P_ASSIGN);
			    
			    switch (Parse(cp)) {
			    case P_NUMBER:
                                {
                                    long var;
				    long num;
				    
                                    num = GetNumber(cp);
                                    var = Lookup(global_num, w);
                                    if (var == -1) 
                                        Fail(cp, "Lookup of '%s' failed", w);
                                    PutObj(cp, CF_SETVAR, sizeof(long), &var);
                                    PutObj(cp, CF_INT, sizeof(long), &num);
				}
				break;
			    case P_STRING:
                                {
                                    long var;
                                    
				    s = GetString(cp);
                                    if (s == 0) Fail(cp, "Parse failed");
                                    var = Lookup(global_string, w);
                                    if (var == -1) Fail(cp, "Lookup of '%s' failed");
                                    PutObj(cp, CF_SETVAR, sizeof(long), &var);
                                    PutObj(cp, CF_STRING, slen(s), s);
				}
				break;
			    default:
				Fail(cp, "Syntax error");
			    }
			    if (Parse(cp) != P_EOS) Fail(cp, "';' expected");
			    GetOp(cp, P_EOS);
                            break;
                        }
                    }
                    GetOp(cp, P_CLOSEBRACE);
                    PutObj(cp, CF_END, 0, 0);
                }
                break;
            case K_INCLUDE:
                {
                    char *fn;

                    if (Parse(cp) != P_STRING) Fail(cp, "String expected");
                    fn = GetString(cp);

                    if (LoadFile(cp.name, fn) != 0) Fail(cp, "Include of file '%s' failed", fn);
                    if (Parse(cp) != P_EOS) Fail(cp, "';' expected");
                    GetOp(cp, P_EOS);
                }
                break;
	    default: 
		Fail(cp, "Syntax error");
            }
            break;
        case P_EOF: return 0;
        default:    Fail(cp, "Syntax error");
        }
    }
}
    
int LoadFile(char *WhereName, char *CfgName, int Level) {
    int fd, rc;
    char *buffer = 0;
    struct stat statbuf;
    CurPos cp;
    char last[256];
    char Cfg[256];
    
    //fprintf(stderr, "Loading file %s %s\n", WhereName, CfgName);
    
    JustDirectory(WhereName, last);
    
    if (IsFullPath(CfgName)) {
        strcpy(Cfg, CfgName);
    } else {
        SlashDir(last);
        strcat(last, CfgName);
        ExpandPath(last, Cfg);
    }
    // puts(Cfg);
    
    //fprintf(stderr, "Loading file %s\n", Cfg);
    if ((fd = open(Cfg, O_RDONLY | O_BINARY)) == -1) {
        fprintf(stderr, "Cannot open '%s', errno=%d\n", Cfg, errno);
        return -1;
    }
    if (fstat(fd, &statbuf) != 0) {
        close(fd);
        fprintf(stderr, "Cannot stat '%s', errno=%d\n", Cfg, errno);
        return -1;
    }
    buffer = (char *) malloc(statbuf.st_size);
    if (buffer == 0) {
        close(fd);
        return -1;
    }
    if (read(fd, buffer, statbuf.st_size) != statbuf.st_size) {
        close(fd);
        return -1;
    }
    close(fd);
    
    cp.sz = statbuf.st_size;
    cp.a = cp.c = buffer;
    cp.z = cp.a + cp.sz;
    cp.line = 1;
    cp.name = Cfg;
    
    rc = ParseConfigFile(cp, Cfg);
    // puts("End Loading file");
    if (Level == 0) 
        PutObj(cp, CF_EOF, 0, 0);

    if (rc == -1) {
        Fail(cp, "Parse failed");
    }
    free(buffer);
    return rc;
}

