/*    h_c.cpp
 *
 *    Copyright (c) 1994-1996, Marko Macek
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 */

#include "fte.h"

#ifdef CONFIG_HILIT_C


#define PRINTF(x) //printf x


#define hsC_Normal       0
#define hsC_Comment      1
#define hsC_CommentL     2
#define hsC_Keyword      4
#define hsC_String1     10
#define hsC_String2     11
#define hsC_CPP         12
#define hsC_CPP_Comm    13
#define hsC_CPP_String1 14
#define hsC_CPP_String2 15
#define hsC_CPP_ABrace  16

int Hilit_C(EBuffer *BF, int LN, PCell B, int Pos, int Width, ELine *Line, hlState &State, hsState *StateMap, int *ECol) {
    ChColor *Colors = BF->Mode->fColorize->Colors;
    int j = 0;
    int firstnw = 0;
    HILIT_VARS(Colors[CLR_Normal], Line);
    int len1 = len;
    char *last = p + len1 - 1;
    int was_include = 0;

    for(i = 0; i < Line->Count;) {
        if (*p != ' ' && *p != 9) firstnw++;
        IF_TAB() else {
            switch(State) {
            default:
            case hsC_Normal:
		if (isalpha(*p) || *p == '_') {
		    j = 0;
		    while (((i + j) < Line->Count) &&
			   (isalnum(Line->Chars[i+j]) ||
			    (Line->Chars[i + j] == '_'))
			   ) j++;
		    if (BF->GetHilitWord(j, &Line->Chars[i], Color)) {
		    //                        Color = hcC_Keyword;
			State = hsC_Keyword;
		    } else {
			int x;
			x = i + j;
			while ((x < Line->Count) &&
			       ((Line->Chars[x] == ' ') || (Line->Chars[x] == 9))) x++;
			if ((x < Line->Count) && (Line->Chars[x] == '(')) {
			    Color = Colors[CLR_Function];
                        } else if ((x < Line->Count) && (Line->Chars[x] == ':' && (x == Line->Count - 1 || Line->Chars[x + 1] != ':')) && firstnw == 1) {
                            Color = Colors[CLR_Label];
                        } else {
                            Color = Colors[CLR_Normal];
                        }
			State = hsC_Normal;
                    }
		    if (StateMap)
			memset(StateMap + i, State, j);
                    if (B) 
			MoveMem(B, C - Pos, Width, Line->Chars + i, Color, j);
		    i += j;
                    len -= j;
                    p += j;
                    C += j;
                    State = hsC_Normal;
                    continue;
                } else if ((len >= 2) && (*p == '/') && (*(p+1) == '*')) {
                    State = hsC_Comment;
		    Color = Colors[CLR_Comment];
                    goto hilit2;
		} else if ((len >= 2) && (*p == '/') && (*(p+1) == '/')) {
		    State = hsC_CommentL;
                    Color = Colors[CLR_Comment];
                    goto hilit2;
                } else if ((len >= 2) && (*p == '0') && (*(p+1) == 'x')) {
                    Color = Colors[CLR_HexNumber];
                    ColorNext();
                    ColorNext();
                    while (len && isxdigit(*p)) ColorNext();
                    if (len && (toupper(*p) == 'U')) ColorNext();
                    if (len && (toupper(*p) == 'L')) ColorNext();
                    continue;
                } else if (isdigit(*p)) {
                    Color = Colors[CLR_Number];
                    ColorNext();
                    while (len && (isdigit(*p) || *p == 'e' || *p == 'E' || *p == '.')) ColorNext();
                    if (len && (toupper(*p) == 'U')) ColorNext();
                    if (len && (toupper(*p) == 'L')) ColorNext();
                    continue;
                } else if (*p == '\'') {
                    State = hsC_String1;
                    Color = Colors[CLR_String];
                    goto hilit;
                } else if (*p == '"') {
                    State = hsC_String2;
                    Color = Colors[CLR_String];
                    goto hilit;
                } else if (firstnw == 1 && *p == '#') {
                    State = hsC_CPP;
                    Color = Colors[CLR_CPreprocessor];
                    goto hilit;
                } else if (ispunct(*p) && *p != '_') {
                    Color = Colors[CLR_Punctuation];
                    goto hilit;
                }
                Color = Colors[CLR_Normal];
                goto hilit;
            case hsC_Comment:
                Color = Colors[CLR_Comment];
                if ((len >= 2) && (*p == '*') && (*(p+1) == '/')) {
                    ColorNext();
                    ColorNext();
                    State = hsC_Normal;
                    continue;
                }
                goto hilit;
            case hsC_CPP_Comm:
                Color = Colors[CLR_Comment];
                if ((len >= 2) && (*p == '*') && (*(p+1) == '/')) {
                    ColorNext();
                    ColorNext();
                    State = hsC_CPP;
                    continue;
                }
                goto hilit;
            case hsC_CPP_ABrace:
                Color = Colors[CLR_String];
                if (*p == '>') {
                    Color = Colors[CLR_CPreprocessor];
                    State = hsC_CPP;
                }
                goto hilit;
            case hsC_CommentL:
                Color = Colors[CLR_Comment];
                goto hilit;
            case hsC_String1:
                Color = Colors[CLR_String];
                if ((len >= 2) && (*p == '\\')) {
                    goto hilit2;
                } else if (*p == '\'') {
                    ColorNext();
                    State = hsC_Normal;
                    continue;
                }
                goto hilit;
            case hsC_String2:
                Color = Colors[CLR_String];
                if ((len >= 2) && (*p == '\\')) {
                    goto hilit2;
                } else if (*p == '"') {
                    ColorNext();
                    State = hsC_Normal;
                    continue;
                }
                goto hilit;
            case hsC_CPP_String1:
                Color = Colors[CLR_String];
                if ((len >= 2) && (*p == '\\')) {
                    goto hilit2;
                } else if (*p == '\'') {
                    ColorNext();
                    State = hsC_CPP;
                    continue;
                }
                goto hilit;
            case hsC_CPP_String2:
                Color = Colors[CLR_String];
                if ((len >= 2) && (*p == '\\')) {
                    goto hilit2;
                } else if (*p == '"') {
                    ColorNext();
                    State = hsC_CPP;
                    continue;
                }
                goto hilit;
            case hsC_CPP:
                if (isalpha(*p) || *p == '_') {
                    j = 0;
                    Color = Colors[CLR_CPreprocessor];
                    while (((i + j) < Line->Count) &&
                           (isalnum(Line->Chars[i+j]) ||
                            (Line->Chars[i + j] == '_'))
                           ) j++;
                    if (j == 7 && memcmp(Line->Chars + i, "include", 7) == 0)
                        was_include = 1;
                    if (StateMap)
                        memset(StateMap + i, State, j);
                    if (B) 
                        MoveMem(B, C - Pos, Width, Line->Chars + i, Color, j);
                    i += j;
                    len -= j;
                    p += j;
                    C += j;
                    continue;
                } else if ((len >= 2) && (*p == '/') && (*(p+1) == '*')) {
                    State = hsC_CPP_Comm;
                    Color = Colors[CLR_Comment];
                    goto hilit2;
                } else if ((len >= 2) && (*p == '/') && (*(p+1) == '/')) {
                    State = hsC_CommentL;
                    Color = Colors[CLR_Comment];
                hilit2:
                    ColorNext();
                hilit:
                    ColorNext();
                    continue;                              
                } else if ((len >= 2) && (*p == '0') && (*(p+1) == 'x')) {
                    Color = Colors[CLR_HexNumber];
                    ColorNext();
                    ColorNext();
                    while (len && isxdigit(*p)) ColorNext();
                    if (len && (toupper(*p) == 'U')) ColorNext();
                    if (len && (toupper(*p) == 'L')) ColorNext();
                    continue;
                } else if (isdigit(*p)) {
                    Color = Colors[CLR_Number];
                    ColorNext();
                    while (len && isdigit(*p)) ColorNext();
                    if (len && (toupper(*p) == 'U')) ColorNext();
                    if (len && (toupper(*p) == 'L')) ColorNext();
                    continue;
                } else if (*p == '\'') {
                    State = hsC_CPP_String1;
                    Color = Colors[CLR_String];
                    goto hilit;
                } else if (*p == '"') {
                    State = hsC_CPP_String2;
                    Color = Colors[CLR_String];
                    goto hilit;
                } else if (*p == '<' && was_include) {
                    ColorNext();
                    State = hsC_CPP_ABrace;
                    continue;
                } else {
                    Color = Colors[CLR_CPreprocessor];
                    goto hilit;
                }
            }
        }
    }
    if (State == hsC_CommentL)
        State = hsC_Normal;
    if ((len1 == 0) || (*last != '\\')) {
        switch(State) {
        case hsC_String1:
        case hsC_String2:
        case hsC_CPP:
        case hsC_CPP_String1:
        case hsC_CPP_String2:
            State = hsC_Normal;
            break;
        }
    }
    *ECol = C;
    return 0;
}

int IsState(hsState *Buf, hsState State, int Len) {
    int I;
    
    for(I = 0; I < Len; I++)
        if (Buf[I] != State) return 0;
    return 1;
}

int LookAt(EBuffer *B, int Row, int Pos, char *What, hsState State, int NoWord) {
    char *P;
    int L;
    int StateLen;
    hsState *StateMap;
    int Len = strlen(What);
    
    if (Row < 0 || Row >= B->RCount) return 0;
    P = B->RLine(Row)->Chars;
    L = B->RLine(Row)->Count;
    Pos = B->CharOffset(B->RLine(Row), Pos);
    if (Pos + strlen(What) > L) return 0;
    if (NoWord && L > Pos + Len && isalnum(P[Pos + Len]))
        return 0;
    if (memcmp(P + Pos, What, Len) == 0)
        return 1;
    else
        return 0;
}

#ifdef CONFIG_INDENT_C

int C_Indent = 4;
int C_BraceOfs = 0;
int C_CaseOfs = 0;
int C_CaseDelta = 4;
int C_ClassOfs = 0;
int C_ClassDelta = 4;
int C_ColonOfs = -4;
int C_CommentOfs = 0;
int C_CommentDelta = 1;

#define C_INDENT            C_Indent
#define C_BRACE_OFS         C_BraceOfs
#define C_CASE_OFS          C_CaseOfs
#define C_CASE_DELTA        C_CaseDelta
#define C_CLASS_OFS         C_ClassOfs
#define C_CLASS_DELTA       C_ClassDelta
#define C_COLON_OFS         C_ColonOfs
#define C_COMMENT_OFS       C_CommentOfs
#define C_COMMENT_DELTA     C_CommentDelta
#define C_CONTINUATION      C_Indent

#define FIND_IF         0x0001
#define FIND_SEMICOLON  0x0002
#define FIND_COMMA      0x0004
#define FIND_COLON      0x0008
#define FIND_ELSE       0x0010
#define FIND_FOR        0x0020
#define FIND_WHILE      0x0040
#define FIND_ENDBLOCK   0x0080
//#define FIND_BEGINBLOCK 0x0100
#define FIND_CLASS      0x0200
#define FIND_CASE       0x0400

static int CheckLabel(EBuffer *B, int Line) {
    PELine L = B->RLine(Line);
    int P = B->CharOffset(L, B->LineIndented(Line));
    int Cnt = 0;
    
    if (Line > 0 && B->RLine(Line - 1)->StateE != hsC_Normal)
	return 0;
    
    while ((P < L->Count) &&
	   (L->Chars[P] == ' ' || L->Chars[P] == 9)) P++;
    while (P < L->Count) {
	if (Cnt > 0)
            if (L->Chars[P] == ':' && (Cnt == 1 || L->Chars[P + 1] != ':')) return 1;
	if (!isalnum(L->Chars[P]) && L->Chars[P] != '_') return 0;
	Cnt++;
	P++;
    }
    return 0;
}

static int SearchBackMatch(int Count, EBuffer *B, int Row, hsState State, char *Open, char *Close, int *OPos, int *OLine, int matchparens = 0, int bolOnly = 0) {
    char *P;
    int L;
    int Pos;
    int LOpen = strlen(Open);
    int LClose = strlen(Close);
    int StateLen;
    hsState *StateMap;
    int CountX[3] = { 0, 0, 0 };
    int didMatch = 0;
    
    *OLine = Row;
    *OPos = 0;
    while (Row >= 0) {
        P = B->RLine(Row)->Chars;
        L = B->RLine(Row)->Count;
        StateMap = NULL;
        if (B->GetMap(Row, &StateLen, &StateMap) == 0) return -1;
        Pos = L - 1;
        if (L > 0) while (Pos >= 0) {
            if (P[Pos] != ' ' && P[Pos] != 9) {
                if (StateMap[Pos] == hsC_Normal) {
                    switch (P[Pos]) {
                      case '{': CountX[0]--; break;
                      case '}': CountX[0]++; break;
                      case '(': CountX[1]--; break;
                      case ')': CountX[1]++; break;
                      case '[': CountX[2]--; break;
                      case ']': CountX[2]++; break;
                    }
                }
                if (!matchparens ||
                    (CountX[0] == 0 && CountX[1] == 0 && CountX[2] == 0))
                {
                    if (LOpen + Pos <= L) {
                        if (IsState(StateMap + Pos, State, LOpen)) {
                            if (memcmp(P + Pos, Open, LOpen) == 0) Count++;
                            if (Count == 0) {
                                if (bolOnly)
                                    didMatch = 1;
                                else {
                                    *OPos = B->ScreenPos(B->RLine(Row), Pos);
                                    *OLine = Row;
                                    free(StateMap);
                                    return B->LineIndented(Row);
                                }
                            }
                        }
                        if (LClose + Pos <= L) {
                            if (IsState(StateMap + Pos, State, LClose)) {
                                if (memcmp(P + Pos, Close, LClose) == 0) Count--;
                            }
                        }
                    }
                }
            }
            Pos--;
        }
        if (bolOnly && didMatch && CountX[1] == 0 && CountX[2] == 0) {
            *OPos = 0;
            *OLine = Row;
            free(StateMap);
            return B->LineIndented(Row);
        }
        if (StateMap) free(StateMap);
        Row--;
    }
    return -1;
}

static int FindPrevIndent(EBuffer *B, int &RowP, int &ColP, char &CharP, int Flags) {
    int StateLen;
    hsState *StateMap = 0;
    char *P;
    int L;

    int Count[4] = {
        0, // { }
        0, // ( )
        0, // [ ]
        0, // if/else (one if for each else)
    };

    assert(RowP >= 0 && RowP < B->RCount);
    L = B->RLine(RowP)->Count;
    if (ColP >= L)
        ColP = L - 1;
    assert(ColP >= -1 && ColP <= L);

    while (RowP >= 0) {
        char BolChar = ' ';
        int BolCol = -1;

        P = B->RLine(RowP)->Chars;
        L = B->RLine(RowP)->Count;
        StateMap = NULL;
        if (B->GetMap(RowP, &StateLen, &StateMap) == 0)
            return 0;
        if (L > 0) while (ColP >= 0) {
            
            if (StateMap[ColP] == hsC_Normal) {
                switch (CharP = P[ColP]) {
                case '{':
                    if (Count[0] == 0 && Count[1] == 0 && Count[2] == 0 && Count[3] == 0)
                    {
                        free(StateMap);
                        return 1;
                    }
                    Count[0]--;
                    break;
                case '}':
                    if (ColP == 0) { /* speed optimization */
                        free(StateMap);
                        return 1;
                    }
                    if (Count[0] == 0 && Count[1] == 0 && Count[2] == 0 && Count[3] == 0 &&
                        (Flags & FIND_ENDBLOCK))
                    {
                        free(StateMap);
                        return 1;
                    }
                    Count[0]++;
                    break;
                case '(':
                    if (Count[0] == 0 && Count[1] == 0 && Count[2] == 0 && Count[3] == 0) {
                        free(StateMap);
                        return 1;
                    }
                    Count[1]--;
                    break;
                case ')':
                    Count[1]++;
                    break;
                case '[':
                    if (Count[0] == 0 && Count[1] == 0 && Count[2] == 0 && Count[3] == 0) {
                        free(StateMap);
                        return 1;
                    }
                    Count[2]--;
                    break;
                case ']':
                    Count[2]++;
                    break;

                case ':':
                    if (ColP >= 1 && P[ColP - 1] == ':') { // skip ::
                        ColP -= 2;
                        continue;
                    }
                case ',':
                case ';':
                    if (Count[0] == 0 &&
                        Count[1] == 0 &&
                        Count[2] == 0 &&
                        Count[3] == 0 &&
                        BolChar == ' ')
                    {
                        if ((CharP == ',' && (Flags & FIND_COMMA)) ||
                            (CharP == ';' && (Flags & FIND_SEMICOLON)) ||
                            (CharP == ':' && (Flags & FIND_COLON)))
                        {
                            BolChar = CharP;
                            BolCol = ColP;
//                            free(StateMap);
//                            return 1;
                        }
                    }
                    break;
                }
            } else if (BolChar == ' ' && StateMap[ColP] == hsC_Keyword) {
                if (L - ColP >= 2 &&
                    IsState(StateMap + ColP, hsC_Keyword, 2) &&
                    memcmp(P + ColP, "if", 2) == 0)
                {
                    //puts("\nif");
                    if (Count[3] > 0)
                        Count[3]--;
                    if (Flags & FIND_IF) {
                        if (Count[0] == 0 && Count[1] == 0 && Count[2] == 0 && Count[3] == 0) {
                            CharP = 'i';
                            free(StateMap);
                            return 1;
                        }
                    }
                }
                if (L - ColP >= 4 &&
                    IsState(StateMap + ColP, hsC_Keyword, 4) &&
                    memcmp(P + ColP, "else", 4) == 0)
                {
                    //puts("\nelse\x7");
                    if (Flags & FIND_ELSE) {
                        if (Count[0] == 0 && Count[1] == 0 && Count[2] == 0 && Count[3] == 0) {
                            CharP = 'e';
                            free(StateMap);
                            return 1;
                        }
                    }
                    Count[3]++;
                }
                if (Count[0] == 0 && Count[1] == 0 && Count[2] == 0 && Count[3] == 0) {
                    
                    if ((Flags & FIND_FOR) &&
                        L - ColP >= 3 &&
                        IsState(StateMap + ColP, hsC_Keyword, 3) &&
                        memcmp(P + ColP, "for", 3) == 0)
                    {
                        CharP = 'f';
                        free(StateMap);
                        return 1;
                    }
                    if ((Flags & FIND_WHILE) &&
                        L - ColP >= 5 &&
                        IsState(StateMap + ColP, hsC_Keyword, 5) &&
                        memcmp(P + ColP, "while", 5) == 0)
                    {
                        CharP = 'w';
                        free(StateMap);
                        return 1;
                    }
                    if ((Flags & FIND_CASE) &&
                        (L - ColP >= 4 &&
                         IsState(StateMap + ColP, hsC_Keyword, 4) &&
                         memcmp(P + ColP, "case", 4) == 0) ||
                        ((L - ColP >= 7) &&
                         IsState(StateMap + ColP, hsC_Keyword, 7) &&
                         memcmp(P + ColP, "default", 7) == 0))
                    {
                        CharP = 'c';
                        free(StateMap);
                        return 1;
                    }
                    if ((Flags & FIND_CLASS) &&
                        (L - ColP >= 5 &&
                         IsState(StateMap + ColP, hsC_Keyword, 5) &&
                         memcmp(P + ColP, "class", 5) == 0))
                    {
                        CharP = 'l';
                        free(StateMap);
                        return 1;
                    }
                    if ((Flags & FIND_CLASS) &&
                        (L - ColP >= 6 &&
                         IsState(StateMap + ColP, hsC_Keyword, 6) &&
                         memcmp(P + ColP, "public", 6) == 0) ||
                        ((L - ColP >= 7) &&
                         IsState(StateMap + ColP, hsC_Keyword, 7) &&
                         memcmp(P + ColP, "private", 7) == 0) ||
                        ((L - ColP >= 9) &&
                         IsState(StateMap + ColP, hsC_Keyword, 9) &&
                         memcmp(P + ColP, "protected", 9) == 0))
                    {
                        CharP = 'p';
                        free(StateMap);
                        return 1;
                    }
                }
            }
            ColP--;
        }
        free(StateMap);
        if (BolChar != ' ') {
            CharP = BolChar;
            ColP = BolCol;
            return 1;
        }
        RowP--;
        if (RowP >= 0) {
            L = B->RLine(RowP)->Count;
            ColP = L - 1;
        }
    }
    return 0;
}

#define SKIP_FORWARD  0
#define SKIP_BACK     1
#define SKIP_MATCH    2
#define SKIP_LINE     4
#define SKIP_TOBOL    8

static int SkipWhite(EBuffer *B, int Bottom, int &Row, int &Col, int Flags) {
    char *P;
    int L;
    int StateLen;
    hsState *StateMap;
    int Count[3] = { 0, 0, 0 };

    assert (Row >= 0 && Row < B->RCount);
    L = B->RLine(Row)->Count;
    if (Col >= L)
        Col = L;
    assert (Col >= -1 && Col <= L) ;

    while (Row >= 0 && Row < B->RCount) {
        P = B->RLine(Row)->Chars;
        L = B->RLine(Row)->Count;
        StateMap = NULL;
        if (B->GetMap(Row, &StateLen, &StateMap) == 0)
            return 0;

        if (L > 0) for ( ;
                        Col >= 0 && Col < L;
                        Col += ((Flags & SKIP_BACK) ? -1 : +1))
        {
            if (P[Col] == ' ' || P[Col] == '\t')
                continue;
            if (StateMap[Col] != hsC_Normal &&
                StateMap[Col] != hsC_Keyword &&
                StateMap[Col] != hsC_String1 &&
                StateMap[Col] != hsC_String2)
                continue;
            if (StateMap[Col] == hsC_Normal && (Flags & SKIP_MATCH)) {
                switch (P[Col]) {
                case '{': Count[0]--; continue;
                case '}': Count[0]++; continue;
                case '(': Count[1]--; continue;
                case ')': Count[1]++; continue;
                case '[': Count[2]--; continue;
                case ']': Count[2]++; continue;
                }
            }
            if (Count[0] == 0 && Count[1] == 0 && Count[2] == 0 && !(Flags & SKIP_TOBOL))
            {
                free(StateMap);
                return 1;
            }
        }
        free(StateMap);
        if (Count[0] == 0 && Count[1] == 0 && Count[2] == 0 && (Flags & SKIP_TOBOL))
            return 1;
        if (Flags & SKIP_LINE) {
            return 1;
        }
        if (Flags & SKIP_BACK) {
            Row--;
            if (Row >= 0) {
                L = B->RLine(Row)->Count;
                Col = L - 1;
            }
        } else {
            if (Row + 1 >= Bottom)
                return 1;
            Row++;
            Col = 0;
        }
    }
    return 0;
}


static int IndentNormal(EBuffer *B, int Line, int StateLen, hsState *StateMap) {
    int I = 0;
    int Pos, L;

    if (LookAt(B, Line, 0, "case", hsC_Keyword) ||
        LookAt(B, Line, 0, "default", hsC_Keyword)) 
    {
        I = SearchBackMatch(-1, B, Line - 1, hsC_Normal, "{", "}", &Pos, &L) + C_CASE_OFS;
        return I;
    } else if (LookAt(B, Line, 0, "public:", hsC_Keyword, 0) ||
               LookAt(B, Line, 0, "private:", hsC_Keyword, 0) ||
               LookAt(B, Line, 0, "protected:", hsC_Keyword, 0))
    {
        I = SearchBackMatch(-1, B, Line - 1, hsC_Normal, "{", "}", &Pos, &L) + C_CLASS_OFS;
        return I;
    } else if (LookAt(B, Line, 0, "else", hsC_Keyword)) {
        I = SearchBackMatch(-1, B, Line - 1, hsC_Keyword, "if", "else", &Pos, &L, 1);
        return I;
    } else if (LookAt(B, Line, 0, "}", hsC_Normal)) {
        I = SearchBackMatch(-1, B, Line - 1, hsC_Normal, "{", "}", &Pos, &L, 0, 1);
        return I;
    } else if (LookAt(B, Line, 0, ")", hsC_Normal)) {
        I = SearchBackMatch(-1, B, Line - 1, hsC_Normal, "(", ")", &Pos, &L);
        return Pos;
    } else if (LookAt(B, Line, 0, "]", hsC_Normal)) {
        I = SearchBackMatch(-1, B, Line - 1, hsC_Normal, "[", "]", &Pos, &L);
        return Pos;
    } else {
        char CharP = ' ';
        int RowP = Line;
        int ColP = -1;
        int PrevRowP = RowP;
        int PrevColP = ColP;
        int FirstRowP;
        int FirstColP;
        int ContinuationIndent = 0;

        if (SkipWhite(B, Line, PrevRowP, PrevColP, SKIP_BACK) != 1)
            return 0;
        
        PrevColP++;
        PRINTF(("\nPrevRowP=%d, PrevColP=%d\n", PrevRowP, PrevColP));

        if (FindPrevIndent(B, RowP, ColP, CharP,
                           FIND_IF |
                           FIND_ELSE |
                           FIND_FOR |
                           FIND_WHILE |
                           FIND_COLON |
                           FIND_SEMICOLON |
                           FIND_COMMA |
                           FIND_ENDBLOCK) != 1)
        {
            if (RowP != PrevRowP)
                ContinuationIndent = C_CONTINUATION;
            I = 0;
            if (LookAt(B, Line, 0, "{", hsC_Normal, 0)) {
                I += C_BRACE_OFS;
                ContinuationIndent = 0;
            }
            return I + ContinuationIndent;
        }

        FirstRowP = RowP;
        FirstColP = ColP;

        PRINTF(("Line=%d, RowP=%d, ColP=%d, CharP=%c\n", Line, RowP, ColP, CharP));

        switch (CharP) {
        case '(':
        case '[':

            ColP++;
            if (SkipWhite(B, Line, RowP, ColP, SKIP_FORWARD | SKIP_LINE) != 1)
                return 0;
            I = B->ScreenPos(B->RLine(RowP), ColP);
            return I;
            
        case '{':
            ColP++;
            if (((PrevRowP != RowP) ||
                 ((PrevRowP == RowP) && (PrevColP != ColP)))
                && FirstRowP != PrevRowP)
                ContinuationIndent = C_CONTINUATION;
            ColP--; ColP--;
            if (SkipWhite(B, Line, RowP, ColP, SKIP_BACK | SKIP_TOBOL | SKIP_MATCH) != 1)
                return 0;
            I = B->LineIndented(RowP) + C_INDENT;
            PRINTF(("'{' indent : Line=%d, RowP=%d, ColP=%d, CharP=%c\n", Line, RowP, ColP, CharP));

            if (!LookAt(B, Line, 0, "{", hsC_Normal, 0))
                I -= C_BRACE_OFS;

            return I + ContinuationIndent;

        case '}':
            ColP++;
            ColP++;
            /*---nobreak---*/
        case ';':
        case ',':
            ColP--;
            if (FindPrevIndent(B, RowP, ColP, CharP,
                               ((CharP == ',') ? FIND_COMMA | FIND_COLON :
//                                (CharP == ';') ? FIND_SEMICOLON | FIND_COLON :
                                FIND_SEMICOLON | FIND_COLON)) != 1)
            {
                if (FirstRowP != PrevRowP)
                        ContinuationIndent = C_CONTINUATION;
                I = 0;
                if (LookAt(B, Line, 0, "{", hsC_Normal, 0)) {
                    I += C_BRACE_OFS;
                    ContinuationIndent = 0;
                }
                return I + ContinuationIndent;
            }
            switch (CharP) {
            case ',':
            case ';':
            case '{':
            case ':':
                ColP++;
                if (SkipWhite(B, Line, RowP, ColP, SKIP_FORWARD) != 1)
                    return 0;
                //ColP--;
                //if (SkipWhite(B, RowP, ColP, SKIP_BACK) != 1)
                //    return 0;
                PRINTF(("';' indent : Line=%d, RowP=%d, ColP=%d, CharP=%c\n", Line, RowP, ColP, CharP));
                I = B->LineIndented(RowP);
                if (((PrevRowP != RowP) ||
                     ((PrevRowP == RowP) && (PrevColP != ColP)))
                    && FirstRowP != PrevRowP)
                    ContinuationIndent = C_CONTINUATION;

                if (LookAt(B, Line, 0, "{", hsC_Normal, 0)) {
                    //I += C_BRACE_OFS;
                    ContinuationIndent = 0;
                }
                if (LookAt(B, Line, 0, "{", hsC_Normal, 0)
                    && LookAt(B, RowP, ColP, "{", hsC_Normal, 0))
                    I -= 0; //C_BRACE_OFS;
                else if (LookAt(B, Line, 0, "{", hsC_Normal, 0)
                         && !LookAt(B, RowP, ColP, "{", hsC_Normal, 0))
                    I += C_BRACE_OFS;
                else if (!LookAt(B, Line, 0, "{", hsC_Normal, 0)
                         && LookAt(B, RowP, ColP, "{", hsC_Normal, 0))
                    I -= C_BRACE_OFS;
                break;
            case '(':
                ColP++;
                if (SkipWhite(B, Line, RowP, ColP, SKIP_FORWARD | SKIP_LINE) != 1)
                    return 0;
                I = B->ScreenPos(B->RLine(RowP), ColP);
                break;
            default:
                I = B->LineIndented(RowP);
                break;
            }
            PRINTF(("';' -- indent : Line=%d, RowP=%d, ColP=%d, CharP=%c\n", Line, RowP, ColP, CharP));

//            else
//            if (LookAt(B, Line, 0, "{", hsC_Normal, 0))
//                I += C_INDENT - C_BRACE_OFS;

            return I + ContinuationIndent;

        case ':':
            ColP--;
            if (FindPrevIndent(B, RowP, ColP, CharP, FIND_SEMICOLON | FIND_COLON | FIND_CLASS | FIND_CASE) != 1) {
                if (FirstRowP != PrevRowP)
                    ContinuationIndent = C_CONTINUATION;
                return 0 + ContinuationIndent;
            }
            switch (CharP) {
            case ':':
                ColP++;
                //if (SkipWhite(B, Line, RowP, ColP, SKIP_FORWARD) != 1)
                //    return 0;
                I = B->LineIndented(RowP) - C_COLON_OFS;
                PRINTF(("':' 0 indent : Line=%d, RowP=%d, ColP=%d, CharP=%c\n", Line, RowP, ColP, CharP));
                break;
            case '{':
            case ';':
                ColP++;
                if (SkipWhite(B, Line, RowP, ColP, SKIP_FORWARD) != 1)
                    return 0;
                I = B->LineIndented(RowP);
                if (FirstRowP == RowP) {
                    I -= C_COLON_OFS;
                }
                if (LookAt(B, Line, 0, "{", hsC_Normal, 0)
                    && LookAt(B, RowP, ColP, "{", hsC_Normal, 0))
                    I -= 0; //C_BRACE_OFS;
                else if (LookAt(B, Line, 0, "{", hsC_Normal, 0)
                         && !LookAt(B, RowP, ColP, "{", hsC_Normal, 0))
                    I += C_BRACE_OFS;
                else if (!LookAt(B, Line, 0, "{", hsC_Normal, 0)
                         && LookAt(B, RowP, ColP, "{", hsC_Normal, 0))
                    I -= C_BRACE_OFS;
///                if (LookAt(B, Line, 0, "{", hsC_Normal, 0)) {
///                    I += C_INDENT - C_BRACE_OFS;
///                }
                PRINTF(("':' 1 indent : Line=%d, RowP=%d, ColP=%d, CharP=%c\n", Line, RowP, ColP, CharP));
                break;
            case 'p':
                ColP++;
                //if (SkipWhite(B, Line, RowP, ColP, SKIP_FORWARD) != 1)
//                    return 0;
                I = B->LineIndented(RowP) + C_CLASS_DELTA;
//                if (FirstRowP == RowP) {
//                    I += C_CLASS_DELTA;
///                if (LookAt(B, Line, 0, "{", hsC_Normal, 0)) {
///                    I += C_INDENT - C_BRACE_OFS;
///                }
//                }
               PRINTF(("':' 2 indent : Line=%d, RowP=%d, ColP=%d, CharP=%c\n", Line, RowP, ColP, CharP));
                break;
            case 'l':
                ColP++;
                I = B->LineIndented(RowP) + C_BRACE_OFS;
                //C_CLASS_OFS + C_CLASS_DELTA;
                break;
            case 'c':
                ColP++;
//                if (SkipWhite(B, Line, RowP, ColP, SKIP_FORWARD) != 1)
//                    return 0;
                I = B->LineIndented(RowP) + C_CASE_DELTA;
//                if (FirstRowP == RowP) {
//                    I += C_CASE_DELTA;
///                if (LookAt(B, Line, 0, "{", hsC_Normal, 0)) {
///                        I += C_INDENT - C_BRACE_OFS;
///                }
//                }

                PRINTF(("':' 3 indent : Line=%d, RowP=%d, ColP=%d, CharP=%c\n", Line, RowP, ColP, CharP));
                break;
            default:
                I = B->LineIndented(RowP);
                break;
            }
            if (((PrevRowP != RowP) ||
                 ((PrevRowP == RowP) && (PrevColP != ColP)))
                && FirstRowP != PrevRowP)
                ContinuationIndent = C_CONTINUATION;
            if (LookAt(B, Line, 0, "{", hsC_Normal, 0)) {
///                I -= C_INDENT - C_BRACE_OFS;
                ContinuationIndent = 0;
            }
            PRINTF(("':' -- indent : Line=%d, RowP=%d, ColP=%d, CharP=%c\n", Line, RowP, ColP, CharP));
            return I + ContinuationIndent;
            
        case 'i':
        case 'f':
        case 'e':
        case 'w':
            I = B->LineIndented(RowP);
            switch (CharP) {
            case 'i': ColP += 2; break; // if
            case 'f': ColP += 3; break; // for
            case 'e': ColP += 4; break; // else
            case 'w': ColP += 5; break; // while
            }
            PRINTF(("'ifew' -- indent 1: Line=%d, RowP=%d, ColP=%d, CharP=%c\n", Line, RowP, ColP, CharP));
            if (SkipWhite(B, Line, RowP, ColP,
                          SKIP_FORWARD | (CharP != 'e' ? SKIP_MATCH : 0)) != 1)
                return 0;
            if (RowP >= Line) {
                RowP = Line;
                ColP = -1;
            } else
                ColP--;
            if (SkipWhite(B, Line, RowP, ColP, SKIP_BACK) != 1)
                return 0;
            ColP++;
            PRINTF(("'ifew' -- indent 2: Line=%d, RowP=%d, ColP=%d, CharP=%c\n", Line, RowP, ColP, CharP));

            if (((PrevRowP != RowP) ||
                 ((PrevRowP == RowP) && (PrevColP != ColP)))
                && FirstRowP != PrevRowP)
                ContinuationIndent = C_CONTINUATION;

            I += C_INDENT;

            if (LookAt(B, Line, 0, "{", hsC_Normal, 0)) {
                I -= C_INDENT - C_BRACE_OFS;
                ContinuationIndent = 0;
            }

            return I + ContinuationIndent;
            
        default:
            return 0;
        }
    }
    return 0;
}

static int IndentComment(EBuffer *B, int Row, int StateLen, hsState *StateMap) {
    int I = 0, R;
    
    //puts("Comment");

    if (Row > 0) {
	R = Row - 1;
	while (R >= 0) {
	    if (B->RLine(R)->Count == 0) R--;
	    else {
		I = B->LineIndented(R);
		break;
	    }
	}
        if (B->RLine(Row - 1)->StateE == hsC_Comment)
            if (LookAt(B, Row - 1, I, "/*", hsC_Comment, 0)) I += C_COMMENT_DELTA;
        if (B->RLine(Row - 1)->StateE == hsC_CPP_Comm)
            if (LookAt(B, Row - 1, I, "/*", hsC_CPP_Comm, 0)) I += C_COMMENT_DELTA;
    }
    return I;
}

static int IndentCPP(EBuffer *B, int Line, int StateLen, hsState *StateMap) {
    if (LookAt(B, Line, 0, "#", hsC_CPP, 0))
	return 0;
    else
	return C_INDENT;
}

int Indent_C(EBuffer *B, int Line, int PosCursor) {
    int I;
    hsState *StateMap = NULL;
    int StateLen = 0;
    int OI;

    OI = I = B->LineIndented(Line);
    if (Line == 0) {
        I = 0;
    } else {
        if (I != 0) B->IndentLine(Line, 0);
        if (B->GetMap(Line, &StateLen, &StateMap) == 0) return 0;
        
        switch (B->RLine(Line - 1)->StateE) {
        case hsC_Comment:
        case hsC_CPP_Comm:
            I = IndentComment(B, Line, StateLen, StateMap);
            break;
        case hsC_CPP:
        /*case hsC_CPP_Comm:*/
        case hsC_CPP_String1:
        case hsC_CPP_String2:
        case hsC_CPP_ABrace:
            I = C_INDENT;
            break;
        default:
            if (StateLen > 0) {                 // line is not empty
                if (StateMap[0] == hsC_CPP || StateMap[0] == hsC_CPP_Comm ||
                    StateMap[0] == hsC_CPP_String1 || StateMap[0] == hsC_CPP_String2 ||
                    StateMap[0] == hsC_CPP_ABrace)
                {
                    I = IndentCPP(B, Line, StateLen, 0);
                } else {
                    I = IndentNormal(B, Line, StateLen, StateMap);
                    if ((StateMap[0] == hsC_Comment || StateMap[0] == hsC_CommentL || StateMap[0] == hsC_CPP_Comm)
                        && ((LookAt(B, Line, 0, "/*", hsC_Comment, 0)
                             || LookAt(B, Line, 0, "/*", hsC_CPP_Comm, 0)
                             || LookAt(B, Line, 0, "//", hsC_CommentL, 0))))
                    {
                        I += C_COMMENT_OFS;
                    } else if (CheckLabel(B, Line)) {
                        if (LookAt(B, Line, 0, "case", hsC_Keyword) ||
                            LookAt(B, Line, 0, "default", hsC_Keyword) ||
                            LookAt(B, Line, 0, "public:", hsC_Keyword, 0) ||
                            LookAt(B, Line, 0, "private:", hsC_Keyword, 0) ||
                            LookAt(B, Line, 0, "protected:", hsC_Keyword, 0))
                            ;
                        else
                            I += C_COLON_OFS;
                    } //else if (LookAt(B, Line, 0, "{", hsC_Normal, 0)) {
                    //    I -= C_INDENT - C_BRACE_OFS;
                    //}
                }
            } else {
                I = IndentNormal(B, Line, 0, NULL);
            }
            break;
        }
    }
    if (StateMap)
        free(StateMap);
    if (I >= 0) 
	B->IndentLine(Line, I);
    else 
	I = 0;
    if (PosCursor == 1) {
	int X = B->CP.Col;

        X = X - OI + I;
        if (X < I) X = I;
        if (X < 0) X = 0;
	if (X > B->LineLen(Line)) {
            X = B->LineLen(Line);
	    if (X < I) X = I;
	}
        if (B->SetPosR(X, Line) == 0) return 0;
    } else if (PosCursor == 2) {
	if (B->SetPosR(I, Line) == 0) return 0;
    }
    return 1;
}
#endif
#endif
