/*    h_sh.cpp
 *
 *    Copyright (c) 1994-1996, Marko Macek
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 */

#include "fte.h"

#ifdef CONFIG_HILIT_SH

#define hsSH_Normal    0
#define hsSH_SQuote    1
#define hsSH_DQuote    2
#define hsSH_BQuote    3
#define hsSH_DBQuote   4
#define hsSH_Control   5
#define hsSH_Keyword   6
#define hsSH_Comment   7
#define hsSH_Variable  8

int Hilit_SH(EBuffer *BF, int LN, PCell B, int Pos, int Width, ELine *Line, hlState &State, hsState *StateMap, int *ECol) {
    ChColor *Colors = BF->Mode->fColorize->Colors;
    HILIT_VARS(Colors[CLR_Normal], Line);
    int j;

    for (i = 0; i < Line->Count;) {
        IF_TAB() else {
            switch (State) {
            case hsSH_Normal:
                Color = Colors[CLR_Normal];
		if (isalpha(*p) || *p == '_') {
                    j = 0;
                    State = hsSH_Keyword;
		    while (((i + j) < Line->Count) &&
			   (isalnum(Line->Chars[i+j]) ||
			    (Line->Chars[i + j] == '_'))
			   ) j++;
		    if (BF->GetHilitWord(j, &Line->Chars[i], Color)) {
		    }
		    if (StateMap)
			memset(StateMap + i, State, j);
                    if (B) 
			MoveMem(B, C - Pos, Width, Line->Chars + i, Color, j);
		    i += j;
                    len -= j;
                    p += j;
                    C += j;
                    Color = Colors[CLR_Normal];
                    State = hsSH_Normal;
                    continue;
                } else if (*p == '\'') {
                    State = hsSH_SQuote;
                    Color = Colors[CLR_String];
                } else if (*p == '"') {
                    State = hsSH_DQuote;
                    Color = Colors[CLR_String];
                } else if (*p == '`') {
                    State = hsSH_BQuote;
                    Color = Colors[CLR_Command];
                } else if (*p == '$') {
                    State = hsSH_Variable;
                    Color = Colors[CLR_Variable];
                    ColorNext();
                    if (len > 0 && *p != '{' && *p != '[')
                        ColorNext();
                    while (len > 0 && (isalnum(*p) || *p == '_'))
                        ColorNext();
                    State = hsSH_Normal;
                    continue;
                } else if (*p == '#') {
                    State = hsSH_Comment;
                    Color = Colors[CLR_Comment];
                } else if (*p == ';' || *p == '|' || 
                           *p == '&' || *p == '!' ||
                           *p == '(' || *p == ')')
                {
                    State = hsSH_Control;
                    Color = Colors[CLR_Control];
                    ColorNext();
                    State = hsSH_Normal;
                    continue;
                }
                goto hilit;
            case hsSH_SQuote:
                Color = Colors[CLR_String];
                if ((len >= 2) && (*p == '\\')) {
                    goto hilit2;
                } else if (*p == '\'') {
                    ColorNext();
                    State = hsSH_Normal;
                    continue;
                }
                goto hilit;
            case hsSH_DQuote:
                Color = Colors[CLR_String];
                if ((len >= 2) && (*p == '\\')) {
                    goto hilit2;
                } else if (*p == '"') {
                    ColorNext();
                    State = hsSH_Normal;
                    continue;
                } else if (*p == '`') {
                    Color = Colors[CLR_Command];
                    State = hsSH_DBQuote;
                } 
                goto hilit;
            case hsSH_BQuote:
                Color = Colors[CLR_Command];
                if ((len >= 2) && (*p == '\\')) {
                    goto hilit2;
                } else if (*p == '`') {
                    ColorNext();
                    State = hsSH_Normal;
                    continue;
                }
                goto hilit;
            case hsSH_DBQuote:
                Color = Colors[CLR_Command];
                if ((len >= 2) && (*p == '\\')) {
                hilit2:
                    ColorNext();
                hilit:
                    ColorNext();
                    continue;
                } else if (*p == '`') {
                    ColorNext();
                    State = hsSH_DQuote;
                    Color = Colors[CLR_String];
                    continue;
                }
                goto hilit;
            case hsSH_Comment:
                Color = Colors[CLR_Comment];
                goto hilit;
            default:
                State = hsSH_Normal;
                Color = Colors[CLR_Normal];
                goto hilit;
            }
        }
    }
    if (State == hsSH_Comment)
        State = hsSH_Normal;
    *ECol = C;
    return 0;
}
#endif
