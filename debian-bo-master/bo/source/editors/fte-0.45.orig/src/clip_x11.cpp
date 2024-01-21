/*    clip_x11.cpp
 *
 *    Copyright (c) 1994-1996, Marko Macek
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 */

#include "fte.h"

int GetXSelection(int *len, char **data);
int SetXSelection(int len, char *data);

int GetPMClip() {
    char *data;
    int len;
    int i,j, l, dx;
    EPoint P;
    
    if (GetXSelection(&len, &data) == 0) {
        SS->Clear();
        j = 0;
        l = 0;
        
	for (i = 0; i < len; i++) {
            if (data[i] == 0x0A) {
                SS->AssertLine(l);
                P.Col = 0; P.Row = l++;
                dx = 0;
                if ((i > 0) && (data[i-1] == 0x0D)) dx++;
                SS->InsertLine(P, i - j - dx, data + j);
                j = i + 1;
            } 
        }
	if (j < len) { // remainder
	    i = len;
	    SS->AssertLine(l);
	    P.Col = 0; P.Row = l++;
	    dx = 0;
	    if ((i > 0) && (data[i-1] == 0x0D)) dx++;
	    SS->InsText(P.Row, P.Col, i - j - dx, data + j);
	    j = i + 1;
	}
	free(data);
	return 1;
    }
    return 0;
}

int PutPMClip() {
    char *p = 0;
    int l = 0;
    PELine L;
    int Len;
    int rc;
    
    for (int i = 0; i < SS->RCount; i++) {
        L = SS->RLine(i);
        p = (char *)realloc(p, l + (Len = L->Count) + 1);
        memcpy(p + l, L->Chars, L->Count);
	l += Len;
	if (i < SS->RCount - 1)
	    p[l++] = '\n';
    }
    rc = (SetXSelection(l, p) == 0);
    free(p);
    return rc?1:0;
}
