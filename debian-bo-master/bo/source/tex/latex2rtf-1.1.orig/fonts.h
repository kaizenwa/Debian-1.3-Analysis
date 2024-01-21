/*
 * $Id: fonts.h,v 1.1 1994/06/17 11:26:29 ralf Exp $
 * History:
 * $Log: fonts.h,v $
 * Revision 1.1  1994/06/17  11:26:29  ralf
 * Initial revision
 *
 */

BOOL WriteFontHeader(FILE* fRtf);
BOOL SetFont(char *TexFont, FILE* fRtf);
int GetFontNumber(char * Fname);
void RemoveFontlist(void);
int GetTexFontNumber(char * Fname);
