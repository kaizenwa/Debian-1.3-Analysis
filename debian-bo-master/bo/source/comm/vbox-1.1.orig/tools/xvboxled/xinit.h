/* Header file with declarations for xinit.c */

#ifndef _XINIT_H
#define _XINIT_H

extern void connect_server (char *displayname);
extern void disconnect_server (void);
extern int load_font (XFontStruct **font, char *fontname);

#endif