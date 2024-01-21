/* $Id: menu.h,v 1.2 1996/04/19 08:52:16 torsten Exp $ */

typedef void (*PaintMenuCallback) (Widget, Widget);

typedef struct PaintMenuItem_s {
    Widget widget;
    char *name;
    PaintMenuCallback callback;
    void *data;
    int flags;
    int nright;
    struct PaintMenuItem_s *right;
    Widget rightShell;
} PaintMenuItem;

#define MF_NONE         0x00
#define MF_CHECK        0x01
#define MF_CHECKON      (0x01|0x02)

#define MF_GROUP1	0x10
#define MF_GROUP2	0x20
#define MF_GROUP3	0x40
#define MF_GROUP4	0x80

typedef struct {
    Widget widget;
    char *name;
    int nitems;
    PaintMenuItem *items;
} PaintMenuBar;

extern Widget MenuBarCreate(Widget, int, PaintMenuBar[]);
extern Widget MenuPopupCreate(Widget, int, PaintMenuItem[]);
extern void MenuCheckItem(Widget, Boolean);

#define MI_NEW(name, flag, cb, cbdata, nright, right) \
  { None, name, (PaintMenuCallback)cb, (void*)cbdata, flag, nright, right, None }
#define MI_RIGHT(name, nright, right) \
  MI_NEW(name, MF_NONE, NULL, NULL, nright, right)
#define MI_FLAGCB(name, flag, cb, cbdata) \
  MI_NEW(name, flag, cb, cbdata, 0, NULL)
#define MI_FLAG(name, flag) \
  MI_FLAGCB(name, flag, NULL, NULL)
#define	MI_SIMPLECB(name, cb, cbdata) \
  MI_FLAGCB(name, MF_NONE, cb, cbdata)
#define	MI_SIMPLE(name) \
  MI_SIMPLECB(name, NULL, NULL)
#define	MI_SEPERATOR()					MI_SIMPLE("")
