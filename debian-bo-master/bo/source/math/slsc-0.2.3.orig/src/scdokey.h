extern void sc_dokey (void);
extern void sc_init_keymaps (void);
extern int Sc_Current_Mode;
extern char *Sc_Modes[];
extern SLKeyMap_List_Type *Sc_Main_Keymap;
extern int Sc_Mark_Set;

/* the function sc_update_indicators depends on these definitions! */
#define SC_ROW_MODE	0
#define SC_COLUMN_MODE	1
#define SC_BLOCK_MODE	2

/* Colors of objects--- first set is normal monochrome */
#define SC_NUMBER_COLOR 1
#define SC_STRING_COLOR 2
#define SC_NEEDS_COLOR 3
#define SC_FORMULA_COLOR 4
#define SC_MODE_COLOR 5

/* This set is inverse video on monochrome systems.  It begins with the 
 * region color. */
#define SC_REGION_COLOR 6
#define SC_LABEL_COLOR 7
#define SC_EXPR_COLOR 8
#define SC_MENU_COLOR 9
/* Cell color must ALWAYS be the last one */
#define SC_CELL_COLOR 10

extern void sc_update_indicators (void);
extern void sc_redraw (void);
extern void sc_update (int);
extern int Sc_Show_Exprs;
extern int sc_get_region (int *, int *, int *, int *);
extern SLang_RLine_Info_Type  *sc_init_readline (void);

extern char Slsc_Root_Dir[256];
extern char *Sc_Printer_String;

extern sc_setkey (int, SLcmd_Cmd_Table_Type *);
extern sc_unsetkey (int, SLcmd_Cmd_Table_Type *);
