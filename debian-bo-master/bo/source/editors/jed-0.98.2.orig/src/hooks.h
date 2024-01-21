/*
 *  Copyright (c) 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
extern int (*X_Read_Hook) (void);	 
extern int (*X_Input_Pending_Hook) (void);
extern void (*X_Get_Term_Size_Hook) (int *, int *);
extern void (*X_Suspend_Hook)(void);
extern int (*X_Argc_Argv_Hook)(int, char **);
extern int (*X_Init_SLang_Hook)(void);
extern int (*X_Init_Term_Hook) (void);
extern void (*X_Reset_Term_Hook) (void);
extern void (*X_Update_Open_Hook)(void);      /* hooks called when starting */
extern void (*X_Update_Close_Hook)(void);     /* and finishing update */
extern void (*X_Define_Keys_Hook) (SLKeyMap_List_Type *);

#ifdef HAS_MOUSE
extern int (*X_Open_Mouse_Hook)(void);
extern void (*X_Close_Mouse_Hook)(void);
extern int (*JMouse_Event_Hook)(void);
extern void (*JMouse_Hide_Mouse_Hook)(int);
#endif

