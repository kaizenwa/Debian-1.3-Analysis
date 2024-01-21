/* Help-related definitions for Xconq.
   Copyright (C) 1991, 1992, 1993, 1994 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

enum nodeclass {
    miscnode,
    utypenode,
    mtypenode,
    ttypenode
};

typedef struct a_helpnode {
    char *key;
    void (*fn) PARAMS ((int arg, char *key, char *buf));
    enum nodeclass nclass;
    int arg;
    char *text;
    int textend;
    int textsize;
    struct a_helpnode *prev;
    struct a_helpnode *next;
} HelpNode;

extern HelpNode *first_help_node;

extern HelpNode *copying_help_node;
extern HelpNode *warranty_help_node;

extern void init_help PARAMS ((void));
extern HelpNode *create_help_node PARAMS ((void));
extern HelpNode *add_help_node PARAMS ((char *key, void (*fn)(int, char *, char *), int arg, HelpNode *prevnode));
extern HelpNode *find_help_node PARAMS ((HelpNode *node, char *str));
extern void create_game_help_nodes PARAMS ((void));
extern char *get_help_text PARAMS ((HelpNode *node));

extern void describe_topics PARAMS ((int arg, char *key, char *buf));
extern void describe_news PARAMS ((int arg, char *key, char *buf));
extern void describe_concepts PARAMS ((int arg, char *key, char *buf));
extern void describe_game_design PARAMS ((int arg, char *key, char *buf));
extern void describe_utype PARAMS ((int u, char *key, char *buf));
extern void describe_mtype PARAMS ((int m, char *key, char *buf));
extern void describe_ttype PARAMS ((int t, char *key, char *buf));
extern void describe_scorekeepers PARAMS ((int arg, char *key, char *buf));
extern void describe_setup PARAMS ((int arg, char *key, char *buf));
extern void describe_command PARAMS ((int ch, char *name, char *help, int onechar, char *buf));

extern int u_property_not_default PARAMS ((int (*fn)(int i), int dflt));
extern int t_property_not_default PARAMS ((int (*fn)(int i), int dflt));
extern int m_property_not_default PARAMS ((int (*fn)(int i), int dflt));
extern int uu_table_row_not_default PARAMS ((int u, int (*fn)(int i, int j), int dflt));
extern int ut_table_row_not_default PARAMS ((int u, int (*fn)(int i, int j), int dflt));
extern int um_table_row_not_default PARAMS ((int u, int (*fn)(int i, int j), int dflt));
extern void u_property_desc PARAMS ((char *buf, int (*fn)(int), void (*formatter)(char *, int)));
extern void t_property_desc PARAMS ((char *buf, int (*fn)(int), void (*formatter)(char *, int)));
extern void m_property_desc PARAMS ((char *buf, int (*fn)(int), void (*formatter)(char *, int)));
extern void uu_table_row_desc PARAMS ((char *buf, int u, int (*fn)(int, int), void (*formatter)(char *, int)));
extern void ut_table_row_desc PARAMS ((char *buf, int u, int (*fn)(int, int), void (*formatter)(char *, int)));
extern void um_table_row_desc PARAMS ((char *buf, int u, int (*fn)(int, int), void (*formatter)(char *, int)));
extern void append_number PARAMS ((char *buf, int value, int dflt));
extern void append_help_phrase PARAMS ((char *buf, char *phrase));
extern void append_notes PARAMS ((char *buf, Obj *notes));

extern void print_any_news PARAMS ((void));
extern void print_game_description_to_file PARAMS ((FILE *fp));

extern void describe_copyright PARAMS ((int arg, char *key, char *buf));
extern void describe_warranty PARAMS ((int arg, char *key, char *buf));
