#ifndef HSEARCH_H
#define HSEARCH_H

typedef struct entry {
    char           *key;
    char           *data;
}               ENTRY;

typedef enum {
    FIND,
    ENTER
}               ACTION;

typedef struct element {
    ENTRY           item;
    struct element *next;
}               ELEMENT;

extern ENTRY   *hsearch();
extern void     hdestroy();
extern int      hcreate();
extern void     hprint();
extern void     htext();

#endif
