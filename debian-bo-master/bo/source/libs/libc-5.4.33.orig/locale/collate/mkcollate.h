typedef enum
  {
    range,
    primary_only,
    secondary
  } order_t;

typedef struct mkorder
  {
    struct mkorder *next;
    order_t node_type;
    unsigned char *low;
    unsigned char *top;
    struct mkorder *extra;
  } mkorder_t;

extern mkorder_t *order;

typedef struct mksub
  {
    struct mksub *next;
    unsigned char *repl;
    unsigned char *with;
  } mksub_t;

extern mksub_t *substitutions;
extern mksub_t *last_sub;
extern int nsubs;
