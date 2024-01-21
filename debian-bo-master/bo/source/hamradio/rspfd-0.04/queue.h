/*
 * queue.h
 *
 * This modules handles all the nasty stuff that invloves dynamically allocated
 * queues or, more correctly, linked lists.  It is assumed that there will be
 * 'shell' functions above thses to handle the entry allocation and de-allocation
 *
 * Revision:
 * 05/03/95 cs  Created
 * 06/04/95 cs	Changed things around so qmark is changed in parameter list
 *		and value is returned
 * 05/05/95 cs  Added nukenode capabilities
 *
 */
#define KEY_SIZE	20
 
struct q_node {
	struct q_node *next;
	char key[KEY_SIZE];
	void *ent;
};

struct queue {
	struct q_node *head;
	struct q_node *tail;
};

typedef struct q_node* qmark;


struct queue *create_queue(void);

void add_qnode(struct queue *q, void *ent, char *key);
void del_qnode(struct queue *q, qmark qm, int nukenode);

void* qfind_next(struct queue *q, char *key, qmark *current);
void* qfind_first(struct queue *q, char *key, qmark *current);

void *q_entry(struct queue *q, qmark current);

void* qmove_first(struct queue *q, qmark *current);
void* qmove_last(struct queue *q, qmark *current);
void* qmove_next(struct queue *q, qmark *current);

void nuke_queue(struct queue *q, int nukenode);
