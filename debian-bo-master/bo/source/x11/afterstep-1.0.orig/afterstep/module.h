#ifndef MODULE_H
#define MODULE_H

struct queue_buff_struct
{
  struct queue_buff_struct *next;
  unsigned long *data;
  int size;
  int done;
};

extern int npipes;
extern int *readPipes;
extern int *writePipes;
extern struct queue_buff_struct **pipeQueue;

#define START_FLAG 0xffffffff

#define M_TOGGLE_PAGING      (1<<0)
#define M_NEW_PAGE           (1<<1)
#define M_NEW_DESK           (1<<2)
#define M_ADD_WINDOW         (1<<3)
#define M_RAISE_WINDOW       (1<<4)
#define M_LOWER_WINDOW       (1<<5)
#define M_CONFIGURE_WINDOW   (1<<6)
#define M_FOCUS_CHANGE       (1<<7)
#define M_DESTROY_WINDOW     (1<<8)
#define M_ICONIFY            (1<<9)
#define M_DEICONIFY          (1<<10)
#define M_WINDOW_NAME        (1<<11)
#define M_ICON_NAME          (1<<12)
#define M_RES_CLASS          (1<<13)
#define M_RES_NAME           (1<<14)
#define M_END_WINDOWLIST     (1<<15)
#define M_ICON_LOCATION      (1<<16)
#define M_MAP                (1<<17)
#define M_SHADE		     (1<<18)
#define M_UNSHADE	     (1<<19)
#define M_LOCKONSEND         (1<<20)

#define MAX_MESSAGES          21
#define MAX_MASK             ((1<<MAX_MESSAGES)-1&~M_LOCKONSEND)

/* M_LOCKONSEND when set causes afterstep to wait for the module to send an
 * unlock message back, needless to say, we wouldn't want this on by default
 */

#define HEADER_SIZE         3
#define MAX_PACKET_SIZE    27
#define MAX_BODY_SIZE      (MAX_PACKET_SIZE - HEADER_SIZE)

#endif /* MODULE_H */

