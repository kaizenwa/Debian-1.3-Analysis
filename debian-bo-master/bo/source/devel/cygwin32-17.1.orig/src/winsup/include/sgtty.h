#ifndef _SGTTY_H
#define _SGTTY_H

#include <termios.h>

#define TIOCGETP	0x1
#define TIOCSETP	0x2
#define TIOCGETC	0x3
#define TIOCSETC	0x4
#define TIOCGLTC	0x5
#define TIOCSLTC	0x6
#define TIOCLGET	0x7
#define TIOCLSET	0x8
#define TIOCFLUSH	0x9
#define TIOCSETN	0xa

struct sgttyb
{
    unsigned short sg_flags;
    char sg_ispeed;
    char sg_ospeed;
    char sg_erase;
    char sg_kill;
    struct termios t;

};

struct tchars
{
    char t_intrc;
    char t_quitc;
    char t_eofc;
    char t_startc;
    char t_stopc;
    char t_brkc;
};

struct ltchars
{
    char t_werasc;
    char t_suspc;
    char t_dsuspc;
    char t_rprntc;
    char t_flushc;
    char t_lnextc;
};



#endif
