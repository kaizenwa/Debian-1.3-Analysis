/*  VER 006  TAB P   $Id: sock.h,v 1.3 1996/11/22 12:31:52 src Exp $
 *
 *  descriptor for an open socket connection 
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  the GNU General Public License applies
 */

typedef struct socket_d {
    int r_fd;
    int w_fd;
    FILE *r_str;
    FILE *w_str;
    int is_telnet;
    int is_echo;
    int chat_mode;
    FILE *f_chat;
} SOCKET_D;
