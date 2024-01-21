#define I_SYS
#define I_ERRNO
#define I_STRING
#define I_STRING
#include "includes.h"

void do_select_loop(int sock, int in_f, int out_f) {
  struct Buffer in, out;
  int i;


  in.start = in.end = in.size = in.alloced = 0;
  in.data = (un_char*) 0;
  out.start = out.end = out.size = out.alloced = 0;
  out.data = (un_char*) 0;

  add_to_buffer (&in, 0);
  get_from_buffer (&in );

  add_to_buffer (&out, 0);
  get_from_buffer (&out);  

  while (sock >= 0 || (out_f >=0 && out.size)) {
    fd_set rd, wr;
    int max;
    max = 0;
    FD_ZERO(&rd);
    FD_ZERO(&wr);

    if (sock >= 0 && in_f < 0 && ! in.size) {
      x__close(sock);
      sock = -1;
    }
    if (sock < 0) {
      if (in_f >= 0) {
        in_f = -1;
        in.start = in.end = in.size = 0;
      }
      if (! out.size) out_f = -1;
      if (out_f < 0) break;
    }

    if (in.size) {
      FD_SET(sock, &wr);
      if (sock > max) max = sock;
    } else if (in_f >= 0) {
      FD_SET(in_f, &rd);
      if (in_f > max) max = in_f;
    }
    
    if (out.size) {
      if (out_f >= 0) {
        FD_SET(out_f, &wr);
        if (out_f > max) max = out_f;
      }else {
        out.start = out.end = out.size = 0;
      }
    } else if (sock >= 0) {
      FD_SET(sock, &rd);
      if(sock > max) max = sock;
    }
    
    if (select(max+1, &rd, &wr, 0, 0) < 0 && errno == EINTR) continue;

    if (sock >= 0 && FD_ISSET(sock, &rd)) {
      i = read_into_buff(sock, &out, 0);
      if (i<1 && termerrno) {
        x__close(sock);
        sock = -1;
      }
    }
    if (sock >= 0 && FD_ISSET(sock, &wr)) {
      i = write_from_buff(sock, &in, 0);
      if (i<1 && termerrno) {
        x__close(sock);
        sock = -1;
      }
    }
    if (in_f >= 0 && sock >= 0 && FD_ISSET(in_f, &rd)) {
      i = read_into_buff(in_f, &in, 0);
      if (i<1 && termerrno) in_f = -1;
    }
    if (out_f >= 0 && FD_ISSET(out_f, &wr)) {
      i = write_from_buff(out_f, &out, 0);
      if (i <1 && termerrno) {
        out_f = -1;
        out.start = out.end = out.size = 0;
      }
    }
  }
  if (sock >= 0) x__close(sock);
}



