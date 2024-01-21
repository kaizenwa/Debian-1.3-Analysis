// Program     : x10-amh
// Version     : 1.05
// Description : X10 Model CP-290 control program (available at Radio Shack)
//               (computer control for X10 modules)
//               This application allows you to control lights and appliances
//               in your home either directly, or through a crontab
// Author      : Copyright (C) 1995 Aaron Hightower (aaron@paradigmsim.com)
//               1217 Shirley Way
//               Bedford, TX  76022 (817)267-6001
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

typedef struct {

  unsigned char id;
  unsigned char ev_num[2];
  unsigned char mode;
  unsigned char daymap;
  unsigned char hour;
  unsigned char minute;
  unsigned char devbm[2];
  unsigned char housecode;
  unsigned char lev_fun;
  unsigned char checksum;

} event;

class x10 {

  private:

    unsigned char events[128][8];

    int listvalid;
    int verbose;
    int hc_char;
    int hc;
    int fd;

  public:

     x10( int _fd );
    ~x10();

  private:

  public:

    int    wack           ( );
    int    wreport        ( );
    void   dump           ( );
    void   send_housecode ( );
    void   config_serial  ( );
    void   selftest       ( );
    void   settime        ( );
    void   gettime        ( );
    void   readtab        ( );
    void   listtab        ( );
    void   verbose_on     ( );
    void   emptytab       ( );
    void   writetab       ( int empty = 0 );
    void   readtab        ( const char *filename );
    void   printevent     ( int i );
    void   setEvent       ( int i, char housecode,
                            const char *devmap, const char *daymap,
                            int mode, int minute, int hour, int dimlevel,
                            int func );

    void   msg            ( u_char *msg, int len      );
    u_char checksum       ( u_char *bytes, int len    );
    void   direct         ( u_char command, char *str );
    void   setFD          ( int _fd                   );
    void   housecode      ( int _hc                   );

};
