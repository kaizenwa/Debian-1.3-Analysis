/* GravityWars 1.1,  (C) Sami Niemi -95 */

/*--------------------------------------------------------------------- beep */
#ifndef NOSOUND
   #define beep(time) ioctl(1,KDMKTONE, 3276800+time);
#else
   #define beep(time) 
#endif


