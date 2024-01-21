/* GravityWars 1.1,  (C) Sami Niemi -95 */

/*------------------ TESTING OPTIONS ------------------------*/
#define SCORE 
#define ANIM
#define NOSOUND
/*#define TRAINER*/
/*-----------------------------------------------------------*/

#define  STEP 10            /* Bullet step */
#define  ACCEL 64           /* Bullet Accel */
#define  N_BULLETS 11       /* (Number of bullets)-1 */
#define  TIME_TO_LOAD 100   /* Bullets Loading Time */

#define  N_CEXCEPT 10       /* n of Exceptions */
#define  N_ANIM 30          /* n of Animations */

#define  N_ACTION 11        /* n of action frames +1 (?) */
#define  N_DESTROYEABLE 38  /* n of red walls (?) */

/* Object Map Definitions */

#define  E_NULL         '.'
#define  E_START        's'
#define  E_FUEL         'f'
#define  E_STOP         'x'
#define  E_WATER        'w'
#define  E_TOP_WATER    'v'
#define  E_AIR          'a'
#define  E_BONUS        '£'
#define  E_BONUS1       '1'
#define  E_BONUS2       '2'
#define  E_BONUS3       '3'
#define  E_BONUS4       '4'
#define  E_BONUS5       '5'
#define  E_BONUS6       '6'
#define  E_BONUS7       '7'
#define  E_BONUS8       '8'

#define  E_WBONUS       'q'
#define  E_WBONUS1      ']'
#define  E_WBONUS2      '('
#define  E_WBONUS3      ')'
#define  E_WBONUS4      '['

#define  E_XTIME        'T'
#define  E_XLIFE        'L'
#define  E_XFUEL        'F'
#define  E_WFUEL        '%'
#define  E_KEY          '&'
#define  E_WKEY         '?'
#define  E_WIND_Q       'Q'
#define  E_WIND_W       'W'
#define  E_WIND_E       'E'
#define  E_WIND_A       'A'
#define  E_WIND_D       'D'
#define  E_WIND_Z       'Z'
#define  E_WIND_X       'X'
#define  E_WIND_C       'C'

#define  L_RED_DOOR     '@'

/* Door Colors */ 

#define  DOOR1COLOR 192
#define  DOOR2COLOR 193
#define  DOOR3COLOR 194
#define  WATERCOLOR 195
#define  GREENCOLOR 196

/* Define Actions */ 
#define  EXPL_START   23
#define  EXPL_STOP    24
#define  SPLASH_START 25
#define  SPLASH_STOP  27

#define SMALLDELAY 30000 /* Beam Adjustment (?) */

/* 1 is fastest (?) */
#define ANIMSPEED 1 /* 1,3,7,15,.. */
#define ANIMSTEP  (ANIMSPEED+1)

#define  TRUE  1
#define  FALSE 0
#define  uchar unsigned char
  


