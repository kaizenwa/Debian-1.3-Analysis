/* xb_config.h - some configuration things */

/* program native class */
#define PRGCLASS "XBanner"

/* 
   if you really want to compile under X11R4 you must uncomment the
   following line. If you are compiling under old OpenVMS, you might also
   need to.
*/
/* #define R4_RESOURCES */

/* on slow machines this might speed things up a bit */
#define EXPLICIT_REGISTER_VARIABLES

/* if you have no usleep() comment out the following line */
#define HAS_USLEEP

/*
   if you have no strcasecmp (=strcmpi) comment out the following line
   (OpenVMS needs to comment it, Ultrix also)
*/
#define HAS_STRCMPI

/*
   if you want much faster PlasmaCloud (about 40% faster) use integer
   arithmetic PlasmaCloud. This is great for most steups of 2 main colors
   where the overall brightness of one main color isn't much different
   than the overall brightness of the other. For example, making clouds
   with -grad1 deepskyblue,white looks great in integer arithmetic,
   but -grad1 white,darkblue looks horrible. Also -grad1 red,green,blue
   looks bad.
*/
#define INT_PLASMA

/*
   Definition of a floating-point random function.
   
   If your system has a function such as Linux's drand48() which takes
   no args, returns a double-precision floating point number in the range
   0.0 - 1.0, then define it here for the PlasmaCloud effect.
   
   Linux users need do nothing. If you are unsure, leave commented-out.

   The example is for Linux, so adapt it to your system:
   
   #define OTHER_FRAND() drand48()
*/
/*#define OTHER_FRAND() your_drand_function()*/
