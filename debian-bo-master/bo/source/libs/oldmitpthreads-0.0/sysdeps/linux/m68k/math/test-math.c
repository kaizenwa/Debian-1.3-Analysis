#include <ansidecl.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

int
DEFUN_VOID(main)
{
  CONST char str[] = "123.456";
  double x,h,li,lr,a,lrr, y;

  x = atof (str);
	
  printf ("%g %g\n", x, pow (10.0, 3.0));
  
  x = 0.5;

  y = cos (x);
  printf("cos (%g) = %g\n", x, y);

  x = acos (y);
  printf("acos (%g) = %g\n", y, x);

  y = sin (x);
  printf("sin (%g) = %g\n", x, y);

  x = asin (y);
  printf("asin (%g) = %g\n", y, x);

  x = cosh(2.0);
  
  printf("cosh(2.0) = %g\n", x);
  
  printf("cosh(%g) = 2.0\n", acosh (x));
  
  x = sinh(2.0);
  
  printf("sinh(2.0) = %g\n", x);
  
  printf("sinh(%g) = 2.0\n", asinh (x));
  
  x = sinh(3.0);
  
  printf("sinh(3.0) = %g\n", x);
  
  h = hypot(2.0,3.0);
  
  printf("h=%g\n", h);
  
  a = atan2(3.0, 2.0);
  
  printf("atan2(3,2) = %g\n", a);
  
  lr = pow(h,4.0);
  
  printf("pow(%g,4.0) = %g\n", h, lr);

  printf("pow2(4.0) = %g\n", pow2 (4.0));
  
  lrr = lr;
  
  li = 4.0 * a;
  
  lr = lr / exp(a*5.0);
  
  printf("%g / exp(%g * 5) = %g\n", lrr, exp(a*5.0), lr);
  
  lrr = li;
  
  li += 5.0 * log(h);
  
  printf("%g + 5*log(%g) = %g\n", lrr, h, li);
  
  printf("cos(%g) = %g,  sin(%g) = %g\n", li, cos(li), li, sin(li));
  
  x = drem(10.3435,6.2831852);

  printf("drem(10.3435,6.2831852) = %g\n", x);

  x = drem(-10.3435,6.2831852);

  printf("drem(-10.3435,6.2831852) = %g\n", x);

  x = drem(-10.3435,-6.2831852);
	
  printf("drem(-10.3435,-6.2831852) = %g\n", x);

  x = drem(10.3435,-6.2831852);

  printf("drem(10.3435,-6.2831852) = %g\n", x);


  printf("x%8.6gx\n", .5);
  printf("x%-8.6gx\n", .5);
  printf("x%6.6gx\n", .5);

  {
    double x = atof ("-1e-17-");
    printf ("%g %c= %g %s!\n",
	    x,
	    x == -1e-17 ? '=' : '!',
	    -1e-17,
	    x == -1e-17 ? "Worked" : "Failed");
  }

  return 0;
}
