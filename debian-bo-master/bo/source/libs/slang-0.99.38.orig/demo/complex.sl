% Test of complex numbers 

define print (z_str)
{
   variable z = eval (z_str);
   fputs (Sprintf ("%s = %s\n", z_str, string (z), 2), stdout); pop ();
}

% Here is a function which returns the length and angle of z.  Note that it 
% uses the 'sign' function which has been overloaded to return a value which 
% indicates whether or not the number is in the upper complex plane.
define polar_form (z)
{
   variable x, y, r, theta;
   x = Real(z);
   y = Imag(z);
   r = abs (z);
   
   if (x == 0.0) theta = PI / 2;
   else	theta = atan (y / x);
   
   theta = sign (z) * theta;
   if (theta < 0) theta = 2 * PI + theta;

   return (r, theta);
}

   
   
variable z = Complex (3, 4);

print ("z");
print ("Real(z)");
print ("Imag(z)");
print ("Complex (polar_form(z)) %(r, theta)");
print ("Complex (polar_form(-z)) %(r, theta)");
print ("z");
print ("z + 2");
print ("z + 4.0");
print ("z + Complex(0, 1)");
print ("-z");
print ("z * z");
print ("z * 5 + 10");
print ("z / 3");
print ("z / Complex(2,2)");
print ("abs (z)");

variable w = z / 3;
print ("w");
print ("z + w");


