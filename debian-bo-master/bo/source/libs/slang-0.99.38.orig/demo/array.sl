% Used by the array demo.

define init_array (a, n)
{
   variable i;
   for (i = 0; i < n; i++) a[i] = i;
}

define sum_elements (a, n)
{
   variable sum = 0.0;
   variable i;
   
   for (i = 0; i < n; i++) sum = sum + a[i];
   return sum;
}

% The C array is called Vector. */
init_array (Vector, 100);

% This will do a formatted print--- if you understand it, you understand 
% S-Lang
define printf (n)
{
   variable str = Sprintf (n);
   fputs (str, stdout); pop ();
}

printf ("Sum of elements is: %f\n", sum_elements (Vector, 100), 1);
quit ();


