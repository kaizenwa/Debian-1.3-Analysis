
% a print function
define p(obj)
{
   print(string(obj));
   print("\n");
}


% calc.sl--- Init file for calc.  This file must be placed in the default
%  directory for calc and is automatically loaded when calc runs.
%
% This file contains S-Lang code for Newton's method, etc...
%
% Here is a function which computes the root of the equation y = f(x) using
% Newtons method.  The usage is:
%  
%   root = newton(s, &f);
%
% where s is a seed value and f is the function whose root is sought.
%
% For example, consider the function my_fun(x) = x^2 - 2 with solution 
% x = sqrt(2).  This function may be expressed in S-Lang as:
%
% define my_func(x)
% {
%   return (x * x - 2);
% }
%    
% To solve the equation my_fun(x) = 0 using the newton routine below, use
%
%     newton(5.0, &myfun);
%
% Here, I have randomly chosen 5.0 as an initial guess.   In addition,
% I have used the '&' operator to pass the function 'myfun' to the routine.


% Newton's method requires the derivative of a function.  Here is such a 
% function called by newton.  Given f(x), it returns df/dx at the point x.
%
% Its usage is:
%  
%    derivative(x, &f);

define derivative(x, f)
{
   variable dx;
   dx = 1.0e-4;        % small number
  
   return ((f(x + dx) - f(x - dx))/(2 * dx));
}

% And now the Newton's method:

define newton(x, f)
{
   variable err, max, dx;
   
   err = 1.0e-6;
   max = 1000;
   
   while (max)
     {
	--max;
	dx = f(x) / derivative(x, &f);
	if (abs(dx) < err)
	  {
	     return(x);
	  }
	
	x = x - dx;
     } 
   
   print ("\7Root not found.  Try another seed!\n");
   return(x);
}

   
   
%% This is a standard benchmark for interpreters.  It is a heavily
%% recursive routine that returns the nth Fibonacci number.  
%% It is defined recursively as:
%%
%%     f_0 = 0, f_1 = 1, .... , f_{n+1} = f_n + f_{n-1}, ...
%%
%%     or {0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, ...}
%%

define fib();               % required for recursion 

define fib(n)
{
   !if (n) return(0);
   --n;
   !if (n) return(1);
  
   fib(n) + fib( --n, n);   %Note that this expression parses to RPN
                            %  n fib --n n fib +
			    %and since --n does not change the stack, the
			    %effect is the same as the C comma operator.
}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Two routines which illustrate the how to deal with files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% type out a file to terminal
define type_file(file)
{
   variable fp, n;
   
   fp = fopen(file, "r");
   if (fp == -1)
     {
	print("\7File could not be opened!\n");
	return;
     }
   
   while (fgets(fp) > 0)
     {
	print(());     % characters on the stack so print them
     }
   
   !if (fclose(fp)) 
     {
	print("\7Error closing file!\n");
     }   
}


%
%  Here is a function that prints the number of lines in a file
%

define count_lines(f)
{
   variable fp, n, nchars, dn;
   
   fp = fopen(f, "r");
   if (fp < 0) error("Unable to open file!");
   n = 0; nchars = 0;
   
   while (dn = fgets(fp), dn > 0)
     {
	pop();     %/* do not care about the characters themselves! */
	++n;
	nchars += dn;
     }
   fclose(fp); pop();		       %/* ignore return value */
   
   print(Sprintf("File consists of %d characters and %d lines.\n",
		 nchars, n, 2));
}


% an apropos function
define apropos (what)
{
   variable n = slapropos(what);
   variable i, f1, f2, f3;

   if (n) print (Sprintf("found %d matches:\n", n, 1));
   else
     {
	print ("No matches.\n");
	return;
     }
   
   loop (n / 3) 
     {
	=f1; =f2; =f3;
	print (Sprintf("%-26s %-26s %s\n", f1, f2, f3, 3));
     }
   n = n mod 3;
   loop (n)
     {
	=f1;
	print (Sprintf("%-26s ", f1, 1));
     }
   if (n) print("\n");
}

%
#ifdef 0
_for (0, 99, 1) 
{
   =$1;
   Writable_array[$1] = $1;
}

_for (0, 99, 1) 
{
   =$1;
   p (Sprintf("Writable_array[$%d]\t=%d", $1, Writable_array[$1], 2));
}
#endif



%%% more help (called from calc.c)
define calc_help ()
{
   p("Additional functions:");
   p("  p();     -- displays top element of the stack (discarding it).");
   p("  quit();  -- quit calculator");
   p("  apropos(\"STRING\");  -- lists all objects containing \"STRING\"");
   p("\nExample: p (2.4 * E);    yields 6.52388.\n");
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
%    end of calc.sl     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
