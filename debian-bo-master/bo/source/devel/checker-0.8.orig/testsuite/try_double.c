double sqr( v )
double v; 
{ 
  return v * v;
}

double GetV()
{
  return 123.0;
}

main()
{
  double y;

  y = 0;
  y += sqr( GetV() );
}



