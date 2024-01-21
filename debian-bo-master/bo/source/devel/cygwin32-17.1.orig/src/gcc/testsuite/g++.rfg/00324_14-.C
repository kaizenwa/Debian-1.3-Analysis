// gcc correctly diagnoses either a warning or (when -pedantic-errors) an error
// for this code... but g++ doesn't.

// Build don't link:

void (*func_ptr) ();
int i;
 
void
test ()
{
  func_ptr = i ? 0 : 0;			// ERROR - 
}
