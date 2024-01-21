#include <iostream.h>

#include <dld.h>

char* dyn_libraries[] = {
"/usr/lib/libg++.a",	/* link in libg++ */
"/usr/lib/libm.a",	/* link in libm */
"/usr/lib/libc.a",	/* link in libc */
LIBGCC,			/* link in libgcc */
0
};

main()
{
	cout<<"DLD function: "<<endl;
	dld_link("sub.o");
	dld_unlink_by_file("sub.o",0);
	cout<<"dyn_load: "<<endl;
	dyn_load("sub.o");
	cout<<"dyn_unload: "<<endl;
	dyn_unload("sub.o");
	cout<<"---"<<endl;
}
