#include <Vlib.h>
#include <stdio.h>

main()
{
	VObject	*obj1, *obj2;

	obj1 = VReadObject(stdin);
	obj2 = VCompressObject (obj1);
	VWriteObject (stdout, obj2);
	exit (0);
}
