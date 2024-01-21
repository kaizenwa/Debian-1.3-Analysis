/*
 *
 * test for _XmOSGetHomeDirName
 *
 */

#include <Xm/Xm.h>
#include <Xm/XmosP.h>
#include <stdio.h>

int
main(int argc,
     char **argv)
{
    printf ("Home directory = %s\n", _XmOSGetHomeDirName());
    exit(0);
}
