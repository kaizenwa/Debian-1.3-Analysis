#inclue <stdio.h>
#include <Xm/XmAll.h>

int
main(int argc, char **argv)
{
    XmRepTypeList rep_types;
    int i, j;

#ifdef LESSTIF
    XmRegisterConverters(); /* necessary with LessTif */
#endif
    rep_types = XmRepTypeGetRegistered();

    fprintf(stderr, "done calling XmRepTypeGetRegistered()\n");

    printf("Representation Type Converters installed (Motif %i.%i.%i)\n",
        XmVERSION, XmREVISION, XmUPDATE_LEVEL);

    printf("\n-----------------\n");
    for(i = 0; rep_types[i].rep_type_name != NULL; i++)
    {
        printf("(%i) name: %s\n", i+1, rep_types[i].rep_type_name);
        printf("values:\n");
        for(j = 0; j < (int)rep_types[i].num_values; j++)
        {
            printf("\t%s", rep_types[i].value_names[j]);
            if(!((j+1) % 2))
                printf("\n");
        }
        printf("\n-----------------\n");
    }
    printf("Number of converters installed: %i\n", i);
    XtFree((char*)rep_types);
    exit(EXIT_SUCCESS);
}
