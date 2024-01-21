#include <stdio.h>
#include <search.h>


struct info {	      /* this is the info stored in the table */
     int age, room; /* other than the key. */
};
#define NUM_EMPL    5000    /* # of elements in search table */
main( )
{
     /* space to store strings */
     char string_space[NUM_EMPL*20];
     /* space to store employee info */
     struct info info_space[NUM_EMPL];
     /* next avail space in string_space */
     char *str_ptr = string_space;
     /* next avail space in info_space */
     struct info *info_ptr = info_space;
     ENTRY item, *found_item, *hsearch( );
     /* name to look for in table */
     char name_to_find[30];
     int i = 0;
     /* create table */
     (void) hcreate(NUM_EMPL);
     while (scanf("%s%d%d", str_ptr, &info_ptr->age,
		      &info_ptr->room) !=
EOF && i++ <
NUM_EMPL) {
		    /* put info in structure, and structure in item */
		    item.key = str_ptr;
		    item.data = (char *)info_ptr;
		    str_ptr += strlen(str_ptr) + 1;
		    info_ptr++;
		    /* put item into table */
		    (void) hsearch(item,
ENTER);
     }
     /* access table */
     item.key = name_to_find;
     while (scanf("%s", item.key) != EOF) {
		   if ((found_item = hsearch(item,
FIND)) != NULL) {
		    /* if item is in the table */
		     (void)printf("found %s, age = %d, room = %d\n",
			 found_item->key,
			 ((struct info *)found_item->data)->age,
			 ((struct info *)found_item->data)->room);
		   } else {
		     (void)printf("no such employee %s\n",
			 name_to_find);
		   }
     }
}
