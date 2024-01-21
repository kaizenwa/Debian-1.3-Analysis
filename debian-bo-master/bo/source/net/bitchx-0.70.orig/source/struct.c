
#include "irc.h"
#include "ircaux.h"
#include "alias.h"
#include "output.h"
#include <stddef.h>

#define CHAR 0
#define STR 1
#define INT 2
#define LONG 3
#define POINT 4

struct _dcc_struct_names {
	char *name;
	int  offset;
	int  type;
} dcc_struct_names[] = {
	{"USER", offsetof(DCC_list, user), STR}, 
	{"USERHOST",offsetof(DCC_list, userhost), STR}, 
	{"FLAGS",offsetof(DCC_list, userhost), INT}, 
	{"READ",offsetof(DCC_list, read), INT}, 
	{"WRITE",offsetof(DCC_list, write), INT},
	{"FILE",offsetof(DCC_list, file), INT}, 
	{"FILESIZE",offsetof(DCC_list, filesize), INT},
	{"DCCNUM",offsetof(DCC_list, dccnum), INT},
	{"DESCRIPTION",offsetof(DCC_list, description), STR},
	{"OTHERNAME",offsetof(DCC_list, othername), STR}, 
	{NULL, 0, 0}
	}, nicks_struct_names[] = {
	{"NICK", offsetof(NickList, nick), STR},
	{"HOST", offsetof(NickList, host), STR},
	{"SERVER", offsetof(NickList, server), STR},
	{"USERLIST", offsetof(NickList, userlist), POINT},
	{"SHITLIST", offsetof(NickList, shitlist), POINT},
	{"BOTLIST", offsetof(NickList, botlist), POINT},
	{"CHANOP", offsetof(NickList, chanop), INT},
	{"AWAY", offsetof(NickList, away), INT},
	{"VOICE", offsetof(NickList,voice),INT},
	{"IRCOP", offsetof(NickList,ircop),INT},
	{NULL, 0, 0},
	};

void update_structs(int todo, void * what)
{
#ifdef WANT_STRUCTS
char buffer[BIG_BUFFER_SIZE+1];
Alias *new_struct, *last = NULL, *tmp;
extern Alias *alias_list[];

	if (!todo)
	{
		for (tmp = alias_list[2]; tmp; tmp = tmp->next)
		{
			if (tmp->what == what)
			{
				if (last)
					last->next = tmp->next;
				else
					alias_list[2] = tmp->next;
				break;
			}
			last = tmp;
		}
		if (tmp)
			new_free((char **)&tmp);
	}
	else
	{
		new_struct = (Alias *)new_malloc(sizeof(Alias));
		new_struct->what = what;
		new_struct->struct_type = todo;
		if (!alias_list[2])
			alias_list[2] = new_struct;
		else
		{
			for (tmp = alias_list[2]; tmp; tmp = tmp->next)
			{
				if (!tmp->next)
					break;
			}
			tmp->next = new_struct;
		}
	}
#endif
}

void print_dcc_struct(DCC_list *tmp)
{
int i = 0;
void *_s = tmp;
char *s;

	for (i = 0; dcc_struct_names[i].name; i++)
	{
		switch(dcc_struct_names[i].type)
		{
			case INT:
				put_it("\t%s\t\t%d", dcc_struct_names[i].name, *(int **)((void *)(_s + dcc_struct_names[i].offset)) );
				break;
			case STR:
				s = *(char **)((void *)(_s + dcc_struct_names[i].offset));
				put_it("\t%s\t\t%s", dcc_struct_names[i].name, s ? s : "(null)");
				break;
			case LONG:
			case CHAR:
			default:
				break;
		}
	}
}

void print_nick_struct(NickList *tmp)
{
#ifdef WANT_STRUCTS
int i = 0;
void *_s = tmp;
char *s;
void *_t;

	for (i = 0; nicks_struct_names[i].name; i++)
	{
		switch(nicks_struct_names[i].type)
		{
			case INT:
				put_it("\t%s\t\t%d", nicks_struct_names[i].name, *(int **)((void *)(_s + nicks_struct_names[i].offset)) );
				break;
			case STR:
				s = *(char **)((void *)(_s + nicks_struct_names[i].offset));
				put_it("\t%s\t\t%s", nicks_struct_names[i].name, s ? s : "(null)");
				break;
#if 0
			case POINT:
				_t = *(void**)((void *)(_s +nicks_struct_names[i].offset));
				put_it("\t%s\t\t%p", nicks_struct_names[i].name, _t ? _t : "(null)");
				break;
#endif
			case LONG:
			case CHAR:
			default:
				break;
		}
	}
#endif
}

void print_structs(char *command, char *args, char *subargs)
{
#ifdef WANT_STRUCTS
Alias *tmp = NULL;
extern Alias *alias_list[];
	if (!&(alias_list[2]))
		return;
 	tmp = alias_list[2];
	while (tmp)
	{
		if (!tmp->what)
		{
			put_it("Something fucked up");
			break;
		}
		switch(tmp->struct_type)
		{
			case DCC_STRUCT:
				print_dcc_struct((DCC_list *)tmp->what);
			case NICK_STRUCT:
				print_nick_struct((NickList *)tmp->what);
			default:
				break;
		}
		tmp = tmp->next;
	}
#endif
}
