/*
 * Copyright Colten Edwards (c) 1996
 * BitchX help file system. 
 * When Chelp is called the help file is loaded from 
 * BitchX.help and saved. This file is never loaded from disk after this.
 * Information from the help file is loaded into an array as 0-Topic.
 * $help() also calls the same routines except this information is loaded 
 * differantly as 1-Topic. this allows us to distingush between them 
 * internally. 
 */
 
#include "irc.h"
#include "array.h"
#include "ircaux.h"
#include "chelp.h"
#include "output.h"
#include "hook.h"
#include "misc.h"
#include "vars.h"

int read_file _((FILE *help_file, int helpfunc));

char *get_help_topic(char *args, int helpfunc)
{
char *p, *new_comm = NULL;
char *subject = NULL;
int num = 0;
char *out = NULL;

	malloc_strcpy(&new_comm, args);
	p = &new_comm[strlen(new_comm)-1];
	while (*p && *p == ' ')
		*p-- = '\0';
	if ((p = strchr(new_comm, ' ')))
		*p = '$';

	malloc_sprintf(&subject, "%d-%s", helpfunc, new_comm);
	if ((num = atoi(function_numitems(subject))))
	{
		int i = 0;
		char *text;

		new_free(&subject);
		malloc_sprintf(&subject, "%d-%s %d", helpfunc, new_comm, i);
		text = function_getitem(subject);
		new_free(&text);
		if (helpfunc)
			malloc_strcpy(&out, empty_string);

		if (do_hook(HELPTOPIC_LIST, "%s", new_comm))
			put_it("\002%s\002: Help on Topic: \002%s\002", version, new_comm);
		
		for (i = 1; i < num; i++)
		{
			malloc_sprintf(&subject, "%d-%s %d", helpfunc, new_comm, i);
			text = function_getitem(subject);
			if (do_hook(HELPSUBJECT_LIST, "%s %s", new_comm, text))
				put_it("%s", convert_output_format(text, NULL));
			new_free(&text);
		}		
		new_free(&subject);
	} 
	else /*if (!helpfunc)*/
	{
		if (do_hook(HELPTOPIC_LIST, "%s", new_comm))
			bitchsay("No help on %s", new_comm);
	}
	new_free(&new_comm);
	return out;
}

void chelp _((char *command, char *args, char *subargs))
{
static int first_time = 1;
	if (first_time)
	{
		char *help_dir = NULL;
		FILE *help_file;
		malloc_sprintf(&help_dir, "%s", get_string_var(BITCHX_HELP_VAR));
		if (!(help_file = uzfopen(&help_dir, ".")))
		{
			new_free(&help_dir);
			return;
		}
		new_free(&help_dir);
		first_time = 0;
		read_file(help_file, 0);
		fclose(help_file);
	}	
	if (!args || !*args)
	{
		userage(command?command:"Bhelp", "<help\002|\002index\002|\002other>");
		return;
	}
	get_help_topic(args, 0);
}

int read_file(FILE *help_file, int helpfunc)
{
char line[BIG_BUFFER_SIZE + 1];
char *topic = NULL;
char *subject = NULL;
char *temp = NULL;
int item_number = 0;

	fgets(line, sizeof(line)-1, help_file);
	if (line)
		line[strlen(line)-1] = '\0';
	while (!feof(help_file))
	{
		if (!line || !*line)
		{
			fgets(line, sizeof(line)-1, help_file);
			continue;
		}
		else if (*line && (*line != ' '/* || *line != 0x10*/)) /* we got a topic copy to topic */
		{
			char *p;
			item_number = 0;
			new_free(&topic); new_free(&subject);
			malloc_strcpy(&topic, line);
			if ((p = strchr(topic, ' ')))
				*p = '$';
			malloc_sprintf(&temp, "%d-%s %d %s", helpfunc, topic, item_number++, line);
			function_setitem(temp);
			new_free(&temp);
			fgets(line, sizeof(line)-1, help_file);
			if (line)
				line[strlen(line)-1] = '\0';
		}
		else if (topic && *topic)
		{ /* we found the subject material */
			do {
				if (!line || (line && *line != ' '))
					break;
				malloc_sprintf(&temp, "%d-%s %d %s", helpfunc, topic, item_number++, line);
				function_setitem(temp);
				fgets(line, sizeof(line)-1, help_file);
				if (line)
					line[strlen(line)-1] = '\0';
				new_free(&temp);
			} while (!feof(help_file));
		}
	}

	return 0;
}
