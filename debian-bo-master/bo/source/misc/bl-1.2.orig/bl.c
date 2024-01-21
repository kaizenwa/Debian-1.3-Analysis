/*  blinkenlights - blink keyboard lights 

    Copyright (C) 1994 - 1996  Greg Hankins <greg.hankins@cc.gatech.edu> 
        Scroll lock support (-S option):
    Copyright (C) 1994 Ed Doolittle <dolittle@math.toronto.edu>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <stdio.h>
#include <unistd.h>
#include <signal.h> 
#include <fcntl.h>
#include <linux/kd.h>
#include <linux/types.h>

/* change this to suit your keyboard */ 
#define MY_LEFT_LED 	LED_NUM
#define MY_MID_LED  	LED_CAP
#define MY_RIGHT_LED 	LED_SCR

#define BL_RIGHT 	0				
#define BL_LEFT 	1
#define BL_BOUNCE 	2
#define BL_CONVERGE 	3
#define BL_BLINK 	4
#define BL_SCRBLINK	5
#define BL_CAPBLINK	6
#define BL_NUMBLINK	7
#define BL_DELAY 	125000	

/* KDSETLED values: */
#define LED_NONE	0
/*	LED_SCR		1	
	LED_NUM 	2 */
#define LED_OUTER	(MY_LEFT_LED + MY_RIGHT_LED)	
/*	LED_CAP 	4 */
#define LED_RIGHT_TWO   (MY_MID_LED + MY_RIGHT_LED)	
#define LED_LEFT_TWO	(MY_LEFT_LED + MY_MID_LED)
#define LED_ALL		7

int ioctl();
char *strcat();
int strcmp();
int open();
int atoi();
void usage();
void right();
void left();
void converge();
void blink();
void show_warranty();
void numblink();
void capblink();
void scrblink();
time_t time();
int rand();
void srand();

unsigned char savedleds = 0; 	/* saved led states */
int ttyfd;			/* fd for the tty */

/* reset leds on signals */
void reset_leds(int signal)
{
	ioctl(ttyfd,KDSETLED,savedleds);
	close(ttyfd);
	exit(0);
}

/* reset num led only on signals */
void reset_num(int signal)
{
        ioctl(ttyfd,KDGETLED,&savedleds);
        ioctl(ttyfd,KDSETLED,savedleds&~LED_NUM);
        close(ttyfd);
        exit(0);
}

/* reset caps led only on signals */
void reset_caps(int signal)
{
        ioctl(ttyfd,KDGETLED,&savedleds);
        ioctl(ttyfd,KDSETLED,savedleds&~LED_CAP);
        close(ttyfd);
        exit(0);
}

/* reset scroll led only on signals */
void reset_scrl(int signal)
{
        ioctl(ttyfd,KDGETLED,&savedleds);
        ioctl(ttyfd,KDSETLED,savedleds&~LED_SCR);
        close(ttyfd);
        exit(0);
}

int main(int argc, char *argv[])		
{
	int dir;	/* direction we are going */
	int delay;	/* delay between blinks */
	char c;		/* char for getopt() */
	char tty[10] = "/dev/";	/* device name */
	extern char *optarg;	/* for getopt() */
        time_t now;	/* current time */

	/* check args and print out message if necessary */
	if(strcmp("-h",argv[argc - 1]) == 0 || strcmp("--help",argv[argc - 1]) == 0
           || argc == 1) usage(argv[0]);

	/* might have to show the warranty */
	if(strcmp("-w",argv[argc - 1]) == 0) show_warranty();

	/* try to open tty */
	strcat(tty,argv[argc - 1]);
	if((ttyfd = open(tty,O_RDWR)) < 0) {	
	       	fprintf(stderr,"opening keyboard %s: ",tty); 
		perror("");
	        usage(argv[0]);
	}

	/* set up defaults, signal handlers, and get the current led states */
	dir = BL_BOUNCE;
	delay = BL_DELAY;
	signal(SIGINT,reset_leds);
	signal(SIGQUIT,reset_leds);
	signal(SIGTERM,reset_leds);
	signal(SIGHUP,reset_leds);
	ioctl(ttyfd,KDGETLED,&savedleds);

	/* get options */
	while((c = getopt(argc,argv,"rlbkcd:NCS")) != -1)
		switch(c) {
		case 'd':
			/* set new delay */
			if(optarg != NULL) delay = atoi(optarg);
			else delay = BL_DELAY;
			break;
		case 'r':
			dir = BL_RIGHT;
			break;
		case 'l':
			dir = BL_LEFT;
			break;
		case 'b':
			dir = BL_BOUNCE;
			break;
		case 'c':
			dir = BL_CONVERGE;
			break;
		case 'k':
			dir = BL_BLINK;
			break;
                case 'N':
                        dir = BL_NUMBLINK;
                        break;
                case 'C':
                        dir = BL_CAPBLINK;
                        break;
                case 'S':
                        dir = BL_SCRBLINK;
                        break;
		default:
	        	usage(argv[0]);
			break;
		}

	/* call blinking functions */
	switch(dir) {
		case BL_RIGHT:
			while(1 > 0) right(delay);
			break;
		case BL_LEFT:
			while(1 > 0) left(delay);
			break;
		case BL_BOUNCE:
			while(1 > 0) {		
				right(delay);
				left(delay);
			}
			break;
		case BL_CONVERGE:
			while(1 > 0) converge(delay);
			break;
		case BL_BLINK:
        		/* get time and seed random number generator */ 
       			now = time(&now) / rand();
        		srand(getpid() + (int)((now >> 16) + now + time(&now)));

			while(1 > 0) blink(delay);
			break;
                case BL_NUMBLINK:
                        signal(SIGINT,reset_num);
                        signal(SIGQUIT,reset_num);
                        signal(SIGTERM,reset_num);
                        signal(SIGHUP,reset_num);
                        while(1 > 0) numblink(delay);
                        break;
                case BL_CAPBLINK:
                        signal(SIGINT,reset_caps);
                        signal(SIGQUIT,reset_caps);
                        signal(SIGTERM,reset_caps);
                        signal(SIGHUP,reset_caps);
                        while(1 > 0) capblink(delay);
                        break;
                case BL_SCRBLINK:
                        signal(SIGINT,reset_scrl);
                        signal(SIGQUIT,reset_scrl);
                        signal(SIGTERM,reset_scrl);
                        signal(SIGHUP,reset_scrl);
                        while(1 > 0) scrblink(delay);
                        break;
		default:
			exit(-1);
	}

	exit(0);
}

/* blink to the right */
void right(int delay)
{
	ioctl(ttyfd,KDSETLED,MY_LEFT_LED);
	usleep(delay);

	ioctl(ttyfd,KDSETLED,MY_MID_LED);
	usleep(delay);
	
	ioctl(ttyfd,KDSETLED,MY_RIGHT_LED);
	usleep(delay);
}

/* blink to the left */
void left(int delay)	
{
	ioctl(ttyfd,KDSETLED,MY_RIGHT_LED);
	usleep(delay);

	ioctl(ttyfd,KDSETLED,MY_MID_LED);
	usleep(delay);
	
	ioctl(ttyfd,KDSETLED,MY_LEFT_LED);
	usleep(delay);
}

/* blink randomly */
void blink(int delay)
{
    	int random;		/* random number */
	unsigned char ledmask;	/* led mask */
	
	/* make a random number between 1 and 7 */
	while((random=rand()) == 0);
	if (random % 7 == 0) random = 7;
	else random = random % 7;
	ledmask = (unsigned char)random;	

	/* set leds */
	ioctl(ttyfd,KDSETLED,ledmask);
	usleep(delay);	
}

/* converge to center - ie blink outer, then inner */
void converge(int delay)
{
	ioctl(ttyfd,KDSETLED,LED_OUTER);
	usleep(delay);

	ioctl(ttyfd,KDSETLED,MY_MID_LED);
	usleep(delay);
}

/* blink num lock only */
void numblink(int delay)
{
        ioctl(ttyfd,KDGETLED,&savedleds);
        ioctl(ttyfd,KDSETLED,savedleds^LED_NUM);
        usleep(delay);
}

/* blink caps lock only */
void capblink(int delay)
{
        ioctl(ttyfd,KDGETLED,&savedleds);
        ioctl(ttyfd,KDSETLED,savedleds^LED_CAP);
        usleep(delay);
}

/* blink scroll lock only */
void scrblink(int delay)
{
        ioctl(ttyfd,KDGETLED,&savedleds);
        ioctl(ttyfd,KDSETLED,savedleds^LED_SCR);
        usleep(delay);
}

/* print helpful information */
void usage(char *name)
{
	printf("blinkenlights version 1.2, Copyright (C) 1994 - 1996 Greg Hankins\n");
	printf("This is free software with ABSOLUTELY NO WARRANTY.\n");
	printf("For details, type '%s -w'.\n",name);
	printf("Usage: %s [-rlbckwNCS] [-d time] <device>\n",name);
	printf("-r	blink right\n");
	printf("-l	blink left\n");
	printf("-b	bounce (right, then left)\n");
	printf("-c	converge to center\n");
	printf("-k	blink all LEDs randomly\n");
        printf("-N      blink Num Lock (leaves other leds unchanged)\n");
        printf("-C      blink Caps Lock (leaves other leds unchanged)\n");
        printf("-S      blink Scroll Lock (leaves other leds unchanged)\n");
	printf("-d	delay value in microseconds (default is 125000)\n");
	printf("-h	prints out this message\n");
	printf("-w	prints out warranty\n");
	printf("<device> is the virtual console you are logged in on (without \"/dev/\")\n");
	printf("         or the virtual console that your X server is running on\n");
	exit(0);
}

void show_warranty()
{
	printf("blinkenlights version 1.2, Copyright (C) 1994 - 1996 Greg Hankins\n");
        printf("
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.\n
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.\n
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.\n\n");

	exit(0);
}
