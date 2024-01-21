/*
    XBlockOut a 3D Tetris

    Copyright (C) 1992,1993,1994  Thierry EXCOFFIER

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

    Contact: Thierry.EXCOFFIER@ligia.univ-lyon1.fr
*/
/*
 * Acces aux options d'un programme
 *
 * Vous devez :
 *	- Inclure ce fichier
 *	- Definir les options :
 *	  int verbose ;
 *	  char *fichierin ;
 *	  ...
 *	  struct options toto[] = { { "v"       , 'b' , (void*)&verbose,
 *				      "Affiche des messages d'explication" } ,
 *	                            { "infile"  , 's' , (void*)&fichierin,
 *				      "Nom du fichier en entree" } ,
 *				    ....
 *				    { "" , ' ' , 0 , "" }
 *				  } ;
 *
 *	  Les types sont les suivants :
 *		'b' int		Booleen (rien derriere)
 *		's' char*	Chaine de caracteres
 *		'i' int  	Entier ecrit apres  de type "int"
 *
 *	- L'appel : prendoptions( &toto , &argc , argv ) ;
 *	  Les options restantes sont retournees.
 *
 *	- Le texte de help permet de definir des bornes, il verifit :
 *	  .......... (choix_1,choix_2,choix_3) .....
 *        Le contenu de chaque groupe de parenthese trouve.
 *	  Les separateurs etant ' ' ',' '/' '|'
 *	  Ce qui est entre un separateur et un ':'
 *	  Pour un nombre #...# ou #-# decrit l'interval.
 *	  Par exemple :
 *		"Affichage du texte (gauche,droite,centre)"
 *		"g:Texte cadre a gauche, d:A droite et c:centre"
 *		"1-31: Jour du mois, 0: jour inconnu"
 *		"Jour d'ouverture (lun,mar,mer,jeu,ven,sam,dim,1-7)"
 *	  S'il y a aucun critere, aucun test n'est fait
 *
 */

#define DECALAGE_DROITE "                    "	/* Decalage a droite	*/
#define NB_DIGIT_MAX	10	/* Nombre Maxi de chiffre dans un entier*/
#define MAXLINE 132
#define V(X)		((void*)&(X))
#define NB_MAX_ARGS 100

#include <stdio.h>

#if HAVE_STDLIB_H
#include <stdlib.h>
#endif

#if HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

#ifdef HAVE_MALLOC_H
#include "malloc.h"
#endif


#include <ctype.h>

#include "ansi.h"
#include "options.h"

void prendoptions( t , argc , argv )
struct options *t ;
int *argc ;
char **argv ;
{
int i,j,l,nbo ;
char **wr ;
char *pc ;
int options_correctes() ;

wr = argv ;
nbo = *argc ;

for(i=1;i<nbo;i++)
   {
   pc = argv[i] ;
   if ( *pc=='-' ) pc++ ;	/* On saute le - */
   j = 0 ;
   while( t[j].nom[0]!='\0' )	/* Pour toutes les options possibles */
      {
      l = strlen( t[j].nom ) ;
      if ( strcmp( pc , t[j].nom ) == 0 )
         {
         switch( t[j].type )
            {
            case 'b' :
               *((int*)(t[j].valeur)) = 1 ;
	       (*argc)-- ;
               break ;
            case 'i' :
            case 's' :
	       /* Recherche l'affectation */
	       while( pc[l]!='\0' && pc[l]!=' ' && pc[l]!='=' && pc[l]!=':' )
			 l++ ;
	       if ( pc[l]!='\0' ) pc = pc+l+1 ;
		    else {
			/* Y a pas une affectation donc : 2 parametres */
	       		 if ( i==nbo-1 )
		   	     {
		             fprintf(stderr,"I need an arg after %s\n",
						argv[i]) ;
		   	     exit(1) ;
		   	     }
			 pc = argv[++i] ;
		         (*argc)-- ;
		         }

		/* Verification des valeurs autorisees */
		/* On recupere les valeurs autorisee dans le help */
		if ( !options_correctes( pc , t[j].help , t[j].nom ) )
		    {
		    proptions(t) ;
		    exit(1) ;
		    }

	       if ( t[j].type=='s' )
		  {
		  strcpy( (char*)t[j].valeur , pc ) ;
	          (*argc)-- ;
		  }
	       else
		  {
		  if ( sscanf(pc,"%d",(int*)(t[j].valeur)) != 1 )
			{
			fprintf(stderr,"<%s> must be an integer\n",
					t[j].nom) ;
		    	proptions(t) ;
		    	exit(1) ;
			}
	          (*argc)-- ;
		  }
	       break ;
	    default :
	       fprintf(stderr,"Option class '%c' unknown\n",t[j].type) ;
	       exit(1) ;
	       break ;
	    } /* Fin switch */
	break ;
        } /* Fin du if qui etait ok */
      j++ ; /* Passage au test suivant */
      }
	/* On n'a pas trouve l'option */
	/* On la conserve dans la liste */
      *wr++ = argv[i] ;
   }
}

void proptions( t )
struct options *t ;
{
int i,j ;			/* Pour les boucle			*/	
char *type ;			/* Indique le type courant		*/
char *valeur ;			/* Valeur courante			*/
char v[NB_DIGIT_MAX] ;		/* Pour stocker l'entier		*/
char buf[MAXLINE] ;		/* Pour un stockage temporaire		*/
int ldd ;			/* Longueur decalage droite		*/

ldd = strlen(DECALAGE_DROITE) ;

fprintf(stderr,"\nOptions are :\n\n") ;

for(i=0;t[i].nom[0]!='\0';i++)
	{
	switch(t[i].type)
		{
		case 'b' : type=""         ; break ;
		case 's' : type="<str>" ; break ;
		case 'i' : type="<int>"; break ;
		default  :
	       		fprintf(stderr,"Option type '%c' inconnu\n",t[i].type) ;
	       		exit(1) ;
	       		break ;
		}
	switch(t[i].type)
		{
		case 'b' :
			   if (*((int*)(t[i].valeur))) valeur="TRUE" ;
						  else valeur="FALSE" ;
			   break ;
		case 's' : valeur = (char*)(t[i].valeur) ;
			   break ;
		case 'i' : 
			   valeur=v ;
			   sprintf(v,"%d",*((int*)(t[i].valeur))) ;
			   break ;
		default  :
	       		fprintf(stderr,"Option type '%c' inconnu\n",t[i].type) ;
	       		exit(1) ;
	       		break ;
		}
	sprintf(buf,"-%s %s [%s]",t[i].nom,type,valeur) ;
	if ( strlen(buf) >= ldd )
	        fprintf(stderr,"%s\n%s",buf,DECALAGE_DROITE) ;
	   else fprintf(stderr,"%s%s",buf,&DECALAGE_DROITE[strlen(buf)]) ;

	valeur = t[i].help ;
	j = 0 ;
	while ( valeur[j]=='\n' || valeur[j]==' ' || valeur[j]=='\t' ) j++ ;
	for(;valeur[j]!='\0';j++)
		if ( valeur[j]=='\n' )
			{
			fprintf(stderr,"\n%s",DECALAGE_DROITE) ;
			while( valeur[j]=='\n' ||
				valeur[j]==' ' ||
				valeur[j]=='\t' ) j++ ;
			j-- ;
			}
		   else {
			fputc(valeur[j],stderr) ;
			}
	fputc('\n',stderr) ;
	}
}

int options_correctes(val,help,nom)
char *val ;
char *help ;
char *nom ;
{
int unchoix ;
int inpar ;
int debutmot ;
int chic ;
int l ;
int i ;
char *choixpossibles ;

unchoix = 0 ;	/* Il n'y a rien pour choisir		*/
inpar   = 0 ;	/* On est a l'exterieur de parenthese	*/
debutmot= 0 ;	/* Le help peut commence par un choix	*/
chic	= 0 ;	/* Passe a 1 si j'ai trouve		*/

l = strlen(val);
choixpossibles = (char*) malloc( 2*strlen(help)+1 ) ;
choixpossibles[0] = '\0' ;

for(i=0;help[i]!='\0';i++)
   {
   switch( help[i] )
	{
	case '(' :
	case '[' :
	case '{' :
		inpar++ ;
		debutmot = i+1 ;
		break ;
	case ')' :
	case ']' :
	case '}' :
		if ( inpar!=0 ) inpar-- ;
	case '|' :
	case ':' :
		strncat(choixpossibles,&help[debutmot],(size_t)(i-debutmot)) ;
		strcat(choixpossibles," ") ;
		if ( comparok(val,l,&help[debutmot]) )
			{
			chic = 1 ;
			break ;
			}
		unchoix = 1 ;
		debutmot = i+1 ;
		break ;
	case ' ' :
	case ',' :
	case '/' :
	case '\t' :
	case '\n' :
		if ( inpar!=0 )
			{
			strncat(choixpossibles,&help[debutmot],(size_t)(i-debutmot)) ;
			strcat(choixpossibles," ") ;
			if ( comparok(val,l,&help[debutmot]) )
				{
				chic = 1 ;
				break ;
				}
			unchoix = 1 ;
			}
		debutmot = i+1 ;
		break ;
	}
   }
if ( unchoix && chic==0 )
	{
	fprintf(stderr,"Possibles choices for <%s> are : %s\n",
			nom,choixpossibles) ;
	}
   else chic = 1 ;

free((void*)choixpossibles) ;
return(chic) ;
}

int comparok(v,lv,h)
char *v	;	/* Valeur a tester		*/
int lv ;	/* Longueur de "v"		*/
char *h ;	/* Debut de la chaine decriptive*/
{
int min,max,val ;

if ( strncmp( h,v,(size_t)lv ) == 0 ) return(1) ;
if ( strncmp( h,"...",3 ) == 0 ) return(1) ;

if ( sscanf(v,"%d",&val)!=1 ) return(0) ;
if ( sscanf(h,"%d%*[-.]%d",&min,&max) == 2 )
	{
	if ( val>=min && val<=max ) return(1) ;
			   else return(0) ;
	}

return(0) ;
}

void stringoption( o,s )
struct options *o ;
char *s ;
{
char *argv[NB_MAX_ARGS] ;
char *b ;
int i ;
int bet ;
int num ;
char *pc ;

i = 1 ;
bet = 0 ;

b = (char*) malloc( strlen(s)+NB_MAX_ARGS ) ;
pc = b ;
argv[0] = "XblResource" ;
argv[i] =  pc ;

while ( *s==' ' || *s=='\t' ) s++ ;
do
   {
   switch( *s )
	{
	case '\t' :
	case ' ' :
		if ( bet==1 )
			{
			*pc++ = *s ;
			break ;
			}
	case '\0' :
		while ( *s==' ' || *s=='\t' ) s++ ;
		if ( *s!='\0' ) s-- ;
		*pc++ = 0 ;
		argv[++i] =  pc ;
		if ( i>=NB_MAX_ARGS )
			{
			fprintf(stderr,"Too many args\n") ;
			exit(1) ;
			}
		break ;
	case '\\' :
		if ( isdigit(s[1]) )
			{
			s++ ;
			num = *s-'0' ;
			if ( isdigit(s[1]) )
				{
				s++ ;
				num = num*8+*s-'0' ;
				if ( isdigit(s[1]) )
					{
					s++ ;
					num = num*8+*s-'0' ;
					}
				}
			*pc++ = num ;
			break ;
			}
		if ( s[1]=='n' )
			{
			s++ ;
			*pc++ = '\n' ;
			break ;
			}
		if ( s[1]=='t' )
			{
			s++ ;
			*pc++ = '\t' ;
			break ;
			}
		if ( s[1]=='b' )
			{
			s++ ;
			*pc++ = '\b' ;
			break ;
			}
		*pc++ = s[1] ;
		if ( s[1]!='\0' ) s++ ;
		break ;
	case '"' :
		bet = 1-bet ;
		break ;
	default :
		*pc++ = *s ;
		break ;
	}
   }
while( *s++ ) ;
/*
printf("i=%d\n",i) ;
for(bet=0;bet<i;bet++) printf("<%s> ",argv[bet]) ;
printf("i=%d\n",i) ;
*/



prendoptions( o,&i,argv ) ;

free( (void*)b ) ;
}		
		


