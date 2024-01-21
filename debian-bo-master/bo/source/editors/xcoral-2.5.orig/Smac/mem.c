/* ########################################################################

				 mem.c

   File: mem.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/mem.c
   Description: 
   Created: Tue Feb 21 12:57:05 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:57:06 MET 1995
   Last maintained by: Bruno Pages

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Bruno Pages

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */



#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <memory.h>

#include "mem.h"
#include "word.h"
#include "error.h"

char * stringdup(s)
     char * s;
{
  char * result = malloc(strlen(s) + 1);

  if (result) {
    char * ps = result - 1;

    s = s - 1;
    while ((*++ps = *++s) != 0)
      ;
  }

  return result;
}

void * Malloc(n)
     int n;
{
  void * result = (void *) malloc(n);

  if ((! result) && n)
    Fatal_Error("no more memory");

  return result;
}

void * Calloc(n, s)
     int n;
     int s;
{
  void * result = (void *) calloc(n, s);

  if ((! result) && n)
    Fatal_Error("no more memory");

  return result;
}

char * Strdup(s)
     char * s;
{
  char * result = stringdup(s);

  if (! result)
    Fatal_Error("no more memory");

  return result;
}

#if RUNTIMECHECK

/* Version simple(iste ?) de malloc et free permettant de verifier les free
   et les acces ou modifications memoire par un test de borne.

   Pour verifier a 100% la legalite des frees et etre certain qu'il n'est
   pas possible de casser les informations liees aux allocations dynamiques
   par une erreur de programmation en smac, a cote du tas proprement dit qui
   est la seule zone atteignable pas l'utilisateur (la pile smac en fait
   parti) il y a un vecteur de <nbre_Objet_du_tas> caracteres. Si le caractere
   de rang i est inferieur a 32 (BIAS) mais non nul, cela veut dire que l'Objet
   de meme rang du tas est le debut d'une zone allouee de log a base 2 de
   la valeur du caractere blocs de 2 Objets (il y a de la marge !), sinon, s'il
   est superieur ou egal a 32 il indique une zone libre et sa taille apres
   avoir enleve le biais.
   
   Les bloc effectivements alloues font 2^x * sizeof(Objet) avec x >= 1
   Ceux-ci sont chaines entre eux, toujours pour etre certain qu'il ne soit
   pas possible de casser ces informations par une erreur de programmation en
   smac, les chainages ne sont pas memorises dans les blocs libres.
   */

char * Address_Min;
char * Address_Char_Max;
char * Address_Int_Max;
char * Address_Pointer_Max;

#define BIAS 32
  
#define IB_ENTRY_TO_HEAP(ibentry)		Heap[(ibentry) * 2]
#define HEAP_TO_IB_ENTRY(add)			(((add) - Heap) / 2)
#define NEXT_IB_ENTRY(ibentry, fbentry)		((ibentry) + (1 << (fbentry)))
#define PREVIOUS_IB_ENTRY(ibentry, fbentry)	((ibentry) - (1 << (fbentry)))


/* le pointeur vers le debut de la memoire dynamique */

static Object * Heap;


/* Le vecteur de validite & tailles des blocs alloues / libres */

static char * Info_Blocks;


/* le tableau des blocs libres chaines */

typedef struct _liste_blocs_libres {
  struct _liste_blocs_libres * next;
  Object * block;
} ListeBlocLibres;

static ListeBlocLibres ** Free_Blocks;


static int FB_Entry_Max;
static int IB_Entry_Max;


/**********
  
 Liberation

 *********/
  
static void remove_free_list(p, fb_index)
     Object * p;
     int fb_index;
{
  ListeBlocLibres ** rprev = &Free_Blocks[fb_index];

  while ((*rprev)->block != p)
    rprev = &(*rprev)->next;

  {
    ListeBlocLibres * l = *rprev;
    
    *rprev = l->next;
    free(l);
  }
}

static void internal_rtcfree_without_collapse(p, fb_index, ib_entry)
     Object * p;
     int fb_index;
     int ib_entry;
{
  ListeBlocLibres * l = (ListeBlocLibres *) malloc(sizeof(ListeBlocLibres));
						     
  l->next = Free_Blocks[fb_index];
  l->block = p;
  Free_Blocks[fb_index] = l;
  
  Info_Blocks[ib_entry] = fb_index + BIAS;
}

static void internal_rtcfree(p, fb_index)
     Object * p;
     int fb_index;
{
  int ib_entry = HEAP_TO_IB_ENTRY(p);
  
  for(;;) {
  
    if (fb_index) {
      int other_ib_entry = PREVIOUS_IB_ENTRY(ib_entry, fb_index);
      
      if ((other_ib_entry >= 0) &&
	  (Info_Blocks[other_ib_entry] == (fb_index + BIAS))) {
	
	/* raccordement avec le bloc precedant */
	
	p = &IB_ENTRY_TO_HEAP(other_ib_entry);
	remove_free_list(p, fb_index);
	Info_Blocks[other_ib_entry] += 1;
	Info_Blocks[ib_entry] = 0;
	fb_index += 1;
	ib_entry = other_ib_entry;
	continue;
      }
    }
    
    if (fb_index != FB_Entry_Max) {
      int other_ib_entry = NEXT_IB_ENTRY(ib_entry, fb_index);
      
      if ((other_ib_entry <= IB_Entry_Max) &&
	  (Info_Blocks[other_ib_entry] == (fb_index + BIAS))) {
	
	/* raccordement avec le bloc suivant */
	
	Info_Blocks[other_ib_entry] = 0;
	Info_Blocks[ib_entry] += 1;
	remove_free_list(&IB_ENTRY_TO_HEAP(other_ib_entry), fb_index);
	fb_index += 1;
	continue;
      }
    }
    
    break;
  }

  /* Ne peut pas raccorder le bloc libere a un autre */
    
  internal_rtcfree_without_collapse(p, fb_index, ib_entry);
}


void RTCFree(p)
     void * p;
{
  /* Verification de la validite de la liberation */
  
  if (! (((Object) p) & CHECK_POINTER_POINTER)) {
    int index = Info_Blocks[HEAP_TO_IB_ENTRY((Object *) p)];

    if (index && (index < BIAS)) {

      /* La liberation est correcte */
      
      internal_rtcfree((Object *) p, index - 1);
      return;
    }
  }

  sprintf(err_msg, "illegal free (address = 0x%lx)", (long) p);
  Error(err_msg);
}


/**********
  
 Allocation

 *********/

static Object * internal_rtcmalloc(index)
     int index;
{
  ListeBlocLibres * lresult;
  Object * block;

  if (index > FB_Entry_Max)
    return 0;
  
  if ((lresult = Free_Blocks[index]) != 0) {
    
    /* il y a un bloc libre */
    
    block = lresult->block;
    Free_Blocks[index] = lresult->next;
    free(lresult);
    Info_Blocks[HEAP_TO_IB_ENTRY(block)] = index + 1;

    return block;
  }

  /* n'a pas trouve de bloc libre de la plus petit
      taille possible, cherche la taille au dessus */
  
  block = internal_rtcmalloc(index + 1);

  if (! block)
    /* il n'y a vraiment rien */
    return 0;

  /* a un bloc 2*plus grand, on le coupe en 2 et rend la moitie inferieure */
  
  Info_Blocks[HEAP_TO_IB_ENTRY(block)] = index + 1;
  {
    /* Libere la moitie superieure */
    Object * dblock = (Object *) block + (2 << index);
    
    internal_rtcfree_without_collapse(dblock, index, HEAP_TO_IB_ENTRY(dblock));
  }
  
  return block;
}


static int size_to_fb_entry(size)
     int size;				/* size > 0 */
{
  int result = 0;
  Object allocsize = sizeof(Object) * 2;

  while (allocsize < size) {
    result += 1;
    allocsize <<= 1;
  }

  return result;
}

void * RTCMalloc(size)
     int size;
{
  return (size <= 0) ? 0 : (void *) internal_rtcmalloc(size_to_fb_entry(size));
}

void * RTCCalloc(n, size)
     int n;
     int size;
{
  if ((n <= 0) || (size <= 0))
    return 0;

  {
    Object * result;
    
    size *= n;
    result = internal_rtcmalloc(size_to_fb_entry(size));

    if (result)
      memset((char *) result, 0, size);

    return (void *) result;
  }
}

/* Pour empecher une liberation, utiliser pour ne pouvoir
   liberer des objets tels que la pile et variables globales */

void forbit_RTCfree(p)
     void * p;
{
  Info_Blocks[HEAP_TO_IB_ENTRY((Object *) p)] = 0;
}


/* Initialisation des allocations, le tas fait 2^log2size */

char * init_rtcmalloc(log2size)
     int log2size;
{
  Heap = (Object *) calloc((1 << log2size) / sizeof(Object), sizeof(Object));

  if (! Heap)
    return "Cannot allocate dynamic memory";

  FB_Entry_Max = size_to_fb_entry(1 << log2size);
  
  Free_Blocks = (ListeBlocLibres **)
    calloc(FB_Entry_Max + 1, sizeof(ListeBlocLibres *));

  if (! Free_Blocks) {
    free(Heap);
    return "Cannot allocate dynamic memory";
  }

  IB_Entry_Max = (1 << log2size) / sizeof(Object) / 2 - 1;
  Info_Blocks = calloc(IB_Entry_Max + 1, 1);

  if (! Info_Blocks) {
    free(Free_Blocks);
    free(Heap);
    return "Cannot allocate dynamic memory";
  }


  Address_Min = (char *) Heap;			/* premiere @ valide */
  Address_Char_Max =				/* dernier &char valide */
     ((char *) Heap) + (1 << log2size) -1;
  Address_Int_Max =				/* dernier &int valide */
     ((char *) Heap) + (1 << log2size) - sizeof(int);
  Address_Pointer_Max =				/* dernier &pointeur valide */
     ((char *) Heap) + (1 << log2size) - sizeof(char *);


  {
    ListeBlocLibres * l = (ListeBlocLibres *) malloc(sizeof(ListeBlocLibres));
						     
    Free_Blocks[FB_Entry_Max] = l;
    l->next = 0;
    l->block = Heap;
  }
  Info_Blocks[0] = FB_Entry_Max + BIAS;

  return 0;
}


#if DEBUGRTCALLOC

#define fb_entry_to_size(entry)	((1 << (entry)) * sizeof(Object) * 2)

static void aff_infos()
{
  int index;

  for (index = 0; index <= IB_Entry_Max; index += 1)
    if (Info_Blocks[index] >= BIAS) {
      ListeBlocLibres * l;
      Object * add;
      
      printf("(%d %d) bloc libre de %d a partir de %x : ",
	     index, Info_Blocks[index],
	     fb_entry_to_size(Info_Blocks[index] - BIAS),
	     add = &IB_ENTRY_TO_HEAP(index));
      for (l = Free_Blocks[Info_Blocks[index] - BIAS]; l; l = l->next)
	if (l->block == add) {
	  printf("ok\n");
	  break;
	}
      if (! l)
	printf("ko\n");
    }
    else if (Info_Blocks[index]) {
      ListeBlocLibres * l;
      Object * add;
      
      printf("(%d %d) bloc alloue de %d a partir de %x : ",
	     index, Info_Blocks[index],
	     fb_entry_to_size(Info_Blocks[index] - 1),
	     add = &IB_ENTRY_TO_HEAP(index));
      for (l = Free_Blocks[Info_Blocks[index] - 1]; l; l = l->next)
	if (l->block == add) {
	  printf("ko\n");
	  break;
	}
      if (! l)
	printf("ok\n");
    }
}

#endif

#endif
