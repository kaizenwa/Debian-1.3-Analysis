#-------La division Robotique de l'Institut de recherche d'Hydro-Qu�bec--------
# 
# Nom     : 
# Fonction: 
# Fichiers: Makefile
# Notes   : 
# 
# Cr��    : 18 juin 93 ---------- Martin Boyer <mboyer@ireq-robot.hydro.qc.ca>
# Modifi� : 31 d�cembre 94 ----4- Martin Boyer <mboyer@ireq-robot.hydro.qc.ca>
#           Copyright (c) 1993, 1994 Martin Boyer et Hydro-Qu�bec
# 
# Historique: 
#------------------------------------------------------------------------------

# Prendre soin de bien d�finir cette variable:
LIBDIR	= /depot/public/lib/ispell

# Il peut �tre requis de changer celle-ci aussi:
HASH = $(LIBDIR)/buildhash

# buildict n'est pas requis pour l'utilisation de francais-IREQ,
# mais si vous d�cidez de modifier le dictionnaire, la variable suivante
# devrait indiquer la chemin du programme buildict:
BUILDICT = ../../addons/buildict/buildict


########################################################################
# Normalement, rien de ce qui appara�t ci-dessous ne devrait �tre chang�
########################################################################

DICTS = Communs.dico Communs-V.dico Acronymes.dico \
	Propres.dico Universel.dico \
	Verbes-1.dico Verbes-1-VI.dico Verbes-1-NV.dico \
	Verbes-2.dico

DICTS_ANGLAIS = Acronyms.dict Universal.dict

BUILDICT_OPT = -vV
AFFIXES = ./francais.aff

all: francais.hash

# Pour la plupart des gens, ce fichier existe d�j�.
francais.dico: $(DICTS) $(DICTS_ANGLAIS) $(AFFIXES)
	csh -fc 'time $(BUILDICT) $(BUILDICT_OPT) -l $(AFFIXES) \
		 $(DICTS) \
		 $(DICTS_ANGLAIS) \
		 > $@'

francais.hash: francais.dico
	$(RM) francais.dico.cnt francais.dico.stat
	$(HASH) francais.dico $(AFFIXES) francais.hash

install: francais.hash
	install francais.hash $(LIBDIR)
	install $(AFFIXES) $(LIBDIR)

# Ne pas faire ceci sans buildict!
# francais.dico requiert buildict et d'importantes ressources informatiques
realclean: clean
	$(RM) francais.dico

clean:
	$(RM) francais.hash francais.dico.cnt francais.dico.stat
