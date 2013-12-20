# MiniC++ compiler
Studying project for
	L3S1 - Langages de programmation et compilation
	JC Filliâtre
	ENS Paris
by Elie Michel

## 27/10/2013 (Day 1)
### News
+lexer.mll
+parser.ml - Just for `token` type and formating function
+test.cpp
+Makefile
+main.ml

Makefile OK (just add further files to $CMX)
Lexer OK (tested on several examples)

### TODO
Add position information for precise errors
Format errors for emacs



## 27/10/2013 (Day 2)
### News
Le projet utilise désormais Git
Le Makefile a été refait (enfin copié sur celui de sysdig)
Début du parser
Ajout de lexemes oubliés

Parser écrit, ainsi que les types de l'arbre de syntaxe abstraite, mais il reste
des conflits à régler

## 19/11/2013
« IF (expr) IF (expr) instr ELSE instr » est ambigu.
différencier proto et decl_vars
Parser fonctionnel, reste à gérer l'affichage des erreurs

## 24/11/2013
Messages d'erreur formatés
Paramètre --parse-only ajouté
Ajouter la position dans l'ast

AST est un .ml pour pouvoir y définir la table de lexing hack
Caractère DEL








## 20/12/2013
*Partie 2*

J'ai tout d'abord pensé faire tout le typage puis toute la production de code,
mais en faisant ça j'ai du mal à me rendre compte des meilleurs structures à
utiliser. J'ai également pensé au début pouvoir ajouter les indications de
position a posteriori, mais jhj me l'a déconseillé et en effet, il est
intéressant d'ajouter ces annotations à l'arbre au fur et à mesure que l'on écrit
les messages d'erreur. Cela permet d'annoter uniquement les nœuds qui le
nécessitent et de visualiser la progression du typeur.

J'ai finalement fait un premier brouillon (très moche sur la prod de code) d'une
« tranche » du compilateur plutôt que de travailler par couche.

Je ne sais pas encore si c'est une bonne idée, mais j'ai utilisé le même type
pour le typage des expressions (NULL, Pointer, Int, Class) et pour les associations
dans l'environnement (Function, HasAttr).

Voir plus précisemment comment fonctionnent les références (en arguments, variable
locale et retour de fonction).





