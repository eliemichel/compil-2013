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




