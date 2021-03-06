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

Considérer un langage avec uniquement :

 * fonctions
 * additions
 * cout
 * variables


## 31/12/2013

J'ai récupéré le fichier mips.ml du td arithc pour simplifier la production de code.
Commencer par imprimer du texte n'est pas forécment la meilleure idée. Les entiers
auraient été un meilleur début pour pouvoir se consacrer directement à la compilation
des expressions sans perdre de temps à discerner les cas où l'on imprime du texte
de ceux où l'on imprime des nombres.

`Li` est modifié afin de prendre comme paramètre une chaîne et non un entier (pour
des questions de capacité)

J'ai maintenant un langage qui peut afficher des chaînes et le résultat d'opérations
binaires.

Ajoutons les variables… On commence par les globales. -> OK pour lecture et écriture

Les chaînes de caractères n'acceptent pas de caractères accentués mais l'erreur qui
en résulte est assez obscure.

## 01/01/2014

Gestion des boucles terminée. Il va falloir faire les blocs sinon l'intérêt des
boucles est limité. Le `while` a été « sucre syntaxiquisé » en `for (;test;)`.

Ajoutons maintenant les variables locales. Il faut les empiler à chaque fois qu'un
environnement est oublié.

Ça marche !

## 03/01/2014

Il faut régler les problèmes de conflits dans la déclaration des variables dans
le typeur.

Problème avec ce genre de code :

		int i = 1;
		{
			i = 2;
			int i = 3;
		}
		std::cout << i; // Affiche '1' au lieu de '2'


J'ai décidé d'abandonner la distinction variables globales / variables locales.

Ne pas oublier de vérifier l'inclusion de `iostream` pour autoriser `std::cout`.

Pb de priorité compéré à g++ : `cout << i = 42 << "\n"` génère une erreur car
`<<` est prioritaire sur `=`.

C'est bon pour les variables ! Passons aux fonctions.

Il faudra vérifier que le type de l'expression retournée est correct.

Beaucoup d'ennuis avec les structures persistantes et l'ordre d'évaluation des
opérandes.

**Faire attention en passant aux fonctions à plusieurs arguments !** Pour ce qui
est des fonctions à 1 argument, c'est bon.

Programme des étapes à venir :

 * Gérer les fonctions à plusieurs arguments
 * Ajouter les pointeurs
 * Gérer les passages par référence
 * Attaquer les objets :-°


## 07/01/2014

Fonctions à plusieurs arguments : Done.

Penser à harmoniser les messages d'erreur.

Ajout des opérateurs unaires. Les pointeurs sont maintenant gérés.

Faire attention aux autres types d'entier !! (octal et hexa)

J'ai géré les entités \x** dans les chaînes mais pour une raisons qui m'est
inconnue (peut-être à cause de mars) le caractère ESC (\x1b) ne fonctionne pas…
En fait la conversion en char d'ocaml n'utilise pas les mêmes conventions que
le shell.

## 08/01/2014

TODO : gérer typer la valeur des `return` -> OK

/!\ Écrire de l'arbre syntaxique à la main n'est pas une bonne idée : on oublie
de vérifier certains points.

TODO : Utiliser Format.sprintf pour les message d'erreur

Le type Env n'est pas pratique à modifier
Il aurait fallu faire un type enregistrement pour pouvoir ajouter des champs
sans avoir à modifier tout le code.


c'est bizarre, l'annoncé impose pas de vérifier que la valeur du return est bien typée.
Enfin je vais le faire quand même. -> Si en fait

Pour les appels de fonctions, les types doivent-ils être exactement égaux ou juste
être des sous-types ? Après tout, ce ne sont que des assignations.

J'ai commencé à me baser sur les exemples données. J'ai pu perfectionner les
références (le passage par référence plantait) ainsi que gérer des cas comme
l'absence de `return` auxquels je n'avais pas pensé. J'avais également oublié
l'évaluation paresseuse des booléens.

Doit-on gérer la surcharge des fonctions ? Je suppose que oui puisqu'on l'a pour les
méthodes.

Bon je préfère commencer les objets plutôt.

TODO : vérifier l'inclusion de iostream et la présence de `int main () {}`

## 09/01/2014

Astuce intéressante : les identifiants n'étant pas autorisés à contenir de points
par exemples, il n'est pas dangereux de donner un sens particulier à ce caractère
dans les clefs d'accès à l'environnement. Cela permet d'éviter les conflits entre
attributs et variables, et pourra aussi servir (en utilisant un autre caractère que
le point) à gérer la surcharge. J'ai été obligé d'utiliser un point plutôt que
`::` car les deux points sont interprétés par MIPS comme une fin de label. Cela
se répercute sur les messages d'erreur qui affichent `A.attr` plutôt que `A::attr`

Déclaration d'attributs ok, avec un code plus propre que le reste je trouve. Parce
que le reste du code du typer est vraiment mal organisé je trouve.

Est-ce que `class int` est autorisé ? -> Non

Pour le traitement des classes, je commence par une session de typing only, le temps
d'organiser les structures pour supporter les objets.

Mon typeur permet le prototypage des fonctions bien que le lexeur ne l'autorise pas.

Mes fonctions sont trop longues.

Il faudra penser à ajouter les constructeurs pour les classes qui ne le
spécifient pas.


Méga hack :

		int hack0() {
			std::cout << "";
		}
		
		void f() {
			std::cout << "0123456789-0123456789";
		}
		
		int hack1() {
			std::cout << "";
		}
		
		int main() {
			std::cout << "Length of \"";
			f();
			// Le -2 compense les deux caractères de fin de chaîne
			std::cout << "\": " << hack0() - hack1() - 2 << "\n";
		}


Registre particulier pour stocker `this` : T0

En fait je vais allouer les objets sur la pile, sauf pour le `new`
En fait non... Et c'est bon, les appels de membres fonctionnent (modulo la
saleté de l'implémentation)






