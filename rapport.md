
Compilateur MiniC++ - Rapport de développement
==============================================

Projet d'Aurélien Delobelle et Élie Michel


## Organisation générale ##

L'organisation du projet suit le modèle donné par le cours :

 * Le lexeur est généré par `ocamllex` à partir du fichier `lexer.mll`
 * Le parseur est généré par `menhir` à partir du fichier `parser.mly`
 * L'arbre de syntaxe abstraite est défini dans `ast.ml`
 * Le fichier principal `main.ml` assemble tous les morceaux en ouvrant le fichier
   et affichant les éventuelles erreurs.

Le Makefile reprend celui fourni avec le tp mini-pascal et ajoute quelques détails
comme de la couleur, l'affichage de la page d'aide après la compilation, ainsi
qu'une condition pour l'inclusion de .depend afin d'éviter la recompilation
intempestive des .mll et .mly lors de l'appel avec la règle `clean`.

`ast.ml` est bien un fichier .ml et non un .mli car possède une ligne à exécuter,
pour le lexer hack.


## Nomenclature et choix de la langue ##

La documentation (affichée en présence de l'argument `--help`, bien qu'encore
très simple pour le moment) ainsi que les erreurs de compilation sont rédigés en
Anglais. Ce choix, bien que tout de même arbitraire, est motivé d'une part par
cohérence par rapport aux en-têtes d'erreurs où l'Anglais est requis et d'autre
part par la nécessité de s'entraîner à documenter dans cette langue.

Les identifiants utilisés dans le code sont également en Anglais, exception faite
des identifiants faisant écho à ceux définis dans la grammaire donnée en énoncé,
et ce afin de faciliter la compréhension du code.

Sauf erreur de notre part, les identifiants sont tous sous la forme `foo_bar`
(par opposition à `fooBar`, je ne connais pas le nom de la notation).


## Difficultés rencontrées ##

### Lexeur ###

Nous avons commencé par le lexer et seule subtilité rencontrée a été de vérifier
sa correction sur des exemples. En effet, créer une fonction `print_token` est
assez fastidieux, aussi l'avons nous générée à partir du texte et d'unt petite
expression régulière.

Nous avons de plus fait le choix de ne pas inclure le caractère de code ASCII 127
aux caractères autorisés dans les chaînes de caractères. Il y a à cela deux raisons :

 * Sélectionner les caractères de code entre 32 et 126 (inclus) peut se faire
   avec la syntaxe suivante : `[' '-'~']`. Ce n'est pas le cas pour ajouter le
   caractère de code 127 car celui-ci est le caractère `DEL` qui ne peut être
   représenté directement dans la syntaxe ocamllex.
 * Ce caractère n'étant pas représentable dans un fichier texte classique,  il n'a
   pas de raison d'apparaître dans un code C++. Il n'est donc pas préjudiciable de
   le retirer.

De plus, on pourrait prétendre que « entre 32 et 127 » ne précise pas si 32 et 127
doivent être inclus et il n'est donc pas rigoureusement faux de faire ce choix.


### Parseur et Arbre de syntaxe abstraite ###

L'ajout du parseur ne peut se faire sans l'ajout simultané de l'arbre de syntaxe
abstraite.

Nous avons repris pas à pas la décomposition de la grammaire MiniC++ donnée dans
l'énoncé en ajoutant d'une part les règles au parseur et le type correspondant
au module `Ast`. Il a tout de même fallu décomposer les disjonctions internes à
certaines expressions.

Nous avons commencé par utiliser pour les différentes composantes de l'arbre des
types produits. Le problème qui est survenu dès la première tentative d'exécution
est que deux types produits ne peuvent contenir de champs de même nom et que le
nommage devenait lourd. D'autant plus que -- sûrement d'ailleurs à cause d'une
erreur stratégique de notre part -- la première tentative d'exécution était assez
tardive et qu'il y a donc eu un grand nombre de types (et donc de règles) à modifier.

Les types utilisés sont donc désormais des tuples, ce qui évite toute considération
de nommage intempestive.

Le problème suivant était un peu plus subtile : le programme compilait, mais levait
*toujours* une erreur, sans exception. Cette exception, `Lexing_done` était en fait
la solution que l'on avait choisi pour signaller que l'analyse lexicale était
terminée mais n'était pas ratrappée par les cas d'erreurs du main (ce n'était ni
une `Parser.Error`, ni une `Lexer.Error`). Il suffisait en fait d'utiliser le token
`EOF` pour signaler au parseur la fin de cette analyse.

Nous avons hésité à indiquer les positions dans l'ast, mais ça surchargeait sans
être utilisé dans cette partie du projet. Nous attendons donc la suite pour le
faire.

Le seul sucre syntaxique appliqué par le parseur concerne l'opérateur `->`.
Je ne sais pas s'il y en a d'autres que l'on aurait éventuellement pu appliquer.


### Conflits Menhir ###

L'étape suivante : c'est la résolution des 105 conflits détectés par Menhir.
Oui, 105, car on n'avait indiqué aucune priorité... On a donc commençé par celles
données dans le sujet et le nombre de conflit a été fortement réduit.

Pour les conflits restant, il nous a fallu aller lire plus en détail la doc de
Menhir et la premier problème qu'elle régla fut celui des opérateurs, grâce au
mot clef `%inline`. Ce problème est en effet explicitement décrit dans la doc.

Un second problème commun avec la doc était celui des `if then else` imbriqués.
On a découvert pour l'occasion `%prefix` mais avont tout de même passé un certain
temps sur le conflit car ne pensions pas qu'il était possible de définir un nouveau
mot clef uniquement pour donner une priorité à une expression, sans qu'il ne soit
jamais généré par le lexeur. On a finalement ajouté le token `IFX` (cette notation
se retrouvait sur plusieurs sources sur Internet) spécialement pour donner une
priorité à la forme `IF expr THEN instr` par rapport au token `ELSE`.

Et enfin, le dernier et peut-être plus virulent conflit a été celui de `virtual`.  
En l'absence de ce mot clef, la distinction entre prototype et déclaration de
variable pouvait rester en suspend si on lisait un type puis un identifiant par
exemple. Je suis aller jusqu'à tenter de rendre ce morceau de grammaire L(1) dans
un égarement passager, puis nous avons finalement fait le lien avec le problème
des opérateurs et donc utilisé la encore `%inline` (ou plutôt `ioption`).

La lecture de la documentation a égélement permis d'utiliser les opérateurs comme
`separated_list` ou `preceded` que nous avions reproduit à la main et donc de façon
verbeuse et maladroite.


### Lexer hack ###

Le *lexer hack* est nécessite le partage d'une même structure de donnée entre
lexeur et parseur. La première idée était d'envoyer cette information à la fois
au lexeur et eu parseur en argument, mais nous ne sommes pas parvenu à passer de
paramètre au parseur (est-ce possible par l'intermédiaire de Menhir ?).

Nous avons donc exploité le fait qu'un module (ici `Ast`) peut être chargé dans
deux fichiers et donc permettre, en utilisant une structure jouant sur les effets
de bord comme une table de hachage, de partager des informations.

Une erreur que nous avions commise au début était de n'ajouter le nom d'une classe
à cette table qu'après avoir entièrement lu la description de celle-ci, ce qui
posait problème dans le cas d'une classe récursive. La règle du parseur concernant
la reconnaissance des déclarations de classe a donc été décomposée en deux afin
d'insérer du code dès que le nom de la classe a été lu.


### Tests finaux ###

Une fois que tout compilait sans aucune mise en garde, nous avons commencé les tests.
Ceux-ci étant nombreux, nous avons rédigé un rapide script shell (`test.sh`).
Nous avons ensuite constaté que sans affichage correct des erreurs, la vérification
était fastidieuse et nous nous sommes donc occupé de ce point.

Le mot clef `std::endl` présent dans les exemples de `test/exec/` censés fonctionner
nous a surpris, mais nous l'avons finalement ajouté au lexeur (puis au parseur)
afin de passer les tests avec succès et donc d'éviter de modifier les tests pour
vérifier s'il n'y avait pas d'autres erreurs. Nous avons découvert de cette façon
la différence entre `endl` et `"\n"`.


