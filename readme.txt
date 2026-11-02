+-----------------------------------------------------------------------+
    Projet vérification de protocoles
+-----------------------------------------------------------------------+


Compilation
+------------------------+
Il vous faudra avoir installé ocaml, dune, menhir.
Tout ceci s'installe très facilement par opam.
Les instructions générales pour installer ocaml sont ici https://ocaml.org/install.

Ceci fait, vous pouvez compiler le projet en lançant

  make

dans le dossier principal (où se trouve ce readme).
Ceci placera l'exécutable compilé, qui s'appelle `pif`, dans ce dossier.


Une commande

  make clean

est fournie également pour retirer tous les fichiers générés par la compilation.
Exécutez-la avant de rendre votre code pour ne rendre que les sources.



Utilisation
+------------------------+

Après compilation, lancer

  ./pif <chemin du fichier contenant les règles>

pour traiter un fichier d'entrée.
Le programme lira le fichier, puis attendra des requêtes de l'utilisateur sur l'entrée standard, de la forme

  att(<message dont on veut vérifier le secret>)

en boucle, jusqu'à ce qu'on lui envoie un signal End-Of-File (ctrl-D).



Organisation du code
+------------------------+

Le dossier `src/` contient tous les fichiers source nécessaires :
- `main.ml(i)` : fichier principal, que vous devrez modifier pour appeler vos fonctions
- `lexer.mll`/`parser.mly` : fichiers pour le lexer et parser
- `syntax.ml(i)` : définitions des termes, substitutions, atomes, règles, et quelques fonctions utiles
- `read.ml(i)` : code gérant l'interaction avec l'utilisateur (lecture des fichiers, de l'entrée standard, messages d'erreur)
- `saturation.ml` : vide pour l'instant, destiné à votre code.

L'idée est que vous ajoutiez votre code dans le fichier `saturation.ml`, mais vous pouvez faire autrement si vous préférez (par exemple si vous voulez séparer votre code en plusieurs fichiers).

Si vous modifiez des fichiers autres que `saturation.ml`, merci de l'indiquer.


Ne touchez pas aux fichiers `pif.ml`, `dune`, `dune-project`, `pif.opam`
dans le dossier principal : ils servent à dune pour la compilation.

Fichiers d'entrée
+------------------------+
Le dossier `examples/` contient les quatre fichiers d'entrée mentionnés dans le sujet.
Les commentaires dans ces fichiers expliquent ce qu'ils modélisent, et le résultat qu'on attend.

Si vous voulez modifier ces fichiers pour faire des tests, ou en ajouter d'autres : la syntaxe complète est bien sûr décrite dans `lexer.mll` et `parser.mly`, mais elle est simple à comprendre en lisant les fichiers d'exemples.

Notez tout de même que les variables doivent commencer par une majuscule, et les symboles de fonction ou de constantes par une minuscule.



