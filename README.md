TRADUCTEUR - PROJET CERCLES²
============================

HOW TO USE
----------

Pour compiler : `make`
Pour nettoyer : `make clean`

L'executable porte le nom scade2b.
Pour traduire un projet, on donne en paramètre le répertoire du projet Scade :

    ./scade2b chemin/vers/projet

Les machines sont générées dans le repertoire `Machines_B`, dans le repertoire du projet.

NEW FEATURES
------------

  - Traduction du projet entier, et non d'un node individuel.
  - Création automatique de machines contenant les définitions de constantes.
  - Génération automatique des machines IMPORTS.

Tests
-----

### Lancer les tests

Il suffit de lancer `make check`. Si tout se passe bien, rien n'est affiché et
la commande réussit. Sinon, une différence entre le résultat attendu et effectif
est affichée au [format unified diff](https://www.gnu.org/software/diffutils/manual/html_node/Detailed-Unified.html).

L'exécution de la suite de tests crée plusieurs fichiers et répertoire: pour
chaque test, des fichiers `.ok` et `.diff` et un sous-répertoire `Machines_B`.
Ils sont supprimés en lançant `make check_clean`.

### Ajouter un test

Les tests considérés sont les sous-répertoires `tests/*.test/`. Ceux qui ne sont
pas de cette forme là ne sont pas lancés. Cela permet de prévoir des tests "à
l'avance" qui ne sont pas intégrés.

Chaque répertoire de test doit avoir 2 sous-répertoires: un répertoire `KCG`
contenant le code source SCADE (c'est-à-dire un fichier `kcg_trace.xml` et un
fichier `kcg_xml_filter_out.scade`), et un répertoire `spec` qui contient la
sortie attendue.

    tests/
        cas1/       # ignoré
        cas2.test/  # lancé
            KCG/                            # code source
                kcg_trace.xml
                kcg_xml_filter_out.scade
            spec/                           # sortie attendue
                M_Consts.mch
                M_Enum.mch
                M_Op_minus.mch
                M_Op_minus_i.imp
                M_Op_plus.mch
                M_Op_plus_i.imp

Pour créer un nouveau test, il suffit de copier la sortie du compilateur de
`Machines_B` à `spec`. Attention à:

  - relire la spécification pour s'assurer de la correction du test
  - ne pas placer du code non distribuable
  - vérifier que les fichiers ont été ajoutés (`git status`)

### Utiliser les outils de couverture

Il est possible d'utiliser [bisect](http://bisect.x9c.fr/) pour analyser la
couverture de code.

#### Installer bisect

Le plus simple est d'utiliser [OPAM](http://opam.ocamlpro.com/), en l'installant
par exemple via `aptitude install opam`. Faire ensuite:

    opam init
    opam switch 4.01.0
    opam install bisect

#### Utiliser bisect

Cela se passe en 3 étapes:

  - compiler le traducteur avec des instructions d'instrumentation
  - lancer la suite de tests
  - exécuter `bisect-report`

Celles-ci sont réalisées en invoquant `make -f Makefile.cov`. Le résultat est
ensuite visible dans `bisect-report`.

Attention, cela crée de nombreux fichiers temporaires dans le répertoire
courant. `ocamlbuild` permet d'en supprimer une partie via `_build/sanitize.sh`
mais il reste les `*.cmp` et `*.out`.
