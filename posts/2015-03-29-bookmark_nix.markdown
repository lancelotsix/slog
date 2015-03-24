---
title: Pense bete pour développer Nix
author: Lancelot SIX
license: cc-by
illustration: /images/nixos.png
---
[Nixos](http://nixos.org) est un système d'exploitation basé sur le gestionnaire de paquets [Nix](http://nixos.org/nix). Ce dernier a été développé dans le prolongement des travaux de thèse de Eelco Dolstra. Comme quoi, des fois les thèses, ça mène à quelque chose (au moins pour la science).

<!--more-->

Nix est un système de gestion de paquets pur, dans le sens entendu par les langages de programmation. Il s'agit donc d'un gestionnaire de paquets sans effets de bord. Chaque paquet résulte de l'évaluation d'une fonction (ou expression), et ne dépend que de cette dernière. Du moment où les entrées données à deux évaluations de la fonction sont identiques, le résultat produit est le même. Chaque paquet est installé dans un dossier qui lui est propre, et dont le nom est donné par un hash des paramètres données en argument de la fonction ayant servi à le générer. Ainsi, deux expressions construisant le même paquet, à l'exception d'un paramètre de configuration, seront installés sous des préfixes distincts. La conséquence de ceci est qu'il est possible d'installer simultanément différents logiciels dépendant de différentes versions d'une même librairie.

Assez parlé des Nix, regardons ce qui m'amène. Il peut être déroutant de travailler un tel environnent, voici pourquoi je me permet cette publication «pense-bête».

## Installer un programme le temps de l'utiliser

Par exemple, si je ne souhaite pas garder `imagemagick` installé en permanence dans mon système (soyons honnête, ça ne me sert pas toues les jours), mais que j'ai ai besoin temporairement.... Voilà ma solution:

```bash
$ nix-shell -p imagemagick
[nix-shell:~/dev/llog/images]$ convert nixos-logo-only-hires.png -rexize 64x64 nixos.png
```

Cette approche est également très pratique pour teste le super logiciel de fou dont on viens d'entendre parler, mais qu'on ne lancera probablement plus de deux fois.

## Fichier de développement par projet

Avec Nix, il est possible d'écrire un descripteur par projet (fichier `default.nix`) permettant de décrire les dépendences devant être disponibles durant le développement. Plus besoin d'installer toute une suite d'outils et de polluer son système pour travailler.

Voici le descripteur que j'utilise pour travailler sur ce blog (et le compiler / déployer):

```nix
{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
let
  inherit (haskellPackages) cabal cabalInstall
    hakyll systemFilepath dataDefaultInstancesOldLocale; # Haskell dependencies here

in cabal.mkDerivation (self: {
  pname = "llog";
  version = "1.0.0";
  src = ./.;
  buildDepends = [
    # As imported above
    hakyll systemFilepath
  ];
  buildTools = [ cabalInstall ];
  enableSplitObjs = false;
})

```

Lancer

```nix
nix-shell
```

et voilà. L'environnement contient toues les outils nécessaires pour travailler.

## Avoir un dépôt local

Une difficulté est de pouvoir disposer de quelques paquets, venant compléter la base de paquets globale. Ceci permet de reproduire un fonctionnement où on n'écrit que quelques `rpm` (déclarant leurs dépendances) à installer au dessus d'un système fonctionnel. La réponse à ce problème m'a été donnée dans [ce blog](http://sandervanderburg.blogspot.fr/2014/07/managing-private-nix-packages-outside.html).

Voici par exemple l'expression (enregistrée par exemple dans le ficher `custom-packages.nix`) construisant `sqlalchemy_migrate-0.7.2`, tout en dépendant en entrée de différentes expressions définies dans le `nixpkgs` global (`pythonPackages.nose`, `pythonPackages.unittest2`, `pythonPackages.scripttest`, `pythonPackages.tempita`, `pythonPackages.decorator` et  `pythonPackages.sqlalchemy` dans le cas qui nous concerne):

```nix
{ system ? builtins.currentSystem }:

let
  pkgs = import <nixpkgs> { inherit system; };
  
  callPackage = pkgs.lib.callPackageWith (pkgs // self);
 
  buildPythonPackages = pkgs.pythonPackages.buildPythonPackage;

  python = pkgs.python;

  pythonPackages = pkgs.pythonPackages;

  self = {

    sqlalchemy_migrate = buildPythonPackages rec {
      
      name = "sqlalchemy-migrate-0.7.2";

      src = pkgs.fetchurl {
        url = "https://pypi.python.org/packages/source/s/sqlalchemy-migrate/${name}.tar.gz";
        sha256 = "0cw3q83q3bmq0izrq10y46svsfvwqnj6bcqivgz47w96xrlcfmbz";
      };

      buildInputs = with pythonPackages ; with self; [ nose unittest2 scripttest ];

      propagatedBuildInputs = with pythonPackages ; with self; [ tempita decorator sqlalchemy ];

      preCheck =
        ''
          echo sqlite:///__tmp__ > test_db.cfg
        '';

      # Some tests fail with "unexpected keyword argument 'script_path'".
      doCheck = false;

      meta = {
        homepage = http://code.google.com/p/sqlalchemy-migrate/;
        description = "Schema migration tools for SQLAlchemy";
      };
    };
  };
in
self
```

Un petit

```bash
nix-build  custom-packages.nix -A sqlalchemy_migrate
```

et le tour est joué. Le paquet se construit gentiment !

Cette technique évite d'avoir à travailler dans une arborescence divergente complète de [nixpkgs](https://github.com/NixOS/nixpkgs).
