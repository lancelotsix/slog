---
title: Getting some space free in NixOs
author: Lancelot SIX
license: cc-by
illustration: /images/nixos.png
---

NixOS is a great operating system. However, by time to time, I just realise that
my `/nix` gets bigger and bigger. I of course use the standard garbage collector
in order to make some space free :

```bash
$ nix-collect-garbage
```

is used to delete unused derivations from the nix-store, and

```bash
$ nix-collect-garbage -d
```

is used to remove past generations (which makes it impossible at that time to
perform a rollback after it). But despite this, `nix` still continue to grow. So
what did I miss ?

Simple : NixOS is just super awsome. What happens is that when I develop and
when add packages to `nixpkgs`, I use the `nix-build` command to test my work.
`nix-build` instantiates and realises the derivation, and at the end places a
symlink to the build derivation directory named `result`. The same kind of thing
happens wen using nixops. All the built targets are kept somewhere in the store,
referenced by symlinks.

As long as those symlinks exists, the referenced derivations will not be removed
from the nix store (as does its dependencies). Therefore, to allow the garbage
collector to do some more cleaning, it is important to remove this symlink.

For those who cannot remember where they where when they used `nix-build`, it is
possible to use the following command to retrieve all the roots :

```bash
$ nix-store --gc --print-roots
```

It is also possible to find those same roots in `/nix/var/nix/gcroots/`. This
folder contains symlinks to all the existing  roots. The so called roots are
what the garbage collector looks for when doing its job. Are preserves those
roots, and all their (indirect and indirect) dependencies.

For those who are interested in this topic, you can check additional
informations
[here](http://lethalman.blogspot.com/2014/08/nix-pill-11-garbage-collector.html).
