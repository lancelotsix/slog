---
title: ZSH as login shell in NixOs
author: Lancelot SIX
license: cc-by
illustration: /images/nixos.png
---

`Zsh`, especially improved with [OhMyZSH](http://ohmyz.sh/), is a great shell to
work with. In [NixOs](http://nixos.org), the default shell is `bash`, and
everything is confired to work with it.

In particular, using `zsh`, one can unconter some difficulties such as the
following:

```bash
$ rsync -rv nixbox:some/dir/ . 
zsh:1: command not found: rsync
rsync: connection unexpectedly closed (0 bytes received so far) [Receiver]
rsync error: remote command not found (code 127) at io.c(226) [Receiver=3.1.1]
```

This is because the environment is not completely set when the shell tried to
launch a command just after the ssh connection is opened:

```bash
$ ssh nixbox 'echo $PATH'
/usr/bin:/bin:/usr/sbin:/sbin:/nix/store/…-openssh-…/bin
$ ssh nixbox 'source /etc/profile && echo $PATH'
/home/foo/bin:/var/setuid-wrappers:/home/foo/.nix-profile/bin:/home/foo/.nix-profile/sbin:…
```

Fortunately, nixos provide a conviniant and declarative way to properly
configure zsh. To do so, one have to update his `/etc/nix/configuration.nix`
file and make sure it contains something similar to the following:

```nix
{ config, pkgsk ... }:
{
  environment = {
    systemPackages = [ pkgs.zsh ];
    etc."zshenv".text =
      ''
      if [ -z "$__ETC_PROFILE_DONE" ]
      then
        . /etc/profile
      fi
      '';
    shells =
      [ "/run/current-system/sw/bin/zsh"
        "/var/run/current-system/sw/bin/zsh"
      ];
  };

  users.extraUsers.foo = {
    …
    shell = "/run/current-system/sw/bin/zsh";
    …
  };
}
```
