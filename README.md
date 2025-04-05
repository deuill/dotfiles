# Deuill's Dotfiles

This repository contains *opinionated* configuration, mainly aimed at Sway-based environments; you can find more generic configuration [here](https://github.com/deuill/archlinux-packages/tree/trunk/deuill-sway/files/etc/skel/.config), and if you're on ArchLinux, installable as a [package](https://github.com/deuill/archlinux-packages/tree/trunk).

## Installation

You can pull these dotfiles into your `$HOME` by performing a bare clone, and then checking out into your home directory:

```sh
$ git clone --bare https://git.deuill.org/deuill/dotfiles.git ~/.dotfiles
$ git --git-dir=$HOME/.dotfiles --work-tree=$HOME checkout
```

You can create an alias for the above `git` invocation to help with the remote work tree:

```sh
$ alias dot="git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
```

You can, and should also hide untracked files, to make managing dotfiles easier:

```sh
$ dot config --local status.showUntrackedFiles no
```
