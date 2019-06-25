# .dotfiles

A repo to store my dotfiles between macos and linux

```
git clone git@github.com:splayemu/dotfiles.git .dotfiles
```

## git-fu

Following this tutorial to maintain dotfiles git repo in a detached working directory:

https://www.electricmonk.nl/log/2015/06/22/keep-your-home-dir-in-git-with-a-detached-working-directory/

This repo ignores everything by default.

### Sync dotfiles
```
dgit reset --hard
./copy-dotfiles.sh
```

### Add a file:
```
git add -f .dotfile.supercool
```
