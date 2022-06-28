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
alias dgit='git --git-dir ~/.dotfiles/.git --work-tree=$HOME'
alias dotfiles="dgit reset --hard; ./copy-dotfiles.sh"
```

Now that bash has all the aliases
```
dotfiles
```

### Add a file:
```
git add -f .dotfile.supercool
```
