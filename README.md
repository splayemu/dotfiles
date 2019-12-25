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

or once bash has all the aliases
```
dotfiles
```

### Add a file:
```
git add -f .dotfile.supercool
```

## Troubleshooting
### Spacemacs
If you are having trouble with spacemacs, just remove your `~/.emacs.d` directory and reinstall Spacemacs:
1. Update emacs
```
sudo apt-get update emacs
```
2. Download spacemacs
```
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
```
3. Update everything through the in editor button
4. Update your packages through the in editor button
