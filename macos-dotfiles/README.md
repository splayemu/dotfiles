## Computer Setup for MacOS

1. Install xcode from Apple Store
2. Install iterm2
3. Install homebrew: https://brew.sh/
4. Install karabiner-elements

After installing karabiner-elements, install the config `./karabiner.json`
```
mv ~/.dotfiles/macos-dotfiles/karabiner.json ~/.config/karabiner/
```

5. Create ssh-keys: `ssh-keygen -m PEM -t rsa`
6. Install emacs 28
```
brew tap d12frosted/emacs-plus
brew install emacs-plus@28
```
Find emacs install location (depends on M1/intel/version of homebrew). 
First look for where emacs is installed. 
Then check where the symlink points to which is where emacs is actually installed.
```
which emacs
ls -ltr ls -ltr /usr/local/bin/
```

Add symlink for emacs
```
ln -s /opt/homebrew/Cellar/emacs-plus@28/28.1/Emacs.app /Applications/Emacs.app
```
Old way: https://emacsformacosx.com/

7. Install tmux
See https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard/

```
brew install reattach-to-user-namespace tmux
```

8. Install fzf
```
brew install fzf
```

8. Install 'Solarized Light' theme for iterm2
Open `iterm-theme/solarized_light.itermcolors` in iterm2. 
![iterm2_color_preferences](https://user-images.githubusercontent.com/1490056/168616987-d5a7fd88-5f0e-43e3-9164-3eba64e4aeb4.png)

9. Update finder to show full paths
```
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true; killall Finder
```

10. Install mission control commands

![mission_control_config.png](mission_control_config.png)

11. Update bash

https://itnext.io/upgrading-bash-on-macos-7138bd1066ba

12. Set bash as default shell

a. Find bash:
```
$ which bash
/opt/homebrew/bin/bash
```

b. Add bash to `/etc/shells`
```
sudo su -
echo /opt/homebrew/bin/bash >> /etc/shells
exit
```

c. Update the shell for the user
```
chsh -s /opt/homebrew/bin/bash
```

## Emacs Shit
Followed this [client server setup](https://www.hhyu.org/posts/emacs_clientserver/).

1. Created the scripts `bin/emacsserver` and `bin/ec`
2. `emacsserver` finds the currently running server
3. `ec` tries to connect to the server or starts a new server
