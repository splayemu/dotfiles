# 256 colors
alias tmux="TERM=screen-256color-bce tmux"

# history stuff found https://unix.stackexchange.com/questions/1288/preserve-bash-history-in-multiple-terminal-windows
export HISTCONTROL=ignoredups:erasedups  # no duplicate entries
export HISTSIZE=100000                   # big big history
export HISTFILESIZE=100000               # big big history
shopt -s histappend                      # append to history, don't overwrite it

# Save and reload the history after each command finishes
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"

# Add Homebrew `/usr/local/bin` and User `~/bin` to the `$PATH`
PATH=/usr/local/bin:$PATH
PATH=$HOME/bin:$PATH
export PATH

# Load the shell dotfiles, and then some:
# * ~/.path can be used to extend `$PATH`.
# * ~/.extra can be used for other settings you don’t want to commit.
for file in ~/.{path,bash_prompt,exports,aliases,functions,extra}; do
  [ -r "$file" ] && source "$file"
done
unset file

export JAVA_HOME=$(/usr/libexec/java_home)

source /usr/local/opt/asdf/asdf.sh

export CHANGELOG_GITHUB_TOKEN="2d06b6c3770261c2232baf6852e4a9abeb6c25d6"

if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi
source ~/.git-completion.bash

complete -C ./autocomplete.rb -o default thor

set -o vi
set show-mode-in-prompt on

echo "Sourced .bashrc.macos"
