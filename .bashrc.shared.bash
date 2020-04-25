# Use dgit to interact with our remote home repo stored in .dotfiles
alias dgit='git --git-dir ~/.dotfiles/.git --work-tree=$HOME'
alias dotfiles="dgit reset --hard; ./copy-dotfiles.sh"

## Better History 
# https://unix.stackexchange.com/questions/1288/preserve-bash-history-in-multiple-terminal-windows
export HISTCONTROL=ignoredups:erasedups  # no duplicate entries
export HISTSIZE=100000                   # big big history
export HISTFILESIZE=100000               # big big history
shopt -s histappend                      # append to history, don't overwrite it
# Save and reload the history after each command finishes
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"


export VISUAL=vim
export EDITOR="$VISUAL"

# install z which builds up a database of common directories
# type `z dir-name` to jump to the directory name with autocomplete
. ~/z/z.sh

echo "Sourced .bashrc.shared.bash"
