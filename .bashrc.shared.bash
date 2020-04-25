
# Use dgit to interact with our remote home repo stored in .dotfiles
alias dgit='git --git-dir ~/.dotfiles/.git --work-tree=$HOME'
alias dotfiles="dgit reset --hard; ./copy-dotfiles.sh"

export VISUAL=vim
export EDITOR="$VISUAL"

# install z which builds up a database of common directories
# type `z dir-name` to jump to the directory name with autocomplete
. ~/z/z.sh

echo "Sourced .bashrc.shared.bash"
