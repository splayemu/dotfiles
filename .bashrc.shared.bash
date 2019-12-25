
# Use dgit to interact with our remote home repo stored in .dotfiles
alias dgit='git --git-dir ~/.dotfiles/.git --work-tree=$HOME'
alias dotfiles="dgit reset --hard; ./copy-dotfiles.sh"

export VISUAL=vim
export EDITOR="$VISUAL"

echo "Sourced .bashrc.shared.bash"
