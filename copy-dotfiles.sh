OS="$(uname -a)"

# For now we just have 1 verison of linux and 1 version of mac so we will just switch on "Linux"
if echo "$OS" | grep -q "Linux"; then
    echo "Copying debian-dotfiles/*"
    cp -rp debian-dotfiles/. ~
else
    echo "Copying macos-dotfiles/*"
    cp -rp macos-dotfiles/. ~
fi
