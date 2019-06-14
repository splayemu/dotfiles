OS="$(uname -a)"

source .bashrc.shared

# For now we just have 1 verison of linux and 1 version of mac so we will just switch on "Linux"
if echo "$OS" | grep -q "Linux"; then
    source .bashrc.debian
else
    source .bashrc.macos
fi
