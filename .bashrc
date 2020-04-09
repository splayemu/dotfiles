OS="$(uname -a)"

source ~/.bashrc.shared.bash

# For now we just have 1 verison of linux and 1 version of mac so we will just switch on "Linux"
if echo "$OS" | grep -q "Linux"; then
    source ~/.bashrc.debian.bash
else
    source ~/.bashrc.macos.bash
fi

if  [ -f ~/.bashrc.company.bash ]; then
    source ~/.bashrc.company.bash
else
    echo "No .bashrc.company.bash config"
fi
