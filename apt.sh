#!/usr/bin/env bash

CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Set the colours you can use
black='\033[0;30m'
white='\033[0;37m'
red='\033[0;31m'
green='\033[0;32m'
yellow='\033[0;33m'
blue='\033[0;34m'
magenta='\033[0;35m'
cyan='\033[0;36m'

#  Reset text attributes to normal + without clearing screen.
alias Reset="tput sgr0"

# Color-echo.
# arg $1 = message
# arg $2 = Color
cecho() {
  echo -e "${2}${1}"
  # Reset # Reset to normal.
  return
}

if hash apt-get 2>/dev/null; then
  cecho "apt has been installed, just continue install ..." $green
else
  cecho "no apt found! exit ..." $yellow
  exit
fi

# Ask for the administrator password upfront.
sudo -v

cecho "config the DNS" $yellow
echo ""

sudo chmod a+w  /etc/resolv.conf
cat > /etc/resolv.conf <<EOF
nameserver 8.8.8.8
nameserver 8.8.4.4
nameserver 114.114.114.114
EOF

# Keep-alive: update existing `sudo` time stamp until the script has finished.
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

echo ""
cecho "Now time to install my favorate tools ..." $yellow

sudo apt-add-repository ppa:ubuntu-elisp/ppa
sudo apt-get update

apps=(
    # Utilities
    libreadline-dev
    libncurses5-dev
    libpcre3-dev
    libssl-dev
    build-essential
    perl
    make
    cmake

    # proxychains
    # privoxy
    polipo

    ## for python
    python-pip
    python-dev

    ## Dev tools
    wget
    curl
    openssh-server
)

for item in ${apps[@]}; do
  cecho "> ${item}" $magenta
done

echo ""

select yn in "Yes" "No"; do
  case $yn in
    Yes )
        cecho "Ok! installing apps, please wait ... " $yellow
        sudo apt-get install -y ${apps[@]}
        break;;
    No ) break;;
  esac
done

echo -e "\033[40;32m install shadowsocks client \033[0m"
sudo -H pip install shadowsocks

echo -e "\033[40;32m install the fzf \033[0m"
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install

echo -e "\033[40;32m install the z, refer: https://github.com/rupa/z/blob/master/z.sh \033[0m"
git clone https://github.com/rupa/z ~/z
. ~/z/z.sh

echo -e "\033[40;32m install liquidprompt \033[0m"
git clone https://github.com/nojhan/liquidprompt.git ~/.liquidprompt
source ~/.liquidprompt/liquidprompt

read -p "install the awesome tool htop2.0, are you sure? (y/n) " -n 1;
if [[ $REPLY =~ ^[Yy]$ ]]; then
  echo "install htop2.0";
  git clone https://github.com/hishamhm/htop
  cd $CURRENT_DIR/htop && ./autogen.sh && ./configure && make && sudo ln -s $CURRENT_DIR/htop/htop /usr/bin/htop
  cd -
fi;


echo "相当于架了一个http代理链接到socks"
read -p "polipo privoxy (y/n) " -n 1;
if [[ $REPLY =~ ^[Yy]$ ]]; then
  if [ -f "/etc/polipo/config" ]; then
    sudo chmod a+w "/etc/polipo/config"

sudo cat >> /etc/polipo/config <<EOF
socksParentProxy = "localhost:1080"
proxyPort = 8123
socksProxyType = socks5
EOF
    sudo chmod a-w "/etc/polipo/config"
  fi
fi

echo -e "\033[40;32m change the default shell into: /bin/bash\033[0m"
sudo chsh -s /bin/bash

cecho "Done, Happy Hacking At the Speed Of The Thought" $green
