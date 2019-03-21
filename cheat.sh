#!/bin/bash

sudo -v

echo -e "\033[40;32m install the cheat ... \033[0m"
# if [ `uname -s` = "Linux" ]; then
  sudo -H pip install cheat
# elif [ `uname -s` = "Darwin" ]; then
#   brew install cheat
# else
#   echo -e "\033[40;32m unsupported system, exit \033[0m"
# fi
# echo -e "\033[40;32m Cheat installed \033[0m"

# BASEDIR=$(dirname $0)
# cd $BASEDIR
# CURRENT_DIR=`pwd`
# or you can try:
CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

lnif() {
    if [ -e "$1" ]; then
        ln -s "$1" "$2"
    fi
}

echo -e "\033[40;32m Ensure you have installed the cheat first ... \033[0m"
# echo -e "\033[40;32m Step1: Backing up current cheat config \033[0m"
# today=`date +%Y%m%d`
# for i in $HOME/.emacs.d; do [ -e $i ] && [ ! -L $i ] && mv $i $i.$today; done
# for i in $HOME/.emacs.d; do [ -L $i ] && unlink $i ; done

echo -e "\033[40;32m Step1: unlink the current cheat config folder \033[0m"
if [ -e "$HOME/.cheat" ]; then
  #unlink "$HOME/.cheat"
  for i in $HOME/.cheat; do [ -L $i ] && unlink $i ; done
fi


echo -e "\033[40;32m Step2: Setting up symlinks \033[0m"
lnif "$CURRENT_DIR/.cheat" "$HOME/.cheat"


cd $CURRENT_DIR
echo -e "\033[40;32m Done, Happy hacking With The Awesome Cheat \033[0m"
