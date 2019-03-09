#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE}")";

git pull origin master;

function doIt() {
	# rsync = remote sync的简称
	# rsnync用于从一个位置到另外一个位置同步文件和文件夹。备份的地址可以是本地也可以是remote server。
	# 语法：$rsync options source destination
	# source和destination可以是本地或者远程目录。对于远程的情况，需要指定login name, remote server name and location
	# --exclude 避开同步指定的文件夹
	#rsync \
	#  --exclude "bootstrap.sh" \
        #  --exclude "brew.sh" \
        #  --exclude "apt.sh" \
        #  --exclude "vim.sh" \
        #  --exclude "emacs.sh" \
        #  --exclude "sss/" \
        #  --exclude "emacs_client.sh" \
        #  --exclude "README.md" \
        #  --exclude "LICENSE-MIT.txt" \
        #  --exclude ".git/" \
        #  --exclude ".DS_Store" \
        #  --exclude ".vimrc" \
        #  --exclude ".vim" \
        #  --exclude ".emacs.d" \
        #  --exclude ".tmux/" \
        #  --exclude ".curlrc" \
        #  -avh --no-perms . ~;
	rsync \
	  --include ".aliases" \
	  --include ".bash_profile" \
	  --include ".bash_prompt" \
	  --include ".bashrc" \
	  --include ".curlrc" \
	  --include ".editorconfig" \
	  --include ".emacs_simple" \
	  --include ".eslintrc" \
	  --include ".exports" \
	  --include ".functions" \
	  --include ".gdbinit" \
	  --include ".gitattributes" \
	  --include ".gitconfig" \
	  --include ".globalrc" \
	  --include ".gvimrc" \
	  --include ".hgignore" \
	  --include ".hushlogin" \
	  --include ".inputrc" \
	  --include ".osx*" \
	  --include ".polipo" \
	  --include ".proxychains.conf" \
	  --include ".screenrc" \
	  # --include ".shadowsocks.json" \
	  --include ".vimrc" \
	  --include ".wgetrc" \
	  --include ".zshrc" \
          --exclude "*" \
          -avh --no-perms . ~;

	  # -a 归档模式，表示以递归方式传输文件，并保持所有文件属性 -v 详细模式输出 -h output numbers in a human-readable format 
	  # --no-perms  不保留权限
	source ~/.bash_profile;
}

if [ "$1" == "--force" -o "$1" == "-f" ]; then
	doIt;
else
	read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1;
	echo "";
	if [[ $REPLY =~ ^[Yy]$ ]]; then
		doIt;
	fi;
fi;
# unset为shell内建指令，可删除变量或函数
unset doIt;


echo ""

echo -e "\033[40;32m start to install command line tools for your system ...\033[0m"

sysType=`uname -s`

echo -e "\033[40;32m Your system is $sysType \033[0m"

if [ $sysType = "Linux" ]; then
    source ./apt.sh;
elif [ $sysType = "Darwin" ]; then
    source ./brew.sh;
else
    echo -e "\033[40;32m unsupported system, exit \033[0m"
fi

echo ""
echo -e "\033[40;32m All done, HAPPY HACKING :-) \033[0m"
echo ""
