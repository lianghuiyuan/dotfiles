##  xuehao zhou’s dotfiles

* This is a derived work from Mathias's dotfiles. Consider to use his setup directly. See: https://github.com/mathiasbynens/dotfiles.
* Disclaimer: I have tuned the dotfiles for my own use. Some of the setup may not be good for you.

## Chaged to Mathias's dotfiles:

### For MacOSX && ubuntu

* use [liquidprompt](https://github.com/nojhan/liquidprompt) for shell prompt instead of the default.
* use [z](https://github.com/rupa/z) for shell auto jump instead of autojump.
* add my awesome vim automatic configuration, you can decide whether use or not in bootsrap.
* add my awesome emacs automatic configuration, you can decide whether use or not in bootsrap.
* support both zsh and bash.
* support both ubuntu and macosx automatic deployment.
* support proxy shadowsocks in terminal using polipo

## Attension: get you into colorful world in item2

- iterm2 - Preferences - Profiles - Text - Text Rendering， remove the **Draw bold text in bright colors**, and then you will get the color world!
- iterm2 - Preferences - Profiles - Terminal - Terminal Emulation - Report Terminal Type: change **xterm** into **xterm-256color**
- man color - add the followiing export into your .bashrc or .zshrc, here you can add into .exports file

    ```
    export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
    export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
    export LESS_TERMCAP_me=$'\E[0m'           # end mode
    export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
    export LESS_TERMCAP_so=$'\E[38;5;246m'    # begin standout-mode - info box
    export LESS_TERMCAP_ue=$'\E[0m'           # end underline
    export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline
    ```

    you can also use the zsh && oh-my-zsh plugin: colored-man-pages instead.

## Installation

**Warning:** If you want to give these dotfiles a try, you should first fork this repository, review the code, and remove things you don’t want or need. Don’t blindly use my settings unless you know what that entails. Use at your own risk!

### Quick intallation

建议采用bash

1. DNS:
```
# 南京信风
nameserver 114.114.114.114
# Google
nameserver 8.8.8.8
# V2EX
nameserver 178.79.131.110
```

2. Install:

对于 mac 用户请安装依赖

    - xcode-select --install
    - wget https://bootstrap.pypa.io/get-pip.py && sudo -H python get-pip.py
    - brew cask install java

    如果安装 pip 失败请在当前目录下使用下面的脚本安装

        sudo chmod a+w sss/get-pip.py
        sudo -H python sss/get-pip.py

ubuntu 用户首先要安装依赖 gnu global, ag

    sudo apt-get build-dep global
    sudo apt-get install libncurses5-dev libncursesw5-dev
    移駕至 https://www.gnu.org/software/global/download.html
     ./configure --with-sqlite3
    make -j4
    make check
    sudo make install
    sudo make installcheck


开始安装

```bash
git clone https://github.com/robertzhouxh/dotfiles /path/to/dotfiles
cd dotfiles
set -- -f; source bootsrap.sh
```

## 安装我的vim配置

./vim.sh

### 常用 vim 配置快捷键

leader is ","

- 列出项目            leader + p
- 列出缓冲区          leader + b
- 列出文件            leader + f
- 打开项目目录        leader + n
- 打开变量列表        leader + ]
- 删除行为空格        leader> +  space
- 窗口切换            C-h,j,k,l>
- 切换到上一个tab(tab last)     Leader + tl
- sudo 写文件         w!!


## 安装我的emacs配置

./emacs.sh

### 常用emacs 快捷键

leader is ","

    leader +
    "."  'avy-goto-char-2                          // 精确快速移动光标
    ","  'other-window                             // 切换窗口
    "a=" 'my-align-single-equals                   // 对其==
    "b"   'projectile-switch-to-buffer             // 切换buffer
    "c"  'comment-dwim                             // 注册选中的行
    "d"  'kill-this-buffer                         // 关闭buffer
    "D"  'delete-window                            // 关闭window
    "e"  (lambda () (interactive) (get-erl-man))   // 获取erlang 帮助文档
    "E"  'sudo-edit-current-file                   // sudo 当前文件
    "g"  'magit-status                             // git 操作
    "hs"  'helm-projectile-ag                      // 搜索text
    "hp" 'helm-projectile                          // 查找工程
    "hd" 'helm-dash-at-point                       // 查找文档
    "o"  'delete-other-windows                     // 删除另一个window
    "O"  'other-frame                              // 切换 frame
    "P"   'projectile-find-file-other-window       // 在另一个窗口中打开搜索到的文件
    "s"  'ag-project                               // Ag search from project's root
    "S"  'delete-trailing-whitespace               // 删除行尾空白
    "t"  'gtags-reindex                            // 建立tags
    "w"  'save-buffer                              // 保存buf
    "x"  'helm-M-x                                 // 进入emacs cmdline

### 对不同语言的支持

修改 lisp/init-languages.el

1. Erlang 部分， 自行适配路径， 版本
2. Golang 部分， 自行安装依赖
    go get github.com/rogpeppe/godef
    go get -u github.com/golang/lint/golint
    go get -u github.com/nsf/gocode
3. Lua
4. js/es7
5. c
6. racket


enjoy it!
