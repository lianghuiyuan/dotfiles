call plug#begin('~/.vim/plugged')
let mapleader = ','
let g:mapleader = ','
"let mapleader = "\<Space>"
let g:plug_timeout = 100

" ================================ Plugins to be installed =======================================
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
Plug 'bronson/vim-trailing-whitespace'
Plug 'rking/ag.vim'
Plug 'kien/ctrlp.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'vim-scripts/DrawIt'

" themes
Plug 'tomasr/molokai'
Plug 'cocopon/iceberg.vim'
Plug 'dracula/vim'
Plug 'daylerees/colour-schemes'

"Load local plugins
if filereadable(expand("~/.vim/vimrc.bundles.local"))
  source ~/.vim/vimrc.bundles.local
endif

call plug#end()


" ================================ global configuration =======================================
set nocompatible                                             " don't bother with vi compatibility
syntax enable                                                " enable syntax highlighting
filetype on                                                  " turn on file type check
filetype indent on                                           " turn on indent acording to file type
filetype plugin on                                           " turn on indent acording to file type and plugin
filetype plugin indent on
set autoindent
set autoread                                                 " reload files when changed on disk, i.e. via `git checkout`
set backspace=2                                              " Fix broken backspace in some setups
set backupcopy=yes                                           " see :help crontab
set clipboard=unnamed                                        " yank and paste with the system clipboard
set directory-=.                                             " don't store swapfiles in the current directory
set ignorecase                                               " case-insensitive search
set incsearch                                                " search as you type
set number                                                   " show line numbers
set ruler                                                    " show where you are
set scrolloff=3                                              " show context above/below cursorline
set showcmd
set smartcase                                                " case-sensitive search if any caps
set wildmenu                                                 " show a navigable menu for tab completion
set wildmode=longest,list,full
set binary
set hlsearch                                                 " highlight search
set noeol                                                    " no end of line at the end of the file
"set cursorcolumn
"set cursorline

" tab
set expandtab                                             " Use spaces instead of tabs and --------> Ctrl+V + Tab]
set smarttab                                              " Be smart when using tabs ;)
set shiftround                                            " Round indent to multiple of 'shiftwidth' for > and < commands
set shiftwidth=4
set tabstop=4

set ai                                                     "Auto indent
set si                                                     "Smart indent
set nowrap                                                 "Don't Wrap lines (it is stupid)

" Linebreak on 500 characters
set lbr
set tw=500

" FileEncode Settings
set encoding=utf-8                                                       " 设置新文件的编码为 UTF-8
set fileencodings=ucs-bom,utf-8,cp936,gb18030,big5,euc-jp,euc-kr,latin1  " 自动判断编码时，依次尝试以下编码：
set termencoding=utf-8
set ffs=unix,dos,mac                                         " Use Unix as the standard file type
set formatoptions+=m
set formatoptions+=B
set hidden                                                   " A buffer becomes hidden when it is abandoned
set wildmode=list:longest
set ttyfast

" configuration for airline
set t_Co=256
set laststatus=2


" macos vs linux clipboard
if has("mac")
  set clipboard+=unnamed
else
  set clipboard=unnamedplus
endif


" ================================ Key Mapping =======================================
" :help map ===> [n|v|nore|un|]map
" nore: no recursive
" map {lhs} {rhs} ===> 表示将{lhs}按键序列映射到{rhs}按键序列
" Command-Line/Ex Mode
" normal mode enter (:) and then get into Command-Line namely C-mode
" normal mode enter (Q) and then get into multi-Command-Line namely Ex-mode

inoremap jj <esc>
nnoremap JJJJ <nop>
cnoremap w!!                 %!sudo tee > /dev/null %

nnoremap <leader>p           :CtrlP<CR>
nnoremap <leader>b           :CtrlPBuffer<CR>
nnoremap <leader>T           :CtrlPClearCache<CR>:CtrlP<CR>
nnoremap <leader>n           :NERDTreeToggle<CR>
nnoremap <leader>a           :Ag<SPACE>
" bind K to grep word under cursor
nnoremap K                   :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>
noremap <Leader>m mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm  " Remove the Windows ^M - when the encodings gets messed up
noremap <silent><leader>/    :nohls<CR>             " 去掉搜索高亮
noremap <C-h>                <C-w>h
noremap <C-j>                <C-w>j
noremap <C-k>                <C-w>k
noremap <C-l>                <C-w>l

map <leader><space>  :FixWhitespace<cr>
map <leader>d :bdelete<cr>
map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tj :tabnext <cr>
map <leader>tk :tabprevious <cr>
map <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/  " Opens a new tab with the current buffer's path, S


command -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!


" Go crazy!
if filereadable(expand("~/.vimrc.local"))
  " In your .vimrc.local, you might like:
  " set whichwrap+=<,>,h,l,[,] " Wrap arrow keys between lines
  " autocmd! bufwritepost .vimrc source ~/.vimrc
  source ~/.vimrc.local
endif

" =========================> plugins config <===============================================
let g:UltiSnipsExpandTrigger       = '<C-j>'
let g:UltiSnipsJumpForwardTrigger  = '<C-j>'
let g:UltiSnipsJumpBackwardTrigger = '<C-k>'
let g:UltiSnipsSnippetDirectories  = ['UltiSnips']
let g:UltiSnipsSnippetsDir         = '~/.vim/UltiSnips'
" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

" Use The Silver Searcher https://github.com/ggreer/the_silver_searcher
if executable('ag')
  " Use Ag over Grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

" Automatic commands
if has("autocmd")
  " COOL HACKS
  " Make sure Vim returns to the same line when you reopen a file.
  augroup line_return
      au!
      au BufReadPost *
          \ if line("'\"") > 0 && line("'\"") <= line("$") |
          \     execute 'normal! g`"zvzz' |
          \ endif
  augroup END

  fun! <SID>StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
  call cursor(l, c)
    endfun

  autocmd BufNewFile,BufRead *.h  setlocal filetype=cpp

  " Highlight TODO, FIXME, NOTE, etc.
  if v:version > 701
    autocmd Syntax * call matchadd('Todo',  '\W\zs\(TODO\|FIXME\|CHANGED\|DONE\|XXX\|BUG\|HACK\)')
    autocmd Syntax * call matchadd('Debug', '\W\zs\(NOTE\|INFO\|IDEA\|NOTICE\)')
  endif

  au BufNewFile,BufRead *.erl setf erlang
  au FileType erlang setlocal errorformat=%f:%l:\ %m
  au BufNewFile,BufRead *.yaml set filetype=yaml.ansible

  autocmd BufRead,BufNewFile *.md set filetype=markdown
  autocmd BufRead,BufNewFile *.md set spell
  autocmd BufNewFile,BufRead *.json setfiletype json syntax=javascript " Treat .json files as .js
  autocmd BufNewFile,BufRead *.md setlocal filetype=markdown " Treat .md files as Markdown
  autocmd FileType python,c,c++,lua set tabstop=4 shiftwidth=4 expandtab ai
  autocmd FileType ruby,javascript,sh,go,html,css,scss set tabstop=2 shiftwidth=2 softtabstop=2 expandtab ai
  autocmd BufRead,BufNew *.md,*.mkd,*.markdown  set filetype=markdown.mkd
  autocmd FileType c,cpp,erlang,go,lua,javascript,python,perl autocmd BufWritePre <buffer> :call <SID>StripTrailingWhitespaces()
endif


" ======================== UI =======================
if (&t_Co == 256 || has('gui_running'))
    if ($TERM_PROGRAM == 'iTerm.app')
        colorscheme molokai
    else
        colorscheme molokai
    endif
endif
