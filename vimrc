set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

filetype off  "required

set number
set nocompatible

set bs=2
set ts=4
set tw=1000000000

set expandtab
set tabstop=8
set softtabstop=4
set shiftwidth=4

"filetype indent off
"filetype plugin on
filetype plugin indent on
syntax on

set autoindent
set showmatch
set showmode
set mousehide

set nowrapscan
set hlsearch
set incsearch

set fileencoding=utf8
set encoding=utf8

au BufWritePre *.cpp,*.h,*.py,*.*pp,*.js,*.html,*.less,*.css,*.md,*.rst,*.clj,*.cljs :%s/\s\+$//e

" Python settings
au BufRead,BufNewFile *.py,*pyw set textwidth=140

" Line wrapping (?)
"set wrap
"set linebreak
" note trailing space at end of next line
"set showbreak=>\ \ \

" File type settings
au BufRead,BufNewFile *.json set filetype=javascript
au BufRead,BufNewFile *.gsp set filetype=html
au BufRead,BufNewFile *.jinja set filetype=htmldjango
au BufNewFile,BufRead *.less set filetype=less
au BufNewFile,BufRead *.edn set filetype=clojure

source ~/.vim/bundles.vim
source ~/.vim/abbreviations.vim
source ~/.vim/bindings.vim

" au InsertLeave,BufWinEnter,BufRead,BufNewFile * colorscheme solarized
"colorscheme desert
" au BufRead,BufNewFile * colorscheme desert


" Extra whitespace mark & remove
hi ExtraWhitespace ctermbg=red guibg=red
au InsertLeave,BufWinEnter * highlight ExtraWhitespace ctermbg=red guibg=red
au InsertLeave,BufWinEnter * match ExtraWhitespace /\s\+$/
