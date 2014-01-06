

Bundle 'gmarik/vundle'
Bundle 'vim-elixir'
Bundle 'vim-less'
Bundle 'vim-sparkup'
Bundle 'vim-flake8'
Bundle 'vim-coffee-script'
Bundle 'bling/vim-bufferline'
Bundle 'airblade/vim-gitgutter'
Bundle 'tpope/vim-fugitive'
Bundle 'guns/vim-clojure-static'
Bundle 'plasticboy/vim-markdown'

Bundle 'tomtom/tcomment_vim'
Bundle 'sjl/gundo.vim'
Bundle 'kien/tabman.vim'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'kien/ctrlp.vim'
Bundle 'majutsushi/tagbar'
Bundle 'mitsuhiko/vim-jinja'
Bundle 'bling/vim-airline'

Bundle 'zeis/vim-kolor'
Bundle 'noahfrederick/vim-hemisu'
Bundle 'Pychimp/vim-luna'

let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_open_new_file = 't'
let g:ctrlp_open_multiple_files = 't'
let g:ctrlp_max_depth = 40
let g:ctrlp_max_height = '20'
let g:ctrlp_us_caching = 1
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_extensions = ['tag', 'buffertag', 'quickfix', 'dir', 'rtscript',
                         \ 'undo', 'line', 'changes', 'mixed', 'bookmarkdir']

let g:flake8_max_line_length = 110
let g:flake8_ignore="E126,#E127"

let g:gundo_close_on_revert = 1

nmap <F8> :TagbarToggle<CR>
nmap <F9> :TMToggle<CR>

let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll|pyc)$',
  \ }

let g:airline#extensions#bufferline#enabled = 1
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#branch#empty_message = ''
let g:airline#extensions#tagbar#enabled = 1
let g:airline#extensions#virtualenv#enabled = 1
let g:airline_detect_paste=1
let g:airline#extensions#tabline#enabled = 0

set laststatus=2
