noremap <C-left> :bprev<CR>
noremap <C-right> :bnext<CR>

" Tabmove bindings
nmap <C-Up> :tabmove +1<CR>
nmap <C-Down> :tabmove -1<CR>

" Spell cheking enabled by default
"map <F6> <Esc>:setlocal spell spelllang=es<CR>

" CtrlP binding for ctags
nmap <C-L> :CtrlPTag<CR>

" Gundo
nnoremap <F5> :GundoToggle<CR>
