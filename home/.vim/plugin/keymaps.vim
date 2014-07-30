" Custom vim keymaps are put here for the sake of the organization
" of the .vimrc file

map <F2> :NERDTree<CR>
map <F3> :NERDTreeClose<CR>
map <F4> :NERDTreeFind<CR>
map <F5> :VimShell<CR>

map <leader>c <c-_><c-_>
map <leader>b <c-_>b

imap <C-Space> <C-x><C-u>

nnoremap <silent> [b :bprevious<CR>
nnoremap <silent> ]b :bnext<CR>
nnoremap <silent> [B :bfirst<CR>
nnoremap <silent> ]B :blast<CR>


cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
cnoremap w!! %!sudo tee > /dev/null %

nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>
nnoremap <F6> :!ctags -R<CR>

noremap <Right> :vertical res +3<CR>
noremap <Left> :vertical res -3<CR>
noremap <Up> :res +3<CR>
noremap <Down> :res -3<CR>
nmap ch :.ChangeHashSyntax<CR>
