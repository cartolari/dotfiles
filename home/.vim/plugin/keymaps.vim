" Custom vim keymaps are put here for the sake of the organization
" of the .vimrc file

map <silent> <F2> :NERDTree<CR>
map <silent> <F3> :NERDTreeClose<CR>
map <silent> <F4> :NERDTreeFind<CR>
map <silent> <F5> :VimShell<CR>

" Tcomment
" Comment Line
map <leader>c <c-_><c-_> 
" Comment Block
map <leader>b <c-_>b

" Move through vim buffers
nnoremap <silent> [b :bprevious<CR>
nnoremap <silent> ]b :bnext<CR>
nnoremap <silent> [B :bfirst<CR>
nnoremap <silent> ]B :blast<CR>

" Get current direcory in command mode
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
" Save as sudo
cnoremap w!! %!sudo tee > /dev/null %

" Clear highlighted searchs
nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>
" Generates ctags file
nnoremap <silent> <F6> :!ctags -R<CR>

"Remap arrow keys to easily resize windows
noremap <silent> <Right> :vertical res +3<CR>
noremap <silent> <Left> :vertical res -3<CR>
noremap <silent> <Up> :res +3<CR>
noremap <silent> <Down> :res -3<CR>

" Changes the old ruby hash syntax :a => :b
" to the newer a: :b
nmap <silent>ch :.ChangeHashSyntax<CR>

" Toggle current folding
nnoremap <Space> za
